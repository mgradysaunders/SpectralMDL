#include "doctest.h"

#include <filesystem>
#include <fstream>

#include "smdl/Support/SpectralRenderImage.h"

namespace fs = std::filesystem;

TEST_CASE("SpectralRenderImage") {
  auto tmpDir{fs::temp_directory_path() / "smdl-spectral-render-image-test"};
  fs::remove_all(tmpDir);
  fs::create_directories(tmpDir);
  // A 3x2 image with 4 bands and distinct values everywhere: pixel
  // (x, y) band b accumulates 5 samples summing to 5 * (100x + 10y + b),
  // so the stored mean recovers 100x + 10y + b exactly.
  constexpr size_t NUM_BANDS = 4;
  constexpr size_t NUM_X = 3;
  constexpr size_t NUM_Y = 2;
  constexpr uint64_t SPP = 5;
  const float wavelengths[NUM_BANDS] = {400.0f, 500.0f, 600.0f, 700.0f};
  auto image{smdl::SpectralRenderImage(NUM_BANDS, NUM_X, NUM_Y)};
  for (size_t y = 0; y < NUM_Y; y++) {
    for (size_t x = 0; x < NUM_X; x++) {
      double sums[NUM_BANDS]{};
      for (size_t b = 0; b < NUM_BANDS; b++)
        sums[b] = double(SPP) * double(100 * x + 10 * y + b);
      image(x, y).addSamples(SPP, sums);
    }
  }
  SUBCASE("Mean accessor divides by the sample count") {
    CHECK(image(0, 0).mean(0) == 0.0);
    CHECK(image(2, 1).mean(3) == 213.0);
    // A pixel with no samples must read back as zero, not NaN.
    auto empty{smdl::SpectralRenderImage(NUM_BANDS, 1, 1)};
    CHECK(empty(0, 0).mean(0) == 0.0);
  }
  SUBCASE("ENVI round trip") {
    auto fileName{(tmpDir / "test.envi").string()};
    const std::string extraLines[] = {std::string("smdl sampler = test-1"),
                                      std::string("smdl args = -spp 5")};
    image.writeENVIFile(smdl::Span<const float>(wavelengths, NUM_BANDS),
                        fileName,
                        smdl::Span<const std::string>(extraLines, 2));
    auto loaded{smdl::SpectralRenderImage::readENVIFile(fileName)};
    CHECK(loaded.image.getNumBands() == NUM_BANDS);
    CHECK(loaded.image.getNumPixelsX() == NUM_X);
    CHECK(loaded.image.getNumPixelsY() == NUM_Y);
    CHECK(loaded.samplesPerPixel == SPP);
    REQUIRE(loaded.wavelengths.size() == NUM_BANDS);
    CHECK(loaded.wavelengths[0] == 400.0f);
    CHECK(loaded.wavelengths[3] == 700.0f);
    // The extra header lines must come back through `fields`.
    REQUIRE(loaded.fields.count("smdl sampler") == 1);
    CHECK(loaded.fields.at("smdl sampler") == "test-1");
    REQUIRE(loaded.fields.count("smdl args") == 1);
    CHECK(loaded.fields.at("smdl args") == "-spp 5");
    // Means and the reconstructed accumulator must match the original.
    for (size_t y = 0; y < NUM_Y; y++) {
      for (size_t x = 0; x < NUM_X; x++) {
        auto pixel{loaded.image(x, y)};
        CHECK(pixel.totalCount == SPP);
        for (size_t b = 0; b < NUM_BANDS; b++)
          CHECK(pixel.mean(b) == doctest::Approx(double(100 * x + 10 * y + b))
                                     .epsilon(1e-12));
      }
    }
    // Merging the loaded image back onto the original must double the
    // counts and totals, leaving the means unchanged: this is the
    // resume merge.
    image.add(loaded.image);
    CHECK(image(1, 1).totalCount == 2 * SPP);
    CHECK(image(1, 1).mean(2) == doctest::Approx(112.0).epsilon(1e-12));
  }
  SUBCASE("ENVI read rejects a missing file") {
    CHECK_THROWS((void)smdl::SpectralRenderImage::readENVIFile(
        (tmpDir / "nonexistent.envi").string()));
  }
  SUBCASE("ENVI read rejects a malformed header") {
    auto fileName{(tmpDir / "bad.envi").string()};
    std::ofstream(fileName + ".hdr") << "NOT ENVI\n";
    std::ofstream(fileName) << "";
    CHECK_THROWS((void)smdl::SpectralRenderImage::readENVIFile(fileName));
  }
  SUBCASE("ENVI read rejects a truncated binary") {
    auto fileName{(tmpDir / "short.envi").string()};
    image.writeENVIFile(smdl::Span<const float>(wavelengths, NUM_BANDS),
                        fileName);
    fs::resize_file(fileName, 8 * (NUM_BANDS * NUM_X * NUM_Y - 1));
    CHECK_THROWS((void)smdl::SpectralRenderImage::readENVIFile(fileName));
  }
  SUBCASE("No 'samples per pixel' reads back with a count of 1") {
    auto fileName{(tmpDir / "foreign.envi").string()};
    image.writeENVIFile(smdl::Span<const float>(wavelengths, NUM_BANDS),
                        fileName);
    // Strip the field to simulate a foreign or legacy header.
    auto headerName{fileName + ".hdr"};
    auto text{std::string()};
    {
      auto file{std::ifstream(headerName)};
      for (std::string line; std::getline(file, line);)
        if (line.rfind("samples per pixel", 0) != 0) text += line + "\n";
    }
    std::ofstream(headerName) << text;
    auto loaded{smdl::SpectralRenderImage::readENVIFile(fileName)};
    CHECK(loaded.samplesPerPixel == 0);
    CHECK(loaded.image(2, 1).totalCount == 1);
    CHECK(loaded.image(2, 1).mean(3) == doctest::Approx(213.0).epsilon(1e-12));
  }
  fs::remove_all(tmpDir);
}
