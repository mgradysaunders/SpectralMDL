#include "doctest.h"

#include <filesystem>
#include <fstream>

#include "smdl/RenderUtil/SpectralFilm.h"

namespace fs = std::filesystem;

TEST_CASE("SpectralFilm") {
  auto tmpDir{fs::temp_directory_path() / "smdl-spectral-film-test"};
  fs::remove_all(tmpDir);
  fs::create_directories(tmpDir);
  // A 3x2 film with 4 bands and distinct values everywhere: pixel
  // (x, y) band b accumulates 5 samples summing to 5 * (100x + 10y + b),
  // so the stored mean recovers 100x + 10y + b exactly.
  constexpr size_t NUM_BANDS = 4;
  constexpr size_t NUM_X = 3;
  constexpr size_t NUM_Y = 2;
  constexpr uint64_t SPP = 5;
  const float wavelengths[NUM_BANDS] = {400.0f, 500.0f, 600.0f, 700.0f};
  auto film{smdl::SpectralFilm(NUM_BANDS, NUM_X, NUM_Y)};
  film.addSamples(SPP);
  for (size_t y = 0; y < NUM_Y; y++) {
    for (size_t x = 0; x < NUM_X; x++) {
      double sums[NUM_BANDS]{};
      for (size_t b = 0; b < NUM_BANDS; b++)
        sums[b] = double(SPP) * double(100 * x + 10 * y + b);
      film.addTotals(x, y, sums);
    }
  }
  SUBCASE("Mean accessor divides by the sample count") {
    CHECK(film.getNumSamples() == SPP);
    CHECK(film.mean(0, 0, 0) == 0.0);
    CHECK(film.mean(2, 1, 3) == 213.0);
    // A film with no samples must read back as zero, not NaN.
    auto empty{smdl::SpectralFilm(NUM_BANDS, 1, 1)};
    CHECK(empty.getNumSamples() == 0);
    CHECK(empty.mean(0, 0, 0) == 0.0);
  }
  SUBCASE("Resize resets the sample count") {
    film.resize(NUM_BANDS, NUM_X, NUM_Y);
    CHECK(film.getNumSamples() == 0);
    CHECK(film.mean(2, 1, 3) == 0.0);
  }
  SUBCASE("ENVI round trip") {
    auto fileName{(tmpDir / "test.envi").string()};
    const std::string extraLines[] = {std::string("smdl sampler = test-1"),
                                      std::string("smdl args = -spp 5")};
    film.writeENVIFile(smdl::Span<const float>(wavelengths, NUM_BANDS),
                       fileName, smdl::Span<const std::string>(extraLines, 2));
    auto loadedFilm{smdl::SpectralFilm{}};
    auto loaded{loadedFilm.readENVIFile(fileName)};
    CHECK(loadedFilm.getNumBands() == NUM_BANDS);
    CHECK(loadedFilm.getNumPixelsX() == NUM_X);
    CHECK(loadedFilm.getNumPixelsY() == NUM_Y);
    CHECK(loaded.samplesPerPixel == SPP);
    CHECK(loadedFilm.getNumSamples() == SPP);
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
        for (size_t b = 0; b < NUM_BANDS; b++)
          CHECK(loadedFilm.mean(x, y, b) ==
                doctest::Approx(double(100 * x + 10 * y + b)).epsilon(1e-12));
      }
    }
    // Merging the loaded film back onto the original must double the
    // count and the totals, leaving the means unchanged: this is the
    // resume merge.
    film.add(loadedFilm);
    CHECK(film.getNumSamples() == 2 * SPP);
    CHECK(film.mean(1, 1, 2) == doctest::Approx(112.0).epsilon(1e-12));
  }
  SUBCASE("ENVI window round trip") {
    // The window is the middle column, which is where a windowed render
    // has samples; everything else is untouched.
    const smdl::int4 window{1, 0, 2, int(NUM_Y)};
    auto windowed{smdl::SpectralFilm(NUM_BANDS, NUM_X, NUM_Y)};
    windowed.addSamples(SPP);
    for (size_t y = 0; y < NUM_Y; y++) {
      double sums[NUM_BANDS]{};
      for (size_t b = 0; b < NUM_BANDS; b++)
        sums[b] = double(SPP) * double(100 + 10 * y + b);
      windowed.addTotals(1, y, sums);
    }
    auto fileName{(tmpDir / "windowed.envi").string()};
    windowed.writeENVIFile(smdl::Span<const float>(wavelengths, NUM_BANDS),
                           fileName, {}, window);
    auto loadedFilm{smdl::SpectralFilm{}};
    auto loaded{loadedFilm.readENVIFile(fileName)};
    CHECK(loaded.samplesPerPixel == SPP);
    CHECK(loaded.window[0] == 1);
    CHECK(loaded.window[1] == 0);
    CHECK(loaded.window[2] == 2);
    CHECK(loaded.window[3] == int(NUM_Y));
    CHECK(loadedFilm.getNumSamples() == SPP);
    for (size_t y = 0; y < NUM_Y; y++) {
      for (size_t x = 0; x < NUM_X; x++) {
        // Outside the window the totals are dropped, so the pixel reads
        // back black rather than claiming the window's samples.
        CHECK(loadedFilm.mean(x, y, 0) ==
              (x == 1 ? doctest::Approx(double(100 + 10 * y))
                      : doctest::Approx(0.0)));
      }
    }
    // The resume merge: only the window has totals, so the count applies
    // to it and the rest stays black.
    windowed.add(loadedFilm);
    CHECK(windowed.getNumSamples() == 2 * SPP);
    CHECK(windowed.mean(1, 0, 2) == doctest::Approx(102.0).epsilon(1e-12));
    CHECK(windowed.mean(0, 0, 0) == 0.0);
  }
  SUBCASE("ENVI window drops totals outside it") {
    // What the guided pass combination leaves behind: a full frame of
    // pixels with zero totals outside the window. The recorded count is
    // the window's, and the pixels outside come back black rather than
    // claiming samples of black.
    const smdl::int4 window{0, 0, int(NUM_X), 1};
    auto windowed{smdl::SpectralFilm(NUM_BANDS, NUM_X, NUM_Y)};
    windowed.addSamples(SPP);
    for (size_t y = 0; y < NUM_Y; y++) {
      for (size_t x = 0; x < NUM_X; x++) {
        double sums[NUM_BANDS]{};
        if (y == 0)
          for (size_t b = 0; b < NUM_BANDS; b++)
            sums[b] = double(SPP) * double(100 * x + b);
        windowed.addTotals(x, y, sums);
      }
    }
    auto fileName{(tmpDir / "combined.envi").string()};
    windowed.writeENVIFile(smdl::Span<const float>(wavelengths, NUM_BANDS),
                           fileName, {}, window);
    auto loadedFilm{smdl::SpectralFilm{}};
    auto loaded{loadedFilm.readENVIFile(fileName)};
    CHECK(loaded.samplesPerPixel == SPP);
    CHECK(loaded.window[3] == 1);
    CHECK(loadedFilm.getNumSamples() == SPP);
    CHECK(loadedFilm.mean(2, 0, 1) == doctest::Approx(201.0).epsilon(1e-12));
    CHECK(loadedFilm.mean(2, 1, 1) == 0.0);
  }
  SUBCASE("ENVI whole-frame window records no window") {
    auto fileName{(tmpDir / "whole.envi").string()};
    film.writeENVIFile(smdl::Span<const float>(wavelengths, NUM_BANDS),
                       fileName, {}, smdl::int4{0, 0, int(NUM_X), int(NUM_Y)});
    auto headerName{fileName + ".hdr"};
    auto text{std::string()};
    {
      auto file{std::ifstream(headerName)};
      for (std::string line; std::getline(file, line);) text += line + "\n";
    }
    CHECK(text.find("smdl window") == std::string::npos);
    auto loadedFilm{smdl::SpectralFilm{}};
    auto loaded{loadedFilm.readENVIFile(fileName)};
    CHECK(loaded.window[0] == 0);
    CHECK(loaded.window[1] == 0);
    CHECK(loaded.window[2] == int(NUM_X));
    CHECK(loaded.window[3] == int(NUM_Y));
    CHECK(loadedFilm.getNumSamples() == SPP);
  }
  SUBCASE("ENVI write rejects a window outside the frame") {
    auto fileName{(tmpDir / "badwindow.envi").string()};
    CHECK_THROWS(film.writeENVIFile(
        smdl::Span<const float>(wavelengths, NUM_BANDS), fileName, {},
        smdl::int4{0, 0, int(NUM_X) + 1, int(NUM_Y)}));
    CHECK_THROWS(
        film.writeENVIFile(smdl::Span<const float>(wavelengths, NUM_BANDS),
                           fileName, {}, smdl::int4{1, 0, 1, int(NUM_Y)}));
  }
  SUBCASE("ENVI read rejects a malformed window") {
    auto fileName{(tmpDir / "brokenwindow.envi").string()};
    film.writeENVIFile(smdl::Span<const float>(wavelengths, NUM_BANDS),
                       fileName);
    std::ofstream(fileName + ".hdr", std::ios::app) << "smdl window = {0, 0}\n";
    CHECK_THROWS(smdl::SpectralFilm().readENVIFile(fileName));
  }
  SUBCASE("ENVI read rejects a missing file") {
    CHECK_THROWS(smdl::SpectralFilm().readENVIFile(
        (tmpDir / "nonexistent.envi").string()));
  }
  SUBCASE("ENVI read rejects a malformed header") {
    auto fileName{(tmpDir / "bad.envi").string()};
    std::ofstream(fileName + ".hdr") << "NOT ENVI\n";
    std::ofstream(fileName) << "";
    CHECK_THROWS(smdl::SpectralFilm().readENVIFile(fileName));
  }
  SUBCASE("ENVI read rejects a truncated binary") {
    auto fileName{(tmpDir / "short.envi").string()};
    film.writeENVIFile(smdl::Span<const float>(wavelengths, NUM_BANDS),
                       fileName);
    fs::resize_file(fileName, 8 * (NUM_BANDS * NUM_X * NUM_Y - 1));
    // A read that fails part way through must leave the film cleared
    // rather than holding the rows that made it in.
    auto loadedFilm{smdl::SpectralFilm(NUM_BANDS, NUM_X, NUM_Y)};
    CHECK_THROWS(loadedFilm.readENVIFile(fileName));
    CHECK(loadedFilm.getNumPixelsX() == 0);
    CHECK(loadedFilm.getNumPixelsY() == 0);
    CHECK(loadedFilm.getNumBands() == 0);
    CHECK(loadedFilm.getNumSamples() == 0);
  }
  SUBCASE("No 'smdl spp' reads back with a count of 1") {
    auto fileName{(tmpDir / "foreign.envi").string()};
    film.writeENVIFile(smdl::Span<const float>(wavelengths, NUM_BANDS),
                       fileName);
    // Strip the field to simulate a foreign or legacy header.
    auto headerName{fileName + ".hdr"};
    auto text{std::string()};
    {
      auto file{std::ifstream(headerName)};
      for (std::string line; std::getline(file, line);)
        if (line.rfind("smdl spp", 0) != 0) text += line + "\n";
    }
    std::ofstream(headerName) << text;
    auto loadedFilm{smdl::SpectralFilm{}};
    auto loaded{loadedFilm.readENVIFile(fileName)};
    CHECK(loaded.samplesPerPixel == 0);
    CHECK(loadedFilm.getNumSamples() == 1);
    CHECK(loadedFilm.mean(2, 1, 3) == doctest::Approx(213.0).epsilon(1e-12));
  }
  fs::remove_all(tmpDir);
}
