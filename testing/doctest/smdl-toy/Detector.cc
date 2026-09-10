#include "Fixtures.h"

#include <cmath>
#include <vector>

#include "smdl/RenderUtil/MonteCarlo.h"
#include "smdl/RenderUtil/SpectralFilm.h"
#include "smdl/Support/RNG.h"

#include "Detector.h"

// The readout is a chain from the film's electrons to a digital number,
// with the shot, read, and dark noise of a stated detector drawn onto
// them. What matters is that with no noise it is a function of the film
// alone, that each noise term has the statistics its model states, that
// the deficit draw leaves a flat field the shot noise's variance at any
// spp, and that the tallies are over the window.

namespace {

constexpr double PI_DOUBLE{3.14159265358979323846};

// The 4 um pixel at 1 ms and f/8 of the sunny-16 arithmetic.
[[nodiscard]] DetectorGeometry geometry() {
  auto value{DetectorGeometry{}};
  value.pixelArea = 16e-12;
  value.exposure = 1e-3;
  value.fNumber = 8;
  value.irradianceScale = PI_DOUBLE / (4 * 64);
  value.pitchUM = float2(4.0f, 4.0f);
  return value;
}

// A detector that reads one digital number per electron with no dark
// current and no read noise, so a digital number is an electron: the
// base every case perturbs.
[[nodiscard]] DetectorSettings plain() {
  auto value{DetectorSettings{}};
  value.fullWell = 60000.0f;
  value.readNoise = 0.0f;
  value.darkCurrent = 0.0f;
  value.bits = 16;
  value.gain = 1.0f;
  return value;
}

[[nodiscard]] int4 wholeFrame(size_t numX, size_t numY) {
  return int4{0, 0, int(numX), int(numY)};
}

// The band film and its squares over a `numX` by `numY` frame, every
// pixel band inside `window` at `electrons` of signal (in film units
// through the detector's own factor) over `spp` samples, and dark
// outside it. With `renderVariance`, each pixel's mean is scattered by
// a Gaussian of that variance in electrons squared, which the squares
// film then claims as the film's variance of its mean: what a render
// that noisy would hand the readout.
struct Films final {
  smdl::SpectralFilm film{};
  smdl::SpectralFilm squares{};
};

[[nodiscard]] Films flatFilms(const Detector &detector, size_t numBands,
                              size_t numX, size_t numY, double electrons,
                              uint64_t spp, double renderVariance = 0.0,
                              std::optional<int4> window = {}) {
  const double k{detector.electronsPerFilmUnit()};
  const auto lit{window.value_or(wholeFrame(numX, numY))};
  auto films{Films{}};
  films.film.resize(numBands, numX, numY);
  films.squares.resize(numBands, numX, numY);
  auto rng{smdl::RNG(12345)};
  auto sums{std::vector<double>(numBands)};
  auto squares{std::vector<double>(numBands)};
  for (size_t y = 0; y < numY; y++) {
    for (size_t x = 0; x < numX; x++) {
      const bool isLit{int(x) >= lit[0] && int(x) < lit[2] &&
                       int(y) >= lit[1] && int(y) < lit[3]};
      for (size_t b = 0; b < numBands; b++) {
        double mean{isLit ? electrons / k : 0.0};
        if (isLit && renderVariance > 0)
          mean += std::sqrt(renderVariance) / k *
                  double(smdl::standardNormalSample(rng.generateFloat()));
        // The per-sample variance whose mean over `spp` samples is the
        // render variance asked for.
        const double perSample{renderVariance / (k * k) * double(spp)};
        sums[b] = mean * double(spp);
        squares[b] = (perSample + mean * mean) * double(spp);
      }
      films.film.addTotals(x, y, sums.data());
      films.squares.addTotals(x, y, squares.data());
    }
  }
  films.film.addSamples(spp);
  films.squares.addSamples(spp);
  return films;
}

[[nodiscard]] Readout readOut(const Detector &detector, const Films &films,
                              DetectorNoise noise, uint64_t seed = 0) {
  auto options{DetectorReadoutOptions{}};
  options.seed = seed;
  options.noise = noise;
  return detector.readOut(
      films.film, films.squares, options,
      wholeFrame(films.film.getNumPixelsX(), films.film.getNumPixelsY()));
}

struct Stats final {
  double mean{};
  double variance{};
};

// The mean and variance of one band's digital numbers over the frame.
[[nodiscard]] Stats statsOf(const Readout &readout, size_t band = 0) {
  double sum{};
  double sumOfSquares{};
  const size_t numPixels{readout.pixelCountX * readout.pixelCountY};
  for (size_t p = 0; p < numPixels; p++) {
    const double value{
        double(readout.digitalNumbers[p * readout.bandCount + band])};
    sum += value;
    sumOfSquares += value * value;
  }
  const double mean{sum / double(numPixels)};
  return {mean, sumOfSquares / double(numPixels) - mean * mean};
}

} // namespace

TEST_CASE("Detector: zero noise is a function of the film") {
  auto settings{plain()};
  settings.darkCurrent = 100.0f;
  settings.blackLevel = 10.0f;
  settings.bits = 12;
  settings.gain = 0.5f;
  const Detector detector{settings, geometry()};
  CHECK(detector.electronsPerFilmUnit() ==
        doctest::Approx(16e-12 * 1e-3 * PI_DOUBLE / 256));
  CHECK(detector.darkElectrons() == doctest::Approx(0.1));
  CHECK(detector.fullWell() == 60000.0);
  CHECK(detector.gain() == 0.5);
  CHECK(detector.topCode() == 4095);
  SUBCASE("Two readouts agree bit for bit and equal the hand chain") {
    const auto films{flatFilms(detector, 3, 4, 3, 1000.0, 16)};
    const auto first{readOut(detector, films, DetectorNoise::NONE)};
    const auto second{readOut(detector, films, DetectorNoise::NONE, 99)};
    CHECK(first.digitalNumbers == second.digitalNumbers);
    CHECK(first.bandCount == 3);
    CHECK(first.pixelCountX == 4);
    CHECK(first.pixelCountY == 3);
    REQUIRE(first.digitalNumbers.size() == 36);
    for (const auto value : first.digitalNumbers)
      CHECK(value == uint16_t(std::round((1000.0 + 0.1 + 10.0) * 0.5)));
    CHECK(first.windowCount == 36);
    CHECK(first.noiseLimitedCount == 0);
    CHECK(first.meanElectrons == doctest::Approx(1000.0));
  }
  SUBCASE("A one-band film reads out as one band") {
    const auto films{flatFilms(detector, 1, 2, 2, 100.0, 4)};
    const auto readout{readOut(detector, films, DetectorNoise::NONE)};
    CHECK(readout.bandCount == 1);
    CHECK(readout.digitalNumbers.size() == 4);
  }
}

TEST_CASE("Detector: two seeds differ and one repeats") {
  auto settings{plain()};
  settings.readNoise = 2.0f;
  const Detector detector{settings, geometry()};
  const auto films{flatFilms(detector, 1, 32, 32, 1000.0, 16)};
  const auto one{readOut(detector, films, DetectorNoise::ALL, 1)};
  const auto oneAgain{readOut(detector, films, DetectorNoise::ALL, 1)};
  const auto two{readOut(detector, films, DetectorNoise::ALL, 2)};
  CHECK(one.digitalNumbers == oneAgain.digitalNumbers);
  CHECK(one.digitalNumbers != two.digitalNumbers);
}

TEST_CASE("Detector: shot noise alone has the variance of its mean") {
  const Detector detector{plain(), geometry()};
  SUBCASE("In the Gaussian regime") {
    const auto films{flatFilms(detector, 1, 512, 512, 1000.0, 16)};
    const auto stats{statsOf(readOut(detector, films, DetectorNoise::SHOT))};
    CHECK(stats.mean == doctest::Approx(1000.0).epsilon(0.005));
    CHECK(stats.variance == doctest::Approx(1000.0).epsilon(0.01));
  }
  SUBCASE("In the Poisson regime") {
    const auto films{flatFilms(detector, 1, 512, 512, 8.0, 16)};
    const auto stats{statsOf(readOut(detector, films, DetectorNoise::SHOT))};
    CHECK(stats.mean == doctest::Approx(8.0).epsilon(0.02));
    CHECK(stats.variance == doctest::Approx(8.0).epsilon(0.02));
  }
}

TEST_CASE("Detector: a photon transfer curve recovers the gain and the read "
          "noise") {
  // A pedestal of 200 electrons, so the read noise's lower half is not
  // clipped at the ADC, as a real camera's black level keeps it.
  auto settings{plain()};
  settings.fullWell = 200000.0f;
  settings.gain = 0.25f;
  settings.readNoise = 20.0f;
  settings.blackLevel = 200.0f;
  const Detector detector{settings, geometry()};
  const auto at{[&](double electrons) {
    return statsOf(readOut(detector,
                           flatFilms(detector, 1, 128, 128, electrons, 16),
                           DetectorNoise::ALL));
  }};
  SUBCASE("The slope of variance against mean is the gain") {
    const auto low{at(4000.0)};
    const auto high{at(16000.0)};
    const double gain{(high.variance - low.variance) / (high.mean - low.mean)};
    CHECK(gain == doctest::Approx(0.25).epsilon(0.05));
  }
  SUBCASE("The variance of a dark frame is the read noise, through the gain") {
    const auto dark{at(0.0)};
    CHECK(dark.mean == doctest::Approx(50.0).epsilon(0.01));
    const double readNoise{std::sqrt(dark.variance - 1.0 / 12.0) / 0.25};
    CHECK(readNoise == doctest::Approx(20.0).epsilon(0.05));
  }
  SUBCASE("The standard deviation is flat where the read noise rules and "
          "goes as the square root where the shot noise does") {
    // Against the signal rather than the mean, which the pedestal sits
    // under, as a photon transfer curve is plotted.
    const auto slope{[&](double lowSignal, double highSignal) {
      const auto low{at(lowSignal)};
      const auto high{at(highSignal)};
      return std::log(std::sqrt(high.variance) / std::sqrt(low.variance)) /
             std::log(highSignal / lowSignal);
    }};
    CHECK(std::abs(slope(2.0, 8.0)) < 0.05);
    CHECK(slope(8000.0, 16000.0) == doctest::Approx(0.5).epsilon(0.1));
  }
}

TEST_CASE("Detector: dark frames are linear in the exposure and double at "
          "T + T_d") {
  auto settings{plain()};
  settings.darkCurrent = 1000.0f;
  const auto darkFrame{[&](const DetectorSettings &at, double exposure) {
    auto shape{geometry()};
    shape.exposure = exposure;
    const Detector detector{at, shape};
    const auto films{flatFilms(detector, 1, 2, 2, 0.0, 4)};
    return readOut(detector, films, DetectorNoise::NONE).digitalNumbers[0];
  }};
  CHECK(darkFrame(settings, 0.01) == 10);
  CHECK(darkFrame(settings, 0.02) == 20);
  auto warmer{settings};
  warmer.temperature =
      settings.referenceTemperature + settings.doublingTemperature;
  CHECK(darkFrame(warmer, 0.01) == 20);
  SUBCASE("A film with no samples reads as a dark frame") {
    auto shape{geometry()};
    shape.exposure = 0.01;
    const Detector detector{settings, shape};
    auto films{Films{}};
    films.film.resize(1, 2, 2);
    films.squares.resize(1, 2, 2);
    const auto readout{readOut(detector, films, DetectorNoise::NONE)};
    for (const auto value : readout.digitalNumbers) CHECK(value == 10);
  }
}

TEST_CASE("Detector: the well clips and the ADC has a top code") {
  SUBCASE("A field past the well reads the top code with no variance, the "
          "gain filling the well") {
    auto settings{plain()};
    settings.readNoise = 3.0f;
    settings.gain = {};
    const Detector detector{settings, geometry()};
    CHECK(detector.gain() == doctest::Approx(65535.0 / 60000.0));
    const auto films{flatFilms(detector, 1, 32, 32, 120000.0, 16)};
    const auto readout{readOut(detector, films, DetectorNoise::ALL)};
    for (const auto value : readout.digitalNumbers) CHECK(value == 65535);
    CHECK(statsOf(readout).variance == 0.0);
    CHECK(readout.wellCount == readout.windowCount);
  }
  SUBCASE("A stated gain past the derived one puts the top code below the "
          "well") {
    auto settings{plain()};
    settings.gain = 2.0f;
    const Detector detector{settings, geometry()};
    CHECK(double(detector.topCode()) / detector.gain() < detector.fullWell());
    const auto films{flatFilms(detector, 1, 2, 2, 50000.0, 16)};
    const auto readout{readOut(detector, films, DetectorNoise::NONE)};
    for (const auto value : readout.digitalNumbers) CHECK(value == 65535);
  }
}

TEST_CASE("Detector: the deficit draw leaves a flat field the shot noise's "
          "variance at any spp") {
  const Detector detector{plain(), geometry()};
  SUBCASE("A render a quarter as noisy as the shot noise is topped up to "
          "it, at 16 spp and at 1024") {
    for (const uint64_t spp : {uint64_t(16), uint64_t(1024)}) {
      CAPTURE(spp);
      const auto films{flatFilms(detector, 1, 256, 256, 1000.0, spp, 250.0)};
      const auto readout{readOut(detector, films, DetectorNoise::SHOT)};
      const auto stats{statsOf(readout)};
      CHECK(stats.variance == doctest::Approx(1000.0).epsilon(0.03));
      CHECK(readout.noiseLimitedCount == 0);
    }
  }
  SUBCASE("A render noisier than the shot noise is left alone and counted") {
    const auto films{flatFilms(detector, 1, 256, 256, 1000.0, 16, 2000.0)};
    const auto readout{readOut(detector, films, DetectorNoise::SHOT)};
    const auto stats{statsOf(readout)};
    CHECK(stats.variance == doctest::Approx(2000.0).epsilon(0.05));
    CHECK(readout.noiseLimitedShare() == 1.0);
  }
  SUBCASE("Asked for the shot noise in full, the readout adds it whatever "
          "the film claims, and still counts") {
    const auto films{flatFilms(detector, 1, 256, 256, 1000.0, 16, 2000.0)};
    const auto readout{readOut(detector, films, DetectorNoise::FULL)};
    const auto stats{statsOf(readout)};
    CHECK(stats.variance == doctest::Approx(3000.0).epsilon(0.05));
    CHECK(readout.noiseLimitedShare() == 1.0);
  }
}

TEST_CASE("Detector: the tallies are over the window and the rest reads as "
          "dark") {
  auto settings{plain()};
  settings.darkCurrent = 1000.0f;
  const Detector detector{settings, geometry()};
  const int4 window{0, 0, 2, 2};
  const auto films{flatFilms(detector, 2, 4, 4, 1000.0, 16, 0.0, window)};
  auto options{DetectorReadoutOptions{}};
  options.noise = DetectorNoise::NONE;
  const auto readout{
      detector.readOut(films.film, films.squares, options, window)};
  CHECK(readout.windowCount == 8);
  CHECK(readout.meanElectrons == doctest::Approx(1000.0));
  for (size_t y = 0; y < 4; y++) {
    for (size_t x = 0; x < 4; x++) {
      const bool isInWindow{x < 2 && y < 2};
      for (size_t b = 0; b < 2; b++) {
        CAPTURE(x);
        CAPTURE(y);
        CHECK(readout.digitalNumbers[(y * 4 + x) * 2 + b] ==
              (isInWindow ? 1001 : 1));
      }
    }
  }
}
