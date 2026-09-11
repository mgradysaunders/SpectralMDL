#include "Fixtures.h"

#include <cmath>
#include <vector>

#include "smdl/RenderUtil/MonteCarlo.h"
#include "smdl/RenderUtil/SpectralFilm.h"
#include "smdl/Support/RNG.h"

#include "Sensor/Detector.h"

// The readout is a chain from the film's electrons to a digital number,
// with the shot, read, and dark noise of a stated detector drawn onto
// them. What matters is that with no noise it is a function of the film
// alone, that each noise term has the statistics its model states, that
// nothing compensates for a film's own noise, that the black level is a
// digital number added after the gain, and that the tallies are over
// the window.

namespace {

// The 4 um pixel at 1 ms of the sunny-16 arithmetic, at f/8 for the log.
[[nodiscard]] DetectorGeometry geometry() {
  auto value{DetectorGeometry{}};
  value.pixelArea = 16e-12;
  value.exposure = 1e-3;
  value.fNumber = 8;
  value.pitchUM = float2(4.0f, 4.0f);
  return value;
}

// A detector that reads one digital number per electron with no dark
// current, no read noise, and no black level, so a digital number is an
// electron: the base every case perturbs.
[[nodiscard]] DetectorSettings plain() {
  auto value{DetectorSettings{}};
  value.fullWell = 60000.0f;
  value.readNoise = 0.0f;
  value.darkCurrent = 0.0f;
  value.blackLevel = 0.0f;
  value.bits = 16;
  value.gain = 1.0f;
  return value;
}

[[nodiscard]] int4 wholeFrame(size_t numX, size_t numY) {
  return int4{0, 0, int(numX), int(numY)};
}

// The band film over a `numX` by `numY` frame, every pixel band inside
// `window` at `electrons` of signal (in film units through the
// detector's own factor), and dark outside it. With `renderVariance`,
// each pixel's mean is scattered by a Gaussian of that variance in
// electrons squared: what a render that noisy would hand the readout.
[[nodiscard]] smdl::SpectralFilm flatFilm(const Detector &detector,
                                          size_t numBands, size_t numX,
                                          size_t numY, double electrons,
                                          double renderVariance = 0.0,
                                          std::optional<int4> window = {}) {
  constexpr uint64_t NUM_SAMPLES{16};
  const double k{detector.electronsPerFilmUnit()};
  const auto lit{window.value_or(wholeFrame(numX, numY))};
  auto film{smdl::SpectralFilm(numBands, numX, numY)};
  auto rng{smdl::RNG(12345)};
  auto sums{std::vector<double>(numBands)};
  for (size_t y = 0; y < numY; y++) {
    for (size_t x = 0; x < numX; x++) {
      const bool isLit{int(x) >= lit[0] && int(x) < lit[2] &&
                       int(y) >= lit[1] && int(y) < lit[3]};
      for (size_t b = 0; b < numBands; b++) {
        double mean{isLit ? electrons / k : 0.0};
        if (isLit && renderVariance > 0)
          mean += std::sqrt(renderVariance) / k *
                  double(smdl::standardNormalSample(rng.generateFloat()));
        sums[b] = mean * double(NUM_SAMPLES);
      }
      film.addTotals(x, y, sums.data());
    }
  }
  film.addSamples(NUM_SAMPLES);
  return film;
}

[[nodiscard]] Readout readOut(const Detector &detector,
                              const smdl::SpectralFilm &film,
                              DetectorNoise noise, uint64_t seed = 0) {
  auto options{DetectorReadoutOptions{}};
  options.seed = seed;
  options.noise = noise;
  return detector.readOut(
      film, options, wholeFrame(film.getNumPixelsX(), film.getNumPixelsY()));
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
  CHECK(detector.electronsPerFilmUnit() == doctest::Approx(16e-12 * 1e-3));
  CHECK(detector.darkElectrons() == doctest::Approx(0.1));
  CHECK(detector.fullWell() == 60000.0);
  CHECK(detector.gain() == 0.5);
  CHECK(detector.topCode() == 4095);
  SUBCASE("Two readouts agree bit for bit and equal the hand chain, the "
          "black level in digital numbers after the gain") {
    const auto film{flatFilm(detector, 3, 4, 3, 1000.0)};
    const auto first{readOut(detector, film, DetectorNoise::NONE)};
    const auto second{readOut(detector, film, DetectorNoise::NONE, 99)};
    CHECK(first.digitalNumbers == second.digitalNumbers);
    CHECK(first.bandCount == 3);
    CHECK(first.pixelCountX == 4);
    CHECK(first.pixelCountY == 3);
    REQUIRE(first.digitalNumbers.size() == 36);
    for (const auto value : first.digitalNumbers)
      CHECK(value == uint16_t(std::round((1000.0 + 0.1) * 0.5 + 10.0)));
    CHECK(first.windowCount == 36);
    CHECK(first.meanElectrons == doctest::Approx(1000.0));
  }
  SUBCASE("A one-band film reads out as one band") {
    const auto film{flatFilm(detector, 1, 2, 2, 100.0)};
    const auto readout{readOut(detector, film, DetectorNoise::NONE)};
    CHECK(readout.bandCount == 1);
    CHECK(readout.digitalNumbers.size() == 4);
  }
}

TEST_CASE("Detector: two seeds differ and one repeats") {
  auto settings{plain()};
  settings.readNoise = 2.0f;
  const Detector detector{settings, geometry()};
  const auto film{flatFilm(detector, 1, 32, 32, 1000.0)};
  const auto one{readOut(detector, film, DetectorNoise::ALL, 1)};
  const auto oneAgain{readOut(detector, film, DetectorNoise::ALL, 1)};
  const auto two{readOut(detector, film, DetectorNoise::ALL, 2)};
  CHECK(one.digitalNumbers == oneAgain.digitalNumbers);
  CHECK(one.digitalNumbers != two.digitalNumbers);
}

TEST_CASE("Detector: shot noise alone has the variance of its mean") {
  const Detector detector{plain(), geometry()};
  SUBCASE("In the Gaussian regime") {
    const auto film{flatFilm(detector, 1, 512, 512, 1000.0)};
    const auto stats{statsOf(readOut(detector, film, DetectorNoise::SHOT))};
    CHECK(stats.mean == doctest::Approx(1000.0).epsilon(0.005));
    CHECK(stats.variance == doctest::Approx(1000.0).epsilon(0.01));
  }
  SUBCASE("In the Poisson regime") {
    const auto film{flatFilm(detector, 1, 512, 512, 8.0)};
    const auto stats{statsOf(readOut(detector, film, DetectorNoise::SHOT))};
    CHECK(stats.mean == doctest::Approx(8.0).epsilon(0.02));
    CHECK(stats.variance == doctest::Approx(8.0).epsilon(0.02));
  }
}

TEST_CASE("Detector: a film's own noise adds to the shot noise") {
  // The film's mean is the signal, so a render with noise of its own
  // reads out with the shot noise on top of it.
  const Detector detector{plain(), geometry()};
  const auto film{flatFilm(detector, 1, 256, 256, 1000.0, 2000.0)};
  const auto stats{statsOf(readOut(detector, film, DetectorNoise::SHOT))};
  CHECK(stats.variance == doctest::Approx(3000.0).epsilon(0.05));
}

TEST_CASE("Detector: a photon transfer curve recovers the gain and the read "
          "noise") {
  // A pedestal of 50 digital numbers, so the read noise's lower half is
  // not clipped at the ADC, as a real camera's black level keeps it.
  auto settings{plain()};
  settings.fullWell = 200000.0f;
  settings.gain = 0.25f;
  settings.readNoise = 20.0f;
  settings.blackLevel = 50.0f;
  const Detector detector{settings, geometry()};
  const auto at{[&](double electrons) {
    return statsOf(readOut(detector, flatFilm(detector, 1, 128, 128, electrons),
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
  const auto darkFrame{[&](double exposure, double temperature) {
    auto shape{geometry()};
    shape.exposure = exposure;
    shape.temperature = temperature;
    const Detector detector{settings, shape};
    const auto film{flatFilm(detector, 1, 2, 2, 0.0)};
    return readOut(detector, film, DetectorNoise::NONE).digitalNumbers[0];
  }};
  const double reference{settings.referenceTemperature};
  CHECK(darkFrame(0.01, reference) == 10);
  CHECK(darkFrame(0.02, reference) == 20);
  CHECK(darkFrame(0.01, reference + settings.doublingTemperature) == 20);
  SUBCASE("A film with no samples reads as a dark frame") {
    auto shape{geometry()};
    shape.exposure = 0.01;
    const Detector detector{settings, shape};
    const auto film{smdl::SpectralFilm(1, 2, 2)};
    const auto readout{readOut(detector, film, DetectorNoise::NONE)};
    for (const auto value : readout.digitalNumbers) CHECK(value == 10);
  }
}

TEST_CASE("Detector: the well clips and the ADC has a top code") {
  SUBCASE("A field past the well reads the top code with no variance, the "
          "gain filling the well above the black level") {
    auto settings{plain()};
    settings.readNoise = 3.0f;
    settings.blackLevel = 535.0f;
    settings.gain = {};
    const Detector detector{settings, geometry()};
    CHECK(detector.gain() == doctest::Approx(65000.0 / 60000.0));
    const auto film{flatFilm(detector, 1, 32, 32, 120000.0)};
    const auto readout{readOut(detector, film, DetectorNoise::ALL)};
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
    const auto film{flatFilm(detector, 1, 2, 2, 50000.0)};
    const auto readout{readOut(detector, film, DetectorNoise::NONE)};
    for (const auto value : readout.digitalNumbers) CHECK(value == 65535);
  }
}

TEST_CASE("Detector: the tallies are over the window and the rest reads as "
          "dark") {
  auto settings{plain()};
  settings.darkCurrent = 1000.0f;
  const Detector detector{settings, geometry()};
  const int4 window{0, 0, 2, 2};
  const auto film{flatFilm(detector, 2, 4, 4, 1000.0, 0.0, window)};
  auto options{DetectorReadoutOptions{}};
  options.noise = DetectorNoise::NONE;
  const auto readout{detector.readOut(film, options, window)};
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
