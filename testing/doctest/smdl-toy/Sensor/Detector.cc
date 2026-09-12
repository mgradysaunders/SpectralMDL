#include "Fixtures.h"

#include <cmath>
#include <utility>
#include <vector>

#include "smdl/RenderUtil/MonteCarlo.h"
#include "smdl/RenderUtil/SpectralFilm.h"
#include "smdl/Support/RNG.h"

#include "Sensor/Detector.h"
#include "Sensor/Sensor.h"

// The readout is a chain from the film's electrons to a digital number,
// with the shot, read, and dark noise of a stated detector drawn onto
// them. What matters is that with no noise it is a function of the film
// alone, that each noise term has the statistics its model states, that
// nothing compensates for a film's own noise, that the black level is a
// digital number added after the gain, and that the tallies are over
// the window. The well and the gain come from the sensor's physics,
// which has its own test; here every case states both.

namespace {

// The 1 ms at f/8 of the sunny-16 arithmetic, at the sensor's base ISO
// unless a case says otherwise.
[[nodiscard]] DetectorShot shot(const Sensor &sensor) {
  DetectorShot value{};
  value.exposure = 1e-3;
  value.fNumber = 8;
  value.iso = sensor.baseISO();
  return value;
}

// A body of 4 um pixels with one flat band, whose detector reads one
// digital number per electron with no dark current, no read noise, and
// no black level, so a digital number is an electron: the base every
// case perturbs.
[[nodiscard]] SensorSettings plain() {
  SensorSettings value{};
  value.pixels = int2(4, 4);
  value.pitchUM = float2(4.0f, 4.0f);
  ResponseBand &band{value.response.bands.emplace_back()};
  band.name = "L";
  band.wavelengths = {400.0f, 700.0f};
  band.values = {1.0f, 1.0f};
  value.detector.fullWell = 60000.0f;
  value.detector.readNoise = 0.0f;
  value.detector.darkCurrent = 0.0f;
  value.detector.blackLevel = 0.0f;
  value.detector.bits = 16;
  value.detector.gain = 1.0f;
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
  const int4 lit{window.value_or(wholeFrame(numX, numY))};
  smdl::SpectralFilm film{numBands, numX, numY};
  smdl::RNG rng{12345};
  std::vector<double> sums(numBands);
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
  DetectorReadoutOptions options{};
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
  SensorSettings settings{plain()};
  settings.detector.darkCurrent = 100.0f;
  settings.detector.blackLevel = 10.0f;
  settings.detector.bits = 12;
  settings.detector.gain = 0.5f;
  const Sensor sensor{settings};
  const Detector detector{sensor, shot(sensor)};
  CHECK(detector.electronsPerFilmUnit() == doctest::Approx(16e-12 * 1e-3));
  CHECK(detector.darkElectrons() == doctest::Approx(0.1));
  CHECK(detector.fullWell() == 60000.0);
  CHECK(detector.gain() == 0.5);
  CHECK(detector.topCode() == 4095);
  CHECK(detector.whiteLevel() == 4095);
  SUBCASE("Two readouts agree bit for bit and equal the hand chain, the "
          "black level in digital numbers after the gain") {
    const smdl::SpectralFilm film{flatFilm(detector, 3, 4, 3, 1000.0)};
    const Readout first{readOut(detector, film, DetectorNoise::NONE)};
    const Readout second{readOut(detector, film, DetectorNoise::NONE, 99)};
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
    const smdl::SpectralFilm film{flatFilm(detector, 1, 2, 2, 100.0)};
    const Readout readout{readOut(detector, film, DetectorNoise::NONE)};
    CHECK(readout.bandCount == 1);
    CHECK(readout.digitalNumbers.size() == 4);
  }
}

TEST_CASE("Detector: two seeds differ and one repeats") {
  SensorSettings settings{plain()};
  settings.detector.readNoise = 2.0f;
  const Sensor sensor{settings};
  const Detector detector{sensor, shot(sensor)};
  const smdl::SpectralFilm film{flatFilm(detector, 1, 32, 32, 1000.0)};
  const Readout one{readOut(detector, film, DetectorNoise::ALL, 1)};
  const Readout oneAgain{readOut(detector, film, DetectorNoise::ALL, 1)};
  const Readout two{readOut(detector, film, DetectorNoise::ALL, 2)};
  CHECK(one.digitalNumbers == oneAgain.digitalNumbers);
  CHECK(one.digitalNumbers != two.digitalNumbers);
}

TEST_CASE("Detector: shot noise alone has the variance of its mean") {
  const Sensor sensor{plain()};
  const Detector detector{sensor, shot(sensor)};
  SUBCASE("In the Gaussian regime") {
    const smdl::SpectralFilm film{flatFilm(detector, 1, 512, 512, 1000.0)};
    const Stats stats{statsOf(readOut(detector, film, DetectorNoise::SHOT))};
    CHECK(stats.mean == doctest::Approx(1000.0).epsilon(0.005));
    CHECK(stats.variance == doctest::Approx(1000.0).epsilon(0.01));
  }
  SUBCASE("In the Poisson regime") {
    const smdl::SpectralFilm film{flatFilm(detector, 1, 512, 512, 8.0)};
    const Stats stats{statsOf(readOut(detector, film, DetectorNoise::SHOT))};
    CHECK(stats.mean == doctest::Approx(8.0).epsilon(0.02));
    CHECK(stats.variance == doctest::Approx(8.0).epsilon(0.02));
  }
}

TEST_CASE("Detector: a film's own noise adds to the shot noise") {
  // The film's mean is the signal, so a render with noise of its own
  // reads out with the shot noise on top of it.
  const Sensor sensor{plain()};
  const Detector detector{sensor, shot(sensor)};
  const smdl::SpectralFilm film{
      flatFilm(detector, 1, 256, 256, 1000.0, 2000.0)};
  const Stats stats{statsOf(readOut(detector, film, DetectorNoise::SHOT))};
  CHECK(stats.variance == doctest::Approx(3000.0).epsilon(0.05));
}

TEST_CASE("Detector: a photon transfer curve recovers the gain and the read "
          "noise") {
  // A pedestal of 50 digital numbers, so the read noise's lower half is
  // not clipped at the ADC, as a real camera's black level keeps it.
  SensorSettings settings{plain()};
  settings.detector.fullWell = 200000.0f;
  settings.detector.gain = 0.25f;
  settings.detector.readNoise = 20.0f;
  settings.detector.blackLevel = 50.0f;
  const Sensor sensor{settings};
  const Detector detector{sensor, shot(sensor)};
  const auto at{[&](double electrons) {
    return statsOf(readOut(detector, flatFilm(detector, 1, 128, 128, electrons),
                           DetectorNoise::ALL));
  }};
  SUBCASE("The slope of variance against mean is the gain") {
    const Stats low{at(4000.0)};
    const Stats high{at(16000.0)};
    const double gain{(high.variance - low.variance) / (high.mean - low.mean)};
    CHECK(gain == doctest::Approx(0.25).epsilon(0.05));
  }
  SUBCASE("The variance of a dark frame is the read noise, through the gain") {
    const Stats dark{at(0.0)};
    CHECK(dark.mean == doctest::Approx(50.0).epsilon(0.01));
    const double readNoise{std::sqrt(dark.variance - 1.0 / 12.0) / 0.25};
    CHECK(readNoise == doctest::Approx(20.0).epsilon(0.05));
  }
  SUBCASE("The standard deviation is flat where the read noise rules and "
          "goes as the square root where the shot noise does") {
    // Against the signal rather than the mean, which the pedestal sits
    // under, as a photon transfer curve is plotted.
    const auto slope{[&](double lowSignal, double highSignal) {
      const Stats low{at(lowSignal)};
      const Stats high{at(highSignal)};
      return std::log(std::sqrt(high.variance) / std::sqrt(low.variance)) /
             std::log(highSignal / lowSignal);
    }};
    CHECK(std::abs(slope(2.0, 8.0)) < 0.05);
    CHECK(slope(8000.0, 16000.0) == doctest::Approx(0.5).epsilon(0.1));
  }
}

TEST_CASE("Detector: dark frames are linear in the exposure and double at "
          "T + T_d") {
  SensorSettings settings{plain()};
  settings.detector.darkCurrent = 1000.0f;
  const Sensor sensor{settings};
  const auto darkFrame{[&](double exposure, double temperature) {
    DetectorShot conditions{shot(sensor)};
    conditions.exposure = exposure;
    conditions.temperature = temperature;
    const Detector detector{sensor, conditions};
    const smdl::SpectralFilm film{flatFilm(detector, 1, 2, 2, 0.0)};
    return readOut(detector, film, DetectorNoise::NONE).digitalNumbers[0];
  }};
  const double reference{settings.detector.referenceTemperature};
  CHECK(darkFrame(0.01, reference) == 10);
  CHECK(darkFrame(0.02, reference) == 20);
  CHECK(darkFrame(0.01, reference + settings.detector.doublingTemperature) ==
        20);
  SUBCASE("A film with no samples reads as a dark frame") {
    DetectorShot conditions{shot(sensor)};
    conditions.exposure = 0.01;
    const Detector detector{sensor, conditions};
    const smdl::SpectralFilm film{1, 2, 2};
    const Readout readout{readOut(detector, film, DetectorNoise::NONE)};
    for (const auto value : readout.digitalNumbers) CHECK(value == 10);
  }
}

TEST_CASE("Detector: the well clips and the ADC has a top code") {
  SUBCASE("A field past the well reads the top code with no variance, the "
          "gain at the base ISO filling the well above the black level") {
    SensorSettings settings{plain()};
    settings.detector.readNoise = 3.0f;
    settings.detector.blackLevel = 535.0f;
    settings.detector.gain = {};
    const Sensor sensor{settings};
    const Detector detector{sensor, shot(sensor)};
    CHECK(detector.gain() == doctest::Approx(65000.0 / 60000.0));
    CHECK(detector.whiteLevel() == 65535);
    const smdl::SpectralFilm film{flatFilm(detector, 1, 32, 32, 120000.0)};
    const Readout readout{readOut(detector, film, DetectorNoise::ALL)};
    for (const auto value : readout.digitalNumbers) CHECK(value == 65535);
    CHECK(statsOf(readout).variance == 0.0);
    CHECK(readout.wellCount == readout.windowCount);
  }
  SUBCASE("A stated gain past the derived one puts the top code below the "
          "well") {
    SensorSettings settings{plain()};
    settings.detector.gain = 2.0f;
    const Sensor sensor{settings};
    const Detector detector{sensor, shot(sensor)};
    CHECK(double(detector.topCode()) / detector.gain() < detector.fullWell());
    CHECK(detector.whiteLevel() == 65535);
    const smdl::SpectralFilm film{flatFilm(detector, 1, 2, 2, 50000.0)};
    const Readout readout{readOut(detector, film, DetectorNoise::NONE)};
    for (const auto value : readout.digitalNumbers) CHECK(value == 65535);
  }
  SUBCASE("A stated gain short of the derived one puts the well below the "
          "top code, which is the white level") {
    SensorSettings settings{plain()};
    settings.detector.gain = 0.5f;
    const Sensor sensor{settings};
    const Detector detector{sensor, shot(sensor)};
    CHECK(detector.whiteLevel() == 30000);
    const smdl::SpectralFilm film{flatFilm(detector, 1, 2, 2, 90000.0)};
    const Readout readout{readOut(detector, film, DetectorNoise::NONE)};
    for (const auto value : readout.digitalNumbers) CHECK(value == 30000);
  }
}

TEST_CASE("Detector: ISO invariance") {
  // Two readouts of one film at two ISOs count the same electrons and
  // differ in the gain alone, so the digital numbers above the black
  // level scale with the ISO until the ADC clips.
  SensorSettings settings{plain()};
  settings.detector.gain = {};
  settings.detector.blackLevel = 1000.0f;
  settings.detector.baseISO = 100.0f;
  settings.detector.fullWell = {};
  const Sensor sensor{settings};
  const auto at{[&](double iso, double electrons) {
    DetectorShot conditions{shot(sensor)};
    conditions.iso = iso;
    const Detector detector{sensor, conditions};
    const smdl::SpectralFilm film{flatFilm(detector, 1, 2, 2, electrons)};
    const Readout readout{readOut(detector, film, DetectorNoise::NONE)};
    return std::pair{readout.meanElectrons, double(readout.digitalNumbers[0])};
  }};
  const auto [lowElectrons, lowCode]{at(100.0, 5000.0)};
  const auto [highElectrons, highCode]{at(400.0, 5000.0)};
  CHECK(lowElectrons == doctest::Approx(5000.0));
  CHECK(highElectrons == doctest::Approx(5000.0));
  CHECK(highCode - 1000.0 ==
        doctest::Approx(4.0 * (lowCode - 1000.0)).epsilon(1e-3));
  SUBCASE("Above the base the ADC clips before the well") {
    DetectorShot conditions{shot(sensor)};
    conditions.iso = 400.0;
    const Detector detector{sensor, conditions};
    CHECK(detector.whiteLevel() == 65535);
    CHECK(sensor.topCodeElectrons(400.0) ==
          doctest::Approx(sensor.fullWell() / 4));
    const auto [electrons, code]{at(400.0, 0.5 * sensor.fullWell())};
    CHECK(code == 65535.0);
  }
}

TEST_CASE("Detector: the tallies are over the window and the rest reads as "
          "dark") {
  SensorSettings settings{plain()};
  settings.detector.darkCurrent = 1000.0f;
  const Sensor sensor{settings};
  const Detector detector{sensor, shot(sensor)};
  const int4 window{0, 0, 2, 2};
  const smdl::SpectralFilm film{
      flatFilm(detector, 2, 4, 4, 1000.0, 0.0, window)};
  DetectorReadoutOptions options{};
  options.noise = DetectorNoise::NONE;
  const Readout readout{detector.readOut(film, options, window)};
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
