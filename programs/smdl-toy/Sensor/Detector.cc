#include <algorithm>
#include <cmath>
#include <utility>

#include "smdl/RenderUtil/MonteCarlo.h"
#include "smdl/Support/Error.h"
#include "smdl/Support/Logger.h"
#include "smdl/Support/Parallel.h"
#include "smdl/Support/Strings.h"

#include "Sensor/Detector.h"

namespace {

// Below this many electrons the shot noise is drawn as the Poisson it
// is; above, as the Gaussian it is close enough to, since Knuth's draw
// below costs a uniform per electron. ISETCam's own switch.
constexpr double POISSON_LIMIT{25.0};

constexpr double TWO_PI_DOUBLE{6.283185307179586476925};

// A uniform double in (0, 1) from one draw, never 0, so that its
// logarithm is finite.
[[nodiscard]] double uniform(smdl::RNG &rng) noexcept {
  return (double(rng.generate()) + 0.5) * 0x1p-32;
}

// Two standard normals from two uniforms, by Box-Muller, in double all
// the way so the tails are what they are.
[[nodiscard]] std::pair<double, double> normalPair(smdl::RNG &rng) noexcept {
  const double r{std::sqrt(-2.0 * std::log(uniform(rng)))};
  const double theta{TWO_PI_DOUBLE * uniform(rng)};
  return {r * std::cos(theta), r * std::sin(theta)};
}

// A Poisson count of mean `mu`, by Knuth's product of uniforms, whose
// cost is `mu + 1` draws: only ever asked for a mean under the limit.
[[nodiscard]] double poisson(double mu, smdl::RNG &rng) noexcept {
  const double limit{std::exp(-mu)};
  double product{1.0};
  uint32_t count{0};
  do {
    count++;
    product *= uniform(rng);
  } while (product > limit);
  return double(count - 1);
}

} // namespace

DetectorNoise parseDetectorNoise(const std::string &name) {
  if (name == "none") return DetectorNoise::NONE;
  if (name == "shot") return DetectorNoise::SHOT;
  if (name == "all") return DetectorNoise::ALL;
  throw smdl::Error(smdl::concat("expected -detector-noise to be 'none', "
                                 "'shot', or 'all', got ",
                                 smdl::Quoted(name)));
}

const char *detectorNoiseName(DetectorNoise noise) noexcept {
  switch (noise) {
  case DetectorNoise::NONE:
    return "none";
  case DetectorNoise::SHOT:
    return "shot";
  case DetectorNoise::ALL:
    return "all";
  }
  return "all";
}

Detector::Detector(const Sensor &sensor, const DetectorShot &shot)
    : mSettings(sensor.settings().detector), mShot(shot),
      mPitchUM(sensor.settings().pitchUM), mFullWell(sensor.fullWell()),
      mBaseISO(sensor.baseISO()), mGain(sensor.gain(shot.iso)) {
  const auto &settings{mSettings};
  mTopCode = uint16_t(settings.topCode());
  const double black{double(settings.blackLevel)};
  mDarkElectrons =
      double(settings.darkCurrent) * shot.exposure *
      std::exp2((shot.temperature - double(settings.referenceTemperature)) /
                double(settings.doublingTemperature));
  mElectronsPerFilmUnit = sensor.pixelArea() * shot.exposure;
  const double topCodeElectrons{(double(mTopCode) - black) / mGain};
  mWhiteLevel = uint16_t(
      std::min(double(mTopCode), std::round(mFullWell * mGain + black)));
  const char *wellSource{sensor.wellSource() == WellSource::STATED ? "stated"
                         : sensor.wellSource() == WellSource::FROM_BASE_ISO
                             ? "from the base ISO"
                             : "from the pitch"};
  SMDL_LOG_INFO(
      "Readout: ", smdl::Brief(mPitchUM.x, 4), " by ",
      smdl::Brief(mPitchUM.y, 4), " um pixels, ",
      smdl::Brief(1e3 * shot.exposure, 4), " ms at f/",
      smdl::Brief(shot.fNumber, 4), ", ", smdl::Brief(mElectronsPerFilmUnit, 4),
      " electrons per unit of film; dark ", smdl::Brief(mDarkElectrons, 4),
      " e- at ", smdl::Brief(shot.temperature, 4), " C; well ",
      smdl::Brief(mFullWell, 6), " e- ", wellSource, "; ISO ",
      smdl::Brief(shot.iso, 6),
      sensor.hasFixedGain() ? " of the stated gain"
      : shot.wasISOMetered  ? " metered"
                            : " stated",
      ", ", smdl::Brief(mGain, 6), " DN/e- over ", settings.bits,
      " bits, black level ", smdl::Brief(settings.blackLevel, 6),
      " DN, white level ", mWhiteLevel, " DN, top code at ",
      smdl::Brief(topCodeElectrons, 6), " e-");
  if (topCodeElectrons < mFullWell)
    SMDL_LOG_INFO("Readout: the top code sits at ",
                  smdl::Brief(topCodeElectrons, 6), " e-, below the well of ",
                  smdl::Brief(mFullWell, 6), " e-, so the ADC clips before ",
                  "the pixel does, as it does above the base ISO of ",
                  smdl::Brief(mBaseISO, 6));
  else if (topCodeElectrons > 1.001 * mFullWell)
    SMDL_LOG_WARN("the well clips at ", mWhiteLevel,
                  " DN, below the top code of ", mTopCode, ", as it does ",
                  sensor.hasFixedGain() ? "under the stated gain"
                                        : "below the base ISO");
}

double Detector::electronsOf(double mean, DetectorNoise noise, smdl::RNG &rng,
                             double &signal) const noexcept {
  // A pixel some material poisoned reads as black, as the tonemap reads
  // it.
  if (!std::isfinite(mean)) mean = 0.0;
  signal = std::max(0.0, mElectronsPerFilmUnit * mean);
  const double mu{signal + mDarkElectrons};
  double electrons{mu};
  if (noise != DetectorNoise::NONE) {
    auto normals{std::pair<double, double>()};
    bool hasNormals{false};
    if (mu < POISSON_LIMIT) {
      electrons = poisson(mu, rng);
    } else {
      normals = normalPair(rng);
      hasNormals = true;
      electrons = mu + std::sqrt(mu) * normals.first;
    }
    if (noise != DetectorNoise::SHOT && mSettings.readNoise > 0) {
      if (!hasNormals) normals = normalPair(rng);
      electrons += double(mSettings.readNoise) * normals.second;
    }
  }
  // The well clips; the ADC clips at zero below, after the black level,
  // which is what the black level is for.
  return std::min(electrons, mFullWell);
}

Readout Detector::readOut(const smdl::SpectralFilm &film,
                          const DetectorReadoutOptions &options,
                          int4 window) const {
  const size_t numBands{film.getNumBands()};
  const size_t numPixelsX{film.getNumPixelsX()};
  const size_t numPixelsY{film.getNumPixelsY()};
  auto readout{Readout{}};
  readout.bandCount = numBands;
  readout.pixelCountX = numPixelsX;
  readout.pixelCountY = numPixelsY;
  readout.digitalNumbers.resize(numBands * numPixelsX * numPixelsY);
  // Each row tallies for itself and the rows fold in order below, so
  // the totals never depend on which thread took which row.
  struct RowTally final {
    double electrons{};
    uint64_t atWell{};
    uint64_t inWindow{};
  };
  auto rows{std::vector<RowTally>(numPixelsY)};
  const double black{double(mSettings.blackLevel)};
  smdl::parallelFor(0, numPixelsY, [&](size_t y) {
    auto &tally{rows[y]};
    const bool isRowInWindow{int(y) >= window[1] && int(y) < window[3]};
    for (size_t x = 0; x < numPixelsX; x++) {
      const bool isInWindow{isRowInWindow && int(x) >= window[0] &&
                            int(x) < window[2]};
      for (size_t b = 0; b < numBands; b++) {
        const size_t index{(y * numPixelsX + x) * numBands + b};
        auto rng{smdl::RNG(
            smdl::mixBits(options.seed ^ smdl::mixBits(uint64_t(index + 1))),
            uint64_t(index))};
        double signal{};
        const double electrons{
            electronsOf(film.mean(x, y, b), options.noise, rng, signal)};
        const double code{std::round(electrons * mGain + black)};
        readout.digitalNumbers[index] =
            uint16_t(std::clamp(code, 0.0, double(mTopCode)));
        if (isInWindow) {
          tally.inWindow++;
          tally.electrons += signal;
          tally.atWell += electrons >= mFullWell ? 1 : 0;
        }
      }
    }
  });
  double electrons{};
  for (const auto &tally : rows) {
    electrons += tally.electrons;
    readout.wellCount += tally.atWell;
    readout.windowCount += tally.inWindow;
  }
  readout.meanElectrons =
      readout.windowCount > 0 ? electrons / double(readout.windowCount) : 0.0;
  SMDL_LOG_INFO(
      "Readout: mean ", smdl::Brief(readout.meanElectrons, 4),
      " e- over the window, ",
      smdl::Brief(100.0 * double(readout.wellCount) /
                      double(std::max<uint64_t>(readout.windowCount, 1)),
                  3),
      "% of pixel bands at the well");
  return readout;
}

DetectorHeader Detector::header(const DetectorReadoutOptions &options) const {
  auto header{DetectorHeader{}};
  header.seed = options.seed;
  header.noise = detectorNoiseName(options.noise);
  header.exposure = mShot.exposure;
  header.pixelWidth = mPitchUM.x;
  header.pixelHeight = mPitchUM.y;
  header.fNumber = mShot.fNumber;
  header.fullWell = mFullWell;
  header.readNoise = mSettings.readNoise;
  header.darkElectrons = mDarkElectrons;
  header.gain = mGain;
  header.blackLevel = mSettings.blackLevel;
  header.electronsPerFilmUnit = mElectronsPerFilmUnit;
  header.bits = uint64_t(mSettings.bits);
  header.iso = mShot.iso;
  header.baseISO = mBaseISO;
  header.wasISOMetered = mShot.wasISOMetered;
  header.whiteLevel = uint64_t(mWhiteLevel);
  return header;
}
