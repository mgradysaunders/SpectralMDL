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

// The leaks as the log spells them: one number when every band leaks
// alike, else one per band in band order.
[[nodiscard]] std::string spellLeaks(const std::vector<double> &leaks) {
  const bool isUniform{
      std::all_of(leaks.begin(), leaks.end(),
                  [&](double leak) { return leak == leaks.front(); })};
  std::string text{};
  for (size_t i = 0; i < (isUniform ? size_t(1) : leaks.size()); i++) {
    if (i > 0) text += ", ";
    smdl::Brief(leaks[i], 3).appendTo(text);
  }
  return text;
}

} // namespace

DetectorNoise parseDetectorNoise(const std::string &name) {
  if (name == "none") return DetectorNoise::NONE;
  if (name == "shot") return DetectorNoise::SHOT;
  if (name == "all") return DetectorNoise::ALL;
  throw smdl::Error(smdl::concat("Expected -detector-noise to be 'none', "
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

DetectorCrosstalk detectorCrosstalkOf(const ResponseSettings &settings) {
  DetectorCrosstalk crosstalk{};
  if (!settings.hasCrosstalk()) return crosstalk;
  crosstalk.leak.assign(settings.crosstalk.begin(), settings.crosstalk.end());
  crosstalk.tileColumns = settings.cfaColumns;
  crosstalk.tileRows = settings.cfaRows();
  crosstalk.tile = settings.cfa;
  return crosstalk;
}

Detector::Detector(const Sensor &sensor, const DetectorShot &shot)
    : mSettings(sensor.settings().detector), mShot(shot),
      mCrosstalk(detectorCrosstalkOf(sensor.settings().response)),
      mPitchUM(sensor.settings().pitchUM), mFullWell(sensor.fullWell()),
      mBaseISO(sensor.baseISO()), mGain(sensor.gain(shot.iso)),
      mWellSource(sensor.wellSource()), mHasFixedGain(sensor.hasFixedGain()) {
  const DetectorSettings &settings{mSettings};
  mTopCode = uint16_t(settings.topCode());
  const double black{double(settings.blackLevel)};
  mDarkElectrons =
      double(settings.darkCurrent) * shot.exposure *
      std::exp2((shot.temperature - double(settings.referenceTemperature)) /
                double(settings.doublingTemperature));
  mElectronsPerFilmUnit = sensor.pixelArea() * shot.exposure;
  mWhiteLevel = uint16_t(
      std::min(double(mTopCode), std::round(mFullWell * mGain + black)));
}

void Detector::logSummary() const {
  const DetectorSettings &settings{mSettings};
  const DetectorShot &shot{mShot};
  const double topCodeElectrons{mSettings.codeRange() / mGain};
  const char *wellSource{mWellSource == WellSource::STATED ? "stated"
                         : mWellSource == WellSource::FROM_BASE_ISO
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
      mHasFixedGain        ? " of the stated gain"
      : shot.wasISOMetered ? " metered"
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
    SMDL_LOG_WARN("The well clips at ", mWhiteLevel,
                  " DN, below the top code of ", mTopCode, ", as it does ",
                  mHasFixedGain ? "under the stated gain"
                                : "below the base ISO");
  if (!mCrosstalk.isEmpty()) {
    double meanLeak{};
    for (const double leak : mCrosstalk.leak) meanLeak += leak;
    meanLeak /= double(mCrosstalk.leak.size());
    SMDL_LOG_INFO("Readout: cross-talk ", spellLeaks(mCrosstalk.leak),
                  " per side, so a pixel keeps ",
                  smdl::Brief(1.0 - 4.0 * meanLeak, 4),
                  " of its own charge and a checker of alternating pixels "
                  "reads at ",
                  smdl::Brief(1.0 - 8.0 * meanLeak, 4), " of its contrast");
  }
}

double Detector::electronsOf(double mean, DetectorNoise noise, smdl::RNG &rng,
                             double &signal) const noexcept {
  signal = std::max(0.0, mElectronsPerFilmUnit * mean);
  const double mu{signal + mDarkElectrons};
  double electrons{mu};
  if (noise != DetectorNoise::NONE) {
    std::pair<double, double> normals{};
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

double Detector::collect(const smdl::SpectralFilm &film, size_t x, size_t y,
                         size_t band, int4 window) const noexcept {
  double total{(1.0 - 4.0 * mCrosstalk.leakAt(x, y, band)) *
               filmMean(film, x, y, band)};
  for (const int2 step : {int2(-1, 0), int2(1, 0), int2(0, -1), int2(0, 1)}) {
    const int tapX{int(x) + step.x};
    const int tapY{int(y) + step.y};
    if (tapX < window[0] || tapX >= window[2] || tapY < window[1] ||
        tapY >= window[3])
      continue;
    total += mCrosstalk.leakAt(size_t(tapX), size_t(tapY), band) *
             filmMean(film, size_t(tapX), size_t(tapY), band);
  }
  return total;
}

Readout Detector::readOut(const smdl::SpectralFilm &film,
                          const DetectorReadoutOptions &options,
                          int4 window) const {
  const size_t numBands{film.getNumBands()};
  const size_t numPixelsX{film.getNumPixelsX()};
  const size_t numPixelsY{film.getNumPixelsY()};
  Readout readout{};
  readout.bandCount = numBands;
  readout.pixelCountX = numPixelsX;
  readout.pixelCountY = numPixelsY;
  readout.digitalNumbers.resize(numBands * numPixelsX * numPixelsY);
  struct RowTally final {
    double electrons{};
    uint64_t atWell{};
    uint64_t inWindow{};
  };
  const double black{double(mSettings.blackLevel)};
  const RowTally tally{parallelRowFold(
      size_t(0), numPixelsY, RowTally{},
      [&](size_t y) {
        RowTally tally{};
        const bool isRowInWindow{int(y) >= window[1] && int(y) < window[3]};
        for (size_t x = 0; x < numPixelsX; x++) {
          const bool isInWindow{isRowInWindow && int(x) >= window[0] &&
                                int(x) < window[2]};
          for (size_t b = 0; b < numBands; b++) {
            const size_t index{(y * numPixelsX + x) * numBands + b};
            smdl::RNG rng{smdl::mixBits(options.seed ^
                                        smdl::mixBits(uint64_t(index + 1))),
                          uint64_t(index)};
            // The gather is a pure function of the film and leaves the
            // draws seeded by the pixel band's index exactly as they
            // were, so a realization is still a function of
            // `(seed, x, y, band)`.
            const double mean{mCrosstalk.isEmpty()
                                  ? filmMean(film, x, y, b)
                                  : collect(film, x, y, b, window)};
            double signal{};
            const double electrons{
                electronsOf(mean, options.noise, rng, signal)};
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
        return tally;
      },
      [](RowTally &running, const RowTally &row) {
        running.electrons += row.electrons;
        running.atWell += row.atWell;
        running.inWindow += row.inWindow;
      })};
  readout.wellCount = tally.atWell;
  readout.windowCount = tally.inWindow;
  readout.meanElectrons =
      tally.inWindow > 0 ? tally.electrons / double(tally.inWindow) : 0.0;
  return readout;
}

DetectorHeader Detector::header(const DetectorReadoutOptions &options) const {
  DetectorHeader header{};
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
  header.crosstalk.assign(mCrosstalk.leak.begin(), mCrosstalk.leak.end());
  return header;
}
