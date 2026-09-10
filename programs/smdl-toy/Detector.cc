#include <algorithm>
#include <cmath>
#include <utility>

#include "smdl/RenderUtil/MonteCarlo.h"
#include "smdl/Support/Error.h"
#include "smdl/Support/Logger.h"
#include "smdl/Support/Parallel.h"
#include "smdl/Support/Strings.h"

#include "Detector.h"

namespace {

// Below this many electrons the shot noise is drawn as the Poisson it
// is; above, as the Gaussian it is close enough to, which is what lets
// the draw be a deficit. ISETCam's own switch.
constexpr double POISSON_LIMIT{25.0};

// The generic well, electrons per square micrometer of pixel; see
// `DetectorSettings::fullWell`.
constexpr double ELECTRONS_PER_SQUARE_MICROMETER{1000.0};

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
  if (name == "full") return DetectorNoise::FULL;
  throw smdl::Error(smdl::concat("expected -detector-noise to be 'none', "
                                 "'shot', 'all', or 'full', got ",
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
  case DetectorNoise::FULL:
    return "full";
  }
  return "all";
}

Detector::Detector(const DetectorSettings &settings,
                   const DetectorGeometry &geometry)
    : mSettings(settings), mGeometry(geometry) {
  const double pixelAreaUM2{geometry.pixelArea * 1e12};
  mFullWell = settings.fullWell
                  ? double(*settings.fullWell)
                  : ELECTRONS_PER_SQUARE_MICROMETER * pixelAreaUM2;
  mTopCode = uint16_t((uint32_t(1) << settings.bits) - 1);
  mGain = settings.gain
              ? double(*settings.gain)
              : double(mTopCode) / (mFullWell + double(settings.blackLevel));
  mDarkElectrons = double(settings.darkCurrent) * geometry.exposure *
                   std::exp2((double(settings.temperature) -
                              double(settings.referenceTemperature)) /
                             double(settings.doublingTemperature));
  mElectronsPerFilmUnit =
      geometry.pixelArea * geometry.exposure * geometry.irradianceScale;
  const double topCodeElectrons{double(mTopCode) / mGain -
                                double(settings.blackLevel)};
  SMDL_LOG_INFO("Readout: ", smdl::Brief(geometry.pitchUM.x, 4), " by ",
                smdl::Brief(geometry.pitchUM.y, 4), " um pixels, ",
                smdl::Brief(1e3 * geometry.exposure, 4), " ms at f/",
                smdl::Brief(geometry.fNumber, 4), ", ",
                smdl::Brief(mElectronsPerFilmUnit, 4),
                " electrons per unit of film; dark ",
                smdl::Brief(mDarkElectrons, 4), " e-; well ",
                smdl::Brief(mFullWell, 6), " e- ",
                settings.fullWell ? "stated" : "from the pitch", ", ",
                smdl::Brief(mGain, 6), " DN/e- over ", settings.bits,
                " bits, top code at ", smdl::Brief(topCodeElectrons, 6), " e-");
  if (settings.gain && topCodeElectrons < mFullWell)
    SMDL_LOG_WARN("the stated gain puts the top code at ",
                  smdl::Brief(topCodeElectrons, 6), " e-, below the well of ",
                  smdl::Brief(mFullWell, 6),
                  " e-, so the ADC clips before the pixel does");
}

double Detector::electronsOf(double mean, double squared, uint64_t numSamples,
                             DetectorNoise noise, smdl::RNG &rng,
                             double &signal,
                             bool &isNoiseLimited) const noexcept {
  // A pixel some material poisoned reads as black, as the tonemap reads
  // it.
  if (!std::isfinite(mean)) mean = 0.0;
  if (!std::isfinite(squared)) squared = 0.0;
  signal = std::max(0.0, mElectronsPerFilmUnit * mean);
  // The film's own variance of its mean, in electrons squared: what the
  // render already put there, which the shot noise drawn below stops
  // short of.
  const double renderVariance{
      numSamples >= 2
          ? mElectronsPerFilmUnit * mElectronsPerFilmUnit *
                std::max(0.0, squared - mean * mean) / double(numSamples)
          : 0.0};
  const double mu{signal + mDarkElectrons};
  double electrons{mu};
  isNoiseLimited = false;
  isNoiseLimited = mu >= POISSON_LIMIT && !(mu > renderVariance);
  if (noise != DetectorNoise::NONE) {
    auto normals{std::pair<double, double>()};
    bool hasNormals{false};
    // The deficit, or the whole of the shot noise when asked for it.
    const double shotVariance{
        noise == DetectorNoise::FULL ? mu : mu - renderVariance};
    if (mu < POISSON_LIMIT) {
      electrons = poisson(mu, rng);
    } else if (shotVariance > 0) {
      normals = normalPair(rng);
      hasNormals = true;
      electrons = mu + std::sqrt(shotVariance) * normals.first;
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
                          const smdl::SpectralFilm &squares,
                          const DetectorReadoutOptions &options,
                          int4 window) const {
  SMDL_SANITY_CHECK(film.getNumBands() == squares.getNumBands());
  SMDL_SANITY_CHECK(film.getNumPixelsX() == squares.getNumPixelsX());
  SMDL_SANITY_CHECK(film.getNumPixelsY() == squares.getNumPixelsY());
  SMDL_SANITY_CHECK(film.getNumSamples() == squares.getNumSamples());
  const size_t numBands{film.getNumBands()};
  const size_t numPixelsX{film.getNumPixelsX()};
  const size_t numPixelsY{film.getNumPixelsY()};
  const uint64_t numSamples{film.getNumSamples()};
  auto readout{Readout{}};
  readout.bandCount = numBands;
  readout.pixelCountX = numPixelsX;
  readout.pixelCountY = numPixelsY;
  readout.digitalNumbers.resize(numBands * numPixelsX * numPixelsY);
  // Each row tallies for itself and the rows fold in order below, so
  // the totals never depend on which thread took which row.
  struct RowTally final {
    double electrons{};
    uint64_t noiseLimited{};
    uint64_t atWell{};
    uint64_t inWindow{};
  };
  auto rows{std::vector<RowTally>(numPixelsY)};
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
        bool isNoiseLimited{};
        const double electrons{
            electronsOf(film.mean(x, y, b), squares.mean(x, y, b), numSamples,
                        options.noise, rng, signal, isNoiseLimited)};
        const double code{
            std::round((electrons + double(mSettings.blackLevel)) * mGain)};
        readout.digitalNumbers[index] =
            uint16_t(std::clamp(code, 0.0, double(mTopCode)));
        if (isInWindow) {
          tally.inWindow++;
          tally.electrons += signal;
          tally.noiseLimited += isNoiseLimited ? 1 : 0;
          tally.atWell += electrons >= mFullWell ? 1 : 0;
        }
      }
    }
  });
  double electrons{};
  for (const auto &tally : rows) {
    electrons += tally.electrons;
    readout.noiseLimitedCount += tally.noiseLimited;
    readout.wellCount += tally.atWell;
    readout.windowCount += tally.inWindow;
  }
  readout.meanElectrons =
      readout.windowCount > 0 ? electrons / double(readout.windowCount) : 0.0;
  SMDL_LOG_INFO(
      "Readout: mean ", smdl::Brief(readout.meanElectrons, 4),
      " e- over the window, ",
      smdl::Brief(100.0 * readout.noiseLimitedShare(), 3),
      "% of pixel bands render-noise limited, ",
      smdl::Brief(100.0 * double(readout.wellCount) /
                      double(std::max<uint64_t>(readout.windowCount, 1)),
                  3),
      "% at the well");
  return readout;
}

DetectorHeader Detector::header(const Readout &readout,
                                const DetectorReadoutOptions &options) const {
  auto header{DetectorHeader{}};
  header.seed = options.seed;
  header.noise = detectorNoiseName(options.noise);
  header.exposure = mGeometry.exposure;
  header.pixelWidth = mGeometry.pitchUM.x;
  header.pixelHeight = mGeometry.pitchUM.y;
  header.fNumber = mGeometry.fNumber;
  header.fullWell = mFullWell;
  header.readNoise = mSettings.readNoise;
  header.darkElectrons = mDarkElectrons;
  header.gain = mGain;
  header.blackLevel = mSettings.blackLevel;
  header.electronsPerFilmUnit = mElectronsPerFilmUnit;
  header.bits = uint64_t(mSettings.bits);
  header.noiseLimitedShare = readout.noiseLimitedShare();
  return header;
}
