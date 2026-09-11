/// \file
/// The detector readout: what a body reads of the band film, as digital
/// numbers. A post-process over the converged film, so the spectral film
/// and its resume are untouched and a saved film reads out any number of
/// times for any number of noise realizations. Scene units are meters,
/// so the pixel area is in square meters and every electron count
/// follows from it.
#pragma once

#include <cstdint>
#include <string>
#include <vector>

#include "smdl/RenderUtil/SpectralFilm.h"
#include "smdl/Support/RNG.h"

#include "Common.h"
#include "IO/RenderHeader.h"
#include "Layout/SensorFile.h"

/// Which noise the readout draws: none, so the digital numbers are a
/// deterministic function of the film; the shot noise alone; or
/// everything, the read noise included. Each term is drawn in full on
/// the film's mean, see `Detector`.
enum class DetectorNoise { NONE, SHOT, ALL };

/// The '-detector-noise' name as `DetectorNoise` spells it.
///
/// \throws smdl::Error  If the name is not recognized.
///
[[nodiscard]] DetectorNoise parseDetectorNoise(const std::string &name);

/// The name back, for the header.
[[nodiscard]] const char *detectorNoiseName(DetectorNoise noise) noexcept;

/// The readout's flags: about this render rather than the instrument,
/// which the sensor file's `detector` block is.
struct DetectorReadoutOptions final {
  /// The realization: the same seed on the same film draws the same
  /// noise, bit for bit, on any thread count.
  uint64_t seed{};

  DetectorNoise noise{DetectorNoise::ALL};
};

/// What the readout takes from the body, the camera, and the shot.
struct DetectorGeometry final {
  /// The pixel's area in square meters.
  double pixelArea{};

  /// The exposure in seconds: how long each line stays open.
  double exposure{};

  /// The body's degrees Celsius at the exposure, which the dark current
  /// follows.
  double temperature{25.0};

  /// The f-number the irradiance came through, for the log and the
  /// header.
  double fNumber{};

  /// The pixel pitch in micrometers, for the log and the header.
  float2 pitchUM{};
};

/// One readout: the digital numbers of every pixel of the frame, band
/// interleaved by pixel as the file is written, and the tallies the log
/// states, which are over the window alone.
struct Readout final {
  size_t bandCount{};

  size_t pixelCountX{};

  size_t pixelCountY{};

  std::vector<uint16_t> digitalNumbers{};

  /// The mean signal electrons over the window's pixel bands, before
  /// the dark current and the noise.
  double meanElectrons{};

  /// The window's pixel bands that read the well.
  uint64_t wellCount{};

  /// The window's pixel bands.
  uint64_t windowCount{};
};

/// The detector: the chain from the electrons a band counts to the
/// digital number the instrument writes, EMVA 1288 in electrons
/// throughout.
///
/// A pixel band's signal is `A_pixel * t_exp` times the band film's
/// mean, in electrons; the dark current adds `mu_I * t_exp * 2^((T -
/// T_ref) / T_d)`; one draw of shot noise covers the sum, since a sum of
/// Poissons is Poisson; Gaussian read noise adds; the well clips; and
/// `round(e * gain + black)` clips to the top code. The gain sits after
/// the pixel-referred noise and before the ADC, the order that makes ISO
/// invariance a phenomenon, and the black level sits after the gain, in
/// digital numbers, as a camera's does.
///
/// The film's mean is taken as the exact signal: the shot noise is drawn
/// on it in full, a Poisson below 25 electrons and a Gaussian of variance
/// `mu` above, and the electrons stay what they are below zero until the
/// ADC, since the black level is there to keep the read noise's lower
/// half. The render's own noise is not shot noise and adds to it, so the
/// output variance is the mean in electrons plus the render's error, and
/// a readout wants a film whose error is well under the shot noise, 1% at
/// 10,000 electrons. Nothing compensates for a film that is not: a
/// firefly or an unconverged caustic reads as signal. Drawing only the
/// difference against the film's own variance is deliberately not done:
/// the per-pixel estimate assumes independent samples, the Owen-scrambled
/// Sobol sampler beats it by five to twenty-five times, and a draw that
/// trusted it would add a fraction of the shot noise to a converged film.
///
/// The draws are per pixel band from a generator seeded by the flag's
/// seed and the pixel band's index, so a realization is a function of
/// `(seed, x, y, band)` and nothing else: the same on any thread count,
/// and bit for bit across runs.
class Detector final {
public:
  /// Derive the well, the gain, the dark mean, and the electrons per
  /// unit of film, and log the summary. The settings were validated at
  /// parse, so nothing here throws; a stated gain that puts the top code
  /// below the well is a warning, being what pushing the ISO does.
  Detector(const DetectorSettings &settings, const DetectorGeometry &geometry);

  /// The well in electrons, stated or derived from the pitch.
  [[nodiscard]] double fullWell() const noexcept { return mFullWell; }

  /// The gain in digital numbers per electron, stated or derived.
  [[nodiscard]] double gain() const noexcept { return mGain; }

  /// The dark electrons at the exposure and temperature.
  [[nodiscard]] double darkElectrons() const noexcept { return mDarkElectrons; }

  /// The signal electrons one unit of film is worth.
  [[nodiscard]] double electronsPerFilmUnit() const noexcept {
    return mElectronsPerFilmUnit;
  }

  /// The ADC's top code, `2^bits - 1`.
  [[nodiscard]] uint16_t topCode() const noexcept { return mTopCode; }

  /// Read the film out. `window` bounds the tallies, not the readout,
  /// which covers the frame so that the unrendered rest reads as dark. A
  /// film with no samples reads as a dark frame.
  [[nodiscard]] Readout readOut(const smdl::SpectralFilm &film,
                                const DetectorReadoutOptions &options,
                                int4 window) const;

  /// The header of a readout drawn under `options`, see `DetectorHeader`.
  [[nodiscard]] DetectorHeader
  header(const DetectorReadoutOptions &options) const;

private:
  /// One pixel band's electrons at the ADC, clipped to the well, from
  /// the film's `mean`. `signal` is the signal electrons before the dark
  /// current and the noise.
  [[nodiscard]] double electronsOf(double mean, DetectorNoise noise,
                                   smdl::RNG &rng,
                                   double &signal) const noexcept;

  DetectorSettings mSettings{};

  DetectorGeometry mGeometry{};

  double mFullWell{};

  double mGain{};

  double mDarkElectrons{};

  double mElectronsPerFilmUnit{};

  uint16_t mTopCode{};
};
