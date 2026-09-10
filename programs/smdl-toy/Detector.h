/// \file
/// The detector readout: what a stated instrument reads of the band
/// film, as digital numbers. A post-process over the converged film, so
/// the spectral film and its resume are untouched and a saved film reads
/// out any number of times for any number of noise realizations. Scene
/// units are meters, so the pixel area is in square meters and every
/// electron count follows from it.
#pragma once

#include <cstdint>
#include <string>
#include <vector>

#include "smdl/RenderUtil/SpectralFilm.h"
#include "smdl/Support/RNG.h"

#include "Common.h"
#include "IO/RenderHeader.h"
#include "Layout/CameraFile.h"

/// Which noise the readout draws: none, so the digital numbers are a
/// deterministic function of the film; the shot noise alone; everything,
/// the read noise included, the shot noise as the deficit against the
/// film's own estimate of its variance; or everything with the shot
/// noise in full, for a render the film's estimate cannot tell is
/// converged, see `Detector`.
enum class DetectorNoise { NONE, SHOT, ALL, FULL };

/// The '-detector-noise' name as `DetectorNoise` spells it.
///
/// \throws smdl::Error  If the name is not recognized.
///
[[nodiscard]] DetectorNoise parseDetectorNoise(const std::string &name);

/// The name back, for the header.
[[nodiscard]] const char *detectorNoiseName(DetectorNoise noise) noexcept;

/// The readout's flags: about this render rather than the instrument,
/// which the camera file's `detector` block is.
struct DetectorReadoutOptions final {
  /// The realization: the same seed on the same film draws the same
  /// noise, bit for bit, on any thread count.
  uint64_t seed{};

  DetectorNoise noise{DetectorNoise::ALL};
};

/// What the readout takes from the camera and the shutter.
struct DetectorGeometry final {
  /// The pixel's area in square meters.
  double pixelArea{};

  /// The exposure in seconds: how long each line stays open.
  double exposure{};

  /// What turns one unit of film into spectral irradiance at the sensor
  /// in W/(m^2 nm); see `Camera::irradianceScale()`.
  double irradianceScale{};

  /// The f-number the irradiance came through, for the log and the
  /// header.
  double fNumber{};

  /// The pixel pitch in micrometers, for the log and the header.
  float2 pitchUM{};
};

/// One readout: the digital numbers of every pixel of the frame, band
/// interleaved by pixel as the file is written, and the tallies the log
/// and the header state, which are over the window alone.
struct Readout final {
  size_t bandCount{};

  size_t pixelCountX{};

  size_t pixelCountY{};

  std::vector<uint16_t> digitalNumbers{};

  /// The mean signal electrons over the window's pixel bands, before
  /// the dark current and the noise.
  double meanElectrons{};

  /// The window's pixel bands whose render noise, by the film's own
  /// estimate, exceeded their shot noise, which the deficit draw leaves
  /// alone.
  uint64_t noiseLimitedCount{};

  /// The window's pixel bands that read the well.
  uint64_t wellCount{};

  /// The window's pixel bands.
  uint64_t windowCount{};

  [[nodiscard]] double noiseLimitedShare() const noexcept {
    return windowCount > 0 ? double(noiseLimitedCount) / double(windowCount)
                           : 0.0;
  }
};

/// The detector: the chain from the electrons a `qe` band counts to the
/// digital number the instrument writes, EMVA 1288 in electrons
/// throughout.
///
/// A pixel band's signal is `A_pixel * t_exp * irradianceScale` times
/// the band film's mean, in electrons; the dark current adds `mu_I *
/// t_exp * 2^((T - T_ref) / T_d)`; one draw of shot noise covers the
/// sum, since a sum of Poissons is Poisson; Gaussian read noise adds;
/// the well clips; and `(e + black) * gain` rounds and clips to the top
/// code. The gain sits after the pixel-referred noise and before the
/// ADC, the order that makes ISO invariance a phenomenon.
///
/// The render's own noise is not shot noise, and the two would add, so
/// the readout draws only the deficit: below 25 electrons a Poisson on
/// the mean, where the render's noise is negligible against the shot
/// noise; above it a Gaussian with variance `max(0, mu - sigma_MC^2)`,
/// with `sigma_MC^2` the film's own estimate of its variance from the
/// squares film. Below zero the electrons stay what they are until the
/// ADC, since the black level is there to keep the read noise's lower
/// half. The output variance then equals the mean in electrons
/// wherever the render is converged against the shot noise, and a pixel
/// band whose render noise already exceeds its shot noise is left alone
/// and counted. Three limitations: the estimate assumes independent
/// samples, and the Owen-scrambled Sobol sampler's are not, so it is an
/// upper bound that loosens with spp and the top-up is conservative
/// wherever the two noises are comparable; a firefly is not Gaussian, so
/// the top-up cannot make an unconverged caustic look like shot noise;
/// and a pixel band under 25 electrons gets the Poisson on its noisy
/// mean whatever its render noise, and is not counted. The first is the
/// one that bites: on a sunlit diffuse ground at 256 spp the film's
/// estimate exceeds the shot noise by twenty times while the render's
/// error is a twelfth of it, so the deficit draw adds nothing and the
/// readout is a twelfth as noisy as the sensor. `DetectorNoise::FULL`
/// draws the shot noise in full on the mean instead, which double-counts
/// only the render's actual noise: the honest choice for a render that
/// is converged by any measure but the film's.
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

  /// Read the film out. `squares` holds the mean squared projection with
  /// the same bands and count; `window` bounds the tallies, not the
  /// readout, which covers the frame so that the unrendered rest reads
  /// as dark. A film with no samples reads as a dark frame.
  [[nodiscard]] Readout readOut(const smdl::SpectralFilm &film,
                                const smdl::SpectralFilm &squares,
                                const DetectorReadoutOptions &options,
                                int4 window) const;

  /// The header of a readout, see `DetectorHeader`.
  [[nodiscard]] DetectorHeader
  header(const Readout &readout, const DetectorReadoutOptions &options) const;

private:
  /// One pixel band's electrons at the ADC, clipped to the well, from
  /// the film's `mean` and `squared` over `numSamples`. `signal` is the
  /// signal electrons before the dark current and the noise, and
  /// `isNoiseLimited` says the render's noise exceeded the shot noise.
  [[nodiscard]] double electronsOf(double mean, double squared,
                                   uint64_t numSamples, DetectorNoise noise,
                                   smdl::RNG &rng, double &signal,
                                   bool &isNoiseLimited) const noexcept;

  DetectorSettings mSettings{};

  DetectorGeometry mGeometry{};

  double mFullWell{};

  double mGain{};

  double mDarkElectrons{};

  double mElectronsPerFilmUnit{};

  uint16_t mTopCode{};
};
