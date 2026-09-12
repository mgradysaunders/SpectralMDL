/// \file
/// Optical glass: the index of refraction of a glass across wavelength, by
/// the dispersion models glass catalogs and lens prescriptions state it
/// with, and a small catalog of common glasses by their makers'
/// coefficients.
#pragma once

#include <algorithm>
#include <array>
#include <cmath>
#include <optional>
#include <string_view>

#include "smdl/Export.h"
#include "smdl/Support/Span.h"

namespace smdl {

/// \addtogroup renderutil
/// \{

/// The spectral lines a glass's dispersion is stated at, in nanometers: the
/// helium d line, the hydrogen F and C lines, and the mercury g line. A
/// catalog's `nd` is the index at d, its Abbe number `Vd` is
/// `(nd - 1) / (nF - nC)`, and its partial dispersion `PgF` is
/// `(ng - nF) / (nF - nC)`.
///
/// \{
inline constexpr float FRAUNHOFER_D_LINE = 587.5618f;
inline constexpr float FRAUNHOFER_F_LINE = 486.1327f;
inline constexpr float FRAUNHOFER_C_LINE = 656.2725f;
inline constexpr float FRAUNHOFER_G_LINE = 435.8343f;
/// \}

/// The domain, in nanometers: what every glass is validated over, and what
/// `OpticalGlass::indexAt()` holds its argument within. It is the 300 to
/// 2500 nm glass makers fit their coefficients over. A dense flint's fit
/// starts where the glass stops transmitting, as late as 380 nm, so the
/// short end extrapolates it.
///
/// \{
inline constexpr float OPTICAL_GLASS_WAVELENGTH_MIN = 300.0f;
inline constexpr float OPTICAL_GLASS_WAVELENGTH_MAX = 2500.0f;
/// \}

/// The index of refraction of a glass, relative to air, across wavelength.
///
/// A glass is stated in one of three ways, which is its `Kind`:
/// - by one index, which is how a prescription without dispersion data
///   states a glass, and how air is stated;
/// - by `nd` and `Vd`, and `PgF` where it is known, which is what a
///   patent's table prints;
/// - by the maker's Sellmeier coefficients, which is what a catalog prints.
///
/// Every factory validates what it builds. Every glass therefore has an
/// index that is finite, at least 1, and never rises with wavelength
/// anywhere in the domain. That is normal dispersion, the only kind a glass
/// has where it transmits.
class SMDL_EXPORT OpticalGlass final {
public:
  /// How a glass was stated.
  enum class Kind {
    /// One index at every wavelength.
    CONSTANT,

    /// The three-term Cauchy `n = A + B / l^2 + C / l^4`, with `l` in
    /// micrometers, fitted through `nd`, `Vd`, and `PgF`. See `abbe()`.
    ABBE,

    /// The three-term Sellmeier `n^2 = 1 + sum_i B_i l^2 / (l^2 - C_i)`,
    /// with `l` in micrometers and `C_i` in square micrometers. See
    /// `sellmeier()`.
    SELLMEIER
  };

  /// Air: exactly 1 at every wavelength.
  OpticalGlass() = default;

  /// A glass that does not disperse: `index` at every wavelength.
  ///
  /// \throws Error  If `index` is not a finite number of at least 1.
  ///
  [[nodiscard]] static OpticalGlass constant(float index);

  /// A glass by its `nd`, its Abbe number `Vd`, and its partial dispersion
  /// `PgF` if known.
  ///
  /// The fit is the three-term Cauchy through three conditions: the index
  /// at d, `nF - nC = (nd - 1) / Vd`, and `ng - nF = PgF (nF - nC)`.
  /// Without a `PgF` it takes Schott's normal line,
  /// `PgF = 0.6438 - 0.001682 Vd`. The ordinary glasses lie near that line;
  /// the anomalous ones an apochromat is made of do not.
  ///
  /// The third term is what keeps the partial dispersion. A two-term Cauchy
  /// through `nd` and `Vd` alone gives every glass the same one, so an
  /// achromat designed on it would have no secondary spectrum at all.
  ///
  /// The fit is within about 3e-4 of the maker's Sellmeier from 450 to 700
  /// nm, and less close past both ends, so a known glass is better stated
  /// by `sellmeier()`.
  ///
  /// \throws Error  If `nd` is not greater than 1, `Vd` is not positive,
  ///                `PgF` is not between 0 and 1, or the fit's index rises
  ///                with wavelength somewhere in the domain. A partial
  ///                dispersion far off the normal line does that.
  ///
  [[nodiscard]] static OpticalGlass
  abbe(float nd, float abbeNumber,
       std::optional<float> partialDispersion = std::nullopt);

  /// A glass by its three-term Sellmeier coefficients `b` and `c`, as a
  /// maker's datasheet prints them, with `c` in square micrometers.
  ///
  /// \throws Error  If a coefficient is not finite, a term has its pole in
  ///                the domain, the index falls below 1 or rises with
  ///                wavelength anywhere in it, or the glass does not
  ///                disperse, which `constant()` states.
  ///
  [[nodiscard]] static OpticalGlass sellmeier(const std::array<float, 3> &b,
                                              const std::array<float, 3> &c);

  /// How the glass was stated.
  [[nodiscard]] Kind kind() const noexcept { return mKind; }

  /// Does the index vary with wavelength? Every glass but a constant one
  /// does, since the factories refuse one that is stated with dispersion
  /// and has none.
  [[nodiscard]] bool isDispersive() const noexcept {
    return mKind != Kind::CONSTANT;
  }

  /// The index at `wavelength` nanometers.
  ///
  /// A wavelength outside the domain reads as the nearest end of it, as a
  /// metal's table clamps (`smdlEvalMetalIOR()`), and one that is not a
  /// number reads as the short end.
  [[nodiscard]] float indexAt(float wavelength) const noexcept {
    return evaluate(wavelength > OPTICAL_GLASS_WAVELENGTH_MIN
                        ? std::min(wavelength, OPTICAL_GLASS_WAVELENGTH_MAX)
                        : OPTICAL_GLASS_WAVELENGTH_MIN);
  }

  /// The index at the d line, `nd`.
  [[nodiscard]] float nd() const noexcept;

  /// The Abbe number `Vd`, infinite for a glass that does not disperse.
  [[nodiscard]] float abbeNumber() const noexcept;

  /// The partial dispersion `PgF`, 0 for a glass that does not disperse.
  [[nodiscard]] float partialDispersion() const noexcept;

private:
  /// The index at `wavelength` nanometers, which the caller holds within
  /// the domain.
  ///
  /// It is a template so that the validation can run it in double, on the
  /// very coefficients the float evaluation uses: at the long end of the
  /// domain, indices a nanometer apart differ by only a few units in the
  /// last place of a float.
  template <typename T> [[nodiscard]] T evaluate(T wavelength) const noexcept {
    const T l{T(1e-3) * wavelength};
    const T l2{l * l};
    const std::array<float, 6> &k{mCoefficients};
    switch (mKind) {
    case Kind::CONSTANT:
      break;
    case Kind::ABBE: {
      const T x{1 / l2};
      return T(k[0]) + x * (T(k[1]) + x * T(k[2]));
    }
    case Kind::SELLMEIER:
      return std::sqrt(1 + T(k[0]) * l2 / (l2 - T(k[3])) +
                       T(k[1]) * l2 / (l2 - T(k[4])) +
                       T(k[2]) * l2 / (l2 - T(k[5])));
    }
    return T(k[0]);
  }

  /// Refuse a glass whose index is not finite, falls below 1, or rises
  /// with wavelength anywhere in the domain.
  ///
  /// \throws Error  If it does.
  ///
  void validate() const;

  Kind mKind{Kind::CONSTANT};

  /// The index, for a constant glass; `A`, `B`, and `C`, for an Abbe fit;
  /// `B_1` to `B_3` and then `C_1` to `C_3`, for a Sellmeier.
  std::array<float, 6> mCoefficients{1, 0, 0, 0, 0, 0};
};

/// One glass of the built-in catalog.
struct OpticalGlassEntry final {
  /// The maker's designation, as the maker spells it.
  std::string_view name{};

  /// The glass, by the maker's Sellmeier coefficients.
  OpticalGlass glass{};
};

/// The built-in catalog, in order of falling Abbe number. It holds 21
/// SCHOTT glasses, from the crowns to the densest flints, including the
/// fluor and phosphate crowns an apochromat pairs with them, plus fused
/// silica and calcium fluoride by Malitson's fits.
///
/// It is built on first use, through `OpticalGlass::sellmeier()`, so every
/// entry is validated as any other glass is.
[[nodiscard]] SMDL_EXPORT Span<const OpticalGlassEntry> opticalGlassCatalog();

/// The catalog entry named `name`, or null.
///
/// The match ignores ASCII case, since catalogs, patents, and design files
/// spell a designation inconsistently. It also takes an alias where the
/// maker's replacement prints the same `nd` and `Vd` as the glass it
/// replaced: `BK7` for N-BK7. `SILICA` and `FLUORITE` are aliases for the
/// two crystals.
[[nodiscard]] SMDL_EXPORT const OpticalGlassEntry *
findOpticalGlass(std::string_view name);

/// The entry points the builtin `models::glass_ior` module reaches through
/// `@(foreign)`, which resolves them out of the host process by name.
///
/// Neither throws, since neither may unwind into JIT'd code, and both
/// return an index that is finite and at least 1 whatever they are given,
/// as `smdlEvalMetalIOR()` zeroes on a metal it does not know.
///
/// \{
extern "C" {

/// The index of refraction of the catalog glass at index `glass` at
/// `wavelength` nanometers, where the index is into `opticalGlassCatalog()`
/// and the wavelength is held within the domain as `OpticalGlass::indexAt()`
/// holds it. An index outside the catalog reads as 1.
[[nodiscard]] SMDL_EXPORT float smdlEvalOpticalGlassIOR(int glass,
                                                        float wavelength);

/// The index of refraction at `wavelength` nanometers of the three-term
/// Cauchy fitted through `nd`, the Abbe number `abbeNumber`, and the partial
/// dispersion `partialDispersion`, which takes Schott's normal line unless
/// it lies between 0 and 1. See `OpticalGlass::abbe()`, whose fit this is;
/// this skips the walk of the domain that validates it, and clamps the
/// index to at least 1 instead. An `nd` that is not greater than 1 or an
/// Abbe number that is not positive reads as 1.
[[nodiscard]] SMDL_EXPORT float smdlEvalAbbeIOR(float nd, float abbeNumber,
                                                float partialDispersion,
                                                float wavelength);
}
/// \}

/// \}

} // namespace smdl
