#include "smdl/RenderUtil/OpticalGlass.h"

#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>

#include "smdl/Support/Error.h"
#include "smdl/Support/Strings.h"
#include "smdl/Support/VectorMath.h"

namespace smdl {

namespace {

// Schott's normal line: the partial dispersion the ordinary glasses lie
// near, as a function of the Abbe number. SCHOTT Technical Information
// TIE-29, "Refractive Index and Dispersion" (2016).
constexpr double NORMAL_LINE_INTERCEPT = 0.6438;
constexpr double NORMAL_LINE_SLOPE = -0.001682;

// The inverse square of a wavelength given in nanometers, taken in
// micrometers, which is the variable the Cauchy form is a polynomial in.
[[nodiscard]] double inverseSquareMicrometers(double wavelength) noexcept {
  const double l{1e-3 * wavelength};
  return 1 / (l * l);
}

// The ends of the domain squared, in square micrometers: where a Sellmeier
// term's C puts its pole if the pole is inside.
constexpr double DOMAIN_MIN_SQUARED{1e-6 *
                                    double(OPTICAL_GLASS_WAVELENGTH_MIN) *
                                    double(OPTICAL_GLASS_WAVELENGTH_MIN)};
constexpr double DOMAIN_MAX_SQUARED{1e-6 *
                                    double(OPTICAL_GLASS_WAVELENGTH_MAX) *
                                    double(OPTICAL_GLASS_WAVELENGTH_MAX)};

// A glass of the catalog as its source prints it.
struct CatalogData final {
  std::string_view name{};
  std::array<float, 3> b{};
  std::array<float, 3> c{};
};

// Malitson prints each C as a wavelength in micrometers, where a datasheet
// prints its square.
[[nodiscard]] constexpr float squared(double wavelength) noexcept {
  return float(wavelength * wavelength);
}

// The SCHOTT entries are the Sellmeier coefficients of the SCHOTT Zemax
// catalog 2017-01-20b, and the crystals are Malitson's fits, all as the
// refractiveindex.info database (public domain, CC0 1.0) carries them:
//
//   https://github.com/polyanskiy/refractiveindex.info-database
//
// SCHOTT fits each glass from where it starts to transmit in the
// ultraviolet, 0.29 to 0.38 um, to 2.5 um (LF5 to 2.325 um).
//
// I. H. Malitson, "Interspecimen comparison of the refractive index of
// fused silica", J. Opt. Soc. Am. 55, 1205-1208 (1965), fitted from 0.21
// to 3.71 um and verified to 6.7 um by C. Z. Tan, J. Non-Cryst. Solids
// 223, 158-163 (1998).
//
// I. H. Malitson, "A redetermination of some optical properties of calcium
// fluoride", Appl. Opt. 2, 1103-1107 (1963), fitted from 0.23 to 9.7 um,
// at 24 C.
//
// etc/scripts/lens_convert.py keeps a copy of these names and of the
// aliases below, so that a converted lens names a built-in glass rather
// than defining it again. A name added here goes there too.
constexpr CatalogData CATALOG[] = {
    {"CAF2",
     {0.5675888f, 0.4710914f, 3.8484723f},
     {squared(0.050263605), squared(0.1003909), squared(34.649040)}},
    {"N-FK51A",
     {0.971247817f, 0.216901417f, 0.904651666f},
     {0.00472301995f, 0.0153575612f, 168.68133f}},
    {"N-PK52A",
     {1.029607f, 0.1880506f, 0.736488165f},
     {0.00516800155f, 0.0166658798f, 138.964129f}},
    {"FUSED-SILICA",
     {0.6961663f, 0.4079426f, 0.8974794f},
     {squared(0.0684043), squared(0.1162414), squared(9.896161)}},
    {"N-BK7",
     {1.03961212f, 0.231792344f, 1.01046945f},
     {0.00600069867f, 0.0200179144f, 103.560653f}},
    {"N-SK16",
     {1.34317774f, 0.241144399f, 0.994317969f},
     {0.00704687339f, 0.0229005f, 92.7508526f}},
    {"N-K5",
     {1.08511833f, 0.199562005f, 0.930511663f},
     {0.00661099503f, 0.024110866f, 111.982777f}},
    {"N-BAK1",
     {1.12365662f, 0.309276848f, 0.881511957f},
     {0.00644742752f, 0.0222284402f, 107.297751f}},
    {"N-BAK4",
     {1.28834642f, 0.132817724f, 0.945395373f},
     {0.00779980626f, 0.0315631177f, 105.965875f}},
    {"N-LAK9",
     {1.46231905f, 0.344399589f, 1.15508372f},
     {0.00724270156f, 0.0243353131f, 85.4686868f}},
    {"N-BAF10",
     {1.5851495f, 0.143559385f, 1.08521269f},
     {0.00926681282f, 0.0424489805f, 105.613573f}},
    {"N-KZFS4",
     {1.35055424f, 0.197575506f, 1.09962992f},
     {0.0087628207f, 0.0371767201f, 90.3866994f}},
    {"LF5",
     {1.28035628f, 0.163505973f, 0.893930112f},
     {0.00929854416f, 0.0449135769f, 110.493685f}},
    {"N-F2",
     {1.39757037f, 0.159201403f, 1.2686543f},
     {0.00995906143f, 0.0546931752f, 119.248346f}},
    {"F2",
     {1.34533359f, 0.209073176f, 0.937357162f},
     {0.00997743871f, 0.0470450767f, 111.886764f}},
    {"SF2",
     {1.40301821f, 0.231767504f, 0.939056586f},
     {0.0105795466f, 0.0493226978f, 112.405955f}},
    {"N-SF2",
     {1.47343127f, 0.163681849f, 1.36920899f},
     {0.0109019098f, 0.0585683687f, 127.404933f}},
    {"N-SF5",
     {1.52481889f, 0.187085527f, 1.42729015f},
     {0.011254756f, 0.0588995392f, 129.141675f}},
    {"N-LASF9",
     {2.00029547f, 0.298926886f, 1.80691843f},
     {0.0121426017f, 0.0538736236f, 156.530829f}},
    {"N-SF10",
     {1.62153902f, 0.256287842f, 1.64447552f},
     {0.0122241457f, 0.0595736775f, 147.468793f}},
    {"N-SF11",
     {1.73759695f, 0.313747346f, 1.89878101f},
     {0.013188707f, 0.0623068142f, 155.23629f}},
    {"N-SF6",
     {1.77931763f, 0.338149866f, 2.08734474f},
     {0.0133714182f, 0.0617533621f, 174.01759f}},
    {"N-SF57",
     {1.87543831f, 0.37375749f, 2.30001797f},
     {0.0141749518f, 0.0640509927f, 177.389795f}}};

// The other names a glass goes by, each where the glass it names prints the
// same nd and Vd. A lead glass is never an alias of its N- replacement:
// SF2 and N-SF2 print the same nd and Vd to the second decimal, and their
// partial dispersions differ by 0.006, which is the secondary spectrum an
// old design was corrected against.
struct CatalogAlias final {
  std::string_view alias{};
  std::string_view name{};
};

constexpr CatalogAlias ALIASES[] = {
    {"BK7", "N-BK7"}, {"SILICA", "FUSED-SILICA"}, {"FLUORITE", "CAF2"}};

// Do two names match, ignoring ASCII case?
[[nodiscard]] bool isSameName(std::string_view a, std::string_view b) noexcept {
  const auto lower{[](char ch) {
    return 'A' <= ch && ch <= 'Z' ? char(ch - 'A' + 'a') : ch;
  }};
  return a.size() == b.size() &&
         std::equal(a.begin(), a.end(), b.begin(),
                    [&](char x, char y) { return lower(x) == lower(y); });
}

} // namespace

OpticalGlass OpticalGlass::constant(float index) {
  if (!(std::isfinite(index) && index >= 1))
    throw Error(concat("expected an index of at least 1, got ", Brief(index)));
  OpticalGlass glass{};
  glass.mCoefficients[0] = index;
  return glass;
}

OpticalGlass OpticalGlass::abbe(float nd, float abbeNumber,
                                std::optional<float> partialDispersion) {
  if (!(std::isfinite(nd) && nd > 1))
    throw Error(concat("expected an index greater than 1, got ", Brief(nd)));
  if (!(std::isfinite(abbeNumber) && abbeNumber > 0))
    throw Error(
        concat("expected a positive Abbe number, got ", Brief(abbeNumber)));
  if (partialDispersion && !(*partialDispersion > 0 && *partialDispersion < 1))
    throw Error(concat("expected a partial dispersion between 0 and 1, got ",
                       Brief(*partialDispersion)));
  const double pgF{partialDispersion
                       ? double(*partialDispersion)
                       : NORMAL_LINE_INTERCEPT +
                             NORMAL_LINE_SLOPE * double(abbeNumber)};
  // The three conditions are linear in the A, B, and C of
  // `A + B x + C x^2`, with `x` the inverse square wavelength. The second
  // and third, divided by `xF - xC` and `xg - xF`, are the mean slopes of
  // the index in `x` across F to C and across g to F: `B + C (xF + xC)`
  // and `B + C (xg + xF)`. Their difference is C alone.
  const double xd{inverseSquareMicrometers(FRAUNHOFER_D_LINE)};
  const double xF{inverseSquareMicrometers(FRAUNHOFER_F_LINE)};
  const double xC{inverseSquareMicrometers(FRAUNHOFER_C_LINE)};
  const double xg{inverseSquareMicrometers(FRAUNHOFER_G_LINE)};
  const double spreadFC{(double(nd) - 1) / double(abbeNumber)};
  const double slopeFC{spreadFC / (xF - xC)};
  const double slopeGF{pgF * spreadFC / (xg - xF)};
  const double c{(slopeGF - slopeFC) / (xg - xC)};
  const double b{slopeFC - c * (xF + xC)};
  const double a{double(nd) - xd * (b + c * xd)};
  OpticalGlass glass{};
  glass.mKind = Kind::ABBE;
  glass.mCoefficients = {float(a), float(b), float(c), 0, 0, 0};
  glass.validate();
  return glass;
}

OpticalGlass OpticalGlass::sellmeier(const std::array<float, 3> &b,
                                     const std::array<float, 3> &c) {
  for (size_t i = 0; i < 3; i++) {
    if (!(std::isfinite(b[i]) && std::isfinite(c[i])))
      throw Error("expected finite Sellmeier coefficients");
    // A term with no weight has no pole, wherever its C would put one.
    if (b[i] != 0 && c[i] >= DOMAIN_MIN_SQUARED && c[i] <= DOMAIN_MAX_SQUARED)
      throw Error(concat("Sellmeier term ", i + 1, " has its pole at ",
                         Brief(1e3 * std::sqrt(double(c[i]))),
                         " nm, inside the ", OPTICAL_GLASS_WAVELENGTH_MIN,
                         " to ", OPTICAL_GLASS_WAVELENGTH_MAX,
                         " nm a glass is evaluated over"));
  }
  OpticalGlass glass{};
  glass.mKind = Kind::SELLMEIER;
  glass.mCoefficients = {b[0], b[1], b[2], c[0], c[1], c[2]};
  glass.validate();
  if (!(glass.evaluate(double(FRAUNHOFER_F_LINE)) >
        glass.evaluate(double(FRAUNHOFER_C_LINE))))
    throw Error("the Sellmeier coefficients describe a glass that does not "
                "disperse, which is stated by its index alone");
  return glass;
}

void OpticalGlass::validate() const {
  // Every nanometer, in double. That is fine enough that nothing smooth
  // hides between the samples; what is not smooth is a pole, which
  // `sellmeier()` finds exactly before this runs.
  double previous{std::numeric_limits<double>::infinity()};
  for (int wavelength = int(OPTICAL_GLASS_WAVELENGTH_MIN);
       wavelength <= int(OPTICAL_GLASS_WAVELENGTH_MAX); wavelength++) {
    const double index{evaluate(double(wavelength))};
    if (!std::isfinite(index))
      throw Error(
          concat("the index is not a finite number at ", wavelength, " nm"));
    if (index < 1)
      throw Error(concat("the index falls to ", Brief(index), " at ",
                         wavelength, " nm, below the index of air"));
    if (index > previous)
      throw Error(concat("the index rises with wavelength at ", wavelength,
                         " nm, which the index of a glass never does where "
                         "the glass transmits"));
    previous = index;
  }
}

float OpticalGlass::nd() const noexcept {
  return float(evaluate(double(FRAUNHOFER_D_LINE)));
}

float OpticalGlass::abbeNumber() const noexcept {
  if (!isDispersive()) return INF;
  const double nd{evaluate(double(FRAUNHOFER_D_LINE))};
  const double nF{evaluate(double(FRAUNHOFER_F_LINE))};
  const double nC{evaluate(double(FRAUNHOFER_C_LINE))};
  return float((nd - 1) / (nF - nC));
}

float OpticalGlass::partialDispersion() const noexcept {
  if (!isDispersive()) return 0;
  const double nF{evaluate(double(FRAUNHOFER_F_LINE))};
  const double nC{evaluate(double(FRAUNHOFER_C_LINE))};
  const double ng{evaluate(double(FRAUNHOFER_G_LINE))};
  return float((ng - nF) / (nF - nC));
}

Span<const OpticalGlassEntry> opticalGlassCatalog() {
  static const std::vector<OpticalGlassEntry> entries{[] {
    std::vector<OpticalGlassEntry> result{};
    for (const auto &data : CATALOG)
      result.push_back({data.name, OpticalGlass::sellmeier(data.b, data.c)});
    return result;
  }()};
  return {entries.data(), entries.size()};
}

const OpticalGlassEntry *findOpticalGlass(std::string_view name) {
  for (const auto &alias : ALIASES) {
    if (isSameName(name, alias.alias)) {
      name = alias.name;
      break;
    }
  }
  for (const auto &entry : opticalGlassCatalog())
    if (isSameName(name, entry.name)) return &entry;
  return nullptr;
}

} // namespace smdl
