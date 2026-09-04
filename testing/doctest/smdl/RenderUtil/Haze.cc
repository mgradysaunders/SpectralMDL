#include "doctest.h"

#include <algorithm>
#include <cmath>
#include <vector>

#include "smdl/RenderUtil/Haze.h"

namespace {

#include "HazeGolden.inl"

// The render-default wavelength grid, which is all the haze needs to
// fix its extinction spectrum.
std::vector<float> makeWavelengths(size_t numBands = 16) {
  std::vector<float> wavelens(numBands);
  for (size_t i = 0; i < numBands; i++)
    wavelens[i] = 380.0f + 340.0f * (float(i) + 0.5f) / float(numBands);
  return wavelens;
}

smdl::HazeOptions makeOptions() {
  smdl::HazeOptions options{};
  options.visibility = 12.0f;
  options.scaleHeight = 900.0f;
  options.baseHeight = 30.0f;
  options.dropletSize = 1.0f;
  return options;
}

smdl::float3 makeDirection(float dz) {
  return {std::sqrt(std::max(0.0f, 1.0f - dz * dz)), 0.0f, dz};
}

// The deflection cosine `MiePhase::sample` produced, with the lobe
// forced: a third component of 0 always picks the Draine lobe and 1
// always picks the Henyey-Greenstein one.
float sampleDeflection(const smdl::MiePhase &phase, float xi, bool draine) {
  const smdl::float3 wo{0.0f, 0.0f, 1.0f};
  smdl::float3 wi{};
  (void)phase.sample({xi, 0.25f, draine ? 0.0f : 1.0f}, wo, wi);
  return -dot(wo, wi);
}

// The optical depth of one band over `[0, t]`, integrated at midpoints
// against the same profile the closed form claims to solve.
float referenceDepth(const smdl::Haze &haze, const smdl::float3 &org,
                     const smdl::float3 &dir, float t, size_t band) {
  constexpr int STEPS{200000};
  std::vector<float> sigma(haze.size());
  const float dt{t / STEPS};
  double depth{0.0};
  for (int i = 0; i < STEPS; i++) {
    haze.extinctionAt(org.z + (float(i) + 0.5f) * dt * dir.z,
                      smdl::Span<float>(sigma.data(), sigma.size()));
    depth += double(sigma[band]);
  }
  return float(depth * double(dt));
}

} // namespace

TEST_CASE("MiePhase") {
  // The goldens below are the ones 'testing/language/df_fog.smdl'
  // asserts against the builtin 'df::fog_vdf', produced by an
  // independent Python transcription of the paper's fits and of the
  // analytic Draine cumulative inversion. Sharing them is what ties the
  // two implementations of this phase function together: neither is the
  // other's reference, and a drift in either shows up here.
  SUBCASE("fitted parameters golden") {
    struct Case final {
      float diameter, gHG, gD, alpha, wD;
    };
    const Case cases[]{{0.05f, 0.034500f, 0.025661f, 250.0f, 0.252181f},
                       {0.8f, 0.854880f, 0.635765f, 250.0f, 0.329837f},
                       {3.0f, 0.945945f, 0.421973f, 14.34051f, 0.380939f},
                       {8.0f, 0.984469f, 0.541064f, 20.32569f, 0.473642f}};
    for (const auto &c : cases) {
      CAPTURE(c.diameter);
      const auto phase{smdl::MiePhase(c.diameter)};
      CHECK(std::abs(phase.asymmetryHG() - c.gHG) < 1e-5f);
      CHECK(std::abs(phase.asymmetryDraine() - c.gD) < 1e-5f);
      CHECK(std::abs(phase.alphaDraine() - c.alpha) < 1e-4f);
      CHECK(std::abs(phase.weightDraine() - c.wD) < 1e-5f);
    }
    // The diameter clamps into the fitted domain.
    const auto clamped{smdl::MiePhase(75.0f)};
    const auto atMax{smdl::MiePhase(50.0f)};
    CHECK(clamped.asymmetryHG() == atMax.asymmetryHG());
    CHECK(clamped.weightDraine() == atMax.weightDraine());
  }
  SUBCASE("phase value golden") {
    struct Case final {
      float diameter, u, expected, tolerance;
    };
    const Case cases[]{{0.05f, -0.9f, 0.099333763f, 1e-6f},
                       {0.05f, 0.0f, 0.059570036f, 1e-6f},
                       {0.05f, 0.9f, 0.11714347f, 1e-6f},
                       {0.8f, -0.9f, 0.0075843035f, 1e-7f},
                       {0.8f, 0.5f, 0.027228639f, 1e-6f},
                       {0.8f, 0.999f, 4.7049151f, 1e-3f},
                       {3.0f, -0.3f, 0.0057958627f, 1e-7f},
                       {3.0f, 0.9f, 0.21670597f, 1e-5f},
                       {3.0f, 0.999f, 15.778123f, 1e-2f},
                       {8.0f, -0.9f, 0.011796226f, 1e-7f},
                       {8.0f, 0.0f, 0.0020117001f, 1e-7f},
                       {8.0f, 0.5f, 0.022505575f, 1e-6f},
                       {8.0f, 0.9f, 0.23496871f, 1e-5f},
                       {20.0f, -0.3f, 0.0025116324f, 1e-7f},
                       {20.0f, 0.9f, 0.24239888f, 1e-5f}};
    for (const auto &c : cases) {
      CAPTURE(c.diameter);
      CAPTURE(c.u);
      CHECK(std::abs(smdl::MiePhase(c.diameter).evaluate(c.u) - c.expected) <
            c.tolerance);
    }
    // The direction-pair overload is the same function of the
    // deflection cosine, which is the negated dot product.
    const auto phase{smdl::MiePhase(8.0f)};
    const smdl::float3 wo{0.0f, 0.0f, 1.0f};
    CHECK(phase.evaluate(wo, smdl::float3{0.6f, 0.0f, -0.8f}) ==
          doctest::Approx(phase.evaluate(0.8f)));
  }
  SUBCASE("cumulative inversion golden") {
    // The lobes are addressed through their fitted diameters, which is
    // where the goldens' explicit parameters came from.
    struct Case final {
      float diameter, xi, expected;
      bool draine;
    };
    const Case cases[]{
        {8.0f, 0.1f, 0.35738498f, true},   {8.0f, 0.5f, 0.87946407f, true},
        {8.0f, 0.9f, 0.98376952f, true},   {3.0f, 0.25f, 0.5857715f, true},
        {0.8f, 0.7f, 0.96125226f, true},   {8.0f, 0.1f, 0.98943122f, false},
        {8.0f, 0.5f, 0.99964007f, false},  {0.8f, 0.7f, 0.98878436f, false},
        {0.05f, 0.5f, 0.051729468f, false}};
    for (const auto &c : cases) {
      CAPTURE(c.diameter);
      CAPTURE(c.xi);
      CAPTURE(c.draine);
      const auto phase{smdl::MiePhase(c.diameter)};
      CHECK(std::abs(sampleDeflection(phase, c.xi, c.draine) - c.expected) <
            1e-5f);
    }
    // The near-isotropic Cardano branch, which the fitted anisotropy
    // only reaches under about 0.03 micrometers of diameter, and which
    // the goldens above cannot address because they name a lobe
    // anisotropy no diameter resolves to. It takes the root of the
    // anisotropy-free cumulative distribution, whose density is even in
    // the deflection cosine, so its median is exactly zero.
    const auto isotropic{smdl::MiePhase(0.02f)};
    REQUIRE(std::abs(isotropic.asymmetryDraine()) < 0.01f);
    CHECK(std::abs(sampleDeflection(isotropic, 0.5f, true)) < 1e-5f);
    CHECK(sampleDeflection(isotropic, 0.25f, true) < 0.0f);
    CHECK(sampleDeflection(isotropic, 0.75f, true) > 0.0f);
  }
  SUBCASE("normalized, non-negative, and sampled as evaluated") {
    // One diameter per branch of the piecewise fits.
    for (float diameter : {0.05f, 0.8f, 3.0f, 12.0f}) {
      CAPTURE(diameter);
      const auto phase{smdl::MiePhase(diameter)};
      // The reference cumulative distribution of the deflection cosine,
      // integrated finely enough to resolve a forward peak a hundredth
      // of a radian wide.
      constexpr int STEPS{1000000};
      std::vector<double> cdf(STEPS + 1, 0.0);
      bool negative{false};
      for (int i = 0; i < STEPS; i++) {
        const double p{
            phase.evaluate(-1.0f + 2.0f * (float(i) + 0.5f) / STEPS)};
        if (p < 0.0) negative = true;
        cdf[size_t(i) + 1] = cdf[size_t(i)] + p * (2.0 * smdl::TWO_PI / STEPS);
      }
      CHECK_FALSE(negative);
      CHECK(std::abs(cdf[STEPS] - 1.0) < 2e-3);
      // The sampler against that reference. The two lobes are drawn
      // separately and recombined at the exact mixture weight rather
      // than letting a stratified lobe dimension quantize it: what is
      // under test is the pair of cumulative inversions, and a weight
      // quantized to the strata would swamp their error.
      constexpr int NX{8192};
      std::vector<float> deflections[2]{};
      const smdl::float3 wo{0.0f, 0.0f, 1.0f};
      float worstSample{0.0f};
      for (int lobe = 0; lobe < 2; lobe++) {
        deflections[lobe].reserve(NX);
        for (int i = 0; i < NX; i++) {
          const float xiY{float(i) * 0.618034f -
                          std::floor(float(i) * 0.618034f)};
          smdl::float3 wi{};
          const float sampled{phase.sample(
              {(float(i) + 0.5f) / NX, xiY, lobe == 0 ? 0.0f : 1.0f}, wo, wi)};
          const float evaluated{phase.evaluate(wo, wi)};
          worstSample = std::max(worstSample, std::abs(sampled - evaluated) /
                                                  std::max(evaluated, 1e-9f));
          CHECK(std::abs(length(wi) - 1.0f) < 1e-4f);
          deflections[lobe].push_back(-dot(wo, wi));
        }
        std::sort(deflections[lobe].begin(), deflections[lobe].end());
      }
      // The value a sample reports must be the density it was drawn
      // from, which is what makes the phase its own sampling weight.
      CHECK(worstSample < 1e-4f);
      const float weights[2]{phase.weightDraine(), 1.0f - phase.weightDraine()};
      float worstCDF{0.0f};
      for (int probe = 1; probe < 32; probe++) {
        const float u0{-1.0f + 2.0f * float(probe) / 32.0f};
        float empirical{0.0f};
        for (int lobe = 0; lobe < 2; lobe++) {
          const auto &d{deflections[lobe]};
          empirical +=
              weights[lobe] *
              float(std::lower_bound(d.begin(), d.end(), u0) - d.begin()) /
              float(d.size());
        }
        const auto bin{
            size_t(std::clamp((u0 + 1.0f) * 0.5f * STEPS, 0.0f, float(STEPS)))};
        worstCDF = std::max(worstCDF, std::abs(empirical - float(cdf[bin])));
      }
      CHECK(worstCDF < 1e-3f);
    }
  }
}

TEST_CASE("Haze") {
  const auto wavelens{makeWavelengths()};
  const auto options{makeOptions()};
  const auto span{smdl::Span<const float>(wavelens.data(), wavelens.size())};
  SUBCASE("optical depth against quadrature") {
    // Rays that climb, descend, and run flat, from origins on both
    // sides of the reference height.
    const auto haze{smdl::Haze(options, span, 1.0f)};
    std::vector<float> sigmaC(haze.size());
    for (const auto &org :
         {smdl::float3{0.0f, 0.0f, 0.0f}, smdl::float3{0.0f, 0.0f, 400.0f},
          smdl::float3{0.0f, 0.0f, -250.0f}}) {
      for (float dz : {0.95f, 0.4f, 0.05f, 0.0f, -0.03f, -0.3f}) {
        const auto dir{makeDirection(dz)};
        haze.extinctionAt(org.z,
                          smdl::Span<float>(sigmaC.data(), sigmaC.size()));
        const float k{haze.shapeExponent(dir.z)};
        for (float t : {500.0f, 6000.0f, 40000.0f}) {
          if (org.z + t * dir.z < -5000.0f) continue;
          for (size_t band : {size_t(0), haze.size() - 1}) {
            CAPTURE(org.z);
            CAPTURE(dz);
            CAPTURE(t);
            CAPTURE(band);
            const float closed{sigmaC[band] * smdl::Haze::shape(k, t)};
            const float refer{referenceDepth(haze, org, dir, t, band)};
            CHECK(std::abs(closed - refer) / std::max(refer, 1e-9f) < 1e-4f);
          }
        }
      }
    }
  }
  SUBCASE("free-flight inversion against its own distribution") {
    // Sampling at `xi` must land where the transmittance has fallen to
    // `1 - xi`, which is the whole claim the analytic sampler makes.
    for (float k : {-2e-3f, -1e-4f, 0.0f, 1e-5f, 4e-4f, 3e-3f}) {
      for (float sigma : {1e-5f, 3.26e-4f, 2e-3f}) {
        for (int i = 1; i < 512; i++) {
          const float xi{float(i) / 512.0f};
          const float t{smdl::Haze::shapeInverse(k, -std::log1p(-xi) / sigma)};
          CAPTURE(k);
          CAPTURE(sigma);
          CAPTURE(xi);
          if (!std::isfinite(t)) {
            // No collision, which is only legitimate where the whole
            // ray carries less optical depth than the sample asked for.
            CHECK(sigma * smdl::Haze::shape(k, smdl::INF) <= -std::log1p(-xi));
            continue;
          }
          const float cdf{-std::expm1(-sigma * smdl::Haze::shape(k, t))};
          CHECK(std::abs(cdf - xi) / xi < 1e-4f);
        }
      }
    }
  }
  SUBCASE("extinction spectrum against its generator") {
    // The tables and this golden come out of the same MODTRAN runs, so
    // what is checked here is the construction the header describes: the
    // Koschmieder amount split between aerosol and Rayleigh, each
    // carrying its own shape, interpolated across the channel grid.
    // Wavelengths off the grid at both ends are in the golden, so the
    // clamp is checked too.
    const auto goldenSpan{
        smdl::Span<const float>(GOLDEN_WAVELENGTHS, GOLDEN_WAVELENGTH_COUNT)};
    for (size_t c = 0; c < GOLDEN_CASE_COUNT; c++) {
      auto goldenOptions{smdl::HazeOptions{}};
      goldenOptions.visibility = GOLDEN_VISIBILITY[c];
      const auto haze{smdl::Haze(goldenOptions, goldenSpan, 1.0f)};
      std::vector<float> sigma(haze.size()), albedo(haze.size());
      haze.extinctionAt(0.0f, smdl::Span<float>(sigma.data(), sigma.size()));
      haze.albedo(smdl::Span<float>(albedo.data(), albedo.size()));
      for (size_t i = 0; i < GOLDEN_WAVELENGTH_COUNT; i++) {
        CAPTURE(GOLDEN_VISIBILITY[c]);
        CAPTURE(GOLDEN_WAVELENGTHS[i]);
        CHECK(sigma[i] ==
              doctest::Approx(GOLDEN_EXTINCTION[c][i]).epsilon(1e-5));
        CHECK(albedo[i] == doctest::Approx(GOLDEN_ALBEDO[c][i]).epsilon(1e-5));
      }
    }
  }
  SUBCASE("the reference wavelength is Koschmieder's") {
    // The one thing the tables are not free to choose: aerosol plus
    // Rayleigh at 550nm is the extinction the meteorological range
    // names, which is what ties the haze to the sun-sky model's
    // visibility.
    const float reference[]{550.0f};
    for (float visibility : {5.0f, 12.0f, 23.0f, 50.0f, 100.0f}) {
      auto referenceOptions{smdl::HazeOptions{}};
      referenceOptions.visibility = visibility;
      const auto haze{smdl::Haze(referenceOptions,
                                 smdl::Span<const float>(reference, 1), 1.0f)};
      float sigma{};
      haze.extinctionAt(0.0f, smdl::Span<float>(&sigma, 1));
      CAPTURE(visibility);
      CHECK(sigma ==
            doctest::Approx(3.912f / (1000.0f * visibility)).epsilon(1e-5));
    }
  }
  SUBCASE("scattering never exceeds extinction") {
    const auto haze{smdl::Haze(options, span, 1.0f)};
    std::vector<float> albedo(haze.size());
    haze.albedo(smdl::Span<float>(albedo.data(), albedo.size()));
    for (size_t i = 0; i < albedo.size(); i++) {
      CAPTURE(i);
      CAPTURE(wavelens[i]);
      CHECK(albedo[i] > 0.0f);
      CHECK(albedo[i] <= 1.0f);
    }
  }
  SUBCASE("zenith depth is finite and approached from below") {
    // An upward ray leaves the atmosphere having accumulated
    // `sigmaC / k`, which is why the sky survives the haze.
    const auto haze{smdl::Haze(options, span, 1.0f)};
    for (float dz : {0.2f, 0.6f, 1.0f}) {
      CAPTURE(dz);
      const float k{haze.shapeExponent(dz)};
      const float limit{smdl::Haze::shape(k, smdl::INF)};
      CHECK(std::abs(limit - 1.0f / k) * k < 1e-5f);
      const float near{smdl::Haze::shape(k, 60.0f * options.scaleHeight / dz)};
      CHECK(near <= limit);
      CHECK(std::abs(near - limit) * k < 1e-5f);
    }
  }
}
