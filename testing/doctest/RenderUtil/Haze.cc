#include "doctest.h"

#include <algorithm>
#include <cmath>
#include <vector>

#include "smdl/RenderUtil/Haze.h"

namespace {

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
  SUBCASE("sun in-scattering against quadrature") {
    // A point sun, so that the closed form is checked against
    // quadrature of the same integral and not against the disk average
    // the real one carries; the average has its own check below.
    auto sun{smdl::HazeSun{}};
    sun.direction = normalize(smdl::float3{0.42f, 0.0f, 0.55f});
    sun.cosRadius = 1.0f;
    sun.irradiance = smdl::SpectralColor(wavelens.size(), 1.0f);
    const auto haze{smdl::Haze(options, span, 1.0f, sun)};
    const float invSunSlope{1.0f / haze.shapeExponent(sun.direction.z)};
    std::vector<float> analytic(haze.size());
    std::vector<float> sigma(haze.size());
    for (const auto &org :
         {smdl::float3{0.0f, 0.0f, 0.0f}, smdl::float3{0.0f, 0.0f, 600.0f}}) {
      for (float dz : {0.9f, 0.25f, 0.0f, -0.2f}) {
        const auto dir{makeDirection(dz)};
        const float phase{haze.phaseOverSunDisk(dir)};
        for (float t : {300.0f, 4000.0f, 30000.0f}) {
          if (org.z + t * dir.z < -5000.0f) continue;
          CAPTURE(org.z);
          CAPTURE(dz);
          CAPTURE(t);
          float tShadow{};
          REQUIRE(haze.sunInscatter(
              org, dir, t, 0.5f,
              smdl::Span<float>(analytic.data(), analytic.size()), tShadow));
          // The distance the visibility is tested at must lie on the
          // segment it was drawn from.
          CHECK(tShadow >= 0.0f);
          CHECK(tShadow <= t);
          constexpr int STEPS{200000};
          const float dt{t / STEPS};
          for (size_t band : {size_t(0), haze.size() / 2}) {
            CAPTURE(band);
            double refer{0.0};
            double depth{0.0};
            for (int i = 0; i < STEPS; i++) {
              haze.extinctionAt(org.z + (float(i) + 0.5f) * dt * dir.z,
                                smdl::Span<float>(sigma.data(), sigma.size()));
              const double mu{double(sigma[band])};
              refer += mu * options.albedo *
                       std::exp(-(depth + 0.5 * mu * dt)) *
                       std::exp(-mu * double(invSunSlope));
              depth += mu * dt;
            }
            refer *= double(dt) * double(phase);
            CHECK(std::abs(refer - analytic[band]) / std::max(refer, 1e-12) <
                  2e-4);
          }
        }
      }
    }
  }
  SUBCASE("sun in-scattering has a finite unbounded limit") {
    // A segment that never turns upward runs its depth away on both
    // sides of a converging integral. The limit follows from the two
    // slopes alone: the medium the segment accumulates against the
    // medium the sun climbs out through.
    auto sun{smdl::HazeSun{}};
    sun.direction = normalize(smdl::float3{0.42f, 0.0f, 0.55f});
    sun.irradiance = smdl::SpectralColor(wavelens.size(), 1.0f);
    const auto haze{smdl::Haze(options, span, 1.0f, sun)};
    const float invSunSlope{1.0f / haze.shapeExponent(sun.direction.z)};
    std::vector<float> analytic(haze.size());
    std::vector<float> sigma(haze.size());
    const smdl::float3 org{0.0f, 0.0f, 0.0f};
    haze.extinctionAt(org.z, smdl::Span<float>(sigma.data(), sigma.size()));
    for (float dz : {-0.05f, -0.3f, -0.8f}) {
      CAPTURE(dz);
      const auto dir{makeDirection(dz)};
      float tShadow{};
      REQUIRE(haze.sunInscatter(
          org, dir, smdl::INF, 0.5f,
          smdl::Span<float>(analytic.data(), analytic.size()), tShadow));
      const float phase{haze.phaseOverSunDisk(dir)};
      for (size_t band : {size_t(0), haze.size() / 2}) {
        CAPTURE(band);
        const float limit{phase * options.albedo *
                          std::exp(-sigma[band] * invSunSlope) *
                          sun.direction.z / (sun.direction.z - dz)};
        CHECK(std::abs(analytic[band] - limit) / std::max(limit, 1e-12f) <
              1e-3f);
      }
    }
  }
  SUBCASE("phase over the sun disk") {
    // At the droplet diameter whose diffraction peak is narrow enough
    // for the difference from the center value to matter, against a far
    // finer average of the same cone.
    auto foggy{options};
    foggy.dropletSize = 12.0f;
    auto sun{smdl::HazeSun{}};
    sun.direction = normalize(smdl::float3{0.42f, 0.0f, 0.55f});
    sun.cosRadius = std::cos(0.2665f * smdl::PI / 180.0f);
    sun.irradiance = smdl::SpectralColor(wavelens.size(), 1.0f);
    const auto haze{smdl::Haze(foggy, span, 1.0f, sun)};
    const auto frame{coordinateSystem(sun.direction)};
    for (float dz : {0.55f, 0.1f, -0.4f}) {
      CAPTURE(dz);
      const auto dir{makeDirection(dz)};
      constexpr int M{65536};
      double refer{0.0};
      for (int i = 0; i < M; i++) {
        const float cosTheta{1.0f -
                             (float(i) + 0.5f) / M * (1.0f - sun.cosRadius)};
        const float sinTheta{
            std::sqrt(std::max(0.0f, 1.0f - cosTheta * cosTheta))};
        const float phi{2.39996323f * float(i)};
        refer += haze.phase().evaluate(
            -dir, frame * smdl::float3(sinTheta * std::cos(phi),
                                       sinTheta * std::sin(phi), cosTheta));
      }
      refer /= M;
      CHECK(std::abs(haze.phaseOverSunDisk(dir) - refer) /
                std::max(refer, 1e-12) <
            5e-3);
    }
  }
  SUBCASE("no sun is no in-scattering") {
    const auto haze{smdl::Haze(options, span, 1.0f)};
    CHECK_FALSE(haze.sun().isValid());
    std::vector<float> radiance(haze.size(), -1.0f);
    float tShadow{-1.0f};
    CHECK_FALSE(haze.sunInscatter(
        smdl::float3{0.0f, 0.0f, 0.0f}, smdl::float3{1.0f, 0.0f, 0.0f}, 100.0f,
        0.5f, smdl::Span<float>(radiance.data(), radiance.size()), tShadow));
    // The outputs are left alone when there is nothing to report.
    CHECK(radiance[0] == -1.0f);
    CHECK(tShadow == -1.0f);
  }
}
