#include "doctest.h"

#include <cmath>
#include <vector>

#include "smdl/Compiler.h"

#include "Color.h"
#include "Layout/Layout.h"
#include "Scene/Primitive.h"
#include "Scene/Scene.h"

// The analytic shapes: that the closed-form surface, the parameters the
// intersector reports, the exact area, and the uniform-area sampling all
// describe the same geometry. Written over every shape rather than over
// the box alone, so the four that predate it are covered by the same
// cases.

static const char *MATERIALS{"#smdl\n"
                             "import ::df::*;\n"
                             "export material dull() = material(\n"
                             "  surface: material_surface(scattering: "
                             "df::diffuse_reflection_bsdf()));\n"};

namespace {

// Deliberately unequal dimensions everywhere, so that a swapped axis or
// a confused radius and height cannot pass.
[[nodiscard]] std::vector<PrimitiveSpec> everyShape() {
  auto specs{std::vector<PrimitiveSpec>()};
  auto &sphere{specs.emplace_back()};
  sphere.shape = PrimitiveSpec::Shape::SPHERE;
  sphere.radius = 0.7f;
  auto &box{specs.emplace_back()};
  box.shape = PrimitiveSpec::Shape::BOX;
  box.size = float3(0.5f, 1.25f, 2.0f);
  auto &disk{specs.emplace_back()};
  disk.shape = PrimitiveSpec::Shape::DISK;
  disk.radius = 1.3f;
  auto &cylinder{specs.emplace_back()};
  cylinder.shape = PrimitiveSpec::Shape::CYLINDER;
  cylinder.radius = 0.6f;
  cylinder.height = 1.7f;
  auto &cone{specs.emplace_back()};
  cone.shape = PrimitiveSpec::Shape::CONE;
  cone.radius = 0.9f;
  cone.height = 1.1f;
  return specs;
}

// The closed-form area, written out independently of the
// implementation's own piece-by-piece sum.
[[nodiscard]] float expectedArea(const PrimitiveSpec &spec) {
  const float r{spec.radius}, h{spec.height};
  const auto &s{spec.size};
  switch (spec.shape) {
  case PrimitiveSpec::Shape::SPHERE:
    return 4.0f * PI * r * r;
  case PrimitiveSpec::Shape::BOX:
    return 2.0f * (s.x * s.y + s.y * s.z + s.z * s.x);
  case PrimitiveSpec::Shape::DISK:
    return PI * r * r;
  case PrimitiveSpec::Shape::CYLINDER:
    return TWO_PI * r * h + 2.0f * PI * r * r;
  case PrimitiveSpec::Shape::CONE:
    return PI * r * std::sqrt(r * r + h * h) + PI * r * r;
  default:
    return 0.0f;
  }
}

// How far the point is from the shape's surface, in the shape's own
// implicit measure. Zero on the surface.
[[nodiscard]] float surfaceResidual(const PrimitiveSpec &spec,
                                    const float3 &p) {
  const float r{spec.radius}, h{spec.height};
  const float rho{std::sqrt(p.x * p.x + p.y * p.y)};
  switch (spec.shape) {
  case PrimitiveSpec::Shape::SPHERE:
    return std::fabs(length(p) - r);
  case PrimitiveSpec::Shape::BOX: {
    // Exactly one coordinate at its half-extent, the other two inside.
    float best{INF};
    for (size_t axis = 0; axis < 3; axis++) {
      const float half{0.5f * spec.size[axis]};
      float residual{std::fabs(std::fabs(p[axis]) - half)};
      for (size_t other = 0; other < 3; other++)
        if (other != axis)
          residual =
              std::max(residual, std::max(0.0f, std::fabs(p[other]) -
                                                    0.5f * spec.size[other]));
      best = std::min(best, residual);
    }
    return best;
  }
  case PrimitiveSpec::Shape::DISK:
    return std::max(std::fabs(p.z), std::max(0.0f, rho - r));
  case PrimitiveSpec::Shape::CYLINDER:
    // The side, or either cap.
    return std::min(
        std::max(std::fabs(rho - r), std::max(0.0f, std::max(-p.z, p.z - h))),
        std::max(std::min(std::fabs(p.z), std::fabs(p.z - h)),
                 std::max(0.0f, rho - r)));
  case PrimitiveSpec::Shape::CONE:
    return std::min(std::max(std::fabs(rho - r * (1.0f - p.z / h)),
                             std::max(0.0f, std::max(-p.z, p.z - h))),
                    std::max(std::fabs(p.z), std::max(0.0f, rho - r)));
  default:
    return INF;
  }
}

// The scene a ray case traces against: one primitive at the origin.
class Fixture final {
public:
  explicit Fixture(const PrimitiveSpec &spec) {
    if (auto error{compiler.addCode("::primtest", MATERIALS)}) {
      MESSAGE(error->message);
      REQUIRE(false);
    }
    LayoutItem item{};
    item.primitive = spec;
    item.materials.all = "dull";
    scene.add(item);
    if (auto error{compiler.compile(smdl::OPT_LEVEL_NONE)}) {
      MESSAGE(error->message);
      REQUIRE(false);
    }
    if (auto error{compiler.jitCompile()}) {
      MESSAGE(error->message);
      REQUIRE(false);
    }
    auto gridSpec{std::vector<float>(16)};
    for (size_t i = 0; i < gridSpec.size(); i++)
      gridSpec[i] = 400.0f + 300.0f * float(i) / float(gridSpec.size() - 1);
    wavelengths =
        Color(smdl::Span<const float>(gridSpec.data(), gridSpec.size()));
    renderGrid().wavelengths = wavelengths;
    scene.commit(wavelengths);
  }

  smdl::Compiler compiler{};
  Scene scene{compiler};
  Color wavelengths{};
};

} // namespace

TEST_CASE("Primitive") {
  SUBCASE("Piece counts") {
    for (const auto &spec : everyShape()) {
      CAPTURE(std::string(spec.name()));
      const auto pieceCount{primitivePieceCount(spec)};
      switch (spec.shape) {
      case PrimitiveSpec::Shape::SPHERE:
      case PrimitiveSpec::Shape::DISK:
        CHECK(pieceCount == 1);
        break;
      case PrimitiveSpec::Shape::BOX:
        CHECK(pieceCount == 6);
        break;
      case PrimitiveSpec::Shape::CYLINDER:
        CHECK(pieceCount == 3);
        break;
      default:
        CHECK(pieceCount == 2);
        break;
      }
    }
  }
  SUBCASE("Areas are exact") {
    for (const auto &spec : everyShape()) {
      CAPTURE(std::string(spec.name()));
      CHECK(primitiveObjectArea(spec) ==
            doctest::Approx(expectedArea(spec)).epsilon(1e-5));
    }
  }
  SUBCASE("Surface from a point agrees with surface from parameters") {
    // Away from the parametric degeneracies (the sphere's poles, the
    // cone's apex, the center of a cap), where the azimuth is not
    // defined and the two constructions cannot be asked to agree.
    for (const auto &spec : everyShape()) {
      CAPTURE(std::string(spec.name()));
      for (uint32_t primID = 0; primID < primitivePieceCount(spec); primID++)
        for (int iu = 0; iu < 7; iu++)
          for (int iv = 1; iv < 7; iv++) {
            const auto uv{
                float2(0.5f / 7.0f + float(iu) / 7.0f, float(iv) / 8.0f)};
            const auto fromUV{evalPrimitiveSurface(spec, primID, uv)};
            const auto fromPoint{
                evalPrimitiveSurfaceAt(spec, primID, fromUV.point)};
            CHECK(length(fromUV.point - fromPoint.point) < 1e-5f);
            CHECK(length(fromUV.normal - fromPoint.normal) < 1e-5f);
            CHECK(length(fromUV.dPdu - fromPoint.dPdu) < 1e-4f);
            CHECK(length(fromUV.dPdv - fromPoint.dPdv) < 1e-4f);
            CHECK(length(fromUV.dNdu - fromPoint.dNdu) < 1e-4f);
            CHECK(length(fromUV.dNdv - fromPoint.dNdv) < 1e-4f);
          }
    }
  }
  SUBCASE("The surface is where the shape says it is") {
    for (const auto &spec : everyShape()) {
      CAPTURE(std::string(spec.name()));
      float worst{0.0f};
      for (uint32_t primID = 0; primID < primitivePieceCount(spec); primID++)
        for (int iu = 0; iu <= 8; iu++)
          for (int iv = 0; iv <= 8; iv++) {
            const auto surface{evalPrimitiveSurface(
                spec, primID, float2(float(iu) / 8.0f, float(iv) / 8.0f))};
            worst = std::max(worst, surfaceResidual(spec, surface.point));
            // The normal is a unit vector perpendicular to both
            // parametric partials wherever they are nondegenerate.
            CHECK(std::fabs(length(surface.normal) - 1.0f) < 1e-5f);
            if (length(surface.dPdu) > 1e-4f)
              CHECK(std::fabs(dot(normalize(surface.dPdu), surface.normal)) <
                    1e-4f);
            if (length(surface.dPdv) > 1e-4f)
              CHECK(std::fabs(dot(normalize(surface.dPdv), surface.normal)) <
                    1e-4f);
          }
      CHECK(worst < 1e-5f);
    }
  }
  SUBCASE("Area sampling is uniform and lands on the surface") {
    // A stratified sweep of the sample domain rather than an RNG, so
    // the piece proportions are the exact area fractions up to the
    // stratum width and the case cannot fail intermittently.
    const int N{64};
    for (const auto &spec : everyShape()) {
      CAPTURE(std::string(spec.name()));
      const auto pieceCount{primitivePieceCount(spec)};
      auto counts{std::vector<int>(pieceCount, 0)};
      float worst{0.0f};
      for (int ix = 0; ix < N; ix++)
        for (int iy = 0; iy < N; iy++) {
          const auto xi{float2((float(ix) + 0.5f) / N, (float(iy) + 0.5f) / N)};
          const auto sample{samplePrimitiveArea(spec, xi)};
          REQUIRE(sample.primID < pieceCount);
          counts[sample.primID]++;
          worst = std::max(worst, surfaceResidual(spec, sample.point));
          // The sample reports the point and normal directly; they must
          // be the same ones its (piece, u, v) rebuilds.
          const auto surface{
              evalPrimitiveSurface(spec, sample.primID, sample.uv)};
          CHECK(length(sample.point - surface.point) < 1e-4f);
          CHECK(length(sample.normal - surface.normal) < 1e-4f);
        }
      CHECK(worst < 1e-5f);
      // Each piece takes its share of the draws, which for the box is
      // the statement that the six faces are sampled by area.
      const float total{primitiveObjectArea(spec)};
      if (spec.shape == PrimitiveSpec::Shape::BOX)
        for (uint32_t primID = 0; primID < pieceCount; primID++) {
          const size_t axis{primID >> 1};
          const float faceArea{spec.size[(axis + 1) % 3] *
                               spec.size[(axis + 2) % 3]};
          CHECK(float(counts[primID]) / float(N * N) ==
                doctest::Approx(faceArea / total).epsilon(0.02));
        }
    }
  }
  SUBCASE("Box rays enter and leave through the right faces") {
    PrimitiveSpec spec{};
    spec.shape = PrimitiveSpec::Shape::BOX;
    spec.size = float3(0.5f, 1.25f, 2.0f);
    Fixture fixture{spec};
    const auto half{0.5f * spec.size};
    for (size_t axis = 0; axis < 3; axis++)
      for (int sign = -1; sign <= 1; sign += 2) {
        CAPTURE(axis);
        CAPTURE(sign);
        auto dir{float3(0.0f)};
        dir[axis] = float(sign);
        // From the center outward: the face at that end, hit at its
        // half-extent, with the outward normal along the ray.
        Ray outward{float3(0.0f), dir, EPS, INF};
        Hit out{};
        REQUIRE(fixture.scene.intersect(outward, out));
        CHECK(out.point[axis] == doctest::Approx(float(sign) * half[axis]));
        CHECK(out.normal[axis] == doctest::Approx(float(sign)));
        CHECK(dot(out.normal, dir) > 0.0f);
        // Continuing past it leaves the box for good.
        Ray beyond{out.point + 1e-3f * dir, dir, EPS, INF};
        Hit again{};
        CHECK(!fixture.scene.intersect(beyond, again));
        // From well outside on the far side, travelling the same way:
        // the opposite face, entered against its outward normal.
        const auto origin{-4.0f * dir};
        Ray inward{origin, dir, EPS, INF};
        Hit in{};
        REQUIRE(fixture.scene.intersect(inward, in));
        CHECK(in.point[axis] == doctest::Approx(-float(sign) * half[axis]));
        CHECK(dot(in.normal, dir) < 0.0f);
        // And the same ray offset past the side of the box misses it.
        auto offset{origin};
        offset[(axis + 1) % 3] = 0.5f * spec.size[(axis + 1) % 3] + 1e-2f;
        Ray past{offset, dir, EPS, INF};
        Hit missed{};
        CHECK(!fixture.scene.intersect(past, missed));
      }
  }
  SUBCASE("A ray crossing the box hits exactly twice") {
    PrimitiveSpec spec{};
    spec.shape = PrimitiveSpec::Shape::BOX;
    spec.size = float3(0.5f, 1.25f, 2.0f);
    Fixture fixture{spec};
    // An oblique direction, so the crossing is not axis-aligned and the
    // entry and exit faces differ.
    const auto dir{normalize(float3(0.37f, 0.51f, -0.77f))};
    const auto origin{float3(-3.0f * dir)};
    int crossings{0};
    auto point{origin};
    for (int i = 0; i < 8; i++) {
      Ray ray{point, dir, EPS, INF};
      Hit hit{};
      if (!fixture.scene.intersect(ray, hit)) break;
      crossings++;
      // Every hit is on the surface, and the outward normal turns from
      // facing the ray to following it as the ray passes through.
      CHECK(surfaceResidual(spec, hit.point) < 1e-4f);
      CHECK((crossings == 1 ? dot(hit.normal, dir) < 0.0f
                            : dot(hit.normal, dir) > 0.0f));
      point = hit.point + 1e-3f * dir;
    }
    CHECK(crossings == 2);
  }
}
