#include "CompileFixtures.h"

#include <cstdint>
#include <deque>
#include <string_view>
#include <utility>
#include <vector>

#include "smdl/RenderUtil/MediumStack.h"

using smdl::float3;
using smdl::MediumBoundary;
using smdl::MediumStack;
using smdl::MediumTies;

// The rules of the medium stack over hand-made boundaries: which entry is
// the medium, which crossings are false, which entries are active, and
// what index a hit refracts against. The materials are evaluated, since
// an entry is what a walk evaluated at the boundary it crossed.

namespace {

const char *MATERIALS{
    "#smdl\n"
    "import ::df::*;\n"
    "export material stack_glass() = material(\n"
    "  ior: 1.5,\n"
    "  surface: material_surface(scattering:\n"
    "    df::specular_bsdf(mode: df::scatter_reflect_transmit)));\n"
    "export material stack_water() = material(\n"
    "  ior: 1.33,\n"
    "  surface: material_surface(scattering:\n"
    "    df::specular_bsdf(mode: df::scatter_reflect_transmit)),\n"
    "  volume: material_volume(absorption_coefficient: color(0.1)));\n"
    "export material stack_flint() = material(\n"
    "  ior: 1.8,\n"
    "  surface: material_surface(scattering:\n"
    "    df::specular_bsdf(mode: df::scatter_reflect_transmit)));\n"
    "export material stack_smoke() = material(\n"
    "  volume: material_volume(\n"
    "    scattering: df::anisotropic_vdf(),\n"
    "    scattering_coefficient: color(0.5),\n"
    "    additive: true));\n"};

// The evaluated materials, the boundaries, and the stacks the cases build
// out of them, all of which live as long as the fixture.
class StackFixture final {
public:
  StackFixture() {
    REQUIRE_OK(compiler.addCode("::stacktest", MATERIALS));
    REQUIRE_OK(compiler.compile(smdl::OPT_LEVEL_O2));
    REQUIRE_OK(compiler.jitCompile());
  }

  StackFixture(const StackFixture &) = delete;

  StackFixture &operator=(const StackFixture &) = delete;

  // The material `name`, evaluated at a state whose geometry normal is +z.
  [[nodiscard]] const smdl::jit::Material &material(std::string_view name) {
    const smdl::jit::MaterialDef *materialDef{
        requireMaterial(compiler, name)};
    smdl::State state{storage_.makeState()};
    state.finalize();
    return materials_.emplace_back(state, materialDef);
  }

  // A boundary of priority `priority` that bounds a medium unless
  // `boundsMedium` says otherwise, identified by its own address.
  [[nodiscard]] const MediumBoundary &boundary(int priority,
                                               bool boundsMedium = true) {
    MediumBoundary &boundary{boundaries_.emplace_back()};
    boundary.id = &boundary;
    boundary.priority = priority;
    boundary.boundsMedium = boundsMedium;
    return boundary;
  }

  // `stack` with the medium `material` entered through `boundary`.
  [[nodiscard]] const MediumStack *enter(const MediumStack *stack,
                                         const smdl::jit::Material &material,
                                         const MediumBoundary &boundary) {
    MediumStack::enter(stack, allocator, &material, boundary);
    return stack;
  }

  // `stack` with the medium entered through `boundary` left.
  [[nodiscard]] const MediumStack *leave(const MediumStack *stack,
                                         const smdl::jit::Material &material,
                                         const MediumBoundary &boundary) {
    MediumStack::leave(stack, allocator, material.def, boundary);
    return stack;
  }

  smdl::Compiler compiler{};
  smdl::BumpPtrAllocator allocator{};

private:
  StateStorage storage_{compiler};

  // Deques, so that what the stacks point to stays put.
  std::deque<smdl::jit::Material> materials_{};

  std::deque<MediumBoundary> boundaries_{};
};

// The active entries of `stack`, from the top down.
[[nodiscard]] std::vector<const MediumStack *>
activeOf(const MediumStack *stack) {
  std::vector<const MediumStack *> active{};
  MediumStack::forEachActive(
      stack, [&](const MediumStack &entry) { active.push_back(&entry); });
  return active;
}

// Out of a boundary's interior, toward it, by the +z geometry normal.
constexpr float3 OUTSIDE{0.0f, 0.0f, 1.0f};
constexpr float3 INSIDE{0.0f, 0.0f, -1.0f};

} // namespace

TEST_CASE("MediumStack: the dominant entry ranks highest, the most recent "
          "among equals") {
  StackFixture fixture{};
  const smdl::jit::Material &glass{fixture.material("stack_glass")};
  const smdl::jit::Material &water{fixture.material("stack_water")};
  const smdl::jit::Material &smoke{fixture.material("stack_smoke")};
  const MediumBoundary &low{fixture.boundary(0)};
  const MediumBoundary &lowToo{fixture.boundary(0)};
  const MediumBoundary &high{fixture.boundary(1)};
  const MediumBoundary &highest{fixture.boundary(5)};
  SUBCASE("Among equals the most recent") {
    const MediumStack *first{fixture.enter(nullptr, water, low)};
    const MediumStack *second{fixture.enter(first, glass, lowToo)};
    CHECK(MediumStack::dominantOf(first) == first);
    CHECK(MediumStack::dominantOf(second) == second);
  }
  SUBCASE("A higher priority under a lower one") {
    const MediumStack *first{fixture.enter(nullptr, glass, high)};
    const MediumStack *second{fixture.enter(first, water, low)};
    CHECK(MediumStack::dominantOf(second) == first);
    // And one more of the higher priority's rank on top of both.
    const MediumStack *third{fixture.enter(second, water, high)};
    CHECK(MediumStack::dominantOf(third) == third);
  }
  SUBCASE("An additive entry never, however high") {
    const MediumStack *first{fixture.enter(nullptr, glass, low)};
    const MediumStack *second{fixture.enter(first, smoke, highest)};
    CHECK(MediumStack::dominantOf(second) == first);
    CHECK(MediumStack::dominantOf(fixture.enter(nullptr, smoke, high)) ==
          nullptr);
  }
  SUBCASE("An entry with no boundary under every placed one") {
    const MediumStack *exterior{new (fixture.allocator) MediumStack{
        nullptr, &water, MediumBoundary{}}};
    CHECK(exterior->priority == MediumStack::LOWEST_PRIORITY);
    CHECK(MediumStack::dominantOf(exterior) == exterior);
    const MediumStack *inside{fixture.enter(exterior, glass, low)};
    CHECK(MediumStack::dominantOf(inside) == inside);
  }
  SUBCASE("Left out of order, the stack keeps it") {
    // The entries above the one left are copied over the entries below
    // it, and the copy of the most recent of the two left of equal
    // priority is the dominant one.
    const MediumStack *stack{fixture.enter(nullptr, water, low)};
    stack = fixture.enter(stack, glass, high);
    stack = fixture.enter(stack, water, lowToo);
    REQUIRE(MediumStack::dominantOf(stack)->boundary == high.id);
    stack = fixture.leave(stack, glass, high);
    REQUIRE(stack);
    CHECK(stack->boundary == lowToo.id);
    CHECK(MediumStack::dominantOf(stack) == stack);
    CHECK(MediumStack::dominantWithout(stack, stack)->boundary == low.id);
  }
  SUBCASE("Leaving what was never entered changes nothing") {
    const MediumStack *stack{fixture.enter(nullptr, water, low)};
    CHECK(fixture.leave(stack, glass, high) == stack);
    CHECK(fixture.leave(nullptr, glass, high) == nullptr);
  }
}

TEST_CASE("MediumStack: a crossing is false where the active media stay as "
          "they are") {
  StackFixture fixture{};
  const smdl::jit::Material &glass{fixture.material("stack_glass")};
  const smdl::jit::Material &water{fixture.material("stack_water")};
  const smdl::jit::Material &flint{fixture.material("stack_flint")};
  const smdl::jit::Material &smoke{fixture.material("stack_smoke")};
  // A float of glass that outranks the water it straddles, a marble of
  // the water's rank, and a wall that bounds nothing.
  const MediumBoundary &sea{fixture.boundary(0)};
  const MediumBoundary &buoy{fixture.boundary(1)};
  const MediumBoundary &marble{fixture.boundary(0)};
  const MediumBoundary &wall{fixture.boundary(0, /*boundsMedium=*/false)};
  const MediumStack *inSea{fixture.enter(nullptr, water, sea)};
  const MediumStack *inBuoy{fixture.enter(inSea, glass, buoy)};
  const auto isFalse{[&](const MediumStack *stack,
                         const MediumBoundary &boundary,
                         const smdl::jit::Material &material, bool isLeaving) {
    return MediumStack::isFalseCrossing(stack, boundary, material.def,
                                        isLeaving);
  }};
  SUBCASE("Entering") {
    CHECK_FALSE(isFalse(inSea, buoy, glass, false));
    CHECK(isFalse(inBuoy, marble, flint, false));
    // Into the dominant medium itself, as a mesh that overlaps itself.
    CHECK(isFalse(inBuoy, buoy, glass, false));
    // Nothing outranks what an empty stack holds.
    CHECK_FALSE(isFalse(nullptr, marble, flint, false));
    // A surface that bounds no medium is a surface wherever it is.
    CHECK_FALSE(isFalse(inBuoy, wall, glass, false));
  }
  SUBCASE("Leaving") {
    // The waterline inside the float.
    CHECK(isFalse(inBuoy, sea, water, true));
    // The float, into the water.
    CHECK_FALSE(isFalse(inBuoy, buoy, glass, true));
    // The float with no water around it, into the air.
    CHECK_FALSE(
        isFalse(fixture.enter(nullptr, glass, buoy), buoy, glass, true));
    // A boundary the walk never entered, as one that began inside it.
    CHECK_FALSE(isFalse(inSea, marble, flint, true));
    // One copy of the dominant medium where another stays.
    const MediumStack *twice{fixture.enter(inBuoy, glass, buoy)};
    CHECK(isFalse(twice, buoy, glass, true));
  }
  SUBCASE("Additive media") {
    const MediumBoundary &plume{fixture.boundary(0)};
    // Below the dominant entry's rank, entering is false.
    CHECK(isFalse(inBuoy, plume, smoke, false));
    CHECK_FALSE(isFalse(inSea, plume, smoke, false));
    // At its rank, active above it and inactive under it.
    CHECK_FALSE(
        isFalse(fixture.enter(inSea, smoke, plume), plume, smoke, true));
    CHECK(
        isFalse(fixture.enter(fixture.enter(nullptr, smoke, plume), water, sea),
                plume, smoke, true));
  }
}

TEST_CASE("MediumStack: the active entries are the dominant one and the "
          "additive ones above it") {
  StackFixture fixture{};
  const smdl::jit::Material &water{fixture.material("stack_water")};
  const smdl::jit::Material &smoke{fixture.material("stack_smoke")};
  const MediumBoundary &sea{fixture.boundary(0)};
  const MediumBoundary &plume{fixture.boundary(0)};
  const MediumBoundary &highPlume{fixture.boundary(1)};
  using Entries = std::vector<const MediumStack *>;
  SUBCASE("At equal priority, the additive entry entered last adds") {
    const MediumStack *seaFirst{fixture.enter(nullptr, water, sea)};
    const MediumStack *plumeOver{fixture.enter(seaFirst, smoke, plume)};
    CHECK(activeOf(plumeOver) == Entries{plumeOver, seaFirst});
    const MediumStack *plumeFirst{fixture.enter(nullptr, smoke, plume)};
    const MediumStack *seaOver{fixture.enter(plumeFirst, water, sea)};
    CHECK(activeOf(seaOver) == Entries{seaOver});
  }
  SUBCASE("Ranked above it, the additive entry adds from either side") {
    const MediumStack *seaFirst{fixture.enter(nullptr, water, sea)};
    const MediumStack *plumeOver{fixture.enter(seaFirst, smoke, highPlume)};
    CHECK(activeOf(plumeOver) == Entries{plumeOver, seaFirst});
    const MediumStack *plumeFirst{fixture.enter(nullptr, smoke, highPlume)};
    const MediumStack *seaOver{fixture.enter(plumeFirst, water, sea)};
    CHECK(activeOf(seaOver) == Entries{seaOver, plumeFirst});
  }
  SUBCASE("With nothing dominant, every additive entry adds") {
    const MediumStack *first{fixture.enter(nullptr, smoke, highPlume)};
    const MediumStack *second{fixture.enter(first, smoke, plume)};
    CHECK(activeOf(second) == Entries{second, first});
  }
}

TEST_CASE("MediumStack: a transmitting pair crosses by the geometry normal") {
  StackFixture fixture{};
  const smdl::jit::Material &glass{fixture.material("stack_glass")};
  const MediumBoundary &buoy{fixture.boundary(0)};
  const MediumBoundary &wall{fixture.boundary(0, /*boundsMedium=*/false)};
  SUBCASE("Inward enters and outward leaves") {
    const MediumStack *stack{};
    MediumStack::update(stack, fixture.allocator, &glass, buoy, OUTSIDE,
                        INSIDE);
    REQUIRE(stack);
    CHECK(stack->boundary == buoy.id);
    CHECK(stack->material == &glass);
    MediumStack::update(stack, fixture.allocator, &glass, buoy, INSIDE,
                        OUTSIDE);
    CHECK(stack == nullptr);
  }
  SUBCASE("A pair on one side, or a boundary of no medium, does nothing") {
    const MediumStack *stack{};
    MediumStack::update(stack, fixture.allocator, &glass, buoy, OUTSIDE,
                        OUTSIDE);
    CHECK(stack == nullptr);
    MediumStack::update(stack, fixture.allocator, &glass, wall, OUTSIDE,
                        INSIDE);
    CHECK(stack == nullptr);
  }
  SUBCASE("The segment leaving a vertex starts on the side it goes") {
    const MediumStack *inside{
        smdl::mediumToward(nullptr, fixture.allocator, &glass, buoy, OUTSIDE,
                           INSIDE)};
    REQUIRE(inside);
    CHECK(inside->boundary == buoy.id);
    CHECK(smdl::mediumToward(nullptr, fixture.allocator, &glass, buoy,
                             OUTSIDE, OUTSIDE) == nullptr);
    CHECK(smdl::mediumToward(inside, fixture.allocator, nullptr, buoy, OUTSIDE,
                             INSIDE) == inside);
  }
}

TEST_CASE("MediumStack: the exterior index is the dominant entry's on the "
          "far side") {
  StackFixture fixture{};
  const smdl::jit::Material &glass{fixture.material("stack_glass")};
  const smdl::jit::Material &water{fixture.material("stack_water")};
  REQUIRE(glass.isInterior(INSIDE));
  REQUIRE_FALSE(glass.isInterior(OUTSIDE));
  const MediumBoundary &sea{fixture.boundary(0)};
  const MediumBoundary &buoy{fixture.boundary(1)};
  const MediumBoundary &wall{fixture.boundary(0, /*boundsMedium=*/false)};
  const MediumStack *inSea{fixture.enter(nullptr, water, sea)};
  const MediumStack *inBuoy{fixture.enter(inSea, glass, buoy)};
  // Entering the float from the water, and leaving it into the water.
  CHECK(smdl::exteriorIOR(inSea, glass, buoy, OUTSIDE) ==
        doctest::Approx(1.33f));
  CHECK(smdl::exteriorIOR(inBuoy, glass, buoy, INSIDE) ==
        doctest::Approx(1.33f));
  // Above the waterline the water is left inside the float, so leaving
  // the float there is into the air.
  const MediumStack *aboveWater{fixture.leave(inBuoy, water, sea)};
  CHECK(smdl::exteriorIOR(aboveWater, glass, buoy, INSIDE) == 1.0f);
  // The float entered from the air, and then the water entered falsely
  // inside it: leaving the float there is into the water.
  const MediumStack *belowWater{
      fixture.enter(fixture.enter(nullptr, glass, buoy), water, sea)};
  CHECK(smdl::exteriorIOR(belowWater, glass, buoy, INSIDE) ==
        doctest::Approx(1.33f));
  // Either side of a surface that bounds no medium, the medium the ray
  // is in, and nothing at all with no stack.
  CHECK(smdl::exteriorIOR(inBuoy, glass, wall, INSIDE) ==
        doctest::Approx(1.5f));
  CHECK(smdl::exteriorIOR(inBuoy, glass, wall, OUTSIDE) ==
        doctest::Approx(1.5f));
  CHECK(smdl::exteriorIOR(nullptr, glass, buoy, OUTSIDE) == 1.0f);
}

TEST_CASE("MediumStack: a walk can cross falsely only past what it cannot "
          "see") {
  StackFixture fixture{};
  const smdl::jit::Material &glass{fixture.material("stack_glass")};
  const smdl::jit::Material &water{fixture.material("stack_water")};
  const MediumBoundary &sea{fixture.boundary(0)};
  const MediumBoundary &marble{fixture.boundary(0)};
  const MediumBoundary &buoy{fixture.boundary(1)};
  constexpr int LOWEST{0};
  CHECK_FALSE(MediumStack::canCrossFalsely(nullptr, LOWEST));
  const MediumStack *inSea{fixture.enter(nullptr, water, sea)};
  CHECK_FALSE(MediumStack::canCrossFalsely(inSea, LOWEST));
  // A marble in the water hides the water, which can be left falsely.
  CHECK(MediumStack::canCrossFalsely(fixture.enter(inSea, glass, marble),
                                     LOWEST));
  // A float outranks what the walk can enter inside it.
  CHECK(MediumStack::canCrossFalsely(fixture.enter(nullptr, glass, buoy),
                                     LOWEST));
  CHECK_FALSE(
      MediumStack::canCrossFalsely(fixture.enter(nullptr, glass, buoy), 1));
}

TEST_CASE("MediumStack: leaving under a medium of equal priority is a tie") {
  StackFixture fixture{};
  const smdl::jit::Material &glass{fixture.material("stack_glass")};
  const smdl::jit::Material &water{fixture.material("stack_water")};
  const MediumBoundary &sea{fixture.boundary(0)};
  const MediumBoundary &marble{fixture.boundary(0)};
  const MediumBoundary &buoy{fixture.boundary(1)};
  const MediumStack *inSea{fixture.enter(nullptr, water, sea)};
  const MediumStack *inMarble{fixture.enter(inSea, glass, marble)};
  CHECK(MediumStack::tiedAbove(inMarble, inSea) == inMarble);
  CHECK(MediumStack::tiedAbove(inMarble, inMarble) == nullptr);
  CHECK(MediumStack::tiedAbove(fixture.enter(inSea, glass, buoy), inSea) ==
        nullptr);
  // The same medium entered twice is one medium, and no tie.
  const MediumStack *twice{fixture.enter(inMarble, glass, marble)};
  CHECK(MediumStack::tiedAbove(twice, inMarble) == nullptr);
}

TEST_CASE("MediumTies: the count sums and the first pair is the least") {
  MediumTies ties{};
  CHECK(ties.count() == 0);
  ties.add(3, MediumTies::pairOf(7, 2));
  ties.add(2, MediumTies::pairOf(4, 9));
  ties.add(1, MediumTies::pairOf(3, 2));
  CHECK(ties.count() == 6);
  CHECK(ties.firstPair() == std::pair<uint32_t, uint32_t>(2, 3));
}
