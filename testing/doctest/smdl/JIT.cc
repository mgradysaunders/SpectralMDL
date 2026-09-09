/// \file
/// The entry points a host calls on a compiled material, and the static
/// flags that say which of them are worth calling: the surface and volume
/// evaluations, the displacement, the hair scattering, the opt-in
/// scatter-normal hooks, the per-side lobe words, and the state fields a
/// material reads.
///
/// `include/smdl/JIT.h` declares all of it and has no `lib/` source of
/// its own, so this mirrors the header the way `RenderUtil/FastMath.cc`
/// and `Support/VectorMath.cc` do.

#include "CompileFixtures.h"

#include <cmath>
#include <cstdint>
#include <string>
#include <vector>

#include "smdl/Compiler.h"
#include "smdl/Manifold.h"

TEST_CASE("MaterialDef: the flags a compile can prove") {
  TempDir tmpDir{"compiler-flags"};
  tmpDir.write(
      "root/mats.mdl",
      "#smdl\n"
      "import ::df::*;\n"
      "import ::state::*;\n"
      "import ::scene::*;\n"
      "export material mat_default() = material();\n"
      "export material mat_plastic() = material(\n"
      "  surface: material_surface(\n"
      "    scattering: df::diffuse_reflection_bsdf(tint: 0.8)));\n"
      "export material mat_cutout_const() = material(\n"
      "  geometry: material_geometry(cutout_opacity: 0.5));\n"
      "export material mat_cutout_folds() = material(\n"
      "  geometry: material_geometry(cutout_opacity: 0.25 + 0.75));\n"
      "export material mat_cutout_runtime() = material(\n"
      "  geometry: material_geometry(\n"
      "    cutout_opacity: scene::data_lookup_float(\"opacity\", 1.0)));\n"
      "export material mat_thin() = material(thin_walled: true);\n"
      "export material mat_thin_runtime() = material(\n"
      "  thin_walled: state::position().x > 0.0);\n"
      "export material mat_volume() = material(\n"
      "  volume: material_volume(absorption_coefficient: color(0.5)));\n"
      "export material mat_volume_additive() = material(\n"
      "  ior: 1.0,\n"
      "  volume: material_volume(\n"
      "    scattering_coefficient: color(0.5),\n"
      "    additive: true));\n"
      "export material mat_volume_surface() = material(\n"
      "  ior: 1.33,\n"
      "  surface: material_surface(\n"
      "    scattering: df::specular_bsdf(mode: "
      "df::scatter_reflect_transmit)),\n"
      "  volume: material_volume(absorption_coefficient: color(0.5)));\n"
      "export material mat_emissive() = material(\n"
      "  surface: material_surface(\n"
      "    scattering: df::diffuse_reflection_bsdf(),\n"
      "    emission: material_emission(emission: df::diffuse_edf())));\n");
  smdl::Compiler compiler{};
  REQUIRE_OK(compiler.add((tmpDir / "root").string()));
  REQUIRE_OK(compiler.compile(smdl::OPT_LEVEL_O2));
  REQUIRE_OK(compiler.jitCompile());
  // The six '#isDefault'-derived structural bits are always known, and
  // at -O2 the constant-foldable value bits are too, including the
  // heterogeneous-volume bit (every material here has a constant, or
  // no, volume, so the '.volumeEvaluate' body folds away from the
  // state and homogeneity is proven), the displacement bit (every
  // material here has a constant, in fact default, displacement, so
  // the '.displacementProbe' body folds to the zero vector), and the
  // normal-remap bit (every material here keeps the state normal, so
  // the '.normalProbe' body folds to the zero vector too).
  constexpr int structuralBits{
      smdl::MATERIAL_HAS_SURFACE | smdl::MATERIAL_HAS_BACKFACE |
      smdl::MATERIAL_HAS_SURFACE_EMISSION |
      smdl::MATERIAL_HAS_BACKFACE_EMISSION | smdl::MATERIAL_HAS_VOLUME |
      smdl::MATERIAL_HAS_HAIR};
  constexpr int allBits{
      structuralBits | smdl::MATERIAL_THIN_WALLED | smdl::MATERIAL_HAS_CUTOUT |
      smdl::MATERIAL_HAS_HETEROGENEOUS_VOLUME |
      smdl::MATERIAL_HAS_DISPLACEMENT | smdl::MATERIAL_REMAPS_NORMAL};
  SUBCASE("Structural and constant-foldable bits are known") {
    auto matDefault{requireMaterial(compiler, "mat_default")};
    CHECK(matDefault->staticFlagsKnown == allBits);
    CHECK(matDefault->staticFlags == 0);
    CHECK(matDefault->isAlwaysOpaque());
    auto matPlastic{requireMaterial(compiler, "mat_plastic")};
    CHECK(matPlastic->staticFlagsKnown == allBits);
    CHECK(matPlastic->staticFlags == smdl::MATERIAL_HAS_SURFACE);
    CHECK(matPlastic->isAlwaysOpaque());
    auto matCutoutConst{requireMaterial(compiler, "mat_cutout_const")};
    CHECK((matCutoutConst->staticFlagsKnown & smdl::MATERIAL_HAS_CUTOUT) != 0);
    CHECK((matCutoutConst->staticFlags & smdl::MATERIAL_HAS_CUTOUT) != 0);
    CHECK(!matCutoutConst->isAlwaysOpaque());
    auto matCutoutFolds{requireMaterial(compiler, "mat_cutout_folds")};
    CHECK(matCutoutFolds->isAlwaysOpaque());
    auto matThin{requireMaterial(compiler, "mat_thin")};
    CHECK((matThin->staticFlagsKnown & smdl::MATERIAL_THIN_WALLED) != 0);
    CHECK((matThin->staticFlags & smdl::MATERIAL_THIN_WALLED) != 0);
    auto matVolume{requireMaterial(compiler, "mat_volume")};
    CHECK(matVolume->hasVolume());
    CHECK(matVolume->hasHomogeneousVolume());
    // Not opaque because it passes shadow rays through, not because it
    // has a volume: the volume-with-surface material below blocks at
    // every hit and is opaque despite its interior.
    CHECK(matVolume->isNullInterface());
    CHECK(!matVolume->isAlwaysOpaque());
    auto matVolumeSurface{requireMaterial(compiler, "mat_volume_surface")};
    CHECK(matVolumeSurface->hasVolume());
    CHECK(!matVolumeSurface->isNullInterface());
    CHECK(matVolumeSurface->isAlwaysOpaque());
    auto matEmissive{requireMaterial(compiler, "mat_emissive")};
    CHECK((matEmissive->staticFlags & smdl::MATERIAL_HAS_SURFACE_EMISSION) !=
          0);
    CHECK(matEmissive->isAlwaysOpaque());
  }
  SUBCASE("Runtime-dependent bits degrade to unknown") {
    auto matCutoutRuntime{requireMaterial(compiler, "mat_cutout_runtime")};
    CHECK(matCutoutRuntime->staticFlagsKnown ==
          (allBits & ~smdl::MATERIAL_HAS_CUTOUT));
    CHECK(!matCutoutRuntime->isAlwaysOpaque());
    auto matThinRuntime{requireMaterial(compiler, "mat_thin_runtime")};
    CHECK(matThinRuntime->staticFlagsKnown ==
          (allBits & ~smdl::MATERIAL_THIN_WALLED));
  }
  SUBCASE("Instances satisfy the static-flags invariant") {
    StateStorage storage{compiler};
    auto state{storage.makeState()};
    for (const auto &materialDef : compiler.getMaterials()) {
      auto materialEval{smdl::JIT::Material(state, &materialDef)};
      CHECK((materialEval.eval.flags & materialDef.staticFlagsKnown) ==
            materialDef.staticFlags);
    }
    // 'opacityEvaluate' agrees with the full evaluation and requires no
    // allocator.
    auto stateNoAlloc{state};
    stateNoAlloc.allocator = nullptr;
    for (const auto &materialDef : compiler.getMaterials()) {
      auto materialEval{smdl::JIT::Material(state, &materialDef)};
      CHECK(materialDef.opacityEvaluate(stateNoAlloc) ==
            materialEval.getCutoutOpacity());
    }
    CHECK(requireMaterial(compiler, "mat_default")
              ->opacityEvaluate(stateNoAlloc) == 1.0f);
    CHECK(requireMaterial(compiler, "mat_cutout_const")
              ->opacityEvaluate(stateNoAlloc) == 0.5f);
    // The additive-volume declaration reaches the instance flags.
    auto instAdditive{smdl::JIT::Material(
        state, requireMaterial(compiler, "mat_volume_additive"))};
    CHECK(instAdditive.hasAdditiveVolume());
    auto instReplacing{
        smdl::JIT::Material(state, requireMaterial(compiler, "mat_volume"))};
    CHECK(!instReplacing.hasAdditiveVolume());
  }
}

TEST_CASE("volumeEvaluate: the coefficients along a position") {
  TempDir tmpDir{"volume-evaluate"};
  // A 32x8x4 Mitsuba volume holding the linear field
  // 'value = x + 10*y + 100*z', so trilinear filtering reproduces it
  // exactly and the maximum is 31 + 70 + 300 = 401.
  {
    auto bytes{std::string("VOL")};
    const auto append{[&](const void *data, size_t size) {
      bytes.append(static_cast<const char *>(data), size);
    }};
    const char version{3};
    append(&version, 1);
    const int32_t header[5]{1, 32, 8, 4, 1};
    append(header, sizeof(header));
    const float bound[6]{0.0f, 0.0f, 0.0f, 1.0f, 1.0f, 1.0f};
    append(bound, sizeof(bound));
    for (int z = 0; z < 4; z++)
      for (int y = 0; y < 8; y++)
        for (int x = 0; x < 32; x++) {
          const float value{float(x) + 10.0f * float(y) + 100.0f * float(z)};
          append(&value, sizeof(value));
        }
    tmpDir.write("root/linear.vol", bytes);
  }
  tmpDir.write(
      "root/vols.mdl",
      "#smdl\n"
      "import ::df::*;\n"
      "import ::state::*;\n"
      "import ::tex::*;\n"
      "export material vol_homog() = material(\n"
      "  ior: 1.0,\n"
      "  volume: material_volume(\n"
      "    scattering: df::anisotropic_vdf(),\n"
      "    absorption_coefficient: color(0.5),\n"
      "    scattering_coefficient: color(2.0),\n"
      "    max_scattering_coefficient: color(2.0)));\n"
      "export material vol_hetero() = material(\n"
      "  ior: 1.0,\n"
      "  volume: material_volume(\n"
      "    scattering: df::anisotropic_vdf(),\n"
      "    scattering_coefficient: 4.0 *\n"
      "      tex::lookup_float(texture_3d(\"linear.vol\"),\n"
      "                        state::position()) * color(1.0),\n"
      "    max_scattering_coefficient: 4.0 *\n"
      "      tex::max_value(texture_3d(\"linear.vol\")) * color(1.0)));\n"
      "export material vol_none() = material();\n"
      "export material vol_fire() = material(\n"
      "  ior: 1.0,\n"
      "  volume: material_volume(\n"
      "    scattering: df::anisotropic_vdf(),\n"
      "    absorption_coefficient: 2.0 *\n"
      "      tex::lookup_float(texture_3d(\"linear.vol\"),\n"
      "                        state::position()) * color(1.0),\n"
      "    emission_intensity: 0.25 *\n"
      "      tex::lookup_float(texture_3d(\"linear.vol\"),\n"
      "                        state::position()) * color(1.0),\n"
      "    max_absorption_coefficient: 2.0 *\n"
      "      tex::max_value(texture_3d(\"linear.vol\")) * color(1.0)));\n"
      "export material vol_hinted() = material(\n"
      "  ior: 1.0,\n"
      "  volume: material_volume(\n"
      "    scattering: df::anisotropic_vdf(),\n"
      "    scattering_coefficient: 4.0 *\n"
      "      tex::lookup_float(texture_3d(\"linear.vol\"),\n"
      "                        state::position()) * color(1.0),\n"
      "    max_scattering_coefficient: 4.0 *\n"
      "      tex::max_value(texture_3d(\"linear.vol\")) * color(1.0),\n"
      "    density: texture_3d(\"linear.vol\"),\n"
      "    density_bound_min: float3(0.0),\n"
      "    density_bound_max: float3(1.0)));\n");
  smdl::Compiler compiler{};
  REQUIRE_OK(compiler.add((tmpDir / "root").string()));
  REQUIRE_OK(compiler.compile(smdl::OPT_LEVEL_O2));
  REQUIRE_OK(compiler.jitCompile());
  const auto N{size_t(compiler.wavelengthBaseMax)};
  auto sigmaA{std::vector<float>(N)};
  auto sigmaS{std::vector<float>(N)};
  auto emission{std::vector<float>(N)};
  // 'volumeEvaluate' is allocation-free, so the partial state carries
  // no allocator: only the object-space position identifies the query.
  auto state{smdl::State()};
  SUBCASE("Homogeneous coefficients are position-independent") {
    auto materialDef{requireMaterial(compiler, "vol_homog")};
    CHECK(materialDef->hasVolume());
    CHECK(materialDef->hasHomogeneousVolume());
    materialDef->volumeEvaluate(state, sigmaA.data(), sigmaS.data(),
                                emission.data());
    for (size_t i = 0; i < N; i++) {
      CHECK(sigmaA[i] == 0.5f);
      CHECK(sigmaS[i] == 2.0f);
    }
  }
  SUBCASE("Heterogeneous coefficients follow the position") {
    auto materialDef{requireMaterial(compiler, "vol_hetero")};
    CHECK(materialDef->hasVolume());
    CHECK(!materialDef->hasHomogeneousVolume());
    // The center of voxel (3, 4, 2) has the exactly representable
    // texture coordinate below, where the field is 243.
    state.position = smdl::float3(3.5f / 32.0f, 4.5f / 8.0f, 2.5f / 4.0f);
    materialDef->volumeEvaluate(state, sigmaA.data(), sigmaS.data(),
                                emission.data());
    for (size_t i = 0; i < N; i++) {
      CHECK(sigmaA[i] == 0.0f);
      CHECK(sigmaS[i] == 4.0f * 243.0f);
    }
    // The center of voxel (0, 0, 0), where the field is 0.
    state.position = smdl::float3(0.5f / 32.0f, 0.5f / 8.0f, 0.5f / 4.0f);
    materialDef->volumeEvaluate(state, sigmaA.data(), sigmaS.data(),
                                emission.data());
    for (size_t i = 0; i < N; i++) CHECK(sigmaS[i] == 0.0f);
  }
  SUBCASE("Emission follows the position and absent emission is zero") {
    auto materialDef{requireMaterial(compiler, "vol_fire")};
    CHECK(materialDef->hasVolume());
    // The center of voxel (3, 4, 2), where the linear field is 243:
    // emission is 0.25 times the field, absorption 2 times it.
    state.position = smdl::float3(3.5f / 32.0f, 4.5f / 8.0f, 2.5f / 4.0f);
    materialDef->volumeEvaluate(state, sigmaA.data(), sigmaS.data(),
                                emission.data());
    for (size_t i = 0; i < N; i++) {
      CHECK(sigmaA[i] == 2.0f * 243.0f);
      CHECK(sigmaS[i] == 0.0f);
      CHECK(emission[i] == 0.25f * 243.0f);
    }
    // A material that declares no emission resolves it to zero.
    auto hetero{requireMaterial(compiler, "vol_hetero")};
    state.position = smdl::float3(3.5f / 32.0f, 4.5f / 8.0f, 2.5f / 4.0f);
    hetero->volumeEvaluate(state, sigmaA.data(), sigmaS.data(),
                           emission.data());
    for (size_t i = 0; i < N; i++) CHECK(emission[i] == 0.0f);
  }
  SUBCASE("No volume evaluates to zero and proves homogeneous") {
    auto materialDef{requireMaterial(compiler, "vol_none")};
    CHECK(!materialDef->hasVolume());
    CHECK(materialDef->hasHomogeneousVolume());
    materialDef->volumeEvaluate(state, sigmaA.data(), sigmaS.data(),
                                emission.data());
    for (size_t i = 0; i < N; i++) {
      CHECK(sigmaA[i] == 0.0f);
      CHECK(sigmaS[i] == 0.0f);
    }
  }
  SUBCASE("Instances expose the density acceleration hint") {
    StateStorage storage{compiler};
    auto fullState{storage.makeState()};
    // A material with the complete hint exposes the grid resource and
    // both corners of the bound box through the instance.
    auto hinted{smdl::JIT::Material(fullState,
                                    requireMaterial(compiler, "vol_hinted"))};
    const auto *grid{hinted.getVolumeDensityGrid()};
    REQUIRE(grid != nullptr);
    CHECK(grid->isValid());
    CHECK(grid->getExtent().x == 32);
    CHECK(grid->getExtent().y == 8);
    CHECK(grid->getExtent().z == 4);
    CHECK(grid->getMaxValue() == 401.0f);
    REQUIRE(hinted.getVolumeDensityBoundMin() != nullptr);
    REQUIRE(hinted.getVolumeDensityBoundMax() != nullptr);
    CHECK(hinted.getVolumeDensityBoundMin()->x == 0.0f);
    CHECK(hinted.getVolumeDensityBoundMax()->x == 1.0f);
    CHECK(hinted.getVolumeDensityBoundMax()->z == 1.0f);
    // The majorant bounds behind the hint cover the extent and bracket
    // the field: some cell reports the peak, none exceeds it, and the
    // same holds below.
    const int E{grid->getMajorantExtent()};
    const auto cells{grid->getMajorantCount()};
    CHECK(cells.x == (32 + E - 1) / E);
    CHECK(cells.y == (8 + E - 1) / E);
    CHECK(cells.z == (4 + E - 1) / E);
    float cellMax{-INFINITY}, cellMin{INFINITY};
    for (int cz = 0; cz < cells.z; cz++)
      for (int cy = 0; cy < cells.y; cy++)
        for (int cx = 0; cx < cells.x; cx++) {
          const auto bounds{grid->getMajorantBounds(cx, cy, cz)};
          cellMax = std::max(cellMax, bounds.y);
          cellMin = std::min(cellMin, bounds.x);
        }
    CHECK(cellMax == grid->getMaxValue());
    CHECK(cellMin == grid->getMinValue());
    // A material without the hint reports null pointers.
    auto unhinted{smdl::JIT::Material(fullState,
                                      requireMaterial(compiler, "vol_hetero"))};
    CHECK(unhinted.getVolumeDensityGrid() == nullptr);
    CHECK(unhinted.getVolumeDensityBoundMin() == nullptr);
    CHECK(unhinted.getVolumeDensityBoundMax() == nullptr);
  }
  SUBCASE("Instances expose the declared majorants") {
    StateStorage storage{compiler};
    auto fullState{storage.makeState()};
    auto homog{
        smdl::JIT::Material(fullState, requireMaterial(compiler, "vol_homog"))};
    REQUIRE(homog.getMaxScatteringCoefficient().size() == N);
    CHECK(homog.getMaxAbsorptionCoefficient().empty());
    for (size_t i = 0; i < N; i++)
      CHECK(homog.getMaxScatteringCoefficient()[i] == 2.0f);
    // The heterogeneous majorant is exact through 'tex::max_value'.
    auto hetero{smdl::JIT::Material(fullState,
                                    requireMaterial(compiler, "vol_hetero"))};
    REQUIRE(hetero.getMaxScatteringCoefficient().size() == N);
    for (size_t i = 0; i < N; i++)
      CHECK(hetero.getMaxScatteringCoefficient()[i] == 4.0f * 401.0f);
  }
}

TEST_CASE("displacementEvaluate: a displaced and an undisplaced material") {
  TempDir tmpDir{"displacement"};
  tmpDir.write("root/disp.mdl",
               "#smdl\n"
               "import ::state::*;\n"
               "export material disp_none() = material();\n"
               "export material disp_const() = material(\n"
               "  geometry: material_geometry(\n"
               "    displacement: float3(0.0, 0.0, 0.25)));\n"
               "export material disp_state() = material(\n"
               "  geometry: material_geometry(\n"
               "    displacement: state::texture_coordinate(0).x *\n"
               "      float3(0.0, 0.0, 1.0)));\n");
  smdl::Compiler compiler{};
  REQUIRE_OK(compiler.add((tmpDir / "root").string()));
  REQUIRE_OK(compiler.compile(smdl::OPT_LEVEL_O2));
  REQUIRE_OK(compiler.jitCompile());
  // 'displacementEvaluate' is allocation-free, so the partial state
  // carries no allocator, exactly as with 'volumeEvaluate'.
  auto state{smdl::State()};
  auto displacement{smdl::float3()};
  SUBCASE("The default material is provably undisplaced") {
    auto materialDef{requireMaterial(compiler, "disp_none")};
    CHECK(materialDef->hasZeroDisplacement());
    CHECK((materialDef->staticFlagsKnown & smdl::MATERIAL_HAS_DISPLACEMENT) !=
          0);
    CHECK((materialDef->staticFlags & smdl::MATERIAL_HAS_DISPLACEMENT) == 0);
    materialDef->displacementEvaluate(state, displacement);
    CHECK(displacement.x == 0.0f);
    CHECK(displacement.y == 0.0f);
    CHECK(displacement.z == 0.0f);
  }
  SUBCASE("A constant displacement is provably non-zero") {
    auto materialDef{requireMaterial(compiler, "disp_const")};
    CHECK(!materialDef->hasZeroDisplacement());
    CHECK((materialDef->staticFlagsKnown & smdl::MATERIAL_HAS_DISPLACEMENT) !=
          0);
    CHECK((materialDef->staticFlags & smdl::MATERIAL_HAS_DISPLACEMENT) != 0);
    materialDef->displacementEvaluate(state, displacement);
    CHECK(displacement.x == 0.0f);
    CHECK(displacement.y == 0.0f);
    CHECK(displacement.z == 0.25f);
  }
  SUBCASE("A state-dependent displacement is unknown, not proven zero") {
    auto materialDef{requireMaterial(compiler, "disp_state")};
    CHECK(!materialDef->hasZeroDisplacement());
    CHECK((materialDef->staticFlagsKnown & smdl::MATERIAL_HAS_DISPLACEMENT) ==
          0);
    state.textureCoordinate[0] = smdl::float3(2.5f, 0.0f, 0.0f);
    materialDef->displacementEvaluate(state, displacement);
    CHECK(displacement.x == 0.0f);
    CHECK(displacement.y == 0.0f);
    CHECK(displacement.z == 2.5f);
  }
}

TEST_CASE("hairScatterEvaluate: a hair material and the default one") {
  TempDir tmpDir{"hair"};
  tmpDir.write("root/hair.mdl", "#smdl\n"
                                "import ::df::*;\n"
                                "export material hair_brown() = material(\n"
                                "  hair: df::chiang_hair_bsdf(\n"
                                "    roughness_R: float2(0.3, 0.4),\n"
                                "    absorption_coefficient: color(0.4)));\n"
                                "export material hair_none() = material();\n");
  smdl::Compiler compiler{};
  REQUIRE_OK(compiler.add((tmpDir / "root").string()));
  REQUIRE_OK(compiler.compile(smdl::OPT_LEVEL_O2));
  REQUIRE_OK(compiler.jitCompile());
  StateStorage storage{compiler};
  auto state{storage.makeState()};
  // The default state's tangent-to-world is identity, so the directions
  // below are written directly in the hair frame: X is the fiber tangent
  // and Z is the cross-section normal.
  auto wo{smdl::float3(0.0f, 0.6f, 0.8f)};
  auto wi{smdl::float3(0.6f, -0.64f, -0.48f)};
  auto f{std::vector<float>(size_t(compiler.wavelengthBaseMax))};
  auto fSpan{smdl::Span<float>(f.data(), f.size())};
  float pdfFwd{};
  float pdfRev{};
  SUBCASE("A hair material evaluates and samples through the entry points") {
    auto materialDef{requireMaterial(compiler, "hair_brown")};
    CHECK(materialDef->hasHair());
    CHECK((materialDef->staticFlagsKnown & smdl::MATERIAL_HAS_HAIR) != 0);
    CHECK((materialDef->staticFlags & smdl::MATERIAL_HAS_HAIR) != 0);
    auto materialEval{smdl::JIT::Material(state, materialDef)};
    CHECK(materialEval.hasHair());
    CHECK(materialEval.hairScatterEvaluate(wo, wi, pdfFwd, pdfRev, fSpan));
    CHECK(pdfFwd > 0.0f);
    CHECK(pdfRev > 0.0f);
    for (float fValue : f) {
      CHECK(fValue > 0.0f);
      CHECK(std::isfinite(fValue));
    }
    auto xi{smdl::float4(0.3f, 0.4f, 0.5f, 0.6f)};
    auto wiSampled{smdl::float3()};
    CHECK(materialEval.hairScatterSample(xi, wo, wiSampled, pdfFwd, pdfRev,
                                         fSpan));
    CHECK(pdfFwd > 0.0f);
    float lengthSquared{wiSampled.x * wiSampled.x + wiSampled.y * wiSampled.y +
                        wiSampled.z * wiSampled.z};
    CHECK(lengthSquared == doctest::Approx(1.0f).epsilon(1e-3));
  }
  SUBCASE("The default hair BSDF is safe to call and reports black") {
    auto materialDef{requireMaterial(compiler, "hair_none")};
    CHECK(!materialDef->hasHair());
    CHECK((materialDef->staticFlagsKnown & smdl::MATERIAL_HAS_HAIR) != 0);
    CHECK((materialDef->staticFlags & smdl::MATERIAL_HAS_HAIR) == 0);
    auto materialEval{smdl::JIT::Material(state, materialDef)};
    CHECK(!materialEval.hasHair());
    CHECK(!materialEval.hairScatterEvaluate(wo, wi, pdfFwd, pdfRev, fSpan));
    CHECK(pdfFwd == 0.0f);
    CHECK(pdfRev == 0.0f);
    for (float fValue : f) {
      CHECK(fValue == 0.0f);
    }
  }
}

TEST_CASE("scatterNormalSample: the opt-in normal-distribution hooks") {
  // The normal distribution entry points are opt-in, so that a host with no
  // half vector to constrain never pays to emit or optimize them.
  auto buildGlossy{[](smdl::Compiler &compiler, bool shouldEmit) {
    compiler.shouldEmitScatterNormal = shouldEmit;
    REQUIRE_OK(compiler.addCode(
        "::glossy", "#smdl\nimport ::df::*;\nexport material m() = material(\n"
                    "  surface: material_surface(scattering: "
                    "df::microfacet_ggx_smith_bsdf(\n"
                    "    roughness_u: 0.4, tint: 0.8)));\n"));
    REQUIRE_OK(compiler.compile(smdl::OPT_LEVEL_NONE));
    REQUIRE_OK(compiler.jitCompile());
  }};
  SUBCASE("Off by default, and the entry points are absent") {
    auto compiler{smdl::Compiler()};
    buildGlossy(compiler, false);
    auto *materialDef{compiler.findMaterial("m")};
    REQUIRE(materialDef);
    // Absent, not merely unresolved: nothing was emitted to resolve.
    CHECK(materialDef->scatterNormalSample.name.empty());
    CHECK(!materialDef->scatterNormalSample);
    CHECK(!materialDef->scatterNormalEvaluate);
    // Everything else is untouched by the switch.
    CHECK(bool(materialDef->scatterSample));
    CHECK(bool(materialDef->scatterEvaluate));
  }
  SUBCASE("On, and the entry points resolve") {
    auto compiler{smdl::Compiler()};
    buildGlossy(compiler, true);
    auto *materialDef{compiler.findMaterial("m")};
    REQUIRE(materialDef);
    CHECK(bool(materialDef->scatterNormalSample));
    CHECK(bool(materialDef->scatterNormalEvaluate));
    CHECK_CONTAINS(materialDef->scatterNormalSample.name,
                   ".scatterNormalSample");
  }
  SUBCASE("The wrapper takes exactly one glossy kind") {
    // The two-domain mixture is not a distribution any single crossing
    // scatters by, so the wrapper refuses it; the raw entry point still
    // reports the mixture for a caller that wants it.
    auto compiler{smdl::Compiler()};
    buildGlossy(compiler, true);
    auto *materialDef{compiler.findMaterial("m")};
    REQUIRE(materialDef);
    StateStorage storage{compiler};
    auto state{storage.makeState()};
    state.finalize();
    auto inst{smdl::JIT::Material(state, materialDef)};
    const auto xi{smdl::float4(0.25f, 0.5f, 0.5f, 0.5f)};
    auto wm{smdl::float3()};
    auto alpha{smdl::float2()};
    float pdf{};
    CHECK(
        !inst.scatterNormalSample(xi, false, wm, pdf, alpha, smdl::DF_GLOSSY));
    CHECK(!inst.scatterNormalSample(xi, false, wm, pdf, alpha, smdl::DF_ALL));
    CHECK(!inst.scatterNormalEvaluate(false, smdl::float3(0.0f, 0.0f, 1.0f),
                                      pdf, smdl::DF_GLOSSY));
    // And exactly one kind answers: this material is glossy in
    // reflection only, so that kind draws and the other reports nothing.
    CHECK(inst.scatterNormalSample(xi, false, wm, pdf, alpha,
                                   smdl::DF_GLOSSY_BRDF));
    CHECK(pdf > 0.0f);
    CHECK(alpha.x == doctest::Approx(0.16f));
    CHECK(!inst.scatterNormalSample(xi, false, wm, pdf, alpha,
                                    smdl::DF_GLOSSY_BTDF));
  }
  SUBCASE("The normal probe and the geometry normal hook") {
    auto compiler{smdl::Compiler()};
    compiler.shouldEmitScatterNormal = true;
    REQUIRE_OK(compiler.addCode(
        "::remap",
        "#smdl\nimport ::df::*;\nimport ::math::*;\n"
        "export material plain() = material(\n"
        "  surface: material_surface(scattering: "
        "df::microfacet_ggx_smith_bsdf(roughness_u: 0.4, tint: 0.8)));\n"
        "export material remapped() = material(\n"
        "  surface: material_surface(scattering: "
        "df::microfacet_ggx_smith_bsdf(roughness_u: 0.4, tint: 0.8)),\n"
        "  geometry: material_geometry(normal: "
        "math::normalize(float3(0.3, 0.0, 1.0))));\n"));
    REQUIRE_OK(compiler.compile(smdl::OPT_LEVEL_O2));
    REQUIRE_OK(compiler.jitCompile());
    auto *plain{compiler.findMaterial("plain")};
    auto *remapped{compiler.findMaterial("remapped")};
    REQUIRE(plain);
    REQUIRE(remapped);
    // The probe folds 'geometry.normal - $state.normal' at O2, so the
    // flag is known on both sides: provably identity on one, provably
    // remapped on the other.
    CHECK(!plain->canRemapNormal());
    CHECK(remapped->canRemapNormal());
    // And the hook reads the field itself, in internal space.
    auto wavelengths{std::vector<float>(size_t(compiler.wavelengthBaseMax))};
    auto state{smdl::State()};
    state.wavelengthMin = 380.0f;
    state.wavelengthMax = 720.0f;
    state.wavelengthBase = wavelengths.data();
    for (uint32_t i = 0; i < compiler.wavelengthBaseMax; i++) {
      float fac{float(i) / float(compiler.wavelengthBaseMax - 1)};
      wavelengths[i] =
          (1 - fac) * state.wavelengthMin + fac * state.wavelengthMax;
    }
    state.finalize();
    auto normal{smdl::float3()};
    REQUIRE(bool(plain->geometryNormalEvaluate));
    plain->geometryNormalEvaluate(state, normal);
    CHECK(normal.x == doctest::Approx(0.0f));
    CHECK(normal.y == doctest::Approx(0.0f));
    CHECK(normal.z == doctest::Approx(1.0f));
    remapped->geometryNormalEvaluate(state, normal);
    const float invLen{1.0f / std::sqrt(1.09f)};
    CHECK(normal.x == doctest::Approx(0.3f * invLen));
    CHECK(normal.y == doctest::Approx(0.0f));
    CHECK(normal.z == doctest::Approx(1.0f * invLen));
  }
  SUBCASE("A remap bars a claim only over a df node given its own normal") {
    auto compiler{smdl::Compiler()};
    compiler.shouldEmitScatterNormal = true;
    REQUIRE_OK(compiler.addCode(
        "::claim",
        "#smdl\nimport ::df::*;\nimport ::math::*;\n"
        "const auto N = math::normalize(float3(0.3, 0.0, 1.0));\n"
        "export material inherits() = material(\n"
        "  surface: material_surface(scattering: df::fresnel_layer(ior: 1.5,\n"
        "    layer: df::specular_bsdf(mode: df::scatter_reflect),\n"
        "    base: df::diffuse_reflection_bsdf(tint: 0.8))),\n"
        "  geometry: material_geometry(normal: N));\n"
        "export material pinned() = material(\n"
        "  surface: material_surface(scattering: df::fresnel_layer(ior: 1.5,\n"
        "    layer: df::specular_bsdf(mode: df::scatter_reflect),\n"
        "    base: df::diffuse_reflection_bsdf(tint: 0.8),\n"
        "    normal: $state.normal)),\n"
        "  geometry: material_geometry(normal: N));\n"));
    REQUIRE_OK(compiler.compile(smdl::OPT_LEVEL_O2));
    REQUIRE_OK(compiler.jitCompile());
    auto *inheritsMaterial{compiler.findMaterial("inherits")};
    auto *pinnedMaterial{compiler.findMaterial("pinned")};
    REQUIRE(inheritsMaterial);
    REQUIRE(pinnedMaterial);
    CHECK(inheritsMaterial->canRemapNormal());
    CHECK(pinnedMaterial->canRemapNormal());
    StateStorage storage{compiler};
    auto state{storage.makeState()};
    state.finalize();
    auto inherits{smdl::JIT::Material(state, inheritsMaterial)};
    auto pinned{smdl::JIT::Material(state, pinnedMaterial)};
    // A defaulted layer normal follows the remapped field, so it reports
    // neither property bit and the walk can solve the whole tree.
    CHECK((inherits.getLobes() &
           (smdl::DF_SETS_NORMAL | smdl::DF_CAN_SET_NORMAL)) == 0);
    CHECK(!smdl::manifoldClaim(inherits, /*isMarked=*/true).empty());
    // Spelling out the state normal detaches the layer from the remapped
    // field even though the two values agree here, which is exactly what
    // `smdl::DF_CAN_SET_NORMAL` exists to report.
    CHECK((pinned.getLobes() & smdl::DF_SETS_NORMAL) == 0);
    CHECK((pinned.getLobes() & smdl::DF_CAN_SET_NORMAL) != 0);
    CHECK(smdl::manifoldClaim(pinned, /*isMarked=*/true).empty());
  }
  SUBCASE("The remap flag degrades to unproven without optimization") {
    auto compiler{smdl::Compiler()};
    buildGlossy(compiler, false); // OPT_LEVEL_NONE inside
    auto *materialDef{compiler.findMaterial("m")};
    REQUIRE(materialDef);
    // Nothing folded, so the identity is unproven and the conservative
    // reading is that the material may remap.
    CHECK(materialDef->canRemapNormal());
  }
}

TEST_CASE("MaterialDef: the lobe words per side of the interface") {
  // The `surface` and `backface` trees scatter on their own sides of the
  // interface, so a question that knows its side reads one word and never
  // the union: claiming a kind on the side that cannot produce it bars
  // the ordinary estimators from transport the manifold gather then fails
  // to draw, and the two lose it between them.
  auto compiler{smdl::Compiler()};
  REQUIRE_OK(compiler.addCode(
      "::sides", "#smdl\nimport ::df::*;\n"
                 "export material one_sided() = material(\n"
                 "  surface: material_surface(scattering: "
                 "df::diffuse_reflection_bsdf(tint: 0.8)));\n"
                 "export material two_sided() = material(\n"
                 "  thin_walled: true,\n"
                 "  surface: material_surface(scattering: "
                 "df::diffuse_reflection_bsdf(tint: 0.8)),\n"
                 "  backface: material_surface(scattering: "
                 "df::specular_bsdf(mode: df::scatter_reflect)));\n"));
  REQUIRE_OK(compiler.compile(smdl::OPT_LEVEL_O2));
  REQUIRE_OK(compiler.jitCompile());
  auto *oneSidedMaterial{compiler.findMaterial("one_sided")};
  auto *twoSidedMaterial{compiler.findMaterial("two_sided")};
  REQUIRE(oneSidedMaterial);
  REQUIRE(twoSidedMaterial);
  StateStorage storage{compiler};
  auto state{storage.makeState()};
  state.finalize();
  const auto lobes{[](const smdl::JIT::Material &material, bool isBackface) {
    return material.getLobes(isBackface) & smdl::DF_ALL;
  }};
  // A material with no `backface` initializer scatters by its `surface`
  // from both sides, so the back side reports the surface word and not
  // the empty one the raw `backfaceLobes` field holds.
  auto oneSided{smdl::JIT::Material(state, oneSidedMaterial)};
  CHECK(lobes(oneSided, false) == smdl::DF_SMOOTH_BRDF);
  CHECK(lobes(oneSided, true) == smdl::DF_SMOOTH_BRDF);
  CHECK((oneSided.getLobes() & smdl::DF_ALL) == smdl::DF_SMOOTH_BRDF);
  // A two-sided one distinguishes, and the sideless union is the two
  // together.
  auto twoSided{smdl::JIT::Material(state, twoSidedMaterial)};
  CHECK(lobes(twoSided, false) == smdl::DF_SMOOTH_BRDF);
  CHECK(lobes(twoSided, true) == smdl::DF_DIRAC_BRDF);
  CHECK((twoSided.getLobes() & smdl::DF_ALL) ==
        (smdl::DF_SMOOTH_BRDF | smdl::DF_DIRAC_BRDF));
  // And the claim follows the side: a diffuse front has no kind a walk
  // can solve, so a mark claims nothing there however the back mirrors.
  // The sideless claim is the union of the two, which is what a load-time
  // enumeration of marked instances asks.
  CHECK(smdl::manifoldClaim(twoSided, /*isBackface=*/false, /*isMarked=*/true)
            .empty());
  CHECK(smdl::manifoldClaim(twoSided, /*isBackface=*/true, /*isMarked=*/true)
            .reflectLobes == smdl::DF_DIRAC_BRDF);
  CHECK(smdl::manifoldClaim(twoSided, /*isMarked=*/true).reflectLobes ==
        smdl::DF_DIRAC_BRDF);
}

TEST_CASE("State: the vertex color a material reads through both spellings") {
  // The renderer's contract: the state carries the color and the count,
  // and the host registers "vertex_color" scene data that reads the
  // state, so the extension spelling and the MDL spelling agree at every
  // hit and `data_isvalid` answers per hit. The getter honors the count
  // too, so a lookup where nothing is present keeps its default.
  const auto registerAlias{[](smdl::Compiler &compiler) {
    compiler.sceneData.set(
        "vertex_color",
        [](smdl::State *state, smdl::SceneData::Kind kind, int size,
           void *out) {
          if (state->vertexColorCount > 0 &&
              kind == smdl::SceneData::Kind::Float && (size == 3 || size == 4))
            for (int i = 0; i < size; i++)
              static_cast<float *>(out)[i] = state->vertexColor[0][i];
        },
        [](const smdl::State *state) { return state->vertexColorCount > 0; });
  }};
  const auto run{[&](const char *source, bool isPresent) {
    auto compiler{smdl::Compiler()};
    compiler.shouldEmitUnitTests = true;
    registerAlias(compiler);
    REQUIRE_OK(compiler.addCode("::vertex_color_test", source));
    REQUIRE_OK(compiler.compile(smdl::OPT_LEVEL_NONE));
    REQUIRE_OK(compiler.jitCompile());
    StateStorage storage{compiler};
    auto state{storage.makeState()};
    if (isPresent) {
      state.vertexColorCount = 1;
      state.vertexColor[0] = smdl::float4(0.25f, 0.5f, 0.75f, 1.0f);
    }
    state.finalize();
    CHECK_OK(compiler.runUnitTests(state));
  }};
  SUBCASE("With a color registered") {
    run(R"(#smdl
import ::scene::*;
import ::state::*;
unit_test "Vertex color present" {
  #assert(state::vertex_color_max() == 1);
  #assert(#all(state::vertex_color() == float4(0.25, 0.5, 0.75, 1.0)));
  #assert(scene::data_isvalid("vertex_color"));
  #assert(#all(scene::data_lookup_float4("vertex_color") == float4(0.25, 0.5, 0.75, 1.0)));
  #assert(#all(scene::data_lookup_float3("vertex_color") == float3(0.25, 0.5, 0.75)));
}
)",
        true);
  }
  SUBCASE("With none registered") {
    run(R"(#smdl
import ::scene::*;
import ::state::*;
unit_test "Vertex color absent" {
  #assert(state::vertex_color_max() == 0);
  #assert(#all(state::vertex_color() == float4(1.0, 1.0, 1.0, 1.0)));
  #assert(!scene::data_isvalid("vertex_color"));
  #assert(#all(scene::data_lookup_float4("vertex_color", float4(0.0, 0.0, 0.0, 0.0)) == float4(0.0, 0.0, 0.0, 0.0)));
}
)",
        false);
  }
}
