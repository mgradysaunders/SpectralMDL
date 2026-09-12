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
  // heterogeneous-coefficients and heterogeneous-VDF bits (every
  // material here has a constant, or no, volume, so the '.volumeEvaluate'
  // and '.vdfEvaluate' bodies read nothing point-varying of the state
  // and both prove point-independent), the displacement bit (every
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
      smdl::MATERIAL_HAS_HETEROGENEOUS_COEFFICIENTS |
      smdl::MATERIAL_HAS_HETEROGENEOUS_VDF | smdl::MATERIAL_HAS_DISPLACEMENT |
      smdl::MATERIAL_REMAPS_NORMAL};
  SUBCASE("Structural and constant-foldable bits are known") {
    const smdl::JIT::MaterialDef *matDefault{
        requireMaterial(compiler, "mat_default")};
    CHECK(matDefault->staticFlagsKnown == allBits);
    CHECK(matDefault->staticFlags == 0);
    CHECK(matDefault->isAlwaysOpaque());
    const smdl::JIT::MaterialDef *matPlastic{
        requireMaterial(compiler, "mat_plastic")};
    CHECK(matPlastic->staticFlagsKnown == allBits);
    CHECK(matPlastic->staticFlags == smdl::MATERIAL_HAS_SURFACE);
    CHECK(matPlastic->isAlwaysOpaque());
    const smdl::JIT::MaterialDef *matCutoutConst{
        requireMaterial(compiler, "mat_cutout_const")};
    CHECK((matCutoutConst->staticFlagsKnown & smdl::MATERIAL_HAS_CUTOUT) != 0);
    CHECK((matCutoutConst->staticFlags & smdl::MATERIAL_HAS_CUTOUT) != 0);
    CHECK(!matCutoutConst->isAlwaysOpaque());
    const smdl::JIT::MaterialDef *matCutoutFolds{
        requireMaterial(compiler, "mat_cutout_folds")};
    CHECK(matCutoutFolds->isAlwaysOpaque());
    const smdl::JIT::MaterialDef *matThin{
        requireMaterial(compiler, "mat_thin")};
    CHECK((matThin->staticFlagsKnown & smdl::MATERIAL_THIN_WALLED) != 0);
    CHECK((matThin->staticFlags & smdl::MATERIAL_THIN_WALLED) != 0);
    const smdl::JIT::MaterialDef *matVolume{
        requireMaterial(compiler, "mat_volume")};
    CHECK(matVolume->hasVolume());
    CHECK(matVolume->hasHomogeneousCoefficients());
    CHECK(matVolume->hasHomogeneousVDF());
    CHECK(matVolume->hasHomogeneousVolume());
    // Not opaque because it passes shadow rays through, not because it
    // has a volume: the volume-with-surface material below blocks at
    // every hit and is opaque despite its interior.
    CHECK(matVolume->isNullInterface());
    CHECK(!matVolume->isAlwaysOpaque());
    const smdl::JIT::MaterialDef *matVolumeSurface{
        requireMaterial(compiler, "mat_volume_surface")};
    CHECK(matVolumeSurface->hasVolume());
    CHECK(!matVolumeSurface->isNullInterface());
    CHECK(matVolumeSurface->isAlwaysOpaque());
    const smdl::JIT::MaterialDef *matEmissive{
        requireMaterial(compiler, "mat_emissive")};
    CHECK((matEmissive->staticFlags & smdl::MATERIAL_HAS_SURFACE_EMISSION) !=
          0);
    CHECK(matEmissive->isAlwaysOpaque());
  }
  SUBCASE("Runtime-dependent bits degrade to unknown") {
    const smdl::JIT::MaterialDef *matCutoutRuntime{
        requireMaterial(compiler, "mat_cutout_runtime")};
    CHECK(matCutoutRuntime->staticFlagsKnown ==
          (allBits & ~smdl::MATERIAL_HAS_CUTOUT));
    CHECK(!matCutoutRuntime->isAlwaysOpaque());
    const smdl::JIT::MaterialDef *matThinRuntime{
        requireMaterial(compiler, "mat_thin_runtime")};
    CHECK(matThinRuntime->staticFlagsKnown ==
          (allBits & ~smdl::MATERIAL_THIN_WALLED));
  }
  SUBCASE("Evaluations satisfy the static-flags invariant") {
    StateStorage storage{compiler};
    smdl::State state{storage.makeState()};
    for (const auto &materialDef : compiler.getMaterials()) {
      smdl::JIT::Material material{state, &materialDef};
      CHECK((material.eval.flags & materialDef.staticFlagsKnown) ==
            materialDef.staticFlags);
    }
    // 'opacityEvaluate' agrees with the full evaluation and requires no
    // allocator.
    smdl::State stateNoAlloc{state};
    stateNoAlloc.allocator = nullptr;
    for (const auto &materialDef : compiler.getMaterials()) {
      smdl::JIT::Material material{state, &materialDef};
      CHECK(materialDef.opacityEvaluate(stateNoAlloc) ==
            material.getCutoutOpacity());
    }
    CHECK(requireMaterial(compiler, "mat_default")
              ->opacityEvaluate(stateNoAlloc) == 1.0f);
    CHECK(requireMaterial(compiler, "mat_cutout_const")
              ->opacityEvaluate(stateNoAlloc) == 0.5f);
    // The additive-volume declaration reaches the evaluation's flags.
    smdl::JIT::Material additiveMaterial{smdl::JIT::Material(
        state, requireMaterial(compiler, "mat_volume_additive"))};
    CHECK(additiveMaterial.hasAdditiveVolume());
    smdl::JIT::Material replacingMaterial{
        smdl::JIT::Material(state, requireMaterial(compiler, "mat_volume"))};
    CHECK(!replacingMaterial.hasAdditiveVolume());
  }
}

TEST_CASE("volumeEvaluate and vdfEvaluate: the volume along a position") {
  TempDir tmpDir{"volume-evaluate"};
  // A 32x8x4 Mitsuba volume holding the linear field
  // 'value = x + 10*y + 100*z', so trilinear filtering reproduces it
  // exactly and the maximum is 31 + 70 + 300 = 401.
  {
    std::string bytes{"VOL"};
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
      "import ::math::*;\n"
      "import ::scene::*;\n"
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
      // Constants that read the state without reading the point: an RGB
      // triple and a spectral table are resampled onto the wavelength
      // grid, and the animation time is the path's.
      "export material vol_rgb() = material(\n"
      "  ior: 1.0,\n"
      "  volume: material_volume(\n"
      "    scattering: df::anisotropic_vdf(),\n"
      "    absorption_coefficient: color(0.01, 0.02, 0.05),\n"
      "    scattering_coefficient: color(5.0, 4.0, 3.0)));\n"
      "export material vol_spectral() = material(\n"
      "  ior: 1.0,\n"
      "  volume: material_volume(\n"
      "    scattering: df::anisotropic_vdf(),\n"
      "    scattering_coefficient: math::emission_color(\n"
      "      float[](380.0, 780.0), float[](0.3, 0.9))));\n"
      "export material vol_time() = material(\n"
      "  ior: 1.0,\n"
      "  volume: material_volume(\n"
      "    scattering: df::anisotropic_vdf(),\n"
      "    scattering_coefficient: color(state::animation_time())));\n"
      // Reads that the walk cannot see through: a scene-data getter hands
      // the whole state to a host callback, and a generator draw writes
      // to the state.
      "export material vol_scene() = material(\n"
      "  ior: 1.0,\n"
      "  volume: material_volume(\n"
      "    scattering: df::anisotropic_vdf(),\n"
      "    scattering_coefficient:\n"
      "      color(scene::data_lookup_float(\"density\", 1.0))));\n"
      "export material vol_rng() = material(\n"
      "  ior: 1.0,\n"
      "  volume: material_volume(\n"
      "    scattering: df::anisotropic_vdf(),\n"
      "    scattering_coefficient: color(state::random_float())));\n"
      // Phase functions over constant coefficients: a bias that follows
      // the position, a color tint, the fog fit, a mix of those two, a
      // mix whose weight follows the position, and no VDF at all.
      "export material vol_vdf_pos() = material(\n"
      "  ior: 1.0,\n"
      "  volume: material_volume(\n"
      "    scattering: df::anisotropic_vdf(\n"
      "      directional_bias: 0.5 * math::sin(state::position().x)),\n"
      "    absorption_coefficient: color(0.05),\n"
      "    scattering_coefficient: color(5.0)));\n"
      "export material vol_vdf_tint() = material(\n"
      "  ior: 1.0,\n"
      "  volume: material_volume(\n"
      "    scattering: df::tint(color(0.2, 0.5, 0.9),\n"
      "                         df::anisotropic_vdf(directional_bias: 0.5)),\n"
      "    scattering_coefficient: color(5.0)));\n"
      "export material vol_vdf_fog() = material(\n"
      "  ior: 1.0,\n"
      "  volume: material_volume(\n"
      "    scattering: df::fog_vdf(particle_size: 10.0),\n"
      "    scattering_coefficient: color(5.0)));\n"
      "export material vol_vdf_mix() = material(\n"
      "  ior: 1.0,\n"
      "  volume: material_volume(\n"
      "    scattering: df::normalized_mix(df::vdf_component[](\n"
      "      df::vdf_component(0.3, df::tint(color(0.2, 0.5, 0.9),\n"
      "        df::anisotropic_vdf(directional_bias: 0.5))),\n"
      "      df::vdf_component(0.7, df::fog_vdf(particle_size: 10.0)))),\n"
      "    scattering_coefficient: color(5.0)));\n"
      "export material vol_vdf_mix_pos() = material(\n"
      "  ior: 1.0,\n"
      "  volume: material_volume(\n"
      "    scattering: df::normalized_mix(df::vdf_component[](\n"
      "      df::vdf_component(#max(state::position().x, 0.0),\n"
      "        df::anisotropic_vdf(directional_bias: 0.5)),\n"
      "      df::vdf_component(0.5, df::fog_vdf(particle_size: 10.0)))),\n"
      "    scattering_coefficient: color(5.0)));\n"
      "export material vol_absorb() = material(\n"
      "  ior: 1.0,\n"
      "  volume: material_volume(absorption_coefficient: color(0.5)));\n"
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
  const size_t N{size_t(compiler.wavelengthBaseMax)};
  std::vector<float> sigmaA(N);
  std::vector<float> sigmaS(N);
  std::vector<float> emission(N);
  // 'volumeEvaluate' is allocation-free, so the partial state carries
  // no allocator: only the object-space position identifies the query.
  smdl::State state{};
  SUBCASE("Homogeneous coefficients are position-independent") {
    const smdl::JIT::MaterialDef *materialDef{
        requireMaterial(compiler, "vol_homog")};
    CHECK(materialDef->hasVolume());
    CHECK(materialDef->hasHomogeneousCoefficients());
    materialDef->volumeEvaluate(state, sigmaA.data(), sigmaS.data(),
                                emission.data());
    for (size_t i = 0; i < N; i++) {
      CHECK(sigmaA[i] == 0.5f);
      CHECK(sigmaS[i] == 2.0f);
    }
  }
  SUBCASE("Heterogeneous coefficients follow the position") {
    const smdl::JIT::MaterialDef *materialDef{
        requireMaterial(compiler, "vol_hetero")};
    CHECK(materialDef->hasVolume());
    CHECK(!materialDef->hasHomogeneousCoefficients());
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
    const smdl::JIT::MaterialDef *materialDef{
        requireMaterial(compiler, "vol_fire")};
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
    const smdl::JIT::MaterialDef *hetero{
        requireMaterial(compiler, "vol_hetero")};
    state.position = smdl::float3(3.5f / 32.0f, 4.5f / 8.0f, 2.5f / 4.0f);
    hetero->volumeEvaluate(state, sigmaA.data(), sigmaS.data(),
                           emission.data());
    for (size_t i = 0; i < N; i++) CHECK(emission[i] == 0.0f);
  }
  SUBCASE("No volume evaluates to zero and proves homogeneous") {
    const smdl::JIT::MaterialDef *materialDef{
        requireMaterial(compiler, "vol_none")};
    CHECK(!materialDef->hasVolume());
    CHECK(materialDef->hasHomogeneousCoefficients());
    materialDef->volumeEvaluate(state, sigmaA.data(), sigmaS.data(),
                                emission.data());
    for (size_t i = 0; i < N; i++) {
      CHECK(sigmaA[i] == 0.0f);
      CHECK(sigmaS[i] == 0.0f);
    }
  }
  SUBCASE("RGB, spectral and time-dependent constants prove homogeneous") {
    // These read the wavelength grid, so the partial state carries one;
    // 'volumeEvaluate' still needs no allocator.
    StateStorage storage{compiler};
    smdl::State gridState{storage.makeState()};
    gridState.allocator = nullptr;
    std::vector<float> sigmaSElsewhere(N);
    for (const char *name : {"vol_rgb", "vol_spectral"}) {
      CAPTURE(name);
      const smdl::JIT::MaterialDef *materialDef{
          requireMaterial(compiler, name)};
      CHECK(materialDef->hasVolume());
      CHECK(materialDef->hasHomogeneousCoefficients());
      gridState.position = smdl::float3(0.0f, 0.0f, 0.0f);
      materialDef->volumeEvaluate(gridState, sigmaA.data(), sigmaS.data(),
                                  emission.data());
      gridState.position = smdl::float3(3.0f, -2.0f, 7.0f);
      materialDef->volumeEvaluate(gridState, sigmaA.data(),
                                  sigmaSElsewhere.data(), emission.data());
      for (size_t i = 0; i < N; i++) CHECK(sigmaS[i] == sigmaSElsewhere[i]);
      // The proof is of a spectrum that really was resampled onto the
      // grid, not of one that folded to a flat constant.
      CHECK(sigmaS.front() != sigmaS.back());
    }
    const smdl::JIT::MaterialDef *timed{requireMaterial(compiler, "vol_time")};
    CHECK(timed->hasHomogeneousCoefficients());
    gridState.animationTime = 2.0f;
    timed->volumeEvaluate(gridState, sigmaA.data(), sigmaS.data(),
                          emission.data());
    for (size_t i = 0; i < N; i++) CHECK(sigmaS[i] == 2.0f);
  }
  SUBCASE("Scene data and the generator keep the coefficients unproven") {
    for (const char *name : {"vol_scene", "vol_rng"}) {
      CAPTURE(name);
      const smdl::JIT::MaterialDef *materialDef{
          requireMaterial(compiler, name)};
      CHECK(materialDef->hasVolume());
      CHECK(!materialDef->hasHomogeneousCoefficients());
    }
  }
  SUBCASE("Constant, tinted, fog and mixed VDFs prove homogeneous") {
    for (const char *name : {"vol_homog", "vol_vdf_tint", "vol_vdf_fog",
                             "vol_vdf_mix", "vol_absorb", "vol_none"}) {
      CAPTURE(name);
      const smdl::JIT::MaterialDef *materialDef{
          requireMaterial(compiler, name)};
      CHECK(materialDef->hasHomogeneousVDF());
      CHECK(materialDef->hasHomogeneousVolume());
    }
    // The two proofs are separate: coefficients that follow the position
    // under a constant phase function prove the VDF and not the volume.
    const smdl::JIT::MaterialDef *hetero{
        requireMaterial(compiler, "vol_hetero")};
    CHECK(hetero->hasHomogeneousVDF());
    CHECK(!hetero->hasHomogeneousCoefficients());
    CHECK(!hetero->hasHomogeneousVolume());
  }
  SUBCASE("A position-dependent bias or mix weight keeps the VDF unproven") {
    for (const char *name : {"vol_vdf_pos", "vol_vdf_mix_pos"}) {
      CAPTURE(name);
      const smdl::JIT::MaterialDef *materialDef{
          requireMaterial(compiler, name)};
      CHECK(materialDef->hasVolume());
      CHECK(materialDef->hasHomogeneousCoefficients());
      CHECK(!materialDef->hasHomogeneousVDF());
      CHECK(!materialDef->hasHomogeneousVolume());
      CHECK(bool(materialDef->vdfEvaluate));
    }
    // A material with no volume has the default 'vdf()' and no entry
    // point to ask about it.
    CHECK(!bool(requireMaterial(compiler, "vol_none")->vdfEvaluate));
  }
  SUBCASE("vdfEvaluate reproduces Henyey-Greenstein at each point's bias") {
    StateStorage storage{compiler};
    smdl::State fullState{storage.makeState()};
    const smdl::JIT::MaterialDef *materialDef{
        requireMaterial(compiler, "vol_vdf_pos")};
    const smdl::float3 wo{smdl::normalize(smdl::float3(0.1f, 0.2f, 1.0f))};
    const smdl::float3 wi{smdl::normalize(smdl::float3(-0.3f, 0.5f, -0.8f))};
    // Henyey-Greenstein in the convention of 'df::anisotropic_vdf', on
    // the cosine between the outgoing and the incoming direction.
    const auto hg{[](float g, float cosTheta) {
      const float denom{1.0f + g * g + 2.0f * g * cosTheta};
      return (1.0f - g * g) /
             (4.0f * 3.14159265358979f * denom * std::sqrt(denom));
    }};
    // The bias is '0.5 * sin(x)': backward, isotropic, and forward.
    for (float x : {-1.5707964f, 0.0f, 1.5707964f}) {
      CAPTURE(x);
      const float g{0.5f * std::sin(x)};
      fullState.position = smdl::float3(x, 0.0f, 0.0f);
      smdl::JIT::VDF vdf{fullState, materialDef};
      REQUIRE(vdf.ptr != nullptr);
      CHECK(vdf.evaluate(wo, wi) ==
            doctest::Approx(hg(g, smdl::dot(wo, wi))).epsilon(1e-4));
      // The sample reports the phase function at the direction it drew.
      smdl::float3 wiSampled{};
      const float sampled{
          vdf.sample(smdl::float4(0.3f, 0.7f, 0.1f, 0.9f), wo, wiSampled)};
      CHECK(sampled > 0.0f);
      CHECK(sampled ==
            doctest::Approx(hg(g, smdl::dot(wo, wiSampled))).epsilon(1e-4));
    }
    // The instance's own VDF is the one 'vdfEvaluate' returns at the
    // state the instance was evaluated with, and the instance's phase
    // functions go through it.
    fullState.position = smdl::float3(0.25f, 0.0f, 0.0f);
    smdl::JIT::Material material{fullState, materialDef};
    smdl::JIT::VDF atBoundary{fullState, materialDef};
    CHECK(material.getVDF().def == materialDef);
    CHECK(material.getVDF().ptr == material.eval.volumeScattering);
    CHECK(material.getVDF().evaluate(wo, wi) == atBoundary.evaluate(wo, wi));
    CHECK(material.volumeScatterEvaluate(wo, wi) ==
          material.getVDF().evaluate(wo, wi));
  }
  SUBCASE("A pure absorber scatters zero through its handle") {
    // The default 'vdf()' is an empty struct, so the copy behind the
    // handle is the smallest allocation there is, and never null.
    StateStorage storage{compiler};
    smdl::State fullState{storage.makeState()};
    const smdl::JIT::MaterialDef *materialDef{
        requireMaterial(compiler, "vol_absorb")};
    const smdl::float3 wo{0.0f, 0.0f, 1.0f};
    const smdl::float3 wi{0.0f, 1.0f, 0.0f};
    smdl::JIT::VDF vdf{fullState, materialDef};
    REQUIRE(vdf.ptr != nullptr);
    CHECK(vdf.evaluate(wo, wi) == 0.0f);
    smdl::float3 wiSampled{};
    CHECK(vdf.sample(smdl::float4(0.5f, 0.5f, 0.5f, 0.5f), wo, wiSampled) ==
          0.0f);
    smdl::JIT::Material material{fullState, materialDef};
    REQUIRE(material.getVDF().ptr != nullptr);
    CHECK(material.getVDF().evaluate(wo, wi) == 0.0f);
  }
  SUBCASE("Evaluations expose the density acceleration hint") {
    StateStorage storage{compiler};
    smdl::State fullState{storage.makeState()};
    // A material with the complete hint exposes the grid resource and
    // both corners of the bound box through the evaluation.
    smdl::JIT::Material hinted{smdl::JIT::Material(
        fullState, requireMaterial(compiler, "vol_hinted"))};
    const smdl::VoxelGrid *grid{hinted.getVolumeDensityGrid()};
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
    const smdl::int3 cells{grid->getMajorantCount()};
    CHECK(cells.x == (32 + E - 1) / E);
    CHECK(cells.y == (8 + E - 1) / E);
    CHECK(cells.z == (4 + E - 1) / E);
    float cellMax{-INFINITY}, cellMin{INFINITY};
    for (int cz = 0; cz < cells.z; cz++)
      for (int cy = 0; cy < cells.y; cy++)
        for (int cx = 0; cx < cells.x; cx++) {
          const smdl::float2 bounds{grid->getMajorantBounds(cx, cy, cz)};
          cellMax = std::max(cellMax, bounds.y);
          cellMin = std::min(cellMin, bounds.x);
        }
    CHECK(cellMax == grid->getMaxValue());
    CHECK(cellMin == grid->getMinValue());
    // A material without the hint reports null pointers.
    smdl::JIT::Material unhinted{smdl::JIT::Material(
        fullState, requireMaterial(compiler, "vol_hetero"))};
    CHECK(unhinted.getVolumeDensityGrid() == nullptr);
    CHECK(unhinted.getVolumeDensityBoundMin() == nullptr);
    CHECK(unhinted.getVolumeDensityBoundMax() == nullptr);
  }
  SUBCASE("Evaluations expose the declared majorants") {
    StateStorage storage{compiler};
    smdl::State fullState{storage.makeState()};
    smdl::JIT::Material homog{
        smdl::JIT::Material(fullState, requireMaterial(compiler, "vol_homog"))};
    REQUIRE(homog.getMaxScatteringCoefficient().size() == N);
    CHECK(homog.getMaxAbsorptionCoefficient().empty());
    for (size_t i = 0; i < N; i++)
      CHECK(homog.getMaxScatteringCoefficient()[i] == 2.0f);
    // The heterogeneous majorant is exact through 'tex::max_value'.
    smdl::JIT::Material hetero{smdl::JIT::Material(
        fullState, requireMaterial(compiler, "vol_hetero"))};
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
  smdl::State state{};
  smdl::float3 displacement{};
  SUBCASE("The default material is provably undisplaced") {
    const smdl::JIT::MaterialDef *materialDef{
        requireMaterial(compiler, "disp_none")};
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
    const smdl::JIT::MaterialDef *materialDef{
        requireMaterial(compiler, "disp_const")};
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
    const smdl::JIT::MaterialDef *materialDef{
        requireMaterial(compiler, "disp_state")};
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
  smdl::State state{storage.makeState()};
  // The default state's tangent-to-world is identity, so the directions
  // below are written directly in the hair frame: X is the fiber tangent
  // and Z is the cross-section normal.
  smdl::float3 wo{0.0f, 0.6f, 0.8f};
  smdl::float3 wi{0.6f, -0.64f, -0.48f};
  std::vector<float> f(size_t(compiler.wavelengthBaseMax));
  smdl::Span<float> fSpan(f.data(), f.size());
  float pdfFwd{};
  float pdfRev{};
  SUBCASE("A hair material evaluates and samples through the entry points") {
    const smdl::JIT::MaterialDef *materialDef{
        requireMaterial(compiler, "hair_brown")};
    CHECK(materialDef->hasHair());
    CHECK((materialDef->staticFlagsKnown & smdl::MATERIAL_HAS_HAIR) != 0);
    CHECK((materialDef->staticFlags & smdl::MATERIAL_HAS_HAIR) != 0);
    smdl::JIT::Material material{state, materialDef};
    CHECK(material.hasHair());
    CHECK(material.hairScatterEvaluate(wo, wi, pdfFwd, pdfRev, fSpan));
    CHECK(pdfFwd > 0.0f);
    CHECK(pdfRev > 0.0f);
    for (float fValue : f) {
      CHECK(fValue > 0.0f);
      CHECK(std::isfinite(fValue));
    }
    smdl::float4 xi{0.3f, 0.4f, 0.5f, 0.6f};
    smdl::float3 wiSampled{};
    CHECK(material.hairScatterSample(xi, wo, wiSampled, pdfFwd, pdfRev, fSpan));
    CHECK(pdfFwd > 0.0f);
    float lengthSquared{wiSampled.x * wiSampled.x + wiSampled.y * wiSampled.y +
                        wiSampled.z * wiSampled.z};
    CHECK(lengthSquared == doctest::Approx(1.0f).epsilon(1e-3));
  }
  SUBCASE("The default hair BSDF is safe to call and reports black") {
    const smdl::JIT::MaterialDef *materialDef{
        requireMaterial(compiler, "hair_none")};
    CHECK(!materialDef->hasHair());
    CHECK((materialDef->staticFlagsKnown & smdl::MATERIAL_HAS_HAIR) != 0);
    CHECK((materialDef->staticFlags & smdl::MATERIAL_HAS_HAIR) == 0);
    smdl::JIT::Material material{state, materialDef};
    CHECK(!material.hasHair());
    CHECK(!material.hairScatterEvaluate(wo, wi, pdfFwd, pdfRev, fSpan));
    CHECK(pdfFwd == 0.0f);
    CHECK(pdfRev == 0.0f);
    for (float fValue : f) {
      CHECK(fValue == 0.0f);
    }
  }
}

TEST_CASE("scatterSample: a rough reflector sampled and evaluated back") {
  // A rough reflector has a density wherever it scatters, so a sampled
  // direction can be evaluated back, and the two must agree on everything
  // they report.
  smdl::Compiler compiler{};
  REQUIRE_OK(compiler.addCode(
      "::rough", "#smdl\nimport ::df::*;\nexport material m() = material(\n"
                 "  surface: material_surface(scattering: "
                 "df::microfacet_ggx_smith_bsdf(\n"
                 "    roughness_u: 0.4, tint: 0.8)));\n"));
  REQUIRE_OK(compiler.compile(smdl::OPT_LEVEL_O2));
  REQUIRE_OK(compiler.jitCompile());
  const smdl::JIT::MaterialDef *materialDef{compiler.findMaterial("m")};
  REQUIRE(materialDef);
  StateStorage storage{compiler};
  smdl::State state{storage.makeState()};
  state.finalize();
  smdl::JIT::Material material{state, materialDef};
  const smdl::float3 wo{0.0f, 0.6f, 0.8f};
  std::vector<float> f(size_t(compiler.wavelengthBaseMax));
  smdl::Span<float> fSpan(f.data(), f.size());
  float pdfFwd{};
  float pdfRev{};
  SUBCASE("The sample agrees with the evaluation at its direction") {
    smdl::float3 wi{};
    int lobe{};
    float lobeChance{};
    REQUIRE(material.scatterSample(smdl::float4(0.25f, 0.5f, 0.5f, 0.5f), wo,
                                   wi, pdfFwd, pdfRev, fSpan, lobe,
                                   smdl::DF_ALL, &lobeChance));
    // Exactly one reflective bit, drawn with a proper chance.
    CHECK(lobe != 0);
    CHECK((lobe & smdl::DF_BRDF) == lobe);
    CHECK((lobe & (lobe - 1)) == 0);
    CHECK(lobeChance > 0.0f);
    CHECK(lobeChance <= 1.0f);
    const std::vector<float> fSampled{f};
    float pdfFwdEvaluated{};
    float pdfRevEvaluated{};
    REQUIRE(material.scatterEvaluate(wo, wi, pdfFwdEvaluated, pdfRevEvaluated,
                                     fSpan));
    CHECK(pdfFwdEvaluated == doctest::Approx(pdfFwd));
    CHECK(pdfRevEvaluated == doctest::Approx(pdfRev));
    for (size_t i = 0; i < f.size(); i++)
      CHECK(f[i] == doctest::Approx(fSampled[i]));
  }
  SUBCASE("A mask without the query's domain excludes the query") {
    // Mirrored about the normal, so the pair is a reflection.
    const smdl::float3 wi{0.0f, -0.6f, 0.8f};
    CHECK(material.scatterEvaluate(wo, wi, pdfFwd, pdfRev, fSpan));
    CHECK(pdfFwd > 0.0f);
    CHECK(!material.scatterEvaluate(wo, wi, pdfFwd, pdfRev, fSpan,
                                    smdl::DF_BTDF));
    CHECK(pdfFwd == 0.0f);
    CHECK(pdfRev == 0.0f);
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
    smdl::Compiler compiler{};
    buildGlossy(compiler, false);
    const smdl::JIT::MaterialDef *materialDef{compiler.findMaterial("m")};
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
    smdl::Compiler compiler{};
    buildGlossy(compiler, true);
    const smdl::JIT::MaterialDef *materialDef{compiler.findMaterial("m")};
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
    smdl::Compiler compiler{};
    buildGlossy(compiler, true);
    const smdl::JIT::MaterialDef *materialDef{compiler.findMaterial("m")};
    REQUIRE(materialDef);
    StateStorage storage{compiler};
    smdl::State state{storage.makeState()};
    state.finalize();
    smdl::JIT::Material material{state, materialDef};
    const smdl::float4 xi{0.25f, 0.5f, 0.5f, 0.5f};
    smdl::float3 wm{};
    smdl::float2 alpha{};
    float pdf{};
    CHECK(!material.scatterNormalSample(xi, false, wm, pdf, alpha,
                                        smdl::DF_GLOSSY));
    CHECK(
        !material.scatterNormalSample(xi, false, wm, pdf, alpha, smdl::DF_ALL));
    CHECK(!material.scatterNormalEvaluate(false, smdl::float3(0.0f, 0.0f, 1.0f),
                                          pdf, smdl::DF_GLOSSY));
    // And exactly one kind answers: this material is glossy in
    // reflection only, so that kind draws and the other reports nothing.
    CHECK(material.scatterNormalSample(xi, false, wm, pdf, alpha,
                                       smdl::DF_GLOSSY_BRDF));
    CHECK(pdf > 0.0f);
    CHECK(alpha.x == doctest::Approx(0.16f));
    CHECK(!material.scatterNormalSample(xi, false, wm, pdf, alpha,
                                        smdl::DF_GLOSSY_BTDF));
  }
  SUBCASE("The evaluate hook reports the density the sample hook drew") {
    smdl::Compiler compiler{};
    buildGlossy(compiler, true);
    const smdl::JIT::MaterialDef *materialDef{compiler.findMaterial("m")};
    REQUIRE(materialDef);
    StateStorage storage{compiler};
    smdl::State state{storage.makeState()};
    state.finalize();
    smdl::JIT::Material material{state, materialDef};
    smdl::float3 wm{};
    smdl::float2 alpha{};
    float pdf{};
    REQUIRE(material.scatterNormalSample(smdl::float4(0.25f, 0.5f, 0.5f, 0.5f),
                                         false, wm, pdf, alpha,
                                         smdl::DF_GLOSSY_BRDF));
    float pdfEvaluated{};
    CHECK(material.scatterNormalEvaluate(false, wm, pdfEvaluated,
                                         smdl::DF_GLOSSY_BRDF));
    CHECK(pdfEvaluated == doctest::Approx(pdf));
  }
  SUBCASE("The normal probe and the geometry normal hook") {
    smdl::Compiler compiler{};
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
    const smdl::JIT::MaterialDef *plain{compiler.findMaterial("plain")};
    const smdl::JIT::MaterialDef *remapped{compiler.findMaterial("remapped")};
    REQUIRE(plain);
    REQUIRE(remapped);
    // The probe folds 'geometry.normal - $state.normal' at O2, so the
    // flag is known on both sides: provably identity on one, provably
    // remapped on the other.
    CHECK(!plain->canRemapNormal());
    CHECK(remapped->canRemapNormal());
    // And the hook reads the field itself, in internal space.
    std::vector<float> wavelengths{
        std::vector<float>(size_t(compiler.wavelengthBaseMax))};
    smdl::State state{};
    state.wavelengthMin = 380.0f;
    state.wavelengthMax = 720.0f;
    state.wavelengthBase = wavelengths.data();
    for (uint32_t i = 0; i < compiler.wavelengthBaseMax; i++) {
      float fac{float(i) / float(compiler.wavelengthBaseMax - 1)};
      wavelengths[i] =
          (1 - fac) * state.wavelengthMin + fac * state.wavelengthMax;
    }
    state.finalize();
    smdl::float3 normal{};
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
    smdl::Compiler compiler{};
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
    const smdl::JIT::MaterialDef *inheritsMaterial{
        compiler.findMaterial("inherits")};
    const smdl::JIT::MaterialDef *pinnedMaterial{
        compiler.findMaterial("pinned")};
    REQUIRE(inheritsMaterial);
    REQUIRE(pinnedMaterial);
    CHECK(inheritsMaterial->canRemapNormal());
    CHECK(pinnedMaterial->canRemapNormal());
    StateStorage storage{compiler};
    smdl::State state{storage.makeState()};
    state.finalize();
    smdl::JIT::Material inherits{state, inheritsMaterial};
    smdl::JIT::Material pinned{state, pinnedMaterial};
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
    smdl::Compiler compiler{};
    buildGlossy(compiler, false); // OPT_LEVEL_NONE inside
    const smdl::JIT::MaterialDef *materialDef{compiler.findMaterial("m")};
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
  smdl::Compiler compiler{};
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
  const smdl::JIT::MaterialDef *oneSidedMaterial{
      compiler.findMaterial("one_sided")};
  const smdl::JIT::MaterialDef *twoSidedMaterial{
      compiler.findMaterial("two_sided")};
  REQUIRE(oneSidedMaterial);
  REQUIRE(twoSidedMaterial);
  StateStorage storage{compiler};
  smdl::State state{storage.makeState()};
  state.finalize();
  const auto lobes{[](const smdl::JIT::Material &material, bool isBackface) {
    return material.getLobes(isBackface) & smdl::DF_ALL;
  }};
  // A material with no `backface` initializer scatters by its `surface`
  // from both sides, so the back side reports the surface word and not
  // the empty one the raw `backfaceLobes` field holds.
  smdl::JIT::Material oneSided{state, oneSidedMaterial};
  CHECK(lobes(oneSided, false) == smdl::DF_SMOOTH_BRDF);
  CHECK(lobes(oneSided, true) == smdl::DF_SMOOTH_BRDF);
  CHECK((oneSided.getLobes() & smdl::DF_ALL) == smdl::DF_SMOOTH_BRDF);
  // A two-sided one distinguishes, and the sideless union is the two
  // together.
  smdl::JIT::Material twoSided{state, twoSidedMaterial};
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
    smdl::Compiler compiler{};
    compiler.shouldEmitUnitTests = true;
    registerAlias(compiler);
    REQUIRE_OK(compiler.addCode("::vertex_color_test", source));
    REQUIRE_OK(compiler.compile(smdl::OPT_LEVEL_NONE));
    REQUIRE_OK(compiler.jitCompile());
    StateStorage storage{compiler};
    smdl::State state{storage.makeState()};
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
