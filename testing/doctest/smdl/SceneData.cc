#include "doctest.h"

#include <array>

#include "smdl/SceneData.h"

TEST_CASE("SceneData") {
  smdl::SceneData sceneData{};
  SUBCASE("setFloat4x4") {
    smdl::float4x4 matrix{};
    for (size_t j{}; j < 4; j++)
      for (size_t i{}; i < 4; i++) matrix[j][i] = float(4 * j + i);
    sceneData.setFloat4x4("matrix", matrix);
    auto *getter{sceneData.get("matrix")};
    REQUIRE(getter);
    // The lookup kind and size are those emitted by
    // 'data_lookup_float4x4' in 'Builtin/scene.smdl': kind Float, 16
    // floats in column-major order.
    std::array<float, 16> out{};
    (*getter)(nullptr, smdl::SceneData::Kind::Float, 16, out.data());
    for (size_t k{}; k < 16; k++) CHECK(out[k] == float(k));
    // A mismatched kind or size must leave the output untouched, so the
    // SMDL-side default survives.
    out.fill(-1.0f);
    (*getter)(nullptr, smdl::SceneData::Kind::Float, 4, out.data());
    for (size_t k{}; k < 16; k++) CHECK(out[k] == -1.0f);
    (*getter)(nullptr, smdl::SceneData::Kind::Color, 16, out.data());
    for (size_t k{}; k < 16; k++) CHECK(out[k] == -1.0f);
  }
  SUBCASE("Presence follows the predicate") {
    // Unregistered: never present. Registered without a predicate:
    // present everywhere. With one: whatever it says of the state.
    smdl::State present{};
    smdl::State absent{};
    present.vertex_color_max = 1;
    CHECK(!sceneData.exists("nothing", &present));
    sceneData.setFloat("everywhere", 1.0f);
    CHECK(sceneData.exists("everywhere", &present));
    CHECK(sceneData.exists("everywhere", &absent));
    CHECK(sceneData.exists("everywhere", nullptr));
    sceneData.set(
        "vertex_color",
        [](smdl::State *state, smdl::SceneData::Kind kind, int size,
           void *out) {
          if (kind == smdl::SceneData::Kind::Float && size <= 4)
            for (int i = 0; i < size; i++)
              static_cast<float *>(out)[i] = state->vertex_color[0][i];
        },
        [](const smdl::State *state) { return state->vertex_color_max > 0; });
    CHECK(sceneData.exists("vertex_color", &present));
    CHECK(!sceneData.exists("vertex_color", &absent));
    auto *getter{sceneData.get("vertex_color")};
    REQUIRE(getter);
    present.vertex_color[0] = smdl::float4(0.25f, 0.5f, 0.75f, 1.0f);
    std::array<float, 4> out{};
    (*getter)(&present, smdl::SceneData::Kind::Float, 4, out.data());
    CHECK(out[0] == 0.25f);
    CHECK(out[1] == 0.5f);
    CHECK(out[2] == 0.75f);
    CHECK(out[3] == 1.0f);
  }
  SUBCASE("setFloat4 does not answer float4x4 lookups") {
    sceneData.setFloat4("vector", smdl::float4(1.0f, 2.0f, 3.0f, 4.0f));
    auto *getter{sceneData.get("vector")};
    REQUIRE(getter);
    std::array<float, 16> out{};
    out.fill(-1.0f);
    (*getter)(nullptr, smdl::SceneData::Kind::Float, 16, out.data());
    for (size_t k{}; k < 16; k++) CHECK(out[k] == -1.0f);
  }
}
