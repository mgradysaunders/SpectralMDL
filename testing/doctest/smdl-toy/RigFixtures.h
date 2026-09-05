/// \file
/// The rigged glTF files the renderer suite tests deformation with,
/// written by hand: one binary buffer in a sidecar and the JSON
/// assembled from fragments, so the files are exact, need no library,
/// and every expected value is arithmetic done on paper. Shared by the
/// evaluator's doctest and the scene's.
#pragma once

#include <algorithm>
#include <cstdint>
#include <cstdio>
#include <filesystem>
#include <fstream>
#include <string>
#include <vector>

#include "smdl/Support/Strings.h"

namespace rig {

constexpr float SIN45 = 0.70710678f;

/// A glTF 2.0 document under construction: accessors appended to one
/// buffer, and the scene-specific members supplied by the caller.
class GltfBuilder final {
public:
  explicit GltfBuilder(std::string binName) : binName(std::move(binName)) {}

  /// Append float data as an accessor of `type` with `components` per
  /// element; returns the accessor index. `bounds` writes `min` and `max`,
  /// which the format requires for positions.
  uint32_t floats(const std::vector<float> &data, const char *type,
                  size_t components, bool bounds = false) {
    const auto view{addView(data.data(), data.size() * sizeof(float), 4)};
    auto accessor{smdl::concat(
        "{\"bufferView\":", view, ",\"componentType\":5126,\"count\":",
        data.size() / components, ",\"type\":\"", type, "\"")};
    if (bounds) {
      auto lower{std::vector<float>(components, +1e30f)};
      auto upper{std::vector<float>(components, -1e30f)};
      for (size_t i = 0; i < data.size(); i++) {
        lower[i % components] = std::min(lower[i % components], data[i]);
        upper[i % components] = std::max(upper[i % components], data[i]);
      }
      accessor += ",\"min\":" + numbers(lower) + ",\"max\":" + numbers(upper);
    }
    accessors.push_back(accessor + "}");
    return uint32_t(accessors.size() - 1);
  }

  /// Append unsigned short data; returns the accessor index.
  uint32_t ushorts(const std::vector<uint16_t> &data, const char *type,
                   size_t components) {
    const auto view{addView(data.data(), data.size() * sizeof(uint16_t), 2)};
    accessors.push_back(smdl::concat(
        "{\"bufferView\":", view, ",\"componentType\":5123,\"count\":",
        data.size() / components, ",\"type\":\"", type, "\"}"));
    return uint32_t(accessors.size() - 1);
  }

  /// The whole document around `body`, the scene-specific members.
  [[nodiscard]] std::string json(const std::string &body) const {
    return smdl::concat(
        "{\"asset\":{\"version\":\"2.0\"},\"buffers\":[{\"uri\":\"", binName,
        "\",\"byteLength\":", bytes.size(), "}],\"bufferViews\":[",
        join(bufferViews), "],\"accessors\":[", join(accessors), "],", body,
        "}");
  }

  [[nodiscard]] static std::string numbers(const std::vector<float> &values) {
    auto text{std::string("[")};
    for (size_t i = 0; i < values.size(); i++) {
      char buffer[32]{};
      std::snprintf(buffer, sizeof(buffer), "%.9g", double(values[i]));
      text += (i == 0 ? "" : ",") + std::string(buffer);
    }
    return text + "]";
  }

  std::string bytes{};

private:
  uint32_t addView(const void *data, size_t size, size_t alignment) {
    while (bytes.size() % alignment != 0) bytes.push_back('\0');
    const auto offset{bytes.size()};
    bytes.append(static_cast<const char *>(data), size);
    bufferViews.push_back(smdl::concat("{\"buffer\":0,\"byteOffset\":", offset,
                                       ",\"byteLength\":", size, "}"));
    return uint32_t(bufferViews.size() - 1);
  }

  [[nodiscard]] static std::string join(const std::vector<std::string> &parts) {
    auto text{std::string()};
    for (size_t i = 0; i < parts.size(); i++)
      text += (i == 0 ? "" : ",") + parts[i];
    return text;
  }

  std::string binName{};
  std::vector<std::string> bufferViews{};
  std::vector<std::string> accessors{};
};

/// The unit quad in the XY plane, facing +Z.
inline const std::vector<float> QUAD_POINTS{0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0};
inline const std::vector<float> QUAD_NORMALS{0, 0, 1, 0, 0, 1,
                                             0, 0, 1, 0, 0, 1};
inline const std::vector<uint16_t> QUAD_INDICES{0, 1, 2, 0, 2, 3};

/// The four files, as paths.
class Files final {
public:
  std::string wave{};
  std::string morph{};
  std::string pendulum{};
  std::string plain{};
};

inline std::string writeFile(const std::filesystem::path &dir,
                             const std::string &name, const std::string &text) {
  const auto path{(dir / name).string()};
  std::ofstream file(path, std::ios::binary | std::ios::trunc);
  file.write(text.data(), std::streamsize(text.size()));
  return path;
}

inline std::string finish(const std::filesystem::path &dir,
                          const std::string &stem, GltfBuilder &gltf,
                          const std::string &body) {
  writeFile(dir, stem + ".bin", gltf.bytes);
  return writeFile(dir, stem + ".gltf", gltf.json(body));
}

/// A strip of two quads along +X skinned to two bones: `root` at the
/// origin holds the x = 0 column, `tip` at x = 1 holds the x = 2 column,
/// and the x = 1 column is split half and half. The clip `wave` turns
/// `tip` a quarter turn about Z over one second, its first key the bind
/// pose. A seventh vertex at (5, 5, 0) has no weights, and the mesh sits
/// under a node translated five units up, which a skinned bake must
/// ignore. Two root nodes, so the reader adds its own root above them.
/// The last triangle exists to reference the weightless vertex: the
/// reader drops vertices no face uses, and renumbers the rest by first
/// use, which is why checks find vertices by position. Material `paint`.
inline std::string writeWave(const std::filesystem::path &dir) {
  GltfBuilder gltf{"wave.bin"};
  const auto points{gltf.floats(
      {0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 1, 0, 2, 0, 0, 2, 1, 0, 5, 5, 0}, "VEC3",
      3, true)};
  const auto normals{gltf.floats(
      {0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1}, "VEC3",
      3)};
  const auto joints{gltf.ushorts({0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1,
                                  0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0},
                                 "VEC4", 4)};
  const auto weights{
      gltf.floats({1, 0, 0, 0, 1, 0, 0, 0, 0.5f, 0.5f, 0, 0, 0.5f, 0.5f,
                   0, 0, 1, 0, 0, 0, 1, 0, 0,    0,    0, 0, 0,    0},
                  "VEC4", 4)};
  const auto indices{
      gltf.ushorts({0, 2, 3, 0, 3, 1, 2, 4, 5, 2, 5, 3, 4, 5, 6}, "SCALAR", 1)};
  // Column-major: the identity, then a translation by (-1, 0, 0).
  const auto bindMatrices{
      gltf.floats({1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0,  0, 0, 1, //
                   1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, -1, 0, 0, 1},
                  "MAT4", 16)};
  const auto times{gltf.floats({0, 1}, "SCALAR", 1)};
  const auto rotations{
      gltf.floats({0, 0, 0, 1, 0, 0, SIN45, SIN45}, "VEC4", 4)};
  return finish(
      dir, "wave", gltf,
      smdl::concat(
          "\"scene\":0,\"scenes\":[{\"nodes\":[0,1]}],"
          "\"nodes\":[{\"name\":\"strip\",\"mesh\":0,\"skin\":0,"
          "\"translation\":[0,0,5]},{\"name\":\"root\",\"children\":[2]},"
          "{\"name\":\"tip\",\"translation\":[1,0,0]}],"
          "\"materials\":[{\"name\":\"paint\"}],"
          "\"meshes\":[{\"name\":\"stripmesh\",\"primitives\":[{"
          "\"attributes\":{\"POSITION\":",
          points, ",\"NORMAL\":", normals, ",\"JOINTS_0\":", joints,
          ",\"WEIGHTS_0\":", weights, "},\"indices\":", indices,
          ",\"material\":0}]}],",
          "\"skins\":[{\"joints\":[1,2],\"inverseBindMatrices\":", bindMatrices,
          "}],",
          "\"animations\":[{\"name\":\"wave\",\"channels\":[{\"sampler\":0,"
          "\"target\":{\"node\":2,\"path\":\"rotation\"}}],"
          "\"samplers\":[{\"input\":",
          times, ",\"output\":", rotations,
          ",\"interpolation\":\"LINEAR\"}]}]"));
}

/// The unit quad, under a wrapper, with two morph targets: `lift` raises
/// it one unit, `stretch` pulls its x = 1 edge out by one unit and tilts
/// the normals toward +X. The mesh's default weights are (0.25, 0), and
/// the clip `open` drives them from (0, 0) to (1, 0.5) over one second.
/// The corner at (1, 1, 0) is authored twice, identically in every
/// attribute and every target, one copy per triangle: what a texture
/// seam leaves behind, for the animated read's own weld to put back
/// together. Material `paint`.
inline std::string writeMorph(const std::filesystem::path &dir) {
  GltfBuilder gltf{"morph.bin"};
  const auto points{gltf.floats({0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0},
                                "VEC3", 3, true)};
  const auto normals{
      gltf.floats({0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1}, "VEC3", 3)};
  const auto indices{gltf.ushorts({0, 1, 2, 0, 4, 3}, "SCALAR", 1)};
  const auto lift{gltf.floats({0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1},
                              "VEC3", 3, true)};
  const auto liftNormals{
      gltf.floats({0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, "VEC3", 3)};
  const auto stretch{gltf.floats({0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0},
                                 "VEC3", 3, true)};
  const auto stretchNormals{gltf.floats(
      {1, 0, -1, 1, 0, -1, 1, 0, -1, 1, 0, -1, 1, 0, -1}, "VEC3", 3)};
  const auto times{gltf.floats({0, 1}, "SCALAR", 1)};
  const auto weights{gltf.floats({0, 0, 1, 0.5f}, "SCALAR", 1)};
  return finish(
      dir, "morph", gltf,
      smdl::concat(
          "\"scene\":0,\"scenes\":[{\"nodes\":[0]}],"
          "\"nodes\":[{\"name\":\"rig\",\"children\":[1]},"
          "{\"name\":\"shape\",\"mesh\":0}],"
          "\"materials\":[{\"name\":\"paint\"}],"
          "\"meshes\":[{\"name\":\"quad\",\"weights\":[0.25,0],"
          "\"primitives\":[{\"attributes\":{\"POSITION\":",
          points, ",\"NORMAL\":", normals, "},\"indices\":", indices,
          ",\"material\":0,\"targets\":[{\"POSITION\":", lift,
          ",\"NORMAL\":", liftNormals, "},{\"POSITION\":", stretch,
          ",\"NORMAL\":", stretchNormals, "}]}]}],",
          "\"animations\":[{\"name\":\"open\",\"channels\":[{\"sampler\":0,"
          "\"target\":{\"node\":1,\"path\":\"weights\"}}],"
          "\"samplers\":[{\"input\":",
          times, ",\"output\":", weights, ",\"interpolation\":\"LINEAR\"}]}]"));
}

/// The unit quad under a node `arm`, under a wrapper, with three clips:
/// `swing` turns it a quarter turn about Z linearly, `hop` lifts it one
/// unit at a step, and `ease` lifts it one unit along a cubic spline
/// with zero tangents. Material `paint`.
inline std::string writePendulum(const std::filesystem::path &dir) {
  GltfBuilder gltf{"pendulum.bin"};
  const auto points{gltf.floats(QUAD_POINTS, "VEC3", 3, true)};
  const auto normals{gltf.floats(QUAD_NORMALS, "VEC3", 3)};
  const auto indices{gltf.ushorts(QUAD_INDICES, "SCALAR", 1)};
  const auto times{gltf.floats({0, 1}, "SCALAR", 1)};
  const auto rotations{
      gltf.floats({0, 0, 0, 1, 0, 0, SIN45, SIN45}, "VEC4", 4)};
  const auto hops{gltf.floats({0, 0, 0, 0, 0, 1}, "VEC3", 3)};
  // In-tangent, value, out-tangent per key.
  const auto eases{gltf.floats(
      {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0}, "VEC3", 3)};
  return finish(
      dir, "pendulum", gltf,
      smdl::concat(
          "\"scene\":0,\"scenes\":[{\"nodes\":[0]}],"
          "\"nodes\":[{\"name\":\"rig\",\"children\":[1]},"
          "{\"name\":\"arm\",\"mesh\":0}],"
          "\"materials\":[{\"name\":\"paint\"}],"
          "\"meshes\":[{\"name\":\"bar\",\"primitives\":[{\"attributes\":{"
          "\"POSITION\":",
          points, ",\"NORMAL\":", normals, "},\"indices\":", indices,
          ",\"material\":0}]}],",
          "\"animations\":["
          "{\"name\":\"swing\",\"channels\":[{\"sampler\":0,\"target\":{"
          "\"node\":1,\"path\":\"rotation\"}}],\"samplers\":[{\"input\":",
          times, ",\"output\":", rotations,
          ",\"interpolation\":\"LINEAR\"}]},"
          "{\"name\":\"hop\",\"channels\":[{\"sampler\":0,\"target\":{"
          "\"node\":1,\"path\":\"translation\"}}],\"samplers\":[{\"input\":",
          times, ",\"output\":", hops,
          ",\"interpolation\":\"STEP\"}]},"
          "{\"name\":\"ease\",\"channels\":[{\"sampler\":0,\"target\":{"
          "\"node\":1,\"path\":\"translation\"}}],\"samplers\":[{\"input\":",
          times, ",\"output\":", eases,
          ",\"interpolation\":\"CUBICSPLINE\"}]}]"));
}

/// The unit quad, under a wrapper, and nothing else. Material `paint`.
inline std::string writePlain(const std::filesystem::path &dir) {
  GltfBuilder gltf{"plain.bin"};
  const auto points{gltf.floats(QUAD_POINTS, "VEC3", 3, true)};
  const auto normals{gltf.floats(QUAD_NORMALS, "VEC3", 3)};
  const auto indices{gltf.ushorts(QUAD_INDICES, "SCALAR", 1)};
  return finish(dir, "plain", gltf,
                smdl::concat("\"scene\":0,\"scenes\":[{\"nodes\":[0]}],"
                             "\"nodes\":[{\"name\":\"rig\",\"children\":[1]},"
                             "{\"name\":\"flat\",\"mesh\":0}],"
                             "\"materials\":[{\"name\":\"paint\"}],"
                             "\"meshes\":[{\"name\":\"quad\",\"primitives\":[{"
                             "\"attributes\":{\"POSITION\":",
                             points, ",\"NORMAL\":", normals,
                             "},\"indices\":", indices, ",\"material\":0}]}]"));
}

/// Write the four files into `dir`, which must exist.
inline Files writeFiles(const std::filesystem::path &dir) {
  Files files{};
  files.wave = writeWave(dir);
  files.morph = writeMorph(dir);
  files.pendulum = writePendulum(dir);
  files.plain = writePlain(dir);
  return files;
}

} // namespace rig
