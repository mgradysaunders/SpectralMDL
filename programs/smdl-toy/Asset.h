#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <vector>

// Deliberately not "Scene.h": keeping this header free of Embree and
// compiler dependencies lets the layout loader (and its doctests) read
// manifests without pulling in the renderer.
#include "smdl/Support/VectorMath.h"

using namespace smdl::vector_type_aliases;
using namespace smdl::matrix_type_aliases;

/// The file extension that marks an asset manifest.
constexpr std::string_view ASSET_EXTENSION = ".asset";

/// One asset package: the mesh file to render, plus the correction from the
/// space it was authored in into the space a composition places it in.
///
/// An asset is a directory named as a unit by an `import` in a layout; its
/// `.asset` manifest resolves that name to the render mesh. The manifest is
/// the same flat `key: value` YAML subset the `.pbr` manifests use (see
/// `smdl::PBRMaps`):
///
/// ```
/// asset: 1                              # the schema version, if given
/// name: Rock Moss Set 01                # the display name
/// render: rock_moss_set_01_4k.gltf      # the mesh, relative to this file
/// up: y                                 # the file's up axis, y or z
/// scale: 1                              # scene units per file unit
/// front: 205                            # the camera azimuth -frame locks to
/// proxy: proxy/rock_moss_set_01.blend   # the layout stand-in
/// materials: [trunk, leaves]            # the file's material names, when
///                                       # 'objects' has nothing to list
/// objects:                              # what 'select' can name
///   - select: rock_moss_set_01_rock03
///     materials: [rock_moss_set_01]
///     pivot: [0.418, 0.0, -0.203]
///     triangles: 5000
/// ```
///
/// Only `render` is required. `up` and `scale` fold into `correction`, which
/// applies innermost, underneath everything the `import` block says. `proxy`
/// and `objects` exist for layout tooling; the renderer reads them and does
/// nothing with them. A directory holding several manifests is an asset with
/// variants; imports must then name the wanted manifest, not the directory.
///
class Asset final {
public:
  /// One selectable object of an asset: what a composition can name in
  /// `select`, and the facts a layout tool needs to stand in for it.
  class Object final {
  public:
    /// The node path, as `-list-objects` reports it. Used verbatim as a
    /// `select` pattern, where a full path matches exactly.
    std::string select{};

    /// The scene material names its subtree uses.
    std::vector<std::string> materials{};

    /// The authored translation that `recenter` removes, in the render
    /// mesh's **own** space, before `correction`. Layout stand-ins are built
    /// around this so the layout origin matches the rendered origin.
    float3 pivot{};

    /// The triangles its subtree places.
    uint64_t triangleCount{};
  };

  /// The display name, or empty if the manifest gives none.
  std::string name{};

  /// The mesh file to render, resolved against the manifest's directory.
  std::string renderFileName{};

  /// The layout stand-in, resolved against the manifest's directory, or
  /// empty. Not checked for existence, because nothing in the renderer ever
  /// opens it.
  std::string proxyFileName{};

  /// The transform from the render mesh's own space into the space the
  /// composition places the asset in: the `up` axis correction and the
  /// `scale` unit conversion, composed.
  float4x4 correction{float4x4(1.0f)};

  /// The camera azimuth of the asset's best view in degrees CCW from +X,
  /// in the corrected space, or unset. `-frame` locks to it unless the
  /// command line says otherwise, overriding the automatic sweep.
  std::optional<float> front{};

  /// What `select` can name, in the order `-list-objects` reports.
  std::vector<Object> objects{};

  /// The whole file's material names, for a file whose geometry sits on
  /// its unnamed root node: `objects` cannot list it (every entry needs a
  /// `select`), and the material names would otherwise be recorded
  /// nowhere. Exists for layout tooling, exactly as `objects` does.
  std::vector<std::string> materials{};
};

/// Read an asset manifest.
///
/// \throws smdl::Error  On any syntax error, reported as `file:line: ...`,
///                      or if the render mesh it names does not exist.
///
[[nodiscard]] Asset readAsset(const std::string &fileName);

/// The manifest of an asset directory: the one file in it whose extension
/// is `.asset`. Returns empty if there is none, so callers can test whether
/// a directory is an asset rather than fail on it.
///
/// \throws smdl::Error  If the directory cannot be read, or holds more than
///                      one manifest.
///
[[nodiscard]] std::string findAssetManifest(const std::string &directory);
