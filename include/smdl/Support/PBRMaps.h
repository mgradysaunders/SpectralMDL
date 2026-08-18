/// \file
#pragma once

#include <array>
#include <optional>
#include <string>
#include <vector>

#include "smdl/Support/Error.h"

namespace smdl {

/// \addtogroup support
/// \{

/// A set of related PBR texture maps, parsed from a `.pbr` manifest file.
///
/// A `.pbr` manifest lives in one directory alongside the maps it names,
/// and records their storage conventions and the physical metadata of the
/// set (see `standardized-textures.md` at the repository root). The SMDL
/// counterpart is the `pbr_maps` struct in the non-standard
/// `::extras::pbr` builtin module.
///
/// The manifest content is a strict flat subset of YAML:
/// - UTF-8 text, `#` comments (full-line, or trailing after whitespace),
/// - `key: value` scalars, where a value is a bare string ending at the
///   comment or end of line, or a double-quoted string with `\"` and
///   `\\` escapes,
/// - inline lists `[a, b]`,
/// - exactly one level of nesting: a map role key with no value followed
///   by consistently indented `key: value` lines, and
/// - no anchors, tags, multi-document streams, multiline strings, flow
///   maps, or tab indentation.
///
/// Every conforming manifest is valid YAML, so external tooling can read
/// it with any stock YAML parser; this implementation accepts only the
/// subset, so manifests stay portable by construction.
///
/// The top-level keys:
/// - `pbr`: the schema version, which must be `1` (optional),
/// - `name`: the display name (optional),
/// - `description`: the description (optional),
/// - `physical_extent`: meters covered by one repeat in U and V, as a
///   list of two positive reals (optional),
/// - `physical_relief`: meters spanned by the height map from 0 to 1, a
///   positive real (optional),
/// - `relief_scale`: the span of the height map from 0 to 1 as a
///   fraction of one UV unit, a positive real (optional). This is the
///   ratio `physical_relief / physical_extent[0]` stated directly, for
///   bespoke maps with no meaningful physical extent; it overrides the
///   physical pair, and declaring it alongside `physical_relief` is a
///   warning (see `warnings`),
/// - `hex_rotation`: the random-rotation fraction for hex tiling in
///   `[0, 1]` (optional, default 0), and
/// - the map roles: `basecolor`, `normal`, `roughness`, `metallic`,
///   `height`, `emission`, `horizon`, and `class`.
///
/// Each map role is either a bare filename or a nested table:
/// - `file`: the filename, relative to the manifest directory (required;
///   absolute paths, backslashes, and `..` components are rejected),
/// - `channel`: `r`, `g`, `b`, or `a`, for scalar roles packed into a
///   multi-channel image (only `roughness`, `metallic`, and `height`),
/// - `color_space`: `srgb` or `linear`, overriding the role default
///   (sRGB for `basecolor` and `emission`, linear otherwise),
/// - `convention`: `opengl`, `directx`, `opengl_xy`, or `directx_xy`,
///   for `normal` only,
/// - `max_slope`: the positive encoding scale of the horizon map, for
///   `horizon` only (default 8, the `PARALLAX_MAX_SLOPE` cap),
/// - `factor`: a real `>= 0` multiplying the decoded (linear,
///   after-gamma) color, for `emission` only (default 1), so a [0, 1]
///   map can express radiant intensity,
/// - `range`: two reals in `[0, 1]` as an inline list, for `roughness`
///   and `metallic` only (default `[0, 1]`): the stored value `v`
///   decodes as `lo + (hi - lo) * v`, concentrating an 8-bit map's
///   precision in a sub-range. `lo > hi` is allowed and decodes an
///   inverted map, e.g. `[1, 0]` reads a gloss map as roughness.
///
/// The `class` role carries per-texel membership in up to four classes:
/// R, G, and B are the first three, and the fourth is the remainder
/// `1 - (R + G + B)`. The manifest does not name the classes and does
/// not say how many there are. What a class *is* belongs to whatever
/// generated the map, which writes it down beside the map
/// (`etc/albedo_cluster.py` writes a `.json` sidecar naming the class
/// count, the mean color of each class, and how it was fit); a material
/// that cares reads that and hard-codes what it found. Consumers here
/// simply take all four, which costs nothing when the map has fewer:
/// the memberships partition every texel, so the unused weights are
/// zero everywhere, to the precision the map is stored at.
///
/// The `horizon` role is a precomputed replacement for the runtime
/// multi-tap horizon search behind `df::horizon_shadow_factor`: its
/// four channels are the steepest blocker tangents toward +U, -U, +V,
/// -V at the relief scale of the maps, each stored as
/// `slope / max_slope` (see `etc/horizon_map.py`, which generates such
/// maps). Because the stored slopes are meaningless without the scale
/// they were baked at, `horizon` requires an effective relief scale:
/// either `relief_scale`, or `physical_extent` and `physical_relief`.
///
class SMDL_EXPORT PBRMaps final {
public:
  /// The map role.
  enum Role : int {
    ROLE_BASECOLOR = 0, ///< Base color, AKA albedo.
    ROLE_NORMAL,        ///< Tangent-space normal.
    ROLE_ROUGHNESS,     ///< Roughness.
    ROLE_METALLIC,      ///< Metallic.
    ROLE_HEIGHT,        ///< Height, 1 is the un-displaced surface.
    ROLE_HORIZON,       ///< Horizon slopes toward +U, -U, +V, -V.
    ROLE_EMISSION,      ///< Emission color.
    ROLE_CLASS,         ///< Class weights in R, G, B, remainder.
    ROLE_COUNT          ///< The number of roles.
  };

  /// The channel a scalar map is packed into.
  enum Channel : int {
    CHANNEL_NATURAL = -1, ///< The natural channels of the image.
    CHANNEL_R = 0,        ///< Red.
    CHANNEL_G = 1,        ///< Green.
    CHANNEL_B = 2,        ///< Blue.
    CHANNEL_A = 3         ///< Alpha.
  };

  /// The color space a map is stored in.
  enum ColorSpace : int {
    COLOR_SPACE_DEFAULT = 0, ///< The role default.
    COLOR_SPACE_LINEAR,      ///< Linear.
    COLOR_SPACE_SRGB         ///< sRGB.
  };

  /// The normal map convention.
  enum NormalConvention : int {
    NORMAL_CONVENTION_OPENGL = 0, ///< 3-channel, Y up (default).
    NORMAL_CONVENTION_DIRECTX,    ///< 3-channel, Y flipped.
    NORMAL_CONVENTION_OPENGL_XY,  ///< 2-channel, Y up, Z reconstructed.
    NORMAL_CONVENTION_DIRECTX_XY  ///< 2-channel, Y flipped, Z reconstructed.
  };

  /// One map in the pack.
  class Map final {
  public:
    /// Is present? I.e., declared by the manifest?
    [[nodiscard]] explicit operator bool() const noexcept {
      return !file.empty();
    }

    /// The color space, resolving `COLOR_SPACE_DEFAULT` by role.
    [[nodiscard]] ColorSpace effectiveColorSpace() const noexcept {
      if (colorSpace != COLOR_SPACE_DEFAULT) return colorSpace;
      return role == ROLE_BASECOLOR || role == ROLE_EMISSION
                 ? COLOR_SPACE_SRGB
                 : COLOR_SPACE_LINEAR;
    }

    /// The role.
    Role role{ROLE_BASECOLOR};

    /// The filename, relative to the manifest directory. Empty if the
    /// manifest does not declare this map.
    std::string file{};

    /// The channel, for scalar roles packed into multi-channel images.
    Channel channel{CHANNEL_NATURAL};

    /// The color space as written, `COLOR_SPACE_DEFAULT` if unwritten.
    ColorSpace colorSpace{COLOR_SPACE_DEFAULT};

    /// The normal convention. Only meaningful for `ROLE_NORMAL`.
    NormalConvention normalConvention{NORMAL_CONVENTION_OPENGL};

    /// The encoding scale of a horizon map: a stored channel value of 1
    /// means this slope. Only meaningful for `ROLE_HORIZON`.
    float maxSlope{8.0f};

    /// The multiplier on the decoded (linear, after-gamma) color. Only
    /// meaningful for `ROLE_EMISSION`.
    float factor{1.0f};

    /// The decode range: a stored value `v` decodes as
    /// `range[0] + (range[1] - range[0]) * v`, so the default is the
    /// identity. Only meaningful for `ROLE_ROUGHNESS` and
    /// `ROLE_METALLIC`.
    std::array<float, 2> range{{0.0f, 1.0f}};

    /// The line number of the declaration, for diagnostics.
    int lineNo{0};
  };

public:
  /// Clear everything.
  void clear() noexcept;

  /// Parse manifest source code.
  ///
  /// \param[in] sourceCode  The manifest source code.
  /// \param[in] sourceName  The name to blame in error messages.
  ///
  [[nodiscard]]
  std::optional<Error> parse(std::string_view sourceCode,
                             const std::string &sourceName = {}) noexcept;

  /// Load from file.
  [[nodiscard]]
  std::optional<Error> loadFromFile(const std::string &fileName) noexcept;

  /// Resolve the manifest filename for the given path.
  ///
  /// If the path is a directory, it must contain exactly one file with
  /// the `.pbr` extension; zero or more than one is ambiguous. If the
  /// path is not a directory, it is returned unchanged.
  ///
  /// \throw Error If the path is a directory without exactly one `.pbr`.
  ///
  [[nodiscard]]
  static std::string resolveFileName(const std::string &path);

  /// The display name of the given role, e.g., `"roughness"`.
  [[nodiscard]] static const char *roleName(Role role) noexcept;

  /// The effective relief scale: the span of the height map from 0 to 1
  /// as a fraction of one UV unit.
  ///
  /// This is `reliefScale` if the manifest declares it, else the ratio
  /// `physicalRelief / physicalExtent[0]` if it declares both, else
  /// nothing. It is the only relief quantity the consumers need: the
  /// parallax march, the normal implied by the height field, and the
  /// horizon decode all work in UV space.
  ///
  [[nodiscard]] std::optional<float> effectiveReliefScale() const noexcept {
    if (reliefScale) return reliefScale;
    if (physicalExtent && physicalRelief)
      return *physicalRelief / (*physicalExtent)[0];
    return std::nullopt;
  }

  /// Access the map with the given role.
  [[nodiscard]] const Map &operator[](Role role) const noexcept {
    return maps[int(role)];
  }

public:
  /// The schema version. Always 1 for now.
  int version{1};

  /// The display name. Optional!
  std::string name{};

  /// The description. Optional!
  std::string description{};

  /// The physical extent in meters covered by one repeat in U and V.
  /// Optional!
  std::optional<std::array<float, 2>> physicalExtent{};

  /// The physical relief in meters spanned by the height map from 0
  /// to 1. Optional!
  std::optional<float> physicalRelief{};

  /// The relief scale as written: the span of the height map from 0 to
  /// 1 as a fraction of one UV unit, overriding the physical pair.
  /// Optional! Read `effectiveReliefScale()` instead of this to resolve
  /// the physical pair as well.
  std::optional<float> reliefScale{};

  /// The random-rotation fraction for hex tiling in `[0, 1]`.
  float hexRotation{0.0f};

  /// The maps, indexed by `Role`. Absent maps have empty `file`s.
  Map maps[ROLE_COUNT]{};

  /// The non-fatal diagnostics of the last parse, each already carrying
  /// the source name and line number, for the caller to report however
  /// it reports warnings. A schema violation is an `Error` instead; a
  /// warning means the manifest is merely ambiguous, and says which
  /// reading won.
  std::vector<std::string> warnings{};
};

/// \}

} // namespace smdl
