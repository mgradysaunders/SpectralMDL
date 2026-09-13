/// \file
/// The `.curves` binary fiber format (hair, grass, cables).
///
/// Deliberately Embree-free, like `PlacesFile.h`, so the layout toolchain
/// and its tests build without `SMDL_TOY`; the Embree-backed runtime
/// object a curves asset instantiates lives in `Curves.h`.
#pragma once

#include <cstdint>
#include <string>
#include <string_view>
#include <vector>

#include "Common.h"

/// The extension that conventionally marks a curves file. Advisory,
/// as everywhere: the magic bytes decide.
constexpr std::string_view CURVES_EXTENSION = ".curves";

/// The magic that begins a curves file.
constexpr std::string_view CURVES_MAGIC = "SMDLCRVS";

/// Where an instant falls among a file's key times: the two keys that
/// bracket it and the fraction of the way from one to the other.
///
/// Found once for a whole file rather than once per point, since every
/// control point of a groom shares the one list of times.
class CurvesKeyBlend final {
public:
  uint32_t lo{};
  uint32_t hi{};
  float fraction{};

  /// Do the two ends coincide, so that the blend is one stored key?
  [[nodiscard]] bool isStill() const noexcept { return lo == hi; }
};

/// A `.curves` file in memory: strands of control points, each point a
/// position and radius at each of the file's key times, under one basis
/// that says what the points mean.
///
/// The file layout is little-endian and deliberately simple, like
/// `.places`:
///
/// ```
/// offset  size      field
///      0      8     magic "SMDLCRVS"
///      8      2     u16 version, currently 1
///     10      2     u16 basis: 0 = linear, 1 = cubic B-spline,
///                   2 = Catmull-Rom
///     12      2     u16 flags, bit 0 = a per-strand root UV column
///                   follows the points, bit 1 = the payload is one
///                   zlib stream
///     14      2     u16 key count K, at least 1
///     16      4     u32 strand count S
///     20      4     u32 point count P
///     24    4*K     float key times in seconds, ascending
/// --- the payload, deflated as a whole when flags bit 1 ---
///        4*(S+1)    u32 offsets: offsets[i] is the first point of
///                   strand i; offsets[0] = 0, strictly increasing,
///                   offsets[S] = P
///         16*P*K    float4 points (x, y, z, radius): K consecutive
///                   values per control point, one per key, strand by
///                   strand
///           8*S     float2 root UVs, only if flags bit 0
/// ```
///
/// The **basis** is a fact about the data, not a rendering choice: the
/// same point array is different geometry under a different basis, so
/// the file that stores the points must say what they mean. The
/// cross-section rendered around them (swept tube or camera-facing
/// ribbon) is the opposite kind of fact, so it lives in the layout
/// grammar instead; see `CurvesSpec`.
///
/// - `LINEAR`: the points are polyline vertices, at least 2 per strand.
/// - `BSPLINE`: the points are uniform cubic B-spline control points
///   exactly as authored, at least 4 per strand. The curve does not in
///   general touch any of them; an author who wants the ends pinned
///   writes the end padding into the file, because the authored basis
///   is exact and the loader must not second-guess it.
/// - `CATMULL_ROM`: the curve passes through every stored point, at
///   least 2 per strand. The loader duplicates each strand's first and
///   last point to give Embree's four-point window its phantom ends,
///   so a converter from polyline grooms (Blender hair curves, Cem
///   Yuksel's `.hair` models) passes its points through verbatim.
///
/// Radii are object-space half-widths, interpolated in the same basis
/// as positions (control radii may dip below zero for B-spline as long
/// as the interpolated radius stays non-negative, exactly Embree's
/// rule). Under an instance transform the radius scales with the
/// geometry, and a non-uniform scale sweeps ellipses: Embree intersects
/// in object space, so silhouette and shading stay consistent however
/// the instance is deformed.
///
/// The **key times** are absolute readings of the render clock, exactly
/// as a layout `motion { at <seconds> ... }` block's are, which is what
/// makes a groom that moves mean the same thing at every shutter. The
/// strands are the only place fiber motion can live: a layout track
/// swings a whole groom as one body, and a mesh clip cannot reach a
/// file of curves. A file with one key is a still groom, and its one
/// time is never consulted.
///
/// Points are single precision on purpose, for the same arithmetic that
/// decided `.places`. There is deliberately no ASCII twin: a groom is
/// bulk data no human ever edits, `-dump-curves` prints a summary, and
/// getting data in is tooling, not format.
class CurvesFile final {
public:
  enum class Basis : uint16_t {
    LINEAR = 0,
    BSPLINE = 1,
    CATMULL_ROM = 2,
  };

  /// The format version read or to be written.
  uint16_t version{1};

  /// What the points mean; see the class comment.
  Basis basis{Basis::BSPLINE};

  /// Deflate the payload. What the reader found, and what the writer
  /// will do, so that a round trip preserves the encoding.
  bool isCompressed{};

  /// The key times in ascending seconds, at least one. One key is a
  /// still groom.
  std::vector<float> keyTimes{0.0f};

  /// The fence-post offsets over CONTROL POINTS, not over stored
  /// entries: strand `i` is the points
  /// `[strandOffsets[i], strandOffsets[i + 1])`. Size S + 1, first 0,
  /// strictly increasing, last equal to `pointCount()`.
  std::vector<uint32_t> strandOffsets{};

  /// The control points, `(x, y, z, radius)` per entry, `keyCount()`
  /// consecutive entries per control point.
  std::vector<float4> points{};

  /// The per-strand root UVs, either empty (no column) or exactly one
  /// per strand: where each strand's root sits on the scalp or ground
  /// it grows from, for texturing a groom by a surface map.
  std::vector<float2> rootUVs{};

  [[nodiscard]] uint32_t strandCount() const noexcept {
    return strandOffsets.empty() ? 0 : uint32_t(strandOffsets.size() - 1);
  }

  [[nodiscard]] uint32_t keyCount() const noexcept {
    return uint32_t(keyTimes.size());
  }

  [[nodiscard]] uint32_t pointCount() const noexcept {
    return keyTimes.empty() ? 0 : uint32_t(points.size() / keyTimes.size());
  }

  /// Does the groom move, so that a still and a moving key differ?
  [[nodiscard]] bool isMoving() const noexcept { return keyCount() > 1; }

  [[nodiscard]] bool hasRootUVs() const noexcept { return !rootUVs.empty(); }

  /// The `keyCount()` stored entries of one control point.
  [[nodiscard]] const float4 *keysOf(uint32_t point) const noexcept {
    return points.data() + size_t(point) * keyTimes.size();
  }

  /// Where `seconds` falls among `keyTimes`, clamped to the ends, and
  /// exact at a key's own time. An empty or one-key file answers that
  /// one key at every instant.
  [[nodiscard]] CurvesKeyBlend blendAt(float seconds) const noexcept;

  /// The control point `point` at `blend`, linearly interpolated, which
  /// is what Embree does between two vertex buffers.
  [[nodiscard]] float4 pointAt(uint32_t point,
                               const CurvesKeyBlend &blend) const noexcept {
    const float4 *keys{keysOf(point)};
    if (blend.isStill()) return keys[blend.lo];
    return (1.0f - blend.fraction) * keys[blend.lo] +
           blend.fraction * keys[blend.hi];
  }

  /// Does a key sit strictly inside the open interval `(open, shut)`?
  /// Such a key is not represented by the two samples the renderer
  /// takes, and the loader says so.
  [[nodiscard]] bool hasKeyBetween(float open, float shut) const noexcept;

  /// The fewest points a strand of `basis` can have; see the class
  /// comment.
  [[nodiscard]] static uint32_t minPointsPerStrand(Basis basis) noexcept {
    return basis == Basis::BSPLINE ? 4 : 2;
  }

  /// The basis as the diagnostics spell it.
  [[nodiscard]] static std::string_view basisName(Basis basis) noexcept {
    switch (basis) {
    case Basis::LINEAR:
      return "linear";
    case Basis::BSPLINE:
      return "b-spline";
    case Basis::CATMULL_ROM:
      return "catmull-rom";
    default:
      return "unknown";
    }
  }
};

/// Read a `.curves` file.
///
/// \throws smdl::Error  If the file cannot be read, the magic, version,
///                      or basis is wrong, the key times are not
///                      ascending, the offsets are not a monotone
///                      fence-post table, any strand has fewer points
///                      than its basis needs, or the sizes do not add
///                      up. Fail-loud, like `.places`: a truncated groom
///                      silently dropped would be a bald patch and a
///                      hunt.
///
[[nodiscard]] CurvesFile readCurvesFile(const std::string &fileName);

/// Write a `.curves` file.
///
/// `rootUVs` must be empty or one per strand, and the key times and
/// offsets must be the valid tables `readCurvesFile()` demands.
///
/// \throws smdl::Error  If the data is inconsistent or the file cannot
///                      be written.
///
void writeCurvesFile(const std::string &fileName, const CurvesFile &curves);

/// The differential geometry of the fiber CENTER at a parameter `u` of
/// one basis window, in object space: the point on the center curve,
/// the unnormalized derivative along it, and the interpolated radius.
/// This is the analytic half a curve hit needs beyond what Embree
/// reports; the surface normal comes from Embree's `Ng` (tube) or the
/// ray (ribbon).
class CurveAxis final {
public:
  float3 point{};
  float3 tangent{};
  float radius{};
};

/// Evaluate the fiber center at `u` in [0, 1] of the basis window
/// starting at `window`: 2 consecutive control points for the linear
/// basis, 4 for the cubic ones, exactly the window Embree's segment
/// index buffer names. The cubic definitions match Embree's (cardinal
/// B-spline with the implicit equidistant knot vector; Catmull-Rom
/// through p1 and p2 with tangents (p2 - p0) / 2 and (p3 - p1) / 2).
[[nodiscard]] CurveAxis evalCurveAxis(CurvesFile::Basis basis,
                                      const float4 *window, float u);
