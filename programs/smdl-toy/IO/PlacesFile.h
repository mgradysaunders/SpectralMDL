/// \file
/// The `.places` binary instance buffer: the bulk payload a
/// `place <asset> * "<file>"` statement scatters from.
#pragma once

#include <cstdint>
#include <string>
#include <string_view>
#include <vector>

#include "Common.h"

/// The extension that conventionally marks a places buffer. Advisory,
/// as everywhere: the magic bytes decide.
constexpr std::string_view PLACES_EXTENSION = ".places";

/// The magic that begins a places buffer.
constexpr std::string_view PLACES_MAGIC = "SMDLPLCS";

/// A `.places` buffer in memory: one placement transform per record,
/// with an optional per-record variant index.
///
/// The file layout is little-endian and deliberately simple:
///
/// ```
/// offset  size  field
///      0     8  magic "SMDLPLCS"
///      8     2  u16 version, currently 1
///     10     2  u16 flags, bit 0 = a variant column follows the
///               transforms, bit 1 = the records are rigid, bit 2 = the
///               payload is one zlib stream
///     12     4  u32 record count N
/// --- the payload, deflated as a whole when bit 2 ---
///     48*N     general records: float 3x4, ROW-major, the top three
///              rows of the affine placement matrix, twelve floats
///              [m00 m01 m02 tx  m10 m11 m12 ty  m20 m21 m22 tz]
///        or
///     20*N     rigid records: four i16 snorm quaternion components
///              (w, x, y, z), then a float3 translation
///      4*N     u32 variant indices, only if flags bit 0; 0xFFFFFFFF
///              means no variant
/// ```
///
/// A record's transform stands exactly where a one-line `place`'s
/// operations would: composed under the bulk place's own operations and
/// over the asset's correction transform. Its variant index picks one
/// of the `variant { ... }` override blocks written at the call site,
/// by order of appearance.
///
/// Transforms are single precision on purpose: positions in half would
/// wobble by tens of pixels at modest scene scale and crack abutting
/// instances.
///
/// This format is deliberately **static**. A scatter moves as a whole
/// through its bulk place's own `motion` track, and anything that
/// deforms goes through a mesh clip; a per-record key block would cost
/// every writer the job of keying a million records to serve a case
/// those two already cover.
///
class PlacesFile final {
public:
  /// The format version read or to be written.
  uint16_t version{1};

  /// Store rotation and translation only, quantizing each quaternion
  /// component to a 16-bit snorm: 20 bytes a record against 48, at a
  /// worst-case rotation error of about 3e-5 radians.
  ///
  /// The writer refuses a transform this cannot express rather than
  /// quantizing one silently, so a scaled, skewed, or mirrored record is
  /// a diagnostic and not a wrong picture.
  ///
  bool isRigid{};

  /// Deflate the payload. What the reader found, and what the writer
  /// will do, so that a round trip preserves the encoding.
  bool isCompressed{};

  /// One full affine matrix per record, rebuilt from (or reduced to)
  /// the stored record; the bottom row is always (0, 0, 0, 1).
  std::vector<float4x4> transforms{};

  /// The per-record variant indices, either empty (no column) or
  /// exactly one per record, `NO_VARIANT` where a record has none.
  std::vector<uint32_t> variants{};

  static constexpr uint32_t NO_VARIANT = 0xFFFFFFFFU;

  /// Anything with a variant at all?
  [[nodiscard]] bool hasVariants() const noexcept { return !variants.empty(); }
};

/// Read a `.places` buffer.
///
/// \throws smdl::Error  If the file cannot be read, the magic or
///                      version is wrong, or the sizes do not add up.
///                      Fail-loud: a truncated scatter silently dropped
///                      would be an empty patch of ground and a hunt.
///
[[nodiscard]] PlacesFile readPlacesFile(const std::string &fileName);

/// Write a `.places` buffer.
///
/// `variants` must be empty or the same length as `transforms`; the
/// column is written only when some record actually has a variant.
///
/// \throws smdl::Error  If the file cannot be written, or `isRigid` is
///                      set and some transform is not a rigid motion.
///
void writePlacesFile(const std::string &fileName, const PlacesFile &places);
