#include "IO/PlacesFile.h"
#include "IO/BinaryFile.h"

#include "Transform.h"

#include "smdl/Support/Error.h"
#include "smdl/Support/Filesystem.h"
#include "smdl/Support/Strings.h"

#include <cmath>
#include <fstream>

// The `.places` reader and writer. Everything here is explicit-width
// little-endian I/O over the layout documented in `PlacesFile.h`; the
// subtleties are the row-major 3x4 against the column-major `float4x4`
// and the snorm quantization of a rigid record.

namespace {

// The fixed-size header, exactly as it sits in the file.
class PlacesHeader final {
public:
  char magic[8]{};
  uint16_t version{};
  uint16_t flags{};
  uint32_t count{};
};

// A general record: the top three rows, row-major.
class GeneralRecord final {
public:
  float rows[12]{};
};

// A rigid record: the rotation as four snorm components in the
// `(w, x, y, z)` order `TransformDecomposition` spells, then the
// translation.
class RigidRecord final {
public:
  int16_t quaternion[4]{};
  float translation[3]{};
};

static_assert(sizeof(PlacesHeader) == 16, "the header is 16 bytes");
static_assert(sizeof(GeneralRecord) == 48, "a general record is 48 bytes");
static_assert(sizeof(RigidRecord) == 20, "a rigid record is 20 bytes");
static_assert(sizeof(float) == 4, "records are 32-bit floats");

constexpr uint16_t FLAG_VARIANTS = 1;
constexpr uint16_t FLAG_RIGID = 2;
constexpr uint16_t FLAG_COMPRESSED = 4;
constexpr uint16_t FLAG_ALL = FLAG_VARIANTS | FLAG_RIGID | FLAG_COMPRESSED;

// The snorm scale. 32767 rather than 32768 so that +1 and -1 are both
// exact, which keeps an axis-aligned rotation exactly axis-aligned.
constexpr float SNORM_SCALE = 32767.0f;

[[nodiscard]] GeneralRecord generalOf(const float4x4 &transform) noexcept {
  GeneralRecord record{};
  for (int i = 0; i < 3; i++)
    for (int j = 0; j < 4; j++) record.rows[4 * i + j] = transform[j][i];
  return record;
}

[[nodiscard]] float4x4 transformOf(const GeneralRecord &record) noexcept {
  float4x4 transform{1.0f};
  // Row-major rows into the column-major matrix: rows[4 * i + j] is
  // row i, column j.
  for (int i = 0; i < 3; i++)
    for (int j = 0; j < 4; j++) transform[j][i] = record.rows[4 * i + j];
  transform[0][3] = transform[1][3] = transform[2][3] = 0.0f;
  transform[3][3] = 1.0f;
  return transform;
}

[[nodiscard]] RigidRecord
rigidOf(const TransformDecomposition &parts) noexcept {
  RigidRecord record{};
  for (int i = 0; i < 4; i++)
    record.quaternion[i] = int16_t(std::lround(
        std::clamp(parts.quaternion[i], -1.0f, 1.0f) * SNORM_SCALE));
  for (int i = 0; i < 3; i++) record.translation[i] = parts.translation[i];
  return record;
}

[[nodiscard]] float4x4 transformOf(const RigidRecord &record) noexcept {
  TransformDecomposition parts{};
  for (int i = 0; i < 4; i++)
    parts.quaternion[i] = record.quaternion[i] * (1.0f / SNORM_SCALE);
  for (int i = 0; i < 3; i++) parts.translation[i] = record.translation[i];
  // `composeTransform()` renormalizes, which is what undoes the
  // rounding the four components picked up independently.
  return composeTransform(parts);
}

} // namespace

PlacesFile readPlacesFile(const std::string &fileName) {
  requireLittleEndianHost("'.places'");
  const auto fail{[&](auto &&...args) {
    throw smdl::Error(
        smdl::concat("Cannot read ", SpellFilePath(fileName), ": ", args...));
  }};
  const std::string contents{smdl::readOrThrow(fileName)};
  PlacesHeader header{};
  if (contents.size() >= sizeof(header))
    std::memcpy(&header, contents.data(), sizeof(header));
  if (!hasMagic(header.magic, PLACES_MAGIC))
    throw smdl::Error(smdl::concat(
        SpellFilePath(fileName),
        " is not a '.places' buffer (bad magic; expected it to begin "
        "with \"SMDLPLCS\")"));
  if (header.version != 1)
    fail("version ", header.version, " (this build reads version 1)");
  if (header.flags & ~FLAG_ALL)
    fail("its flags hold unknown bits (", header.flags & ~uint16_t(FLAG_ALL),
         ")");
  PlacesFile places{};
  places.version = header.version;
  places.isRigid = (header.flags & FLAG_RIGID) != 0;
  places.isCompressed = (header.flags & FLAG_COMPRESSED) != 0;
  const bool hasVariants{(header.flags & FLAG_VARIANTS) != 0};
  const size_t recordSize{places.isRigid ? sizeof(RigidRecord)
                                         : sizeof(GeneralRecord)};
  const size_t payloadSize{header.count * recordSize +
                           (hasVariants ? header.count * sizeof(uint32_t) : 0)};
  const BinaryPayload payload{contents, sizeof(header), payloadSize,
                              places.isCompressed, fileName};
  ByteReader reader{payload.bytes()};
  places.transforms.reserve(header.count);
  for (uint32_t i = 0; i < header.count; i++) {
    if (places.isRigid) {
      RigidRecord record{};
      reader.takeRecord(record);
      places.transforms.push_back(transformOf(record));
    } else {
      GeneralRecord record{};
      reader.takeRecord(record);
      places.transforms.push_back(transformOf(record));
    }
  }
  if (hasVariants) reader.takeArray(places.variants, header.count);
  if (!reader.empty())
    fail("truncated (the header promises ",
         SpellCounted(header.count, "record"), ")");
  return places;
}

void writePlacesFile(const std::string &fileName, const PlacesFile &places) {
  requireLittleEndianHost("'.places'");
  if (!places.variants.empty() &&
      places.variants.size() != places.transforms.size())
    throw smdl::Error(
        "The variant column must be empty or one entry per record");
  // The column earns its bytes only if some record uses it.
  bool anyVariant{false};
  for (const auto variant : places.variants)
    if (variant != PlacesFile::NO_VARIANT) anyVariant = true;
  std::vector<std::byte> payload{};
  payload.reserve(places.transforms.size() * (places.isRigid
                                                  ? sizeof(RigidRecord)
                                                  : sizeof(GeneralRecord)));
  for (size_t i = 0; i < places.transforms.size(); i++) {
    if (!places.isRigid) {
      pushRecord(payload, generalOf(places.transforms[i]));
      continue;
    }
    const TransformDecomposition parts{
        decomposeTransform(places.transforms[i])};
    if (!parts.isRigid())
      throw smdl::Error(smdl::concat(
          "Cannot write places buffer ", SpellFilePath(fileName), ": record ",
          i,
          " is scaled, skewed, or mirrored, which a rigid record cannot "
          "express"));
    pushRecord(payload, rigidOf(parts));
  }
  if (anyVariant) pushArray(payload, places.variants);
  if (places.isCompressed) payload = smdl::compressBytes(payload);
  std::ofstream stream{fileName, std::ios::binary};
  if (!stream)
    throw smdl::Error(
        smdl::concat("Cannot write places buffer ", SpellFilePath(fileName)));
  PlacesHeader header{};
  setMagic(header.magic, PLACES_MAGIC);
  header.version = 1;
  header.flags = uint16_t((anyVariant ? FLAG_VARIANTS : 0) |
                          (places.isRigid ? FLAG_RIGID : 0) |
                          (places.isCompressed ? FLAG_COMPRESSED : 0));
  header.count = uint32_t(places.transforms.size());
  putRecord(stream, header);
  stream.write(reinterpret_cast<const char *>(payload.data()),
               std::streamsize(payload.size()));
  if (!stream)
    throw smdl::Error(
        smdl::concat("Cannot write places buffer ", SpellFilePath(fileName)));
}
