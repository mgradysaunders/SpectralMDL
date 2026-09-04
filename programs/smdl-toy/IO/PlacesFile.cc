#include "IO/PlacesFile.h"

#include "smdl/Support/Error.h"
#include "smdl/Support/Strings.h"

#include <cstring>
#include <fstream>

// The `.places` reader and writer. Everything here is explicit-width
// little-endian I/O over the layout documented in `PlacesFile.h`; the
// only subtlety is the row-major 3x4 against the column-major
// `float4x4`.

namespace {

// The fixed-size header, exactly as it sits in the file.
class PlacesHeader final {
public:
  char magic[8]{};
  uint16_t version{};
  uint16_t flags{};
  uint32_t count{};
  uint32_t reserved{};
};

static_assert(sizeof(PlacesHeader) == 20, "the header is 20 bytes");
static_assert(sizeof(float) == 4, "records are 32-bit floats");

constexpr uint16_t FLAG_VARIANTS = 1;

// This code assumes a little-endian host outright, like the rest of the
// renderer's binary I/O; the check is here so a big-endian port fails
// loudly instead of scattering garbage.
[[nodiscard]] bool hostIsLittleEndian() noexcept {
  const uint32_t probe{1};
  return *reinterpret_cast<const unsigned char *>(&probe) == 1;
}

} // namespace

PlacesFile readPlacesFile(const std::string &fileName) {
  if (!hostIsLittleEndian())
    throw smdl::Error("'.places' I/O requires a little-endian host");
  auto stream{std::ifstream(fileName, std::ios::binary)};
  if (!stream)
    throw smdl::Error(
        smdl::concat("cannot open places buffer ", smdl::QuotedPath(fileName)));
  auto header{PlacesHeader()};
  stream.read(reinterpret_cast<char *>(&header), sizeof(header));
  if (!stream ||
      std::memcmp(header.magic, PLACES_MAGIC.data(), PLACES_MAGIC.size()) != 0)
    throw smdl::Error(smdl::concat(
        smdl::QuotedPath(fileName),
        " is not a '.places' buffer (bad magic; expected it to begin "
        "with \"SMDLPLCS\")"));
  if (header.version != 1)
    throw smdl::Error(smdl::concat("cannot read ", smdl::QuotedPath(fileName),
                                   ": version ", header.version,
                                   " (this build reads version 1)"));
  if (header.reserved != 0)
    throw smdl::Error(smdl::concat("cannot read ", smdl::QuotedPath(fileName),
                                   ": the reserved time-sample field is ",
                                   header.reserved,
                                   " (must be 0 in version 1)"));
  auto places{PlacesFile()};
  places.version = header.version;
  places.transforms.resize(header.count, float4x4(1.0f));
  for (auto &transform : places.transforms) {
    float rows[12]{};
    stream.read(reinterpret_cast<char *>(rows), sizeof(rows));
    // Row-major rows into the column-major matrix: rows[4 * i + j] is
    // row i, column j.
    for (int i = 0; i < 3; i++)
      for (int j = 0; j < 4; j++) transform[j][i] = rows[4 * i + j];
    transform[0][3] = transform[1][3] = transform[2][3] = 0.0f;
    transform[3][3] = 1.0f;
  }
  if (header.flags & FLAG_VARIANTS) {
    places.variants.resize(header.count);
    stream.read(reinterpret_cast<char *>(places.variants.data()),
                std::streamsize(sizeof(uint32_t) * places.variants.size()));
  }
  if (!stream)
    throw smdl::Error(smdl::concat("cannot read ", smdl::QuotedPath(fileName),
                                   ": truncated (the header "
                                   "promises ",
                                   header.count, " record(s))"));
  return places;
}

void writePlacesFile(const std::string &fileName, const PlacesFile &places) {
  if (!hostIsLittleEndian())
    throw smdl::Error("'.places' I/O requires a little-endian host");
  if (!places.variants.empty() &&
      places.variants.size() != places.transforms.size())
    throw smdl::Error(
        "the variant column must be empty or one entry per record");
  // The column earns its bytes only if some record uses it.
  auto anyVariant{false};
  for (const auto variant : places.variants)
    if (variant != PlacesFile::NO_VARIANT) anyVariant = true;
  auto stream{std::ofstream(fileName, std::ios::binary)};
  if (!stream)
    throw smdl::Error(smdl::concat("cannot write places buffer ",
                                   smdl::QuotedPath(fileName)));
  auto header{PlacesHeader()};
  std::memcpy(header.magic, PLACES_MAGIC.data(), PLACES_MAGIC.size());
  header.version = 1;
  header.flags = anyVariant ? FLAG_VARIANTS : 0;
  header.count = uint32_t(places.transforms.size());
  header.reserved = 0;
  stream.write(reinterpret_cast<const char *>(&header), sizeof(header));
  for (const auto &transform : places.transforms) {
    float rows[12]{};
    for (int i = 0; i < 3; i++)
      for (int j = 0; j < 4; j++) rows[4 * i + j] = transform[j][i];
    stream.write(reinterpret_cast<const char *>(rows), sizeof(rows));
  }
  if (anyVariant)
    stream.write(reinterpret_cast<const char *>(places.variants.data()),
                 std::streamsize(sizeof(uint32_t) * places.variants.size()));
  if (!stream)
    throw smdl::Error(smdl::concat("cannot write places buffer ",
                                   smdl::QuotedPath(fileName)));
}
