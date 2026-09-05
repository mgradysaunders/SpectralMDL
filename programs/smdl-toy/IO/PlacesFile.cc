#include "IO/PlacesFile.h"
#include "IO/BinaryFile.h"

#include "smdl/Support/Error.h"
#include "smdl/Support/Strings.h"

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

} // namespace

PlacesFile readPlacesFile(const std::string &fileName) {
  requireLittleEndianHost("'.places'");
  auto stream{std::ifstream(fileName, std::ios::binary)};
  if (!stream)
    throw smdl::Error(
        smdl::concat("cannot open places buffer ", smdl::QuotedPath(fileName)));
  auto header{PlacesHeader()};
  getRecord(stream, header);
  if (!stream || !hasMagic(header.magic, PLACES_MAGIC))
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
    getRecord(stream, rows);
    // Row-major rows into the column-major matrix: rows[4 * i + j] is
    // row i, column j.
    for (int i = 0; i < 3; i++)
      for (int j = 0; j < 4; j++) transform[j][i] = rows[4 * i + j];
    transform[0][3] = transform[1][3] = transform[2][3] = 0.0f;
    transform[3][3] = 1.0f;
  }
  if (header.flags & FLAG_VARIANTS)
    getArray(stream, places.variants, header.count);
  if (!stream)
    throw smdl::Error(smdl::concat("cannot read ", smdl::QuotedPath(fileName),
                                   ": truncated (the header "
                                   "promises ",
                                   header.count, " record(s))"));
  return places;
}

void writePlacesFile(const std::string &fileName, const PlacesFile &places) {
  requireLittleEndianHost("'.places'");
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
  setMagic(header.magic, PLACES_MAGIC);
  header.version = 1;
  header.flags = anyVariant ? FLAG_VARIANTS : 0;
  header.count = uint32_t(places.transforms.size());
  header.reserved = 0;
  putRecord(stream, header);
  for (const auto &transform : places.transforms) {
    float rows[12]{};
    for (int i = 0; i < 3; i++)
      for (int j = 0; j < 4; j++) rows[4 * i + j] = transform[j][i];
    putRecord(stream, rows);
  }
  if (anyVariant) putArray(stream, places.variants);
  if (!stream)
    throw smdl::Error(smdl::concat("cannot write places buffer ",
                                   smdl::QuotedPath(fileName)));
}
