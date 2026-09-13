#include "Fixtures.h"

#include <fstream>
#include <iterator>
#include <optional>
#include <string>
#include <vector>

#include "smdl/Support/Error.h"
#include "smdl/Support/MD5Hash.h"

#include "IO/DNG.h"

// The DNG writer. What matters is that a reader finds the tags where
// the format says they are, that the numbers reach the strip in order,
// that an untiled sensor's planes are laid down in red, green, blue
// order, and that a shape the format cannot carry is refused rather
// than written.
//
// One readout is one file, so the hash below pins the rest: it is what
// these bytes were when Adobe's own `dng_validate` last accepted them,
// and a change to the writer has to restate it deliberately. Nothing
// the writer puts in a file comes from the clock or the host, which is
// what lets a hash stand for it at all.

namespace {

// The hashes of the two files the cases below write. See the note
// above before restating one.
//
// \{
const std::string CFA_HASH{"5d3030f5a999db278f83024e74ac8b61"};
const std::string LINEAR_HASH{"a561a906f87dc6c5e5fe7c674e67804f"};
// \}

constexpr size_t PIXEL_COUNT_X = 16;
constexpr size_t PIXEL_COUNT_Y = 12;

// A mosaic whose numbers say where they came from, so that a sample out
// of place is a sample out of order.
[[nodiscard]] std::vector<uint16_t> mosaicNumbers() {
  std::vector<uint16_t> numbers{};
  for (size_t y = 0; y < PIXEL_COUNT_Y; y++)
    for (size_t x = 0; x < PIXEL_COUNT_X; x++)
      numbers.push_back(uint16_t(300 + 7 * x + 13 * y));
  return numbers;
}

// The same frame with every plane at every pixel, each plane a decade
// apart so that the order they are written in is readable.
[[nodiscard]] std::vector<uint16_t> planeNumbers() {
  std::vector<uint16_t> numbers{};
  for (size_t y = 0; y < PIXEL_COUNT_Y; y++)
    for (size_t x = 0; x < PIXEL_COUNT_X; x++)
      for (size_t k = 0; k < 3; k++)
        numbers.push_back(uint16_t(300 + 7 * x + 13 * y + 1000 * k));
  return numbers;
}

// A whole instrument, every field filled in with a number that is not a
// default, so that a change to any one of them changes the file. The
// matrices are a real fit's, which is what makes the rounding of a
// rational worth pinning.
[[nodiscard]] DNGImage makeImage(smdl::Span<const uint16_t> numbers,
                                 bool hasCFA) {
  DNGImage image{};
  image.pixelCountX = PIXEL_COUNT_X;
  image.pixelCountY = PIXEL_COUNT_Y;
  image.digitalNumbers = numbers;
  image.hasCFA = hasCFA;
  image.cfa = {0, 1, 1, 2};
  image.blackLevel = 128.5;
  image.whiteLevel = 4000;
  image.xyzToCamera1 = double3x3(double3(1.2912, -0.5685, -0.1009),
                                 double3(-0.4343, 1.5279, 0.1754),
                                 double3(-0.0586, 0.2613, 0.7396));
  image.xyzToCamera2 = double3x3(double3(0.8768, -0.5357, -0.1777),
                                 double3(-0.1692, 1.3216, 0.2462),
                                 double3(-0.1237, 0.1719, 0.5339));
  image.cameraToXYZ1 = double3x3(double3(0.7277, 0.2464, 0.1188),
                                 double3(0.1162, 0.8021, -0.4113),
                                 double3(0.1203, -0.0485, 1.1174));
  image.cameraToXYZ2 = double3x3(double3(0.7005, 0.2728, 0.0756),
                                 double3(0.1569, 0.8499, -0.2543),
                                 double3(0.1068, -0.1227, 1.0036));
  image.illuminant1 = 17;
  image.illuminant2 = 21;
  image.asShotNeutral = double3(0.528809, 1.0, 0.658513);
  image.baselineExposure = 0.746673;
  image.noiseProfile.fill(double2(1.04961e-05, 2.47878e-10));
  image.window = int4(4, 4, 12, 8);
  image.make = "smdl-toy";
  image.model = "Test Sensor";
  image.uniqueCameraModel = "smdl-toy Test Sensor";
  image.software = "smdl-toy";
  image.description = "smdl-toy readout, all noise, seed 0";
  image.exposureTime = 0.01;
  image.fNumber = 16.0;
  image.iso = 100.0;
  return image;
}

[[nodiscard]] std::vector<uint8_t> readFile(const std::string &fileName) {
  std::ifstream stream{fileName, std::ios::binary};
  return std::vector<uint8_t>(std::istreambuf_iterator<char>(stream),
                              std::istreambuf_iterator<char>());
}

[[nodiscard]] uint32_t at16(const std::vector<uint8_t> &bytes, size_t offset) {
  REQUIRE(offset + 2 <= bytes.size());
  return uint32_t(bytes[offset]) | uint32_t(bytes[offset + 1]) << 8;
}

[[nodiscard]] uint32_t at32(const std::vector<uint8_t> &bytes, size_t offset) {
  REQUIRE(offset + 4 <= bytes.size());
  return at16(bytes, offset) | at16(bytes, offset + 2) << 16;
}

// One entry as a reader finds it: the type and the count it declares,
// and where its payload is, which is the entry itself for the four
// bytes that fit there.
struct Entry final {
  uint32_t type{};
  uint32_t count{};
  size_t payload{};
};

// The entry for `tag` in the directory at `directory`, or nothing.
[[nodiscard]] std::optional<Entry> findEntry(const std::vector<uint8_t> &bytes,
                                             size_t directory, uint16_t tag) {
  const uint32_t entryCount{at16(bytes, directory)};
  for (uint32_t i = 0; i < entryCount; i++) {
    const size_t entry{directory + 2 + 12 * i};
    if (at16(bytes, entry) != tag) continue;
    Entry found{};
    found.type = at16(bytes, entry + 2);
    found.count = at32(bytes, entry + 4);
    // A byte, short, or long payload of four bytes or fewer sits in the
    // entry; everything else is pointed at.
    const uint32_t size{found.type == 1 || found.type == 2 || found.type == 7
                            ? 1u
                        : found.type == 3 ? 2u
                        : found.type == 4 ? 4u
                                          : 8u};
    found.payload =
        size * found.count <= 4 ? entry + 8 : size_t(at32(bytes, entry + 8));
    return found;
  }
  return std::nullopt;
}

// The `i`th value of an entry of bytes, shorts, or longs.
[[nodiscard]] uint32_t valueOf(const std::vector<uint8_t> &bytes,
                               const Entry &entry, uint32_t i = 0) {
  switch (entry.type) {
  case 1:
    return bytes[entry.payload + i];
  case 3:
    return at16(bytes, entry.payload + 2 * i);
  default:
    return at32(bytes, entry.payload + 4 * i);
  }
}

// The tags the cases below name, which are the ones a reader cannot do
// without.
enum : uint16_t {
  TAG_IMAGE_WIDTH = 256,
  TAG_IMAGE_LENGTH = 257,
  TAG_BITS_PER_SAMPLE = 258,
  TAG_COMPRESSION = 259,
  TAG_PHOTOMETRIC_INTERPRETATION = 262,
  TAG_STRIP_OFFSETS = 273,
  TAG_ORIENTATION = 274,
  TAG_SAMPLES_PER_PIXEL = 277,
  TAG_STRIP_BYTE_COUNTS = 279,
  TAG_CFA_REPEAT_PATTERN_DIM = 33421,
  TAG_CFA_PATTERN = 33422,
  TAG_EXIF_IFD = 34665,
  TAG_ISO_SPEED = 34867,
  TAG_DNG_VERSION = 50706,
  TAG_UNIQUE_CAMERA_MODEL = 50708,
  TAG_CFA_PLANE_COLOR = 50710,
  TAG_BLACK_LEVEL = 50714,
  TAG_WHITE_LEVEL = 50717,
  TAG_DEFAULT_CROP_ORIGIN = 50719,
  TAG_DEFAULT_CROP_SIZE = 50720,
  TAG_COLOR_MATRIX_1 = 50721,
  TAG_AS_SHOT_NEUTRAL = 50728,
  TAG_FORWARD_MATRIX_2 = 50965,
  TAG_NOISE_PROFILE = 51041
};

} // namespace

TEST_CASE("DNG: the file a readout makes") {
  TempDir tmpDir{"toy-dng"};
  const std::string fileName{(tmpDir / "readout.dng").string()};
  const std::vector<uint16_t> mosaic{mosaicNumbers()};
  const DNGImage image{makeImage(mosaic, true)};
  SUBCASE("Begins as a little-endian TIFF whose first directory is the raw") {
    writeDNGFile(fileName, image);
    const std::vector<uint8_t> bytes{readFile(fileName)};
    REQUIRE(bytes.size() > 8);
    CHECK(bytes[0] == 'I');
    CHECK(bytes[1] == 'I');
    CHECK(at16(bytes, 2) == 42);
    const size_t directory{at32(bytes, 4)};
    CHECK(directory == 8);
    // Every entry in ascending order of tag, which a reader is entitled
    // to assume, and nothing after the one directory.
    const uint32_t entryCount{at16(bytes, directory)};
    CHECK(entryCount > 20);
    for (uint32_t i = 1; i < entryCount; i++)
      CHECK(at16(bytes, directory + 2 + 12 * i) >
            at16(bytes, directory + 2 + 12 * (i - 1)));
    CHECK(at32(bytes, directory + 2 + 12 * entryCount) == 0);
  }
  SUBCASE("States the mosaic a raw developer reads") {
    writeDNGFile(fileName, image);
    const std::vector<uint8_t> bytes{readFile(fileName)};
    const auto entry{[&](uint16_t tag) {
      const std::optional<Entry> found{findEntry(bytes, 8, tag)};
      REQUIRE_MESSAGE(found.has_value(), "no entry for tag ", tag);
      return *found;
    }};
    CHECK(valueOf(bytes, entry(TAG_IMAGE_WIDTH)) == PIXEL_COUNT_X);
    CHECK(valueOf(bytes, entry(TAG_IMAGE_LENGTH)) == PIXEL_COUNT_Y);
    CHECK(valueOf(bytes, entry(TAG_BITS_PER_SAMPLE)) == 16);
    CHECK(valueOf(bytes, entry(TAG_COMPRESSION)) == 1);
    CHECK(valueOf(bytes, entry(TAG_ORIENTATION)) == 1);
    CHECK(valueOf(bytes, entry(TAG_SAMPLES_PER_PIXEL)) == 1);
    // The color filter array value of TIFF-EP, with the tile and the
    // colors its entries name.
    CHECK(valueOf(bytes, entry(TAG_PHOTOMETRIC_INTERPRETATION)) == 32803);
    CHECK(valueOf(bytes, entry(TAG_CFA_REPEAT_PATTERN_DIM), 0) == 2);
    CHECK(valueOf(bytes, entry(TAG_CFA_REPEAT_PATTERN_DIM), 1) == 2);
    for (uint32_t i = 0; i < 4; i++) {
      CAPTURE(i);
      CHECK(valueOf(bytes, entry(TAG_CFA_PATTERN), i) == image.cfa[i]);
      if (i < 3) CHECK(valueOf(bytes, entry(TAG_CFA_PLANE_COLOR), i) == i);
    }
    // The levels, the one crop, and the counts of the calibration the
    // format states per plane.
    CHECK(valueOf(bytes, entry(TAG_WHITE_LEVEL)) == image.whiteLevel);
    CHECK(entry(TAG_BLACK_LEVEL).count == 1);
    CHECK(at32(bytes, entry(TAG_BLACK_LEVEL).payload) == 1285000);
    CHECK(at32(bytes, entry(TAG_BLACK_LEVEL).payload + 4) == 10000);
    CHECK(valueOf(bytes, entry(TAG_DEFAULT_CROP_ORIGIN), 0) == 4);
    CHECK(valueOf(bytes, entry(TAG_DEFAULT_CROP_ORIGIN), 1) == 4);
    CHECK(valueOf(bytes, entry(TAG_DEFAULT_CROP_SIZE), 0) == 8);
    CHECK(valueOf(bytes, entry(TAG_DEFAULT_CROP_SIZE), 1) == 4);
    CHECK(entry(TAG_COLOR_MATRIX_1).count == 9);
    CHECK(entry(TAG_FORWARD_MATRIX_2).count == 9);
    CHECK(entry(TAG_AS_SHOT_NEUTRAL).count == 3);
    CHECK(entry(TAG_NOISE_PROFILE).count == 6);
    // A matrix goes in row scan order over ten thousand, against the
    // column-major matrix the program holds: the numerator of entry `i`
    // sits at `8 i` and its denominator four bytes after.
    const size_t matrix{entry(TAG_COLOR_MATRIX_1).payload};
    CHECK(int32_t(at32(bytes, matrix)) == 12912);
    CHECK(at32(bytes, matrix + 4) == 10000);
    CHECK(int32_t(at32(bytes, matrix + 8)) == -4343);
    CHECK(int32_t(at32(bytes, matrix + 24)) == -5685);
    CHECK(valueOf(bytes, entry(TAG_DNG_VERSION), 0) == 1);
    CHECK(valueOf(bytes, entry(TAG_DNG_VERSION), 1) == 4);
    CHECK(std::string(reinterpret_cast<const char *>(
              &bytes[entry(TAG_UNIQUE_CAMERA_MODEL).payload])) ==
          image.uniqueCameraModel);
    // The shot, which the Exif directory the raw one points at holds.
    const Entry exif{entry(TAG_EXIF_IFD)};
    CHECK(valueOf(bytes, findEntry(bytes, valueOf(bytes, exif), TAG_ISO_SPEED)
                             .value()) == 100);
  }
  SUBCASE("Lays the numbers down row by row in the one strip") {
    writeDNGFile(fileName, image);
    const std::vector<uint8_t> bytes{readFile(fileName)};
    const Entry offsets{findEntry(bytes, 8, TAG_STRIP_OFFSETS).value()};
    const Entry counts{findEntry(bytes, 8, TAG_STRIP_BYTE_COUNTS).value()};
    const size_t strip{valueOf(bytes, offsets)};
    REQUIRE(valueOf(bytes, counts) == 2 * mosaic.size());
    REQUIRE(bytes.size() == strip + 2 * mosaic.size());
    for (size_t i = 0; i < mosaic.size(); i++) {
      CAPTURE(i);
      CHECK(at16(bytes, strip + 2 * i) == mosaic[i]);
    }
  }
  SUBCASE("Is the file it was pinned at") {
    writeDNGFile(fileName, image);
    CHECK(std::string(smdl::MD5Hash::hashFile(fileName)) == CFA_HASH);
  }
  SUBCASE("Refuses what it cannot write") {
    const auto write{[&](const DNGImage &bad) {
      return smdl::catchAndReturnError([&] { writeDNGFile(fileName, bad); });
    }};
    DNGImage empty{image};
    empty.pixelCountY = 0;
    CHECK_ERROR(write(empty), "the frame is empty");
    DNGImage short_{image};
    short_.pixelCountX = PIXEL_COUNT_X + 1;
    CHECK_ERROR(write(short_), "digital numbers for");
    DNGImage plane{image};
    plane.cfa = {0, 1, 1, 3};
    CHECK_ERROR(write(plane), "not red, green, or blue");
    DNGImage levels{image};
    levels.whiteLevel = 128;
    CHECK_ERROR(write(levels), "does not fall below the white level");
    DNGImage window{image};
    window.window = int4(4, 4, 12, 13);
    CHECK_ERROR(write(window), "falls outside the frame");
  }
}

TEST_CASE("DNG: the file a sensor with no tile makes") {
  TempDir tmpDir{"toy-dng-linear"};
  const std::string fileName{(tmpDir / "readout.dng").string()};
  const std::vector<uint16_t> planes{planeNumbers()};
  const DNGImage image{makeImage(planes, false)};
  writeDNGFile(fileName, image);
  const std::vector<uint8_t> bytes{readFile(fileName)};
  const auto entry{[&](uint16_t tag) {
    const std::optional<Entry> found{findEntry(bytes, 8, tag)};
    REQUIRE_MESSAGE(found.has_value(), "no entry for tag ", tag);
    return *found;
  }};
  SUBCASE("Holds every plane at every pixel rather than a mosaic") {
    CHECK(valueOf(bytes, entry(TAG_PHOTOMETRIC_INTERPRETATION)) == 34892);
    CHECK(valueOf(bytes, entry(TAG_SAMPLES_PER_PIXEL)) == 3);
    // The fields the format states per sample, which the tile made one
    // of.
    CHECK(entry(TAG_BITS_PER_SAMPLE).count == 3);
    CHECK(entry(TAG_WHITE_LEVEL).count == 3);
    CHECK(entry(TAG_BLACK_LEVEL).count == 3);
    CHECK(!findEntry(bytes, 8, TAG_CFA_PATTERN).has_value());
    CHECK(!findEntry(bytes, 8, TAG_CFA_REPEAT_PATTERN_DIM).has_value());
  }
  SUBCASE("Interleaves the planes by pixel") {
    const size_t strip{valueOf(bytes, entry(TAG_STRIP_OFFSETS))};
    REQUIRE(valueOf(bytes, entry(TAG_STRIP_BYTE_COUNTS)) == 2 * planes.size());
    for (size_t i = 0; i < planes.size(); i++) {
      CAPTURE(i);
      CHECK(at16(bytes, strip + 2 * i) == planes[i]);
    }
  }
  SUBCASE("Is the file it was pinned at") {
    CHECK(std::string(smdl::MD5Hash::hashFile(fileName)) == LINEAR_HASH);
  }
}
