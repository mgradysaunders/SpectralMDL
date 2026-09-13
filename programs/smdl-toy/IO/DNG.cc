#include "IO/DNG.h"

#include <algorithm>
#include <cmath>
#include <cstring>
#include <fstream>

#include "smdl/Support/Error.h"
#include "smdl/Support/Strings.h"

// The writer for the format documented in `DNG.h`. Every field is
// encoded little-endian here rather than written as the host's own
// bytes, since one readout must be one file wherever it is written.
//
// The layout is the simplest a reader accepts: the raw image is the
// first directory, the values too large to sit in a directory entry
// follow it, then the Exif directory and its values, and the single
// uncompressed strip is last. The spec recommends but does not require
// a thumbnail in the first directory, and there is none here: a
// developer renders its own preview, and a renderer that wrote one
// would be writing a second, worse develop into the file.

namespace {

//--{ TIFF mechanics

// The field types the tags here use, as the tag tables spell them.
enum class FieldType : uint16_t {
  BYTE = 1,
  ASCII = 2,
  SHORT = 3,
  LONG = 4,
  RATIONAL = 5,
  UNDEFINED = 7,
  SRATIONAL = 10,
  DOUBLE = 12
};

// The tags, by the names the TIFF, TIFF-EP, Exif, and DNG
// specifications give them.
enum Tag : uint16_t {
  TAG_NEW_SUBFILE_TYPE = 254,
  TAG_IMAGE_WIDTH = 256,
  TAG_IMAGE_LENGTH = 257,
  TAG_BITS_PER_SAMPLE = 258,
  TAG_COMPRESSION = 259,
  TAG_PHOTOMETRIC_INTERPRETATION = 262,
  TAG_IMAGE_DESCRIPTION = 270,
  TAG_MAKE = 271,
  TAG_MODEL = 272,
  TAG_STRIP_OFFSETS = 273,
  TAG_ORIENTATION = 274,
  TAG_SAMPLES_PER_PIXEL = 277,
  TAG_ROWS_PER_STRIP = 278,
  TAG_STRIP_BYTE_COUNTS = 279,
  TAG_PLANAR_CONFIGURATION = 284,
  TAG_SOFTWARE = 305,
  TAG_SAMPLE_FORMAT = 339,
  TAG_CFA_REPEAT_PATTERN_DIM = 33421,
  TAG_CFA_PATTERN = 33422,
  TAG_EXPOSURE_TIME = 33434,
  TAG_F_NUMBER = 33437,
  TAG_EXIF_IFD = 34665,
  TAG_PHOTOGRAPHIC_SENSITIVITY = 34855,
  TAG_SENSITIVITY_TYPE = 34864,
  TAG_ISO_SPEED = 34867,
  TAG_EXIF_VERSION = 36864,
  TAG_DNG_VERSION = 50706,
  TAG_DNG_BACKWARD_VERSION = 50707,
  TAG_UNIQUE_CAMERA_MODEL = 50708,
  TAG_CFA_PLANE_COLOR = 50710,
  TAG_CFA_LAYOUT = 50711,
  TAG_BLACK_LEVEL = 50714,
  TAG_WHITE_LEVEL = 50717,
  TAG_DEFAULT_CROP_ORIGIN = 50719,
  TAG_DEFAULT_CROP_SIZE = 50720,
  TAG_COLOR_MATRIX_1 = 50721,
  TAG_COLOR_MATRIX_2 = 50722,
  TAG_AS_SHOT_NEUTRAL = 50728,
  TAG_BASELINE_EXPOSURE = 50730,
  TAG_CALIBRATION_ILLUMINANT_1 = 50778,
  TAG_CALIBRATION_ILLUMINANT_2 = 50779,
  TAG_FORWARD_MATRIX_1 = 50964,
  TAG_FORWARD_MATRIX_2 = 50965,
  TAG_NOISE_PROFILE = 51041
};

// The photometric interpretations of a raw directory: one band per
// pixel under a tile, and every band at every pixel without one.
constexpr uint16_t PHOTOMETRIC_CFA = 32803;
constexpr uint16_t PHOTOMETRIC_LINEAR_RAW = 34892;

// What a rational field divides by. The matrices take the denominator
// every camera profile is written with, which quantizes a matrix entry
// far below what the fit behind it is worth; the rest take one fine
// enough that the file reads back what was asked for.
//
// \{
constexpr double MATRIX_DENOMINATOR = 10000.0;
constexpr double FINE_DENOMINATOR = 1000000.0;
constexpr double LEVEL_DENOMINATOR = 10000.0;
// \}

void put8(std::vector<uint8_t> &bytes, uint8_t value) {
  bytes.push_back(value);
}

void put16(std::vector<uint8_t> &bytes, uint16_t value) {
  bytes.push_back(uint8_t(value));
  bytes.push_back(uint8_t(value >> 8));
}

void put32(std::vector<uint8_t> &bytes, uint32_t value) {
  for (int shift = 0; shift < 32; shift += 8)
    bytes.push_back(uint8_t(value >> shift));
}

void put64(std::vector<uint8_t> &bytes, uint64_t value) {
  for (int shift = 0; shift < 64; shift += 8)
    bytes.push_back(uint8_t(value >> shift));
}

void putDouble(std::vector<uint8_t> &bytes, double value) {
  uint64_t bits{};
  std::memcpy(&bits, &value, sizeof(bits));
  put64(bytes, bits);
}

// One directory entry: the tag, the type, and the payload in full. A
// payload of four bytes or fewer sits in the entry itself and anything
// larger is placed after the directory and pointed at.
struct Field final {
  uint16_t tag{};

  FieldType type{FieldType::BYTE};

  uint32_t count{};

  std::vector<uint8_t> bytes{};

  [[nodiscard]] bool isInline() const noexcept { return bytes.size() <= 4; }
};

[[nodiscard]] Field byteField(uint16_t tag, std::vector<uint8_t> values) {
  return Field{tag, FieldType::BYTE, uint32_t(values.size()),
               std::move(values)};
}

[[nodiscard]] Field undefinedField(uint16_t tag, std::string_view text) {
  return Field{tag, FieldType::UNDEFINED, uint32_t(text.size()),
               std::vector<uint8_t>(text.begin(), text.end())};
}

// The text and the null that terminates it, which the count includes.
[[nodiscard]] Field asciiField(uint16_t tag, const std::string &text) {
  std::vector<uint8_t> bytes(text.begin(), text.end());
  bytes.push_back(0);
  return Field{tag, FieldType::ASCII, uint32_t(bytes.size()), std::move(bytes)};
}

[[nodiscard]] Field shortField(uint16_t tag,
                               std::initializer_list<uint16_t> values) {
  std::vector<uint8_t> bytes{};
  for (const auto value : values) put16(bytes, value);
  return Field{tag, FieldType::SHORT, uint32_t(values.size()),
               std::move(bytes)};
}

// One value repeated, for the per-sample fields of a multi-plane image.
[[nodiscard]] Field shortField(uint16_t tag, uint16_t value, size_t count) {
  std::vector<uint8_t> bytes{};
  for (size_t i = 0; i < count; i++) put16(bytes, value);
  return Field{tag, FieldType::SHORT, uint32_t(count), std::move(bytes)};
}

[[nodiscard]] Field longField(uint16_t tag,
                              std::initializer_list<uint32_t> values) {
  std::vector<uint8_t> bytes{};
  for (const auto value : values) put32(bytes, value);
  return Field{tag, FieldType::LONG, uint32_t(values.size()), std::move(bytes)};
}

[[nodiscard]] Field longField(uint16_t tag, uint32_t value, size_t count) {
  std::vector<uint8_t> bytes{};
  for (size_t i = 0; i < count; i++) put32(bytes, value);
  return Field{tag, FieldType::LONG, uint32_t(count), std::move(bytes)};
}

// The numerator `value` takes over `denominator`, held inside the range
// the field's own integer carries.
[[nodiscard]] int64_t numeratorOf(double value, double denominator,
                                  bool isSigned) noexcept {
  const double scaled{std::round(value * denominator)};
  const double lower{isSigned ? -2147483648.0 : 0.0};
  return int64_t(std::clamp(std::isfinite(scaled) ? scaled : 0.0, lower,
                            isSigned ? 2147483647.0 : 4294967295.0));
}

[[nodiscard]] Field rationalField(uint16_t tag, smdl::Span<const double> values,
                                  double denominator) {
  std::vector<uint8_t> bytes{};
  for (const auto value : values) {
    put32(bytes, uint32_t(numeratorOf(value, denominator, false)));
    put32(bytes, uint32_t(denominator));
  }
  return Field{tag, FieldType::RATIONAL, uint32_t(values.size()),
               std::move(bytes)};
}

[[nodiscard]] Field srationalField(uint16_t tag,
                                   smdl::Span<const double> values,
                                   double denominator) {
  std::vector<uint8_t> bytes{};
  for (const auto value : values) {
    put32(bytes, uint32_t(int32_t(numeratorOf(value, denominator, true))));
    put32(bytes, uint32_t(int32_t(denominator)));
  }
  return Field{tag, FieldType::SRATIONAL, uint32_t(values.size()),
               std::move(bytes)};
}

// A color matrix in the row scan order the spec states, against the
// column-major matrix the program holds it in.
[[nodiscard]] Field matrixField(uint16_t tag, const double3x3 &matrix) {
  std::array<double, 9> values{};
  for (size_t i = 0; i < 3; i++)
    for (size_t j = 0; j < 3; j++) values[3 * i + j] = matrix[j][i];
  return srationalField(tag, smdl::Span<const double>(values.data(), 9),
                        MATRIX_DENOMINATOR);
}

[[nodiscard]] Field doubleField(uint16_t tag, smdl::Span<const double> values) {
  std::vector<uint8_t> bytes{};
  for (const auto value : values) putDouble(bytes, value);
  return Field{tag, FieldType::DOUBLE, uint32_t(values.size()),
               std::move(bytes)};
}

// The bytes a payload takes where it is placed, which TIFF requires to
// begin at an even offset.
[[nodiscard]] uint32_t paddedSize(const Field &field) noexcept {
  return uint32_t((field.bytes.size() + 1) & ~size_t(1));
}

// The bytes a directory takes: the count, the entries, the offset of
// the directory that follows, and the payloads that did not fit.
[[nodiscard]] uint32_t directorySize(const std::vector<Field> &fields) {
  uint32_t size{uint32_t(2 + 12 * fields.size() + 4)};
  for (const auto &field : fields)
    if (!field.isInline()) size += paddedSize(field);
  return size;
}

// Fill in the offset one entry holds, which is a long that sits in the
// entry itself, so the directory keeps the size it was measured at.
void patchOffset(std::vector<Field> &fields, uint16_t tag, uint32_t offset) {
  for (auto &field : fields)
    if (field.tag == tag) field = longField(tag, {offset});
}

// Append a directory that begins at `offset`, then the payloads it
// points at. The entries go in ascending order of tag, which a reader
// is entitled to assume.
void putDirectory(std::vector<uint8_t> &bytes, std::vector<Field> fields,
                  uint32_t offset) {
  std::sort(
      fields.begin(), fields.end(),
      [](const Field &lhs, const Field &rhs) { return lhs.tag < rhs.tag; });
  uint32_t valueOffset{offset + uint32_t(2 + 12 * fields.size() + 4)};
  put16(bytes, uint16_t(fields.size()));
  for (const auto &field : fields) {
    put16(bytes, field.tag);
    put16(bytes, uint16_t(field.type));
    put32(bytes, field.count);
    if (!field.isInline()) {
      put32(bytes, valueOffset);
      valueOffset += paddedSize(field);
      continue;
    }
    for (size_t i = 0; i < 4; i++)
      put8(bytes, i < field.bytes.size() ? field.bytes[i] : uint8_t(0));
  }
  put32(bytes, 0);
  for (const auto &field : fields) {
    if (field.isInline()) continue;
    bytes.insert(bytes.end(), field.bytes.begin(), field.bytes.end());
    if (field.bytes.size() & 1) bytes.push_back(0);
  }
}

//--}

//--{ The directories

// The raw directory. The two entries that hold an offset take a
// placeholder, since where the Exif directory and the strip land
// depends on how many entries there are; `patchOffset()` fills them in
// once the sizes are known, and an entry's own size never changes.
[[nodiscard]] std::vector<Field> rawFields(const DNGImage &image,
                                           uint32_t imageSize) {
  const size_t sampleCount{image.sampleCount()};
  const double3 &neutral{image.asShotNeutral};
  const double baselineExposure{image.baselineExposure};
  // The levels are per sample, which is one under a tile and three
  // without, the repeat dimensions of the black level being 1 by 1.
  const std::array<double, 3> blackLevel{image.blackLevel, image.blackLevel,
                                         image.blackLevel};
  std::array<double, 6> noise{};
  for (size_t k = 0; k < 3; k++) {
    noise[2 * k] = image.noiseProfile[k].x;
    noise[2 * k + 1] = image.noiseProfile[k].y;
  }
  std::vector<Field> fields{
      longField(TAG_NEW_SUBFILE_TYPE, {0}),
      longField(TAG_IMAGE_WIDTH, {uint32_t(image.pixelCountX)}),
      longField(TAG_IMAGE_LENGTH, {uint32_t(image.pixelCountY)}),
      shortField(TAG_BITS_PER_SAMPLE, 16, sampleCount),
      shortField(TAG_COMPRESSION, {1}),
      shortField(TAG_PHOTOMETRIC_INTERPRETATION,
                 {image.hasCFA ? PHOTOMETRIC_CFA : PHOTOMETRIC_LINEAR_RAW}),
      longField(TAG_STRIP_OFFSETS, {0}),
      longField(TAG_EXIF_IFD, {0}),
      shortField(TAG_ORIENTATION, {1}),
      shortField(TAG_SAMPLES_PER_PIXEL, {uint16_t(sampleCount)}),
      longField(TAG_ROWS_PER_STRIP, {uint32_t(image.pixelCountY)}),
      longField(TAG_STRIP_BYTE_COUNTS, {imageSize}),
      shortField(TAG_PLANAR_CONFIGURATION, {1}),
      shortField(TAG_SAMPLE_FORMAT, 1, sampleCount),
      byteField(TAG_DNG_VERSION, {1, 4, 0, 0}),
      byteField(TAG_DNG_BACKWARD_VERSION, {1, 1, 0, 0}),
      rationalField(TAG_BLACK_LEVEL,
                    smdl::Span<const double>(blackLevel.data(), sampleCount),
                    LEVEL_DENOMINATOR),
      longField(TAG_WHITE_LEVEL, uint32_t(image.whiteLevel), sampleCount),
      matrixField(TAG_COLOR_MATRIX_1, image.xyzToCamera1),
      matrixField(TAG_COLOR_MATRIX_2, image.xyzToCamera2),
      matrixField(TAG_FORWARD_MATRIX_1, image.cameraToXYZ1),
      matrixField(TAG_FORWARD_MATRIX_2, image.cameraToXYZ2),
      shortField(TAG_CALIBRATION_ILLUMINANT_1, {image.illuminant1}),
      shortField(TAG_CALIBRATION_ILLUMINANT_2, {image.illuminant2}),
      rationalField(TAG_AS_SHOT_NEUTRAL,
                    smdl::Span<const double>(&neutral[0], 3), FINE_DENOMINATOR),
      srationalField(TAG_BASELINE_EXPOSURE,
                     smdl::Span<const double>(&baselineExposure, 1),
                     FINE_DENOMINATOR),
      doubleField(TAG_NOISE_PROFILE,
                  smdl::Span<const double>(noise.data(), noise.size()))};
  if (image.hasCFA) {
    fields.push_back(shortField(TAG_CFA_REPEAT_PATTERN_DIM, {2, 2}));
    fields.push_back(byteField(TAG_CFA_PATTERN, {image.cfa[0], image.cfa[1],
                                                 image.cfa[2], image.cfa[3]}));
    fields.push_back(byteField(TAG_CFA_PLANE_COLOR, {0, 1, 2}));
    fields.push_back(shortField(TAG_CFA_LAYOUT, {1}));
  }
  // The rendered part of the frame, which a developer opens to. The
  // whole frame is what a reader assumes anyway, so it writes neither
  // tag.
  const int4 frame{0, 0, int(image.pixelCountX), int(image.pixelCountY)};
  if (!isAllTrue(image.window == frame)) {
    fields.push_back(
        longField(TAG_DEFAULT_CROP_ORIGIN,
                  {uint32_t(image.window[0]), uint32_t(image.window[1])}));
    fields.push_back(longField(TAG_DEFAULT_CROP_SIZE,
                               {uint32_t(image.window[2] - image.window[0]),
                                uint32_t(image.window[3] - image.window[1])}));
  }
  if (!image.description.empty())
    fields.push_back(asciiField(TAG_IMAGE_DESCRIPTION, image.description));
  if (!image.make.empty()) fields.push_back(asciiField(TAG_MAKE, image.make));
  if (!image.model.empty())
    fields.push_back(asciiField(TAG_MODEL, image.model));
  if (!image.software.empty())
    fields.push_back(asciiField(TAG_SOFTWARE, image.software));
  if (!image.uniqueCameraModel.empty())
    fields.push_back(
        asciiField(TAG_UNIQUE_CAMERA_MODEL, image.uniqueCameraModel));
  return fields;
}

// The shot. The sensitivity field is a short, which the fast end of a
// modern instrument runs past, so the speed is stated again as the long
// that Exif added for exactly that, and the type says which kind of
// speed both are.
[[nodiscard]] std::vector<Field> exifFields(const DNGImage &image) {
  const double exposureTime{image.exposureTime};
  const double fNumber{image.fNumber};
  const double iso{std::max(image.iso, 0.0)};
  return {undefinedField(TAG_EXIF_VERSION, "0231"),
          rationalField(TAG_EXPOSURE_TIME,
                        smdl::Span<const double>(&exposureTime, 1),
                        FINE_DENOMINATOR),
          rationalField(TAG_F_NUMBER, smdl::Span<const double>(&fNumber, 1),
                        FINE_DENOMINATOR),
          shortField(TAG_PHOTOGRAPHIC_SENSITIVITY,
                     {uint16_t(std::min(std::round(iso), 65535.0))}),
          shortField(TAG_SENSITIVITY_TYPE, {3}),
          longField(TAG_ISO_SPEED,
                    {uint32_t(std::min(std::round(iso), 4294967295.0))})};
}

//--}

} // namespace

void writeDNGFile(const std::string &fileName, const DNGImage &image) {
  const auto fail{[&](auto &&...args) {
    throw smdl::Error(smdl::concat("Cannot write DNG ",
                                   smdl::QuotedPath(fileName), ": ", args...));
  }};
  const size_t sampleCount{image.sampleCount()};
  const size_t valueCount{image.pixelCountX * image.pixelCountY * sampleCount};
  if (image.pixelCountX == 0 || image.pixelCountY == 0)
    fail("the frame is empty");
  if (image.digitalNumbers.size() != valueCount)
    fail(image.digitalNumbers.size(), " digital numbers for ",
         smdl::Counted(sampleCount, "sample"), " over ", image.pixelCountX, "x",
         image.pixelCountY, " pixels");
  if (image.hasCFA && std::any_of(image.cfa.begin(), image.cfa.end(),
                                  [](uint8_t plane) { return plane > 2; }))
    fail("the tile names a plane that is not red, green, or blue");
  if (!(image.blackLevel < double(image.whiteLevel)))
    fail("the black level ", image.blackLevel,
         " does not fall below the white level ", image.whiteLevel);
  const int4 &window{image.window};
  if (!(0 <= window[0] && window[0] < window[2] &&
        window[2] <= int(image.pixelCountX) && 0 <= window[1] &&
        window[1] < window[3] && window[3] <= int(image.pixelCountY)))
    fail("the window [", window[0], ", ", window[1], ", ", window[2], ", ",
         window[3], "] falls outside the frame");
  // The image data is last, so its offset is everything before it: the
  // header, the raw directory, and the Exif directory the raw directory
  // points at.
  const uint32_t imageSize{uint32_t(2 * valueCount)};
  const uint32_t headerSize{8};
  const std::vector<Field> exif{exifFields(image)};
  std::vector<Field> raw{rawFields(image, imageSize)};
  const uint32_t exifOffset{headerSize + directorySize(raw)};
  const uint32_t imageOffset{exifOffset + directorySize(exif)};
  patchOffset(raw, TAG_EXIF_IFD, exifOffset);
  patchOffset(raw, TAG_STRIP_OFFSETS, imageOffset);
  std::vector<uint8_t> bytes{};
  bytes.reserve(imageOffset);
  put8(bytes, 'I');
  put8(bytes, 'I');
  put16(bytes, 42);
  put32(bytes, headerSize);
  putDirectory(bytes, std::move(raw), headerSize);
  putDirectory(bytes, exif, exifOffset);
  std::ofstream stream{fileName, std::ios::binary};
  if (!stream) fail("cannot open the file");
  stream.write(reinterpret_cast<const char *>(bytes.data()),
               std::streamsize(bytes.size()));
  // A row at a time, encoded rather than copied, so that the file is
  // the same on any host.
  std::vector<uint8_t> row{};
  const size_t rowCount{image.pixelCountX * sampleCount};
  for (size_t y = 0; y < image.pixelCountY; y++) {
    row.clear();
    for (size_t i = 0; i < rowCount; i++)
      put16(row, image.digitalNumbers[y * rowCount + i]);
    stream.write(reinterpret_cast<const char *>(row.data()),
                 std::streamsize(row.size()));
  }
  stream.flush();
  if (!stream) fail("the write failed");
}
