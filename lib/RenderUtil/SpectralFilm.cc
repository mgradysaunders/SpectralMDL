#include <algorithm>
#include <cctype>
#include <cstdlib>
#include <cstring>

#include "smdl/RenderUtil/SpectralFilm.h"

#include "smdl/Support/Error.h"
#include "smdl/Support/Filesystem.h"
#include "smdl/Support/Strings.h"

#include "llvm/Support/Endian.h"

namespace smdl {

void SpectralFilm::clear() noexcept {
  mNumBands = 0;
  mNumPixelsX = 0;
  mNumPixelsY = 0;
  mNumSamples = 0;
  mTotals.reset();
}

void SpectralFilm::resize(size_t nBands, size_t nPixelsX, size_t nPixelsY) {
  clear();
  mNumBands = nBands;
  mNumPixelsX = nPixelsX;
  mNumPixelsY = nPixelsY;
  // `make_unique` value-initializes, so every accumulator starts at zero.
  // A plain `new T[n]` is uninitialized, which happens to be zero for a
  // fresh mmap-backed allocation but is dirty when the allocator reuses
  // heap memory, e.g. the second `resize()` of an iterative render loop.
  mTotals = std::make_unique<double[]>(mNumPixelsX * mNumPixelsY * mNumBands);
}

void SpectralFilm::add(const SpectralFilm &other) noexcept {
  SMDL_SANITY_CHECK(mNumBands == other.mNumBands);
  SMDL_SANITY_CHECK(mNumPixelsX == other.mNumPixelsX);
  SMDL_SANITY_CHECK(mNumPixelsY == other.mNumPixelsY);
  mNumSamples += other.mNumSamples;
  const size_t numTotals{mNumPixelsX * mNumPixelsY * mNumBands};
  for (size_t i{}; i < numTotals; i++) mTotals[i] += other.mTotals[i];
}

namespace {

// The header keys, spelled once each. The reader looks a key up by the
// same constant the writer stamped it with, so a rename cannot leave
// the two halves of the format describing different files, which is
// exactly what a header key silently going unread looks like.
constexpr const char *ENVI_FILE_TYPE{"file type"};
constexpr const char *ENVI_DATA_TYPE{"data type"};
constexpr const char *ENVI_BYTE_ORDER{"byte order"};
constexpr const char *ENVI_SAMPLES{"samples"};
constexpr const char *ENVI_LINES{"lines"};
constexpr const char *ENVI_BANDS{"bands"};
constexpr const char *ENVI_HEADER_OFFSET{"header offset"};
constexpr const char *ENVI_INTERLEAVE{"interleave"};
constexpr const char *ENVI_WAVELENGTH{"wavelength"};
constexpr const char *ENVI_WAVELENGTH_UNITS{"wavelength units"};
constexpr const char *ENVI_BAND_NAMES{"band names"};
constexpr const char *ENVI_SPP{"render spp"};
constexpr const char *ENVI_CROP_WINDOW{"render crop window"};

// The data types, as the format numbers them.
constexpr uint64_t ENVI_FLOAT32{4};
constexpr uint64_t ENVI_FLOAT64{5};
constexpr uint64_t ENVI_UINT16{12};

// Write one `key = value` line.
template <typename... Ts>
void writeField(std::ostream &stream, const char *name, Ts &&...values) {
  stream << name << " = ";
  (stream << ... << values) << '\n';
}

// Write one `key = {a, b, c}` line, the format's array form.
template <typename T>
void writeArrayField(std::ostream &stream, const char *name,
                     Span<const T> values) {
  stream << name << " = {";
  for (size_t i = 0; i < values.size(); i++)
    stream << values[i] << (i + 1 < values.size() ? ", " : "}");
  stream << '\n';
}

// Parse the `{a, b, c}` array form. The braces and commas carry no
// information the whitespace does not, so they become whitespace and
// the rest is a run of numbers.
[[nodiscard]] std::vector<double> parseArrayValue(std::string value) {
  auto values{std::vector<double>()};
  for (auto &c : value)
    if (c == '{' || c == '}' || c == ',') c = ' ';
  const char *ptr{value.c_str()};
  char *end{};
  for (double v{std::strtod(ptr, &end)}; end != ptr;
       v = std::strtod(ptr, &end)) {
    values.push_back(v);
    ptr = end;
  }
  return values;
}

// Parse the `{a, b, c}` array form of a list of names: the braces go,
// the commas split, and each name is trimmed.
[[nodiscard]] std::vector<std::string> parseNameList(std::string value) {
  auto names{std::vector<std::string>()};
  for (auto &c : value)
    if (c == '{' || c == '}') c = ' ';
  for (size_t pos{}; pos <= value.size();) {
    auto end{value.find(',', pos)};
    if (end == std::string::npos) end = value.size();
    auto name{value.substr(pos, end - pos)};
    const char *WS{" \t\r\n"};
    name.erase(0, name.find_first_not_of(WS));
    name.erase(name.find_last_not_of(WS) + 1);
    if (!name.empty()) names.push_back(std::move(name));
    pos = end + 1;
  }
  return names;
}

// Is `window` a non-empty sub-rectangle of the `nX` by `nY` frame? Both
// directions ask: the writer so a bad window never reaches a file, the
// reader so a bad file never reaches a film.
[[nodiscard]] bool isSubWindow(int4 window, size_t nX, size_t nY) noexcept {
  return 0 <= window[0] && 0 <= window[1] && //
         window[0] < window[2] && window[1] < window[3] &&
         window[2] <= int(nX) && window[3] <= int(nY);
}

// The byte order the format records for the host, and reads back to
// decide whether the file needs swapping.
[[nodiscard]] uint64_t hostByteOrder() noexcept {
  return llvm::endianness::native == llvm::endianness::little ? 0 : 1;
}

// What both writers refuse before touching a file.
void checkWindowAndNames(const std::string &fileName,
                         std::optional<int4> window, size_t nX, size_t nY,
                         Span<const std::string> bandNames, size_t nBands) {
  if (window && !isSubWindow(*window, nX, nY))
    throw Error(concat("cannot write ", QuotedPath(fileName), ": the window ",
                       (*window)[0], ",", (*window)[1], ",", (*window)[2], ",",
                       (*window)[3], " is not a non-empty sub-rectangle of ",
                       nX, "x", nY));
  if (!bandNames.empty() && bandNames.size() != nBands)
    throw Error(concat("cannot write ", QuotedPath(fileName), ": ",
                       bandNames.size(), " band names for ", nBands, " bands"));
}

} // namespace

void SpectralFilm::writeENVIFile(Span<const float> wavelengths,
                                 const std::string &fileName,
                                 Span<const std::string> extraHeaderLines,
                                 std::optional<int4> cropWindow,
                                 Span<const std::string> bandNames,
                                 bool shouldWriteDouble) const {
  const auto noCrop{int4{0, 0, int(mNumPixelsX), int(mNumPixelsY)}};
  checkWindowAndNames(fileName, cropWindow, mNumPixelsX, mNumPixelsY, bandNames,
                      mNumBands);
  const auto pixelWindow{cropWindow.value_or(noCrop)};
  // Write the header file
  {
    auto file{openOrThrow(fileName + ".hdr", std::ios::out)};
    file << "ENVI\n";
    writeField(file, ENVI_FILE_TYPE, "ENVI Standard");
    writeField(file, ENVI_DATA_TYPE,
               shouldWriteDouble ? ENVI_FLOAT64 : ENVI_FLOAT32);
    writeField(file, ENVI_BYTE_ORDER, hostByteOrder());
    writeField(file, ENVI_SAMPLES, mNumPixelsX);
    writeField(file, ENVI_LINES, mNumPixelsY);
    writeField(file, ENVI_BANDS, mNumBands);
    if (!wavelengths.empty()) {
      writeField(file, ENVI_WAVELENGTH_UNITS, "Nanometers");
      writeArrayField(file, ENVI_WAVELENGTH, wavelengths);
    }
    if (!bandNames.empty()) writeArrayField(file, ENVI_BAND_NAMES, bandNames);
    writeField(file, ENVI_HEADER_OFFSET, 0);
    writeField(file, ENVI_INTERLEAVE, "bip");
    // A zero count is not recorded, and such a file cannot seed a
    // resumed accumulation.
    if (mNumSamples > 0) {
      writeField(file, ENVI_SPP, mNumSamples);
      // Only when it narrows the image: a whole-image window is what the
      // reader assumes anyway, so every unwindowed header stays as it was.
      if (!isAllTrue(pixelWindow == noCrop))
        writeArrayField(file, ENVI_CROP_WINDOW,
                        Span<const int>(&pixelWindow[0], 4));
    }
    for (const auto &line : extraHeaderLines) file << line << '\n';
  }
  // Write the binary file a row at a time. The pixel values are means,
  // not the raw accumulated totals, so the file holds physically
  // meaningful radiance at any sample count.
  {
    const size_t valueSize{shouldWriteDouble ? sizeof(double) : sizeof(float)};
    auto row{std::vector<char>(mNumPixelsX * mNumBands * valueSize)};
    auto file{openOrThrow(fileName, std::ios::out | std::ios::binary)};
    for (size_t iY = 0; iY < mNumPixelsY; iY++) {
      char *ptr{row.data()};
      for (size_t iX = 0; iX < mNumPixelsX; iX++) {
        for (size_t i = 0; i < mNumBands; i++, ptr += valueSize) {
          const double value{mean(iX, iY, i)};
          if (shouldWriteDouble) {
            std::memcpy(ptr, &value, 8);
          } else {
            const float valueFloat{float(value)};
            std::memcpy(ptr, &valueFloat, 4);
          }
        }
      }
      file.write(row.data(), std::streamsize(row.size()));
    }
  }
}

void writeENVIFileUInt16(Span<const uint16_t> data, size_t numBands,
                         size_t numPixelsX, size_t numPixelsY,
                         const std::string &fileName,
                         Span<const std::string> bandNames,
                         Span<const std::string> extraHeaderLines,
                         std::optional<int4> window, uint64_t numSamples) {
  const auto noCrop{int4{0, 0, int(numPixelsX), int(numPixelsY)}};
  if (data.size() != numBands * numPixelsX * numPixelsY)
    throw Error(concat("cannot write ", QuotedPath(fileName), ": ", data.size(),
                       " values for ", numBands, " bands over ", numPixelsX,
                       "x", numPixelsY, " pixels"));
  checkWindowAndNames(fileName, window, numPixelsX, numPixelsY, bandNames,
                      numBands);
  const auto pixelWindow{window.value_or(noCrop)};
  {
    auto file{openOrThrow(fileName + ".hdr", std::ios::out)};
    file << "ENVI\n";
    writeField(file, ENVI_FILE_TYPE, "ENVI Standard");
    writeField(file, ENVI_DATA_TYPE, ENVI_UINT16);
    writeField(file, ENVI_BYTE_ORDER, hostByteOrder());
    writeField(file, ENVI_SAMPLES, numPixelsX);
    writeField(file, ENVI_LINES, numPixelsY);
    writeField(file, ENVI_BANDS, numBands);
    if (!bandNames.empty()) writeArrayField(file, ENVI_BAND_NAMES, bandNames);
    writeField(file, ENVI_HEADER_OFFSET, 0);
    writeField(file, ENVI_INTERLEAVE, "bip");
    if (numSamples > 0) {
      writeField(file, ENVI_SPP, numSamples);
      if (!isAllTrue(pixelWindow == noCrop))
        writeArrayField(file, ENVI_CROP_WINDOW,
                        Span<const int>(&pixelWindow[0], 4));
    }
    for (const auto &line : extraHeaderLines) file << line << '\n';
  }
  {
    auto file{openOrThrow(fileName, std::ios::out | std::ios::binary)};
    file.write(reinterpret_cast<const char *>(data.data()),
               std::streamsize(2 * data.size()));
  }
}

namespace {

// Parse the `key = value` lines of an ENVI header into a map keyed by
// the lower-cased key. A value that opens a `{...}` array continues
// across lines until the closing brace, per the format.
[[nodiscard]]
std::map<std::string, std::string> parseENVIHeader(const std::string &fileName,
                                                   const std::string &text) {

  std::map<std::string, std::string> fields{};
  size_t pos{};
  const auto trim{[](std::string str) {
    const char *WS{" \t\r\n"};
    str.erase(0, str.find_first_not_of(WS));
    str.erase(str.find_last_not_of(WS) + 1);
    return str;
  }};
  const auto nextLine{[&]() -> std::string {
    auto end{text.find('\n', pos)};
    if (end == std::string::npos) end = text.size();
    auto line{text.substr(pos, end - pos)};
    pos = std::min(end + 1, text.size());
    return line;
  }};
  if (trim(nextLine()) != "ENVI")
    throw Error(concat("cannot load ", QuotedPath(fileName + ".hdr"),
                       ": missing 'ENVI' magic line"));
  while (pos < text.size()) {
    auto line{nextLine()};
    auto eq{line.find('=')};
    if (eq == std::string::npos) continue;
    auto key{trim(line.substr(0, eq))};
    auto value{trim(line.substr(eq + 1))};
    if (key.empty()) continue;
    std::transform(key.begin(), key.end(), key.begin(),
                   [](unsigned char c) { return std::tolower(c); });
    // An array value continues to the closing brace.
    while (!value.empty() && value.front() == '{' &&
           value.find('}') == std::string::npos && pos < text.size())
      value += " " + trim(nextLine());
    fields[key] = value;
  }
  return fields;
}

// Extract a required unsigned integer header field.
[[nodiscard]]
uint64_t requiredCount(const std::string &fileName,
                       std::map<std::string, std::string> &fields,
                       const char *key) {
  auto itr{fields.find(key)};
  if (itr == fields.end())
    throw Error(concat("cannot load ", QuotedPath(fileName + ".hdr"),
                       ": missing ", Quoted(key), " field"));
  auto value{std::strtoull(itr->second.c_str(), nullptr, 10)};
  fields.erase(itr);
  return value;
}

} // namespace

SpectralFilm::ENVIFileInfo
SpectralFilm::readENVIFile(const std::string &fileName) try {
  auto result{ENVIFileInfo{}};
  auto fields{parseENVIHeader(fileName, readOrThrow(fileName + ".hdr"))};
  const auto nX{requiredCount(fileName, fields, ENVI_SAMPLES)};
  const auto nY{requiredCount(fileName, fields, ENVI_LINES)};
  const auto nBands{requiredCount(fileName, fields, ENVI_BANDS)};
  // Accept exactly the formats the writer emits: 32-bit or 64-bit
  // floats, band-interleaved-by-pixel. The byte order is the one thing
  // worth fixing up rather than rejecting.
  const auto type{requiredCount(fileName, fields, ENVI_DATA_TYPE)};
  if (type != ENVI_FLOAT32 && type != ENVI_FLOAT64)
    throw Error(concat("cannot load ", QuotedPath(fileName), ": data type ",
                       type, " (expected 4 or 5, a 32-bit or 64-bit float)"));
  const size_t valueSize{type == ENVI_FLOAT64 ? sizeof(double) : sizeof(float)};
  if (auto itr{fields.find(ENVI_INTERLEAVE)};
      itr != fields.end() && itr->second == "bip") {
    fields.erase(itr);
  } else {
    throw Error(concat("cannot load ", QuotedPath(fileName),
                       ": expected 'interleave = bip'"));
  }
  const auto byteOrder{requiredCount(fileName, fields, ENVI_BYTE_ORDER)};
  const auto headerOffset{
      fields.count(ENVI_HEADER_OFFSET)
          ? requiredCount(fileName, fields, ENVI_HEADER_OFFSET)
          : 0};
  if (auto itr{fields.find(ENVI_SPP)}; itr != fields.end()) {
    result.samplesPerPixel = std::strtoull(itr->second.c_str(), nullptr, 10);
    fields.erase(itr);
  }
  result.cropWindow = int4{0, 0, int(nX), int(nY)};
  if (auto itr{fields.find(ENVI_CROP_WINDOW)}; itr != fields.end()) {
    const auto bounds{parseArrayValue(itr->second)};
    if (bounds.size() != 4)
      throw Error(concat("cannot load ", QuotedPath(fileName + ".hdr"), ": ",
                         bounds.size(), " values in ", Quoted(ENVI_CROP_WINDOW),
                         " (expected 4)"));
    for (size_t i = 0; i < 4; i++) result.cropWindow[i] = int(bounds[i]);
    if (const auto &cropWindow{result.cropWindow};
        !isSubWindow(cropWindow, nX, nY))
      throw Error(concat("cannot load ", QuotedPath(fileName + ".hdr"), ": ",
                         Quoted(ENVI_CROP_WINDOW), " ",          //
                         cropWindow[0], ",", cropWindow[1], ",", //
                         cropWindow[2], ",", cropWindow[3],      //
                         " is not a non-empty sub-rectangle of ", nX, "x", nY));
    fields.erase(itr);
  }
  if (auto itr{fields.find(ENVI_WAVELENGTH)}; itr != fields.end()) {
    for (const double w : parseArrayValue(itr->second))
      result.wavelengths.push_back(float(w));
    fields.erase(itr);
    if (result.wavelengths.size() != nBands)
      throw Error(concat("cannot load ", QuotedPath(fileName + ".hdr"), ": ",
                         result.wavelengths.size(), " wavelengths for ", nBands,
                         " bands"));
  }
  if (auto itr{fields.find(ENVI_BAND_NAMES)}; itr != fields.end()) {
    result.bandNames = parseNameList(itr->second);
    fields.erase(itr);
    if (result.bandNames.size() != nBands)
      throw Error(concat("cannot load ", QuotedPath(fileName + ".hdr"), ": ",
                         result.bandNames.size(), " band names for ", nBands,
                         " bands"));
  }
  fields.erase(ENVI_FILE_TYPE);
  fields.erase(ENVI_WAVELENGTH_UNITS);
  result.fields = std::move(fields);
  // Read the binary file a row at a time, reconstructing the accumulator
  // invariant: totals are means times the sample count, or the means
  // themselves at a count of 1 when the header does not record the count.
  const auto count{std::max(result.samplesPerPixel, uint64_t(1))};
  auto file{openOrThrow(fileName, std::ios::in | std::ios::binary)};
  file.ignore(std::streamsize(headerOffset));
  resize(nBands, nX, nY);
  addSamples(count);
  const bool shouldSwapBytes{byteOrder != hostByteOrder()};
  auto row{std::vector<char>(nX * nBands * valueSize)};
  auto values{std::vector<double>(nBands)};
  for (size_t iY = 0; iY < nY; iY++) {
    if (!file.read(row.data(), std::streamsize(row.size())))
      throw Error(concat("cannot load ", QuotedPath(fileName),
                         ": unexpected end of file"));
    const char *ptr{row.data()};
    for (size_t iX = 0; iX < nX; iX++) {
      for (size_t i = 0; i < nBands; i++, ptr += valueSize) {
        // NOLINTNEXTLINE
        char bytes[8]{};
        std::memcpy(bytes, ptr, valueSize);
        if (shouldSwapBytes) std::reverse(bytes, bytes + valueSize);
        double mean{};
        if (valueSize == 8) {
          std::memcpy(&mean, bytes, 8);
        } else {
          float meanFloat{};
          std::memcpy(&meanFloat, bytes, 4);
          mean = meanFloat;
        }
        values[i] = mean * double(count);
      }
      // The count belongs to the window, so whatever the file holds
      // outside it is read past and dropped, leaving those pixels the
      // zeros `resize()` wrote. That is what keeps the one count honest
      // when this film is merged and rendered into again.
      if (int(iX) >= result.cropWindow[0] && int(iX) < result.cropWindow[2] &&
          int(iY) >= result.cropWindow[1] && int(iY) < result.cropWindow[3])
        addTotals(iX, iY, values.data());
    }
  }
  return result;
} catch (...) {
  // A partly read film is worse than none: a caller that catches the
  // error sees an empty film rather than totals for the rows that made
  // it in before the file ran out.
  clear();
  throw;
}

} // namespace smdl
