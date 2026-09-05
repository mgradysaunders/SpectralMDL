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
constexpr const char *ENVI_SPP{"render spp"};
constexpr const char *ENVI_CROP_WINDOW{"render crop window"};

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

} // namespace

void SpectralFilm::writeENVIFile(Span<const float> wavelengths,
                                 const std::string &fileName,
                                 Span<const std::string> extraHeaderLines,
                                 std::optional<int4> cropWindow) const {
  const auto noCrop{int4{0, 0, int(mNumPixelsX), int(mNumPixelsY)}};
  if (cropWindow && !isSubWindow(*cropWindow, mNumPixelsX, mNumPixelsY))
    throw Error(concat("cannot write ", Quoted(fileName), ": the window ",
                       (*cropWindow)[0], ",", (*cropWindow)[1], ",",
                       (*cropWindow)[2], ",", (*cropWindow)[3],
                       " is not a non-empty sub-rectangle of ", mNumPixelsX,
                       "x", mNumPixelsY));
  const auto pixelWindow{cropWindow.value_or(noCrop)};
  // Write the header file
  {
    auto file{openOrThrow(fileName + ".hdr", std::ios::out)};
    file << "ENVI\n";
    writeField(file, ENVI_FILE_TYPE, "ENVI Standard");
    writeField(file, ENVI_DATA_TYPE, 5);
    writeField(file, ENVI_BYTE_ORDER, hostByteOrder());
    writeField(file, ENVI_SAMPLES, mNumPixelsX);
    writeField(file, ENVI_LINES, mNumPixelsY);
    writeField(file, ENVI_BANDS, mNumBands);
    writeField(file, ENVI_WAVELENGTH_UNITS, "Nanometers");
    writeArrayField(file, ENVI_WAVELENGTH, wavelengths);
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
  // Write the binary file. The pixel values are means, not the raw
  // accumulated totals, so the file holds physically meaningful
  // radiance at any sample count.
  {
    auto file{openOrThrow(fileName, std::ios::out | std::ios::binary)};
    for (size_t iY = 0; iY < mNumPixelsY; iY++) {
      for (size_t iX = 0; iX < mNumPixelsX; iX++) {
        for (size_t i = 0; i < mNumBands; i++) {
          double pixelValue{mean(iX, iY, i)};
          file.write(reinterpret_cast<const char *>(&pixelValue), 8);
        }
      }
    }
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
    throw Error(concat("cannot load ", Quoted(fileName + ".hdr"),
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
    throw Error(concat("cannot load ", Quoted(fileName + ".hdr"), ": missing ",
                       Quoted(key), " field"));
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
  // Accept exactly the format the writer emits: 64-bit floats,
  // band-interleaved-by-pixel. The byte order is the one thing worth
  // fixing up rather than rejecting.
  if (auto type{requiredCount(fileName, fields, ENVI_DATA_TYPE)}; type != 5) {
    throw Error(concat("cannot load ", Quoted(fileName), ": data type ", type,
                       " (expected 5, 64-bit float)"));
  }
  if (auto itr{fields.find(ENVI_INTERLEAVE)};
      itr != fields.end() && itr->second == "bip") {
    fields.erase(itr);
  } else {
    throw Error(concat("cannot load ", Quoted(fileName),
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
      throw Error(concat("cannot load ", Quoted(fileName + ".hdr"), ": ",
                         bounds.size(), " values in ", Quoted(ENVI_CROP_WINDOW),
                         " (expected 4)"));
    for (size_t i = 0; i < 4; i++) result.cropWindow[i] = int(bounds[i]);
    if (const auto &cropWindow{result.cropWindow};
        !isSubWindow(cropWindow, nX, nY))
      throw Error(concat("cannot load ", Quoted(fileName + ".hdr"), ": ",
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
      throw Error(concat("cannot load ", Quoted(fileName + ".hdr"), ": ",
                         result.wavelengths.size(), " wavelengths for ", nBands,
                         " bands"));
  }
  fields.erase(ENVI_FILE_TYPE);
  fields.erase(ENVI_WAVELENGTH_UNITS);
  result.fields = std::move(fields);
  // Read the binary file, reconstructing the accumulator invariant:
  // totals are means times the sample count, or the means themselves
  // at a count of 1 when the header does not record the count.
  const auto count{std::max(result.samplesPerPixel, uint64_t(1))};
  auto file{openOrThrow(fileName, std::ios::in | std::ios::binary)};
  file.ignore(std::streamsize(headerOffset));
  resize(nBands, nX, nY);
  addSamples(count);
  const bool swapBytes{byteOrder != hostByteOrder()};
  auto values{std::vector<double>(nBands)};
  for (size_t iY = 0; iY < nY; iY++) {
    for (size_t iX = 0; iX < nX; iX++) {
      for (size_t i = 0; i < nBands; i++) {
        // NOLINTNEXTLINE
        char bytes[8]{};
        if (!file.read(bytes, 8))
          throw Error(concat("cannot load ", Quoted(fileName),
                             ": unexpected end of file"));
        if (swapBytes) std::reverse(bytes, bytes + 8);
        double mean{};
        std::memcpy(&mean, bytes, 8);
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
