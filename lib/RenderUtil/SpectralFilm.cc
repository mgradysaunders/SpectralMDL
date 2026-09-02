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

// Vector `operator==` yields one result per component, per MDL
// semantics, so whole-rectangle equality needs spelling out.
[[nodiscard]] static bool sameWindow(int4 lhs, int4 rhs) noexcept {
  return lhs[0] == rhs[0] && lhs[1] == rhs[1] && //
         lhs[2] == rhs[2] && lhs[3] == rhs[3];
}

void SpectralFilm::writeENVIFile(Span<const float> wavelengths,
                                 const std::string &fileName,
                                 Span<const std::string> extraHeaderLines,
                                 std::optional<int4> window) const {
  const auto wholeImage{int4{0, 0, int(mNumPixelsX), int(mNumPixelsY)}};
  if (window && !(0 <= (*window)[0] && (*window)[0] < (*window)[2] &&
                  (*window)[2] <= wholeImage[2] && 0 <= (*window)[1] &&
                  (*window)[1] < (*window)[3] && (*window)[3] <= wholeImage[3]))
    throw Error(concat("cannot write ", Quoted(fileName), ": the window ",
                       (*window)[0], ",", (*window)[1], ",", (*window)[2], ",",
                       (*window)[3], " is not a non-empty sub-rectangle of ",
                       mNumPixelsX, "x", mNumPixelsY));
  const auto pixelWindow{window.value_or(wholeImage)};
  // Write the header file
  {
    auto file{openOrThrow(fileName + ".hdr", std::ios::out)};
    file << "ENVI\n";
    file << "file type = ENVI Standard\n";
    file << "data type = 5\n";
    file << "byte order = "
         << (llvm::endianness::native == llvm::endianness::little ? 0 : 1)
         << '\n';
    file << "samples = " << mNumPixelsX << '\n';
    file << "lines = " << mNumPixelsY << '\n';
    file << "bands = " << mNumBands << '\n';
    file << "wavelength units = Nanometers\n";
    file << "wavelength = {";
    for (size_t i = 0; i < wavelengths.size(); i++) {
      file << wavelengths[i];
      file << (i + 1 < wavelengths.size() ? ", " : "}\n");
    }
    file << "header offset = 0\n";
    file << "interleave = bip\n";
    // A zero count is not recorded, and such a file cannot seed a
    // resumed accumulation.
    if (mNumSamples > 0) {
      file << "smdl spp = " << mNumSamples << '\n';
      // Only when it narrows the image: a whole-image window is what the
      // reader assumes anyway, so every unwindowed header stays as it was.
      if (!sameWindow(pixelWindow, wholeImage))
        file << "smdl window = {" << pixelWindow[0] << ", " << pixelWindow[1]
             << ", " << pixelWindow[2] << ", " << pixelWindow[3] << "}\n";
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

// Parse the `key = value` lines of an ENVI header into a map keyed by
// the lower-cased key. A value that opens a `{...}` array continues
// across lines until the closing brace, per the format.
[[nodiscard]] static std::map<std::string, std::string>
parseENVIHeader(const std::string &fileName, const std::string &text) {
  auto trim{[](std::string str) {
    const char *WS{" \t\r\n"};
    str.erase(0, str.find_first_not_of(WS));
    str.erase(str.find_last_not_of(WS) + 1);
    return str;
  }};
  auto fields{std::map<std::string, std::string>{}};
  size_t pos{};
  auto nextLine{[&]() -> std::string {
    size_t end{text.find('\n', pos)};
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
    auto equals{line.find('=')};
    if (equals == std::string::npos) continue;
    auto key{trim(line.substr(0, equals))};
    auto value{trim(line.substr(equals + 1))};
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
[[nodiscard]] static uint64_t
requiredCount(const std::string &fileName,
              std::map<std::string, std::string> &fields, const char *key) {
  auto itr{fields.find(key)};
  if (itr == fields.end())
    throw Error(concat("cannot load ", Quoted(fileName + ".hdr"), ": missing ",
                       Quoted(key), " field"));
  auto value{uint64_t(std::strtoull(itr->second.c_str(), nullptr, 10))};
  fields.erase(itr);
  return value;
}

SpectralFilm::ENVIFileInfo
SpectralFilm::readENVIFile(const std::string &fileName) try {
  auto result{ENVIFileInfo{}};
  auto fields{parseENVIHeader(fileName, readOrThrow(fileName + ".hdr"))};
  const auto numPixelsX{requiredCount(fileName, fields, "samples")};
  const auto numPixelsY{requiredCount(fileName, fields, "lines")};
  const auto numBands{requiredCount(fileName, fields, "bands")};
  // Accept exactly the format the writer emits: 64-bit floats,
  // band-interleaved-by-pixel. The byte order is the one thing worth
  // fixing up rather than rejecting.
  if (auto dataType{requiredCount(fileName, fields, "data type")};
      dataType != 5)
    throw Error(concat("cannot load ", Quoted(fileName), ": data type ",
                       dataType, " (expected 5, 64-bit float)"));
  if (auto itr{fields.find("interleave")};
      itr == fields.end() || itr->second != "bip")
    throw Error(concat("cannot load ", Quoted(fileName),
                       ": expected 'interleave = bip'"));
  else
    fields.erase(itr);
  const auto byteOrder{requiredCount(fileName, fields, "byte order")};
  const auto headerOffset{fields.count("header offset")
                              ? requiredCount(fileName, fields, "header offset")
                              : 0};
  if (auto itr{fields.find("smdl spp")}; itr != fields.end()) {
    result.samplesPerPixel =
        uint64_t(std::strtoull(itr->second.c_str(), nullptr, 10));
    fields.erase(itr);
  }
  result.window = int4{0, 0, int(numPixelsX), int(numPixelsY)};
  if (auto itr{fields.find("smdl window")}; itr != fields.end()) {
    auto value{itr->second};
    for (auto &c : value)
      if (c == '{' || c == '}' || c == ',') c = ' ';
    auto bounds{std::vector<int>()};
    const char *ptr{value.c_str()};
    char *end{};
    for (long i{std::strtol(ptr, &end, 10)}; end != ptr;
         i = std::strtol(ptr, &end, 10)) {
      bounds.push_back(int(i));
      ptr = end;
    }
    if (bounds.size() != 4)
      throw Error(concat("cannot load ", Quoted(fileName + ".hdr"), ": ",
                         bounds.size(),
                         " values in 'smdl window' (expected 4)"));
    result.window = int4(bounds.data());
    if (!(0 <= result.window[0] && result.window[0] < result.window[2] &&
          result.window[2] <= int(numPixelsX) && 0 <= result.window[1] &&
          result.window[1] < result.window[3] &&
          result.window[3] <= int(numPixelsY)))
      throw Error(
          concat("cannot load ", Quoted(fileName + ".hdr"), ": 'smdl window' ",
                 result.window[0], ",", result.window[1], ",", result.window[2],
                 ",", result.window[3], " is not a non-empty sub-rectangle of ",
                 numPixelsX, "x", numPixelsY));
    fields.erase(itr);
  }
  if (auto itr{fields.find("wavelength")}; itr != fields.end()) {
    // Strip the braces, then parse comma-separated floats.
    auto value{itr->second};
    for (auto &c : value)
      if (c == '{' || c == '}' || c == ',') c = ' ';
    const char *ptr{value.c_str()};
    char *end{};
    for (double w{std::strtod(ptr, &end)}; end != ptr;
         w = std::strtod(ptr, &end)) {
      result.wavelengths.push_back(float(w));
      ptr = end;
    }
    fields.erase(itr);
    if (result.wavelengths.size() != numBands)
      throw Error(concat("cannot load ", Quoted(fileName + ".hdr"), ": ",
                         result.wavelengths.size(), " wavelengths for ",
                         numBands, " bands"));
  }
  fields.erase("file type");
  fields.erase("wavelength units");
  result.fields = std::move(fields);
  // Read the binary file, reconstructing the accumulator invariant:
  // totals are means times the sample count, or the means themselves
  // at a count of 1 when the header does not record the count.
  const auto count{std::max(result.samplesPerPixel, uint64_t(1))};
  auto file{openOrThrow(fileName, std::ios::in | std::ios::binary)};
  file.ignore(std::streamsize(headerOffset));
  resize(numBands, numPixelsX, numPixelsY);
  addSamples(count);
  const bool swapBytes{byteOrder !=
                       (llvm::endianness::native == llvm::endianness::little
                            ? uint64_t(0)
                            : uint64_t(1))};
  auto values{std::vector<double>(numBands)};
  for (size_t iY = 0; iY < numPixelsY; iY++) {
    for (size_t iX = 0; iX < numPixelsX; iX++) {
      for (size_t i = 0; i < numBands; i++) {
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
      if (int(iX) >= result.window[0] && int(iX) < result.window[2] &&
          int(iY) >= result.window[1] && int(iY) < result.window[3])
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
