#include <algorithm>
#include <cctype>
#include <cstdlib>

#include "smdl/Support/SpectralRenderImage.h"

#include "smdl/Support/Error.h"
#include "smdl/Support/Filesystem.h"
#include "smdl/Support/StringHelpers.h"

#include "llvm/Support/Endian.h"

namespace smdl {

void SpectralRenderImage::clear() noexcept {
  mNumBands = 0;
  mNumPixelsX = 0;
  mNumPixelsY = 0;
  mBuf.reset();
}

void SpectralRenderImage::resize(size_t nBands, size_t nPixelsX,
                                 size_t nPixelsY) {
  clear();
  mNumBands = nBands;
  mNumPixelsX = nPixelsX;
  mNumPixelsY = nPixelsY;
  // Value-initialize so every accumulator starts at zero. A plain `new
  // uint8_t[n]` is uninitialized, which happens to be zero for a fresh
  // mmap-backed allocation but is dirty when the allocator reuses heap
  // memory, e.g. the second `resize()` of an iterative render loop.
  mBuf.reset(new uint8_t[getImageSizeInBytes()]());
}

SpectralRenderImage::PixelRef
SpectralRenderImage::operator()(size_t iX, size_t iY) noexcept {
  SMDL_SANITY_CHECK(iX < mNumPixelsX);
  SMDL_SANITY_CHECK(iY < mNumPixelsY);
  auto ptr{mBuf.get() + getPixelSizeInBytes() * (mNumPixelsX * iY + iX)};
  return {*reinterpret_cast<AtomicUInt64 *>(ptr),
          reinterpret_cast<AtomicDouble *>(ptr + sizeof(AtomicUInt64)),
          mNumBands};
}

SpectralRenderImage::PixelConstRef
SpectralRenderImage::operator()(size_t iX, size_t iY) const noexcept {
  SMDL_SANITY_CHECK(iX < mNumPixelsX);
  SMDL_SANITY_CHECK(iY < mNumPixelsY);
  auto ptr{mBuf.get() + getPixelSizeInBytes() * (mNumPixelsX * iY + iX)};
  return {*reinterpret_cast<const AtomicUInt64 *>(ptr),
          reinterpret_cast<const AtomicDouble *>(ptr + sizeof(AtomicUInt64)),
          mNumBands};
}

void SpectralRenderImage::add(const SpectralRenderImage &other) noexcept {
  SMDL_SANITY_CHECK(mNumBands == other.mNumBands);
  SMDL_SANITY_CHECK(mNumPixelsX == other.mNumPixelsX);
  SMDL_SANITY_CHECK(mNumPixelsY == other.mNumPixelsY);
  for (size_t iY{}; iY < mNumPixelsY; iY++) {
    for (size_t iX{}; iX < mNumPixelsX; iX++) {
      auto lhs{operator()(iX, iY)};
      auto rhs{other(iX, iY)};
      lhs.totalCount += rhs.totalCount;
      for (size_t i = 0; i < mNumBands; i++) lhs.totals[i] += rhs.totals[i];
    }
  }
}

void SpectralRenderImage::writeENVIFile(
    Span<const float> wavelengths, const std::string &fileName,
    Span<const std::string> extraHeaderLines) const {
  // The per-pixel sample count, recorded in the header when it is
  // uniform over the image. Zero (an empty or partly empty image)
  // and non-uniform counts are simply not recorded, and the file
  // cannot seed a resumed accumulation.
  uint64_t samplesPerPixel{
      mNumPixelsX > 0 && mNumPixelsY > 0
          ? operator()(0, 0).totalCount.load(std::memory_order_relaxed)
          : 0};
  for (size_t iY = 0; iY < mNumPixelsY && samplesPerPixel > 0; iY++)
    for (size_t iX = 0; iX < mNumPixelsX; iX++)
      if (operator()(iX, iY).totalCount.load(std::memory_order_relaxed) !=
          samplesPerPixel) {
        samplesPerPixel = 0;
        break;
      }
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
    if (samplesPerPixel > 0)
      file << "samples per pixel = " << samplesPerPixel << '\n';
    for (const auto &line : extraHeaderLines) file << line << '\n';
  }
  // Write the binary file. The pixel values are means, not the raw
  // accumulated totals, so the file holds physically meaningful
  // radiance at any sample count.
  {
    auto file{openOrThrow(fileName, std::ios::out | std::ios::binary)};
    for (size_t iY = 0; iY < mNumPixelsY; iY++) {
      for (size_t iX = 0; iX < mNumPixelsX; iX++) {
        auto pixel{operator()(iX, iY)};
        for (size_t i = 0; i < mNumBands; i++) {
          double pixelValue{pixel.mean(i)};
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

SpectralRenderImage::ENVIFile
SpectralRenderImage::readENVIFile(const std::string &fileName) {
  auto result{ENVIFile{}};
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
  if (auto itr{fields.find("samples per pixel")}; itr != fields.end()) {
    result.samplesPerPixel =
        uint64_t(std::strtoull(itr->second.c_str(), nullptr, 10));
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
  result.image.resize(numBands, numPixelsX, numPixelsY);
  const bool swapBytes{byteOrder !=
                       (llvm::endianness::native == llvm::endianness::little
                            ? uint64_t(0)
                            : uint64_t(1))};
  for (size_t iY = 0; iY < numPixelsY; iY++) {
    for (size_t iX = 0; iX < numPixelsX; iX++) {
      auto pixel{result.image(iX, iY)};
      pixel.totalCount = count;
      for (size_t i = 0; i < numBands; i++) {
        char bytes[8]{};
        if (!file.read(bytes, 8))
          throw Error(concat("cannot load ", Quoted(fileName),
                             ": unexpected end of file"));
        if (swapBytes) std::reverse(bytes, bytes + 8);
        double mean{};
        std::memcpy(&mean, bytes, 8);
        pixel.totals[i] = mean * double(count);
      }
    }
  }
  return result;
}

} // namespace smdl
