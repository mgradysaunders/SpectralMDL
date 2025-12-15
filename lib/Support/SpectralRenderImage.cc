#include "smdl/Support/SpectralRenderImage.h"
#include "smdl/Support/Filesystem.h"

#include "llvm/Support/Endian.h"

namespace smdl {

void SpectralRenderImage::clear() noexcept {
  numBands = 0;
  numPixelsX = 0;
  numPixelsY = 0;
  buf.reset();
}

void SpectralRenderImage::resize(size_t nBands, size_t nPixelsX,
                                 size_t nPixelsY) {
  clear();
  numBands = nBands;
  numPixelsX = nPixelsX;
  numPixelsY = nPixelsY;
  buf.reset(new uint8_t[getImageSizeInBytes()]);
}

SpectralRenderImage::PixelRef
SpectralRenderImage::operator()(size_t iX, size_t iY) noexcept {
  SMDL_SANITY_CHECK(iX < numPixelsX);
  SMDL_SANITY_CHECK(iY < numPixelsY);
  auto ptr{buf.get() + getPixelSizeInBytes() * (numPixelsX * iY + iX)};
  return {*reinterpret_cast<AtomicUInt64 *>(ptr),
          reinterpret_cast<AtomicDouble *>(ptr + sizeof(AtomicUInt64)),
          numBands};
}

SpectralRenderImage::PixelConstRef
SpectralRenderImage::operator()(size_t iX, size_t iY) const noexcept {
  SMDL_SANITY_CHECK(iX < numPixelsX);
  SMDL_SANITY_CHECK(iY < numPixelsY);
  auto ptr{buf.get() + getPixelSizeInBytes() * (numPixelsX * iY + iX)};
  return {*reinterpret_cast<const AtomicUInt64 *>(ptr),
          reinterpret_cast<const AtomicDouble *>(ptr + sizeof(AtomicUInt64)),
          numBands};
}

void SpectralRenderImage::add(const SpectralRenderImage &other) noexcept {
  SMDL_SANITY_CHECK(numBands == other.numBands);
  SMDL_SANITY_CHECK(numPixelsX == other.numPixelsX);
  SMDL_SANITY_CHECK(numPixelsY == other.numPixelsY);
  for (size_t iY{}; iY < numPixelsY; iY++) {
    for (size_t iX{}; iX < numPixelsX; iX++) {
      auto lhs{operator()(iX, iY)};
      auto rhs{other(iX, iY)};
      lhs.totalCount += rhs.totalCount;
      for (size_t i = 0; i < numBands; i++)
        lhs.totals[i] += rhs.totals[i];
    }
  }
}

void SpectralRenderImage::writeENVIFile(Span<const float> wavelengths,
                                        const std::string &fileName) const {
  // Write the header file
  {
    auto file{openOrThrow(fileName + ".hdr", std::ios::out)};
    file << "ENVI\n";
    file << "file type = ENVI Standard\n";
    file << "data type = 5\n";
    file << "byte order = "
         << (llvm::endianness::native == llvm::endianness::little ? 0 : 1)
         << '\n';
    file << "samples = " << numPixelsX << '\n';
    file << "lines = " << numPixelsY << '\n';
    file << "bands = " << numBands << '\n';
    file << "wavelength units = Nanometers\n";
    file << "wavelength = {";
    for (size_t i = 0; i < wavelengths.size(); i++) {
      file << wavelengths[i];
      file << (i + 1 < wavelengths.size() ? ", " : "}\n");
    }
    file << "header offset = 0\n";
    file << "interleave = bip\n";
  }
  // Write the binary file
  {
    auto file{openOrThrow(fileName, std::ios::out | std::ios::binary)};
    for (size_t iY = 0; iY < numPixelsY; iY++) {
      for (size_t iX = 0; iX < numPixelsX; iX++) {
        auto pixel{operator()(iX, iY)};
        for (double pixelValue : pixel) {
          file.write(reinterpret_cast<const char *>(&pixelValue), 8);
        }
      }
    }
  }
}

} // namespace smdl
