#include "smdl/Support/SpectralRenderImage.h"
#include "smdl/Support/Filesystem.h"

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
  mBuf.reset(new uint8_t[getImageSizeInBytes()]);
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
      for (size_t i = 0; i < mNumBands; i++)
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
  }
  // Write the binary file
  {
    auto file{openOrThrow(fileName, std::ios::out | std::ios::binary)};
    for (size_t iY = 0; iY < mNumPixelsY; iY++) {
      for (size_t iX = 0; iX < mNumPixelsX; iX++) {
        auto pixel{operator()(iX, iY)};
        for (double pixelValue : pixel) {
          file.write(reinterpret_cast<const char *>(&pixelValue), 8);
        }
      }
    }
  }
}

} // namespace smdl
