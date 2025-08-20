#include "smdl/Support/SpectralRenderImage.h"

#include "smdl/common.h"

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
  buf.reset(new uint8_t[image_size_in_bytes()]);
}

SpectralRenderImage::PixelRef
SpectralRenderImage::operator()(size_t iX, size_t iY) noexcept {
  SMDL_SANITY_CHECK(iX < numPixelsX);
  SMDL_SANITY_CHECK(iY < numPixelsY);
  auto ptr{buf.get() + pixel_size_in_bytes() * (numPixelsX * iY + iX)};
  return {*reinterpret_cast<AtomicUInt64 *>(ptr),
          reinterpret_cast<AtomicDouble *>(ptr + sizeof(AtomicUInt64)),
          numBands};
}

SpectralRenderImage::PixelConstRef
SpectralRenderImage::operator()(size_t iX, size_t iY) const noexcept {
  SMDL_SANITY_CHECK(iX < numPixelsX);
  SMDL_SANITY_CHECK(iY < numPixelsY);
  auto ptr{buf.get() + pixel_size_in_bytes() * (numPixelsX * iY + iX)};
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

} // namespace smdl
