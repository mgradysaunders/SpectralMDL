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

SpectralRenderImage::PixelReference
SpectralRenderImage::pixel_reference(size_t pixelX, size_t pixelY) noexcept {
  SMDL_SANITY_CHECK(pixelX < numPixelsX);
  SMDL_SANITY_CHECK(pixelY < numPixelsY);
  auto ptr{buf.get() + pixel_size_in_bytes() * (numPixelsX * pixelY + pixelX)};
  return {*reinterpret_cast<AtomicUInt64 *>(ptr),
          reinterpret_cast<AtomicDouble *>(ptr + sizeof(AtomicUInt64)),
          numBands};
}

void SpectralRenderImage::add(const SpectralRenderImage &other) noexcept {
  SMDL_SANITY_CHECK(numBands == other.numBands);
  SMDL_SANITY_CHECK(numPixelsX == other.numPixelsX);
  SMDL_SANITY_CHECK(numPixelsY == other.numPixelsY);
  for (size_t pixelY{}; pixelY < numPixelsY; pixelY++) {
    for (size_t pixelX{}; pixelX < numPixelsX; pixelX++) {
      auto lhs{pixel_reference(pixelX, pixelY)};
      auto rhs{const_cast<SpectralRenderImage &>(other).pixel_reference(
          pixelX, pixelY)};
      lhs.totalCount += rhs.totalCount;
      for (size_t i = 0; i < numBands; i++)
        lhs.totals[i] += rhs.totals[i];
    }
  }
}

} // namespace smdl
