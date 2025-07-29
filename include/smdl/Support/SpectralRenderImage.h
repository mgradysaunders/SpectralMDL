/// \file
#pragma once

#include <atomic>
#include <cstring>
#include <memory>

#include "smdl/Export.h"
#include "smdl/Support/Span.h"

namespace smdl {

/// \addtogroup Support
/// \{

class SMDL_EXPORT SpectralRenderImage final {
public:
  using AtomicUInt64 = std::atomic<uint64_t>;

  class AtomicDouble final {
  public:
    template <typename U, typename T>
    [[nodiscard]] static U bit_cast(const T &from) noexcept {
      U to{};
      static_assert(sizeof(to) == sizeof(from));
      std::memcpy(&to, &from, sizeof(from));
      return to;
    }

    AtomicDouble() = default;

    AtomicDouble(double value) noexcept : bits(bit_cast<uint64_t>(value)) {}

    void operator=(double value) noexcept { bits = bit_cast<uint64_t>(value); }

    void operator+=(double value) noexcept {
      uint64_t oldBits = bits;
      uint64_t newBits = 0;
      do {
        newBits = bit_cast<uint64_t>(bit_cast<double>(oldBits) + value);
      } while (!bits.compare_exchange_weak(oldBits, newBits));
    }

    operator double() const noexcept {
      return bit_cast<double>(uint64_t(bits));
    }

  private:
    AtomicUInt64 bits{};
  };

  static_assert(sizeof(AtomicUInt64) == 8 && sizeof(AtomicDouble) == 8);

public:
  SpectralRenderImage() = default;

  explicit SpectralRenderImage(size_t nBands,   //
                               size_t nPixelsX, //
                               size_t nPixelsY) {
    resize(nBands, nPixelsX, nPixelsY);
  }

  void clear() noexcept;

  void resize(size_t nBands, size_t nPixelsX, size_t nPixelsY);

  [[nodiscard]] size_t num_bands() const noexcept { return numBands; }

  [[nodiscard]] size_t num_pixels_x() const noexcept { return numPixelsX; }

  [[nodiscard]] size_t num_pixels_y() const noexcept { return numPixelsY; }

  [[nodiscard]] size_t image_size_in_bytes() const noexcept {
    return numPixelsX * numPixelsY * pixel_size_in_bytes();
  }

  [[nodiscard]] size_t pixel_size_in_bytes() const noexcept {
    return sizeof(AtomicUInt64) + sizeof(AtomicDouble) +
           sizeof(AtomicDouble) * numBands;
  }

  struct PixelReference final {
    template <typename Float>
    void add_sample(double weight, Span<Float> values) noexcept {
      numSamples += 1;
      if (weight > 0.0) {
        totalWeight += weight;
        for (size_t i{}; i < values.size(); i++) {
          totalValues[i] += weight * double(values[i]);
        }
      }
    }

    AtomicUInt64 &numSamples;
    AtomicDouble &totalWeight;
    AtomicDouble *totalValues;
  };

  [[nodiscard]] PixelReference pixel_reference(size_t pixelX,
                                               size_t pixelY) noexcept;

private:
  size_t numBands{};

  size_t numPixelsX{};

  size_t numPixelsY{};

  std::unique_ptr<uint8_t[]> buf{};
};

} // namespace smdl
