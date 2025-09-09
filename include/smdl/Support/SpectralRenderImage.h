/// \file
#pragma once

#include <atomic>
#include <cmath>
#include <cstring>
#include <memory>

#include "smdl/Export.h"
#include "smdl/Support/MacroHelpers.h"
#include "smdl/Support/Span.h"

namespace smdl {

/// \addtogroup Support
/// \{

/// A spectral render image helper.
class SMDL_EXPORT SpectralRenderImage final {
public:
  /// An atomic 64-bit unsigned integer.
  using AtomicUInt64 = std::atomic<uint64_t>;

  /// An atomic double to accumulate contributions in a thread-safe way.
  class AtomicDouble final {
  public:
    /// Bit cast between trivial types of equal size.
    template <typename To, typename From>
    [[nodiscard]] static To bit_cast(const From &from) noexcept {
      To to{};
      static_assert(sizeof(to) == sizeof(from));
      std::memcpy(&to, &from, sizeof(from));
      return to;
    }

    /// Construct zero.
    AtomicDouble() = default;

    /// Construct from value.
    AtomicDouble(double value) noexcept : bits(bit_cast<uint64_t>(value)) {}

    /// Assign new value.
    void operator=(double value) noexcept { bits = bit_cast<uint64_t>(value); }

    /// Atomic add.
    void operator+=(double value) noexcept {
      uint64_t oldBits = bits;
      uint64_t newBits = 0;
      do {
        newBits = bit_cast<uint64_t>(bit_cast<double>(oldBits) + value);
      } while (!bits.compare_exchange_weak(oldBits, newBits));
    }

    /// Read out the value.
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

  /// Clear.
  void clear() noexcept;

  /// Resize.
  void resize(size_t nBands, size_t nPixelsX, size_t nPixelsY);

  /// The number of spectral bands.
  [[nodiscard]] size_t num_bands() const noexcept { return numBands; }

  /// The number of pixels in X.
  [[nodiscard]] size_t num_pixels_x() const noexcept { return numPixelsX; }

  /// The number of pixels in Y.
  [[nodiscard]] size_t num_pixels_y() const noexcept { return numPixelsY; }

  /// The image size in bytes.
  [[nodiscard]] size_t image_size_in_bytes() const noexcept {
    return numPixelsX * numPixelsY * pixel_size_in_bytes();
  }

  /// The pixel size in bytes.
  [[nodiscard]] size_t pixel_size_in_bytes() const noexcept {
    return sizeof(AtomicUInt64) + sizeof(AtomicDouble) * numBands;
  }

  /// A pixel reference.
  struct PixelRef final {
  public:
    /// The total number of calls to `add`.
    AtomicUInt64 &totalCount;

    /// The totals for each spectral band.
    AtomicDouble *const totals;

    /// The number of spectral bands.
    const size_t numBands;

    /// The size.
    [[nodiscard]] size_t size() const noexcept { return numBands; }

    /// Range-API begin.
    [[nodiscard]] AtomicDouble *begin() const noexcept { return totals; }

    /// Range-API end.
    [[nodiscard]] AtomicDouble *end() const noexcept {
      return totals + numBands;
    }

    /// Access by index.
    [[nodiscard]] AtomicDouble &operator[](size_t i) noexcept {
      SMDL_SANITY_CHECK(i < numBands);
      return totals[i];
    }

    /// Add contribution.
    ///
    /// \param[in] valuePtr
    /// The pointer to `numBands` contribution values.
    ///
    template <typename T> void add(const T *valuePtr) noexcept {
      totalCount += 1;
      for (size_t i = 0; i < numBands; i++) {
        totals[i] += static_cast<double>(valuePtr[i]);
      }
    }

    /// Add contribution with weight.
    ///
    /// \param[in] weight
    /// The weight.
    ///
    /// \param[in] valuePtr
    /// The pointer to `numBands` contribution values.
    ///
    template <typename T> void add(double weight, const T *valuePtr) noexcept {
      totalCount += 1;
      if (std::isfinite(weight) && weight > 0.0) {
        for (size_t i = 0; i < numBands; i++) {
          totals[i] += weight * static_cast<double>(valuePtr[i]);
        }
      }
    }
  };

  /// A pixel const reference.
  struct PixelConstRef final {
  public:
    /// The total number of calls to `add`.
    const AtomicUInt64 &totalCount;

    /// The totals for each spectral band.
    const AtomicDouble *const totals;

    /// The number of spectral bands.
    const size_t numBands;

    /// The size.
    [[nodiscard]] size_t size() const noexcept { return numBands; }

    /// Range-API begin.
    [[nodiscard]] const AtomicDouble *begin() const noexcept { return totals; }

    /// Range-API end.
    [[nodiscard]] const AtomicDouble *end() const noexcept {
      return totals + numBands;
    }

    /// Access by index.
    [[nodiscard]] const AtomicDouble &operator[](size_t i) noexcept {
      SMDL_SANITY_CHECK(i < numBands);
      return totals[i];
    }
  };

  /// Get pixel reference.
  [[nodiscard]] PixelRef operator()(size_t iX, size_t iY) noexcept;

  /// Get pixel const reference.
  [[nodiscard]] PixelConstRef operator()(size_t iX, size_t iY) const noexcept;

  /// Add the contents of another image pixel-by-pixel.
  void add(const SpectralRenderImage &other) noexcept;

  /// Write an ENVI Standard image file and header.
  ///
  /// \param[in] wavelengths
  /// The wavelengths in nanometers.
  ///
  /// \param[in] fileName
  /// The filename of the image.
  ///
  void write_envi_file(Span<const float> wavelengths,
                       const std::string &fileName) const;

private:
  size_t numBands{};

  size_t numPixelsX{};

  size_t numPixelsY{};

  std::unique_ptr<uint8_t[]> buf{};
};

/// \}

} // namespace smdl
