/// \file
#pragma once

#include <atomic>
#include <cmath>
#include <cstring>
#include <map>
#include <memory>
#include <string>
#include <vector>

#include "smdl/Export.h"
#include "smdl/Support/Macros.h"
#include "smdl/Support/Span.h"

namespace smdl {

/// \addtogroup support
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
    AtomicDouble(double value) noexcept : mBits(bit_cast<uint64_t>(value)) {}

    /// Assign new value.
    void operator=(double value) noexcept { mBits = bit_cast<uint64_t>(value); }

    /// Atomic add.
    void operator+=(double value) noexcept {
      uint64_t oldBits = mBits;
      uint64_t newBits = 0;
      do {
        newBits = bit_cast<uint64_t>(bit_cast<double>(oldBits) + value);
      } while (!mBits.compare_exchange_weak(oldBits, newBits));
    }

    /// Read out the value.
    operator double() const noexcept {
      return bit_cast<double>(uint64_t(mBits));
    }

  private:
    AtomicUInt64 mBits{};
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
  [[nodiscard]] size_t getNumBands() const noexcept { return mNumBands; }

  /// The number of pixels in X.
  [[nodiscard]] size_t getNumPixelsX() const noexcept { return mNumPixelsX; }

  /// The number of pixels in Y.
  [[nodiscard]] size_t getNumPixelsY() const noexcept { return mNumPixelsY; }

  /// The image size in bytes.
  [[nodiscard]] size_t getImageSizeInBytes() const noexcept {
    return mNumPixelsX * mNumPixelsY * getPixelSizeInBytes();
  }

  /// The pixel size in bytes.
  [[nodiscard]] size_t getPixelSizeInBytes() const noexcept {
    return sizeof(AtomicUInt64) + sizeof(AtomicDouble) * mNumBands;
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

    /// Add an already-summed contribution of `numSamples` samples: the
    /// count grows by `numSamples` while the totals grow by the given
    /// sums. The buffer invariant this maintains is that `totals` is
    /// always the raw sum over `totalCount` samples, and the mean is
    /// taken only at the edges (see `mean()`).
    ///
    /// \param[in] numSamples
    /// The number of samples summed into `totalPtr`.
    ///
    /// \param[in] totalPtr
    /// The pointer to `numBands` summed contribution values.
    ///
    template <typename T>
    void addSamples(uint64_t numSamples, const T *totalPtr) noexcept {
      totalCount += numSamples;
      for (size_t i = 0; i < numBands; i++) {
        totals[i] += static_cast<double>(totalPtr[i]);
      }
    }

    /// The mean of band `i`: the accumulated total divided by the
    /// sample count, or 0 with no samples. This is the single source of
    /// radiance values; every output must read the buffer through it.
    [[nodiscard]] double mean(size_t i) const noexcept {
      SMDL_SANITY_CHECK(i < numBands);
      uint64_t count{totalCount.load(std::memory_order_relaxed)};
      return count > 0 ? double(totals[i]) / double(count) : 0.0;
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

    /// The mean of band `i`: the accumulated total divided by the
    /// sample count, or 0 with no samples. This is the single source of
    /// radiance values; every output must read the buffer through it.
    [[nodiscard]] double mean(size_t i) const noexcept {
      SMDL_SANITY_CHECK(i < numBands);
      uint64_t count{totalCount.load(std::memory_order_relaxed)};
      return count > 0 ? double(totals[i]) / double(count) : 0.0;
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
  /// The binary file holds the per-pixel per-band MEAN, i.e., the
  /// accumulated totals divided by each pixel's sample count, so the
  /// pixel values are physically meaningful radiance no matter how many
  /// samples were folded in. When the sample count is uniform over the
  /// image, the header records it as `samples per pixel = N`, which is
  /// what `readENVIFile()` uses to reconstruct the totals so that a
  /// later render can keep accumulating.
  ///
  /// \param[in] wavelengths
  /// The wavelengths in nanometers.
  ///
  /// \param[in] fileName
  /// The filename of the image. The header is written alongside as
  /// `fileName + ".hdr"`.
  ///
  /// \param[in] extraHeaderLines
  /// Additional `key = value` lines appended to the header verbatim,
  /// for application metadata. Lines whose keys ENVI readers do not
  /// recognize are ignored by convention.
  ///
  void writeENVIFile(Span<const float> wavelengths, const std::string &fileName,
                     Span<const std::string> extraHeaderLines = {}) const;

  /// The result of `readENVIFile()`, defined after the class.
  struct ENVIFile;

  /// Read an ENVI Standard image file and header, as written by
  /// `writeENVIFile()`.
  ///
  /// The header is read from `fileName + ".hdr"`. Only the exact
  /// format the writer emits is accepted (`data type = 5`, `interleave
  /// = bip`); a byte order other than the native one is swapped on
  /// load.
  ///
  /// \throws Error if either file is missing or malformed.
  ///
  [[nodiscard]] static ENVIFile readENVIFile(const std::string &fileName);

private:
  size_t mNumBands{};

  size_t mNumPixelsX{};

  size_t mNumPixelsY{};

  std::unique_ptr<uint8_t[]> mBuf{};
};

/// The result of `SpectralRenderImage::readENVIFile()`.
struct SpectralRenderImage::ENVIFile final {
  /// The image, with totals and per-pixel counts reconstructed from
  /// the stored means and the `samples per pixel` header field.
  SpectralRenderImage image{};

  /// The wavelengths in nanometers, empty if the header has none.
  std::vector<float> wavelengths{};

  /// The value of the `samples per pixel` header field, or 0 if the
  /// header does not carry one (a foreign or legacy file). Without
  /// it the image reads back with a count of 1 per pixel, so means
  /// stay meaningful but the true sample count is unknown.
  uint64_t samplesPerPixel{};

  /// Every header field this loader has no specific handling for,
  /// keyed by the lower-cased field name, values verbatim.
  std::map<std::string, std::string> fields{};
};

/// \}

} // namespace smdl
