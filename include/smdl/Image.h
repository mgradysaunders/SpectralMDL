/// \file
#pragma once

#include "smdl/Common.h"

namespace smdl {

/// \addtogroup Main
/// \{

/// Unpack half-precision to single-precision.
[[nodiscard]] SMDL_EXPORT float unpackHalf(const void *ptr) noexcept;

/// An image.
///
/// This is a generic container for image data held by the compiler
/// that is used at runtime by instances of `texture_2d` and/or
/// `texture_cube`.
///
class SMDL_EXPORT Image final {
public:
  typedef void *(*image_malloc_t)(size_t);

  typedef void *(*image_calloc_t)(size_t, size_t);

  typedef void *(*image_realloc_t)(void *, size_t);

  typedef void (*image_free_t)(void *);

  /// The function to use to mimic `std::malloc`.
  static image_malloc_t image_malloc;

  /// The function to use to mimic `std::calloc`.
  static image_calloc_t image_calloc;

  /// The function to use to mimic `std::realloc`.
  static image_realloc_t image_realloc;

  /// The function to use to mimic `std::free`.
  static image_free_t image_free;

  /// The underlying format.
  enum Format : int {
    UINT8 = 1,   ///< 8-bit unsigned integer.
    UINT16 = 2,  ///< 16-bit unsigned integer.
    FLOAT16 = 3, ///< 16-bit floating point, AKA half precision.
    FLOAT32 = 4  ///< 32-bit floating point, AKA single precision.
  };

  Image() = default;

  /// Non-copyable, and non-movable!
  Image(const Image &) = delete;

public:
  /// Clear everything.
  void clear();

  /// \name Load API
  ///
  /// The API here is specifically tailored to the internal usage by
  /// the compiler. In particular, loading is factored into two stages,
  /// so that the computationally expensive portion can be done in
  /// parallel at the end of the compile!
  ///
  /// \{

  /// Try to start loading from the given file name.
  ///
  /// The implementation quickly scans for the format, size, and number of
  /// channels. This is enough information to allocate the `texels` buffer,
  /// which is understood to exist for the lifetime of the SMDL compiler.
  ///
  /// That being the case, the buffer pointer can be baked into JIT-compiled
  /// SMDL code, but we can defer loading the entire image until later, so
  /// that we can parallelize all of the image loads.
  ///
  [[nodiscard]] std::optional<Error>
  startLoad(const std::string &fileName) noexcept;

  /// If `start_load()` was successful, finish loading the texels.
  ///
  /// \throw std::runtime_error If anything fails.
  ///
  void finishLoad();

  /// \}

  /// Flip vertically.
  void flipVertically() noexcept;

public:
  /// Get the format.
  [[nodiscard]] Format getFormat() const noexcept { return mFormat; }

  /// Get the number of channels, must be 1, 2, or 4.
  [[nodiscard]] int getNumChannels() const noexcept { return mNumChannels; }

  /// Get the number of texels in X.
  [[nodiscard]] int getNumTexelsX() const noexcept { return mNumTexelsX; }

  /// Get the number of texels in Y.
  [[nodiscard]] int getNumTexelsY() const noexcept { return mNumTexelsY; }

  /// Get the texel size in bytes.
  ///
  /// \note
  /// This is necessarily the number of channels times the
  /// implied size of the format.
  ///
  [[nodiscard]] int getTexelSizeInBytes() const noexcept { return mTexelSize; }

  /// Get texels.
  [[nodiscard]] auto getTexels() noexcept -> std::byte * {
    return mTexels.get();
  }

  /// Get texels, const variant.
  [[nodiscard]] auto getTexels() const noexcept -> const std::byte * {
    return mTexels.get();
  }

  /// Fetch texel.
  ///
  /// The valid runtime formats must contain 1, 2, or 4 channels of UINT8,
  /// UINT16, FLOAT16, or FLOAT32 channel type. The implementation converts all
  /// types to `float` by unsigned normalized integer conversion or by
  /// half-to-single precision conversion.
  ///
  /// Format  | Conversion
  /// --------|----------------------
  /// UINT8   | `value / 255.0f`
  /// UINT16  | `value / 65535.0f`
  /// FLOAT16 | `unpack_half(value)`
  /// FLOAT32 | `value`
  ///
  /// The implementation copies post-conversion channel values to
  /// the returned `float4` in order. Any channel not present is
  /// set to NaN.
  ///
  [[nodiscard]] float4 fetch(int x, int y) const noexcept;

private:
  /// The format.
  Format mFormat{UINT8};

  /// The number of channels, must be 1, 2, or 4 for proper alignment.
  int mNumChannels{1};

  /// The number of texels in X.
  int mNumTexelsX{0};

  /// The number of texels in Y.
  int mNumTexelsY{0};

  /// The texel size in bytes.
  int mTexelSize{1};

  /// The texel memory.
  std::unique_ptr<std::byte[]> mTexels{};

  /// The function to finish loading the image.
  std::function<void()> mFinishLoad{};
};

/// Write an 8-bit-per-channel image to the given file name.
///
/// \param[in] fileName
/// The file name, which must end with a recognized extension. The
/// extension test is case-insensitive:
/// - `.png` writes PNG,
/// - `.jpg` or `.jpeg` writes JPEG at quality 90,
/// - `.bmp` writes BMP,
/// - `.tga` writes TGA,
/// - `.pgm` or `.ppm` writes binary PNM.
///
/// \param[in] numTexelsX
/// The number of texels in X, must be positive.
///
/// \param[in] numTexelsY
/// The number of texels in Y, must be positive.
///
/// \param[in] numChannels
/// The number of channels, must be 1, 2, 3, or 4.
///
/// \param[in] texels
/// The texels, understood to be tightly packed 8-bit channels in
/// row-major order starting from the top-left corner, i.e., channel
/// `c` of the texel at `(x, y)` is the byte at offset
/// `numChannels * (x + numTexelsX * y) + c`.
///
/// The PNG, JPEG, BMP, and TGA cases are handled by stb_image_write
/// with the usual channel interpretations: 1 is gray, 2 is gray and
/// alpha, 3 is RGB, and 4 is RGBA, where formats without alpha drop
/// the alpha channel. The PGM case writes only the first channel of
/// each texel, and the PPM case writes the first three channels of
/// each texel, zero-padding if there are fewer than three.
///
/// \note
/// The requirements on the dimensions, the channel count, and the
/// texel pointer are enforced by fatal sanity checks, not by
/// recoverable errors.
///
/// \returns
/// `std::nullopt` if successful, or else an `Error` describing why
/// the image could not be written.
///
[[nodiscard]]
SMDL_EXPORT std::optional<Error>
write8bitImage(const std::string &fileName, int numTexelsX, int numTexelsY,
               int numChannels, const void *texels);

/// \}

} // namespace smdl
