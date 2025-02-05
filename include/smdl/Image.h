#pragma once

#include "smdl/common.h"

namespace smdl {

/// \addtogroup Main
/// \{

/// An image.
///
/// This is a generic container for image data held by the compiler
/// that is used at runtime by instances of `texture_2d` and/or
/// `texture_cube`.
///
class SMDL_EXPORT Image final {
public:
  /// The underlying format.
  enum Format : int {
    U8 = 1,  ///< 8-bit unsigned integer.
    U16 = 2, ///< 16-bit unsigned integer.
    F16 = 3, ///< 16-bit floating point, AKA half precision.
    F32 = 4  ///< 32-bit floating point, AKA single precision.
  };

  Image() = default;

  /// Non-copyable, and non-movable!
  Image(const Image &) = delete;

public:
  /// Get the format.
  [[nodiscard]] Format get_format() const { return format; }

  /// Get the number of channels, must be 1, 2, or 4.
  [[nodiscard]] int get_num_channels() const { return numChannels; }

  /// Get the number of texels in X.
  [[nodiscard]] int get_num_texels_x() const { return numTexelsX; }

  /// Get the number of texels in Y.
  [[nodiscard]] int get_num_texels_y() const { return numTexelsY; }

  /// Get the texel size in bytes.
  ///
  /// \note
  /// This is necessarily the number of channels times the
  /// implied size of the format.
  ///
  [[nodiscard]] int get_texel_size_in_bytes() const { return texelSize; }

  /// Get the texels.
  [[nodiscard]] auto get_texels() -> std::byte * { return texels.get(); }

  /// Get the texels, const variant.
  [[nodiscard]] auto get_texels() const -> const std::byte * {
    return texels.get();
  }

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
  /// \throw std::runtime_error If anything fails.
  ///
  void start_load(const std::string &fileName);

  /// If `start_load()` was successful, finish loading the texels.
  ///
  /// \throw std::runtime_error If anything fails.
  ///
  void finish_load();

  /// \}

private:
  /// The format.
  Format format{U8};

  /// The number of channels, must be 1, 2, or 4 for proper alignment.
  int numChannels{1};

  /// The number of texels in X.
  int numTexelsX{0};

  /// The number of texels in Y.
  int numTexelsY{0};

  /// The texel size in bytes.
  int texelSize{1};

  /// The texel memory.
  std::unique_ptr<std::byte[]> texels{};

  /// The function to finish loading the image.
  std::function<void()> finishLoad{};
};

/// \}

} // namespace smdl
