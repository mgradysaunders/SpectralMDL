#if defined(_WIN32)
#ifndef NOMINMAX
#define NOMINMAX
#endif
#endif

#include "smdl/Resource/Image.h"

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-function"
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

extern "C" {
#define STBI_ASSERT(X) ((void)0)
#define STBI_MALLOC(sz) ::smdl::Image::image_malloc(sz)
#define STBI_REALLOC(p, newsz) ::smdl::Image::image_realloc(p, newsz)
#define STBI_FREE(p) ::smdl::Image::image_free(p)
#define STBI_ONLY_JPEG 1
#define STBI_ONLY_PNG 1
#define STBI_ONLY_TGA 1
#define STBI_ONLY_BMP 1
#define STBI_ONLY_PNM 1
#define STBI_ONLY_HDR 1
#define STB_IMAGE_STATIC 1
#define STB_IMAGE_IMPLEMENTATION 1
#include "thirdparty/stb_image.h"

#define STBIW_ASSERT(X) ((void)0)
#define STBIW_MALLOC(sz) ::smdl::Image::image_malloc(sz)
#define STBIW_REALLOC(p, newsz) ::smdl::Image::image_realloc(p, newsz)
#define STBIW_FREE(p) ::smdl::Image::image_free(p)
#define STB_IMAGE_WRITE_STATIC 1
#define STB_IMAGE_WRITE_IMPLEMENTATION 1
#include "thirdparty/stb_image_write.h"
} // extern "C"

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic pop
#endif

// NOTE: Outside the 'extern "C"' block above because the implementation
// pulls in SIMD intrinsics headers.
#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-function"
#endif
#define STBIR_ASSERT(X) ((void)0)
#define STBIR_MALLOC(sz, user) ::smdl::Image::image_malloc(sz)
#define STBIR_FREE(p, user) ::smdl::Image::image_free(p)
#define STB_IMAGE_RESIZE_STATIC 1
#define STB_IMAGE_RESIZE_IMPLEMENTATION 1
#include "thirdparty/stb_image_resize2.h"
#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic pop
#endif

#define TINYEXR_MALLOC(sz) ::smdl::Image::image_malloc(sz)
#define TINYEXR_CALLOC(n, sz) ::smdl::Image::image_calloc(n, sz)
#define TINYEXR_FREE(p) ::smdl::Image::image_free(p)
#define TINYEXR_USE_MINIZ 1
#define TINYEXR_USE_THREAD 0
#define TINYEXR_IMPLEMENTATION 1
#include "thirdparty/tinyexr.h"

namespace tinyexr {

[[nodiscard]] static const EXRChannelInfo *FindChannel(const EXRHeader &header,
                                                       std::string_view name) {
  for (int iC = 0; iC < header.num_channels; iC++)
    if (header.channels[iC].name == name) return &header.channels[iC];
  return nullptr;
}

[[nodiscard]] static size_t GetPixelSize(const EXRChannelInfo &info) {
  return info.pixel_type == TINYEXR_PIXELTYPE_HALF ? 2 : 4;
}

static void
ForEachPixel(const EXRHeader &header, const EXRImage &image,
             const std::function<void(int iX, int iY, int iC, const void *pixel,
                                      size_t pixelSize)> &callback) {
  if (header.tiled) {
    size_t nTileX{size_t(header.tile_size_x)};
    size_t nTileY{size_t(header.tile_size_y)};
    size_t nC{size_t(header.num_channels)};
    for (size_t iTile = 0; iTile < size_t(image.num_tiles); iTile++) {
      auto &tile{image.tiles[iTile]};
      size_t i{};
      for (size_t iTileY = 0; iTileY < nTileY; iTileY++) {
        for (size_t iTileX = 0; iTileX < nTileX; iTileX++) {
          auto iX = tile.offset_x * nTileX + iTileX;
          auto iY = tile.offset_y * nTileY + iTileY;
          if (iX < size_t(image.width) && iY < size_t(image.height)) {
            for (size_t iC = 0; iC < nC; iC++) {
              size_t pixelSize{GetPixelSize(header.channels[iC])};
              callback(int(iX), int(iY), int(iC),
                       tile.images[iC] + pixelSize * i, pixelSize);
            }
          }
          i++;
        }
      }
    }
  } else {
    size_t nX{size_t(image.width)};
    size_t nY{size_t(image.height)};
    size_t nC{size_t(header.num_channels)};
    size_t i{};
    for (size_t iY = 0; iY < nY; iY++) {
      for (size_t iX = 0; iX < nX; iX++) {
        for (size_t iC = 0; iC < nC; iC++) {
          size_t pixelSize{GetPixelSize(header.channels[iC])};
          callback(int(iX), int(iY), int(iC), image.images[iC] + pixelSize * i,
                   pixelSize);
        }
        i++;
      }
    }
  }
}

} // namespace tinyexr

namespace smdl {

float unpackHalf(const void *ptr) noexcept {
#if __clang__
  return float(*static_cast<const _Float16 *>(ptr));
#else
  uint16_t h = *static_cast<const uint16_t *>(ptr);
  uint32_t f = 0;
  int32_t exponent = (h >> 10) & 0x001F;
  int32_t negative = (h >> 15) & 0x0001;
  int32_t mantissa = h & 0x03FF;
  f = negative << 31;
  if (exponent == 0) {
    if (mantissa != 0) {
      exponent = 113;
      while (!(mantissa & 0x0400)) exponent--, mantissa <<= 1;
      f |= (exponent << 23) | ((mantissa & ~0x0400) << 13); // Subnormal
    }
  } else {
    f |= mantissa << 13;
    f |= exponent == 31 ? 0x7F800000 : ((exponent + 112) << 23);
  }
  float result{};
  std::memcpy(&result, &f, sizeof(result));
  return result;
#endif // #if __clang__
}

Image::image_malloc_t Image::image_malloc = &std::malloc;

Image::image_calloc_t Image::image_calloc = &std::calloc;

Image::image_realloc_t Image::image_realloc = &std::realloc;

Image::image_free_t Image::image_free = &std::free;

void Image::clear() {
  mFormat = UINT8;
  mNumTexelsX = 0;
  mNumTexelsY = 0;
  mNumChannels = 1;
  mTexelSize = 1;
  mNumLevels = 1;
  mMipLevelsRequested = false;
  mMipLevelsGenerated = false;
  mMipFilter = MIP_MEAN;
  mLevelOffsets.assign(1, 0);
  mSizeInBytes = 0;
  mTexels.reset();
  mFinishLoad = nullptr;
}

std::optional<Error> Image::startLoad(const std::string &fileName) noexcept {
  clear();
  auto error{catchAndReturnError([&] {
    if (stbi_info(fileName.c_str(), &mNumTexelsX, &mNumTexelsY,
                  &mNumChannels)) {
      // If the number of channels is 3, i.e., RGB, round it up to 4
      // so all of our alignment assumptions work.
      if (mNumChannels == 3) mNumChannels = 4;
      // Determine whether we should load 32-bit float, 16-bit unsigned int, or
      // 8-bit unsigned int.
      if (stbi_is_hdr(fileName.c_str())) {
        mFormat = FLOAT32;
        mTexelSize = 4 * mNumChannels;
      } else if (stbi_is_16_bit(fileName.c_str())) {
        mFormat = UINT16;
        mTexelSize = 2 * mNumChannels;
      } else {
        mFormat = UINT8;
        mTexelSize = 1 * mNumChannels;
      }
      // Defer the actual load until later!
      mFinishLoad = [this, fileName]() {
        // NOTE: The thread-local variant because 'finishLoad()' may be
        // called from many threads at once.
        stbi_set_flip_vertically_on_load_thread(0);
        int nX{};
        int nY{};
        int nChannels{};
        void *ptr{};
        switch (mFormat) {
        default:
        case Format::UINT8:
          // Load 8-bit unsigned int.
          ptr = stbi_load(fileName.c_str(), &nX, &nY, &nChannels, mNumChannels);
          break;
        case Format::UINT16:
          // Load 16-bit unsigned int.
          ptr = stbi_load_16(fileName.c_str(), &nX, &nY, &nChannels,
                             mNumChannels);
          break;
        case Format::FLOAT32:
          // Load 32-bit float.
          ptr =
              stbi_loadf(fileName.c_str(), &nX, &nY, &nChannels, mNumChannels);
          break;
        }
        if (!ptr)
          throw std::runtime_error(std::string("stb image failure: ") +
                                   stbi_failure_reason());
        // Copy into the pre-allocated texel buffer, then free the pointer.
        SMDL_SANITY_CHECK(mTexels != nullptr);
        SMDL_SANITY_CHECK(mNumTexelsX == nX);
        SMDL_SANITY_CHECK(mNumTexelsY == nY);
        std::memcpy(mTexels.get(), ptr,
                    size_t(mNumTexelsX) * size_t(mNumTexelsY) *
                        size_t(mTexelSize));
        stbi_image_free(ptr);
      };
    } else if (EXRVersion version{};
               ParseEXRVersionFromFile(&version, fileName.c_str()) ==
               TINYEXR_SUCCESS) {
      // Fail if deep or multipart!
      if (version.non_image || version.multipart)
        throw std::runtime_error("deep or multipart EXR is not supported");
      // Parse the header. Hold it in a shared pointer, so that exactly
      // 1 'FreeEXRHeader()' happens no matter whether the finish
      // function below runs, throws, is copied, or is dropped without
      // ever being called.
      auto header{
          std::shared_ptr<EXRHeader>(new EXRHeader{}, [](EXRHeader *ptr) {
            FreeEXRHeader(ptr);
            delete ptr;
          })};
      InitEXRHeader(header.get());
      const char *err{};
      if (ParseEXRHeaderFromFile(header.get(), &version, fileName.c_str(),
                                 &err) != TINYEXR_SUCCESS) {
        FreeEXRErrorMessage(err);
        throw std::runtime_error("cannot parse EXR header");
      }
      int nX{header->data_window.max_x - header->data_window.min_x + 1};
      int nY{header->data_window.max_y - header->data_window.min_y + 1};
      if (nX < 0 || nY < 0)
        throw std::runtime_error(
            "cannot parse EXR header: invalid data window");

      const auto setFormatFromPixelType{[&](int pixelType) {
        if (pixelType == TINYEXR_PIXELTYPE_UINT)
          throw std::runtime_error("uint EXR is not supported");
        else if (pixelType == TINYEXR_PIXELTYPE_HALF)
          mFormat = FLOAT16, mTexelSize = 2 * mNumChannels;
        else if (pixelType == TINYEXR_PIXELTYPE_FLOAT)
          mFormat = FLOAT32, mTexelSize = 4 * mNumChannels;
        else
          SMDL_SANITY_CHECK_MSG(false, "unknown EXR pixel type");
      }};
      if (header->num_channels == 1) {
        // 1-channel R
        mNumChannels = 1;
        setFormatFromPixelType(header->channels[0].pixel_type);
      } else {
        // 4-channel RGBA
        mNumChannels = 4;
        const EXRChannelInfo *channels[4] = {
            tinyexr::FindChannel(*header, "R"), //
            tinyexr::FindChannel(*header, "G"),
            tinyexr::FindChannel(*header, "B"),
            tinyexr::FindChannel(*header, "A")};
        if (!channels[0])
          throw std::runtime_error("expected EXR channel 'R' is missing");
        if (!channels[1])
          throw std::runtime_error("expected EXR channel 'G' is missing");
        if (!channels[2])
          throw std::runtime_error("expected EXR channel 'B' is missing");
        // NOTE: We allow missing 'A' channel!
        for (auto channel : channels)
          if (channel && channel->pixel_type != channels[0]->pixel_type)
            throw std::runtime_error("inconsistent EXR pixel types");
        setFormatFromPixelType(channels[0]->pixel_type);
      }
      mNumTexelsX = nX;
      mNumTexelsY = nY;
      mFinishLoad = [this, fileName, header]() {
        EXRImage image{};
        InitEXRImage(&image);
        const char *err{};
        if (LoadEXRImageFromFile(&image, header.get(), fileName.c_str(),
                                 &err) != TINYEXR_SUCCESS) {
          auto message{std::string("tinyexr failed: ") + err};
          FreeEXRErrorMessage(err);
          throw std::runtime_error(message);
        }
        SMDL_DEFER([&image]() { FreeEXRImage(&image); });
        SMDL_SANITY_CHECK(mNumTexelsX == image.width);
        SMDL_SANITY_CHECK(mNumTexelsY == image.height);
        if (mNumChannels == 1) {
          // 1-channel R
          tinyexr::ForEachPixel(
              *header, image,
              [&](int iX, int iY, int iC, const void *pixel, size_t pixelSize) {
                auto i{size_t(iX) + size_t(mNumTexelsX) * size_t(iY)};
                auto texel{mTexels.get() + size_t(mTexelSize) * i};
                if (iC == 0) std::memcpy(texel, pixel, pixelSize);
              });
        } else {
          // 4-channel RGBA
          const EXRChannelInfo *channels[4] = {
              tinyexr::FindChannel(*header, "R"),
              tinyexr::FindChannel(*header, "G"),
              tinyexr::FindChannel(*header, "B"),
              tinyexr::FindChannel(*header, "A")};
          // RGB Required!
          int channelIndexR = int(channels[0] - &header->channels[0]);
          int channelIndexG = int(channels[1] - &header->channels[0]);
          int channelIndexB = int(channels[2] - &header->channels[0]);
          int channelIndexA =
              channels[3] ? int(channels[3] - &header->channels[0]) : -1;
          tinyexr::ForEachPixel(
              *header, image,
              [&](int iX, int iY, int iC, const void *pixel, size_t pixelSize) {
                auto i{size_t(iX) + size_t(mNumTexelsX) * size_t(iY)};
                auto texel{mTexels.get() + size_t(mTexelSize) * i};
                if (iC == channelIndexR) {
                  std::memcpy(texel + 0 * pixelSize, pixel, pixelSize);
                } else if (iC == channelIndexG) {
                  std::memcpy(texel + 1 * pixelSize, pixel, pixelSize);
                } else if (iC == channelIndexB) {
                  std::memcpy(texel + 2 * pixelSize, pixel, pixelSize);
                } else if (iC == channelIndexA) {
                  std::memcpy(texel + 3 * pixelSize, pixel, pixelSize);
                }
              });
          // If the alpha channel is missing, fill with 1.
          if (!channels[3]) {
            if (mFormat == FLOAT16) {
              auto one{uint16_t(0x3C00)};
              auto itr{mTexels.get() + 6};
              for (int i = 0; i < mNumTexelsX * mNumTexelsY; i++) {
                std::memcpy(itr, &one, 2);
                itr += mTexelSize;
              }
            } else if (mFormat == FLOAT32) {
              auto one{float(1.0f)};
              auto itr{mTexels.get() + 12};
              for (int i = 0; i < mNumTexelsX * mNumTexelsY; i++) {
                std::memcpy(itr, &one, 4);
                itr += mTexelSize;
              }
            } else {
              SMDL_SANITY_CHECK_MSG(
                  false, "format must be FLOAT16 or FLOAT32 by now!");
            }
          }
        }
      };
    } else {
      throw std::runtime_error("failed to open file or recognize image format");
    }
    // How many levels the extent implies, which is all of the layout
    // that can be settled here: whether anything wants them is not known
    // until every reference has been seen, so 'finishLoad()' does the
    // rest once the answer is in.
    mNumLevels = 1;
    while ((std::max(mNumTexelsX, mNumTexelsY) >> (mNumLevels - 1)) > 1)
      mNumLevels++;
  })};
  if (error) {
    clear();
    error->message =
        concat("cannot load ", QuotedPath(fileName), ": ", error->message);
  }
  return error;
}

void Image::allocate() {
  const auto numLevels{getNumLevels()};
  mLevelOffsets.assign(size_t(numLevels), 0);
  size_t offset{0};
  for (int level = 0; level < numLevels; level++) {
    mLevelOffsets[size_t(level)] = offset;
    offset += size_t(getNumTexelsX(level)) * size_t(getNumTexelsY(level)) *
              size_t(mTexelSize);
  }
  mSizeInBytes = offset;
  mTexels.reset(static_cast<std::byte *>(
      ::operator new(mSizeInBytes, std::align_val_t(TEXEL_ALIGNMENT))));
  // Level 0 only: the rest is written by 'generateMipLevels()' before
  // anything can read it, and zeroing here is so that a decode failure
  // leaves well-defined zero texels.
  std::memset(mTexels.get(), 0,
              size_t(mNumTexelsX) * size_t(mNumTexelsY) * size_t(mTexelSize));
}

void Image::finishLoad() {
  if (!mFinishLoad) return;
  allocate();
  // NOTE: Move into a local and null the member before invoking, so
  // that a throw cannot leave a stale finish function behind to be
  // invoked again.
  auto finishLoad{std::move(mFinishLoad)};
  mFinishLoad = nullptr;
  try {
    finishLoad();
  } catch (...) {
    // Only level 0 was zeroed by 'allocate()', so zero all of it here: a
    // partially decoded image and an ungenerated chain must still read
    // as well-defined zero texels.
    std::memset(mTexels.get(), 0, mSizeInBytes);
    throw;
  }
  if (mMipLevelsRequested) {
    generateMipLevels();
    mMipLevelsGenerated = true;
  }
}

void Image::abandonLoad() noexcept { mFinishLoad = nullptr; }

void Image::generateMipLevels() noexcept {
  if (mMipFilter == MIP_MAX)
    generateMaxMipLevels();
  else
    generateMeanMipLevels();
}

void Image::generateMeanMipLevels() noexcept {
  const auto dataType{mFormat == UINT8     ? STBIR_TYPE_UINT8
                      : mFormat == UINT16  ? STBIR_TYPE_UINT16
                      : mFormat == FLOAT16 ? STBIR_TYPE_HALF_FLOAT
                                           : STBIR_TYPE_FLOAT};
  // The plain N-channel layouts, so no channel gets alpha semantics:
  // the chain averages stored values as-is (see the class doc comment
  // for why filtering is gamma-agnostic).
  const auto layout{mNumChannels == 1   ? STBIR_1CHANNEL
                    : mNumChannels == 2 ? STBIR_2CHANNEL
                                        : STBIR_4CHANNEL};
  for (int level = 1; level < mNumLevels; level++) {
    // Each level halves the previous one, so the box filter is an
    // area average whenever the parent extent is even, and blends the
    // straddling texels with the correct fractional weights when it
    // is odd. Averaging level to level keeps the whole chain
    // mean-preserving.
    stbir_resize(mTexels.get() + mLevelOffsets[size_t(level - 1)],
                 getNumTexelsX(level - 1), getNumTexelsY(level - 1),
                 getNumTexelsX(level - 1) * mTexelSize,
                 mTexels.get() + mLevelOffsets[size_t(level)],
                 getNumTexelsX(level), getNumTexelsY(level),
                 getNumTexelsX(level) * mTexelSize, layout, dataType,
                 STBIR_EDGE_CLAMP, STBIR_FILTER_BOX);
  }
}

void Image::generateMaxMipLevels() noexcept {
  const auto channelSize{mTexelSize / mNumChannels};
  // Compare in single precision and copy the winning channel's stored
  // bytes, so every format reduces without a pack step.
  auto valueOf{[&](const void *ptr) -> float {
    switch (mFormat) {
    case UINT8:
      return *static_cast<const uint8_t *>(ptr);
    case UINT16:
      return *static_cast<const uint16_t *>(ptr);
    case FLOAT16:
      return unpackHalf(ptr);
    default:
      return *static_cast<const float *>(ptr);
    }
  }};
  auto wrap{[](int i, int n) { return ((i % n) + n) % n; }};
  for (int level = 1; level < mNumLevels; level++) {
    const int prevX{getNumTexelsX(level - 1)};
    const int prevY{getNumTexelsY(level - 1)};
    const int numX{getNumTexelsX(level)};
    const int numY{getNumTexelsY(level)};
    auto prevTexels{mTexels.get() + mLevelOffsets[size_t(level - 1)]};
    auto texels{mTexels.get() + mLevelOffsets[size_t(level)]};
    // Level 1 alone adds the one-texel border around the pair it
    // covers; the higher levels inherit it from their children. The
    // last texel of an odd extent widens to cover the remainder.
    const int border{level == 1 ? 1 : 0};
    for (int j = 0; j < numY; j++) {
      const int y0{2 * j - border};
      const int y1{(j == numY - 1 ? prevY : std::min(2 * j + 2, prevY)) +
                   border};
      for (int i = 0; i < numX; i++) {
        const int x0{2 * i - border};
        const int x1{(i == numX - 1 ? prevX : std::min(2 * i + 2, prevX)) +
                     border};
        const uint8_t *best[4]{};
        float bestValue[4]{};
        for (int y = y0; y < y1; y++) {
          for (int x = x0; x < x1; x++) {
            auto src{prevTexels + size_t(mTexelSize) *
                                      (size_t(wrap(x, prevX)) +
                                       size_t(prevX) * size_t(wrap(y, prevY)))};
            for (int c = 0; c < mNumChannels; c++) {
              auto ptr{reinterpret_cast<const uint8_t *>(src) +
                       size_t(c) * size_t(channelSize)};
              const float value{valueOf(ptr)};
              if (!best[c] || value > bestValue[c]) {
                best[c] = ptr;
                bestValue[c] = value;
              }
            }
          }
        }
        auto dst{reinterpret_cast<uint8_t *>(
            texels +
            size_t(mTexelSize) * (size_t(i) + size_t(numX) * size_t(j)))};
        for (int c = 0; c < mNumChannels; c++)
          std::memcpy(dst + size_t(c) * size_t(channelSize), best[c],
                      size_t(channelSize));
      }
    }
  }
}

void Image::flipVertically() noexcept {
  // The levels in memory, not the levels the extent implies: a chain
  // that was never requested was never allocated either.
  for (int level = 0; level < getNumLevelsInMemory(); level++) {
    auto levelTexels{mTexels.get() + mLevelOffsets[size_t(level)]};
    auto numTexelsY{getNumTexelsY(level)};
    auto rowSize{size_t(mTexelSize) * size_t(getNumTexelsX(level))};
    for (int iY = 0; iY < numTexelsY / 2; iY++) {
      std::swap_ranges(levelTexels + rowSize * size_t(iY),
                       levelTexels + rowSize * size_t(iY + 1),
                       levelTexels + rowSize * size_t(numTexelsY - iY - 1));
    }
  }
}

float4 Image::fetch(int x, int y, int level) const noexcept {
  SMDL_SANITY_CHECK(mTexels != nullptr);
  SMDL_SANITY_CHECK(0 <= level && level < getNumLevels());
  SMDL_SANITY_CHECK(0 <= x && x < getNumTexelsX(level));
  SMDL_SANITY_CHECK(0 <= y && y < getNumTexelsY(level));
  auto texel{float4{std::numeric_limits<float>::quiet_NaN(),
                    std::numeric_limits<float>::quiet_NaN(),
                    std::numeric_limits<float>::quiet_NaN(),
                    std::numeric_limits<float>::quiet_NaN()}};
  auto texelPtr{mTexels.get() + mLevelOffsets[size_t(level)] +
                size_t(mTexelSize) *
                    (size_t(x) + size_t(getNumTexelsX(level)) * size_t(y))};
  for (int i = 0; i < mNumChannels; i++) {
    switch (mFormat) {
    case UINT8:
      texel[i] = *reinterpret_cast<const uint8_t *>(texelPtr) / 255.0f;
      texelPtr += 1;
      break;
    case UINT16:
      texel[i] = *reinterpret_cast<const uint16_t *>(texelPtr) / 65535.0f;
      texelPtr += 2;
      break;
    case FLOAT16:
      texel[i] = unpackHalf(texelPtr);
      texelPtr += 2;
      break;
    case FLOAT32:
      texel[i] = *reinterpret_cast<const float *>(texelPtr);
      texelPtr += 4;
      break;
    default:
      SMDL_SANITY_CHECK_MSG(false, "Unexpected texel format!");
      break;
    }
  }
  return texel;
}

std::optional<Error> write8bitImage(const std::string &fileName, int numTexelsX,
                                    int numTexelsY, int numChannels,
                                    const void *ptr) {
  SMDL_SANITY_CHECK(0 < numTexelsX);
  SMDL_SANITY_CHECK(0 < numTexelsY);
  SMDL_SANITY_CHECK(0 < numChannels && numChannels <= 4);
  SMDL_SANITY_CHECK(ptr);
  int result{};
  if (hasExtension(fileName, ".png")) {
    result = stbi_write_png(fileName.c_str(), numTexelsX, numTexelsY,
                            numChannels, ptr, 0);
  } else if (hasExtension(fileName, ".jpeg") ||
             hasExtension(fileName, ".jpg")) {
    result = stbi_write_jpg(fileName.c_str(), numTexelsX, numTexelsY,
                            numChannels, ptr, 90);
  } else if (hasExtension(fileName, ".bmp")) {
    result = stbi_write_bmp(fileName.c_str(), numTexelsX, numTexelsY,
                            numChannels, ptr);
  } else if (hasExtension(fileName, ".tga")) {
    result = stbi_write_tga(fileName.c_str(), numTexelsX, numTexelsY,
                            numChannels, ptr);
  } else if (hasExtension(fileName, ".pgm") || hasExtension(fileName, ".ppm")) {
    return catchAndReturnError([&] {
      auto isGray{hasExtension(fileName, ".pgm")};
      auto stream{openOrThrow(fileName, std::ios::binary | std::ios::out)};
      stream << (isGray ? "P5 " : "P6 ");
      stream << numTexelsX << ' ';
      stream << numTexelsY << " 255\n";
      auto texelPtr{static_cast<const char *>(ptr)};
      auto texelPtrEnd{texelPtr + size_t(numChannels) * size_t(numTexelsX) *
                                      size_t(numTexelsY)};
      if (isGray) {
        for (; texelPtr < texelPtrEnd; texelPtr += numChannels) {
          stream.write(texelPtr, 1);
        }
      } else {
        auto effNumChannels{std::min(numChannels, 3)};
        for (; texelPtr < texelPtrEnd; texelPtr += numChannels) {
          stream.write(texelPtr, effNumChannels);
          stream.write("\0\0\0", 3 - effNumChannels);
        }
      }
    });
  } else {
    return Error(concat("cannot write ", QuotedPath(fileName),
                        ": unrecognized extension"));
  }
  if (result == 0) {
    return Error(concat("cannot write ", QuotedPath(fileName)));
  }
  return std::nullopt;
}

std::optional<Error> writeFloatImage(const std::string &fileName,
                                     int numTexelsX, int numTexelsY,
                                     int numChannels, const float *ptr) {
  SMDL_SANITY_CHECK(0 < numTexelsX);
  SMDL_SANITY_CHECK(0 < numTexelsY);
  SMDL_SANITY_CHECK(numChannels == 1 || numChannels == 3 || numChannels == 4);
  SMDL_SANITY_CHECK(ptr);
  if (hasExtension(fileName, ".exr")) {
    const char *message{};
    if (SaveEXR(ptr, numTexelsX, numTexelsY, numChannels, /*save_as_fp16=*/0,
                fileName.c_str(), &message) != TINYEXR_SUCCESS) {
      auto error{Error(concat("cannot write ", QuotedPath(fileName), ": ",
                              message ? message : "unknown error"))};
      FreeEXRErrorMessage(message);
      return error;
    }
    return std::nullopt;
  } else if (hasExtension(fileName, ".hdr")) {
    if (stbi_write_hdr(fileName.c_str(), numTexelsX, numTexelsY, numChannels,
                       ptr) == 0) {
      return Error(concat("cannot write ", QuotedPath(fileName)));
    }
    return std::nullopt;
  } else {
    return Error(concat("cannot write ", QuotedPath(fileName),
                        ": unrecognized extension"));
  }
}

} // namespace smdl
