#if defined(_WIN32)
#ifndef NOMINMAX
#define NOMINMAX
#endif
#endif

#include "smdl/Image.h"

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-function"
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

extern "C" {
#define STBI_ASSERT(X) ((void)0)
#define STBI_MALLOC(sz) ::smdl::Image::Image_malloc(sz)
#define STBI_REALLOC(p, newsz) ::smdl::Image::Image_realloc(p, newsz)
#define STBI_FREE(p) ::smdl::Image::Image_free(p)
#define STB_ONLY_JPEG 1
#define STB_ONLY_PNG 1
#define STB_ONLY_TGA 1
#define STB_ONLY_BMP 1
#define STB_ONLY_PNM 1
#define STB_ONLY_HDR 1
#define STB_IMAGE_STATIC 1
#define STB_IMAGE_IMPLEMENTATION 1
#include "thirdparty/stb_image.h"

#define STBIW_ASSERT(X) ((void)0)
#define STBIW_MALLOC(sz) ::smdl::Image::Image_malloc(sz)
#define STBIW_REALLOC(p, newsz) ::smdl::Image::Image_realloc(p, newsz)
#define STBIW_FREE(p) ::smdl::Image::Image_free(p)
#define STB_IMAGE_WRITE_STATIC 1
#define STB_IMAGE_WRITE_IMPLEMENTATION 1
#include "thirdparty/stb_image_write.h"
} // extern "C"

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic pop
#endif

#define TINYEXR_MALLOC(sz) ::smdl::Image::Image_malloc(sz)
#define TINYEXR_CALLOC(n, sz) ::smdl::Image::Image_calloc(n, sz)
#define TINYEXR_FREE(p) ::smdl::Image::Image_free(p)
#define TINYEXR_USE_MINIZ 1
#define TINYEXR_USE_STB_ZLIB 1
#define TINYEXR_USE_THREAD 0
#define TINYEXR_IMPLEMENTATION 1
#include "thirdparty/tinyexr.h"

namespace tinyexr {

[[nodiscard]] static const EXRChannelInfo *FindChannel(const EXRHeader &header,
                                                       std::string_view name) {
  for (int iC = 0; iC < header.num_channels; iC++)
    if (header.channels[iC].name == name)
      return &header.channels[iC];
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

Image::Image_malloc_t Image::Image_malloc = &std::malloc;

Image::Image_calloc_t Image::Image_calloc = &std::calloc;

Image::Image_realloc_t Image::Image_realloc = &std::realloc;

Image::Image_free_t Image::Image_free = &std::free;

void Image::clear() {
  format = U8;
  numChannels = 1;
  numTexelsX = 0;
  numTexelsY = 0;
  texelSize = 1;
  texels.reset();
  finishLoad = nullptr;
}

std::optional<Error> Image::start_load(const std::string &fileName) noexcept {
  clear();
  auto error{catch_and_return_error([&] {
    if (stbi_info(fileName.c_str(), &numTexelsX, &numTexelsY, &numChannels)) {
      // If the number of channels is 3, i.e., RGB, round it up to 4
      // so all of our alignment assumptions work.
      if (numChannels == 3)
        numChannels = 4;
      // Determine whether we should load 32-bit float, 16-bit unsigned int, or
      // 8-bit unsigned int.
      if (stbi_is_hdr(fileName.c_str())) {
        format = F32;
        texelSize = 4 * numChannels;
      } else if (stbi_is_16_bit(fileName.c_str())) {
        format = U16;
        texelSize = 2 * numChannels;
      } else {
        format = U8;
        texelSize = 1 * numChannels;
      }
      // Pre-allocate the texels.
      texels.reset(new std::byte[size_t(numTexelsX) * size_t(numTexelsY) *
                                 size_t(texelSize)]);
      // Defer the actual load until later!
      finishLoad = [this, fileName]() {
        stbi_set_flip_vertically_on_load(1);
        int nTexelsX{};
        int nTexelsY{};
        int nChannels{};
        void *ptr{};
        switch (format) {
        default:
        case Format::U8:
          // Load 8-bit unsigned int.
          ptr = stbi_load(fileName.c_str(), &nTexelsX, &nTexelsY, &nChannels,
                          numChannels);
          break;
        case Format::U16:
          // Load 16-bit unsigned int.
          ptr = stbi_load_16(fileName.c_str(), &nTexelsX, &nTexelsY, &nChannels,
                             numChannels);
          break;
        case Format::F32:
          // Load 32-bit float.
          ptr = stbi_loadf(fileName.c_str(), &nTexelsX, &nTexelsY, &nChannels,
                           numChannels);
          break;
        }
        if (!ptr)
          throw std::runtime_error(std::string("stb image failure: ") +
                                   stbi_failure_reason());
        // Copy into the pre-allocated texel buffer, then free the pointer.
        SMDL_SANITY_CHECK(texels != nullptr);
        SMDL_SANITY_CHECK(numTexelsX == nTexelsX);
        SMDL_SANITY_CHECK(numTexelsY == nTexelsY);
        std::memcpy(texels.get(), ptr,
                    size_t(numTexelsX) * size_t(numTexelsY) *
                        size_t(texelSize));
        stbi_image_free(ptr);
      };
    } else if (EXRVersion version{};
               ParseEXRVersionFromFile(&version, fileName.c_str()) ==
               TINYEXR_SUCCESS) {
      // Fail if deep or multipart!
      if (version.non_image || version.multipart)
        throw std::runtime_error("deep or multipart EXR is not supported");
      // Parse the header.
      EXRHeader header{};
      InitEXRHeader(&header);
      const char *err{};
      if (ParseEXRHeaderFromFile(&header, &version, fileName.c_str(), &err) !=
          TINYEXR_SUCCESS) {
        FreeEXRErrorMessage(err);
        throw std::runtime_error("cannot parse EXR header");
      }
      int nTexelsX{header.data_window.max_x - header.data_window.min_x + 1};
      int nTexelsY{header.data_window.max_y - header.data_window.min_y + 1};
      if (nTexelsX < 0 || nTexelsY < 0)
        throw std::runtime_error(
            "cannot parse EXR header: invalid data window");

      auto setFormatFromPixelType{[&](int pixelType) {
        if (pixelType == TINYEXR_PIXELTYPE_UINT)
          throw std::runtime_error("uint EXR is not supported");
        else if (pixelType == TINYEXR_PIXELTYPE_HALF)
          format = F16, texelSize = 2 * numChannels;
        else if (pixelType == TINYEXR_PIXELTYPE_FLOAT)
          format = F32, texelSize = 4 * numChannels;
        else
          SMDL_SANITY_CHECK(false, "unknown EXR pixel type");
      }};
      if (header.num_channels == 1) {
        // 1-channel R
        numChannels = 1;
        setFormatFromPixelType(header.channels[0].pixel_type);
      } else {
        // 4-channel RGBA
        numChannels = 4;
        const EXRChannelInfo *channels[4] = {
            tinyexr::FindChannel(header, "R"), //
            tinyexr::FindChannel(header, "G"),
            tinyexr::FindChannel(header, "B"),
            tinyexr::FindChannel(header, "A")};
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
      numTexelsX = nTexelsX;
      numTexelsY = nTexelsY;
      texels.reset(new std::byte[size_t(numTexelsX) * size_t(numTexelsY) *
                                 size_t(texelSize)]);
      finishLoad = [this, fileName, header]() {
        auto headerDtor{Defer(
            [&header]() { FreeEXRHeader(const_cast<EXRHeader *>(&header)); })};
        EXRImage image{};
        InitEXRImage(&image);
        const char *err{};
        if (LoadEXRImageFromFile(&image, &header, fileName.c_str(), &err) !=
            TINYEXR_SUCCESS) {
          auto message{std::string("tinyexr failed: ") + err};
          FreeEXRErrorMessage(err);
          throw std::runtime_error(message);
        }
        auto imageDtor{Defer([&image]() { FreeEXRImage(&image); })};
        SMDL_SANITY_CHECK(numTexelsX == image.width);
        SMDL_SANITY_CHECK(numTexelsY == image.height);
        if (numChannels == 1) {
          // 1-channel R
          tinyexr::ForEachPixel(
              header, image,
              [&](int iX, int iY, int iC, const void *pixel, size_t pixelSize) {
                iY = numTexelsY - iY - 1; // Flip vertically!
                if (iC == 0)
                  std::memcpy(texels.get() +
                                  ptrdiff_t((iX + numTexelsX * iY) * texelSize),
                              pixel, pixelSize);
              });
        } else {
          // 4-channel RGBA
          const EXRChannelInfo *channels[4] = {
              tinyexr::FindChannel(header, "R"),
              tinyexr::FindChannel(header, "G"),
              tinyexr::FindChannel(header, "B"),
              tinyexr::FindChannel(header, "A")};
          int channelIndexR =
              int(channels[0] - &header.channels[0]); // Required!
          int channelIndexG =
              int(channels[1] - &header.channels[0]); // Required!
          int channelIndexB =
              int(channels[2] - &header.channels[0]); // Required!
          int channelIndexA =
              channels[3] ? int(channels[3] - &header.channels[0]) : -1;
          tinyexr::ForEachPixel(
              header, image,
              [&](int iX, int iY, int iC, const void *pixel, size_t pixelSize) {
                iY = numTexelsY - iY - 1; // Flip vertically!
                auto texel{texels.get() +
                           ptrdiff_t((iX + numTexelsX * iY) * texelSize)};
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
            if (format == F16) {
              auto one{uint16_t(0x3C00)};
              auto itr{texels.get() + 6};
              for (int i = 0; i < numTexelsY * numTexelsY;
                   i++, itr += texelSize)
                std::memcpy(itr, &one, 2);
            } else if (format == F32) {
              auto one{float(1.0f)};
              auto itr{texels.get() + 12};
              for (int i = 0; i < numTexelsY * numTexelsY;
                   i++, itr += texelSize)
                std::memcpy(itr, &one, 4);
            } else {
              SMDL_SANITY_CHECK(false, "format must be F16 or F32 by now!");
            }
          }
        }
      };
    }
  })};
  if (error) {
    clear();
    error->message =
        concat("cannot load ", quoted_path(fileName), ": ", error->message);
  }
  return error;
}

void Image::finish_load() {
  if (finishLoad) {
    finishLoad();
    finishLoad = nullptr;
  }
}

[[nodiscard]] static float unpack_half(const void *ptr) noexcept {
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
      while (!(mantissa & 0x0400))
        exponent--, mantissa <<= 1;
      f |= (exponent << 23) | ((mantissa & ~0x0400) << 13); // Subnormal
    }
  } else {
    f |= mantissa << 13;
    f |= exponent == 31 ? 0x7F800000 : ((exponent + 112) << 23);
  }
  return std::bit_cast<float>(f);
#endif // #if __clang__
}

float4 Image::fetch(int x, int y) const noexcept {
  SMDL_SANITY_CHECK(texels != nullptr);
  SMDL_SANITY_CHECK(0 <= x && x < numTexelsX);
  SMDL_SANITY_CHECK(0 <= y && y < numTexelsY);
  auto texel{float4{std::numeric_limits<float>::quiet_NaN(),
                    std::numeric_limits<float>::quiet_NaN(),
                    std::numeric_limits<float>::quiet_NaN(),
                    std::numeric_limits<float>::quiet_NaN()}};
  auto texelPtr{&texels[texelSize * (x + numTexelsX * y)]};
  for (int i = 0; i < numChannels; i++) {
    switch (format) {
    case U8:
      texel[i] = *reinterpret_cast<const uint8_t *>(texelPtr) / 255.0f;
      texelPtr += 1;
      break;
    case U16:
      texel[i] = *reinterpret_cast<const uint16_t *>(texelPtr) / 65535.0f;
      texelPtr += 2;
      break;
    case F16:
      texel[i] = unpack_half(texelPtr);
      texelPtr += 2;
      break;
    case F32:
      texel[i] = *reinterpret_cast<const float *>(texelPtr);
      texelPtr += 4;
      break;
    default:
      SMDL_SANITY_CHECK(false, "Unexpected texel format!");
      break;
    }
  }
  return texel;
}

} // namespace smdl
