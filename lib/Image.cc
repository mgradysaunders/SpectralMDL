#if defined(_WIN32)
#ifndef NOMINMAX
#define NOMINMAX
#endif
#endif

#include "smdl/Image.h"

#define STBI_ASSERT(X) ((void)0)
#define STB_ONLY_JPEG 1
#define STB_ONLY_PNG 1
#define STB_ONLY_HDR 1
#define STB_IMAGE_STATIC 1
#define STB_IMAGE_IMPLEMENTATION 1
#include "stb_image.h"

#define STBIW_ASSERT(X) ((void)0)
#define STB_IMAGE_WRITE_STATIC 1
#define STB_IMAGE_WRITE_IMPLEMENTATION 1
#include "stb_image_write.h"

#define TINYEXR_USE_MINIZ 0
#define TINYEXR_USE_STB_ZLIB 1
#define TINYEXR_IMPLEMENTATION 1
#include "tinyexr.h"

namespace smdl {

void Image::flip_horizontal() {
  for (int_t y = 0; y < extent.y; y++) {
    auto *row = texels.data() + y * extent.x;
    for (int_t x = 0; x < extent.x / 2; x++) {
      std::swap(row[x], row[extent.x - 1 - x]);
    }
  }
}

void Image::flip_vertical() {
  for (int_t y = 0; y < extent.y / 2; y++) {
    auto *row0 = texels.data() + y * extent.x;
    auto *row1 = texels.data() + (extent.y - 1 - y) * extent.x;
    for (int_t x = 0; x < extent.x; x++) {
      std::swap(row0[x], row1[x]);
    }
  }
}

Image load_image(const std::filesystem::path &fname) {
  auto fnameStr{fname.string()};
  auto fnameExtStr{fname.extension().string()};
  std::transform(fnameExtStr.begin(), fnameExtStr.end(), fnameExtStr.begin(), [](char c) {
    return std::tolower(static_cast<unsigned char>(c));
  });
  if (fnameExtStr == ".jpeg" || fnameExtStr == ".jpg" || fnameExtStr == ".png") {
    int x{};
    int y{};
    int numChannels{};
    if (float *texels{stbi_loadf(fnameStr.c_str(), &x, &y, &numChannels, 4)}) {
      Image image{};
      image.extent.x = x;
      image.extent.y = y;
      image.texels.resize(x * y);
      std::memcpy(image.texels.data(), texels, sizeof(float4_t) * x * y);
      stbi_image_free(texels);
      return image;
    } else {
      throw Error(std::format("stb image failure: '{}'", stbi_failure_reason()));
    }
  } else if (fnameExtStr == ".exr") {
    float *texels{};
    int x{};
    int y{};
    const char *error{};
    if (LoadEXR(&texels, &x, &y, fnameStr.c_str(), &error) < 0) {
      std::string message{std::format("tinyexr failure: {}", error)};
      FreeEXRErrorMessage(error);
      free(texels);
      throw Error(std::move(message));
    }
    Image image{};
    image.extent.x = x;
    image.extent.y = y;
    image.texels.resize(x * y);
    std::memcpy(image.texels.data(), texels, sizeof(float4_t) * x * y);
    free(texels);
    return image;
  }
  throw Error(std::format("unrecognized image file extension '{}'", fnameExtStr));
  return {};
}

void save_image(const std::filesystem::path &fname, const Image &image) {
  auto fnameStr{fname.string()};
  auto fnameExtStr{fname.extension().string()};
  std::transform(fnameExtStr.begin(), fnameExtStr.end(), fnameExtStr.begin(), [](char c) {
    return std::tolower(static_cast<unsigned char>(c));
  });
  if (fnameExtStr == ".jpeg" || fnameExtStr == ".jpg" || fnameExtStr == ".png") {
    auto texels{std::vector<uint8_t>{}};
    texels.reserve(4 * image.extent.x * image.extent.y);
    for (auto &texel : image.texels) {
      texels.push_back(static_cast<uint8_t>(std::fmax(0.0f, std::fmin(255.0f * texel.x, 255.0f))));
      texels.push_back(static_cast<uint8_t>(std::fmax(0.0f, std::fmin(255.0f * texel.y, 255.0f))));
      texels.push_back(static_cast<uint8_t>(std::fmax(0.0f, std::fmin(255.0f * texel.z, 255.0f))));
      texels.push_back(static_cast<uint8_t>(std::fmax(0.0f, std::fmin(255.0f * texel.w, 255.0f))));
    }
    int result = 0;
    if (fnameExtStr == ".jpeg" || fnameExtStr == ".jpg") {
      result = stbi_write_jpg(fnameStr.c_str(), image.extent.x, image.extent.y, 4, texels.data(), 90);
    } else {
      result = stbi_write_png(fnameStr.c_str(), image.extent.x, image.extent.y, 4, texels.data(), 0);
    }
    if (!result)
      throw Error("stb image write failure");
  } else if (fnameExtStr == ".exr") {
    const char *error{};
    if (SaveEXR(
            reinterpret_cast<const float *>(image.texels.data()), image.extent.x, image.extent.y, 4, /*save_as_fp16=*/true,
            fnameStr.c_str(), &error) < 0) {
      std::string message{std::format("tinyexr failure: {}", error)};
      FreeEXRErrorMessage(error);
      throw Error(std::move(message));
    }
  } else {
    throw Error(std::format("unrecognized image file extension '{}'", fnameExtStr));
  }
}

} // namespace smdl
