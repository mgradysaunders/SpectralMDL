#if defined(_WIN32)
#ifndef NOMINMAX
#define NOMINMAX
#endif
#endif

#include "smdl/ImageLoader.h"

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

Image default_image_loader(const std::filesystem::path &fname) {
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
  }
  throw Error(std::format("unrecognized image file extension '{}'", fnameExtStr));
  return {};
}

} // namespace smdl
