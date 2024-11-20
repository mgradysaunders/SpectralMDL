#pragma once

#include "smdl/type.h"

namespace smdl {

struct SMDL_EXPORT Image final {
  void flip_horizontal();

  void flip_vertical();

  [[nodiscard]] auto &get_texel(int2_t i) { return texels[i.y * extent.x + i.x]; }

  [[nodiscard]] auto &get_texel(int2_t i) const { return texels[i.y * extent.x + i.x]; }

  int2_t extent{};

  std::vector<float4_t> texels{};
};

[[nodiscard]] SMDL_EXPORT Image load_image(const std::filesystem::path &fname);

SMDL_EXPORT void save_image(const std::filesystem::path &fname, const Image &image);

} // namespace smdl
