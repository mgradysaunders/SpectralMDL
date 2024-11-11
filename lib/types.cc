#include "smdl/types.h"

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

} // namespace smdl
