#include "smdl/type.h"

#if WITH_PTEX
#include "Ptexture.h"
#endif // #if WITH_PTEX

namespace smdl {

extern "C" {

SMDL_EXPORT void smdl_ptex_eval(const state_t &state, texture_ptex_t &tex, int_t first, int_t num, float_t *result) {
  std::fill(result, result + num, 0.0f);
#if WITH_PTEX
  auto ptexture{static_cast<Ptexture_t *>(tex.ptr)};
  if (!ptexture || !ptexture->filter)
    return;
  int_t num_channels = static_cast<PtexTexture *>(ptexture->texture)->numChannels() - num;
  num = std::min(num, num_channels);
  if (num <= 0)
    return;
  static_cast<PtexFilter *>(ptexture->filter)
      ->eval(
          result, first, num, state.ptex_face_id, state.ptex_face_uv.x, state.ptex_face_uv.y,
          /*uw1=*/0.0f, /*vw1=*/0.0f,
          /*uw2=*/0.0f, /*vw2=*/0.0f,
          /*width=*/1.0f, /*blur=*/0.0f);
#endif
}

} // extern "C"

} // namespace smdl
