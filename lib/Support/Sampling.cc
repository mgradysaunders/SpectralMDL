#include "smdl/Support/Sampling.h"

namespace smdl {

std::pair<int, float> Distribution1D::index_sample(float u,
                                                   float *uRemap) const {
  if (cmfs.size() < 2)
    return {0, 1.0f};
  auto itr{std::lower_bound(cmfs.begin(), cmfs.end(), double(u))};
  if (itr == cmfs.begin())
    ++itr;
  if (itr == cmfs.end())
    --itr;
  --itr;
  auto i{int(itr - cmfs.begin())};
  auto cmf0{*itr++};
  auto cmf1{*itr};
  auto pmf{cmf1 - cmf0};
  if (uRemap) {
    *uRemap = float(double(u) - cmf0) / pmf;
    *uRemap = std::fmax(*uRemap, std::numeric_limits<float>::denorm_min());
    *uRemap = std::fmin(*uRemap, 1 - std::numeric_limits<float>::epsilon() / 2);
  }
  return {i, float(pmf)};
}

} // namespace smdl
