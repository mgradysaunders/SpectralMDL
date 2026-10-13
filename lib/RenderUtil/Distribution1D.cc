#include "smdl/RenderUtil/Distribution1D.h"

#include <algorithm>
#include <cmath>

#include "smdl/RenderUtil/MonteCarlo.h"
#include "smdl/Support/Macros.h"

namespace smdl {

namespace {

// The CMF is stored as a fixed-point fraction of the unit interval, in
// units of 2^-32. Fixed point costs four bytes an entry like a `float`
// would, so a large table stays half the size the equivalent `double`
// one would be and has a real chance of fitting in cache. Unlike a
// `float`, though, the difference of two neighboring entries is an exact
// integer subtraction, and that difference is the PMF: in floating point
// it cancels away most of the mantissa as soon as the entries are close
// together, which is precisely what happens as a distribution grows. At
// a million entries the float form loses over a tenth of the PMF, while
// this form is still good to a part in ten thousand.
constexpr double CMF_SCALE = 4294967296.0; // 2^32

// Quantize a CMF value in [0, 1]. Truncating keeps a nondecreasing
// sequence nondecreasing; the top of the range saturates because 1.0
// scales to exactly one past the largest representable value.
[[nodiscard]]
SMDL_ALWAYS_INLINE uint32_t quantizeCMF(double cmf) noexcept {
  return uint32_t(std::min(CMF_SCALE * cmf, 4294967295.0));
}

} // namespace

Distribution1D::Distribution1D(Span<const float> values) {
  // Accumulate and normalize in double, then quantize once, so that each
  // stored entry carries the rounding of a single conversion rather than
  // the accumulated drift of a running fixed-point sum.
  std::vector<double> sums{};
  sums.reserve(values.size() + 1);
  sums.emplace_back(0.0);
  for (const auto &value : values) {
    // Deliberately `fmax`: the values come from files (image texels, IES
    // profiles, BSDF tables), and this is the one place a NaN among them
    // is dropped rather than left to poison the whole table.
    totalSum_ += std::fmax(static_cast<double>(value), 0.0);
    sums.emplace_back(totalSum_);
  }
  cmfs_.resize(sums.size());
  // An all-zero distribution stays all-zero, so every PMF is zero rather
  // than NaN. This arises legitimately, e.g., as a conditional row of a
  // `Distribution2D` over a region with no density.
  if (totalSum_ > 0) {
    for (size_t i = 0; i < sums.size(); i++)
      cmfs_[i] = quantizeCMF(sums[i] / totalSum_);
  }
}

int Distribution1D::indexDraw(float xi, float *xiRemap,
                              float *pmf) const noexcept {
  if (cmfs_.size() < 2) {
    if (pmf) *pmf = 1;
    return 0;
  }
  const uint32_t key{quantizeCMF(std::clamp(double(xi), 0.0, 1.0))};
  auto itr{std::lower_bound(cmfs_.begin(), cmfs_.end(), key)};
  if (itr == cmfs_.begin()) ++itr;
  if (itr == cmfs_.end()) --itr;
  --itr;
  // A key of zero stops the search at the first entry whatever its mass,
  // so step past the indexes with none.
  while (itr + 2 < cmfs_.end() && itr[0] == itr[1]) ++itr;
  int i{int(itr - cmfs_.begin())};
  uint32_t cmf0{*itr++};
  uint32_t cmf1{*itr};
  // Nondecreasing entries, so this cannot wrap.
  const uint32_t width{cmf1 - cmf0};
  if (xiRemap) {
    // Against the dequantized bounds rather than against `key`, so the
    // remapped sample keeps the resolution of the incoming float instead
    // of inheriting that of the table.
    const double bound0{INV_CMF_SCALE * double(cmf0)};
    const double bound1{INV_CMF_SCALE * double(cmf1)};
    // Zero width means the entry cannot be sampled at all, which only
    // arises from an all-zero distribution; the remap has nowhere to
    // land, so give it the bottom of the interval rather than a
    // division by zero.
    *xiRemap = canonicalize(
        width > 0 ? float((double(xi) - bound0) / (bound1 - bound0)) : 0.0f);
  }
  if (pmf) *pmf = float(INV_CMF_SCALE * double(width));
  return i;
}

} // namespace smdl
