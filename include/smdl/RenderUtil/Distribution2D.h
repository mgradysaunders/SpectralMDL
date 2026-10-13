/// \file
#pragma once

#include <vector>

#include "smdl/Export.h"
#include "smdl/RenderUtil/Distribution1D.h"
#include "smdl/Support/Span.h"
#include "smdl/Support/VectorMath.h"

namespace smdl {

/// \addtogroup renderutil
/// \{

/// A data-driven distribution in 2 dimensions.
class SMDL_EXPORT Distribution2D final {
public:
  /// Default constructor.
  Distribution2D() = default;

  /// Constructor.
  ///
  /// \param[in] numTexelsX  The number of texels in X.
  /// \param[in] numTexelsY  The number of texels in Y.
  /// \param[in] values      The values in row-major order.
  ///
  explicit Distribution2D(int numTexelsX, int numTexelsY,
                          Span<const float> values);

public:
  /// Clear.
  void clear() noexcept {
    numTexelsX_ = 0;
    numTexelsY_ = 0;
    conditionals_.clear();
    marginal_.clear();
  }

  /// The number of pixels in X.
  [[nodiscard]] int getNumTexelsX() const noexcept { return numTexelsX_; }

  /// The number of pixels in Y.
  [[nodiscard]] int getNumTexelsY() const noexcept { return numTexelsY_; }

  /// The unnormalized sum over all values.
  [[nodiscard]] float unnormalizedSum() const noexcept {
    return marginal_.unnormalizedSum();
  }

  /// The pixel probability mass function (PMF).
  [[nodiscard]] float pixelPMF(int2 i) const noexcept {
    if (0 <= i.y && i.y < numTexelsY_)
      return marginal_.indexPMF(i.y) * conditionals_[i.y].indexPMF(i.x);
    return 0.0f;
  }

  /// The pixel sampling routine.
  ///
  /// \param[in]  xi       The random sample \f$ \xi \in (0,1)^2 \f$.
  /// \param[out] xiRemap  If non-null, receives the remapped random sample.
  /// \param[out] pmf      If non-null, receives the associated PMF.
  ///
  [[nodiscard]] int2 pixelDraw(float2 xi, float2 *xiRemap = {},
                               float *pmf = {}) const noexcept;

  /// The direction PDF.
  ///
  /// \param[in]  wi      The incident direction \f$ \omega_i \f$.
  /// \param[out] iPixel  If non-null, receives the associated pixel index.
  ///
  [[nodiscard]] float directionPDF(float3 wi, int2 *iPixel = {}) const noexcept;

  /// The direction sampling routine.
  ///
  /// The pixel and the density both come back from `directionPDF()` of
  /// the direction returned, rather than from the pixel the draw picked.
  /// The two disagree whenever rounding puts the direction across a pixel
  /// boundary from the rectangle it was drawn in, and where the
  /// neighboring pixel is much darker the density reported that way is
  /// wrong by that whole ratio, which multiple importance sampling then
  /// divides by. Reporting the density of the direction in hand costs one
  /// more arccosine and arctangent and makes the two exactly agree.
  ///
  /// A direction whose recovered density is zero comes back with a pdf of
  /// zero and a pixel index of `(-1, -1)`, which callers already have to
  /// handle: it means this direction cannot be drawn, so the sample
  /// carries nothing.
  ///
  /// \param[in]  xi      The random sample \f$ \xi \in (0,1)^2 \f$.
  /// \param[out] iPixel  If non-null, receives the associated pixel index.
  /// \param[out] pdf     If non-null, receives the associated PDF.
  ///
  [[nodiscard]] float3 directionDraw(float2 xi, int2 *iPixel = {},
                                     float *pdf = {}) const noexcept;

private:
  int numTexelsX_{};
  int numTexelsY_{};
  std::vector<Distribution1D> conditionals_{};
  Distribution1D marginal_{};
};

/// \}

} // namespace smdl
