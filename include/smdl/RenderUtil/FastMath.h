/// \file
#pragma once

#include <algorithm>
#include <cmath>
#include <cstdint>
#include <cstring>
#include <limits>

namespace smdl {

/// \addtogroup renderutil
/// \{

/// \name Functions (fast math)
///
/// Inline polynomial approximations of the transcendentals the rendering
/// utilities evaluate per band or per sample. Each compiles to
/// straight-line arithmetic that inlines and vectorizes, and each carries
/// the error bound its documentation states, which the doctests pin.
///
/// Where they pay: in a loop over independent values, the per-band shape,
/// they run several times faster than libm, and they beat `acosf` in any
/// shape. In a dependent scalar chain they do not beat glibc's `expf` and
/// `logf`, whose latency is about the same, so swapping those out buys
/// nothing.
///
/// What they owe the caller: a finite argument in the stated domain. There
/// are no domain guards, so `fastLog` of zero, a negative, or an infinity,
/// and a NaN passed to any of them, are unspecified.
///
/// \{

/// The exponential \f$ e^x \f$ to 3e-7 relative (about two ulp) wherever
/// the result is a normal float, exactly 1 at 0. Saturates outside that
/// range: below -87.33, where the true value is denormal, the result is
/// 0; above 88.37, where the power of two the result needs is not
/// representable, the result is infinity.
///
/// The argument is reduced to an integer power of two and a remainder
/// within half a ln2 of zero, the remainder taken as the degree-6 Taylor
/// series and the power built by assembling the exponent field. The
/// reduction subtracts ln2 in two pieces (Cody-Waite), because the
/// one-piece form loses a digit per decade of the argument.
[[nodiscard]] inline float fastExp(float x) noexcept {
  constexpr float X_MIN = -87.33f;
  constexpr float X_MAX = 88.72f;
  const float xc{std::clamp(x, X_MIN, X_MAX)};
  const float n{std::floor(xc * 1.4426950408889634f + 0.5f)};
  const float u{(xc - n * 0.693145751953125f) - n * 1.42860682030941723e-6f};
  const float p{
      1.0f +
      u * (1.0f +
           u * (0.5f + u * (0.16666667f +
                            u * (0.041666668f +
                                 u * (0.008333334f + u * 0.0013888889f)))))};
  const std::uint32_t bits{std::uint32_t(int(n) + 127) << 23};
  float s{};
  std::memcpy(&s, &bits, sizeof(s));
  return x < X_MIN ? 0.0f : p * s;
}

/// The exponential \f$ e^x \f$ to 1e-8 relative wherever the result is a
/// normal double, exactly 1 at 0; 0 below -708.39 and infinity above
/// 709.44, on the same grounds as the float overload. The one-piece
/// reduction is enough here: the rounding of `n ln2` at n = 1024 is 1e-13
/// relative, far under the degree-7 series' own error.
[[nodiscard]] inline double fastExp(double x) noexcept {
  constexpr double X_MIN = -708.39;
  constexpr double X_MAX = 709.78;
  const double xc{std::clamp(x, X_MIN, X_MAX)};
  const double n{std::floor(xc * 1.4426950408889634 + 0.5)};
  const double u{xc - n * 0.6931471805599453};
  const double p{
      1.0 +
      u * (1.0 +
           u * (0.5 + u * (0.16666666666666666 +
                           u * (0.041666666666666664 +
                                u * (0.008333333333333333 +
                                     u * (0.001388888888888889 +
                                          u * 0.0001984126984126984))))))};
  const std::uint64_t bits{std::uint64_t(int(n) + 1023) << 52};
  double s{};
  std::memcpy(&s, &bits, sizeof(s));
  return x < X_MIN ? 0.0 : p * s;
}

/// The natural logarithm of a positive finite float, denormals included,
/// to 3e-7 relative to \f$ \ln x \f$ away from 1 and 1e-7 absolute within
/// [0.5, 2], exactly 0 at 1.
///
/// The exponent comes off the bit pattern and the mantissa, folded into
/// [2/3, 4/3), goes through the odd series in (m - 1) / (m + 1), cut at
/// the term that lands below float rounding of the result. A denormal is
/// first scaled into the normal range, exactly, so that its exponent
/// reads correctly too.
[[nodiscard]] inline float fastLog(float x) noexcept {
  const bool denormal{x < std::numeric_limits<float>::min()};
  x = denormal ? x * 8388608.0f : x;
  std::uint32_t bits{};
  std::memcpy(&bits, &x, sizeof(bits));
  int e{int((bits >> 23) & 0xFF) - 127 - (denormal ? 23 : 0)};
  bits = (bits & 0x807FFFFFu) | (127u << 23);
  float m{};
  std::memcpy(&m, &bits, sizeof(m));
  if (m > 1.3333333f) {
    m *= 0.5f;
    e += 1;
  }
  const float s{(m - 1.0f) / (m + 1.0f)};
  const float s2{s * s};
  return float(e) * 0.6931472f +
         s * (2.0f + s2 * (0.6666667f +
                           s2 * (0.4f + s2 * (0.2857143f + s2 * 0.2222222f))));
}

/// The natural logarithm of a positive finite double, denormals included,
/// to 2e-11 relative to \f$ \ln x \f$ away from 1 and 5e-12 absolute
/// within [0.5, 2], exactly 0 at 1. The same reduction as the float
/// overload with two more terms of the series.
[[nodiscard]] inline double fastLog(double x) noexcept {
  const bool denormal{x < std::numeric_limits<double>::min()};
  x = denormal ? x * 4503599627370496.0 : x;
  std::uint64_t bits{};
  std::memcpy(&bits, &x, sizeof(bits));
  int e{int((bits >> 52) & 0x7FF) - 1023 - (denormal ? 52 : 0)};
  bits = (bits & 0x800FFFFFFFFFFFFFull) | (1023ull << 52);
  double m{};
  std::memcpy(&m, &bits, sizeof(m));
  if (m > 1.3333333333333333) {
    m *= 0.5;
    e += 1;
  }
  const double s{(m - 1.0) / (m + 1.0)};
  const double s2{s * s};
  return double(e) * 0.6931471805599453 +
         s * (2.0 +
              s2 *
                  (0.6666666666666666 +
                   s2 * (0.4 + s2 * (0.2857142857142857 +
                                     s2 * (0.2222222222222222 +
                                           s2 * (0.18181818181818182 +
                                                 s2 * 0.15384615384615385))))));
}

/// The arccosine of \f$ x \in [-1, 1] \f$ to 5e-7 radians, exact at both
/// endpoints and continuous through 0 to float resolution. Abramowitz and
/// Stegun 4.4.46, the degree-7 polynomial in |x| scaled by sqrt(1 - |x|),
/// reflected through acos(x) = pi - acos(-x) for a negative argument: one
/// square root and no division.
[[nodiscard]] inline float fastAcos(float x) noexcept {
  const float a{std::abs(x)};
  const float p{
      std::sqrt(1.0f - a) *
      (1.5707963050f +
       a * (-0.2145988016f +
            a * (0.0889789874f +
                 a * (-0.0501743046f +
                      a * (0.0308918810f +
                           a * (-0.0170881256f +
                                a * (0.0066700901f + a * -0.0012624911f)))))))};
  return x < 0.0f ? 3.14159265f - p : p;
}

/// \}

/// \}

} // namespace smdl
