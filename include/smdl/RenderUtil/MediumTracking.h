/// \file
/// Spectral free-flight and transmittance estimators for participating
/// media: the closed forms of a homogeneous medium, delta tracking and
/// residual ratio tracking against majorant spans, and the component pick
/// of overlapping media. Every spectrum is a span of any number of bands,
/// owned by the caller, so nothing here allocates.
#pragma once

#include <algorithm>
#include <cmath>
#include <cstddef>

#include "smdl/RenderUtil/FastMath.h"
#include "smdl/RenderUtil/MajorantWalk.h"
#include "smdl/Support/Macros.h"
#include "smdl/Support/Span.h"
#include "smdl/Support/VectorMath.h"

namespace smdl {

/// \addtogroup renderutil
/// \{

/// The Beer-Lambert transmittance \f$ e^{-\tau} \f$ of the optical depth
/// \f$ \tau \f$: exactly 1 for a negative depth, and exactly 0 past 87 and
/// for a depth that is not a number. The cutoff is a termination threshold
/// just above `fastExp()`'s floor, where the true value is a normal float
/// that flushing denormals would not retire.
[[nodiscard]] SMDL_ALWAYS_INLINE float beerLambert(float opticalDepth) noexcept {
  return opticalDepth < 87.0f ? fastExp(-std::max(opticalDepth, 0.0f)) : 0.0f;
}

namespace detail {

[[nodiscard]] SMDL_ALWAYS_INLINE float
bandAverage(Span<const float> values) noexcept {
  float sum{0.0f};
  for (float value : values) sum += value;
  return sum / float(values.size());
}

[[nodiscard]] SMDL_ALWAYS_INLINE float
bandMax(Span<const float> values) noexcept {
  float result{values[0]};
  for (size_t i = 1; i < values.size(); i++)
    result = std::max(result, values[i]);
  return result;
}

SMDL_ALWAYS_INLINE void bandFill(Span<float> values, float value) noexcept {
  for (float &each : values) each = value;
}

} // namespace detail

/// Multiply `beta` by the transmittance of `sigma` over `depth`, band by
/// band. A band with no extinction keeps its throughput however long the
/// depth.
SMDL_ALWAYS_INLINE void attenuateBeerLambert(Span<float> beta,
                                             Span<const float> sigma,
                                             float depth) noexcept {
  SMDL_DEBUG_CHECK(beta.size() == sigma.size());
  depth = std::min(depth, FLOAT_MAX);
  for (size_t i = 0; i < beta.size(); i++)
    beta[i] *= beerLambert(sigma[i] * depth);
}

/// Add the emission of a homogeneous medium of extinction `sigmaT` over
/// `[0, depth]` to `emitted`: the emission coefficient times the integral of
/// the transmittance, whose extinction-free limit is linear in the depth.
SMDL_ALWAYS_INLINE void
accumulateHomogeneousEmission(Span<float> emitted, Span<const float> emission,
                              Span<const float> sigmaT, float depth) noexcept {
  SMDL_DEBUG_CHECK(emitted.size() == emission.size());
  SMDL_DEBUG_CHECK(emitted.size() == sigmaT.size());
  // `std::exp` rather than `fastExp()`: at small optical depth the
  // difference against one cancels down to the error of the exponential.
  for (size_t i = 0; i < emitted.size(); i++)
    emitted[i] += emission[i] * (sigmaT[i] > 1e-12f
                                     ? (1.0f - std::exp(-sigmaT[i] * depth)) /
                                           sigmaT[i]
                                     : depth);
}

/// Clamp coefficients queried inside a tracked medium to its declared
/// majorants at the local `scale`, and the emission nonnegative, so that a
/// majorant that lies renders a clamped medium rather than negative weights.
/// The majorants must be nonnegative.
SMDL_ALWAYS_INLINE void clampToMajorants(Span<float> sigmaA, Span<float> sigmaS,
                                         Span<float> emission,
                                         Span<const float> maxSigmaA,
                                         Span<const float> maxSigmaS,
                                         float scale) noexcept {
  SMDL_DEBUG_CHECK(sigmaA.size() == sigmaS.size());
  SMDL_DEBUG_CHECK(sigmaA.size() == maxSigmaA.size());
  SMDL_DEBUG_CHECK(sigmaA.size() == maxSigmaS.size());
  for (size_t i = 0; i < sigmaA.size(); i++) {
    sigmaA[i] = std::clamp(sigmaA[i], 0.0f, maxSigmaA[i] * scale);
    sigmaS[i] = std::clamp(sigmaS[i], 0.0f, maxSigmaS[i] * scale);
  }
  for (float &value : emission) value = std::max(value, 0.0f);
}

/// Sample a free flight through a homogeneous medium of extinction `sigmaT`
/// and scattering `sigmaS` over `[0, tEnd)`, against the band `hero` drawn
/// uniformly by the caller, weighted by the single-sample balance heuristic
/// over all bands. `xi` is a canonical draw in `(0, 1)`, and `scratch` holds
/// one float per band.
///
/// \return
/// True on a real collision at `t`, multiplying `beta` by the weight of
/// scattering there, or false where the flight survives to `tEnd`,
/// multiplying `beta` by the survival weight.
///
[[nodiscard]] inline bool
homogeneousFreeFlight(float xi, size_t hero, Span<const float> sigmaT,
                      Span<const float> sigmaS, float tEnd, Span<float> scratch,
                      Span<float> beta, float &t) noexcept {
  const size_t numBands{beta.size()};
  SMDL_DEBUG_CHECK(hero < numBands);
  SMDL_DEBUG_CHECK(sigmaT.size() == numBands && sigmaS.size() == numBands);
  SMDL_DEBUG_CHECK(scratch.size() == numBands);
  // The flight off the complement of the canonical draw, the same
  // exponential without the libm call. A band with no extinction keeps
  // transmittance 1 through the min against the largest float.
  const float tScatter{-fastLog(1.0f - xi) / sigmaT[hero]};
  const float depth{std::min({tScatter, tEnd, FLOAT_MAX})};
  Span<float> transmittance{scratch};
  for (size_t i = 0; i < numBands; i++)
    transmittance[i] = beerLambert(sigmaT[i] * depth);
  if (tScatter < tEnd) {
    float pdf{0.0f};
    for (size_t i = 0; i < numBands; i++)
      pdf += sigmaT[i] * transmittance[i];
    pdf /= float(numBands);
    for (size_t i = 0; i < numBands; i++)
      beta[i] *= sigmaS[i] * transmittance[i] / pdf;
    t = tScatter;
    return true;
  }
  const float invAverage{1.0f / detail::bandAverage(transmittance)};
  for (size_t i = 0; i < numBands; i++)
    beta[i] *= transmittance[i] * invAverage;
  return false;
}

/// What a tentative collision decided, and what a tracked segment came to.
enum class TrackOutcome {
  CONTINUE,  ///< A null collision; tracking continues.
  SURVIVED,  ///< The segment survived to its end.
  SCATTERED, ///< A real collision.
  DEAD,      ///< No band carries throughput, or the collision cap was hit.
};

/// The cap on tentative collisions in one tracked segment, a guard against
/// marching forever through leaky geometry with a positive majorant.
/// Reaching it honestly would take an optical depth in the tens of
/// thousands, and the segment is taken to be fully absorbed.
inline constexpr int MAX_TENTATIVE_COLLISIONS{65536};

/// The scalar majorant of a tracked segment, split into the part that the
/// spans of a density grid scale and the constant rest: over a span the
/// local majorant is `base + grid * span.scale`. Without a grid the walk
/// yields one span of scale 1, so the whole majorant may sit in `grid`.
struct TrackingMajorant final {
  float base{};

  float grid{};

  [[nodiscard]] float operator()(const MajorantSpan &span) const noexcept {
    return base + grid * span.scale;
  }
};

/// The per-band working storage of one tracked segment, every span one
/// float per band. After `deltaTrack()` returns `SCATTERED`, `sigmaA` and
/// `sigmaS` hold the coefficients at the collision.
struct TrackingScratch final {
  Span<float> weight{};

  Span<float> sigmaA{};

  Span<float> sigmaS{};

  Span<float> emission{};
};

/// The null-collision march every tracked estimator shares: one exponential
/// in optical depth consumed across the majorant spans of `spans`,
/// `majorantOf(span)` giving the local rate and `collide(t, span, m, tau)`
/// deciding each tentative collision, drawing the next flight into `tau`
/// when it returns `CONTINUE`. `draw()` returns canonical floats.
template <typename Draw, typename MajorantOf, typename Collide>
[[nodiscard]] TrackOutcome trackSpans(MajorantSpanWalk &spans, Draw &&draw,
                                      MajorantOf &&majorantOf,
                                      Collide &&collide) {
  MajorantSpan span{};
  int numCollisions{0};
  float tau{-fastLog(draw())};
  while (spans.next(span)) {
    // A zero majorant reaches here only where a part of the majorant that
    // does not scale with the grid kept the walk from skipping the span.
    const float m{majorantOf(span)};
    if (!(m > 0.0f)) continue;
    float tCurr{span.t0};
    while (true) {
      if (const float dTau{m * (span.t1 - tCurr)}; tau >= dTau) {
        tau -= dTau;
        break;
      }
      if (SMDL_UNLIKELY(++numCollisions > MAX_TENTATIVE_COLLISIONS))
        return TrackOutcome::DEAD;
      tCurr += tau / m;
      if (const TrackOutcome outcome{collide(tCurr, span, m, tau)};
          outcome != TrackOutcome::CONTINUE)
        return outcome;
    }
  }
  return TrackOutcome::SURVIVED;
}

/// Delta tracking with spectral MIS in the null-scattering formulation
/// (Miller et al. 2019): the band `hero`, drawn uniformly by the caller,
/// decides real against null collisions, and the per-band products of the
/// null-collision factors carry the single-sample balance heuristic over
/// all bands through the chain.
///
/// `query(t, scale, sigmaA, sigmaS, emission)` writes the coefficients at
/// distance `t`, clamped to the majorants at the span's `scale`
/// (`clampToMajorants()`). `draw()` returns canonical floats. Pass an empty
/// `emitted` for a medium that does not emit; otherwise the emission adds
/// up at every tentative collision, unweighted by `beta`.
///
/// \return
/// `SCATTERED` with `t` set and `beta` multiplied by the weight of
/// scattering there, `SURVIVED` with `beta` multiplied by the survival
/// weight, or `DEAD` with `beta` zeroed.
///
template <typename Draw, typename Query>
[[nodiscard]] TrackOutcome
deltaTrack(MajorantSpanWalk spans, const TrackingMajorant &majorant,
           size_t hero, Draw &&draw, Query &&query,
           const TrackingScratch &scratch, Span<float> beta,
           Span<float> emitted, float &t) {
  const size_t numBands{beta.size()};
  SMDL_DEBUG_CHECK(hero < numBands);
  SMDL_DEBUG_CHECK(scratch.weight.size() == numBands);
  SMDL_DEBUG_CHECK(emitted.empty() || emitted.size() == numBands);
  Span<float> weight{scratch.weight};
  Span<float> sigmaA{scratch.sigmaA};
  Span<float> sigmaS{scratch.sigmaS};
  Span<float> emission{scratch.emission};
  // The weight is meaningful only up to a common scale, which cancels in
  // every terminal weight, so it is renormalized as it goes to keep the
  // products from underflowing.
  detail::bandFill(weight, 1.0f);
  const auto collide{[&](float tCurr, const MajorantSpan &span, float m,
                         float &tau) {
    query(tCurr, span.scale, sigmaA, sigmaS, emission);
    // The emission at every tentative collision, before classification,
    // integrates transmittance times the emission coefficient over the
    // segment in expectation.
    if (SMDL_UNLIKELY(!emitted.empty())) {
      if (const float pdf{m * detail::bandAverage(weight)}; pdf > 0.0f)
        for (size_t i = 0; i < numBands; i++)
          emitted[i] += emission[i] * weight[i] / pdf;
    }
    if (draw() * m < sigmaA[hero] + sigmaS[hero]) {
      // A real collision, weighted by the scattering coefficient over the
      // mixture density of every hero having produced this chain, the
      // absorption folded into the weight rather than terminating.
      float pdf{0.0f};
      for (size_t i = 0; i < numBands; i++)
        pdf += (sigmaA[i] + sigmaS[i]) * weight[i];
      pdf /= float(numBands);
      if (SMDL_UNLIKELY(!(pdf > 0.0f))) return TrackOutcome::DEAD;
      for (size_t i = 0; i < numBands; i++)
        beta[i] *= sigmaS[i] * weight[i] / pdf;
      t = tCurr;
      return TrackOutcome::SCATTERED;
    }
    // A null collision, with the next flight drawn before the chain is
    // renormalized. Every band at the majorant means no band carries
    // throughput.
    for (size_t i = 0; i < numBands; i++)
      weight[i] *= m - (sigmaA[i] + sigmaS[i]);
    const float renormalize{detail::bandMax(weight)};
    tau = -fastLog(draw());
    if (SMDL_UNLIKELY(!(renormalize > 0.0f))) return TrackOutcome::DEAD;
    for (float &value : weight) value /= renormalize;
    return TrackOutcome::CONTINUE;
  }};
  const TrackOutcome outcome{trackSpans(spans, draw, majorant, collide)};
  if (outcome == TrackOutcome::DEAD) {
    detail::bandFill(beta, 0.0f);
  } else if (outcome == TrackOutcome::SURVIVED) {
    const float invAverage{1.0f / detail::bandAverage(weight)};
    for (size_t i = 0; i < numBands; i++) beta[i] *= weight[i] * invAverage;
  }
  return outcome;
}

/// Residual ratio tracking (Novak et al. 2014) of the transmittance of a
/// segment, multiplied into `beta` band by band with no hero. Per span the
/// extinction splits into the analytic control, `control` scaled by the
/// span's lower bound `scaleMin`, and a residual that is ratio tracked at
/// the rate of the majorant less the control. An empty `control` is zero,
/// which reduces this to plain ratio tracking. `query` and `draw` are as
/// for `deltaTrack()`, and the `emission` that `query` writes is unused.
///
/// \return
/// `SURVIVED`, or `DEAD` with `beta` zeroed.
///
template <typename Draw, typename Query>
[[nodiscard]] TrackOutcome
residualRatioTrack(MajorantSpanWalk spans, const TrackingMajorant &majorant,
                   Span<const float> control, Draw &&draw, Query &&query,
                   const TrackingScratch &scratch, Span<float> beta) {
  const size_t numBands{beta.size()};
  SMDL_DEBUG_CHECK(control.empty() || control.size() == numBands);
  SMDL_DEBUG_CHECK(scratch.weight.size() == numBands);
  // The control transmittance is accumulated as an optical depth and
  // exponentiated once at the end.
  Span<float> controlDepth{scratch.weight};
  Span<float> sigmaA{scratch.sigmaA};
  Span<float> sigmaS{scratch.sigmaS};
  detail::bandFill(controlDepth, 0.0f);
  float invM{};
  const auto majorantOf{[&](const MajorantSpan &span) {
    float scaleMin{0.0f};
    if (!control.empty() && span.scaleMin > 0.0f) {
      scaleMin = span.scaleMin;
      for (size_t i = 0; i < numBands; i++)
        controlDepth[i] += control[i] * scaleMin * (span.t1 - span.t0);
    }
    const float m{majorant.grid * (span.scale - scaleMin) + majorant.base};
    invM = 1.0f / m;
    return m;
  }};
  const auto collide{[&](float tCurr, const MajorantSpan &span, float m,
                         float &tau) {
    query(tCurr, span.scale, sigmaA, sigmaS, scratch.emission);
    // The factor exceeds one where the extinction dips below the control,
    // which the estimator covers for residuals of either sign.
    const float scaleMin{control.empty() ? 0.0f : span.scaleMin};
    for (size_t i = 0; i < numBands; i++) {
      const float controlI{control.empty() ? 0.0f : control[i] * scaleMin};
      beta[i] *= (m - (sigmaA[i] + sigmaS[i] - controlI)) * invM;
    }
    tau = -fastLog(draw());
    return SMDL_UNLIKELY(!(detail::bandMax(beta) > 0.0f))
               ? TrackOutcome::DEAD
               : TrackOutcome::CONTINUE;
  }};
  if (trackSpans(spans, draw, majorantOf, collide) == TrackOutcome::DEAD) {
    detail::bandFill(beta, 0.0f);
    return TrackOutcome::DEAD;
  }
  attenuateBeerLambert(beta, controlDepth, 1.0f);
  return TrackOutcome::SURVIVED;
}

/// Pick which of `numComponents` overlapping media scatters at a real
/// collision (decomposition tracking, Kutz et al. 2017): component `k` with
/// probability proportional to the band average of its scattering
/// coefficient there, `sigmaSOf(k)`, folding its per-band share of the
/// summed `sigmaS` over that probability into `beta`. A band with no
/// scattering keeps zero throughput rather than forming 0/0.
///
/// \return
/// The index picked, or 0 with `beta` unchanged where nothing scatters.
///
template <typename SigmaSOf>
[[nodiscard]] size_t pickScatterComponent(float xi, size_t numComponents,
                                          SigmaSOf &&sigmaSOf,
                                          Span<const float> sigmaS,
                                          Span<float> beta) {
  float totalAverage{0.0f};
  for (size_t k = 0; k < numComponents; k++)
    totalAverage += detail::bandAverage(sigmaSOf(k));
  if (!(totalAverage > 0.0f)) return 0;
  size_t picked{0};
  float pickedChance{0.0f};
  float cdf{0.0f};
  for (size_t k = 0; k < numComponents; k++) {
    const float average{detail::bandAverage(sigmaSOf(k))};
    if (!(average > 0.0f)) continue;
    picked = k;
    pickedChance = average / totalAverage;
    cdf += pickedChance;
    if (xi < cdf) break;
  }
  const Span<const float> pickedSigmaS{sigmaSOf(picked)};
  const float invChance{1.0f / pickedChance};
  for (size_t i = 0; i < beta.size(); i++)
    beta[i] = sigmaS[i] > 0.0f ? beta[i] * (pickedSigmaS[i] / sigmaS[i] *
                                            invChance)
                               : 0.0f;
  return picked;
}

/// \}

} // namespace smdl
