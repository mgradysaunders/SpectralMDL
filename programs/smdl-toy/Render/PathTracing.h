#pragma once

#include <memory>

#include "Render/Context.h"

struct CameraSample;

/// The walker of a block's paths: what `tracePath()` carries along a
/// path, opaque to the caller. Built once per block by `makePathWalk()`
/// rather than per path, because building it is over a kilobyte of
/// default member initializers, most of them the manifold coverage's
/// chain of hits, and no path reads what the last one left.
class PathWalk;

struct PathWalkDeleter final {
  void operator()(PathWalk *walk) const noexcept;
};

/// Make the walker of `path`'s block, bound to `render` and `path` for
/// as long as it lives.
[[nodiscard]] std::unique_ptr<PathWalk, PathWalkDeleter>
makePathWalk(const RenderContext &render, PathContext &path);

/// Trace the camera path `camera` starts with `walk` and return its
/// radiance estimate.
///
/// Direct lighting is gathered at every scattering vertex as the walk
/// reaches it, so nothing is retained per vertex. Each vertex pairs
/// light sampling with the walk's own continuation as the BSDF-sampling
/// half of the MIS estimate: an emitter hit or an environment escape
/// contributes MIS-weighted against what light sampling at the previous
/// vertex would have produced, and the camera segment, which no light
/// sampling competes with, contributes at weight 1.
///
/// With `RenderContext::mneeOptions` enabled, a light gather whose straight
/// shadow segment is blocked by up to `MNEEOptions::depth` smooth
/// refractive interfaces connects through them by manifold next-event
/// estimation instead of reading as occluded: toward the sun and sky,
/// toward punctual lights (whose through-interface transport no other
/// estimator can reach at all), and toward area lights. The walk's own
/// arrivals at lights through such chains, environment escapes and
/// emitter hits alike, are weighed against the gather by re-walk MIS:
/// the arrival keeps its full weight exactly where the gather cannot
/// produce the transport (a chain family or fold solution the walk does
/// not reach, a failed walk, a light the sampler never draws), so the
/// combined estimator is unbiased rather than exclusive.
///
/// With `Guiding::tree`, non-Dirac surface bounces one-sample-MIS the
/// SD-tree against the BSDF and Russian roulette becomes adjoint-driven;
/// without one, direction sampling and roulette are plain path tracing's.
[[nodiscard]] Color tracePath(PathWalk &walk, const CameraSample &camera);
