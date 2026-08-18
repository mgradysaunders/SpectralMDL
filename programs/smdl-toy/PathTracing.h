#pragma once

#include "Medium.h"

class LightSampler;

struct Guiding;

struct GuideRecord;

/// Trace a camera path and return its radiance estimate.
///
/// The path starts on `ray`, whose direction must be normalized, carrying
/// `cameraWeight` as the initial throughput and `cameraConeAngle` as the
/// per-pixel ray cone spread (zero switches the LOD cone off end to end).
/// Direct lighting is gathered at every scattering vertex as the walk
/// reaches it, so nothing is retained per vertex; `maxDepth` bounds only
/// the walk itself, and Russian roulette terminates paths long before it.
///
/// Each vertex pairs light sampling with the walk's own continuation as
/// the BSDF-sampling half of the MIS estimate: an emitter hit or an
/// environment escape contributes MIS-weighted against what light
/// sampling at the previous vertex would have produced, and the camera
/// segment, which no light sampling competes with, contributes at
/// weight 1.
///
/// The walk starts inside `exteriorMedium`, which may be null for
/// vacuum: this is the bottom of the nested-medium stack, typically a
/// scene-wide fog or atmosphere named by the composition's `medium`
/// directive, whose `MediumStack` entry the caller owns for the whole
/// render.
///
/// The `guiding` may be null or have a null tree, in which case direction
/// sampling and Russian roulette behave as plain path tracing; with a
/// tree, non-delta surface bounces one-sample-MIS the SD-tree against the
/// BSDF and roulette becomes adjoint-driven.
///
/// If `records` is non-null it must hold `maxDepth` entries: the walk
/// appends one `GuideRecord` per vertex, returns the count in
/// `numRecords`, and the completed buffer feeds `trainGuiding()`. A null
/// `records` retains nothing.
[[nodiscard]] Color tracePath(smdl::Compiler &compiler, const Scene &scene,
                              Sampler &sampler, const Color &wavelengths,
                              smdl::BumpPtrAllocator &allocator, Ray ray,
                              float cameraWeight, float cameraConeAngle,
                              const MediumStack *exteriorMedium,
                              uint64_t maxDepth, const LightSampler &lights,
                              const Guiding *guiding, GuideRecord *records,
                              uint64_t &numRecords);
