#pragma once

#include "raytracing.h"

class Camera;

class Light;

class MediumStack final {
public:
  const MediumStack *prev{};

  smdl::JIT::MaterialInstance materialInstance{};

  static void Update(const MediumStack *&stack,
                     smdl::BumpPtrAllocator &allocator,
                     smdl::JIT::MaterialInstance mat, const float3 &wo,
                     const float3 &wi) {
    if (mat.isTransmitting(wo, wi)) {
      if (mat.isInterior(wi)) {
        stack = new (allocator) MediumStack{stack, mat};
      } else if (stack) {
        stack = stack->prev;
      }
    }
  }
};

/// The index of refraction of the medium surrounding the object being hit,
/// which the material needs to form the relative IOR across the interface.
/// On a front-face hit that is the medium the ray currently travels in (the
/// top of the stack); on a back-face hit the ray travels inside the object
/// itself, so the surrounding medium is the next stack entry below it.
[[nodiscard]] inline float ExteriorIOR(const MediumStack *stack,
                                       const smdl::JIT::MaterialInstance &mat,
                                       const float3 &wo) noexcept {
  if (mat.isInterior(wo)) stack = stack ? stack->prev : nullptr;
  return stack ? stack->materialInstance.getIOR() : 1.0f;
}

class Vertex final {
public:
  [[nodiscard]]
  bool scatter_evaluate(const float3 &wo, const float3 &wi, float &pdfFwd,
                        float &pdfRev, Color &f) const {
    if (isVolume) {
      // The phase function is reciprocal (it depends only on the angle
      // between the directions), so it is its own reverse PDF.
      float phase{materialInstance.volumeScatterEvaluate(wo, wi)};
      pdfFwd = phase;
      pdfRev = phase;
      f = Color(phase);
      return phase > 0;
    } else {
      return materialInstance.scatterEvaluate(wo, wi, pdfFwd, pdfRev, f);
    }
  }

  [[nodiscard]]
  bool scatter_sample(const float4 &xi, const float3 &wo, float3 &wi,
                      float &wpdfFwd, float &wpdfRev, Color &f,
                      bool &isDeltaBounce) const {
    if (isVolume) {
      float phase{materialInstance.volumeScatterSample(xi, wo, wi)};
      wpdfFwd = phase;
      wpdfRev = phase;
      f = Color(phase);
      isDeltaBounce = false;
      return phase > 0;
    } else {
      return materialInstance.scatterSample(xi, wo, wi, wpdfFwd, wpdfRev, f,
                                            isDeltaBounce);
    }
  }

  [[nodiscard]]
  float convert_pdf(float wpdf, const Vertex &next) const {
    if (!next.isInfiniteLight) {
      auto v{next.point - point};
      wpdf /= lengthSquared(v);
      if (!next.isVolume) {
        wpdf *= absDot(next.materialInstance.getGeometryNormal(), normalize(v));
      }
    }
    return wpdf;
  }

public:
  float3 point{};

  float2 pixel{};

  Color beta{};

  const Camera *camera{};

  const Light *light{};

  const MediumStack *medium{};

  smdl::JIT::MaterialInstance materialInstance{};

  /// The index in `Scene::meshInstances` of the surface this vertex sits
  /// on, so emissive hits can be matched back to their `AreaLight`.
  uint32_t meshInstanceIndex{INVALID_INDEX};

  float3 wNext{};

  /// The solid-angle density that sampled `wNext` — the full mixture
  /// density when path guiding participated — for recording radiance
  /// estimates into the SD-tree.
  float pdfWNext{};

  float pdfFwd{QUIET_NAN};

  float pdfRev{QUIET_NAN};

  bool isDeltaBounce{};

  bool isVolume{};

  bool isInfiniteLight{};
};

[[nodiscard]]
inline float3 Direction(const Vertex &from, const Vertex &to) noexcept {
  return normalize(to.point - from.point);
}

[[nodiscard]]
inline float3 DistanceSquared(const Vertex &from, const Vertex &to) noexcept {
  if (from.isInfiniteLight || to.isInfiniteLight) return 1.0f;
  return lengthSquared(to.point - from.point);
}

[[nodiscard]] bool
test_visibility(const Scene &scene, Sampler &sampler, const Color &wavelengths,
                smdl::BumpPtrAllocator &allocator, const MediumStack *medium,
                const float3 &point0, const float3 &point1, Color &beta);

/// Trace the given ray to its nearest real surface hit, passing through
/// cutouts stochastically and accumulating medium transmittance into `beta`
/// the same way `test_visibility` does. On a hit, fills in `hit` and the
/// material instance constructed at it and returns `true`; returns `false`
/// if the ray escapes the scene. The ray direction must be normalized so
/// the transmittance distances are metric.
[[nodiscard]] bool trace_nearest(const Scene &scene, Sampler &sampler,
                                 const Color &wavelengths,
                                 smdl::BumpPtrAllocator &allocator,
                                 const MediumStack *medium, Ray ray, Hit &hit,
                                 smdl::JIT::MaterialInstance &materialInstance,
                                 Color &beta);

class EnvLight;

class LightSampler;

struct Guiding;

/// Trace a path, writing up to `maxDepth` vertices into `path` and returning
/// the number written. Every vertex is cleared before it is filled in, so
/// `path` does not need to be zero-initialized between calls. The `lights`
/// may be null or have no environment, in which case the walk falls back to
/// pure BSDF sampling. The `guiding` may be null or have a null tree, in
/// which case direction sampling and Russian roulette behave as before;
/// with a tree, non-delta surface bounces one-sample-MIS the SD-tree
/// against the BSDF and roulette becomes adjoint-driven.
[[nodiscard]] uint64_t random_walk(smdl::Compiler &compiler, const Scene &scene,
                                   Sampler &sampler, const Color &wavelengths,
                                   smdl::BumpPtrAllocator &allocator,
                                   smdl::Transport transport, Vertex path0,
                                   float wpdfFwd, uint64_t maxDepth,
                                   Vertex *path, const LightSampler *lights,
                                   const Guiding *guiding);

class Subpath final {
public:
  Subpath() = default;

  Subpath(const Vertex *first, int count) : first(first), count(count) {}

  [[nodiscard]] bool empty() const noexcept { return count == 0; }

  [[nodiscard]] int depth() const noexcept { return count; };

  [[nodiscard]] Subpath subpath(int n) const noexcept { return {first, n}; }

  [[nodiscard]] const Vertex &operator[](int i) const noexcept {
    i += (i < 0) ? count : 0;
    SMDL_SANITY_CHECK(0 <= i && i < count);
    return first[i];
  }

  [[nodiscard]] bool reconnect(const Vertex &vLast, float wpdfRev,
                               Color &f) const;

private:
  const Vertex *first{};

  int count{};
};
