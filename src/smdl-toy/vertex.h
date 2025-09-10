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
    if (mat.is_transmitting(wo, wi)) {
      if (mat.is_interior(wi)) {
        stack = new (allocator) MediumStack{stack, mat};
      } else if (stack) {
        stack = stack->prev;
      }
    }
  }
};

class Vertex final {
public:
  [[nodiscard]]
  bool scatter_evaluate(const float3 &wo, const float3 &wi, float &pdfFwd,
                        float &pdfRev, Color &f) const {
    if (isVolume) {
      // TODO
      float phase = smdl::uniform_sphere_pdf();
      pdfFwd = phase;
      pdfRev = phase;
      f = phase;
      return true;
    } else {
      return materialInstance.scatter_evaluate(wo, wi, pdfFwd, pdfRev, f);
    }
  }

  [[nodiscard]]
  bool scatter_sample(const float4 &xi, const float3 &wo, float3 &wi,
                      float &wpdfFwd, float &wpdfRev, Color &f,
                      bool &isDeltaBounce) const {
    return materialInstance.scatter_sample(xi, wo, wi, wpdfFwd, wpdfRev, f,
                                           isDeltaBounce);
  }

  [[nodiscard]]
  float convert_pdf(float wpdf, const Vertex &next) const {
    if (!next.isInfiniteLight) {
      auto v{next.point - point};
      wpdf /= length_squared(v);
      if (!next.isVolume) {
        wpdf *= abs_dot(next.materialInstance.geometry_normal(), normalize(v));
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

  float3 wNext{};

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
  if (from.isInfiniteLight || to.isInfiniteLight)
    return 1.0f;
  return length_squared(to.point - from.point);
}

[[nodiscard]] bool test_visibility(const Scene &scene, const AnyRandom &random,
                                   const Color &wavelengths,
                                   smdl::BumpPtrAllocator &allocator,
                                   const MediumStack *medium,
                                   const float3 &point0, const float3 &point1,
                                   Color &beta);

[[nodiscard]] uint64_t random_walk(const Scene &scene, const AnyRandom &random,
                                   const Color &wavelengths,
                                   smdl::BumpPtrAllocator &allocator,
                                   smdl::Transport transport, Vertex path0,
                                   float wpdfFwd, uint64_t maxDepth,
                                   Vertex *path);

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
