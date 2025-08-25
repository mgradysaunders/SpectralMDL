#include "vertex.h"

#include <vector>

class MediumStack final {
public:
  MediumStack() { history.reserve(16); }

  void update(smdl::JIT::MaterialInstance mat, const float3 &wo,
              const float3 &wi) {
    if (!mat.is_thin_walled() && mat.is_transmitting(wo, wi)) {
      if (mat.is_interior(wi)) {
        history.push_back(current);
        current = mat;
      } else if (!history.empty()) {
        current = history.back();
        history.pop_back();
      }
    }
  }

  smdl::JIT::MaterialInstance current{};
  std::vector<smdl::JIT::MaterialInstance> history{};
};

bool test_visibility(const Scene &scene, const AnyRandom &random,
                     const Color &wavelengths,
                     smdl::BumpPtrAllocator &allocator, //
                     const float3 &point0, const float3 &point1, Color &beta) {
  Ray ray{point0, point1 - point0, EPS, 1.0f - EPS};
  while (ray.tmin < ray.tmax) {
    Hit hit{};
    if (!scene.intersect(ray, hit)) {
      break;
    }
    smdl::State state{};
    state.allocator = &allocator;
    state.wavelength_base = wavelengths.data();
    state.wavelength_min = WAVELENGTH_MIN;
    state.wavelength_max = WAVELENGTH_MAX;
    hit.apply_geometry_to_state(state);
    smdl::JIT::MaterialInstance materialInstance{state, hit.material};
    if (float opacity{materialInstance.cutout_opacity()};
        opacity == 1 || float(random) < opacity) {
      return false; // Blocks visibility!
    }
    ray.tmin = smdl::increment_float(ray.tmax + EPS);
    ray.tmax = 1.0f - EPS;
  }
  return true;
}

uint64_t random_walk(const Scene &scene, const AnyRandom &random,
                     const Color &wavelengths,
                     smdl::BumpPtrAllocator &allocator,
                     smdl::Transport transport, Vertex path0, float wpdfFwd,
                     uint64_t maxDepth, Vertex *path) {
  if (maxDepth == 0)
    return 0;
  path[0] = std::move(path0);
  if (maxDepth == 1)
    return 1;

  // Default construct a medium stack, assuming we start on
  // the exterior of all materials with interior participating
  // media.
  MediumStack mediumStack{};

  // We declare the state here and set up the variables that never
  // change. The other state variables get updated at every vertex
  // on the path by `Hit::apply_geometry_to_state()`.
  smdl::State state{};
  state.allocator = &allocator;
  state.wavelength_base = wavelengths.data();
  state.wavelength_min = WAVELENGTH_MIN;
  state.wavelength_max = WAVELENGTH_MAX;
  state.transport = transport;

  Color beta{(1.0f / wpdfFwd) * path[0].beta};
  Color f{};
  Ray ray{path[0].point, path[0].wNext, EPS, INF};
  uint64_t depth{1};
  while (depth < maxDepth) {
    auto &vertexPrev{path[depth - 1]};
    auto &vertex{path[depth]};

    auto hit{Hit{}};
    bool hitSurface{scene.intersect(ray, hit)};
    bool hitMedium{false};
#if 0
    if (mediumStack.current.has_medium()) {
      Color muA = Color(mediumStack.current.absorption_coefficient());
      Color muS = Color(mediumStack.current.scattering_coefficient());
      Color mu = muA + muS;
      float t =
          -std::log1p(-float(random)) / mu[random.index(WAVELENGTH_BASE_MAX)];
      t = std::min(t, ray.tmax);
      Color Tr{};
      for (size_t i = 0; i < WAVELENGTH_BASE_MAX; i++)
        Tr[i] =
            std::exp(-mu[i] * std::min(t, std::numeric_limits<float>::max()));
      if (t < ray.tmax) {
        beta *= muS * Tr / (mu * Tr).average();
        hitMedium = true;
        vertex.point = ray(t);
        vertex.beta = beta;
        vertex.wNext = smdl::uniform_sphere_sample(float2(random));
        vertex.materialInstance = mediumStack.current;
        // TODO
      } else {
        beta *= Tr / Tr.average();
      }
    }
#endif
    if (!hitSurface && !hitMedium) {
      if (transport == smdl::TRANSPORT_RADIANCE) {
        ++depth;
        vertex.point =
            vertexPrev.point + 2 * scene.boundRadius * vertexPrev.wNext;
        vertex.beta = beta;
        // Set wNext to point in the direction of propagation
        // for infinite light vertices
        vertex.wNext = -vertexPrev.wNext;
        vertex.pdfFwd = wpdfFwd;
        vertex.isInfiniteLight = true;
      }
      break;
    }

    hit.apply_geometry_to_state(state);
    auto materialInstance{smdl::JIT::MaterialInstance(state, hit.material)};
    if (float opacity{materialInstance.cutout_opacity()};
        opacity < 1 && (opacity == 0 || float(random) > opacity)) {
      mediumStack.update(materialInstance, -ray.dir, ray.dir);
      ray = Ray{hit.point, ray.dir, EPS, INF};
      continue;
    }

    ++depth;
    vertex.point = hit.point;
    vertex.beta = beta;
    vertex.materialInstance = materialInstance;
    float wpdfRev{};
    if (!materialInstance.scatter_sample(float4(random), -vertexPrev.wNext,
                                         vertex.wNext, wpdfFwd, wpdfRev,
                                         f.data(), vertex.isDeltaBounce)) {
      break;
    }
    beta *= (1.0f / wpdfFwd) * f;
    beta.set_non_finite_to_zero();
    if (beta.is_all_zero()) {
      break;
    }
    mediumStack.update(materialInstance, -vertexPrev.wNext, vertex.wNext);
    ray = Ray{vertex.point, vertex.wNext, EPS, INF};
  }
  return depth;
}
