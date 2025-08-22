#include "vertex.h"

uint64_t random_walk(const Scene &scene, const RandomFP &random,
                     const Color &wavelengths,
                     smdl::BumpPtrAllocator &allocator,
                     smdl::Transport transport, Vertex path0, float wpdfFwd,
                     uint64_t maxDepth, Vertex *path) {
  if (maxDepth == 0)
    return 0;
  path[0] = std::move(path0);
  if (maxDepth == 1)
    return 1;

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
    if (!hitSurface && !hitMedium) {
      if (transport == smdl::TRANSPORT_RADIANCE) {
        ++depth;
        vertex.point =
            vertexPrev.point + 2 * scene.boundRadius * vertexPrev.wNext;
        vertex.beta = beta;
        vertex.wNext = vertexPrev.wNext;
        vertex.pdfFwd = wpdfFwd;
        vertex.isInfiniteLight = true;
      }
      break;
    }

    hit.apply_geometry_to_state(state);
    auto materialInstance{smdl::JIT::MaterialInstance(state, hit.material)};
    if (float opacity{materialInstance.cutout_opacity()};
        opacity < 1 && (opacity == 0 || float(random) > opacity)) {
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
    ray = Ray{vertex.point, vertex.wNext, EPS, INF};
  }
  return depth;
}
