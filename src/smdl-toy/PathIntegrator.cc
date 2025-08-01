#include "PathIntegrator.h"

void PathIntegrator::integrate(const Scene &scene, const Color &wavelengthBase,
                               smdl::SpectralRenderImage &renderImage) const {
  const auto imageExtentX{size_t(scene.camera.imageExtent.x)};
  const auto imageExtentY{size_t(scene.camera.imageExtent.y)};
  smdl::parallel_for(0, imageExtentX * imageExtentY, [&](size_t pixelIndex) {
    auto result{Color()};
    const auto pixelIndexX{pixelIndex % imageExtentX};
    const auto pixelIndexY{pixelIndex / imageExtentX};
    auto allocator{smdl::BumpPtrAllocator()};
    auto rng{make_RNG(seed, 0x68712063UL, 0x7F245C06UL, pixelIndexX,
                      0xF3A24269UL, 0x6E806700UL, pixelIndexY, 0xD0B1F8D7UL,
                      0xB729A7D3UL)};
    auto rngf{[&rng] { return generate_canonical(rng); }};
    auto path{std::vector<Vertex>(size_t(maxOrder + 1))};
    for (unsigned sample = 0; sample < samplesPerPixel; sample++) {
      auto pathLen{scene.trace_path_from_camera(
          allocator, rngf, wavelengthBase,
          smdl::float2(static_cast<float>(pixelIndexX) + rngf(),
                       static_cast<float>(pixelIndexY) + rngf()),
          path.size(), path.data())};
      for (unsigned order = minOrder; order < pathLen; order++) {
        const auto &cameraVertex{path[order]};
        if (cameraVertex.isAtInfinity) {
          // TODO
          continue;
        }
        if (order > maxOrder) {
          continue;
        }
        if (auto lightVertex{Vertex{}}; Light_last_vertex_sample(
                scene, rngf(), smdl::float2(rngf(), rngf()), cameraVertex,
                lightVertex)) {
          if (scene.test_visibility(allocator, rngf, wavelengthBase,
                                    cameraVertex, lightVertex,
                                    lightVertex.beta)) {
            result += lightVertex.beta;
          }
        }
      }
      allocator.reset();
    }
    result /= float(samplesPerPixel);
    renderImage.pixel_reference(pixelIndexX, pixelIndexY)
        .add_sample(1.0, smdl::Span<float>(result.data(), result.size()));
  });
}
