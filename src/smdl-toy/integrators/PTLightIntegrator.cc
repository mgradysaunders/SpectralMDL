#include "PTLightIntegrator.h"

void PTLightIntegrator::integrate(
    const Scene &scene, const Color &wavelengthBase,
    smdl::SpectralRenderImage &renderImage) const {
  const auto extentX{size_t(scene.camera.extent.x)};
  const auto extentY{size_t(scene.camera.extent.y)};
  smdl::parallel_for(0, extentX * extentY, [&](size_t pixelIndex) {
    auto result{Color()};
    const auto pixelIndexX{pixelIndex % extentX};
    const auto pixelIndexY{pixelIndex / extentX};
    auto rng{make_RNG(seed, 0x68712063UL, 0x7F245C06UL, pixelIndexX,
                      0xF3A24269UL, 0x6E806700UL, pixelIndexY, 0xD0B1F8D7UL,
                      0xB729A7D3UL)};
    auto allocator{smdl::BumpPtrAllocator()};
    auto random{Random([&rng]() { return smdl::generate_canonical(rng); })};
    auto path{std::vector<Vertex>(size_t(maxOrder + 1))};
    for (size_t sample = 0; sample < samplesPerPixel; sample++) {
      auto pathLen{scene.trace_path_from_light(
          allocator, random, wavelengthBase, path.size(), path.data())};
      for (size_t order = minOrder; order < pathLen; order++) {
        const auto &lightVertex{path[order]};
        if (lightVertex.isAtInfinity) {
          continue;
        }
        if (order > maxOrder) {
          continue;
        }
        Vertex cameraVertex{};
        if (scene.camera.last_vertex_sample(random, lightVertex,
                                            cameraVertex)) {
          if (scene.test_visibility(allocator, random, wavelengthBase,
                                    lightVertex, cameraVertex,
                                    cameraVertex.beta)) {
            renderImage
                .pixel_reference(size_t(cameraVertex.pixelCoord.x),
                                 size_t(cameraVertex.pixelCoord.y))
                .add(1.0 / double(samplesPerPixel), cameraVertex.beta.data());
          }
        }
      }
      allocator.reset();
    }
  });
}
