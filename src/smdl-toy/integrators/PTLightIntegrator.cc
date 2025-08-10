#include "PTLightIntegrator.h"

void PTLightIntegrator::integrate(
    const Scene &scene, const Color &wavelengthBase,
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
    for (size_t sample = 0; sample < samplesPerPixel; sample++) {
      auto pathLen{scene.trace_path_from_light(allocator, rngf, wavelengthBase,
                                               path.size(), path.data())};
      for (size_t order = minOrder; order < size_t(pathLen); order++) {
        const auto &lightVertex{path[order]};
        if (lightVertex.isAtInfinity) {
          continue;
        }
        if (order > maxOrder) {
          continue;
        }
        Vertex cameraVertex{};
        if (Camera_last_vertex_sample(scene.camera,
                                      smdl::float2(rngf(), rngf()), lightVertex,
                                      cameraVertex)) {
          if (scene.test_visibility(allocator, rngf, wavelengthBase,
                                    lightVertex, cameraVertex,
                                    cameraVertex.beta)) {
            renderImage
                .pixel_reference(size_t(cameraVertex.imageCoord.x),
                                 size_t(cameraVertex.imageCoord.y))
                .add(1.0 / double(samplesPerPixel), cameraVertex.beta.data());
          }
        }
      }
    }
    allocator.reset();
  });
}
