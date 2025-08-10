#include "BDPTIntegrator.h"

void BDPTIntegrator::integrate(const Scene &scene, const Color &wavelengthBase,
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
    auto cameraPath{std::vector<Vertex>(size_t(maxOrder + 2))};
    auto lightPath{std::vector<Vertex>(size_t(maxOrder + 2))};
    for (size_t sample = 0; sample < samplesPerPixel; sample++) {
      auto cameraPathLen{scene.trace_path_from_camera(
          allocator, rngf, wavelengthBase,
          smdl::float2(static_cast<float>(pixelIndexX) + rngf(),
                       static_cast<float>(pixelIndexY) + rngf()),
          cameraPath.size(), cameraPath.data())};
      auto lightPathLen{scene.trace_path_from_light(
          allocator, rngf, wavelengthBase, lightPath.size(), lightPath.data())};
      for (size_t s = 0; s < cameraPathLen; s++) {
        for (size_t t = 0; t < lightPathLen; t++) {
          size_t order{s + t};
          if (!(minOrder <= order && order <= maxOrder))
            continue;
          Color beta{};
          float misWeight{};
          smdl::float2 imageCoord{cameraPath[0].imageCoord};
          if (connect_bidirectional(scene, allocator, rngf, wavelengthBase,
                                    &cameraPath[s], &lightPath[t], beta,
                                    misWeight, imageCoord)) {
            renderImage
                .pixel_reference(size_t(imageCoord.x), size_t(imageCoord.y))
                .add(misWeight / float(samplesPerPixel), beta.data());
          }
        }
      }
      allocator.reset();
    }
  });
}
