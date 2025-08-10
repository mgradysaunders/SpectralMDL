#include "Integrator.h"

void Integrator::integrate_and_write_file(
    const Scene &scene, float imageScale,
    const std::string &imageFileName) const {
  Color wavelengthBase{};
  for (size_t i = 0; i < WAVELENGTH_BASE_MAX; i++) {
    float t = i / float(WAVELENGTH_BASE_MAX - 1);
    wavelengthBase[i] = (1 - t) * WAVELENGTH_MIN + t * WAVELENGTH_MAX;
  }
  auto renderImage{smdl::SpectralRenderImage(WAVELENGTH_BASE_MAX,
                                             scene.camera.imageExtent.x,
                                             scene.camera.imageExtent.y)};
  integrate(scene, wavelengthBase, renderImage);
  auto rgbImage{std::vector<uint8_t>(size_t(scene.camera.imageExtent.x * //
                                            scene.camera.imageExtent.y * 3))};
  auto rgbImageIndex{0};
  for (int y = 0; y < scene.camera.imageExtent.y; y++) {
    for (int x = 0; x < scene.camera.imageExtent.x; x++) {
      auto color{Color()};
      auto pixel{renderImage.pixel_reference(x, y)};
      for (size_t i = 0; i < WAVELENGTH_BASE_MAX; i++)
        color[i] = float(double(pixel[i]));
      smdl::State state{};
      state.wavelength_base = wavelengthBase.data();
      state.wavelength_min = WAVELENGTH_MIN;
      state.wavelength_max = WAVELENGTH_MAX;
      auto rgb{scene.compiler.jit_color_to_rgb(state, color.data())};
      rgb[0] *= imageScale;
      rgb[1] *= imageScale;
      rgb[2] *= imageScale;
      rgb[0] = std::pow(std::fmin(std::fmax(0.0f, rgb[0]), 1.0f), 1.0f / 2.2f);
      rgb[1] = std::pow(std::fmin(std::fmax(0.0f, rgb[1]), 1.0f), 1.0f / 2.2f);
      rgb[2] = std::pow(std::fmin(std::fmax(0.0f, rgb[2]), 1.0f), 1.0f / 2.2f);
      rgbImage[rgbImageIndex++] = std::round(255.0f * rgb[0]);
      rgbImage[rgbImageIndex++] = std::round(255.0f * rgb[1]);
      rgbImage[rgbImageIndex++] = std::round(255.0f * rgb[2]);
    }
  }
  if (auto error{smdl::write_8_bit_image(imageFileName, //
                                         scene.camera.imageExtent.x,
                                         scene.camera.imageExtent.y, 3,
                                         rgbImage.data())}) {
    error->print();
  }
}
