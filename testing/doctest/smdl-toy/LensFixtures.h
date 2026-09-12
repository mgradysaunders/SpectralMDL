/// \file
/// Published lens prescriptions, spelled the way a `.lens` file spells
/// them, for the cases that need a real design rather than a lens that is
/// merely present.
#pragma once

#include "Layout/LensFile.h"

/// A surface, spelled the way the file spells one: millimeters, radius
/// signed toward the film, index of the space behind it.
[[nodiscard]] inline LensSurface surfaceOf(float radius, float thickness,
                                           float ior, float diameter) {
  LensSurface surface{};
  surface.radius = radius;
  surface.thickness = thickness;
  surface.medium = smdl::OpticalGlass::constant(ior);
  surface.diameter = diameter;
  return surface;
}

[[nodiscard]] inline LensSurface stopOf(float thickness, float diameter) {
  LensSurface surface{surfaceOf(0, thickness, 1, diameter)};
  surface.isStop = true;
  return surface;
}

/// The double Gauss pbrt distributes, from US 2,673,491 (Tronnier) by way
/// of Modern Lens Design p.312, scaled to 50 mm, as
/// `etc/lenses/dgauss-50mm.lens` ships it. A real design whose focal
/// length and f-number are printed on it, which is what makes it the
/// transcription check.
[[nodiscard]] inline LensPrescription dgauss50mm() {
  LensPrescription lens{};
  lens.name = "Double Gauss 50mm f/2";
  lens.surfaces = {
      surfaceOf(29.475f, 3.76f, 1.67f, 25.2f),
      surfaceOf(84.83f, 0.12f, 1, 25.2f),
      surfaceOf(19.275f, 4.025f, 1.67f, 23.0f),
      surfaceOf(40.77f, 3.275f, 1.699f, 23.0f),
      surfaceOf(12.75f, 5.705f, 1, 18.0f),
      stopOf(4.5f, 17.1f),
      surfaceOf(-14.495f, 1.18f, 1.603f, 17.0f),
      surfaceOf(40.77f, 6.065f, 1.658f, 20.0f),
      surfaceOf(-20.385f, 0.19f, 1, 20.0f),
      surfaceOf(437.065f, 3.22f, 1.717f, 20.0f),
      surfaceOf(-39.73f, 0, 1, 20.0f),
  };
  return lens;
}
