#include "Vertex.h"

void Vertex::calculate_path_PDF(Method method, const Vertex &from) noexcept {
  if (flags.isTerminalOnIntangibleSurface) {
    pathSpacePDF[method] = 0.0f;
  } else if (from.flags.isSurfaceScatteringDeltaDirection ||
             from.flags.isTerminalDeltaDirection) {
    // TODO?
    pathSpacePDF[method] = 1.0f;
  } else {
    float factor{1.0f};
    if (!flags.isTerminalAtInfinity) {
      // If not a terminal vertex at infinity, pick up volume
      // factor of inverse distance squared.
      auto separation{position - from.position};
      auto separationDistanceSquared{smdl::length_squared(separation)};
      factor /= separationDistanceSquared;
      // If on a surface, pick up projected area factor.
      if (flags.isSurfaceScattering) {
        factor *= std::abs(smdl::dot(separation, intersection.geometryNormal)) /
                  std::sqrt(separationDistanceSquared);
      }
    }
    pathSpacePDF[method] =
        factor * from.directionSolidAnglePDF[(
                     is_on_same_subpath(from) ? method : !method)];
  }
}
