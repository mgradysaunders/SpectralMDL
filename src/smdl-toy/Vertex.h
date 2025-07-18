#pragma once

#include "common.h"

enum Method : int {
  METHOD_FORWARD = 0,
  METHOD_REVERSE = 1,
};

[[nodiscard]] constexpr Method operator!(Method method) noexcept {
  return Method(!int(method));
}

class BPDF final {
public:
  /// The forward value.
  float forward{};

  /// The reverse value.
  float reverse{};

  [[nodiscard]] auto operator[](Method method) noexcept -> float & {
    return (&forward)[int(method)];
  }

  [[nodiscard]] auto operator[](Method method) const noexcept -> const float & {
    return (&forward)[int(method)];
  }
};

class Vertex final {
public:
  [[nodiscard]] bool is_on_same_subpath(const Vertex &other) const noexcept {
    return flags.isOnLightSubpath == other.flags.isOnLightSubpath;
  }

  [[nodiscard]] bool is_on_light_subpath() const noexcept {
    return flags.isOnLightSubpath == 1;
  }

  [[nodiscard]] bool is_on_camera_subpath() const noexcept {
    return flags.isOnLightSubpath == 0;
  }

  [[nodiscard]] bool is_terminal_vertex() const noexcept {
    return flags.isTerminal;
  }

  [[nodiscard]] bool is_surface_scattering_vertex() const noexcept {
    return flags.isSurfaceScattering;
  }

  [[nodiscard]] bool is_volume_scattering_vertex() const noexcept {
    return flags.isVolumeScattering;
  }

  [[nodiscard]] bool is_scattering_vertex() const noexcept {
    return flags.isSurfaceScattering | flags.isVolumeScattering;
  }

  [[nodiscard]] bool scatter_sample(const smdl::float4 &xi,
                                    const smdl::float3 &wo, smdl::float3 &wi,
                                    float &pdfForward, float &pdfReverse,
                                    Color &beta, int &isDelta) const {
    Color f{};
    if (!material->scatter_sample(
            materialInstance, xi,
            smdl::transpose(materialInstance.tangent_space) * wo, wi,
            pdfForward, pdfReverse, &f[0], isDelta) ||
        !(pdfForward > 0)) {
      return false;
    }
    wi = materialInstance.tangent_space * wi;
    wi = smdl::normalize(wi);
    for (size_t i = 0; i < WAVELENGTH_BASE_MAX; i++) {
      if (beta[i] *= f[i] / pdfForward; !std::isfinite(beta[i])) {
        beta[i] = 0.0f;
      }
    }
    return true;
  }

  void calculate_path_PDF(Method method, const Vertex &from) noexcept;

public:
  struct Flags final {
    /// Is on light subpath? If false, on camera subpath.
    uint8_t isOnLightSubpath : 1;

    /// Is terminal vertex?
    uint8_t isTerminal : 1;

    /// Is terminal vertex at infinity?
    uint8_t isTerminalAtInfinity : 1;

    /// Is terminal vertex on intangible surface?
    uint8_t isTerminalOnIntangibleSurface : 1;

    /// Is terminal vertex with delta position?
    uint8_t isTerminalDeltaPosition : 1;

    /// Is terminal vertex with delta direction?
    uint8_t isTerminalDeltaDirection : 1;

    /// Is surface scattering vertex?
    uint8_t isSurfaceScattering : 1;

    /// Is surface scattering vertex with delta direction?
    uint8_t isSurfaceScatteringDeltaDirection : 1;

    /// Is volume scattering vertex?
    uint8_t isVolumeScattering : 1;

    /// Is known to be non-connectible?
    uint8_t isKnownNonConnectible : 1;

  } flags = {.isOnLightSubpath = 0,
             .isTerminal = 0,
             .isTerminalAtInfinity = 0,
             .isTerminalOnIntangibleSurface = 0,
             .isTerminalDeltaPosition = 0,
             .isTerminalDeltaDirection = 0,
             .isSurfaceScattering = 0,
             .isSurfaceScatteringDeltaDirection = 0,
             .isVolumeScattering = 0,
             .isKnownNonConnectible = 0};

  /// The position.
  smdl::float3 position{};

  /// If applicable, the intersection.
  Intersection intersection{};

  /// If applicable, the material.
  const smdl::JIT::Material *material{};

  /// If applicable, the material instance.
  smdl::JIT::Material::Instance materialInstance{};

  /// The Monte-Carlo weight.
  Color beta{};

  /// If applicable, the direction to the previous vertex.
  smdl::float3 omegaO{};

  /// If applicable, the direction to the next vertex.
  smdl::float3 omegaI{};

  /// The direction solid-angle PDF.
  BPDF directionSolidAnglePDF{};

  /// The path-space PDF.
  BPDF pathSpacePDF{};
};
