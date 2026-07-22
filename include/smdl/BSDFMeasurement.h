/// \file
#pragma once

#include <unordered_map>

#include "smdl/Common.h"
#include "smdl/Support/Sampling.h"

namespace smdl {

/// \addtogroup Main
/// \{

/// A BSDF measurement (`.mbsdf`).
class SMDL_EXPORT BSDFMeasurement final {
public:
  enum Kind { KIND_REFLECTION, KIND_TRANSMISSION };

  enum Type { TYPE_FLOAT, TYPE_FLOAT3 };

  [[nodiscard]] static constexpr size_t size_of(Type type) noexcept {
    switch (type) {
    case TYPE_FLOAT:
      static_assert(sizeof(float) == 4);
      return sizeof(float);
    case TYPE_FLOAT3:
      static_assert(sizeof(float3) == 16); // Aligned to 16
      return sizeof(float3);
    default:
      return 0;
    }
  }

  BSDFMeasurement() = default;

  BSDFMeasurement(const BSDFMeasurement &) = delete;

  ~BSDFMeasurement() { clear(); }

  /// Load from file memory.
  [[nodiscard]]
  std::optional<Error> loadFromFileMemory(const std::string &file) noexcept;

  /// Load from file.
  [[nodiscard]]
  std::optional<Error> loadFromFile(const std::string &fileName) noexcept;

  void clear() noexcept;

  /// The raw table fetch, always widened to `float3`.
  ///
  /// The table is indexed by `(iThetao * numTheta + iThetai) * numPhi + iPhi`,
  /// with each index clamped into range. For `TYPE_FLOAT` measurements the
  /// scalar value is splatted across all three components.
  ///
  [[nodiscard]] float3 fetch(int iThetao, int iThetai, int iPhi) const noexcept;

  /// Interpolate the measurement at the given angles.
  ///
  /// The table is understood to be sampled at cell centers, with the zenith
  /// angles `thetao` and `thetai` equally spaced over `[0, pi/2]` and the
  /// azimuth difference angle `phi` equally spaced over `[0, pi]`, and is
  /// interpolated trilinearly with clamping at the boundaries.
  ///
  [[nodiscard]] float3 interpolate(float thetao, float thetai,
                                   float phi) const noexcept;

  /// Interpolate the measurement for the given directions.
  ///
  /// The directions are interpreted in the hemisphere-agnostic sense: the
  /// zenith angles are formed from `|wo.z|` and `|wi.z|`, and the azimuth
  /// difference is wrapped into `[0, pi]`, so the caller is responsible for
  /// orienting directions consistently with `kind`.
  ///
  [[nodiscard]] float3 interpolate(float3 wo, float3 wi) const noexcept;

  /// The direction PDF of `directionSample()` with respect to solid angle,
  /// conditioned on the outgoing direction `wo`.
  [[nodiscard]] float directionPDF(float3 wo, float3 wi) const noexcept;

  /// The direction sampling routine, importance sampling the tabulated
  /// measurement (weighted by the projected solid angle) conditioned on the
  /// outgoing direction `wo`.
  ///
  /// \param[in] xi
  /// The random sample \f$ \xi \in (0,1)^2 \f$.
  ///
  /// \param[in] wo
  /// The outgoing direction.
  ///
  /// \param[out] pdf
  /// If non-null, receives the associated PDF.
  ///
  /// \returns
  /// The sampled incoming direction, always in the upper hemisphere. The
  /// caller is responsible for flipping it below the horizon for
  /// `KIND_TRANSMISSION` measurements.
  ///
  [[nodiscard]] float3 directionSample(float2 xi, float3 wo,
                                       float *pdf = {}) const noexcept;

public:
  /// The kind.
  Kind kind{KIND_REFLECTION};

  /// The type.
  Type type{TYPE_FLOAT};

  /// The number of samples in zenith.
  size_t numTheta{};

  /// The number of samples in azimuth.
  size_t numPhi{};

  /// The buffer of `numTheta * numTheta * numPhi` samples either of type
  /// `float` or `float3`.
  void *buffer{};

  /// The meta-data.
  std::unordered_map<std::string, std::string> metaData;

  /// The sampling distributions, one per outgoing zenith angle cell.
  ///
  /// Each is built on load over the `numTheta` by `numPhi` table slice for
  /// the corresponding outgoing angle, weighted by the projected solid angle
  /// of the incoming direction, and backs `directionPDF()` and
  /// `directionSample()`.
  ///
  std::vector<Distribution2D> distributions;
};

/// \}

} // namespace smdl
