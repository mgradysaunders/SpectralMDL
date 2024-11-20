#pragma once

#include "smdl/common.h"

namespace smdl {

// intensity_mode
enum class intensity_mode_t : int_t {
  intensity_radiant_exitance = 0,

  intensity_power = 1,
};

struct default_bsdf_t final {};

struct default_vdf_t final {};

struct default_edf_t final {};

struct default_hair_bsdf_t final {};

struct material_emission_t final {};

struct material_surface_t final {};

struct material_volume_t final {};

struct material_geometry_t final {};

struct material_t final {};

struct bsdf_measurement_t final {
  void *ptr{};
};

struct light_profile_t final {
  void *ptr{};
};

struct string_t final {
  const void *ptr{};

  int_t len{};

  [[nodiscard]] operator std::string_view() const { return std::string_view(static_cast<const char *>(ptr), len); }

  [[nodiscard]] operator std::string() const { return std::string(std::string_view(*this)); }
};

struct source_location_t final {
  string_t file{};

  int_t line{};
};

struct image_t final {
  int2_t extent{};

  float4_t *texels{};
};

struct texture_2d_t final {
  int_t gamma{};

  int2_t tile_count{};

  image_t *tiles{};
};

struct texture_3d_t final {
  int_t gamma{};

  int3_t extent{};

  int3_t stride{};

  float4_t *texels{};
};

struct texture_cube_t final {
  int_t gamma{};

  int2_t extent{};

  float4_t **texels{};
};

struct Ptexture_t final {
  void *texture{};

  void *filter{};
};

struct texture_ptex_t final {
  int_t gamma{};

  void *ptr{};
};

struct SMDL_EXPORT state_t final {
public:
  /// The position in object space.
  float3_t position{};

  /// The normal in object space.
  float3_t normal{0, 0, 1};

  /// The geometry normal in object space.
  float3_t geometry_normal{0, 0, 1};

  float3_t motion{};

  static constexpr size_t TEXTURE_SPACE_MAX = 4;

  int_t texture_space_max{1};

  float3_t texture_coordinate[TEXTURE_SPACE_MAX]{};

  // The texture tangent U vector(s) in object space.
  float3_t texture_tangent_u[TEXTURE_SPACE_MAX] = {float3_t{1, 0, 0}, float3_t{1, 0, 0}, float3_t{1, 0, 0}, float3_t{1, 0, 0}};

  // The texture tangent V vector(s) in object space.
  float3_t texture_tangent_v[TEXTURE_SPACE_MAX] = {float3_t{0, 1, 0}, float3_t{0, 1, 0}, float3_t{0, 1, 0}, float3_t{0, 1, 0}};

  // The geometry tangent U vector(s) in object space.
  float3_t geometry_tangent_u[TEXTURE_SPACE_MAX] = {float3_t{1, 0, 0}, float3_t{1, 0, 0}, float3_t{1, 0, 0}, float3_t{1, 0, 0}};

  // The geometry tangent V vector(s) in object space.
  float3_t geometry_tangent_v[TEXTURE_SPACE_MAX] = {float3_t{0, 1, 0}, float3_t{0, 1, 0}, float3_t{0, 1, 0}, float3_t{0, 1, 0}};

  /// The object ID.
  int_t object_id{};

  /// The Ptex face ID if applicable.
  int_t ptex_face_id{};

  /// The Ptex face UV if applicable.
  float2_t ptex_face_uv{};

  float3_t direction{};

  /// The animation time.
  float_t animation_time{0.0f};

  /// The wavelengths in nanometers.
  const float_t *wavelength_base{};

  /// The wavelength range minimum in nanometers.
  float_t wavelength_min{380.0f};

  /// The wavelength range maximum in nanometers.
  float_t wavelength_max{720.0f};

  /// The meters per scene unit.
  float_t meters_per_scene_unit{1.0f};

  /// The object-to-world matrix.
  float4x4_t object_to_world_matrix_fwd{float4x4_t(1.0f)};

  /// The world-to-object matrix.
  float4x4_t object_to_world_matrix_inv{float4x4_t(1.0f)};

  /// The internal-to-object matrix.
  float4x4_t internal_to_object_matrix_fwd{float4x4_t(1.0f)};

  /// The internal-to-object matrix inverse.
  float4x4_t internal_to_object_matrix_inv{float4x4_t(1.0f)};

  /// The internal-to-world matrix.
  float4x4_t internal_to_world_matrix_fwd{float4x4_t(1.0f)};

  /// The internal-to-world matrix inverse.
  float4x4_t internal_to_world_matrix_inv{float4x4_t(1.0f)};

public:
  void finalize_for_runtime_conventions();
};


} // namespace smdl
