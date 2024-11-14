#include "smdl/type.h"

namespace smdl {

void Image::flip_horizontal() {
  for (int_t y = 0; y < extent.y; y++) {
    auto *row = texels.data() + y * extent.x;
    for (int_t x = 0; x < extent.x / 2; x++) {
      std::swap(row[x], row[extent.x - 1 - x]);
    }
  }
}

void Image::flip_vertical() {
  for (int_t y = 0; y < extent.y / 2; y++) {
    auto *row0 = texels.data() + y * extent.x;
    auto *row1 = texels.data() + (extent.y - 1 - y) * extent.x;
    for (int_t x = 0; x < extent.x; x++) {
      std::swap(row0[x], row1[x]);
    }
  }
}

void state_t::finalize_for_runtime_conventions() {
  // 1. Orthonormalize the shading normal and tangent vectors. Still in object space!
  normal = normalize(normal);
  for (int i = 0; i < texture_space_max; i++) {
    auto &tu{texture_tangent_u[i]};
    auto &tv{texture_tangent_v[i]};
    auto &tw{normal};
    tu = normalize(tu - dot(tu, tw) * tw);
    tv = normalize(tv - dot(tv, tw) * tw - dot(tv, tu) * tu);
  }

  // 2. Orthonormalize the geometry normal and tangent vectors. Still in object space!
  geometry_normal = normalize(geometry_normal);
  for (int i = 0; i < texture_space_max; i++) {
    auto &tu{geometry_tangent_u[i]};
    auto &tv{geometry_tangent_v[i]};
    auto &tw{geometry_normal};
    tu = normalize(tu - dot(tu, tw) * tw);
    tv = normalize(tv - dot(tv, tw) * tw - dot(tv, tu) * tu);
  }

  // 3. Construct the internal-to-object matrix and matrix inverse.
  internal_to_object_matrix_fwd[0] = {geometry_tangent_u[0].x, geometry_tangent_u[0].y, geometry_tangent_u[0].z, 0.0f};
  internal_to_object_matrix_fwd[1] = {geometry_tangent_v[0].x, geometry_tangent_v[0].y, geometry_tangent_v[0].z, 0.0f};
  internal_to_object_matrix_fwd[2] = {geometry_normal.x, geometry_normal.y, geometry_normal.z, 0.0f};
  internal_to_object_matrix_fwd[3] = {position.x, position.y, position.z, 1.0f};
  internal_to_object_matrix_inv[0] = {geometry_tangent_u[0].x, geometry_tangent_v[0].x, geometry_normal.x, 0.0f};
  internal_to_object_matrix_inv[1] = {geometry_tangent_u[0].y, geometry_tangent_v[0].y, geometry_normal.y, 0.0f};
  internal_to_object_matrix_inv[2] = {geometry_tangent_u[0].z, geometry_tangent_v[0].z, geometry_normal.z, 0.0f};
  internal_to_object_matrix_inv[3] = {
      -dot(geometry_tangent_u[0], position), //
      -dot(geometry_tangent_v[0], position), //
      -dot(geometry_normal, position),       //
      1.0f};

  // 4. Convert all relevant quantities from object space to internal space.
  position = {};
  geometry_normal = {0, 0, 1};
  normal = transform_affine(internal_to_object_matrix_inv, normal, 0.0f);
  motion = transform_affine(internal_to_object_matrix_inv, motion, 0.0f);
  direction = transform_affine(internal_to_object_matrix_inv, direction, 0.0f);
  for (int i = 0; i < texture_space_max; i++) {
    texture_tangent_u[i] = transform_affine(internal_to_object_matrix_inv, texture_tangent_u[i], 0.0f);
    texture_tangent_v[i] = transform_affine(internal_to_object_matrix_inv, texture_tangent_v[i], 0.0f);
    geometry_tangent_u[i] = transform_affine(internal_to_object_matrix_inv, geometry_tangent_u[i], 0.0f);
    geometry_tangent_v[i] = transform_affine(internal_to_object_matrix_inv, geometry_tangent_v[i], 0.0f);
  }

  // 5. Cache the internal-to-world and world-to-internal transformations.
  internal_to_world_matrix_fwd = object_to_world_matrix_fwd * internal_to_object_matrix_fwd;
  internal_to_world_matrix_inv = internal_to_object_matrix_inv * object_to_world_matrix_inv;
}

} // namespace smdl
