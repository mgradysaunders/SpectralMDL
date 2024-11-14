#pragma once

namespace smdl::Compiler::builtins {

static const char *df = R"*(
#extended_syntax
using ::math import *;
import ::state::*;
export enum scatter_mode { scatter_reflect = 1, scatter_transmit = 2, scatter_reflect_transmit = 3 };
@(pure) bool weighted_bool_sample(&float u, float weight) {
  return *u < weight ? (*u = (*u / weight), true) : (*u = (*u - weight) / (1 - weight), false);
}
@(pure) int uniform_wavelength_index_sample(&float u) {
  const int i(#min(int(*u *= $WAVELENGTH_BASE_MAX), $WAVELENGTH_BASE_MAX - 1));
  *u -= i;
  return i;
}
@(pure) float2 uniform_disk_sample(float2 u) {
  u = 2 * u - 1;
  return float2(0) if (#all(u == 0));
  float rad(0);
  float phi(0);
  if ((absu := #abs(u), absu.x > absu.y))
    rad = u.x, phi = $PI / 4 * (u.y / u.x);
  else
    rad = u.y, phi = $PI / 2 - $PI / 4 * (u.x / u.y);
  return rad * float2(#cos(phi), #sin(phi));
}
@(pure) auto cosine_hemisphere_sample(float2 u) {
  return float3((p := uniform_disk_sample(u)), #sqrt(#max(1 - #sum(p * p), 0)));
}
@(pure macro) auto reflect(const float3 wi, const float3 wm) = 2 * #sum(wi * wm) * wm - wi;
@(pure macro) auto refract(const float3 wi, const float3 wm, const float ior, const float cos_thetat) = -ior * wi + (ior * #sum(wi * wm) + cos_thetat) * wm;
@(pure macro) auto refract(const float3 wi, const float3 wm, const float ior) {
  const auto cos_thetai(#sum(wi * wm));
  const auto cos_thetat(#sqrt(1 - ior * ior * (1 - cos_thetai * cos_thetai)));
  return refract(wi, wm, ior, cos_thetat * -#sign(cos_thetai));
}
@(pure macro) auto refraction_half_vector(const float3 wi, const float3 wt, const float ior) = -ior * wi + wt; 
@(pure macro) auto refraction_half_vector_jacobian(const float3 wi, const float3 wt, const float ior) {
  const auto vh(refraction_half_vector(wi, wt, ior));
  const auto len2_vh(#sum(vh * vh));
  const auto len1_vh(#sqrt(len2_vh));
  const auto wh(vh / len1_vh);
  return #abs(#sum(wt * wh)) / len2_vh;
}
@(pure macro) float4 quat_rotate(const float theta, const float3 v) = float4(#sin(theta / 2) * normalize(v), #cos(theta / 2));
@(pure macro) float4 quat_rotate(const float3 u, const float3 v) = normalize(float4(cross(u, v), 1 + dot(u, v)));
@(pure macro) float4 quat_transpose(const float4 q) = float4(-1.0, -1.0, -1.0, 1.0) * q;
@(pure macro) float4 quat_inverse(const float4 q) = quat_transpose(q) / dot(q, q);
@(pure macro) float4 quat_product(const float4 q, const float4 r) = float4(1.0, 1.0, 1.0, -1.0) * (q.wwwx * r.xyzx + q.xyzy * r.wwwy + q.yzxz * r.zxyz - q.zxyw * r.yzxw);
@(pure) float3 quat_transform_vector(const float4 q, const float3 u) {
  return (w := q.w) * w * u + (v := q.xyz) * dot(v, u) + cross(v, 2.0 * w * u + cross(v, u));
}
struct eval_bsdf_parameters {
  const float3 geometric_wo;
  const float3 geometric_wi;
  float3 wo = geometric_wo;
  float3 wi = geometric_wi;
  const float hit_side = #sign(geometric_wo.z);
  const float ior = 1.0 / 1.4;
  const bool thin_walled = false;
  const scatter_mode mode = (geometric_wo.z < 0) == (geometric_wi.z < 0) ? scatter_reflect : scatter_transmit;
};
struct eval_bsdf_result {
  float2 pdf = float2(0);
  color f = color(0);
  bool is_black = false;
};
struct eval_bsdf_sample_parameters {
  const float3 geometric_wo;
  float3 wo = geometric_wo;
  float4 xi;
  const float hit_side = #sign(geometric_wo.z);
  const float ior = 1.0 / 1.4;
  const bool thin_walled = false;
};
struct eval_bsdf_sample_result {
  float3 wi = float3(0);
  scatter_mode mode = scatter_none;
  ?color delta_f = null;
};
@(macro) auto eval_bsdf(const &default_bsdf this, inline const &eval_bsdf_parameters params) {
  return eval_bsdf_result(is_black: true);
}
@(macro) auto eval_bsdf_sample(const &default_bsdf this, inline const &eval_bsdf_sample_parameters params) {
  return eval_bsdf_sample_result(wi: float3(0.0), mode: scatter_none);
}
export struct diffuse_reflection_bsdf: bsdf {
  color tint = color(1.0);
  float roughness = 0.0;
  string handle = "";
};
auto eval_bsdf(const &diffuse_reflection_bsdf this, inline const &eval_bsdf_parameters params) {
  if (mode == scatter_reflect) {
    const auto sigma2(this.roughness * this.roughness);
    const auto a(1.00 - sigma2 / (2.0 * sigma2 + 0.66));
    const auto b(0.45 * sigma2 / (sigma2 + 0.09));
    const auto z(#abs(auto(wi.z, wo.z)));
    const auto t(#max(#sum(wo.xy * wi.xy), 0) / #max_value(z));
    return eval_bsdf_result(pdf: (pdf := z / $PI), f: pdf[0] * (a + t * b) * this.tint);
  } else {
    return eval_bsdf_result(is_black: true);
  }
}
auto eval_bsdf_sample(const &diffuse_reflection_bsdf this, inline const &eval_bsdf_sample_parameters params) {
  return eval_bsdf_sample_result(wi: hit_side * cosine_hemisphere_sample(xi.xy), mode: scatter_reflect);
}
export struct diffuse_transmission_bsdf: bsdf {
  color tint = color(1.0);
  string handle = "";
};
auto eval_bsdf(const &diffuse_transmission_bsdf this, inline const &eval_bsdf_parameters params) {
  if (mode == scatter_transmit) {
    return eval_bsdf_result(pdf: (pdf := #abs(auto(wi.z, wo.z)) / $PI), f: pdf[0] * this.tint);
  } else {
    return eval_bsdf_result(is_black: true);
  }
}
auto eval_bsdf_sample(const &diffuse_transmission_bsdf this, inline const &eval_bsdf_sample_parameters params) {
  return eval_bsdf_sample_result(wi: -hit_side * cosine_hemisphere_sample(xi.xy), mode: scatter_transmit);
}
export struct sheen_bsdf: bsdf {
  float roughness;
  color tint = color(1.0);
  color multiscatter_tint = color(0.0);
  string handle = "";
};
@(pure) auto sheen_l(const auto fit, const float cos_theta) {
  return fit[0] / (1 + fit[1] * #pow(cos_theta, fit[2])) + fit[3] * cos_theta + fit[4];
}
@(pure) auto sheen_lambda(const auto fit, const float cos_theta) {
  return #exp(#abs(cos_theta) < 0.5 ? sheen_l(fit, cos_theta) : 2 * sheen_l(fit, 0.5) - sheen_l(fit, #max(1 - cos_theta, 0)));
}
auto eval_bsdf(const &sheen_bsdf this, inline const &eval_bsdf_parameters params) {
  if (mode == scatter_reflect) {
    const auto alpha(saturate(lerp(0.1, 1.0, this.roughness * this.roughness)));
    const auto fit(lerp(auto(21.5473, 3.82987, 0.19823, -1.97760, -4.32054),
                        auto(25.3245, 3.32435, 0.16801, -1.27393, -4.85967), (1 - alpha) * (1 - alpha)));
    const auto z(#abs(auto(wi.z, wo.z)));
    const auto wh(normalize(wo + wi));
    const auto cos_thetah(wh.z);
    const auto sin_thetah(#sqrt(1.001 - cos_thetah * cos_thetah)); 
    const auto d((2 + 1 / alpha) * #pow(sin_thetah, 1 / alpha) / $TWO_PI);
    const auto g(1 / (1 + sheen_lambda(fit, z[0]) + sheen_lambda(fit, z[1])));
    return eval_bsdf_result(pdf: z / $PI, f: d * g / (4 * z[1]) * this.tint);
  } else {
    return eval_bsdf_result(is_black: true);
  }
}
auto eval_bsdf_sample(const &sheen_bsdf this, const &eval_bsdf_sample_parameters params) {
  return eval_bsdf_sample_result(wi: cosine_hemisphere_sample(params.xi.xy) * params.hit_side, mode: scatter_reflect);
}
export @(macro) int material__eval_bsdf(
    const &material this, const &float3 wo, const &float3 wi,  
    const &float pdf_fwd, const &float pdf_rev, const &color f) {
  auto params(eval_bsdf_parameters(
    geometric_wo: normalize(*wo),
    geometric_wi: normalize(*wi)));
  /*
  perturb_normal(&params, this.geometry.normal);
   */
  auto result(eval_bsdf_result(is_black: true));
  if (#typeof(this.backface) != #typeof(material_surface()) && (params.hit_side < 0)) { 
    result = eval_bsdf(&this.backface.scattering, &params);
  } else {
    result = eval_bsdf(&this.surface.scattering, &params);
  }
  if (result.is_black) {
    *pdf_fwd = 0.0;
    *pdf_rev = 0.0;
    *f = color(0.0);
  } else {
    *pdf_fwd = result.pdf.x;
    *pdf_rev = result.pdf.y;
    *f = result.f;
  }
  return !result.is_black;
}
/*
export @(macro) int material__bsdf_sample(
    const &material this, const &float4 xi, const &float3 wo, 
    const &float3 wi, const &float pdf_fwd, const &float pdf_rev, const &color f, const &int is_delta) {
}
 */
)*";

static const char *debug = R"*(#extended_syntax
export @(pure macro) bool assert(const bool condition, const string reason) {
  #assert(condition, reason) if ($DEBUG);
  return true;
}
export @(pure macro) bool breakpoint() = #breakpoint();
export @(pure macro) bool print(const auto a) = #print(a);
)*";

static const char *limits = R"*(#extended_syntax
export const int INT_MIN = $INT_MIN;
export const int INT_MAX = $INT_MAX;
export const float FLOAT_MIN = $FLOAT_MIN;
export const float FLOAT_MAX = $FLOAT_MAX;
export const double DOUBLE_MIN = $DOUBLE_MIN;
export const double DOUBLE_MAX = $DOUBLE_MAX;
)*";

static const char *math = R"*(#extended_syntax
export const float PI = $PI;
export const float TWO_PI = $TWO_PI;
export const float HALF_PI = $HALF_PI;
export @(pure macro) auto abs(const auto a) = #abs(a);
export @(pure macro) auto all(const auto a) = #all(a);
export @(pure macro) auto any(const auto a) = #any(a);
export @(pure macro) auto max(const auto a, const auto b) = #max(a, b);
export @(pure macro) auto min(const auto a, const auto b) = #min(a, b);
export @(pure macro) auto clamp(const auto a, const auto min, const auto max) = #max(min, #min(a, max));
export @(pure macro) auto saturate(const auto a) = clamp(a, 0.0, 1.0);
export @(pure macro) auto floor(const auto a) = #floor(a);
export @(pure macro) auto ceil(const auto a) = #ceil(a);
export @(pure macro) auto round(const auto a) = #round(a);
export @(pure macro) auto trunc(const auto a) = #trunc(a);
export @(pure macro) auto fmod(const auto a, const auto b) = a % b;
export @(pure macro) auto isfinite(const auto a) = #isfpclass(a, 0b0111111000);
export @(pure macro) auto isnormal(const auto a) = #isfpclass(a, 0b0100001000);
export @(pure macro) auto isinf(const auto a) = #isfpclass(a, 0b1000000100);
export @(pure macro) auto isnan(const auto a) = #isfpclass(a, 0b0000000011);
export @(pure macro) auto cos(const auto a) = #cos(a);
export @(pure macro) auto sin(const auto a) = #sin(a);
export @(pure macro) auto tan(const auto a) = #tan(a);
export @(pure macro) auto acos(const auto a) = #acos(a);
export @(pure macro) auto asin(const auto a) = #asin(a);
export @(pure macro) auto atan(const auto a) = #atan(a);
export @(pure macro) auto cosh(const auto a) = #cosh(a);
export @(pure macro) auto sinh(const auto a) = #sinh(a);
export @(pure macro) auto tanh(const auto a) = #tanh(a);
export @(pure macro) auto atan2(const auto y, const auto x) = #atan2(y, x);
export @(pure macro) auto sincos(const auto a) = auto[2](#sin(a), #cos(a));
export @(pure macro) auto radians(const auto a) = a * (PI / 180.0);
export @(pure macro) auto degrees(const auto a) = a * (180.0 / PI);
export @(pure macro) auto exp(const auto a) = #exp(a);
export @(pure macro) auto exp2(const auto a) = #exp2(a);
export @(pure macro) auto exp10(const auto a) = #exp10(a);
export @(pure macro) auto log(const auto a) = #log(a);
export @(pure macro) auto log2(const auto a) = #log2(a);
export @(pure macro) auto log10(const auto a) = #log10(a);
export @(pure macro) auto min_value(const auto a) = #min_value(a);
export @(pure macro) auto max_value(const auto a) = #max_value(a);
export @(pure macro) auto lerp(const auto a, const auto b, const auto l) = (1.0 - l) * a + l * b;
export @(pure macro) auto step(const auto a, const auto b) = #select(b < a, 0.0, 1.0);
export @(pure macro) auto smoothstep(const auto a, const auto b, const auto l) { 
  const auto t(saturate(l)), s(1 - t);
  return s * s * (1 + 2 * t) * a + t * t * (1 + 2 * s) * b;
}
export @(pure macro) auto dot(const auto a, const auto b) = #sum(a * b);
export @(pure macro) auto length(const auto a) = #sqrt(#sum(a * a));
export @(pure macro) auto normalize(const auto a) = a * (1 / length(a));
export @(pure macro) auto distance(const auto a, const auto b) = length(b - a);
export @(pure macro) auto cross(const auto a, const auto b) = a.yzx * b.zxy - a.zxy * b.yzx;
export @(noinline) color blackbody(const float temperature) {
  const auto t(color($state.wavelength_base) * (temperature / 14.387e6));
  auto res(1 + 2 * t);
  res = 1 + 3 * t * res;
  res = 1 + 4 * t * res;
  res = 1 + 5 * t * res;
  const auto rcp1(1 / t);
  auto rcp(rcp1 / 6);
  for (int k = 1; k < 10; ++k) {
    res += rcp;
    rcp *= rcp1 / (6 + k);
  }
  return 5.659994086 / res;
}
)*";

static const char *scene = R"*(#extended_syntax
@(pure foreign) int smdl_data_isvalid(&void data, &string name);
@(pure foreign) int smdl_data_lookup_int(&void data, &string name, &int value);
@(pure foreign) int smdl_data_lookup_int2(&void data, &string name, &int2 value);
@(pure foreign) int smdl_data_lookup_int3(&void data, &string name, &int3 value);
@(pure foreign) int smdl_data_lookup_int4(&void data, &string name, &int4 value);
@(pure foreign) int smdl_data_lookup_float(&void data, &string name, &float value);
@(pure foreign) int smdl_data_lookup_float2(&void data, &string name, &float2 value);
@(pure foreign) int smdl_data_lookup_float3(&void data, &string name, &float3 value);
@(pure foreign) int smdl_data_lookup_float4(&void data, &string name, &float4 value);
@(pure foreign) int smdl_data_lookup_color(&void data, &string name, &color value);
export @(pure macro) bool data_isvalid(string name) = smdl_data_isvalid($data, &name) != 0;
export @(pure macro) int data_lookup_int(string name, int default_value = int()) {
  auto value(default_value);
  smdl_data_lookup_int($data, &name, &value);
  return value;
}
export @(pure macro) int2 data_lookup_int2(string name, int2 default_value = int2()) {
  auto value(default_value);
  smdl_data_lookup_int2($data, &name, &value);
  return value;
}
export @(pure macro) int3 data_lookup_int3(string name, int3 default_value = int3()) {
  auto value(default_value);
  smdl_data_lookup_int3($data, &name, &value);
  return value;
}
export @(pure macro) int4 data_lookup_int4(string name, int4 default_value = int4()) {
  auto value(default_value);
  smdl_data_lookup_int4($data, &name, &value);
  return value;
}
export @(pure macro) float data_lookup_float(string name, float default_value = float()) {
  auto value(default_value);
  smdl_data_lookup_float($data, &name, &value);
  return value;
}
export @(pure macro) float2 data_lookup_float2(string name, float2 default_value = float2()) {
  auto value(default_value);
  smdl_data_lookup_float2($data, &name, &value);
  return value;
}
export @(pure macro) float3 data_lookup_float3(string name, float3 default_value = float3()) {
  auto value(default_value);
  smdl_data_lookup_float3($data, &name, &value);
  return value;
}
export @(pure macro) float4 data_lookup_float4(string name, float4 default_value = float4()) {
  auto value(default_value);
  smdl_data_lookup_float4($data, &name, &value);
  return value;
}
)*";

static const char *state = R"*(#extended_syntax
import ::math::*;
export enum coordinate_space { coordinate_internal = 0, coordinate_object = 1, coordinate_world = 2 };
export @(macro) float3 position() = $state.position;
export @(macro) float3 normal() = $state.normal;
export @(macro) float3 geometry_normal() = $state.geometry_normal;
export @(macro) float3 motion() = $state.motion;
export @(macro) int texture_space_max() = $state.texture_space_max;
export @(macro) float3 texture_coordinate(const int i) = $state.texture_coordinate[i];
export @(macro) float3 texture_tangent_u(const int i) = $state.texture_tangent_u[i];
export @(macro) float3 texture_tangent_v(const int i) = $state.texture_tangent_v[i];
export @(macro) float3 geometry_tangent_u(const int i) = $state.geometry_tangent_u[i];
export @(macro) float3 geometry_tangent_v(const int i) = $state.geometry_tangent_v[i];
export @(macro) float3x3 tangent_space(const int i) = float3x3($state.texture_tangent_u[i], $state.texture_tangent_v[i], $state.normal);
export @(macro) float3x3 geometry_tangent_space(const int i) = float3x3($state.geometry_tangent_u[i], $state.geometry_tangent_v[i], $state.geometry_normal);
export @(macro) int object_id() = $state.object_id;
export @(macro) float3 direction() = $state.direction;
export @(macro) float animation_time() = $state.animation_time;
export const int WAVELENGTH_BASE_MAX = $WAVELENGTH_BASE_MAX;
export @(macro) float wavelength_min() = $state.wavelength_min;
export @(macro) float wavelength_max() = $state.wavelength_max;
export @(macro) float[WAVELENGTH_BASE_MAX] wavelength_base() = $state.wavelength_base;
export @(macro) float meters_per_scene_unit() = $state.meters_per_scene_unit;
export @(macro) float scene_units_per_meter() = 1.0 / $state.meters_per_scene_unit;
)*";

static const char *std = R"*(#extended_syntax
export using ::debug import *;
export using ::df import *;
export using ::limits import *;
export using ::math import *;
export using ::scene import *;
export using ::state import *;
export using ::tex import *;
)*";

static const char *tex = R"*(#extended_syntax
export enum gamma_mode { gamma_default = 0, gamma_linear = 1, gamma_srgb = 2 };
@(pure macro) float4 apply_gamma_mode(const gamma_mode gamma, const float4 texel) = gamma == gamma_srgb ? float4((texel * texel).xyz, texel.w) : texel;
@(pure) &image_t access_uv_tile(const texture_2d tex, const int2 uv_tile) {
  return null if (#any((uv_tile < 0) | (uv_tile >= tex.tile_count)));
  return &tex.tiles[tex.tile_count.x * uv_tile.y + uv_tile.x];
}
export @(pure macro) int width(const texture_2d tex, const int2 uv_tile = int2(0)) = (tile := access_uv_tile(tex, uv_tile)) ? tile.extent.x : 0;
export @(pure macro) int width(const texture_3d tex) = tex.extent.x;
export @(pure macro) int width(const texture_cube tex) = tex.extent.x;
export @(pure macro) int height(const texture_2d tex, const int2 uv_tile = int2(0)) = (tile := access_uv_tile(tex, uv_tile)) ? tile.extent.y : 0;
export @(pure macro) int height(const texture_3d tex) = tex.extent.y;
export @(pure macro) int height(const texture_cube tex) = tex.extent.y;
export @(pure macro) int depth(const texture_3d tex) = tex.extent.z;
export @(pure macro) bool texture_isvalid(const texture_2d tex) = bool(tex.tiles);
export @(pure macro) bool texture_isvalid(const texture_3d tex) = bool(tex.texels);
export @(pure macro) bool texture_isvalid(const texture_cube tex) = bool(tex.texels);
export @(pure macro) bool texture_isvalid(const texture_ptex tex) = bool(tex.ptr);
@(pure) float4 access_texel(const texture_2d tex, const int2 coord, const int2 uv_tile = int2(0)) {
  const auto tile(access_uv_tile(tex, uv_tile));
  return null if (!tile || #any((coord < 0) | (coord >= tile.extent)));
  return tile.texels[tile.extent.x * coord.y + coord.x];
}
export @(pure macro) float4 texel_float4(const texture_2d tex, const int2 coord, const int2 uv_tile = int2(0)) = access_texel(tex, coord, uv_tile);
export @(pure macro) float3 texel_float3(const texture_2d tex, const int2 coord, const int2 uv_tile = int2(0)) = access_texel(tex, coord, uv_tile).xyz;
export @(pure macro) float2 texel_float2(const texture_2d tex, const int2 coord, const int2 uv_tile = int2(0)) = access_texel(tex, coord, uv_tile).xy;
export @(pure macro) float texel_float(const texture_2d tex, const int2 coord, const int2 uv_tile = int2(0)) = access_texel(tex, coord, uv_tile).x;
export @(pure macro) color texel_color(const texture_2d tex, const int2 coord, const int2 uv_tile = int2(0)) = color(access_texel(tex, coord, uv_tile).xyz);
@(pure) float4 access_texel(const texture_3d tex, const int3 coord) {
  return null if (#any((coord < 0) | (coord >= tex.extent)));
  return tex.texels[#sum(tex.stride * coord)];
}
export @(pure macro) float4 texel_float4(const texture_3d tex, const int3 coord) = access_texel(tex, coord);
export @(pure macro) float3 texel_float3(const texture_3d tex, const int3 coord) = access_texel(tex, coord).xyz;
export @(pure macro) float2 texel_float2(const texture_3d tex, const int3 coord) = access_texel(tex, coord).xy;
export @(pure macro) float texel_float(const texture_3d tex, const int3 coord) = access_texel(tex, coord).x;
export @(pure macro) color texel_color(const texture_3d tex, const int3 coord) = color(access_texel(tex, coord).xyz);
export enum wrap_mode { wrap_clamp = 0, wrap_repeat = 1, wrap_mirrored_repeat = 2, wrap_clip = 3 };
@(pure macro) auto apply_wrap_mode(const auto wrap, const auto n, auto i) {
  auto rem(i % n);
  const auto neg(#select(rem < 0, 1, 0)); 
  rem += n * neg;
  const auto quo(i / n + neg);
  const auto repeat(rem);
  const auto mirror(#select(quo & 1, n - 1 - rem, rem));
  i = #select(wrap == 0, i, #select(wrap == 1, repeat, mirror));
  i = #max(0, #min(i, n - 1));
  return i;
}
export @(pure macro) float4 lookup_float4(
    const texture_2d tex, 
    float2 coord, 
    const wrap_mode wrap_u = wrap_repeat, 
    const wrap_mode wrap_v = wrap_repeat,
    const float2 crop_u = float2(0.0, 1.0),
    const float2 crop_v = float2(0.0, 1.0)) {
  if (#any(tex.tile_count > 1)) {
    const int2 tile_index(#floor(coord));
    const auto tile(access_uv_tile(tex, tile_index));
    return null if (!tile || !tile.texels);
    const auto extent(tile.extent);
    coord -= tile_index;
    coord *= extent;
    const int2 ic(coord);
    const int2 ic0(#min(ic, extent - 1));
    const int2 ic1(#min(ic + 1, extent - 1));
    coord -= ic;
    const auto s(coord.x);
    const auto t(coord.y);
    const auto texel0((1 - s) * tile.texels[extent.x * ic0.y + ic0.x] + s * tile.texels[extent.x * ic0.y + ic1.x]);
    const auto texel1((1 - s) * tile.texels[extent.x * ic1.y + ic0.x] + s * tile.texels[extent.x * ic1.y + ic1.x]);
    return apply_gamma_mode(gamma_mode(tex.gamma), (1 - t) * texel0 + t * texel1);
  } else {
    const int2 wrap(int(wrap_u), int(wrap_v));
    const auto tile(access_uv_tile(tex, int2(0)));
    return null if (!tile || !tile.texels);
    const auto extent(tile.extent);
    const auto icrop_u(int2(crop_u * extent));
    const auto icrop_v(int2(crop_v * extent));
    const auto icorner0(int2(icrop_u[0], icrop_v[0]));
    const auto icorner1(int2(icrop_u[1], icrop_v[1]));
    const auto subextent(icorner1 - icorner0); 
    coord *= subextent;
    const int2 ic(coord);
    const auto ic0(icorner0 + apply_wrap_mode(wrap, subextent, ic));
    const auto ic1(icorner0 + apply_wrap_mode(wrap, subextent, ic + 1));
    coord -= ic;
    const auto s(coord.x);
    const auto t(coord.y);
    const auto texel0((1 - s) * tile.texels[extent.x * ic0.y + ic0.x] + s * tile.texels[extent.x * ic0.y + ic1.x]);
    const auto texel1((1 - s) * tile.texels[extent.x * ic1.y + ic0.x] + s * tile.texels[extent.x * ic1.y + ic1.x]);
    return apply_gamma_mode(gamma_mode(tex.gamma), (1 - t) * texel0 + t * texel1);
  }
}
export @(pure macro) float3 lookup_float3(
    const texture_2d tex,
    const float2 coord,
    const wrap_mode wrap_u = wrap_repeat,
    const wrap_mode wrap_v = wrap_repeat,
    const float2 crop_u = float2(0.0, 1.0),
    const float2 crop_v = float2(0.0, 1.0)) = lookup_float4(tex, coord, wrap_u, wrap_v, crop_u, crop_v).xyz;
export @(pure macro) float2 lookup_float2(
    const texture_2d tex,
    const float2 coord,
    const wrap_mode wrap_u = wrap_repeat,
    const wrap_mode wrap_v = wrap_repeat,
    const float2 crop_u = float2(0.0, 1.0),
    const float2 crop_v = float2(0.0, 1.0)) = lookup_float4(tex, coord, wrap_u, wrap_v, crop_u, crop_v).xy;
export @(pure macro) float lookup_float(
    const texture_2d tex,
    const float2 coord,
    const wrap_mode wrap_u = wrap_repeat,
    const wrap_mode wrap_v = wrap_repeat,
    const float2 crop_u = float2(0.0, 1.0),
    const float2 crop_v = float2(0.0, 1.0)) = lookup_float4(tex, coord, wrap_u, wrap_v, crop_u, crop_v).x;
export @(pure macro) color lookup_color(
    const texture_2d tex,
    const float2 coord,
    const wrap_mode wrap_u = wrap_repeat,
    const wrap_mode wrap_v = wrap_repeat,
    const float2 crop_u = float2(0.0, 1.0),
    const float2 crop_v = float2(0.0, 1.0)) = color(lookup_float4(tex, coord, wrap_u, wrap_v, crop_u, crop_v).xyz);
)*";

[[nodiscard]] static const char *get_src(auto name) {
  if (name == "df")
    return df;
  if (name == "debug")
    return debug;
  if (name == "limits")
    return limits;
  if (name == "math")
    return math;
  if (name == "scene")
    return scene;
  if (name == "state")
    return state;
  if (name == "std")
    return std;
  if (name == "tex")
    return tex;
  return nullptr;
}

} // namespace smdl::Compiler::builtins
