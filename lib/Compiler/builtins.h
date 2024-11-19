#pragma once

namespace smdl::Compiler::builtins {

static const char *df = R"*(
#smdl_syntax
using ::math import *;
import ::microfacet::*;
import ::state::*;
export enum scatter_mode { scatter_none = 0, scatter_reflect = 1, scatter_transmit = 2, scatter_reflect_transmit = 3 };
@(pure) bool weighted_bool_sample(const &float xi, float weight) {
  return *xi < weight ? (*xi = (*xi / weight), true) : (*xi = (*xi - weight) / (1 - weight), false);
}
@(pure) int uniform_wavelength_index_sample(const &float xi) {
  const int i(#min(int(*xi *= $WAVELENGTH_BASE_MAX), $WAVELENGTH_BASE_MAX - 1));
  *xi -= i;
  return i;
}
@(pure) float2 uniform_disk_sample(float2 xi) {
  xi = 2 * xi - 1;
  return float2(0) if (#all(xi == 0));
  float rad(0);
  float phi(0);
  if ((absxi := #abs(xi), absxi.x > absxi.y))
    rad = xi.x, phi = $PI / 4 * (xi.y / xi.x);
  else
    rad = xi.y, phi = $PI / 2 - $PI / 4 * (xi.x / xi.y);
  return rad * float2(#cos(phi), #sin(phi));
}
@(pure) float3 cosine_hemisphere_sample(float2 xi) {
  return float3((p := uniform_disk_sample(xi)), #sqrt(#max(1 - #sum(p * p), 0)));
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
@(pure macro) float3 quat_transform_vector(const float4 q, const float3 u) {
  return (w := q.w) * w * u + (v := q.xyz) * dot(v, u) + cross(v, 2.0 * w * u + cross(v, u));
}
struct eval_bsdf_parameters {
  const float3 geometry_wo;
  const float3 geometry_wi;
  float3 wo = geometry_wo;
  float3 wi = geometry_wi;
  const float hit_side = #sign(geometry_wo.z);
  const float ior = 1.0 / 1.4;
  const bool thin_walled = false;
  const scatter_mode mode = (geometry_wo.z < 0) == (geometry_wi.z < 0) ? scatter_reflect : scatter_transmit;
};
struct eval_bsdf_result {
  float2 pdf = float2(0);
  color f = color(0);
  bool is_black = false;
};
@(pure) float4 perturb_normal(const &eval_bsdf_parameters this, const float3 normal) {
  const float4 q(quat_rotate(normalize(normal) * #sign(normal.z), float3(0, 0, 1)));
  this.wo = quat_transform_vector(q, this.wo);
  this.wi = quat_transform_vector(q, this.wi);
  return q;
}
@(pure) float4 perturb_tangent(const &eval_bsdf_parameters this, const float3 tangent) {
  const float4 q(quat_rotate(normalize(float3(tangent.xy, 0)), float3(1, 0, 0)));
  this.wo = quat_transform_vector(q, this.wo);
  this.wi = quat_transform_vector(q, this.wi);
  return q;
}
struct eval_bsdf_sample_parameters {
  const float3 geometry_wo;
  float3 wo = geometry_wo;
  float4 xi;
  const float hit_side = #sign(geometry_wo.z);
  const float ior = 1.0 / 1.4;
  const bool thin_walled = false;
};
struct eval_bsdf_sample_result {
  float3 wi = float3(0);
  scatter_mode mode = scatter_none;
  ?color delta_f = null;
};
@(pure) float4 perturb_normal(const &eval_bsdf_sample_parameters this, const float3 normal) {
  const float4 q(quat_rotate(normalize(normal) * #sign(normal.z), float3(0, 0, 1)));
  this.wo = quat_transform_vector(q, this.wo);
  return q;
}
@(pure) float4 perturb_tangent(const &eval_bsdf_sample_parameters this, const float3 tangent) {
  const float4 q(quat_rotate(normalize(float3(tangent.xy, 0.0)), float3(1, 0, 0)));
  this.wo = quat_transform_vector(q, this.wo);
  return q;
}
@(macro) auto eval_bsdf(const &default_bsdf this, inline const &eval_bsdf_parameters params) {
  return eval_bsdf_result(is_black: true);
}
@(macro) auto eval_bsdf_sample(const &default_bsdf this, inline const &eval_bsdf_sample_parameters params) {
  return eval_bsdf_sample_result(wi: float3(0.0), mode: scatter_none);
}
export struct diffuse_reflection_bsdf: bsdf {
  color tint = color(1.0);
  float roughness = 0.0;
  void string handle = "";
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
  void string handle = "";
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
export struct specular_bsdf: bsdf {
  color tint = color(1.0);
  scatter_mode mode = scatter_reflect;
  void string handle = "";
};
auto eval_bsdf(const &specular_bsdf this, const &eval_bsdf_parameters params) {
  return eval_bsdf_result(is_black: true);
}
auto eval_bsdf_sample(const &specular_bsdf this, inline const &eval_bsdf_sample_parameters params) {
  const auto brdf_weight(#select((int(this.mode) & int(scatter_reflect)), 1.0, 0.0));
  const auto btdf_weight(#select((int(this.mode) & int(scatter_transmit)), 1.0, 0.0));
  const auto brdf_prob(brdf_weight / (brdf_weight + btdf_weight));
  if (xi.x < brdf_prob) {
    return eval_bsdf_sample_result(wi: reflect(wo, float3(0, 0, 1)), mode: scatter_reflect, delta_f: this.tint);
  } else {
    return eval_bsdf_sample_result(wi: refract(wo, float3(0, 0, 1), #select(hit_side < 0, 1 / ior, ior)), mode: scatter_transmit, delta_f: this.tint);
  }
}
export struct sheen_bsdf: bsdf {
  float roughness;
  color tint = color(1.0);
  color multiscatter_tint = color(0.0);
  void string handle = "";
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
@(macro) auto eval_bsdf(const auto this, inline const &eval_bsdf_parameters params, const float3 normal) {
  preserve wo, wi;
  wo = geometry_wo;
  wi = geometry_wi;
  perturb_normal(params, normal);
  return eval_bsdf(this, params);
}
@(macro) auto eval_bsdf_sample(const auto this, inline const &eval_bsdf_sample_parameters params, const float3 normal) {
  auto q(perturb_normal(params, normal));
  auto result(eval_bsdf_sample(this, params));
  result.wi = quat_transform_vector(quat_transpose(q), result.wi) if (result.mode != scatter_none);
  return result;
}
export struct weighted_layer: bsdf {
  float weight;
  bsdf layer = bsdf();
  bsdf base = bsdf();
  float3 normal = state::normal();
};
@(macro) auto eval_bsdf(const &weighted_layer this, const &eval_bsdf_parameters params) {
  const auto result0(eval_bsdf(&this.base, params));
  const auto result1(eval_bsdf(&this.layer, params, this.normal));
  return eval_bsdf_result(pdf: lerp(result0.pdf, result1.pdf, this.weight), f: lerp(result0.f, result1.f, this.weight));
}
@(macro) auto eval_bsdf_sample(const &weighted_layer this, const &eval_bsdf_sample_parameters params) {
  return weighted_bool_sample(&params.xi.z, this.weight) 
    ? eval_bsdf_sample(&this.layer, params, this.normal) 
    : eval_bsdf_sample(&this.base, params);
}
export struct color_weighted_layer: bsdf {
  color weight;
  bsdf layer = bsdf();
  bsdf base = bsdf();
  float3 normal = state::normal();
};
@(macro) auto eval_bsdf(const &color_weighted_layer this, const &eval_bsdf_parameters params) {
  const auto result0(eval_bsdf(&this.base, params));
  const auto result1(eval_bsdf(&this.layer, params, this.normal));
  return eval_bsdf_result(pdf: lerp(result0.pdf, result1.pdf, average(this.weight)), f: lerp(result0.f, result1.f, this.weight));
}
@(macro) auto eval_bsdf_sample(const &color_weighted_layer this, const &eval_bsdf_sample_parameters params) {
  const auto i(uniform_wavelength_index_sample(&params.xi.w));
  return weighted_bool_sample(&params.xi.z, this.weight[i])
    ? eval_bsdf_sample(&this.layer, params, this.normal)
    : eval_bsdf_sample(&this.base, params);
}
@(pure macro) auto schlick_F0(const auto ior) = #pow((ior - 1) / (ior + 1), 2);
@(pure macro) auto schlick_factor(const auto cos_theta) = #pow(#max(1 - #abs(cos_theta), 0), 5);
@(pure macro) auto schlick_factor(const auto cos_theta, const auto exponent) = #pow(#max(1 - #abs(cos_theta), 0), exponent);
@(pure macro) auto schlick_F(const auto cos_theta, const auto F0, const auto F90 = 1.0) = F0 + (F90 - F0) * schlick_factor(cos_theta);
@(pure macro) auto schlick_F(const auto cos_theta, const auto F0, const auto F90, const float exponent) = F0 + (F90 - F0) * schlick_factor(cos_theta, exponent);
export struct fresnel_layer: bsdf {
  float ior;
  float weight = 1.0;
  bsdf layer = bsdf();
  bsdf base = bsdf();
  float3 normal = state::normal();
};
@(macro) auto eval_bsdf(const &fresnel_layer this, const &eval_bsdf_parameters params) {
  const auto result0(eval_bsdf(&this.base, params));
  const auto result1(eval_bsdf(&this.layer, params, this.normal));
  const auto wh(normalize(params.wo + params.wi));
  return eval_bsdf_result(
    pdf: lerp(result0.pdf, result1.pdf, this.weight * schlick_factor(auto(params.wo.z, params.wi.z))),
    f:   lerp(result0.f,   result1.f,   this.weight * schlick_F(dot(params.wi, wh), schlick_F0(this.ior))));
}
@(macro) auto eval_bsdf_sample(const &fresnel_layer this, const &eval_bsdf_sample_parameters params) {
  return uniform_bool_sample(&params.xi.z, this.weight * schlick_factor(params.wo.z))
    ? eval_bsdf_sample(&this.layer, params, this.normal)
    : eval_bsdf_sample(&this.base, params);
}
export @(macro) int material__eval_bsdf(
    const &material this, const &float3 wo, const &float3 wi,  
    const &float pdf_fwd, const &float pdf_rev, const &color f) {
  auto params(eval_bsdf_parameters(
    geometry_wo: normalize(state::transform_vector(state::coordinate_object, state::coordinate_internal, *wo)),
    geometry_wi: normalize(state::transform_vector(state::coordinate_object, state::coordinate_internal, *wi)),
    thin_walled: this.thin_walled));
  perturb_normal(&params, this.geometry.normal);
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
export @(macro) int material__eval_bsdf_sample(
    const &material this, const &float4 xi, const &float3 wo, 
    const &float3 wi, const &float pdf_fwd, const &float pdf_rev, const &color f, const &int is_delta) {
  auto sample_params(eval_bsdf_sample_parameters(
    geometry_wo: normalize(state::transform_vector(state::coordinate_object, state::coordinate_internal, *wo)), xi: *xi,
    thin_walled: this.thin_walled));
  auto q(perturb_normal(&sample_params, this.geometry.normal));
  auto sample_result(eval_bsdf_sample_result());
  if (#typeof(this.backface) != #typeof(material_surface()) && (sample_params.hit_side < 0)) { 
    sample_result = eval_bsdf_sample(&this.backface.scattering, &sample_params);
  } else {
    sample_result = eval_bsdf_sample(&this.surface.scattering, &sample_params);
  }
  if (sample_result.mode == scatter_none) {
    *wi = float3(0.0);
    *pdf_fwd = 0.0;
    *pdf_rev = 0.0;
    *f = color(0.0);
    *is_delta = false;
    return false;
  } else {
    sample_result.wi = quat_transform_vector(quat_transpose(q), sample_result.wi);
    sample_result.wi = normalize(sample_result.wi);
    if (sample_result.delta_f) {
      *pdf_fwd = 1.0;
      *pdf_rev = 1.0;
      *f = *sample_result.delta_f;
      *is_delta = true;
    } else {
      auto params(eval_bsdf_parameters(
        geometry_wo: sample_params.geometry_wo, 
        geometry_wi: sample_result.wi, 
        thin_walled: this.thin_walled));
      perturb_normal(&params, this.geometry.normal);
      auto result(eval_bsdf_result());
      if (#typeof(this.backface) != #typeof(material_surface()) && (params.hit_side < 0)) { 
        result = eval_bsdf(&this.backface.scattering, &params);
      } else {
        result = eval_bsdf(&this.surface.scattering, &params);
      }
      *pdf_fwd = result.pdf.x;
      *pdf_rev = result.pdf.y;
      *f = result.f;
      *is_delta = false;
    }
    *wi = normalize(state::transform_vector(state::coordinate_internal, state::coordinate_object, sample_result.wi));
    return true;
  }
}
)*";

static const char *debug = R"*(#smdl_syntax
export @(pure macro) bool assert(const bool condition, const string reason) {
  #assert(condition, reason) if ($DEBUG);
  return true;
}
export @(pure macro) bool breakpoint() {
  #breakpoint() if ($DEBUG);
  return true;
}
export @(pure macro) bool print(const auto a) {
  #print(a) if ($DEBUG);
  return true;
}
)*";

static const char *limits = R"*(#smdl_syntax
export const int INT_MIN = $INT_MIN;
export const int INT_MAX = $INT_MAX;
export const float FLOAT_MIN = $FLOAT_MIN;
export const float FLOAT_MAX = $FLOAT_MAX;
export const double DOUBLE_MIN = $DOUBLE_MIN;
export const double DOUBLE_MAX = $DOUBLE_MAX;
)*";

static const char *math = R"*(#smdl_syntax
import ::rgb::*;
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
export @(noinline) float luminance(const color a) {
  float result(0.0);
  for (int i = 0; i < $WAVELENGTH_BASE_MAX; ++i)
    result += rgb::wyman_1931_y($state.wavelength_base[i]) * a[i];
  return result / $WAVELENGTH_BASE_MAX;
}
)*";

static const char *scene = R"*(#smdl_syntax
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

static const char *state = R"*(#smdl_syntax
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
export @(macro) float4x4 transform(const coordinate_space from, const coordinate_space to) {
  if (from == to) {
    return float4x4(1.0);
  } else if ((from == coordinate_internal) & (to == coordinate_object)) {
    return $state.internal_to_object_matrix_fwd;
  } else if ((from == coordinate_internal) & (to == coordinate_world)) {
    return $state.internal_to_world_matrix_fwd;
  } else if ((from == coordinate_object) & (to == coordinate_world)) {
    return $state.object_to_world_matrix_fwd;
  } else if ((from == coordinate_object) & (to == coordinate_internal)) {
    return $state.internal_to_object_matrix_inv;
  } else if ((from == coordinate_world) & (to == coordinate_object)) {
    return $state.object_to_world_matrix_inv;
  } else if ((from == coordinate_world) & (to == coordinate_internal)) {
    return $state.internal_to_world_matrix_inv;
  } else {
    return float4x4(1.0);
  }
}
export @(macro) float3 transform_point(const coordinate_space from, const coordinate_space to, const float3 point) {
  return from == to ? point : (transform(from, to) * float4(point, 1)).xyz;
}
export @(macro) float3 transform_vector(const coordinate_space from, const coordinate_space to, const float3 vector) {
  return from == to ? vector : (transform(from, to) * float4(vector, 0)).xyz;
}
export @(macro) float3 transform_normal(const coordinate_space from, const coordinate_space to, const float3 normal) {
  return from == to ? normal : (float4(normal, 0) * transform(to, from)).xyz;
}
export @(macro) float transform_scale(const coordinate_space from, const coordinate_space to, const float scale) {
  return 1.0 * scale;
}
)*";

static const char *std = R"*(#smdl_syntax
export using ::debug import *;
export using ::df import *;
export using ::limits import *;
export using ::math import *;
export using ::scene import *;
export using ::state import *;
export using ::tex import *;
)*";

static const char *tex = R"*(#smdl_syntax
export enum gamma_mode { gamma_default = 0, gamma_linear = 1, gamma_srgb = 2 };
@(pure macro) float4 apply_gamma_mode(const gamma_mode gamma, const float4 texel) = gamma == gamma_srgb ? float4((texel * texel).xyz, texel.w) : texel;
@(pure macro) float3 apply_gamma_mode(const gamma_mode gamma, const float3 texel) = gamma == gamma_srgb ? (texel * texel) : texel;
@(pure macro) float2 apply_gamma_mode(const gamma_mode gamma, const float2 texel) = gamma == gamma_srgb ? (texel * texel) : texel;
@(pure macro) float apply_gamma_mode(const gamma_mode gamma, const float texel) = gamma == gamma_srgb ? (texel * texel) : texel;
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
    coord -= 0.5;
    const int2 ic(#floor(coord));
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
    coord -= 0.5;
    const int2 ic(#floor(coord));
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
@(foreign) void smdl_ptex_eval(&texture_ptex tex, int first, int num, &float result);
export @(macro) float4 lookup_float4(texture_ptex tex, const int channel = 0) {
  float4 result;
  smdl_ptex_eval(&tex, channel, 4, &result[0]);
  return apply_gamma_mode(gamma_mode(tex.gamma), result);
}
export @(macro) float3 lookup_float3(texture_ptex tex, const int channel = 0) {
  float3 result;
  smdl_ptex_eval(&tex, channel, 3, &result[0]);
  return apply_gamma_mode(gamma_mode(tex.gamma), result);
}
export @(macro) float2 lookup_float2(texture_ptex tex, const int channel = 0) {
  float2 result;
  smdl_ptex_eval(&tex, channel, 2, &result[0]);
  return apply_gamma_mode(gamma_mode(tex.gamma), result);
}
export @(macro) float lookup_float(texture_ptex tex, const int channel = 0) {
  float result;
  smdl_ptex_eval(&tex, channel, 1, &result);
  return apply_gamma_mode(gamma_mode(tex.gamma), result);
}
)*";

static const char *microfacet = R"*(
#smdl_syntax
using ::math import *;
@(pure foreign) float erfcf(float x);
@(pure foreign) float betaf(float x, float y);
export tag microfacet_slope;
export struct ggx: default microfacet_slope {};
export struct beckmann: microfacet_slope {};
export struct microfacet {
  float2 alpha;
  microfacet_slope slope = microfacet_slope();
};
@(pure macro) auto microfacet_smith_lambda(const ggx this, const float m) = 0.5 * (#sign(m) * #sqrt(1 + 1 / (m * m))) - 0.5;
@(pure macro) auto microfacet_slope_pdf(const ggx this, const float2 m) = 1.0 / ($PI * (t := 1 + dot(m, m)) * t);
@(pure noinline) auto microfacet_visible_slope_sample(const ggx this, const float xi0, float xi1, float cos_thetao) {
  return #sqrt(xi0 / (1 - xi0)) * float2(#cos(phi := $TWO_PI * xi1), #sin(phi)) if (cos_thetao > +0.9999);
  cos_thetao = #max(cos_thetao, -0.9999);
  const auto sin_thetao(#sqrt(1 - cos_thetao * cos_thetao));
  const auto tan_thetao(sin_thetao / cos_thetao);
  const auto mu(xi0 * (1 + 1 / cos_thetao) - 1);
  const auto nu(1 / (1 - mu * mu));
  const auto d(#sqrt(#max(nu * (mu * mu - (1 - nu) * tan_thetao * tan_thetao), 0)));
  const auto mx0(-nu * tan_thetao - d);
  const auto mx1(-nu * tan_thetao + d);
  const auto mx(#select((mu < 0) | (mx1 * sin_thetao > cos_thetao), mx0, mx1));
  auto my(xi1 > 0.5 ? +1.0 : -1.0);
  xi1 = saturate(my * (2 * xi1 - 1));
  my *= #sqrt(1 + mx * mx) *
        ((xi1 * (xi1 * (xi1 * 0.273850 - 0.733690) + 0.463410)) /
         (xi1 * (xi1 * (xi1 * 0.093073 + 0.309420) - 1.000000) + 0.597999));
  return float2(mx, my);
}
@(pure macro) auto microfacet_smith_lambda(const beckmann this, const float m) = 0.5 * (#exp(-m * m) / m / #sqrt($PI) - erfcf(m));
@(pure macro) auto microfacet_slope_pdf(const beckmann this, const float2 m) =  1.0 / ($PI * #exp(-dot(m, m)));
@(pure noinline) auto microfacet_visible_slope_sample(const beckmann this, const float xi0, const float xi1, float cos_thetao) {
  return float2(0.0);
}
export @(pure) auto microfacet_smith_lambda(microfacet this, const float3 w) {
  return microfacet_smith_lambda(visit this.slope, w.z / length(this.alpha * w.xy));
}
export @(pure) auto microfacet_slope_pdf(microfacet this, float2 m) {
  return microfacet_slope_pdf(visit this.slope, m / this.alpha) / (this.alpha.x * this.alpha.y);
}
export @(pure) auto microfacet_normal_pdf(microfacet this, const float3 wm) {
  return 0.0 if (!(wm.z > 1e-5));
  return microfacet_slope_pdf(this, -wm.xy / wm.z) / #max(#pow(wm.z, 4), 0.001);
}
export @(pure) auto microfacet_visible_normal_pdf(microfacet this, const float3 wo, const float3 wm) {
  return 0.0 if (!(wm.z > 1e-5));
  float cos_thetao(#max(dot(wo, wm), 0));
  float proj_areao((1 + microfacet_smith_lambda(this, wo)) * #max(wo.z, 0.001));
  return cos_thetao / proj_areao * microfacet_normal_pdf(this, wm);
}
export @(pure) auto microfacet_visible_normal_sample(microfacet this, const float xi0, const float xi1, const float3 wo) {
  const auto w11(normalize(float3(this.alpha * wo.xy, wo.z)));
  const auto sin_theta(length(w11.xy));
  const auto cos_phi(w11.x / sin_theta);
  const auto sin_phi(w11.y / sin_theta);
  const auto m11(microfacet_visible_slope_sample(visit this.slope, xi0, xi1, w11.z));
  const auto m(float2(
      this.alpha.x * dot(float2(cos_phi, -sin_phi), m11), 
      this.alpha.y * dot(float2(sin_phi, +cos_phi), m11)));
  return #all(isfinite(m)) ? normalize(float3(m, 1)) : wo.z == 0 ? normalize(wo) : float3(0, 0, 1);
}
export struct microfacet_specular_result {
  float3 wm = float3(0.0);
  float2 pdf = float2(0.0);
  float f = 0.0;
};
export @(pure) auto microfacet_specular_reflection(microfacet this, float3 wo, float3 wi) {
  if (wo.z < 0) wo = -wo, wi = -wi;
  if (wi.z < 0) return microfacet_specular_result();
  const auto wm(normalize(wo + wi));
  const auto d(microfacet_normal_pdf(this, wm));
  const auto lambdao(microfacet_smith_lambda(this, wo)), proj_areao((1 + lambdao) * #max(wo.z, 0.001));
  const auto lambdai(microfacet_smith_lambda(this, wi)), proj_areai((1 + lambdai) * #max(wi.z, 0.001));
  const auto g(1 / (1 + lambdao + lambdai));
  return microfacet_specular_result(
      wm, 
      0.25 * d / float2(proj_areao, proj_areai),
      0.25 * d * g / #max(wo.z, 0.001) * step(0.0, wi.z));
}
export @(pure) auto microfacet_specular_refraction(microfacet this, float3 wo, float3 wi, float ior) {
  if (wo.z < 0) wo = -wo, wi = -wi, ior = 1 / ior;
  if (wi.z > 0) return microfacet_specular_result();
  auto wm(normalize(refraction_half_vector(wo, wi, ior)));
  wm = wm * #sign(wm.z); 
  const auto cos_thetao(dot(wo, wm));
  const auto cos_thetai(dot(wi, wm));
  if (!((cos_thetao > 0) & (cos_thetai < 0)))
    return microfacet_specular_result();
  const float d(microfacet_normal_pdf(this, wm));
  const auto lambdao(microfacet_smith_lambda(this, +wo)), proj_areao((1 + lambdao) * #max(+wo.z, 0.001));
  const auto lambdai(microfacet_smith_lambda(this, -wi)), proj_areai((1 + lambdai) * #max(-wi.z, 0.001));
  const auto g(betaf(1 + lambdao, 1 + lambdai));
  const auto j(float2(
    +cos_thetao * refraction_half_vector_jacobian(wo, wi, ior),
    -cos_thetai * refraction_half_vector_jacobian(wi, wo, 1 / ior)));
  return microfacet_specular_result(
      wm, 
      d * j / float2(proj_areao, proj_areai),
      d * g * j[0] / #max(wo.z, 0.001) * step(0.0, wi.z));
}
)*";

static const char *rgb = R"*(#smdl_syntax
const int RGB_TO_COLOR_NUM_WAVES = 32;
const float RGB_TO_COLOR_MIN_WAVE = 380.0;
const float RGB_TO_COLOR_MAX_WAVE = 720.0;
const static auto RGB_TO_COLOR_TABLES = auto[](
  auto[]( 
    +1.0618958, +1.0615020, +1.0614336, +1.0622711, +1.0622036, +1.0625060, +1.0623939, +1.0624707,
    +1.0625048, +1.0624366, +1.0620694, +1.0613167, +1.0610334, +1.0613868, +1.0614215, +1.0620337,
    +1.0625497, +1.0624317, +1.0625249, +1.0624278, +1.0624750, +1.0625539, +1.0625327, +1.0623922,
    +1.0623651, +1.0625256, +1.0612278, +1.0594263, +1.0599811, +1.0602547, +1.0601263, +1.0606565),
  auto[]( 
    +1.0414628, +1.0328661, +1.0126146, +1.0350461, +1.0078661, +1.0422280, +1.0442597, +1.0535238,
    +1.0180776, +1.0442730, +1.0529362, +1.0537034, +1.0533901, +1.0537783, +1.0527093, +1.0530449,
    +1.0550555, +1.0553674, +1.0454307, +0.6234895, +0.1803807, -0.0076304, -0.0001522, -0.0075102,
    -0.0021709, +0.0006592, +0.0122788, -0.0044670, +0.0171198, +0.0049211, +0.0058763, +0.0252594),
  auto[]( 
    +0.9942214, +0.9898694, +0.9829366, +0.9962787, +1.0198956, +1.0166396, +1.0220913, +0.9965166,
    +1.0097766, +1.0215422, +0.6403195, +0.0025012, +0.0065340, +0.0028334, -0.0000000, -0.0090592,
    +0.0033937, -0.0030639, +0.2220394, +0.6314114, +0.9748099, +0.9720956, +1.0173770, +0.9987519,
    +0.9470173, +0.8525862, +0.9489780, +0.9475188, +0.9959894, +0.8630135, +0.8915099, +0.8486649),
  auto[]( 
    +0.0055741, -0.0047983, -0.0052537, -0.0064571, -0.0059694, -0.0021837, +0.0167811, +0.0960964,
    +0.2121736, +0.3616913, +0.5396101, +0.7440881, +0.9220957, +1.0460304, +1.0513825, +1.0511992,
    +1.0510530, +1.0517397, +1.0516043, +1.0511944, +1.0511590, +1.0516613, +1.0514039, +1.0515941,
    +1.0511460, +1.0515124, +1.0508871, +1.0508924, +1.0477493, +1.0493273, +1.0435964, +1.0392281),
  auto[]( 
    +0.1657560, +0.1184644, +0.1240829, +0.1137127, +0.0789924, +0.0322056, -0.0107984, +0.0180520,
    +0.0053407, +0.0136549, -0.0059564, -0.0018444, -0.0105719, -0.0029376, -0.0107905, -0.0080224,
    -0.0022669, +0.0070200, -0.0081528, +0.6077287, +0.9883156, +0.9939169, +1.0039339, +0.9923450,
    +0.9992653, +1.0084622, +0.9835830, +1.0085024, +0.9745114, +0.9854327, +0.9349576, +0.9871391),
  auto[]( 
    +0.0026494, -0.0050175, -0.0125472, -0.0094555, -0.0125261, -0.0079171, -0.0079956, -0.0093559,
    +0.0654686, +0.3957288, +0.7524402, +0.9637648, +0.9985443, +0.9999298, +0.9993908, +0.9999437,
    +0.9993912, +0.9991124, +0.9601958, +0.6318628, +0.2579740, +0.0094015, -0.0030798, -0.0045230,
    -0.0068933, -0.0090352, -0.0085914, -0.0083691, -0.0078686, -0.0000084, +0.0054301, -0.0027746),
  auto[]( 
    +0.9920977, +0.9887643, +0.9953904, +0.9952932, +0.9918145, +1.0002584, +0.9996848, +0.9998812,
    +0.9850401, +0.7902985, +0.5608220, +0.3313346, +0.1369241, +0.0189149, -0.0000051, -0.0004240,
    -0.0004193, +0.0017473, +0.0037999, -0.0005510, -0.0000437, +0.0075875, +0.0257957, +0.0381684,
    +0.0494896, +0.0495960, +0.0498148, +0.0398409, +0.0305010, +0.0212431, +0.0069597, +0.0041734));
export @(hot noinline) color rgb_to_color(float3 rgb) {
  if ((rgb.x == rgb.y) & (rgb.x == rgb.z)) {
    return color(rgb.x);
  } else {
    const int i0(#all(rgb.xx < rgb.yz) ? 0 : rgb.y < rgb.z ? 1 : 2);
    int i1_tmp((i0 + 1) % 3);
    int i2_tmp((i0 + 2) % 3);
    const bool should_swap(rgb[i1_tmp] > rgb[i2_tmp]);
    const int i1(should_swap ? i2_tmp : i1_tmp);
    const int i2(should_swap ? i1_tmp : i2_tmp);
    const float coeff_w(rgb[i0]);
    const float coeff_cmy(rgb[i1] - rgb[i0]);
    const float coeff_rgb(rgb[i2] - rgb[i1]);
    color c(0.0);
    color w(color($state.wavelength_base));
    w -= RGB_TO_COLOR_MIN_WAVE;
    w *= RGB_TO_COLOR_NUM_WAVES / (RGB_TO_COLOR_MAX_WAVE - RGB_TO_COLOR_MIN_WAVE);
    for (int i = 0; i < $WAVELENGTH_BASE_MAX; i++) {
      float t(w[i]);
      if ((0.0 <= t) & (t <= RGB_TO_COLOR_NUM_WAVES)) {
        int t0(#min(int(t), RGB_TO_COLOR_NUM_WAVES - 2));
        t = #min(t - t0, 1.0);
        c[i] = #sum(float2(1 - t, t) * 
              (coeff_w * float2(&RGB_TO_COLOR_TABLES[0][t0]) +
               coeff_cmy * float2(&RGB_TO_COLOR_TABLES[i0 + 1][t0]) +
               coeff_rgb * float2(&RGB_TO_COLOR_TABLES[i2 + 4][t0])));
      }
    }
    c = #max(c * 0.94, 0.0);
    c = #min(c, 1.0);
    return c;
  }
}
export @(macro) color rgb_to_color(const float r, const float g, const float b) {
  return rgb_to_color(float3(r, g, b));
}
export @(pure) float3 wyman_1931_xyz(const float w) {
  auto x(w - auto(442.0, 599.8, 501.1, 568.8, 530.9, 437.0, 459.0));
  x *= #select(x < 0, auto(0.0624, 0.0264, 0.0490, 0.0213, 0.0613, 0.0845, 0.0385),
                      auto(0.0374, 0.0323, 0.0382, 0.0247, 0.0322, 0.0278, 0.0725));
  x *= 0.5 * x;
  const auto x1(x);
  auto y(1.0 + x);
  y += (x *= x1 * 0.5);
  y += (x *= x1 * 0.333333);
  y += (x *= x1 * 0.25);
  y = 1.0 / y;
  y *= auto(0.362, 1.056, -0.065, 0.821, 0.286, 1.217, 0.681);
  return float3(y[0] + y[1] + y[2], y[3] + y[4], y[5] + y[6]);
}
export @(pure) float wyman_1931_y(const float w) {
  auto x(w - auto(568.8, 530.9));
  x *= #select(x < 0, auto(0.0213, 0.0613), auto(0.0247, 0.0322));
  x *= 0.5 * x;
  const auto x1(x);
  auto y(1.0 + x);
  y += (x *= x1 * 0.5);
  y += (x *= x1 * 0.333333);
  y += (x *= x1 * 0.25);
  return #sum(auto(0.821, 0.286) / y);
}
export @(hot noinline) float3 color_to_rgb(const color c) {
  float3 result(0.0);
  for (int i = 0; i < $WAVELENGTH_BASE_MAX; ++i)
    result += wyman_1931_xyz($state.wavelength_base[i]) * c[i];
  result *= 0.01;
  result /= $WAVELENGTH_BASE_MAX;
  result *= $state.wavelength_max - $state.wavelength_min; 
  result = float3x3(
    float3(+3.240450, -0.969266, +0.0556434),
    float3(-1.537140, +1.876010, -0.2040260),
    float3(-0.498532, +0.041556, +1.0572300)) * result;
  return result;
}
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
  if (name == "microfacet")
    return microfacet;
  if (name == "rgb")
    return rgb;
  return nullptr;
}

} // namespace smdl::Compiler::builtins
