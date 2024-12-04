#pragma once

namespace smdl::Compiler::builtins {

static const char *anno = R"*(#smdl_syntax
)*";

static const char *df = R"*(
#smdl_syntax
using ::math import *;
using ::monte_carlo import *;
using ::specular import *;
import ::state::*;
export enum scatter_mode { scatter_none = 0, scatter_reflect = 1, scatter_transmit = 2, scatter_reflect_transmit = 3 };
@(pure macro)
float scatter_reflect_chance(const scatter_mode mode) {
  const auto refl_weight(#select((int(mode) & int(scatter_reflect)), 1.0, 0.0));
  const auto tran_weight(#select((int(mode) & int(scatter_transmit)), 1.0, 0.0));
  return refl_weight / (refl_weight + tran_weight);
}
const float STABILITY_EPS = 0.0001;
const float DEFAULT_IOR = 1 / 1.4;
typedef $(color | float) color_or_float;
typedef $(color | float | void) color_or_float_or_void;
@(pure noinline)
float3x3 build_orthogonal_tangent_space(const float3 normal, const float3 tangent_u) {
  const float3 tw(normalize(normal) * #sign(normal.z));
  const float3 tu(normalize(tangent_u - dot(tangent_u, tw) * tw));
  const float3 tv(normalize(cross(tw, tu)));
  return float3x3(tu, tv, tw);
}
struct evaluate_bsdf_parameters {
  const float3 primary_wo;
  const float3 primary_wi;
  const scatter_mode mode = (primary_wo.z < 0) == (primary_wi.z < 0) ? scatter_reflect : scatter_transmit;
  const bool thin_walled = false;
  const bool backface = false;
  float ior = backface ? 1 / DEFAULT_IOR : DEFAULT_IOR;
  float3 normal = state::normal();
  float3 tangent_u = state::texture_tangent_u(0);
  float3 wo = primary_wo;
  float3 wi = primary_wi;
};
struct evaluate_bsdf_result {
  color_or_float f = 0.0;
  float2 pdf = float2(0.0);
  const bool is_black = false;
};
struct evaluate_bsdf_sample_parameters {
  const float3 primary_wo;
  const bool thin_walled = false;
  const bool backface = false;
  float ior = backface ? 1 / DEFAULT_IOR : DEFAULT_IOR;
  float3 normal = state::normal();
  float3 tangent_u = state::texture_tangent_u(0);
  float3 wo = primary_wo;
  float4 xi;
};
struct evaluate_bsdf_sample_result {
  float3 wi = float3(0.0);
  scatter_mode mode = scatter_none;
  ?color delta_f = null;
};
@(pure noinline)
bool perturb_tangent_space(inline const &evaluate_bsdf_parameters this) {
  auto tbn(build_orthogonal_tangent_space(normal, tangent_u));
  wo = primary_wo * tbn;
  wi = primary_wi * tbn;
  return ((wo.z < 0) == (primary_wo.z < 0)) & ((wi.z < 0) == (primary_wi.z < 0));
}
@(pure noinline) 
?float3x3 perturb_tangent_space(inline const &evaluate_bsdf_sample_parameters this) {
  auto tbn(build_orthogonal_tangent_space(normal, tangent_u));
  wo = primary_wo * tbn;
  return tbn if ((wo.z < 0) == (primary_wo.z < 0));
  return null;
}
@(pure)
float3 half_direction(inline const &evaluate_bsdf_parameters this) {
  return normalize(mode == scatter_reflect ? wo + wi : refraction_half_vector(wo, wi, ior));
}
@(pure foreign) float erff(float x);
@(pure foreign) float erfcf(float x);
@(pure foreign) float lgammaf(float x);
@(pure macro) float betaf(float x, float y) = #exp(lgammaf(x) + lgammaf(y) - lgammaf(x + y));
@(pure noinline)
float inverse_erff(const float y) {
  float w = -#log((1 - y) * (1 + y));
  float x = 0;
  if (w < 5) {
    w = w - 2.5;
    x = w * 2.81022636e-08 + 3.43273939e-7;
    x = w * x - 3.52338770e-6;
    x = w * x - 4.39150654e-6;
    x = w * x + 2.18580870e-4;
    x = w * x - 1.25372503e-3;
    x = w * x - 4.17768164e-3;
    x = w * x + 2.46640727e-1;
    x = w * x + 1.50140941;
  } else {
    w = #sqrt(w) - 3;
    x = w * -2.00214257e-4 + 1.00950558e-4;
    x = w * x + 1.34934322e-3;
    x = w * x - 3.67342844e-3;
    x = w * x + 5.73950773e-3;
    x = w * x - 7.62246130e-3;
    x = w * x + 9.43887047e-3;
    x = w * x + 1.00167406;
    x = w * x + 2.83297682;
  }
  return x * y;
}
@(pure macro)
auto evaluate_bsdf(const &default_bsdf this, const &evaluate_bsdf_parameters params) {
  return evaluate_bsdf_result(is_black: true);
}
@(pure macro)
auto evaluate_bsdf_sample(const &default_bsdf this, const &evaluate_bsdf_sample_parameters params) {
  return evaluate_bsdf_sample_result(wi: float3(0.0), mode: scatter_none);
}
export struct diffuse_reflection_bsdf: bsdf {
  const color_or_float tint = 1.0;
  const float roughness = 0.0;
  void string handle = "";
};
@(pure noinline)
auto evaluate_bsdf(const &diffuse_reflection_bsdf this, inline const &evaluate_bsdf_parameters params) {
  if (mode == scatter_reflect && perturb_tangent_space(params)) {
    const auto roughness(saturate(this.roughness));
    const auto cos_theta(#abs(float2(wi.z, wo.z)));
    if (roughness == 0) {
      return evaluate_bsdf_result(pdf: (pdf := cos_theta / $PI), f: pdf[0] * this.tint);
    } else {
      const auto sigma2(2.0 * roughness * roughness);
      const auto A(1.00 - sigma2 / (2.0 * sigma2 + 0.66));
      const auto B(0.45 * sigma2 / (sigma2 + 0.09));
      const auto fac(#max(#sum(wo.xy * wi.xy), 0) / #max_value(cos_theta));
      const auto fit(return_from {
        float3 fit(2.2858097e+01, 3.0605976e+01, 1.7212993e+01);
        fit = fit * roughness + float3(-1.0746762e+02, -1.4071259e+02, -7.9453520e+01);
        fit = fit * roughness + float3( 2.1076268e+02,  2.7144544e+02,  1.5198188e+02);
        fit = fit * roughness + float3(-2.2137146e+02, -2.8444799e+02, -1.5380799e+02);
        fit = fit * roughness + float3( 1.3156994e+02,  1.7425125e+02,  8.5750507e+01);
        fit = fit * roughness + float3(-4.1630793e+01, -6.1425120e+01, -2.3544818e+01);
        fit = fit * roughness + float3( 5.1545962e+00,  1.0499041e+01,  1.6004720e+00);
        fit = fit * roughness + float3(-9.1964624e-02,  3.7044917e-02,  4.3948036e-02);
        fit = fit * roughness + float3( 1.0003311e+00,  1.4157300e-03,  1.0000467e+00);
        return fit;
      });
      const auto Ewo(#min(fit[0] - #pow(#abs(wo.z) * fit[1], 1.2), 0.999));
      const auto Ewi(#min(fit[0] - #pow(#abs(wi.z) * fit[1], 1.2), 0.999));
      const auto Eav(#min(fit[2], 0.999));
      return evaluate_bsdf_result(pdf: (pdf := cos_theta / $PI), f: pdf[0] * (A + fac * B + (1 - Ewo) * (1 - Ewi) / (1 - Eav)) * this.tint);
    }
  } else {
    return evaluate_bsdf_result(is_black: true);
  }
}
@(pure)
auto evaluate_bsdf_sample(const &diffuse_reflection_bsdf this, inline const &evaluate_bsdf_sample_parameters params) {
  if ((tbn := perturb_tangent_space(params))) {
    return evaluate_bsdf_sample_result(wi: (*tbn) * cosine_hemisphere_sample(xi.xy), mode: scatter_reflect);
  } else {
    return evaluate_bsdf_sample_result(); 
  }
}
export struct diffuse_transmission_bsdf: bsdf {
  const color_or_float tint = 1.0;
  void string handle = "";
};
@(pure) 
auto evaluate_bsdf(const &diffuse_transmission_bsdf this, inline const &evaluate_bsdf_parameters params) {
  if (mode == scatter_transmit && perturb_tangent_space(params)) {
    const auto cos_theta(#abs(float2(wi.z, wo.z)));
    const auto pdf(cos_theta / $PI);
    return evaluate_bsdf_result(pdf: pdf, f: this.tint * pdf[0]);
  } else {
    return evaluate_bsdf_result(is_black: true);
  }
}
@(pure)
auto evaluate_bsdf_sample(const &diffuse_transmission_bsdf this, inline const &evaluate_bsdf_sample_parameters params) {
  if ((tbn := perturb_tangent_space(params))) {
    return evaluate_bsdf_sample_result(wi: (*tbn) * -cosine_hemisphere_sample(xi.xy), mode: scatter_transmit);
  } else {
    return evaluate_bsdf_sample_result(); 
  }
}
export struct specular_bsdf: bsdf {
  const color_or_float tint = 1.0;
  const scatter_mode mode = scatter_reflect;
  void string handle = "";
};
@(pure macro)
auto evaluate_bsdf(const &specular_bsdf this, const &evaluate_bsdf_parameters params) {
  return evaluate_bsdf_result(is_black: true); 
}
@(pure)
auto evaluate_bsdf_sample(const &specular_bsdf this, inline const &evaluate_bsdf_sample_parameters params) {
  if ((tbn := perturb_tangent_space(params))) {
    return xi.x < scatter_reflect_chance(this.mode)
      ? evaluate_bsdf_sample_result(wi: (*tbn) * reflect(wo, float3(0, 0, 1)),      mode: scatter_reflect,  delta_f: color(this.tint))
      : evaluate_bsdf_sample_result(wi: (*tbn) * refract(wo, float3(0, 0, 1), ior), mode: scatter_transmit, delta_f: color(this.tint));
  } else {
    return evaluate_bsdf_sample_result(); 
  }
}
export struct sheen_bsdf: bsdf {
  const float roughness;
  const color_or_float tint = 1.0;
  const color_or_float_or_void multiscatter_tint = null;
  void string handle = "";
};
@(pure macro)
auto sheen_lambda_l(const auto fit, const float mu) = fit[0] / (1.0 + fit[1] * #pow(mu, fit[2])) + fit[3] * mu + fit[4];
@(pure noinline)
auto sheen_lambda(const auto fit, const float mu) = #exp(mu < 0.5 ? sheen_lambda_l(fit, mu) : 2.0 * sheen_lambda_l(fit, 0.5) - sheen_lambda_l(fit, #max(1.0 - mu, 0.0)));
@(pure noinline) 
auto evaluate_bsdf(const &sheen_bsdf this, inline const &evaluate_bsdf_parameters params) {
  if (mode == scatter_reflect && perturb_tangent_space(params)) {
    const auto roughness(saturate(this.roughness));
    const auto cos_thetao(#abs(wo.z));
    const auto cos_thetai(#abs(wi.z));
    const auto pdf(float2(cos_thetai, cos_thetao) / $PI);
    const auto fss(let {
      const auto alpha(lerp(0.1, 1.0, roughness * roughness));
      const auto fit(lerp(auto(21.5473, 3.82987, 0.19823, -1.97760, -4.32054),
                          auto(25.3245, 3.32435, 0.16801, -1.27393, -4.85967), (1 - alpha) * (1 - alpha))); 
      const auto cos_thetah(normalize(wo + wi).z);
      const auto sin_thetah(#sqrt(1 - cos_thetah * cos_thetah + STABILITY_EPS));
      const auto D((2 + 1 / alpha) * #pow(sin_thetah, 1 / alpha) / $TWO_PI);
      const auto G2(1 / (1 + sheen_lambda(fit, cos_thetao) + sheen_lambda(fit, cos_thetai)));
    } in D * G2 / (4 * cos_thetao + STABILITY_EPS));
    if (#is_void(this.multiscatter_tint)) {
      return evaluate_bsdf_result(pdf: pdf, f: fss * this.tint);
    } else {
      const auto fms(let {
        const auto fit(return_from {
          float4 fit(7.8849563e+00, 1.0884491e+01, -1.6968964e+00, -2.7087439e-01);
          fit = fit * roughness + float4(-3.2346706e+01, -4.8377170e+01,  5.4675246e+00,  1.0047828e+00);
          fit = fit * roughness + float4( 5.3026224e+01,  8.8888751e+01, -3.9836205e+00, -1.3797632e+00);
          fit = fit * roughness + float4(-4.2615764e+01, -8.6238862e+01, -3.9847111e+00,  8.3431138e-01);
          fit = fit * roughness + float4( 1.3553998e+01,  4.4328392e+01,  6.2216980e+00, -3.4164580e-01);
          fit = fit * roughness + float4( 2.6918201e+00, -9.2638026e+00, -1.7865819e+00, -6.4843047e-02);
          fit = fit * roughness + float4(-2.1587465e+00,  8.5394786e-01,  2.0808058e-01,  4.3847363e-01);
          fit = fit * roughness + float4( 1.6297278e-02, -3.0556821e-02, -8.5395124e-03, -4.0936379e-04);
          fit = fit * roughness + float4( 8.6410438e-01,  6.1500189e-03,  1.3796256e-03,  7.5187484e-02);
          return fit;
        });
        const auto Ewo(fit[0] * #pow(1.0 / (1.0 + fit[1] * cos_thetao), 1.0 / fit[2]));
        const auto Ewi(fit[0] * #pow(1.0 / (1.0 + fit[1] * cos_thetai), 1.0 / fit[2]));
        const auto Eav(fit[3]);
      } in (1 - Ewo) * (1 - Ewi) / (1 - Eav) * cos_thetai / $PI);
      return evaluate_bsdf_result(pdf: pdf, f: this.tint * (fss + this.multiscatter_tint * fms));
    }
  } else {
    return evaluate_bsdf_result(is_black: true);
  }
}
@(pure)
auto evaluate_bsdf_sample(const &sheen_bsdf this, inline const &evaluate_bsdf_sample_parameters params) {
  if ((tbn := perturb_tangent_space(params))) {
    return evaluate_bsdf_sample_result(wi: (*tbn) * cosine_hemisphere_sample(xi.xy), mode: scatter_reflect);
  } else {
    return evaluate_bsdf_sample_result(); 
  }
}
export @(pure macro) 
auto backscattering_glossy_reflection_bsdf(
    const float roughness_u,
    const float roughness_v = roughness_u,
    const color_or_float tint = 1.0,
    const color_or_float_or_void multiscatter_tint = null,
    const string handle = "") {
  return sheen_bsdf(roughness: #sqrt(saturate(roughness_u) * saturate(roughness_v)), tint: tint, multiscatter_tint: multiscatter_tint);
}
tag microfacet_distribution;
struct microfacet_distribution_ggx: default microfacet_distribution {};
struct microfacet_distribution_beckmann: microfacet_distribution {};
struct microfacet_distribution_blinn: microfacet_distribution {};
tag microfacet_shadowing;
struct microfacet_shadowing_smith: default microfacet_shadowing {};
struct microfacet_shadowing_vcavities: microfacet_shadowing {};
struct microfacet_bsdf: bsdf {
  const float2 roughness;
  const float2 alpha = roughness * roughness;
  const float roughness0 = #sqrt(#prod(roughness));
  const color_or_float tint;
  const color_or_float_or_void multiscatter_tint = null;
  const float3 tangent_u = state::texture_tangent_u(0);
  scatter_mode mode = scatter_reflect;
  microfacet_distribution distribution = microfacet_distribution();
  microfacet_shadowing shadowing = microfacet_shadowing();
};
@(macro) auto microfacet_bsdf_constructor(
    float roughness_u,
    float roughness_v = roughness_u,
    const color_or_float tint = 1.0,
    const color_or_float_or_void multiscatter_tint = null,
    const float3 tangent_u = state::texture_tangent_u(0),
    const scatter_mode mode = scatter_reflect,
    const string handle = "",
    microfacet_distribution distribution = microfacet_distribution(),
    microfacet_shadowing shadowing = microfacet_shadowing()) {
  roughness_u = saturate(roughness_u);
  roughness_v = saturate(roughness_v);
  return microfacet_bsdf(
    roughness: float2(roughness_u, roughness_v), 
    tint: tint, multiscatter_tint: multiscatter_tint, tangent_u: normalize(tangent_u),
    mode: mode, distribution: distribution, shadowing: shadowing);
}
export auto simple_glossy_bsdf(*) = 
    microfacet_bsdf_constructor(distribution: microfacet_distribution_blinn(), shadowing: microfacet_shadowing_vcavities());
export auto microfacet_ggx_smith_bsdf(*) =
    microfacet_bsdf_constructor(distribution: microfacet_distribution_ggx(), shadowing: microfacet_shadowing_smith());
export auto microfacet_ggx_vcavities_bsdf(*) =
    microfacet_bsdf_constructor(distribution: microfacet_distribution_ggx(), shadowing: microfacet_shadowing_vcavities());
export auto microfacet_beckmann_smith_bsdf(*) =
    microfacet_bsdf_constructor(distribution: microfacet_distribution_beckmann(), shadowing: microfacet_shadowing_smith());
export auto microfacet_beckmann_vcavities_bsdf(*) =
    microfacet_bsdf_constructor(distribution: microfacet_distribution_beckmann(), shadowing: microfacet_shadowing_vcavities());
@(pure macro) auto smith_lambda(const microfacet_distribution_ggx this, const float m) = 0.5 * (#sign(m) * #sqrt(1 + 1 / (m * m + STABILITY_EPS))) - 0.5;
@(pure macro) auto smith_lambda(const microfacet_distribution_beckmann this, const float m) = 0.5 * (#exp(-m * m) / m / #sqrt($PI) - erfcf(m));
@(pure macro) auto smith_slope_pdf(const microfacet_distribution_ggx this, const float2 m) = (1.0 / $PI) / #pow(1 + #sum(m * m), 2);
@(pure macro) auto smith_slope_pdf(const microfacet_distribution_beckmann this, const float2 m) = (1.0 / $PI) * #exp(-#sum(m * m));
@(pure macro) auto smith_normal_pdf(const microfacet_distribution this, const float2 alpha, const float3 wm) {
  return 0.0 if (!(wm.z > 0));
  return smith_slope_pdf(this, -wm.xy / (wm.z * alpha + STABILITY_EPS)) / (alpha.x * alpha.y * #pow(wm.z, 4) + STABILITY_EPS);
}
@(pure noinline) auto smith_visible_slope_sample(const microfacet_distribution_ggx this, const float xi0, float xi1, float cos_thetao) {
  return #sqrt(xi0 / (1 - xi0 + STABILITY_EPS)) * float2(#cos(phi := $TWO_PI * xi1), #sin(phi)) if (cos_thetao > +0.9999);
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
@(pure) float beckmann_visible_cdf(const float cos_thetao, const float sin_thetao, const float x) {
  return 0.5 * (cos_thetao * erfcf(-x) + sin_thetao * #exp(-x * x) / #sqrt($PI));
}
@(pure noinline) auto smith_visible_slope_sample(const microfacet_distribution_beckmann this, float xi0, float xi1, float cos_thetao) {
  xi0 = #max(xi0, 1e-5);
  if (cos_thetao < -0.9999) cos_thetao = -0.9999;
  if (cos_thetao > +0.9999) return #sqrt(-#log(1 - xi0)) * float2(#cos((phi := $TWO_PI * xi1)), #sin(phi));
  const auto thetao(#acos(cos_thetao));
  const auto sin_thetao(#sqrt(1 - cos_thetao * cos_thetao));
  const auto cot_thetao(cos_thetao / sin_thetao);
  auto xmax(erff(cot_thetao));
  auto xmin(-1.0);
  auto x(-0.0564);
  x = thetao * x + 0.4265;
  x = thetao * x - 0.876;
  x = thetao * x + 1;
  x = xmax - (1 + xmax) * #pow(1 - xi0, x); 
  /*
  const auto visible_cdf_norm(1.0 / beckmann_visible_cdf(cot_thetao));
  for (int iter = 0; iter < 1000; iter++) {
    const auto a(inverse_erff(x));
    const auto f(x >= cot_thetao ? 1 : visible_cdf_norm * beckmann_visible_cdf(a));
    const auto g(0.5 * visible_cdf_norm * (cos_thetao - a * sin_thetao));
  }
  return float2(inverse_erff(x), inverse_erff(2 * xi1 - 1));
   */
  return float2(0);
}
@(pure noinline) auto smith_visible_normal_sample(const microfacet_distribution this, const float xi0, const float xi1, const float2 alpha, const float3 wo) {
  const auto w11(normalize(float3(alpha * wo.xy, wo.z)));
  const auto sin_theta(length(w11.xy));
  const auto cos_phi(w11.x / sin_theta);
  const auto sin_phi(w11.y / sin_theta);
  const auto m11(smith_visible_slope_sample(this, xi0, xi1, w11.z));
  const auto m(float2(
      alpha.x * dot(float2(cos_phi, -sin_phi), m11), 
      alpha.y * dot(float2(sin_phi, +cos_phi), m11)));
  return #all(isfinite(m)) ? normalize(float3(m, 1)) : wo.z == 0 ? normalize(wo) : float3(0, 0, 1);
}
@(pure) void blinn_normal_first_quadrant_sample(const float xi0, const float xi1, const float2 e, &float phi, &float cos_theta) {
  if (e.x == e.y) {
    *phi = $HALF_PI * xi0;
    *cos_theta = #pow(xi1, 1 / (1 + e.x));
  } else {
    *phi = #atan(#sqrt((1 + e.x) / (1 + e.y)) * #tan($HALF_PI * xi0));
    *cos_theta = #pow(xi1, 1 / (1 + e.x * (cos_phi := #cos(*phi)) * cos_phi + e.y * (sin_phi := #sin(*phi)) * sin_phi));
  }
}
@(pure) float3 blinn_normal_sample(const float xi0, const float xi1, const float2 e) {
  float phi = 0;
  float cos_theta = 0;
  if (xi0 < 0.25) {
    blinn_normal_first_quadrant_sample(4 * xi0, xi1, e, &phi, &cos_theta);
  } else if (xi0 < 0.5) {
    blinn_normal_first_quadrant_sample(4 * (0.5 - xi0), xi1, e, &phi, &cos_theta), phi = $PI - phi;
  } else if (xi0 < 0.75) {
    blinn_normal_first_quadrant_sample(4 * (xi0 - 0.5), xi1, e, &phi, &cos_theta), phi += $PI;
  } else {
    blinn_normal_first_quadrant_sample(4 * (1 - xi0), xi1, e, &phi, &cos_theta), phi = $TWO_PI - phi;
  }
  return float3(#sqrt(1.001 - cos_theta * cos_theta) * float2(#cos(phi), #sin(phi)), cos_theta);
}
@(pure) auto evaluate_bsdf(const &microfacet_bsdf this, inline const &evaluate_bsdf_parameters params) {
  const auto distribution(#typeof(this.distribution));
  const auto shadowing(#typeof(this.shadowing));
  preserve tangent_u;
  tangent_u = this.tangent_u;
  if (!perturb_tangent_space(params))
    return evaluate_bsdf_result(is_black: true);
  const auto reflect_chance(scatter_reflect_chance(this.mode));
  const auto wm(normalize(mode == scatter_reflect ? wo + wi : refraction_half_vector(wo, wi, ior)));
  const auto dot_wo_wm(#sum(wo * wm));
  const auto dot_wi_wm(#sum(wi * wm));
  if $(distribution == microfacet_distribution_blinn) {
    const auto e(2 / (this.alpha * this.alpha + STABILITY_EPS));
    const auto d(#pow(wm.z, (e.x * wm.x * wm.x + e.y * wm.y * wm.y) / (1 - wm.z * wm.z + STABILITY_EPS)) / $TWO_PI);
    const auto norm1(#sqrt(#prod(1 + e))); 
    const auto norm2(#sqrt(#prod(2 + e))); 
    const auto g(#min(1, 2 * wm.z * 
                 #min(#abs(wo.z / (dot_wo_wm + STABILITY_EPS)),
                      #abs(wi.z / (dot_wi_wm + STABILITY_EPS))))); 
    switch (int(mode) & int(this.mode)) {
      case int(scatter_reflect): {
        const auto fss_pdf(norm1 * d / (4 * float2(dot_wo_wm, dot_wi_wm) + STABILITY_EPS));
        const auto fss(norm2 * d * g / (4 * #abs(wo.z) + STABILITY_EPS));
        if (#is_void(this.multiscatter_tint)) {
          return evaluate_bsdf_result(pdf: reflect_chance * fss_pdf, f: this.tint * (reflect_chance * fss));
        } else {
          const auto r0(this.roughness0);
          const auto fms_pdf(#abs(wi.z) / $PI);
          const auto fms(return_from { 
            return 0.0;
          });
          return evaluate_bsdf_result(
            pdf: reflect_chance * (0.8 * fss_pdf + 0.2 * fms_pdf), 
            f: reflect_chance * this.tint * (fss + this.multiscatter_tint * fms));
        }
      }
      case int(scatter_transmit): {
        return evaluate_bsdf_result(is_black: true) if (!((dot_wo_wm > 0) & (dot_wi_wm < 0)));
      }
      default: break;
    } 
  } else {
    const auto d(smith_normal_pdf(this.distribution, this.alpha, wm));
    const auto lambdao(smith_lambda(this.distribution, #abs(wo.z) / (length(this.alpha * wo.xy) + STABILITY_EPS)));
    const auto lambdai(smith_lambda(this.distribution, #abs(wi.z) / (length(this.alpha * wi.xy) + STABILITY_EPS)));
    const auto proj_areao((1 + lambdao) * #abs(wo.z));
    const auto proj_areai((1 + lambdai) * #abs(wi.z));
    const auto g(return_from {
      if $(shadowing == microfacet_shadowing_smith) 
        return mode == scatter_reflect ? 1 / (1 + lambdao + lambdai) : betaf(1 + lambdao, 1 + lambdai);
      else
        return #min(1, 2 * wm.z * 
               #min(#abs(wo.z / (dot_wo_wm + STABILITY_EPS)), 
                    #abs(wi.z / (dot_wi_wm + STABILITY_EPS))));
    });
    switch (int(mode) & int(this.mode)) {
      case int(scatter_reflect): {
        const auto fss_pdf(d / (4 * float2(proj_areao, proj_areai) + STABILITY_EPS));
        const auto fss(d * g / (4 * #abs(wo.z) + STABILITY_EPS));
        if (#is_void(this.multiscatter_tint)) {
          return evaluate_bsdf_result(pdf: reflect_chance * fss_pdf, f: this.tint * (reflect_chance * fss));
        } else {
          const auto r0(this.roughness0);
          const auto fms_pdf(#abs(wi.z) / $PI);
          const auto fms(return_from { 
            if (distribution == microfacet_distribution_ggx) {
              const auto fit(return_from {
                auto fit(float3(0.0));
                if (r0 < 0.06299) {
                  fit = float3(29.2553519, 0.3728114, 0.1845677);
                } else if (r0 < 0.1259843) {
                  fit = float3(-6.2876897e+07, -4.9194766e+06, 3.3173219e+06);
                  fit = fit * r0 + float3( 3.4025124e+07,  2.4576227e+06, -1.6123845e+06);
                  fit = fit * r0 + float3(-7.2367180e+06, -4.8558372e+05,  3.0882525e+05);
                  fit = fit * r0 + float3( 7.5011789e+05,  4.7293248e+04, -2.9071452e+04);
                  fit = fit * r0 + float3(-3.7635419e+04, -2.2570320e+03,  1.3407965e+03);
                  fit = fit * r0 + float3( 7.5899155e+02,  4.2444028e+01, -2.4013229e+01);
                } else if (r0 < 0.503970) {
                  fit = float3(-4.2253228e+03, 1.6913746e+02, -7.8179263e+00);
                  fit = fit * r0 + float3( 9.6952817e+03, -3.5406749e+02,  1.1760316e+01);
                  fit = fit * r0 + float3(-8.8256161e+03,  2.7638671e+02, -5.2975185e+00);
                  fit = fit * r0 + float3( 4.0337334e+03, -9.8577485e+01,  1.2001528e+00);
                  fit = fit * r0 + float3(-9.5198727e+02,  1.5754557e+01, -1.4087179e-01);
                  fit = fit * r0 + float3( 1.0006441e+02, -1.4926819e-01,  1.1518670e-01);
                } else {
                  fit = float3(-5.9786880e+01, -5.8887035e+01, 4.1460911e+02);
                  fit = fit * r0 + float3( 2.2101716e+02,  2.1996966e+02, -1.3466323e+03);
                  fit = fit * r0 + float3(-3.3692861e+02, -3.1412912e+02,  1.7465495e+03);
                  fit = fit * r0 + float3( 2.8006161e+02,  2.1178043e+02, -1.1260412e+03);
                  fit = fit * r0 + float3(-1.3115488e+02, -6.7437900e+01,  3.6066098e+02);
                  fit = fit * r0 + float3( 2.8927807e+01,  8.9207463e+00, -4.5752769e+01);
                }
                fit.z *= #pow(2.71828182459 * fit.x * fit.y, 1.0 / fit.y);
                return fit;
              });
              const auto Ewo(#exp(-fit[2] * #abs(wo.z) * #exp(-fit[0] * #pow(#abs(wo.z), fit[1]))));
              const auto Ewi(#exp(-fit[2] * #abs(wi.z) * #exp(-fit[0] * #pow(#abs(wi.z), fit[1]))));
              const auto Eav(return_from {
                float fit = -0.40461439;
                fit = fit * r0 + 2.33942628;
                fit = fit * r0 - 3.15953698;
                fit = fit * r0 + 0.69762445;
                fit = fit * r0 - 0.06449884;
                fit = fit * r0 + 1.00125673;
                return fit;
              });
              return (1 - Ewo) * (1 - Ewi) / (1 - Eav + STABILITY_EPS) * #abs(wi.z) / $PI;
            } else {
              return 0;
            }
          });
          return evaluate_bsdf_result(
            pdf: reflect_chance * (0.8 * fss_pdf + 0.2 * fms_pdf), 
            f: reflect_chance * this.tint * (fss + this.multiscatter_tint * fms));
        } 
      }
      case int(scatter_transmit): {
        return evaluate_bsdf_result(is_black: true) if (!((dot_wo_wm > 0) & (dot_wi_wm < 0)));
        const auto j(float2(
          refraction_half_vector_jacobian(wo, wi, ior),
          refraction_half_vector_jacobian(wi, wo, 1 / ior)));
        const auto fss_pdf(d * j * float2(dot_wo_wm, -dot_wi_wm) / (float2(proj_areao, proj_areai) + STABILITY_EPS));
        const auto fss(d * g * j[0] * dot_wo_wm / (#abs(wo.z) + STABILITY_EPS));
        return evaluate_bsdf_result(pdf: (1 - reflect_chance) * fss_pdf, f: this.tint * ((1 - reflect_chance) * fss));
      }
      default: break;
    }
  }
  return evaluate_bsdf_result(is_black: true);
}
@(pure) auto evaluate_bsdf_sample(const &microfacet_bsdf this, inline const &evaluate_bsdf_sample_parameters params) {
  const auto distribution(#typeof(this.distribution));
  preserve tangent_u;
  tangent_u = this.tangent_u;
  auto tbn(perturb_tangent_space(params));
  if (!tbn)
    return evaluate_bsdf_sample_result();
  const auto mode(weighted_bool_sample(&xi.z, scatter_reflect_chance(this.mode)) ? scatter_reflect : scatter_transmit);
  if (!#is_void(this.multiscatter_tint) && mode == scatter_reflect) {
    if (weighted_bool_sample(&xi.w, 0.2))
      return evaluate_bsdf_sample_result(wi: (*tbn) * cosine_hemisphere_sample(xi.xy), mode: scatter_reflect);
  }
  const auto wm(return_from {
    if $(distribution == microfacet_distribution_blinn)
      return blinn_normal_sample(xi.x, xi.y, 2 / (this.alpha * this.alpha + STABILITY_EPS));
    else
      return smith_visible_normal_sample(this.distribution, xi.x, xi.y, this.alpha, wo);
  });
  const auto wi(normalize(mode == scatter_reflect ? reflect(wo, wm) : refract(wo, wm, ior)));
  return evaluate_bsdf_sample_result(wi: (*tbn) * wi, mode: mode);
}
export struct ward_geisler_moroder_bsdf: bsdf {
  const float roughness_u;
  const float roughness_v = roughness_u;
  const color_or_float tint = 1.0;
  const color_or_float_or_void multiscatter_tint = null; 
  const float3 tangent_u = state::texture_tangent_u(0);
  void string handle = "";
};
@(pure noinline) auto evaluate_bsdf(const &ward_geisler_moroder_bsdf this, inline const &evaluate_bsdf_parameters params) {
  if (mode == scatter_reflect) {
    preserve tangent_u;
    tangent_u = this.tangent_u;
    if (!perturb_tangent_space(params))
      return evaluate_bsdf_result(is_black: true);
    const auto roughness(saturate(float2(this.roughness_u, this.roughness_v)));
    const auto alpha(#max(0.001, #pow(roughness, 2)));
    const auto cos_thetao(#abs(wo.z));
    const auto cos_thetai(#abs(wi.z));
    const auto f(#sum((vh := wo + wi) * vh) / ($PI * alpha.x * alpha.y * #pow(vh.z, 4)) * #exp(-#sum((g := vh.xy / (vh.z * alpha)) * g)));
    const auto fss_pdf(float2(f * (cos_thetao + cos_thetai) / 2));
    const auto fss(f * cos_thetai);
    if (#is_void(this.multiscatter_tint)) {
      return evaluate_bsdf_result(pdf: fss_pdf, f: this.tint * fss);
    } else {
      const auto fms_pdf(float2(cos_thetai, cos_thetao) / $PI);
      const auto fms(let {
        const auto r0(#sqrt(roughness.x * roughness.y));
        const auto fit(return_from {
          float3 fit(-1.1992005e+02, -1.4040313e+01, 7.8306640e-01);
          fit = fit * r0 + float3( 4.1985368e+02,  4.6807753e+01, -1.6213743e+00);
          fit = fit * r0 + float3(-5.8448171e+02, -6.1370147e+01, -1.3797964e+00);
          fit = fit * r0 + float3( 4.2351783e+02,  4.1399258e+01,  5.6539624e+00);
          fit = fit * r0 + float3(-1.6959530e+02, -1.4979874e+01, -3.8064856e+00);
          fit = fit * r0 + float3( 3.7025769e+01,  3.0665596e+00, -1.2666234e-01);
          fit = fit * r0 + float3(-3.4191809e+00, -2.9108604e-01, -1.8175253e-02);
          fit = fit * r0 + float3( 1.6044891e-01,  8.8001559e-03,  1.4868175e-03);
          fit = fit * r0 + float3(-7.1467185e-04,  1.8095055e-01,  9.9998607e-01);
          return fit;
        });
        const auto Ewo(#min(1 - fit[1] * (t := #pow(cos_thetao / fit[0], 2.0 / 3.0)) * #exp(1 - t), 0.999));
        const auto Ewi(#min(1 - fit[1] * (t := #pow(cos_thetai / fit[0], 2.0 / 3.0)) * #exp(1 - t), 0.999));
        const auto Eav(#min(fit[2], 0.999));
      } in (1 - Ewo) * (1 - Ewi) / (1 - Eav) * cos_thetai / $PI);
      return evaluate_bsdf_result(pdf: 0.8 * fss_pdf + 0.2 * fms_pdf, f: this.tint * (fss + this.multiscatter_tint * fms));
    }
  } else {
    return evaluate_bsdf_result(is_black: true);
  }
}
@(pure noinline) auto evaluate_bsdf_sample(const &ward_geisler_moroder_bsdf this, const &evaluate_bsdf_sample_parameters params) {
  preserve tangent_u;
  tangent_u = this.tangent_u;
  auto tbn(perturb_tangent_space(params));
  if (!tbn)
    return evaluate_bsdf_sample_result();
  if (#is_void(this.multiscatter_tint) || weighted_bool_sample(&params.xi.w, 0.8)) { 
    auto wo(params.wo);
    const auto roughness(saturate(float2(this.roughness_u, this.roughness_v)));
    const auto alpha(#max(0.001, #pow(roughness, 2)));
    const auto phi(atan2(alpha.y * #sin(t := $TWO_PI * params.xi.x), alpha.x * #cos(t)));
    const auto cos_phi(#cos(phi)); 
    const auto sin_phi(#sin(phi));
    const auto theta(#atan(#sqrt(-#log(1 - params.xi.y) / (#pow(cos_phi / alpha.x, 2) + #pow(sin_phi / alpha.y, 2)))));
    const auto wm(float3(#sin(theta) * float2(cos_phi, sin_phi), #cos(theta)));
    auto wi(normalize(reflect(wo, wm)));
    if (wi.z < 0) {
      return evaluate_bsdf_sample_result(); 
    } else {
      return evaluate_bsdf_sample_result(wi: (*tbn) * wi, mode: scatter_reflect);
    }
  } else {
    return evaluate_bsdf_sample_result(wi: (*tbn) * cosine_hemisphere_sample(params.xi.xy), mode: scatter_reflect);
  }
}
@(macro)
auto evaluate_bsdf(const &auto this, const &evaluate_bsdf_parameters params, const float3 normal) {
  preserve params.normal;
  params.normal = normal;
  return evaluate_bsdf(this, params);
}
@(macro)
auto evaluate_bsdf_sample(const &auto this, const &evaluate_bsdf_sample_parameters params, const float3 normal) {
  preserve params.normal;
  params.normal = normal;
  return evaluate_bsdf_sample(this, params);
}
export struct weighted_layer: bsdf {
  float weight;
  bsdf layer = bsdf();
  bsdf base = bsdf();
  float3 normal = state::normal();
};
@(macro)
auto evaluate_bsdf(const &weighted_layer this, const &evaluate_bsdf_parameters params) {
  const auto result0(evaluate_bsdf(&this.base, params));
  const auto result1(evaluate_bsdf(&this.layer, params, this.normal));
  return evaluate_bsdf_result(pdf: lerp(result0.pdf, result1.pdf, this.weight), f: lerp(result0.f, result1.f, this.weight));
}
@(macro)
auto evaluate_bsdf_sample(const &weighted_layer this, const &evaluate_bsdf_sample_parameters params) {
  return weighted_bool_sample(&params.xi.z, this.weight) 
    ? evaluate_bsdf_sample(&this.layer, params, this.normal) 
    : evaluate_bsdf_sample(&this.base, params);
}
export struct color_weighted_layer: bsdf {
  color weight;
  bsdf layer = bsdf();
  bsdf base = bsdf();
  float3 normal = state::normal();
};
@(macro)
auto evaluate_bsdf(const &color_weighted_layer this, const &evaluate_bsdf_parameters params) {
  const auto result0(evaluate_bsdf(&this.base, params));
  const auto result1(evaluate_bsdf(&this.layer, params, this.normal));
  return evaluate_bsdf_result(pdf: lerp(result0.pdf, result1.pdf, average(this.weight)), f: lerp(result0.f, result1.f, this.weight));
}
@(macro)
auto evaluate_bsdf_sample(const &color_weighted_layer this, const &evaluate_bsdf_sample_parameters params) {
  const auto i(uniform_wavelength_index_sample(&params.xi.w));
  return weighted_bool_sample(&params.xi.z, this.weight[i])
    ? evaluate_bsdf_sample(&this.layer, params, this.normal)
    : evaluate_bsdf_sample(&this.base, params);
}
export struct fresnel_layer: bsdf {
  float ior;
  float weight = 1.0;
  bsdf layer = bsdf();
  bsdf base = bsdf();
  float3 normal = state::normal();
};
@(macro)
auto evaluate_bsdf(const &fresnel_layer this, inline const &evaluate_bsdf_parameters params) {
  preserve ior;
  ior = backface ? (1 / this.ior) : this.ior;
  const auto result0(evaluate_bsdf(&this.base, params));
  const auto result1(evaluate_bsdf(&this.layer, params, this.normal));
  return evaluate_bsdf_result(
    pdf: lerp(result0.pdf, result1.pdf, this.weight * schlick_fresnel(float2(wo.z, wi.z), schlick_F0(ior))),
    f:   lerp(result0.f,   result1.f,   this.weight * dielectric_fresnel(dot(wo, half_direction(params)), ior)));
}
@(macro)
auto evaluate_bsdf_sample(const &fresnel_layer this, inline const &evaluate_bsdf_sample_parameters params) {
  preserve ior;
  ior = backface ? (1 / this.ior) : this.ior;
  float chance(this.weight * schlick_fresnel(wo.z, schlick_F0(ior)));
  if (weighted_bool_sample(&xi.z, chance)) {
    auto result(evaluate_bsdf_sample(&this.layer, params, this.normal));
    *result.delta_f *= (this.weight * dielectric_fresnel(dot(wo, normalize(wo + result.wi)), ior) / chance) if (result.delta_f);
    return result;
  } else {
    auto result(evaluate_bsdf_sample(&this.base, params));
    *result.delta_f *= (1 - this.weight * dielectric_fresnel(dot(wo, refraction_half_vector(wo, result.wi, ior)), ior)) / (1 - chance) if (result.delta_f);
    return result;
  }
}
export struct custom_curve_layer: bsdf {
  float normal_reflectivity;
  float grazing_reflectivity = 1.0;
  float exponent = 5.0;
  float weight = 1.0;
  bsdf layer = bsdf();
  bsdf base = bsdf();
  float3 normal = state::normal();
};
@(pure macro) auto evaluate_bsdf(const &custom_curve_layer this, inline const &evaluate_bsdf_parameters params) {
  const auto result0(evaluate_bsdf(&this.base, params));
  const auto result1(evaluate_bsdf(&this.layer, params, this.normal));
  const auto F(schlick_fresnel(auto(wo.z, wi.z, dot(wo, half_direction(params))), this.normal_reflectivity, this.grazing_reflectivity, this.exponent));
  return evaluate_bsdf_result(
    pdf: lerp(result0.pdf, result1.pdf, this.weight * F.xy),
    f:   lerp(result0.f,   result1.f,   this.weight * F.z));
}
@(pure macro) auto evaluate_bsdf_sample(const &custom_curve_layer this, const &evaluate_bsdf_sample_parameters params) {
  return weighted_bool_sample(&params.xi.z, this.weight * schlick_fresnel(params.wo.z, this.normal_reflectivity, this.grazing_reflectivity, this.exponent))
    ? evaluate_bsdf_sample(&this.layer, params, this.normal)
    : evaluate_bsdf_sample(&this.base, params);
}
struct tint1: bsdf {
  color tint;
  bsdf base;
};
struct tint2: bsdf {
  color reflection_tint;
  color transmission_tint;
  bsdf base;
};
export @(pure macro) auto tint(const color tint, const bsdf base) {
  return tint1(tint, base);
}
export @(pure macro) auto tint(const color reflection_tint, const color transmission_tint, const bsdf base) {
  return tint2(reflection_tint, transmission_tint, base);
}
@(pure macro) auto evaluate_bsdf(const &tint1 this, const &evaluate_bsdf_parameters params) {
  auto result(evaluate_bsdf(&this.base, params));
  if (!result.is_black) result.f *= this.tint;
  return result;
}
@(pure macro) auto evaluate_bsdf(const &tint2 this, const &evaluate_bsdf_parameters params) {
  auto result(evaluate_bsdf(&this.base, params));
  if (!result.is_black) result.f *= params.mode == scatter_reflect ? this.reflection_tint : this.transmission_tint;
  return result;
}
@(pure macro) auto evaluate_bsdf_sample(const &tint1 this, const &evaluate_bsdf_sample_parameters params) {
  auto result(evaluate_bsdf_sample(&this.base, params));
  if ((result.mode != scatter_none) & bool(result.delta_f)) *result.delta_f *= this.tint;
  return result;
}
@(pure macro) auto evaluate_bsdf_sample(const &tint2 this, const &evaluate_bsdf_sample_parameters params) {
  auto result(evaluate_bsdf_sample(&this.base, params));
  if ((result.mode != scatter_none) & bool(result.delta_f))
    *result.delta_f *= result.mode == scatter_reflect ? this.reflection_tint : this.transmission_tint;
  return result;
}
export struct fresnel_factor: bsdf {
  color ior;
  color extinction_coefficient;
  bsdf base = bsdf();
};
export struct thin_film: bsdf {
  float thickness; 
  color ior;
  bsdf base = bsdf();
};
export struct directional_factor: bsdf {
  color_or_float normal_tint = 1.0;
  color_or_float grazing_tint = 1.0;
  float exponent = 5.0;
  bsdf base = bsdf();
};
@(pure macro) auto evaluate_bsdf(const &directional_factor this, inline const &evaluate_bsdf_parameters params) {
  auto result = evaluate_bsdf(&this.base, params);
  if (!result.is_black && mode == scatter_reflect)
    result.f *= schlick_fresnel(dot(wo, half_direction(params)), this.normal_tint, this.grazing_tint, this.exponent);
  return result;
}
@(pure macro) auto evaluate_bsdf_sample(const &directional_factor this, inline const &evaluate_bsdf_parameters params) {
  return evaluate_bsdf_sample(&this.base, params); 
}
tag component;
export struct bsdf_component: component {
  float weight    = 0.0;
  bsdf  component = bsdf();
  float chance    = weight; 
};
export struct edf_component: component {
  float weight    = 0.0;
  edf   component = edf();
  float chance    = weight; 
};
export struct vdf_component: component {
  float weight    = 0.0;
  vdf   component = vdf();
  float chance    = weight; 
};
struct component_mix: bsdf, edf, vdf {
  component[] components;
};
export auto normalized_mix(component[<N>] components) {
  float total_weight(0);
  float total_chance(0);
  for (int i = 0; i < N; i++) {
    auto component(&components[i]);
    component.weight = #max(component.weight, 0.0);
    component.chance = #max(component.chance, 0.0);
    total_weight += component.weight;
    total_chance += component.chance;
  }
  if (total_weight > 1.0)
    total_weight = 1.0 / total_weight;
  else
    total_weight = 1.0;
  total_chance = 1.0 / total_chance if (total_chance > 0.0);
  for (int i = 0; i < N; i++) {
    auto component(&components[i]);
    component.weight *= total_weight;
    component.chance *= total_chance;
  }
  return component_mix(components);
}
export auto clamped_mix(component[<N>] components) {
  float total_weight(0);
  float total_chance(0);
  for (int i = 0; i < N; i++) {
    auto component(&components[i]);
    component.weight = #max(component.weight, 0.0);
    component.chance = #max(component.chance, 0.0);
    if (total_weight + component.weight < 1.0) {
      total_weight += component.weight;
      total_chance += component.chance;
    } else {
      component.weight = 1.0 - total_weight;
      for (int j = i + 1; j < N; j++) {
        components[j].weight = 0;
        components[j].chance = 0;
      }
      break;
    }
  }
  total_chance = 1.0 / total_chance if (total_chance > 0.0);
  for (int i = 0; i < N; i++) {
    components[i].chance *= total_chance;
  }
  return component_mix(components);
}
@(pure macro) auto evaluate_bsdf(const &component_mix this, const &evaluate_bsdf_parameters params) {
  const auto N(this.components.size);
  auto result(evaluate_bsdf_result(is_black: true));
  for (int i = 0; i < N; i++) {
    visit component in this.components[i] { 
      auto component_result(evaluate_bsdf(&component.component, params));
      if (!component_result.is_black) {
        result.pdf += component.chance * component_result.pdf;
        result.f   += component.weight * component_result.f;
        result.is_black = false;
      }
    }
  }
  return result;
}
@(pure macro) auto evaluate_bsdf_sample(const &component_mix this, const &evaluate_bsdf_sample_parameters params) {
  const auto N(this.components.size);
  const auto xi(&params.xi.y);
  for (int i = 0; i < N; i++) {
    visit component in this.components[i] { 
      if (!(*xi < component.chance)) {
        *xi -= component.chance;
      } else {
        *xi /= component.chance;
        auto result(evaluate_bsdf_sample(&component.component, params));
        if ((result.mode != scatter_none) & bool(result.delta_f))
          *result.delta_f *= component.weight;
        return result;
      }
    }
  }
  return evaluate_bsdf_sample_result();
}
tag color_component;
export struct color_bsdf_component: color_component {
  color weight    = color(0.0);
  bsdf  component = bsdf();
  float chance    = average(weight); 
};
export struct color_edf_component: color_component {
  color weight    = color(0.0);
  edf   component = edf();
  float chance    = average(weight); 
};
export struct diffuse_edf: edf {
  void string handle = "";
};
export struct spot_edf: edf {
  float exponent;
  float spread = $PI;
  bool global_distribution = true;
  float3x3 global_frame = float3x3(1.0);
  void string handle = "";
};
export struct anisotropic_vdf: vdf {
  float directional_bias = 0.0;
  void string handle = "";
};
export @(macro) int material__evaluate_bsdf(
    const &material this, const &float3 wo, const &float3 wi,  
    const &float pdf_fwd, const &float pdf_rev, const &color f) {
  auto ior(1 / 1.4); 
  auto primary_wo(normalize(state::transform_vector(state::coordinate_object, state::coordinate_internal, *wo)));
  auto primary_wi(normalize(state::transform_vector(state::coordinate_object, state::coordinate_internal, *wi)));
  auto backface(primary_wo.z < 0);
  if (backface) {
    primary_wo = -primary_wo;
    primary_wi = -primary_wi;
    ior = 1 / ior;
  }
  auto params(evaluate_bsdf_parameters(primary_wo: primary_wo, primary_wi: primary_wi, thin_walled: this.thin_walled, backface: backface, normal: this.geometry.normal, ior: ior));
  auto result(return_from {
    if (#typeof(this.backface) != #typeof(material_surface()) && backface) 
      return evaluate_bsdf(&this.backface.scattering, &params);
    else
      return evaluate_bsdf(&this.surface.scattering, &params);
  });
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
export @(macro) int material__evaluate_bsdf_sample(
    const &material this, const &float4 xi, const &float3 wo, 
    const &float3 wi, const &float pdf_fwd, const &float pdf_rev, const &color f, const &int is_delta) {
  auto ior(1 / 1.4); 
  auto primary_wo(normalize(state::transform_vector(state::coordinate_object, state::coordinate_internal, *wo)));
  auto backface(primary_wo.z < 0);
  if (backface) {
    primary_wo = -primary_wo;
    ior = 1 / ior;
  }
  auto sample_params(evaluate_bsdf_sample_parameters(primary_wo: primary_wo, thin_walled: this.thin_walled, backface: backface, xi: *xi));
  auto sample_result(return_from {
    if (#typeof(this.backface) != #typeof(material_surface()) && backface)
      return evaluate_bsdf_sample(&this.backface.scattering, &sample_params);
    else
      return evaluate_bsdf_sample(&this.surface.scattering, &sample_params);
  });
  return false if (sample_result.mode == scatter_none);
  auto primary_wi(normalize(sample_result.wi));
  const auto mode((primary_wo.z < 0) == (primary_wi.z < 0) ? scatter_reflect : scatter_transmit);
  return false if (mode != sample_result.mode);
  *wi = normalize(state::transform_vector(state::coordinate_internal, state::coordinate_object, primary_wi));
  *wi = -*wi if (backface);
  if (sample_result.delta_f) {
    *pdf_fwd = 1.0;
    *pdf_rev = 1.0;
    *f = *sample_result.delta_f;
    *is_delta = true;
  } else {
    auto params(evaluate_bsdf_parameters(primary_wo: primary_wo, primary_wi: primary_wi, thin_walled: this.thin_walled, backface: backface, ior: ior));
    auto result(return_from {
      if (#typeof(this.backface) != #typeof(material_surface()) && backface)
        return evaluate_bsdf(&this.backface.scattering, &params);
      else
        return evaluate_bsdf(&this.surface.scattering, &params);
    });
    return false if (result.is_black);
    *pdf_fwd = result.pdf[0];
    *pdf_rev = result.pdf[1];
    *f = result.f;
    *is_delta = false;
  }
  return true;
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
export @(pure macro) auto frac(const auto a) = a - #floor(a);
export @(pure macro) auto fmod(const auto a, const auto b) = a % b;
export @(pure macro) auto isfinite(const auto a) = #isfpclass(a, 0b0111111000);
export @(pure macro) auto isnormal(const auto a) = #isfpclass(a, 0b0100001000);
export @(pure macro) auto isinf(const auto a) = #isfpclass(a, 0b1000000100);
export @(pure macro) auto isnan(const auto a) = #isfpclass(a, 0b0000000011);
export @(pure macro) auto sign(const auto a) = #sign(a);
export @(pure macro) auto sqrt(const auto a) = #sqrt(a);
export @(pure macro) auto rsqrt(const auto a) = 1.0 / #sqrt(a);
export @(pure macro) auto pow(const auto a, const auto b) = #pow(a, b);
export @(pure macro) auto cos(const auto a) = #cos(a);
export @(pure macro) auto sin(const auto a) = #sin(a);
export @(pure macro) auto tan(const auto a) = #tan(a);
export @(pure macro) auto acos(const auto a) = #acos(a);
export @(pure macro) auto asin(const auto a) = #asin(a);
export @(pure macro) auto atan(const auto a) = #atan(a);
export @(pure macro) auto cosh(const auto a) = #cosh(a);
export @(pure macro) auto sinh(const auto a) = #sinh(a);
export @(pure macro) auto tanh(const auto a) = #tanh(a);
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
export @(pure macro) auto transpose(const auto a) = #transpose(a);
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
@(pure foreign) float atan2f(float y, float x); 
export @(pure macro) auto atan2(const float y, const float x) = atan2f(y, x);
export @(pure macro) auto atan2(const float2 y, const float2 x) = float2(atan2(y[0], x[0]), atan2(y[1], x[1]));
export @(pure macro) auto atan2(const float3 y, const float3 x) = float3(atan2(y[0], x[0]), atan2(y[1], x[1]), atan2(y[2], x[2]));
export @(pure macro) auto atan2(const float4 y, const float4 x) = float4(atan2(y[0], x[0]), atan2(y[1], x[1]), atan2(y[2], x[2]), atan2(y[3], x[3]));
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
export enum gamma_mode { gamma_default = 0, gamma_linear = 0, gamma_srgb = 1 };
@(pure macro) float4 apply_gamma_mode(const gamma_mode gamma, const float4 texel) = gamma == gamma_srgb ? float4((texel * texel).xyz, texel.w) : texel;
@(pure macro) float3 apply_gamma_mode(const gamma_mode gamma, const float3 texel) = gamma == gamma_srgb ? (texel * texel) : texel;
@(pure macro) float2 apply_gamma_mode(const gamma_mode gamma, const float2 texel) = gamma == gamma_srgb ? (texel * texel) : texel;
@(pure macro) float apply_gamma_mode(const gamma_mode gamma, const float texel) = gamma == gamma_srgb ? (texel * texel) : texel;
@(pure) &tile_2d access_uv_tile(const texture_2d tex, const int2 uv_tile) {
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
export @(macro) color texel_color(const texture_2d tex, const int2 coord, const int2 uv_tile = int2(0)) = color(access_texel(tex, coord, uv_tile).xyz);
@(pure) float4 access_texel(const texture_3d tex, const int3 coord) {
  return null if (#any((coord < 0) | (coord >= tex.extent)));
  return tex.texels[#sum(tex.stride * coord)];
}
export @(pure macro) float4 texel_float4(const texture_3d tex, const int3 coord) = access_texel(tex, coord);
export @(pure macro) float3 texel_float3(const texture_3d tex, const int3 coord) = access_texel(tex, coord).xyz;
export @(pure macro) float2 texel_float2(const texture_3d tex, const int3 coord) = access_texel(tex, coord).xy;
export @(pure macro) float texel_float(const texture_3d tex, const int3 coord) = access_texel(tex, coord).x;
export @(macro) color texel_color(const texture_3d tex, const int3 coord) = color(access_texel(tex, coord).xyz);
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
export @(macro) color lookup_color(
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

static const char *monte_carlo = R"*(#smdl_syntax
import ::math::*;
export @(pure) float2 advance_low_discrepancy(const &float2 xi) = (*xi = math::frac(*xi + float2(0.75487766, 0.56984029)));
export @(pure) float3 advance_low_discrepancy(const &float3 xi) = (*xi = math::frac(*xi + float3(0.81917251, 0.67104360, 0.54970047)));
export @(pure) float4 advance_low_discrepancy(const &float4 xi) = (*xi = math::frac(*xi + float4(0.85667488, 0.73389185, 0.62870672, 0.53859725)));
export @(pure) bool weighted_bool_sample(const &float xi, const float weight) {
  if (*xi < weight) {
    *xi = (*xi / weight);
    return true;
  } else {
    *xi = (*xi - weight) / (1 - weight);
    return false;
  }
}
export @(pure) int uniform_wavelength_index_sample(const &float xi) {
  const int i(#min(int(*xi *= $WAVELENGTH_BASE_MAX), $WAVELENGTH_BASE_MAX - 1));
  *xi -= i;
  return i;
}
export @(pure) float2 uniform_disk_sample(float2 xi) {
  xi = 2 * xi - 1;
  xi = #select(xi == 0, 1e-5, xi);
  const bool cond((absxi := #abs(xi), absxi.x > absxi.y));
  const float rad(#select(cond, xi.x, xi.y));
  const float phi(#select(cond, ($PI / 4) * xi.y / xi.x, ($PI / 2) - ($PI / 4) * xi.x / xi.y));
  return rad * float2(#cos(phi), #sin(phi));
}
export @(pure) float3 cosine_hemisphere_sample(float2 xi) = float3((p := uniform_disk_sample(xi)), #sqrt(#max(1 - #sum(p * p), 0)));
)*";

static const char *quat = R"*(#smdl_syntax
using ::math import *;
export @(pure macro) float4 quat_rotate(const float theta, const float3 v) = float4(#sin(theta / 2) * normalize(v), #cos(theta / 2));
export @(pure macro) float4 quat_rotate(const float3 u, const float3 v) = normalize(float4(cross(u, v), 1 + dot(u, v)));
export @(pure macro) float4 quat_transpose(const float4 q) = float4(-1.0, -1.0, -1.0, 1.0) * q;
export @(pure macro) float3 quat_transform_vector(const float4 q, const float3 u) = (w := q.w) * w * u + (v := q.xyz) * dot(v, u) + cross(v, 2 * w * u + cross(v, u));
export @(pure macro) float4 quat_multiply(const float4 q, const float4 r) = float4(1.0, 1.0, 1.0, -1.0) * (q.wwwx * r.xyzx + q.xyzy * r.wwwy + q.yzxz * r.zxyz - q.zxyw * r.yzxw);
export @(pure macro) float3 quat_unit_x(const float4 q) = quat_transform_vector(q, float3(1.0, 0.0, 0.0));
export @(pure macro) float3 quat_unit_y(const float4 q) = quat_transform_vector(q, float3(0.0, 1.0, 0.0));
export @(pure macro) float3 quat_unit_z(const float4 q) = quat_transform_vector(q, float3(0.0, 0.0, 1.0));
export @(pure noinline) float3x3 quat_to_float3x3(const float4 q) {
  const float4 q2(q * q);
  const float xx(#sum(float4(1.0, -1.0, -1.0, 1.0) * q2));
  const float yy(#sum(float4(-1.0, 1.0, -1.0, 1.0) * q2));
  const float zz(#sum(float4(-1.0, -1.0, 1.0, 1.0) * q2));
  const float2 x_yz(2.0 * (q.xx * q.yz + float2(1.0, -1.0) * q.ww * q.zy));
  const float2 y_zx(2.0 * (q.yy * q.zx + float2(1.0, -1.0) * q.ww * q.xz));
  const float2 z_xy(2.0 * (q.zz * q.xy + float2(1.0, -1.0) * q.ww * q.yx));
  return float3x3(
    float3(xx, x_yz[0], x_yz[1]),
    float3(y_zx[1], yy, y_zx[0]),
    float3(z_xy[0], z_xy[1], zz));
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
export @(hot noinline) color rgb_to_color_implementation(float3 rgb) {
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
export @(macro) color rgb_to_color(const float3 rgb) {
  if (#all(rgb.xx == rgb.yz)) {
    return color(rgb.x);
  } else {
    return rgb_to_color_implementation(rgb);
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
@(visible noinline) void rgb_to_color_jit(const &float3 rgb, const &float cptr) {
  color c(rgb_to_color(*rgb));
  #memcpy(cptr, &c, #sizeof(color));
}
@(visible noinline) void color_to_rgb_jit(const &float cptr, const &float3 rgb) {
  *rgb = color_to_rgb(color(cptr));
}
)*";

static const char *specular = R"*(#smdl_syntax
export @(pure macro) auto reflect(const float3 wi, const float3 wm) = 2 * #sum(wi * wm) * wm - wi;
export @(pure macro) auto refract(const float3 wi, const float3 wm, const float ior, const float cos_thetat) = -ior * wi + (ior * #sum(wi * wm) + cos_thetat) * wm;
export @(pure macro) auto refract(const float3 wi, const float3 wm, const float ior) {
  return refract(wi, wm, ior, #sqrt(1 - ior * ior * (1 - (cos_thetai := #sum(wi * wm)) * cos_thetai)) * -#sign(cos_thetai));
}
export @(pure macro) auto refraction_half_vector(const float3 wo, const float3 wi, const float ior) = (vh := -ior * wo + wi) * #sign(vh.z);
export @(pure macro) auto refraction_half_vector_jacobian(const float3 wo, const float3 wi, const float ior) {
  return #abs(#sum(wi * (vh := refraction_half_vector(wo, wi, ior)))) / ((vh2 := #sum(vh * vh)) * #sqrt(vh2));
}
export @(pure macro) auto schlick_F0(const auto ior) = #pow((ior - 1) / (ior + 1), 2);
export @(pure macro) auto schlick_fresnel(
    const auto cos_theta,
    const auto F0, 
    const auto F90 = 1.0,
    const float exponent = 5) = F0 + (F90 - F0) * #pow(#max(1 - #abs(cos_theta), 0), exponent);
export @(pure) float dielectric_fresnel(const float cos_thetai, const float ior) {
  const auto cos2_thetat(1 - ior * ior * (1 - cos_thetai * cos_thetai));
  return 1 if (cos2_thetat < 0);
  const auto cos_thetat(#sqrt(cos2_thetat) * #sign(cos_thetai));
  const auto rs((ior * cos_thetai - cos_thetat) / (ior * cos_thetai + cos_thetat));
  const auto rp((cos_thetai - ior * cos_thetat) / (cos_thetai + ior * cos_thetat));
  return #min(0.5 * (rs * rs + rp * rp), 1.0);
}
)*";

[[nodiscard]] static const char *get_src(auto name) {
  if (name == "anno")
    return anno;
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
  if (name == "monte_carlo")
    return monte_carlo;
  if (name == "quat")
    return quat;
  if (name == "rgb")
    return rgb;
  if (name == "specular")
    return specular;
  return nullptr;
}

} // namespace smdl::Compiler::builtins
