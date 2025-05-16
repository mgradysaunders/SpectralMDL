#pragma once

#include <string_view>

namespace smdl::builtin {

static const char *anno = R"*(#smdl
)*";

static const char *api = R"*(#smdl
const int RGB_TO_COLOR_NUM_WAVELENGTHS=32;
const float RGB_TO_COLOR_MIN_WAVELENGTH=380.0;
const float RGB_TO_COLOR_MAX_WAVELENGTH=720.0;
const static auto RGB_TO_COLOR_CURVES=auto[](auto[](1.0618958,1.0615020,1.0614336,1.0622711,1.0622036,1.0625060,1.0623939,1.0624707,1.0625048,1.0624366,1.0620694,1.0613167,1.0610334,1.0613868,1.0614215,1.0620337,1.0625497,1.0624317,1.0625249,1.0624278,1.0624750,1.0625539,1.0625327,1.0623922,1.0623651,1.0625256,1.0612278,1.0594263,1.0599811,1.0602547,1.0601263,1.0606565),auto[](1.0414628,1.0328661,1.0126146,1.0350461,1.0078661,1.0422280,1.0442597,1.0535238,1.0180776,1.0442730,1.0529362,1.0537034,1.0533901,1.0537783,1.0527093,1.0530449,1.0550555,1.0553674,1.0454307,0.6234895,0.1803807,-0.0076304,-0.0001522,-0.0075102,-0.0021709,0.0006592,0.0122788,-0.0044670,0.0171198,0.0049211,0.0058763,0.0252594),auto[](0.9942214,0.9898694,0.9829366,0.9962787,1.0198956,1.0166396,1.0220913,0.9965166,1.0097766,1.0215422,0.6403195,0.0025012,0.0065340,0.0028334,-0.0000000,-0.0090592,0.0033937,-0.0030639,0.2220394,0.6314114,0.9748099,0.9720956,1.0173770,0.9987519,0.9470173,0.8525862,0.9489780,0.9475188,0.9959894,0.8630135,0.8915099,0.8486649),auto[](0.0055741,-0.0047983,-0.0052537,-0.0064571,-0.0059694,-0.0021837,0.0167811,0.0960964,0.2121736,0.3616913,0.5396101,0.7440881,0.9220957,1.0460304,1.0513825,1.0511992,1.0510530,1.0517397,1.0516043,1.0511944,1.0511590,1.0516613,1.0514039,1.0515941,1.0511460,1.0515124,1.0508871,1.0508924,1.0477493,1.0493273,1.0435964,1.0392281),auto[](0.1657560,0.1184644,0.1240829,0.1137127,0.0789924,0.0322056,-0.0107984,0.0180520,0.0053407,0.0136549,-0.0059564,-0.0018444,-0.0105719,-0.0029376,-0.0107905,-0.0080224,-0.0022669,0.0070200,-0.0081528,0.6077287,0.9883156,0.9939169,1.0039339,0.9923450,0.9992653,1.0084622,0.9835830,1.0085024,0.9745114,0.9854327,0.9349576,0.9871391),auto[](0.0026494,-0.0050175,-0.0125472,-0.0094555,-0.0125261,-0.0079171,-0.0079956,-0.0093559,0.0654686,0.3957288,0.7524402,0.9637648,0.9985443,0.9999298,0.9993908,0.9999437,0.9993912,0.9991124,0.9601958,0.6318628,0.2579740,0.0094015,-0.0030798,-0.0045230,-0.0068933,-0.0090352,-0.0085914,-0.0083691,-0.0078686,-0.0000084,0.0054301,-0.0027746),auto[](0.9920977,0.9887643,0.9953904,0.9952932,0.9918145,1.0002584,0.9996848,0.9998812,0.9850401,0.7902985,0.5608220,0.3313346,0.1369241,0.0189149,-0.0000051,-0.0004240,-0.0004193,0.0017473,0.0037999,-0.0005510,-0.0000437,0.0075875,0.0257957,0.0381684,0.0494896,0.0495960,0.0498148,0.0398409,0.0305010,0.0212431,0.0069597,0.0041734));
@(hot noinline) color rgb_to_color_nontrivial(float3 rgb){
  #assert(bool($state.wavelength_base));
  const int k0(#all(rgb.xx<rgb.yz)?0:rgb.y<rgb.z?1:2);
  const int k0_plus_1((k0+1)%3);
  const int k0_plus_2((k0+2)%3);
  const bool should_swap(rgb[k0_plus_1]>rgb[k0_plus_2]);
  const int k1(should_swap?k0_plus_2:k0_plus_1);
  const int k2(should_swap?k0_plus_1:k0_plus_2);
  const float coeff_w(rgb[k0]);
  const float coeff_cmy(rgb[k1]-rgb[k0]);
  const float coeff_rgb(rgb[k2]-rgb[k1]);
  color c(0.0);
  color w(color($state.wavelength_base));
  w-=RGB_TO_COLOR_MIN_WAVELENGTH;
  w*=RGB_TO_COLOR_NUM_WAVELENGTHS/(RGB_TO_COLOR_MAX_WAVELENGTH-RGB_TO_COLOR_MIN_WAVELENGTH);
  for(int i=0;i<$WAVELENGTH_BASE_MAX;i++){
    auto t(w[i]);
    if((0.0<=t)&(t<=RGB_TO_COLOR_NUM_WAVELENGTHS)){
      int t0(#min(int(t),RGB_TO_COLOR_NUM_WAVELENGTHS-2));
      t=#min(t-t0,1.0);
      c[i]=#sum(float2(1-t,t)*(coeff_w*float2(&RGB_TO_COLOR_CURVES[0][t0])+coeff_cmy*float2(&RGB_TO_COLOR_CURVES[k0+1][t0])+coeff_rgb*float2(&RGB_TO_COLOR_CURVES[k2+4][t0])));
    }
  }
  return #max(c*0.94,0.0);
}
export @(macro) color $rgb_to_color(const float3 rgb){
  if(#all(rgb.xx==rgb.yz)){
    return color(rgb.x);
  } else {
    return rgb_to_color_nontrivial(rgb);
  }
}
export @(pure) float3 $wyman_xyz(const float w){
  auto x(w-auto(442.0,599.8,501.1,568.8,530.9,437.0,459.0));
  x*=#select(x<0,auto(0.0624,0.0264,0.0490,0.0213,0.0613,0.0845,0.0385),auto(0.0374,0.0323,0.0382,0.0247,0.0322,0.0278,0.0725),);
  x*=0.5*x;
  const auto x1(x);
  auto y(1+x);
  y+=(x*=x1*0.5);
  y+=(x*=x1*0.333333);
  y+=(x*=x1*0.25);
  y=auto(0.362,1.056,-0.065,0.821,0.286,1.217,0.681)*0.01/y;
  return float3(y[0]+y[1]+y[2],y[3]+y[4],y[5]+y[6]);
}
export @(pure) float $wyman_y(const float w){
  auto x(w-auto(568.8,530.9));
  x*=#select(x<0,auto(0.0213,0.0613),auto(0.0247,0.0322));
  x*=0.5*x;
  const auto x1(x);
  auto y(1+x);
  y+=(x*=x1*0.5);
  y+=(x*=x1*0.333333);
  y+=(x*=x1*0.25);
  return #sum(auto(0.821,0.286)*0.01/y);
}
export @(hot noinline) float3 $color_to_rgb(const color c){
  float3 result(0.0);
  for(int i=0;i<$WAVELENGTH_BASE_MAX;++i){
    result+=$wyman_xyz($state.wavelength_base[i])*c[i];
  }
  result/=$WAVELENGTH_BASE_MAX;
  result*=$state.wavelength_max-$state.wavelength_min;
  return float3x3(float3(3.240450,-0.969266,0.0556434),float3(-1.537140,1.876010,-0.2040260),float3(-0.498532,0.041556,1.0572300),)*result;
}
@(visible noinline) void jit_rgb_to_color(const &float3 rgb,const &float cptr){
  color c($rgb_to_color(*rgb));
  #memcpy(cptr,&c,#sizeof(color));
}
@(visible noinline) void jit_color_to_rgb(const &float cptr,const &float3 rgb){
  *rgb=$color_to_rgb(color(cptr));
}
export enum intensity_mode{intensity_radiant_exitance,intensity_power,};
export tag bsdf;
export tag vdf;
export tag edf;
export tag hair_bsdf;
export struct $default_bsdf: default bsdf{
};
export struct $default_vdf: default vdf{
};
export struct $default_edf: default edf{
};
export struct $default_hair_bsdf: default hair_bsdf{
};
export struct material_emission{
  edf emission=edf();
  $(color|float) intensity=1.0;
  intensity_mode mode=intensity_radiant_exitance;
};
export struct material_surface{
  bsdf scattering=bsdf();
  material_emission emission=material_emission();
};
export struct material_volume{
  vdf scattering=vdf();
  $(color|void) absorption_coefficient=void();
  $(color|void) scattering_coefficient=void();
};
export struct material_geometry{
  float3 displacement=float3();
  float cutout_opacity=1.0;
  float3 normal=$state.normal;
};
export struct material{
  bool thin_walled=false;
  material_surface surface=material_surface();
  material_surface backface=material_surface();
  color ior=color(1.0);
  material_volume volume=material_volume();
  material_geometry geometry=material_geometry();
  hair_bsdf hair=hair_bsdf();
};
const int THIN_WALLED=0b00001;
const int HAS_SURFACE=0b00010;
const int HAS_BACKFACE=0b00100;
const int HAS_VOLUME=0b01000;
const int HAS_HAIR=0b10000;
export struct $material_instance{
  const &material mat;
  const &float3 displacement=&mat.geometry.displacement;
  const &float cutout_opacity=&mat.geometry.cutout_opacity;
  const &float3 normal=&mat.geometry.normal;
  const &color ior=&mat.ior;
  const &color absorption_coefficient=($(#typeof(mat.volume.absorption_coefficient)==void)?void():&mat.volume.absorption_coefficient);
  const &color scattering_coefficient=($(#typeof(mat.volume.scattering_coefficient)==void)?void():&mat.volume.scattering_coefficient);
  const int wavelength_base_max=$WAVELENGTH_BASE_MAX;
  const int flags=(mat.thin_walled?THIN_WALLED:0)|(#typeof(mat.surface)!=#typeof(material_surface())?HAS_SURFACE:0)|(#typeof(mat.backface)!=#typeof(material_surface())?HAS_BACKFACE:0)|(#typeof(mat.volume)!=#typeof(material_volume())?HAS_VOLUME:0)|(#typeof(mat.hair)!=#typeof(hair_bsdf())?HAS_HAIR:0);
  const float3x3 tangent_space=float3x3($state.tangent_to_object_matrix[0].xyz,$state.tangent_to_object_matrix[1].xyz,$state.tangent_to_object_matrix[2].xyz,);
};
)*";

static const char *debug = R"*(#smdl
export @(pure macro) bool assert(const bool condition,const string reason){
  #assert(condition,reason) if($DEBUG);
  return true;
}
export @(pure macro) bool breakpoint(){
  #breakpoint() if($DEBUG);
  return true;
}
export @(pure macro) bool print(const auto a){
  #print(a) if($DEBUG);
  return true;
}
)*";

static const char *df = R"*(#smdl
using ::math import *;
export enum scatter_mode{scatter_none=0,scatter_reflect=1,scatter_transmit=2,scatter_reflect_transmit=3};
@(pure macro) float scatter_reflect_chance(const scatter_mode mode){
  const auto refl_weight(#select((int(mode)&1)!=0,1.0,0.0));
  const auto tran_weight(#select((int(mode)&2)!=0,1.0,0.0));
  return refl_weight/(refl_weight+tran_weight);
}
@(pure noinline) float3x3 build_tangent_space(const float3 normal,const float3 tangent_u){
  const auto tw(normalize(normal)*#sign(normal.z));
  const auto tu(normalize(tangent_u-dot(tangent_u,tw)*tw));
  const auto tv(normalize(cross(tw,tu)));
  return float3x3(tu,tv,tw);
}
const float SCATTER_EPS=0.001;
struct scatter_evaluate_parameters{
  float3 wo0;
  float3 wi0;
  scatter_mode mode=(wo0.z<0)==(wi0.z<0)?scatter_reflect:scatter_transmit;
  bool hit_backface=wo0.z<0;
  bool thin_walled=false;
  float ior=1/1.4;
  float3 normal=float3(0,0,1);
  float3 tangent_u=float3(1,0,0);
  float3 wo=wo0;
  float3 wi=wi0;
  finalize {
    if(hit_backface){
      wo0=-wo0;
      wi0=-wi0;
      wo=-wo;
      wi=-wi;
      ior=1/ior;
    }
  }
};
@(pure noinline) bool recalculate_tangent_space(inline const &scatter_evaluate_parameters params){
  auto tbn(build_tangent_space(normal,tangent_u));
  wo=wo0*tbn;
  wi=wi0*tbn;
  return ((wo.z<0)==(wo0.z<0))&((wi.z<0)==(wi0.z<0));
}
@(pure) float3 half_direction(inline const &scatter_evaluate_parameters params){
  return normalize(mode==scatter_reflect?wo+wi:specular::refraction_half_vector(wo,wi,ior));
}
struct scatter_evaluate_result{
  $(color|float) f=0.0;
  float2 pdf=float2(0.0);
  bool is_black=false;
};
struct scatter_sample_parameters{
  float3 wo0;
  bool hit_backface=wo0.z<0;
  bool thin_walled=false;
  float ior=1/1.4;
  float3 normal=float3(0,0,1);
  float3 tangent_u=float3(1,0,0);
  float3 wo=wo0;
  float4 xi;
  finalize {
    if(hit_backface){
      wo0=-wo0;
      wo=-wo;
      ior=1/ior;
    }
  }
};
@(pure noinline) ?float3x3 recalculate_tangent_space(inline const &scatter_sample_parameters params){
  auto tbn(build_tangent_space(normal,tangent_u));
  wo=wo0*tbn;
  return tbn if((wo.z<0)==(wo0.z<0));
}
struct scatter_sample_result{
  float3 wi=float3(0.0);
  scatter_mode mode=scatter_none;
  ?color delta_f=void();
};
@(pure macro) auto scatter_evaluate(const &$default_bsdf this,const &scatter_evaluate_parameters params){
  return scatter_evaluate_result(is_black: true);
}
@(pure macro) auto scatter_sample(const &$default_bsdf this,const &scatter_sample_parameters params){
  return scatter_sample_result();
}
export struct diffuse_reflection_bsdf: bsdf{
  $(color|float) tint=1.0;
  float roughness=0.0;
  string handle="";
};
@(pure) auto scatter_evaluate(inline const &diffuse_reflection_bsdf this,inline const &scatter_evaluate_parameters params){
  if(mode==scatter_reflect&&recalculate_tangent_space(params)){
    const auto cos_theta(#abs(auto(wi.z,wo.z)));
    const auto pdf(cos_theta/$PI);
    if(roughness==0){
      return scatter_evaluate_result(f: pdf[0]*tint,pdf: pdf);
    } else {
      const auto sigma2(2.0*roughness*roughness);
      const auto A(1.00-sigma2/(2.0*sigma2+0.66));
      const auto B(0.45*sigma2/(sigma2+0.09));
      const auto fac(#max(#sum(wo.xy*wi.xy),0)/#max_value(cos_theta));
      return scatter_evaluate_result(f: pdf[0]*(A+fac*B)*tint,pdf: pdf);
    }
  } else {
    return scatter_evaluate_result(is_black: true);
  }
}
@(pure) auto scatter_sample(inline const &diffuse_reflection_bsdf this,inline const &scatter_sample_parameters params){
  if((tbn:=recalculate_tangent_space(params))){
    return scatter_sample_result(wi: (*tbn)*monte_carlo::cosine_hemisphere_sample(xi.xy),mode: scatter_reflect);
  } else {
    return scatter_sample_result();
  }
}
export struct diffuse_transmission_bsdf: bsdf{
  $(color|float) tint=1.0;
  string handle="";
};
@(pure) auto scatter_evaluate(inline const &diffuse_transmission_bsdf this,inline const &scatter_evaluate_parameters params){
  if(mode==scatter_transmit&&recalculate_tangent_space(params)){
    const auto cos_theta(#abs(auto(wi.z,wo.z)));
    const auto pdf(cos_theta/$PI);
    return scatter_evaluate_result(f: tint*pdf[0],pdf: pdf);
  } else {
    return scatter_evaluate_result(is_black: true);
  }
}
@(pure) auto scatter_sample(inline const &diffuse_transmission_bsdf this,inline const &scatter_sample_parameters params){
  if((tbn:=recalculate_tangent_space(params))){
    return scatter_sample_result(wi: (*tbn)*-monte_carlo::cosine_hemisphere_sample(xi.xy),mode: scatter_transmit);
  } else {
    return scatter_sample_result();
  }
}
export struct sheen_bsdf: bsdf{
  float roughness;
  $(color|float) tint=1.0;
  $(color|float|void) multiscatter_tint=void();
  void multiscatter=void();
  string handle="";
  finalize {
    roughness=saturate(roughness);
  }
};
@(pure) float sheen_lambda_l(const auto fit,const float mu)=fit[0]/(1.0+fit[1]*#pow(mu,fit[2]))+fit[3]*mu+fit[4];
@(pure) float sheen_lambda(const auto fit,const float mu)=#exp(mu<0.5?sheen_lambda_l(fit,mu):2*sheen_lambda_l(fit,0.5)-sheen_lambda_l(fit,#max(1-mu,0)));
@(pure) auto scatter_evaluate(inline const &sheen_bsdf this,inline const &scatter_evaluate_parameters params){
  if(mode==scatter_reflect&&recalculate_tangent_space(params)){
    const auto cos_thetao(#abs(wo.z));
    const auto cos_thetai(#abs(wi.z));
    const auto pdf(float2(cos_thetai,cos_thetao)/$PI);
    const auto fss=let {
      const auto alpha(lerp(0.1,1.0,roughness*roughness));
      const auto fit=lerp(auto(21.5473,3.82987,0.19823,-1.97760,-4.32054),auto(25.3245,3.32435,0.16801,-1.27393,-4.85967),(1-alpha)*(1-alpha),);
      const auto cos_thetah(normalize(wo+wi).z);
      const auto sin_thetah(#sqrt(1-cos_thetah*cos_thetah+SCATTER_EPS));
      const auto D(1/$TWO_PI*(2+1/alpha)*#pow(sin_thetah,1/alpha));
      const auto G(1/(1+sheen_lambda(fit,cos_thetao)+sheen_lambda(fit,cos_thetai)));
    } in D*G/(4*cos_thetao+SCATTER_EPS);
    return scatter_evaluate_result(f: tint*fss,pdf: pdf);
  } else {
    return scatter_evaluate_result(is_black: true);
  }
}
@(pure) auto scatter_sample(inline const &sheen_bsdf this,inline const &scatter_sample_parameters params){
  if((tbn:=recalculate_tangent_space(params))){
    return scatter_sample_result(wi: (*tbn)*monte_carlo::cosine_hemisphere_sample(xi.xy),mode: scatter_reflect);
  } else {
    return scatter_sample_result();
  }
}
export struct ward_geisler_moroder_bsdf: bsdf{
  float roughness_u;
  float roughness_v=roughness_u;
  $(color|float) tint=1.0;
  $(color|float|void) multiscatter_tint=void();
  float3 tangent_u=$state.texture_tangent_u[0];
  string handle="";
  finalize {
    roughness_u=saturate(roughness_u);
    roughness_v=saturate(roughness_v);
  }
};
@(pure noinline) auto scatter_evaluate(const &ward_geisler_moroder_bsdf this,inline const &scatter_evaluate_parameters params){
  preserve tangent_u;
  tangent_u=this.tangent_u;
  if(mode==scatter_reflect&&recalculate_tangent_space(params)){
    const auto cos_thetao(#abs(wo.z));
    const auto cos_thetai(#abs(wi.z));
    const auto roughness(this.roughness_u,this.roughness_v);
    const auto alpha(#max(0.001,roughness*roughness));
    const auto f(#sum((h:=wo+wi)*h)/($PI*alpha.x*alpha.y*#pow(h.z,4))*#exp(-#sum((g:=h.xy/(h.z*alpha))*g)));
    const auto fss_pdf(float2(f*(cos_thetao+cos_thetai)/2));
    const auto fss(f*cos_thetai);
    if$(#typeof(this.multiscatter_tint)==void){
      return scatter_evaluate_result(f: this.tint*fss,pdf: fss_pdf);
    } else {
      const auto fms_pdf(float2(cos_thetai,cos_thetao)/$PI);
      const auto fms=let {
        const auto r0(#sqrt(roughness.x*roughness.y));
        const auto fit=return_from{
          float3 fit(-1.1992005e+02,-1.4040313e+01,7.8306640e-01);
          fit=fit*r0+float3(4.1985368e+02,4.6807753e+01,-1.6213743e+00);
          fit=fit*r0+float3(-5.8448171e+02,-6.1370147e+01,-1.3797964e+00);
          fit=fit*r0+float3(4.2351783e+02,4.1399258e+01,5.6539624e+00);
          fit=fit*r0+float3(-1.6959530e+02,-1.4979874e+01,-3.8064856e+00);
          fit=fit*r0+float3(3.7025769e+01,3.0665596e+00,-1.2666234e-01);
          fit=fit*r0+float3(-3.4191809e+00,-2.9108604e-01,-1.8175253e-02);
          fit=fit*r0+float3(1.6044891e-01,8.8001559e-03,1.4868175e-03);
          fit=fit*r0+float3(-7.1467185e-04,1.8095055e-01,9.9998607e-01);
          return fit;
        };
        const auto Ewo(#min(1-fit[1]*(t:=#pow(cos_thetao/fit[0],2.0/3.0))*#exp(1-t),0.999));
        const auto Ewi(#min(1-fit[1]*(t:=#pow(cos_thetai/fit[0],2.0/3.0))*#exp(1-t),0.999));
        const auto Eav(#min(fit[2],0.999));
      } in (1-Ewo)*(1-Ewi)/(1-Eav)*cos_thetai/$PI;
      return scatter_evaluate_result(f: this.tint*(fss+this.multiscatter_tint*fms),pdf: 0.8*fss_pdf+0.2*fms_pdf);
    }
  } else {
    return scatter_evaluate_result(is_black: true);
  }
}
@(pure noinline) auto scatter_sample(const &ward_geisler_moroder_bsdf this,inline const &scatter_sample_parameters params){
  preserve tangent_u;
  tangent_u=this.tangent_u;
  if((tbn:=recalculate_tangent_space(params))){
    if(#typeof(this.multiscatter_tint)!=void&&monte_carlo::bool_sample(&xi.w,0.2)){
      return scatter_sample_result(wi: (*tbn)*monte_carlo::cosine_hemisphere_sample(xi.xy),mode: scatter_reflect);
    }
    const auto roughness(this.roughness_u,this.roughness_v);
    const auto alpha(#max(0.001,roughness*roughness));
    const auto phi(#atan2(alpha.y*#sin(t:=$TWO_PI*xi.x),alpha.x*#cos(t)));
    const auto cos_phi(#cos(phi));
    const auto sin_phi(#sin(phi));
    const auto theta(#atan(#sqrt(-#log(1-xi.y)/(#pow(cos_phi/alpha.x,2)+#pow(sin_phi/alpha.y,2)))));
    const auto wm(float3(#sin(theta)*float2(cos_phi,sin_phi),#cos(theta)));
    const auto wi(normalize(specular::reflect(wo,wm)));
    if(wi.z>0){
      return scatter_sample_result(wi: (*tbn)*wi,mode: scatter_reflect);
    }
  }
  return scatter_sample_result();
}
struct tint1: bsdf,edf,hair_bsdf{
  $(color|float) tint;
  auto base;
};
struct tint2: bsdf{
  $(color|float) reflection_tint;
  $(color|float) transmission_tint;
  bsdf base;
};
export @(pure macro) auto tint(const auto tint,const bsdf base)=tint1(tint,base);
export @(pure macro) auto tint(const auto tint,const edf base)=tint1(tint,base);
export @(pure macro) auto tint(const auto tint,const hair_bsdf base)=tint1(tint,base);
export @(pure macro) auto tint(const auto reflection_tint,const auto transmission_tint,const bsdf base)=tint2(reflection_tint,transmission_tint,base);
@(pure macro) auto scatter_evaluate(const &tint1 this,const &scatter_evaluate_parameters params){
  auto result(scatter_evaluate(visit &this.base,params));
  if(!result.is_black)
    result.f*=this.tint;
  return result;
}
@(pure macro) auto scatter_evaluate(const &tint2 this,const &scatter_evaluate_parameters params){
  auto result(scatter_evaluate(visit &this.base,params));
  if(!result.is_black){
    if(params.mode==scatter_reflect){
      result.f*=this.reflection_tint;
    } else {
      result.f*=this.transmission_tint;
    }
  }
  return result;
}
@(pure macro) auto scatter_sample(const &tint1 this,const &scatter_sample_parameters params){
  auto result(scatter_sample(visit &this.base,params));
  if((result.mode!=scatter_none)&bool(result.delta_f))
    *result.delta_f*=this.tint;
  return result;
}
@(pure macro) auto scatter_sample(const &tint2 this,const &scatter_sample_parameters params){
  auto result(scatter_sample(visit &this.base,params));
  if((result.mode!=scatter_none)&bool(result.delta_f)){
    if(params.mode==scatter_reflect){
      *result.delta_f*=this.reflection_tint;
    } else {
      *result.delta_f*=this.transmission_tint;
    }
  }
  return result;
}
export struct weighted_layer: bsdf{
  $(color|float) weight;
  bsdf layer=bsdf();
  bsdf base=bsdf();
  float3 normal=$state.normal;
  float chance=average(weight);
  finalize {
    weight=saturate(weight);
    chance=saturate(chance);
  }
};
@(pure macro) auto scatter_evaluate(const &weighted_layer this,inline const &scatter_evaluate_parameters params){
  auto result0(scatter_evaluate(visit &this.base,params));
  preserve normal;
  normal=this.normal;
  auto result1(scatter_evaluate(visit &this.layer,params));
  return scatter_evaluate_result(f: lerp(result0.f,result1.f,this.weight),pdf: lerp(result0.pdf,result1.pdf,this.chance),is_black: result0.is_black&result1.is_black);
}
@(pure macro) auto scatter_sample(const &weighted_layer this,inline const &scatter_sample_parameters params){
  if(monte_carlo::bool_sample(&xi.w,this.chance)){
    preserve normal;
    normal=this.normal;
    return scatter_sample(visit &this.layer,params);
  } else {
    return scatter_sample(visit &this.base,params);
  }
}
export typedef weighted_layer color_weighted_layer;
export @(pure macro) int $scatter_evaluate(
  const &$material_instance instance,
  const &float3 wo,
  const &float3 wi,
  const &float pdf_fwd,
  const &float pdf_rev,
  const &float f,
){
  auto params=scatter_evaluate_parameters(
    wo0: normalize(*wo),
    wi0: normalize(*wi),
    thin_walled: instance.mat.thin_walled,
    normal: normalize(*instance.normal),
  );
  auto result=#typeof(instance.mat.backface)==#typeof(material_surface())||!params.hit_backface?scatter_evaluate(&instance.mat.surface.scattering,&params):scatter_evaluate(&instance.mat.backface.scattering,&params);
  visit result in result{
    if(result.is_black){
      *pdf_fwd=0;
      *pdf_rev=0;
      for(int i=0;i<$WAVELENGTH_BASE_MAX;i++)
        f[i]=0.0;
    } else {
      *pdf_fwd=result.pdf[0];
      *pdf_rev=result.pdf[1];
      if(#typeof(result.f)==float){
        for(int i=0;i<$WAVELENGTH_BASE_MAX;i++)
          f[i]=result.f;
      } else {
        #memcpy(f,&result.f,#sizeof(float)*$WAVELENGTH_BASE_MAX);
      }
    }
    return !result.is_black;
  }
}
export @(pure macro) int $scatter_sample(
  const &$material_instance instance,
  const &float4 xi,
  const &float3 wo,
  const &float3 wi,
  const &float pdf_fwd,
  const &float pdf_rev,
  const &float f,
  const &int is_delta,
){
  auto params=scatter_sample_parameters(
    xi: saturate(*xi),
    wo0: normalize(*wo),
    thin_walled: instance.mat.thin_walled,
    normal: normalize(*instance.normal),
  );
  auto result=#typeof(instance.mat.backface)==#typeof(material_surface())||!params.hit_backface?scatter_sample(&instance.mat.surface.scattering,&params):scatter_sample(&instance.mat.backface.scattering,&params);
  visit result in result{
    *wi=#select(params.hit_backface,-result.wi,result.wi);
    if(result.mode==scatter_none||((wo.z<0)==(wi.z<0))!=(result.mode==scatter_reflect)){
      *pdf_fwd=0.0;
      *pdf_rev=0.0;
      for(int i=0;i<$WAVELENGTH_BASE_MAX;i++)
        f[i]=0.0;
      return false;
    }
    if((*is_delta=bool(result.delta_f))){
      *pdf_fwd=1.0;
      *pdf_rev=1.0;
      #memcpy(f,&*result.delta_f,#sizeof(float)*$WAVELENGTH_BASE_MAX);
      return true;
    } else {
      return $scatter_evaluate(instance,wo,wi,pdf_fwd,pdf_rev,f);
    }
  }
}
)*";

static const char *limits = R"*(#smdl
export const int INT_MIN=$INT_MIN;
export const int INT_MAX=$INT_MAX;
export const float FLOAT_MIN=$FLOAT_MIN;
export const float FLOAT_MAX=$FLOAT_MAX;
export const double DOUBLE_MIN=$DOUBLE_MIN;
export const double DOUBLE_MAX=$DOUBLE_MAX;
)*";

static const char *math = R"*(#smdl
export const float PI=$PI;
export const float TWO_PI=$TWO_PI;
export const float HALF_PI=$HALF_PI;
export @(pure macro) auto abs(const auto a)=#abs(a);
export @(pure macro) auto all(const auto a)=#all(a);
export @(pure macro) auto any(const auto a)=#any(a);
export @(pure macro) auto max(const auto a,const auto b)=#max(a,b);
export @(pure macro) auto min(const auto a,const auto b)=#min(a,b);
export @(pure macro) auto clamp(const auto a,const auto min,const auto max)=#max(min,#min(a,max));
export @(pure macro) auto saturate(const auto a)=clamp(a,0.0,1.0);
export @(pure macro) auto floor(const auto a)=#floor(a);
export @(pure macro) auto ceil(const auto a)=#ceil(a);
export @(pure macro) auto round(const auto a)=#round(a);
export @(pure macro) auto trunc(const auto a)=#trunc(a);
export @(pure macro) auto frac(const auto a)=a-#floor(a);
export @(pure macro) auto fmod(const auto a,const auto b)=a%b;
export @(pure macro) auto isfinite(const auto a)=#isfpclass(a,0b0111111000);
export @(pure macro) auto isnormal(const auto a)=#isfpclass(a,0b0100001000);
export @(pure macro) auto isinf(const auto a)=#isfpclass(a,0b1000000100);
export @(pure macro) auto isnan(const auto a)=#isfpclass(a,0b0000000011);
export @(pure macro) auto sign(const auto a)=#sign(a);
export @(pure macro) auto sqrt(const auto a)=#sqrt(a);
export @(pure macro) auto rsqrt(const auto a)=1.0/#sqrt(a);
export @(pure macro) auto pow(const auto a,const auto b)=#pow(a,b);
export @(pure macro) auto cos(const auto a)=#cos(a);
export @(pure macro) auto sin(const auto a)=#sin(a);
export @(pure macro) auto tan(const auto a)=#tan(a);
export @(pure macro) auto acos(const auto a)=#acos(a);
export @(pure macro) auto asin(const auto a)=#asin(a);
export @(pure macro) auto atan(const auto a)=#atan(a);
export @(pure macro) auto atan2(const auto y,const auto x)=#atan2(y,x);
export @(pure macro) auto cosh(const auto a)=#cosh(a);
export @(pure macro) auto sinh(const auto a)=#sinh(a);
export @(pure macro) auto tanh(const auto a)=#tanh(a);
export @(pure macro) auto sincos(const auto a)=auto[2](#sin(a),#cos(a));
export @(pure macro) auto radians(const auto a)=a*(PI/180.0);
export @(pure macro) auto degrees(const auto a)=a*(180.0/PI);
export @(pure macro) auto exp(const auto a)=#exp(a);
export @(pure macro) auto exp2(const auto a)=#exp2(a);
export @(pure macro) auto exp10(const auto a)=#exp10(a);
export @(pure macro) auto log(const auto a)=#log(a);
export @(pure macro) auto log2(const auto a)=#log2(a);
export @(pure macro) auto log10(const auto a)=#log10(a);
export @(pure macro) auto min_value(const auto a)=#min_value(a);
export @(pure macro) auto max_value(const auto a)=#max_value(a);
export @(pure macro) auto average(const auto a)=#sum(a)/a.size;
export @(pure macro) auto lerp(const auto a,const auto b,const auto l)=(1.0-l)*a+l*b;
export @(pure macro) auto step(const auto a,const auto b)=#select(b<a,0.0,1.0);
export @(pure macro) auto smoothstep(const auto a,const auto b,const auto l){
  const auto t(saturate(l));
  const auto s(1-t);
  return s*s*(1+2*t)*a+t*t*(1+2*s)*b;
}
export @(pure macro) auto dot(const auto a,const auto b)=#sum(a*b);
export @(pure macro) auto length(const auto a)=#sqrt(#sum(a*a));
export @(pure macro) auto normalize(const auto a)=a*(1/length(a));
export @(pure macro) auto distance(const auto a,const auto b)=length(b-a);
export @(pure macro) auto cross(const auto a,const auto b)=a.yzx*b.zxy-a.zxy*b.yzx;
export @(pure macro) auto transpose(const auto a)=#transpose(a);
export @(noinline) color blackbody(const float temperature){
  const auto t(color($state.wavelength_base)*(temperature/14.387e6));
  auto res(1+2*t);
  res=1+3*t*res;
  res=1+4*t*res;
  res=1+5*t*res;
  const auto rcp1(1/t);
  auto rcp(rcp1/6);
  for(int k=1;k<10;++k){
    res+=rcp;
    rcp*=rcp1/(6+k);
  }
  return 5.659994086/res;
}
export @(noinline) float luminance(const color a){
  float result(0.0);
  for(int i=0;i<$WAVELENGTH_BASE_MAX;++i){
    result+=$wyman_y($state.wavelength_base[i])*a[i];
  }
  return result/$WAVELENGTH_BASE_MAX;
}
export namespace monte_carlo {
export @(pure macro) float2 advance_low_discrepancy(const &float2 xi)=(*xi=frac(*xi+float2(0.75487766,0.56984029)));
export @(pure macro) float3 advance_low_discrepancy(const &float3 xi)=(*xi=frac(*xi+float3(0.81917251,0.67104360,0.54970047)));
export @(pure macro) float4 advance_low_discrepancy(const &float4 xi)=(*xi=frac(*xi+float4(0.85667488,0.73389185,0.62870672,0.53859725)));
export @(pure) bool bool_sample(const &float xi,const float chance){
  if(*xi<chance){
    *xi=(*xi/chance);
    return true;
  } else {
    *xi=(*xi-chance)/(1-chance);
    return false;
  }
}
export @(pure) int uniform_wavelength_index_sample(const &float xi){
  const int i(#min(int(*xi*=$WAVELENGTH_BASE_MAX),$WAVELENGTH_BASE_MAX-1));
  *xi-=i;
  return i;
}
export @(pure noinline) float2 uniform_disk_sample(float2 xi){
  xi=2*xi-1;
  xi=#select(xi==0,1e-5,xi);
  const bool cond((absxi:=#abs(xi),absxi.x>absxi.y));
  const float rad(#select(cond,xi.x,xi.y));
  const float phi(#select(cond,($PI/4)*xi.y/xi.x,($PI/2)-($PI/4)*xi.x/xi.y));
  return rad*float2(#cos(phi),#sin(phi));
}
export @(pure noinline) float3 cosine_hemisphere_sample(float2 xi)=float3((p:=uniform_disk_sample(xi)),#sqrt(#max(1-#sum(p*p),0)));
}
export namespace specular {
export @(pure macro) auto reflect(const float3 wi,const float3 wm)=2*#sum(wi*wm)*wm-wi;
export @(pure macro) auto refract(const float3 wi,const float3 wm,const float ior){
  const auto cos_thetai(#sum(wi*wm));
  const auto cos2_thetai(#min(cos_thetai*cos_thetai,1));
  const auto cos2_thetat(#max(1-ior*ior*(1-cos2_thetai),0));
  const auto cos_thetat(#sqrt(cos2_thetat)*-#sign(cos_thetai));
  return -ior*wi+(ior*cos_thetai+cos_thetat)*wm;
}
export @(pure macro) auto refraction_half_vector(const float3 wo,const float3 wi,const float ior,)=(vh:=-ior*wo+wi)*#sign(vh.z);
export @(pure macro) auto refraction_half_vector_jacobian(const float3 wo,const float3 wi,const float ior,)=#abs(#sum(wi*(vh:=refraction_half_vector(wo,wi,ior))))/((vh2:=#sum(vh*vh))*#sqrt(vh2));
export @(pure macro) auto schlick_F0(const auto ior)=#pow((ior-1)/(ior+1),2);
export @(pure macro) auto schlick_fresnel(
  const auto cos_theta,
  const auto F0,
  const auto F90=1.0,
  const float exponent=5,
)=F0+(F90-F0)*#pow(#max(1-#abs(cos_theta),0),exponent);
export @(pure) auto dielectric_fresnel(const float cos_thetai,const auto ior){
  const auto cos2_thetat(1-ior*ior*(1-cos_thetai*cos_thetai));
  const auto cos_thetat(#sqrt(#max(cos2_thetat,0))*#sign(cos_thetai));
  const auto rs((ior*cos_thetai-cos_thetat)/(ior*cos_thetai+cos_thetat));
  const auto rp((cos_thetai-ior*cos_thetat)/(cos_thetai+ior*cos_thetat));
  return #min(0.5*(rs*rs+rp*rp),1.0);
}
}
)*";

static const char *scene = R"*(#smdl
export @(macro) bool data_isvalid(const string name)=#data_exists(name);
export @(macro) int data_lookup_int(const string name,int default_value=int())=#data_lookup(name,default_value);
export @(macro) int2 data_lookup_int2(const string name,int2 default_value=int2())=#data_lookup(name,default_value);
export @(macro) int3 data_lookup_int3(const string name,int3 default_value=int3())=#data_lookup(name,default_value);
export @(macro) int4 data_lookup_int4(const string name,int4 default_value=int4())=#data_lookup(name,default_value);
export @(macro) float data_lookup_float(const string name,float default_value=float())=#data_lookup(name,default_value);
export @(macro) float2 data_lookup_float2(const string name,float2 default_value=float2())=#data_lookup(name,default_value);
export @(macro) float3 data_lookup_float3(const string name,float3 default_value=float3())=#data_lookup(name,default_value);
export @(macro) float4 data_lookup_float4(const string name,float4 default_value=float4())=#data_lookup(name,default_value);
export @(macro) color data_lookup_color(const string name,color default_value=color())=#data_lookup(name,default_value);
)*";

static const char *state = R"*(#smdl
import ::math::*;
export enum coordinate_space{coordinate_internal=0,coordinate_object=1,coordinate_world=2};
export @(macro) float3 position()=$state.position;
export @(macro) float3 normal()=$state.normal;
export @(macro) float3 geometry_normal()=$state.geometry_normal;
export @(macro) float3 motion()=$state.motion;
export @(macro) int texture_space_max()=$state.texture_space_max;
export @(macro) float3 texture_coordinate(const int i)=$state.texture_coordinate[i];
export @(macro) float3 texture_tangent_u(const int i)=$state.texture_tangent_u[i];
export @(macro) float3 texture_tangent_v(const int i)=$state.texture_tangent_v[i];
export @(macro) float3 geometry_tangent_u(const int i)=$state.geometry_tangent_u[i];
export @(macro) float3 geometry_tangent_v(const int i)=$state.geometry_tangent_v[i];
export @(macro) float3x3 tangent_space(const int i)=float3x3($state.texture_tangent_u[i],$state.texture_tangent_v[i],$state.normal);
export @(macro) float3x3 geometry_tangent_space(const int i)=float3x3($state.geometry_tangent_u[i],$state.geometry_tangent_v[i],$state.geometry_normal);
export @(macro) int object_id()=$state.object_id;
export @(macro) float3 direction()=$state.direction;
export @(macro) float animation_time()=$state.animation_time;
export const int WAVELENGTH_BASE_MAX=$WAVELENGTH_BASE_MAX;
export @(macro) float wavelength_min()=$state.wavelength_min;
export @(macro) float wavelength_max()=$state.wavelength_max;
export @(macro) float[WAVELENGTH_BASE_MAX] wavelength_base()=$state.wavelength_base;
export @(macro) float meters_per_scene_unit()=$state.meters_per_scene_unit;
export @(macro) float scene_units_per_meter()=1.0/$state.meters_per_scene_unit;
@(pure) float4x4 affine_inverse(float4x4 matrix){
  return float4x4(
    float4(matrix[0].x,matrix[1].x,matrix[2].x,0.0),
    float4(matrix[0].y,matrix[1].y,matrix[2].y,0.0),
    float4(matrix[0].z,matrix[1].z,matrix[2].z,0.0),
    float4(-#sum(matrix[0]*matrix[3]),-#sum(matrix[1]*matrix[3]),-#sum(matrix[2]*matrix[3]),1.0),
  );
}
export @(macro) float4x4 transform(const coordinate_space from,const coordinate_space to){
  if(from==to){
    return float4x4(1.0);
  } else if((from==coordinate_internal)&(to==coordinate_object)){
    return $state.tangent_to_object_matrix;
  } else if((from==coordinate_internal)&(to==coordinate_world)){
    return $state.object_to_world_matrix*$state.tangent_to_object_matrix;
  } else if((from==coordinate_object)&(to==coordinate_world)){
    return $state.object_to_world_matrix;
  } else if((from==coordinate_object)&(to==coordinate_internal)){
    return affine_inverse($state.tangent_to_object_matrix);
  } else if((from==coordinate_world)&(to==coordinate_object)){
    return affine_inverse($state.object_to_world_matrix);
  } else if((from==coordinate_world)&(to==coordinate_internal)){
    return affine_inverse($state.object_to_world_matrix*$state.tangent_to_object_matrix);
  } else {
    return float4x4(1.0);
  }
}
export @(macro) float3 transform_point(const coordinate_space from,const coordinate_space to,const float3 point){
  return from==to?point:(transform(from,to)*float4(point,1)).xyz;
}
export @(macro) float3 transform_vector(const coordinate_space from,const coordinate_space to,const float3 vector){
  return from==to?vector:(transform(from,to)*float4(vector,0)).xyz;
}
export @(macro) float3 transform_normal(const coordinate_space from,const coordinate_space to,const float3 normal){
  return from==to?normal:(float4(normal,0)*transform(to,from)).xyz;
}
export @(macro) float transform_scale(const coordinate_space from,const coordinate_space to,const float scale){
  return 1.0*scale;
}
)*";

static const char *std = R"*(#smdl
export using ::debug import *;
export using ::df import *;
export using ::limits import *;
export using ::math import *;
export using ::scene import *;
export using ::state import *;
export using ::tex import *;
)*";

static const char *tex = R"*(#smdl
import ::math::lerp;
export enum gamma_mode{gamma_default=0,gamma_linear=0,gamma_srgb=1};
@(pure macro) float4 apply_gamma(const int gamma,const float4 texel)=gamma==int(gamma_srgb)?float4((texel*texel).xyz,texel.w):texel;
@(pure macro) float3 apply_gamma(const int gamma,const float3 texel)=gamma==int(gamma_srgb)?(texel*texel):texel;
@(pure macro) float2 apply_gamma(const int gamma,const float2 texel)=gamma==int(gamma_srgb)?(texel*texel):texel;
@(pure macro) float apply_gamma(const int gamma,const float texel)=gamma==int(gamma_srgb)?(texel*texel):texel;
@(pure macro) int uv_tile_index(const texture_2d tex,const int2 uv_tile){
  return -1 if(#any((uv_tile<0)|(uv_tile>=tex.tile_count)));
  return uv_tile.y*tex.tile_count_u+uv_tile.x;
}
export @(pure macro) int width(const texture_2d tex,const int2 uv_tile=int2(0)){
  const auto i(uv_tile_index(tex,uv_tile));
  return i<0?0:tex.tile_extents[i].x;
}
export @(pure macro) int height(const texture_2d tex,const int2 uv_tile=int2(0)){
  const auto i(uv_tile_index(tex,uv_tile));
  return i<0?0:tex.tile_extents[i].y;
}
export @(pure macro) bool texture_isvalid(const texture_2d tex)=bool(tex.tile_buffers[0]);
export @(pure macro) bool texture_isvalid(const texture_ptex tex)=bool(tex.texture);
@(pure) auto texel_fetch(const texture_2d tex,const int2 coord,const int2 uv_tile=int2(0)){
  const auto i(uv_tile_index(tex,uv_tile));
  return tex.texel_type(0) if(i<0);
  const auto tile_extent(tex.tile_extents[i]);
  const auto tile_buffer(tex.tile_buffers[i]);
  return tex.texel_type(0) if(!tile_buffer|#any((coord<0)|(coord>=tile_extent)));
  return tile_buffer[coord.y*tile_extent.x+coord.x];
}
export @(pure macro) float4 texel_float4(const texture_2d tex,const int2 coord,const int2 uv_tile=int2(0))=apply_gamma(tex.gamma,#unpack_float4(texel_fetch(tex,coord,uv_tile)));
export @(pure macro) float3 texel_float3(const texture_2d tex,const int2 coord,const int2 uv_tile=int2(0))=apply_gamma(tex.gamma,#unpack_float4(texel_fetch(tex,coord,uv_tile)).xyz);
export @(pure macro) float2 texel_float2(const texture_2d tex,const int2 coord,const int2 uv_tile=int2(0))=apply_gamma(tex.gamma,#unpack_float4(texel_fetch(tex,coord,uv_tile)).xy);
export @(pure macro) float texel_float(const texture_2d tex,const int2 coord,const int2 uv_tile=int2(0))=apply_gamma(tex.gamma,#unpack_float4(texel_fetch(tex,coord,uv_tile)).x);
export enum wrap_mode{wrap_clamp=0,wrap_repeat=1,wrap_mirrored_repeat=2,wrap_clip=3};
@(pure macro) auto apply_wrap(const auto wrap,const auto n,auto i){
  auto rem(i%n);
  const auto neg(#select(rem<0,1,0));
  rem+=n*neg;
  const auto quo(i/n+neg);
  const auto repeat(rem);
  const auto mirror(#select((quo&1)==1,n-1-rem,rem));
  i=#select(wrap==0,i,#select(wrap==1,repeat,mirror));
  i=#max(0,#min(i,n-1));
  return i;
}
export @(pure) float4 lookup_float4(
  const texture_2d tex,
  float2 coord,
  const wrap_mode wrap_u=wrap_repeat,
  const wrap_mode wrap_v=wrap_repeat,
  const float2 crop_u=float2(0.0,1.0),
  const float2 crop_v=float2(0.0,1.0),
){
  if((tex.tile_count_u>1)|(tex.tile_count_v>1)){
    const int2 tile_index(#floor(coord));
    const auto i(uv_tile_index(tex,tile_index));
    return float4(0) if(i<0);
    const auto tile_extent(tex.tile_extents[i]);
    const auto tile_buffer(tex.tile_buffers[i]);
    return float4(0) if(!tile_buffer);
    coord-=tile_index;
    coord*=tile_extent;
    coord-=0.5;
    const int2 ic(#floor(coord));
    const int2 ic0(#min(ic,tile_extent-1));
    const int2 ic1(#min(ic+1,tile_extent-1));
    coord-=ic;
    return apply_gamma(tex.gamma,math::lerp(math::lerp(#unpack_float4(tile_buffer[ic0.x+tile_extent.x*ic0.y]),#unpack_float4(tile_buffer[ic1.x+tile_extent.x*ic0.y]),coord.x),math::lerp(#unpack_float4(tile_buffer[ic0.x+tile_extent.x*ic1.y]),#unpack_float4(tile_buffer[ic1.x+tile_extent.x*ic1.y]),coord.x),coord.y),);
  } else {
    const auto i(uv_tile_index(tex,int2(0)));
    return float4(0) if(i<0);
    const auto tile_extent(tex.tile_extents[i]);
    const auto tile_buffer(tex.tile_buffers[i]);
    return float4(0) if(!tile_buffer);
    const auto icrop_u(int2(crop_u*tile_extent));
    const auto icrop_v(int2(crop_v*tile_extent));
    const auto icorner0(int2(icrop_u[0],icrop_v[0]));
    const auto icorner1(int2(icrop_u[1],icrop_v[1]));
    const auto subextent(icorner1-icorner0);
    coord*=subextent;
    coord-=0.5;
    const int2 wrap(int(wrap_u),int(wrap_v));
    const int2 ic(#floor(coord));
    const auto ic0(icorner0+apply_wrap(wrap,subextent,ic));
    const auto ic1(icorner0+apply_wrap(wrap,subextent,ic+1));
    coord-=ic;
    return apply_gamma(tex.gamma,math::lerp(math::lerp(#unpack_float4(tile_buffer[ic0.x+tile_extent.x*ic0.y]),#unpack_float4(tile_buffer[ic1.x+tile_extent.x*ic0.y]),coord.x),math::lerp(#unpack_float4(tile_buffer[ic0.x+tile_extent.x*ic1.y]),#unpack_float4(tile_buffer[ic1.x+tile_extent.x*ic1.y]),coord.x),coord.y),);
  }
}
export @(pure macro) float3 lookup_float3(
  const texture_2d tex,
  const float2 coord,
  const wrap_mode wrap_u=wrap_repeat,
  const wrap_mode wrap_v=wrap_repeat,
  const float2 crop_u=float2(0.0,1.0),
  const float2 crop_v=float2(0.0,1.0),
)=lookup_float4(tex,coord,wrap_u,wrap_v,crop_u,crop_v).xyz;
export @(pure macro) float2 lookup_float2(
  const texture_2d tex,
  const float2 coord,
  const wrap_mode wrap_u=wrap_repeat,
  const wrap_mode wrap_v=wrap_repeat,
  const float2 crop_u=float2(0.0,1.0),
  const float2 crop_v=float2(0.0,1.0),
)=lookup_float4(tex,coord,wrap_u,wrap_v,crop_u,crop_v).xy;
export @(pure macro) float lookup_float(
  const texture_2d tex,
  const float2 coord,
  const wrap_mode wrap_u=wrap_repeat,
  const wrap_mode wrap_v=wrap_repeat,
  const float2 crop_u=float2(0.0,1.0),
  const float2 crop_v=float2(0.0,1.0),
)=lookup_float4(tex,coord,wrap_u,wrap_v,crop_u,crop_v).x;
export @(macro) float4 lookup_float4(texture_ptex tex,const int channel=0){
  float4 result;
  #ptex_evaluate(tex,channel,4,&result[0]);
  return result;
}
export @(macro) float3 lookup_float3(texture_ptex tex,const int channel=0){
  float3 result;
  #ptex_evaluate(tex,channel,3,&result[0]);
  return result;
}
export @(macro) float2 lookup_float2(texture_ptex tex,const int channel=0){
  float2 result;
  #ptex_evaluate(tex,channel,2,&result[0]);
  return result;
}
export @(macro) float lookup_float(texture_ptex tex,const int channel=0){
  float result;
  #ptex_evaluate(tex,channel,1,&result);
  return result;
}
)*";

[[nodiscard]] static const char *get_source_code(std::string_view name) {
  if (name == "anno")
    return anno;
  if (name == "api")
    return api;
  if (name == "debug")
    return debug;
  if (name == "df")
    return df;
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

} // namespace smdl::builtin
