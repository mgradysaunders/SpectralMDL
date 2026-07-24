#pragma once

#include <array>
#include <string_view>

namespace smdl::builtin {

static const char *const anno = R"*(#smdl
export annotation soft_range(auto min,auto max);
export annotation hard_range(auto min,auto max);
export annotation display_name(string name);
export annotation in_group(string group);
export annotation in_group(string group,string subgroup);
export annotation in_group(string group,string subgroup,string subsubgroup);
export annotation ui_order(int order);
export annotation enable_if(string condition);
export annotation hidden();
export annotation description(string description);
export annotation thumbnail(string name);
export annotation author(string name);
export annotation contributor(string name);
export annotation copyright_notice(string copyright);
export annotation created(int year,int month,int day,string notes);
export annotation modified(int year,int month,int day,string notes);
export annotation version(int major,int minor,int patch,string prerelease="");
export annotation dependency(string module_name,int major,int minor,int patch,string prerelease="");
export annotation key_words(string[] words);
export annotation unused(string description="");
export annotation deprecated(string description="");
export annotation usage(string hint="");
export annotation origin(string name="");
)*";

static const char *const api = R"*(#smdl
const int RGB_TO_COLOR_NUM_WAVELENGTHS=32;
const float RGB_TO_COLOR_MIN_WAVELENGTH=380.0;
const float RGB_TO_COLOR_MAX_WAVELENGTH=720.0;
const static auto RGB_TO_COLOR_CURVES=auto[](
  auto[](1.0618958,1.0615020,1.0614336,1.0622711,1.0622036,1.0625060,1.0623939,1.0624707,1.0625048,1.0624366,1.0620694,1.0613167,1.0610334,1.0613868,1.0614215,1.0620337,1.0625497,1.0624317,1.0625249,1.0624278,1.0624750,1.0625539,1.0625327,1.0623922,1.0623651,1.0625256,1.0612278,1.0594263,1.0599811,1.0602547,1.0601263,1.0606565),
  auto[](1.0414628,1.0328661,1.0126146,1.0350461,1.0078661,1.0422280,1.0442597,1.0535238,1.0180776,1.0442730,1.0529362,1.0537034,1.0533901,1.0537783,1.0527093,1.0530449,1.0550555,1.0553674,1.0454307,0.6234895,0.1803807,-0.0076304,-0.0001522,-0.0075102,-0.0021709,0.0006592,0.0122788,-0.0044670,0.0171198,0.0049211,0.0058763,0.0252594),
  auto[](0.9942214,0.9898694,0.9829366,0.9962787,1.0198956,1.0166396,1.0220913,0.9965166,1.0097766,1.0215422,0.6403195,0.0025012,0.0065340,0.0028334,-0.0000000,-0.0090592,0.0033937,-0.0030639,0.2220394,0.6314114,0.9748099,0.9720956,1.0173770,0.9987519,0.9470173,0.8525862,0.9489780,0.9475188,0.9959894,0.8630135,0.8915099,0.8486649),
  auto[](0.0055741,-0.0047983,-0.0052537,-0.0064571,-0.0059694,-0.0021837,0.0167811,0.0960964,0.2121736,0.3616913,0.5396101,0.7440881,0.9220957,1.0460304,1.0513825,1.0511992,1.0510530,1.0517397,1.0516043,1.0511944,1.0511590,1.0516613,1.0514039,1.0515941,1.0511460,1.0515124,1.0508871,1.0508924,1.0477493,1.0493273,1.0435964,1.0392281),
  auto[](0.1657560,0.1184644,0.1240829,0.1137127,0.0789924,0.0322056,-0.0107984,0.0180520,0.0053407,0.0136549,-0.0059564,-0.0018444,-0.0105719,-0.0029376,-0.0107905,-0.0080224,-0.0022669,0.0070200,-0.0081528,0.6077287,0.9883156,0.9939169,1.0039339,0.9923450,0.9992653,1.0084622,0.9835830,1.0085024,0.9745114,0.9854327,0.9349576,0.9871391),
  auto[](0.0026494,-0.0050175,-0.0125472,-0.0094555,-0.0125261,-0.0079171,-0.0079956,-0.0093559,0.0654686,0.3957288,0.7524402,0.9637648,0.9985443,0.9999298,0.9993908,0.9999437,0.9993912,0.9991124,0.9601958,0.6318628,0.2579740,0.0094015,-0.0030798,-0.0045230,-0.0068933,-0.0090352,-0.0085914,-0.0083691,-0.0078686,-0.0000084,0.0054301,-0.0027746),
  auto[](0.9920977,0.9887643,0.9953904,0.9952932,0.9918145,1.0002584,0.9996848,0.9998812,0.9850401,0.7902985,0.5608220,0.3313346,0.1369241,0.0189149,-0.0000051,-0.0004240,-0.0004193,0.0017473,0.0037999,-0.0005510,-0.0000437,0.0075875,0.0257957,0.0381684,0.0494896,0.0495960,0.0498148,0.0398409,0.0305010,0.0212431,0.0069597,0.0041734),
);
@(hot noinline)
color nontrivialRGBToColor(float3 rgb){
  #assert(bool($state.wavelength_base));
  const int k0(#all(rgb.xx<rgb.yz)?0:rgb.y<rgb.z?1:2);
  const int k0Plus1((k0+1)%3);
  const int k0Plus2((k0+2)%3);
  const bool shouldSwap(rgb[k0Plus1]>rgb[k0Plus2]);
  const int k1(shouldSwap?k0Plus2:k0Plus1);
  const int k2(shouldSwap?k0Plus1:k0Plus2);
  const float coeffW(rgb[k0]);
  const float coeffCMY(rgb[k1]-rgb[k0]);
  const float coeffRGB(rgb[k2]-rgb[k1]);
  color c(0.0);
  color w(color($state.wavelength_base));
  w-=RGB_TO_COLOR_MIN_WAVELENGTH;
  w*=RGB_TO_COLOR_NUM_WAVELENGTHS/(RGB_TO_COLOR_MAX_WAVELENGTH-RGB_TO_COLOR_MIN_WAVELENGTH);
  for(int i=0;i<$WAVELENGTH_BASE_MAX;i++){
    auto t(w[i]);
    if((0.0<=t)&(t<=RGB_TO_COLOR_NUM_WAVELENGTHS)){
      int j(#min(int(t),RGB_TO_COLOR_NUM_WAVELENGTHS-2));
      t=#min(t-j,1.0);
      c[i]=#sum(float2(1-t,t)*(coeffW*float2(&RGB_TO_COLOR_CURVES[0][j])+coeffCMY*float2(&RGB_TO_COLOR_CURVES[k0+1][j])+coeffRGB*float2(&RGB_TO_COLOR_CURVES[k2+4][j])));
    }
  }
  return #max(c*0.94,0.0);
}
@(macro)
export color _rgb_to_color(const float3 rgb){
  if(#all(rgb.xx==rgb.yz)){
    return color(rgb.x);
  } else {
    return nontrivialRGBToColor(rgb);
  }
}
@(pure macro)
export int _lower_bound(int count,const &float xs,const float x){
  int first=0;
  while(count>0){
    const int step=count/2;
    const int i=first+step;
    if(xs[i]<x){
      first=i+1;
      count=count-step+1;
    } else {
      count=step;
    }
  }
  return first;
}
@(pure)
export float _polyline_lerp(int count,const &float xs,const &float ys,const float x){
  if(count<=0){
    return 0.0;
  } else if(count==1){
    return ys[0];
  } else {
    int i=_lower_bound(count,xs,x)-1;
    i=#min(i,count-2);
    i=#max(i,0);
    const auto x0=xs[i];
    const auto x1=xs[i+1];
    float t=(x-x0)/(x1-x0);
    t=#max(t,0.0);
    t=#min(t,1.0);
    return (1-t)*ys[i]+t*ys[i+1];
  }
}
@(noinline)
export color _samples_to_color(const int count,const &float wavelengths,const &float amplitudes){
  auto c=color(0.0);
  if(count>0){
    if(count==1){
      c=color(amplitudes[0]);
    } else {
      for(int i=0;i<$WAVELENGTH_BASE_MAX;i++){
        c[i]=_polyline_lerp(count,wavelengths,amplitudes,$state.wavelength_base[i]);
      }
    }
  }
  return c;
}
@(pure)
export float3 _wyman_xyz(const float w){
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
@(pure)
export float _wyman_y(const float w){
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
@(hot noinline)
export float3 _color_to_rgb(const color c){
  float3 result(0.0);
  for(int i=0;i<$WAVELENGTH_BASE_MAX;++i){
    result+=_wyman_xyz($state.wavelength_base[i])*c[i];
  }
  result/=$WAVELENGTH_BASE_MAX;
  result*=$state.wavelength_max-$state.wavelength_min;
  return float3x3(float3(3.240450,-0.969266,0.0556434),float3(-1.537140,1.876010,-0.2040260),float3(-0.498532,0.041556,1.0572300),)*result;
}
@(visible noinline)
void smdlRGBToColor(const &float3 rgb,const &float cptr){
  color c(_rgb_to_color(*rgb));
  #memcpy(cptr,&c,#sizeof(color));
}
@(visible noinline)
void smdlColorToRGB(const &float cptr,const &float3 rgb){
  *rgb=_color_to_rgb(color(cptr));
}
export enum intensity_mode{intensity_radiant_exitance,intensity_power,};
export tag bsdf;
export tag vdf;
export tag edf;
export tag hair_bsdf;
export struct _default_bsdf:default bsdf{
  static const int df_flags=0;
};
export struct _default_vdf:default vdf{
  static const int df_flags=0;
};
export struct _default_edf:default edf{
  static const int df_flags=0;
};
export struct _default_hair_bsdf:default hair_bsdf{
  static const int df_flags=0;
};
export struct texture_2d{
  texture_2d(const string name,const auto gamma=0)=#load_texture_2d(name,int(gamma));
  const int2 tile_count=int2(1,1);
  const auto tile_extents=int2[](int2(0));
  const auto tile_buffers=auto[](cast<&float4>(none));
  const int gamma=0;
};
export struct texture_3d{
  texture_3d(const string name,const auto gamma=0)=#load_texture_3d(name,int(gamma));
  const int gamma=0;
};
export struct texture_cube{
  texture_cube(const string name,const auto gamma=0)=#load_texture_cube(name,int(gamma));
  const int gamma=0;
};
export struct texture_ptex{
  texture_ptex(const string name,const auto gamma=0)=#load_texture_ptex(name,int(gamma));
  const &void ptr=none;
  const int gamma=0;
};
export struct bsdf_measurement{
  bsdf_measurement(const string name)=#load_bsdf_measurement(name);
  const &void ptr=none;
  const int mode=0;
  const int num_theta=0;
  const int num_phi=0;
  const auto buffer=cast<&float>(none);
};
export struct light_profile{
  light_profile(const string name)=#load_light_profile(name);
  const &void ptr=none;
  const float max_intensity=0;
  const float power=0;
};
export struct spectral_curve{
  spectral_curve(const string name)=#load_spectral_curve(name);
  spectral_curve(const string name,const int curve_index)=#load_spectral_curve(name,curve_index);
  spectral_curve(const string name,const string curve_name)=#load_spectral_curve(name,curve_name);
  const int count=0;
  const &float wavelengths=none;
  const &float amplitudes=none;
};
@(macro)
export color _spectral_curve_to_color(const spectral_curve curve){
  return _samples_to_color(curve.count,curve.wavelengths,curve.amplitudes);
}
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
  $(?color) absorption_coefficient=none;
  $(?color) scattering_coefficient=none;
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
  float ior=1.4;
  material_volume volume=material_volume();
  material_geometry geometry=material_geometry();
  hair_bsdf hair=hair_bsdf();
  float temperature=-1;
};
const int MATERIAL_TRANSPORT_IMPORTANCE=(1<<0);
const int MATERIAL_THIN_WALLED=(1<<1);
const int MATERIAL_HAS_SURFACE=(1<<2);
const int MATERIAL_HAS_BACKFACE=(1<<3);
const int MATERIAL_HAS_VOLUME=(1<<6);
const int MATERIAL_HAS_HAIR=(1<<7);
export struct _MaterialInstance{
  &material ptr;
  &material_geometry geometry=&ptr.geometry;
  float ior=ptr.ior;
  float temperature=ptr.temperature;
  &color absorption_coefficient=#is_void(ptr.volume.absorption_coefficient)?none:&ptr.volume.absorption_coefficient;
  &color scattering_coefficient=#is_void(ptr.volume.scattering_coefficient)?none:&ptr.volume.scattering_coefficient;
  int wavelength_base_max=$WAVELENGTH_BASE_MAX;
  int flags=$state.transport|(ptr.thin_walled?MATERIAL_THIN_WALLED:0)|(!#is_default(ptr.surface)?MATERIAL_HAS_SURFACE:0)|(!#is_default(ptr.backface)?MATERIAL_HAS_BACKFACE:0)|(!#is_default(ptr.volume)?MATERIAL_HAS_VOLUME:0)|(!#is_default(ptr.hair)?MATERIAL_HAS_HAIR:0);
  int df_flags_surface=ptr.surface.scattering.df_flags;
  int df_flags_backface=ptr.backface.scattering.df_flags;
  float3x3 tangent_to_world=let {
                              const auto tangent_to_world_matrix=$state.object_to_world_matrix*$state.tangent_to_object_matrix;
                            } in float3x3(tangent_to_world_matrix[0].xyz,tangent_to_world_matrix[1].xyz,tangent_to_world_matrix[2].xyz,);
};
export struct _AlbedoLUT{
  const int num_cos_theta=0;
  const int num_roughness=0;
  const &float directional_albedo=none;
  const &float average_albedo=none;
};
export struct complex{
  auto a=0.0;
  auto b=0.0;
};
@(pure macro)
export auto _complex_neg(const complex z)=complex(-z.a,-z.b);
@(pure macro)
export auto _complex_conj(const complex z)=complex(z.a,-z.b);
@(pure macro)
export auto _complex_norm(const complex z)=z.a*z.a+z.b*z.b;
@(pure macro)
export auto _complex_abs(const complex z)=#sqrt(_complex_norm(z));
@(pure macro)
export auto _complex_inv(const complex z)=let {
                                            const auto denom=1.0/_complex_norm(z);
                                          } in complex(z.a*denom,-z.b*denom);
@(pure macro)
export auto _complex_add(const complex z,const complex w)=complex(z.a+w.a,z.b+w.b);
@(pure macro)
export auto _complex_sub(const complex z,const complex w)=complex(z.a-w.a,z.b-w.b);
@(pure macro)
export auto _complex_mul(const complex z,const complex w)=complex(z.a*w.a-z.b*w.b,z.a*w.b+z.b*w.a);
@(pure macro)
export auto _complex_div(const complex z,const complex w)=_complex_mul(z,_complex_inv(w));
@(pure macro)
export auto _complex_exp(const complex z)=let {
                                            const auto a=#exp(z.a);
                                          } in complex(a*#cos(z.b),a*#sin(z.b));
@(pure macro)
export auto _complex_log(const complex z)=complex(#log(_complex_abs(z)),#atan2(z.b,z.a));
@(pure macro)
export auto _complex_sqrt(const complex z)=let {
                                             const auto absz=_complex_abs(z);
                                           } in complex(#sqrt(0.5*(absz+z.a)),#sqrt(0.5*(absz-z.a))*#sign(z.b),);
@(pure)
export int32_t _hash(auto value){
  if$(#is_arithmetic_scalar(value)){
    if$(#is_arithmetic_integral(value)){
      if$(#sizeof(value)<=4){
        auto hash(int32_t(value)+3266445271);
        hash^=hash>>>16,hash*=0x85EBCA6B;
        hash^=hash>>>13,hash*=0xC2B2AE35;
        hash^=hash>>>16;
        return hash;
      } else {
        auto hash(int64_t(value)+13898551614298330943);
        hash^=hash>>>33,hash*=0xFF51AFD7ED558CCD;
        hash^=hash>>>33,hash*=0xC4CEB9FE1A85EC53;
        hash^=hash>>>33;
        return hash;
      }
    } else {
      return _hash(#bitcast(#type_int(8*#sizeof(value)),value));
    }
  } else if$(#is_array(value)|#is_arithmetic_vector(value)|#is_arithmetic_matrix(value)|(#typeof(value)==color)){
    auto totalHash(_hash(value[0]));
    for(int i=1;i<#num(value);++i){
      auto hash(_hash(value[i]));
      hash=0x55555555*(hash^(hash>>>16));
      hash=3423571495*(hash^(hash>>>16));
      totalHash=#rotl(totalHash,10)^hash;
    }
    return totalHash;
  } else if$(#is_pointer(value)){
    return _hash(#bitcast(intptr_t,value));
  } else if$(#is_union(value)){
    visit v in value{
      return _hash(v);
    }
  } else {
    #panic("Unimplemented hash");
    return 0;
  }
}
)*";

static const char *const debug = R"*(#smdl
@(pure macro)
export bool assert(const bool condition,const string reason){
  #assert(condition,reason) if($DEBUG);
  return true;
}
@(pure macro)
export bool breakpoint(){
  #breakpoint() if($DEBUG);
  return true;
}
@(pure macro)
export bool print(const auto a){
  #print(a) if($DEBUG);
  return true;
}
)*";

static const char *const df = R"*(#smdl
using ::math import *;
import ::tex::*;
const float EPSILON=1e-6;
const float MULTISCATTER_DIFFUSE_CHANCE=0.2;
const int DF_REFLECTION=(1<<0);
const int DF_TRANSMISSION=(1<<1);
const int DF_DIFFUSE=(1<<2);
const int DF_GLOSSY=(1<<3);
const int DF_SPECULAR=(1<<4);
@(macro)
float3x3 orthonormalBasis(float3 z){
  z=normalize(z);
  auto x=z.z<-0.9999?float3(0.0,-1.0,0.0):float3(-z.x/(z.z+1.0)+1.0,-z.y/(z.z+1.0),-1.0);
  x=normalize(x-dot(x,z)*z);
  auto y=normalize(cross(z,x));
  return float3x3(x,y,z);
}
export enum scatter_mode{
  scatter_none=0x0,
  scatter_reflect=0x1,
  scatter_transmit=0x2,
  scatter_reflect_transmit=0x3,
};
@(pure macro)
float scatterReflectChance(const scatter_mode mode){
  const auto reflWeight(#select((int(mode)&1)!=0,1.0,0.0));
  const auto tranWeight(#select((int(mode)&2)!=0,1.0,0.0));
  return reflWeight/(reflWeight+tranWeight);
}
@(pure foreign)
double erf(double x);
@(pure foreign)
double erfc(double x);
export namespace monte_carlo {
@(pure macro)
export float2 nextLowDiscrepancy(const &float2 xi)=(*xi=frac(*xi+float2(0.75487766,0.56984029)));
@(pure macro)
export float3 nextLowDiscrepancy(const &float3 xi)=(*xi=frac(*xi+float3(0.81917251,0.67104360,0.54970047)));
@(pure macro)
export float4 nextLowDiscrepancy(const &float4 xi)=(*xi=frac(*xi+float4(0.85667488,0.73389185,0.62870672,0.53859725)));
@(pure macro)
export bool boolSample(const &float xi,const float chance){
  if(*xi<chance){
    *xi=(*xi/chance);
    return true;
  } else {
    *xi=(*xi-chance)/(1-chance);
    return false;
  }
}
@(pure macro)
export int uniformWavelengthIndexSample(const &float xi){
  const int i(#min(int(*xi*=$WAVELENGTH_BASE_MAX),$WAVELENGTH_BASE_MAX-1));
  *xi-=i;
  return i;
}
@(pure)
export float2 uniformDiskSample(float2 xi){
  xi=2*xi-1;
  xi=#select(xi==0,EPSILON,xi);
  const bool cond((absxi:=#abs(xi),absxi.x>absxi.y));
  const float rad(#select(cond,xi.x,xi.y));
  const float phi(#select(cond,($PI/4)*xi.y/xi.x,($PI/2)-($PI/4)*xi.x/xi.y));
  return rad*float2(#cos(phi),#sin(phi));
}
@(pure)
export float3 cosineHemisphereSample(float2 xi){
  return float3((p:=uniformDiskSample(xi)),#sqrt(#max(1-#sum(p*p),0)));
}
@(pure)
export float3 uniformHemisphereSample(float2 xi){
  const float cosTheta=saturate(xi.x);
  const float sinTheta=#sqrt(1-cosTheta*cosTheta);
  return float3(sinTheta*#cos(phi:=$TWO_PI*xi.y),sinTheta*#sin(phi),cosTheta,);
}
@(pure)
export float3 uniformSphereSample(float2 xi){
  const float cosTheta=2*saturate(xi.x)-1;
  const float sinTheta=#sqrt(1-cosTheta*cosTheta);
  return float3(sinTheta*#cos(phi:=$TWO_PI*xi.y),sinTheta*#sin(phi),cosTheta,);
}
@(pure)
export double erfInverse(double y){
  double w=-#log(#max(1e-6d,(1-y)*(1+y)));
  double x=0;
  if(w<5){
    w=w-2.5d;
    x=w*2.81022636e-08d+3.43273939e-7d;
    x=w*x-3.52338770e-6d;
    x=w*x-4.39150654e-6d;
    x=w*x+2.18580870e-4d;
    x=w*x-1.25372503e-3d;
    x=w*x-4.17768164e-3d;
    x=w*x+2.46640727e-1d;
    x=w*x+1.50140941d;
  } else {
    w=#sqrt(w)-3;
    x=x*-2.00214257e-4d+1.00950558e-4d;
    x=w*x+1.34934322e-3d;
    x=w*x-3.67342844e-3d;
    x=w*x+5.73950773e-3d;
    x=w*x-7.62246130e-3d;
    x=w*x+9.43887047e-3d;
    x=w*x+1.00167406d;
    x=w*x+2.83297682d;
  }
  x*=y;
  x-=(erf(x)-y)/(1.1283791671d*#exp(-x*x));
  x-=(erf(x)-y)/(1.1283791671d*#exp(-x*x));
  return x;
}
}
export namespace specular {
@(pure)
export float3 reflect(const float3 wi,const float3 wm)=2*#sum(wi*wm)*wm-wi;
@(pure)
export float3 refract(const float3 wi,const float3 wm,const float ior){
  const auto cosThetai(#sum(wi*wm));
  const auto cos2Thetai(#min(cosThetai*cosThetai,1));
  const auto cos2Thetat(#max(1-ior*ior*(1-cos2Thetai),0));
  const auto cosThetat(#sqrt(cos2Thetat)*-#sign(cosThetai));
  return -ior*wi+(ior*cosThetai+cosThetat)*wm;
}
@(pure)
export float3 reflectionHalfVector(const float3 wo,const float3 wi)=(vh:=(wo+wi))*#sign(vh.z);
@(pure)
export float3 refractionHalfVector(const float3 wo,const float3 wi,const float ior,)=(vh:=-(ior*wo+wi))*#sign(vh.z);
@(pure)
export auto refractionHalfVectorJacobian(const float3 wo,const float3 wi,const float ior,)=#abs(#sum(wi*(vh:=refractionHalfVector(wo,wi,ior))))/((vh2:=#sum(vh*vh))*#sqrt(vh2));
@(pure macro)
export auto schlickF0(const auto ior)=#pow((ior-1)/(ior+1),2);
@(pure macro)
export auto schlickFresnel(
  const auto cosTheta,
  const auto F0,
  const auto F90=1.0,
  const float exponent=5,
)=F0+(F90-F0)*#pow(#max(1-#abs(cosTheta),0),exponent);
@(pure)
export auto dielectricFresnel(const float cosThetai,const auto ior){
  const auto cosThetat=#sqrt(#max(1.0-ior*ior*(1.0-cosThetai*cosThetai),0.0))*#sign(cosThetai);
  const auto iorCosThetai=ior*cosThetai;
  const auto iorCosThetat=ior*cosThetat;
  const auto rs=(iorCosThetai-cosThetat)/(iorCosThetai+cosThetat);
  const auto rp=(cosThetai-iorCosThetat)/(cosThetai+iorCosThetat);
  return #min(0.5*(rs*rs+rp*rp),1.0);
}
@(pure)
export auto conductorFresnel(const float cosThetai,const auto ior){
  const auto cosThetat=#sqrt(1.0-ior*ior*(1.0-cosThetai*cosThetai))*#sign(cosThetai);
  const auto iorCosThetai=ior*cosThetai;
  const auto iorCosThetat=ior*cosThetat;
  const auto rs=(iorCosThetai-cosThetat)/(iorCosThetai+cosThetat);
  const auto rp=(cosThetai-iorCosThetat)/(cosThetai+iorCosThetat);
  return #min(0.5*(#norm(rs)+#norm(rp)),1.0);
}
}
@(pure noinline)
float3x3 calculateTangentSpace(const float3 normal,const float3 tangent_u){
  const auto tw(normalize(normal)*#sign(normal.z));
  const auto tu(normalize(tangent_u-dot(tangent_u,tw)*tw));
  const auto tv(normalize(cross(tw,tu)));
  return float3x3(tu,tv,tw);
}
struct ScatterEvaluateParameters{
  bool isImportance;
  float3 wo0;
  float3 wi0;
  scatter_mode mode=(wo0.z<0)==(wi0.z<0)?scatter_reflect:scatter_transmit;
  bool hitBackface=wo0.z<0;
  bool thin_walled=false;
  float ior=1/1.4;
  float3 normal=float3(0,0,1);
  float3 tangent_u=float3(1,0,0);
  float3 wo=wo0;
  float3 wi=wi0;
  float shadingNormalCorrection=1;
  finalize {
    if(hitBackface){
      wo0=-wo0;
      wi0=-wi0;
      wo=-wo;
      wi=-wi;
      ior=1.0/ior;
    }
  }
};
struct ScatterEvaluateResult{
  $(color|float) f=0.0;
  float2 pdf=float2(0.0);
  bool isBlack=false;
};
@(pure noinline)
bool recalculateTangentSpace(inline const &ScatterEvaluateParameters params){
  auto tbn(calculateTangentSpace(normal,tangent_u));
  wo=normalize(wo0*tbn);
  wi=normalize(wi0*tbn);
  if(isImportance){
    const auto numer=wo.z*wi0.z;
    const auto denom=wi.z*wo0.z;
    shadingNormalCorrection=(denom==0?1:#abs(numer/denom));
  } else {
    shadingNormalCorrection=1;
  }
  return ((wo.z<0)==(wo0.z<0))&((wi.z<0)==(wi0.z<0));
}
@(pure)
float3 halfDirection(inline const &ScatterEvaluateParameters params){
  return normalize(mode==scatter_reflect?specular::reflectionHalfVector(wo,wi):specular::refractionHalfVector(wo,wi,ior));
}
struct ScatterSampleParameters{
  bool isImportance;
  float3 wo0;
  bool hitBackface=wo0.z<0;
  bool thin_walled=false;
  float ior=1/1.4;
  float3 normal=float3(0,0,1);
  float3 tangent_u=float3(1,0,0);
  float3 wo=wo0;
  float4 xi;
  finalize {
    if(hitBackface){
      wo0=-wo0;
      wo=-wo;
      ior=1/ior;
    }
  }
};
struct ScatterSampleResult{
  float3 wi=float3(0.0);
  scatter_mode mode=scatter_none;
  ?color fDelta=none;
};
@(pure noinline)
?float3x3 recalculateTangentSpace(inline const &ScatterSampleParameters params){
  auto tbn(calculateTangentSpace(normal,tangent_u));
  wo=wo0*tbn;
  return tbn if((wo.z<0)==(wo0.z<0));
}
@(pure)
float3 halfDirection(inline const &ScatterSampleParameters this,inline const &ScatterSampleResult result){
  return normalize(mode==scatter_reflect?specular::reflectionHalfVector(wo,wi):specular::refractionHalfVector(wo,wi,ior));
}
@(pure macro)
float sampleShadingNormalCorrection(inline const &ScatterSampleParameters params,const float3 wiShading,const float3 wiNatural){
  const auto numer(wo.z*wiNatural.z);
  const auto denom(wiShading.z*wo0.z);
  return denom==0?1.0:#abs(numer/denom);
}
@(pure macro)
auto ScatterEvaluateResultWithMultiscatter(
  const auto this,
  const auto f,
  const float2 pdf,
  float cosThetao[[anno::unused()]],
  float cosThetai[[anno::unused()]],
  const float roughness[[anno::unused()]],
  const string lutName[[anno::unused()]],
){
  if(#typeof(this.multiscatter_tint)==void||(#typeof(this.multiscatter_tint)==float&&this.multiscatter_tint==0.0)){
    return ScatterEvaluateResult(f: this.tint*f,pdf: pdf);
  } else {
    cosThetao=#abs(cosThetao);
    cosThetai=#abs(cosThetai);
    const auto lut(#albedo_lut(lutName));
    float t((lut.num_roughness-1)*saturate(roughness));
    const int j(#min(int(#floor(t)),lut.num_roughness-2));
    t=t-j;
    const float Ewo=return_from{
      float s((lut.num_cos_theta-1)*#min(cosThetao,1));
      const int i(#min(int(#floor(s)),lut.num_cos_theta-2));
      const &float ptr0(&lut.directional_albedo[lut.num_roughness*(i+0)+j]);
      const &float ptr1(&lut.directional_albedo[lut.num_roughness*(i+1)+j]);
      s=s-i;
      return #min(1.0,lerp(lerp(ptr0[0],ptr0[1],t),lerp(ptr1[0],ptr1[1],t),s));
    };
    const float Ewi=return_from{
      float s((lut.num_cos_theta-1)*#min(cosThetai,1));
      const int i(#min(int(#floor(s)),lut.num_cos_theta-2));
      const &float ptr0(&lut.directional_albedo[lut.num_roughness*(i+0)+j]);
      const &float ptr1(&lut.directional_albedo[lut.num_roughness*(i+1)+j]);
      s=s-i;
      return #min(1.0,lerp(lerp(ptr0[0],ptr0[1],t),lerp(ptr1[0],ptr1[1],t),s));
    };
    const float Eav=#min(1.0,lerp(lut.average_albedo[j],lut.average_albedo[j+1],t));
    const auto ms_f=cosThetai/$PI*(1-Ewo)*(1-Ewi)/(1-Eav+1e-6);
    const auto ms_pdf=auto(cosThetai,cosThetao)/$PI;
    return ScatterEvaluateResult(f: this.tint*(f+this.multiscatter_tint*ms_f),pdf: lerp(pdf,ms_pdf,MULTISCATTER_DIFFUSE_CHANCE));
  }
}
@(pure macro)
?ScatterSampleResult ScatterSampleResultWithMultiscatter(const auto this,const &float4 xi[[anno::unused()]],const float3x3 tbn[[anno::unused()]]){
  if(#typeof(this.multiscatter_tint)==void||(#typeof(this.multiscatter_tint)==float&&this.multiscatter_tint==0.0)){
  } else {
    if(monte_carlo::boolSample(&xi.w,MULTISCATTER_DIFFUSE_CHANCE)){
      return ScatterSampleResult(wi: tbn*monte_carlo::cosineHemisphereSample(xi.xy),mode: scatter_reflect);
    }
  }
}
@(pure macro)
auto scatterEvaluate(const &_default_bsdf this[[anno::unused()]],const &ScatterEvaluateParameters params[[anno::unused()]]){
  return ScatterEvaluateResult(isBlack: true);
}
@(pure macro)
auto scatterSample(const &_default_bsdf this[[anno::unused()]],const &ScatterSampleParameters params[[anno::unused()]]){
  return ScatterSampleResult();
}
@(macro)
auto scatterEvaluate(const &_default_vdf this[[anno::unused()]],const &ScatterEvaluateParameters params[[anno::unused()]]){
  return ScatterEvaluateResult(isBlack: true);
}
@(macro)
auto scatterSample(const &_default_vdf this[[anno::unused()]],const &ScatterSampleParameters params[[anno::unused()]]){
  return ScatterSampleResult();
}
export struct diffuse_reflection_bsdf:bsdf{
  const $(color|float) tint=1.0;
  const float roughness=0.0;
  void handle="";
  const $(?(color|float)) multiscatter_tint=none;
  static const int df_flags=DF_REFLECTION|DF_DIFFUSE;
};
@(pure)
auto scatterEvaluate(const &diffuse_reflection_bsdf this,inline const &ScatterEvaluateParameters params){
  if(mode==scatter_reflect&&recalculateTangentSpace(params)){
    const auto cosTheta(#abs(auto(wi.z,wo.z)));
    const auto pdf(cosTheta/$PI);
    if(this.roughness==0){
      auto result(ScatterEvaluateResult(f: this.tint*pdf[0],pdf: pdf));
      result.f*=shadingNormalCorrection if(isImportance);
      return result;
    } else {
      const auto sigma2(2.0*this.roughness*this.roughness);
      const auto A(1.00-sigma2/(2.0*sigma2+0.66));
      const auto B(0.45*sigma2/(sigma2+0.09));
      const auto f(pdf[0]*(A+#max(#sum(wo.xy*wi.xy),0)/(#max_value(cosTheta)+EPSILON)*B));
      auto result(ScatterEvaluateResultWithMultiscatter(this,f,pdf,wo.z,wi.z,this.roughness,"diffuse_reflection_bsdf"));
      result.f*=shadingNormalCorrection if(isImportance);
      return result;
    }
  } else {
    return ScatterEvaluateResult(isBlack: true);
  }
}
@(pure)
auto scatterSample(const &diffuse_reflection_bsdf this[[anno::unused()]],inline const &ScatterSampleParameters params){
  if((tbn:=recalculateTangentSpace(params))){
    return ScatterSampleResult(wi: (*tbn)*monte_carlo::cosineHemisphereSample(xi.xy),mode: scatter_reflect);
  } else {
    return ScatterSampleResult();
  }
}
export struct diffuse_transmission_bsdf:bsdf{
  const $(color|float) tint=1.0;
  void handle="";
  static const int df_flags=DF_TRANSMISSION|DF_DIFFUSE;
};
@(pure)
auto scatterEvaluate(inline const &diffuse_transmission_bsdf this,inline const &ScatterEvaluateParameters params){
  if(mode==scatter_transmit&&recalculateTangentSpace(params)){
    const auto cosTheta(#abs(auto(wi.z,wo.z)));
    const auto pdf(cosTheta/$PI);
    auto result(ScatterEvaluateResult(f: tint*pdf[0],pdf: pdf));
    result.f*=shadingNormalCorrection if(isImportance);
    return result;
  } else {
    return ScatterEvaluateResult(isBlack: true);
  }
}
@(pure)
auto scatterSample(inline const &diffuse_transmission_bsdf this,inline const &ScatterSampleParameters params){
  if((tbn:=recalculateTangentSpace(params))){
    return ScatterSampleResult(wi: (*tbn)*-monte_carlo::cosineHemisphereSample(xi.xy),mode: scatter_transmit);
  } else {
    return ScatterSampleResult();
  }
}
export struct specular_bsdf:bsdf{
  const $(color|float) tint=1.0;
  const scatter_mode mode=scatter_reflect;
  void handle="";
  const int df_flags=int(mode)|DF_SPECULAR;
};
@(pure macro)
auto scatterEvaluate(const &specular_bsdf this[[anno::unused()]],const &ScatterEvaluateParameters params[[anno::unused()]]){
  return ScatterEvaluateResult(isBlack: true);
}
@(pure macro)
auto scatterSample(const &specular_bsdf this,inline const &ScatterSampleParameters params){
  if((tbn:=recalculateTangentSpace(params))){
    if(xi.x<scatterReflectChance(this.mode)){
      const auto wiLocal(specular::reflect(wo,float3(0,0,1)));
      auto result=ScatterSampleResult(wi: (*tbn)*wiLocal,mode: scatter_reflect,fDelta: color(this.tint));
      *result.fDelta*=sampleShadingNormalCorrection(params,wiLocal,result.wi) if(isImportance);
      return result;
    } else {
      const auto wiLocal(specular::refract(wo,float3(0,0,1),ior));
      auto result=ScatterSampleResult(wi: (*tbn)*wiLocal,mode: scatter_transmit,fDelta: color(this.tint));
      if(isImportance){
        *result.fDelta*=sampleShadingNormalCorrection(params,wiLocal,result.wi);
      } else {
        *result.fDelta*=ior*ior;
      }
      return result;
    }
  } else {
    return ScatterSampleResult();
  }
}
export struct sheen_bsdf:bsdf{
  float roughness;
  const $(color|float) tint=1.0;
  const $(?(color|float)) multiscatter_tint=none;
  void multiscatter=none;
  void handle="";
  static const int df_flags=DF_REFLECTION|DF_DIFFUSE;
  finalize {
    roughness=saturate(roughness);
  }
};
@(pure)
float sheen_lambda_L(const auto fit,const float mu){
  return fit[0]/(1.0+fit[1]*#pow(mu,fit[2]))+fit[3]*mu+fit[4];
}
@(pure)
float sheen_lambda(const auto fit,const float mu){
  return #exp(mu<0.5?sheen_lambda_L(fit,mu):2*sheen_lambda_L(fit,0.5)-sheen_lambda_L(fit,#max(1-mu,0)));
}
@(pure)
auto scatterEvaluate(const &sheen_bsdf this,inline const &ScatterEvaluateParameters params){
  if(mode==scatter_reflect&&recalculateTangentSpace(params)){
    const auto cosThetao(#abs(wo.z));
    const auto cosThetai(#abs(wi.z));
    const auto pdf(float2(cosThetai,cosThetao)/$PI);
    const auto f=let {
      const auto alpha(lerp(0.1,1.0,#pow(this.roughness,2)));
      const auto fit=lerp(auto(21.5473,3.82987,0.19823,-1.97760,-4.32054),auto(25.3245,3.32435,0.16801,-1.27393,-4.85967),#pow(1-#pow(this.roughness,2),2),);
      const auto cosThetah(normalize(wo+wi).z);
      const auto sinThetah(#sqrt(1-cosThetah*cosThetah));
      const auto D(1/$TWO_PI*(2+1/alpha)*#pow(sinThetah,1/alpha));
      const auto G(1/(1+sheen_lambda(fit,cosThetao)+sheen_lambda(fit,cosThetai)));
    } in D*G/(4*cosThetao);
    auto result(ScatterEvaluateResultWithMultiscatter(this,f,pdf,cosThetao,cosThetai,this.roughness,"sheen_bsdf"));
    result.f*=shadingNormalCorrection if(isImportance);
    return result;
  } else {
    return ScatterEvaluateResult(isBlack: true);
  }
}
@(pure)
auto scatterSample(const &sheen_bsdf this[[anno::unused()]],inline const &ScatterSampleParameters params){
  if((tbn:=recalculateTangentSpace(params))){
    return ScatterSampleResult(wi: (*tbn)*monte_carlo::cosineHemisphereSample(xi.xy),mode: scatter_reflect);
  } else {
    return ScatterSampleResult();
  }
}
export struct ward_geisler_moroder_bsdf:bsdf{
  float roughness_u;
  float roughness_v=roughness_u;
  $(color|float) tint=1.0;
  $(?(color|float)) multiscatter_tint=none;
  float3 tangent_u=$state.texture_tangent_u[0];
  void handle="";
  static const int df_flags=DF_REFLECTION|DF_GLOSSY;
  finalize {
    roughness_u=saturate(roughness_u);
    roughness_v=saturate(roughness_v);
  }
};
@(pure noinline)
auto scatterEvaluate(const &ward_geisler_moroder_bsdf this,inline const &ScatterEvaluateParameters params){
  preserve tangent_u;
  tangent_u=this.tangent_u;
  if(mode==scatter_reflect&&recalculateTangentSpace(params)){
    const auto cosThetao(#abs(wo.z));
    const auto cosThetai(#abs(wi.z));
    const auto roughness(this.roughness_u,this.roughness_v);
    const auto roughness0(#sqrt(#prod(roughness)));
    const auto alpha(#max(0.001,roughness*roughness));
    const auto f0(#sum((h:=wo+wi)*h)/($PI*alpha.x*alpha.y*#pow(h.z,4))*#exp(-#sum((g:=h.xy/(h.z*alpha))*g)));
    const auto f(f0*cosThetai);
    const auto pdf(float2(f0*(cosThetao+cosThetai)/2));
    auto result(ScatterEvaluateResultWithMultiscatter(this,f,pdf,cosThetao,cosThetai,roughness0,"ward_geisler_moroder_bsdf"));
    result.f*=shadingNormalCorrection if(isImportance);
    return result;
  } else {
    return ScatterEvaluateResult(isBlack: true);
  }
}
@(pure noinline)
auto scatterSample(const &ward_geisler_moroder_bsdf this,inline const &ScatterSampleParameters params){
  preserve tangent_u;
  tangent_u=this.tangent_u;
  if((tbn:=recalculateTangentSpace(params))){
    if(result:=ScatterSampleResultWithMultiscatter(this,xi,*tbn)){
      return *result;
    } else {
      const auto roughness(this.roughness_u,this.roughness_v);
      const auto alpha(#max(0.001,roughness*roughness));
      const auto phi(#atan2(alpha.y*#sin(t:=$TWO_PI*xi.x),alpha.x*#cos(t)));
      const auto cosPhi(#cos(phi));
      const auto sinPhi(#sin(phi));
      const auto theta(#atan(#sqrt(-#log(1-xi.y)/(#pow(cosPhi/alpha.x,2)+#pow(sinPhi/alpha.y,2)))));
      const auto wm(float3(#sin(theta)*float2(cosPhi,sinPhi),#cos(theta)));
      const auto wi(normalize(specular::reflect(wo,wm)));
      if(wi.z>0){
        return ScatterSampleResult(wi: (*tbn)*wi,mode: scatter_reflect);
      }
    }
  }
  return ScatterSampleResult();
}
export namespace microfacet {
export tag Distribution;
export struct DistributionGGX:default Distribution{};
export struct DistributionBeckmann:Distribution{};
@(pure macro)
export float smithLambda(const DistributionGGX this[[anno::unused()]],const float m){
  return 0.5*(#sign(m)*#sqrt(1+1/(m*m+EPSILON)))-0.5;
}
@(pure macro)
export float smithLambda(const DistributionBeckmann this[[anno::unused()]],const float m){
  return 0.5*(#exp(-m*m)/m/#sqrt($PI)-float(erfc(m)));
}
@(pure macro)
export float smithSlopePDF(const DistributionGGX this[[anno::unused()]],const float2 m){
  return (1/$PI)/#pow(1+#sum(m*m),2);
}
@(pure macro)
export float smithSlopePDF(const DistributionBeckmann this[[anno::unused()]],const float2 m){
  return (1/$PI)*#exp(-#sum(m*m));
}
@(pure)
export float2 smithVisibleSlopeSample(
  const DistributionGGX this[[anno::unused()]],
  const float xi0,
  const float xi1,
  float cosThetao,
){
  return #sqrt(xi0/(1-xi0+EPSILON))*float2(#cos(phi:=$TWO_PI*xi1),#sin(phi)) if(cosThetao>1-EPSILON);
  cosThetao=#max(cosThetao,-0.9999);
  const auto mx=return_from{
    const auto sinThetao(#sqrt(1-cosThetao*cosThetao));
    const auto tanThetao(sinThetao/cosThetao);
    const auto mu(xi0*(1+1/cosThetao)-1);
    const auto nu(1/(1-mu*mu));
    const auto D(#sqrt(#max(nu*(mu*mu-(1-nu)*tanThetao*tanThetao),0)));
    const auto mx0(-nu*tanThetao-D);
    const auto mx1(-nu*tanThetao+D);
    return #select((mu<0)|(mx1*sinThetao>cosThetao),mx0,mx1);
  };
  const auto my=return_from{
    const auto s(#select(xi1>0.5,1.0,-1.0));
    const auto t(#min(s*(2*xi1-1),1));
    return #sqrt(1+mx*mx)*s*((t*(t*(t*0.27385-0.73369)+0.46341))/(t*(t*(t*0.093073+0.30942)-1.0)+0.597999));
  };
  return float2(mx,my);
}
@(pure)
export float2 smithVisibleSlopeSample(
  const DistributionBeckmann this[[anno::unused()]],
  float xi0,
  float xi1,
  float cosThetao,
){
  return #sqrt(-#log(1-xi0+EPSILON))*float2(#cos((phi:=$TWO_PI*xi1)),#sin(phi)) if(cosThetao>1-EPSILON);
  xi0=#max(xi0,EPSILON);
  xi1=#max(xi1,EPSILON);
  const float invSqrtPi=1/#sqrt($PI);
  const float thetao=#acos(cosThetao);
  const float sinThetao=#sqrt(#max(0,1-cosThetao*cosThetao));
  const float tanThetao=sinThetao/cosThetao;
  const float cotThetao=1/tanThetao;
  float xmin=-1;
  float xmax=float(erf(cotThetao));
  float x=xmax-(1+xmax)*#pow(1-xi0,1+thetao*(-0.876+thetao*(0.4265-0.0594*thetao)));
  float norm=1/(1+xmax+invSqrtPi*tanThetao*#exp(-cotThetao*cotThetao));
  for(int i=0;i<10;++i){
    if(!(xmin<=x&&x<=xmax))
      x=0.5*(xmin+xmax);
    const float a=monte_carlo::erfInverse(x);
    const float f=norm*(1+x+invSqrtPi*tanThetao*#exp(-a*a))-xi0;
    break if(f~==[1e-5]0.0);
    if(f>0)
      xmax=x;
    else
      xmin=x;
    x-=f/(norm*(1-a*tanThetao));
  }
  return float2(monte_carlo::erfInverse(x),monte_carlo::erfInverse(2*xi1-1),);
}
@(pure macro)
export float smithNormalPDF(const Distribution this[[anno::unused()]],const float2 alpha,const float3 wm){
  return wm.z>0.0?smithSlopePDF(this,-wm.xy/(wm.z*alpha+EPSILON))/(alpha.x*alpha.y*#pow(wm.z,4)+EPSILON):0.0;
}
@(pure)
export float3 smithVisibleNormalSample(
  const Distribution this,
  const float xi0,
  const float xi1,
  const float2 alpha,
  const float3 wo,
){
  const auto w11(normalize(float3(alpha*wo.xy,wo.z)));
  const auto sinTheta(length(w11.xy));
  const auto cosPhi(w11.x/sinTheta);
  const auto sinPhi(w11.y/sinTheta);
  const auto m11(smithVisibleSlopeSample(this,xi0,xi1,w11.z));
  const auto m(float2(alpha.x*dot(float2(cosPhi,-sinPhi),m11),alpha.y*dot(float2(sinPhi,cosPhi),m11)));
  return #all(isfinite(m))?normalize(float3(-m,1)):wo.z==0?normalize(wo):float3(0,0,1);
}
export struct DistributionBlinn:Distribution{};
@(pure)
export void blinnNormalFirstQuadrantSample(
  const float xi0,
  const float xi1,
  const float2 e,
  &float phi,
  &float cosTheta,
){
  if(e.x==e.y){
    *phi=$HALF_PI*xi0;
    *cosTheta=#pow(xi1,1/(1+e.x));
  } else {
    *phi=#atan(#sqrt((1+e.x)/(1+e.y))*#tan($HALF_PI*xi0));
    *cosTheta=#pow(xi1,1/(1+e.x*(cosPhi:=#cos(*phi))*cosPhi+e.y*(sinPhi:=#sin(*phi))*sinPhi));
  }
}
@(pure)
export float3 blinnNormalSample(const float xi0,const float xi1,const float2 e,){
  float phi=0;
  float cosTheta=0;
  if(xi0<0.25){
    blinnNormalFirstQuadrantSample(4*xi0,xi1,e,&phi,&cosTheta);
  } else if(xi0<0.5){
    blinnNormalFirstQuadrantSample(4*(0.5-xi0),xi1,e,&phi,&cosTheta),phi=$PI-phi;
  } else if(xi0<0.75){
    blinnNormalFirstQuadrantSample(4*(xi0-0.5),xi1,e,&phi,&cosTheta),phi+=$PI;
  } else {
    blinnNormalFirstQuadrantSample(4*(1-xi0),xi1,e,&phi,&cosTheta),phi=$TWO_PI-phi;
  }
  return float3(#sqrt(1-cosTheta*cosTheta+EPSILON)*float2(#cos(phi),#sin(phi)),cosTheta);
}
export tag Shadowing;
export struct ShadowingSmith:default Shadowing{};
export struct ShadowingVCavities:Shadowing{};
@(foreign pure)
double lgamma(double x);
@(pure)
export double beta(const double x,const double y)=#exp(lgamma(x)+lgamma(y)-lgamma(x+y));
}
struct microfacet_bsdf:bsdf{
  const float2 roughness;
  const float roughness0=#sqrt(#prod(roughness));
  const float2 alpha=clamp(roughness*roughness,EPSILON,1.0);
  $(color|float) tint;
  $(?(color|float)) multiscatter_tint=none;
  float3 tangent_u=$state.texture_tangent_u[0];
  const scatter_mode mode=scatter_reflect;
  const microfacet::Distribution distribution=microfacet::Distribution();
  const microfacet::Shadowing shadowing=microfacet::Shadowing();
  const int df_flags=int(mode)|DF_GLOSSY;
};
@(pure noinline)
auto scatterEvaluate(const &microfacet_bsdf this,inline const &ScatterEvaluateParameters params){
  auto reflectChance(scatterReflectChance(this.mode));
  auto effectiveMode(this.mode&mode);
  preserve tangent_u;
  tangent_u=this.tangent_u;
  return ScatterEvaluateResult(isBlack: true) if(!recalculateTangentSpace(params)||effectiveMode==scatter_none);
  preserve wi,mode;
  if(thin_walled&&effectiveMode==scatter_transmit){
    reflectChance=1-reflectChance;
    effectiveMode=scatter_reflect;
    mode=scatter_reflect;
    wi.z=-wi.z;
  }
  const auto cosThetao(#abs(wo.z));
  const auto cosThetai(#abs(wi.z));
  const auto wm(halfDirection(params));
  const auto dotWoWm(#sum(wo*wm));
  const auto dotWiWm(#sum(wi*wm));
  if$(this.distribution<:microfacet::DistributionBlinn){
    const auto e(2/(this.alpha*this.alpha+EPSILON));
    const auto D(#pow(wm.z,(e.x*wm.x*wm.x+e.y*wm.y*wm.y)/(1-wm.z*wm.z+EPSILON))/$TWO_PI);
    const auto norm1(#sqrt(#prod(1+e)));
    const auto norm2(#sqrt(#prod(2+e)));
    const auto G(#min(1,2*wm.z*#min(#abs(cosThetao/dotWoWm),#abs(cosThetai/dotWiWm))));
    if(effectiveMode==scatter_reflect){
      const auto pdf(norm1*D/(4*float2(dotWoWm,dotWiWm)+EPSILON));
      const auto f(norm2*D*G/(4*cosThetao+EPSILON));
      auto result(ScatterEvaluateResultWithMultiscatter(this,f,pdf,cosThetao,cosThetai,this.roughness0,"simple_glossy_bsdf"));
      result.f*=shadingNormalCorrection if(isImportance);
      result.f*=reflectChance;
      result.pdf*=reflectChance;
      return result;
    } else {
      return ScatterEvaluateResult(isBlack: true) if(!((dotWoWm>0)&(dotWiWm<0)));
      const auto jac(float2(specular::refractionHalfVectorJacobian(wo,wi,ior),specular::refractionHalfVectorJacobian(wi,wo,1/ior)));
      const auto pdf(norm1*D*jac);
      const auto f(norm2*D*G*jac[0]*dotWoWm/(cosThetao+EPSILON));
      auto result(ScatterEvaluateResult(f: this.tint*f,pdf: pdf));
      result.f*=shadingNormalCorrection if(isImportance);
      result.f*=ior*ior if(!isImportance);
      result.f*=1-reflectChance;
      result.pdf*=1-reflectChance;
      return result;
    }
  } else {
    const auto D(microfacet::smithNormalPDF(this.distribution,this.alpha,wm));
    const auto lambdao(microfacet::smithLambda(this.distribution,cosThetao/length(this.alpha*wo.xy)));
    const auto lambdai(microfacet::smithLambda(this.distribution,cosThetai/length(this.alpha*wi.xy)));
    const auto projAreao((1+lambdao)*cosThetao);
    const auto projAreai((1+lambdai)*cosThetai);
    const auto G=return_from{
      if$(this.shadowing<:microfacet::ShadowingSmith){
        return effectiveMode==scatter_reflect?float(1/(1+lambdao+lambdai)):float(microfacet::beta(1+lambdao,1+lambdai));
      } else {
        return #min(1,2*wm.z*#min(#abs(cosThetao/dotWoWm),#abs(cosThetai/dotWiWm)));
      }
    };
    if(effectiveMode==scatter_reflect){
      const auto lutName(this.distribution<:microfacet::DistributionGGX?"microfacet_ggx_smith_bsdf":"microfacet_beckmann_smith_bsdf");
      const auto pdf(D/(4*float2(projAreao,projAreai)+EPSILON));
      const auto f(D*G/(4*cosThetao+EPSILON));
      auto result(ScatterEvaluateResultWithMultiscatter(this,f,pdf,cosThetao,cosThetai,this.roughness0,lutName));
      result.f*=shadingNormalCorrection if(isImportance);
      result.f*=reflectChance;
      result.pdf*=reflectChance;
      return result;
    } else {
      return ScatterEvaluateResult(isBlack: true) if(!((dotWoWm>0)&(dotWiWm<0)));
      const auto jac(float2(specular::refractionHalfVectorJacobian(wo,wi,ior),specular::refractionHalfVectorJacobian(wi,wo,1/ior)));
      const auto pdf(D*jac*float2(dotWoWm,-dotWiWm)/(float2(projAreao,projAreai)+EPSILON));
      const auto f(D*G*jac[0]*dotWoWm/(cosThetao+EPSILON));
      auto result(ScatterEvaluateResult(f: this.tint*f,pdf: pdf));
      result.f*=shadingNormalCorrection if(isImportance);
      result.f*=ior*ior if(!isImportance);
      result.f*=1-reflectChance;
      result.pdf*=1-reflectChance;
      return result;
    }
  }
  return ScatterEvaluateResult(isBlack: true);
}
@(pure noinline)
auto scatterSample(const &microfacet_bsdf this,inline const &ScatterSampleParameters params){
  preserve tangent_u;
  tangent_u=this.tangent_u;
  auto tbn(recalculateTangentSpace(params));
  if(!tbn)
    return ScatterSampleResult();
  const auto mode(monte_carlo::boolSample(&xi.z,scatterReflectChance(this.mode))?scatter_reflect:scatter_transmit);
  if(mode==scatter_reflect||thin_walled){
    if(result:=ScatterSampleResultWithMultiscatter(this,&xi,*tbn)){
      if(mode==scatter_transmit)
        return ScatterSampleResult(wi: result.wi*float3(1,1,-1),mode: mode);
      return *result;
    }
  }
  const auto wm=return_from{
    if$(this.distribution<:microfacet::DistributionBlinn){
      return microfacet::blinnNormalSample(xi.x,xi.y,2/(this.alpha*this.alpha+EPSILON));
    } else {
      return microfacet::smithVisibleNormalSample(this.distribution,xi.x,xi.y,this.alpha,wo);
    }
  };
  const auto wi=return_from{
    if(mode==scatter_reflect){
      return specular::reflect(wo,wm);
    } else {
      if(thin_walled){
        return specular::reflect(wo,wm)*float3(1,1,-1);
      } else {
        return specular::refract(wo,wm,ior);
      }
    }
  };
  return ScatterSampleResult(wi: normalize((*tbn)*wi),mode: mode);
}
@(macro)
auto makeMicrofacetBSDF(
  const float roughness_u,
  const float roughness_v=roughness_u,
  const $(color|float) tint=1.0,
  const $(?(color|float)) multiscatter_tint=none,
  const float3 tangent_u=$state.texture_tangent_u[0],
  const scatter_mode mode=scatter_reflect,
  const string handle=""[[anno::unused()]],
  const microfacet::Distribution distribution=microfacet::Distribution(),
  const microfacet::Shadowing shadowing=microfacet::Shadowing(),
){
  if((roughness_u>0)|(roughness_v>0)){
    return microfacet_bsdf(
             roughness: float2(roughness_u,roughness_v),
             tint: tint,
             multiscatter_tint: multiscatter_tint,
             tangent_u: normalize(tangent_u),
             mode: mode,
             distribution: distribution,
             shadowing: shadowing,
           );
  } else {
    return specular_bsdf(tint: tint,mode: mode);
  }
}
export auto simple_glossy_bsdf(*)=makeMicrofacetBSDF(distribution: microfacet::DistributionBlinn(),shadowing: microfacet::ShadowingVCavities());
export auto microfacet_ggx_smith_bsdf(*)=makeMicrofacetBSDF(distribution: microfacet::DistributionGGX(),shadowing: microfacet::ShadowingSmith());
export auto microfacet_ggx_vcavities_bsdf(*)=makeMicrofacetBSDF(distribution: microfacet::DistributionGGX(),shadowing: microfacet::ShadowingVCavities());
export auto microfacet_beckmann_smith_bsdf(*)=makeMicrofacetBSDF(distribution: microfacet::DistributionBeckmann(),shadowing: microfacet::ShadowingSmith());
export auto microfacet_beckmann_vcavities_bsdf(*)=makeMicrofacetBSDF(distribution: microfacet::DistributionBeckmann(),shadowing: microfacet::ShadowingVCavities());
export bool bsdf_measurement_isvalid(const bsdf_measurement measurement)=bool(measurement.buffer);
@(pure foreign)
void smdBSDFMeasurementInterpolate(&void measurement,&float3 wo,&float3 wi,&float3 result);
@(pure foreign)
float smdBSDFMeasurementDirectionPDF(&void measurement,&float3 wo,&float3 wi);
@(pure foreign)
void smdBSDFMeasurementDirectionSample(&void measurement,&float2 xi,&float3 wo,&float3 wi,&float pdf);
export struct measured_bsdf:bsdf{
  bsdf_measurement measurement;
  float multiplier=1.0;
  scatter_mode mode=scatter_reflect;
  string handle="";
  const int df_flags=(int(mode)&measurement.mode)|DF_GLOSSY;
};
@(macro)
auto scatterEvaluate(const &measured_bsdf this,inline const &ScatterEvaluateParameters params){
  const auto enabledMode(int(this.mode)&this.measurement.mode);
  return ScatterEvaluateResult(isBlack: true) if(!bool(this.measurement.ptr)||(int(mode)&enabledMode)==0);
  return ScatterEvaluateResult(isBlack: true) if(!recalculateTangentSpace(params));
  auto wiUpper(float3(wi.x,wi.y,#abs(wi.z)));
  auto f3(float3(0.0));
  smdBSDFMeasurementInterpolate(this.measurement.ptr,&wo,&wiUpper,&f3);
  auto result(ScatterEvaluateResult(f: this.multiplier*#abs(wi.z)*color(f3),pdf: float2(smdBSDFMeasurementDirectionPDF(this.measurement.ptr,&wo,&wiUpper),smdBSDFMeasurementDirectionPDF(this.measurement.ptr,&wiUpper,&wo),),));
  result.f*=shadingNormalCorrection if(isImportance);
  return result;
}
@(macro)
auto scatterSample(const &measured_bsdf this,inline const &ScatterSampleParameters params){
  const auto enabledMode(int(this.mode)&this.measurement.mode);
  return ScatterSampleResult() if(!bool(this.measurement.ptr)||enabledMode==0);
  if((tbn:=recalculateTangentSpace(params))){
    auto xi2(xi.xy);
    auto wiLocal(float3(0.0));
    float pdf=0.0;
    smdBSDFMeasurementDirectionSample(this.measurement.ptr,&xi2,&wo,&wiLocal,&pdf);
    return ScatterSampleResult() if(!(pdf>0.0));
    wiLocal.z=-wiLocal.z if(enabledMode==int(scatter_transmit));
    return ScatterSampleResult(wi: normalize((*tbn)*wiLocal),mode: enabledMode==int(scatter_transmit)?scatter_transmit:scatter_reflect,);
  } else {
    return ScatterSampleResult();
  }
}
struct tint1:bsdf,edf,hair_bsdf{
  $(color|float) tint;
  auto base;
  const int df_flags=base.df_flags;
};
struct tint2:bsdf{
  $(color|float) reflection_tint;
  $(color|float) transmission_tint;
  bsdf base;
  const int df_flags=base.df_flags;
};
@(macro)
export auto tint(const auto tint,const bsdf base)=tint1(tint,base);
@(macro)
export auto tint(const auto tint,const edf base)=tint1(tint,base);
@(macro)
export auto tint(const auto tint,const hair_bsdf base)=tint1(tint,base);
@(macro)
export auto tint(const auto reflection_tint,const auto transmission_tint,const bsdf base)=tint2(reflection_tint,transmission_tint,base);
@(macro)
auto scatterEvaluate(const &tint1 this,const &ScatterEvaluateParameters params){
  auto result(scatterEvaluate(visit &this.base,params));
  if(!result.isBlack)
    result.f*=this.tint;
  return result;
}
@(macro)
auto scatterEvaluate(const &tint2 this,const &ScatterEvaluateParameters params){
  auto result(scatterEvaluate(visit &this.base,params));
  if(!result.isBlack){
    if(params.mode==scatter_reflect){
      result.f*=this.reflection_tint;
    } else {
      result.f*=this.transmission_tint;
    }
  }
  return result;
}
@(macro)
auto scatterSample(const &tint1 this,const &ScatterSampleParameters params){
  auto result(scatterSample(visit &this.base,params));
  if((result.mode!=scatter_none)&bool(result.fDelta))
    *result.fDelta*=this.tint;
  return result;
}
@(macro)
auto scatterSample(const &tint2 this,const &ScatterSampleParameters params){
  auto result(scatterSample(visit &this.base,params));
  if((result.mode!=scatter_none)&bool(result.fDelta)){
    if(params.mode==scatter_reflect){
      *result.fDelta*=this.reflection_tint;
    } else {
      *result.fDelta*=this.transmission_tint;
    }
  }
  return result;
}
export struct weighted_layer:bsdf{
  $(color|float) weight;
  bsdf layer=bsdf();
  bsdf base=bsdf();
  float3 normal=$state.normal;
  float chance=average(weight);
  const int df_flags=layer.df_flags|base.df_flags;
  finalize {
    weight=saturate(weight);
    chance=saturate(chance);
  }
};
@(macro)
auto scatterEvaluate(const &weighted_layer this,inline const &ScatterEvaluateParameters params){
  auto result0(scatterEvaluate(visit &this.base,params));
  preserve normal;
  normal=this.normal;
  auto result1(scatterEvaluate(visit &this.layer,params));
  return ScatterEvaluateResult(f: lerp(result0.f,result1.f,this.weight),pdf: lerp(result0.pdf,result1.pdf,this.chance),isBlack: result0.isBlack&result1.isBlack);
}
@(macro)
auto scatterSample(const &weighted_layer this,inline const &ScatterSampleParameters params){
  if(monte_carlo::boolSample(&xi.w,this.chance)){
    preserve normal;
    normal=this.normal;
    return scatterSample(visit &this.layer,params);
  } else {
    return scatterSample(visit &this.base,params);
  }
}
export typedef weighted_layer color_weighted_layer;
export struct thin_film:bsdf{
  $(color|float) thickness;
  $(color|float) ior;
  bsdf base=bsdf();
  const int df_flags=base.df_flags;
};
@(macro)
auto thinFilmFactor(const auto thickness,const auto filmIOR,const float baseIOR,const float cosTheta1){
  const auto eta2(filmIOR);
  const auto eta3(baseIOR);
  const auto sin2Theta1(#max(1-cosTheta1*cosTheta1,0.0));
  const auto cosTheta2(#sqrt(#max(1-sin2Theta1/(eta2*eta2),0.0)));
  const auto cosTheta3(#sqrt(#max(1-sin2Theta1/(eta3*eta3),0.0)));
  const auto rs12((cosTheta1-eta2*cosTheta2)/(cosTheta1+eta2*cosTheta2));
  const auto rp12((eta2*cosTheta1-cosTheta2)/(eta2*cosTheta1+cosTheta2));
  const auto rs23((eta2*cosTheta2-eta3*cosTheta3)/(eta2*cosTheta2+eta3*cosTheta3));
  const auto rp23((eta3*cosTheta2-eta2*cosTheta3)/(eta3*cosTheta2+eta2*cosTheta3));
  const auto phi(2.0*$TWO_PI*eta2*cosTheta2*thickness/color($state.wavelength_base));
  const auto phase(complex(#cos(phi),#sin(phi)));
  const auto Rs(#norm((rs12+rs23*phase)/(1+rs12*rs23*phase)));
  const auto Rp(#norm((rp12+rp23*phase)/(1+rp12*rp23*phase)));
  const auto rs13((cosTheta1-eta3*cosTheta3)/(cosTheta1+eta3*cosTheta3));
  const auto rp13((eta3*cosTheta1-cosTheta3)/(eta3*cosTheta1+cosTheta3));
  const float R13(0.5*(rs13*rs13+rp13*rp13));
  return R13>EPSILON?0.5*(Rs+Rp)/R13:color(1.0);
}
@(macro)
auto scatterEvaluate(const &thin_film this,const &ScatterEvaluateParameters params){
  auto result(scatterEvaluate(visit &this.base,params));
  if(!result.isBlack&&params.mode==scatter_reflect){
    return ScatterEvaluateResult(f: thinFilmFactor(this.thickness,this.ior,1/params.ior,#abs(dot(params.wo,halfDirection(params))))*result.f,pdf: result.pdf,);
  } else {
    return result;
  }
}
@(macro)
auto scatterSample(const &thin_film this,const &ScatterSampleParameters params){
  auto result(scatterSample(visit &this.base,params));
  if((result.mode==scatter_reflect)&bool(result.fDelta)){
    *result.fDelta*=thinFilmFactor(this.thickness,this.ior,1/params.ior,#abs(dot(params.wo,halfDirection(params,&result))));
  }
  return result;
}
export struct fresnel_factor:bsdf{
  $(color|float) ior;
  $(color|float) extinction_coefficient;
  bsdf base=bsdf();
  const int df_flags=base.df_flags;
};
@(macro)
auto scatterEvaluate(const &fresnel_factor this,const &ScatterEvaluateParameters params){
  auto result(scatterEvaluate(visit &this.base,params));
  if(!result.isBlack&&params.mode==scatter_reflect){
    return ScatterEvaluateResult(f: specular::conductorFresnel(#abs(dot(params.wo,halfDirection(params))),1.0/complex(this.ior,this.extinction_coefficient))*result.f,pdf: result.pdf,);
  }
  return result;
}
@(macro)
auto scatterSample(const &fresnel_factor this,const &ScatterSampleParameters params){
  auto result(scatterSample(visit &this.base,params));
  if((result.mode==scatter_reflect)&bool(result.fDelta)){
    *result.fDelta*=specular::conductorFresnel(#abs(dot(params.wo,halfDirection(params,&result))),1.0/complex(this.ior,this.extinction_coefficient));
  }
  return result;
}
export struct directional_factor:bsdf{
  $(color|float) normal_tint=1.0;
  $(color|float) grazing_tint=1.0;
  float exponent=5.0;
  bsdf base=bsdf();
  const int df_flags=base.df_flags;
};
@(macro)
auto scatterEvaluate(const &directional_factor this,const &ScatterEvaluateParameters params){
  auto result(scatterEvaluate(visit &this.base,params));
  if(!result.isBlack&&params.mode==scatter_reflect){
    return ScatterEvaluateResult(f: specular::schlickFresnel(dot(params.wo,halfDirection(params)),this.normal_tint,this.grazing_tint,this.exponent)*result.f,pdf: result.pdf);
  } else {
    return result;
  }
}
@(macro)
auto scatterSample(const &directional_factor this,const &ScatterSampleParameters params){
  auto result(scatterSample(visit &this.base,params));
  if((result.mode==scatter_reflect)&bool(result.fDelta)){
    *result.fDelta*=specular::schlickFresnel(dot(params.wo,halfDirection(params,&result)),this.normal_tint,this.grazing_tint,this.exponent);
  }
  return result;
}
export struct measured_curve_factor:bsdf{
  color[] curve_values;
  bsdf base=bsdf();
  const int df_flags=base.df_flags;
};
@(pure macro)
color evaluateMeasuredCurve(const color[<N>] curve_values,const float cosAlpha){
  const auto t(saturate(#acos(saturate(#abs(cosAlpha)))*(2/$PI))*(N-1));
  const int i(int(t));
  return saturate(lerp(curve_values[i],curve_values[#min(i+1,N-1)],t-i));
}
@(macro)
auto scatterEvaluate(const &measured_curve_factor this,const &ScatterEvaluateParameters params){
  auto result(scatterEvaluate(visit &this.base,params));
  if(!result.isBlack&&params.mode==scatter_reflect){
    return ScatterEvaluateResult(f: evaluateMeasuredCurve(this.curve_values,dot(params.wo,halfDirection(params)))*result.f,pdf: result.pdf);
  } else {
    return result;
  }
}
@(macro)
auto scatterSample(const &measured_curve_factor this,const &ScatterSampleParameters params){
  auto result(scatterSample(visit &this.base,params));
  if((result.mode==scatter_reflect)&bool(result.fDelta)){
    *result.fDelta*=evaluateMeasuredCurve(this.curve_values,dot(params.wo,halfDirection(params,&result)));
  }
  return result;
}
export struct measured_factor:bsdf{
  texture_2d values;
  bsdf base=bsdf();
  const int df_flags=base.df_flags;
};
@(pure macro)
color evaluateMeasuredFactor(const &measured_factor this,const float cosAlpha,const float cosBeta){
  return saturate(tex::lookup_color(
                    this.values,
                    float2(#acos(saturate(#abs(cosAlpha))),#acos(saturate(cosBeta)))*(2/$PI),
                    tex::wrap_clamp,
                    tex::wrap_clamp,
                  ),);
}
@(macro)
auto scatterEvaluate(const &measured_factor this,const &ScatterEvaluateParameters params){
  auto result(scatterEvaluate(visit &this.base,params));
  if(!result.isBlack&&params.mode==scatter_reflect){
    const auto h(halfDirection(params));
    return ScatterEvaluateResult(f: evaluateMeasuredFactor(this,dot(params.wo,h),h.z)*result.f,pdf: result.pdf);
  } else {
    return result;
  }
}
@(macro)
auto scatterSample(const &measured_factor this,const &ScatterSampleParameters params){
  auto result(scatterSample(visit &this.base,params));
  if((result.mode==scatter_reflect)&bool(result.fDelta)){
    const auto h(halfDirection(params,&result));
    *result.fDelta*=evaluateMeasuredFactor(this,dot(params.wo,h),h.z);
  }
  return result;
}
export struct fresnel_layer:bsdf{
  $(color|float) ior;
  $(color|float) weight=1.0;
  bsdf layer=bsdf();
  bsdf base=bsdf();
  float3 normal=$state.normal;
  const float _averageIOR=average(ior);
  const float _averageWeight=average(weight);
  const int df_flags=layer.df_flags|base.df_flags;
};
@(macro)
auto scatterEvaluate(const &fresnel_layer this,inline const &ScatterEvaluateParameters params){
  const auto cosThetao(dot(wo,this.normal)*#sign(this.normal.z));
  const auto cosThetai(dot(wi,this.normal)*#sign(this.normal.z));
  if((cosThetao<EPSILON)|((mode==scatter_reflect)&(cosThetai<EPSILON))|((mode==scatter_transmit)&(cosThetai>-EPSILON)))
    return ScatterEvaluateResult(isBlack: true);
  const auto result0(scatterEvaluate(visit &this.base,params));
  const auto result1=return_from{
    preserve normal,ior;
    normal=this.normal,ior=1.0/this._averageIOR;
    return scatterEvaluate(visit &this.layer,params);
  };
  if(result0.isBlack&result1.isBlack){
    return ScatterEvaluateResult(isBlack: true);
  } else {
    return ScatterEvaluateResult(f: lerp(result0.f,result1.f,this.weight*specular::dielectricFresnel(dot(wo,halfDirection(params)),1/this.ior),),pdf: lerp(result0.pdf,result1.pdf,this._averageWeight*specular::schlickFresnel(float2(cosThetao,cosThetai),specular::schlickF0(this._averageIOR)),),);
  }
}
@(macro)
auto scatterSample(const &fresnel_layer this,inline const &ScatterSampleParameters params){
  const auto cosTheta(dot(wo,this.normal)*#sign(this.normal.z));
  if(cosTheta<EPSILON)
    return ScatterSampleResult();
  const auto chance(this._averageWeight*specular::schlickFresnel(cosTheta,specular::schlickF0(this._averageIOR)));
  if(monte_carlo::boolSample(&xi.z,chance)){
    preserve normal,ior;
    normal=this.normal,ior=1.0/this._averageIOR;
    auto result(scatterSample(visit &this.layer,params));
    *result.fDelta*=this.weight*specular::dielectricFresnel(dot(wo,halfDirection(params,&result)),1/this.ior)/chance if(result.fDelta);
    return result;
  } else {
    auto result(scatterSample(visit &this.base,params));
    *result.fDelta*=(1-this.weight*specular::dielectricFresnel(dot(wo,halfDirection(params,&result)),1/this.ior))/(1-chance) if(result.fDelta);
    return result;
  }
}
export typedef fresnel_layer color_fresnel_layer;
export struct custom_curve_layer:bsdf{
  $(color|float) normal_reflectivity;
  $(color|float) grazing_reflectivity=1.0;
  float exponent=5.0;
  $(color|float) weight=1.0;
  bsdf layer=bsdf();
  bsdf base=bsdf();
  float3 normal=$state.normal;
  const float _averageNormalReflectivity=average(normal_reflectivity);
  const float _averageGrazingReflectivity=average(grazing_reflectivity);
  const float _averageWeight=average(weight);
  const int df_flags=layer.df_flags|base.df_flags;
};
@(macro)
auto scatterEvaluate(const &custom_curve_layer this,inline const &ScatterEvaluateParameters params){
  const auto cosThetao(dot(wo,this.normal)*#sign(this.normal.z));
  const auto cosThetai(dot(wi,this.normal)*#sign(this.normal.z));
  if((cosThetao<EPSILON)|((mode==scatter_reflect)&(cosThetai<EPSILON))|((mode==scatter_transmit)&(cosThetai>-EPSILON)))
    return ScatterEvaluateResult(isBlack: true);
  const auto result0(scatterEvaluate(visit &this.base,params));
  const auto result1=return_from{
    preserve normal;
    normal=this.normal;
    return scatterEvaluate(visit &this.layer,params);
  };
  if(result0.isBlack&result1.isBlack){
    return ScatterEvaluateResult(isBlack: true);
  } else {
    return ScatterEvaluateResult(f: lerp(result0.f,result1.f,this.weight*specular::schlickFresnel(dot(wo,halfDirection(params)),this.normal_reflectivity,this.grazing_reflectivity,this.exponent),),pdf: lerp(result0.pdf,result1.pdf,this._averageWeight*specular::schlickFresnel(float2(cosThetao,cosThetai),this._averageNormalReflectivity,this._averageGrazingReflectivity,this.exponent),),);
  }
}
@(macro)
auto scatterSample(const &custom_curve_layer this,inline const &ScatterSampleParameters params){
  const auto cosTheta(dot(wo,this.normal)*#sign(this.normal.z));
  if(cosTheta<EPSILON)
    return ScatterSampleResult();
  const auto chance(this._averageWeight*specular::schlickFresnel(cosTheta,this._averageNormalReflectivity,this._averageGrazingReflectivity,this.exponent));
  if(monte_carlo::boolSample(&xi.z,chance)){
    preserve normal;
    normal=this.normal;
    auto result(scatterSample(visit &this.layer,params));
    *result.fDelta*=this.weight*specular::schlickFresnel(dot(wo,halfDirection(params,&result)),this.normal_reflectivity,this.grazing_reflectivity,this.exponent)/chance if(result.fDelta);
    return result;
  } else {
    auto result(scatterSample(visit &this.base,params));
    *result.fDelta*=(1-this.weight*specular::schlickFresnel(dot(wo,halfDirection(params,&result)),this.normal_reflectivity,this.grazing_reflectivity,this.exponent))/(1-chance) if(result.fDelta);
    return result;
  }
}
export typedef custom_curve_layer color_custom_curve_layer;
export struct measured_curve_layer:bsdf{
  color[] curve_values;
  $(color|float) weight=1.0;
  bsdf layer=bsdf();
  bsdf base=bsdf();
  float3 normal=$state.normal;
  const float _averageWeight=average(weight);
  const int df_flags=layer.df_flags|base.df_flags;
};
@(macro)
auto scatterEvaluate(const &measured_curve_layer this,inline const &ScatterEvaluateParameters params){
  const auto cosThetao(dot(wo,this.normal)*#sign(this.normal.z));
  const auto cosThetai(dot(wi,this.normal)*#sign(this.normal.z));
  if((cosThetao<EPSILON)|((mode==scatter_reflect)&(cosThetai<EPSILON))|((mode==scatter_transmit)&(cosThetai>-EPSILON)))
    return ScatterEvaluateResult(isBlack: true);
  const auto result0(scatterEvaluate(visit &this.base,params));
  const auto result1=return_from{
    preserve normal;
    normal=this.normal;
    return scatterEvaluate(visit &this.layer,params);
  };
  if(result0.isBlack&result1.isBlack){
    return ScatterEvaluateResult(isBlack: true);
  } else {
    return ScatterEvaluateResult(f: lerp(result0.f,result1.f,this.weight*evaluateMeasuredCurve(this.curve_values,dot(wo,halfDirection(params))),),pdf: lerp(result0.pdf,result1.pdf,this._averageWeight*float2(average(evaluateMeasuredCurve(this.curve_values,cosThetao)),average(evaluateMeasuredCurve(this.curve_values,cosThetai)),),),);
  }
}
@(macro)
auto scatterSample(const &measured_curve_layer this,inline const &ScatterSampleParameters params){
  const auto cosTheta(dot(wo,this.normal)*#sign(this.normal.z));
  if(cosTheta<EPSILON)
    return ScatterSampleResult();
  const auto chance(this._averageWeight*average(evaluateMeasuredCurve(this.curve_values,cosTheta)));
  if(monte_carlo::boolSample(&xi.z,chance)){
    preserve normal;
    normal=this.normal;
    auto result(scatterSample(visit &this.layer,params));
    *result.fDelta*=this.weight*evaluateMeasuredCurve(this.curve_values,dot(wo,halfDirection(params,&result)))/chance if(result.fDelta);
    return result;
  } else {
    auto result(scatterSample(visit &this.base,params));
    *result.fDelta*=(1-this.weight*evaluateMeasuredCurve(this.curve_values,dot(wo,halfDirection(params,&result))))/(1-chance) if(result.fDelta);
    return result;
  }
}
export typedef measured_curve_layer color_measured_curve_layer;
tag component;
export struct bsdf_component:component{
  float weight=0.0;
  bsdf component=bsdf();
  float chance=weight;
};
export struct edf_component:component{
  float weight=0.0;
  edf component=edf();
  float chance=weight;
};
export struct vdf_component:component{
  float weight=0.0;
  vdf component=vdf();
  float chance=weight;
};
struct component_mix:bsdf,edf,vdf{
  component[] components;
  int df_flags=0;
};
@(macro)
export auto normalized_mix(component[<N>] components){
  int df_flags(0);
  float total_weight(0);
  float total_chance(0);
  for(int i=0;i<N;i++){
    auto component(&components[i]);
    component.weight=#max(component.weight,0.0);
    component.chance=#max(component.chance,0.0);
    total_weight+=component.weight;
    total_chance+=component.chance;
    df_flags|=component.component.df_flags;
  }
  if(total_weight>1.0)
    total_weight=1.0/total_weight;
  else
    total_weight=1.0;
  total_chance=1.0/total_chance if(total_chance>0.0);
  for(int i=0;i<N;i++){
    auto component(&components[i]);
    component.weight*=total_weight;
    component.chance*=total_chance;
  }
  return component_mix(components,df_flags);
}
@(macro)
export auto clamped_mix(component[<N>] components){
  int df_flags(0);
  float total_weight(0);
  float total_chance(0);
  for(int i=0;i<N;i++){
    auto component(&components[i]);
    component.weight=#max(component.weight,0.0);
    component.chance=#max(component.chance,0.0);
    if(total_weight+component.weight<1.0){
      total_weight+=component.weight;
      total_chance+=component.chance;
      df_flags|=component.component.df_flags;
    } else {
      component.weight=1.0-total_weight;
      for(int j=i+1;j<N;j++){
        components[j].weight=0;
        components[j].chance=0;
      }
      break;
    }
  }
  total_chance=1.0/total_chance if(total_chance>0.0);
  for(int i=0;i<N;i++){
    components[i].chance*=total_chance;
  }
  return component_mix(components,df_flags);
}
@(macro)
export auto unbounded_mix(component[<N>] components){
  int df_flags(0);
  float total_chance(0);
  for(int i=0;i<N;i++){
    auto component(&components[i]);
    component.weight=#max(component.weight,0.0);
    component.chance=#max(component.chance,0.0);
    total_chance+=component.chance;
    df_flags|=component.df_flags;
  }
  total_chance=1.0/total_chance if(total_chance>0.0);
  for(int i=0;i<N;i++){
    components[i].chance*=total_chance;
  }
  return component_mix(components,df_flags);
}
@(macro)
auto scatterEvaluate(const &component_mix this,const &ScatterEvaluateParameters params){
  auto result(ScatterEvaluateResult(f: color(0),isBlack: true));
  for(int i=0;i<#num(this.components);i++){
    visit component in this.components[i]{
      auto component_result(scatterEvaluate(visit &component.component,params));
      if(!component_result.isBlack){
        result.pdf+=component.chance*component_result.pdf;
        result.f+=component.weight*component_result.f;
        result.isBlack=false;
      }
    }
  }
  return result;
}
@(macro)
auto scatterSample(const &component_mix this,const &ScatterSampleParameters params){
  const auto xi(&params.xi.z);
  for(int i=0;i<#num(this.components);i++){
    visit component in this.components[i]{
      if(!(*xi<component.chance)){
        *xi-=component.chance;
      } else {
        *xi/=component.chance;
        auto result(scatterSample(visit &component.component,params));
        if((result.mode!=scatter_none)&bool(result.fDelta))
          *result.fDelta*=component.weight;
        return result;
      }
    }
  }
  return ScatterSampleResult();
}
export struct anisotropic_vdf:vdf{
  float directional_bias=0.0;
  void handle="";
  static const int df_flags=0;
  finalize {
    directional_bias=#max(directional_bias,-0.999);
    directional_bias=#min(directional_bias,0.999);
  }
};
@(macro)
auto scatterEvaluate(const &anisotropic_vdf this,inline const &ScatterEvaluateParameters params){
  const auto cosTheta=dot(wo,wi);
  const auto g=this.directional_bias;
  const auto p=(1.0-g*g)/(4.0*$PI*(denom:=1.0+g*g+2.0*g*cosTheta)*#sqrt(denom));
  return ScatterEvaluateResult(f: p,pdf: float2(p));
}
@(macro)
auto scatterSample(const &anisotropic_vdf this,inline const &ScatterSampleParameters params){
  const auto g=this.directional_bias;
  const auto cosTheta=#abs(g)<0.001?1.0-2.0*xi.x:-(1.0+g*g-#pow((1.0-g*g)/(1.0+g*(1.0-2.0*xi.x)),2))/(2.0*g);
  const auto sinTheta=#sqrt(#max(0.0,1.0-cosTheta*cosTheta));
  const auto phi=2.0*$PI*xi.y;
  return ScatterSampleResult(wi: orthonormalBasis(wo)*float3(sinTheta*#cos(phi),sinTheta*#sin(phi),cosTheta),mode: scatter_reflect_transmit);
}
@(macro)
export int _scatterEvaluate(
  const &_MaterialInstance instance,
  const &float3 woWorld,
  const &float3 wiWorld,
  const &float pdfFwd,
  const &float pdfRev,
  const &float f,
){
  auto params=ScatterEvaluateParameters(
    isImportance: (instance.flags&1)!=0,
    ior: 1.0/instance.ior,
    wo0: normalize((*woWorld)*instance.tangent_to_world),
    wi0: normalize((*wiWorld)*instance.tangent_to_world),
    normal: normalize(instance.geometry.normal),
    thin_walled: instance.ptr.thin_walled,
  );
  auto result=#is_default(instance.ptr.backface)||!params.hitBackface?scatterEvaluate(visit &instance.ptr.surface.scattering,&params):scatterEvaluate(visit &instance.ptr.backface.scattering,&params);
  visit result in result{
    if(result.isBlack){
      *pdfFwd=0.0;
      *pdfRev=0.0;
      for(int i=0;i<$WAVELENGTH_BASE_MAX;i++)
        f[i]=0.0;
    } else {
      *pdfFwd=result.pdf[0];
      *pdfRev=result.pdf[1];
      if(#typeof(result.f)==float){
        for(int i=0;i<$WAVELENGTH_BASE_MAX;i++)
          f[i]=result.f;
      } else {
        #memcpy(f,&result.f,#sizeof(float)*$WAVELENGTH_BASE_MAX);
      }
    }
    return !result.isBlack;
  }
}
@(macro)
export int _scatterSample(
  const &_MaterialInstance instance,
  const &float4 xi,
  const &float3 woWorld,
  const &float3 wiWorld,
  const &float pdfFwd,
  const &float pdfRev,
  const &float f,
  const &int isDelta,
){
  auto wo=normalize((*woWorld)*instance.tangent_to_world);
  auto params=ScatterSampleParameters(
    isImportance: (instance.flags&1)!=0,
    xi: *xi,
    wo0: wo,
    ior: 1.0/instance.ior,
    normal: normalize(instance.geometry.normal),
    thin_walled: instance.ptr.thin_walled,
  );
  auto result=#is_default(instance.ptr.backface)||!params.hitBackface?scatterSample(visit &instance.ptr.surface.scattering,&params):scatterSample(visit &instance.ptr.backface.scattering,&params);
  visit result in result{
    const auto wi=#select(params.hitBackface,-result.wi,result.wi);
    if(result.mode==scatter_none||((wo.z<0.0)==(wi.z<0.0))!=(result.mode==scatter_reflect)){
      *pdfFwd=0.0;
      *pdfRev=0.0;
      for(int i=0;i<$WAVELENGTH_BASE_MAX;i++)
        f[i]=0.0;
      return false;
    }
    *wiWorld=normalize(instance.tangent_to_world*wi);
    if((*isDelta=bool(result.fDelta))){
      *pdfFwd=1.0;
      *pdfRev=1.0;
      #memcpy(f,&*result.fDelta,#sizeof(float)*$WAVELENGTH_BASE_MAX);
      return true;
    } else {
      return _scatterEvaluate(instance,woWorld,wiWorld,pdfFwd,pdfRev,f);
    }
  }
}
@(macro)
export float _volumeScatterEvaluate(const &_MaterialInstance instance,const &float3 woWorld,const &float3 wiWorld,){
  auto params=ScatterEvaluateParameters(
    isImportance: 0,
    wo0: normalize(*woWorld),
    wi0: normalize(*wiWorld),
    hitBackface: false,
  );
  return scatterEvaluate(visit &instance.ptr.volume.scattering,&params).f;
}
@(macro)
export float _volumeScatterSample(
  const &_MaterialInstance instance,
  const &float4 xi,
  const &float3 woWorld,
  const &float3 wiWorld,
){
  auto wo=normalize(*woWorld);
  auto params=ScatterSampleParameters(xi: *xi,wo0: wo,hitBackface: false);
  auto result=scatterSample(visit &instance.ptr.volume.scattering,&params);
  if(result.mode==scatter_none){
    return 0.0;
  }
  *wiWorld=normalize(result.wi);
  return _volumeScatterEvaluate(instance,woWorld,wiWorld);
}
)*";

static const char *const io = R"*(#smdl
export typedef &void FILE;
export const auto stdin=cast<FILE>($stdin);
export const auto stdout=cast<FILE>($stdout);
export const auto stderr=cast<FILE>($stderr);
@(foreign pure)
export FILE fopen(string filename,string mode);
@(foreign pure)
export void fclose(FILE file);
@(foreign pure)
export void fflush(FILE file);
@(foreign pure)
export int feof(FILE file);
@(foreign pure)
export int ferror(FILE file);
@(foreign pure)
export int fgetc(FILE file);
@(foreign pure)
export &char fgets(&char str,int count,FILE file);
@(foreign pure)
export int fputc(int ch,FILE file);
@(foreign pure)
export int fputs(string str,FILE file);
@(foreign pure)
export size_t fread(&void buffer,size_t size,size_t count,FILE file);
@(foreign pure)
export size_t fwrite(&void buffer,size_t size,size_t count,FILE file);
@(foreign pure)
export int fscanf(FILE file,string format,...);
@(foreign pure)
export int fprintf(FILE file,string format,...);
@(foreign pure)
export long ftell(FILE file);
export const int SEEK_SET=$SEEK_SET;
export const int SEEK_CUR=$SEEK_CUR;
export const int SEEK_END=$SEEK_END;
@(foreign pure)
export int fseek(FILE file,long offset,int origin);
@(foreign pure)
export void rewind(FILE file);
@(foreign pure)
export void clearerr(FILE file);
@(foreign pure)
export void perror(string message="");
@(foreign pure)
export FILE tmpfile();
)*";

static const char *const limits = R"*(#smdl
export const int INT_MIN=$INT_MIN;
export const int INT_MAX=$INT_MAX;
export const float FLOAT_MIN=$FLOAT_MIN;
export const float FLOAT_MAX=$FLOAT_MAX;
export const double DOUBLE_MIN=$DOUBLE_MIN;
export const double DOUBLE_MAX=$DOUBLE_MAX;
)*";

static const char *const pcg32 = R"*(#smdl
const int64_t PCG32_MULTIPLIER=6364136223846793005;
const int64_t PCG32_DEFAULT_INCREMENT=1442695040888963407;
export struct pcg32{
  pcg32(int64_t seed)=return_from{
    auto pcg(pcg32(state: seed));
    pcg.state=pcg.state+pcg.increment;
    pcg.state=pcg.state*PCG32_MULTIPLIER+pcg.increment;
    return pcg;
  };
  pcg32(int64_t seed,int64_t stream)=return_from{
    auto pcg(pcg32(state: seed,increment: (stream<<1)|1));
    pcg.state=pcg.state+pcg.increment;
    pcg.state=pcg.state*PCG32_MULTIPLIER+pcg.increment;
    return pcg;
  };
  int64_t state=0;
  int64_t increment=PCG32_DEFAULT_INCREMENT;
};
@(pure)
export int32_t generate_int(inline const &pcg32 this){
  state=state*PCG32_MULTIPLIER+increment;
  return #rotr(int32_t(((state>>>18)^state)>>>27),int32_t(31&(state>>>59)));
}
@(pure)
export int32_t generate_int(const &pcg32 this,const int32_t bound){
  if(bound>1){
    const auto xmin((-bound)%bound);
    while(true){
      const auto x(generate_int(this));
      return x%bound if(x>=xmin);
    }
  }
  return 0;
}
@(pure)
export float generate_float(const &pcg32 this){
  return #min(float(#unsigned_to_fp(generate_int(this),double)/4294967296.0d),1.0-$FLOAT_EPS/2);
}
@(pure)
export float2 generate_float2(const &pcg32 this)=float2(generate_float(this),generate_float(this));
@(pure)
export float3 generate_float3(const &pcg32 this)=float3(generate_float(this),generate_float(this),generate_float(this));
@(pure)
export float4 generate_float4(const &pcg32 this)=float4(generate_float(this),generate_float(this),generate_float(this),generate_float(this));
@(pure)
export void discard(inline const &pcg32 this,int64_t n){
  int64_t aTotal(1);
  int64_t bTotal(0);
  int64_t a(PCG32_MULTIPLIER);
  int64_t b(increment);
  while(n!=0){
    if((n&1)!=0){
      aTotal=aTotal*a;
      bTotal=bTotal*a+b;
    }
    b*=a+1;
    a*=a;
    n>>>=1;
  }
  state=state*aTotal+bTotal;
}
)*";

static const char *const prospect = R"*(#smdl
using ::math import *;
export struct prospect_result{
  color reflectance=color(0);
  color transmittance=color(0);
};
export const float PROSPECT_MIN_WAVELENGTH=400.0;
export const float PROSPECT_MAX_WAVELENGTH=2500.0;
export const int PROSPECT_TABLE_SIZE=526;
export static const auto PROSPECT_TABLE_IORS=float[526](1.5115000e+00,1.5115000e+00,1.5095000e+00,1.5071000e+00,1.5050000e+00,1.5032000e+00,1.5019000e+00,1.5008000e+00,1.4997000e+00,1.4988000e+00,1.4980000e+00,1.4969000e+00,1.4959000e+00,1.4951000e+00,1.4943000e+00,1.4937000e+00,1.4931000e+00,1.4925000e+00,1.4920000e+00,1.4915000e+00,1.4910000e+00,1.4904000e+00,1.4899000e+00,1.4893000e+00,1.4887000e+00,1.4880000e+00,1.4873000e+00,1.4865000e+00,1.4856000e+00,1.4846000e+00,1.4836000e+00,1.4825000e+00,1.4813000e+00,1.4801000e+00,1.4788000e+00,1.4774000e+00,1.4760000e+00,1.4746000e+00,1.4732000e+00,1.4717000e+00,1.4701000e+00,1.4685000e+00,1.4670000e+00,1.4654000e+00,1.4639000e+00,1.4624000e+00,1.4609000e+00,1.4595000e+00,1.4582000e+00,1.4570000e+00,1.4559000e+00,1.4548000e+00,1.4538000e+00,1.4528000e+00,1.4519000e+00,1.4510000e+00,1.4502000e+00,1.4495000e+00,1.4489000e+00,1.4484000e+00,1.4480000e+00,1.4477000e+00,1.4474000e+00,1.4472000e+00,1.4470000e+00,1.4468000e+00,1.4467000e+00,1.4465000e+00,1.4463000e+00,1.4461000e+00,1.4458000e+00,1.4456000e+00,1.4453000e+00,1.4450000e+00,1.4447000e+00,1.4444000e+00,1.4440000e+00,1.4435000e+00,1.4430000e+00,1.4423000e+00,1.4417000e+00,1.4409000e+00,1.4402000e+00,1.4394000e+00,1.4387000e+00,1.4380000e+00,1.4374000e+00,1.4368000e+00,1.4363000e+00,1.4357000e+00,1.4352000e+00,1.4348000e+00,1.4345000e+00,1.4342000e+00,1.4341000e+00,1.4340000e+00,1.4340000e+00,1.4341000e+00,1.4342000e+00,1.4343000e+00,1.4345000e+00,1.4347000e+00,1.4347000e+00,1.4347000e+00,1.4347000e+00,1.4347000e+00,1.4347000e+00,1.4348000e+00,1.4348000e+00,1.4348000e+00,1.4348000e+00,1.4348000e+00,1.4347000e+00,1.4347000e+00,1.4347000e+00,1.4346000e+00,1.4345000e+00,1.4345000e+00,1.4345000e+00,1.4344000e+00,1.4342000e+00,1.4341000e+00,1.4340000e+00,1.4339000e+00,1.4338000e+00,1.4337000e+00,1.4335000e+00,1.4334000e+00,1.4333000e+00,1.4332000e+00,1.4331000e+00,1.4329000e+00,1.4328000e+00,1.4326000e+00,1.4324000e+00,1.4322000e+00,1.4320000e+00,1.4319000e+00,1.4317000e+00,1.4316000e+00,1.4314000e+00,1.4312000e+00,1.4309000e+00,1.4307000e+00,1.4304000e+00,1.4302000e+00,1.4299000e+00,1.4296000e+00,1.4293000e+00,1.4290000e+00,1.4287000e+00,1.4284000e+00,1.4281000e+00,1.4277000e+00,1.4273000e+00,1.4270000e+00,1.4266000e+00,1.4263000e+00,1.4259000e+00,1.4255000e+00,1.4251000e+00,1.4247000e+00,1.4242000e+00,1.4238000e+00,1.4234000e+00,1.4230000e+00,1.4225000e+00,1.4220000e+00,1.4216000e+00,1.4212000e+00,1.4207000e+00,1.4202000e+00,1.4197000e+00,1.4193000e+00,1.4188000e+00,1.4183000e+00,1.4178000e+00,1.4173000e+00,1.4169000e+00,1.4164000e+00,1.4159000e+00,1.4155000e+00,1.4150000e+00,1.4146000e+00,1.4142000e+00,1.4137000e+00,1.4132000e+00,1.4128000e+00,1.4124000e+00,1.4119000e+00,1.4115000e+00,1.4110000e+00,1.4106000e+00,1.4102000e+00,1.4098000e+00,1.4094000e+00,1.4089000e+00,1.4085000e+00,1.4081000e+00,1.4077000e+00,1.4073000e+00,1.4069000e+00,1.4065000e+00,1.4061000e+00,1.4057000e+00,1.4052000e+00,1.4048000e+00,1.4044000e+00,1.4040000e+00,1.4035000e+00,1.4031000e+00,1.4027000e+00,1.4023000e+00,1.4019000e+00,1.4014000e+00,1.4010000e+00,1.4006000e+00,1.4001000e+00,1.3997000e+00,1.3993000e+00,1.3989000e+00,1.3984000e+00,1.3980000e+00,1.3976000e+00,1.3972000e+00,1.3968000e+00,1.3964000e+00,1.3960000e+00,1.3956000e+00,1.3952000e+00,1.3947000e+00,1.3943000e+00,1.3939000e+00,1.3935000e+00,1.3931000e+00,1.3927000e+00,1.3923000e+00,1.3919000e+00,1.3915000e+00,1.3911000e+00,1.3907000e+00,1.3903000e+00,1.3899000e+00,1.3895000e+00,1.3890000e+00,1.3886000e+00,1.3882000e+00,1.3877000e+00,1.3873000e+00,1.3869000e+00,1.3865000e+00,1.3860000e+00,1.3855000e+00,1.3851000e+00,1.3846000e+00,1.3841000e+00,1.3836000e+00,1.3831000e+00,1.3826000e+00,1.3821000e+00,1.3816000e+00,1.3810000e+00,1.3805000e+00,1.3800000e+00,1.3794000e+00,1.3788000e+00,1.3782000e+00,1.3776000e+00,1.3770000e+00,1.3764000e+00,1.3758000e+00,1.3752000e+00,1.3745000e+00,1.3739000e+00,1.3732000e+00,1.3726000e+00,1.3720000e+00,1.3713000e+00,1.3706000e+00,1.3699000e+00,1.3693000e+00,1.3687000e+00,1.3681000e+00,1.3675000e+00,1.3668000e+00,1.3661000e+00,1.3655000e+00,1.3648000e+00,1.3641000e+00,1.3634000e+00,1.3628000e+00,1.3622000e+00,1.3615000e+00,1.3608000e+00,1.3601000e+00,1.3595000e+00,1.3589000e+00,1.3582000e+00,1.3576000e+00,1.3569000e+00,1.3563000e+00,1.3557000e+00,1.3550000e+00,1.3544000e+00,1.3537000e+00,1.3531000e+00,1.3525000e+00,1.3518000e+00,1.3512000e+00,1.3505000e+00,1.3499000e+00,1.3493000e+00,1.3487000e+00,1.3481000e+00,1.3475000e+00,1.3469000e+00,1.3463000e+00,1.3456000e+00,1.3450000e+00,1.3445000e+00,1.3439000e+00,1.3433000e+00,1.3428000e+00,1.3422000e+00,1.3417000e+00,1.3411000e+00,1.3406000e+00,1.3401000e+00,1.3396000e+00,1.3391000e+00,1.3386000e+00,1.3380000e+00,1.3376000e+00,1.3372000e+00,1.3367000e+00,1.3363000e+00,1.3358000e+00,1.3354000e+00,1.3350000e+00,1.3346000e+00,1.3342000e+00,1.3338000e+00,1.3334000e+00,1.3330000e+00,1.3326000e+00,1.3322000e+00,1.3319000e+00,1.3316000e+00,1.3312000e+00,1.3308000e+00,1.3305000e+00,1.3302000e+00,1.3299000e+00,1.3295000e+00,1.3292000e+00,1.3289000e+00,1.3286000e+00,1.3283000e+00,1.3279000e+00,1.3276000e+00,1.3273000e+00,1.3270000e+00,1.3267000e+00,1.3264000e+00,1.3261000e+00,1.3259000e+00,1.3256000e+00,1.3253000e+00,1.3250000e+00,1.3247000e+00,1.3245000e+00,1.3242000e+00,1.3239000e+00,1.3236000e+00,1.3233000e+00,1.3231000e+00,1.3229000e+00,1.3226000e+00,1.3224000e+00,1.3221000e+00,1.3218000e+00,1.3216000e+00,1.3213000e+00,1.3210000e+00,1.3207000e+00,1.3204000e+00,1.3202000e+00,1.3199000e+00,1.3197000e+00,1.3194000e+00,1.3191000e+00,1.3189000e+00,1.3186000e+00,1.3183000e+00,1.3180000e+00,1.3177000e+00,1.3174000e+00,1.3171000e+00,1.3167000e+00,1.3164000e+00,1.3161000e+00,1.3158000e+00,1.3154000e+00,1.3150000e+00,1.3147000e+00,1.3144000e+00,1.3140000e+00,1.3136000e+00,1.3132000e+00,1.3128000e+00,1.3124000e+00,1.3120000e+00,1.3116000e+00,1.3112000e+00,1.3107000e+00,1.3103000e+00,1.3098000e+00,1.3094000e+00,1.3090000e+00,1.3085000e+00,1.3080000e+00,1.3075000e+00,1.3070000e+00,1.3066000e+00,1.3061000e+00,1.3057000e+00,1.3052000e+00,1.3047000e+00,1.3043000e+00,1.3038000e+00,1.3033000e+00,1.3028000e+00,1.3023000e+00,1.3019000e+00,1.3014000e+00,1.3009000e+00,1.3004000e+00,1.2999000e+00,1.2995000e+00,1.2990000e+00,1.2985000e+00,1.2980000e+00,1.2975000e+00,1.2970000e+00,1.2965000e+00,1.2960000e+00,1.2956000e+00,1.2951000e+00,1.2947000e+00,1.2942000e+00,1.2937000e+00,1.2932000e+00,1.2927000e+00,1.2922000e+00,1.2917000e+00,1.2912000e+00,1.2907000e+00,1.2902000e+00,1.2898000e+00,1.2893000e+00,1.2888000e+00,1.2883000e+00,1.2878000e+00,1.2874000e+00,1.2870000e+00,1.2865000e+00,1.2861000e+00,1.2856000e+00,1.2852000e+00,1.2847000e+00,1.2843000e+00,1.2839000e+00,1.2834000e+00,1.2830000e+00,1.2826000e+00,1.2822000e+00,1.2817000e+00,1.2813000e+00,1.2809000e+00,1.2805000e+00,1.2801000e+00,1.2798000e+00,1.2795000e+00,1.2791000e+00,1.2788000e+00,1.2786000e+00,1.2784000e+00,1.2780000e+00,1.2776000e+00,1.2773000e+00,1.2769000e+00,1.2765000e+00,1.2761000e+00,1.2757000e+00,1.2754000e+00,1.2751000e+00,1.2748000e+00,1.2745000e+00,1.2742000e+00,1.2739000e+00,1.2737000e+00,1.2735000e+00,1.2732000e+00,1.2730000e+00,1.2727000e+00,1.2725000e+00,1.2723000e+00,1.2721000e+00,1.2719000e+00,1.2717000e+00,1.2715000e+00,1.2713000e+00,1.2712000e+00,1.2711000e+00,1.2710000e+00,1.2709000e+00,1.2708000e+00,1.2708000e+00,1.2708000e+00,1.2708000e+00,1.2710000e+00,1.2713000e+00,1.2717000e+00,1.2722000e+00,1.2728000e+00,1.2736000e+00);
export static const auto PROSPECT_TABLE_ABSORPTIONS=auto[526](
  auto(6.4881500e-02,1.6734000e-01,6.6674700e-02,5.2720000e-01,5.8000000e-05,1.0970000e+02,0.0000000e+00,1.2793000e+02),
  auto(7.0900000e-02,1.6761300e-01,5.8277000e-02,5.2320000e-01,6.1000000e-05,8.7130000e+01,0.0000000e+00,1.0160900e+02),
  auto(7.1223100e-02,1.6723900e-01,5.3115800e-02,5.1920000e-01,6.5000000e-05,7.0130000e+01,0.0000000e+00,8.1784400e+01),
  auto(7.2018500e-02,1.6544600e-01,4.9387300e-02,5.1520000e-01,6.9000000e-05,5.6160000e+01,0.0000000e+00,6.5492800e+01),
  auto(7.0762900e-02,1.6628800e-01,4.6898700e-02,5.1120000e-01,7.4000000e-05,4.4630000e+01,0.0000000e+00,5.2046700e+01),
  auto(6.9819300e-02,1.6716400e-01,4.5428600e-02,5.0720000e-01,7.9000000e-05,3.5670000e+01,0.0000000e+00,4.1597700e+01),
  auto(7.0472700e-02,1.6859900e-01,4.4249500e-02,5.0320000e-01,8.4000000e-05,2.8320000e+01,0.0000000e+00,3.3026300e+01),
  auto(7.1622300e-02,1.6772500e-01,4.3804600e-02,4.9920000e-01,8.9000000e-05,2.2760000e+01,0.0000000e+00,2.6542300e+01),
  auto(7.3652100e-02,1.6790500e-01,4.3958800e-02,4.9480000e-01,9.4000000e-05,1.7850000e+01,0.0000000e+00,2.0816400e+01),
  auto(7.4691100e-02,1.6817700e-01,4.4276800e-02,4.9000000e-01,9.9000000e-05,1.3920000e+01,0.0000000e+00,1.6233300e+01),
  auto(7.3794200e-02,1.6956900e-01,4.4786500e-02,4.8520000e-01,1.0400000e-04,1.0960000e+01,0.0000000e+00,1.2781400e+01),
  auto(6.9104700e-02,1.6990500e-01,4.5415400e-02,4.8050000e-01,1.0800000e-04,8.9470000e+00,0.0000000e+00,1.0433800e+01),
  auto(6.2668100e-02,1.6934500e-01,4.5994000e-02,4.7570000e-01,1.1200000e-04,7.2680000e+00,0.0000000e+00,8.4758100e+00),
  auto(5.4732400e-02,1.6446400e-01,4.6953000e-02,4.7080000e-01,1.1600000e-04,6.2220000e+00,0.0000000e+00,7.2559900e+00),
  auto(4.8139000e-02,1.5822400e-01,4.7813800e-02,4.6580000e-01,1.2000000e-04,5.3700000e+00,0.0000000e+00,6.2624000e+00),
  auto(4.3873300e-02,1.5167200e-01,4.8839300e-02,4.6080000e-01,1.2400000e-04,4.5750000e+00,0.0000000e+00,5.3352800e+00),
  auto(4.1774300e-02,1.4507600e-01,4.9840900e-02,4.5660000e-01,1.2800000e-04,4.0060000e+00,0.0000000e+00,4.6717300e+00),
  auto(4.0301700e-02,1.3919100e-01,5.1149800e-02,4.5250000e-01,1.3300000e-04,3.6710000e+00,0.0000000e+00,4.2810600e+00),
  auto(3.9292000e-02,1.3548000e-01,5.2819700e-02,4.4840000e-01,1.3800000e-04,3.2820000e+00,0.0000000e+00,3.8274100e+00),
  auto(3.8259900e-02,1.3416900e-01,5.4939800e-02,4.4420000e-01,1.4400000e-04,2.9830000e+00,0.0000000e+00,3.4787200e+00),
  auto(3.6775700e-02,1.3327100e-01,5.7151500e-02,4.4010000e-01,1.5200000e-04,2.8030000e+00,0.0000000e+00,3.2688100e+00),
  auto(3.4582900e-02,1.3042200e-01,5.9477800e-02,4.3500000e-01,1.6200000e-04,2.7020000e+00,0.0000000e+00,3.1510200e+00),
  auto(3.1518900e-02,1.2456600e-01,6.1809400e-02,4.2980000e-01,1.7400000e-04,2.6130000e+00,0.0000000e+00,3.0472300e+00),
  auto(2.7692100e-02,1.1652000e-01,6.3970300e-02,4.2470000e-01,1.8900000e-04,2.5360000e+00,0.0000000e+00,2.9574400e+00),
  auto(2.3428300e-02,1.0793000e-01,6.5909600e-02,4.1950000e-01,2.0900000e-04,2.4710000e+00,0.0000000e+00,2.8816400e+00),
  auto(1.9048500e-02,9.9004400e-02,6.7770900e-02,4.1440000e-01,2.3800000e-04,2.4170000e+00,0.0000000e+00,2.8186600e+00),
  auto(1.4934300e-02,8.9853600e-02,6.9140200e-02,4.1090000e-01,2.7300000e-04,2.3740000e+00,0.0000000e+00,2.7685200e+00),
  auto(1.1295900e-02,8.0588400e-02,7.0073500e-02,4.0740000e-01,3.1000000e-04,2.3410000e+00,0.0000000e+00,2.7300300e+00),
  auto(8.2461000e-03,7.1319100e-02,7.0871900e-02,4.0380000e-01,3.4900000e-04,2.3180000e+00,0.0000000e+00,2.7032100e+00),
  auto(5.8680500e-03,6.2156400e-02,7.1389600e-02,4.0000000e-01,3.8600000e-04,2.3040000e+00,0.0000000e+00,2.6868800e+00),
  auto(4.3337900e-03,5.3210600e-02,7.1682100e-02,3.9620000e-01,4.0900000e-04,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(3.6525200e-03,4.4592400e-02,7.1864100e-02,3.9240000e-01,4.0900000e-04,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(3.7696700e-03,3.6412200e-02,7.2240300e-02,3.8860000e-01,4.2300000e-04,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(4.4842800e-03,2.8780600e-02,7.2475800e-02,3.8240000e-01,4.4500000e-04,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(5.5645500e-03,2.1807900e-02,7.2506100e-02,3.7390000e-01,4.7000000e-04,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(6.7864000e-03,1.5604800e-02,7.2063300e-02,3.6540000e-01,4.9500000e-04,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(7.9483300e-03,1.0281800e-02,7.1067100e-02,3.5970000e-01,5.2700000e-04,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(8.9036800e-03,5.9492500e-03,6.9355400e-02,3.5400000e-01,5.6400000e-04,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(9.6821300e-03,2.7177800e-03,6.6732600e-02,3.4890000e-01,6.1100000e-04,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(1.0385500e-02,6.9786300e-04,6.3357500e-02,3.4450000e-01,6.4600000e-04,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(1.1048000e-02,2.1316300e-13,5.9651500e-02,3.4010000e-01,6.7200000e-04,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(1.1827200e-02,0.0000000e+00,5.5543100e-02,3.3300000e-01,6.9900000e-04,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(1.2933000e-02,0.0000000e+00,5.0900000e-02,3.2580000e-01,7.3400000e-04,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(1.4301400e-02,0.0000000e+00,4.6174300e-02,3.1820000e-01,7.8700000e-04,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(1.5693500e-02,0.0000000e+00,4.1733800e-02,3.1000000e-01,8.5800000e-04,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(1.6945200e-02,0.0000000e+00,3.7412000e-02,3.0190000e-01,9.5200000e-04,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(1.8065800e-02,0.0000000e+00,3.2845800e-02,2.9400000e-01,1.0790000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(1.9018600e-02,0.0000000e+00,2.8665200e-02,2.8610000e-01,1.2530000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(1.9766300e-02,0.0000000e+00,2.5133500e-02,2.7840000e-01,1.4590000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(2.0326600e-02,0.0000000e+00,2.1917600e-02,2.7100000e-01,1.7000000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(2.0854000e-02,0.0000000e+00,1.8740100e-02,2.6360000e-01,2.2240000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(2.1545800e-02,0.0000000e+00,1.5987000e-02,2.5660000e-01,2.4480000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(2.2552500e-02,0.0000000e+00,1.3800300e-02,2.4970000e-01,2.6530000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(2.3841900e-02,0.0000000e+00,1.1883700e-02,2.4310000e-01,2.7150000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(2.5269100e-02,0.0000000e+00,1.0021900e-02,2.3660000e-01,2.7640000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(2.6530300e-02,0.0000000e+00,8.4110300e-03,2.3020000e-01,2.8100000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(2.7379200e-02,0.0000000e+00,7.0566500e-03,2.2440000e-01,2.8680000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(2.7821100e-02,0.0000000e+00,5.9044400e-03,2.1850000e-01,2.9220000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(2.8194400e-02,0.0000000e+00,4.9336200e-03,2.1290000e-01,2.9880000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(2.9181400e-02,0.0000000e+00,4.1234100e-03,2.0740000e-01,3.0380000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(3.1247500e-02,0.0000000e+00,3.4530300e-03,2.0200000e-01,3.1110000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(3.4254600e-02,0.0000000e+00,2.9017000e-03,1.9680000e-01,3.1810000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(3.7588000e-02,0.0000000e+00,2.4486500e-03,1.9160000e-01,3.2630000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(4.0221700e-02,0.0000000e+00,2.0730800e-03,1.8650000e-01,3.3620000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(4.2880000e-02,0.0000000e+00,1.7542200e-03,1.8160000e-01,3.5080000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(4.7494900e-02,0.0000000e+00,1.4712800e-03,1.7680000e-01,3.7910000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(5.4699600e-02,0.0000000e+00,1.2035000e-03,1.7170000e-01,4.0190000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(6.2750000e-02,0.0000000e+00,9.3007700e-04,1.6660000e-01,4.0980000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(6.8674900e-02,0.0000000e+00,6.3883500e-04,1.6130000e-01,4.1500000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(7.1401500e-02,0.0000000e+00,3.9030300e-04,1.5590000e-01,4.2230000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(6.8920600e-02,0.0000000e+00,2.0109700e-04,1.5040000e-01,4.3180000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(5.6858300e-02,0.0000000e+00,7.3101600e-05,1.4510000e-01,4.4580000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(4.0858500e-02,0.0000000e+00,8.2009100e-06,1.3970000e-01,4.6460000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(2.7502100e-02,0.0000000e+00,0.0000000e+00,1.3450000e-01,4.9030000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(1.8173300e-02,0.0000000e+00,0.0000000e+00,1.2950000e-01,5.2440000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(1.2240600e-02,0.0000000e+00,0.0000000e+00,1.2450000e-01,5.7220000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(8.5869500e-03,0.0000000e+00,0.0000000e+00,1.2000000e-01,6.3030000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(6.2803000e-03,0.0000000e+00,0.0000000e+00,1.1560000e-01,6.9930000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(4.6965200e-03,0.0000000e+00,0.0000000e+00,1.1110000e-01,7.8930000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(3.5302000e-03,0.0000000e+00,0.0000000e+00,1.0670000e-01,9.1090000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(2.6434000e-03,0.0000000e+00,0.0000000e+00,1.0240000e-01,1.0720000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(1.9636100e-03,0.0000000e+00,0.0000000e+00,9.8290000e-02,1.2680000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(1.4467700e-03,0.0000000e+00,0.0000000e+00,9.4220000e-02,1.4870000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(1.0590700e-03,0.0000000e+00,0.0000000e+00,9.0220000e-02,1.7870000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(7.7288900e-04,0.0000000e+00,0.0000000e+00,8.6310000e-02,2.2070000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(5.6764200e-04,0.0000000e+00,0.0000000e+00,8.2390000e-02,2.5320000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(4.2566500e-04,0.0000000e+00,0.0000000e+00,7.9010000e-02,2.6720000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(3.3141100e-04,0.0000000e+00,0.0000000e+00,7.5620000e-02,2.7220000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(2.6796000e-04,0.0000000e+00,0.0000000e+00,7.2450000e-02,2.7410000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(2.0863600e-04,0.0000000e+00,0.0000000e+00,6.9490000e-02,2.7540000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(1.5305800e-04,0.0000000e+00,0.0000000e+00,6.6530000e-02,2.7710000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(1.0318700e-04,0.0000000e+00,0.0000000e+00,6.3850000e-02,2.7740000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(6.0984700e-05,0.0000000e+00,0.0000000e+00,6.1170000e-02,2.7610000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(2.8411800e-05,0.0000000e+00,0.0000000e+00,5.8430000e-02,2.7480000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(7.4298300e-06,0.0000000e+00,0.0000000e+00,5.5640000e-02,2.7100000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(1.0524900e-13,0.0000000e+00,0.0000000e+00,5.2840000e-02,2.6590000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,5.0500000e-02,2.6130000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,4.8160000e-02,2.5130000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,4.5940000e-02,2.4120000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,4.3850000e-02,2.3370000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,4.1760000e-02,2.2460000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,3.9850000e-02,2.2040000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,3.7930000e-02,2.1770000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,3.6150000e-02,2.1980000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,3.4510000e-02,2.2480000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,3.2860000e-02,2.3290000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,3.1390000e-02,2.5160000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,2.9910000e-02,2.9140000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,2.8520000e-02,3.4590000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,2.7200000e-02,3.7880000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,2.5890000e-02,3.9490000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,2.4690000e-02,4.0570000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,2.3500000e-02,4.1490000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,2.2380000e-02,4.2540000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,2.1330000e-02,4.3600000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,2.0290000e-02,4.4540000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.9380000e-02,4.5520000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8470000e-02,4.7050000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.7620000e-02,4.8670000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.6830000e-02,5.0500000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.6040000e-02,5.2980000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.5330000e-02,5.5280000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.4630000e-02,5.7450000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.4000000e-02,5.9820000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.3430000e-02,6.1850000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2860000e-02,6.4070000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2450000e-02,6.6720000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2050000e-02,6.9890000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1650000e-02,7.3580000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1250000e-02,7.7920000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.0860000e-02,8.5280000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.0470000e-02,9.8190000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.0080000e-02,1.1130000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,9.6910000e-03,1.3270000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,9.3090000e-03,1.5570000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,8.9310000e-03,1.8180000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,8.5570000e-03,2.1870000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,8.1870000e-03,2.5420000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,7.8220000e-03,3.2740000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,7.4620000e-03,3.9300000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,7.1070000e-03,4.3850000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,6.7580000e-03,4.6630000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,6.4140000e-03,4.7720000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,6.0760000e-03,4.8270000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,5.7450000e-03,4.8670000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,5.4190000e-03,4.8210000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,5.1010000e-03,4.7380000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,4.7900000e-03,4.6040000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,4.4860000e-03,4.4340000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,4.1900000e-03,4.2650000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,3.9010000e-03,4.0720000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,3.6200000e-03,3.8680000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,3.3480000e-03,3.6400000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,3.0850000e-03,3.4020000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,2.8300000e-03,3.1910000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,2.5850000e-03,2.9570000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,2.3480000e-03,2.7240000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,2.1220000e-03,2.5060000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.9050000e-03,2.3310000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.6990000e-03,2.1510000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.5030000e-03,1.9810000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.3180000e-03,1.8410000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1440000e-03,1.7150000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,9.8110000e-04,1.6130000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,8.2970000e-04,1.5320000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,6.9010000e-04,1.4750000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,5.6260000e-04,1.4380000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,4.4730000e-04,1.4120000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,3.4460000e-04,1.4060000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,2.5480000e-04,1.4260000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.7800000e-04,1.4430000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1460000e-04,1.5190000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,6.4880000e-05,1.5800000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,2.9010000e-05,1.6770000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,7.2970000e-06,1.7770000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.9060000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.0310000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.1660000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.2980000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.3530000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.5280000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.7650000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.1640000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.7700000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,4.7120000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.0520000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,7.5350000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,9.2530000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.0410000e+00,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1310000e+00,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1700000e+00,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1960000e+00,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2050000e+00,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2230000e+00,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2290000e+00,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2390000e+00,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2520000e+00,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2620000e+00,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2720000e+00,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2820000e+00,2.3000000e+00,0.0000000e+00,2.6821200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2720000e+00,2.3000000e+00,0.0000000e+00,2.6822600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2730000e+00,2.3020000e+00,0.0000000e+00,2.6847400e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2580000e+00,2.3080000e+00,0.0000000e+00,2.6914400e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2470000e+00,2.3170000e+00,0.0000000e+00,2.7023600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2400000e+00,2.3300000e+00,0.0000000e+00,2.7174600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2140000e+00,2.3460000e+00,0.0000000e+00,2.7357100e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2050000e+00,2.3640000e+00,0.0000000e+00,2.7567400e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1970000e+00,2.3840000e+00,0.0000000e+00,2.7803100e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1800000e+00,2.4060000e+00,0.0000000e+00,2.8058400e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1610000e+00,2.4300000e+00,0.0000000e+00,2.8335000e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1550000e+00,2.4540000e+00,0.0000000e+00,2.8622100e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1380000e+00,2.4800000e+00,0.0000000e+00,2.8918700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1230000e+00,2.5060000e+00,0.0000000e+00,2.9221100e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1070000e+00,2.5320000e+00,0.0000000e+00,2.9525300e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1010000e+00,2.5580000e+00,0.0000000e+00,2.9827300e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.0940000e+00,2.5830000e+00,0.0000000e+00,3.0120600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.0830000e+00,2.6070000e+00,0.0000000e+00,3.0404700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.0800000e+00,2.6310000e+00,0.0000000e+00,3.0677500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.0870000e+00,2.6520000e+00,0.0000000e+00,3.0929600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1070000e+00,2.6720000e+00,0.0000000e+00,3.1158200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1210000e+00,2.6890000e+00,0.0000000e+00,3.1362300e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1560000e+00,2.7040000e+00,0.0000000e+00,3.1533900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1810000e+00,2.7160000e+00,0.0000000e+00,3.1675200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2160000e+00,2.7250000e+00,0.0000000e+00,3.1776800e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2670000e+00,2.7300000e+00,0.0000000e+00,3.1836000e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.3230000e+00,2.7210000e+00,0.0000000e+00,3.1825500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.3920000e+00,2.7120000e+00,0.0000000e+00,3.1627500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.4890000e+00,2.6880000e+00,0.0000000e+00,3.1349000e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.5800000e+00,2.6750000e+00,0.0000000e+00,3.1201300e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.7010000e+00,2.6680000e+00,0.0000000e+00,3.1151200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8310000e+00,2.6830000e+00,0.0000000e+00,3.1208500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.9740000e+00,2.6830000e+00,0.0000000e+00,3.1191500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.1530000e+00,2.6700000e+00,0.0000000e+00,3.1092300e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.3120000e+00,2.6650000e+00,0.0000000e+00,3.1125300e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.5400000e+00,2.6840000e+00,0.0000000e+00,3.1366000e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.7010000e+00,2.7170000e+00,0.0000000e+00,3.1729500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.8890000e+00,2.7510000e+00,0.0000000e+00,3.2124000e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.1460000e+00,2.8180000e+00,0.0000000e+00,3.2850400e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.3190000e+00,2.8990000e+00,0.0000000e+00,3.3854300e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.5040000e+00,2.9660000e+00,0.0000000e+00,3.4451500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.7010000e+00,2.8870000e+00,0.0000000e+00,3.4037900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.9270000e+00,2.8880000e+00,0.0000000e+00,3.3515200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,4.2160000e+00,2.8840000e+00,0.0000000e+00,3.3879900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,4.6300000e+00,3.0140000e+00,0.0000000e+00,3.5134200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.2600000e+00,3.1280000e+00,0.0000000e+00,3.6146700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.2420000e+00,2.9790000e+00,0.0000000e+00,3.5063200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,7.6330000e+00,2.9500000e+00,0.0000000e+00,3.4535900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,9.5910000e+00,3.0660000e+00,0.0000000e+00,3.5642200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1840000e+01,3.0880000e+00,0.0000000e+00,3.6029500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.4340000e+01,3.0650000e+00,0.0000000e+00,3.5843200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.7140000e+01,3.0940000e+00,0.0000000e+00,3.5999500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.9800000e+01,3.1580000e+00,0.0000000e+00,3.6519000e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.2020000e+01,3.2980000e+00,0.0000000e+00,3.7515600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.4070000e+01,3.5340000e+00,0.0000000e+00,3.9537000e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.5710000e+01,3.7750000e+00,0.0000000e+00,4.2553400e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.7010000e+01,4.0710000e+00,0.0000000e+00,4.6075300e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.8060000e+01,4.3950000e+00,0.0000000e+00,4.9714800e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.8920000e+01,4.7080000e+00,0.0000000e+00,5.3198300e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.9560000e+01,4.9740000e+00,0.0000000e+00,5.6125800e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.0030000e+01,5.1790000e+00,3.0352600e-04,5.8529700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.0360000e+01,5.3580000e+00,5.4994700e-04,6.0549800e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.0520000e+01,5.4810000e+00,1.0993200e-03,6.1981500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.0560000e+01,5.5600000e+00,2.2062900e-03,6.2781900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.0490000e+01,5.6110000e+00,4.0619800e-03,6.3530400e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.0290000e+01,5.6820000e+00,7.2676900e-03,6.4116400e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.9980000e+01,5.7110000e+00,1.3145400e-02,6.4466500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.9360000e+01,5.7320000e+00,2.3776600e-02,6.4755300e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.8510000e+01,5.7450000e+00,4.3005900e-02,6.4949500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.7540000e+01,5.7800000e+00,7.3929500e-02,6.5276100e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.6380000e+01,5.8080000e+00,1.3081100e-01,6.5639100e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.5220000e+01,5.8270000e+00,2.9863900e-01,6.5920500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.4130000e+01,5.9080000e+00,6.2766600e-01,6.6146100e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.3070000e+01,5.9630000e+00,1.1202700e+00,6.5975300e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.1930000e+01,5.9940000e+00,1.7046700e+00,6.5506000e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.0820000e+01,6.0080000e+00,2.3136600e+00,6.4953100e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.9720000e+01,6.0270000e+00,2.8537000e+00,6.4351000e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8740000e+01,5.9890000e+00,3.2721900e+00,6.3579700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.7840000e+01,5.9710000e+00,3.6163500e+00,6.2719400e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.6880000e+01,5.9390000e+00,3.9053800e+00,6.2089000e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.6060000e+01,5.9090000e+00,4.1695100e+00,6.1375000e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.5250000e+01,5.8760000e+00,4.3680900e+00,6.0604400e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.4470000e+01,5.8180000e+00,4.5288700e+00,5.9852500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.3810000e+01,5.7760000e+00,4.6639000e+00,5.9311200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.3160000e+01,5.7620000e+00,4.7651400e+00,5.8975000e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2540000e+01,5.7370000e+00,4.8523400e+00,5.8693600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1930000e+01,5.7510000e+00,4.9121200e+00,5.8559700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1370000e+01,5.7440000e+00,4.9231500e+00,5.8541100e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.0880000e+01,5.7350000e+00,4.8682400e+00,5.8509600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.0440000e+01,5.7310000e+00,4.7621300e+00,5.8509200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.0010000e+01,5.6970000e+00,4.6515000e+00,5.8445600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,9.6600000e+00,5.7130000e+00,4.5769100e+00,5.8644200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,9.3050000e+00,5.7430000e+00,4.5754100e+00,5.8981600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.9770000e+00,5.7460000e+00,4.6261500e+00,5.8943700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.6650000e+00,5.7490000e+00,4.7121600e+00,5.8742700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.3640000e+00,5.7390000e+00,4.8185800e+00,5.8684500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.1040000e+00,5.7450000e+00,4.8899500e+00,5.8547100e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,7.8340000e+00,5.7250000e+00,4.9692100e+00,5.8169600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,7.6090000e+00,5.6990000e+00,5.0432300e+00,5.7853200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,7.3740000e+00,5.6590000e+00,5.1108200e+00,5.7212200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,7.1640000e+00,5.5910000e+00,5.1564000e+00,5.6341100e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.9870000e+00,5.5170000e+00,5.1773200e+00,5.5615700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.8280000e+00,5.4500000e+00,5.1857000e+00,5.4888900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.6950000e+00,5.3910000e+00,5.1673100e+00,5.4252900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.5670000e+00,5.3470000e+00,5.1721700e+00,5.3667500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.4490000e+00,5.2920000e+00,5.2143200e+00,5.3080600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.3450000e+00,5.2530000e+00,5.2702600e+00,5.2524600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.2140000e+00,5.2110000e+00,5.3256900e+00,5.1999500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.1210000e+00,5.1930000e+00,5.3381300e+00,5.1691600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.0480000e+00,5.1780000e+00,5.2871000e+00,5.1518400e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.9730000e+00,5.1390000e+00,5.1865100e+00,5.1326100e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.9110000e+00,5.1260000e+00,5.0830200e+00,5.1196700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.8260000e+00,5.1090000e+00,5.0110100e+00,5.1228700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.7460000e+00,5.1360000e+00,4.9884600e+00,5.1457100e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.6840000e+00,5.1930000e+00,5.0295900e+00,5.2089500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.6480000e+00,5.2730000e+00,5.1254100e+00,5.2995900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.6180000e+00,5.3890000e+00,5.2925700e+00,5.4025100e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.6030000e+00,5.5220000e+00,5.5542600e+00,5.5433600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.5630000e+00,5.6980000e+00,5.9126900e+00,5.6313900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.5420000e+00,5.6890000e+00,6.3491200e+00,5.6441500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.5330000e+00,5.8470000e+00,6.8895000e+00,5.6882900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.5240000e+00,6.0220000e+00,7.5084100e+00,5.8277600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.5200000e+00,6.2140000e+00,8.1996400e+00,5.9647900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.5270000e+00,6.4260000e+00,8.8845800e+00,6.1087200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.5200000e+00,6.6160000e+00,9.4843200e+00,6.2481300e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.5380000e+00,6.8070000e+00,9.9421500e+00,6.4031500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.5710000e+00,7.0060000e+00,1.0196200e+01,6.5895800e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.6290000e+00,7.1640000e+00,1.0264600e+01,6.7592400e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.6930000e+00,7.3150000e+00,1.0219400e+01,6.9235900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.7660000e+00,7.4400000e+00,1.0131700e+01,7.0929000e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.8290000e+00,7.5570000e+00,1.0031400e+01,7.2364000e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.9300000e+00,7.6530000e+00,1.0015000e+01,7.3477400e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.0250000e+00,7.7080000e+00,1.0088200e+01,7.4142300e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.1360000e+00,7.7120000e+00,1.0200900e+01,7.3908200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.2560000e+00,7.6440000e+00,1.0318900e+01,7.2879600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.3920000e+00,7.5170000e+00,1.0370200e+01,7.1473500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.5710000e+00,7.4080000e+00,1.0336700e+01,7.0231600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.7480000e+00,7.3320000e+00,1.0197500e+01,6.9691700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.9550000e+00,7.2950000e+00,9.9798400e+00,6.9572400e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,7.1790000e+00,7.2920000e+00,9.6744600e+00,6.9681700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,7.4140000e+00,7.2500000e+00,9.3079600e+00,6.9902200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,7.6690000e+00,7.1860000e+00,8.8756300e+00,6.9720100e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,7.8930000e+00,7.1010000e+00,8.3591600e+00,6.9265300e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.1030000e+00,6.9740000e+00,7.8585700e+00,6.8444600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.2640000e+00,6.7930000e+00,7.3995500e+00,6.7344300e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.4300000e+00,6.6610000e+00,7.0150600e+00,6.6060500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.5350000e+00,6.5220000e+00,6.7311600e+00,6.4994700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.6400000e+00,6.4790000e+00,6.5643300e+00,6.4763700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.7380000e+00,6.4610000e+00,6.4547800e+00,6.4626300e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.7780000e+00,6.4210000e+00,6.3699600e+00,6.4127100e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.8090000e+00,6.3720000e+00,6.3275500e+00,6.3640500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.7890000e+00,6.3140000e+00,6.2857700e+00,6.3103900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.7500000e+00,6.2580000e+00,6.2124600e+00,6.2573800e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.7440000e+00,6.2310000e+00,6.0807100e+00,6.2348700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.7870000e+00,6.1920000e+00,5.8970200e+00,6.2236200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.8110000e+00,6.1430000e+00,5.7008700e+00,6.1928300e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.8540000e+00,6.0630000e+00,5.5438500e+00,6.1520900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.8840000e+00,6.0440000e+00,5.4308200e+00,6.1058200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.9510000e+00,5.9970000e+00,5.3286400e+00,6.0628600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,9.1100000e+00,5.9270000e+00,5.1829000e+00,6.0246200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,9.3150000e+00,5.8320000e+00,4.9598000e+00,5.9371300e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,9.6330000e+00,5.6810000e+00,4.7094800e+00,5.8377000e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.0050000e+01,5.6200000e+00,4.4895400e+00,5.7638700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.0590000e+01,5.5520000e+00,4.3846900e+00,5.7140700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1390000e+01,5.5070000e+00,4.3243600e+00,5.6684300e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2350000e+01,5.4620000e+00,4.2574100e+00,5.6068000e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.3750000e+01,5.4400000e+00,4.1286900e+00,5.6089100e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.5860000e+01,5.4610000e+00,3.9605000e+00,5.6570500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8530000e+01,5.3790000e+00,3.6927100e+00,5.5674600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.2500000e+01,5.1710000e+00,3.4007000e+00,5.3889700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.7510000e+01,5.1290000e+00,3.2797700e+00,5.3790700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.5040000e+01,5.1850000e+00,3.3293000e+00,5.4313500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,4.5300000e+01,5.2040000e+00,3.5500200e+00,5.4250700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.7610000e+01,5.3180000e+00,4.0563800e+00,5.4867900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,7.0590000e+01,5.5610000e+00,4.8856200e+00,5.6515200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.4690000e+01,5.8300000e+00,5.9127700e+00,5.8295000e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,9.7220000e+01,6.1930000e+00,7.1474300e+00,6.0818000e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.0780000e+02,6.7100000e+00,8.5562300e+00,6.4597700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1570000e+02,7.2380000e+00,1.0027200e+01,6.8866300e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2150000e+02,7.7670000e+00,1.1429900e+01,7.2812100e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2570000e+02,8.1500000e+00,1.2752200e+01,7.5415000e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2860000e+02,8.4490000e+00,1.3949100e+01,7.7295500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.3030000e+02,8.7230000e+00,1.4981200e+01,7.9031800e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.3060000e+02,8.9090000e+00,1.5829100e+01,7.9954600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2990000e+02,8.9830000e+00,1.6491200e+01,8.0230500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2820000e+02,9.0280000e+00,1.6976500e+01,8.0062400e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2560000e+02,9.0410000e+00,1.7275500e+01,7.9710200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2240000e+02,9.0210000e+00,1.7409700e+01,7.9282900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1900000e+02,8.9740000e+00,1.7433900e+01,7.8761200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1490000e+02,8.9230000e+00,1.7386800e+01,7.8231800e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1100000e+02,8.8800000e+00,1.7374400e+01,7.7741900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.0660000e+02,8.8610000e+00,1.7454600e+01,7.7565500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.0230000e+02,8.8720000e+00,1.7623400e+01,7.7268400e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,9.8310000e+01,8.8550000e+00,1.7815700e+01,7.6775200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,9.4120000e+01,8.8430000e+00,1.7960900e+01,7.6395800e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,9.0090000e+01,8.8070000e+00,1.7992400e+01,7.6133800e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.6110000e+01,8.7950000e+00,1.7840800e+01,7.6004600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.2620000e+01,8.7600000e+00,1.7533400e+01,7.6144700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,7.9040000e+01,8.7900000e+00,1.7124900e+01,7.7010200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,7.5570000e+01,8.8740000e+00,1.6659300e+01,7.8610600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,7.2340000e+01,8.9800000e+00,1.6143000e+01,8.0334600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.9240000e+01,9.0760000e+00,1.5593500e+01,8.2195900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.6340000e+01,9.2140000e+00,1.5062300e+01,8.4545600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.3580000e+01,9.4060000e+00,1.4534900e+01,8.7399100e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.1050000e+01,9.6380000e+00,1.4016100e+01,9.0601800e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.8570000e+01,9.9530000e+00,1.3552000e+01,9.4740800e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.6320000e+01,1.0390000e+01,1.3179600e+01,1.0020800e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.3980000e+01,1.0910000e+01,1.2921500e+01,1.0645200e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.1970000e+01,1.1500000e+01,1.2821600e+01,1.1326300e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.0060000e+01,1.2170000e+01,1.2953500e+01,1.2062300e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,4.8060000e+01,1.2870000e+01,1.3335300e+01,1.2829500e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,4.6350000e+01,1.3640000e+01,1.3947800e+01,1.3611900e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,4.4730000e+01,1.4410000e+01,1.4684300e+01,1.4389400e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,4.3020000e+01,1.5170000e+01,1.5362900e+01,1.5145700e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,4.1550000e+01,1.5910000e+01,1.5751200e+01,1.5901500e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,4.0020000e+01,1.6530000e+01,1.5638900e+01,1.6627200e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.8680000e+01,1.6980000e+01,1.4907700e+01,1.7248900e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.7260000e+01,1.7340000e+01,1.3593000e+01,1.7808600e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.5920000e+01,1.7610000e+01,1.1886900e+01,1.8349200e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.4790000e+01,1.7830000e+01,1.0019700e+01,1.8849200e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.3630000e+01,1.7990000e+01,8.2462800e+00,1.9280100e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.2510000e+01,1.8200000e+01,6.7859300e+00,1.9687300e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.1540000e+01,1.8410000e+01,5.7404300e+00,2.0065500e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.0520000e+01,1.8610000e+01,5.0492900e+00,2.0371700e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.9580000e+01,1.8810000e+01,4.6525900e+00,2.0639300e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.8650000e+01,1.9030000e+01,4.5379300e+00,2.0898700e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.7760000e+01,1.9210000e+01,4.5858700e+00,2.1119300e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.6920000e+01,1.9350000e+01,4.7485200e+00,2.1240800e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.6170000e+01,1.9470000e+01,4.9975800e+00,2.1336400e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.5470000e+01,1.9600000e+01,5.3431300e+00,2.1437300e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.4850000e+01,1.9660000e+01,5.7465600e+00,2.1480200e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.4200000e+01,1.9740000e+01,6.2901800e+00,2.1479400e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.3580000e+01,1.9790000e+01,7.0995600e+00,2.1447600e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.3040000e+01,1.9860000e+01,8.1987700e+00,2.1377200e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.2520000e+01,1.9930000e+01,9.5999100e+00,2.1266500e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.2020000e+01,2.0010000e+01,1.1288300e+01,2.1150900e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.1620000e+01,2.0130000e+01,1.3212800e+01,2.1026900e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.1230000e+01,2.0220000e+01,1.5276600e+01,2.0856000e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.0850000e+01,2.0270000e+01,1.7431500e+01,2.0635500e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.0490000e+01,2.0290000e+01,1.9611700e+01,2.0377700e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.0180000e+01,2.0260000e+01,2.1735100e+01,2.0103600e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.9870000e+01,2.0300000e+01,2.3712700e+01,1.9853800e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.9630000e+01,2.0260000e+01,2.5462200e+01,1.9586100e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.9420000e+01,2.0160000e+01,2.6891100e+01,1.9296700e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.9230000e+01,2.0070000e+01,2.8013000e+01,1.9023600e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.9040000e+01,1.9910000e+01,2.8849700e+01,1.8742500e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8840000e+01,1.9690000e+01,2.9439600e+01,1.8434900e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8660000e+01,1.9470000e+01,2.9807100e+01,1.8127500e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8490000e+01,1.9220000e+01,2.9968000e+01,1.7834900e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8370000e+01,1.8990000e+01,2.9952300e+01,1.7558100e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8360000e+01,1.8740000e+01,2.9749500e+01,1.7309900e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8340000e+01,1.8520000e+01,2.9433100e+01,1.7095400e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8340000e+01,1.8290000e+01,2.9037400e+01,1.6901500e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8320000e+01,1.8070000e+01,2.8506700e+01,1.6718000e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8280000e+01,1.7870000e+01,2.7816400e+01,1.6569200e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8320000e+01,1.7730000e+01,2.6998500e+01,1.6510300e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8370000e+01,1.7630000e+01,2.6084400e+01,1.6543600e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8420000e+01,1.7640000e+01,2.5168200e+01,1.6665000e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8510000e+01,1.7790000e+01,2.4403700e+01,1.6941600e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8600000e+01,1.8140000e+01,2.3892400e+01,1.7395400e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8700000e+01,1.8660000e+01,2.3664300e+01,1.7994600e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8880000e+01,1.9310000e+01,2.3810200e+01,1.8738700e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.9080000e+01,2.0170000e+01,2.4331300e+01,1.9637600e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.9290000e+01,2.1140000e+01,2.5166400e+01,2.0606500e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.9510000e+01,2.2100000e+01,2.6257000e+01,2.1585200e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.9800000e+01,2.3130000e+01,2.7499400e+01,2.2556100e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.0040000e+01,2.4100000e+01,2.8752500e+01,2.3491400e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.0380000e+01,2.4990000e+01,2.9947700e+01,2.4342500e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.0710000e+01,2.5830000e+01,3.1075300e+01,2.5160000e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.1030000e+01,2.6570000e+01,3.2200700e+01,2.5858100e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.1450000e+01,2.7180000e+01,3.3314300e+01,2.6375400e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.1830000e+01,2.7630000e+01,3.4325900e+01,2.6797200e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.2230000e+01,2.8210000e+01,3.5197400e+01,2.7278000e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.2760000e+01,2.8780000e+01,3.5770400e+01,2.7902900e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.3270000e+01,2.9370000e+01,3.5914000e+01,2.8500300e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.3770000e+01,2.9820000e+01,3.5517300e+01,2.9073700e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.4330000e+01,3.0280000e+01,3.4712100e+01,2.9712300e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.4850000e+01,3.0660000e+01,3.3586500e+01,3.0283700e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.5490000e+01,3.0750000e+01,3.2317200e+01,3.0566600e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.6110000e+01,3.0580000e+01,3.1148500e+01,3.0524900e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.6810000e+01,3.0290000e+01,3.0235100e+01,3.0297600e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.7440000e+01,3.0000000e+01,2.9561500e+01,3.0082700e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.8190000e+01,2.9840000e+01,2.9051400e+01,2.9922300e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.9030000e+01,2.9760000e+01,2.8682500e+01,2.9892200e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.9690000e+01,2.9850000e+01,2.8352500e+01,3.0050700e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.0540000e+01,3.0050000e+01,2.8099300e+01,3.0291200e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.1450000e+01,3.0220000e+01,2.7958300e+01,3.0511000e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.2190000e+01,3.0290000e+01,2.7965600e+01,3.0597900e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.3110000e+01,3.0190000e+01,2.8017500e+01,3.0460300e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.4040000e+01,2.9930000e+01,2.8012400e+01,3.0200500e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.4860000e+01,2.9660000e+01,2.7838600e+01,2.9872400e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.5920000e+01,2.9260000e+01,2.7355200e+01,2.9528200e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.6970000e+01,2.8900000e+01,2.6584200e+01,2.9209800e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.8010000e+01,2.8700000e+01,2.5675000e+01,2.9059200e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.9130000e+01,2.8500000e+01,2.4712500e+01,2.8997000e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,4.0240000e+01,2.8230000e+01,2.3776500e+01,2.8848500e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,4.1470000e+01,2.8030000e+01,2.3002400e+01,2.8723600e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,4.2680000e+01,2.7910000e+01,2.2354200e+01,2.8630000e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,4.3880000e+01,2.7810000e+01,2.1836600e+01,2.8592000e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,4.5130000e+01,2.7830000e+01,2.1417200e+01,2.8660300e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,4.6380000e+01,2.7820000e+01,2.0978000e+01,2.8696200e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,4.7640000e+01,2.7830000e+01,2.0549200e+01,2.8754600e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,4.8940000e+01,2.7990000e+01,2.0154000e+01,2.9018100e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.0210000e+01,2.8190000e+01,1.9790100e+01,2.9237200e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.1590000e+01,2.8310000e+01,1.9363500e+01,2.9461000e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.3100000e+01,2.8590000e+01,1.8956000e+01,2.9835500e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.4620000e+01,2.8980000e+01,1.8534600e+01,3.0342600e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.6180000e+01,2.9440000e+01,1.8003700e+01,3.0951900e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.8080000e+01,3.0020000e+01,1.7576200e+01,3.1619300e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.9760000e+01,3.0640000e+01,1.7358800e+01,3.2397200e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.1460000e+01,3.1320000e+01,1.7222600e+01,3.3176600e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.3160000e+01,3.2040000e+01,1.7123800e+01,3.3916200e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.5390000e+01,3.2820000e+01,1.7060400e+01,3.4905900e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.7250000e+01,3.3840000e+01,1.6994200e+01,3.6032100e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.9180000e+01,3.4720000e+01,1.6885100e+01,3.7063400e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,7.1740000e+01,3.5610000e+01,1.6869400e+01,3.8049000e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,7.3810000e+01,3.6360000e+01,1.6707900e+01,3.8910600e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,7.5910000e+01,3.6990000e+01,1.6087700e+01,3.9725800e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,7.8390000e+01,3.7920000e+01,1.5297800e+01,4.0647900e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.0460000e+01,3.8330000e+01,1.4514900e+01,4.1612400e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.2950000e+01,3.8840000e+01,1.3578400e+01,4.2086700e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.5100000e+01,3.9000000e+01,1.2592400e+01,4.2302300e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.7080000e+01,3.9160000e+01,1.1626900e+01,4.2782100e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.9450000e+01,3.9510000e+01,1.0463700e+01,4.3311200e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,9.2040000e+01,3.9580000e+01,9.5368600e+00,4.3428200e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,9.3580000e+01,3.9170000e+01,9.1024700e+00,4.3201200e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,9.5300000e+01,3.8710000e+01,9.4077800e+00,4.2636600e+01),
);
@(noinline)
export prospect_result prospect(
  float num_layers=1.5,
  float incident_cone_angle=0.7,
  float dry_matter=5.0,
  float water=0.1,
  float chlorophylls=30.0,
  float anthocyanins=1.0,
  float carotenoids=1.5,
  float proteins=0.0,
  float carbons=0.0,
  float browns=0.0,
){
  color ior(0);
  color k(0);
  const auto contents(chlorophylls,carotenoids,anthocyanins,browns,1e-1*water,1e-3*dry_matter,1e-3*proteins,1e-3*carbons);
  for(int i=0;i<$WAVELENGTH_BASE_MAX;i++){
    float w=(PROSPECT_TABLE_SIZE-1)*saturate(($state.wavelength_base[i]-PROSPECT_MIN_WAVELENGTH)/(PROSPECT_MAX_WAVELENGTH-PROSPECT_MIN_WAVELENGTH));
    const int w0=#min(int(#floor(w)),PROSPECT_TABLE_SIZE-2);
    const int w1=w0+1;
    w-=w0;
    ior[i]=lerp(PROSPECT_TABLE_IORS[w0],PROSPECT_TABLE_IORS[w1],w);
    k[i]=dot(lerp(PROSPECT_TABLE_ABSORPTIONS[w0],PROSPECT_TABLE_ABSORPTIONS[w1],w),contents);
  }
  const auto tau=return_from{
    const auto num=(1.236150246012*k+3.672877420834)*k+1.0;
    const auto den=((0.618075123006*k+3.664716300259)*k+4.621903634050)*k+1.0;
    return clamp(#exp(-k)*num/den,0.0,0.999);
  };
  const auto t12=return_from{
    auto tmp(-0.17369388*ior+1.31899730);
    tmp=tmp*ior-4.02936997;
    tmp=tmp*ior+6.21265658;
    tmp=tmp*ior-4.99648418;
    tmp=tmp*ior+2.66515836;
    return saturate(tmp);
  };
  const auto r12=1-t12;
  const auto t21=t12/(ior*ior);
  const auto r21=1-t21;
  const auto tAlpha=return_from{
    auto tmp(5.9796905e-01,-1.9041080e+00,1.6576156e+00);
    tmp=tmp*incident_cone_angle+auto(-4.1001221e+00,1.2956352e+01,-1.1049849e+01);
    tmp=tmp*incident_cone_angle+auto(1.1477769e+01,-3.6044872e+01,3.0242981e+01);
    tmp=tmp*incident_cone_angle+auto(-1.7172335e+01,5.3666636e+01,-4.4411331e+01);
    tmp=tmp*incident_cone_angle+auto(1.5069425e+01,-4.6911094e+01,3.8289770e+01);
    tmp=tmp*incident_cone_angle+auto(-7.8923812e+00,2.4474973e+01,-1.9667279e+01);
    tmp=tmp*incident_cone_angle+auto(2.4020134e+00,-7.4210148e+00,5.8397553e+00);
    tmp=tmp*incident_cone_angle+auto(-3.8620638e-01,1.1877490e+00,-9.0387653e-01);
    tmp=tmp*incident_cone_angle+auto(-4.8754145e-02,1.6941738e-02,1.0405082e+00);
    return saturate(tmp[0]*ior*ior+tmp[1]*ior+tmp[2]);
  };
  const auto rAlpha=1-tAlpha;
  const auto tau_r21=tau*r21;
  const auto tmp0=tau*t21/(1-#pow(tau_r21,2));
  const auto tA=tAlpha*tmp0;
  const auto rA=rAlpha+tau_r21*tA;
  const auto t=t12*tmp0;
  const auto r=r12+tau_r21*t;
  const auto add_r_t=r+t;
  const auto sub_r_t=r-t;
  const auto sub_r2_t2=r*r-t*t;
  const auto d=#sqrt((1+add_r_t)*(1+sub_r_t)*(1-add_r_t)*(1-sub_r_t));
  const auto a=(1+d+sub_r2_t2)/(2*r);
  const auto b=(1+d-sub_r2_t2)/(2*t);
  const auto bNm1=#pow(b,num_layers-1);
  const auto tmp1=#pow(a*bNm1,2)-1;
  color tSub=bNm1*(a*a-1)/tmp1;
  color rSub=a*(bNm1*bNm1-1)/tmp1;
  for(int i=0;i<$WAVELENGTH_BASE_MAX;i++){
    const auto ri=r[i];
    const auto ti=t[i];
    if(ri+ti>1){
      tSub[i]=ti/(ti+(1-ti)*(num_layers-1));
      rSub[i]=1-tSub[i];
    } else if(!isfinite(rSub[i])||!isfinite(tSub[i])){
      tSub[i]=0;
      rSub[i]=1;
    }
  }
  const auto one_minus_rSub_r=1-rSub*r;
  return prospect_result(reflectance: rA+tA*rSub*t/one_minus_rSub_r,transmittance: tA*tSub/one_minus_rSub_r);
}
)*";

static const char *const marmit = R"*(#smdl
using ::math import *;
export struct marmit_result{
  color reflectance=color(0);
  color reflectance_wet=color(0);
};
export const float MARMIT_MIN_WAVELENGTH=400.0;
export const float MARMIT_MAX_WAVELENGTH=2500.0;
export const int MARMIT_TABLE_SIZE=264;
export const float MARMIT_MIXING_EXPONENT=2.27;
export static const auto MARMIT_TABLE_ABSORPTIONS=float[264](5.8000000e-05,6.4984791e-05,7.3961977e-05,8.3942966e-05,9.3923954e-05,1.0390494e-04,1.1190875e-04,1.1989354e-04,1.2787833e-04,1.3782890e-04,1.5169582e-04,1.7349810e-04,2.0808745e-04,2.7126996e-04,3.4692395e-04,4.0768821e-04,4.2214829e-04,4.6838403e-04,5.2480989e-04,6.0760456e-04,6.7002281e-04,7.3120532e-04,8.5206084e-04,1.0678935e-03,1.4402015e-03,2.1741901e-03,2.6327338e-03,2.7589696e-03,2.8618251e-03,2.9807224e-03,3.1026730e-03,3.2533346e-03,3.4902357e-03,3.9903916e-03,4.1432776e-03,4.3053574e-03,4.6202662e-03,5.1960266e-03,6.2190532e-03,7.7595399e-03,1.0478373e-02,1.4529217e-02,2.1402958e-02,2.6493449e-02,2.7380042e-02,2.7681255e-02,2.7633087e-02,2.7165802e-02,2.6211502e-02,2.4305548e-02,2.2633673e-02,2.1824388e-02,2.2383559e-02,2.4786357e-02,3.3469194e-02,3.9156262e-02,4.1295403e-02,4.3371049e-02,4.5305894e-02,4.8309354e-02,5.2412308e-02,5.6949532e-02,6.1372152e-02,6.6089209e-02,7.2680080e-02,8.3459224e-02,1.0801803e-01,1.4987386e-01,2.0914362e-01,3.0817464e-01,4.2640746e-01,4.7426548e-01,4.8559121e-01,4.7610929e-01,4.4816959e-01,4.1265352e-01,3.7059379e-01,3.2529306e-01,2.7932866e-01,2.3837934e-01,2.0328992e-01,1.7535392e-01,1.5572677e-01,1.4497347e-01,1.4078527e-01,1.4372349e-01,1.5596563e-01,1.7441630e-01,1.9891388e-01,2.2535442e-01,2.4684612e-01,3.0258614e-01,4.3825438e-01,7.0110019e-01,9.9940737e-01,1.1558965e+00,1.2017063e+00,1.2266625e+00,1.2473806e+00,1.2683121e+00,1.2754115e+00,1.2638987e+00,1.2427951e+00,1.2082091e+00,1.1866358e+00,1.1577352e+00,1.1289543e+00,1.1030914e+00,1.0875161e+00,1.0838075e+00,1.1151354e+00,1.1700577e+00,1.2450879e+00,1.3620820e+00,1.5408251e+00,1.7737077e+00,2.0738001e+00,2.4385417e+00,2.8046537e+00,3.2404328e+00,3.6110189e+00,4.0831335e+00,4.9678182e+00,6.9821837e+00,1.0777019e+01,1.5809918e+01,2.0957662e+01,2.4918992e+01,2.7547179e+01,2.9242555e+01,3.0198566e+01,3.0537570e+01,3.0391875e+01,2.9675172e+01,2.8037443e+01,2.5813958e+01,2.3617450e+01,2.1401333e+01,1.9255608e+01,1.7387155e+01,1.5678046e+01,1.4165977e+01,1.2875868e+01,1.1674949e+01,1.0679736e+01,9.8499423e+00,9.1589774e+00,8.5319908e+00,7.9861643e+00,7.5072635e+00,7.0878586e+00,6.7712775e+00,6.5171763e+00,6.2906116e+00,6.0902196e+00,5.9474330e+00,5.7936615e+00,5.6691350e+00,5.6119709e+00,5.5547171e+00,5.5294190e+00,5.5225403e+00,5.5265036e+00,5.5934486e+00,5.7201826e+00,5.8667218e+00,6.0657560e+00,6.3052816e+00,6.6346160e+00,7.0347278e+00,7.5043570e+00,7.9664502e+00,8.3214848e+00,8.5709506e+00,8.7513499e+00,8.8025903e+00,8.7476202e+00,8.7948996e+00,8.8635308e+00,9.0019142e+00,9.4151059e+00,1.0219798e+01,1.1683006e+01,1.4391653e+01,1.9719141e+01,2.9744088e+01,4.8905897e+01,7.4663711e+01,1.0024685e+02,1.1730163e+02,1.2651509e+02,1.3039706e+02,1.2942913e+02,1.2475055e+02,1.1794670e+02,1.0989923e+02,1.0129867e+02,9.3107823e+01,8.5246802e+01,7.8192875e+01,7.1596133e+01,6.5692118e+01,6.0475897e+01,5.5783826e+01,5.1541361e+01,4.7683278e+01,4.4360006e+01,4.1224355e+01,3.8385488e+01,3.5684155e+01,3.3401698e+01,3.1338720e+01,2.9394575e+01,2.7600382e+01,2.6036600e+01,2.4730099e+01,2.3484923e+01,2.2429970e+01,2.1549691e+01,2.0788565e+01,2.0125026e+01,1.9598560e+01,1.9200902e+01,1.8808893e+01,1.8470427e+01,1.8359254e+01,1.8337472e+01,1.8283925e+01,1.8377094e+01,1.8521219e+01,1.8720550e+01,1.9104502e+01,1.9542117e+01,2.0080917e+01,2.0747716e+01,2.1492014e+01,2.2285338e+01,2.3318404e+01,2.4377130e+01,2.5544056e+01,2.6859973e+01,2.8263872e+01,2.9755440e+01,3.1504263e+01,3.3175559e+01,3.4931125e+01,3.7040435e+01,3.9193771e+01,4.1539909e+01,4.3948561e+01,4.6437483e+01,4.9000875e+01,5.1653323e+01,5.4677894e+01,5.8138646e+01,6.1514553e+01,6.5436050e+01,6.9238078e+01,7.3853932e+01,7.8420393e+01,8.2970777e+01,8.7095007e+01,9.2042710e+01,9.5303000e+01);
@(noinline)
export marmit_result marmit(
  color reflectance=color(0.3),
  float water_thickness=0.01,
  float wet_fraction=1.0,
  float suspension_ior=1.53,
  float suspension_k=0.0,
  float suspension_fraction=0.0,
){
  color absorption(0);
  color s(0);
  for(int i=0;i<$WAVELENGTH_BASE_MAX;i++){
    const float frac=saturate(($state.wavelength_base[i]-MARMIT_MIN_WAVELENGTH)/(MARMIT_MAX_WAVELENGTH-MARMIT_MIN_WAVELENGTH));
    float w=(MARMIT_TABLE_SIZE-1)*frac;
    const int w0=#min(int(#floor(w)),MARMIT_TABLE_SIZE-2);
    w-=w0;
    absorption[i]=lerp(MARMIT_TABLE_ABSORPTIONS[w0],MARMIT_TABLE_ABSORPTIONS[w0+1],w);
    s[i]=2.0*frac-1.0;
  }
  const auto ior=return_from{
    auto n=-0.0000601351*s-0.0248482921;
    n=n*s-0.0005980717;
    n=n*s+0.0015633866;
    n=n*s-0.0109188837;
    n=n*s-0.0244547175;
    n=n*s+1.3129684894;
    return n;
  };
  color n=ior;
  color alpha=absorption;
  if(suspension_fraction>0.0){
    const auto d=suspension_fraction;
    const auto lambda=(MARMIT_MIN_WAVELENGTH+0.5*(s+1.0)*(MARMIT_MAX_WAVELENGTH-MARMIT_MIN_WAVELENGTH))*1.0e-7;
    const auto k_w=absorption*lambda*0.07957747154594767;
    const auto ew_re=ior*ior-k_w*k_w;
    const auto ew_im=2.0*ior*k_w;
    const float ei_re=suspension_ior*suspension_ior-suspension_k*suspension_k;
    const float ei_im=2.0*suspension_ior*suspension_k;
    const auto e_re=d*ei_re+(1.0-d)*ew_re;
    const auto e_im=d*ei_im+(1.0-d)*ew_im;
    const auto mag=#sqrt(e_re*e_re+e_im*e_im);
    n=#sqrt(0.5*(mag+e_re));
    const auto k=e_im/(2.0*n);
    alpha=12.566370614359172*k/lambda;
  }
  const auto r12=return_from{
    const auto v=(n-1.0)/(n+1.0);
    const auto num=(((-6.087330777978*v+5.072538780150)*v-2.292800947895)*v+5.106344072818)*v+0.666666666667;
    const auto den=(-14.079305083126*v+15.544722876889)*v+1.0;
    return saturate(v*num/den);
  };
  const auto t12=1-r12;
  const auto t21=t12/(n*n);
  const auto r21=1-t21;
  const auto tau=return_from{
    const auto x=#min(alpha*water_thickness,50.0);
    const auto num=(1.236150246012*x+3.672877420834)*x+1.0;
    const auto den=((0.618075123006*x+3.664716300259)*x+4.621903634050)*x+1.0;
    return saturate(#exp(-x)*num/den);
  };
  const auto tau2_reflectance=tau*tau*reflectance;
  const auto wet=saturate(t12*t21*tau2_reflectance/(1-r21*tau2_reflectance));
  const auto e=1.0/MARMIT_MIXING_EXPONENT;
  const auto mixed=#pow(wet_fraction*#pow(wet,e)+(1.0-wet_fraction)*#pow(reflectance,e),MARMIT_MIXING_EXPONENT);
  return marmit_result(reflectance: saturate(mixed),reflectance_wet: wet);
}
export const float SOIL_MIN_WAVELENGTH=400.0;
export const float SOIL_MAX_WAVELENGTH=2298.0;
export const int SOIL_TABLE_SIZE=261;
export static const auto SOIL_CURVES=auto[261](
  auto(-0.9151818,2.283778,-13.49007,-4.426793,-1.551209,0.814577),
  auto(-0.7850408,2.382168,-12.47427,-0.805078,-1.536577,0.790004),
  auto(-0.711167,2.401793,-11.53958,3.003254,-1.544359,0.7903268),
  auto(-0.6307067,2.424772,-10.61618,6.254494,-1.559712,0.7957837),
  auto(-0.5293845,2.47108,-9.666577,9.064483,-1.568295,0.7951231),
  auto(-0.4272342,2.515677,-8.83361,11.32061,-1.588384,0.7997233),
  auto(-0.3389463,2.553098,-8.171594,12.66647,-1.603,0.8024464),
  auto(-0.2641774,2.592648,-7.734584,12.91788,-1.615885,0.8052612),
  auto(-0.2038002,2.62935,-7.511543,12.56061,-1.634227,0.8115278),
  auto(-0.1538948,2.661665,-7.377611,12.13367,-1.650354,0.8173005),
  auto(-0.1100589,2.687321,-7.235088,11.9409,-1.659466,0.8199032),
  auto(-0.06440248,2.707992,-6.971191,12.11937,-1.664921,0.8201567),
  auto(-0.009157672,2.729442,-6.539579,12.77451,-1.671097,0.8202849),
  auto(0.05567357,2.753561,-5.947066,13.77311,-1.676495,0.8202885),
  auto(0.1243609,2.777657,-5.290834,14.93138,-1.682931,0.8213536),
  auto(0.1931956,2.80034,-4.593732,16.24571,-1.685853,0.8202112),
  auto(0.26207,2.823601,-3.896951,17.50369,-1.686915,0.8179418),
  auto(0.3289154,2.844188,-3.210456,18.57238,-1.688772,0.8165839),
  auto(0.3953706,2.864766,-2.511113,19.36731,-1.690196,0.8156624),
  auto(0.4639461,2.888989,-1.781895,19.70381,-1.689949,0.8139852),
  auto(0.5354471,2.919157,-1.045868,19.35622,-1.68829,0.8118492),
  auto(0.6088107,2.954526,-0.3307201,18.2204,-1.686703,0.8102459),
  auto(0.682543,2.994094,0.3526001,16.43386,-1.684881,0.8087685),
  auto(0.7531942,3.03387,0.9893881,14.27339,-1.683497,0.8071628),
  auto(0.8172792,3.070379,1.552804,12.04023,-1.6826,0.8052986),
  auto(0.875023,3.103361,2.036196,9.915022,-1.682795,0.8039127),
  auto(0.9277053,3.134143,2.448965,8.027644,-1.684348,0.8034778),
  auto(0.9707498,3.156911,2.78983,6.550345,-1.685705,0.8030112),
  auto(1.005055,3.17205,3.058204,5.469606,-1.686161,0.8019293),
  auto(1.033974,3.182702,3.289677,4.608479,-1.686624,0.8009482),
  auto(1.060697,3.191295,3.495767,3.921636,-1.687846,0.8003531),
  auto(1.086048,3.198903,3.679302,3.345579,-1.689045,0.7996181),
  auto(1.111642,3.206352,3.829945,2.803375,-1.690115,0.7988123),
  auto(1.135911,3.211201,3.963732,2.330015,-1.691838,0.7982304),
  auto(1.157946,3.211518,4.088785,1.948732,-1.692922,0.7971265),
  auto(1.179393,3.211009,4.217649,1.606013,-1.693149,0.7955015),
  auto(1.201084,3.210497,4.340768,1.266403,-1.693375,0.7939022),
  auto(1.222036,3.208874,4.463207,0.9126484,-1.694084,0.7926506),
  auto(1.242339,3.2065,4.589447,0.5713673,-1.694318,0.7912023),
  auto(1.262691,3.204462,4.72209,0.2535055,-1.693896,0.7894044),
  auto(1.2832,3.202975,4.856764,-0.05937947,-1.693452,0.7876107),
  auto(1.303256,3.201243,4.9949,-0.3283114,-1.693686,0.7862567),
  auto(1.322445,3.198659,5.141041,-0.5208041,-1.693932,0.7848732),
  auto(1.340483,3.194819,5.2879,-0.6608467,-1.693877,0.7832721),
  auto(1.357297,3.189634,5.424268,-0.7595797,-1.693676,0.7815585),
  auto(1.372691,3.182988,5.549252,-0.8101767,-1.693525,0.7798879),
  auto(1.38727,3.175837,5.661009,-0.8180354,-1.693373,0.7782625),
  auto(1.401151,3.168462,5.752506,-0.7893489,-1.693595,0.7769536),
  auto(1.41377,3.160426,5.821635,-0.7039582,-1.693845,0.775718),
  auto(1.424672,3.151264,5.869364,-0.5670439,-1.693777,0.7743149),
  auto(1.434252,3.141482,5.8976,-0.393035,-1.69379,0.7728767),
  auto(1.442752,3.131628,5.914393,-0.2000732,-1.69415,0.7715868),
  auto(1.449916,3.121558,5.916482,-0.004931163,-1.695017,0.7705945),
  auto(1.455739,3.111372,5.904179,0.1928357,-1.696067,0.7697467),
  auto(1.46001,3.100912,5.876646,0.3801837,-1.696868,0.7688117),
  auto(1.463347,3.090812,5.833174,0.5259147,-1.697594,0.7678747),
  auto(1.465786,3.080691,5.775986,0.6450709,-1.698703,0.7671648),
  auto(1.467288,3.070301,5.708103,0.7498011,-1.700172,0.7666672),
  auto(1.467426,3.059026,5.631748,0.8321612,-1.701797,0.7662086),
  auto(1.46651,3.047107,5.550948,0.8963911,-1.703489,0.7657196),
  auto(1.465147,3.035176,5.465855,0.9404374,-1.70506,0.7652171),
  auto(1.464106,3.024424,5.37403,0.9325192,-1.706523,0.7647793),
  auto(1.463025,3.014028,5.280285,0.897839,-1.708433,0.7646808),
  auto(1.46178,3.003607,5.191277,0.867356,-1.7108,0.7648726),
  auto(1.460891,2.993649,5.110038,0.8426526,-1.71287,0.7648363),
  auto(1.460507,2.984311,5.034524,0.7929246,-1.714588,0.7645914),
  auto(1.460248,2.974925,4.959947,0.7130314,-1.716354,0.7643846),
  auto(1.46001,2.965373,4.891171,0.6190625,-1.717949,0.7641781),
  auto(1.460589,2.956498,4.838689,0.5378992,-1.719504,0.7639442),
  auto(1.462032,2.948211,4.808182,0.462024,-1.72081,0.7634809),
  auto(1.463497,2.93973,4.788946,0.3696248,-1.72208,0.7629441),
  auto(1.464162,2.930228,4.775207,0.2503054,-1.723848,0.7627158),
  auto(1.464865,2.920359,4.781424,0.1228078,-1.727045,0.7630331),
  auto(1.465316,2.909525,4.816746,0.01791617,-1.731032,0.7634438),
  auto(1.464353,2.896053,4.871728,-0.07559254,-1.736669,0.7642999),
  auto(1.460863,2.878946,4.946042,-0.1813301,-1.744046,0.7653614),
  auto(1.455852,2.859951,5.038731,-0.2536596,-1.751624,0.7663319),
  auto(1.452961,2.844304,5.11355,-0.2840609,-1.757096,0.7668397),
  auto(1.454024,2.834318,5.144387,-0.3674996,-1.760376,0.7672764),
  auto(1.456539,2.826885,5.147148,-0.4749115,-1.761648,0.7673888),
  auto(1.459763,2.820617,5.140437,-0.5461433,-1.763469,0.7681572),
  auto(1.46532,2.817353,5.140063,-0.5746147,-1.764108,0.7683857),
  auto(1.471753,2.816864,5.174792,-0.5331267,-1.761899,0.767257),
  auto(1.478046,2.81717,5.211176,-0.4520493,-1.759031,0.7663818),
  auto(1.485114,2.817938,5.213331,-0.4599377,-1.758017,0.7665509),
  auto(1.492226,2.818455,5.191352,-0.5153157,-1.756016,0.7662815),
  auto(1.498295,2.817497,5.173855,-0.5069894,-1.754152,0.7659634),
  auto(1.503537,2.815942,5.159364,-0.4609303,-1.752881,0.7661798),
  auto(1.508144,2.813593,5.154572,-0.4255018,-1.751137,0.7658383),
  auto(1.511876,2.809826,5.157527,-0.3735715,-1.749301,0.7651133),
  auto(1.515334,2.805164,5.175236,-0.304516,-1.748672,0.7649018),
  auto(1.51821,2.799417,5.200896,-0.2292289,-1.748886,0.7651114),
  auto(1.521423,2.79339,5.232192,-0.1518506,-1.749847,0.7654355),
  auto(1.523932,2.78617,5.275841,-0.03826191,-1.751033,0.7657966),
  auto(1.525845,2.778266,5.329178,0.08061486,-1.752496,0.7662997),
  auto(1.526827,2.769073,5.38763,0.1999053,-1.754706,0.7669116),
  auto(1.526819,2.758386,5.453147,0.3462227,-1.757497,0.7676471),
  auto(1.525958,2.746508,5.524169,0.5422224,-1.760505,0.7683451),
  auto(1.524847,2.734143,5.598205,0.7573369,-1.764269,0.7693521),
  auto(1.522348,2.719715,5.687076,0.9565574,-1.769362,0.7705151),
  auto(1.515535,2.698654,5.824516,1.191041,-1.779031,0.772755),
  auto(1.501578,2.666548,6.034357,1.505577,-1.795419,0.7763787),
  auto(1.482583,2.627149,6.283827,1.882215,-1.816111,0.7809732),
  auto(1.465352,2.591375,6.49938,2.236955,-1.833959,0.7848683),
  auto(1.454335,2.565724,6.638111,2.563542,-1.844739,0.7873179),
  auto(1.449005,2.548949,6.713505,2.850522,-1.849383,0.7883358),
  auto(1.446533,2.536561,6.759821,3.109434,-1.851511,0.7887095),
  auto(1.444869,2.5256,6.793192,3.338288,-1.853422,0.7890029),
  auto(1.443775,2.515495,6.825941,3.542049,-1.855649,0.7893918),
  auto(1.44351,2.506677,6.855321,3.726272,-1.857338,0.7896429),
  auto(1.443974,2.499158,6.872754,3.907879,-1.857703,0.7894396),
  auto(1.44587,2.493922,6.880793,4.062723,-1.857231,0.7890011),
  auto(1.448416,2.489869,6.884453,4.2015,-1.856215,0.7883998),
  auto(1.450439,2.485405,6.882706,4.342693,-1.855088,0.7878545),
  auto(1.452318,2.481093,6.875985,4.47024,-1.853788,0.787225),
  auto(1.454841,2.477786,6.86891,4.584654,-1.852993,0.7868916),
  auto(1.457285,2.474497,6.861032,4.695634,-1.852233,0.7865203),
  auto(1.459039,2.470449,6.854436,4.812594,-1.851455,0.7860628),
  auto(1.459481,2.464758,6.851899,4.948432,-1.851029,0.7856679),
  auto(1.458692,2.457234,6.857134,5.112715,-1.852024,0.7857204),
  auto(1.456609,2.448044,6.868737,5.283012,-1.854726,0.786239),
  auto(1.452486,2.436602,6.887944,5.461747,-1.858732,0.7869926),
  auto(1.446,2.422216,6.919619,5.655106,-1.864327,0.7880607),
  auto(1.43836,2.405832,6.967322,5.844761,-1.872523,0.7899293),
  auto(1.429728,2.387569,7.028271,6.023344,-1.883984,0.7929213),
  auto(1.418921,2.366015,7.10488,6.207524,-1.898867,0.7970837),
  auto(1.405012,2.34024,7.200481,6.434186,-1.91746,0.8023438),
  auto(1.388319,2.310936,7.31233,6.719343,-1.939355,0.8084536),
  auto(1.370546,2.280662,7.42838,7.023726,-1.963485,0.8151855),
  auto(1.352772,2.251242,7.531004,7.321993,-1.987644,0.8219637),
  auto(1.335592,2.223958,7.596335,7.619175,-2.010268,0.8284344),
  auto(1.318956,2.198507,7.631135,7.901733,-2.031912,0.8347437),
  auto(1.300707,2.171336,7.675601,8.164066,-2.057166,0.8422913),
  auto(1.275967,2.137303,7.733169,8.449005,-2.094932,0.8540319),
  auto(1.237829,2.090657,7.780321,8.807313,-2.161638,0.8758602),
  auto(1.183334,2.03087,7.811075,9.246156,-2.271073,0.9132556),
  auto(1.117757,1.966537,7.819188,9.725477,-2.420157,0.9663186),
  auto(1.05308,1.912507,7.776255,10.08484,-2.580222,1.025323),
  auto(1.001056,1.876051,7.698883,10.488,-2.718991,1.077804),
  auto(0.9722544,1.85169,7.703128,11.21053,-2.827625,1.11964),
  auto(0.9672355,1.834181,7.850521,12.1505,-2.913722,1.153155),
  auto(0.9706617,1.819838,8.009032,13.00699,-2.97912,1.1786),
  auto(0.9692203,1.805263,8.074962,13.76545,-3.022729,1.195381),
  auto(0.9664544,1.793051,8.090596,14.34402,-3.048827,1.205121),
  auto(0.969351,1.78998,8.102828,14.61958,-3.061816,1.209449),
  auto(0.9766952,1.793651,8.119941,14.66112,-3.061545,1.208412),
  auto(0.9855105,1.798729,8.140231,14.54079,-3.044306,1.200599),
  auto(0.9954505,1.803303,8.16037,14.36316,-3.009596,1.185808),
  auto(1.006046,1.807334,8.176933,14.21363,-2.962472,1.166245),
  auto(1.016784,1.81159,8.186724,14.08082,-2.908803,1.144446),
  auto(1.028947,1.818234,8.190795,13.90455,-2.852579,1.121877),
  auto(1.042465,1.826988,8.188459,13.70851,-2.796342,1.099501),
  auto(1.054954,1.834969,8.176219,13.54918,-2.741066,1.077794),
  auto(1.065955,1.841942,8.155699,13.38509,-2.687242,1.056924),
  auto(1.076034,1.848704,8.130314,13.20855,-2.635708,1.037217),
  auto(1.085067,1.854828,8.098246,13.05797,-2.5873,1.018991),
  auto(1.093691,1.8611,8.055872,12.93993,-2.542354,1.002292),
  auto(1.103578,1.869735,8.010327,12.78865,-2.500946,0.9869907),
  auto(1.11479,1.880402,7.972116,12.56268,-2.462834,0.9728733),
  auto(1.126019,1.891188,7.938168,12.30377,-2.428251,0.9601111),
  auto(1.136175,1.900868,7.900056,12.06867,-2.397254,0.9488196),
  auto(1.144843,1.908863,7.856043,11.89077,-2.369772,0.9389373),
  auto(1.152327,1.915434,7.815161,11.75489,-2.345576,0.9303168),
  auto(1.159049,1.921212,7.776633,11.63077,-2.324044,0.9226224),
  auto(1.165245,1.926542,7.739017,11.50614,-2.304824,0.9157219),
  auto(1.170829,1.931048,7.700141,11.38885,-2.288198,0.9097681),
  auto(1.175677,1.934817,7.660108,11.27579,-2.273674,0.9046025),
  auto(1.179713,1.93773,7.61923,11.17786,-2.260736,0.8999892),
  auto(1.183181,1.939995,7.580654,11.09427,-2.249436,0.8959923),
  auto(1.186129,1.941676,7.542206,11.02146,-2.23956,0.8924673),
  auto(1.188638,1.943083,7.505574,10.94099,-2.231006,0.8894432),
  auto(1.191134,1.944762,7.470481,10.84663,-2.223799,0.8869496),
  auto(1.193195,1.946088,7.439166,10.75598,-2.2179,0.8849111),
  auto(1.194471,1.946792,7.410442,10.66694,-2.212779,0.8830473),
  auto(1.194552,1.946357,7.381351,10.57935,-2.208764,0.8816141),
  auto(1.19378,1.944829,7.349298,10.51631,-2.206306,0.8808204),
  auto(1.191958,1.941605,7.32017,10.4839,-2.205597,0.8806395),
  auto(1.188687,1.93645,7.2969,10.46342,-2.20623,0.8808269),
  auto(1.184131,1.929704,7.279645,10.45959,-2.208765,0.8817592),
  auto(1.178672,1.92204,7.262741,10.47767,-2.213337,0.8834597),
  auto(1.171689,1.913072,7.247627,10.51923,-2.219599,0.8856322),
  auto(1.162746,1.902102,7.237786,10.60801,-2.228295,0.8886932),
  auto(1.152671,1.889548,7.242041,10.73635,-2.240307,0.8930278),
  auto(1.141951,1.875702,7.256368,10.87796,-2.255988,0.89868),
  auto(1.130928,1.861276,7.274232,11.02611,-2.274972,0.90539),
  auto(1.121605,1.84927,7.296228,11.14638,-2.296984,0.9130588),
  auto(1.114348,1.840144,7.326384,11.20315,-2.320552,0.9211711),
  auto(1.108128,1.831987,7.351123,11.23454,-2.34418,0.9294926),
  auto(1.102182,1.823518,7.368574,11.27778,-2.365227,0.937083),
  auto(1.098343,1.818064,7.359896,11.27648,-2.379852,0.9422834),
  auto(1.097013,1.815682,7.330415,11.20999,-2.388014,0.9449906),
  auto(1.096722,1.81428,7.304751,11.14139,-2.391478,0.9460472),
  auto(1.096576,1.812402,7.280331,11.0806,-2.3928,0.9465954),
  auto(1.094976,1.80773,7.252409,11.02742,-2.39402,0.9472784),
  auto(1.093799,1.804108,7.239388,10.98546,-2.396265,0.9482824),
  auto(1.091834,1.800554,7.210868,10.92829,-2.400678,0.9496868),
  auto(1.087457,1.794388,7.144293,10.78745,-2.410853,0.953233),
  auto(1.080758,1.785541,7.077229,10.73619,-2.431505,0.9606972),
  auto(1.072565,1.77579,7.064251,10.94505,-2.469178,0.9745515),
  auto(1.060285,1.76409,7.07072,11.27762,-2.532281,0.9979203),
  auto(1.040582,1.748333,7.082612,11.85151,-2.635295,1.036331),
  auto(1.009679,1.730013,7.032708,12.84579,-2.799297,1.098805),
  auto(0.9659397,1.711865,6.90101,14.04508,-3.039568,1.193924),
  auto(0.9067375,1.696368,6.679914,15.31782,-3.346034,1.320534),
  auto(0.8313722,1.688121,6.345857,16.45522,-3.671541,1.461434),
  auto(0.7399556,1.678724,5.895899,17.20813,-3.952715,1.589111),
  auto(0.6529989,1.666224,5.438233,17.3052,-4.144138,1.680632),
  auto(0.5885108,1.645985,5.142361,17.41146,-4.254957,1.735491),
  auto(0.555857,1.627397,5.056991,17.76982,-4.324936,1.770271),
  auto(0.5415461,1.608725,5.084904,18.36137,-4.365726,1.789287),
  auto(0.5412635,1.597178,5.132031,18.7075,-4.379921,1.794936),
  auto(0.5516561,1.593406,5.204692,18.77208,-4.366875,1.78758),
  auto(0.569859,1.593919,5.283598,18.49183,-4.338261,1.773291),
  auto(0.5928456,1.596731,5.356656,18.11543,-4.298944,1.753163),
  auto(0.6152951,1.597959,5.429453,17.71499,-4.250752,1.729336),
  auto(0.6352867,1.597429,5.494174,17.33104,-4.190242,1.700376),
  auto(0.655876,1.597761,5.543916,16.85631,-4.125736,1.669942),
  auto(0.6796214,1.60124,5.596467,16.43944,-4.062548,1.640719),
  auto(0.7066568,1.608527,5.655932,15.87048,-3.999505,1.611317),
  auto(0.7360771,1.618737,5.726564,15.28016,-3.934664,1.580604),
  auto(0.7627474,1.626352,5.79563,14.80498,-3.870591,1.549853),
  auto(0.7835282,1.630388,5.82516,14.40151,-3.807412,1.521679),
  auto(0.801762,1.634277,5.85066,13.97652,-3.744506,1.493892),
  auto(0.8169483,1.635699,5.874354,13.65573,-3.682124,1.467024),
  auto(0.8311113,1.638271,5.8805,13.28168,-3.621186,1.440899),
  auto(0.8418513,1.639659,5.850949,12.81035,-3.560506,1.415102),
  auto(0.8513431,1.6414,5.826125,12.2828,-3.500807,1.388541),
  auto(0.8611639,1.644465,5.807126,11.79906,-3.443135,1.363415),
  auto(0.8711503,1.648399,5.786364,11.39136,-3.386427,1.339383),
  auto(0.8778338,1.648433,5.770034,11.07808,-3.330598,1.31691),
  auto(0.8843499,1.648844,5.754794,10.7696,-3.27638,1.294492),
  auto(0.8911554,1.650143,5.730013,10.38287,-3.225518,1.273276),
  auto(0.8977678,1.652946,5.661777,9.957204,-3.176067,1.252869),
  auto(0.9040429,1.65577,5.593498,9.561231,-3.128881,1.23369),
  auto(0.9088211,1.656907,5.541014,9.155375,-3.082221,1.214735),
  auto(0.9128085,1.655734,5.507279,8.797057,-3.03993,1.197842),
  auto(0.9171685,1.654606,5.457231,8.556242,-3.004173,1.184089),
  auto(0.9197558,1.653782,5.413213,8.492203,-2.96922,1.170043),
  auto(0.9193677,1.655189,5.362508,8.49759,-2.933797,1.155355),
  auto(0.9141371,1.658041,5.265819,8.488946,-2.896807,1.140178),
  auto(0.9024309,1.659337,5.131184,8.441141,-2.858515,1.124489),
  auto(0.8859531,1.658196,4.980843,8.373348,-2.819287,1.107351),
  auto(0.8679229,1.656327,4.85027,8.241505,-2.784427,1.092405),
  auto(0.8478266,1.652984,4.73271,8.070289,-2.753096,1.079643),
  auto(0.8239002,1.648382,4.602087,7.793358,-2.722216,1.066949),
  auto(0.7973245,1.642644,4.428226,7.511592,-2.693339,1.054843),
  auto(0.7751088,1.644148,4.243428,7.281522,-2.666147,1.04343),
  auto(0.7634545,1.648665,4.15917,6.9982,-2.64904,1.036296),
  auto(0.7678985,1.649358,4.333288,6.973814,-2.649686,1.037527),
  auto(0.7860472,1.648512,4.651645,7.402009,-2.664951,1.044222),
  auto(0.808223,1.656774,4.882949,7.968898,-2.678621,1.048766),
  auto(0.8186658,1.663424,4.958608,8.350281,-2.686561,1.05125),
  auto(0.8157203,1.661578,4.947163,8.666169,-2.693006,1.054449),
  auto(0.8069019,1.655573,4.913877,8.915213,-2.699829,1.057612),
  auto(0.7990204,1.650569,4.883731,9.11409,-2.708561,1.060795),
  auto(0.7917028,1.64413,4.88013,9.397324,-2.722386,1.066543),
  auto(0.7869081,1.640037,4.854711,9.570004,-2.739885,1.073439),
  auto(0.7792578,1.635717,4.792817,9.491983,-2.755607,1.079045),
  auto(0.7685891,1.630584,4.747445,9.486741,-2.771483,1.084703),
  auto(0.7544413,1.622808,4.731509,9.668162,-2.787866,1.090443),
  auto(0.7459775,1.624409,4.653887,9.709371,-2.808524,1.098035),
);
@(noinline)
export color soil_albedo(
  float humus=0.5,
  float iron=0.5,
  float aridity=0.5,
  float moisture=0.0,
){
  const float h=saturate(humus);
  const float fe=saturate(iron);
  const float ar=saturate(aridity);
  const float m=saturate(moisture);
  const float lightness=1.0-h;
  const float chroma=#pow(fe,#exp(0.45*(h-0.5)-0.55*(ar-0.5)));
  const float redness=ar;
  const float l=lerp(-1.138774,-0.2968323,lightness);
  const float c=lerp(-0.04893016,0.0518981,chroma);
  const float r=redness<=0.5?0.01466600*(1.0-2.0*redness):-0.02271506*(2.0*redness-1.0);
  const float warp=((3.89536*m-6.037163)*m+3.081546)*m+0.06025699;
  const float wet=1.04202*(warp*m);
  const auto weights=auto(1.0,l,c,r,wet,wet*wet);
  color result(0);
  for(int i=0;i<$WAVELENGTH_BASE_MAX;i++){
    float t=(SOIL_TABLE_SIZE-1)*saturate(($state.wavelength_base[i]-SOIL_MIN_WAVELENGTH)/(SOIL_MAX_WAVELENGTH-SOIL_MIN_WAVELENGTH));
    const int t0=#min(int(#floor(t)),SOIL_TABLE_SIZE-2);
    t-=t0;
    result[i]=1.0/(1.0+#exp(-dot(lerp(SOIL_CURVES[t0],SOIL_CURVES[t0+1],t),weights)));
  }
  return result;
}
)*";

static const char *const math = R"*(#smdl
export const float PI=$PI;
export const float TWO_PI=$TWO_PI;
export const float HALF_PI=$HALF_PI;
@(macro)
export auto abs(const auto a)=#abs(a);
@(macro)
export auto all(const auto a)=#all(a);
@(macro)
export auto any(const auto a)=#any(a);
@(macro)
export auto max(const auto a,const auto b)=#max(a,b);
@(macro)
export auto min(const auto a,const auto b)=#min(a,b);
@(macro)
export auto clamp(const auto a,const auto min,const auto max)=#max(min,#min(a,max));
@(macro)
export auto saturate(const auto a)=clamp(a,0.0,1.0);
@(macro)
export auto floor(const auto a)=#floor(a);
@(macro)
export auto ceil(const auto a)=#ceil(a);
@(macro)
export auto round(const auto a)=#round(a);
@(macro)
export auto trunc(const auto a)=#trunc(a);
@(macro)
export auto frac(const auto a)=a-#floor(a);
@(macro)
export auto fmod(const auto a,const auto b)=a%b;
@(macro)
export auto modf(const auto a)=auto[2](a0:=#trunc(a),a-a0);
@(macro)
export auto isfinite(const auto a)=#isfpclass(a,0b0111111000);
@(macro)
export auto isnormal(const auto a)=#isfpclass(a,0b0100001000);
@(macro)
export auto isinf(const auto a)=#isfpclass(a,0b1000000100);
@(macro)
export auto isnan(const auto a)=#isfpclass(a,0b0000000011);
@(macro)
export auto sign(const auto a)=#sign(a);
@(macro)
export auto sqrt(const auto a)=#sqrt(a);
@(macro)
export auto rsqrt(const auto a)=1.0/#sqrt(a);
@(macro)
export auto pow(const auto a,const auto b)=#pow(a,b);
@(macro)
export auto cos(const auto a)=#cos(a);
@(macro)
export auto sin(const auto a)=#sin(a);
@(macro)
export auto tan(const auto a)=#tan(a);
@(macro)
export auto acos(const auto a)=#acos(a);
@(macro)
export auto asin(const auto a)=#asin(a);
@(macro)
export auto atan(const auto a)=#atan(a);
@(macro)
export auto atan2(const auto y,const auto x)=#atan2(y,x);
@(macro)
export auto cosh(const auto a)=#cosh(a);
@(macro)
export auto sinh(const auto a)=#sinh(a);
@(macro)
export auto tanh(const auto a)=#tanh(a);
@(macro)
export auto sincos(const auto a)=auto[2](#sin(a),#cos(a));
@(macro)
export auto radians(const auto a)=a*(PI/180.0);
@(macro)
export auto degrees(const auto a)=a*(180.0/PI);
@(macro)
export auto exp(const auto a)=#exp(a);
@(macro)
export auto exp2(const auto a)=#exp2(a);
@(macro)
export auto exp10(const auto a)=#exp10(a);
@(macro)
export auto log(const auto a)=#log(a);
@(macro)
export auto log2(const auto a)=#log2(a);
@(macro)
export auto log10(const auto a)=#log10(a);
@(macro)
export auto min_value(const auto a)=#min_value(a);
@(macro)
export auto max_value(const auto a)=#max_value(a);
@(pure)
export float min_value_wavelength(const color a){
  int imin=0;
  float amin=a[0];
  for(int i=1;i<$WAVELENGTH_BASE_MAX;i++){
    if(amin>a[i]){
      amin=a[i];
      imin=i;
    }
  }
  return $state.wavelength_base[imin];
}
@(pure)
export float max_value_wavelength(const color a){
  int imax=0;
  float amax=a[0];
  for(int i=1;i<$WAVELENGTH_BASE_MAX;i++){
    if(amax<a[i]){
      amax=a[i];
      imax=i;
    }
  }
  return $state.wavelength_base[imax];
}
@(macro)
export auto average(const auto a)=#sum(a)/#num(a);
@(macro)
export auto lerp(const auto a,const auto b,const auto l)=(1.0-l)*a+l*b;
@(macro)
export auto step(const auto a,const auto b)=#select(b<a,0.0,1.0);
@(macro)
export auto smoothstep(const auto a,const auto b,const auto l){
  const auto t(saturate(l));
  const auto s(1-t);
  return s*s*(1+2*t)*a+t*t*(1+2*s)*b;
}
@(macro)
export auto dot(const auto a,const auto b)=#sum(a*b);
@(macro)
export auto length(const auto a)=#sqrt(#sum(a*a));
@(macro)
export auto normalize(const auto a)=a*(1/length(a));
@(macro)
export auto distance(const auto a,const auto b)=length(b-a);
@(macro)
export auto cross(const auto a,const auto b)=a.yzx*b.zxy-a.zxy*b.yzx;
@(macro)
export auto transpose(const auto a)=#transpose(a);
@(macro)
export float luminance(const float3 a)=dot(float3(0.2126,0.7152,0.0722),a);
@(noinline)
export float luminance(const color a){
  float result(0.0);
  for(int i=0;i<$WAVELENGTH_BASE_MAX;++i){
    result+=_wyman_y($state.wavelength_base[i])*a[i];
  }
  return result/$WAVELENGTH_BASE_MAX;
}
@(noinline)
export color blackbody(const float temperature){
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
export float eval_at_wavelength(color a,float wavelength){
  if$($WAVELENGTH_BASE_MAX==1){
    return a[0];
  } else {
    return _polyline_lerp($WAVELENGTH_BASE_MAX,&$state.wavelength_base[0],&a[0],wavelength);
  }
}
)*";

static const char *const scene = R"*(#smdl
@(foreign pure)
int smdlDataExists(&void sceneData,string name);
@(foreign)
void smdlDataLookup(&void sceneData,string name,int kind,int size,&void result);
@(macro)
auto data_lookup(const string name,auto value){
  const int kind=#is_arithmetic_integral(value)?0:#is_arithmetic_floating_point(value)?1:2;
  smdlDataLookup($SCENE_DATA,name,kind,#num(value),cast<&void>(&value));
  return value;
}
@(macro)
export bool data_isvalid(const string name)=smdlDataExists($SCENE_DATA,name)!=0;
@(macro)
export int data_lookup_int(const string name,int default_value=int())=data_lookup(name,default_value);
@(macro)
export int2 data_lookup_int2(const string name,int2 default_value=int2())=data_lookup(name,default_value);
@(macro)
export int3 data_lookup_int3(const string name,int3 default_value=int3())=data_lookup(name,default_value);
@(macro)
export int4 data_lookup_int4(const string name,int4 default_value=int4())=data_lookup(name,default_value);
@(macro)
export float data_lookup_float(const string name,float default_value=float())=data_lookup(name,default_value);
@(macro)
export float2 data_lookup_float2(const string name,float2 default_value=float2())=data_lookup(name,default_value);
@(macro)
export float3 data_lookup_float3(const string name,float3 default_value=float3())=data_lookup(name,default_value);
@(macro)
export float4 data_lookup_float4(const string name,float4 default_value=float4())=data_lookup(name,default_value);
@(macro)
export color data_lookup_color(const string name,color default_value=color())=data_lookup(name,default_value);
)*";

static const char *const state = R"*(#smdl
import ::math::*;
export enum coordinate_space{coordinate_internal=0,coordinate_object=1,coordinate_world=2};
@(macro)
export float3 position()=$state.position;
@(macro)
export float3 normal()=$state.normal;
@(macro)
export float3 geometry_normal()=$state.geometry_normal;
@(macro)
export float3 motion()=$state.motion;
@(macro)
export int texture_space_max()=$state.texture_space_max;
@(macro)
export float3 texture_coordinate(const int i)=$state.texture_coordinate[i];
@(macro)
export float3 texture_tangent_u(const int i)=$state.texture_tangent_u[i];
@(macro)
export float3 texture_tangent_v(const int i)=$state.texture_tangent_v[i];
@(macro)
export float3 geometry_tangent_u(const int i)=$state.geometry_tangent_u[i];
@(macro)
export float3 geometry_tangent_v(const int i)=$state.geometry_tangent_v[i];
@(macro)
export float3x3 tangent_space(const int i)=float3x3($state.texture_tangent_u[i],$state.texture_tangent_v[i],$state.normal);
@(macro)
export float3x3 geometry_tangent_space(const int i)=float3x3($state.geometry_tangent_u[i],$state.geometry_tangent_v[i],$state.geometry_normal);
@(macro)
export int object_id()=$state.object_id;
@(macro)
export float3 direction()=float3(0.0,0.0,0.0);
@(macro)
export float animation_time()=$state.animation_time;
export const int WAVELENGTH_BASE_MAX=$WAVELENGTH_BASE_MAX;
@(macro)
export float wavelength_min()=$state.wavelength_min;
@(macro)
export float wavelength_max()=$state.wavelength_max;
@(macro)
export float[WAVELENGTH_BASE_MAX] wavelength_base()=$state.wavelength_base;
@(macro)
export float meters_per_scene_unit()=$state.meters_per_scene_unit;
@(macro)
export float scene_units_per_meter()=1.0/$state.meters_per_scene_unit;
@(pure macro)
float4x4 affine_inverse(const float4x4 matrix){
  return float4x4(
           float4(matrix[0].x,matrix[1].x,matrix[2].x,0.0),
           float4(matrix[0].y,matrix[1].y,matrix[2].y,0.0),
           float4(matrix[0].z,matrix[1].z,matrix[2].z,0.0),
           float4(-#sum(matrix[0]*matrix[3]),-#sum(matrix[1]*matrix[3]),-#sum(matrix[2]*matrix[3]),1.0),
         );
}
@(macro)
export float4x4 transform(const coordinate_space from,const coordinate_space to){
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
@(macro)
export float3 transform_point(const coordinate_space from,const coordinate_space to,const float3 point){
  return from==to?point:(transform(from,to)*float4(point,1)).xyz;
}
@(macro)
export float3 transform_vector(const coordinate_space from,const coordinate_space to,const float3 vector){
  return from==to?vector:(transform(from,to)*float4(vector,0)).xyz;
}
@(macro)
export float3 transform_normal(const coordinate_space from,const coordinate_space to,const float3 normal){
  return from==to?normal:(float4(normal,0)*transform(to,from)).xyz;
}
@(macro)
export float transform_scale(const coordinate_space from,const coordinate_space to,const float scale){
  return 1.0*scale;
}
)*";

static const char *const std = R"*(#smdl
export using ::debug import *;
export using ::df import *;
export using ::limits import *;
export using ::math import *;
export using ::scene import *;
export using ::state import *;
export using ::tex import *;
)*";

static const char *const tex = R"*(#smdl
import ::math::lerp;
export enum gamma_mode{gamma_default=0,gamma_linear=0,gamma_srgb=1};
@(pure macro)
auto decodeSRGB(const auto texel)=#pow(texel,2.2);
@(pure macro)
float4 applyGamma(const int gamma,const float4 texel)=gamma==int(gamma_srgb)?float4(decodeSRGB(texel.rgb),texel.a):texel;
@(pure macro)
float3 applyGamma(const int gamma,const float3 texel)=gamma==int(gamma_srgb)?decodeSRGB(texel):texel;
@(pure macro)
float2 applyGamma(const int gamma,const float2 texel)=gamma==int(gamma_srgb)?decodeSRGB(texel):texel;
@(pure macro)
float applyGamma(const int gamma,const float texel)=gamma==int(gamma_srgb)?decodeSRGB(texel):texel;
@(pure macro)
int getTileIndex(const texture_2d tex,const int2 uv_tile){
  return -1 if(#any((uv_tile<0)|(uv_tile>=tex.tile_count)));
  return uv_tile.y*tex.tile_count.x+uv_tile.x;
}
@(pure macro)
export int width(const texture_2d tex,const int2 uv_tile=int2(0)){
  const auto i(getTileIndex(tex,uv_tile));
  return i<0?0:tex.tile_extents[i].x;
}
@(pure macro)
export int width(const texture_3d tex)=0;
@(pure macro)
export int width(const texture_cube tex)=0;
@(pure macro)
export int height(const texture_2d tex,const int2 uv_tile=int2(0)){
  const auto i(getTileIndex(tex,uv_tile));
  return i<0?0:tex.tile_extents[i].y;
}
@(pure macro)
export int height(const texture_3d tex)=0;
@(pure macro)
export int height(const texture_cube tex)=0;
@(pure macro)
export bool texture_isvalid(const texture_2d tex)=bool(tex.tile_buffers[0]);
@(pure macro)
export bool texture_isvalid(const texture_3d tex)=false;
@(pure macro)
export bool texture_isvalid(const texture_cube tex)=false;
@(pure macro)
export bool texture_isvalid(const texture_ptex tex)=bool(tex.ptr);
@(pure)
auto texel_fetch(const texture_2d tex,const int2 coord,const int2 uv_tile=int2(0)){
  const auto texel_type(*#typeof(tex.tile_buffers[0]));
  const auto i(getTileIndex(tex,uv_tile));
  return texel_type(0) if(i<0);
  const auto tileExtent(tex.tile_extents[i]);
  const auto tileBuffer(tex.tile_buffers[i]);
  return texel_type(0) if(!tileBuffer|#any((coord<0)|(coord>=tileExtent)));
  return tileBuffer[coord.y*tileExtent.x+coord.x];
}
@(pure macro)
export float4 texel_float4(const texture_2d tex,const int2 coord,const int2 uv_tile=int2(0)){
  return applyGamma(tex.gamma,#unpack_float4(texel_fetch(tex,coord,uv_tile)));
}
@(pure macro)
export float3 texel_float3(const texture_2d tex,const int2 coord,const int2 uv_tile=int2(0)){
  return applyGamma(tex.gamma,#unpack_float4(texel_fetch(tex,coord,uv_tile)).xyz);
}
@(pure macro)
export float2 texel_float2(const texture_2d tex,const int2 coord,const int2 uv_tile=int2(0)){
  return applyGamma(tex.gamma,#unpack_float4(texel_fetch(tex,coord,uv_tile)).xy);
}
@(pure macro)
export float texel_float(const texture_2d tex,const int2 coord,const int2 uv_tile=int2(0)){
  return applyGamma(tex.gamma,#unpack_float4(texel_fetch(tex,coord,uv_tile)).x);
}
@(pure macro)
export color texel_color(const texture_2d tex,const int2 coord,const int2 uv_tile=int2(0)){
  return color(texel_float3(tex,coord,uv_tile));
}
export enum wrap_mode{wrap_clamp=0,wrap_repeat=1,wrap_mirrored_repeat=2,wrap_clip=3};
@(pure macro)
auto applyWrap(const auto wrap,const auto n,auto i){
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
@(pure)
export float4 lookup_float4(
  const texture_2d tex,
  float2 coord,
  const wrap_mode wrap_u=wrap_repeat,
  const wrap_mode wrap_v=wrap_repeat,
  const float2 crop_u=float2(0.0,1.0),
  const float2 crop_v=float2(0.0,1.0),
){
  if((tex.tile_count.x>1)|(tex.tile_count.y>1)){
    const int2 tileIndex(#floor(coord));
    const auto i(getTileIndex(tex,tileIndex));
    return float4(0) if(i<0);
    const auto tileExtent(tex.tile_extents[i]);
    const auto tileBuffer(tex.tile_buffers[i]);
    return float4(0) if(!tileBuffer);
    coord-=tileIndex;
    coord*=tileExtent;
    coord-=0.5;
    const int2 ic(#floor(coord));
    const int2 ic0(#min(ic,tileExtent-1));
    const int2 ic1(#min(ic+1,tileExtent-1));
    coord-=ic;
    return applyGamma(tex.gamma,math::lerp(math::lerp(#unpack_float4(tileBuffer[ic0.x+tileExtent.x*ic0.y]),#unpack_float4(tileBuffer[ic1.x+tileExtent.x*ic0.y]),coord.x),math::lerp(#unpack_float4(tileBuffer[ic0.x+tileExtent.x*ic1.y]),#unpack_float4(tileBuffer[ic1.x+tileExtent.x*ic1.y]),coord.x),coord.y),);
  } else {
    const auto i(getTileIndex(tex,int2(0)));
    return float4(0) if(i<0);
    const auto tileExtent(tex.tile_extents[i]);
    const auto tileBuffer(tex.tile_buffers[i]);
    return float4(0) if(!tileBuffer);
    const auto iCropU(int2(crop_u*tileExtent));
    const auto iCropV(int2(crop_v*tileExtent));
    const auto iCorner0(int2(iCropU[0],iCropV[0]));
    const auto iCorner1(int2(iCropU[1],iCropV[1]));
    const auto subextent(iCorner1-iCorner0);
    coord*=subextent;
    coord-=0.5;
    const int2 wrap(int(wrap_u),int(wrap_v));
    const int2 ic(#floor(coord));
    const auto ic0(iCorner0+applyWrap(wrap,subextent,ic));
    const auto ic1(iCorner0+applyWrap(wrap,subextent,ic+1));
    coord-=ic;
    return applyGamma(tex.gamma,math::lerp(math::lerp(#unpack_float4(tileBuffer[ic0.x+tileExtent.x*ic0.y]),#unpack_float4(tileBuffer[ic1.x+tileExtent.x*ic0.y]),coord.x),math::lerp(#unpack_float4(tileBuffer[ic0.x+tileExtent.x*ic1.y]),#unpack_float4(tileBuffer[ic1.x+tileExtent.x*ic1.y]),coord.x),coord.y),);
  }
}
@(pure macro)
export float3 lookup_float3(
  const texture_2d tex,
  const float2 coord,
  const wrap_mode wrap_u=wrap_repeat,
  const wrap_mode wrap_v=wrap_repeat,
  const float2 crop_u=float2(0.0,1.0),
  const float2 crop_v=float2(0.0,1.0),
)=lookup_float4(tex,coord,wrap_u,wrap_v,crop_u,crop_v).xyz;
@(pure macro)
export float2 lookup_float2(
  const texture_2d tex,
  const float2 coord,
  const wrap_mode wrap_u=wrap_repeat,
  const wrap_mode wrap_v=wrap_repeat,
  const float2 crop_u=float2(0.0,1.0),
  const float2 crop_v=float2(0.0,1.0),
)=lookup_float4(tex,coord,wrap_u,wrap_v,crop_u,crop_v).xy;
@(pure macro)
export float lookup_float(
  const texture_2d tex,
  const float2 coord,
  const wrap_mode wrap_u=wrap_repeat,
  const wrap_mode wrap_v=wrap_repeat,
  const float2 crop_u=float2(0.0,1.0),
  const float2 crop_v=float2(0.0,1.0),
)=lookup_float4(tex,coord,wrap_u,wrap_v,crop_u,crop_v).x;
@(pure macro)
export color lookup_color(
  const texture_2d tex,
  const float2 coord,
  const wrap_mode wrap_u=wrap_repeat,
  const wrap_mode wrap_v=wrap_repeat,
  const float2 crop_u=float2(0.0,1.0),
  const float2 crop_v=float2(0.0,1.0),
)=color(lookup_float4(tex,coord,wrap_u,wrap_v,crop_u,crop_v).xyz);
@(foreign)
void smdlPtexEvaluate(&void tex,int gamma,int first,int num,&float result);
@(macro)
export float4 lookup_float4(const texture_ptex tex,const int channel=0){
  float4 result;
  smdlPtexEvaluate(tex.ptr,tex.gamma,channel,4,&result[0]);
  return result;
}
@(macro)
export float3 lookup_float3(const texture_ptex tex,const int channel=0){
  float3 result;
  smdlPtexEvaluate(tex.ptr,tex.gamma,channel,3,&result[0]);
  return result;
}
@(macro)
export float2 lookup_float2(const texture_ptex tex,const int channel=0){
  float2 result;
  smdlPtexEvaluate(tex.ptr,tex.gamma,channel,2,&result[0]);
  return result;
}
@(macro)
export float lookup_float(const texture_ptex tex,const int channel=0){
  float result;
  smdlPtexEvaluate(tex.ptr,tex.gamma,channel,1,&result);
  return result;
}
@(macro)
export color lookup_color(const texture_ptex tex,const int channel=0){
  float3 result;
  smdlPtexEvaluate(tex.ptr,tex.gamma,channel,3,&result[0]);
  return color(result);
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
  if (name == "io")
    return io;
  if (name == "limits")
    return limits;
  if (name == "pcg32")
    return pcg32;
  if (name == "prospect")
    return prospect;
  if (name == "marmit")
    return marmit;
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
#include "builtin/albedo/diffuse_reflection_bsdf.inl"
#include "builtin/albedo/microfacet_ggx_smith_bsdf.inl"
#include "builtin/albedo/microfacet_beckmann_smith_bsdf.inl"
#include "builtin/albedo/sheen_bsdf.inl"
#include "builtin/albedo/simple_glossy_bsdf.inl"
#include "builtin/albedo/ward_geisler_moroder_bsdf.inl"
[[nodiscard]] static const AlbedoLUT *get_albedo(std::string_view name) {
  if (name == "diffuse_reflection_bsdf")
    return &diffuse_reflection_bsdf;
  if (name == "microfacet_ggx_smith_bsdf")
    return &microfacet_ggx_smith_bsdf;
  if (name == "microfacet_beckmann_smith_bsdf")
    return &microfacet_beckmann_smith_bsdf;
  if (name == "sheen_bsdf")
    return &sheen_bsdf;
  if (name == "simple_glossy_bsdf")
    return &simple_glossy_bsdf;
  if (name == "ward_geisler_moroder_bsdf")
    return &ward_geisler_moroder_bsdf;
  return nullptr;
}

} // namespace smdl::builtin
