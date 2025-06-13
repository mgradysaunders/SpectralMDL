#pragma once

#include <array>
#include <string_view>

namespace smdl::builtin {

static const char *anno = R"*(#smdl
)*";

static const char *api = R"*(#smdl
export typedef #type_int(8) $int8_t;
export typedef #type_int(16) $int16_t;
export typedef #type_int(32) $int32_t;
export typedef #type_int(64) $int64_t;
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
@(hot noinline)color rgb_to_color_nontrivial(float3 rgb){
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
      int j(#min(int(t),RGB_TO_COLOR_NUM_WAVELENGTHS-2));
      t=#min(t-j,1.0);
      c[i]=#sum(float2(1-t,t)*(coeff_w*float2(&RGB_TO_COLOR_CURVES[0][j])+coeff_cmy*float2(&RGB_TO_COLOR_CURVES[k0+1][j])+coeff_rgb*float2(&RGB_TO_COLOR_CURVES[k2+4][j])));
    }
  }
  return #max(c*0.94,0.0);
}
export @(macro)color $rgb_to_color(const float3 rgb){
  if(#all(rgb.xx==rgb.yz)){
    return color(rgb.x);
  } else {
    return rgb_to_color_nontrivial(rgb);
  }
}
export @(pure)float3 $wyman_xyz(const float w){
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
export @(pure)float $wyman_y(const float w){
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
export @(hot noinline)float3 $color_to_rgb(const color c){
  float3 result(0.0);
  for(int i=0;i<$WAVELENGTH_BASE_MAX;++i){
    result+=$wyman_xyz($state.wavelength_base[i])*c[i];
  }
  result/=$WAVELENGTH_BASE_MAX;
  result*=$state.wavelength_max-$state.wavelength_min;
  return float3x3(float3(3.240450,-0.969266,0.0556434),float3(-1.537140,1.876010,-0.2040260),float3(-0.498532,0.041556,1.0572300),)*result;
}
@(visible noinline)void jit_rgb_to_color(const &float3 rgb,const &float cptr){
  color c($rgb_to_color(*rgb));
  #memcpy(cptr,&c,#sizeof(color));
}
@(visible noinline)void jit_color_to_rgb(const &float cptr,const &float3 rgb){
  *rgb=$color_to_rgb(color(cptr));
}
export typedef $int32_t $hash_t;
export @(pure)$hash_t $hash(auto value){
  if$(#is_arithmetic_scalar(value)){
    if$(#is_arithmetic_integral(value)){
      if$(#sizeof(value)<=4){
        auto h($int32_t(value)+3266445271);
        h^=h>>>16,h*=0x85EBCA6B;
        h^=h>>>13,h*=0xC2B2AE35;
        h^=h>>>16;
        return h;
      } else {
        auto h($int64_t(value)+13898551614298330943);
        h^=h>>>33,h*=0xFF51AFD7ED558CCD;
        h^=h>>>33,h*=0xC4CEB9FE1A85EC53;
        h^=h>>>33;
        return h;
      }
    } else {
      auto h(#type_int(8*#sizeof(value))());
      #memcpy(&h,&value,#sizeof(value));
      return $hash(h);
    }
  } else if$(#is_array(value)|#is_arithmetic_vector(value)|#is_arithmetic_matrix(value)|(#typeof(value)==color)){
    auto hTotal($hash(value[0]));
    for(int i=1;i<#num(value);++i){
      auto h($hash(value[i]));
      h=0x55555555*(h^(h>>>16));
      h=3423571495*(h^(h>>>16));
      hTotal=#rotl(hTotal,10)^h;
    }
    return hTotal;
  } else if$(#is_pointer(value)){
    auto h(#type_int(8*#sizeof(value))());
    #memcpy(&h,&value,#sizeof(value));
    return $hash(h);
  } else if$(#is_union(value)){
    visit v in value{
      return $hash(v);
    }
  } else {
    #panic("Unimplemented hash");
    return 0;
  }
}
export enum intensity_mode{intensity_radiant_exitance,intensity_power,};
export tag bsdf;
export tag vdf;
export tag edf;
export tag hair_bsdf;
export struct $default_bsdf:default bsdf{};
export struct $default_vdf:default vdf{};
export struct $default_edf:default edf{};
export struct $default_hair_bsdf:default hair_bsdf{};
export struct texture_ptex{
  texture_ptex(const string name,const auto gamma=0)=texture_ptex(handle: #load_ptexture(name),gamma: int(gamma));
  const &void handle=null;
  const int gamma=0;
};
export struct bsdf_measurement{
  bsdf_measurement(const string filename)=bsdf_measurement(handle: #load_bsdf_measurement(filename));
  const &void handle=null;
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
const int THIN_WALLED=(1<<0);
const int HAS_SURFACE=(1<<1);
const int HAS_BACKFACE=(1<<2);
const int HAS_VOLUME=(1<<3);
const int HAS_HAIR=(1<<4);
const int HAS_POSSIBLY_NON_ZERO_BRDF=(1<<5);
const int HAS_POSSIBLY_NON_ZERO_BTDF=(1<<6);
export struct $material_instance{
  const &material mat;
  const &float3 displacement=&mat.geometry.displacement;
  const &float cutout_opacity=&mat.geometry.cutout_opacity;
  const &float3 normal=&mat.geometry.normal;
  const &color ior=&mat.ior;
  const &color absorption_coefficient=($(#typeof(mat.volume.absorption_coefficient)==void)?null:&mat.volume.absorption_coefficient);
  const &color scattering_coefficient=($(#typeof(mat.volume.scattering_coefficient)==void)?null:&mat.volume.scattering_coefficient);
  const int wavelength_base_max=$WAVELENGTH_BASE_MAX;
  const int flags=(mat.thin_walled?THIN_WALLED:0)|(#typeof(mat.surface)!=#typeof(material_surface())?HAS_SURFACE:0)|(#typeof(mat.backface)!=#typeof(material_surface())?HAS_BACKFACE:0)|(#typeof(mat.volume)!=#typeof(material_volume())?HAS_VOLUME:0)|(#typeof(mat.hair)!=#typeof(hair_bsdf())?HAS_HAIR:0);
  const float3x3 tangent_space=float3x3($state.tangent_to_object_matrix[0].xyz,$state.tangent_to_object_matrix[1].xyz,$state.tangent_to_object_matrix[2].xyz,);
};
export struct $albedo_lut{
  const int num_cos_theta=0;
  const int num_roughness=0;
  const &float directional_albedo=null;
  const &float average_albedo=null;
};
)*";

static const char *debug = R"*(#smdl
export @(pure macro)bool assert(const bool condition,const string reason){
  #assert(condition,reason) if($DEBUG);
  return true;
}
export @(pure macro)bool breakpoint(){
  #breakpoint() if($DEBUG);
  return true;
}
export @(pure macro)bool print(const auto a){
  #print(a) if($DEBUG);
  return true;
}
)*";

static const char *df = R"*(#smdl
using ::math import *;
const float EPSILON=1e-6;
const float MULTISCATTER_DIFFUSE_CHANCE=0.2;
export enum scatter_mode{
  scatter_none=0x0,
  scatter_reflect=0x1,
  scatter_transmit=0x2,
  scatter_reflect_transmit=0x3,
};
@(pure macro)float scatter_reflect_chance(const scatter_mode mode){
  const auto refl_weight(#select((int(mode)&1)!=0,1.0,0.0));
  const auto tran_weight(#select((int(mode)&2)!=0,1.0,0.0));
  return refl_weight/(refl_weight+tran_weight);
}
@(pure foreign)double erf(double x);
@(pure foreign)double erfc(double x);
export namespace monte_carlo {
export @(pure macro)float2 next_low_discrepancy(const &float2 xi)=(*xi=frac(*xi+float2(0.75487766,0.56984029)));
export @(pure macro)float3 next_low_discrepancy(const &float3 xi)=(*xi=frac(*xi+float3(0.81917251,0.67104360,0.54970047)));
export @(pure macro)float4 next_low_discrepancy(const &float4 xi)=(*xi=frac(*xi+float4(0.85667488,0.73389185,0.62870672,0.53859725)));
export @(pure)bool bool_sample(const &float xi,const float chance){
  if(*xi<chance){
    *xi=(*xi/chance);
    return true;
  } else {
    *xi=(*xi-chance)/(1-chance);
    return false;
  }
}
export @(pure)int uniform_wavelength_index_sample(const &float xi){
  const int i(#min(int(*xi*=$WAVELENGTH_BASE_MAX),$WAVELENGTH_BASE_MAX-1));
  *xi-=i;
  return i;
}
export @(pure noinline)float2 uniform_disk_sample(float2 xi){
  xi=2*xi-1;
  xi=#select(xi==0,EPSILON,xi);
  const bool cond((absxi:=#abs(xi),absxi.x>absxi.y));
  const float rad(#select(cond,xi.x,xi.y));
  const float phi(#select(cond,($PI/4)*xi.y/xi.x,($PI/2)-($PI/4)*xi.x/xi.y));
  return rad*float2(#cos(phi),#sin(phi));
}
export @(pure noinline)float3 cosine_hemisphere_sample(float2 xi){
  return float3((p:=uniform_disk_sample(xi)),#sqrt(#max(1-#sum(p*p),0)));
}
export @(pure noinline)float3 uniform_hemisphere_sample(float2 xi){
  const float cos_theta=saturate(xi.x);
  const float sin_theta=#sqrt(1-cos_theta*cos_theta);
  return float3(sin_theta*#cos(phi:=$TWO_PI*xi.y),sin_theta*#sin(phi),cos_theta,);
}
export @(pure noinline)float3 uniform_sphere_sample(float2 xi){
  const float cos_theta=2*saturate(xi.x)-1;
  const float sin_theta=#sqrt(1-cos_theta*cos_theta);
  return float3(sin_theta*#cos(phi:=$TWO_PI*xi.y),sin_theta*#sin(phi),cos_theta,);
}
export @(pure noinline)double erf_inverse(double y){
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
export @(pure macro)float3 reflect(const float3 wi,const float3 wm)=2*#sum(wi*wm)*wm-wi;
export @(pure macro)float3 refract(const float3 wi,const float3 wm,const float ior){
  const auto cos_thetai(#sum(wi*wm));
  const auto cos2_thetai(#min(cos_thetai*cos_thetai,1));
  const auto cos2_thetat(#max(1-ior*ior*(1-cos2_thetai),0));
  const auto cos_thetat(#sqrt(cos2_thetat)*-#sign(cos_thetai));
  return -ior*wi+(ior*cos_thetai+cos_thetat)*wm;
}
export @(pure macro)float3 reflection_half_vector(const float3 wo,const float3 wi)=(vh:=(wo+wi))*#sign(vh.z);
export @(pure macro)float3 refraction_half_vector(const float3 wo,const float3 wi,const float ior,)=(vh:=-(ior*wo+wi))*#sign(vh.z);
export @(pure macro)auto refraction_half_vector_jacobian(const float3 wo,const float3 wi,const float ior,)=#abs(#sum(wi*(vh:=refraction_half_vector(wo,wi,ior))))/((vh2:=#sum(vh*vh))*#sqrt(vh2));
export @(pure macro)auto schlick_F0(const auto ior)=#pow((ior-1)/(ior+1),2);
export @(pure macro)auto schlick_fresnel(
  const auto cos_theta,
  const auto F0,
  const auto F90=1.0,
  const float exponent=5,
)=F0+(F90-F0)*#pow(#max(1-#abs(cos_theta),0),exponent);
export @(pure)auto dielectric_fresnel(const float cos_thetai,const auto ior){
  const auto cos2_thetat(1-ior*ior*(1-cos_thetai*cos_thetai));
  const auto cos_thetat(#sqrt(#max(cos2_thetat,0))*#sign(cos_thetai));
  const auto rs((ior*cos_thetai-cos_thetat)/(ior*cos_thetai+cos_thetat));
  const auto rp((cos_thetai-ior*cos_thetat)/(cos_thetai+ior*cos_thetat));
  return #min(0.5*(rs*rs+rp*rp),1.0);
}
}
@(pure noinline)float3x3 calculate_tangent_space(const float3 normal,const float3 tangent_u){
  const auto tw(normalize(normal)*#sign(normal.z));
  const auto tu(normalize(tangent_u-dot(tangent_u,tw)*tw));
  const auto tv(normalize(cross(tw,tu)));
  return float3x3(tu,tv,tw);
}
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
struct scatter_evaluate_result{
  $(color|float) f=0.0;
  float2 pdf=float2(0.0);
  bool is_black=false;
};
@(pure noinline)bool recalculate_tangent_space(inline const &scatter_evaluate_parameters params){
  auto tbn(calculate_tangent_space(normal,tangent_u));
  wo=wo0*tbn;
  wi=wi0*tbn;
  return ((wo.z<0)==(wo0.z<0))&((wi.z<0)==(wi0.z<0));
}
@(pure)float3 half_direction(inline const &scatter_evaluate_parameters params){
  return normalize(mode==scatter_reflect?specular::reflection_half_vector(wo,wi):specular::refraction_half_vector(wo,wi,ior));
}
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
struct scatter_sample_result{
  float3 wi=float3(0.0);
  scatter_mode mode=scatter_none;
  ?color delta_f=null;
};
@(pure noinline)?float3x3 recalculate_tangent_space(inline const &scatter_sample_parameters params){
  auto tbn(calculate_tangent_space(normal,tangent_u));
  wo=wo0*tbn;
  return tbn if((wo.z<0)==(wo0.z<0));
}
@(pure)float3 half_direction(inline const &scatter_sample_parameters this,inline const &scatter_sample_result result){
  return normalize(mode==scatter_reflect?specular::reflection_half_vector(wo,wi):specular::refraction_half_vector(wo,wi,ior));
}
export @(pure macro)auto $energy_loss_compensation(
  const string lut_name[[anno::unused()]],
  const float cos_thetao[[anno::unused()]],
  const float cos_thetai[[anno::unused()]],
  const float roughness[[anno::unused()]],
  const auto multiscatter_tint,
){
  if$(multiscatter_tint<:void){
    return 0.0;
  } else {
    const $albedo_lut lut(#albedo_lut(lut_name));
    float t((lut.num_roughness-1)*#max(0,#min(roughness,1)));
    const int j(#min(int(#floor(t)),lut.num_roughness-2));
    t=t-j;
    const float Ewo=return_from{
      float s((lut.num_cos_theta-1)*#max(0,#min(#abs(cos_thetao),1)));
      const int i(#min(int(#floor(s)),lut.num_cos_theta-2));
      const &float ptr0(&lut.directional_albedo[lut.num_roughness*(i+0)+j]);
      const &float ptr1(&lut.directional_albedo[lut.num_roughness*(i+1)+j]);
      return #min(1.0,lerp(lerp(ptr0[0],ptr0[1],t),lerp(ptr1[0],ptr1[1],t),s-i));
    };
    const float Ewi=return_from{
      float s((lut.num_cos_theta-1)*#max(0,#min(#abs(cos_thetai),1)));
      const int i(#min(int(#floor(s)),lut.num_cos_theta-2));
      const &float ptr0(&lut.directional_albedo[lut.num_roughness*(i+0)+j]);
      const &float ptr1(&lut.directional_albedo[lut.num_roughness*(i+1)+j]);
      return #min(1.0,lerp(lerp(ptr0[0],ptr0[1],t),lerp(ptr1[0],ptr1[1],t),s-i));
    };
    const float Eav=#min(1,(1-t)*lut.average_albedo[j]+t*lut.average_albedo[j+1]);
    return #abs(cos_thetai)/$PI*(1-Ewo)*(1-Ewi)/(1-Eav+1e-6)*multiscatter_tint;
  }
}
@(pure macro)auto scatter_evaluate(const &$default_bsdf this,const &scatter_evaluate_parameters params){
  return scatter_evaluate_result(is_black: true);
}
@(pure macro)auto scatter_sample(const &$default_bsdf this,const &scatter_sample_parameters params){
  return scatter_sample_result();
}
export struct diffuse_reflection_bsdf:bsdf{
  $(color|float) tint=1.0;
  float roughness=0.0;
  string handle="";
  $(?(color|float)) multiscatter_tint=null;
  static const scatter_mode mode=scatter_reflect;
};
@(pure)auto scatter_evaluate(inline const &diffuse_reflection_bsdf this,inline const &scatter_evaluate_parameters params){
  if(mode==scatter_reflect&&recalculate_tangent_space(params)){
    const auto cos_theta(#abs(auto(wi.z,wo.z)));
    const auto pdf(cos_theta/$PI);
    if(roughness==0){
      return scatter_evaluate_result(f: pdf[0]*tint,pdf: pdf);
    } else {
      const auto sigma2(2.0*roughness*roughness);
      const auto A(1.00-sigma2/(2.0*sigma2+0.66));
      const auto B(0.45*sigma2/(sigma2+0.09));
      const auto fss(pdf[0]*(A+#max(#sum(wo.xy*wi.xy),0)/(#max_value(cos_theta)+EPSILON)*B));
      const auto fms($energy_loss_compensation("diffuse_reflection_bsdf",wo.z,wi.z,roughness,multiscatter_tint));
      return scatter_evaluate_result(f: tint*(fss+fms),pdf: pdf);
    }
  } else {
    return scatter_evaluate_result(is_black: true);
  }
}
@(pure)auto scatter_sample(inline const &diffuse_reflection_bsdf this,inline const &scatter_sample_parameters params){
  if((tbn:=recalculate_tangent_space(params))){
    return scatter_sample_result(wi: (*tbn)*monte_carlo::cosine_hemisphere_sample(xi.xy),mode: scatter_reflect);
  } else {
    return scatter_sample_result();
  }
}
export struct diffuse_transmission_bsdf:bsdf{
  $(color|float) tint=1.0;
  string handle="";
  static const scatter_mode mode=scatter_transmit;
};
@(pure)auto scatter_evaluate(inline const &diffuse_transmission_bsdf this,inline const &scatter_evaluate_parameters params){
  if(mode==scatter_transmit&&recalculate_tangent_space(params)){
    const auto cos_theta(#abs(auto(wi.z,wo.z)));
    const auto pdf(cos_theta/$PI);
    return scatter_evaluate_result(f: tint*pdf[0],pdf: pdf);
  } else {
    return scatter_evaluate_result(is_black: true);
  }
}
@(pure)auto scatter_sample(inline const &diffuse_transmission_bsdf this,inline const &scatter_sample_parameters params){
  if((tbn:=recalculate_tangent_space(params))){
    return scatter_sample_result(wi: (*tbn)*-monte_carlo::cosine_hemisphere_sample(xi.xy),mode: scatter_transmit);
  } else {
    return scatter_sample_result();
  }
}
export struct specular_bsdf:bsdf{
  $(color|float) tint=1.0;
  scatter_mode mode=scatter_reflect;
  string handle="";
};
@(pure macro)auto scatter_evaluate(const &specular_bsdf this,const &scatter_evaluate_parameters params){
  return scatter_evaluate_result(is_black: true);
}
@(pure macro)auto scatter_sample(const &specular_bsdf this,inline const &scatter_sample_parameters params){
  if((tbn:=recalculate_tangent_space(params))){
    return xi.x<scatter_reflect_chance(this.mode)?scatter_sample_result(wi: (*tbn)*specular::reflect(wo,float3(0,0,1)),mode: scatter_reflect,delta_f: color(this.tint)):scatter_sample_result(wi: (*tbn)*specular::refract(wo,float3(0,0,1),ior),mode: scatter_transmit,delta_f: color(this.tint));
  } else {
    return scatter_sample_result();
  }
}
export struct sheen_bsdf:bsdf{
  float roughness;
  $(color|float) tint=1.0;
  $(?(color|float)) multiscatter_tint=null;
  void multiscatter=null;
  string handle="";
  finalize {
    roughness=saturate(roughness);
  }
};
@(pure)float sheen_lambda_L(const auto fit,const float mu){
  return fit[0]/(1.0+fit[1]*#pow(mu,fit[2]))+fit[3]*mu+fit[4];
}
@(pure)float sheen_lambda(const auto fit,const float mu){
  return #exp(mu<0.5?sheen_lambda_L(fit,mu):2*sheen_lambda_L(fit,0.5)-sheen_lambda_L(fit,#max(1-mu,0)));
}
@(pure)auto scatter_evaluate(inline const &sheen_bsdf this,inline const &scatter_evaluate_parameters params){
  if(mode==scatter_reflect&&recalculate_tangent_space(params)){
    const auto cos_thetao(#abs(wo.z));
    const auto cos_thetai(#abs(wi.z));
    const auto pdf(float2(cos_thetai,cos_thetao)/$PI);
    const auto fss=let {
      const auto alpha(lerp(0.1,1.0,roughness*roughness));
      const auto fit=lerp(auto(21.5473,3.82987,0.19823,-1.97760,-4.32054),auto(25.3245,3.32435,0.16801,-1.27393,-4.85967),#pow(1-#pow(roughness,2),2),);
      const auto cos_thetah(normalize(wo+wi).z);
      const auto sin_thetah(#sqrt(1-cos_thetah*cos_thetah));
      const auto D(1/$TWO_PI*(2+1/alpha)*#pow(sin_thetah,1/alpha));
      const auto G(1/(1+sheen_lambda(fit,cos_thetao)+sheen_lambda(fit,cos_thetai)));
    } in D*G/(4*cos_thetao);
    const auto fms($energy_loss_compensation("sheen_bsdf",wo.z,wi.z,roughness,multiscatter_tint));
    return scatter_evaluate_result(f: tint*(fss+fms),pdf: pdf);
  } else {
    return scatter_evaluate_result(is_black: true);
  }
}
@(pure)auto scatter_sample(inline const &sheen_bsdf this,inline const &scatter_sample_parameters params){
  if((tbn:=recalculate_tangent_space(params))){
    return scatter_sample_result(wi: (*tbn)*monte_carlo::cosine_hemisphere_sample(xi.xy),mode: scatter_reflect);
  } else {
    return scatter_sample_result();
  }
}
export struct ward_geisler_moroder_bsdf:bsdf{
  float roughness_u;
  float roughness_v=roughness_u;
  $(color|float) tint=1.0;
  $(?(color|float)) multiscatter_tint=null;
  float3 tangent_u=$state.texture_tangent_u[0];
  string handle="";
  finalize {
    roughness_u=saturate(roughness_u);
    roughness_v=saturate(roughness_v);
  }
};
@(pure noinline)auto scatter_evaluate(const &ward_geisler_moroder_bsdf this,inline const &scatter_evaluate_parameters params){
  preserve tangent_u;
  tangent_u=this.tangent_u;
  if(mode==scatter_reflect&&recalculate_tangent_space(params)){
    const auto cos_thetao(#abs(wo.z));
    const auto cos_thetai(#abs(wi.z));
    const auto roughness(this.roughness_u,this.roughness_v);
    const auto alpha(#max(0.001,roughness*roughness));
    const auto f(#sum((h:=wo+wi)*h)/($PI*alpha.x*alpha.y*#pow(h.z,4))*#exp(-#sum((g:=h.xy/(h.z*alpha))*g)));
    const auto fss_pdf(float2(f*(cos_thetao+cos_thetai)/2));
    const auto fms_pdf(float2(cos_thetai,cos_thetao)/$PI);
    const auto fss(f*cos_thetai);
    const auto fms($energy_loss_compensation("ward_geisler_moroder_bsdf",wo.z,wi.z,#sqrt(#prod(roughness)),this.multiscatter_tint));
    if$(#typeof(this.multiscatter_tint)==void){
      return scatter_evaluate_result(f: this.tint*fss,pdf: fss_pdf);
    } else {
      return scatter_evaluate_result(f: this.tint*(fss+fms),pdf: lerp(fss_pdf,fms_pdf,MULTISCATTER_DIFFUSE_CHANCE));
    }
  } else {
    return scatter_evaluate_result(is_black: true);
  }
}
@(pure noinline)auto scatter_sample(const &ward_geisler_moroder_bsdf this,inline const &scatter_sample_parameters params){
  preserve tangent_u;
  tangent_u=this.tangent_u;
  if((tbn:=recalculate_tangent_space(params))){
    if(#typeof(this.multiscatter_tint)!=void&&monte_carlo::bool_sample(&xi.w,MULTISCATTER_DIFFUSE_CHANCE)){
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
export namespace microfacet {
export tag distribution;
export struct distribution_ggx:default distribution{};
export struct distribution_beckmann:distribution{};
export @(pure macro)float smith_lambda(const distribution_ggx this[[anno::unused()]],
                                       const float m){
  return 0.5*(#sign(m)*#sqrt(1+1/(m*m+EPSILON)))-0.5;
}
export @(pure macro)float smith_lambda(const distribution_beckmann this[[anno::unused()]],
                                       const float m){
  return 0.5*(#exp(-m*m)/m/#sqrt($PI)-float(erfc(m)));
}
export @(pure macro)float smith_slope_pdf(const distribution_ggx this[[anno::unused()]],
                                          const float2 m){
  return (1/$PI)/#pow(1+#sum(m*m),2);
}
export @(pure macro)float smith_slope_pdf(const distribution_beckmann this[[anno::unused()]],
                                          const float2 m){
  return (1/$PI)*#exp(-#sum(m*m));
}
export @(pure noinline)float2 smith_visible_slope_sample(
  const distribution_ggx this[[anno::unused()]],
  const float xi0,
  const float xi1,
  float cos_thetao,
){
  return #sqrt(xi0/(1-xi0+EPSILON))*float2(#cos(phi:=$TWO_PI*xi1),#sin(phi)) if(cos_thetao>1-EPSILON);
  cos_thetao=#max(cos_thetao,-0.9999);
  const auto mx=return_from{
    const auto sin_thetao(#sqrt(1-cos_thetao*cos_thetao));
    const auto tan_thetao(sin_thetao/cos_thetao);
    const auto mu(xi0*(1+1/cos_thetao)-1);
    const auto nu(1/(1-mu*mu));
    const auto D(#sqrt(#max(nu*(mu*mu-(1-nu)*tan_thetao*tan_thetao),0)));
    const auto mx0(-nu*tan_thetao-D);
    const auto mx1(-nu*tan_thetao+D);
    return #select((mu<0)|(mx1*sin_thetao>cos_thetao),mx0,mx1);
  };
  const auto my=return_from{
    const auto s(#select(xi1>0.5,1.0,-1.0));
    const auto t(#min(s*(2*xi1-1),1));
    return #sqrt(1+mx*mx)*s*((t*(t*(t*0.27385-0.73369)+0.46341))/(t*(t*(t*0.093073+0.30942)-1.0)+0.597999));
  };
  return float2(mx,my);
}
export @(pure noinline)float2 smith_visible_slope_sample(
  const distribution_beckmann this[[anno::unused()]],
  float xi0,
  float xi1,
  float cos_thetao,
){
  return #sqrt(-#log(1-xi0+EPSILON))*float2(#cos((phi:=$TWO_PI*xi1)),#sin(phi)) if(cos_thetao>1-EPSILON);
  xi0=#max(xi0,EPSILON);
  xi1=#max(xi1,EPSILON);
  const float SQRT_PI_INV=1/#sqrt($PI);
  const float thetao=#acos(cos_thetao);
  const float sin_thetao=#sqrt(#max(0,1-cos_thetao*cos_thetao));
  const float tan_thetao=sin_thetao/cos_thetao;
  const float cot_thetao=1/tan_thetao;
  float xmin=-1;
  float xmax=float(erf(cot_thetao));
  float x=xmax-(1+xmax)*#pow(1-xi0,1+thetao*(-0.876+thetao*(0.4265-0.0594*thetao)));
  float norm=1/(1+xmax+SQRT_PI_INV*tan_thetao*#exp(-cot_thetao*cot_thetao));
  for(int i=0;i<10;++i){
    if(!(xmin<=x&&x<=xmax))
      x=0.5*(xmin+xmax);
    const float a=monte_carlo::erf_inverse(x);
    const float f=norm*(1+x+SQRT_PI_INV*tan_thetao*#exp(-a*a))-xi0;
    break if(f~==[1e-5]0.0);
    if(f>0)
      xmax=x;
    else
      xmin=x;
    x-=f/(norm*(1-a*tan_thetao));
  }
  return float2(monte_carlo::erf_inverse(x),monte_carlo::erf_inverse(2*xi1-1),);
}
export @(pure macro)float smith_normal_pdf(const distribution this[[anno::unused()]],
                                           const float2 alpha,const float3 wm){
  return wm.z>0.0?smith_slope_pdf(this,-wm.xy/(wm.z*alpha+EPSILON))/(alpha.x*alpha.y*#pow(wm.z,4)+EPSILON):0.0;
}
export @(pure noinline)float3 smith_visible_normal_sample(
  const distribution this,
  const float xi0,
  const float xi1,
  const float2 alpha,
  const float3 wo,
){
  const auto w11(normalize(float3(alpha*wo.xy,wo.z)));
  const auto sin_theta(length(w11.xy));
  const auto cos_phi(w11.x/sin_theta);
  const auto sin_phi(w11.y/sin_theta);
  const auto m11(smith_visible_slope_sample(this,xi0,xi1,w11.z));
  const auto m(float2(alpha.x*dot(float2(cos_phi,-sin_phi),m11),alpha.y*dot(float2(sin_phi,cos_phi),m11)));
  return #all(isfinite(m))?normalize(float3(-m,1)):wo.z==0?normalize(wo):float3(0,0,1);
}
export struct distribution_blinn:distribution{};
export @(pure macro)void blinn_normal_first_quadrant_sample(
  const float xi0,
  const float xi1,
  const float2 e,
  &float phi,
  &float cos_theta,
){
  if(e.x==e.y){
    *phi=$HALF_PI*xi0;
    *cos_theta=#pow(xi1,1/(1+e.x));
  } else {
    *phi=#atan(#sqrt((1+e.x)/(1+e.y))*#tan($HALF_PI*xi0));
    *cos_theta=#pow(xi1,1/(1+e.x*(cos_phi:=#cos(*phi))*cos_phi+e.y*(sin_phi:=#sin(*phi))*sin_phi));
  }
}
export @(pure noinline)float3 blinn_normal_sample(const float xi0,const float xi1,const float2 e,){
  float phi=0;
  float cos_theta=0;
  if(xi0<0.25){
    blinn_normal_first_quadrant_sample(4*xi0,xi1,e,&phi,&cos_theta);
  } else if(xi0<0.5){
    blinn_normal_first_quadrant_sample(4*(0.5-xi0),xi1,e,&phi,&cos_theta),phi=$PI-phi;
  } else if(xi0<0.75){
    blinn_normal_first_quadrant_sample(4*(xi0-0.5),xi1,e,&phi,&cos_theta),phi+=$PI;
  } else {
    blinn_normal_first_quadrant_sample(4*(1-xi0),xi1,e,&phi,&cos_theta),phi=$TWO_PI-phi;
  }
  return float3(#sqrt(1-cos_theta*cos_theta+EPSILON)*float2(#cos(phi),#sin(phi)),cos_theta);
}
export tag shadowing;
export struct shadowing_smith:default shadowing{};
export struct shadowing_vcavities:shadowing{};
@(pure foreign)double lgamma(double x);
export @(macro)double beta(const double x,const double y)=#exp(lgamma(x)+lgamma(y)-lgamma(x+y));
}
struct microfacet_bsdf:bsdf{
  const float2 roughness;
  const float roughness0=#sqrt(#prod(roughness));
  const float2 alpha=clamp(roughness*roughness,EPSILON,1.0);
  $(color|float) tint;
  $(?(color|float)) multiscatter_tint=null;
  float3 tangent_u=$state.texture_tangent_u[0];
  const scatter_mode mode=scatter_reflect;
  const microfacet::distribution distribution=microfacet::distribution();
  const microfacet::shadowing shadowing=microfacet::shadowing();
};
@(pure noinline)auto scatter_evaluate(const &microfacet_bsdf this,inline const &scatter_evaluate_parameters params){
  preserve tangent_u;
  tangent_u=this.tangent_u;
  return scatter_evaluate_result(is_black: true) if(!recalculate_tangent_space(params));
  const auto reflect_chance(scatter_reflect_chance(this.mode));
  const auto wm(normalize(mode==scatter_reflect?wo+wi:specular::refraction_half_vector(wo,wi,ior)));
  const auto dot_wo_wm(#sum(wo*wm));
  const auto dot_wi_wm(#sum(wi*wm));
  if$(this.distribution<:microfacet::distribution_blinn){
    const auto e(2/(this.alpha*this.alpha+EPSILON));
    const auto D(#pow(wm.z,(e.x*wm.x*wm.x+e.y*wm.y*wm.y)/(1-wm.z*wm.z+EPSILON))/$TWO_PI);
    const auto norm1(#sqrt(#prod(1+e)));
    const auto norm2(#sqrt(#prod(2+e)));
    const auto G(#min(1,2*wm.z*#min(#abs(wo.z/dot_wo_wm),#abs(wi.z/dot_wi_wm))));
    switch(mode&this.mode){
    case scatter_reflect: {
      const auto fss_pdf(norm1*D/(4*float2(dot_wo_wm,dot_wi_wm)+EPSILON));
      const auto fss(norm2*D*G/(4*#abs(wo.z)+EPSILON));
      if$(this.multiscatter_tint<:void){
        return scatter_evaluate_result(f: this.tint*(reflect_chance*fss),pdf: reflect_chance*fss_pdf);
      } else {
        const auto fms_pdf(float2(#abs(wi.z),#abs(wo.z))/$PI);
        const auto fms($energy_loss_compensation("simple_glossy_bsdf",wo.z,wi.z,this.roughness0,this.multiscatter_tint));
        return scatter_evaluate_result(f: reflect_chance*(this.tint*(fss+fms)),pdf: reflect_chance*lerp(fss_pdf,fms_pdf,MULTISCATTER_DIFFUSE_CHANCE),);
      }
    }
    case scatter_transmit: {
      return scatter_evaluate_result(is_black: true) if(!((dot_wo_wm>0)&(dot_wi_wm<0)));
    }
    default: return scatter_evaluate_result(is_black: true);
    }
  } else {
    const auto D(microfacet::smith_normal_pdf(this.distribution,this.alpha,wm));
    const auto lambdao(microfacet::smith_lambda(this.distribution,#abs(wo.z)/length(this.alpha*wo.xy)));
    const auto lambdai(microfacet::smith_lambda(this.distribution,#abs(wi.z)/length(this.alpha*wi.xy)));
    const auto proj_areao((1+lambdao)*#abs(wo.z));
    const auto proj_areai((1+lambdai)*#abs(wi.z));
    const auto G=return_from{
      if$(this.shadowing<:microfacet::shadowing_smith){
        return mode==scatter_reflect?1/(1+lambdao+lambdai):microfacet::beta(1+lambdao,1+lambdai);
      } else {
        return #min(1,2*wm.z*#min(#abs(wo.z/dot_wo_wm),#abs(wi.z/dot_wi_wm)));
      }
    };
    switch(mode&this.mode){
    case scatter_reflect: {
      const auto fss_pdf(D/(4*float2(proj_areao,proj_areai)+EPSILON));
      const auto fss(D*G/(4*#abs(wo.z)+EPSILON));
      if$(this.multiscatter_tint<:void){
        return scatter_evaluate_result(f: this.tint*(reflect_chance*fss),pdf: reflect_chance*fss_pdf);
      } else {
        const auto fms_pdf(float2(#abs(wi.z),#abs(wo.z))/$PI);
        const auto fms=return_from{
          if$(this.distribution<:microfacet::distribution_ggx){
            return $energy_loss_compensation("microfacet_ggx_smith_bsdf",wo.z,wi.z,this.roughness0,this.multiscatter_tint);
          } else {
            return $energy_loss_compensation("microfacet_beckmann_smith_bsdf",wo.z,wi.z,this.roughness0,this.multiscatter_tint);
          }
        };
        return scatter_evaluate_result(f: reflect_chance*(this.tint*(fss+fms)),pdf: reflect_chance*lerp(fss_pdf,fms_pdf,MULTISCATTER_DIFFUSE_CHANCE),);
      }
    }
    case scatter_transmit: {
      return scatter_evaluate_result(is_black: true) if(!((dot_wo_wm>0)&(dot_wi_wm<0)));
      const auto jac(float2(specular::refraction_half_vector_jacobian(wo,wi,ior),specular::refraction_half_vector_jacobian(wi,wo,1/ior)));
      const auto fss_pdf(D*jac*float2(dot_wo_wm,-dot_wi_wm)/(float2(proj_areao,proj_areai)+EPSILON));
      const auto fss(D*G*jac[0]*dot_wo_wm/(#abs(wo.z)+EPSILON));
      return scatter_evaluate_result(f: this.tint*((1-reflect_chance)*fss),pdf: (1-reflect_chance)*fss_pdf);
    }
    default: return scatter_evaluate_result(is_black: true);
    }
  }
  return scatter_evaluate_result(is_black: true);
}
@(pure noinline)auto scatter_sample(const &microfacet_bsdf this,inline const &scatter_sample_parameters params){
  preserve tangent_u;
  tangent_u=this.tangent_u;
  auto tbn(recalculate_tangent_space(params));
  if(!tbn)
    return scatter_sample_result();
  const auto mode(monte_carlo::bool_sample(&xi.z,scatter_reflect_chance(this.mode))?scatter_reflect:scatter_transmit);
  if(!(this.multiscatter_tint<:void)&&mode==scatter_reflect&&monte_carlo::bool_sample(&xi.w,MULTISCATTER_DIFFUSE_CHANCE)){
    return scatter_sample_result(wi: (*tbn)*monte_carlo::cosine_hemisphere_sample(xi.xy),mode: scatter_reflect);
  }
  const auto wm=return_from{
    if$(this.distribution<:microfacet::distribution_blinn){
      return microfacet::blinn_normal_sample(xi.x,xi.y,2/(this.alpha*this.alpha+EPSILON));
    } else {
      return microfacet::smith_visible_normal_sample(this.distribution,xi.x,xi.y,this.alpha,wo);
    }
  };
  const auto wi=normalize(mode==scatter_reflect?specular::reflect(wo,wm):specular::refract(wo,wm,ior));
  return scatter_sample_result(wi: (*tbn)*wi,mode: mode);
}
@(macro)auto initialize_microfacet_bsdf(
  const float roughness_u,
  const float roughness_v=roughness_u,
  const $(color|float) tint=1.0,
  const $(?(color|float)) multiscatter_tint=null,
  const float3 tangent_u=$state.texture_tangent_u[0],
  const scatter_mode mode=scatter_reflect,
  const string handle="" [[anno::unused()]],
  const microfacet::distribution distribution=microfacet::distribution(),
  const microfacet::shadowing shadowing=microfacet::shadowing(),
){
  if((roughness_u>0)|(roughness_v>0)){
    return microfacet_bsdf(roughness: float2(roughness_u,roughness_v),tint: tint,multiscatter_tint: multiscatter_tint,tangent_u: normalize(tangent_u),mode: mode,distribution: distribution,shadowing: shadowing);
  } else {
    return specular_bsdf(tint: tint,mode: mode);
  }
}
export auto simple_glossy_bsdf(*)=initialize_microfacet_bsdf(distribution: microfacet::distribution_blinn(),shadowing: microfacet::shadowing_vcavities());
export auto microfacet_ggx_smith_bsdf(*)=initialize_microfacet_bsdf(distribution: microfacet::distribution_ggx(),shadowing: microfacet::shadowing_smith());
export auto microfacet_ggx_vcavities_bsdf(*)=initialize_microfacet_bsdf(distribution: microfacet::distribution_ggx(),shadowing: microfacet::shadowing_vcavities());
export auto microfacet_beckmann_smith_bsdf(*)=initialize_microfacet_bsdf(distribution: microfacet::distribution_beckmann(),shadowing: microfacet::shadowing_smith());
export auto microfacet_beckmann_vcavities_bsdf(*)=initialize_microfacet_bsdf(distribution: microfacet::distribution_beckmann(),shadowing: microfacet::shadowing_vcavities());
struct tint1:bsdf,edf,hair_bsdf{
  $(color|float) tint;
  auto base;
};
struct tint2:bsdf{
  $(color|float) reflection_tint;
  $(color|float) transmission_tint;
  bsdf base;
};
export @(pure macro)auto tint(const auto tint,const bsdf base)=tint1(tint,base);
export @(pure macro)auto tint(const auto tint,const edf base)=tint1(tint,base);
export @(pure macro)auto tint(const auto tint,const hair_bsdf base)=tint1(tint,base);
export @(pure macro)auto tint(const auto reflection_tint,const auto transmission_tint,const bsdf base)=tint2(reflection_tint,transmission_tint,base);
@(macro)auto scatter_evaluate(const &tint1 this,const &scatter_evaluate_parameters params){
  auto result(scatter_evaluate(visit &this.base,params));
  if(!result.is_black)
    result.f*=this.tint;
  return result;
}
@(macro)auto scatter_evaluate(const &tint2 this,const &scatter_evaluate_parameters params){
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
@(macro)auto scatter_sample(const &tint1 this,const &scatter_sample_parameters params){
  auto result(scatter_sample(visit &this.base,params));
  if((result.mode!=scatter_none)&bool(result.delta_f))
    *result.delta_f*=this.tint;
  return result;
}
@(macro)auto scatter_sample(const &tint2 this,const &scatter_sample_parameters params){
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
export struct weighted_layer:bsdf{
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
@(macro)auto scatter_evaluate(const &weighted_layer this,inline const &scatter_evaluate_parameters params){
  auto result0(scatter_evaluate(visit &this.base,params));
  preserve normal;
  normal=this.normal;
  auto result1(scatter_evaluate(visit &this.layer,params));
  return scatter_evaluate_result(f: lerp(result0.f,result1.f,this.weight),pdf: lerp(result0.pdf,result1.pdf,this.chance),is_black: result0.is_black&result1.is_black);
}
@(macro)auto scatter_sample(const &weighted_layer this,inline const &scatter_sample_parameters params){
  if(monte_carlo::bool_sample(&xi.w,this.chance)){
    preserve normal;
    normal=this.normal;
    return scatter_sample(visit &this.layer,params);
  } else {
    return scatter_sample(visit &this.base,params);
  }
}
export typedef weighted_layer color_weighted_layer;
export struct fresnel_layer:bsdf{
  $(color|float) ior;
  $(color|float) weight=1.0;
  bsdf layer=bsdf();
  bsdf base=bsdf();
  float3 normal=$state.normal;
  const float av_ior=average(ior);
  const float av_weight=average(weight);
};
@(macro)auto scatter_evaluate(const &fresnel_layer this,inline const &scatter_evaluate_parameters params){
  const auto cos_thetao(dot(wo,this.normal)*#sign(this.normal.z));
  const auto cos_thetai(dot(wi,this.normal)*#sign(this.normal.z));
  if((cos_thetao<EPSILON)|((mode==scatter_reflect)&(cos_thetai<EPSILON))|((mode==scatter_transmit)&(cos_thetai>-EPSILON)))
    return scatter_evaluate_result(is_black: true);
  const auto result0(scatter_evaluate(visit &this.base,params));
  const auto result1=return_from{
    preserve normal,ior;
    normal=this.normal,ior=1/this.av_ior;
    return scatter_evaluate(visit &this.layer,params);
  };
  if(result0.is_black&result1.is_black){
    return scatter_evaluate_result(is_black: true);
  } else {
    return scatter_evaluate_result(f: lerp(result0.f,result1.f,this.weight*specular::dielectric_fresnel(dot(wo,half_direction(params)),1/this.ior)),pdf: lerp(result0.pdf,result1.pdf,this.av_weight*specular::schlick_fresnel(auto(cos_thetao,cos_thetai),specular::schlick_F0(this.av_ior))),);
  }
}
@(macro)auto scatter_sample(const &fresnel_layer this,inline const &scatter_sample_parameters params){
  const auto cos_theta(dot(wo,this.normal)*#sign(this.normal.z));
  if(cos_theta<EPSILON)
    return scatter_sample_result();
  const auto chance(this.av_weight*specular::schlick_fresnel(cos_theta,specular::schlick_F0(this.av_ior)));
  if(monte_carlo::bool_sample(&xi.z,chance)){
    preserve normal,ior;
    normal=this.normal,ior=1/this.av_ior;
    auto result(scatter_sample(visit &this.layer,params));
    *result.delta_f*=this.weight*specular::dielectric_fresnel(dot(wo,half_direction(params,&result)),1/this.ior)/chance if(result.delta_f);
    return result;
  } else {
    auto result(scatter_sample(visit &this.base,params));
    *result.delta_f*=(1-this.weight*specular::dielectric_fresnel(dot(wo,half_direction(params,&result)),1/this.ior))/(1-chance) if(result.delta_f);
    return result;
  }
}
export typedef fresnel_layer color_fresnel_layer;
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
};
export @(pure macro)auto normalized_mix(component[<N>] components){
  float total_weight(0);
  float total_chance(0);
  for(int i=0;i<N;i++){
    auto component(&components[i]);
    component.weight=#max(component.weight,0.0);
    component.chance=#max(component.chance,0.0);
    total_weight+=component.weight;
    total_chance+=component.chance;
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
  return component_mix(components);
}
export @(pure macro)auto clamped_mix(component[<N>] components){
  float total_weight(0);
  float total_chance(0);
  for(int i=0;i<N;i++){
    auto component(&components[i]);
    component.weight=#max(component.weight,0.0);
    component.chance=#max(component.chance,0.0);
    if(total_weight+component.weight<1.0){
      total_weight+=component.weight;
      total_chance+=component.chance;
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
  return component_mix(components);
}
export @(pure macro)auto unbounded_mix(component[<N>] components){
  float total_chance(0);
  for(int i=0;i<N;i++){
    auto component(&components[i]);
    component.weight=#max(component.weight,0.0);
    component.chance=#max(component.chance,0.0);
    total_chance+=component.chance;
  }
  total_chance=1.0/total_chance if(total_chance>0.0);
  for(int i=0;i<N;i++){
    components[i].chance*=total_chance;
  }
  return component_mix(components);
}
@(macro)auto scatter_evaluate(const &component_mix this,const &scatter_evaluate_parameters params){
  auto result(scatter_evaluate_result(f: color(0),is_black: true));
  for(int i=0;i<#num(this.components);i++){
    visit component in this.components[i]{
      auto component_result(scatter_evaluate(&component.component,params));
      if(!component_result.is_black){
        result.pdf+=component.chance*component_result.pdf;
        result.f+=component.weight*component_result.f;
        result.is_black=false;
      }
    }
  }
  return result;
}
@(macro)auto scatter_sample(const &component_mix this,const &scatter_sample_parameters params){
  const auto xi(&params.xi.z);
  for(int i=0;i<#num(this.components);i++){
    visit component in this.components[i]{
      if(!(*xi<component.chance)){
        *xi-=component.chance;
      } else {
        *xi/=component.chance;
        auto result(scatter_sample(&component.component,params));
        if((result.mode!=scatter_none)&bool(result.delta_f))
          *result.delta_f*=component.weight;
        return result;
      }
    }
  }
  return scatter_sample_result();
}
export @(macro)int $scatter_evaluate(
  const &$material_instance instance,
  const &float3 wo,
  const &float3 wi,
  const &float pdf_fwd,
  const &float pdf_rev,
  const &float f,
){
  auto params=scatter_evaluate_parameters(wo0: normalize(*wo),wi0: normalize(*wi),normal: normalize(*instance.normal),thin_walled: instance.mat.thin_walled);
  auto result=instance.mat.backface<:#typeof(material_surface())||!params.hit_backface?scatter_evaluate(visit &instance.mat.surface.scattering,&params):scatter_evaluate(visit &instance.mat.backface.scattering,&params);
  visit result in result{
    if(result.is_black){
      *pdf_fwd=0.0;
      *pdf_rev=0.0;
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
export @(macro)int $scatter_sample(
  const &$material_instance instance,
  const &float4 xi,
  const &float3 wo,
  const &float3 wi,
  const &float pdf_fwd,
  const &float pdf_rev,
  const &float f,
  const &int is_delta,
){
  auto params=scatter_sample_parameters(xi: saturate(*xi),wo0: normalize(*wo),normal: normalize(*instance.normal),thin_walled: instance.mat.thin_walled);
  auto result=instance.mat.backface<:#typeof(material_surface())||!params.hit_backface?scatter_sample(visit &instance.mat.surface.scattering,&params):scatter_sample(visit &instance.mat.backface.scattering,&params);
  visit result in result{
    *wi=#select(params.hit_backface,-result.wi,result.wi);
    if(result.mode==scatter_none||((wo.z<0.0)==(wi.z<0.0))!=(result.mode==scatter_reflect)){
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
export @(pure macro)auto abs(const auto a)=#abs(a);
export @(pure macro)auto all(const auto a)=#all(a);
export @(pure macro)auto any(const auto a)=#any(a);
export @(pure macro)auto max(const auto a,const auto b)=#max(a,b);
export @(pure macro)auto min(const auto a,const auto b)=#min(a,b);
export @(pure macro)auto clamp(const auto a,const auto min,const auto max)=#max(min,#min(a,max));
export @(pure macro)auto saturate(const auto a)=clamp(a,0.0,1.0);
export @(pure macro)auto floor(const auto a)=#floor(a);
export @(pure macro)auto ceil(const auto a)=#ceil(a);
export @(pure macro)auto round(const auto a)=#round(a);
export @(pure macro)auto trunc(const auto a)=#trunc(a);
export @(pure macro)auto frac(const auto a)=a-#floor(a);
export @(pure macro)auto fmod(const auto a,const auto b)=a%b;
export @(pure macro)auto modf(const auto a)=auto[2](a0:=#trunc(a),a-a0);
export @(pure macro)auto isfinite(const auto a)=#isfpclass(a,0b0111111000);
export @(pure macro)auto isnormal(const auto a)=#isfpclass(a,0b0100001000);
export @(pure macro)auto isinf(const auto a)=#isfpclass(a,0b1000000100);
export @(pure macro)auto isnan(const auto a)=#isfpclass(a,0b0000000011);
export @(pure macro)auto sign(const auto a)=#sign(a);
export @(pure macro)auto sqrt(const auto a)=#sqrt(a);
export @(pure macro)auto rsqrt(const auto a)=1.0/#sqrt(a);
export @(pure macro)auto pow(const auto a,const auto b)=#pow(a,b);
export @(pure macro)auto cos(const auto a)=#cos(a);
export @(pure macro)auto sin(const auto a)=#sin(a);
export @(pure macro)auto tan(const auto a)=#tan(a);
export @(pure macro)auto acos(const auto a)=#acos(a);
export @(pure macro)auto asin(const auto a)=#asin(a);
export @(pure macro)auto atan(const auto a)=#atan(a);
export @(pure macro)auto atan2(const auto y,const auto x)=#atan2(y,x);
export @(pure macro)auto cosh(const auto a)=#cosh(a);
export @(pure macro)auto sinh(const auto a)=#sinh(a);
export @(pure macro)auto tanh(const auto a)=#tanh(a);
export @(pure macro)auto sincos(const auto a)=auto[2](#sin(a),#cos(a));
export @(pure macro)auto radians(const auto a)=a*(PI/180.0);
export @(pure macro)auto degrees(const auto a)=a*(180.0/PI);
export @(pure macro)auto exp(const auto a)=#exp(a);
export @(pure macro)auto exp2(const auto a)=#exp2(a);
export @(pure macro)auto exp10(const auto a)=#exp10(a);
export @(pure macro)auto log(const auto a)=#log(a);
export @(pure macro)auto log2(const auto a)=#log2(a);
export @(pure macro)auto log10(const auto a)=#log10(a);
export @(pure macro)auto min_value(const auto a)=#min_value(a);
export @(pure macro)auto max_value(const auto a)=#max_value(a);
export @(pure)float min_value_wavelength(const color a){
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
export @(pure)float max_value_wavelength(const color a){
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
export @(pure macro)auto average(const auto a)=#sum(a)/#num(a);
export @(pure macro)auto lerp(const auto a,const auto b,const auto l)=(1.0-l)*a+l*b;
export @(pure macro)auto step(const auto a,const auto b)=#select(b<a,0.0,1.0);
export @(pure macro)auto smoothstep(const auto a,const auto b,const auto l){
  const auto t(saturate(l));
  const auto s(1-t);
  return s*s*(1+2*t)*a+t*t*(1+2*s)*b;
}
export @(pure macro)auto dot(const auto a,const auto b)=#sum(a*b);
export @(pure macro)auto length(const auto a)=#sqrt(#sum(a*a));
export @(pure macro)auto normalize(const auto a)=a*(1/length(a));
export @(pure macro)auto distance(const auto a,const auto b)=length(b-a);
export @(pure macro)auto cross(const auto a,const auto b)=a.yzx*b.zxy-a.zxy*b.yzx;
export @(pure macro)auto transpose(const auto a)=#transpose(a);
export @(macro)float luminance(const float3 a)=dot(float3(0.2126,0.7152,0.0722),a);
export @(noinline)float luminance(const color a){
  float result(0.0);
  for(int i=0;i<$WAVELENGTH_BASE_MAX;++i){
    result+=$wyman_y($state.wavelength_base[i])*a[i];
  }
  return result/$WAVELENGTH_BASE_MAX;
}
export @(noinline)color blackbody(const float temperature){
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
    int first=0;
    if$($WAVELENGTH_BASE_MAX<=4){
      while(first<$WAVELENGTH_BASE_MAX){
        break if(!($state.wavelength_base[first]<wavelength));
        first++;
      }
    } else {
      int count=$WAVELENGTH_BASE_MAX;
      while(count>0){
        const int step=count/2;
        const int i=first+step;
        if($state.wavelength_base[i]<wavelength){
          first=i+1;
          count=count-step+1;
        } else {
          count=step;
        }
      }
    }
    const int i=clamp(first-1,0,$WAVELENGTH_BASE_MAX-2);
    const float w0=$state.wavelength_base[i];
    const float w1=$state.wavelength_base[i+1];
    return lerp(a[i],a[i+1],saturate((wavelength-w0)/(w1-w0)));
  }
}
)*";

static const char *scene = R"*(#smdl
@(foreign pure)int smdl_data_exists(&void scene_data,string name);
@(foreign)void smdl_data_lookup(&void scene_data,string name,int kind,int size,&void result);
@(macro)auto data_lookup(const string name,auto value){
  const int kind=#is_arithmetic_integral(value)?0:#is_arithmetic_floating_point(value)?1:2;
  smdl_data_lookup($scene_data,name,kind,#num(value),cast<&void>(&value));
  return value;
}
export @(macro)bool data_isvalid(const string name)=smdl_data_exists($scene_data,name)!=0;
export @(macro)int data_lookup_int(const string name,int default_value=int())=data_lookup(name,default_value);
export @(macro)int2 data_lookup_int2(const string name,int2 default_value=int2())=data_lookup(name,default_value);
export @(macro)int3 data_lookup_int3(const string name,int3 default_value=int3())=data_lookup(name,default_value);
export @(macro)int4 data_lookup_int4(const string name,int4 default_value=int4())=data_lookup(name,default_value);
export @(macro)float data_lookup_float(const string name,float default_value=float())=data_lookup(name,default_value);
export @(macro)float2 data_lookup_float2(const string name,float2 default_value=float2())=data_lookup(name,default_value);
export @(macro)float3 data_lookup_float3(const string name,float3 default_value=float3())=data_lookup(name,default_value);
export @(macro)float4 data_lookup_float4(const string name,float4 default_value=float4())=data_lookup(name,default_value);
export @(macro)color data_lookup_color(const string name,color default_value=color())=data_lookup(name,default_value);
)*";

static const char *state = R"*(#smdl
import ::math::*;
export enum coordinate_space{coordinate_internal=0,coordinate_object=1,coordinate_world=2};
export @(macro)float3 position()=$state.position;
export @(macro)float3 normal()=$state.normal;
export @(macro)float3 geometry_normal()=$state.geometry_normal;
export @(macro)float3 motion()=$state.motion;
export @(macro)int texture_space_max()=$state.texture_space_max;
export @(macro)float3 texture_coordinate(const int i)=$state.texture_coordinate[i];
export @(macro)float3 texture_tangent_u(const int i)=$state.texture_tangent_u[i];
export @(macro)float3 texture_tangent_v(const int i)=$state.texture_tangent_v[i];
export @(macro)float3 geometry_tangent_u(const int i)=$state.geometry_tangent_u[i];
export @(macro)float3 geometry_tangent_v(const int i)=$state.geometry_tangent_v[i];
export @(macro)float3x3 tangent_space(const int i)=float3x3($state.texture_tangent_u[i],$state.texture_tangent_v[i],$state.normal);
export @(macro)float3x3 geometry_tangent_space(const int i)=float3x3($state.geometry_tangent_u[i],$state.geometry_tangent_v[i],$state.geometry_normal);
export @(macro)int object_id()=$state.object_id;
export @(macro)float3 direction()=$state.direction;
export @(macro)float animation_time()=$state.animation_time;
export const int WAVELENGTH_BASE_MAX=$WAVELENGTH_BASE_MAX;
export @(macro)float wavelength_min()=$state.wavelength_min;
export @(macro)float wavelength_max()=$state.wavelength_max;
export @(macro)float[WAVELENGTH_BASE_MAX] wavelength_base()=$state.wavelength_base;
export @(macro)float meters_per_scene_unit()=$state.meters_per_scene_unit;
export @(macro)float scene_units_per_meter()=1.0/$state.meters_per_scene_unit;
@(pure)float4x4 affine_inverse(float4x4 matrix){
  return float4x4(
    float4(matrix[0].x,matrix[1].x,matrix[2].x,0.0),
    float4(matrix[0].y,matrix[1].y,matrix[2].y,0.0),
    float4(matrix[0].z,matrix[1].z,matrix[2].z,0.0),
    float4(-#sum(matrix[0]*matrix[3]),-#sum(matrix[1]*matrix[3]),-#sum(matrix[2]*matrix[3]),1.0),
  );
}
export @(macro)float4x4 transform(const coordinate_space from,const coordinate_space to){
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
export @(macro)float3 transform_point(const coordinate_space from,const coordinate_space to,const float3 point){
  return from==to?point:(transform(from,to)*float4(point,1)).xyz;
}
export @(macro)float3 transform_vector(const coordinate_space from,const coordinate_space to,const float3 vector){
  return from==to?vector:(transform(from,to)*float4(vector,0)).xyz;
}
export @(macro)float3 transform_normal(const coordinate_space from,const coordinate_space to,const float3 normal){
  return from==to?normal:(float4(normal,0)*transform(to,from)).xyz;
}
export @(macro)float transform_scale(const coordinate_space from,const coordinate_space to,const float scale){
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
@(pure macro)float4 apply_gamma(const int gamma,const float4 texel)=gamma==int(gamma_srgb)?float4((texel*texel).xyz,texel.w):texel;
@(pure macro)float3 apply_gamma(const int gamma,const float3 texel)=gamma==int(gamma_srgb)?(texel*texel):texel;
@(pure macro)float2 apply_gamma(const int gamma,const float2 texel)=gamma==int(gamma_srgb)?(texel*texel):texel;
@(pure macro)float apply_gamma(const int gamma,const float texel)=gamma==int(gamma_srgb)?(texel*texel):texel;
@(pure macro)int uv_tile_index(const texture_2d tex,const int2 uv_tile){
  return -1 if(#any((uv_tile<0)|(uv_tile>=tex.tile_count)));
  return uv_tile.y*tex.tile_count_u+uv_tile.x;
}
export @(pure macro)int width(const texture_2d tex,const int2 uv_tile=int2(0)){
  const auto i(uv_tile_index(tex,uv_tile));
  return i<0?0:tex.tile_extents[i].x;
}
export @(pure macro)int height(const texture_2d tex,const int2 uv_tile=int2(0)){
  const auto i(uv_tile_index(tex,uv_tile));
  return i<0?0:tex.tile_extents[i].y;
}
export @(pure macro)bool texture_isvalid(const texture_2d tex)=bool(tex.tile_buffers[0]);
export @(pure macro)bool texture_isvalid(const texture_ptex tex)=bool(tex.handle);
@(pure)auto texel_fetch(const texture_2d tex,const int2 coord,const int2 uv_tile=int2(0)){
  const auto i(uv_tile_index(tex,uv_tile));
  return tex.texel_type(0) if(i<0);
  const auto tile_extent(tex.tile_extents[i]);
  const auto tile_buffer(tex.tile_buffers[i]);
  return tex.texel_type(0) if(!tile_buffer|#any((coord<0)|(coord>=tile_extent)));
  return tile_buffer[coord.y*tile_extent.x+coord.x];
}
export @(pure macro)float4 texel_float4(const texture_2d tex,const int2 coord,const int2 uv_tile=int2(0)){
  return apply_gamma(tex.gamma,#unpack_float4(texel_fetch(tex,coord,uv_tile)));
}
export @(pure macro)float3 texel_float3(const texture_2d tex,const int2 coord,const int2 uv_tile=int2(0)){
  return apply_gamma(tex.gamma,#unpack_float4(texel_fetch(tex,coord,uv_tile)).xyz);
}
export @(pure macro)float2 texel_float2(const texture_2d tex,const int2 coord,const int2 uv_tile=int2(0)){
  return apply_gamma(tex.gamma,#unpack_float4(texel_fetch(tex,coord,uv_tile)).xy);
}
export @(pure macro)float texel_float(const texture_2d tex,const int2 coord,const int2 uv_tile=int2(0)){
  return apply_gamma(tex.gamma,#unpack_float4(texel_fetch(tex,coord,uv_tile)).x);
}
export @(pure macro)color texel_color(const texture_2d tex,const int2 coord,const int2 uv_tile=int2(0)){
  return color(texel_float3(tex,coord,uv_tile));
}
export enum wrap_mode{wrap_clamp=0,wrap_repeat=1,wrap_mirrored_repeat=2,wrap_clip=3};
@(pure macro)auto apply_wrap(const auto wrap,const auto n,auto i){
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
export @(pure)float4 lookup_float4(
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
export @(pure macro)float3 lookup_float3(
  const texture_2d tex,
  const float2 coord,
  const wrap_mode wrap_u=wrap_repeat,
  const wrap_mode wrap_v=wrap_repeat,
  const float2 crop_u=float2(0.0,1.0),
  const float2 crop_v=float2(0.0,1.0),
)=lookup_float4(tex,coord,wrap_u,wrap_v,crop_u,crop_v).xyz;
export @(pure macro)float2 lookup_float2(
  const texture_2d tex,
  const float2 coord,
  const wrap_mode wrap_u=wrap_repeat,
  const wrap_mode wrap_v=wrap_repeat,
  const float2 crop_u=float2(0.0,1.0),
  const float2 crop_v=float2(0.0,1.0),
)=lookup_float4(tex,coord,wrap_u,wrap_v,crop_u,crop_v).xy;
export @(pure macro)float lookup_float(
  const texture_2d tex,
  const float2 coord,
  const wrap_mode wrap_u=wrap_repeat,
  const wrap_mode wrap_v=wrap_repeat,
  const float2 crop_u=float2(0.0,1.0),
  const float2 crop_v=float2(0.0,1.0),
)=lookup_float4(tex,coord,wrap_u,wrap_v,crop_u,crop_v).x;
export @(pure macro)color lookup_color(
  const texture_2d tex,
  const float2 coord,
  const wrap_mode wrap_u=wrap_repeat,
  const wrap_mode wrap_v=wrap_repeat,
  const float2 crop_u=float2(0.0,1.0),
  const float2 crop_v=float2(0.0,1.0),
)=color(lookup_float4(tex,coord,wrap_u,wrap_v,crop_u,crop_v).xyz);
@(foreign)void smdl_ptex_evaluate(
  &void tex,
  int gamma,
  int first,
  int num,
  &float result,
);
export @(macro)float4 lookup_float4(const texture_ptex tex,const int channel=0){
  float4 result;
  smdl_ptex_evaluate(tex.handle,tex.gamma,channel,4,&result[0]);
  return result;
}
export @(macro)float3 lookup_float3(const texture_ptex tex,const int channel=0){
  float3 result;
  smdl_ptex_evaluate(tex.handle,tex.gamma,channel,3,&result[0]);
  return result;
}
export @(macro)float2 lookup_float2(const texture_ptex tex,const int channel=0){
  float2 result;
  smdl_ptex_evaluate(tex.handle,tex.gamma,channel,2,&result[0]);
  return result;
}
export @(macro)float lookup_float(const texture_ptex tex,const int channel=0){
  float result;
  smdl_ptex_evaluate(tex.handle,tex.gamma,channel,1,&result);
  return result;
}
export @(macro)color lookup_color(const texture_ptex tex,const int channel=0){
  float3 result;
  smdl_ptex_evaluate(tex.handle,tex.gamma,channel,3,&result[0]);
  return color(result);
}
)*";

static const char *PCG32 = R"*(#smdl
const $int64_t PCG32_MULTIPLIER=6364136223846793005;
const $int64_t PCG32_DEFAULT_INCREMENT=1442695040888963407;
export struct PCG32{
  $int64_t state=0;
  $int64_t increment=PCG32_DEFAULT_INCREMENT;
};
export @(pure macro)auto make_PCG32($int64_t seed){
  auto pcg(PCG32(state: seed));
  pcg.state=pcg.state+pcg.increment;
  pcg.state=pcg.state*PCG32_MULTIPLIER+pcg.increment;
  return pcg;
}
export @(pure macro)auto make_PCG32($int64_t seed,$int64_t stream){
  auto pcg(PCG32(state: seed,increment: (stream<<1)|1));
  pcg.state=pcg.state+pcg.increment;
  pcg.state=pcg.state*PCG32_MULTIPLIER+pcg.increment;
  return pcg;
}
export @(pure)$int32_t generate_int(inline const &PCG32 this){
  state=state*PCG32_MULTIPLIER+increment;
  return #rotr($int32_t(((state>>>18)^state)>>>27),$int32_t(31&(state>>>59)));
}
export @(pure)$int32_t generate_int(const &PCG32 this,const $int32_t bound){
  if(bound>1){
    const auto xmin((-bound)%bound);
    while(true){
      const auto x(generate_int(this));
      return x%bound if(x>=xmin);
    }
  }
  return 0;
}
export @(pure)float generate_float(const &PCG32 this){
  return #min(float(#unsigned_to_fp(generate_int(this),double)/4294967296.0d),1.0-$FLOAT_EPS/2);
}
export @(pure)float2 generate_float2(const &PCG32 this)=float2(generate_float(this),generate_float(this));
export @(pure)float3 generate_float3(const &PCG32 this)=float3(generate_float(this),generate_float(this),generate_float(this));
export @(pure)float4 generate_float4(const &PCG32 this)=float4(generate_float(this),generate_float(this),generate_float(this),generate_float(this));
export @(pure)void discard(inline const &PCG32 this,$int64_t n){
  $int64_t aTotal(1);
  $int64_t bTotal(0);
  $int64_t a(PCG32_MULTIPLIER);
  $int64_t b(increment);
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

static const char *PROSPECT = R"*(#smdl
using ::math import *;
export struct PROSPECT_result{
  color reflectance=color(0);
  color transmittance=color(0);
};
export const float PROSPECT_MIN_WAVELENGTH=400.0;
export const float PROSPECT_MAX_WAVELENGTH=2500.0;
export const int PROSPECT_TABLE_SIZE=1051;
export static const auto PROSPECT_TABLE_IORS=float[1051](1.5115000e+00,1.5115000e+00,1.5115000e+00,1.5108000e+00,1.5095000e+00,1.5081000e+00,1.5071000e+00,1.5060000e+00,1.5050000e+00,1.5041000e+00,1.5032000e+00,1.5026000e+00,1.5019000e+00,1.5013000e+00,1.5008000e+00,1.5002000e+00,1.4997000e+00,1.4992000e+00,1.4988000e+00,1.4984000e+00,1.4980000e+00,1.4974000e+00,1.4969000e+00,1.4964000e+00,1.4959000e+00,1.4955000e+00,1.4951000e+00,1.4947000e+00,1.4943000e+00,1.4940000e+00,1.4937000e+00,1.4934000e+00,1.4931000e+00,1.4928000e+00,1.4925000e+00,1.4923000e+00,1.4920000e+00,1.4917000e+00,1.4915000e+00,1.4912000e+00,1.4910000e+00,1.4907000e+00,1.4904000e+00,1.4902000e+00,1.4899000e+00,1.4896000e+00,1.4893000e+00,1.4890000e+00,1.4887000e+00,1.4884000e+00,1.4880000e+00,1.4876000e+00,1.4873000e+00,1.4869000e+00,1.4865000e+00,1.4861000e+00,1.4856000e+00,1.4851000e+00,1.4846000e+00,1.4841000e+00,1.4836000e+00,1.4830000e+00,1.4825000e+00,1.4819000e+00,1.4813000e+00,1.4807000e+00,1.4801000e+00,1.4794000e+00,1.4788000e+00,1.4781000e+00,1.4774000e+00,1.4767000e+00,1.4760000e+00,1.4753000e+00,1.4746000e+00,1.4739000e+00,1.4732000e+00,1.4725000e+00,1.4717000e+00,1.4709000e+00,1.4701000e+00,1.4693000e+00,1.4685000e+00,1.4677000e+00,1.4670000e+00,1.4662000e+00,1.4654000e+00,1.4647000e+00,1.4639000e+00,1.4632000e+00,1.4624000e+00,1.4616000e+00,1.4609000e+00,1.4602000e+00,1.4595000e+00,1.4588000e+00,1.4582000e+00,1.4576000e+00,1.4570000e+00,1.4565000e+00,1.4559000e+00,1.4553000e+00,1.4548000e+00,1.4543000e+00,1.4538000e+00,1.4533000e+00,1.4528000e+00,1.4523000e+00,1.4519000e+00,1.4514000e+00,1.4510000e+00,1.4506000e+00,1.4502000e+00,1.4498000e+00,1.4495000e+00,1.4492000e+00,1.4489000e+00,1.4486000e+00,1.4484000e+00,1.4482000e+00,1.4480000e+00,1.4478000e+00,1.4477000e+00,1.4475000e+00,1.4474000e+00,1.4473000e+00,1.4472000e+00,1.4471000e+00,1.4470000e+00,1.4469000e+00,1.4468000e+00,1.4468000e+00,1.4467000e+00,1.4466000e+00,1.4465000e+00,1.4464000e+00,1.4463000e+00,1.4462000e+00,1.4461000e+00,1.4460000e+00,1.4458000e+00,1.4457000e+00,1.4456000e+00,1.4454000e+00,1.4453000e+00,1.4451000e+00,1.4450000e+00,1.4449000e+00,1.4447000e+00,1.4446000e+00,1.4444000e+00,1.4442000e+00,1.4440000e+00,1.4438000e+00,1.4435000e+00,1.4433000e+00,1.4430000e+00,1.4427000e+00,1.4423000e+00,1.4420000e+00,1.4417000e+00,1.4413000e+00,1.4409000e+00,1.4405000e+00,1.4402000e+00,1.4398000e+00,1.4394000e+00,1.4391000e+00,1.4387000e+00,1.4384000e+00,1.4380000e+00,1.4377000e+00,1.4374000e+00,1.4371000e+00,1.4368000e+00,1.4366000e+00,1.4363000e+00,1.4360000e+00,1.4357000e+00,1.4354000e+00,1.4352000e+00,1.4350000e+00,1.4348000e+00,1.4346000e+00,1.4345000e+00,1.4343000e+00,1.4342000e+00,1.4341000e+00,1.4341000e+00,1.4340000e+00,1.4340000e+00,1.4340000e+00,1.4340000e+00,1.4340000e+00,1.4341000e+00,1.4342000e+00,1.4342000e+00,1.4343000e+00,1.4343000e+00,1.4344000e+00,1.4345000e+00,1.4346000e+00,1.4347000e+00,1.4347000e+00,1.4347000e+00,1.4347000e+00,1.4347000e+00,1.4347000e+00,1.4347000e+00,1.4347000e+00,1.4347000e+00,1.4347000e+00,1.4347000e+00,1.4347000e+00,1.4348000e+00,1.4348000e+00,1.4348000e+00,1.4348000e+00,1.4348000e+00,1.4348000e+00,1.4348000e+00,1.4348000e+00,1.4348000e+00,1.4348000e+00,1.4347000e+00,1.4347000e+00,1.4347000e+00,1.4347000e+00,1.4347000e+00,1.4346000e+00,1.4346000e+00,1.4346000e+00,1.4345000e+00,1.4345000e+00,1.4345000e+00,1.4345000e+00,1.4345000e+00,1.4344000e+00,1.4344000e+00,1.4343000e+00,1.4342000e+00,1.4342000e+00,1.4341000e+00,1.4341000e+00,1.4340000e+00,1.4340000e+00,1.4339000e+00,1.4338000e+00,1.4338000e+00,1.4337000e+00,1.4337000e+00,1.4336000e+00,1.4335000e+00,1.4335000e+00,1.4334000e+00,1.4334000e+00,1.4333000e+00,1.4332000e+00,1.4332000e+00,1.4331000e+00,1.4331000e+00,1.4330000e+00,1.4329000e+00,1.4329000e+00,1.4328000e+00,1.4327000e+00,1.4326000e+00,1.4325000e+00,1.4324000e+00,1.4323000e+00,1.4322000e+00,1.4321000e+00,1.4320000e+00,1.4320000e+00,1.4319000e+00,1.4318000e+00,1.4317000e+00,1.4316000e+00,1.4316000e+00,1.4315000e+00,1.4314000e+00,1.4313000e+00,1.4312000e+00,1.4310000e+00,1.4309000e+00,1.4308000e+00,1.4307000e+00,1.4306000e+00,1.4304000e+00,1.4303000e+00,1.4302000e+00,1.4300000e+00,1.4299000e+00,1.4297000e+00,1.4296000e+00,1.4295000e+00,1.4293000e+00,1.4292000e+00,1.4290000e+00,1.4289000e+00,1.4287000e+00,1.4285000e+00,1.4284000e+00,1.4282000e+00,1.4281000e+00,1.4279000e+00,1.4277000e+00,1.4275000e+00,1.4273000e+00,1.4272000e+00,1.4270000e+00,1.4268000e+00,1.4266000e+00,1.4264000e+00,1.4263000e+00,1.4261000e+00,1.4259000e+00,1.4257000e+00,1.4255000e+00,1.4253000e+00,1.4251000e+00,1.4249000e+00,1.4247000e+00,1.4245000e+00,1.4242000e+00,1.4240000e+00,1.4238000e+00,1.4236000e+00,1.4234000e+00,1.4232000e+00,1.4230000e+00,1.4228000e+00,1.4225000e+00,1.4223000e+00,1.4220000e+00,1.4218000e+00,1.4216000e+00,1.4214000e+00,1.4212000e+00,1.4209000e+00,1.4207000e+00,1.4205000e+00,1.4202000e+00,1.4200000e+00,1.4197000e+00,1.4195000e+00,1.4193000e+00,1.4190000e+00,1.4188000e+00,1.4185000e+00,1.4183000e+00,1.4181000e+00,1.4178000e+00,1.4176000e+00,1.4173000e+00,1.4171000e+00,1.4169000e+00,1.4166000e+00,1.4164000e+00,1.4161000e+00,1.4159000e+00,1.4157000e+00,1.4155000e+00,1.4153000e+00,1.4150000e+00,1.4148000e+00,1.4146000e+00,1.4144000e+00,1.4142000e+00,1.4139000e+00,1.4137000e+00,1.4135000e+00,1.4132000e+00,1.4130000e+00,1.4128000e+00,1.4126000e+00,1.4124000e+00,1.4121000e+00,1.4119000e+00,1.4117000e+00,1.4115000e+00,1.4113000e+00,1.4110000e+00,1.4108000e+00,1.4106000e+00,1.4104000e+00,1.4102000e+00,1.4100000e+00,1.4098000e+00,1.4096000e+00,1.4094000e+00,1.4092000e+00,1.4089000e+00,1.4087000e+00,1.4085000e+00,1.4083000e+00,1.4081000e+00,1.4079000e+00,1.4077000e+00,1.4075000e+00,1.4073000e+00,1.4071000e+00,1.4069000e+00,1.4067000e+00,1.4065000e+00,1.4063000e+00,1.4061000e+00,1.4059000e+00,1.4057000e+00,1.4054000e+00,1.4052000e+00,1.4050000e+00,1.4048000e+00,1.4046000e+00,1.4044000e+00,1.4042000e+00,1.4040000e+00,1.4037000e+00,1.4035000e+00,1.4033000e+00,1.4031000e+00,1.4029000e+00,1.4027000e+00,1.4025000e+00,1.4023000e+00,1.4021000e+00,1.4019000e+00,1.4016000e+00,1.4014000e+00,1.4012000e+00,1.4010000e+00,1.4008000e+00,1.4006000e+00,1.4004000e+00,1.4001000e+00,1.3999000e+00,1.3997000e+00,1.3995000e+00,1.3993000e+00,1.3991000e+00,1.3989000e+00,1.3987000e+00,1.3984000e+00,1.3982000e+00,1.3980000e+00,1.3978000e+00,1.3976000e+00,1.3974000e+00,1.3972000e+00,1.3970000e+00,1.3968000e+00,1.3966000e+00,1.3964000e+00,1.3962000e+00,1.3960000e+00,1.3958000e+00,1.3956000e+00,1.3954000e+00,1.3952000e+00,1.3949000e+00,1.3947000e+00,1.3945000e+00,1.3943000e+00,1.3941000e+00,1.3939000e+00,1.3937000e+00,1.3935000e+00,1.3933000e+00,1.3931000e+00,1.3929000e+00,1.3927000e+00,1.3925000e+00,1.3923000e+00,1.3921000e+00,1.3919000e+00,1.3917000e+00,1.3915000e+00,1.3913000e+00,1.3911000e+00,1.3909000e+00,1.3907000e+00,1.3905000e+00,1.3903000e+00,1.3901000e+00,1.3899000e+00,1.3897000e+00,1.3895000e+00,1.3892000e+00,1.3890000e+00,1.3888000e+00,1.3886000e+00,1.3884000e+00,1.3882000e+00,1.3880000e+00,1.3877000e+00,1.3875000e+00,1.3873000e+00,1.3871000e+00,1.3869000e+00,1.3867000e+00,1.3865000e+00,1.3863000e+00,1.3860000e+00,1.3858000e+00,1.3855000e+00,1.3853000e+00,1.3851000e+00,1.3848000e+00,1.3846000e+00,1.3843000e+00,1.3841000e+00,1.3839000e+00,1.3836000e+00,1.3834000e+00,1.3831000e+00,1.3829000e+00,1.3826000e+00,1.3823000e+00,1.3821000e+00,1.3818000e+00,1.3816000e+00,1.3813000e+00,1.3810000e+00,1.3808000e+00,1.3805000e+00,1.3803000e+00,1.3800000e+00,1.3797000e+00,1.3794000e+00,1.3791000e+00,1.3788000e+00,1.3785000e+00,1.3782000e+00,1.3779000e+00,1.3776000e+00,1.3773000e+00,1.3770000e+00,1.3767000e+00,1.3764000e+00,1.3761000e+00,1.3758000e+00,1.3755000e+00,1.3752000e+00,1.3748000e+00,1.3745000e+00,1.3742000e+00,1.3739000e+00,1.3736000e+00,1.3732000e+00,1.3729000e+00,1.3726000e+00,1.3723000e+00,1.3720000e+00,1.3716000e+00,1.3713000e+00,1.3710000e+00,1.3706000e+00,1.3703000e+00,1.3699000e+00,1.3696000e+00,1.3693000e+00,1.3690000e+00,1.3687000e+00,1.3684000e+00,1.3681000e+00,1.3678000e+00,1.3675000e+00,1.3672000e+00,1.3668000e+00,1.3665000e+00,1.3661000e+00,1.3658000e+00,1.3655000e+00,1.3651000e+00,1.3648000e+00,1.3645000e+00,1.3641000e+00,1.3638000e+00,1.3634000e+00,1.3631000e+00,1.3628000e+00,1.3625000e+00,1.3622000e+00,1.3618000e+00,1.3615000e+00,1.3612000e+00,1.3608000e+00,1.3605000e+00,1.3601000e+00,1.3598000e+00,1.3595000e+00,1.3592000e+00,1.3589000e+00,1.3585000e+00,1.3582000e+00,1.3579000e+00,1.3576000e+00,1.3573000e+00,1.3569000e+00,1.3566000e+00,1.3563000e+00,1.3560000e+00,1.3557000e+00,1.3553000e+00,1.3550000e+00,1.3547000e+00,1.3544000e+00,1.3541000e+00,1.3537000e+00,1.3534000e+00,1.3531000e+00,1.3528000e+00,1.3525000e+00,1.3521000e+00,1.3518000e+00,1.3515000e+00,1.3512000e+00,1.3509000e+00,1.3505000e+00,1.3502000e+00,1.3499000e+00,1.3496000e+00,1.3493000e+00,1.3490000e+00,1.3487000e+00,1.3484000e+00,1.3481000e+00,1.3478000e+00,1.3475000e+00,1.3472000e+00,1.3469000e+00,1.3466000e+00,1.3463000e+00,1.3459000e+00,1.3456000e+00,1.3453000e+00,1.3450000e+00,1.3447000e+00,1.3445000e+00,1.3442000e+00,1.3439000e+00,1.3436000e+00,1.3433000e+00,1.3431000e+00,1.3428000e+00,1.3425000e+00,1.3422000e+00,1.3419000e+00,1.3417000e+00,1.3414000e+00,1.3411000e+00,1.3409000e+00,1.3406000e+00,1.3404000e+00,1.3401000e+00,1.3398000e+00,1.3396000e+00,1.3393000e+00,1.3391000e+00,1.3388000e+00,1.3386000e+00,1.3383000e+00,1.3380000e+00,1.3378000e+00,1.3376000e+00,1.3374000e+00,1.3372000e+00,1.3369000e+00,1.3367000e+00,1.3365000e+00,1.3363000e+00,1.3361000e+00,1.3358000e+00,1.3356000e+00,1.3354000e+00,1.3352000e+00,1.3350000e+00,1.3348000e+00,1.3346000e+00,1.3344000e+00,1.3342000e+00,1.3340000e+00,1.3338000e+00,1.3336000e+00,1.3334000e+00,1.3332000e+00,1.3330000e+00,1.3328000e+00,1.3326000e+00,1.3324000e+00,1.3322000e+00,1.3320000e+00,1.3319000e+00,1.3317000e+00,1.3316000e+00,1.3314000e+00,1.3312000e+00,1.3310000e+00,1.3308000e+00,1.3307000e+00,1.3305000e+00,1.3303000e+00,1.3302000e+00,1.3300000e+00,1.3299000e+00,1.3297000e+00,1.3295000e+00,1.3294000e+00,1.3292000e+00,1.3291000e+00,1.3289000e+00,1.3287000e+00,1.3286000e+00,1.3284000e+00,1.3283000e+00,1.3281000e+00,1.3279000e+00,1.3278000e+00,1.3276000e+00,1.3275000e+00,1.3273000e+00,1.3271000e+00,1.3270000e+00,1.3268000e+00,1.3267000e+00,1.3266000e+00,1.3264000e+00,1.3263000e+00,1.3261000e+00,1.3260000e+00,1.3259000e+00,1.3257000e+00,1.3256000e+00,1.3254000e+00,1.3253000e+00,1.3252000e+00,1.3250000e+00,1.3249000e+00,1.3247000e+00,1.3246000e+00,1.3245000e+00,1.3243000e+00,1.3242000e+00,1.3240000e+00,1.3239000e+00,1.3238000e+00,1.3236000e+00,1.3235000e+00,1.3233000e+00,1.3232000e+00,1.3231000e+00,1.3230000e+00,1.3229000e+00,1.3227000e+00,1.3226000e+00,1.3225000e+00,1.3224000e+00,1.3223000e+00,1.3221000e+00,1.3220000e+00,1.3218000e+00,1.3217000e+00,1.3216000e+00,1.3214000e+00,1.3213000e+00,1.3211000e+00,1.3210000e+00,1.3209000e+00,1.3207000e+00,1.3206000e+00,1.3204000e+00,1.3203000e+00,1.3202000e+00,1.3200000e+00,1.3199000e+00,1.3198000e+00,1.3197000e+00,1.3196000e+00,1.3194000e+00,1.3193000e+00,1.3191000e+00,1.3190000e+00,1.3189000e+00,1.3187000e+00,1.3186000e+00,1.3184000e+00,1.3183000e+00,1.3182000e+00,1.3180000e+00,1.3179000e+00,1.3177000e+00,1.3175000e+00,1.3174000e+00,1.3172000e+00,1.3171000e+00,1.3169000e+00,1.3167000e+00,1.3166000e+00,1.3164000e+00,1.3163000e+00,1.3161000e+00,1.3159000e+00,1.3158000e+00,1.3156000e+00,1.3154000e+00,1.3152000e+00,1.3150000e+00,1.3149000e+00,1.3147000e+00,1.3146000e+00,1.3144000e+00,1.3142000e+00,1.3140000e+00,1.3138000e+00,1.3136000e+00,1.3134000e+00,1.3132000e+00,1.3130000e+00,1.3128000e+00,1.3126000e+00,1.3124000e+00,1.3122000e+00,1.3120000e+00,1.3118000e+00,1.3116000e+00,1.3114000e+00,1.3112000e+00,1.3109000e+00,1.3107000e+00,1.3105000e+00,1.3103000e+00,1.3101000e+00,1.3098000e+00,1.3096000e+00,1.3094000e+00,1.3092000e+00,1.3090000e+00,1.3087000e+00,1.3085000e+00,1.3082000e+00,1.3080000e+00,1.3078000e+00,1.3075000e+00,1.3073000e+00,1.3070000e+00,1.3068000e+00,1.3066000e+00,1.3063000e+00,1.3061000e+00,1.3059000e+00,1.3057000e+00,1.3055000e+00,1.3052000e+00,1.3050000e+00,1.3047000e+00,1.3045000e+00,1.3043000e+00,1.3040000e+00,1.3038000e+00,1.3035000e+00,1.3033000e+00,1.3031000e+00,1.3028000e+00,1.3026000e+00,1.3023000e+00,1.3021000e+00,1.3019000e+00,1.3016000e+00,1.3014000e+00,1.3011000e+00,1.3009000e+00,1.3007000e+00,1.3004000e+00,1.3002000e+00,1.2999000e+00,1.2997000e+00,1.2995000e+00,1.2992000e+00,1.2990000e+00,1.2987000e+00,1.2985000e+00,1.2983000e+00,1.2980000e+00,1.2978000e+00,1.2975000e+00,1.2973000e+00,1.2970000e+00,1.2967000e+00,1.2965000e+00,1.2962000e+00,1.2960000e+00,1.2958000e+00,1.2956000e+00,1.2954000e+00,1.2951000e+00,1.2949000e+00,1.2947000e+00,1.2944000e+00,1.2942000e+00,1.2939000e+00,1.2937000e+00,1.2935000e+00,1.2932000e+00,1.2930000e+00,1.2927000e+00,1.2924000e+00,1.2922000e+00,1.2919000e+00,1.2917000e+00,1.2914000e+00,1.2912000e+00,1.2910000e+00,1.2907000e+00,1.2905000e+00,1.2902000e+00,1.2900000e+00,1.2898000e+00,1.2895000e+00,1.2893000e+00,1.2890000e+00,1.2888000e+00,1.2886000e+00,1.2883000e+00,1.2881000e+00,1.2878000e+00,1.2876000e+00,1.2874000e+00,1.2872000e+00,1.2870000e+00,1.2867000e+00,1.2865000e+00,1.2863000e+00,1.2861000e+00,1.2859000e+00,1.2856000e+00,1.2854000e+00,1.2852000e+00,1.2849000e+00,1.2847000e+00,1.2845000e+00,1.2843000e+00,1.2841000e+00,1.2839000e+00,1.2837000e+00,1.2834000e+00,1.2832000e+00,1.2830000e+00,1.2828000e+00,1.2826000e+00,1.2824000e+00,1.2822000e+00,1.2820000e+00,1.2817000e+00,1.2815000e+00,1.2813000e+00,1.2811000e+00,1.2809000e+00,1.2807000e+00,1.2805000e+00,1.2803000e+00,1.2801000e+00,1.2799000e+00,1.2798000e+00,1.2796000e+00,1.2795000e+00,1.2793000e+00,1.2791000e+00,1.2790000e+00,1.2788000e+00,1.2787000e+00,1.2786000e+00,1.2785000e+00,1.2784000e+00,1.2782000e+00,1.2780000e+00,1.2778000e+00,1.2776000e+00,1.2775000e+00,1.2773000e+00,1.2771000e+00,1.2769000e+00,1.2767000e+00,1.2765000e+00,1.2763000e+00,1.2761000e+00,1.2759000e+00,1.2757000e+00,1.2756000e+00,1.2754000e+00,1.2753000e+00,1.2751000e+00,1.2749000e+00,1.2748000e+00,1.2746000e+00,1.2745000e+00,1.2743000e+00,1.2742000e+00,1.2741000e+00,1.2739000e+00,1.2738000e+00,1.2737000e+00,1.2736000e+00,1.2735000e+00,1.2733000e+00,1.2732000e+00,1.2731000e+00,1.2730000e+00,1.2729000e+00,1.2727000e+00,1.2726000e+00,1.2725000e+00,1.2724000e+00,1.2723000e+00,1.2722000e+00,1.2721000e+00,1.2720000e+00,1.2719000e+00,1.2718000e+00,1.2717000e+00,1.2716000e+00,1.2715000e+00,1.2714000e+00,1.2713000e+00,1.2713000e+00,1.2712000e+00,1.2712000e+00,1.2711000e+00,1.2710000e+00,1.2710000e+00,1.2709000e+00,1.2709000e+00,1.2709000e+00,1.2708000e+00,1.2708000e+00,1.2708000e+00,1.2708000e+00,1.2708000e+00,1.2708000e+00,1.2708000e+00,1.2709000e+00,1.2710000e+00,1.2712000e+00,1.2713000e+00,1.2715000e+00,1.2717000e+00,1.2719000e+00,1.2722000e+00,1.2725000e+00,1.2728000e+00,1.2732000e+00,1.2736000e+00);
export static const auto PROSPECT_TABLE_ABSORPTIONS=auto[1051](
  auto(6.4881500e-02,1.6734000e-01,6.6674700e-02,5.2720000e-01,5.8000000e-05,1.0970000e+02,0.0000000e+00,1.2793000e+02),
  auto(6.8551300e-02,1.6718400e-01,6.1933200e-02,5.2520000e-01,5.9000000e-05,9.7980000e+01,0.0000000e+00,1.1426300e+02),
  auto(7.0900000e-02,1.6761300e-01,5.8277000e-02,5.2320000e-01,6.1000000e-05,8.7130000e+01,0.0000000e+00,1.0160900e+02),
  auto(7.1320200e-02,1.6751300e-01,5.5365700e-02,5.2120000e-01,6.3000000e-05,7.8060000e+01,0.0000000e+00,9.1032200e+01),
  auto(7.1223100e-02,1.6723900e-01,5.3115800e-02,5.1920000e-01,6.5000000e-05,7.0130000e+01,0.0000000e+00,8.1784400e+01),
  auto(7.1491400e-02,1.6588200e-01,5.0951500e-02,5.1720000e-01,6.7000000e-05,6.3000000e+01,0.0000000e+00,7.3469500e+01),
  auto(7.2018500e-02,1.6544600e-01,4.9387300e-02,5.1520000e-01,6.9000000e-05,5.6160000e+01,0.0000000e+00,6.5492800e+01),
  auto(7.1493400e-02,1.6608800e-01,4.7876100e-02,5.1320000e-01,7.2000000e-05,5.0010000e+01,0.0000000e+00,5.8320800e+01),
  auto(7.0762900e-02,1.6628800e-01,4.6898700e-02,5.1120000e-01,7.4000000e-05,4.4630000e+01,0.0000000e+00,5.2046700e+01),
  auto(7.0094000e-02,1.6662200e-01,4.6157200e-02,5.0920000e-01,7.6000000e-05,3.9960000e+01,0.0000000e+00,4.6600600e+01),
  auto(6.9819300e-02,1.6716400e-01,4.5428600e-02,5.0720000e-01,7.9000000e-05,3.5670000e+01,0.0000000e+00,4.1597700e+01),
  auto(7.0281500e-02,1.6798000e-01,4.4845100e-02,5.0520000e-01,8.2000000e-05,3.1700000e+01,0.0000000e+00,3.6968000e+01),
  auto(7.0472700e-02,1.6859900e-01,4.4249500e-02,5.0320000e-01,8.4000000e-05,2.8320000e+01,0.0000000e+00,3.3026300e+01),
  auto(7.0809500e-02,1.6814200e-01,4.3957600e-02,5.0120000e-01,8.7000000e-05,2.5350000e+01,0.0000000e+00,2.9562700e+01),
  auto(7.1622300e-02,1.6772500e-01,4.3804600e-02,4.9920000e-01,8.9000000e-05,2.2760000e+01,0.0000000e+00,2.6542300e+01),
  auto(7.2449100e-02,1.6755100e-01,4.3861200e-02,4.9720000e-01,9.2000000e-05,2.0240000e+01,0.0000000e+00,2.3603500e+01),
  auto(7.3652100e-02,1.6790500e-01,4.3958800e-02,4.9480000e-01,9.4000000e-05,1.7850000e+01,0.0000000e+00,2.0816400e+01),
  auto(7.4334900e-02,1.6793700e-01,4.4110500e-02,4.9240000e-01,9.7000000e-05,1.5750000e+01,0.0000000e+00,1.8367400e+01),
  auto(7.4691100e-02,1.6817700e-01,4.4276800e-02,4.9000000e-01,9.9000000e-05,1.3920000e+01,0.0000000e+00,1.6233300e+01),
  auto(7.4570300e-02,1.6869500e-01,4.4509500e-02,4.8760000e-01,1.0200000e-04,1.2330000e+01,0.0000000e+00,1.4379000e+01),
  auto(7.3794200e-02,1.6956900e-01,4.4786500e-02,4.8520000e-01,1.0400000e-04,1.0960000e+01,0.0000000e+00,1.2781400e+01),
  auto(7.1622300e-02,1.6963100e-01,4.5061500e-02,4.8290000e-01,1.0600000e-04,9.9240000e+00,0.0000000e+00,1.1573200e+01),
  auto(6.9104700e-02,1.6990500e-01,4.5415400e-02,4.8050000e-01,1.0800000e-04,8.9470000e+00,0.0000000e+00,1.0433800e+01),
  auto(6.6026400e-02,1.6977000e-01,4.5752800e-02,4.7810000e-01,1.1000000e-04,8.0870000e+00,0.0000000e+00,9.4309200e+00),
  auto(6.2668100e-02,1.6934500e-01,4.5994000e-02,4.7570000e-01,1.1200000e-04,7.2680000e+00,0.0000000e+00,8.4758100e+00),
  auto(5.8774700e-02,1.6744100e-01,4.6413600e-02,4.7330000e-01,1.1400000e-04,6.6600000e+00,0.0000000e+00,7.7667700e+00),
  auto(5.4732400e-02,1.6446400e-01,4.6953000e-02,4.7080000e-01,1.1600000e-04,6.2220000e+00,0.0000000e+00,7.2559900e+00),
  auto(5.1125400e-02,1.6136200e-01,4.7360000e-02,4.6830000e-01,1.1800000e-04,5.7820000e+00,0.0000000e+00,6.7428700e+00),
  auto(4.8139000e-02,1.5822400e-01,4.7813800e-02,4.6580000e-01,1.2000000e-04,5.3700000e+00,0.0000000e+00,6.2624000e+00),
  auto(4.5645500e-02,1.5496900e-01,4.8335300e-02,4.6330000e-01,1.2200000e-04,4.9460000e+00,0.0000000e+00,5.7679400e+00),
  auto(4.3873300e-02,1.5167200e-01,4.8839300e-02,4.6080000e-01,1.2400000e-04,4.5750000e+00,0.0000000e+00,5.3352800e+00),
  auto(4.2653300e-02,1.4831900e-01,4.9370500e-02,4.5870000e-01,1.2600000e-04,4.2590000e+00,0.0000000e+00,4.9667700e+00),
  auto(4.1774300e-02,1.4507600e-01,4.9840900e-02,4.5660000e-01,1.2800000e-04,4.0060000e+00,0.0000000e+00,4.6717300e+00),
  auto(4.0911300e-02,1.4200400e-01,5.0420300e-02,4.5460000e-01,1.3000000e-04,3.8530000e+00,0.0000000e+00,4.4933000e+00),
  auto(4.0301700e-02,1.3919100e-01,5.1149800e-02,4.5250000e-01,1.3300000e-04,3.6710000e+00,0.0000000e+00,4.2810600e+00),
  auto(3.9727300e-02,1.3700800e-01,5.1940600e-02,4.5040000e-01,1.3500000e-04,3.4620000e+00,0.0000000e+00,4.0373200e+00),
  auto(3.9292000e-02,1.3548000e-01,5.2819700e-02,4.4840000e-01,1.3800000e-04,3.2820000e+00,0.0000000e+00,3.8274100e+00),
  auto(3.8772600e-02,1.3463900e-01,5.3989700e-02,4.4630000e-01,1.4100000e-04,3.1020000e+00,0.0000000e+00,3.6175000e+00),
  auto(3.8259900e-02,1.3416900e-01,5.4939800e-02,4.4420000e-01,1.4400000e-04,2.9830000e+00,0.0000000e+00,3.4787200e+00),
  auto(3.7510000e-02,1.3374500e-01,5.6154800e-02,4.4220000e-01,1.4800000e-04,2.9130000e+00,0.0000000e+00,3.3970900e+00),
  auto(3.6775700e-02,1.3327100e-01,5.7151500e-02,4.4010000e-01,1.5200000e-04,2.8030000e+00,0.0000000e+00,3.2688100e+00),
  auto(3.5760600e-02,1.3217300e-01,5.8265200e-02,4.3750000e-01,1.5700000e-04,2.7510000e+00,0.0000000e+00,3.2081700e+00),
  auto(3.4582900e-02,1.3042200e-01,5.9477800e-02,4.3500000e-01,1.6200000e-04,2.7020000e+00,0.0000000e+00,3.1510200e+00),
  auto(3.3184200e-02,1.2781800e-01,6.0669800e-02,4.3240000e-01,1.6700000e-04,2.6560000e+00,0.0000000e+00,3.0973800e+00),
  auto(3.1518900e-02,1.2456600e-01,6.1809400e-02,4.2980000e-01,1.7400000e-04,2.6130000e+00,0.0000000e+00,3.0472300e+00),
  auto(2.9662200e-02,1.2065500e-01,6.2922400e-02,4.2720000e-01,1.8100000e-04,2.5730000e+00,0.0000000e+00,3.0005900e+00),
  auto(2.7692100e-02,1.1652000e-01,6.3970300e-02,4.2470000e-01,1.8900000e-04,2.5360000e+00,0.0000000e+00,2.9574400e+00),
  auto(2.5590100e-02,1.1227400e-01,6.4964400e-02,4.2210000e-01,1.9800000e-04,2.5020000e+00,0.0000000e+00,2.9177900e+00),
  auto(2.3428300e-02,1.0793000e-01,6.5909600e-02,4.1950000e-01,2.0900000e-04,2.4710000e+00,0.0000000e+00,2.8816400e+00),
  auto(2.1223900e-02,1.0350200e-01,6.6851900e-02,4.1700000e-01,2.2300000e-04,2.4430000e+00,0.0000000e+00,2.8489800e+00),
  auto(1.9048500e-02,9.9004400e-02,6.7770900e-02,4.1440000e-01,2.3800000e-04,2.4170000e+00,0.0000000e+00,2.8186600e+00),
  auto(1.6942900e-02,9.4450200e-02,6.8479200e-02,4.1270000e-01,2.5500000e-04,2.3940000e+00,0.0000000e+00,2.7918400e+00),
  auto(1.4934300e-02,8.9853600e-02,6.9140200e-02,4.1090000e-01,2.7300000e-04,2.3740000e+00,0.0000000e+00,2.7685200e+00),
  auto(1.3049700e-02,8.5228400e-02,6.9563000e-02,4.0920000e-01,2.9100000e-04,2.3560000e+00,0.0000000e+00,2.7475300e+00),
  auto(1.1295900e-02,8.0588400e-02,7.0073500e-02,4.0740000e-01,3.1000000e-04,2.3410000e+00,0.0000000e+00,2.7300300e+00),
  auto(9.7008100e-03,7.5947300e-02,7.0471900e-02,4.0570000e-01,3.2900000e-04,2.3280000e+00,0.0000000e+00,2.7148700e+00),
  auto(8.2461000e-03,7.1319100e-02,7.0871900e-02,4.0380000e-01,3.4900000e-04,2.3180000e+00,0.0000000e+00,2.7032100e+00),
  auto(6.9574300e-03,6.6717500e-02,7.1165200e-02,4.0190000e-01,3.6800000e-04,2.3100000e+00,0.0000000e+00,2.6938800e+00),
  auto(5.8680500e-03,6.2156400e-02,7.1389600e-02,4.0000000e-01,3.8600000e-04,2.3040000e+00,0.0000000e+00,2.6868800e+00),
  auto(4.9912900e-03,5.7649500e-02,7.1573500e-02,3.9810000e-01,4.0400000e-04,2.3010000e+00,0.0000000e+00,2.6833900e+00),
  auto(4.3337900e-03,5.3210600e-02,7.1682100e-02,3.9620000e-01,4.0900000e-04,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(3.8873900e-03,4.8853700e-02,7.1775400e-02,3.9430000e-01,4.1600000e-04,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(3.6525200e-03,4.4592400e-02,7.1864100e-02,3.9240000e-01,4.0900000e-04,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(3.6193400e-03,4.0440700e-02,7.2059700e-02,3.9050000e-01,4.2700000e-04,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(3.7696700e-03,3.6412200e-02,7.2240300e-02,3.8860000e-01,4.2300000e-04,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(4.0689900e-03,3.2520900e-02,7.2349200e-02,3.8670000e-01,4.2900000e-04,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(4.4842800e-03,2.8780600e-02,7.2475800e-02,3.8240000e-01,4.4500000e-04,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(4.9934800e-03,2.5205000e-02,7.2528500e-02,3.7820000e-01,4.5600000e-04,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(5.5645500e-03,2.1807900e-02,7.2506100e-02,3.7390000e-01,4.7000000e-04,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(6.1705900e-03,1.8603300e-02,7.2340400e-02,3.6960000e-01,4.8000000e-04,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(6.7864000e-03,1.5604800e-02,7.2063300e-02,3.6540000e-01,4.9500000e-04,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(7.3880000e-03,1.2826400e-02,7.1602400e-02,3.6250000e-01,5.0300000e-04,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(7.9483300e-03,1.0281800e-02,7.1067100e-02,3.5970000e-01,5.2700000e-04,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(8.4536400e-03,7.9847900e-03,7.0306900e-02,3.5680000e-01,5.4400000e-04,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(8.9036800e-03,5.9492500e-03,6.9355400e-02,3.5400000e-01,5.6400000e-04,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(9.3093900e-03,4.1889800e-03,6.8139800e-02,3.5110000e-01,5.8800000e-04,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(9.6821300e-03,2.7177800e-03,6.6732600e-02,3.4890000e-01,6.1100000e-04,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(1.0040000e-02,1.5494700e-03,6.5061200e-02,3.4670000e-01,6.3100000e-04,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(1.0385500e-02,6.9786300e-04,6.3357500e-02,3.4450000e-01,6.4600000e-04,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(1.0724200e-02,1.7676800e-04,6.1517500e-02,3.4230000e-01,6.5800000e-04,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(1.1048000e-02,2.1316300e-13,5.9651500e-02,3.4010000e-01,6.7200000e-04,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(1.1401500e-02,0.0000000e+00,5.7591000e-02,3.3660000e-01,6.8600000e-04,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(1.1827200e-02,0.0000000e+00,5.5543100e-02,3.3300000e-01,6.9900000e-04,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(1.2340200e-02,0.0000000e+00,5.3243200e-02,3.2940000e-01,7.1800000e-04,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(1.2933000e-02,0.0000000e+00,5.0900000e-02,3.2580000e-01,7.3400000e-04,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(1.3597900e-02,0.0000000e+00,4.8480800e-02,3.2230000e-01,7.5900000e-04,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(1.4301400e-02,0.0000000e+00,4.6174300e-02,3.1820000e-01,7.8700000e-04,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(1.5010200e-02,0.0000000e+00,4.3870500e-02,3.1410000e-01,8.1900000e-04,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(1.5693500e-02,0.0000000e+00,4.1733800e-02,3.1000000e-01,8.5800000e-04,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(1.6339700e-02,0.0000000e+00,3.9543100e-02,3.0590000e-01,8.9600000e-04,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(1.6945200e-02,0.0000000e+00,3.7412000e-02,3.0190000e-01,9.5200000e-04,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(1.7526900e-02,0.0000000e+00,3.5042600e-02,2.9790000e-01,1.0000000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(1.8065800e-02,0.0000000e+00,3.2845800e-02,2.9400000e-01,1.0790000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(1.8567900e-02,0.0000000e+00,3.0606500e-02,2.9000000e-01,1.1590000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(1.9018600e-02,0.0000000e+00,2.8665200e-02,2.8610000e-01,1.2530000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(1.9419100e-02,0.0000000e+00,2.6802900e-02,2.8210000e-01,1.3560000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(1.9766300e-02,0.0000000e+00,2.5133500e-02,2.7840000e-01,1.4590000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(2.0065600e-02,0.0000000e+00,2.3486500e-02,2.7470000e-01,1.5670000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(2.0326600e-02,0.0000000e+00,2.1917600e-02,2.7100000e-01,1.7000000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(2.0584900e-02,0.0000000e+00,2.0284300e-02,2.6730000e-01,1.8600000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(2.0854000e-02,0.0000000e+00,1.8740100e-02,2.6360000e-01,2.2240000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(2.1171600e-02,0.0000000e+00,1.7242100e-02,2.6010000e-01,2.3660000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(2.1545800e-02,0.0000000e+00,1.5987000e-02,2.5660000e-01,2.4480000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(2.2003300e-02,0.0000000e+00,1.4844700e-02,2.5320000e-01,2.5870000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(2.2552500e-02,0.0000000e+00,1.3800300e-02,2.4970000e-01,2.6530000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(2.3176900e-02,0.0000000e+00,1.2801100e-02,2.4630000e-01,2.6910000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(2.3841900e-02,0.0000000e+00,1.1883700e-02,2.4310000e-01,2.7150000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(2.4561000e-02,0.0000000e+00,1.0918400e-02,2.3980000e-01,2.7400000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(2.5269100e-02,0.0000000e+00,1.0021900e-02,2.3660000e-01,2.7640000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(2.5950300e-02,0.0000000e+00,9.1553400e-03,2.3340000e-01,2.7850000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(2.6530300e-02,0.0000000e+00,8.4110300e-03,2.3020000e-01,2.8100000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(2.7009800e-02,0.0000000e+00,7.7072700e-03,2.2730000e-01,2.8390000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(2.7379200e-02,0.0000000e+00,7.0566500e-03,2.2440000e-01,2.8680000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(2.7644900e-02,0.0000000e+00,6.4565700e-03,2.2150000e-01,2.8930000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(2.7821100e-02,0.0000000e+00,5.9044400e-03,2.1850000e-01,2.9220000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(2.7977600e-02,0.0000000e+00,5.3976500e-03,2.1560000e-01,2.9550000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(2.8194400e-02,0.0000000e+00,4.9336200e-03,2.1290000e-01,2.9880000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(2.8572900e-02,0.0000000e+00,4.5097300e-03,2.1020000e-01,3.0110000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(2.9181400e-02,0.0000000e+00,4.1234100e-03,2.0740000e-01,3.0380000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(3.0077100e-02,0.0000000e+00,3.7720400e-03,2.0470000e-01,3.0760000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(3.1247500e-02,0.0000000e+00,3.4530300e-03,2.0200000e-01,3.1110000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(3.2658500e-02,0.0000000e+00,3.1637800e-03,1.9940000e-01,3.1440000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(3.4254600e-02,0.0000000e+00,2.9017000e-03,1.9680000e-01,3.1810000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(3.5948900e-02,0.0000000e+00,2.6641900e-03,1.9420000e-01,3.2230000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(3.7588000e-02,0.0000000e+00,2.4486500e-03,1.9160000e-01,3.2630000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(3.9013600e-02,0.0000000e+00,2.2524700e-03,1.8900000e-01,3.3150000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(4.0221700e-02,0.0000000e+00,2.0730800e-03,1.8650000e-01,3.3620000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(4.1418300e-02,0.0000000e+00,1.9078600e-03,1.8410000e-01,3.4230000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(4.2880000e-02,0.0000000e+00,1.7542200e-03,1.8160000e-01,3.5080000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(4.4861200e-02,0.0000000e+00,1.6095600e-03,1.7920000e-01,3.6360000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(4.7494900e-02,0.0000000e+00,1.4712800e-03,1.7680000e-01,3.7910000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(5.0848700e-02,0.0000000e+00,1.3368000e-03,1.7420000e-01,3.9310000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(5.4699600e-02,0.0000000e+00,1.2035000e-03,1.7170000e-01,4.0190000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(5.8867900e-02,0.0000000e+00,1.0687900e-03,1.6910000e-01,4.0720000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(6.2750000e-02,0.0000000e+00,9.3007700e-04,1.6660000e-01,4.0980000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(6.6112100e-02,0.0000000e+00,7.8475900e-04,1.6410000e-01,4.1220000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(6.8674900e-02,0.0000000e+00,6.3883500e-04,1.6130000e-01,4.1500000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(7.0465200e-02,0.0000000e+00,5.0727100e-04,1.5860000e-01,4.1730000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(7.1401500e-02,0.0000000e+00,3.9030300e-04,1.5590000e-01,4.2230000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(7.1127200e-02,0.0000000e+00,2.8816600e-04,1.5320000e-01,4.2700000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(6.8920600e-02,0.0000000e+00,2.0109700e-04,1.5040000e-01,4.3180000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(6.3947600e-02,0.0000000e+00,1.2933000e-04,1.4780000e-01,4.3810000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(5.6858300e-02,0.0000000e+00,7.3101600e-05,1.4510000e-01,4.4580000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(4.8725800e-02,0.0000000e+00,3.2646600e-05,1.4240000e-01,4.5450000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(4.0858500e-02,0.0000000e+00,8.2009100e-06,1.3970000e-01,4.6460000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(3.3671000e-02,0.0000000e+00,0.0000000e+00,1.3700000e-01,4.7600000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(2.7502100e-02,0.0000000e+00,0.0000000e+00,1.3450000e-01,4.9030000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(2.2330300e-02,0.0000000e+00,0.0000000e+00,1.3200000e-01,5.0710000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(1.8173300e-02,0.0000000e+00,0.0000000e+00,1.2950000e-01,5.2440000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(1.4852100e-02,0.0000000e+00,0.0000000e+00,1.2700000e-01,5.4700000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(1.2240600e-02,0.0000000e+00,0.0000000e+00,1.2450000e-01,5.7220000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(1.0192500e-02,0.0000000e+00,0.0000000e+00,1.2230000e-01,5.9950000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(8.5869500e-03,0.0000000e+00,0.0000000e+00,1.2000000e-01,6.3030000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(7.3125900e-03,0.0000000e+00,0.0000000e+00,1.1780000e-01,6.6280000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(6.2803000e-03,0.0000000e+00,0.0000000e+00,1.1560000e-01,6.9930000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(5.4214400e-03,0.0000000e+00,0.0000000e+00,1.1330000e-01,7.4150000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(4.6965200e-03,0.0000000e+00,0.0000000e+00,1.1110000e-01,7.8930000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(4.0720400e-03,0.0000000e+00,0.0000000e+00,1.0890000e-01,8.4450000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(3.5302000e-03,0.0000000e+00,0.0000000e+00,1.0670000e-01,9.1090000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(3.0574900e-03,0.0000000e+00,0.0000000e+00,1.0460000e-01,9.8710000e-03,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(2.6434000e-03,0.0000000e+00,0.0000000e+00,1.0240000e-01,1.0720000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(2.2809200e-03,0.0000000e+00,0.0000000e+00,1.0030000e-01,1.1680000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(1.9636100e-03,0.0000000e+00,0.0000000e+00,9.8290000e-02,1.2680000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(1.6868800e-03,0.0000000e+00,0.0000000e+00,9.6250000e-02,1.3720000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(1.4467700e-03,0.0000000e+00,0.0000000e+00,9.4220000e-02,1.4870000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(1.2386000e-03,0.0000000e+00,0.0000000e+00,9.2180000e-02,1.6210000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(1.0590700e-03,0.0000000e+00,0.0000000e+00,9.0220000e-02,1.7870000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(9.0408600e-04,0.0000000e+00,0.0000000e+00,8.8270000e-02,1.9920000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(7.7288900e-04,0.0000000e+00,0.0000000e+00,8.6310000e-02,2.2070000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(6.6161800e-04,0.0000000e+00,0.0000000e+00,8.4350000e-02,2.3940000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(5.6764200e-04,0.0000000e+00,0.0000000e+00,8.2390000e-02,2.5320000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(4.8994000e-04,0.0000000e+00,0.0000000e+00,8.0700000e-02,2.6230000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(4.2566500e-04,0.0000000e+00,0.0000000e+00,7.9010000e-02,2.6720000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(3.7315900e-04,0.0000000e+00,0.0000000e+00,7.7320000e-02,2.7020000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(3.3141100e-04,0.0000000e+00,0.0000000e+00,7.5620000e-02,2.7220000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(2.9841400e-04,0.0000000e+00,0.0000000e+00,7.3930000e-02,2.7330000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(2.6796000e-04,0.0000000e+00,0.0000000e+00,7.2450000e-02,2.7410000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(2.3795200e-04,0.0000000e+00,0.0000000e+00,7.0970000e-02,2.7480000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(2.0863600e-04,0.0000000e+00,0.0000000e+00,6.9490000e-02,2.7540000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(1.8025600e-04,0.0000000e+00,0.0000000e+00,6.8010000e-02,2.7630000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(1.5305800e-04,0.0000000e+00,0.0000000e+00,6.6530000e-02,2.7710000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(1.2728700e-04,0.0000000e+00,0.0000000e+00,6.5190000e-02,2.7730000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(1.0318700e-04,0.0000000e+00,0.0000000e+00,6.3850000e-02,2.7740000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(8.1004900e-05,0.0000000e+00,0.0000000e+00,6.2510000e-02,2.7700000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(6.0984700e-05,0.0000000e+00,0.0000000e+00,6.1170000e-02,2.7610000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(4.3372000e-05,0.0000000e+00,0.0000000e+00,5.9830000e-02,2.7540000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(2.8411800e-05,0.0000000e+00,0.0000000e+00,5.8430000e-02,2.7480000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(1.6349400e-05,0.0000000e+00,0.0000000e+00,5.7040000e-02,2.7310000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(7.4298300e-06,0.0000000e+00,0.0000000e+00,5.5640000e-02,2.7100000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(1.8983200e-06,0.0000000e+00,0.0000000e+00,5.4240000e-02,2.6900000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(1.0524900e-13,0.0000000e+00,0.0000000e+00,5.2840000e-02,2.6590000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,5.1670000e-02,2.6330000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,5.0500000e-02,2.6130000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,4.9330000e-02,2.5580000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,4.8160000e-02,2.5130000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,4.6990000e-02,2.4660000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,4.5940000e-02,2.4120000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,4.4900000e-02,2.3740000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,4.3850000e-02,2.3370000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,4.2810000e-02,2.3040000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,4.1760000e-02,2.2460000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,4.0810000e-02,2.2380000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,3.9850000e-02,2.2040000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,3.8890000e-02,2.2040000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,3.7930000e-02,2.1770000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,3.6980000e-02,2.1880000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,3.6150000e-02,2.1980000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,3.5330000e-02,2.2230000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,3.4510000e-02,2.2480000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,3.3680000e-02,2.3040000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,3.2860000e-02,2.3290000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,3.2120000e-02,2.4460000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,3.1390000e-02,2.5160000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,3.0650000e-02,2.7690000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,2.9910000e-02,2.9140000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,2.9180000e-02,3.2140000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,2.8520000e-02,3.4590000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,2.7860000e-02,3.6620000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,2.7200000e-02,3.7880000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,2.6550000e-02,3.8540000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,2.5890000e-02,3.9490000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,2.5290000e-02,4.0000000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,2.4690000e-02,4.0570000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,2.4100000e-02,4.1150000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,2.3500000e-02,4.1490000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,2.2900000e-02,4.1990000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,2.2380000e-02,4.2540000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,2.1860000e-02,4.2800000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,2.1330000e-02,4.3600000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,2.0810000e-02,4.3790000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,2.0290000e-02,4.4540000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.9830000e-02,4.5050000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.9380000e-02,4.5520000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8930000e-02,4.6580000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8470000e-02,4.7050000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8020000e-02,4.7520000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.7620000e-02,4.8670000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.7230000e-02,4.9600000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.6830000e-02,5.0500000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.6430000e-02,5.1530000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.6040000e-02,5.2980000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.5680000e-02,5.3860000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.5330000e-02,5.5280000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.4980000e-02,5.5960000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.4630000e-02,5.7450000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.4280000e-02,5.8310000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.4000000e-02,5.9820000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.3710000e-02,6.0350000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.3430000e-02,6.1850000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.3140000e-02,6.2690000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2860000e-02,6.4070000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2650000e-02,6.5620000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2450000e-02,6.6720000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2250000e-02,6.7690000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2050000e-02,6.9890000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1850000e-02,7.0850000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1650000e-02,7.3580000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1450000e-02,7.5620000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1250000e-02,7.7920000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1050000e-02,8.2920000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.0860000e-02,8.5280000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.0660000e-02,9.2680000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.0470000e-02,9.8190000e-02,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.0270000e-02,1.0420000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.0080000e-02,1.1130000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,9.8840000e-03,1.2460000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,9.6910000e-03,1.3270000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,9.5000000e-03,1.4100000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,9.3090000e-03,1.5570000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,9.1200000e-03,1.6880000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,8.9310000e-03,1.8180000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,8.7430000e-03,2.0500000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,8.5570000e-03,2.1870000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,8.3710000e-03,2.3860000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,8.1870000e-03,2.5420000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,8.0040000e-03,2.9760000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,7.8220000e-03,3.2740000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,7.6410000e-03,3.6220000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,7.4620000e-03,3.9300000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,7.2840000e-03,4.1840000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,7.1070000e-03,4.3850000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,6.9320000e-03,4.6110000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,6.7580000e-03,4.6630000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,6.5850000e-03,4.7330000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,6.4140000e-03,4.7720000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,6.2440000e-03,4.8000000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,6.0760000e-03,4.8270000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,5.9100000e-03,4.8640000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,5.7450000e-03,4.8670000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,5.5810000e-03,4.8570000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,5.4190000e-03,4.8210000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,5.2590000e-03,4.7860000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,5.1010000e-03,4.7380000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,4.9450000e-03,4.6770000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,4.7900000e-03,4.6040000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,4.6370000e-03,4.5320000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,4.4860000e-03,4.4340000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,4.3370000e-03,4.3620000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,4.1900000e-03,4.2650000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,4.0440000e-03,4.1680000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,3.9010000e-03,4.0720000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,3.7600000e-03,3.9630000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,3.6200000e-03,3.8680000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,3.4830000e-03,3.7600000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,3.3480000e-03,3.6400000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,3.2150000e-03,3.5210000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,3.0850000e-03,3.4020000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,2.9560000e-03,3.2970000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,2.8300000e-03,3.1910000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,2.7060000e-03,3.0860000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,2.5850000e-03,2.9570000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,2.4650000e-03,2.8400000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,2.3480000e-03,2.7240000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,2.2340000e-03,2.6210000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,2.1220000e-03,2.5060000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,2.0120000e-03,2.3910000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.9050000e-03,2.3310000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8010000e-03,2.2510000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.6990000e-03,2.1510000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.6000000e-03,2.0640000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.5030000e-03,1.9810000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.4090000e-03,1.9040000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.3180000e-03,1.8410000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2300000e-03,1.7900000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1440000e-03,1.7150000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.0610000e-03,1.6640000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,9.8110000e-04,1.6130000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,9.0400000e-04,1.5620000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,8.2970000e-04,1.5320000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,7.5840000e-04,1.5100000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,6.9010000e-04,1.4750000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,6.2480000e-04,1.4470000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,5.6260000e-04,1.4380000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,5.0340000e-04,1.4260000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,4.4730000e-04,1.4120000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,3.9440000e-04,1.4090000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,3.4460000e-04,1.4060000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,2.9810000e-04,1.4080000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,2.5480000e-04,1.4260000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,2.1480000e-04,1.4380000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.7800000e-04,1.4430000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.4460000e-04,1.4750000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1460000e-04,1.5190000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,8.8040000e-05,1.5470000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,6.4880000e-05,1.5800000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,4.5190000e-05,1.6320000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,2.9010000e-05,1.6770000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.6370000e-05,1.7120000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,7.2970000e-06,1.7770000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8300000e-06,1.8660000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.9060000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.9670000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.0310000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.0790000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.1660000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.2190000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.2980000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.3460000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.3530000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.4500000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.5280000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.6500000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.7650000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.8910000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.1640000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.3780000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.7700000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,4.2810000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,4.7120000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.2020000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.0520000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.8630000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,7.5350000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.5970000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,9.2530000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,9.7690000e-01,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.0410000e+00,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1000000e+00,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1310000e+00,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1500000e+00,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1700000e+00,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1900000e+00,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1960000e+00,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2000000e+00,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2050000e+00,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2070000e+00,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2230000e+00,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2340000e+00,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2290000e+00,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2330000e+00,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2390000e+00,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2440000e+00,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2520000e+00,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2580000e+00,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2620000e+00,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2670000e+00,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2720000e+00,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2770000e+00,2.3000000e+00,0.0000000e+00,2.6822200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2820000e+00,2.3000000e+00,0.0000000e+00,2.6821200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2830000e+00,2.3000000e+00,0.0000000e+00,2.6820800e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2720000e+00,2.3000000e+00,0.0000000e+00,2.6822600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2670000e+00,2.3010000e+00,0.0000000e+00,2.6830600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2730000e+00,2.3020000e+00,0.0000000e+00,2.6847400e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2650000e+00,2.3050000e+00,0.0000000e+00,2.6875900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2580000e+00,2.3080000e+00,0.0000000e+00,2.6914400e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2570000e+00,2.3120000e+00,0.0000000e+00,2.6964200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2470000e+00,2.3170000e+00,0.0000000e+00,2.7023600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2430000e+00,2.3230000e+00,0.0000000e+00,2.7094500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2400000e+00,2.3300000e+00,0.0000000e+00,2.7174600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2240000e+00,2.3380000e+00,0.0000000e+00,2.7261600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2140000e+00,2.3460000e+00,0.0000000e+00,2.7357100e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2130000e+00,2.3540000e+00,0.0000000e+00,2.7459100e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2050000e+00,2.3640000e+00,0.0000000e+00,2.7567400e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1990000e+00,2.3740000e+00,0.0000000e+00,2.7681500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1970000e+00,2.3840000e+00,0.0000000e+00,2.7803100e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1890000e+00,2.3950000e+00,0.0000000e+00,2.7927700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1800000e+00,2.4060000e+00,0.0000000e+00,2.8058400e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1710000e+00,2.4180000e+00,0.0000000e+00,2.8194400e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1610000e+00,2.4300000e+00,0.0000000e+00,2.8335000e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1570000e+00,2.4420000e+00,0.0000000e+00,2.8477600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1550000e+00,2.4540000e+00,0.0000000e+00,2.8622100e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1480000e+00,2.4670000e+00,0.0000000e+00,2.8770000e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1380000e+00,2.4800000e+00,0.0000000e+00,2.8918700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1300000e+00,2.4930000e+00,0.0000000e+00,2.9069900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1230000e+00,2.5060000e+00,0.0000000e+00,2.9221100e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1160000e+00,2.5190000e+00,0.0000000e+00,2.9373500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1070000e+00,2.5320000e+00,0.0000000e+00,2.9525300e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1010000e+00,2.5450000e+00,0.0000000e+00,2.9676800e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1010000e+00,2.5580000e+00,0.0000000e+00,2.9827300e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1000000e+00,2.5700000e+00,0.0000000e+00,2.9975500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.0940000e+00,2.5830000e+00,0.0000000e+00,3.0120600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.0850000e+00,2.5950000e+00,0.0000000e+00,3.0263700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.0830000e+00,2.6070000e+00,0.0000000e+00,3.0404700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.0820000e+00,2.6190000e+00,0.0000000e+00,3.0542900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.0800000e+00,2.6310000e+00,0.0000000e+00,3.0677500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.0800000e+00,2.6420000e+00,0.0000000e+00,3.0805800e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.0870000e+00,2.6520000e+00,0.0000000e+00,3.0929600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.0990000e+00,2.6620000e+00,0.0000000e+00,3.1047000e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1070000e+00,2.6720000e+00,0.0000000e+00,3.1158200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1110000e+00,2.6810000e+00,0.0000000e+00,3.1264100e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1210000e+00,2.6890000e+00,0.0000000e+00,3.1362300e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1370000e+00,2.6970000e+00,0.0000000e+00,3.1451600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1560000e+00,2.7040000e+00,0.0000000e+00,3.1533900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1700000e+00,2.7110000e+00,0.0000000e+00,3.1609900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1810000e+00,2.7160000e+00,0.0000000e+00,3.1675200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1960000e+00,2.7210000e+00,0.0000000e+00,3.1731100e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2160000e+00,2.7250000e+00,0.0000000e+00,3.1776800e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2390000e+00,2.7280000e+00,0.0000000e+00,3.1808400e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2670000e+00,2.7300000e+00,0.0000000e+00,3.1836000e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2970000e+00,2.7310000e+00,0.0000000e+00,3.1857700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.3230000e+00,2.7210000e+00,0.0000000e+00,3.1825500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.3510000e+00,2.7280000e+00,0.0000000e+00,3.1743600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.3920000e+00,2.7120000e+00,0.0000000e+00,3.1627500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.4400000e+00,2.7020000e+00,0.0000000e+00,3.1486500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.4890000e+00,2.6880000e+00,0.0000000e+00,3.1349000e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.5320000e+00,2.6820000e+00,0.0000000e+00,3.1264000e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.5800000e+00,2.6750000e+00,0.0000000e+00,3.1201300e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.6420000e+00,2.6700000e+00,0.0000000e+00,3.1161900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.7010000e+00,2.6680000e+00,0.0000000e+00,3.1151200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.7580000e+00,2.6740000e+00,0.0000000e+00,3.1184200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8310000e+00,2.6830000e+00,0.0000000e+00,3.1208500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.9110000e+00,2.6750000e+00,0.0000000e+00,3.1203000e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.9740000e+00,2.6830000e+00,0.0000000e+00,3.1191500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.0470000e+00,2.6610000e+00,0.0000000e+00,3.1144100e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.1530000e+00,2.6700000e+00,0.0000000e+00,3.1092300e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.2430000e+00,2.6640000e+00,0.0000000e+00,3.1075900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.3120000e+00,2.6650000e+00,0.0000000e+00,3.1125300e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.4150000e+00,2.6810000e+00,0.0000000e+00,3.1223600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.5400000e+00,2.6840000e+00,0.0000000e+00,3.1366000e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.6310000e+00,2.7120000e+00,0.0000000e+00,3.1560000e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.7010000e+00,2.7170000e+00,0.0000000e+00,3.1729500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.7800000e+00,2.7440000e+00,0.0000000e+00,3.1903700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.8890000e+00,2.7510000e+00,0.0000000e+00,3.2124000e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.0330000e+00,2.7780000e+00,0.0000000e+00,3.2439700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.1460000e+00,2.8180000e+00,0.0000000e+00,3.2850400e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.2260000e+00,2.8650000e+00,0.0000000e+00,3.3328300e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.3190000e+00,2.8990000e+00,0.0000000e+00,3.3854300e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.4120000e+00,2.9360000e+00,0.0000000e+00,3.4339900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.5040000e+00,2.9660000e+00,0.0000000e+00,3.4451500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.6000000e+00,2.9810000e+00,0.0000000e+00,3.4309800e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.7010000e+00,2.8870000e+00,0.0000000e+00,3.4037900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.8020000e+00,2.8770000e+00,0.0000000e+00,3.3748300e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.9270000e+00,2.8880000e+00,0.0000000e+00,3.3515200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,4.0640000e+00,2.8990000e+00,0.0000000e+00,3.3564200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,4.2160000e+00,2.8840000e+00,0.0000000e+00,3.3879900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,4.3890000e+00,2.9290000e+00,0.0000000e+00,3.4362500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,4.6300000e+00,3.0140000e+00,0.0000000e+00,3.5134200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,4.9040000e+00,3.0750000e+00,0.0000000e+00,3.5913900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.2600000e+00,3.1280000e+00,0.0000000e+00,3.6146700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.7200000e+00,3.1030000e+00,0.0000000e+00,3.5746300e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.2420000e+00,2.9790000e+00,0.0000000e+00,3.5063200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.9270000e+00,2.9200000e+00,0.0000000e+00,3.4559600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,7.6330000e+00,2.9500000e+00,0.0000000e+00,3.4535900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.5450000e+00,3.0250000e+00,0.0000000e+00,3.5035300e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,9.5910000e+00,3.0660000e+00,0.0000000e+00,3.5642200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.0630000e+01,3.0990000e+00,0.0000000e+00,3.5988200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1840000e+01,3.0880000e+00,0.0000000e+00,3.6029500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.3160000e+01,3.0850000e+00,0.0000000e+00,3.5921900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.4340000e+01,3.0650000e+00,0.0000000e+00,3.5843200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.5780000e+01,3.0670000e+00,0.0000000e+00,3.5874900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.7140000e+01,3.0940000e+00,0.0000000e+00,3.5999500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8540000e+01,3.1360000e+00,0.0000000e+00,3.6220500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.9800000e+01,3.1580000e+00,0.0000000e+00,3.6519000e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.0890000e+01,3.2300000e+00,0.0000000e+00,3.6920500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.2020000e+01,3.2980000e+00,0.0000000e+00,3.7515600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.3130000e+01,3.4190000e+00,0.0000000e+00,3.8371800e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.4070000e+01,3.5340000e+00,0.0000000e+00,3.9537000e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.4930000e+01,3.6320000e+00,0.0000000e+00,4.0951400e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.5710000e+01,3.7750000e+00,0.0000000e+00,4.2553400e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.6410000e+01,3.9310000e+00,0.0000000e+00,4.4290100e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.7010000e+01,4.0710000e+00,0.0000000e+00,4.6075300e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.7560000e+01,4.2420000e+00,0.0000000e+00,4.7883500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.8060000e+01,4.3950000e+00,0.0000000e+00,4.9714800e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.8560000e+01,4.5610000e+00,0.0000000e+00,5.1518000e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.8920000e+01,4.7080000e+00,0.0000000e+00,5.3198300e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.9260000e+01,4.8460000e+00,0.0000000e+00,5.4749500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.9560000e+01,4.9740000e+00,0.0000000e+00,5.6125800e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.9820000e+01,5.0710000e+00,1.6873500e-04,5.7373200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.0030000e+01,5.1790000e+00,3.0352600e-04,5.8529700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.0230000e+01,5.2710000e+00,4.1390500e-04,5.9611400e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.0360000e+01,5.3580000e+00,5.4994700e-04,6.0549800e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.0460000e+01,5.4410000e+00,7.6172600e-04,6.1341700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.0520000e+01,5.4810000e+00,1.0993200e-03,6.1981500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.0550000e+01,5.5230000e+00,1.5855000e-03,6.2426900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.0560000e+01,5.5600000e+00,2.2062900e-03,6.2781900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.0540000e+01,5.5820000e+00,3.0126900e-03,6.3156900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.0490000e+01,5.6110000e+00,4.0619800e-03,6.3530400e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.0410000e+01,5.6530000e+00,5.4312000e-03,6.3815100e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.0290000e+01,5.6820000e+00,7.2676900e-03,6.4116400e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.0140000e+01,5.6800000e+00,9.7742800e-03,6.4326000e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.9980000e+01,5.7110000e+00,1.3145400e-02,6.4466500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.9710000e+01,5.7150000e+00,1.7679200e-02,6.4616900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.9360000e+01,5.7320000e+00,2.3776600e-02,6.4755300e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.8990000e+01,5.7430000e+00,3.1977100e-02,6.4854700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.8510000e+01,5.7450000e+00,4.3005900e-02,6.4949500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.8000000e+01,5.7570000e+00,5.7080900e-02,6.5115100e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.7540000e+01,5.7800000e+00,7.3929500e-02,6.5276100e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.6990000e+01,5.7980000e+00,9.3858200e-02,6.5467600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.6380000e+01,5.8080000e+00,1.3081100e-01,6.5639100e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.5810000e+01,5.8250000e+00,1.9584800e-01,6.5798500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.5220000e+01,5.8270000e+00,2.9863900e-01,6.5920500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.4680000e+01,5.8780000e+00,4.3986000e-01,6.6064200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.4130000e+01,5.9080000e+00,6.2766600e-01,6.6146100e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.3640000e+01,5.9400000e+00,8.5843700e-01,6.6122100e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.3070000e+01,5.9630000e+00,1.1202700e+00,6.5975300e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.2500000e+01,5.9820000e+00,1.4024700e+00,6.5778000e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.1930000e+01,5.9940000e+00,1.7046700e+00,6.5506000e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.1390000e+01,6.0140000e+00,2.0147000e+00,6.5189300e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.0820000e+01,6.0080000e+00,2.3136600e+00,6.4953100e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.0250000e+01,6.0210000e+00,2.5979100e+00,6.4696600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.9720000e+01,6.0270000e+00,2.8537000e+00,6.4351000e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.9240000e+01,6.0250000e+00,3.0766600e+00,6.3969200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8740000e+01,5.9890000e+00,3.2721900e+00,6.3579700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8270000e+01,5.9840000e+00,3.4495600e+00,6.3139900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.7840000e+01,5.9710000e+00,3.6163500e+00,6.2719400e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.7340000e+01,5.9510000e+00,3.7643100e+00,6.2416100e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.6880000e+01,5.9390000e+00,3.9053800e+00,6.2089000e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.6470000e+01,5.9310000e+00,4.0457800e+00,6.1738900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.6060000e+01,5.9090000e+00,4.1695100e+00,6.1375000e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.5620000e+01,5.8840000e+00,4.2755200e+00,6.1012400e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.5250000e+01,5.8760000e+00,4.3680900e+00,6.0604400e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.4890000e+01,5.8430000e+00,4.4553600e+00,6.0199500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.4470000e+01,5.8180000e+00,4.5288700e+00,5.9852500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.4130000e+01,5.7990000e+00,4.6004600e+00,5.9524200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.3810000e+01,5.7760000e+00,4.6639000e+00,5.9311200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.3480000e+01,5.7760000e+00,4.7137800e+00,5.9154100e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.3160000e+01,5.7620000e+00,4.7651400e+00,5.8975000e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2820000e+01,5.7560000e+00,4.8129600e+00,5.8819800e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2540000e+01,5.7370000e+00,4.8523400e+00,5.8693600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2230000e+01,5.7570000e+00,4.8878300e+00,5.8629900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1930000e+01,5.7510000e+00,4.9121200e+00,5.8559700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1660000e+01,5.7510000e+00,4.9200200e+00,5.8555700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1370000e+01,5.7440000e+00,4.9231500e+00,5.8541100e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1140000e+01,5.7540000e+00,4.9047800e+00,5.8509200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.0880000e+01,5.7350000e+00,4.8682400e+00,5.8509600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.0640000e+01,5.7320000e+00,4.8183300e+00,5.8560700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.0440000e+01,5.7310000e+00,4.7621300e+00,5.8509200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.0220000e+01,5.7240000e+00,4.7037300e+00,5.8450800e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.0010000e+01,5.6970000e+00,4.6515000e+00,5.8445600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,9.8390000e+00,5.7030000e+00,4.6020200e+00,5.8493900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,9.6600000e+00,5.7130000e+00,4.5769100e+00,5.8644200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,9.4770000e+00,5.7310000e+00,4.5659200e+00,5.8839500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,9.3050000e+00,5.7430000e+00,4.5754100e+00,5.8981600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,9.1330000e+00,5.7530000e+00,4.6012900e+00,5.9007700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.9770000e+00,5.7460000e+00,4.6261500e+00,5.8943700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.8200000e+00,5.7460000e+00,4.6636500e+00,5.8847000e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.6650000e+00,5.7490000e+00,4.7121600e+00,5.8742700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.5090000e+00,5.7400000e+00,4.7701100e+00,5.8712600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.3640000e+00,5.7390000e+00,4.8185800e+00,5.8684500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.2340000e+00,5.7490000e+00,4.8579600e+00,5.8630400e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.1040000e+00,5.7450000e+00,4.8899500e+00,5.8547100e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,7.9590000e+00,5.7350000e+00,4.9290200e+00,5.8371500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,7.8340000e+00,5.7250000e+00,4.9692100e+00,5.8169600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,7.7120000e+00,5.7020000e+00,5.0037800e+00,5.8015300e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,7.6090000e+00,5.6990000e+00,5.0432300e+00,5.7853200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,7.4950000e+00,5.6850000e+00,5.0800000e+00,5.7567600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,7.3740000e+00,5.6590000e+00,5.1108200e+00,5.7212200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,7.2520000e+00,5.6130000e+00,5.1372500e+00,5.6778200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,7.1640000e+00,5.5910000e+00,5.1564000e+00,5.6341100e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,7.0840000e+00,5.5520000e+00,5.1668300e+00,5.5948700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.9870000e+00,5.5170000e+00,5.1773200e+00,5.5615700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.9100000e+00,5.4940000e+00,5.1855500e+00,5.5259700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.8280000e+00,5.4500000e+00,5.1857000e+00,5.4888900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.7420000e+00,5.4280000e+00,5.1773700e+00,5.4568700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.6950000e+00,5.3910000e+00,5.1673100e+00,5.4252900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.6300000e+00,5.3760000e+00,5.1629200e+00,5.3964100e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.5670000e+00,5.3470000e+00,5.1721700e+00,5.3667500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.5010000e+00,5.3190000e+00,5.1891900e+00,5.3367300e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.4490000e+00,5.2920000e+00,5.2143200e+00,5.3080600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.4010000e+00,5.2740000e+00,5.2436000e+00,5.2809700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.3450000e+00,5.2530000e+00,5.2702600e+00,5.2524600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.2820000e+00,5.2330000e+00,5.3010300e+00,5.2252200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.2140000e+00,5.2110000e+00,5.3256900e+00,5.1999500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.1630000e+00,5.2100000e+00,5.3414800e+00,5.1797800e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.1210000e+00,5.1930000e+00,5.3381300e+00,5.1691600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.0760000e+00,5.1770000e+00,5.3212100e+00,5.1611500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.0480000e+00,5.1780000e+00,5.2871000e+00,5.1518400e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.0050000e+00,5.1520000e+00,5.2403300e+00,5.1402900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.9730000e+00,5.1390000e+00,5.1865100e+00,5.1326100e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.9400000e+00,5.1210000e+00,5.1315600e+00,5.1236100e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.9110000e+00,5.1260000e+00,5.0830200e+00,5.1196700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.8750000e+00,5.1040000e+00,5.0417700e+00,5.1219300e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.8260000e+00,5.1090000e+00,5.0110100e+00,5.1228700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.7870000e+00,5.1120000e+00,4.9923900e+00,5.1301300e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.7460000e+00,5.1360000e+00,4.9884600e+00,5.1457100e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.7050000e+00,5.1520000e+00,5.0023300e+00,5.1759700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.6840000e+00,5.1930000e+00,5.0295900e+00,5.2089500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.6580000e+00,5.2440000e+00,5.0707800e+00,5.2511700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.6480000e+00,5.2730000e+00,5.1254100e+00,5.2995900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.6260000e+00,5.3350000e+00,5.1981000e+00,5.3483500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.6180000e+00,5.3890000e+00,5.2925700e+00,5.4025100e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.6140000e+00,5.4530000e+00,5.4113500e+00,5.4610100e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.6030000e+00,5.5220000e+00,5.5542600e+00,5.5433600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.5840000e+00,5.6050000e+00,5.7222200e+00,5.6007100e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.5630000e+00,5.6980000e+00,5.9126900e+00,5.6313900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.5450000e+00,5.7430000e+00,6.1223000e+00,5.6412200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.5420000e+00,5.6890000e+00,6.3491200e+00,5.6441500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.5320000e+00,5.7550000e+00,6.6088800e+00,5.6534200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.5330000e+00,5.8470000e+00,6.8895000e+00,5.6882900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.5290000e+00,5.9180000e+00,7.1929300e+00,5.7646800e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.5240000e+00,6.0220000e+00,7.5084100e+00,5.8277600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.5260000e+00,6.1160000e+00,7.8486500e+00,5.8936800e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.5200000e+00,6.2140000e+00,8.1996400e+00,5.9647900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.5220000e+00,6.3190000e+00,8.5474500e+00,6.0381000e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.5270000e+00,6.4260000e+00,8.8845800e+00,6.1087200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.5190000e+00,6.5410000e+00,9.1982800e+00,6.1772900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.5200000e+00,6.6160000e+00,9.4843200e+00,6.2481300e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.5180000e+00,6.7280000e+00,9.7294400e+00,6.3216700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.5380000e+00,6.8070000e+00,9.9421500e+00,6.4031500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.5440000e+00,6.9070000e+00,1.0092900e+01,6.4965700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.5710000e+00,7.0060000e+00,1.0196200e+01,6.5895800e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.6060000e+00,7.0930000e+00,1.0245200e+01,6.6777400e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.6290000e+00,7.1640000e+00,1.0264600e+01,6.7592400e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.6640000e+00,7.2320000e+00,1.0252100e+01,6.8414600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.6930000e+00,7.3150000e+00,1.0219400e+01,6.9235900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.7330000e+00,7.3610000e+00,1.0178800e+01,7.0108800e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.7660000e+00,7.4400000e+00,1.0131700e+01,7.0929000e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.7970000e+00,7.5050000e+00,1.0071300e+01,7.1678200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.8290000e+00,7.5570000e+00,1.0031400e+01,7.2364000e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.8770000e+00,7.6040000e+00,1.0009400e+01,7.2937400e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.9300000e+00,7.6530000e+00,1.0015000e+01,7.3477400e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.9720000e+00,7.6910000e+00,1.0041100e+01,7.3901000e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.0250000e+00,7.7080000e+00,1.0088200e+01,7.4142300e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.0870000e+00,7.7300000e+00,1.0152900e+01,7.4130300e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.1360000e+00,7.7120000e+00,1.0200900e+01,7.3908200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.1980000e+00,7.6950000e+00,1.0270100e+01,7.3499700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.2560000e+00,7.6440000e+00,1.0318900e+01,7.2879600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.3350000e+00,7.5850000e+00,1.0355100e+01,7.2200100e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.3920000e+00,7.5170000e+00,1.0370200e+01,7.1473500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.4930000e+00,7.4510000e+00,1.0372600e+01,7.0766100e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.5710000e+00,7.4080000e+00,1.0336700e+01,7.0231600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.6580000e+00,7.3630000e+00,1.0272400e+01,6.9905100e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.7480000e+00,7.3320000e+00,1.0197500e+01,6.9691700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.8420000e+00,7.3320000e+00,1.0103400e+01,6.9559200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.9550000e+00,7.2950000e+00,9.9798400e+00,6.9572400e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,7.0540000e+00,7.2910000e+00,9.8373200e+00,6.9627900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,7.1790000e+00,7.2920000e+00,9.6744600e+00,6.9681700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,7.2740000e+00,7.2830000e+00,9.4971700e+00,6.9820600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,7.4140000e+00,7.2500000e+00,9.3079600e+00,6.9902200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,7.5360000e+00,7.2280000e+00,9.0969600e+00,6.9826200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,7.6690000e+00,7.1860000e+00,8.8756300e+00,6.9720100e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,7.7760000e+00,7.1430000e+00,8.6198300e+00,6.9535700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,7.8930000e+00,7.1010000e+00,8.3591600e+00,6.9265300e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.0000000e+00,7.0380000e+00,8.1113400e+00,6.8912500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.1030000e+00,6.9740000e+00,7.8585700e+00,6.8444600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.2050000e+00,6.8890000e+00,7.6185400e+00,6.7927800e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.2640000e+00,6.7930000e+00,7.3995500e+00,6.7344300e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.3520000e+00,6.7290000e+00,7.1959000e+00,6.6706300e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.4300000e+00,6.6610000e+00,7.0150600e+00,6.6060500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.4770000e+00,6.5720000e+00,6.8537800e+00,6.5469800e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.5350000e+00,6.5220000e+00,6.7311600e+00,6.4994700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.5930000e+00,6.4820000e+00,6.6358000e+00,6.4795800e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.6400000e+00,6.4790000e+00,6.5643300e+00,6.4763700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.6890000e+00,6.4960000e+00,6.5046900e+00,6.4720000e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.7380000e+00,6.4610000e+00,6.4547800e+00,6.4626300e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.7770000e+00,6.4300000e+00,6.4096300e+00,6.4403700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.7780000e+00,6.4210000e+00,6.3699600e+00,6.4127100e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.8050000e+00,6.3790000e+00,6.3460500e+00,6.3860500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.8090000e+00,6.3720000e+00,6.3275500e+00,6.3640500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.7990000e+00,6.3210000e+00,6.3091700e+00,6.3385100e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.7890000e+00,6.3140000e+00,6.2857700e+00,6.3103900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.7670000e+00,6.2770000e+00,6.2551500e+00,6.2795500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.7500000e+00,6.2580000e+00,6.2124600e+00,6.2573800e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.7390000e+00,6.2340000e+00,6.1518700e+00,6.2412900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.7440000e+00,6.2310000e+00,6.0807100e+00,6.2348700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.7550000e+00,6.2050000e+00,5.9942400e+00,6.2282400e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.7870000e+00,6.1920000e+00,5.8970200e+00,6.2236200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.7980000e+00,6.1590000e+00,5.7986800e+00,6.2126000e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.8110000e+00,6.1430000e+00,5.7008700e+00,6.1928300e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.8360000e+00,6.0980000e+00,5.6161000e+00,6.1732900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.8540000e+00,6.0630000e+00,5.5438500e+00,6.1520900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.8680000e+00,6.0530000e+00,5.4828900e+00,6.1278400e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.8840000e+00,6.0440000e+00,5.4308200e+00,6.1058200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.9000000e+00,5.9960000e+00,5.3831000e+00,6.0838300e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.9510000e+00,5.9970000e+00,5.3286400e+00,6.0628600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,9.0100000e+00,5.9540000e+00,5.2650100e+00,6.0450500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,9.1100000e+00,5.9270000e+00,5.1829000e+00,6.0246200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,9.1950000e+00,5.8900000e+00,5.0805600e+00,5.9869600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,9.3150000e+00,5.8320000e+00,4.9598000e+00,5.9371300e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,9.4570000e+00,5.7680000e+00,4.8410300e+00,5.8857000e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,9.6330000e+00,5.6810000e+00,4.7094800e+00,5.8377000e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,9.8240000e+00,5.6550000e+00,4.5884500e+00,5.7933600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.0050000e+01,5.6200000e+00,4.4895400e+00,5.7638700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.0310000e+01,5.5670000e+00,4.4127500e+00,5.7376200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.0590000e+01,5.5520000e+00,4.3846900e+00,5.7140700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.0940000e+01,5.5390000e+00,4.3581200e+00,5.6912800e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1390000e+01,5.5070000e+00,4.3243600e+00,5.6684300e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1840000e+01,5.4870000e+00,4.2810200e+00,5.6417500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2350000e+01,5.4620000e+00,4.2574100e+00,5.6068000e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.3010000e+01,5.4480000e+00,4.2062300e+00,5.5969700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.3750000e+01,5.4400000e+00,4.1286900e+00,5.6089100e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.4780000e+01,5.4490000e+00,4.0471100e+00,5.6449200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.5860000e+01,5.4610000e+00,3.9605000e+00,5.6570500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.7090000e+01,5.4450000e+00,3.8338900e+00,5.6329800e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8530000e+01,5.3790000e+00,3.6927100e+00,5.5674600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.0510000e+01,5.2280000e+00,3.5385700e+00,5.4672300e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.2500000e+01,5.1710000e+00,3.4007000e+00,5.3889700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.4830000e+01,5.1240000e+00,3.3201400e+00,5.3621400e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.7510000e+01,5.1290000e+00,3.2797700e+00,5.3790700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.1440000e+01,5.1730000e+00,3.2935300e+00,5.4055400e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.5040000e+01,5.1850000e+00,3.3293000e+00,5.4313500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.9270000e+01,5.2000000e+00,3.4015300e+00,5.4347600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,4.5300000e+01,5.2040000e+00,3.5500200e+00,5.4250700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.0600000e+01,5.2440000e+00,3.7668600e+00,5.4383900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.7610000e+01,5.3180000e+00,4.0563800e+00,5.4867900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.4010000e+01,5.4410000e+00,4.4267000e+00,5.5608000e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,7.0590000e+01,5.5610000e+00,4.8856200e+00,5.6515200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,7.8410000e+01,5.6870000e+00,5.3723800e+00,5.7393400e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.4690000e+01,5.8300000e+00,5.9127700e+00,5.8295000e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,9.1890000e+01,5.9980000e+00,6.5176700e+00,5.9437800e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,9.7220000e+01,6.1930000e+00,7.1474300e+00,6.0818000e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.0330000e+02,6.4490000e+00,7.8356100e+00,6.2573900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.0780000e+02,6.7100000e+00,8.5562300e+00,6.4597700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1230000e+02,6.9700000e+00,9.2849600e+00,6.6730100e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1570000e+02,7.2380000e+00,1.0027200e+01,6.8866300e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1900000e+02,7.5060000e+00,1.0734400e+01,7.0943300e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2150000e+02,7.7670000e+00,1.1429900e+01,7.2812100e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2400000e+02,7.9770000e+00,1.2105900e+01,7.4267000e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2570000e+02,8.1500000e+00,1.2752200e+01,7.5415000e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2750000e+02,8.2920000e+00,1.3370100e+01,7.6411500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2860000e+02,8.4490000e+00,1.3949100e+01,7.7295500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2960000e+02,8.5830000e+00,1.4486100e+01,7.8163400e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.3030000e+02,8.7230000e+00,1.4981200e+01,7.9031800e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.3060000e+02,8.8210000e+00,1.5427900e+01,7.9622200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.3060000e+02,8.9090000e+00,1.5829100e+01,7.9954600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.3040000e+02,8.9490000e+00,1.6181300e+01,8.0172500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2990000e+02,8.9830000e+00,1.6491200e+01,8.0230500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2910000e+02,9.0430000e+00,1.6757900e+01,8.0171000e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2820000e+02,9.0280000e+00,1.6976500e+01,8.0062400e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2700000e+02,9.0520000e+00,1.7148100e+01,7.9939500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2560000e+02,9.0410000e+00,1.7275500e+01,7.9710200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2420000e+02,9.0360000e+00,1.7361900e+01,7.9513600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2240000e+02,9.0210000e+00,1.7409700e+01,7.9282900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.2060000e+02,9.0080000e+00,1.7431000e+01,7.9028300e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1900000e+02,8.9740000e+00,1.7433900e+01,7.8761200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1700000e+02,8.9570000e+00,1.7413200e+01,7.8485000e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1490000e+02,8.9230000e+00,1.7386800e+01,7.8231800e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1280000e+02,8.8950000e+00,1.7373500e+01,7.7980900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.1100000e+02,8.8800000e+00,1.7374400e+01,7.7741900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.0880000e+02,8.8550000e+00,1.7401900e+01,7.7623900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.0660000e+02,8.8610000e+00,1.7454600e+01,7.7565500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.0440000e+02,8.8760000e+00,1.7530300e+01,7.7407600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.0230000e+02,8.8720000e+00,1.7623400e+01,7.7268400e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.0050000e+02,8.8280000e+00,1.7720400e+01,7.7039800e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,9.8310000e+01,8.8550000e+00,1.7815700e+01,7.6775200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,9.6310000e+01,8.8330000e+00,1.7898500e+01,7.6552500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,9.4120000e+01,8.8430000e+00,1.7960900e+01,7.6395800e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,9.2060000e+01,8.8080000e+00,1.7996000e+01,7.6243300e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,9.0090000e+01,8.8070000e+00,1.7992400e+01,7.6133800e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.8040000e+01,8.7940000e+00,1.7940200e+01,7.6062600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.6110000e+01,8.7950000e+00,1.7840800e+01,7.6004600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.4600000e+01,8.7540000e+00,1.7705200e+01,7.6013400e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.2620000e+01,8.7600000e+00,1.7533400e+01,7.6144700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.0760000e+01,8.7620000e+00,1.7339500e+01,7.6450900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,7.9040000e+01,8.7900000e+00,1.7124900e+01,7.7010200e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,7.7230000e+01,8.8110000e+00,1.6896700e+01,7.7736500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,7.5570000e+01,8.8740000e+00,1.6659300e+01,7.8610600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,7.3920000e+01,8.9130000e+00,1.6408600e+01,7.9429800e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,7.2340000e+01,8.9800000e+00,1.6143000e+01,8.0334600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,7.0820000e+01,9.0010000e+00,1.5870000e+01,8.1221800e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.9240000e+01,9.0760000e+00,1.5593500e+01,8.2195900e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.7790000e+01,9.1410000e+00,1.5327000e+01,8.3285500e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.6340000e+01,9.2140000e+00,1.5062300e+01,8.4545600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.4900000e+01,9.3030000e+00,1.4803100e+01,8.5935600e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.3580000e+01,9.4060000e+00,1.4534900e+01,8.7399100e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.2320000e+01,9.5160000e+00,1.4272400e+01,8.8944800e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.1050000e+01,9.6380000e+00,1.4016100e+01,9.0601800e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.9800000e+01,9.7630000e+00,1.3772800e+01,9.2473400e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.8570000e+01,9.9530000e+00,1.3552000e+01,9.4740800e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.7460000e+01,1.0140000e+01,1.3351800e+01,9.7330700e+00),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.6320000e+01,1.0390000e+01,1.3179600e+01,1.0020800e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.5030000e+01,1.0630000e+01,1.3035900e+01,1.0328200e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.3980000e+01,1.0910000e+01,1.2921500e+01,1.0645200e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.2940000e+01,1.1190000e+01,1.2846500e+01,1.0975800e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.1970000e+01,1.1500000e+01,1.2821600e+01,1.1326300e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.0980000e+01,1.1810000e+01,1.2856200e+01,1.1689500e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.0060000e+01,1.2170000e+01,1.2953500e+01,1.2062300e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,4.9150000e+01,1.2510000e+01,1.3114400e+01,1.2440800e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,4.8060000e+01,1.2870000e+01,1.3335300e+01,1.2829500e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,4.7180000e+01,1.3250000e+01,1.3619400e+01,1.3216800e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,4.6350000e+01,1.3640000e+01,1.3947800e+01,1.3611900e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,4.5530000e+01,1.4030000e+01,1.4315200e+01,1.4007000e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,4.4730000e+01,1.4410000e+01,1.4684300e+01,1.4389400e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,4.3790000e+01,1.4810000e+01,1.5049300e+01,1.4764000e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,4.3020000e+01,1.5170000e+01,1.5362900e+01,1.5145700e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,4.2280000e+01,1.5540000e+01,1.5600300e+01,1.5521900e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,4.1550000e+01,1.5910000e+01,1.5751200e+01,1.5901500e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,4.0720000e+01,1.6220000e+01,1.5771300e+01,1.6275000e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,4.0020000e+01,1.6530000e+01,1.5638900e+01,1.6627200e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.9340000e+01,1.6780000e+01,1.5352800e+01,1.6952300e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.8680000e+01,1.6980000e+01,1.4907700e+01,1.7248900e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.7900000e+01,1.7180000e+01,1.4310800e+01,1.7532700e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.7260000e+01,1.7340000e+01,1.3593000e+01,1.7808600e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.6640000e+01,1.7480000e+01,1.2780100e+01,1.8081500e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.5920000e+01,1.7610000e+01,1.1886900e+01,1.8349200e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.5360000e+01,1.7710000e+01,1.0962800e+01,1.8607400e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.4790000e+01,1.7830000e+01,1.0019700e+01,1.8849200e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.4130000e+01,1.7940000e+01,9.1041800e+00,1.9063700e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.3630000e+01,1.7990000e+01,8.2462800e+00,1.9280100e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.3090000e+01,1.8090000e+01,7.4680800e+00,1.9486000e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.2510000e+01,1.8200000e+01,6.7859300e+00,1.9687300e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.2000000e+01,1.8300000e+01,6.2165100e+00,1.9885100e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.1540000e+01,1.8410000e+01,5.7404300e+00,2.0065500e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.0970000e+01,1.8520000e+01,5.3490100e+00,2.0229900e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.0520000e+01,1.8610000e+01,5.0492900e+00,2.0371700e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.9990000e+01,1.8710000e+01,4.8093900e+00,2.0507800e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.9580000e+01,1.8810000e+01,4.6525900e+00,2.0639300e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.9060000e+01,1.8900000e+01,4.5635700e+00,2.0769700e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.8650000e+01,1.9030000e+01,4.5379300e+00,2.0898700e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.8220000e+01,1.9110000e+01,4.5396000e+00,2.1020500e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.7760000e+01,1.9210000e+01,4.5858700e+00,2.1119300e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.7370000e+01,1.9290000e+01,4.6507100e+00,2.1185700e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.6920000e+01,1.9350000e+01,4.7485200e+00,2.1240800e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.6570000e+01,1.9400000e+01,4.8624900e+00,2.1289800e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.6170000e+01,1.9470000e+01,4.9975800e+00,2.1336400e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.5840000e+01,1.9530000e+01,5.1660500e+00,2.1390700e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.5470000e+01,1.9600000e+01,5.3431300e+00,2.1437300e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.5180000e+01,1.9620000e+01,5.5395500e+00,2.1467500e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.4850000e+01,1.9660000e+01,5.7465600e+00,2.1480200e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.4550000e+01,1.9700000e+01,5.9933600e+00,2.1481800e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.4200000e+01,1.9740000e+01,6.2901800e+00,2.1479400e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.3860000e+01,1.9760000e+01,6.6582300e+00,2.1462900e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.3580000e+01,1.9790000e+01,7.0995600e+00,2.1447600e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.3280000e+01,1.9820000e+01,7.6127500e+00,2.1417900e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.3040000e+01,1.9860000e+01,8.1987700e+00,2.1377200e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.2750000e+01,1.9890000e+01,8.8693900e+00,2.1326000e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.2520000e+01,1.9930000e+01,9.5999100e+00,2.1266500e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.2270000e+01,1.9970000e+01,1.0415100e+01,2.1208900e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.2020000e+01,2.0010000e+01,1.1288300e+01,2.1150900e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.1830000e+01,2.0070000e+01,1.2227400e+01,2.1093000e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.1620000e+01,2.0130000e+01,1.3212800e+01,2.1026900e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.1400000e+01,2.0170000e+01,1.4234000e+01,2.0942900e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.1230000e+01,2.0220000e+01,1.5276600e+01,2.0856000e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.1020000e+01,2.0230000e+01,1.6344300e+01,2.0750200e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.0850000e+01,2.0270000e+01,1.7431500e+01,2.0635500e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.0670000e+01,2.0280000e+01,1.8521600e+01,2.0510800e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.0490000e+01,2.0290000e+01,1.9611700e+01,2.0377700e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.0330000e+01,2.0310000e+01,2.0687700e+01,2.0238600e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.0180000e+01,2.0260000e+01,2.1735100e+01,2.0103600e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.0020000e+01,2.0300000e+01,2.2752600e+01,1.9980800e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.9870000e+01,2.0300000e+01,2.3712700e+01,1.9853800e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.9760000e+01,2.0290000e+01,2.4622200e+01,1.9726100e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.9630000e+01,2.0260000e+01,2.5462200e+01,1.9586100e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.9520000e+01,2.0230000e+01,2.6214000e+01,1.9441600e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.9420000e+01,2.0160000e+01,2.6891100e+01,1.9296700e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.9330000e+01,2.0120000e+01,2.7490300e+01,1.9157200e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.9230000e+01,2.0070000e+01,2.8013000e+01,1.9023600e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.9130000e+01,1.9980000e+01,2.8461300e+01,1.8888900e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.9040000e+01,1.9910000e+01,2.8849700e+01,1.8742500e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8960000e+01,1.9800000e+01,2.9168700e+01,1.8591900e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8840000e+01,1.9690000e+01,2.9439600e+01,1.8434900e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8740000e+01,1.9590000e+01,2.9647600e+01,1.8280100e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8660000e+01,1.9470000e+01,2.9807100e+01,1.8127500e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8560000e+01,1.9360000e+01,2.9910300e+01,1.7984000e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8490000e+01,1.9220000e+01,2.9968000e+01,1.7834900e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8430000e+01,1.9120000e+01,2.9983800e+01,1.7695200e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8370000e+01,1.8990000e+01,2.9952300e+01,1.7558100e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8380000e+01,1.8850000e+01,2.9868600e+01,1.7428300e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8360000e+01,1.8740000e+01,2.9749500e+01,1.7309900e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8350000e+01,1.8610000e+01,2.9602500e+01,1.7198600e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8340000e+01,1.8520000e+01,2.9433100e+01,1.7095400e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8330000e+01,1.8400000e+01,2.9246600e+01,1.6992200e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8340000e+01,1.8290000e+01,2.9037400e+01,1.6901500e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8340000e+01,1.8180000e+01,2.8790500e+01,1.6806700e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8320000e+01,1.8070000e+01,2.8506700e+01,1.6718000e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8290000e+01,1.7980000e+01,2.8184100e+01,1.6638100e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8280000e+01,1.7870000e+01,2.7816400e+01,1.6569200e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8290000e+01,1.7800000e+01,2.7423000e+01,1.6529500e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8320000e+01,1.7730000e+01,2.6998500e+01,1.6510300e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8360000e+01,1.7680000e+01,2.6551900e+01,1.6519000e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8370000e+01,1.7630000e+01,2.6084400e+01,1.6543600e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8370000e+01,1.7630000e+01,2.5619300e+01,1.6590000e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8420000e+01,1.7640000e+01,2.5168200e+01,1.6665000e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8460000e+01,1.7700000e+01,2.4757300e+01,1.6778400e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8510000e+01,1.7790000e+01,2.4403700e+01,1.6941600e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8570000e+01,1.7950000e+01,2.4112000e+01,1.7143800e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8600000e+01,1.8140000e+01,2.3892400e+01,1.7395400e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8650000e+01,1.8360000e+01,2.3741500e+01,1.7679200e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8700000e+01,1.8660000e+01,2.3664300e+01,1.7994600e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8790000e+01,1.8950000e+01,2.3687400e+01,1.8347800e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8880000e+01,1.9310000e+01,2.3810200e+01,1.8738700e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.8970000e+01,1.9720000e+01,2.4028600e+01,1.9168000e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.9080000e+01,2.0170000e+01,2.4331300e+01,1.9637600e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.9190000e+01,2.0650000e+01,2.4714000e+01,2.0124200e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.9290000e+01,2.1140000e+01,2.5166400e+01,2.0606500e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.9400000e+01,2.1630000e+01,2.5683800e+01,2.1093900e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.9510000e+01,2.2100000e+01,2.6257000e+01,2.1585200e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.9660000e+01,2.2620000e+01,2.6867900e+01,2.2070300e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.9800000e+01,2.3130000e+01,2.7499400e+01,2.2556100e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,1.9910000e+01,2.3610000e+01,2.8124900e+01,2.3037400e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.0040000e+01,2.4100000e+01,2.8752500e+01,2.3491400e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.0200000e+01,2.4550000e+01,2.9362900e+01,2.3919300e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.0380000e+01,2.4990000e+01,2.9947700e+01,2.4342500e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.0540000e+01,2.5410000e+01,3.0509400e+01,2.4762800e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.0710000e+01,2.5830000e+01,3.1075300e+01,2.5160000e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.0870000e+01,2.6260000e+01,3.1639600e+01,2.5524000e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.1030000e+01,2.6570000e+01,3.2200700e+01,2.5858100e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.1260000e+01,2.6880000e+01,3.2763500e+01,2.6145600e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.1450000e+01,2.7180000e+01,3.3314300e+01,2.6375400e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.1640000e+01,2.7420000e+01,3.3835400e+01,2.6584800e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.1830000e+01,2.7630000e+01,3.4325900e+01,2.6797200e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.2020000e+01,2.7940000e+01,3.4780700e+01,2.7020100e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.2230000e+01,2.8210000e+01,3.5197400e+01,2.7278000e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.2450000e+01,2.8500000e+01,3.5527800e+01,2.7584500e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.2760000e+01,2.8780000e+01,3.5770400e+01,2.7902900e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.3010000e+01,2.9130000e+01,3.5898200e+01,2.8209200e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.3270000e+01,2.9370000e+01,3.5914000e+01,2.8500300e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.3510000e+01,2.9600000e+01,3.5780300e+01,2.8789300e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.3770000e+01,2.9820000e+01,3.5517300e+01,2.9073700e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.4090000e+01,3.0060000e+01,3.5151100e+01,2.9393600e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.4330000e+01,3.0280000e+01,3.4712100e+01,2.9712300e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.4590000e+01,3.0530000e+01,3.4188400e+01,3.0024800e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.4850000e+01,3.0660000e+01,3.3586500e+01,3.0283700e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.5230000e+01,3.0760000e+01,3.2943600e+01,3.0473600e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.5490000e+01,3.0750000e+01,3.2317200e+01,3.0566600e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.5790000e+01,3.0720000e+01,3.1712700e+01,3.0574900e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.6110000e+01,3.0580000e+01,3.1148500e+01,3.0524900e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.6530000e+01,3.0450000e+01,3.0669500e+01,3.0417700e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.6810000e+01,3.0290000e+01,3.0235100e+01,3.0297600e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.7120000e+01,3.0150000e+01,2.9859900e+01,3.0181400e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.7440000e+01,3.0000000e+01,2.9561500e+01,3.0082700e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.7890000e+01,2.9910000e+01,2.9297500e+01,2.9987600e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.8190000e+01,2.9840000e+01,2.9051400e+01,2.9922300e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.8550000e+01,2.9760000e+01,2.8856500e+01,2.9897900e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.9030000e+01,2.9760000e+01,2.8682500e+01,2.9892200e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.9340000e+01,2.9790000e+01,2.8501700e+01,2.9951500e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,2.9690000e+01,2.9850000e+01,2.8352500e+01,3.0050700e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.0020000e+01,2.9990000e+01,2.8204200e+01,3.0169300e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.0540000e+01,3.0050000e+01,2.8099300e+01,3.0291200e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.0920000e+01,3.0110000e+01,2.8008100e+01,3.0419400e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.1450000e+01,3.0220000e+01,2.7958300e+01,3.0511000e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.1800000e+01,3.0290000e+01,2.7954300e+01,3.0580200e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.2190000e+01,3.0290000e+01,2.7965600e+01,3.0597900e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.2720000e+01,3.0270000e+01,2.7976200e+01,3.0563100e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.3110000e+01,3.0190000e+01,2.8017500e+01,3.0460300e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.3480000e+01,3.0100000e+01,2.8029800e+01,3.0332700e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.4040000e+01,2.9930000e+01,2.8012400e+01,3.0200500e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.4470000e+01,2.9840000e+01,2.7961500e+01,3.0038600e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.4860000e+01,2.9660000e+01,2.7838600e+01,2.9872400e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.5470000e+01,2.9480000e+01,2.7630700e+01,2.9705800e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.5920000e+01,2.9260000e+01,2.7355200e+01,2.9528200e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.6560000e+01,2.9130000e+01,2.7007900e+01,2.9344400e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.6970000e+01,2.8900000e+01,2.6584200e+01,2.9209800e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.7610000e+01,2.8720000e+01,2.6147500e+01,2.9127600e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.8010000e+01,2.8700000e+01,2.5675000e+01,2.9059200e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.8480000e+01,2.8580000e+01,2.5202400e+01,2.9030300e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.9130000e+01,2.8500000e+01,2.4712500e+01,2.8997000e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,3.9570000e+01,2.8380000e+01,2.4222800e+01,2.8923600e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,4.0240000e+01,2.8230000e+01,2.3776500e+01,2.8848500e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,4.0780000e+01,2.8170000e+01,2.3375400e+01,2.8775400e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,4.1470000e+01,2.8030000e+01,2.3002400e+01,2.8723600e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,4.1960000e+01,2.8010000e+01,2.2674100e+01,2.8668700e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,4.2680000e+01,2.7910000e+01,2.2354200e+01,2.8630000e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,4.3190000e+01,2.7840000e+01,2.2086100e+01,2.8600200e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,4.3880000e+01,2.7810000e+01,2.1836600e+01,2.8592000e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,4.4390000e+01,2.7810000e+01,2.1629900e+01,2.8621200e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,4.5130000e+01,2.7830000e+01,2.1417200e+01,2.8660300e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,4.5650000e+01,2.7850000e+01,2.1199300e+01,2.8688800e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,4.6380000e+01,2.7820000e+01,2.0978000e+01,2.8696200e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,4.6900000e+01,2.7790000e+01,2.0756200e+01,2.8707600e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,4.7640000e+01,2.7830000e+01,2.0549200e+01,2.8754600e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,4.8170000e+01,2.7880000e+01,2.0340100e+01,2.8864600e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,4.8940000e+01,2.7990000e+01,2.0154000e+01,2.9018100e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,4.9400000e+01,2.8120000e+01,1.9975100e+01,2.9137800e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.0210000e+01,2.8190000e+01,1.9790100e+01,2.9237200e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.0780000e+01,2.8170000e+01,1.9578400e+01,2.9341900e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.1590000e+01,2.8310000e+01,1.9363500e+01,2.9461000e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.2460000e+01,2.8480000e+01,1.9154600e+01,2.9621000e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.3100000e+01,2.8590000e+01,1.8956000e+01,2.9835500e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.4010000e+01,2.8740000e+01,1.8754700e+01,3.0088300e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.4620000e+01,2.8980000e+01,1.8534600e+01,3.0342600e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.5550000e+01,2.9230000e+01,1.8270200e+01,3.0633700e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.6180000e+01,2.9440000e+01,1.8003700e+01,3.0951900e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.7130000e+01,2.9730000e+01,1.7773400e+01,3.1276900e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.8080000e+01,3.0020000e+01,1.7576200e+01,3.1619300e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.8780000e+01,3.0350000e+01,1.7451000e+01,3.2006000e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,5.9760000e+01,3.0640000e+01,1.7358800e+01,3.2397200e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.0390000e+01,3.0980000e+01,1.7285800e+01,3.2783700e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.1460000e+01,3.1320000e+01,1.7222600e+01,3.3176600e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.2500000e+01,3.1640000e+01,1.7157300e+01,3.3533800e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.3160000e+01,3.2040000e+01,1.7123800e+01,3.3916200e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.4270000e+01,3.2330000e+01,1.7085600e+01,3.4369900e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.5390000e+01,3.2820000e+01,1.7060400e+01,3.4905900e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.6090000e+01,3.3390000e+01,1.7038900e+01,3.5463200e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.7250000e+01,3.3840000e+01,1.6994200e+01,3.6032100e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.8420000e+01,3.4260000e+01,1.6916800e+01,3.6577900e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,6.9180000e+01,3.4720000e+01,1.6885100e+01,3.7063400e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,7.0460000e+01,3.5190000e+01,1.6901400e+01,3.7555500e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,7.1740000e+01,3.5610000e+01,1.6869400e+01,3.8049000e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,7.2490000e+01,3.6040000e+01,1.6820200e+01,3.8520200e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,7.3810000e+01,3.6360000e+01,1.6707900e+01,3.8910600e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,7.5140000e+01,3.6740000e+01,1.6448000e+01,3.9288600e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,7.5910000e+01,3.6990000e+01,1.6087700e+01,3.9725800e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,7.7180000e+01,3.7350000e+01,1.5702400e+01,4.0176100e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,7.8390000e+01,3.7920000e+01,1.5297800e+01,4.0647900e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,7.9700000e+01,3.8120000e+01,1.4889700e+01,4.1173300e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.0460000e+01,3.8330000e+01,1.4514900e+01,4.1612400e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.1730000e+01,3.8740000e+01,1.4086100e+01,4.1874300e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.2950000e+01,3.8840000e+01,1.3578400e+01,4.2086700e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.4230000e+01,3.8790000e+01,1.3081900e+01,4.2218600e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.5100000e+01,3.9000000e+01,1.2592400e+01,4.2302300e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.6170000e+01,3.9060000e+01,1.2115000e+01,4.2488800e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.7080000e+01,3.9160000e+01,1.1626900e+01,4.2782100e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.8840000e+01,3.9480000e+01,1.1034500e+01,4.3057200e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,8.9450000e+01,3.9510000e+01,1.0463700e+01,4.3311200e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,9.0890000e+01,3.9530000e+01,9.9541700e+00,4.3462900e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,9.2040000e+01,3.9580000e+01,9.5368600e+00,4.3428200e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,9.3080000e+01,3.9400000e+01,9.2426600e+00,4.3359600e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,9.3580000e+01,3.9170000e+01,9.1024700e+00,4.3201200e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,9.4480000e+01,3.8930000e+01,9.1472100e+00,4.2960900e+01),
  auto(0.0000000e+00,0.0000000e+00,0.0000000e+00,0.0000000e+00,9.5300000e+01,3.8710000e+01,9.4077800e+00,4.2636600e+01),
);
export @(noinline)PROSPECT_result PROSPECT(
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
    float w=PROSPECT_TABLE_SIZE*saturate(($state.wavelength_base[i]-PROSPECT_MIN_WAVELENGTH)/(PROSPECT_MAX_WAVELENGTH-PROSPECT_MIN_WAVELENGTH));
    const int w0=#min(int(#floor(w)),PROSPECT_TABLE_SIZE-2);
    const int w1=w0+1;
    w-=w0;
    ior[i]=lerp(PROSPECT_TABLE_IORS[w0],PROSPECT_TABLE_IORS[w1],w);
    k[i]=dot(lerp(PROSPECT_TABLE_ABSORPTIONS[w0],PROSPECT_TABLE_ABSORPTIONS[w1],w),contents);
  }
  k=#min(k,50.0);
  const auto tau=clamp(#exp(1.770761*#pow(k,1.089248)-3.28915*k),0.0,0.999);
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
  const auto tmp0=tau*t21/(1-#pow(r21*tau,2));
  const auto tA=tAlpha*tmp0;
  const auto rA=rAlpha+r21*tau*tA;
  const auto t=t12*tmp0;
  const auto r=r12+r21*tau*t;
  const auto d=#sqrt((1+(r+t))*(1+(r-t))*(1-(r+t))*(1-(r-t)));
  const auto a=(1+d+(r*r-t*t))/(2*r);
  const auto b=(1+d-(r*r-t*t))/(2*t);
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
  return PROSPECT_result(reflectance: rA+tA*rSub*t/(1-rSub*r),transmittance: tA*tSub/(1-rSub*r));
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
  if (name == "PCG32")
    return PCG32;
  if (name == "PROSPECT")
    return PROSPECT;
  return nullptr;
}
#include "builtin/albedo/diffuse_reflection_bsdf.inl"
#include "builtin/albedo/microfacet_ggx_smith_bsdf.inl"
#include "builtin/albedo/microfacet_beckmann_smith_bsdf.inl"
#include "builtin/albedo/sheen_bsdf.inl"
#include "builtin/albedo/simple_glossy_bsdf.inl"
#include "builtin/albedo/ward_geisler_moroder_bsdf.inl"
[[nodiscard]] static const AlbedoLUT *get_albedo_lut(std::string_view name) {
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
