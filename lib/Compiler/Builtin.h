#pragma once

#include <array>
#include <string_view>

namespace smdl::builtin {

static const char *const api = R"*(/// The API module, which implements the types and functions the MDL
/// specification treats as intrinsic: the resource types, the material
/// model structures, and the spectral color conversions, plus internal
/// helpers shared by the other builtin modules.
#smdl

/// The number of wavelengths in the RGB-to-color curves.
const int RGB_TO_COLOR_NUM_WAVELENGTHS=32;

/// The minimum wavelength of the RGB-to-color curves in nanometers.
const float RGB_TO_COLOR_MIN_WAVELENGTH=380.;

/// The maximum wavelength of the RGB-to-color curves in nanometers.
const float RGB_TO_COLOR_MAX_WAVELENGTH=720.;

/// The RGB-to-color curves.
const static auto RGB_TO_COLOR_CURVES=auto[](
auto[](1.0618958,1.061502,1.0614336,1.0622711,1.0622036,1.062506,1.0623939,1.0624707,1.0625048,1.0624366,1.0620694,1.0613167,1.0610334,1.0613868,1.0614215,1.0620337,1.0625497,1.0624317,1.0625249,1.0624278,1.062475,1.0625539,1.0625327,1.0623922,1.0623651,1.0625256,1.0612278,1.0594263,1.0599811,1.0602547,1.0601263,1.0606565),
auto[](1.0414628,1.0328661,1.0126146,1.0350461,1.0078661,1.042228,1.0442597,1.0535238,1.0180776,1.044273,1.0529362,1.0537034,1.0533901,1.0537783,1.0527093,1.0530449,1.0550555,1.0553674,1.0454307,0.6234895,0.1803807,-76304e-7,-1522e-7,-75102e-7,-21709e-7,6592e-7,0.0122788,-4467e-6,0.0171198,49211e-7,58763e-7,0.0252594),
auto[](0.9942214,0.9898694,0.9829366,0.9962787,1.0198956,1.0166396,1.0220913,0.9965166,1.0097766,1.0215422,0.6403195,25012e-7,6534e-6,28334e-7,-0.,-90592e-7,33937e-7,-30639e-7,0.2220394,0.6314114,0.9748099,0.9720956,1.017377,0.9987519,0.9470173,0.8525862,0.948978,0.9475188,0.9959894,0.8630135,0.8915099,0.8486649),
auto[](55741e-7,-47983e-7,-52537e-7,-64571e-7,-59694e-7,-21837e-7,0.0167811,0.0960964,0.2121736,0.3616913,0.5396101,0.7440881,0.9220957,1.0460304,1.0513825,1.0511992,1.051053,1.0517397,1.0516043,1.0511944,1.051159,1.0516613,1.0514039,1.0515941,1.051146,1.0515124,1.0508871,1.0508924,1.0477493,1.0493273,1.0435964,1.0392281),
auto[](0.165756,0.1184644,0.1240829,0.1137127,0.0789924,0.0322056,-0.0107984,0.018052,53407e-7,0.0136549,-59564e-7,-18444e-7,-0.0105719,-29376e-7,-0.0107905,-80224e-7,-22669e-7,702e-5,-81528e-7,0.6077287,0.9883156,0.9939169,1.0039339,0.992345,0.9992653,1.0084622,0.983583,1.0085024,0.9745114,0.9854327,0.9349576,0.9871391),
auto[](26494e-7,-50175e-7,-0.0125472,-94555e-7,-0.0125261,-79171e-7,-79956e-7,-93559e-7,0.0654686,0.3957288,0.7524402,0.9637648,0.9985443,0.9999298,0.9993908,0.9999437,0.9993912,0.9991124,0.9601958,0.6318628,0.257974,94015e-7,-30798e-7,-4523e-6,-68933e-7,-90352e-7,-85914e-7,-83691e-7,-78686e-7,-84e-7,54301e-7,-27746e-7),
auto[](0.9920977,0.9887643,0.9953904,0.9952932,0.9918145,1.0002584,0.9996848,0.9998812,0.9850401,0.7902985,0.560822,0.3313346,0.1369241,0.0189149,-51e-7,-424e-6,-4193e-7,17473e-7,37999e-7,-551e-6,-437e-7,75875e-7,0.0257957,0.0381684,0.0494896,0.049596,0.0498148,0.0398409,0.030501,0.0212431,69597e-7,41734e-7),
); /// The nontrivial RGB-to-color implementation.
///
/// This is factored into an internal function because, despite the fact
/// that the `color` type is not necessarily RGB, every MDL codebase on
/// planet Earth assumes that it is, e.g., uses `color(1.0, 1.0, 1.0)`
/// to mean white instead of `color(1.0)`.
///
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
color c(0.);
color w(color($state.wavelength_base));
w-=RGB_TO_COLOR_MIN_WAVELENGTH;
w*=RGB_TO_COLOR_NUM_WAVELENGTHS/(RGB_TO_COLOR_MAX_WAVELENGTH-RGB_TO_COLOR_MIN_WAVELENGTH);
for(int i=0;i<$WAVELENGTH_BASE_MAX;i++){
auto t(w[i]);
if((0.<=t)&(t<=RGB_TO_COLOR_NUM_WAVELENGTHS)){
int j(#min(int(t),RGB_TO_COLOR_NUM_WAVELENGTHS-2));
t=#min(t-j,1.);
c[i]=#sum(float2(1-t,t)*(coeffW*float2(&RGB_TO_COLOR_CURVES[0][j])+coeffCMY*float2(&RGB_TO_COLOR_CURVES[k0+1][j])+coeffRGB*float2(&RGB_TO_COLOR_CURVES[k2+4][j])));
}
}
return #max(c*0.94,0.);
}

/// Convert RGB to color, used by `color` constructor!
@(macro)
export color _rgb_to_color(const float3 rgb){
if(#all(rgb.xx==rgb.yz)){
return color(rgb.x);
} else {
return nontrivialRGBToColor(rgb);
}
}
export struct _UniformLerp{
int i;
float t;
};
@(pure macro)
export auto _uniform_lerp_index_and_fraction(const int count,const float xmin,const float xmax,const float x){
const float t=(count-1)*#max(0.,#min((x-xmin)/(xmax-xmin),1.));
const int i=#min(int(#floor(t)),count-2);
return _UniformLerp(i,t-i);
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
export float _polyline_lerp(const int count,const &float xs,const &float ys,const float x){
if(count<=0){
return 0.;
} else if(count==1){
return ys[0];
} else {
int i=_lower_bound(count,xs,x)-1;
i=#min(i,count-2);
i=#max(i,0);
const auto x0=xs[i];
const auto x1=xs[i+1];
float t=(x-x0)/(x1-x0);
t=#max(t,0.);
t=#min(t,1.);
return (1-t)*ys[i]+t*ys[i+1];
}
}
@(noinline)
export color _samples_to_color(const int count,const &float wavelengths,const &float amplitudes){
auto c=color(0.);
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

/// The fits of CIE 1931 XYZ by Wyman et al for wavelength in nanometers.
///
/// NOTE: The implementation here does not exactly look like the published
/// piecewise gaussian equations because it is calculating the X, Y, and Z
/// fits in parallel by explicitly evaluating the first few terms of the
/// exponential series.
///
@(pure)
export float3 _wyman_xyz(const float w){
auto x(w-auto(442.,599.8,501.1,568.8,530.9,437.,459.));
x*=#select(x<0,auto(0.0624,0.0264,0.049,0.0213,0.0613,0.0845,0.0385),auto(0.0374,0.0323,0.0382,0.0247,0.0322,0.0278,0.0725),);
x*=0.5*x;
const auto x1(x);
auto y(1+x);
y+=(x*=x1*0.5);
y+=(x*=x1*0.333333);
y+=(x*=x1*0.25);
y=auto(0.362,1.056,-0.065,0.821,0.286,1.217,0.681)*0.01/y;
return float3(y[0]+y[1]+y[2],y[3]+y[4],y[5]+y[6]);
}

/// The fit of CIE 1931 Y, without X or Z, by Wyman et al fit
/// for wavelength in nanometers.
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

/// Convert color to RGB, used by `float3` constructor!
@(hot noinline)
export float3 _color_to_rgb(const color c){
float3 result(0.);
for(int i=0;i<$WAVELENGTH_BASE_MAX;++i){
result+=_wyman_xyz($state.wavelength_base[i])*c[i];
}
result/=$WAVELENGTH_BASE_MAX;
result*=$state.wavelength_max-$state.wavelength_min;
return float3x3(float3(3.24045,-0.969266,0.0556434),float3(-1.53714,1.87601,-0.204026),float3(-0.498532,0.041556,1.05723),)*result;
}

/// The JIT-visible RGB-to-color function advertised by the `Compiler`
/// for convenience.
@(visible noinline)
void smdlRGBToColor(const &float3 rgb,const &float cptr){
color c(_rgb_to_color(*rgb));
#memcpy(cptr,&c,#sizeof(color));
}

/// The JIT-visible color-to-RGB function advertised by the `Compiler`
/// for convenience.
@(visible noinline)
void smdlColorToRGB(const &float cptr,const &float3 rgb){
*rgb=_color_to_rgb(color(cptr));
}

/// The intensity mode enum.
export enum intensity_mode{intensity_radiant_exitance, ///< Power (watts) per unit area (meters squared).
intensity_power,                                       ///< Power (watts).
};

/// The Bidirectional Scattering Distribution Function (BSDF) tag.
export tag bsdf;

/// The Volume Distribution Function (VDF) tag.
export tag vdf;

/// The Emission Distribution Function (EDF) tag.
export tag edf;

/// The hair Bidirectional Scattering Distribution Function (BSDF) tag.
export tag hair_bsdf;

/// The default BSDF is just an empty struct!
export struct _default_bsdf:default bsdf{
/// The flags.
static const int df_flags=0;
};

/// The default VDF is just an empty struct!
export struct _default_vdf:default vdf{
/// The flags.
static const int df_flags=0;
};

/// The default EDF is just an empty struct!
export struct _default_edf:default edf{
/// The flags.
static const int df_flags=0;
};

/// The default hair BSDF is just an empty struct!
export struct _default_hair_bsdf:default hair_bsdf{
/// The flags.
static const int df_flags=0;
};

/// The texture 2D structure.
export struct texture_2d{
texture_2d(const string name,const auto gamma=0)=#load_texture_2d(name,int(gamma));

/// The tile count.
const int2 tile_count=int2(1,1);

/// The tile extents.
const auto tile_extents=int2[](int2(0));

/// The tile buffers.
const auto tile_buffers=auto[](cast<&float4>(none));

/// The gamma mode.
const int gamma=0;
};

/// The texture 3D structure.
export struct texture_3d{
texture_3d(const string name,const auto gamma=0)=#load_texture_3d(name,int(gamma));

/// The gamma mode.
const int gamma=0;
};

/// The texture cube structure.
export struct texture_cube{
texture_cube(const string name,const auto gamma=0)=#load_texture_cube(name,int(gamma));

/// The gamma mode.
const int gamma=0;
};

/// The texture ptex structure.
export struct texture_ptex{
texture_ptex(const string name,const auto gamma=0)=#load_texture_ptex(name,int(gamma));

/// The pointer to the `smdl::Ptexture`
const &void ptr=none;

/// The gamma mode.
const int gamma=0;
};

/// The BSDF measurement structure.
export struct bsdf_measurement{
bsdf_measurement(const string name)=#load_bsdf_measurement(name);

/// The pointer to the `smdl::BSDFMeasurement`.
const &void ptr=none;

/// The scatter mode, either `scatter_reflect` or `scatter_transmit`.
const int mode=0;

/// The number of samples in zenith.
const int num_theta=0;

/// The number of samples in azimuth.
const int num_phi=0;

/// The buffer `smdl::BSDFMeasurement::buffer` which points to a
/// table of `num_theta * num_theta * num_phi` values of type `float`
/// or `float3`.
const auto buffer=cast<&float>(none);
};

/// The light profile structure.
export struct light_profile{
light_profile(const string name)=#load_light_profile(name);

/// The pointer to the `smdl::LightProfile`
const &void ptr=none;

/// The maximum intensity.
const float max_intensity=0;

/// The power.
const float power=0;
};

/// The spectral curve structure.
export struct spectral_curve{
spectral_curve(const string name)=#load_spectral_curve(name);
spectral_curve(const string name,const int curve_index)=#load_spectral_curve(name,curve_index);
spectral_curve(const string name,const string curve_name)=#load_spectral_curve(name,curve_name);

/// The number of wavelengths and values.
const int count=0;

/// The wavelengths in nanometers.
const &float wavelengths=none;

/// The amplitudes.
const &float amplitudes=none;
};
@(macro)
export color _spectral_curve_to_color(const spectral_curve curve){
return _samples_to_color(curve.count,curve.wavelengths,curve.amplitudes);
}

/// The material emission description.
export struct material_emission{
/// The Emission Distribution Function (EDF).
edf emission=edf();

/// The intensity multiplier.
$(color|float) intensity=1.;

/// The intensity mode.
intensity_mode mode=intensity_radiant_exitance;
};

/// The material surface description.
export struct material_surface{
/// The Bidirectional Scattering Distribution Function (BSDF).
bsdf scattering=bsdf();

/// The material emission description.
material_emission emission=material_emission();
};

/// The material volume description.
export struct material_volume{
/// The Volume Distribution Function (VDF).
vdf scattering=vdf();

/// The absorption coefficient in units of inverse distance.
$(?color) absorption_coefficient=none;

/// The scattering coefficient in units of inverse distance.
$(?color) scattering_coefficient=none;
};

/// The material geometry description.
export struct material_geometry{
/// The displacement.
float3 displacement=float3();

/// The cutout opacity between 0 (transparent) and 1 (opaque).
float cutout_opacity=1.;

/// The normal.
float3 normal=$state.normal;
};

/// The material description.
export struct material{
/// Thin walled?
bool thin_walled=false;

/// The material surface description.
material_surface surface=material_surface();

/// If non-default, the backface surface description.
material_surface backface=material_surface();

/// The index of refraction.
///
/// NOTE: In the MDL specification, IOR is type `color` but for
/// implementation simplicity this is restricted to being type `float`.
///
float ior=1.4;

/// The material volume description.
material_volume volume=material_volume();

/// The material geometry description.
material_geometry geometry=material_geometry();

/// The hair Bidirectional Scattering Distribution Function (BSDF).
hair_bsdf hair=hair_bsdf();

/// The temperature in kelvin for renderers that support blackbody
/// emission, where a negative value means unset. NOTE: This is
/// non-standard!
float temperature=-1;
};
const int MATERIAL_TRANSPORT_IMPORTANCE=(1<<0);
const int MATERIAL_THIN_WALLED=(1<<1);
const int MATERIAL_HAS_SURFACE=(1<<2);
const int MATERIAL_HAS_BACKFACE=(1<<3);
const int MATERIAL_HAS_SURFACE_EMISSION=(1<<4);
const int MATERIAL_HAS_BACKFACE_EMISSION=(1<<5);
const int MATERIAL_HAS_VOLUME=(1<<6);
const int MATERIAL_HAS_HAIR=(1<<7);

/// An instance of a material corresponding to `smdl::JIT::Material::Instance`
/// in the C++ API.
export struct _MaterialInstance{
/// The material deep copied with `#bump()`.
&material ptr;

/// The geometry displacement.
&material_geometry geometry=&ptr.geometry;

/// The index of refraction.
float ior=ptr.ior;

/// The exterior index of refraction, being the absolute index of the
/// medium on the front side of the geometry. This defaults to 1 and is
/// meant to be overwritten between instance evaluation and scattering
/// by renderers that track nested dielectrics. The relative ratio the
/// scattering calculations refract with is `exterior_ior / ior`.
float exterior_ior=1.;

/// The temperature.
float temperature=ptr.temperature;

/// The volume absorption coefficient.
&color absorption_coefficient=#is_void(ptr.volume.absorption_coefficient)?none:&ptr.volume.absorption_coefficient;

/// The volume scattering coefficient.
&color scattering_coefficient=#is_void(ptr.volume.scattering_coefficient)?none:&ptr.volume.scattering_coefficient;

/// The `surface` emission intensity, or `none` if the `surface` has no
/// non-default emission EDF. See `df::_emissionEvaluate()` for how the
/// `intensity_mode` units are resolved.
&color surface_emission_intensity=#is_default(ptr.surface.emission.emission)?none:#bump(color(ptr.surface.emission.intensity));

/// The `backface` emission intensity, or `none` if the `backface` has no
/// non-default emission EDF.
&color backface_emission_intensity=#is_default(ptr.backface.emission.emission)?none:#bump(color(ptr.backface.emission.intensity));

/// The wavelength count.
int wavelength_base_max=$WAVELENGTH_BASE_MAX;

/// The flags.
int flags=$state.transport|(ptr.thin_walled?MATERIAL_THIN_WALLED:0)|(!#is_default(ptr.surface)?MATERIAL_HAS_SURFACE:0)|(!#is_default(ptr.backface)?MATERIAL_HAS_BACKFACE:0)|(!#is_default(ptr.surface.emission.emission)?MATERIAL_HAS_SURFACE_EMISSION:0)|(!#is_default(ptr.backface.emission.emission)?MATERIAL_HAS_BACKFACE_EMISSION:0)|(!#is_default(ptr.volume)?MATERIAL_HAS_VOLUME:0)|(!#is_default(ptr.hair)?MATERIAL_HAS_HAIR:0);

/// The df flags for the `surface` component.
int df_flags_surface=ptr.surface.scattering.df_flags;

/// The df flags for the `backface` component.
int df_flags_backface=ptr.backface.scattering.df_flags;

/// The emission intensity modes: bit 0 is set if the `surface` emission
/// intensity is `intensity_power` (as opposed to the default
/// `intensity_radiant_exitance`), and bit 1 likewise for the `backface`.
int emission_modes=(int(ptr.surface.emission.mode)==int(intensity_power)?1:0)|(int(ptr.backface.emission.mode)==int(intensity_power)?2:0);

/// The tangent-to-world matrix held by the `State` during construction.
float3x3 tangent_to_world=let {
const auto tangent_to_world_matrix=$state.object_to_world_matrix*$state.tangent_to_object_matrix;
} in float3x3(tangent_to_world_matrix[0].xyz,tangent_to_world_matrix[1].xyz,tangent_to_world_matrix[2].xyz,);
};

/// Albedo look-up table (LUT) for energy correction.
export struct _AlbedoLUT{
/// The number of samples of view angle cosine.
const int num_cos_theta=0;

/// The number of samples of roughness.
const int num_roughness=0;

/// The directional albedo.
///
/// NOTE: This must point to `num_cos_theta` rows by `num_roughness`
/// values.
///
const &float directional_albedo=none;

/// The average albedo.
///
/// NOTE: This must point to `num_roughness` values.
///
const &float average_albedo=none;
};

/// A complex value.
export struct complex{
/// The real coefficient.
auto a=0.;

/// The imaginary coefficient.
auto b=0.;
};

/// Complex negative.
@(pure macro)
export auto _complex_neg(const complex z)=complex(-z.a,-z.b);

/// Complex conjugate.
@(pure macro)
export auto _complex_conj(const complex z)=complex(z.a,-z.b);

/// Complex norm.
@(pure macro)
export auto _complex_norm(const complex z)=z.a*z.a+z.b*z.b;

/// Complex absolute value.
@(pure macro)
export auto _complex_abs(const complex z)=#sqrt(_complex_norm(z));

/// Complex inverse.
@(pure macro)
export auto _complex_inv(const complex z)=let {
const auto denom=1./_complex_norm(z);
} in complex(z.a*denom,-z.b*denom);

/// Complex addition.
@(pure macro)
export auto _complex_add(const complex z,const complex w)=complex(z.a+w.a,z.b+w.b);

/// Complex subtraction.
@(pure macro)
export auto _complex_sub(const complex z,const complex w)=complex(z.a-w.a,z.b-w.b);

/// Complex multiplication.
@(pure macro)
export auto _complex_mul(const complex z,const complex w)=complex(z.a*w.a-z.b*w.b,z.a*w.b+z.b*w.a);

/// Complex division.
@(pure macro)
export auto _complex_div(const complex z,const complex w)=_complex_mul(z,_complex_inv(w));

/// Complex exponential.
@(pure macro)
export auto _complex_exp(const complex z)=let {
const auto a=#exp(z.a);
} in complex(a*#cos(z.b),a*#sin(z.b));

/// Complex logarithm.
@(pure macro)
export auto _complex_log(const complex z)=complex(#log(_complex_abs(z)),#atan2(z.b,z.a));

/// Complex square root.
@(pure macro)
export auto _complex_sqrt(const complex z)=let {
const auto absz=_complex_abs(z);
} in complex(#sqrt(0.5*(absz+z.a)),#sqrt(0.5*(absz-z.a))*#sign(z.b),);

/// A hash function for use with procedural algorithms.
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

static const char *const anno = R"*(/// The standard annotations, following the MDL specification. These carry
/// metadata for tools and user interfaces and do not affect compilation.
#smdl

/// The recommended range for the annotated value. Values outside remain valid.
export annotation soft_range(auto min,auto max);

/// The required range for the annotated value. Values outside are invalid.
export annotation hard_range(auto min,auto max);

/// The human-readable name to display instead of the identifier.
export annotation display_name(string name);

/// The group to organize the annotated item under in a user interface.
export annotation in_group(string group);

/// The group and subgroup to organize the annotated item under in a user interface.
export annotation in_group(string group,string subgroup);

/// The group, subgroup, and sub-subgroup to organize the annotated item under in a user interface.
export annotation in_group(string group,string subgroup,string subsubgroup);

/// The relative position of the annotated item in a user interface.
export annotation ui_order(int order);

/// The condition, as an MDL expression over sibling parameters, under which
/// the annotated parameter is enabled in a user interface.
export annotation enable_if(string condition);

/// Hides the annotated item from user interfaces.
export annotation hidden();

/// The human-readable description. This is also understood by `smdl doc`
/// as a fallback for declarations without `///` documentation comments.
export annotation description(string description);

/// The thumbnail image to preview the annotated item in a user interface.
export annotation thumbnail(string name);

/// The author.
export annotation author(string name);

/// A contributor.
export annotation contributor(string name);

/// The copyright notice.
export annotation copyright_notice(string copyright);

/// The creation date and notes.
export annotation created(int year,int month,int day,string notes);

/// The last modification date and notes.
export annotation modified(int year,int month,int day,string notes);

/// The version of the annotated module.
export annotation version(int major,int minor,int patch,string prerelease="");

/// The version of another module that the annotated module depends on.
export annotation dependency(string module_name,int major,int minor,int patch,string prerelease="");

/// The keywords for search and categorization.
export annotation key_words(string[] words);

/// Marks the annotated item as intentionally unused.
export annotation unused(string description="");

/// Marks the annotated item as deprecated.
export annotation deprecated(string description="");

/// The hint describing the intended usage of the annotated item, e.g., `"color"` or `"normal"`.
export annotation usage(string hint="");

/// The qualified name of the entity the annotated item originates from.
export annotation origin(string name="");
)*";

static const char *const debug = R"*(/// Debugging functions, following the MDL specification. These only do
/// anything when the compiler is in debug mode, and they always return
/// `true` so they may be chained into boolean expressions.
#smdl

/// Asserts that `condition` holds, reporting `reason` if it does not.
@(pure macro)
export bool assert(const bool condition,const string reason){
#assert(condition,reason) if($DEBUG);
return true;
}

/// Breaks into the debugger.
@(pure macro)
export bool breakpoint(){
#breakpoint() if($DEBUG);
return true;
}

/// Prints the given value to the console.
@(pure macro)
export bool print(const auto a){
#print(a) if($DEBUG);
return true;
}
)*";

static const char *const df = R"*(/// The distribution functions, following the MDL specification: the
/// elemental BSDFs, EDFs, and VDFs, the modifier and layering
/// combinators, and the mixers, plus the internal scattering and
/// emission entry points the compiler exposes to renderers.
///
/// NOTE: The `handle` of every distribution function here should be a
/// `string` by the specification, but we `void` it because we have no
/// use for it.
///
#smdl
using ::math import *;
import ::tex::*;

/// An arbitrary epsilon for stabilizing scattering calculations.
const float EPSILON=1e-6;

/// An arbitrary chance for sampling diffusely in BSDFs with multiple-scattering.
const float MULTISCATTER_DIFFUSE_CHANCE=0.2;

/// The default absolute index of refraction, matching the default of
/// `material.ior` in `api.smdl`.
const float DEFAULT_IOR=1.4;

/// Convert a user-facing absolute IOR to the relative ratio oriented with
/// the current frame of the given scatter parameters, honoring the backface
/// reciprocation applied in the parameter `finalize` blocks and the
/// exterior medium of the instance.
@(pure macro)
auto relativeIOR(const auto params,const auto absoluteIOR){
if(params.hitBackface&!params.thin_walled){
return absoluteIOR/params.exterior_ior;
} else {
return params.exterior_ior/absoluteIOR;
}
}
const int DF_REFLECTION=(1<<0);
const int DF_TRANSMISSION=(1<<1);
const int DF_DIFFUSE=(1<<2);
const int DF_GLOSSY=(1<<3);
const int DF_SPECULAR=(1<<4);
@(macro)
float3x3 orthonormalBasis(float3 z){
z=normalize(z);
auto x=z.z<-0.9999?float3(0.,-1.,0.):float3(-z.x/(z.z+1.)+1.,-z.y/(z.z+1.),-1.);
x=normalize(x-dot(x,z)*z);
auto y=normalize(cross(z,x));
return float3x3(x,y,z);
}

/// The scatter mode, describing the hemispheres a BSDF scatters into.
export enum scatter_mode{
scatter_none=0x0,             ///< None
scatter_reflect=0x1,          ///< Reflect (same hemisphere)
scatter_transmit=0x2,         ///< Transmit (opposite hemisphere)
scatter_reflect_transmit=0x3, ///< Reflect or transmit
};
@(pure macro)
float scatterReflectChance(const scatter_mode mode){
const auto reflWeight(#select((int(mode)&1)!=0,1.,0.));
const auto tranWeight(#select((int(mode)&2)!=0,1.,0.));
return reflWeight/#max(reflWeight+tranWeight,1.);
}

/// Declare libm `erf`
@(pure foreign)
double erf(double x);

/// Declare libm `erfc`
@(pure foreign)
double erfc(double x);

/// The Monte Carlo utilities: low-discrepancy sequences and the canonical
/// sampling routines shared by the distribution implementations.
export namespace monte_carlo {

/// Next canonical random vector in quasi-random 2-dimensional low discrepancy sequence.
@(pure macro)
export float2 nextLowDiscrepancy(const &float2 xi)=(*xi=frac(*xi+float2(0.75487766,0.56984029)));

/// Next canonical random vector in quasi-random 3-dimensional low discrepancy sequence.
@(pure macro)
export float3 nextLowDiscrepancy(const &float3 xi)=(*xi=frac(*xi+float3(0.81917251,0.6710436,0.54970047)));

/// Next canonical random vector in quasi-random 4-dimensional low discrepancy sequence.
@(pure macro)
export float4 nextLowDiscrepancy(const &float4 xi)=(*xi=frac(*xi+float4(0.85667488,0.73389185,0.62870672,0.53859725)));

/// Bool sample with `chance` probability of returning `true`.
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

/// Uniform wavelength index sample.
@(pure macro)
export int uniformWavelengthIndexSample(const &float xi){
const int i(#min(int(*xi*=$WAVELENGTH_BASE_MAX),$WAVELENGTH_BASE_MAX-1));
*xi-=i;
return i;
}

/// Uniform disk sample.
@(pure)
export float2 uniformDiskSample(float2 xi){
xi=2*xi-1;
xi=#select(xi==0,EPSILON,xi);
const bool cond((absxi:=#abs(xi),absxi.x>absxi.y));
const float rad(#select(cond,xi.x,xi.y));
const float phi(#select(cond,($PI/4)*xi.y/xi.x,($PI/2)-($PI/4)*xi.x/xi.y));
return rad*float2(#cos(phi),#sin(phi));
}

/// Cosine-weighted hemisphere sample.
@(pure)
export float3 cosineHemisphereSample(float2 xi){
return float3((p:=uniformDiskSample(xi)),#sqrt(#max(1-#sum(p*p),0)));
}

/// Uniform hemisphere sample.
@(pure)
export float3 uniformHemisphereSample(float2 xi){
const float cosTheta=saturate(xi.x);
const float sinTheta=#sqrt(1-cosTheta*cosTheta);
return float3(sinTheta*#cos(phi:=$TWO_PI*xi.y),sinTheta*#sin(phi),cosTheta,);
}

/// Uniform sphere sample.
@(pure)
export float3 uniformSphereSample(float2 xi){
const float cosTheta=2*saturate(xi.x)-1;
const float sinTheta=#sqrt(1-cosTheta*cosTheta);
return float3(sinTheta*#cos(phi:=$TWO_PI*xi.y),sinTheta*#sin(phi),cosTheta,);
}

/// Erf inverse.
@(pure)
export double erfInverse(double y){
double w=-#log(#max(1e-6d,(1-y)*(1+y)));
double x=0;
if(w<5){
w=w-2.5d;
x=w*2.81022636e-8d+3.43273939e-7d;
x=w*x-3.5233877e-6d;
x=w*x-4.39150654e-6d;
x=w*x+2.1858087e-4d;
x=w*x-1.25372503e-3d;
x=w*x-4.17768164e-3d;
x=w*x+0.246640727d;
x=w*x+1.50140941d;
} else {
w=#sqrt(w)-3;
x=x*-2.00214257e-4d+1.00950558e-4d;
x=w*x+1.34934322e-3d;
x=w*x-3.67342844e-3d;
x=w*x+5.73950773e-3d;
x=w*x-0.0076224613d;
x=w*x+9.43887047e-3d;
x=w*x+1.00167406d;
x=w*x+2.83297682d;
}
x*=y;
x-=(erf(x)-y)/(1.1283791671d*#exp(-x*x));
x-=(erf(x)-y)/(1.1283791671d*#exp(-x*x));
return x;
}
} /// The specular utilities: reflection and refraction geometry and the
/// Fresnel equations, all in terms of relative IORs.
export namespace specular {

/// Reflect direction `wi` across normal direction `wm`.
@(pure)
export float3 reflect(const float3 wi,const float3 wm)=2*#sum(wi*wm)*wm-wi;

/// Reflect direction `wi` across normal direction `wm` with index of refraction `ior`.
@(pure)
export float3 refract(const float3 wi,const float3 wm,const float ior){
const auto cosThetai(#sum(wi*wm));
const auto cos2Thetai(#min(cosThetai*cosThetai,1));
const auto cos2Thetat(#max(1-ior*ior*(1-cos2Thetai),0));
const auto cosThetat(#sqrt(cos2Thetat)*-#sign(cosThetai));
return -ior*wi+(ior*cosThetai+cosThetat)*wm;
}

/// Calculate half vector that reflects direction `wo` to direction `wi`.
///
/// NOTE: The result is not normalized, and is guaranteed to be in the
/// upper Z hemisphere.
///
@(pure)
export float3 reflectionHalfVector(const float3 wo,const float3 wi)=(vh:=(wo+wi))*#sign(vh.z);

/// Calculate half vector that refracts direction `wo` to direction `wi` through index-of-refraction `ior`.
///
/// NOTE: The result is not normalized, and is guaranteed to be in the
/// upper Z hemisphere.
///
@(pure)
export float3 refractionHalfVector(const float3 wo,const float3 wi,const float ior,)=(vh:=-(ior*wo+wi))*#sign(vh.z);

/// Calculate the Jacobian of `refractionHalfVector` with respect to `wi`,
/// which converts microsurface normal densities into incoming direction
/// densities.
@(pure)
export auto refractionHalfVectorJacobian(const float3 wo,const float3 wi,const float ior,)=#abs(#sum(wi*(vh:=refractionHalfVector(wo,wi,ior))))/((vh2:=#sum(vh*vh))*#sqrt(vh2));

/// The Schlick reflectance at normal incidence for the given relative IOR.
@(pure macro)
export auto schlickF0(const auto ior)=#pow((ior-1)/(ior+1),2);

/// The Schlick approximation of Fresnel reflectance, interpolating from
/// `F0` at normal incidence to `F90` at grazing incidence.
@(pure macro)
export auto schlickFresnel(
const auto cosTheta,
const auto F0,
const auto F90=1.,
const float exponent=5,
)=F0+(F90-F0)*#pow(#max(1-#abs(cosTheta),0),exponent);

/// The exact unpolarized Fresnel reflectance of a dielectric interface
/// with the given relative IOR, folding total internal reflection to 1.
@(pure)
export auto dielectricFresnel(const float cosThetai,const auto ior){
const auto cosThetat=#sqrt(#max(1.-ior*ior*(1.-cosThetai*cosThetai),0.))*#sign(cosThetai);
const auto iorCosThetai=ior*cosThetai;
const auto iorCosThetat=ior*cosThetat;
const auto rs=(iorCosThetai-cosThetat)/(iorCosThetai+cosThetat);
const auto rp=(cosThetai-iorCosThetat)/(cosThetai+iorCosThetat);
return #min(0.5*(rs*rs+rp*rp),1.);
}

/// The exact unpolarized Fresnel reflectance of a conductor interface
/// with the given complex relative IOR.
@(pure)
export auto conductorFresnel(const float cosThetai,const auto ior){
const auto cosThetat=#sqrt(1.-ior*ior*(1.-cosThetai*cosThetai))*#sign(cosThetai);
const auto iorCosThetai=ior*cosThetai;
const auto iorCosThetat=ior*cosThetat;
const auto rs=(iorCosThetai-cosThetat)/(iorCosThetai+cosThetat);
const auto rp=(cosThetai-iorCosThetat)/(cosThetai+iorCosThetat);
return #min(0.5*(#norm(rs)+#norm(rp)),1.);
}
} /// Calculate the orthogonal right-handed tangent space from the
/// given normal and tangent vectors.
@(pure noinline)
float3x3 calculateTangentSpace(const float3 normal,const float3 tangent_u){
const auto tw(normalize(normal)*#sign(normal.z));
const auto tu(normalize(tangent_u-dot(tangent_u,tw)*tw));
const auto tv(normalize(cross(tw,tu)));
return float3x3(tu,tv,tw);
}
struct ScatterEvaluateParameters{
/// Is transporting importance? i.e., tracing rays from lights to cameras?
bool isImportance;

/// The reference outgoing direction in the natural tangent space.
float3 wo0;

/// The reference incoming direction in the natural tangent space.
float3 wi0;

/// The reference mode.
scatter_mode mode=(wo0.z<0)==(wi0.z<0)?scatter_reflect:scatter_transmit;

/// Hit backface?
bool hitBackface=wo0.z<0;

/// Is thin walled?
bool thin_walled=false;

/// The relative upper-to-lower index of refraction.
float ior=1/DEFAULT_IOR;

/// The absolute index of refraction of the exterior medium, needed by
/// modifier BSDFs to convert user-facing absolute IORs into relative
/// ratios. See `relativeIOR()`.
float exterior_ior=1.;

/// The normal direction.
float3 normal=float3(0,0,1);

/// The tangent direction.
float3 tangent_u=float3(1,0,0);

/// The outgoing direction.
float3 wo=wo0;

/// The incoming direction.
float3 wi=wi0;
float shadingNormalCorrection=1;
finalize {
if(hitBackface){
wo0=-wo0;
wi0=-wi0;
wo=-wo;
wi=-wi;
ior=1./ior if(!thin_walled);
}
}
};
struct ScatterEvaluateResult{
/// The Bidirectional Scattering Distribution Function (BSDF) evaluation.
$(color|float) f=0.;

/// The Probability Density Function (PDF) evaluations.
/// - `pdf[0]` is the forward density of sampling `wi` given `wo`.
/// - `pdf[1]` is the reverse density of sampling `wo` given `wi`.
float2 pdf=float2(0.);

/// Is known to be black by construction? Faster than checking every
/// element of `f`!
bool isBlack=false;
};

/// Recalculate the effective tangent space. Returns `true` if the
/// directions are still consistent with the scatter mode after applying
/// the effective tangent space.
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

/// The primary outgoing direction in the natural geometric tangent space.
float3 wo0;

/// Hit backface?
bool hitBackface=wo0.z<0;

/// Is thin walled?
bool thin_walled=false;

/// The active index of refraction.
float ior=1/DEFAULT_IOR;

/// The absolute index of refraction of the exterior medium, needed by
/// modifier BSDFs to convert user-facing absolute IORs into relative
/// ratios. See `relativeIOR()`.
float exterior_ior=1.;

/// The active normal direction.
float3 normal=float3(0,0,1);

/// The active tangent direction.
float3 tangent_u=float3(1,0,0);

/// The active outgoing direction (expanded in the active tangent space).
float3 wo=wo0;

/// The canonical random sample in `[0,1]^4`.
float4 xi;
finalize {
if(hitBackface){
wo0=-wo0;
wo=-wo;
ior=1/ior if(!thin_walled);
}
}
};
struct ScatterSampleResult{
/// The sampled incoming direction.
float3 wi=float3(0.);

/// The sampled scatter mode.
scatter_mode mode=scatter_none;

/// If sampled from a directional delta distribution, the BSDF evaluation (which is otherwise unevaluable).
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

/// Calculate the shading-normal correction factor for a sampled delta direction under
/// importance transport, consistent with the convention `recalculateTangentSpace`
/// applies on the evaluate path. Expects the sampled incoming direction both in the
/// shading tangent space `wiShading` and in the natural tangent space `wiNatural`.
@(pure macro)
float sampleShadingNormalCorrection(inline const &ScatterSampleParameters params,const float3 wiShading,const float3 wiNatural){
const auto numer(wo.z*wiNatural.z);
const auto denom(wiShading.z*wo0.z);
return denom==0?1.:#abs(numer/denom);
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
if(#typeof(this.multiscatter_tint)==void||(#typeof(this.multiscatter_tint)==float&&this.multiscatter_tint==0.)){
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
return #min(1.,lerp(lerp(ptr0[0],ptr0[1],t),lerp(ptr1[0],ptr1[1],t),s));
};
const float Ewi=return_from{
float s((lut.num_cos_theta-1)*#min(cosThetai,1));
const int i(#min(int(#floor(s)),lut.num_cos_theta-2));
const &float ptr0(&lut.directional_albedo[lut.num_roughness*(i+0)+j]);
const &float ptr1(&lut.directional_albedo[lut.num_roughness*(i+1)+j]);
s=s-i;
return #min(1.,lerp(lerp(ptr0[0],ptr0[1],t),lerp(ptr1[0],ptr1[1],t),s));
};
const float Eav=#min(1.,lerp(lut.average_albedo[j],lut.average_albedo[j+1],t));
const auto ms_f=cosThetai/$PI*(1-Ewo)*(1-Ewi)/(1-Eav+1e-6);
const auto ms_pdf=auto(cosThetai,cosThetao)/$PI;
return ScatterEvaluateResult(f: this.tint*(f+this.multiscatter_tint*ms_f),pdf: lerp(pdf,ms_pdf,MULTISCATTER_DIFFUSE_CHANCE));
}
}
@(pure macro)
?ScatterSampleResult ScatterSampleResultWithMultiscatter(const auto this,const &float4 xi[[anno::unused()]],const float3x3 tbn[[anno::unused()]]){
if(#typeof(this.multiscatter_tint)==void||(#typeof(this.multiscatter_tint)==float&&this.multiscatter_tint==0.)){
} else {
if(monte_carlo::boolSample(&xi.w,MULTISCATTER_DIFFUSE_CHANCE)){
return ScatterSampleResult(wi: tbn*monte_carlo::cosineHemisphereSample(xi.xy),mode: scatter_reflect);
}
}
}
struct EmissionEvaluateParameters{
/// The emission direction in the natural tangent space, pointing away
/// from the surface.
float3 wi0;

/// Hit backface?
bool hitBackface=wi0.z<0;

/// The normal direction.
float3 normal=float3(0,0,1);

/// The tangent direction.
float3 tangent_u=float3(1,0,0);

/// The emission direction (expanded in the active tangent space).
float3 wi=wi0;
finalize {
if(hitBackface){
wi0=-wi0;
wi=-wi;
}
}
};
struct EmissionEvaluateResult{
/// The Emission Distribution Function (EDF) evaluation, normalized such
/// that the cosine-weighted integral over the upper hemisphere is 1.
$(color|float) f=0.;

/// The Probability Density Function (PDF) with respect to solid angle
/// of sampling `wi`.
float pdf=0.;

/// Is known to be black by construction? Faster than checking every
/// element of `f`!
bool isBlack=false;
};
struct EmissionSampleParameters{
/// The canonical random sample in `[0,1]^4`.
float4 xi;

/// The normal direction.
float3 normal=float3(0,0,1);

/// The tangent direction.
float3 tangent_u=float3(1,0,0);
};
struct EmissionSampleResult{
/// The sampled emission direction in the natural tangent space.
float3 wi=float3(0.);

/// Is valid?
bool isValid=false;
};

/// Recalculate the effective tangent space for emission evaluation.
/// Returns `true` if `wi` remains in the upper hemisphere after applying
/// the effective tangent space.
@(pure noinline)
bool recalculateTangentSpace(inline const &EmissionEvaluateParameters params){
auto tbn(calculateTangentSpace(normal,tangent_u));
wi=normalize(wi0*tbn);
return wi.z>0;
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
@(pure macro)
auto emissionEvaluate(const &_default_edf this[[anno::unused()]],const &EmissionEvaluateParameters params[[anno::unused()]]){
return EmissionEvaluateResult(isBlack: true);
}
@(pure macro)
auto emissionSample(const &_default_edf this[[anno::unused()]],const &EmissionSampleParameters params[[anno::unused()]]){
return EmissionSampleResult();
}
const float EON_CONSTANT1=0.5-2./(3.*$PI);
const float EON_CONSTANT2=2./3.-28./(15.*$PI);

/// The directional albedo of the FON single-scattering lobe at unit
/// single-scattering albedo, using the rational fit from the EON paper
/// (error below 0.1% over the whole angular range).
@(pure)
float eonDirectionalAlbedo(const float mu,const float r){
const float mucomp(1-saturate(mu));
const float GoverPi(mucomp*(0.0571085289+mucomp*(0.491881867+mucomp*(-0.332181442+mucomp*0.0714429953))));
return (1+r*GoverPi)/(1+EON_CONSTANT1*r);
}

/// The LTC matrix coefficients `(a, b, c, d)` for CLTC sampling of the
/// EON model, fit as functions of the view cosine and roughness.
@(pure)
float4 eonLTCCoeffs(const float mu,const float r){
return float4(
r*(-0.303392+(-0.518982+0.111709*mu)*mu+(-0.276266+0.335918*mu)*r)+1,
r*(-1.16407+1.15859*mu+(0.150815-0.150105*mu)*r)/(mu*mu*mu-1.43545),
r*(0.20013+(-0.506373+0.261777*mu)*mu)+1,
r*(0.540852+(-1.01625+0.475392*mu)*mu)/(-1.0743+(0.0725628+mu)*mu),
);
}

/// The azimuthal frame `(cos(phi), sin(phi))` of the given direction, or
/// `(1, 0)` at the pole.
@(pure macro)
float2 azimuthFrame(const float3 w){
const float len(length(w.xy));
return len>EPSILON?w.xy/len:float2(1,0);
}

/// The density over `wi` of CLTC sampling for the EON model. Both
/// directions are in the local shading frame with `wo.z > 0`.
@(pure)
float eonCLTCPdf(const float3 wo,const float3 wi,const float r){
const float4 co(eonLTCCoeffs(wo.z,r));
const float2 e(azimuthFrame(wo));
const float3 wiStd(e.x*wi.x+e.y*wi.y,-e.y*wi.x+e.x*wi.y,wi.z);
const float3 wh(co.z*(wiStd.x-co.y*wiStd.z),(co.x-co.y*co.w)*wiStd.y,-co.z*(co.w*wiStd.x-co.x*wiStd.z));
const float detM(co.z*(co.x-co.y*co.w));
const float len2(#sum(wh*wh));
return detM*detM/(len2*len2+EPSILON)*#max(wh.z,0.)/($PI*0.5*(1+1/#sqrt(1+co.w*co.w)));
}

/// Sample the CLTC lobe for the EON model. Returns the sampled incoming
/// direction in the local shading frame, guaranteed to be in the upper
/// hemisphere by the clipped construction.
@(pure)
float3 eonCLTCSample(const float3 wo,const float r,const float xi0,const float xi1){
const float4 co(eonLTCCoeffs(wo.z,r));
const float rad(#sqrt(xi0));
const float phi($TWO_PI*xi1);
const float y(rad*#sin(phi));
const float x(-lerp(#sqrt(#max(1-y*y,0.)),rad*#cos(phi),0.5*(1+1/#sqrt(1+co.w*co.w))));
const float3 wh(x,y,#sqrt(#max(1-x*x-y*y,0.)));
const float3 wiLTC(normalize(float3(co.x*wh.x+co.y*wh.z,co.z*wh.y,co.w*wh.x+wh.z)));
const float2 e(azimuthFrame(wo));
return float3(e.x*wiLTC.x-e.y*wiLTC.y,e.y*wiLTC.x+e.x*wiLTC.y,wiLTC.z);
}

/// The probability of choosing the uniform hemisphere lobe over the CLTC
/// lobe when sampling the EON model, by one-sample MIS.
@(pure macro)
float eonUniformLobeChance(const float mu,const float r){
return #pow(#max(r,0.),0.1)*(0.162925+mu*(-0.372058+(0.538233-0.290822*mu)*mu));
}

/// The diffuse reflection BSDF, being Lambertian reflection with optional
/// roughness, in which case it is the energy-preserving Oren-Nayar (EON)
/// model of Portsmouth, Kutz, and Hill.
export struct diffuse_reflection_bsdf:bsdf{
/// The tint.
///
/// > Scaling factor, defined as a color, multiplied by the
/// > result of the distribution function.
///
const $(color|float) tint=1.;

/// The roughness.
///
/// > Oren-Nayar roughness coefficient, simulating view-dependent diffuse
/// > reflection. Range: `[0,1]`, with `0` specifying complete view
/// > independence.
///
const float roughness=0.;

/// The handle.
void handle="";

/// The multiscatter tint.
///
/// NOTE: The EON multiple-scattering lobe is part of the model and is
/// applied at full strength by default (`none`), which is what makes the
/// model energy preserving. Setting an explicit value scales the
/// multiple-scattering lobe for artistic control.
const $(?(color|float)) multiscatter_tint=none;

/// The flags.
static const int df_flags=DF_REFLECTION|DF_DIFFUSE;
};
@(pure)
auto scatterEvaluate(const &diffuse_reflection_bsdf this,inline const &ScatterEvaluateParameters params){
if(mode==scatter_reflect&&recalculateTangentSpace(params)){
const auto cosTheta(#abs(auto(wi.z,wo.z)));
if(this.roughness==0){
const auto pdf(cosTheta/$PI);
auto result(ScatterEvaluateResult(f: this.tint*pdf[0],pdf: pdf));
result.f*=shadingNormalCorrection if(isImportance);
return result;
} else {
const float r(this.roughness);
const float AF(1/(1+EON_CONSTANT1*r));
const float s(#sum(wo.xy*wi.xy));
const float sOverT(s>0?s/(#max_value(cosTheta)+EPSILON):s);
const float fSS(cosTheta[0]/$PI*AF*(1+r*sOverT));
const float EFo(eonDirectionalAlbedo(cosTheta[1],r));
const float EFi(eonDirectionalAlbedo(cosTheta[0],r));
const float avgEF(AF*(1+EON_CONSTANT2*r));
const auto rho(this.tint);
const auto rhoMS(rho*rho*avgEF/(1-rho*(1-avgEF)+EPSILON));
const float msShape(cosTheta[0]/$PI*#max(1-EFo,EPSILON)*#max(1-EFi,EPSILON)/#max(1-avgEF,EPSILON));
const float2 chanceU(float2(eonUniformLobeChance(cosTheta[1],r),eonUniformLobeChance(cosTheta[0],r)));
const float2 pdf(chanceU/$TWO_PI+(1-chanceU)*float2(eonCLTCPdf(wo,wi,r),eonCLTCPdf(wi,wo,r)));
if(#typeof(this.multiscatter_tint)==void){
auto result(ScatterEvaluateResult(f: rho*fSS+rhoMS*msShape,pdf: pdf));
result.f*=shadingNormalCorrection if(isImportance);
return result;
} else {
auto result(ScatterEvaluateResult(f: rho*fSS+this.multiscatter_tint*(rhoMS*msShape),pdf: pdf));
result.f*=shadingNormalCorrection if(isImportance);
return result;
}
}
} else {
return ScatterEvaluateResult(isBlack: true);
}
}
@(pure)
auto scatterSample(const &diffuse_reflection_bsdf this,inline const &ScatterSampleParameters params){
if((tbn:=recalculateTangentSpace(params))){
if(this.roughness>0){
if(monte_carlo::boolSample(&xi.z,eonUniformLobeChance(wo.z,this.roughness))){
return ScatterSampleResult(wi: (*tbn)*monte_carlo::uniformHemisphereSample(xi.xy),mode: scatter_reflect);
} else {
return ScatterSampleResult(wi: (*tbn)*eonCLTCSample(wo,this.roughness,xi.x,xi.y),mode: scatter_reflect);
}
}
return ScatterSampleResult(wi: (*tbn)*monte_carlo::cosineHemisphereSample(xi.xy),mode: scatter_reflect);
} else {
return ScatterSampleResult();
}
} /// The diffuse transmission BSDF, being Lambertian transmission through
/// the surface.
export struct diffuse_transmission_bsdf:bsdf{
/// The tint.
///
/// > Scaling factor, defined as a color, multiplied by the
/// > result of the distribution function.
///
const $(color|float) tint=1.;

/// The handle.
void handle="";

/// The flags.
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
} /// The specular BSDF, being perfect mirror reflection and/or refractive
/// transmission as a directional delta distribution.
export struct specular_bsdf:bsdf{
/// The tint.
///
/// > Scaling factor, defined as a color, multiplied by the
/// > result of the distribution function.
///
const $(color|float) tint=1.;

/// The scatter mode.
///
/// > One of three values: `scatter_reflect`, `scatter_transmit`,
/// > or (for both) `scatter_reflect_transmit`.
///
/// NOTE: With `scatter_reflect_transmit`, the reflect/transmit split is
/// weighted by the dielectric Fresnel term for the active IOR, matching
/// the reference MDL semantics. Total internal reflection folds entirely
/// into reflection.
///
const scatter_mode mode=scatter_reflect;

/// The handle.
void handle="";

/// The flags.
const int df_flags=int(mode)|DF_SPECULAR;
};
@(pure macro)
auto scatterEvaluate(const &specular_bsdf this[[anno::unused()]],const &ScatterEvaluateParameters params[[anno::unused()]]){
return ScatterEvaluateResult(isBlack: true);
}
@(pure macro)
auto scatterSample(const &specular_bsdf this,inline const &ScatterSampleParameters params){
return ScatterSampleResult() if(this.mode==scatter_none);
if((tbn:=recalculateTangentSpace(params))){
const auto reflectChance(this.mode==scatter_reflect_transmit?specular::dielectricFresnel(wo.z,ior):scatterReflectChance(this.mode));
if(xi.x<reflectChance){
const auto wiLocal(specular::reflect(wo,float3(0,0,1)));
auto result=ScatterSampleResult(wi: (*tbn)*wiLocal,mode: scatter_reflect,fDelta: color(this.tint));
*result.fDelta*=sampleShadingNormalCorrection(params,wiLocal,result.wi) if(isImportance);
return result;
} else {
const auto wiLocal(thin_walled?-wo:specular::refract(wo,float3(0,0,1),ior));
auto result=ScatterSampleResult(wi: (*tbn)*wiLocal,mode: scatter_transmit,fDelta: color(this.tint));
if(isImportance){
*result.fDelta*=sampleShadingNormalCorrection(params,wiLocal,result.wi);
} else if(!thin_walled){
*result.fDelta*=ior*ior;
}
return result;
}
} else {
return ScatterSampleResult();
}
} /// The 32x32 LTC fit of the volumetric sheen layer of Zeltner, Burley, and
/// Chiang, indexed by roughness (rows) and view cosine (columns), with three
/// entries `(aInv, bInv, R)`: the inverse transform coefficients and the
/// directional albedo. Data from the authors' supplemental code
/// (https://github.com/tizian/ltc-sheen, Apache-2.0).
static const auto SHEEN_LTC_TABLE=float3[32][32](float3[32](float3(0.01415,6e-4,1e-5),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.)),float3[32](float3(0.01941,-232e-5,0.05839),float3(0.01741,-581e-5,71e-5),float3(0.0461,-769e-5,7e-5),float3(0.10367,-74e-4,2e-5),float3(0.06244,-0.02445,0.),float3(0.23927,-242e-5,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.)),float3[32](float3(0.01927,-0.01424,0.38834),float3(0.01895,-218e-5,0.09768),float3(0.03002,-194e-5,0.01072),float3(0.03912,-384e-5,15e-4),float3(0.04938,-668e-5,39e-5),float3(0.05239,-0.01107,12e-5),float3(0.06018,-746e-5,6e-5),float3(0.0652,-0.01591,3e-5),float3(0.08253,-0.01052,2e-5),float3(0.21093,-0.01495,2e-5),float3(0.12785,-0.0153,1e-5),float3(0.1903,-0.01428,1e-5),float3(0.15254,-0.01276,0.),float3(0.16585,-0.02071,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.)),float3[32](float3(0.03084,-0.04909,0.55348),float3(0.03764,-71e-4,0.29827),float3(0.03952,-236e-5,0.11755),float3(0.04092,-201e-5,0.03677),float3(0.04433,-298e-5,983e-5),float3(0.05014,-546e-5,288e-5),float3(0.0557,-834e-5,101e-5),float3(0.06215,-0.01121,46e-5),float3(0.0666,-0.01294,23e-5),float3(0.07902,-0.01692,14e-5),float3(0.10099,-0.01639,1e-4),float3(0.10794,-0.01738,6e-5),float3(0.10632,-0.02032,4e-5),float3(0.12623,-0.01947,3e-5),float3(0.13931,-0.02354,2e-5),float3(0.15353,-0.0291,2e-5),float3(0.16109,-0.02565,1e-5),float3(0.14583,-0.02903,1e-5),float3(0.27891,-0.03066,1e-5),float3(0.22622,-0.03044,1e-5),float3(0.18932,-0.04045,1e-5),float3(0.20219,-0.03226,0.),float3(0.30269,-0.03443,0.),float3(0.38379,-0.03023,0.),float3(0.39038,-0.0361,0.),float3(0.4631,-0.02022,0.),float3(0.44663,-0.0259,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.),float3(0.,0.,0.)),float3[32](float3(0.04118,-0.10668,0.63273),float3(0.05152,-0.02772,0.42999),float3(0.05724,-717e-5,0.24931),float3(0.05863,-421e-5,0.1304),float3(0.05952,-351e-5,0.0591),float3(0.06149,-399e-5,0.0243),float3(0.06448,-522e-5,981e-5),float3(0.07004,-676e-5,433e-5),float3(0.07774,-866e-5,2e-3),float3(0.08632,-0.01099,105e-5),float3(0.09629,-0.01332,62e-5),float3(0.10592,-0.0159,39e-5),float3(0.10718,-0.01724,24e-5),float3(0.12207,-0.02041,18e-5),float3(0.13413,-0.02237,13e-5),float3(0.13702,-0.02503,9e-5),float3(0.15294,-0.02664,8e-5),float3(0.15121,-0.02803,6e-5),float3(0.17652,-0.03188,5e-5),float3(0.19532,-0.03147,4e-5),float3(0.20831,-0.03346,3e-5),float3(0.19762,-0.03476,2e-5),float3(0.24202,-0.03464,2e-5),float3(0.32995,-0.03125,2e-5),float3(0.30857,-0.03303,2e-5),float3(0.39596,-0.03009,2e-5),float3(0.38346,-0.03198,1e-5),float3(0.42503,-0.02518,1e-5),float3(0.41592,-0.03195,1e-5),float3(0.42512,-0.01668,1e-5),float3(0.36714,-0.02978,1e-5),float3(0.46502,-394e-5,1e-5)),float3[32](float3(0.05088,-0.16006,0.67021),float3(0.06485,-0.05552,0.50797),float3(0.07697,-0.01274,0.34475),float3(0.08334,-932e-5,0.22264),float3(0.08654,-582e-5,0.133),float3(0.0895,-507e-5,0.07375),float3(0.09263,-531e-5,0.03867),float3(0.09711,-616e-5,0.02006),float3(0.10113,-792e-5,0.01032),float3(0.10913,-949e-5,57e-4),float3(0.11586,-0.01167,324e-5),float3(0.12425,-0.01421,197e-5),float3(0.12986,-0.01635,122e-5),float3(0.1353,-0.01858,8e-4),float3(0.15037,-0.02237,59e-5),float3(0.15165,-0.0245,4e-4),float3(0.15858,-0.02714,3e-4),float3(0.1666,-0.03145,22e-5),float3(0.18051,-0.03321,18e-5),float3(0.18022,-0.03295,13e-5),float3(0.19896,-0.03492,12e-5),float3(0.21095,-0.03365,9e-5),float3(0.21862,-0.03733,8e-5),float3(0.23861,-0.0393,7e-5),float3(0.25384,-0.03879,6e-5),float3(0.27394,-0.0358,5e-5),float3(0.28563,-0.04089,4e-5),float3(0.2916,-0.03604,3e-5),float3(0.293,-0.03863,3e-5),float3(0.33458,-0.03575,2e-5),float3(0.36514,-0.02621,2e-5),float3(0.38746,124e-5,2e-5)),float3[32](float3(0.06054,-0.18133,0.68765),float3(0.0771,-0.08871,0.55387),float3(0.09569,-0.028,0.41042),float3(0.10917,-0.01456,0.29613),float3(0.11813,-0.01052,0.20481),float3(0.12469,-904e-5,0.13463),float3(0.12958,-864e-5,0.08432),float3(0.13406,-898e-5,0.05135),float3(0.13801,-0.01019,0.03073),float3(0.14324,-0.0111,0.01861),float3(0.14801,-0.01354,0.01132),float3(0.15359,-0.01614,706e-5),float3(0.15945,-0.01922,454e-5),float3(0.16688,-0.02116,304e-5),float3(0.17552,-0.02363,212e-5),float3(0.17956,-0.02606,145e-5),float3(0.18275,-0.02887,102e-5),float3(0.19293,-0.0327,78e-5),float3(0.20081,-0.03492,59e-5),float3(0.20817,-0.03607,46e-5),float3(0.21658,-0.03714,36e-5),float3(0.22866,-0.03827,3e-4),float3(0.23912,-0.03832,25e-5),float3(0.24736,-0.03918,2e-4),float3(0.26573,-0.04071,17e-5),float3(0.26821,-0.04382,13e-5),float3(0.28767,-0.0386,12e-5),float3(0.30592,-0.0369,1e-4),float3(0.31228,-0.04032,8e-5),float3(0.35297,-0.03136,8e-5),float3(0.3557,-0.02365,6e-5),float3(0.37077,-281e-5,6e-5)),float3[32](float3(0.07075,-0.18042,0.69478),float3(0.08974,-0.10806,0.58263),float3(0.11306,-0.04959,0.45689),float3(0.13241,-0.02418,0.35318),float3(0.14704,-0.01632,0.26617),float3(0.15763,-0.01333,0.19342),float3(0.16591,-0.01204,0.1365),float3(0.17258,-0.01177,0.09391),float3(0.17781,-0.01198,0.06299),float3(0.18234,-0.01284,0.04183),float3(0.187,-0.01408,0.02782),float3(0.19101,-0.01571,0.01845),float3(0.19657,-0.01794,0.01259),float3(0.20165,-0.02041,864e-5),float3(0.20731,-0.02235,604e-5),float3(0.2115,-0.02516,421e-5),float3(0.21692,-0.02822,303e-5),float3(0.22536,-0.03168,228e-5),float3(0.23235,-0.03631,172e-5),float3(0.2372,-0.0384,129e-5),float3(0.24295,-0.04024,99e-5),float3(0.25154,-0.04645,79e-5),float3(0.262,-0.04435,66e-5),float3(0.26907,-0.04644,52e-5),float3(0.2804,-0.04369,44e-5),float3(0.28922,-0.05007,34e-5),float3(0.30452,-0.04809,29e-5),float3(0.31567,-0.04719,24e-5),float3(0.33294,-0.04179,21e-5),float3(0.35084,-0.03537,19e-5),float3(0.37226,-0.02633,17e-5),float3(0.37956,196e-5,13e-5)),float3[32](float3(0.08222,-0.17531,0.69794),float3(0.10394,-0.13135,0.601),float3(0.13034,-0.08092,0.49085),float3(0.15361,-0.04065,0.39802),float3(0.17244,-0.02431,0.31773),float3(0.1862,-0.01864,0.24683),float3(0.19671,-0.0162,0.18787),float3(0.20509,-0.01498,0.13964),float3(0.21199,-0.0146,0.1022),float3(0.21726,-0.01487,0.07319),float3(0.22245,-0.01549,0.05252),float3(0.22702,-0.01665,0.03747),float3(0.23174,-0.01802,0.02685),float3(0.23571,-0.01985,0.01915),float3(0.23966,-0.02185,0.01375),float3(0.24384,-0.02424,996e-5),float3(0.24877,-0.02636,733e-5),float3(0.25548,-0.02871,552e-5),float3(0.26047,-0.03133,415e-5),float3(0.26863,-0.03455,322e-5),float3(0.27404,-0.03705,247e-5),float3(0.28088,-0.03931,194e-5),float3(0.2901,-0.04546,157e-5),float3(0.29704,-0.04797,126e-5),float3(0.30559,-0.0499,102e-5),float3(0.31519,-0.04903,82e-5),float3(0.32721,-0.04842,69e-5),float3(0.33828,-0.04495,59e-5),float3(0.35424,-0.0431,48e-5),float3(0.36868,-0.03925,42e-5),float3(0.3857,-0.02709,37e-5),float3(0.39707,103e-5,3e-4)),float3[32](float3(0.09597,-0.17397,0.69862),float3(0.11972,-0.15003,0.61474),float3(0.14796,-0.10394,0.51662),float3(0.1735,-0.06278,0.43321),float3(0.19481,-0.03698,0.35989),float3(0.21136,-0.02667,0.29352),float3(0.22369,-0.02186,0.23483),float3(0.2333,-0.01962,0.18491),float3(0.24109,-0.01854,0.14298),float3(0.24787,-0.0181,0.1096),float3(0.25359,-0.01829,0.08309),float3(0.25878,-0.01882,0.06268),float3(0.2632,-0.0199,0.04689),float3(0.26831,-0.02103,0.03539),float3(0.27305,-0.02243,0.02666),float3(0.2781,-0.02425,0.02015),float3(0.28206,-0.02611,0.01519),float3(0.28734,-0.02809,0.0116),float3(0.29228,-0.03045,888e-5),float3(0.29719,-0.03211,683e-5),float3(0.30256,-0.03413,531e-5),float3(0.30857,-0.03666,417e-5),float3(0.31652,-0.03882,334e-5),float3(0.32438,-0.0415,27e-4),float3(0.33291,-0.04323,22e-4),float3(0.34012,-0.04376,178e-5),float3(0.35087,-0.0459,148e-5),float3(0.36243,-0.04591,123e-5),float3(0.37467,-0.04202,101e-5),float3(0.38986,-0.03859,87e-5),float3(0.40394,-0.03046,73e-5),float3(0.41719,25e-5,61e-5)),float3[32](float3(0.11173,-0.17686,0.6984),float3(0.13694,-0.16229,0.62456),float3(0.16797,-0.12071,0.5378),float3(0.19358,-0.08778,0.46227),float3(0.21571,-0.05746,0.39486),float3(0.23427,-0.03882,0.33234),float3(0.249,-0.03044,0.27724),float3(0.26011,-0.0262,0.22722),float3(0.26884,-0.02412,0.18345),float3(0.27668,-0.0229,0.14737),float3(0.28326,-0.02247,0.11687),float3(0.28928,-0.02235,0.09223),float3(0.29445,-0.02283,0.07219),float3(0.29932,-0.02349,0.05639),float3(0.30454,-0.02444,0.04424),float3(0.30943,-0.02562,0.0346),float3(0.31431,-0.02725,0.02709),float3(0.31861,-0.02859,0.02113),float3(0.32326,-0.03047,0.01656),float3(0.32881,-0.03199,0.01309),float3(0.33479,-0.03417,0.01041),float3(0.34094,-0.03618,831e-5),float3(0.34705,-0.03771,665e-5),float3(0.35341,-0.0392,534e-5),float3(0.36079,-0.0407,434e-5),float3(0.36863,-0.04138,355e-5),float3(0.37512,-0.04066,288e-5),float3(0.38607,-0.04125,241e-5),float3(0.39611,-0.03916,2e-3),float3(0.41085,-0.03764,17e-4),float3(0.42341,-0.02956,142e-5),float3(0.43959,2e-4,123e-5)),float3[32](float3(0.12869,-0.17952,0.69766),float3(0.15653,-0.16921,0.63215),float3(0.18847,-0.13634,0.55391),float3(0.21397,-0.10854,0.48615),float3(0.23632,-0.08164,0.42355),float3(0.25652,-0.05663,0.36707),float3(0.27273,-0.04256,0.31348),float3(0.28564,-0.03522,0.26549),float3(0.29568,-0.03124,0.22185),float3(0.30413,-0.02895,0.18403),float3(0.31142,-0.0276,0.15141),float3(0.31801,-0.02688,0.1238),float3(0.32424,-0.02653,0.10104),float3(0.32941,-0.02669,0.08149),float3(0.33479,-0.02714,0.06598),float3(0.33933,-0.02784,0.05291),float3(0.34444,-0.02878,0.04275),float3(0.34892,-0.02988,0.03428),float3(0.35415,-0.03136,0.02771),float3(0.35873,-0.03261,0.02222),float3(0.36457,-0.03429,0.01807),float3(0.36975,-0.03567,0.01456),float3(0.37684,-0.03742,0.01195),float3(0.38258,-0.03792,969e-5),float3(0.39038,-0.03954,798e-5),float3(0.39755,-0.04002,653e-5),float3(0.40428,-0.04014,534e-5),float3(0.41192,-0.03889,441e-5),float3(0.42141,-0.03739,368e-5),float3(0.43074,-0.03386,307e-5),float3(0.44659,-0.0281,264e-5),float3(0.46013,13e-5,224e-5)),float3[32](float3(0.14693,-0.17953,0.69641),float3(0.17828,-0.17294,0.63746),float3(0.20991,-0.14837,0.56861),float3(0.23513,-0.12458,0.50592),float3(0.25809,-0.10127,0.44888),float3(0.2785,-0.07912,0.39646),float3(0.29576,-0.05884,0.34628),float3(0.30999,-0.04715,0.30001),float3(0.32128,-0.04041,0.25727),float3(0.33053,-0.03637,0.21872),float3(0.33854,-0.03401,0.1853),float3(0.34549,-0.03244,0.15569),float3(0.35212,-0.03165,0.13073),float3(0.35806,-0.03113,0.10901),float3(0.36371,-0.03104,0.09059),float3(0.36901,-0.03134,0.07509),float3(0.37416,-0.03173,0.06205),float3(0.37907,-0.03225,0.05112),float3(0.38378,-0.03304,0.04199),float3(0.38887,-0.03406,0.03458),float3(0.39366,-0.03515,0.02839),float3(0.39953,-0.03594,0.02356),float3(0.40534,-0.03728,0.01946),float3(0.41134,-0.03825,0.01604),float3(0.41832,-0.0391,0.01337),float3(0.42583,-0.0396,0.01115),float3(0.43323,-0.0395,925e-5),float3(0.44084,-0.03877,766e-5),float3(0.44897,-0.03647,639e-5),float3(0.45832,-0.03283,536e-5),float3(0.47095,-0.02659,456e-5),float3(0.4834,-2e-5,39e-4)),float3[32](float3(0.16755,-0.17802,0.69548),float3(0.20139,-0.17499,0.64297),float3(0.23191,-0.15708,0.57917),float3(0.25781,-0.13619,0.52314),float3(0.28103,-0.11601,0.47074),float3(0.30124,-0.09763,0.42115),float3(0.31874,-0.07926,0.37424),float3(0.33379,-0.06262,0.33015),float3(0.34648,-0.05277,0.29017),float3(0.35662,-0.04616,0.25188),float3(0.36536,-0.04217,0.21825),float3(0.37289,-0.03952,0.18762),float3(0.37956,-0.03771,0.16043),float3(0.38585,-0.03664,0.13705),float3(0.39167,-0.03576,0.11638),float3(0.39736,-0.03561,0.09898),float3(0.40272,-0.03531,0.0836),float3(0.40787,-0.03537,0.0705),float3(0.41311,-0.03577,0.05948),float3(0.41811,-0.03606,0.04991),float3(0.42324,-0.03657,0.04182),float3(0.42871,-0.03725,0.03523),float3(0.43459,-0.0379,0.02974),float3(0.44015,-0.0383,0.02485),float3(0.44634,-0.0385,0.02085),float3(0.45322,-0.03897,0.01764),float3(0.46073,-0.03886,0.01489),float3(0.46815,-0.03764,0.01243),float3(0.47699,-0.03576,0.01052),float3(0.48579,-0.03199,882e-5),float3(0.49728,-0.02678,762e-5),float3(0.50776,-3e-5,638e-5)),float3[32](float3(0.19101,-0.1757,0.69518),float3(0.22471,-0.1763,0.64677),float3(0.25559,-0.16233,0.58896),float3(0.28173,-0.14459,0.53726),float3(0.30495,-0.12739,0.48924),float3(0.32541,-0.11162,0.44375),float3(0.34337,-0.09667,0.40045),float3(0.35848,-0.08188,0.35844),float3(0.37152,-0.06821,0.31901),float3(0.38267,-0.05887,0.28296),float3(0.39205,-0.0524,0.24905),float3(0.40013,-0.04793,0.21813),float3(0.40731,-0.04499,0.19041),float3(0.41385,-0.043,0.16557),float3(0.41996,-0.04151,0.1436),float3(0.42572,-0.04033,0.12396),float3(0.43136,-0.03975,0.10719),float3(0.43675,-0.03947,0.0924),float3(0.4421,-0.03943,0.07951),float3(0.44729,-0.03917,0.06811),float3(0.45254,-0.03889,0.05809),float3(0.45786,-0.03887,0.0496),float3(0.46336,-0.03909,0.04242),float3(0.46904,-0.03903,0.03607),float3(0.47514,-0.03922,0.03098),float3(0.48162,-0.03917,0.0265),float3(0.48829,-0.03827,0.02249),float3(0.49548,-0.03702,0.01911),float3(0.50339,-0.03465,0.0163),float3(0.51184,-0.03118,0.01385),float3(0.5215,-0.02485,0.01187),float3(0.53151,-21e-5,0.01004)),float3[32](float3(0.21608,-0.17314,0.69404),float3(0.2491,-0.17599,0.65077),float3(0.28069,-0.16514,0.59738),float3(0.30724,-0.15021,0.55038),float3(0.33032,-0.13557,0.50616),float3(0.35096,-0.12161,0.46415),float3(0.36879,-0.10876,0.4226),float3(0.38457,-0.09665,0.38393),float3(0.39755,-0.08454,0.34548),float3(0.40917,-0.07412,0.3109),float3(0.41915,-0.06463,0.27757),float3(0.4279,-0.05848,0.24745),float3(0.43555,-0.05371,0.21932),float3(0.44253,-0.05046,0.194),float3(0.44886,-0.04821,0.17119),float3(0.45491,-0.04588,0.15001),float3(0.46058,-0.04438,0.13153),float3(0.4659,-0.04412,0.11577),float3(0.47137,-0.04304,0.10099),float3(0.4767,-0.04215,0.08803),float3(0.48207,-0.04161,0.07655),float3(0.4873,-0.04144,0.06667),float3(0.49275,-0.04059,0.05763),float3(0.49832,-0.04032,0.05),float3(0.50411,-0.03974,0.04326),float3(0.51004,-0.03905,0.03745),float3(0.51646,-0.038,0.03233),float3(0.5232,-0.03639,0.02793),float3(0.53028,-0.03436,0.0241),float3(0.53801,-0.03031,0.0207),float3(0.54612,-0.02493,0.01799),float3(0.5554,4e-5,0.01521)),float3[32](float3(0.24162,-0.17052,0.69373),float3(0.27454,-0.17439,0.6537),float3(0.30746,-0.16579,0.60556),float3(0.33408,-0.15358,0.56229),float3(0.35697,-0.14112,0.52099),float3(0.37775,-0.12867,0.4822),float3(0.39587,-0.11735,0.44396),float3(0.41165,-0.107,0.40686),float3(0.42539,-0.09729,0.37145),float3(0.43716,-0.08817,0.33754),float3(0.44735,-0.07945,0.30538),float3(0.45639,-0.07137,0.27528),float3(0.46453,-0.0639,0.24688),float3(0.47188,-0.05902,0.22137),float3(0.4784,-0.05627,0.19867),float3(0.48473,-0.05274,0.17677),float3(0.4906,-0.05014,0.15718),float3(0.49627,-0.04809,0.13948),float3(0.50156,-0.04704,0.12389),float3(0.50672,-0.04617,0.10988),float3(0.51205,-0.04471,0.09688),float3(0.5172,-0.04402,0.0856),float3(0.52258,-0.04284,0.07528),float3(0.52812,-0.0414,0.06603),float3(0.53331,-0.04084,0.05819),float3(0.53939,-0.03922,0.05082),float3(0.54508,-0.03852,0.04467),float3(0.5511,-0.03648,0.03901),float3(0.55773,-0.03362,0.03404),float3(0.56432,-0.02977,0.02969),float3(0.57204,-0.02257,0.02565),float3(0.58012,13e-5,0.02208)),float3[32](float3(0.26781,-0.16726,0.69374),float3(0.30184,-0.1713,0.65782),float3(0.33546,-0.16494,0.613),float3(0.36236,-0.15479,0.5735),float3(0.38543,-0.14389,0.5356),float3(0.40587,-0.13313,0.49892),float3(0.42394,-0.12315,0.46321),float3(0.4398,-0.11396,0.4281),float3(0.45355,-0.10555,0.39357),float3(0.466,-0.09751,0.36179),float3(0.47658,-0.08994,0.33055),float3(0.48605,-0.08303,0.30167),float3(0.49437,-0.07661,0.27438),float3(0.50188,-0.07012,0.24851),float3(0.50886,-0.06435,0.22461),float3(0.51521,-0.06079,0.20331),float3(0.52114,-0.05751,0.18338),float3(0.52686,-0.0549,0.16512),float3(0.53234,-0.05237,0.14824),float3(0.5376,-0.05016,0.13281),float3(0.54305,-0.04763,0.1185),float3(0.54802,-0.04636,0.10604),float3(0.55334,-0.04452,0.09447),float3(0.5585,-0.04311,0.08417),float3(0.5635,-0.04186,0.07499),float3(0.56858,-0.04059,0.06675),float3(0.57426,-0.03828,0.05906),float3(0.57961,-0.03642,0.0524),float3(0.58579,-0.03293,0.04618),float3(0.59207,-0.02837,0.04061),float3(0.59839,-0.02183,0.03571),float3(0.60588,14e-5,0.03099)),float3[32](float3(0.29552,-0.16307,0.69478),float3(0.33034,-0.16735,0.66097),float3(0.36464,-0.16269,0.6201),float3(0.39162,-0.15439,0.58318),float3(0.41468,-0.14491,0.54812),float3(0.43479,-0.13568,0.51374),float3(0.45273,-0.12677,0.48033),float3(0.46884,-0.11827,0.44784),float3(0.48278,-0.11077,0.41543),float3(0.49535,-0.10366,0.38456),float3(0.50635,-0.09707,0.35481),float3(0.51611,-0.09088,0.32656),float3(0.52482,-0.08505,0.29983),float3(0.53261,-0.0792,0.27402),float3(0.53981,-0.07391,0.25049),float3(0.54631,-0.06982,0.22916),float3(0.55239,-0.06584,0.20899),float3(0.55818,-0.06111,0.18974),float3(0.56348,-0.05885,0.17283),float3(0.56875,-0.0556,0.15672),float3(0.57406,-0.05216,0.14161),float3(0.57898,-0.04974,0.12804),float3(0.58395,-0.04738,0.11552),float3(0.5889,-0.04525,0.10412),float3(0.59356,-0.04361,0.09392),float3(0.59866,-0.04122,0.08435),float3(0.60366,-0.03869,0.07566),float3(0.60857,-0.03626,0.06785),float3(0.61392,-0.03267,0.06058),float3(0.61896,-0.02853,0.05418),float3(0.62446,-0.02146,0.04812),float3(0.63114,13e-5,0.04224)),float3[32](float3(0.32493,-0.158,0.69636),float3(0.36044,-0.16239,0.66479),float3(0.395,-0.15918,0.62662),float3(0.42214,-0.15229,0.59208),float3(0.44504,-0.14422,0.55948),float3(0.46513,-0.13589,0.52832),float3(0.4826,-0.12801,0.49686),float3(0.49819,-0.12058,0.46577),float3(0.5122,-0.11368,0.4355),float3(0.52491,-0.10715,0.4064),float3(0.53607,-0.10121,0.37791),float3(0.54604,-0.09561,0.35063),float3(0.55497,-0.09044,0.3244),float3(0.56304,-0.08553,0.29963),float3(0.57042,-0.08073,0.27592),float3(0.57716,-0.07643,0.2543),float3(0.58338,-0.07244,0.23411),float3(0.58922,-0.06816,0.21474),float3(0.59463,-0.06506,0.19748),float3(0.59983,-0.06156,0.18085),float3(0.6051,-0.05721,0.16496),float3(0.60997,-0.05394,0.15071),float3(0.61473,-0.05071,0.13745),float3(0.61957,-0.04764,0.12513),float3(0.62409,-0.04512,0.11401),float3(0.62852,-0.04243,0.10365),float3(0.63251,-0.04049,0.09444),float3(0.6375,-0.03634,0.08526),float3(0.64231,-0.03217,0.07697),float3(0.64613,-0.02798,0.06969),float3(0.65086,-0.02084,0.06274),float3(0.65651,-2e-5,0.05589)),float3[32](float3(0.35603,-0.15221,0.69769),float3(0.39159,-0.15659,0.66832),float3(0.42682,-0.15414,0.63306),float3(0.4544,-0.14815,0.60175),float3(0.47706,-0.14135,0.57114),float3(0.49652,-0.1343,0.5413),float3(0.5137,-0.1272,0.51227),float3(0.52869,-0.12066,0.48316),float3(0.54234,-0.11418,0.4552),float3(0.55442,-0.10835,0.42705),float3(0.56541,-0.10297,0.39979),float3(0.5752,-0.0981,0.37306),float3(0.58426,-0.09325,0.34799),float3(0.59238,-0.08885,0.32368),float3(0.59985,-0.08462,0.30051),float3(0.60666,-0.08047,0.27914),float3(0.61307,-0.07647,0.25902),float3(0.6189,-0.07278,0.24017),float3(0.62438,-0.06918,0.22242),float3(0.62953,-0.06581,0.20569),float3(0.63458,-0.06234,0.1895),float3(0.63958,-0.05873,0.17436),float3(0.64422,-0.05531,0.16053),float3(0.64882,-0.05182,0.14758),float3(0.65331,-0.04822,0.13555),float3(0.65733,-0.04527,0.12469),float3(0.6615,-0.0416,0.11431),float3(0.66582,-0.03748,0.10458),float3(0.6699,-0.0327,0.09545),float3(0.67332,-0.02788,0.08731),float3(0.67675,-0.02071,0.07957),float3(0.68186,2e-5,0.07179)),float3[32](float3(0.38874,-0.14547,0.6999),float3(0.42458,-0.14938,0.67322),float3(0.46012,-0.14757,0.63986),float3(0.48761,-0.14257,0.61072),float3(0.51008,-0.13671,0.5821),float3(0.52938,-0.13046,0.55452),float3(0.54611,-0.12415,0.52757),float3(0.5605,-0.11845,0.49993),float3(0.57333,-0.11284,0.47308),float3(0.5847,-0.10773,0.44623),float3(0.59503,-0.10264,0.42075),float3(0.60417,-0.09831,0.39479),float3(0.61278,-0.09386,0.37078),float3(0.62064,-0.08976,0.34736),float3(0.62794,-0.08593,0.32486),float3(0.63473,-0.08203,0.30393),float3(0.64102,-0.07852,0.28339),float3(0.64684,-0.07474,0.26497),float3(0.65232,-0.07134,0.2468),float3(0.6575,-0.06788,0.23017),float3(0.66247,-0.06462,0.21383),float3(0.6673,-0.06134,0.19836),float3(0.67176,-0.05808,0.18437),float3(0.6759,-0.05475,0.17126),float3(0.6804,-0.05106,0.15847),float3(0.68408,-0.04764,0.1473),float3(0.68813,-0.04367,0.13611),float3(0.69178,-0.03955,0.12594),float3(0.69592,-0.03412,0.11582),float3(0.69962,-0.02813,0.10669),float3(0.70287,-0.02016,0.09818),float3(0.70573,6e-5,0.09005)),float3[32](float3(0.42286,-0.13764,0.70253),float3(0.45893,-0.14091,0.67808),float3(0.4945,-0.13944,0.64711),float3(0.52171,-0.13527,0.61953),float3(0.54404,-0.13016,0.59306),float3(0.56282,-0.12479,0.5671),float3(0.57891,-0.11949,0.54128),float3(0.5929,-0.11426,0.51588),float3(0.60505,-0.10951,0.48993),float3(0.61587,-0.1047,0.46536),float3(0.62546,-0.10029,0.44072),float3(0.63399,-0.09638,0.41598),float3(0.64186,-0.09229,0.39289),float3(0.64911,-0.0888,0.36972),float3(0.65567,-0.08519,0.34812),float3(0.66186,-0.08157,0.32779),float3(0.66775,-0.07846,0.30734),float3(0.67323,-0.07504,0.28886),float3(0.67844,-0.07174,0.27113),float3(0.68333,-0.06833,0.25452),float3(0.68809,-0.06521,0.23828),float3(0.69262,-0.06204,0.22301),float3(0.69697,-0.05878,0.20869),float3(0.70127,-0.05553,0.19491),float3(0.70529,-0.05205,0.18216),float3(0.70896,-0.04825,0.17048),float3(0.71278,-0.04429,0.15888),float3(0.71635,-0.03988,0.14822),float3(0.71997,-0.03495,0.13785),float3(0.72323,-0.029,0.12826),float3(0.72673,-0.02079,0.11891),float3(0.72944,4e-5,0.11016)),float3[32](float3(0.45873,-0.12835,0.70614),float3(0.49433,-0.131,0.68304),float3(0.52949,-0.12981,0.654),float3(0.55633,-0.12616,0.6285),float3(0.57809,-0.12174,0.60424),float3(0.59634,-0.11714,0.57985),float3(0.6118,-0.11273,0.55495),float3(0.62517,-0.10827,0.53092),float3(0.63669,-0.10407,0.50671),float3(0.64685,-0.09994,0.48305),float3(0.65584,-0.09624,0.45902),float3(0.66376,-0.09244,0.43645),float3(0.67113,-0.08921,0.41315),float3(0.67765,-0.08575,0.39173),float3(0.6837,-0.08256,0.37068),float3(0.6893,-0.07941,0.35059),float3(0.69443,-0.07619,0.33162),float3(0.69947,-0.07326,0.31286),float3(0.70415,-0.07019,0.29534),float3(0.70855,-0.06708,0.27878),float3(0.71287,-0.06409,0.26263),float3(0.71661,-0.0608,0.24796),float3(0.72066,-0.05775,0.23332),float3(0.72451,-0.05456,0.21962),float3(0.72821,-0.05117,0.20663),float3(0.73152,-0.04735,0.19465),float3(0.73501,-0.04351,0.18279),float3(0.73849,-0.03929,0.1716),float3(0.7416,-0.03433,0.16118),float3(0.74481,-0.02845,0.15104),float3(0.74776,-0.02023,0.14164),float3(0.75105,6e-5,0.13208)),float3[32](float3(0.49554,-0.11754,0.70978),float3(0.53027,-0.11957,0.68862),float3(0.56475,-0.11844,0.66139),float3(0.59075,-0.11545,0.63717),float3(0.6117,-0.11182,0.61414),float3(0.62919,-0.10788,0.59143),float3(0.64398,-0.1041,0.56846),float3(0.65664,-0.10044,0.54542),float3(0.66758,-0.09697,0.52211),float3(0.677,-0.09338,0.49989),float3(0.68535,-0.09011,0.47731),float3(0.69283,-0.08701,0.45504),float3(0.69943,-0.08398,0.43352),float3(0.70556,-0.08119,0.41218),float3(0.711,-0.07822,0.39227),float3(0.71624,-0.07555,0.37245),float3(0.72083,-0.07263,0.35421),float3(0.72523,-0.06975,0.33639),float3(0.72925,-0.06691,0.31941),float3(0.73339,-0.06419,0.30275),float3(0.73715,-0.0613,0.28712),float3(0.7407,-0.05852,0.27199),float3(0.74402,-0.05539,0.25799),float3(0.74744,-0.05231,0.24427),float3(0.75049,-0.04891,0.23147),float3(0.75399,-0.04558,0.21879),float3(0.75685,-0.04169,0.20716),float3(0.75973,-0.03736,0.19611),float3(0.76249,-0.03256,0.18534),float3(0.76538,-0.02662,0.17518),float3(0.76785,-0.01865,0.16553),float3(0.77115,5e-5,0.15588)),float3[32](float3(0.53272,-0.10493,0.71478),float3(0.5661,-0.10643,0.69461),float3(0.59929,-0.10534,0.66938),float3(0.62423,-0.10298,0.64618),float3(0.6443,-0.10006,0.62412),float3(0.66087,-0.09686,0.60285),float3(0.6749,-0.09377,0.58132),float3(0.68693,-0.09079,0.5591),float3(0.69725,-0.08792,0.53711),float3(0.70612,-0.08524,0.51494),float3(0.71373,-0.08232,0.49425),float3(0.72045,-0.0797,0.47333),float3(0.72654,-0.07708,0.4529),float3(0.73215,-0.07461,0.43265),float3(0.73731,-0.07219,0.41291),float3(0.74188,-0.06976,0.3941),float3(0.74597,-0.06728,0.37624),float3(0.74997,-0.0649,0.35873),float3(0.75337,-0.0623,0.34249),float3(0.75709,-0.05978,0.32632),float3(0.7606,-0.0572,0.31088),float3(0.76378,-0.05462,0.29607),float3(0.76659,-0.0518,0.2823),float3(0.76955,-0.04891,0.26889),float3(0.7723,-0.04571,0.25622),float3(0.77514,-0.04252,0.24377),float3(0.77793,-0.03908,0.23184),float3(0.78043,-0.03509,0.22064),float3(0.78282,-0.03041,0.21005),float3(0.78468,-0.02457,0.20018),float3(0.78754,-0.01737,0.19021),float3(0.78995,6e-5,0.18089)),float3[32](float3(0.56932,-0.09075,0.71955),float3(0.60103,-0.09168,0.7007),float3(0.63267,-0.09087,0.67652),float3(0.65651,-0.08897,0.65469),float3(0.67543,-0.08664,0.63411),float3(0.69104,-0.0842,0.61379),float3(0.70419,-0.08174,0.59365),float3(0.7155,-0.07948,0.57227),float3(0.72506,-0.0771,0.55184),float3(0.73326,-0.07491,0.53105),float3(0.74047,-0.07278,0.51035),float3(0.74675,-0.07074,0.49002),float3(0.7523,-0.06867,0.47031),float3(0.75728,-0.06656,0.45135),float3(0.76209,-0.06453,0.4325),float3(0.76616,-0.06249,0.41451),float3(0.7699,-0.06046,0.39705),float3(0.77334,-0.05835,0.38038),float3(0.7765,-0.05612,0.36453),float3(0.77934,-0.0539,0.34929),float3(0.78214,-0.05159,0.33447),float3(0.78495,-0.04919,0.32031),float3(0.78732,-0.0466,0.30694),float3(0.7898,-0.04409,0.29376),float3(0.79245,-0.04133,0.28119),float3(0.79502,-0.03835,0.26903),float3(0.79706,-0.035,0.25765),float3(0.79938,-0.03151,0.24642),float3(0.80116,-0.02721,0.23605),float3(0.80364,-0.02228,0.2258),float3(0.80529,-0.01553,0.2163),float3(0.80743,8e-5,0.20694)),float3[32](float3(0.60446,-0.07481,0.72541),float3(0.63433,-0.0754,0.70789),float3(0.66433,-0.07476,0.68481),float3(0.68668,-0.07333,0.66418),float3(0.70457,-0.07166,0.64427),float3(0.71943,-0.06986,0.6248),float3(0.73164,-0.06808,0.6053),float3(0.742,-0.06634,0.58563),float3(0.75088,-0.06465,0.56577),float3(0.75865,-0.06301,0.54574),float3(0.76531,-0.0614,0.52594),float3(0.77096,-0.05975,0.50704),float3(0.77604,-0.05817,0.48812),float3(0.78047,-0.05654,0.46984),float3(0.78462,-0.05499,0.45179),float3(0.78846,-0.05337,0.4344),float3(0.79171,-0.05168,0.41775),float3(0.79502,-0.04998,0.40149),float3(0.79763,-0.04815,0.38649),float3(0.80039,-0.04639,0.37146),float3(0.80272,-0.04448,0.35737),float3(0.80522,-0.04252,0.34354),float3(0.80755,-0.04044,0.3302),float3(0.80954,-0.03811,0.31798),float3(0.81129,-0.03571,0.30606),float3(0.81256,-0.03305,0.29475),float3(0.81479,-0.03031,0.2833),float3(0.81667,-0.02709,0.27265),float3(0.81865,-0.0235,0.26226),float3(0.82066,-0.01932,0.25205),float3(0.82209,-0.01368,0.24262),float3(0.82367,2e-5,0.23366)),float3[32](float3(0.63783,-0.05754,0.7311),float3(0.6658,-0.05784,0.7145),float3(0.69379,-0.05736,0.69314),float3(0.71493,-0.05642,0.67318),float3(0.73184,-0.05529,0.65409),float3(0.74566,-0.0541,0.63543),float3(0.75715,-0.0529,0.61665),float3(0.76684,-0.05171,0.59776),float3(0.77502,-0.05055,0.57875),float3(0.78192,-0.04938,0.56005),float3(0.78802,-0.04824,0.54142),float3(0.79346,-0.04716,0.52249),float3(0.79805,-0.04606,0.50438),float3(0.80183,-0.0448,0.48754),float3(0.80557,-0.04364,0.4704),float3(0.809,-0.04245,0.45369),float3(0.81218,-0.04128,0.43725),float3(0.81485,-0.03999,0.42211),float3(0.81744,-0.03873,0.40702),float3(0.81962,-0.03727,0.39305),float3(0.82179,-0.0358,0.37945),float3(0.82385,-0.03429,0.3662),float3(0.82598,-0.03268,0.35338),float3(0.82778,-0.03089,0.34141),float3(0.8291,-0.02898,0.33004),float3(0.83055,-0.02688,0.31907),float3(0.83184,-0.02463,0.30846),float3(0.83322,-0.02206,0.29842),float3(0.83509,-0.01925,0.28818),float3(0.83626,-0.01574,0.27866),float3(0.83803,-0.01128,0.26919),float3(0.83936,-1e-5,0.26038)),float3[32](float3(0.66881,-0.03902,0.7375),float3(0.69495,-0.03917,0.72144),float3(0.72127,-0.0389,0.70116),float3(0.74089,-0.03836,0.68204),float3(0.75676,-0.03768,0.66379),float3(0.76971,-0.03701,0.64562),float3(0.78031,-0.03631,0.62792),float3(0.78909,-0.03559,0.61014),float3(0.79688,-0.03488,0.59193),float3(0.80339,-0.03421,0.57344),float3(0.80909,-0.03354,0.55536),float3(0.81375,-0.03284,0.53793),float3(0.81807,-0.03212,0.52063),float3(0.82174,-0.0314,0.50374),float3(0.82535,-0.03066,0.48732),float3(0.82861,-0.02992,0.47141),float3(0.83083,-0.02915,0.45633),float3(0.8332,-0.02828,0.44181),float3(0.83559,-0.02745,0.4273),float3(0.83745,-0.02649,0.41385),float3(0.84005,-0.02553,0.40059),float3(0.8407,-0.0244,0.38881),float3(0.84245,-0.02332,0.37643),float3(0.84401,-0.0221,0.36506),float3(0.84503,-0.02076,0.35431),float3(0.84665,-0.0194,0.34339),float3(0.84777,-0.01782,0.33324),float3(0.84905,-0.01601,0.32345),float3(0.84999,-0.0139,0.31425),float3(0.85072,-0.01143,0.30523),float3(0.85236,-812e-5,0.29632),float3(0.85341,2e-5,0.28782)),float3[32](float3(0.6974,-0.01972,0.74339),float3(0.72178,-0.0198,0.72804),float3(0.7463,-0.01969,0.70892),float3(0.76475,-0.01946,0.69082),float3(0.77931,-0.01916,0.67367),float3(0.79173,-0.01888,0.65573),float3(0.80171,-0.0186,0.63826),float3(0.81003,-0.01833,0.6209),float3(0.81725,-0.01802,0.60355),float3(0.82283,-0.01768,0.58677),float3(0.82781,-0.01737,0.56971),float3(0.83251,-0.01712,0.55229),float3(0.83628,-0.01681,0.53567),float3(0.83983,-0.01645,0.5197),float3(0.84284,-0.0161,0.50407),float3(0.84559,-0.01577,0.48879),float3(0.8479,-0.01538,0.47446),float3(0.85013,-0.01498,0.46044),float3(0.85214,-0.01455,0.4471),float3(0.85384,-0.01407,0.43403),float3(0.85537,-0.01356,0.42174),float3(0.85665,-0.01304,0.41005),float3(0.8586,-0.01251,0.39844),float3(0.85961,-0.01189,0.38755),float3(0.86039,-0.01119,0.37756),float3(0.86098,-0.0104,0.36805),float3(0.86192,-953e-5,0.35846),float3(0.86278,-857e-5,0.34911),float3(0.86425,-751e-5,0.33962),float3(0.86491,-614e-5,0.33144),float3(0.86581,-433e-5,0.323),float3(0.86677,2e-5,0.31508)),float3[32](float3(0.72363,0.,0.74973),float3(0.74617,-2e-5,0.73527),float3(0.76908,0.,0.71668),float3(0.78618,-2e-5,0.69953),float3(0.79981,-3e-5,0.68291),float3(0.8111,-1e-5,0.66623),float3(0.8206,-1e-5,0.6494),float3(0.82853,-4e-5,0.63212),float3(0.83504,-2e-5,0.61545),float3(0.84058,-3e-5,0.59849),float3(0.84521,-5e-5,0.58226),float3(0.84932,-2e-5,0.56605),float3(0.85275,-0.,0.55034),float3(0.85605,-1e-5,0.53461),float3(0.85877,-1e-5,0.51974),float3(0.86129,-1e-5,0.50561),float3(0.8632,-5e-5,0.49191),float3(0.86521,-4e-5,0.47875),float3(0.86654,-2e-5,0.46632),float3(0.86846,-2e-5,0.45401),float3(0.86997,-1e-5,0.44209),float3(0.87153,3e-5,0.43044),float3(0.87288,2e-5,0.42005),float3(0.87352,3e-5,0.4099),float3(0.87465,4e-5,0.39997),float3(0.87549,3e-5,0.39069),float3(0.87626,6e-5,0.38171),float3(0.87676,4e-5,0.37342),float3(0.87714,6e-5,0.36523),float3(0.87859,6e-5,0.35675),float3(0.87952,9e-5,0.34897),float3(0.87958,3e-5,0.34187)));

/// The cosine-weighted average of the `R` column of `SHEEN_LTC_TABLE` per
/// roughness row, for multiscatter energy compensation.
static const auto SHEEN_LTC_AVERAGE_R=float[32](0.,0.,26e-5,149e-5,379e-5,706e-5,0.01137,0.01684,0.02359,0.03168,0.04118,0.05208,0.06438,0.07798,0.09297,0.10917,0.12663,0.14516,0.16462,0.18485,0.20591,0.22761,0.24971,0.27224,0.2948,0.31728,0.33982,0.36221,0.38402,0.40553,0.42656,0.44686); /// Bilinearly interpolate the sheen LTC table by roughness and view
/// cosine, returning `(aInv, bInv, R)`.
@(pure)
float3 sheenLTCFetch(const float roughness,const float mu){
float rowF(saturate(roughness)*31);
float colF(saturate(mu)*31);
const int ri(#min(int(rowF),30));
const int ci(#min(int(colF),30));
rowF-=ri;
colF-=ci;
return lerp(lerp(SHEEN_LTC_TABLE[ri+0][ci+0],SHEEN_LTC_TABLE[ri+0][ci+1],colF),lerp(SHEEN_LTC_TABLE[ri+1][ci+0],SHEEN_LTC_TABLE[ri+1][ci+1],colF),rowF);
}

/// Evaluate the sheen LTC lobe: the normalized density over `wiStd` given
/// the coefficients fetched for the view direction, with `wiStd` rotated
/// into the frame where the view lies in the xz-plane. Includes the cosine
/// of the incoming direction by construction.
@(pure)
float sheenLTCEval(const float3 wiStd,const float aInv,const float bInv){
const float3 wiOrig(float3(aInv*wiStd.x+bInv*wiStd.z,aInv*wiStd.y,wiStd.z));
const float len2(#sum(wiOrig*wiOrig));
return #max(wiOrig.z,0.)/$PI*(aInv*aInv)/(len2*len2+EPSILON);
}

/// The sheen BSDF for fabric-like grazing highlights, using the
/// multiple-scattering LTC sheen of Zeltner, Burley, and Chiang.
export struct sheen_bsdf:bsdf{
/// The roughness.
///
/// > Roughness coefficient. Range: `[0,inf)`, with `0` specifying pure
/// > specular reflection.
///
float roughness;

/// The tint.
///
/// > Scaling factor, defined as a color, multiplied by the
/// > result of the distribution function.
///
const $(color|float) tint=1.;

/// The multiscatter tint.
///
/// > Scaling factor, defined as a color, of the diffuse multiscattering
/// > compensation, `color(0.0)` does not add any, `color(1.0)` fully
/// > compensates the energy loss.
///
const $(?(color|float)) multiscatter_tint=none;

/// The multiscatter lobe.
///
/// Currently unused, part of the later MDL spec?
///
void multiscatter=none;

/// The handle.
void handle="";

/// The flags.
static const int df_flags=DF_REFLECTION|DF_DIFFUSE;
finalize {
roughness=saturate(roughness);
}
};
@(pure)
auto scatterEvaluate(const &sheen_bsdf this,inline const &ScatterEvaluateParameters params){
if(mode==scatter_reflect&&recalculateTangentSpace(params)){
const auto cosThetao(#abs(wo.z));
const auto cosThetai(#abs(wi.z));
const auto coO(sheenLTCFetch(this.roughness,cosThetao));
const auto coI(sheenLTCFetch(this.roughness,cosThetai));
const float2 eo(azimuthFrame(wo));
const float2 ei(azimuthFrame(wi));
const float3 wiStd(eo.x*wi.x+eo.y*wi.y,-eo.y*wi.x+eo.x*wi.y,wi.z);
const float3 woStd(ei.x*wo.x+ei.y*wo.y,-ei.y*wo.x+ei.x*wo.y,wo.z);
const float DfromO(sheenLTCEval(wiStd,coO.x,coO.y));
const float DfromI(sheenLTCEval(woStd,coI.x,coI.y));
const float f(0.5*(coO.z*DfromO+coI.z*DfromI*cosThetai/(cosThetao+EPSILON)));
const float2 pdf(float2(coO.x>EPSILON?DfromO:cosThetai/$PI,coI.x>EPSILON?DfromI:cosThetao/$PI));
if(#typeof(this.multiscatter_tint)==void||(#typeof(this.multiscatter_tint)==float&&this.multiscatter_tint==0.)){
auto result(ScatterEvaluateResult(f: this.tint*f,pdf: pdf));
result.f*=shadingNormalCorrection if(isImportance);
return result;
} else {
float rowF(saturate(this.roughness)*31);
const int ri(#min(int(rowF),30));
rowF-=ri;
const float avgR(lerp(SHEEN_LTC_AVERAGE_R[ri],SHEEN_LTC_AVERAGE_R[ri+1],rowF));
const auto msF(cosThetai/$PI*#max(1-coO.z,0.)*#max(1-coI.z,0.)/#max(1-avgR,1e-4));
const auto msPdf(float2(cosThetai,cosThetao)/$PI);
auto result(ScatterEvaluateResult(f: this.tint*(f+this.multiscatter_tint*msF),pdf: lerp(pdf,msPdf,MULTISCATTER_DIFFUSE_CHANCE)));
result.f*=shadingNormalCorrection if(isImportance);
return result;
}
} else {
return ScatterEvaluateResult(isBlack: true);
}
}
@(pure)
auto scatterSample(const &sheen_bsdf this,inline const &ScatterSampleParameters params){
if((tbn:=recalculateTangentSpace(params))){
if(result:=ScatterSampleResultWithMultiscatter(this,&xi,*tbn)){
return *result;
}
const auto co(sheenLTCFetch(this.roughness,wo.z));
if(co.x>EPSILON){
const float3 wiOrig(monte_carlo::cosineHemisphereSample(xi.xy));
const float3 wiStd(normalize(float3((wiOrig.x-wiOrig.z*co.y)/co.x,wiOrig.y/co.x,wiOrig.z)));
const float2 e(azimuthFrame(wo));
const float3 wiLocal(float3(e.x*wiStd.x-e.y*wiStd.y,e.y*wiStd.x+e.x*wiStd.y,wiStd.z));
return ScatterSampleResult(wi: (*tbn)*wiLocal,mode: scatter_reflect);
}
return ScatterSampleResult(wi: (*tbn)*monte_carlo::cosineHemisphereSample(xi.xy),mode: scatter_reflect);
} else {
return ScatterSampleResult();
}
} /// The anisotropic glossy reflection of the Ward BSDF in the
/// bounded-albedo Geisler-Moroder variant.
export struct ward_geisler_moroder_bsdf:bsdf{
/// The roughness in U.
///
/// > Roughness coefficient in the U direction. Range: `[0,inf)`, with `0`
/// > specifying pure specular reflection.
///
float roughness_u;

/// The roughness in V.
///
/// > Roughness coefficient in the V direction. Range: `[0,inf)`, with `0`
/// > specifying pure specular reflection.
///
float roughness_v=roughness_u;

/// The tint.
///
/// > Scaling factor, defined as a color, multiplied by the
/// > result of the distribution function.
///
$(color|float) tint=1.;

/// The multiscatter tint.
///
/// > Scaling factor, defined as a color, of the diffuse multiscattering
/// > compensation, `color(0.0)` does not add any, `color(1.0)` fully
/// > compensates the energy loss.
///
$(?(color|float)) multiscatter_tint=none;

/// The tangent in U.
float3 tangent_u=$state.texture_tangent_u[0];

/// The handle.
void handle="";

/// The flags.
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
const auto alpha(#max(1e-3,roughness*roughness));
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
const auto alpha(#max(1e-3,roughness*roughness));
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
} /// The microfacet utilities: the distributions (GGX, Beckmann, Blinn),
/// their slope and normal sampling routines, and the shadowing
/// techniques, shared by all of the `microfacet_*_bsdf` variants.
export namespace microfacet {

/// The tag to identify microfacet distributions.
export tag Distribution;

/// The GGX (Ground-Glass-X) or Trowbridge-Reitz distribution.
export struct DistributionGGX:default Distribution{};

/// The Beckmann or Gaussian distribution.
export struct DistributionBeckmann:Distribution{};

/// The Smith Lambda function for the GGX distribution.
@(pure macro)
export float smithLambda(const DistributionGGX this[[anno::unused()]],const float m){
return 0.5*(#sign(m)*#sqrt(1+1/(m*m+EPSILON)))-0.5;
}

/// The Smith Lambda function for the Beckmann distribution.
@(pure macro)
export float smithLambda(const DistributionBeckmann this[[anno::unused()]],const float m){
return 0.5*(#exp(-m*m)/m/#sqrt($PI)-float(erfc(m)));
}

/// The 2-dimensional Smith slope PDF for the GGX distribution.
@(pure macro)
export float smithSlopePDF(const DistributionGGX this[[anno::unused()]],const float2 m){
return (1/$PI)/#pow(1+#sum(m*m),2);
}

/// The 2-dimensional Smith slope PDF for the Beckmann distribution.
@(pure macro)
export float smithSlopePDF(const DistributionBeckmann this[[anno::unused()]],const float2 m){
return (1/$PI)*#exp(-#sum(m*m));
}

/// The Smith visible slope sampling function for the GGX distribution.
@(pure)
export float2 smithVisibleSlopeSample(
const DistributionGGX this[[anno::unused()]],
const float xi0, ///< A canonical random number in `[0,1]`
const float xi1, ///< A canonical random number in `[0,1]`
float cosThetao, ///< The outgoing zenith angle cosine
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
const auto s(#select(xi1>0.5,1.,-1.));
const auto t(#min(s*(2*xi1-1),1));
return #sqrt(1+mx*mx)*s*((t*(t*(t*0.27385-0.73369)+0.46341))/(t*(t*(t*0.093073+0.30942)-1.)+0.597999));
};
return float2(mx,my);
}

/// The Smith visible slope sampling function for the Beckmann distribution.
@(pure)
export float2 smithVisibleSlopeSample(
const DistributionBeckmann this[[anno::unused()]],
float xi0,       ///< A canonical random number in `[0,1]`
float xi1,       ///< A canonical random number in `[0,1]`
float cosThetao, ///< The outgoing zenith angle cosine
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
break if(f~==|1e-5|0.);
if(f>0)
xmax=x;
else
xmin=x;
x-=f/(norm*(1-a*tanThetao));
}
return float2(monte_carlo::erfInverse(x),monte_carlo::erfInverse(2*xi1-1),);
}

/// The Smith normal PDF, being the slope PDF mapped onto normals for the
/// given squared roughness `alpha`.
@(pure macro)
export float smithNormalPDF(const Distribution this[[anno::unused()]],const float2 alpha,const float3 wm){
return wm.z>0.?smithSlopePDF(this,-wm.xy/(wm.z*alpha+EPSILON))/(alpha.x*alpha.y*#pow(wm.z,4)+EPSILON):0.;
}

/// The Smith visible normal sampling function.
@(pure)
export float3 smithVisibleNormalSample(
const Distribution this,
const float xi0,    ///< A canonical random number in `[0,1]`
const float xi1,    ///< A canonical random number in `[0,1]`
const float2 alpha, ///< The squared roughness
const float3 wo,    ///< The outgoing direction
){
const auto w11(normalize(float3(alpha*wo.xy,wo.z)));
const auto sinTheta(length(w11.xy));
const auto cosPhi(w11.x/sinTheta);
const auto sinPhi(w11.y/sinTheta);
const auto m11(smithVisibleSlopeSample(this,xi0,xi1,w11.z));
const auto m(float2(alpha.x*dot(float2(cosPhi,-sinPhi),m11),alpha.y*dot(float2(sinPhi,cosPhi),m11)));
return #all(isfinite(m))?normalize(float3(-m,1)):wo.z==0?normalize(wo):float3(0,0,1);
}

/// The bounded spherical-cap factor for GGX visible-normal sampling of
/// reflection-only lobes, after Eto and Tokuyoshi, "Bounded VNDF Sampling
/// for Smith-GGX Reflections", SIGGRAPH Asia 2023 Technical Communications.
/// Shrinks the spherical cap of Dupuy and Benyoub to exclude microsurface
/// normals whose reflection vectors would fall below the horizon. The
/// anisotropic bound uses the paper's conservative loosening with the
/// minimum roughness.
@(pure)
export float ggxBoundedCapFactor(const float2 alpha,const float3 wo){
const float a(saturate(#min(alpha.x,alpha.y)));
const float s(1+length(wo.xy));
const float a2(a*a);
const float s2(s*s);
return (1-a2)*s2/(s2+a2*wo.z*wo.z);
}

/// The GGX visible normal sampling function through a bounded spherical
/// cap, valid for reflection-only lobes. Equivalent to the standard
/// visible-normal distribution up to the tightened cap, whose density
/// correction is folded into the PDFs reported by `scatterEvaluate`.
@(pure)
export float3 ggxBoundedVisibleNormalSample(
const float xi0,    ///< A canonical random number in `[0,1]`
const float xi1,    ///< A canonical random number in `[0,1]`
const float2 alpha, ///< The squared roughness
const float3 wo,    ///< The outgoing direction
){
const float3 woStd(normalize(float3(alpha*wo.xy,wo.z)));
const float b((wo.z>0?ggxBoundedCapFactor(alpha,wo):1.)*woStd.z);
const float z((1-xi1)*(1+b)-b);
const float sinTheta(#sqrt(saturate(1-z*z)));
const float phi($TWO_PI*xi0);
const float3 mStd(woStd+float3(sinTheta*#cos(phi),sinTheta*#sin(phi),z));
return normalize(float3(alpha*mStd.xy,mStd.z));
}

/// The Blinn distribution.
export struct DistributionBlinn:Distribution{};

/// The Blinn normal first-quadrant sampling function.
@(pure)
export void blinnNormalFirstQuadrantSample(
const float xi0, ///< A canonical random number in `[0,1]`
const float xi1, ///< A canonical random number in `[0,1]`
const float2 e,  ///< The exponent
&float phi,      ///< The output azimuth angle
&float cosTheta, ///< The output zenith angle cosine
){
if(e.x==e.y){
*phi=$HALF_PI*xi0;
*cosTheta=#pow(xi1,1/(1+e.x));
} else {
*phi=#atan(#sqrt((1+e.x)/(1+e.y))*#tan($HALF_PI*xi0));
*cosTheta=#pow(xi1,1/(1+e.x*(cosPhi:=#cos(*phi))*cosPhi+e.y*(sinPhi:=#sin(*phi))*sinPhi));
}
}

/// The Blinn normal sampling function.
@(pure)
export float3 blinnNormalSample(const float xi0, ///< A canonical random number in `[0,1]`
const float xi1,                                 ///< A canonical random number in `[0,1]`
const float2 e,                                  ///< The exponent
){
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

/// The tag to identify microfacet shadowing functions.
export tag Shadowing;

/// The Smith shadowing function. (This is the principled correct solution!)
export struct ShadowingSmith:default Shadowing{};

/// The V-cavities shadowing function. (This is the older simpler approximation!)
export struct ShadowingVCavities:Shadowing{};
@(foreign pure)
double lgamma(double x);

/// The beta function for Smith shadowing in transmission calculations.
@(pure)
export double beta(const double x,const double y)=#exp(lgamma(x)+lgamma(y)-lgamma(x+y));
}
struct microfacet_bsdf:bsdf{
/// The roughness in `[0,1]^2`.
const float2 roughness;

/// The geometric mean roughness.
const float roughness0=#sqrt(#prod(roughness));

/// The roughness squared in `[EPSILON,1]^2`.
///
/// NOTE: This is the effective roughness parameter that is actually
/// used in microfacet equations. It is squared for perceptual linearity,
/// meaning that adjusting the `roughness` parameter more closely tracks
/// qualitative changes in the apparent roughness of the BSDF.
///
const float2 alpha=clamp(roughness*roughness,EPSILON,1.);

/// The tint.
$(color|float) tint;

/// The multiscatter tint, or `none` for no multiscatter.
$(?(color|float)) multiscatter_tint=none;

/// The tangent direction for orienting anistropic roughness.
float3 tangent_u=$state.texture_tangent_u[0];

/// The scatter mode. With `scatter_reflect_transmit`, the reflect and
/// transmit lobes are weighted by the dielectric Fresnel term, forming a
/// complete rough dielectric in one lobe.
const scatter_mode mode=scatter_reflect;

/// The microfacet distribution.
const microfacet::Distribution distribution=microfacet::Distribution();

/// The microfacet shadowing technique.
const microfacet::Shadowing shadowing=microfacet::Shadowing();

/// The flags.
const int df_flags=int(mode)|DF_GLOSSY;
};
@(pure noinline)
auto scatterEvaluate(const &microfacet_bsdf this,inline const &ScatterEvaluateParameters params){
auto effectiveMode(this.mode&mode);
preserve tangent_u;
tangent_u=this.tangent_u;
return ScatterEvaluateResult(isBlack: true) if(!recalculateTangentSpace(params)||effectiveMode==scatter_none);
preserve wi,mode;
bool thinTransmit=false;
if(thin_walled&&effectiveMode==scatter_transmit){
thinTransmit=true;
effectiveMode=scatter_reflect;
mode=scatter_reflect;
wi.z=-wi.z;
}
const auto cosThetao(#abs(wo.z));
const auto cosThetai(#abs(wi.z));
const auto wm(halfDirection(params));
const auto dotWoWm(#sum(wo*wm));
const auto dotWiWm(#sum(wi*wm));
float fWeight=1.;
float2 pdfWeight=float2(1.);
if(this.mode==scatter_reflect_transmit){
const auto F(specular::dielectricFresnel(dotWoWm,ior));
const auto Fo(specular::dielectricFresnel(cosThetao,ior));
const auto Fi(specular::dielectricFresnel(cosThetai,effectiveMode==scatter_reflect?ior:1/ior));
const bool complement(thinTransmit|(effectiveMode==scatter_transmit));
fWeight=complement?1-F:F;
pdfWeight=complement?1-float2(Fo,Fi):float2(Fo,Fi);
}
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
result.f*=fWeight;
result.pdf*=pdfWeight;
return result;
} else {
return ScatterEvaluateResult(isBlack: true) if(!((dotWoWm>0)&(dotWiWm<0)));
const auto jac(float2(specular::refractionHalfVectorJacobian(wo,wi,ior),specular::refractionHalfVectorJacobian(wi,wo,1/ior)));
const auto pdf(norm1*D*jac);
const auto f(norm2*D*G*jac[0]*dotWoWm/(cosThetao+EPSILON));
auto result(ScatterEvaluateResult(f: this.tint*f,pdf: pdf));
result.f*=shadingNormalCorrection if(isImportance);
result.f*=ior*ior if(!isImportance);
result.f*=fWeight;
result.pdf*=pdfWeight;
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
auto pdf(D/(4*float2(projAreao,projAreai)+EPSILON));
if$(this.distribution<:microfacet::DistributionGGX){
if(this.mode==scatter_reflect){
const float2 k(float2(microfacet::ggxBoundedCapFactor(this.alpha,wo),microfacet::ggxBoundedCapFactor(this.alpha,wi)));
pdf=D/(4*float2(projAreao,projAreai)+2*(k-1)*float2(cosThetao,cosThetai)+EPSILON);
}
}
const auto f(D*G/(4*cosThetao+EPSILON));
auto result(ScatterEvaluateResultWithMultiscatter(this,f,pdf,cosThetao,cosThetai,this.roughness0,lutName));
result.f*=shadingNormalCorrection if(isImportance);
result.f*=fWeight;
result.pdf*=pdfWeight;
return result;
} else {
return ScatterEvaluateResult(isBlack: true) if(!((dotWoWm>0)&(dotWiWm<0)));
const auto jac(float2(specular::refractionHalfVectorJacobian(wo,wi,ior),specular::refractionHalfVectorJacobian(wi,wo,1/ior)));
const auto pdf(D*jac*float2(dotWoWm,-dotWiWm)/(float2(projAreao,projAreai)+EPSILON));
const auto f(D*G*jac[0]*dotWoWm/(cosThetao+EPSILON));
auto result(ScatterEvaluateResult(f: this.tint*f,pdf: pdf));
result.f*=shadingNormalCorrection if(isImportance);
result.f*=ior*ior if(!isImportance);
result.f*=fWeight;
result.pdf*=pdfWeight;
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
const auto reflectChance(this.mode==scatter_reflect_transmit?specular::dielectricFresnel(#abs(wo.z),ior):scatterReflectChance(this.mode));
const auto mode(monte_carlo::boolSample(&xi.z,reflectChance)?scatter_reflect:scatter_transmit);
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
if$(this.distribution<:microfacet::DistributionGGX){
if(this.mode==scatter_reflect)
return microfacet::ggxBoundedVisibleNormalSample(xi.x,xi.y,this.alpha,wo);
}
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
const $(color|float) tint=1.,
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
} /// The simple glossy BSDF, being a Blinn microfacet BSDF with V-cavities
/// shadowing, degenerating to `specular_bsdf` at zero roughness.
export auto simple_glossy_bsdf(*)=makeMicrofacetBSDF(distribution: microfacet::DistributionBlinn(),shadowing: microfacet::ShadowingVCavities());

/// The GGX microfacet BSDF with Smith shadowing, degenerating to
/// `specular_bsdf` at zero roughness.
export auto microfacet_ggx_smith_bsdf(*)=makeMicrofacetBSDF(distribution: microfacet::DistributionGGX(),shadowing: microfacet::ShadowingSmith());

/// The GGX microfacet BSDF with V-cavities shadowing, degenerating to
/// `specular_bsdf` at zero roughness.
export auto microfacet_ggx_vcavities_bsdf(*)=makeMicrofacetBSDF(distribution: microfacet::DistributionGGX(),shadowing: microfacet::ShadowingVCavities());

/// The Beckmann microfacet BSDF with Smith shadowing, degenerating to
/// `specular_bsdf` at zero roughness.
export auto microfacet_beckmann_smith_bsdf(*)=makeMicrofacetBSDF(distribution: microfacet::DistributionBeckmann(),shadowing: microfacet::ShadowingSmith());

/// The Beckmann microfacet BSDF with V-cavities shadowing, degenerating to
/// `specular_bsdf` at zero roughness.
export auto microfacet_beckmann_vcavities_bsdf(*)=makeMicrofacetBSDF(distribution: microfacet::DistributionBeckmann(),shadowing: microfacet::ShadowingVCavities());

/// Is the BSDF measurement valid, i.e., backed by loaded measurement data?
export bool bsdf_measurement_isvalid(const bsdf_measurement measurement)=bool(measurement.buffer);

/// Declare `smdBSDFMeasurementInterpolate` in `lib/BSDFMeasurement.cc`
@(pure foreign)
void smdBSDFMeasurementInterpolate(&void measurement,&float3 wo,&float3 wi,&float3 result);

/// Declare `smdBSDFMeasurementDirectionPDF` in `lib/BSDFMeasurement.cc`
@(pure foreign)
float smdBSDFMeasurementDirectionPDF(&void measurement,&float3 wo,&float3 wi);

/// Declare `smdBSDFMeasurementDirectionSample` in `lib/BSDFMeasurement.cc`
@(pure foreign)
void smdBSDFMeasurementDirectionSample(&void measurement,&float2 xi,&float3 wo,&float3 wi,&float pdf);

/// The measured BSDF, evaluating and importance sampling a loaded
/// `bsdf_measurement`.
export struct measured_bsdf:bsdf{
/// The measurement.
bsdf_measurement measurement;

/// The multiplier.
float multiplier=1.;

/// The scatter mode.
scatter_mode mode=scatter_reflect;

/// The handle.
string handle="";

/// The flags.
const int df_flags=(int(mode)&measurement.mode)|DF_GLOSSY;
};
@(macro)
auto scatterEvaluate(const &measured_bsdf this,inline const &ScatterEvaluateParameters params){
const auto enabledMode(int(this.mode)&this.measurement.mode);
return ScatterEvaluateResult(isBlack: true) if(!bool(this.measurement.ptr)||(int(mode)&enabledMode)==0);
return ScatterEvaluateResult(isBlack: true) if(!recalculateTangentSpace(params));
auto wiUpper(float3(wi.x,wi.y,#abs(wi.z)));
auto f3(float3(0.));
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
auto wiLocal(float3(0.));
float pdf=0.;
smdBSDFMeasurementDirectionSample(this.measurement.ptr,&xi2,&wo,&wiLocal,&pdf);
return ScatterSampleResult() if(!(pdf>0.));
wiLocal.z=-wiLocal.z if(enabledMode==int(scatter_transmit));
return ScatterSampleResult(wi: normalize((*tbn)*wiLocal),mode: enabledMode==int(scatter_transmit)?scatter_transmit:scatter_reflect,);
} else {
return ScatterSampleResult();
}
}
static const auto HAPKE_QUAD=auto[16](auto(0.0426509835,0.999861409,0.0166482032,0.00832467848,-0.989400935),auto(0.0977876067,0.996212554,0.086951409,0.0435581917,-0.944575023),auto(0.149474641,0.977808138,0.209502376,0.105926541,-0.865631202),auto(0.19576673,0.927094889,0.374826716,0.194503508,-0.755404408),auto(0.23498483,0.825200872,0.564839376,0.309466966,-0.617876244),auto(0.265710439,0.658971885,0.752167571,0.453393802,-0.458016778),auto(0.286832774,0.428057064,0.903751708,0.632854058,-0.281603551),auto(0.297588323,0.148691866,0.988883577,0.860878018,-0.0950125098),auto(0.297588323,-0.148691866,0.988883577,1.16160476,0.0950125098),auto(0.286832774,-0.428057064,0.903751708,1.58014314,0.281603551),auto(0.265710439,-0.658971885,0.752167571,2.20558816,0.458016778),auto(0.23498483,-0.825200872,0.564839376,3.23136267,0.617876244),auto(0.19576673,-0.927094889,0.374826716,5.14129545,0.755404408),auto(0.149474641,-0.977808138,0.209502376,9.44050459,0.865631202),auto(0.0977876067,-0.996212554,0.086951409,22.9577942,0.944575023),auto(0.0426509835,-0.999861409,0.0166482032,120.124759,0.989400935));
static const auto HAPKE_W_CHEB=auto[5](auto(0.0443692637,-0.0330322166,-0.0414806793,0.0377322324,-0.0036831791,-0.00503835424,8.09641201e-4,3.28879959e-4,2.18482055e-5,-1.60545071e-5,-1.57504064e-5),auto(-0.00594076498,7.57196179e-4,0.0081566095,-6.27186314e-4,-0.00248094082,-4.88022104e-4,2.97539468e-4,3.75439189e-4,-8.86163641e-6,-3.29277167e-5,-3.83729896e-5),auto(-9.63613192e-4,4.66153409e-4,0.00149075512,-6.73457031e-4,-7.58809229e-4,1.27478645e-4,2.95317848e-4,1.1745609e-4,-6.84391571e-5,-5.81575106e-5,-1.47705227e-5),auto(2.75487885e-4,6.03421013e-5,-4.03479982e-4,-1.90433817e-4,1.11667449e-4,1.85513524e-4,6.24427123e-5,-7.37454173e-5,-7.16632707e-5,-2.55167593e-5,1.67814334e-5),auto(-2.62793436e-5,-3.65689393e-5,6.2527643e-6,6.15744651e-5,6.90324901e-5,-6.91503114e-6,-5.56055434e-5,-4.09210276e-5,7.60283579e-6,2.61753306e-5,1.97457206e-5));
static const auto HAPKE_G_CHEB=auto[4](auto(0.299680994,-0.25994946,0.0835171816,-0.0234697031,0.0061327028,-0.00150566063,3.6640673e-4),auto(-0.0526057365,0.0529757455,-0.0205834026,0.00702317686,-0.00220447865,6.35609701e-4,-1.79704911e-4),auto(-0.0107799361,0.00980414131,-0.00314615761,8.44617944e-4,-2.0008313e-4,4.17196758e-5,-7.49359084e-6),auto(0.00182228246,-0.00183367741,8.1867027e-4,-3.34625137e-4,1.23845828e-4,-4.06381678e-5,1.27990763e-5)); /// Evaluate a Chebyshev series by the Clenshaw recurrence, `x` in `[-1,1]`.
///
/// NOTE: Generic in both the coefficients and `x`, so the same routine serves
/// the scalar density evaluation at construction and the spectral `EbarK`
/// evaluation.
///
@(pure)
auto hapkeChebEval(const auto c,const int n,const auto x){
auto b1(0.*x);
auto b2(0.*x);
for(int k=n-1;k>=1;k--){
const auto t(2.*x*b1-b2+c[k]);
b2=b1;
b1=t;
}
return x*b1-b2+c[0];
}

/// The tangent of the half phase angle, which is the natural argument of the surge.
@(pure macro)
float hapkeTanHalfAngle(const float cosG)=#sqrt((1.-cosG)/#max(1.+cosG,1e-12));

/// The two-lobe Henyey-Greenstein phase function in Hapke's convention, where the
/// first lobe peaks at `g = 0`, which is to say at backscatter. The sharpness `b`
/// narrows both lobes and the weight `c` balances backward against forward.
@(pure)
float hapkePhase(const float b,const float c,const float cosG){
const auto numer(1.-b*b);
const auto back(1.-2.*b*cosG+b*b);
const auto fwd(1.+2.*b*cosG+b*b);
return 0.5*(1.+c)*numer/(back*#sqrt(back))+0.5*(1.-c)*numer/(fwd*#sqrt(fwd));
}

/// The shadow-hiding opposition effect (SHOE, Hapke 1986). Near `g = 0` a grain hides
/// its own shadow, brightening retro-reflection by `B0 / (1 + tan(g/2)/h)`.
@(pure macro)
float hapkeSurge(const float B0,const float h,const float tanHalfG)=B0/(1.+tanHalfG/h);

/// The Chandrasekhar H-function in the Hapke (2002) rational-log approximation, whose
/// error is under 1 percent against the exact integral-equation solution. The product
/// `H(mu0) H(mu) - 1` is the isotropic approximation of everything scattered more than
/// once: it grows from 1 on a dark surface toward roughly 3 as `w` approaches 1 at
/// high cosines, which is where bright surfaces get their flattened limbs.
@(pure)
auto hapkeH(const float x,const auto w,const auto r0){
const auto xs(#max(x,1e-6));
return 1./(1.-w*xs*(r0+0.5*(1.-2.*r0*xs)*#log(1.+1./xs)));
}

/// The energy-conserving Hapke BRDF for granular surfaces.
///
/// The distribution is a normalized Hapke shape kernel times the prescribed `albedo`,
/// `f = albedo * K / EbarK`, where `EbarK` is the kernel's own bihemispherical albedo,
/// so that the realized white-sky albedo equals `albedo` exactly for every setting of
/// the four directional parameters. They redistribute energy without creating or
/// destroying it. The kernel itself is
///
///     K = w / (4 pi (mu0e + mue)) * [ptilde(g) + H(mu0e) H(mue) - 1] * S
///
/// which is the Lommel-Seeliger single-scattering core, the surge-folded two-lobe
/// Henyey-Greenstein phase function `ptilde`, isotropic multiple scattering built from
/// the Hapke (2002) H-functions, and the Hapke (1984) macroscopic-roughness shadowing
/// `S`, assembled in its manifestly reciprocal form.
///
/// NOTE: The `albedo` is not a tint. It is the reflectance the surface
/// actually realizes under uniform illumination, so it is meaningful to
/// drive it with a measured or modeled soil spectrum.
///
export struct hapke_granular_bsdf:bsdf{
/// The albedo, realized exactly as the bihemispherical (white-sky) reflectance.
const $(color|float) albedo=1.;

/// The roughness, being the Hapke mean facet slope `theta_bar = 30 degrees * roughness`.
float roughness=0.;

/// The porosity. `0` is compacted, with a wide opposition surge, and `1` is fluffy
/// fairy-castle structure, with a narrow one.
float porosity=0.5;

/// The hotspot, being the amplitude `B0` of the shadow-hiding opposition surge.
float hotspot=0.8;

/// The backscatter. `0` is forward-scattering translucent grains and `1` is rough
/// opaque backscatterers, where soils sit high.
float backscatter=0.8;

/// The single-scattering albedo `w`.
auto _ssa=albedo;

/// The diffusive reflectance `r0`, which is the clamped albedo by construction.
auto _diffRefl=albedo;

/// The albedo per unit kernel, `A / (A_ss + w G)`.
auto _scale=albedo;

/// The Henyey-Greenstein lobe sharpness `b`.
float _hgSharpness=0.;

/// The Henyey-Greenstein backward-to-forward weight `c`.
float _hgBackWeight=0.;

/// The opposition surge width `h`.
float _surgeWidth=0.;

/// The reciprocal of the surge normalization `N_B`.
float _invSurgeNorm=1.;

/// The tangent of the mean facet slope, where `0` selects the smooth path.
float _tanMeanSlope=0.;

/// The handle.
void handle="";

/// The flags.
static const int df_flags=DF_REFLECTION|DF_DIFFUSE;
finalize {
roughness=saturate(roughness);
porosity=saturate(porosity);
hotspot=saturate(hotspot);
backscatter=saturate(backscatter);
_hgSharpness=0.65+(0.1-0.65)*backscatter;
_hgBackWeight=3.29*#exp(-17.4*_hgSharpness*_hgSharpness)-0.908;
_surgeWidth=-0.375*#log(1.-(0.1+(0.48-0.1)*(1.-porosity)));
const auto meanSlope(($PI/6)*roughness);
_tanMeanSlope=meanSlope<1e-8?0.:#tan(meanSlope);
const auto y(2.*roughness-1.);
const auto y2(2.*y*y-1.);
const auto y3(2.*y*y2-y);
const auto y4(2.*y*y3-y2);
const auto wcol(HAPKE_W_CHEB[0]+y*HAPKE_W_CHEB[1]+y2*HAPKE_W_CHEB[2]+y3*HAPKE_W_CHEB[3]+y4*HAPKE_W_CHEB[4],);
const auto gcol(HAPKE_G_CHEB[0]+y*HAPKE_G_CHEB[1]+y2*HAPKE_G_CHEB[2]+y3*HAPKE_G_CHEB[3],);
float surgeAcc=0.;
float ssAcc=0.;
for(int j=0;j<#num(HAPKE_QUAD);j++){
const auto node(HAPKE_QUAD[j]);
const auto weightedPhase(node[0]*hapkePhase(_hgSharpness,_hgBackWeight,node[1]));
const auto surge(hapkeSurge(hotspot,_surgeWidth,node[3]));
const auto density(#max(hapkeChebEval(wcol,#num(wcol),node[4]),0.));
surgeAcc+=weightedPhase*surge*node[2];
ssAcc+=weightedPhase*(1.+surge)*density;
}
_invSurgeNorm=1./(1.+0.5*surgeAcc);
const auto ssAlbedoPerW(ssAcc*_invSurgeNorm);
const auto A(clamp(albedo,1e-6,1.-1e-6));
const auto gamma((1.-A)/(1.+A));
_ssa=1.-gamma*gamma;
_diffRefl=A;
_scale=A/(ssAlbedoPerW+_ssa*hapkeChebEval(gcol,#num(gcol),2.*gamma-1.));
}
};

/// Evaluate the Hapke BRDF, without the cosine factor, for directions expressed in the
/// shading tangent space and understood to be in the upper hemisphere.
@(pure noinline)
auto hapkeEvaluateBRDF(const &hapke_granular_bsdf this,const float3 wo,const float3 wi){
const auto mu0(clamp(wi.z,1e-4,1.));
const auto mu(clamp(wo.z,1e-4,1.));
const auto cosG(clamp(#sum(wi*wo),-1.,1.));
const auto sinScale(length(wi.xy)*length(wo.xy));
const auto cosPsi(sinScale>1e-6?clamp(#sum(wi.xy*wo.xy)/sinScale,-1.,1.):1.);
const auto psi(#acos(cosPsi));
const auto surge(hapkeSurge(this.hotspot,this._surgeWidth,hapkeTanHalfAngle(cosG)));
const auto phase(hapkePhase(this._hgSharpness,this._hgBackWeight,cosG)*(1.+surge)*this._invSurgeNorm);
const auto geom=return_from{
return auto(1./(4.*$PI*(mu0+mu)),mu0,mu) if(this._tanMeanSlope==0.);
const auto tanT(this._tanMeanSlope);
const auto cotT(1./tanT);
const auto chi(1./#sqrt(1.+$PI*tanT*tanT));
const auto sin0(#sqrt(#max(1.-mu0*mu0,0.)));
const auto sine(#sqrt(#max(1.-mu*mu,0.)));
const auto cotI(cotT*mu0/#max(sin0,1e-6));
const auto cotE(cotT*mu/#max(sine,1e-6));
const auto E1i(#exp(-(2./$PI)*cotI));
const auto E1e(#exp(-(2./$PI)*cotE));
const auto E2i(#exp(-(1./$PI)*cotI*cotI));
const auto E2e(#exp(-(1./$PI)*cotE*cotE));
const auto f(#exp(-2.*#tan(#min(0.5*psi,1.5707))));
const auto etaI(chi*(mu0+sin0*tanT*E2i/(2.-E1i)));
const auto etaE(chi*(mu+sine*tanT*E2e/(2.-E1e)));
const bool bigI(mu0<=mu);
const auto E1L(bigI?E1i:E1e);
const auto E1l(bigI?E1e:E1i);
const auto E2L(bigI?E2i:E2e);
const auto E2l(bigI?E2e:E2i);
const auto sinHalfPsi(#sin(0.5*psi));
const auto s2(sinHalfPsi*sinHalfPsi);
const auto D(#max(2.-E1L-(psi/$PI)*E1l,1e-6));
const auto cosL(#min(mu0,mu));
const auto cosl(#max(mu0,mu));
const auto sinL(#sqrt(#max(1.-cosL*cosL,0.)));
const auto sinl(#sqrt(#max(1.-cosl*cosl,0.)));
const auto effL(chi*(cosL+sinL*tanT*(E2L-s2*E2l)/D));
const auto effl(chi*(cosl+sinl*tanT*(#cos(psi)*E2L+s2*E2l)/D));
const auto mu0Eff(bigI?effL:effl);
const auto muEff(bigI?effl:effL);
const auto etaMin(bigI?etaE:etaI);
const auto shadow(chi/(1.-f+f*chi*cosl/etaMin));
const auto factor(mu0Eff*muEff/((mu0Eff+muEff)*etaI*etaE)*shadow/(4.*$PI));
return auto(factor,mu0Eff,muEff);
};
const auto H0(hapkeH(geom[1],this._ssa,this._diffRefl));
const auto H1(hapkeH(geom[2],this._ssa,this._diffRefl));
return this._scale*geom[0]*(phase+#max(H0*H1-1.,0.));
}
@(pure)
auto scatterEvaluate(const &hapke_granular_bsdf this,inline const &ScatterEvaluateParameters params){
if(mode==scatter_reflect&&recalculateTangentSpace(params)){
const auto cosTheta(#abs(auto(wi.z,wo.z)));
const auto pdf(cosTheta/$PI);
auto result(ScatterEvaluateResult(f: hapkeEvaluateBRDF(this,wo,wi)*cosTheta[0],pdf: pdf));
result.f*=shadingNormalCorrection if(isImportance);
return result;
} else {
return ScatterEvaluateResult(isBlack: true);
}
}
@(pure)
auto scatterSample(const &hapke_granular_bsdf this[[anno::unused()]],inline const &ScatterSampleParameters params){
if((tbn:=recalculateTangentSpace(params))){
return ScatterSampleResult(wi: (*tbn)*monte_carlo::cosineHemisphereSample(xi.xy),mode: scatter_reflect);
} else {
return ScatterSampleResult();
}
} /// A diffuse (Lambertian) Emission Distribution Function (EDF), i.e.,
/// constant radiance over the upper hemisphere.
export struct diffuse_edf:edf{
/// The handle.
void handle="";

/// The flags.
static const int df_flags=DF_DIFFUSE;
};
@(pure)
auto emissionEvaluate(const &diffuse_edf this[[anno::unused()]],inline const &EmissionEvaluateParameters params){
if(recalculateTangentSpace(params)){
return EmissionEvaluateResult(f: 1./$PI,pdf: wi.z/$PI);
} else {
return EmissionEvaluateResult(isBlack: true);
}
}
@(pure)
auto emissionSample(const &diffuse_edf this[[anno::unused()]],inline const &EmissionSampleParameters params){
return EmissionSampleResult(wi: calculateTangentSpace(normal,tangent_u)*monte_carlo::cosineHemisphereSample(xi.xy),isValid: true);
} /// A spot Emission Distribution Function (EDF), i.e., an exponentiated
/// cosine falloff restricted to a cone.
///
/// The cosine is remapped so that it reaches zero at the boundary of the
/// cone: with `theta0 = spread / 2` the distribution is proportional to
/// `((cos(theta) - cos(theta0)) / (1 - cos(theta0)))^exponent` inside the
/// cone and zero outside of it.
///
export struct spot_edf:edf{
/// The exponent of the cosine falloff.
float exponent;

/// The spread, being the full angle of the emission cone in radians. The
/// default of `$PI` corresponds to the full upper hemisphere.
float spread=$PI;

/// > Boolean that chooses between two interpretations of the EDF: the
/// > directional distribution applied per point in tangent space, or the
/// > distribution of the light source as a whole in the global frame.
///
/// NOTE: Only the per-point tangent-space interpretation is supported, so
/// this is accepted and ignored. The luminaire-as-a-whole interpretation
/// is a host-side concern, e.g., a delta spot light.
///
void global_distribution=true;

/// The global frame.
///
/// NOTE: Only the per-point tangent-space interpretation is supported, so
/// this is accepted and ignored.
///
void global_frame=float3x3(1.);

/// The handle.
void handle="";

/// The precomputed cosine of the cone half angle.
const float _cosSpread=#cos(0.5*#min(#max(spread,EPSILON),$PI));

/// The precomputed normalization such that the cosine-weighted integral
/// over the cone is 1, i.e., with `c0 = _cosSpread` and `k = exponent`,
/// the integral of `((x - c0)/(1 - c0))^k x 2 pi dx` over `x` in `[c0, 1]`.
///
/// NOTE: This must clamp `exponent` itself because field initializers run
/// before the `finalize` block.
///
const float _normalization=let {
const float k=#max(exponent,0.);
} in 2.*$PI*(1.-_cosSpread)*(_cosSpread/(k+1.)+(1.-_cosSpread)/(k+2.));

/// The flags.
static const int df_flags=DF_GLOSSY;
finalize {
exponent=#max(exponent,0.);
}
};
@(pure)
auto emissionEvaluate(const &spot_edf this,inline const &EmissionEvaluateParameters params){
if(recalculateTangentSpace(params)){
const auto mu((wi.z-this._cosSpread)/(1.-this._cosSpread));
if(!(mu>0.))
return EmissionEvaluateResult(isBlack: true);
const auto muPowK(#pow(mu,this.exponent));
return EmissionEvaluateResult(f: muPowK/this._normalization,pdf: (this.exponent+1.)*muPowK/(2.*$PI*(1.-this._cosSpread)),);
} else {
return EmissionEvaluateResult(isBlack: true);
}
}
@(pure)
auto emissionSample(const &spot_edf this,inline const &EmissionSampleParameters params){
const auto mu(#pow(xi.x,1./(this.exponent+1.)));
const auto cosTheta(this._cosSpread+(1.-this._cosSpread)*mu);
const auto sinTheta(#sqrt(#max(1.-cosTheta*cosTheta,0.)));
const auto phi($TWO_PI*xi.y);
return EmissionSampleResult(wi: calculateTangentSpace(normal,tangent_u)*float3(sinTheta*#cos(phi),sinTheta*#sin(phi),cosTheta),isValid: true,);
} /// Declare the light profile interpolation routine implemented in
/// `lib/LightProfile.cc` and registered as a JIT builtin by
/// `#load_light_profile` in `lib/Compiler/Emitter.cc`.
@(pure foreign)
float smdlLightProfileInterpolate(const &void profile,const &float3 wo);

/// Declare the light profile direction PDF routine, being the exact
/// solid-angle density over the sphere of the direction sampling routine
/// in the profile's own coordinate system.
@(pure foreign)
float smdlLightProfileDirectionPDF(const &void profile,const &float3 wi);

/// Declare the light profile direction sampling routine.
@(pure foreign)
void smdlLightProfileDirectionSample(const &void profile,const &float2 xi,const &float3 wi,const &float pdf);

/// A measured Emission Distribution Function (EDF), i.e., an IES light
/// profile applied per point in tangent space: the profile's vertical
/// axis (vertical angle 0) is aligned with the shading normal, and its
/// horizontal angle 0 is aligned with `tangent_u`.
///
/// The EDF value is `multiplier * profile(wi) / max_intensity`, so unlike
/// `diffuse_edf` and `spot_edf` it is NOT normalized such that the
/// cosine-weighted hemisphere integral is 1: with `multiplier = 1`, the
/// `material_emission.intensity` is the radiance emitted in the peak
/// direction of the profile, and the profile shapes the falloff. Hosts
/// that need the true emitted power of the profile for light selection
/// should use `light_profile.power`.
///
/// NOTE: The lower hemisphere of the profile is clipped: a surface cannot
/// emit below its own horizon. Directions sampled there are rejected, and
/// the PDF is reported with respect to solid angle over the full sphere,
/// so evaluation and sampling remain consistent for MIS.
///
export struct measured_edf:edf{
/// The light profile.
light_profile profile;

/// The multiplier.
float multiplier=1.;

/// > Boolean that chooses between two interpretations of the EDF: the
/// > directional distribution applied per point in tangent space, or the
/// > distribution of the light source as a whole in the global frame.
///
/// NOTE: Only the per-point tangent-space interpretation is supported, so
/// this is accepted and ignored. The luminaire-as-a-whole interpretation
/// is a host-side concern, e.g., a delta point light driven directly by
/// the C++ `smdl::LightProfile` API.
///
void global_distribution=true;

/// The global frame.
///
/// NOTE: Only the per-point tangent-space interpretation is supported, so
/// this is accepted and ignored.
///
void global_frame=float3x3(1.);

/// The tangent direction, orienting the horizontal angle of the profile.
float3 tangent_u=$state.texture_tangent_u[0];

/// The handle.
void handle="";

/// The precomputed scale, folding the multiplier and the normalization
/// by the maximum intensity of the profile.
const float _scale=profile.max_intensity>0.?#max(multiplier,0.)/profile.max_intensity:0.;

/// The flags.
static const int df_flags=DF_GLOSSY;
};
@(pure)
auto emissionEvaluate(const &measured_edf this,inline const &EmissionEvaluateParameters params){
preserve tangent_u;
tangent_u=this.tangent_u;
if((this._scale>0.)&&recalculateTangentSpace(params)){
const auto intensity(smdlLightProfileInterpolate(this.profile.ptr,&wi));
if(!(intensity>0.))
return EmissionEvaluateResult(isBlack: true);
return EmissionEvaluateResult(f: this._scale*intensity,pdf: smdlLightProfileDirectionPDF(this.profile.ptr,&wi),);
} else {
return EmissionEvaluateResult(isBlack: true);
}
}
@(pure)
auto emissionSample(const &measured_edf this,inline const &EmissionSampleParameters params){
if(!(this._scale>0.))
return EmissionSampleResult();
float2 xiDirection(xi.x,xi.y);
float3 w(0.);
float pdf(0.);
smdlLightProfileDirectionSample(this.profile.ptr,&xiDirection,&w,&pdf);
if(!((pdf>0.)&(w.z>0.)))
return EmissionSampleResult();
return EmissionSampleResult(wi: calculateTangentSpace(normal,this.tangent_u)*w,isValid: true);
} /// A 1-value tint.
struct tint1:bsdf,edf,hair_bsdf{
/// The tint multiplier.
$(color|float) tint;

/// The base `bsdf`, `edf`, or `hair_bsdf`.
auto base;

/// The flags.
const int df_flags=base.df_flags;
};

/// A 2-value tint.
struct tint2:bsdf{
/// The tint multiplier on reflection.
$(color|float) reflection_tint;

/// The tint multiplier on transmission.
$(color|float) transmission_tint;

/// The base `bsdf`.
bsdf base;

/// The flags.
const int df_flags=base.df_flags;
};

/// Construct 1-value tint of the given `bsdf`.
@(macro)
export auto tint(const auto tint,const bsdf base)=tint1(tint,base);

/// Construct 1-value tint of the given `edf`.
@(macro)
export auto tint(const auto tint,const edf base)=tint1(tint,base);

/// Construct 1-value tint of the given `hair_bsdf`.
@(macro)
export auto tint(const auto tint,const hair_bsdf base)=tint1(tint,base);

/// Construct 2-value tint of the given `bsdf`.
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
auto emissionEvaluate(const &tint1 this,const &EmissionEvaluateParameters params){
auto result(emissionEvaluate(visit &this.base,params));
if(!result.isBlack)
result.f*=this.tint;
return result;
}
@(macro)
auto emissionSample(const &tint1 this,const &EmissionSampleParameters params){
return emissionSample(visit &this.base,params);
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
} /// The weighted layer, blending the `layer` BSDF over the `base` BSDF by
/// the given `weight`, with the layer evaluated using its own `normal`.
export struct weighted_layer:bsdf{
/// The weight.
$(color|float) weight;

/// The layer BSDF.
bsdf layer=bsdf();

/// The base BSDF.
bsdf base=bsdf();

/// The normal to use for the layer.
float3 normal=$state.normal;

/// The chance of sampling the layer BSDF.
///
/// NOTE: If the weight is a `float`, then the chance is the same
/// as the weight. However, if the weight is a `color`, we
/// have to average it down to a single probability.
///
float chance=average(weight);

/// The flags.
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

/// The `color_weighted_layer` is also implemented by the `weighted_layer`.
export typedef weighted_layer color_weighted_layer; /// A thin film layer.
///
/// > Add reflective thin-film interference color to an elemental or
/// > compound BSDF.
///
export struct thin_film:bsdf{
/// The thickness in nanometers.
$(color|float) thickness;

/// The index of refraction.
$(color|float) ior;

/// The base BSDF.
bsdf base=bsdf();

/// The flags.
const int df_flags=base.df_flags;
};

/// Evaluate the thin-film interference factor.
///
/// This is the ratio of the Airy interference reflectance of the coated interface
/// (ambient medium, then a film of the given `thickness` in nanometers and `filmIOR`,
/// then the base medium of the given `baseIOR`, everything relative to the ambient
/// medium) to the plain Fresnel reflectance of the uncoated interface. The MDL
/// specification defines `thin_film` as modulating the Fresnel term of the base BSDF,
/// so the ratio converts an uncoated Fresnel reflectance into the coated one. It
/// reduces to the identity at zero thickness, consistent with the elimination rules
/// in the specification's normal form, and may exceed one at wavelengths with
/// constructive interference.
///
/// NOTE: The cosines through the film and into the base are clamped at zero,
/// so total internal reflection at a buried interface is handled with the
/// correct magnitude but without the phase shift of the evanescent case.
///
@(macro)
auto thinFilmFactor(const auto thickness,const auto filmIOR,const float baseIOR,const float cosTheta1){
const auto eta2(filmIOR);
const auto eta3(baseIOR);
const auto sin2Theta1(#max(1-cosTheta1*cosTheta1,0.));
const auto cosTheta2(#sqrt(#max(1-sin2Theta1/(eta2*eta2),0.)));
const auto cosTheta3(#sqrt(#max(1-sin2Theta1/(eta3*eta3),0.)));
const auto rs12((cosTheta1-eta2*cosTheta2)/(cosTheta1+eta2*cosTheta2));
const auto rp12((eta2*cosTheta1-cosTheta2)/(eta2*cosTheta1+cosTheta2));
const auto rs23((eta2*cosTheta2-eta3*cosTheta3)/(eta2*cosTheta2+eta3*cosTheta3));
const auto rp23((eta3*cosTheta2-eta2*cosTheta3)/(eta3*cosTheta2+eta2*cosTheta3));
const auto phi(2.*$TWO_PI*eta2*cosTheta2*thickness/color($state.wavelength_base));
const auto phase(complex(#cos(phi),#sin(phi)));
const auto Rs(#norm((rs12+rs23*phase)/(1+rs12*rs23*phase)));
const auto Rp(#norm((rp12+rp23*phase)/(1+rp12*rp23*phase)));
const auto rs13((cosTheta1-eta3*cosTheta3)/(cosTheta1+eta3*cosTheta3));
const auto rp13((eta3*cosTheta1-cosTheta3)/(eta3*cosTheta1+cosTheta3));
const float R13(0.5*(rs13*rs13+rp13*rp13));
return R13>EPSILON?0.5*(Rs+Rp)/R13:color(1.);
}

/// The film IOR relative to the incident medium. The user-facing film IOR is
/// absolute. From outside, the incident medium is the exterior. From inside
/// a solid (a backface hit), the incident medium is the base itself, whose
/// absolute index equals `params.ior * params.exterior_ior` after the
/// `finalize` reciprocation.
@(macro)
auto thinFilmIncidentRelativeIOR(const &thin_film this,const auto params){
if(params.hitBackface&!params.thin_walled){
return this.ior/(params.ior*params.exterior_ior);
} else {
return this.ior/params.exterior_ior;
}
}
@(macro)
auto scatterEvaluate(const &thin_film this,const &ScatterEvaluateParameters params){
auto result(scatterEvaluate(visit &this.base,params));
if(!result.isBlack&&params.mode==scatter_reflect){
return ScatterEvaluateResult(f: thinFilmFactor(this.thickness,thinFilmIncidentRelativeIOR(this,params),1/params.ior,#abs(dot(params.wo,halfDirection(params))))*result.f,pdf: result.pdf,);
} else {
return result;
}
}
@(macro)
auto scatterSample(const &thin_film this,const &ScatterSampleParameters params){
auto result(scatterSample(visit &this.base,params));
if((result.mode==scatter_reflect)&bool(result.fDelta)){
*result.fDelta*=thinFilmFactor(this.thickness,thinFilmIncidentRelativeIOR(this,params),1/params.ior,#abs(dot(params.wo,halfDirection(params,&result))));
}
return result;
} /// A fresnel factor.
///
/// > Modifier weighting a base BSDF based on the Fresnel reflection
/// > equation for a complex number IOR, comprising a real number IOR
/// > and an extinction coefficient. This modifier is useful to model
/// > the reflectance behavior of conductors and semi-conductors.
///
export struct fresnel_factor:bsdf{
/// The index of refraction.
$(color|float) ior;

/// The extinction coefficient.
$(color|float) extinction_coefficient;

/// The base BSDF.
bsdf base=bsdf();

/// The flags.
const int df_flags=base.df_flags;
};
@(macro)
auto scatterEvaluate(const &fresnel_factor this,const &ScatterEvaluateParameters params){
auto result(scatterEvaluate(visit &this.base,params));
if(!result.isBlack&&params.mode==scatter_reflect){
return ScatterEvaluateResult(f: specular::conductorFresnel(#abs(dot(params.wo,halfDirection(params))),relativeIOR(params,complex(this.ior,this.extinction_coefficient)))*result.f,pdf: result.pdf,);
}
return result;
}
@(macro)
auto scatterSample(const &fresnel_factor this,const &ScatterSampleParameters params){
auto result(scatterSample(visit &this.base,params));
if((result.mode==scatter_reflect)&bool(result.fDelta)){
*result.fDelta*=specular::conductorFresnel(#abs(dot(params.wo,halfDirection(params,&result))),relativeIOR(params,complex(this.ior,this.extinction_coefficient)));
}
return result;
} /// The directional factor, modulating the base BSDF by a Schlick-style
/// curve from `normal_tint` to `grazing_tint`.
export struct directional_factor:bsdf{
/// The normal tint.
///
/// > Color scaling factor at the normal.
///
$(color|float) normal_tint=1.;

/// The grazing tint.
///
/// > Color scaling factor at the grazing angle.
///
$(color|float) grazing_tint=1.;

/// The exponent.
///
/// > Exponent for directional factor. Default value (5.0) is
/// > from Schlick's approximation.
///
float exponent=5.;

/// The base BSDF.
///
/// > Base BSDF to be modified by directional factor.
///
bsdf base=bsdf();

/// The flags.
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
} /// The measured curve factor, modulating the base BSDF by a measured
/// reflectance curve over the half-vector angle.
export struct measured_curve_factor:bsdf{
/// The curve values.
///
/// > Measured data for the reflection behavior. A 1-d function
/// > measured in the pre-image range from zero to pi/2 with equally
/// > spaced measured reflectance values.
///
/// NOTE: The deferred size is inferred at construction and recovered
/// with `#num` in the implementation.
///
color[] curve_values;

/// The base BSDF.
///
/// > Base BSDF to be modified by the measured reflectance curve.
///
bsdf base=bsdf();

/// The flags.
const int df_flags=base.df_flags;
};

/// Evaluate the reflectivity of a measured curve at `cosAlpha`, the cosine of the
/// angle between the outgoing direction and the half vector, linearly interpolating
/// the equally spaced `curve_values` over `[0, pi/2]`.
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
} /// The measured factor, modulating the base BSDF by a 2-dimensional
/// measured reflectance texture over the half-vector angles.
export struct measured_factor:bsdf{
/// The values.
///
/// > Measured data of type color for the reflection behavior. A 2-d
/// > function measured in the pre-image range `[0,pi/2]^2` with equally
/// > spaced reflectance values, where the texture-space u-coordinate
/// > corresponds to the angle alpha between the incoming direction and
/// > the half-vector h from the microfacet model, and the texture-space
/// > v-coordinate corresponds to the angle beta between the half-vector
/// > h and the shading surface normal.
///
texture_2d values;

/// The base BSDF.
///
/// > Base BSDF to be modified by the measured reflectance values.
///
bsdf base=bsdf();

/// The flags.
const int df_flags=base.df_flags;
};

/// Evaluate the reflectivity of the measured factor, where `cosAlpha` is the cosine
/// of the angle between the outgoing direction and the half vector `h`, and `cosBeta`
/// is the cosine of the angle between `h` and the shading normal.
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
} /// The Fresnel layer, blending the `layer` BSDF over the `base` BSDF by
/// the exact dielectric Fresnel term of the given `ior`.
export struct fresnel_layer:bsdf{
/// The index of refraction.
///
/// NOTE: This is the absolute IOR of the layer interface. It both weights
/// the layer by the dielectric Fresnel term and, non-standardly, defines
/// the refractive interface for the nested `layer` BSDF (overriding the
/// material IOR), so that the Fresnel weight and any refraction in the
/// layer always agree with each other.
///
$(color|float) ior;

/// The weight.
$(color|float) weight=1.;

/// The layer BSDF.
bsdf layer=bsdf();

/// The base BSDF.
bsdf base=bsdf();

/// The normal to use for the layer.
float3 normal=$state.normal;

/// The precomputed average index of refraction.
const float _averageIOR=average(ior);

/// The precomputed average weight.
const float _averageWeight=average(weight);

/// The flags.
const int df_flags=layer.df_flags|base.df_flags;
};
@(macro)
auto scatterEvaluate(const &fresnel_layer this,inline const &ScatterEvaluateParameters params){
const auto cosThetao(dot(wo,this.normal)*#sign(this.normal.z));
const auto cosThetai(dot(wi,this.normal)*#sign(this.normal.z));
if((cosThetao<EPSILON)|((mode==scatter_reflect)&(cosThetai<EPSILON))|((mode==scatter_transmit)&(cosThetai>-EPSILON)))
return ScatterEvaluateResult(isBlack: true);
const auto result0(scatterEvaluate(visit &this.base,params));
preserve normal,ior;
normal=this.normal,ior=relativeIOR(params,this._averageIOR);
const auto result1(scatterEvaluate(visit &this.layer,params));
if(result0.isBlack&result1.isBlack){
return ScatterEvaluateResult(isBlack: true);
} else {
const auto pdfIOR(relativeIOR(params,this._averageIOR));
return ScatterEvaluateResult(f: lerp(result0.f,result1.f,this.weight*specular::dielectricFresnel(dot(wo,halfDirection(params)),relativeIOR(params,this.ior)),),pdf: lerp(result0.pdf,result1.pdf,this._averageWeight*float2(specular::dielectricFresnel(cosThetao,pdfIOR),specular::dielectricFresnel(cosThetai,mode==scatter_reflect?pdfIOR:1/pdfIOR),),),);
}
}
@(macro)
auto scatterSample(const &fresnel_layer this,inline const &ScatterSampleParameters params){
const auto cosTheta(dot(wo,this.normal)*#sign(this.normal.z));
if(cosTheta<EPSILON)
return ScatterSampleResult();
const auto chance(this._averageWeight*specular::dielectricFresnel(cosTheta,relativeIOR(params,this._averageIOR)));
if(monte_carlo::boolSample(&xi.z,chance)){
preserve normal,ior;
normal=this.normal,ior=relativeIOR(params,this._averageIOR);
auto result(scatterSample(visit &this.layer,params));
*result.fDelta*=this.weight*specular::dielectricFresnel(dot(wo,halfDirection(params,&result)),relativeIOR(params,this.ior))/chance if(result.fDelta);
return result;
} else {
auto result(scatterSample(visit &this.base,params));
*result.fDelta*=(1-this.weight*specular::dielectricFresnel(dot(wo,halfDirection(params,&result)),relativeIOR(params,this.ior)))/(1-chance) if(result.fDelta);
return result;
}
}

/// The `color_fresnel_layer` is also implemented by the `fresnel_layer`.
export typedef fresnel_layer color_fresnel_layer; /// A custom-curve layer.
///
/// NOTE: Unlike `fresnel_layer`, this combinator carries no IOR and does
/// not define a refractive interface: a nested transmissive `layer`
/// refracts with the enclosing interface, which is the material IOR by
/// default.
///
export struct custom_curve_layer:bsdf{
/// The reflectivity at normal incidence.
$(color|float) normal_reflectivity;

/// The reflectivity at grazing incidence.
$(color|float) grazing_reflectivity=1.;

/// The exponent.
float exponent=5.;

/// The weight.
$(color|float) weight=1.;

/// The layer BSDF.
bsdf layer=bsdf();

/// The base BSDF.
bsdf base=bsdf();

/// The normal to use for the layer.
float3 normal=$state.normal;

/// The precomputed average normal reflectivity.
const float _averageNormalReflectivity=average(normal_reflectivity);

/// The precomputed average grazing reflectivity.
const float _averageGrazingReflectivity=average(grazing_reflectivity);

/// The precomputed average weight.
const float _averageWeight=average(weight);

/// The flags.
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

/// The `color_custom_curve_layer` is also implemented by the `custom_curve_layer`.
export typedef custom_curve_layer color_custom_curve_layer; /// A measured-curve layer.
///
/// NOTE: Unlike `fresnel_layer`, this combinator carries no IOR and does
/// not define a refractive interface: a nested transmissive `layer`
/// refracts with the enclosing interface, which is the material IOR by
/// default.
///
export struct measured_curve_layer:bsdf{
/// The curve values.
///
/// > Measured data for the reflection behavior. A 1-d function
/// > measured in the pre-image range from zero to pi/2 with equally
/// > spaced measured reflectance values.
///
color[] curve_values;

/// The weight.
$(color|float) weight=1.;

/// The layer BSDF.
bsdf layer=bsdf();

/// The base BSDF.
bsdf base=bsdf();

/// The normal to use for the layer.
float3 normal=$state.normal;

/// The precomputed average weight.
const float _averageWeight=average(weight);

/// The flags.
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

/// The `color_measured_curve_layer` is also implemented by the `measured_curve_layer`.
export typedef measured_curve_layer color_measured_curve_layer;
tag component;

/// The weighted BSDF component for use with the mixers.
export struct bsdf_component:component{
float weight=0.;       ///< The weight.
bsdf component=bsdf(); ///< The component BSDF.
float chance=weight;   ///< The sampling chance. NOTE: This is non-standard!
};
/// The weighted EDF component for use with the mixers.
export struct edf_component:component{
float weight=0.;     ///< The weight.
edf component=edf(); ///< The component EDF.
float chance=weight; ///< The sampling chance. NOTE: This is non-standard!
};
/// The weighted VDF component for use with the mixers.
export struct vdf_component:component{
float weight=0.;     ///< The weight.
vdf component=vdf(); ///< The component VDF.
float chance=weight; ///< The sampling chance. NOTE: This is non-standard!
};
struct component_mix:bsdf,edf,vdf{
component[] components;
int df_flags=0;
};

/// Constructs a mixture of the given components, normalizing the weights
/// when they sum to more than 1.
@(macro)
export auto normalized_mix(component[<N>] components){
int df_flags(0);
float total_weight(0);
float total_chance(0);
for(int i=0;i<N;i++){
auto component(&components[i]);
component.weight=#max(component.weight,0.);
component.chance=#max(component.chance,0.);
total_weight+=component.weight;
total_chance+=component.chance;
df_flags|=component.component.df_flags;
}
if(total_weight>1.)
total_weight=1./total_weight;
else
total_weight=1.;
total_chance=1./total_chance if(total_chance>0.);
for(int i=0;i<N;i++){
auto component(&components[i]);
component.weight*=total_weight;
component.chance*=total_chance;
}
return component_mix(components,df_flags);
}

/// Constructs a mixture of the given components, clamping the running
/// weight sum at 1 in declaration order.
@(macro)
export auto clamped_mix(component[<N>] components){
int df_flags(0);
float total_weight(0);
float total_chance(0);
for(int i=0;i<N;i++){
auto component(&components[i]);
component.weight=#max(component.weight,0.);
component.chance=#max(component.chance,0.);
if(total_weight+component.weight<1.){
total_weight+=component.weight;
total_chance+=component.chance;
df_flags|=component.component.df_flags;
} else {
component.weight=1.-total_weight;
for(int j=i+1;j<N;j++){
components[j].weight=0;
components[j].chance=0;
}
break;
}
}
total_chance=1./total_chance if(total_chance>0.);
for(int i=0;i<N;i++){
components[i].chance*=total_chance;
}
return component_mix(components,df_flags);
}

/// Constructs a mixture of the given components without normalizing or
/// clamping the weights.
@(macro)
export auto unbounded_mix(component[<N>] components){
int df_flags(0);
float total_chance(0);
for(int i=0;i<N;i++){
auto component(&components[i]);
component.weight=#max(component.weight,0.);
component.chance=#max(component.chance,0.);
total_chance+=component.chance;
df_flags|=component.df_flags;
}
total_chance=1./total_chance if(total_chance>0.);
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
@(macro)
auto emissionEvaluate(const &component_mix this,const &EmissionEvaluateParameters params){
auto result(EmissionEvaluateResult(f: color(0),isBlack: true));
for(int i=0;i<#num(this.components);i++){
visit component in this.components[i]{
auto component_result(emissionEvaluate(visit &component.component,params));
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
auto emissionSample(const &component_mix this,const &EmissionSampleParameters params){
const auto xi(&params.xi.z);
for(int i=0;i<#num(this.components);i++){
visit component in this.components[i]{
if(!(*xi<component.chance)){
*xi-=component.chance;
} else {
*xi/=component.chance;
return emissionSample(visit &component.component,params);
}
}
}
return EmissionSampleResult();
} /// The anisotropic VDF, being the Henyey-Greenstein phase function with
/// the given directional bias.
export struct anisotropic_vdf:vdf{
/// The directional bias `g` in `(-1, 1)`: negative is backward
/// scattering, `0` is isotropic, and positive is forward scattering.
float directional_bias=0.;

/// The handle.
void handle="";

/// The flags.
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
const auto p=(1.-g*g)/(4.*$PI*(denom:=1.+g*g+2.*g*cosTheta)*#sqrt(denom));
return ScatterEvaluateResult(f: p,pdf: float2(p));
}
@(macro)
auto scatterSample(const &anisotropic_vdf this,inline const &ScatterSampleParameters params){
const auto g=this.directional_bias;
const auto cosTheta=#abs(g)<1e-3?1.-2.*xi.x:-(1.+g*g-#pow((1.-g*g)/(1.+g*(1.-2.*xi.x)),2))/(2.*g);
const auto sinTheta=#sqrt(#max(0.,1.-cosTheta*cosTheta));
const auto phi=2.*$PI*xi.y;
return ScatterSampleResult(wi: orthonormalBasis(wo)*float3(sinTheta*#cos(phi),sinTheta*#sin(phi),cosTheta),mode: scatter_reflect_transmit);
}
@(macro)
export int _scatterEvaluate(
const &_MaterialInstance instance,
const &float3 woWorld, ///< The outgoing direction in world space
const &float3 wiWorld, ///< The incoming direction in world space
const &float pdfFwd,   ///< output: The PDF of sampling `wi` from `wo`
const &float pdfRev,   ///< output: The PDF of sampling `wo` from `wi`
const &float f,        ///< output: The scattering function
){
auto params=ScatterEvaluateParameters(
isImportance: (instance.flags&1)!=0,
ior: instance.exterior_ior/instance.ior,
exterior_ior: instance.exterior_ior,
wo0: normalize((*woWorld)*instance.tangent_to_world),
wi0: normalize((*wiWorld)*instance.tangent_to_world),
normal: normalize(instance.geometry.normal),
thin_walled: instance.ptr.thin_walled,
);
auto result=#is_default(instance.ptr.backface)||!params.hitBackface?scatterEvaluate(visit &instance.ptr.surface.scattering,&params):scatterEvaluate(visit &instance.ptr.backface.scattering,&params);
visit result in result{
if(result.isBlack){
*pdfFwd=0.;
*pdfRev=0.;
for(int i=0;i<$WAVELENGTH_BASE_MAX;i++)
f[i]=0.;
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
const &float4 xi,      ///< The canonical random sample in `[0,1]^4`
const &float3 woWorld, ///< The outgoing direction in world space
const &float3 wiWorld, ///< output: The incoming direction in world space
const &float pdfFwd,   ///< output: The PDF of sampling `wi` from `wo`
const &float pdfRev,   ///< output: The PDF of sampling `wo` from `wi`
const &float f,        ///< output: The scattering function
const &int isDelta,    ///< output: Is delta direction?
){
auto wo=normalize((*woWorld)*instance.tangent_to_world);
auto params=ScatterSampleParameters(
isImportance: (instance.flags&1)!=0,
xi: *xi,
wo0: wo,
ior: instance.exterior_ior/instance.ior,
exterior_ior: instance.exterior_ior,
normal: normalize(instance.geometry.normal),
thin_walled: instance.ptr.thin_walled,
);
auto result=#is_default(instance.ptr.backface)||!params.hitBackface?scatterSample(visit &instance.ptr.surface.scattering,&params):scatterSample(visit &instance.ptr.backface.scattering,&params);
visit result in result{
const auto wi=#select(params.hitBackface,-result.wi,result.wi);
if(result.mode==scatter_none||((wo.z<0.)==(wi.z<0.))!=(result.mode==scatter_reflect)){
*pdfFwd=0.;
*pdfRev=0.;
for(int i=0;i<$WAVELENGTH_BASE_MAX;i++)
f[i]=0.;
return false;
}
*wiWorld=normalize(instance.tangent_to_world*wi);
if((*isDelta=bool(result.fDelta))){
*pdfFwd=1.;
*pdfRev=1.;
#memcpy(f,&*result.fDelta,#sizeof(float)*$WAVELENGTH_BASE_MAX);
return true;
} else {
return _scatterEvaluate(instance,woWorld,wiWorld,pdfFwd,pdfRev,f);
}
}
}
@(macro)
export float _volumeScatterEvaluate(const &_MaterialInstance instance,const &float3 woWorld, ///< The outgoing direction in world space
const &float3 wiWorld,                                                                       ///< The incoming direction in world space
){
auto params=ScatterEvaluateParameters(
isImportance: 0,
wo0: normalize(*woWorld),
wi0: normalize(*wiWorld),
hitBackface: false,
ior: 1.,
);
return scatterEvaluate(visit &instance.ptr.volume.scattering,&params).f;
}
@(macro)
export float _volumeScatterSample(
const &_MaterialInstance instance,
const &float4 xi,      ///< The canonical random sample in `[0,1]^4`
const &float3 woWorld, ///< The outgoing direction in world space
const &float3 wiWorld, ///< output: The incoming direction in world space
){
auto wo=normalize(*woWorld);
auto params=ScatterSampleParameters(
isImportance: false,
xi: *xi,
wo0: wo,
hitBackface: false,
ior: 1.,
);
auto result=scatterSample(visit &instance.ptr.volume.scattering,&params);
if(result.mode==scatter_none){
return 0.;
}
*wiWorld=normalize(result.wi);
return _volumeScatterEvaluate(instance,woWorld,wiWorld);
}

/// Calculate the average emission intensities of the front and back sides.
@(macro)
float2 _emissionSideWeights(const &_MaterialInstance instance){
float frontWeight=0.;
float backWeight=0.;
if$(!#is_default(instance.ptr.surface.emission.emission)){
frontWeight=#max(average(instance.ptr.surface.emission.intensity),0.);
}
if(instance.ptr.thin_walled){
if$(!#is_default(instance.ptr.backface)){
if$(!#is_default(instance.ptr.backface.emission.emission)){
backWeight=#max(average(instance.ptr.backface.emission.intensity),0.);
}
} else {
backWeight=frontWeight;
}
}
return float2(frontWeight,backWeight);
}

/// Evaluate the emission of the given `material_surface` side, weighting
/// the PDF by the side chance and applying the intensity.
@(macro)
int _emissionEvaluateSide(
const &material_surface side,
const float chance,
const &EmissionEvaluateParameters params,
const &float pdf,
const &float Le,
){
if(!(chance>0.))
return false;
auto result=emissionEvaluate(visit &side.emission.emission,params);
visit result in result{
if(result.isBlack)
return false;
*pdf=chance*result.pdf;
color LeResult(color(side.emission.intensity)*result.f);
#memcpy(Le,&LeResult,#sizeof(float)*$WAVELENGTH_BASE_MAX);
return true;
}
}
@(macro)
export int _emissionEvaluate(
const &_MaterialInstance instance,
const &float3 wiWorld, ///< The emission direction in world space
const &float pdf,      ///< output: The PDF of sampling `wiWorld`
const &float Le,       ///< output: The emitted radiance
){
*pdf=0.;
for(int i=0;i<$WAVELENGTH_BASE_MAX;i++)
Le[i]=0.;
const auto weights=_emissionSideWeights(instance);
const auto totalWeight=weights.x+weights.y;
if(!(totalWeight>0.))
return false;
auto params=EmissionEvaluateParameters(wi0: normalize((*wiWorld)*instance.tangent_to_world),normal: normalize(instance.geometry.normal),);
if(!params.hitBackface){
return _emissionEvaluateSide(&instance.ptr.surface,weights.x/totalWeight,&params,pdf,Le);
} else {
if(!instance.ptr.thin_walled)
return false;
if$(!#is_default(instance.ptr.backface)){
return _emissionEvaluateSide(&instance.ptr.backface,weights.y/totalWeight,&params,pdf,Le);
} else {
return _emissionEvaluateSide(&instance.ptr.surface,weights.y/totalWeight,&params,pdf,Le);
}
}
}
@(macro)
export int _emissionSample(
const &_MaterialInstance instance,
const &float4 xi,      ///< The canonical random sample in `[0,1]^4`
const &float3 wiWorld, ///< output: The emission direction in world space
const &float pdf,      ///< output: The PDF of sampling `wiWorld`
const &float Le,       ///< output: The emitted radiance
){
*pdf=0.;
for(int i=0;i<$WAVELENGTH_BASE_MAX;i++)
Le[i]=0.;
const auto weights=_emissionSideWeights(instance);
const auto totalWeight=weights.x+weights.y;
if(!(totalWeight>0.))
return false;
auto params=EmissionSampleParameters(xi: *xi,normal: normalize(instance.geometry.normal),);
const bool sampleFront=monte_carlo::boolSample(&params.xi.w,weights.x/totalWeight);
auto result=return_from{
if(sampleFront){
return emissionSample(visit &instance.ptr.surface.emission.emission,&params);
} else if$(!#is_default(instance.ptr.backface)){
return emissionSample(visit &instance.ptr.backface.emission.emission,&params);
} else {
return emissionSample(visit &instance.ptr.surface.emission.emission,&params);
}
};
visit result in result{
if(!result.isValid||!(result.wi.z>0.))
return false;
const auto wiNatural=sampleFront?result.wi:-result.wi;
*wiWorld=normalize(instance.tangent_to_world*wiNatural);
return _emissionEvaluate(instance,wiWorld,pdf,Le);
}
}
)*";

static const char *const limits = R"*(/// Numeric limits of the builtin arithmetic types, following the MDL
/// specification.
#smdl

/// The most negative value of type `int`.
export const int INT_MIN=$INT_MIN;

/// The largest value of type `int`.
export const int INT_MAX=$INT_MAX;

/// The smallest positive normalized value of type `float`.
export const float FLOAT_MIN=$FLOAT_MIN;

/// The largest finite value of type `float`.
export const float FLOAT_MAX=$FLOAT_MAX;

/// The smallest positive normalized value of type `double`.
export const double DOUBLE_MIN=$DOUBLE_MIN;

/// The largest finite value of type `double`.
export const double DOUBLE_MAX=$DOUBLE_MAX;
)*";

static const char *const math = R"*(/// Elementary math functions, following the MDL specification. Most of
/// these are generic and componentwise: they accept scalars, vectors,
/// matrices, and `color` where sensible, applying the operation to each
/// component.
#smdl

/// The constant `pi`.
export const float PI=$PI;

/// The constant `2 * pi`.
export const float TWO_PI=$TWO_PI;

/// The constant `pi / 2`.
export const float HALF_PI=$HALF_PI;

/// The absolute value.
@(macro)
export auto abs(const auto a)=#abs(a);

/// Is every component true?
@(macro)
export auto all(const auto a)=#all(a);

/// Is any component true?
@(macro)
export auto any(const auto a)=#any(a);

/// The maximum of `a` and `b`.
@(macro)
export auto max(const auto a,const auto b)=#max(a,b);

/// The minimum of `a` and `b`.
@(macro)
export auto min(const auto a,const auto b)=#min(a,b);

/// The value `a` clamped to the range `[min, max]`.
@(macro)
export auto clamp(const auto a,const auto min,const auto max)=#max(min,#min(a,max));

/// The value `a` clamped to the range `[0, 1]`.
@(macro)
export auto saturate(const auto a)=clamp(a,0.,1.);

/// The value rounded down toward negative infinity.
@(macro)
export auto floor(const auto a)=#floor(a);

/// The value rounded up toward positive infinity.
@(macro)
export auto ceil(const auto a)=#ceil(a);

/// The value rounded to the nearest integer.
@(macro)
export auto round(const auto a)=#round(a);

/// The value rounded toward zero.
@(macro)
export auto trunc(const auto a)=#trunc(a);

/// The fractional part, `a - floor(a)`.
@(macro)
export auto frac(const auto a)=a-#floor(a);

/// The remainder of `a` divided by `b`.
@(macro)
export auto fmod(const auto a,const auto b)=a%b;

/// The integral and fractional parts as an array of two, `[trunc(a), a - trunc(a)]`.
@(macro)
export auto modf(const auto a)=auto[2](a0:=#trunc(a),a-a0);

/// Is neither infinite nor NaN?
@(macro)
export auto isfinite(const auto a)=#isfpclass(a,0b0111111000);

/// Is a normalized floating-point number, i.e., finite and neither zero nor subnormal?
@(macro)
export auto isnormal(const auto a)=#isfpclass(a,0b0100001000);

/// Is positive or negative infinity?
@(macro)
export auto isinf(const auto a)=#isfpclass(a,0b1000000100);

/// Is NaN?
@(macro)
export auto isnan(const auto a)=#isfpclass(a,0b0000000011);

/// The sign of the value.
@(macro)
export auto sign(const auto a)=#sign(a);

/// The square root.
@(macro)
export auto sqrt(const auto a)=#sqrt(a);

/// The reciprocal of the square root.
@(macro)
export auto rsqrt(const auto a)=1./#sqrt(a);

/// The power `a` raised to `b`.
@(macro)
export auto pow(const auto a,const auto b)=#pow(a,b);

/// The cosine of an angle in radians.
@(macro)
export auto cos(const auto a)=#cos(a);

/// The sine of an angle in radians.
@(macro)
export auto sin(const auto a)=#sin(a);

/// The tangent of an angle in radians.
@(macro)
export auto tan(const auto a)=#tan(a);

/// The arccosine in radians.
@(macro)
export auto acos(const auto a)=#acos(a);

/// The arcsine in radians.
@(macro)
export auto asin(const auto a)=#asin(a);

/// The arctangent in radians.
@(macro)
export auto atan(const auto a)=#atan(a);

/// The arctangent of `y / x` in radians, using the signs to select the quadrant.
@(macro)
export auto atan2(const auto y,const auto x)=#atan2(y,x);

/// The hyperbolic cosine.
@(macro)
export auto cosh(const auto a)=#cosh(a);

/// The hyperbolic sine.
@(macro)
export auto sinh(const auto a)=#sinh(a);

/// The hyperbolic tangent.
@(macro)
export auto tanh(const auto a)=#tanh(a);

/// The sine and cosine of an angle in radians as an array of two, `[sin(a), cos(a)]`.
@(macro)
export auto sincos(const auto a)=auto[2](#sin(a),#cos(a));

/// Converts degrees to radians.
@(macro)
export auto radians(const auto a)=a*(PI/180.);

/// Converts radians to degrees.
@(macro)
export auto degrees(const auto a)=a*(180./PI);

/// The natural exponential `e^a`.
@(macro)
export auto exp(const auto a)=#exp(a);

/// The base-2 exponential `2^a`.
@(macro)
export auto exp2(const auto a)=#exp2(a);

/// The base-10 exponential `10^a`.
@(macro)
export auto exp10(const auto a)=#exp10(a);

/// The natural logarithm.
@(macro)
export auto log(const auto a)=#log(a);

/// The base-2 logarithm.
@(macro)
export auto log2(const auto a)=#log2(a);

/// The base-10 logarithm.
@(macro)
export auto log10(const auto a)=#log10(a);

/// The smallest component of the value.
@(macro)
export auto min_value(const auto a)=#min_value(a);

/// The largest component of the value.
@(macro)
export auto max_value(const auto a)=#max_value(a);

/// The wavelength in nanometers at which the color attains its smallest component.
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

/// The wavelength in nanometers at which the color attains its largest component.
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

/// The average of all components of the value.
@(macro)
export auto average(const auto a)=#sum(a)/#num(a);

/// The linear interpolation from `a` to `b` by factor `l`.
@(macro)
export auto lerp(const auto a,const auto b,const auto l)=(1.-l)*a+l*b;

/// The step function, `0` where `b < a` and `1` elsewhere.
@(macro)
export auto step(const auto a,const auto b)=#select(b<a,0.,1.);

/// The smooth Hermite interpolation from `a` to `b` by factor `l` clamped to `[0, 1]`.
@(macro)
export auto smoothstep(const auto a,const auto b,const auto l){
const auto t(saturate(l));
const auto s(1-t);
return s*s*(1+2*t)*a+t*t*(1+2*s)*b;
}

/// The dot product.
@(macro)
export auto dot(const auto a,const auto b)=#sum(a*b);

/// The Euclidean length.
@(macro)
export auto length(const auto a)=#sqrt(#sum(a*a));

/// The vector scaled to unit length.
@(macro)
export auto normalize(const auto a)=a*(1/length(a));

/// The Euclidean distance between `a` and `b`.
@(macro)
export auto distance(const auto a,const auto b)=length(b-a);

/// The cross product.
@(macro)
export auto cross(const auto a,const auto b)=a.yzx*b.zxy-a.zxy*b.yzx;

/// The matrix transpose.
@(macro)
export auto transpose(const auto a)=#transpose(a);

/// The luminance of the RGB value, using the Rec. 709 coefficients.
@(macro)
export float luminance(const float3 a)=dot(float3(0.2126,0.7152,0.0722),a);

/// The luminance of the spectral color, weighted by the CIE Y curve.
@(noinline)
export float luminance(const color a){
float result(0.);
for(int i=0;i<$WAVELENGTH_BASE_MAX;++i){
result+=_wyman_y($state.wavelength_base[i])*a[i];
}
return result/$WAVELENGTH_BASE_MAX;
}

/// The blackbody emission spectrum for the given temperature in kelvin.
@(noinline)
export color blackbody(const float temperature){
const auto t(color($state.wavelength_base)*(temperature/14387e3));
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

/// The color evaluated at the given wavelength in nanometers by piecewise-linear interpolation.
export float eval_at_wavelength(color a,float wavelength){
if$($WAVELENGTH_BASE_MAX==1){
return a[0];
} else {
return _polyline_lerp($WAVELENGTH_BASE_MAX,&$state.wavelength_base[0],&a[0],wavelength);
}
}
)*";

static const char *const scene = R"*(/// Scene data lookup, following the MDL specification. Values are looked
/// up by name in renderer-provided scene data, e.g., per-vertex or
/// per-object attributes, falling back to the given default when the name
/// is unavailable.
#smdl
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

/// Is scene data with the given name available?
@(macro)
export bool data_isvalid(const string name)=smdlDataExists($SCENE_DATA,name)!=0;

/// Returns the named scene data as `int`, else `default_value`.
@(macro)
export int data_lookup_int(const string name,int default_value=int())=data_lookup(name,default_value);

/// Returns the named scene data as `int2`, else `default_value`.
@(macro)
export int2 data_lookup_int2(const string name,int2 default_value=int2())=data_lookup(name,default_value);

/// Returns the named scene data as `int3`, else `default_value`.
@(macro)
export int3 data_lookup_int3(const string name,int3 default_value=int3())=data_lookup(name,default_value);

/// Returns the named scene data as `int4`, else `default_value`.
@(macro)
export int4 data_lookup_int4(const string name,int4 default_value=int4())=data_lookup(name,default_value);

/// Returns the named scene data as `float`, else `default_value`.
@(macro)
export float data_lookup_float(const string name,float default_value=float())=data_lookup(name,default_value);

/// Returns the named scene data as `float2`, else `default_value`.
@(macro)
export float2 data_lookup_float2(const string name,float2 default_value=float2())=data_lookup(name,default_value);

/// Returns the named scene data as `float3`, else `default_value`.
@(macro)
export float3 data_lookup_float3(const string name,float3 default_value=float3())=data_lookup(name,default_value);

/// Returns the named scene data as `float4`, else `default_value`.
@(macro)
export float4 data_lookup_float4(const string name,float4 default_value=float4())=data_lookup(name,default_value);

/// Returns the named scene data as `color`, else `default_value`.
@(macro)
export color data_lookup_color(const string name,color default_value=color())=data_lookup(name,default_value);
)*";

static const char *const state = R"*(/// The renderer state in the current shading context, following the MDL
/// specification. Unless documented otherwise, positions, normals, and
/// tangents are in internal space, which SMDL takes to be tangent space.
#smdl
import ::math::*;

/// The coordinate space, used by the `transform*` functions.
export enum coordinate_space{coordinate_internal=0, ///< The internal space, which SMDL takes to be tangent space.
coordinate_object=1,                                ///< The object space.
coordinate_world=2,                                 ///< The world space.
};

/// The position of the shading point.
@(macro)
export float3 position()=$state.position;

/// The shading normal, possibly perturbed by bump or normal mapping.
@(macro)
export float3 normal()=$state.normal;

/// The true geometric surface normal.
@(macro)
export float3 geometry_normal()=$state.geometry_normal;

/// The motion vector of the shading point.
@(macro)
export float3 motion()=$state.motion;

/// The number of available texture spaces.
@(macro)
export int texture_space_max()=$state.texture_space_max;

/// The texture coordinates of texture space `i`.
@(macro)
export float3 texture_coordinate(const int i)=$state.texture_coordinate[i];

/// The tangent in the direction of increasing U in texture space `i`.
@(macro)
export float3 texture_tangent_u(const int i)=$state.texture_tangent_u[i];

/// The tangent in the direction of increasing V in texture space `i`.
@(macro)
export float3 texture_tangent_v(const int i)=$state.texture_tangent_v[i];

/// The geometric tangent in the direction of increasing U in texture space `i`.
@(macro)
export float3 geometry_tangent_u(const int i)=$state.geometry_tangent_u[i];

/// The geometric tangent in the direction of increasing V in texture space `i`.
@(macro)
export float3 geometry_tangent_v(const int i)=$state.geometry_tangent_v[i];

/// The shading tangent space of texture space `i` as the matrix of tangent U, tangent V, and normal.
@(macro)
export float3x3 tangent_space(const int i)=float3x3($state.texture_tangent_u[i],$state.texture_tangent_v[i],$state.normal);

/// The geometric tangent space of texture space `i` as the matrix of tangent U, tangent V, and normal.
@(macro)
export float3x3 geometry_tangent_space(const int i)=float3x3($state.geometry_tangent_u[i],$state.geometry_tangent_v[i],$state.geometry_normal);

/// The object ID provided by the renderer.
@(macro)
export int object_id()=$state.object_id;

/// The lookup direction in environment lookups. NOTE: Not implemented yet, always zero.
@(macro)
export float3 direction()=float3(0.,0.,0.);

/// The animation time of the current sample.
@(macro)
export float animation_time()=$state.animation_time;

/// The compile-time number of wavelengths in spectral calculations.
export const int WAVELENGTH_BASE_MAX=$WAVELENGTH_BASE_MAX;

/// The minimum supported wavelength in nanometers.
@(macro)
export float wavelength_min()=$state.wavelength_min;

/// The maximum supported wavelength in nanometers.
@(macro)
export float wavelength_max()=$state.wavelength_max;

/// The wavelengths in nanometers that spectral `color` values are sampled at.
@(macro)
export float[WAVELENGTH_BASE_MAX] wavelength_base()=$state.wavelength_base;

/// The conversion factor from scene units to meters.
@(macro)
export float meters_per_scene_unit()=$state.meters_per_scene_unit;

/// The conversion factor from meters to scene units.
@(macro)
export float scene_units_per_meter()=1./$state.meters_per_scene_unit;
@(pure macro)
float4x4 affine_inverse(const float4x4 matrix){
return float4x4(
float4(matrix[0].x,matrix[1].x,matrix[2].x,0.),
float4(matrix[0].y,matrix[1].y,matrix[2].y,0.),
float4(matrix[0].z,matrix[1].z,matrix[2].z,0.),
float4(-#sum(matrix[0]*matrix[3]),-#sum(matrix[1]*matrix[3]),-#sum(matrix[2]*matrix[3]),1.),
);
}

/// The affine transform matrix from coordinate space `from` to coordinate space `to`.
@(macro)
export float4x4 transform(const coordinate_space from,const coordinate_space to){
if(from==to){
return float4x4(1.);
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
return float4x4(1.);
}
}

/// Transforms a point from coordinate space `from` to coordinate space `to`.
@(macro)
export float3 transform_point(const coordinate_space from,const coordinate_space to,const float3 point){
return from==to?point:(transform(from,to)*float4(point,1)).xyz;
}

/// Transforms a vector from coordinate space `from` to coordinate space `to`.
@(macro)
export float3 transform_vector(const coordinate_space from,const coordinate_space to,const float3 vector){
return from==to?vector:(transform(from,to)*float4(vector,0)).xyz;
}

/// Transforms a normal from coordinate space `from` to coordinate space `to`, using the inverse transpose.
@(macro)
export float3 transform_normal(const coordinate_space from,const coordinate_space to,const float3 normal){
return from==to?normal:(float4(normal,0)*transform(to,from)).xyz;
}

/// Transforms a scalar distance from coordinate space `from` to coordinate space `to`. NOTE: Not implemented yet, returns `scale` unchanged.
@(macro)
export float transform_scale(const coordinate_space from,const coordinate_space to,const float scale){
return 1.*scale;
}
)*";

static const char *const std = R"*(/// The catch-all standard module, re-exporting the entire public API of
/// the other standard modules for convenience.
#smdl
export using ::debug import *;
export using ::df import *;
export using ::limits import *;
export using ::math import *;
export using ::scene import *;
export using ::state import *;
export using ::tex import *;
)*";

static const char *const tex = R"*(/// Texture lookup functions, following the MDL specification, plus
/// non-standard support for Ptex textures.
#smdl
import ::math::lerp;

/// The gamma mode, describing how texel values convert to linear space.
export enum gamma_mode{gamma_default=0, ///< The default, treated the same as `gamma_linear`.
gamma_linear=0,                         ///< Linear, no conversion.
gamma_srgb=1,                           ///< The sRGB decoding to linear.
};
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

/// The width in texels, of the given uv-tile for uv-tilesets.
@(pure macro)
export int width(const texture_2d tex,const int2 uv_tile=int2(0)){
const auto i(getTileIndex(tex,uv_tile));
return i<0?0:tex.tile_extents[i].x;
}

/// The width in texels. NOTE: Not implemented yet, always zero.
@(pure macro)
export int width(const texture_3d tex)=0; /// The width in texels. NOTE: Not implemented yet, always zero.
@(pure macro)
export int width(const texture_cube tex)=0; /// The height in texels, of the given uv-tile for uv-tilesets.
@(pure macro)
export int height(const texture_2d tex,const int2 uv_tile=int2(0)){
const auto i(getTileIndex(tex,uv_tile));
return i<0?0:tex.tile_extents[i].y;
}

/// The height in texels. NOTE: Not implemented yet, always zero.
@(pure macro)
export int height(const texture_3d tex)=0; /// The height in texels. NOTE: Not implemented yet, always zero.
@(pure macro)
export int height(const texture_cube tex)=0; /// Is the texture valid, i.e., backed by loaded image data?
@(pure macro)
export bool texture_isvalid(const texture_2d tex)=bool(tex.tile_buffers[0]);

/// Is the texture valid? NOTE: Not implemented yet, always false.
@(pure macro)
export bool texture_isvalid(const texture_3d tex)=false; /// Is the texture valid? NOTE: Not implemented yet, always false.
@(pure macro)
export bool texture_isvalid(const texture_cube tex)=false; /// Is the texture valid, i.e., backed by a loaded Ptex file?
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

/// The texel at integer `coord` as `float4`, with gamma applied but no filtering or wrapping.
@(pure macro)
export float4 texel_float4(const texture_2d tex,const int2 coord,const int2 uv_tile=int2(0)){
return applyGamma(tex.gamma,#unpack_float4(texel_fetch(tex,coord,uv_tile)));
}

/// The texel at integer `coord` as `float3`, with gamma applied but no filtering or wrapping.
@(pure macro)
export float3 texel_float3(const texture_2d tex,const int2 coord,const int2 uv_tile=int2(0)){
return applyGamma(tex.gamma,#unpack_float4(texel_fetch(tex,coord,uv_tile)).xyz);
}

/// The texel at integer `coord` as `float2`, with gamma applied but no filtering or wrapping.
@(pure macro)
export float2 texel_float2(const texture_2d tex,const int2 coord,const int2 uv_tile=int2(0)){
return applyGamma(tex.gamma,#unpack_float4(texel_fetch(tex,coord,uv_tile)).xy);
}

/// The texel at integer `coord` as `float`, with gamma applied but no filtering or wrapping.
@(pure macro)
export float texel_float(const texture_2d tex,const int2 coord,const int2 uv_tile=int2(0)){
return applyGamma(tex.gamma,#unpack_float4(texel_fetch(tex,coord,uv_tile)).x);
}

/// The texel at integer `coord` as `color`, with gamma applied but no filtering or wrapping.
@(pure macro)
export color texel_color(const texture_2d tex,const int2 coord,const int2 uv_tile=int2(0)){
return color(texel_float3(tex,coord,uv_tile));
}

/// The wrap mode, describing how out-of-range texture coordinates are handled.
export enum wrap_mode{
wrap_clamp=0,           ///< Clamp to the edge.
wrap_repeat=1,          ///< Repeat, keeping only the fractional part.
wrap_mirrored_repeat=2, ///< Repeat, mirroring on every other repetition.
wrap_clip=3,            ///< Clip, so lookups outside `[0, 1)` return zero.
};
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

/// The bilinearly filtered lookup at `coord` as `float4`, honoring the wrap
/// modes and crop windows, which are ignored for uv-tilesets.
@(pure)
export float4 lookup_float4(
const texture_2d tex,
float2 coord,
const wrap_mode wrap_u=wrap_repeat,
const wrap_mode wrap_v=wrap_repeat,
const float2 crop_u=float2(0.,1.),
const float2 crop_v=float2(0.,1.),
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

/// The bilinearly filtered lookup at `coord` as `float3`, honoring the wrap
/// modes and crop windows, which are ignored for uv-tilesets.
@(pure macro)
export float3 lookup_float3(
const texture_2d tex,
const float2 coord,
const wrap_mode wrap_u=wrap_repeat,
const wrap_mode wrap_v=wrap_repeat,
const float2 crop_u=float2(0.,1.),
const float2 crop_v=float2(0.,1.),
)=lookup_float4(tex,coord,wrap_u,wrap_v,crop_u,crop_v).xyz;

/// The bilinearly filtered lookup at `coord` as `float2`, honoring the wrap
/// modes and crop windows, which are ignored for uv-tilesets.
@(pure macro)
export float2 lookup_float2(
const texture_2d tex,
const float2 coord,
const wrap_mode wrap_u=wrap_repeat,
const wrap_mode wrap_v=wrap_repeat,
const float2 crop_u=float2(0.,1.),
const float2 crop_v=float2(0.,1.),
)=lookup_float4(tex,coord,wrap_u,wrap_v,crop_u,crop_v).xy;

/// The bilinearly filtered lookup at `coord` as `float`, honoring the wrap
/// modes and crop windows, which are ignored for uv-tilesets.
@(pure macro)
export float lookup_float(
const texture_2d tex,
const float2 coord,
const wrap_mode wrap_u=wrap_repeat,
const wrap_mode wrap_v=wrap_repeat,
const float2 crop_u=float2(0.,1.),
const float2 crop_v=float2(0.,1.),
)=lookup_float4(tex,coord,wrap_u,wrap_v,crop_u,crop_v).x;

/// The bilinearly filtered lookup at `coord` as `color`, honoring the wrap
/// modes and crop windows, which are ignored for uv-tilesets.
@(pure macro)
export color lookup_color(
const texture_2d tex,
const float2 coord,
const wrap_mode wrap_u=wrap_repeat,
const wrap_mode wrap_v=wrap_repeat,
const float2 crop_u=float2(0.,1.),
const float2 crop_v=float2(0.,1.),
)=color(lookup_float4(tex,coord,wrap_u,wrap_v,crop_u,crop_v).xyz);
@(foreign)
void smdlPtexEvaluate(&void tex,int gamma,int first,int num,&float result);

/// The Ptex lookup of four channels starting at `channel` as `float4`. NOTE: This is non-standard!
@(macro)
export float4 lookup_float4(const texture_ptex tex,const int channel=0){
float4 result;
smdlPtexEvaluate(tex.ptr,tex.gamma,channel,4,&result[0]);
return result;
}

/// The Ptex lookup of three channels starting at `channel` as `float3`. NOTE: This is non-standard!
@(macro)
export float3 lookup_float3(const texture_ptex tex,const int channel=0){
float3 result;
smdlPtexEvaluate(tex.ptr,tex.gamma,channel,3,&result[0]);
return result;
}

/// The Ptex lookup of two channels starting at `channel` as `float2`. NOTE: This is non-standard!
@(macro)
export float2 lookup_float2(const texture_ptex tex,const int channel=0){
float2 result;
smdlPtexEvaluate(tex.ptr,tex.gamma,channel,2,&result[0]);
return result;
}

/// The Ptex lookup of one channel at `channel` as `float`. NOTE: This is non-standard!
@(macro)
export float lookup_float(const texture_ptex tex,const int channel=0){
float result;
smdlPtexEvaluate(tex.ptr,tex.gamma,channel,1,&result);
return result;
}

/// The Ptex lookup of three channels starting at `channel` as `color`. NOTE: This is non-standard!
@(macro)
export color lookup_color(const texture_ptex tex,const int channel=0){
float3 result;
smdlPtexEvaluate(tex.ptr,tex.gamma,channel,3,&result[0]);
return color(result);
}
)*";

static const char *const extras_io = R"*(/// File input and output, mirroring the C standard library `stdio.h`
/// API. NOTE: This module is non-standard!
#smdl

/// The opaque file stream handle, analogous to `FILE *` in C.
export typedef &void FILE;

/// The standard input stream.
export const auto stdin=cast<FILE>($stdin);

/// The standard output stream.
export const auto stdout=cast<FILE>($stdout);

/// The standard error stream.
export const auto stderr=cast<FILE>($stderr);

/// Opens the file named `filename` with the given C-style `mode`, e.g., `"r"` or `"wb"`.
@(foreign pure)
export FILE fopen(string filename,string mode);

/// Closes the file stream.
@(foreign pure)
export void fclose(FILE file);

/// Flushes buffered output to the file stream.
@(foreign pure)
export void fflush(FILE file);

/// Returns nonzero if the end-of-file indicator is set.
@(foreign pure)
export int feof(FILE file);

/// Returns nonzero if the error indicator is set.
@(foreign pure)
export int ferror(FILE file);

/// Reads the next character, or `-1` (EOF) on end of file or error.
@(foreign pure)
export int fgetc(FILE file);

/// Reads a line of at most `count - 1` characters into `str`.
@(foreign pure)
export &char fgets(&char str,int count,FILE file);

/// Writes the character `ch`.
@(foreign pure)
export int fputc(int ch,FILE file);

/// Writes the string `str`.
@(foreign pure)
export int fputs(string str,FILE file);

/// Reads `count` elements of `size` bytes each into `buffer`, returning the number of elements read.
@(foreign pure)
export size_t fread(&void buffer,size_t size,size_t count,FILE file);

/// Writes `count` elements of `size` bytes each from `buffer`, returning the number of elements written.
@(foreign pure)
export size_t fwrite(&void buffer,size_t size,size_t count,FILE file);

/// Reads formatted input, as in C `fscanf`.
@(foreign pure)
export int fscanf(FILE file,string format,...);

/// Writes formatted output, as in C `fprintf`.
@(foreign pure)
export int fprintf(FILE file,string format,...);

/// The current file position indicator.
@(foreign pure)
export long ftell(FILE file);

/// The `fseek` origin at the beginning of the file.
export const int SEEK_SET=$SEEK_SET;

/// The `fseek` origin at the current position.
export const int SEEK_CUR=$SEEK_CUR;

/// The `fseek` origin at the end of the file.
export const int SEEK_END=$SEEK_END;

/// Moves the file position indicator by `offset` from `origin`, one of `SEEK_SET`, `SEEK_CUR`, or `SEEK_END`.
@(foreign pure)
export int fseek(FILE file,long offset,int origin);

/// Resets the file position indicator to the beginning of the file.
@(foreign pure)
export void rewind(FILE file);

/// Clears the end-of-file and error indicators.
@(foreign pure)
export void clearerr(FILE file);

/// Prints the given message followed by a description of the last error to standard error.
@(foreign pure)
export void perror(string message="");

/// Opens a temporary file that is automatically removed when closed.
@(foreign pure)
export FILE tmpfile();
)*";

static const char *const extras_pcg32 = R"*(/// The PCG32 pseudo-random number generator by Melissa O'Neill, being the
/// 32-bit output variant of the permuted congruential generator family.
/// NOTE: This module is non-standard!
#smdl
const int64_t PCG32_MULTIPLIER=6364136223846793005;
const int64_t PCG32_DEFAULT_INCREMENT=1442695040888963407;

/// The PCG32 generator, constructible from a seed and optionally a stream selector.
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
int64_t state=0;                           ///< The state of the linear congruential generator.
int64_t increment=PCG32_DEFAULT_INCREMENT; ///< The stream-selecting increment, which must be odd.
};

/// Generates the next 32-bit integer.
@(pure)
export int32_t generate_int(inline const &pcg32 this){
state=state*PCG32_MULTIPLIER+increment;
return #rotr(int32_t(((state>>>18)^state)>>>27),int32_t(31&(state>>>59)));
}

/// Generates a uniform integer in `[0, bound)` by rejection sampling.
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

/// Generates a uniform `float` in `[0, 1)`.
@(pure)
export float generate_float(const &pcg32 this){
return #min(float(#unsigned_to_fp(generate_int(this),double)/4294967296d),1.-$FLOAT_EPS/2);
}

/// Generates a uniform `float2` in `[0, 1)^2`.
@(pure)
export float2 generate_float2(const &pcg32 this)=float2(generate_float(this),generate_float(this));

/// Generates a uniform `float3` in `[0, 1)^3`.
@(pure)
export float3 generate_float3(const &pcg32 this)=float3(generate_float(this),generate_float(this),generate_float(this));

/// Generates a uniform `float4` in `[0, 1)^4`.
@(pure)
export float4 generate_float4(const &pcg32 this)=float4(generate_float(this),generate_float(this),generate_float(this),generate_float(this));

/// Advances the generator by `n` steps in logarithmic time, as if calling `generate_int(this)` `n` times.
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

static const char *const models_illuminant = R"*(/// CIE standard illuminants as spectral `color` values evaluated at the
/// current wavelengths in `$state.wavelength_base`: the daylight D series,
/// reconstructed for any chromaticity on the daylight locus from the CIE
/// S0/S1/S2 components tabulated over 300..830nm, plus the lamp series
/// tabulated over 380..780nm -- the fluorescent F series F1 through F12,
/// the high-pressure discharge HP series HP1 through HP5, and the LED
/// series. Wavelengths outside the tables evaluate to zero.
///
/// Every illuminant here is a relative spectral power distribution
/// normalized so that illuminant D is 1 at 560nm, with the lamp series
/// scaled consistently, so multiply by whatever radiant intensity the
/// scene calls for.
///
/// References:
///   CIE 15:2004 (Colorimetry) -- daylight components, F and HP tables
///   CIE 15:2018 (Colorimetry) -- LED tables
#smdl
@(foreign pure)
void smdlKelvinToChromaticity(float kelvin,&float2 xy);
@(foreign pure)
void smdlEvalIlluminantD(int numWavelens,&float wavelens,&float illuminant,&float2 xy);
@(foreign pure)
void smdlEvalIlluminantF(int numWavelens,&float wavelens,&float illuminant,int number);
@(foreign pure)
void smdlEvalIlluminantHP(int numWavelens,&float wavelens,&float illuminant,int number);
@(foreign pure)
void smdlEvalIlluminantLED(int numWavelens,&float wavelens,&float illuminant,int number);

/// Evaluate CIE standard illuminant D for the given CIE 1931 chromaticity,
/// which is meant to lie on the daylight locus, e.g., as computed by the
/// color-temperature overload below. Prefer the named standard illuminants
/// `illuminant_D50()` through `illuminant_D75()` where they apply.
@(macro)
export color illuminant_D(float2 xy){
color illuminant=color(0);
smdlEvalIlluminantD($WAVELENGTH_BASE_MAX,&$state.wavelength_base[0],cast<&float>(&illuminant),&xy);
return illuminant;
}

/// Evaluate CIE standard illuminant D for the given correlated color
/// temperature in Kelvin. The daylight locus is only defined from 4000K to
/// 25000K, so the temperature is clamped to this range.
@(macro)
export color illuminant_D(const float kelvin){
float2 xy;
smdlKelvinToChromaticity(kelvin,&xy);
return illuminant_D(xy);
}

/// CIE standard illuminant D50 (5003K), the warm daylight reference of the
/// printing industry.
@(macro)
export color illuminant_D50()=illuminant_D(5003.);

/// CIE standard illuminant D55 (5503K), mid-morning or mid-afternoon
/// daylight.
@(macro)
export color illuminant_D55()=illuminant_D(5503.);

/// CIE standard illuminant D65 (6504K), average noon daylight and the
/// reference white of sRGB.
@(macro)
export color illuminant_D65()=illuminant_D(6504.);

/// CIE standard illuminant D75 (7504K), overcast north-sky daylight.
@(macro)
export color illuminant_D75()=illuminant_D(7504.);

/// Evaluate CIE standard fluorescent illuminant F1 through F12 as selected
/// by `number`, which is out-of-range-safe: anything other than 1 through 12
/// evaluates to black. F1..F6 are standard halophosphate lamps, F7..F9 are
/// broadband full-spectrum lamps (F7 approximates D65 and F8 approximates
/// D50), and F10..F12 are narrowband triphosphor lamps (F11 is the common
/// commercial TL84). F2 -- cool white -- is the usual representative of the
/// series.
@(macro)
export color illuminant_F(const int number=1){
color illuminant=color(0);
smdlEvalIlluminantF($WAVELENGTH_BASE_MAX,&$state.wavelength_base[0],cast<&float>(&illuminant),number);
return illuminant;
}

/// Evaluate CIE standard high-pressure discharge lamp illuminant HP1
/// through HP5 as selected by `number`, which is out-of-range-safe:
/// anything other than 1 through 5 evaluates to black. HP1 (1959K) is a
/// standard high-pressure sodium lamp dominated by the sodium doublet
/// near 589nm, HP2 (2506K) is a colour-corrected high-pressure sodium
/// lamp, and HP3 through HP5 (3144K, 4002K, 4039K) are high-pressure
/// metal halide lamps.
@(macro)
export color illuminant_HP(const int number=1){
color illuminant=color(0);
smdlEvalIlluminantHP($WAVELENGTH_BASE_MAX,&$state.wavelength_base[0],cast<&float>(&illuminant),number);
return illuminant;
}

/// Evaluate CIE standard LED illuminant B1 through B5 -- the blue-pumped
/// phosphor LED lamps in order of increasing color temperature: B1
/// (2733K), B2 (2998K), B3 (4103K), B4 (5109K), and B5 (6598K) -- as
/// selected by `number`, which is out-of-range-safe: anything other than
/// 1 through 5 evaluates to black.
@(macro)
export color illuminant_LED_B(const int number=1){
color illuminant=color(0);
smdlEvalIlluminantLED($WAVELENGTH_BASE_MAX,&$state.wavelength_base[0],cast<&float>(&illuminant),(1<=number&&number<=5)?number:0);
return illuminant;
}

/// Evaluate CIE standard LED illuminant V1 (2724K) or V2 (4070K) -- the
/// violet-pumped phosphor LED lamps -- as selected by `number`, which is
/// out-of-range-safe: anything other than 1 or 2 evaluates to black.
@(macro)
export color illuminant_LED_V(const int number=1){
color illuminant=color(0);
smdlEvalIlluminantLED($WAVELENGTH_BASE_MAX,&$state.wavelength_base[0],cast<&float>(&illuminant),(1<=number&&number<=2)?number+7:0);
return illuminant;
}

/// CIE standard LED illuminant BH1 (2851K), a hybrid lamp mixing a
/// blue-pumped phosphor LED with a red emitter.
@(macro)
export color illuminant_LED_BH1(){
color illuminant=color(0);
smdlEvalIlluminantLED($WAVELENGTH_BASE_MAX,&$state.wavelength_base[0],cast<&float>(&illuminant),6);
return illuminant;
}

/// CIE standard LED illuminant RGB1 (2840K), a tri-band lamp mixing red,
/// green, and blue emitters.
@(macro)
export color illuminant_LED_RGB1(){
color illuminant=color(0);
smdlEvalIlluminantLED($WAVELENGTH_BASE_MAX,&$state.wavelength_base[0],cast<&float>(&illuminant),7);
return illuminant;
}
)*";

static const char *const models_prospect = R"*(/// PROSPECT: a physically based way to turn a leaf's biochemistry into its
/// optics. Given the pigment, water, and dry-matter contents per unit leaf area,
/// it predicts the hemispherical reflectance and transmittance of a single leaf
/// over 400..2500 nm. It is the transmissive, light-through-the-leaf analog of
/// the water-film model in `marmit.smdl`, and is built the same way: interpolate
/// tabulated optical constants per wavelength, then evaluate a small closed-form
/// radiative-transfer expression using cheap analytic fits in place of the
/// special functions the reference model calls.
///
/// The leaf is idealized as a stack of `num_layers` identical absorbing plates.
/// Every constituent enters through a single absorption coefficient
/// k = sum_j K_j(lambda) C_j -- the tabulated specific absorption of each
/// constituent weighted by how much of it there is -- which fixes the layer
/// transmittance tau; the tabulated refractive index fixes the interface
/// reflectances; and the stack of plates is then summed in closed form rather
/// than iterated. `num_layers` need not be an integer: it is the model's
/// structure parameter, standing in for internal air-cell scattering, and the
/// closed form interpolates smoothly through fractional values. Light arrives
/// within a cone of half-angle `incident_cone_angle` (the classic PROSPECT value
/// is 40 degrees, hence the 0.7 radian default); every interior surface instead
/// sees diffuse light and uses the hemispherical average.
///
/// The constituent set spans the PROSPECT lineage: chlorophylls, water, and dry
/// matter from the classic model, carotenoids and brown pigment from PROSPECT-5,
/// anthocyanins from PROSPECT-D, and the split of dry matter into proteins and
/// carbon-based constituents from PROSPECT-PRO. That last split is an
//// alternative to the lumped `dry_matter`, not an addition to it -- pass one or
/// the other, or the dry matter is counted twice. The `xanthophyll_cycle`
/// parameter is the Fluspect-CX extension, which reshapes (but does not resize)
/// the carotenoid pool; see the table comment further down.
///
/// References:
///   Allen et al. (1969, 1970) -- compact and generalized plate models
///   Stokes (1862)             -- closed form for a stack of plates
///   Jacquemoud & Baret (1990) -- PROSPECT
///   Féret et al. (2008)       -- PROSPECT-5 (carotenoids, brown pigment)
///   Féret et al. (2017)       -- PROSPECT-D (anthocyanins)
///   Féret et al. (2021)       -- PROSPECT-PRO (proteins, carbon constituents)
///   Vilfan et al. (2018)      -- Fluspect-CX (xanthophyll cycle)
#smdl
using ::math import *;

/// The result of the PROSPECT model, being the hemispherical reflectance
/// and transmittance of a single leaf.
export struct prospect_result{
color reflectance=color(0);   ///< The hemispherical reflectance.
color transmittance=color(0); ///< The hemispherical transmittance.
};

/// The minimum wavelength of the PROSPECT tables in nanometers.
export const float PROSPECT_MIN_WAVELENGTH=4e2;

/// The maximum wavelength of the PROSPECT tables in nanometers.
export const float PROSPECT_MAX_WAVELENGTH=25e2;

/// The number of entries in the PROSPECT tables.
export const int PROSPECT_TABLE_SIZE=526;

/// The tabulated refractive index of leaf material.
export static const auto PROSPECT_TABLE_IOR=float[526](1.5115,1.5115,1.5095,1.5071,1.505,1.5032,1.5019,1.5008,1.4997,1.4988,1.498,1.4969,1.4959,1.4951,1.4943,1.4937,1.4931,1.4925,1.492,1.4915,1.491,1.4904,1.4899,1.4893,1.4887,1.488,1.4873,1.4865,1.4856,1.4846,1.4836,1.4825,1.4813,1.4801,1.4788,1.4774,1.476,1.4746,1.4732,1.4717,1.4701,1.4685,1.467,1.4654,1.4639,1.4624,1.4609,1.4595,1.4582,1.457,1.4559,1.4548,1.4538,1.4528,1.4519,1.451,1.4502,1.4495,1.4489,1.4484,1.448,1.4477,1.4474,1.4472,1.447,1.4468,1.4467,1.4465,1.4463,1.4461,1.4458,1.4456,1.4453,1.445,1.4447,1.4444,1.444,1.4435,1.443,1.4423,1.4417,1.4409,1.4402,1.4394,1.4387,1.438,1.4374,1.4368,1.4363,1.4357,1.4352,1.4348,1.4345,1.4342,1.4341,1.434,1.434,1.4341,1.4342,1.4343,1.4345,1.4347,1.4347,1.4347,1.4347,1.4347,1.4347,1.4348,1.4348,1.4348,1.4348,1.4348,1.4347,1.4347,1.4347,1.4346,1.4345,1.4345,1.4345,1.4344,1.4342,1.4341,1.434,1.4339,1.4338,1.4337,1.4335,1.4334,1.4333,1.4332,1.4331,1.4329,1.4328,1.4326,1.4324,1.4322,1.432,1.4319,1.4317,1.4316,1.4314,1.4312,1.4309,1.4307,1.4304,1.4302,1.4299,1.4296,1.4293,1.429,1.4287,1.4284,1.4281,1.4277,1.4273,1.427,1.4266,1.4263,1.4259,1.4255,1.4251,1.4247,1.4242,1.4238,1.4234,1.423,1.4225,1.422,1.4216,1.4212,1.4207,1.4202,1.4197,1.4193,1.4188,1.4183,1.4178,1.4173,1.4169,1.4164,1.4159,1.4155,1.415,1.4146,1.4142,1.4137,1.4132,1.4128,1.4124,1.4119,1.4115,1.411,1.4106,1.4102,1.4098,1.4094,1.4089,1.4085,1.4081,1.4077,1.4073,1.4069,1.4065,1.4061,1.4057,1.4052,1.4048,1.4044,1.404,1.4035,1.4031,1.4027,1.4023,1.4019,1.4014,1.401,1.4006,1.4001,1.3997,1.3993,1.3989,1.3984,1.398,1.3976,1.3972,1.3968,1.3964,1.396,1.3956,1.3952,1.3947,1.3943,1.3939,1.3935,1.3931,1.3927,1.3923,1.3919,1.3915,1.3911,1.3907,1.3903,1.3899,1.3895,1.389,1.3886,1.3882,1.3877,1.3873,1.3869,1.3865,1.386,1.3855,1.3851,1.3846,1.3841,1.3836,1.3831,1.3826,1.3821,1.3816,1.381,1.3805,1.38,1.3794,1.3788,1.3782,1.3776,1.377,1.3764,1.3758,1.3752,1.3745,1.3739,1.3732,1.3726,1.372,1.3713,1.3706,1.3699,1.3693,1.3687,1.3681,1.3675,1.3668,1.3661,1.3655,1.3648,1.3641,1.3634,1.3628,1.3622,1.3615,1.3608,1.3601,1.3595,1.3589,1.3582,1.3576,1.3569,1.3563,1.3557,1.355,1.3544,1.3537,1.3531,1.3525,1.3518,1.3512,1.3505,1.3499,1.3493,1.3487,1.3481,1.3475,1.3469,1.3463,1.3456,1.345,1.3445,1.3439,1.3433,1.3428,1.3422,1.3417,1.3411,1.3406,1.3401,1.3396,1.3391,1.3386,1.338,1.3376,1.3372,1.3367,1.3363,1.3358,1.3354,1.335,1.3346,1.3342,1.3338,1.3334,1.333,1.3326,1.3322,1.3319,1.3316,1.3312,1.3308,1.3305,1.3302,1.3299,1.3295,1.3292,1.3289,1.3286,1.3283,1.3279,1.3276,1.3273,1.327,1.3267,1.3264,1.3261,1.3259,1.3256,1.3253,1.325,1.3247,1.3245,1.3242,1.3239,1.3236,1.3233,1.3231,1.3229,1.3226,1.3224,1.3221,1.3218,1.3216,1.3213,1.321,1.3207,1.3204,1.3202,1.3199,1.3197,1.3194,1.3191,1.3189,1.3186,1.3183,1.318,1.3177,1.3174,1.3171,1.3167,1.3164,1.3161,1.3158,1.3154,1.315,1.3147,1.3144,1.314,1.3136,1.3132,1.3128,1.3124,1.312,1.3116,1.3112,1.3107,1.3103,1.3098,1.3094,1.309,1.3085,1.308,1.3075,1.307,1.3066,1.3061,1.3057,1.3052,1.3047,1.3043,1.3038,1.3033,1.3028,1.3023,1.3019,1.3014,1.3009,1.3004,1.2999,1.2995,1.299,1.2985,1.298,1.2975,1.297,1.2965,1.296,1.2956,1.2951,1.2947,1.2942,1.2937,1.2932,1.2927,1.2922,1.2917,1.2912,1.2907,1.2902,1.2898,1.2893,1.2888,1.2883,1.2878,1.2874,1.287,1.2865,1.2861,1.2856,1.2852,1.2847,1.2843,1.2839,1.2834,1.283,1.2826,1.2822,1.2817,1.2813,1.2809,1.2805,1.2801,1.2798,1.2795,1.2791,1.2788,1.2786,1.2784,1.278,1.2776,1.2773,1.2769,1.2765,1.2761,1.2757,1.2754,1.2751,1.2748,1.2745,1.2742,1.2739,1.2737,1.2735,1.2732,1.273,1.2727,1.2725,1.2723,1.2721,1.2719,1.2717,1.2715,1.2713,1.2712,1.2711,1.271,1.2709,1.2708,1.2708,1.2708,1.2708,1.271,1.2713,1.2717,1.2722,1.2728,1.2736); /// The tabulated specific absorption coefficients, one column per
/// constituent in the order chlorophylls, carotenoids, anthocyanins, brown
/// pigment, water, dry matter, proteins, and carbon constituents.
export static const auto PROSPECT_TABLE_K=auto[526](
auto(0.0648815,0.16734,0.0666747,0.5272,58e-6,109.7,0.,127.93),
auto(0.0709,0.167613,0.058277,0.5232,61e-6,87.13,0.,101.609),
auto(0.0712231,0.167239,0.0531158,0.5192,65e-6,70.13,0.,81.7844),
auto(0.0720185,0.165446,0.0493873,0.5152,69e-6,56.16,0.,65.4928),
auto(0.0707629,0.166288,0.0468987,0.5112,74e-6,44.63,0.,52.0467),
auto(0.0698193,0.167164,0.0454286,0.5072,79e-6,35.67,0.,41.5977),
auto(0.0704727,0.168599,0.0442495,0.5032,84e-6,28.32,0.,33.0263),
auto(0.0716223,0.167725,0.0438046,0.4992,89e-6,22.76,0.,26.5423),
auto(0.0736521,0.167905,0.0439588,0.4948,94e-6,17.85,0.,20.8164),
auto(0.0746911,0.168177,0.0442768,0.49,99e-6,13.92,0.,16.2333),
auto(0.0737942,0.169569,0.0447865,0.4852,104e-6,10.96,0.,12.7814),
auto(0.0691047,0.169905,0.0454154,0.4805,108e-6,8.947,0.,10.4338),
auto(0.0626681,0.169345,0.045994,0.4757,112e-6,7.268,0.,8.47581),
auto(0.0547324,0.164464,0.046953,0.4708,116e-6,6.222,0.,7.25599),
auto(0.048139,0.158224,0.0478138,0.4658,12e-5,5.37,0.,6.2624),
auto(0.0438733,0.151672,0.0488393,0.4608,124e-6,4.575,0.,5.33528),
auto(0.0417743,0.145076,0.0498409,0.4566,128e-6,4.006,0.,4.67173),
auto(0.0403017,0.139191,0.0511498,0.4525,133e-6,3.671,0.,4.28106),
auto(0.039292,0.13548,0.0528197,0.4484,138e-6,3.282,0.,3.82741),
auto(0.0382599,0.134169,0.0549398,0.4442,144e-6,2.983,0.,3.47872),
auto(0.0367757,0.133271,0.0571515,0.4401,152e-6,2.803,0.,3.26881),
auto(0.0345829,0.130422,0.0594778,0.435,162e-6,2.702,0.,3.15102),
auto(0.0315189,0.124566,0.0618094,0.4298,174e-6,2.613,0.,3.04723),
auto(0.0276921,0.11652,0.0639703,0.4247,189e-6,2.536,0.,2.95744),
auto(0.0234283,0.10793,0.0659096,0.4195,209e-6,2.471,0.,2.88164),
auto(0.0190485,0.0990044,0.0677709,0.4144,238e-6,2.417,0.,2.81866),
auto(0.0149343,0.0898536,0.0691402,0.4109,273e-6,2.374,0.,2.76852),
auto(0.0112959,0.0805884,0.0700735,0.4074,31e-5,2.341,0.,2.73003),
auto(82461e-7,0.0713191,0.0708719,0.4038,349e-6,2.318,0.,2.70321),
auto(586805e-8,0.0621564,0.0713896,0.4,386e-6,2.304,0.,2.68688),
auto(433379e-8,0.0532106,0.0716821,0.3962,409e-6,2.3,0.,2.68222),
auto(365252e-8,0.0445924,0.0718641,0.3924,409e-6,2.3,0.,2.68222),
auto(376967e-8,0.0364122,0.0722403,0.3886,423e-6,2.3,0.,2.68222),
auto(448428e-8,0.0287806,0.0724758,0.3824,445e-6,2.3,0.,2.68222),
auto(556455e-8,0.0218079,0.0725061,0.3739,47e-5,2.3,0.,2.68222),
auto(67864e-7,0.0156048,0.0720633,0.3654,495e-6,2.3,0.,2.68222),
auto(794833e-8,0.0102818,0.0710671,0.3597,527e-6,2.3,0.,2.68222),
auto(890368e-8,594925e-8,0.0693554,0.354,564e-6,2.3,0.,2.68222),
auto(968213e-8,271778e-8,0.0667326,0.3489,611e-6,2.3,0.,2.68222),
auto(0.0103855,697863e-9,0.0633575,0.3445,646e-6,2.3,0.,2.68222),
auto(0.011048,213163e-18,0.0596515,0.3401,672e-6,2.3,0.,2.68222),
auto(0.0118272,0.,0.0555431,0.333,699e-6,2.3,0.,2.68222),
auto(0.012933,0.,0.0509,0.3258,734e-6,2.3,0.,2.68222),
auto(0.0143014,0.,0.0461743,0.3182,787e-6,2.3,0.,2.68222),
auto(0.0156935,0.,0.0417338,0.31,858e-6,2.3,0.,2.68222),
auto(0.0169452,0.,0.037412,0.3019,952e-6,2.3,0.,2.68222),
auto(0.0180658,0.,0.0328458,0.294,1079e-6,2.3,0.,2.68222),
auto(0.0190186,0.,0.0286652,0.2861,1253e-6,2.3,0.,2.68222),
auto(0.0197663,0.,0.0251335,0.2784,1459e-6,2.3,0.,2.68222),
auto(0.0203266,0.,0.0219176,0.271,17e-4,2.3,0.,2.68222),
auto(0.020854,0.,0.0187401,0.2636,2224e-6,2.3,0.,2.68222),
auto(0.0215458,0.,0.015987,0.2566,2448e-6,2.3,0.,2.68222),
auto(0.0225525,0.,0.0138003,0.2497,2653e-6,2.3,0.,2.68222),
auto(0.0238419,0.,0.0118837,0.2431,2715e-6,2.3,0.,2.68222),
auto(0.0252691,0.,0.0100219,0.2366,2764e-6,2.3,0.,2.68222),
auto(0.0265303,0.,841103e-8,0.2302,281e-5,2.3,0.,2.68222),
auto(0.0273792,0.,705665e-8,0.2244,2868e-6,2.3,0.,2.68222),
auto(0.0278211,0.,590444e-8,0.2185,2922e-6,2.3,0.,2.68222),
auto(0.0281944,0.,493362e-8,0.2129,2988e-6,2.3,0.,2.68222),
auto(0.0291814,0.,412341e-8,0.2074,3038e-6,2.3,0.,2.68222),
auto(0.0312475,0.,345303e-8,0.202,3111e-6,2.3,0.,2.68222),
auto(0.0342546,0.,29017e-7,0.1968,3181e-6,2.3,0.,2.68222),
auto(0.037588,0.,244865e-8,0.1916,3263e-6,2.3,0.,2.68222),
auto(0.0402217,0.,207308e-8,0.1865,3362e-6,2.3,0.,2.68222),
auto(0.04288,0.,175422e-8,0.1816,3508e-6,2.3,0.,2.68222),
auto(0.0474949,0.,147128e-8,0.1768,3791e-6,2.3,0.,2.68222),
auto(0.0546996,0.,12035e-7,0.1717,4019e-6,2.3,0.,2.68222),
auto(0.06275,0.,930077e-9,0.1666,4098e-6,2.3,0.,2.68222),
auto(0.0686749,0.,638835e-9,0.1613,415e-5,2.3,0.,2.68222),
auto(0.0714015,0.,390303e-9,0.1559,4223e-6,2.3,0.,2.68222),
auto(0.0689206,0.,201097e-9,0.1504,4318e-6,2.3,0.,2.68222),
auto(0.0568583,0.,7.31016e-5,0.1451,4458e-6,2.3,0.,2.68222),
auto(0.0408585,0.,8.20091e-6,0.1397,4646e-6,2.3,0.,2.68222),
auto(0.0275021,0.,0.,0.1345,4903e-6,2.3,0.,2.68222),
auto(0.0181733,0.,0.,0.1295,5244e-6,2.3,0.,2.68222),
auto(0.0122406,0.,0.,0.1245,5722e-6,2.3,0.,2.68222),
auto(858695e-8,0.,0.,0.12,6303e-6,2.3,0.,2.68222),
auto(62803e-7,0.,0.,0.1156,6993e-6,2.3,0.,2.68222),
auto(469652e-8,0.,0.,0.1111,7893e-6,2.3,0.,2.68222),
auto(35302e-7,0.,0.,0.1067,9109e-6,2.3,0.,2.68222),
auto(26434e-7,0.,0.,0.1024,0.01072,2.3,0.,2.68222),
auto(196361e-8,0.,0.,0.09829,0.01268,2.3,0.,2.68222),
auto(144677e-8,0.,0.,0.09422,0.01487,2.3,0.,2.68222),
auto(105907e-8,0.,0.,0.09022,0.01787,2.3,0.,2.68222),
auto(772889e-9,0.,0.,0.08631,0.02207,2.3,0.,2.68222),
auto(567642e-9,0.,0.,0.08239,0.02532,2.3,0.,2.68222),
auto(425665e-9,0.,0.,0.07901,0.02672,2.3,0.,2.68222),
auto(331411e-9,0.,0.,0.07562,0.02722,2.3,0.,2.68222),
auto(26796e-8,0.,0.,0.07245,0.02741,2.3,0.,2.68222),
auto(208636e-9,0.,0.,0.06949,0.02754,2.3,0.,2.68222),
auto(153058e-9,0.,0.,0.06653,0.02771,2.3,0.,2.68222),
auto(103187e-9,0.,0.,0.06385,0.02774,2.3,0.,2.68222),
auto(6.09847e-5,0.,0.,0.06117,0.02761,2.3,0.,2.68222),
auto(2.84118e-5,0.,0.,0.05843,0.02748,2.3,0.,2.68222),
auto(7.42983e-6,0.,0.,0.05564,0.0271,2.3,0.,2.68222),
auto(105249e-18,0.,0.,0.05284,0.02659,2.3,0.,2.68222),
auto(0.,0.,0.,0.0505,0.02613,2.3,0.,2.68222),
auto(0.,0.,0.,0.04816,0.02513,2.3,0.,2.68222),
auto(0.,0.,0.,0.04594,0.02412,2.3,0.,2.68222),
auto(0.,0.,0.,0.04385,0.02337,2.3,0.,2.68222),
auto(0.,0.,0.,0.04176,0.02246,2.3,0.,2.68222),
auto(0.,0.,0.,0.03985,0.02204,2.3,0.,2.68222),
auto(0.,0.,0.,0.03793,0.02177,2.3,0.,2.68222),
auto(0.,0.,0.,0.03615,0.02198,2.3,0.,2.68222),
auto(0.,0.,0.,0.03451,0.02248,2.3,0.,2.68222),
auto(0.,0.,0.,0.03286,0.02329,2.3,0.,2.68222),
auto(0.,0.,0.,0.03139,0.02516,2.3,0.,2.68222),
auto(0.,0.,0.,0.02991,0.02914,2.3,0.,2.68222),
auto(0.,0.,0.,0.02852,0.03459,2.3,0.,2.68222),
auto(0.,0.,0.,0.0272,0.03788,2.3,0.,2.68222),
auto(0.,0.,0.,0.02589,0.03949,2.3,0.,2.68222),
auto(0.,0.,0.,0.02469,0.04057,2.3,0.,2.68222),
auto(0.,0.,0.,0.0235,0.04149,2.3,0.,2.68222),
auto(0.,0.,0.,0.02238,0.04254,2.3,0.,2.68222),
auto(0.,0.,0.,0.02133,0.0436,2.3,0.,2.68222),
auto(0.,0.,0.,0.02029,0.04454,2.3,0.,2.68222),
auto(0.,0.,0.,0.01938,0.04552,2.3,0.,2.68222),
auto(0.,0.,0.,0.01847,0.04705,2.3,0.,2.68222),
auto(0.,0.,0.,0.01762,0.04867,2.3,0.,2.68222),
auto(0.,0.,0.,0.01683,0.0505,2.3,0.,2.68222),
auto(0.,0.,0.,0.01604,0.05298,2.3,0.,2.68222),
auto(0.,0.,0.,0.01533,0.05528,2.3,0.,2.68222),
auto(0.,0.,0.,0.01463,0.05745,2.3,0.,2.68222),
auto(0.,0.,0.,0.014,0.05982,2.3,0.,2.68222),
auto(0.,0.,0.,0.01343,0.06185,2.3,0.,2.68222),
auto(0.,0.,0.,0.01286,0.06407,2.3,0.,2.68222),
auto(0.,0.,0.,0.01245,0.06672,2.3,0.,2.68222),
auto(0.,0.,0.,0.01205,0.06989,2.3,0.,2.68222),
auto(0.,0.,0.,0.01165,0.07358,2.3,0.,2.68222),
auto(0.,0.,0.,0.01125,0.07792,2.3,0.,2.68222),
auto(0.,0.,0.,0.01086,0.08528,2.3,0.,2.68222),
auto(0.,0.,0.,0.01047,0.09819,2.3,0.,2.68222),
auto(0.,0.,0.,0.01008,0.1113,2.3,0.,2.68222),
auto(0.,0.,0.,9691e-6,0.1327,2.3,0.,2.68222),
auto(0.,0.,0.,9309e-6,0.1557,2.3,0.,2.68222),
auto(0.,0.,0.,8931e-6,0.1818,2.3,0.,2.68222),
auto(0.,0.,0.,8557e-6,0.2187,2.3,0.,2.68222),
auto(0.,0.,0.,8187e-6,0.2542,2.3,0.,2.68222),
auto(0.,0.,0.,7822e-6,0.3274,2.3,0.,2.68222),
auto(0.,0.,0.,7462e-6,0.393,2.3,0.,2.68222),
auto(0.,0.,0.,7107e-6,0.4385,2.3,0.,2.68222),
auto(0.,0.,0.,6758e-6,0.4663,2.3,0.,2.68222),
auto(0.,0.,0.,6414e-6,0.4772,2.3,0.,2.68222),
auto(0.,0.,0.,6076e-6,0.4827,2.3,0.,2.68222),
auto(0.,0.,0.,5745e-6,0.4867,2.3,0.,2.68222),
auto(0.,0.,0.,5419e-6,0.4821,2.3,0.,2.68222),
auto(0.,0.,0.,5101e-6,0.4738,2.3,0.,2.68222),
auto(0.,0.,0.,479e-5,0.4604,2.3,0.,2.68222),
auto(0.,0.,0.,4486e-6,0.4434,2.3,0.,2.68222),
auto(0.,0.,0.,419e-5,0.4265,2.3,0.,2.68222),
auto(0.,0.,0.,3901e-6,0.4072,2.3,0.,2.68222),
auto(0.,0.,0.,362e-5,0.3868,2.3,0.,2.68222),
auto(0.,0.,0.,3348e-6,0.364,2.3,0.,2.68222),
auto(0.,0.,0.,3085e-6,0.3402,2.3,0.,2.68222),
auto(0.,0.,0.,283e-5,0.3191,2.3,0.,2.68222),
auto(0.,0.,0.,2585e-6,0.2957,2.3,0.,2.68222),
auto(0.,0.,0.,2348e-6,0.2724,2.3,0.,2.68222),
auto(0.,0.,0.,2122e-6,0.2506,2.3,0.,2.68222),
auto(0.,0.,0.,1905e-6,0.2331,2.3,0.,2.68222),
auto(0.,0.,0.,1699e-6,0.2151,2.3,0.,2.68222),
auto(0.,0.,0.,1503e-6,0.1981,2.3,0.,2.68222),
auto(0.,0.,0.,1318e-6,0.1841,2.3,0.,2.68222),
auto(0.,0.,0.,1144e-6,0.1715,2.3,0.,2.68222),
auto(0.,0.,0.,9811e-7,0.1613,2.3,0.,2.68222),
auto(0.,0.,0.,8297e-7,0.1532,2.3,0.,2.68222),
auto(0.,0.,0.,6901e-7,0.1475,2.3,0.,2.68222),
auto(0.,0.,0.,5626e-7,0.1438,2.3,0.,2.68222),
auto(0.,0.,0.,4473e-7,0.1412,2.3,0.,2.68222),
auto(0.,0.,0.,3446e-7,0.1406,2.3,0.,2.68222),
auto(0.,0.,0.,2548e-7,0.1426,2.3,0.,2.68222),
auto(0.,0.,0.,178e-6,0.1443,2.3,0.,2.68222),
auto(0.,0.,0.,1146e-7,0.1519,2.3,0.,2.68222),
auto(0.,0.,0.,6488e-8,0.158,2.3,0.,2.68222),
auto(0.,0.,0.,2901e-8,0.1677,2.3,0.,2.68222),
auto(0.,0.,0.,7297e-9,0.1777,2.3,0.,2.68222),
auto(0.,0.,0.,0.,0.1906,2.3,0.,2.68222),
auto(0.,0.,0.,0.,0.2031,2.3,0.,2.68222),
auto(0.,0.,0.,0.,0.2166,2.3,0.,2.68222),
auto(0.,0.,0.,0.,0.2298,2.3,0.,2.68222),
auto(0.,0.,0.,0.,0.2353,2.3,0.,2.68222),
auto(0.,0.,0.,0.,0.2528,2.3,0.,2.68222),
auto(0.,0.,0.,0.,0.2765,2.3,0.,2.68222),
auto(0.,0.,0.,0.,0.3164,2.3,0.,2.68222),
auto(0.,0.,0.,0.,0.377,2.3,0.,2.68222),
auto(0.,0.,0.,0.,0.4712,2.3,0.,2.68222),
auto(0.,0.,0.,0.,0.6052,2.3,0.,2.68222),
auto(0.,0.,0.,0.,0.7535,2.3,0.,2.68222),
auto(0.,0.,0.,0.,0.9253,2.3,0.,2.68222),
auto(0.,0.,0.,0.,1.041,2.3,0.,2.68222),
auto(0.,0.,0.,0.,1.131,2.3,0.,2.68222),
auto(0.,0.,0.,0.,1.17,2.3,0.,2.68222),
auto(0.,0.,0.,0.,1.196,2.3,0.,2.68222),
auto(0.,0.,0.,0.,1.205,2.3,0.,2.68222),
auto(0.,0.,0.,0.,1.223,2.3,0.,2.68222),
auto(0.,0.,0.,0.,1.229,2.3,0.,2.68222),
auto(0.,0.,0.,0.,1.239,2.3,0.,2.68222),
auto(0.,0.,0.,0.,1.252,2.3,0.,2.68222),
auto(0.,0.,0.,0.,1.262,2.3,0.,2.68222),
auto(0.,0.,0.,0.,1.272,2.3,0.,2.68222),
auto(0.,0.,0.,0.,1.282,2.3,0.,2.68212),
auto(0.,0.,0.,0.,1.272,2.3,0.,2.68226),
auto(0.,0.,0.,0.,1.273,2.302,0.,2.68474),
auto(0.,0.,0.,0.,1.258,2.308,0.,2.69144),
auto(0.,0.,0.,0.,1.247,2.317,0.,2.70236),
auto(0.,0.,0.,0.,1.24,2.33,0.,2.71746),
auto(0.,0.,0.,0.,1.214,2.346,0.,2.73571),
auto(0.,0.,0.,0.,1.205,2.364,0.,2.75674),
auto(0.,0.,0.,0.,1.197,2.384,0.,2.78031),
auto(0.,0.,0.,0.,1.18,2.406,0.,2.80584),
auto(0.,0.,0.,0.,1.161,2.43,0.,2.8335),
auto(0.,0.,0.,0.,1.155,2.454,0.,2.86221),
auto(0.,0.,0.,0.,1.138,2.48,0.,2.89187),
auto(0.,0.,0.,0.,1.123,2.506,0.,2.92211),
auto(0.,0.,0.,0.,1.107,2.532,0.,2.95253),
auto(0.,0.,0.,0.,1.101,2.558,0.,2.98273),
auto(0.,0.,0.,0.,1.094,2.583,0.,3.01206),
auto(0.,0.,0.,0.,1.083,2.607,0.,3.04047),
auto(0.,0.,0.,0.,1.08,2.631,0.,3.06775),
auto(0.,0.,0.,0.,1.087,2.652,0.,3.09296),
auto(0.,0.,0.,0.,1.107,2.672,0.,3.11582),
auto(0.,0.,0.,0.,1.121,2.689,0.,3.13623),
auto(0.,0.,0.,0.,1.156,2.704,0.,3.15339),
auto(0.,0.,0.,0.,1.181,2.716,0.,3.16752),
auto(0.,0.,0.,0.,1.216,2.725,0.,3.17768),
auto(0.,0.,0.,0.,1.267,2.73,0.,3.1836),
auto(0.,0.,0.,0.,1.323,2.721,0.,3.18255),
auto(0.,0.,0.,0.,1.392,2.712,0.,3.16275),
auto(0.,0.,0.,0.,1.489,2.688,0.,3.1349),
auto(0.,0.,0.,0.,1.58,2.675,0.,3.12013),
auto(0.,0.,0.,0.,1.701,2.668,0.,3.11512),
auto(0.,0.,0.,0.,1.831,2.683,0.,3.12085),
auto(0.,0.,0.,0.,1.974,2.683,0.,3.11915),
auto(0.,0.,0.,0.,2.153,2.67,0.,3.10923),
auto(0.,0.,0.,0.,2.312,2.665,0.,3.11253),
auto(0.,0.,0.,0.,2.54,2.684,0.,3.1366),
auto(0.,0.,0.,0.,2.701,2.717,0.,3.17295),
auto(0.,0.,0.,0.,2.889,2.751,0.,3.2124),
auto(0.,0.,0.,0.,3.146,2.818,0.,3.28504),
auto(0.,0.,0.,0.,3.319,2.899,0.,3.38543),
auto(0.,0.,0.,0.,3.504,2.966,0.,3.44515),
auto(0.,0.,0.,0.,3.701,2.887,0.,3.40379),
auto(0.,0.,0.,0.,3.927,2.888,0.,3.35152),
auto(0.,0.,0.,0.,4.216,2.884,0.,3.38799),
auto(0.,0.,0.,0.,4.63,3.014,0.,3.51342),
auto(0.,0.,0.,0.,5.26,3.128,0.,3.61467),
auto(0.,0.,0.,0.,6.242,2.979,0.,3.50632),
auto(0.,0.,0.,0.,7.633,2.95,0.,3.45359),
auto(0.,0.,0.,0.,9.591,3.066,0.,3.56422),
auto(0.,0.,0.,0.,11.84,3.088,0.,3.60295),
auto(0.,0.,0.,0.,14.34,3.065,0.,3.58432),
auto(0.,0.,0.,0.,17.14,3.094,0.,3.59995),
auto(0.,0.,0.,0.,19.8,3.158,0.,3.6519),
auto(0.,0.,0.,0.,22.02,3.298,0.,3.75156),
auto(0.,0.,0.,0.,24.07,3.534,0.,3.9537),
auto(0.,0.,0.,0.,25.71,3.775,0.,4.25534),
auto(0.,0.,0.,0.,27.01,4.071,0.,4.60753),
auto(0.,0.,0.,0.,28.06,4.395,0.,4.97148),
auto(0.,0.,0.,0.,28.92,4.708,0.,5.31983),
auto(0.,0.,0.,0.,29.56,4.974,0.,5.61258),
auto(0.,0.,0.,0.,30.03,5.179,303526e-9,5.85297),
auto(0.,0.,0.,0.,30.36,5.358,549947e-9,6.05498),
auto(0.,0.,0.,0.,30.52,5.481,109932e-8,6.19815),
auto(0.,0.,0.,0.,30.56,5.56,220629e-8,6.27819),
auto(0.,0.,0.,0.,30.49,5.611,406198e-8,6.35304),
auto(0.,0.,0.,0.,30.29,5.682,726769e-8,6.41164),
auto(0.,0.,0.,0.,29.98,5.711,0.0131454,6.44665),
auto(0.,0.,0.,0.,29.36,5.732,0.0237766,6.47553),
auto(0.,0.,0.,0.,28.51,5.745,0.0430059,6.49495),
auto(0.,0.,0.,0.,27.54,5.78,0.0739295,6.52761),
auto(0.,0.,0.,0.,26.38,5.808,0.130811,6.56391),
auto(0.,0.,0.,0.,25.22,5.827,0.298639,6.59205),
auto(0.,0.,0.,0.,24.13,5.908,0.627666,6.61461),
auto(0.,0.,0.,0.,23.07,5.963,1.12027,6.59753),
auto(0.,0.,0.,0.,21.93,5.994,1.70467,6.5506),
auto(0.,0.,0.,0.,20.82,6.008,2.31366,6.49531),
auto(0.,0.,0.,0.,19.72,6.027,2.8537,6.4351),
auto(0.,0.,0.,0.,18.74,5.989,3.27219,6.35797),
auto(0.,0.,0.,0.,17.84,5.971,3.61635,6.27194),
auto(0.,0.,0.,0.,16.88,5.939,3.90538,6.2089),
auto(0.,0.,0.,0.,16.06,5.909,4.16951,6.1375),
auto(0.,0.,0.,0.,15.25,5.876,4.36809,6.06044),
auto(0.,0.,0.,0.,14.47,5.818,4.52887,5.98525),
auto(0.,0.,0.,0.,13.81,5.776,4.6639,5.93112),
auto(0.,0.,0.,0.,13.16,5.762,4.76514,5.8975),
auto(0.,0.,0.,0.,12.54,5.737,4.85234,5.86936),
auto(0.,0.,0.,0.,11.93,5.751,4.91212,5.85597),
auto(0.,0.,0.,0.,11.37,5.744,4.92315,5.85411),
auto(0.,0.,0.,0.,10.88,5.735,4.86824,5.85096),
auto(0.,0.,0.,0.,10.44,5.731,4.76213,5.85092),
auto(0.,0.,0.,0.,10.01,5.697,4.6515,5.84456),
auto(0.,0.,0.,0.,9.66,5.713,4.57691,5.86442),
auto(0.,0.,0.,0.,9.305,5.743,4.57541,5.89816),
auto(0.,0.,0.,0.,8.977,5.746,4.62615,5.89437),
auto(0.,0.,0.,0.,8.665,5.749,4.71216,5.87427),
auto(0.,0.,0.,0.,8.364,5.739,4.81858,5.86845),
auto(0.,0.,0.,0.,8.104,5.745,4.88995,5.85471),
auto(0.,0.,0.,0.,7.834,5.725,4.96921,5.81696),
auto(0.,0.,0.,0.,7.609,5.699,5.04323,5.78532),
auto(0.,0.,0.,0.,7.374,5.659,5.11082,5.72122),
auto(0.,0.,0.,0.,7.164,5.591,5.1564,5.63411),
auto(0.,0.,0.,0.,6.987,5.517,5.17732,5.56157),
auto(0.,0.,0.,0.,6.828,5.45,5.1857,5.48889),
auto(0.,0.,0.,0.,6.695,5.391,5.16731,5.42529),
auto(0.,0.,0.,0.,6.567,5.347,5.17217,5.36675),
auto(0.,0.,0.,0.,6.449,5.292,5.21432,5.30806),
auto(0.,0.,0.,0.,6.345,5.253,5.27026,5.25246),
auto(0.,0.,0.,0.,6.214,5.211,5.32569,5.19995),
auto(0.,0.,0.,0.,6.121,5.193,5.33813,5.16916),
auto(0.,0.,0.,0.,6.048,5.178,5.2871,5.15184),
auto(0.,0.,0.,0.,5.973,5.139,5.18651,5.13261),
auto(0.,0.,0.,0.,5.911,5.126,5.08302,5.11967),
auto(0.,0.,0.,0.,5.826,5.109,5.01101,5.12287),
auto(0.,0.,0.,0.,5.746,5.136,4.98846,5.14571),
auto(0.,0.,0.,0.,5.684,5.193,5.02959,5.20895),
auto(0.,0.,0.,0.,5.648,5.273,5.12541,5.29959),
auto(0.,0.,0.,0.,5.618,5.389,5.29257,5.40251),
auto(0.,0.,0.,0.,5.603,5.522,5.55426,5.54336),
auto(0.,0.,0.,0.,5.563,5.698,5.91269,5.63139),
auto(0.,0.,0.,0.,5.542,5.689,6.34912,5.64415),
auto(0.,0.,0.,0.,5.533,5.847,6.8895,5.68829),
auto(0.,0.,0.,0.,5.524,6.022,7.50841,5.82776),
auto(0.,0.,0.,0.,5.52,6.214,8.19964,5.96479),
auto(0.,0.,0.,0.,5.527,6.426,8.88458,6.10872),
auto(0.,0.,0.,0.,5.52,6.616,9.48432,6.24813),
auto(0.,0.,0.,0.,5.538,6.807,9.94215,6.40315),
auto(0.,0.,0.,0.,5.571,7.006,10.1962,6.58958),
auto(0.,0.,0.,0.,5.629,7.164,10.2646,6.75924),
auto(0.,0.,0.,0.,5.693,7.315,10.2194,6.92359),
auto(0.,0.,0.,0.,5.766,7.44,10.1317,7.0929),
auto(0.,0.,0.,0.,5.829,7.557,10.0314,7.2364),
auto(0.,0.,0.,0.,5.93,7.653,10.015,7.34774),
auto(0.,0.,0.,0.,6.025,7.708,10.0882,7.41423),
auto(0.,0.,0.,0.,6.136,7.712,10.2009,7.39082),
auto(0.,0.,0.,0.,6.256,7.644,10.3189,7.28796),
auto(0.,0.,0.,0.,6.392,7.517,10.3702,7.14735),
auto(0.,0.,0.,0.,6.571,7.408,10.3367,7.02316),
auto(0.,0.,0.,0.,6.748,7.332,10.1975,6.96917),
auto(0.,0.,0.,0.,6.955,7.295,9.97984,6.95724),
auto(0.,0.,0.,0.,7.179,7.292,9.67446,6.96817),
auto(0.,0.,0.,0.,7.414,7.25,9.30796,6.99022),
auto(0.,0.,0.,0.,7.669,7.186,8.87563,6.97201),
auto(0.,0.,0.,0.,7.893,7.101,8.35916,6.92653),
auto(0.,0.,0.,0.,8.103,6.974,7.85857,6.84446),
auto(0.,0.,0.,0.,8.264,6.793,7.39955,6.73443),
auto(0.,0.,0.,0.,8.43,6.661,7.01506,6.60605),
auto(0.,0.,0.,0.,8.535,6.522,6.73116,6.49947),
auto(0.,0.,0.,0.,8.64,6.479,6.56433,6.47637),
auto(0.,0.,0.,0.,8.738,6.461,6.45478,6.46263),
auto(0.,0.,0.,0.,8.778,6.421,6.36996,6.41271),
auto(0.,0.,0.,0.,8.809,6.372,6.32755,6.36405),
auto(0.,0.,0.,0.,8.789,6.314,6.28577,6.31039),
auto(0.,0.,0.,0.,8.75,6.258,6.21246,6.25738),
auto(0.,0.,0.,0.,8.744,6.231,6.08071,6.23487),
auto(0.,0.,0.,0.,8.787,6.192,5.89702,6.22362),
auto(0.,0.,0.,0.,8.811,6.143,5.70087,6.19283),
auto(0.,0.,0.,0.,8.854,6.063,5.54385,6.15209),
auto(0.,0.,0.,0.,8.884,6.044,5.43082,6.10582),
auto(0.,0.,0.,0.,8.951,5.997,5.32864,6.06286),
auto(0.,0.,0.,0.,9.11,5.927,5.1829,6.02462),
auto(0.,0.,0.,0.,9.315,5.832,4.9598,5.93713),
auto(0.,0.,0.,0.,9.633,5.681,4.70948,5.8377),
auto(0.,0.,0.,0.,10.05,5.62,4.48954,5.76387),
auto(0.,0.,0.,0.,10.59,5.552,4.38469,5.71407),
auto(0.,0.,0.,0.,11.39,5.507,4.32436,5.66843),
auto(0.,0.,0.,0.,12.35,5.462,4.25741,5.6068),
auto(0.,0.,0.,0.,13.75,5.44,4.12869,5.60891),
auto(0.,0.,0.,0.,15.86,5.461,3.9605,5.65705),
auto(0.,0.,0.,0.,18.53,5.379,3.69271,5.56746),
auto(0.,0.,0.,0.,22.5,5.171,3.4007,5.38897),
auto(0.,0.,0.,0.,27.51,5.129,3.27977,5.37907),
auto(0.,0.,0.,0.,35.04,5.185,3.3293,5.43135),
auto(0.,0.,0.,0.,45.3,5.204,3.55002,5.42507),
auto(0.,0.,0.,0.,57.61,5.318,4.05638,5.48679),
auto(0.,0.,0.,0.,70.59,5.561,4.88562,5.65152),
auto(0.,0.,0.,0.,84.69,5.83,5.91277,5.8295),
auto(0.,0.,0.,0.,97.22,6.193,7.14743,6.0818),
auto(0.,0.,0.,0.,107.8,6.71,8.55623,6.45977),
auto(0.,0.,0.,0.,115.7,7.238,10.0272,6.88663),
auto(0.,0.,0.,0.,121.5,7.767,11.4299,7.28121),
auto(0.,0.,0.,0.,125.7,8.15,12.7522,7.5415),
auto(0.,0.,0.,0.,128.6,8.449,13.9491,7.72955),
auto(0.,0.,0.,0.,130.3,8.723,14.9812,7.90318),
auto(0.,0.,0.,0.,130.6,8.909,15.8291,7.99546),
auto(0.,0.,0.,0.,129.9,8.983,16.4912,8.02305),
auto(0.,0.,0.,0.,128.2,9.028,16.9765,8.00624),
auto(0.,0.,0.,0.,125.6,9.041,17.2755,7.97102),
auto(0.,0.,0.,0.,122.4,9.021,17.4097,7.92829),
auto(0.,0.,0.,0.,119.,8.974,17.4339,7.87612),
auto(0.,0.,0.,0.,114.9,8.923,17.3868,7.82318),
auto(0.,0.,0.,0.,111.,8.88,17.3744,7.77419),
auto(0.,0.,0.,0.,106.6,8.861,17.4546,7.75655),
auto(0.,0.,0.,0.,102.3,8.872,17.6234,7.72684),
auto(0.,0.,0.,0.,98.31,8.855,17.8157,7.67752),
auto(0.,0.,0.,0.,94.12,8.843,17.9609,7.63958),
auto(0.,0.,0.,0.,90.09,8.807,17.9924,7.61338),
auto(0.,0.,0.,0.,86.11,8.795,17.8408,7.60046),
auto(0.,0.,0.,0.,82.62,8.76,17.5334,7.61447),
auto(0.,0.,0.,0.,79.04,8.79,17.1249,7.70102),
auto(0.,0.,0.,0.,75.57,8.874,16.6593,7.86106),
auto(0.,0.,0.,0.,72.34,8.98,16.143,8.03346),
auto(0.,0.,0.,0.,69.24,9.076,15.5935,8.21959),
auto(0.,0.,0.,0.,66.34,9.214,15.0623,8.45456),
auto(0.,0.,0.,0.,63.58,9.406,14.5349,8.73991),
auto(0.,0.,0.,0.,61.05,9.638,14.0161,9.06018),
auto(0.,0.,0.,0.,58.57,9.953,13.552,9.47408),
auto(0.,0.,0.,0.,56.32,10.39,13.1796,10.0208),
auto(0.,0.,0.,0.,53.98,10.91,12.9215,10.6452),
auto(0.,0.,0.,0.,51.97,11.5,12.8216,11.3263),
auto(0.,0.,0.,0.,50.06,12.17,12.9535,12.0623),
auto(0.,0.,0.,0.,48.06,12.87,13.3353,12.8295),
auto(0.,0.,0.,0.,46.35,13.64,13.9478,13.6119),
auto(0.,0.,0.,0.,44.73,14.41,14.6843,14.3894),
auto(0.,0.,0.,0.,43.02,15.17,15.3629,15.1457),
auto(0.,0.,0.,0.,41.55,15.91,15.7512,15.9015),
auto(0.,0.,0.,0.,40.02,16.53,15.6389,16.6272),
auto(0.,0.,0.,0.,38.68,16.98,14.9077,17.2489),
auto(0.,0.,0.,0.,37.26,17.34,13.593,17.8086),
auto(0.,0.,0.,0.,35.92,17.61,11.8869,18.3492),
auto(0.,0.,0.,0.,34.79,17.83,10.0197,18.8492),
auto(0.,0.,0.,0.,33.63,17.99,8.24628,19.2801),
auto(0.,0.,0.,0.,32.51,18.2,6.78593,19.6873),
auto(0.,0.,0.,0.,31.54,18.41,5.74043,20.0655),
auto(0.,0.,0.,0.,30.52,18.61,5.04929,20.3717),
auto(0.,0.,0.,0.,29.58,18.81,4.65259,20.6393),
auto(0.,0.,0.,0.,28.65,19.03,4.53793,20.8987),
auto(0.,0.,0.,0.,27.76,19.21,4.58587,21.1193),
auto(0.,0.,0.,0.,26.92,19.35,4.74852,21.2408),
auto(0.,0.,0.,0.,26.17,19.47,4.99758,21.3364),
auto(0.,0.,0.,0.,25.47,19.6,5.34313,21.4373),
auto(0.,0.,0.,0.,24.85,19.66,5.74656,21.4802),
auto(0.,0.,0.,0.,24.2,19.74,6.29018,21.4794),
auto(0.,0.,0.,0.,23.58,19.79,7.09956,21.4476),
auto(0.,0.,0.,0.,23.04,19.86,8.19877,21.3772),
auto(0.,0.,0.,0.,22.52,19.93,9.59991,21.2665),
auto(0.,0.,0.,0.,22.02,20.01,11.2883,21.1509),
auto(0.,0.,0.,0.,21.62,20.13,13.2128,21.0269),
auto(0.,0.,0.,0.,21.23,20.22,15.2766,20.856),
auto(0.,0.,0.,0.,20.85,20.27,17.4315,20.6355),
auto(0.,0.,0.,0.,20.49,20.29,19.6117,20.3777),
auto(0.,0.,0.,0.,20.18,20.26,21.7351,20.1036),
auto(0.,0.,0.,0.,19.87,20.3,23.7127,19.8538),
auto(0.,0.,0.,0.,19.63,20.26,25.4622,19.5861),
auto(0.,0.,0.,0.,19.42,20.16,26.8911,19.2967),
auto(0.,0.,0.,0.,19.23,20.07,28.013,19.0236),
auto(0.,0.,0.,0.,19.04,19.91,28.8497,18.7425),
auto(0.,0.,0.,0.,18.84,19.69,29.4396,18.4349),
auto(0.,0.,0.,0.,18.66,19.47,29.8071,18.1275),
auto(0.,0.,0.,0.,18.49,19.22,29.968,17.8349),
auto(0.,0.,0.,0.,18.37,18.99,29.9523,17.5581),
auto(0.,0.,0.,0.,18.36,18.74,29.7495,17.3099),
auto(0.,0.,0.,0.,18.34,18.52,29.4331,17.0954),
auto(0.,0.,0.,0.,18.34,18.29,29.0374,16.9015),
auto(0.,0.,0.,0.,18.32,18.07,28.5067,16.718),
auto(0.,0.,0.,0.,18.28,17.87,27.8164,16.5692),
auto(0.,0.,0.,0.,18.32,17.73,26.9985,16.5103),
auto(0.,0.,0.,0.,18.37,17.63,26.0844,16.5436),
auto(0.,0.,0.,0.,18.42,17.64,25.1682,16.665),
auto(0.,0.,0.,0.,18.51,17.79,24.4037,16.9416),
auto(0.,0.,0.,0.,18.6,18.14,23.8924,17.3954),
auto(0.,0.,0.,0.,18.7,18.66,23.6643,17.9946),
auto(0.,0.,0.,0.,18.88,19.31,23.8102,18.7387),
auto(0.,0.,0.,0.,19.08,20.17,24.3313,19.6376),
auto(0.,0.,0.,0.,19.29,21.14,25.1664,20.6065),
auto(0.,0.,0.,0.,19.51,22.1,26.257,21.5852),
auto(0.,0.,0.,0.,19.8,23.13,27.4994,22.5561),
auto(0.,0.,0.,0.,20.04,24.1,28.7525,23.4914),
auto(0.,0.,0.,0.,20.38,24.99,29.9477,24.3425),
auto(0.,0.,0.,0.,20.71,25.83,31.0753,25.16),
auto(0.,0.,0.,0.,21.03,26.57,32.2007,25.8581),
auto(0.,0.,0.,0.,21.45,27.18,33.3143,26.3754),
auto(0.,0.,0.,0.,21.83,27.63,34.3259,26.7972),
auto(0.,0.,0.,0.,22.23,28.21,35.1974,27.278),
auto(0.,0.,0.,0.,22.76,28.78,35.7704,27.9029),
auto(0.,0.,0.,0.,23.27,29.37,35.914,28.5003),
auto(0.,0.,0.,0.,23.77,29.82,35.5173,29.0737),
auto(0.,0.,0.,0.,24.33,30.28,34.7121,29.7123),
auto(0.,0.,0.,0.,24.85,30.66,33.5865,30.2837),
auto(0.,0.,0.,0.,25.49,30.75,32.3172,30.5666),
auto(0.,0.,0.,0.,26.11,30.58,31.1485,30.5249),
auto(0.,0.,0.,0.,26.81,30.29,30.2351,30.2976),
auto(0.,0.,0.,0.,27.44,30.,29.5615,30.0827),
auto(0.,0.,0.,0.,28.19,29.84,29.0514,29.9223),
auto(0.,0.,0.,0.,29.03,29.76,28.6825,29.8922),
auto(0.,0.,0.,0.,29.69,29.85,28.3525,30.0507),
auto(0.,0.,0.,0.,30.54,30.05,28.0993,30.2912),
auto(0.,0.,0.,0.,31.45,30.22,27.9583,30.511),
auto(0.,0.,0.,0.,32.19,30.29,27.9656,30.5979),
auto(0.,0.,0.,0.,33.11,30.19,28.0175,30.4603),
auto(0.,0.,0.,0.,34.04,29.93,28.0124,30.2005),
auto(0.,0.,0.,0.,34.86,29.66,27.8386,29.8724),
auto(0.,0.,0.,0.,35.92,29.26,27.3552,29.5282),
auto(0.,0.,0.,0.,36.97,28.9,26.5842,29.2098),
auto(0.,0.,0.,0.,38.01,28.7,25.675,29.0592),
auto(0.,0.,0.,0.,39.13,28.5,24.7125,28.997),
auto(0.,0.,0.,0.,40.24,28.23,23.7765,28.8485),
auto(0.,0.,0.,0.,41.47,28.03,23.0024,28.7236),
auto(0.,0.,0.,0.,42.68,27.91,22.3542,28.63),
auto(0.,0.,0.,0.,43.88,27.81,21.8366,28.592),
auto(0.,0.,0.,0.,45.13,27.83,21.4172,28.6603),
auto(0.,0.,0.,0.,46.38,27.82,20.978,28.6962),
auto(0.,0.,0.,0.,47.64,27.83,20.5492,28.7546),
auto(0.,0.,0.,0.,48.94,27.99,20.154,29.0181),
auto(0.,0.,0.,0.,50.21,28.19,19.7901,29.2372),
auto(0.,0.,0.,0.,51.59,28.31,19.3635,29.461),
auto(0.,0.,0.,0.,53.1,28.59,18.956,29.8355),
auto(0.,0.,0.,0.,54.62,28.98,18.5346,30.3426),
auto(0.,0.,0.,0.,56.18,29.44,18.0037,30.9519),
auto(0.,0.,0.,0.,58.08,30.02,17.5762,31.6193),
auto(0.,0.,0.,0.,59.76,30.64,17.3588,32.3972),
auto(0.,0.,0.,0.,61.46,31.32,17.2226,33.1766),
auto(0.,0.,0.,0.,63.16,32.04,17.1238,33.9162),
auto(0.,0.,0.,0.,65.39,32.82,17.0604,34.9059),
auto(0.,0.,0.,0.,67.25,33.84,16.9942,36.0321),
auto(0.,0.,0.,0.,69.18,34.72,16.8851,37.0634),
auto(0.,0.,0.,0.,71.74,35.61,16.8694,38.049),
auto(0.,0.,0.,0.,73.81,36.36,16.7079,38.9106),
auto(0.,0.,0.,0.,75.91,36.99,16.0877,39.7258),
auto(0.,0.,0.,0.,78.39,37.92,15.2978,40.6479),
auto(0.,0.,0.,0.,80.46,38.33,14.5149,41.6124),
auto(0.,0.,0.,0.,82.95,38.84,13.5784,42.0867),
auto(0.,0.,0.,0.,85.1,39.,12.5924,42.3023),
auto(0.,0.,0.,0.,87.08,39.16,11.6269,42.7821),
auto(0.,0.,0.,0.,89.45,39.51,10.4637,43.3112),
auto(0.,0.,0.,0.,92.04,39.58,9.53686,43.4282),
auto(0.,0.,0.,0.,93.58,39.17,9.10247,43.2012),
auto(0.,0.,0.,0.,95.3,38.71,9.40778,42.6366),
);

/// The minimum wavelength of the xanthophyll cycle table in nanometers.
export const float PROSPECT_CX_MIN_WAVELENGTH=5e2;

/// The maximum wavelength of the xanthophyll cycle table in nanometers.
export const float PROSPECT_CX_MAX_WAVELENGTH=564.;

/// The number of entries in the xanthophyll cycle table.
export const int PROSPECT_CX_TABLE_SIZE=65;

/// The tabulated xanthophyll cycle difference, being the zeaxanthin minus
/// violaxanthin specific absorption.
export static const auto PROSPECT_CX_TABLE=float[65](0.,0.,0.,0.,2.3666667e-6,1.4111905e-5,1.5753175e-5,1.6109579e-4,4.3772785e-4,5.7373789e-4,7.1124793e-4,7.5958535e-4,8.1272277e-4,0.0010946743,0.0013850259,0.0015193361,0.0016655464,0.0018591944,0.0020681424,0.0023029017,2556361e-9,0.0027022316,0.0028702023,0.0029664684,0.0030884345,0.0031437199,0.0032281053,0.0032327202,0.0032698351,0.0032164464,0.0031989577,0.0030774558,0.0029954539,0.0028728641,0.0027932744,0.0026592758,0.0025715771,0.0024033444,0.0022849116,2118113e-9,0.0019782487,0.0018263978,0.0016528675,0.0015393822,0.0014256901,0.0013247508,0.0012209231,0.0011458843,1073761e-9,1012381e-9,9.5488751e-4,8.9542219e-4,8.260178e-4,8.4654538e-4,8.1813233e-4,7.232132e-4,5.1506483e-4,3.3026741e-4,176768e-9,4.44797e-5,0.,0.,0.,0.,0.); /// Evaluate the PROSPECT model for the given leaf biochemistry, returning
/// the hemispherical reflectance and transmittance of a single leaf.
@(noinline)
export prospect_result prospect(
float num_layers=1.5,          ///< The number of layers.
float incident_cone_angle=0.7, ///< The incident cone angle in radians.
float dry_matter=5.,           ///< The dry matter content in milligrams per square centimeter.
float water=0.01,              ///< The water content in centimeters equivalent thickness.
float chlorophylls=30.,        ///< The chlorophyll content in micrograms per square centimeter.
float anthocyanins=1.,         ///< The anthocyanin content in micrograms per square centimeter.
float carotenoids=1.5,         ///< The carotenoid content in micrograms per square centimeter.
float xanthophyll_cycle=0.,    ///< The xanthophyll de-epoxidation state, 0 for violaxanthin and 1 for zeaxanthin.
float proteins=0.,             ///< The protein content in milligrams per square centimeter.
float carbons=0.,              ///< The carbon constituent content in milligrams per square centimeter.
float browns=0.,               ///< The brown pigment content in arbitrary units.
){
num_layers=#max(num_layers,1.);
const auto contents=auto(chlorophylls,carotenoids,anthocyanins,browns,water,1e-3*dry_matter,1e-3*proteins,1e-3*carbons)/num_layers;
const auto xanthophylls=carotenoids*(1.-clamp(xanthophyll_cycle,0.,1.5))/num_layers;
color ior(0);
color k(0);
for(int i=0;i<$WAVELENGTH_BASE_MAX;i++){
const auto {w0,w}=_uniform_lerp_index_and_fraction(PROSPECT_TABLE_SIZE,PROSPECT_MIN_WAVELENGTH,PROSPECT_MAX_WAVELENGTH,$state.wavelength_base[i]);
const auto {x0,x}=_uniform_lerp_index_and_fraction(PROSPECT_CX_TABLE_SIZE,PROSPECT_CX_MIN_WAVELENGTH,PROSPECT_CX_MAX_WAVELENGTH,$state.wavelength_base[i]);
ior[i]=lerp(PROSPECT_TABLE_IOR[w0],PROSPECT_TABLE_IOR[w0+1],w);
k[i]=dot(lerp(PROSPECT_TABLE_K[w0],PROSPECT_TABLE_K[w0+1],w),contents)-xanthophylls*lerp(PROSPECT_CX_TABLE[x0],PROSPECT_CX_TABLE[x0+1],x);
}
const auto tau=return_from{
const auto num=(1.236150246012*k+3.672877420834)*k+1.;
const auto den=((0.618075123006*k+3.664716300259)*k+4.62190363405)*k+1.;
return clamp(#exp(-k)*num/den,0.,0.999);
};
const auto t12=return_from{
auto tmp(-0.17369388*ior+1.3189973);
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
auto tmp(0.59796905,-1.904108,1.6576156);
tmp=tmp*incident_cone_angle+auto(-4.1001221,12.956352,-11.049849);
tmp=tmp*incident_cone_angle+auto(11.477769,-36.044872,30.242981);
tmp=tmp*incident_cone_angle+auto(-17.172335,53.666636,-44.411331);
tmp=tmp*incident_cone_angle+auto(15.069425,-46.911094,38.28977);
tmp=tmp*incident_cone_angle+auto(-7.8923812,24.474973,-19.667279);
tmp=tmp*incident_cone_angle+auto(2.4020134,-7.4210148,5.8397553);
tmp=tmp*incident_cone_angle+auto(-0.38620638,1.187749,-0.90387653);
tmp=tmp*incident_cone_angle+auto(-0.048754145,0.016941738,1.0405082);
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

static const char *const models_marmit = R"*(/// MARMIT -- a physically based way to layer a film of water over an arbitrary
/// diffuse reflectance spectrum, darkening and spectrally reshaping it the way a
/// wet surface differs from a dry one. It is the reflective, water-on-top analog
/// of the leaf model in `prospect.smdl`, and is built the same way: interpolate
/// tabulated optical constants of water per wavelength, then evaluate a small
/// closed-form radiative-transfer expression using cheap analytic fits in place
/// of the special functions the reference model calls.
///
/// References:
///   Bablet et al. (2018)   -- MARMIT
///   Dupiau et al. (2022)   -- MARMIT-2
///   Segelstein (1981)      -- real refractive index of water
///   Buiteveld/Kou/Wieliczka -- absorption coefficient of water (cm^-1)
#smdl
using ::math import *;

/// The result of the MARMIT model, being the reflectance of the wetted
/// surface.
export struct marmit_result{
/// Reflectance after wetting: the `wet_fraction`-weighted mix of the wetted
/// and dry reflectances (this is the model's primary output).
color reflectance=color(0);

/// Reflectance of the fully wetted surface (i.e. `wet_fraction` = 1), exposed
/// as a convenience since it is computed along the way.
color reflectance_wet=color(0);
};

/// The minimum wavelength of the MARMIT table in nanometers.
export const float MARMIT_MIN_WAVELENGTH=4e2;
/// The maximum wavelength of the MARMIT table in nanometers.
export const float MARMIT_MAX_WAVELENGTH=25e2;
/// The number of entries in the MARMIT table.
export const int MARMIT_TABLE_SIZE=264; /// The exponent of the generalized power mean that mixes the wet and dry
/// reflectances.
export const float MARMIT_MIXING_EXPONENT=2.27;

/// The tabulated absorption coefficient of water in inverse centimeters.
export static const auto MARMIT_TABLE_ALPHA=float[264](58e-6,6.4984791e-5,7.3961977e-5,8.3942966e-5,9.3923954e-5,1.0390494e-4,1.1190875e-4,1.1989354e-4,1.2787833e-4,1.378289e-4,1.5169582e-4,1.734981e-4,2.0808745e-4,2.7126996e-4,3.4692395e-4,4.0768821e-4,4.2214829e-4,4.6838403e-4,5.2480989e-4,6.0760456e-4,6.7002281e-4,7.3120532e-4,8.5206084e-4,0.0010678935,0.0014402015,0.0021741901,0.0026327338,0.0027589696,0.0028618251,0.0029807224,3102673e-9,0.0032533346,0.0034902357,0.0039903916,0.0041432776,0.0043053574,0.0046202662,0.0051960266,0.0062190532,0.0077595399,0.010478373,0.014529217,0.021402958,0.026493449,0.027380042,0.027681255,0.027633087,0.027165802,0.026211502,0.024305548,0.022633673,0.021824388,0.022383559,0.024786357,0.033469194,0.039156262,0.041295403,0.043371049,0.045305894,0.048309354,0.052412308,0.056949532,0.061372152,0.066089209,0.07268008,0.083459224,0.10801803,0.14987386,0.20914362,0.30817464,0.42640746,0.47426548,0.48559121,0.47610929,0.44816959,0.41265352,0.37059379,0.32529306,0.27932866,0.23837934,0.20328992,0.17535392,0.15572677,0.14497347,0.14078527,0.14372349,0.15596563,0.1744163,0.19891388,0.22535442,0.24684612,0.30258614,0.43825438,0.70110019,0.99940737,1.1558965,1.2017063,1.2266625,1.2473806,1.2683121,1.2754115,1.2638987,1.2427951,1.2082091,1.1866358,1.1577352,1.1289543,1.1030914,1.0875161,1.0838075,1.1151354,1.1700577,1.2450879,1.362082,1.5408251,1.7737077,2.0738001,2.4385417,2.8046537,3.2404328,3.6110189,4.0831335,4.9678182,6.9821837,10.777019,15.809918,20.957662,24.918992,27.547179,29.242555,30.198566,30.53757,30.391875,29.675172,28.037443,25.813958,23.61745,21.401333,19.255608,17.387155,15.678046,14.165977,12.875868,11.674949,10.679736,9.8499423,9.1589774,8.5319908,7.9861643,7.5072635,7.0878586,6.7712775,6.5171763,6.2906116,6.0902196,5.947433,5.7936615,5.669135,5.6119709,5.5547171,5.529419,5.5225403,5.5265036,5.5934486,5.7201826,5.8667218,6.065756,6.3052816,6.634616,7.0347278,7.504357,7.9664502,8.3214848,8.5709506,8.7513499,8.8025903,8.7476202,8.7948996,8.8635308,9.0019142,9.4151059,10.219798,11.683006,14.391653,19.719141,29.744088,48.905897,74.663711,100.24685,117.30163,126.51509,130.39706,129.42913,124.75055,117.9467,109.89923,101.29867,93.107823,85.246802,78.192875,71.596133,65.692118,60.475897,55.783826,51.541361,47.683278,44.360006,41.224355,38.385488,35.684155,33.401698,31.33872,29.394575,27.600382,26.0366,24.730099,23.484923,22.42997,21.549691,20.788565,20.125026,19.59856,19.200902,18.808893,18.470427,18.359254,18.337472,18.283925,18.377094,18.521219,18.72055,19.104502,19.542117,20.080917,20.747716,21.492014,22.285338,23.318404,24.37713,25.544056,26.859973,28.263872,29.75544,31.504263,33.175559,34.931125,37.040435,39.193771,41.539909,43.948561,46.437483,49.000875,51.653323,54.677894,58.138646,61.514553,65.43605,69.238078,73.853932,78.420393,82.970777,87.095007,92.04271,95.303); /// Evaluate the MARMIT model, layering a film of water of the given
/// thickness over the given dry reflectance spectrum.
@(noinline)
export marmit_result marmit(
color reflectance=color(0.3), ///< The dry reflectance of the underlying material.
float water_thickness=0.01,   ///< The equivalent water-film thickness in centimeters, clamped to be non-negative.
float wet_fraction=1.,        ///< The fraction of the surface covered by water, clamped to [0, 1].
float suspension_ior=1.53,    ///< Real refractive index n_i of particles suspended in the film.
float suspension_k=0.,        ///< Imaginary refractive index k_i of the suspended particles.
float suspension_fraction=0., ///< Volume fraction d_i of suspension in the film; 0 = pure water.
){
const color s=2.*saturate((color(&$state.wavelength_base[0])-MARMIT_MIN_WAVELENGTH)/(MARMIT_MAX_WAVELENGTH-MARMIT_MIN_WAVELENGTH))-1.;
color ior=return_from{
color ior=-6.01351e-5*s-0.0248482921;
ior=ior*s-5.980717e-4;
ior=ior*s+0.0015633866;
ior=ior*s-0.0109188837;
ior=ior*s-0.0244547175;
ior=ior*s+1.3129684894;
return ior;
};
color alpha(0);
for(int i=0;i<$WAVELENGTH_BASE_MAX;i++){
const auto {w0,w}=_uniform_lerp_index_and_fraction(MARMIT_TABLE_SIZE,MARMIT_MIN_WAVELENGTH,MARMIT_MAX_WAVELENGTH,$state.wavelength_base[i]);
alpha[i]=lerp(MARMIT_TABLE_ALPHA[w0],MARMIT_TABLE_ALPHA[w0+1],w);
}
if(suspension_fraction>0.){
const auto d=suspension_fraction;
const auto lambda=(MARMIT_MIN_WAVELENGTH+0.5*(s+1.)*(MARMIT_MAX_WAVELENGTH-MARMIT_MIN_WAVELENGTH))*1e-7;
const auto k_w=alpha*lambda*0.07957747154594767;
const auto ew_re=ior*ior-k_w*k_w;
const auto ew_im=2.*ior*k_w;
const float ei_re=suspension_ior*suspension_ior-suspension_k*suspension_k;
const float ei_im=2.*suspension_ior*suspension_k;
const auto e_re=d*ei_re+(1.-d)*ew_re;
const auto e_im=d*ei_im+(1.-d)*ew_im;
const auto mag=#sqrt(e_re*e_re+e_im*e_im);
ior=#sqrt(0.5*(mag+e_re));
alpha=12.566370614359172*0.5*e_im/ior/lambda;
}
const auto r12=return_from{
const auto v=(ior-1.)/(ior+1.);
const auto num=(((-6.087330777978*v+5.07253878015)*v-2.292800947895)*v+5.106344072818)*v+0.666666666667;
const auto den=(-14.079305083126*v+15.544722876889)*v+1.;
return saturate(v*num/den);
};
const auto t12=1-r12;
const auto t21=t12/(ior*ior);
const auto r21=1-t21;
const auto tau=return_from{
const auto x=#max(#min(alpha*water_thickness,50.),0.);
const auto num=(1.236150246012*x+3.672877420834)*x+1.;
const auto den=((0.618075123006*x+3.664716300259)*x+4.62190363405)*x+1.;
return saturate(#exp(-x)*num/den);
};
const auto tau2_reflectance=tau*tau*reflectance;
const auto wet=saturate(t12*t21*tau2_reflectance/(1-r21*tau2_reflectance));
const auto f=saturate(wet_fraction);
const auto e=1./MARMIT_MIXING_EXPONENT;
const auto mixed=#pow(f*#pow(wet,e)+(1.-f)*#pow(reflectance,e),MARMIT_MIXING_EXPONENT);
return marmit_result(reflectance: saturate(mixed),reflectance_wet: wet);
}

/// The minimum wavelength of the soil albedo table in nanometers.
export const float SOIL_MIN_WAVELENGTH=4e2;
/// The maximum wavelength of the soil albedo table in nanometers.
export const float SOIL_MAX_WAVELENGTH=2298.;
/// The number of knots in the soil albedo table.
export const int SOIL_TABLE_SIZE=261;

/// The 6 spectral component curves in logit space at each of the 261 knots:
/// constant, lightness, chroma, yellow-red, water, and water-squared terms.
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
auto(-9157672e-9,2.729442,-6.539579,12.77451,-1.671097,0.8202849),
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
auto(1.449916,3.121558,5.916482,-4931163e-9,-1.695017,0.7705945),
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
); /// Evaluate the empirical soil albedo model, mapping four normalized
/// pedogenic parameters to a dry-state reflectance spectrum.
@(noinline)
export color soil_albedo(
float humus=0.5,   ///< The humus content. 0 = pale quartz/carbonate, 1 = dark topsoil.
float iron=0.5,    ///< The iron-pigment load. 0 = gray, 1 = maximally pigmented.
float aridity=0.5, ///< The hematite:goethite balance. 0 = cool/moist goethite-yellow, 1 = hot/dry hematite-red.
float moisture=0., ///< The moisture darkening. 0 = bone dry, 1 = the darkest wet state the soil reaches.
){
const float h=saturate(humus);
const float fe=saturate(iron);
const float ar=saturate(aridity);
const float m=saturate(moisture);
const float lightness=1.-h;
const float chroma=#pow(fe,#exp(0.45*(h-0.5)-0.55*(ar-0.5)));
const float redness=ar;
const float l=lerp(-1.138774,-0.2968323,lightness);
const float c=lerp(-0.04893016,0.0518981,chroma);
const float r=redness<=0.5?0.014666*(1.-2.*redness):-0.02271506*(2.*redness-1.);
const float warp=((3.89536*m-6.037163)*m+3.081546)*m+0.06025699;
const float wet=1.04202*(warp*m);
const auto weights=auto(1.,l,c,r,wet,wet*wet);
color result(0);
for(int i=0;i<$WAVELENGTH_BASE_MAX;i++){
float t=(SOIL_TABLE_SIZE-1)*saturate(($state.wavelength_base[i]-SOIL_MIN_WAVELENGTH)/(SOIL_MAX_WAVELENGTH-SOIL_MIN_WAVELENGTH));
const int t0=#min(int(#floor(t)),SOIL_TABLE_SIZE-2);
t-=t0;
result[i]=1./(1.+#exp(-dot(lerp(SOIL_CURVES[t0],SOIL_CURVES[t0+1],t),weights)));
}
return result;
}
)*";

static const char *const models_metal_ior = R"*(/// Complex refractive indices of common metals -- the wavelength-dependent
/// n + ik that conductor Fresnel terms expect -- interpolated per wavelength
/// from published measurements. Each metal is backed by a table extracted from
/// the refractiveindex.info database and embedded in the support library (see
/// `lib/Support/MetalIOR.cc` for the citations and the processing applied);
/// `metal_ior()` linearly interpolates the table of the selected metal at the
/// current wavelengths in `$state.wavelength_base`.
///
/// Coverage varies by source: every metal covers at least 380nm to 2200nm,
/// and the ultraviolet and infrared ends differ, so the tabulated range of
/// each metal is noted in the `Metal` enumeration. Wavelengths outside a
/// metal's range clamp to the nearest table entry. Cobalt and lithium are
/// each stitched from two sources with the tail ratio-corrected for
/// continuity at the seam, and tin below 730nm is a Drude-Lorentz
/// extrapolation, so treat the visible appearance of tin as approximate.
#smdl
@(foreign pure)
void smdlEvalMetalIOR(int metal,int numWavelens,&float wavelens,&float iorN,&float iorK);

/// The metals with builtin complex IOR tables.
export enum Metal{
Ag=0, ///< Silver, 270..14000nm (Yang et al 2015).
Al,   ///< Aluminum, 0.124..14000nm (Rakic 1995).
Au,   ///< Gold, 300..14000nm (Olmon et al 2012).
Co,   ///< Cobalt, 188..2480nm (Johnson & Christy 1974 + Werner et al 2009).
Cu,   ///< Copper, 210..14000nm (Querry 1985).
CuZn, ///< Brass of 70% copper and 30% zinc, 210..14000nm (Querry 1985).
Fe,   ///< Iron, 210..14000nm (Querry 1985).
Hg,   ///< Liquid mercury, 63.6..6199nm (Inagaki et al 1981).
Li,   ///< Lithium, 326..8266nm (Mathewson & Myers 1971 + Rasigni 1977).
Mg,   ///< Magnesium, 0.0248..14000nm (Hagemann et al 1975).
Na,   ///< Sodium, 313..2238nm (Smith 1969).
Ni,   ///< Nickel, 248..6199nm (Rakic et al 1998).
Pb,   ///< Lead, 17.6..2480nm (Werner et al 2009).
Pt,   ///< Platinum, 248..12398nm (Rakic et al 1998).
Sn,   ///< Tin, 380..12000nm (Golovashkin & Motulevich 1964, Drude-Lorentz below 730nm).
Ti,   ///< Titanium, 248..14000nm (Rakic et al 1998).
Zn,   ///< Zinc, 17.6..2480nm (Werner et al 2009).
};

/// Evaluate the complex IOR of the given metal at the current wavelengths.
///
/// The result is a `complex` with `color` coefficients: `.a` is the real
/// index of refraction n and `.b` is the extinction coefficient k, ready to
/// be passed along to `df::fresnel_factor(ior: ..., extinction_coefficient:
/// ...)` to model a conductor.
@(macro)
export auto metal_ior(const Metal metal){
auto ior=complex(color(0),color(0));
const &float iorN=cast<&float>(&ior.a);
const &float iorK=cast<&float>(&ior.b);
smdlEvalMetalIOR(cast<int>(metal),$WAVELENGTH_BASE_MAX,&$state.wavelength_base[0],iorN,iorK);
return ior;
}
)*";

static const std::string_view all_names[]{
    "api",
    "anno",
    "debug",
    "df",
    "limits",
    "math",
    "scene",
    "state",
    "std",
    "tex",
    "extras::io",
    "extras::pcg32",
    "models::illuminant",
    "models::prospect",
    "models::marmit",
    "models::metal_ior",
};

[[nodiscard]] static const char *get_source_code(std::string_view name) {
  if (name == "api")
    return api;
  if (name == "anno")
    return anno;
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
  if (name == "extras::io")
    return extras_io;
  if (name == "extras::pcg32")
    return extras_pcg32;
  if (name == "models::illuminant")
    return models_illuminant;
  if (name == "models::prospect")
    return models_prospect;
  if (name == "models::marmit")
    return models_marmit;
  if (name == "models::metal_ior")
    return models_metal_ior;
  return nullptr;
}
#include "Builtin/Albedo/microfacet_ggx_smith_bsdf.inl"
#include "Builtin/Albedo/microfacet_beckmann_smith_bsdf.inl"
#include "Builtin/Albedo/simple_glossy_bsdf.inl"
#include "Builtin/Albedo/ward_geisler_moroder_bsdf.inl"
[[nodiscard]] static const AlbedoLUT *get_albedo(std::string_view name) {
  if (name == "microfacet_ggx_smith_bsdf")
    return &microfacet_ggx_smith_bsdf;
  if (name == "microfacet_beckmann_smith_bsdf")
    return &microfacet_beckmann_smith_bsdf;
  if (name == "simple_glossy_bsdf")
    return &simple_glossy_bsdf;
  if (name == "ward_geisler_moroder_bsdf")
    return &ward_geisler_moroder_bsdf;
  return nullptr;
}

} // namespace smdl::builtin
