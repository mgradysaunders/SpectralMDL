// vim:foldmethod=marker:foldlevel=0:fmr=--{,--}
#include "Context.h"
#include "Emitter.h"
#include "Parser.h"
#include "builtin.h"

namespace smdl::Compiler {

Context::Context(MDLInstance &mdl, llvm::BumpPtrAllocator &bumpAllocator)
    : mdl(mdl),                                                                        //
      llvmContext(mdl.get_llvm_context()),                                             //
      llvmModule(mdl.get_llvm_module()),                                               //
      llvmLayout(mdl.get_llvm_module().getDataLayout()),                               //
      llvmTargetLibraryInfoImpl(llvm::Triple(llvm_get_native_target_triple())),        //
      llvmTargetLibraryInfo(llvmTargetLibraryInfoImpl),                                //
      bumpAllocator(bumpAllocator),                                                    //
      compilerFunctionType(bump_allocate<CompilerType>(*this, "compiler_function")),   //
      compilerIntrinsicType(bump_allocate<CompilerType>(*this, "compiler_intrinsic")), //
      compilerModuleType(bump_allocate<CompilerType>(*this, "compiler_module")),       //
      compilerTypeType(bump_allocate<CompilerType>(*this, "compiler_type")),           //
      autoType(bump_allocate<AutoType>(*this)),                                        //
      voidType(bump_allocate<VoidType>(*this)),                                        //
      colorType(bump_allocate<ColorType>(*this)),                                      //
      intensityModeType(bump_allocate<EnumType>(*this, builtin_enum_type<intensity_mode_t>)),
      lightProfileType(bump_allocate<StructType>(*this, builtin_struct_type<light_profile_t>)),
      tile2DType(bump_allocate<StructType>(*this, builtin_struct_type<tile_2d_t>)),
      texture2DType(bump_allocate<StructType>(*this, builtin_struct_type<texture_2d_t>)),
      texture3DType(bump_allocate<StructType>(*this, builtin_struct_type<texture_3d_t>)),
      textureCubeType(bump_allocate<StructType>(*this, builtin_struct_type<texture_cube_t>)),
      texturePtexType(bump_allocate<StructType>(*this, builtin_struct_type<texture_ptex_t>)),
      stateType(bump_allocate<StructType>(*this, builtin_struct_type<state_t>)),
      stringType(bump_allocate<StructType>(*this, builtin_struct_type<string_t>)),
      sourceLocationType(bump_allocate<StructType>(*this, builtin_struct_type<source_location_t>)),
      bsdfType(bump_allocate<TagType>(*this, "bsdf")),          //
      edfType(bump_allocate<TagType>(*this, "edf")),            //
      vdfType(bump_allocate<TagType>(*this, "vdf")),            //
      hairBsdfType(bump_allocate<TagType>(*this, "hair_bsdf")), //
      defaultBsdfType(bump_allocate<StructType>(*this, builtin_struct_type<default_bsdf_t>)),
      defaultEdfType(bump_allocate<StructType>(*this, builtin_struct_type<default_edf_t>)),
      defaultVdfType(bump_allocate<StructType>(*this, builtin_struct_type<default_vdf_t>)),
      defaultHairBsdfType(bump_allocate<StructType>(*this, builtin_struct_type<default_hair_bsdf_t>)),
      materialEmissionType(bump_allocate<StructType>(*this, builtin_struct_type<material_emission_t>)),
      materialSurfaceType(bump_allocate<StructType>(*this, builtin_struct_type<material_surface_t>)),
      materialVolumeType(bump_allocate<StructType>(*this, builtin_struct_type<material_volume_t>)),
      materialGeometryType(bump_allocate<StructType>(*this, builtin_struct_type<material_geometry_t>)),
      materialType(bump_allocate<StructType>(*this, builtin_struct_type<material_t>)) {
#define KEYWORD_CONSTANT(name, value) \
  {                                   \
    name, KeywordConstant { value }   \
  }
#define KEYWORD_TYPE(name, type)                          \
  {                                                       \
    name, KeywordConstant { get_compile_time_type(type) } \
  }
#define EXTENDED_KEYWORD_CONSTANT(name, value) \
  {                                            \
    name, KeywordConstant { value, true }      \
  }
#define EXTENDED_KEYWORD_TYPE(name, type)                       \
  {                                                             \
    name, KeywordConstant { get_compile_time_type(type), true } \
  }
  keywordConstants = {
      KEYWORD_TYPE("auto", autoType.get()),
      KEYWORD_TYPE("bsdf", bsdfType.get()),
      KEYWORD_TYPE("bool", get_bool_type()),
      KEYWORD_TYPE("bool2", get_bool_type(Extent(2))),
      KEYWORD_TYPE("bool3", get_bool_type(Extent(3))),
      KEYWORD_TYPE("bool4", get_bool_type(Extent(4))),
      EXTENDED_KEYWORD_TYPE("byte", get_arithmetic_type(Scalar::Byte)),
      KEYWORD_TYPE("color", get_color_type()),
      EXTENDED_KEYWORD_TYPE("default_bsdf", defaultBsdfType.get()),
      EXTENDED_KEYWORD_TYPE("default_edf", defaultEdfType.get()),
      EXTENDED_KEYWORD_TYPE("default_vdf", defaultVdfType.get()),
      EXTENDED_KEYWORD_TYPE("default_hair_bsdf", defaultHairBsdfType.get()),
      KEYWORD_TYPE("double", get_double_type()),
      KEYWORD_TYPE("double2", get_double_type(Extent(2))),
      KEYWORD_TYPE("double3", get_double_type(Extent(3))),
      KEYWORD_TYPE("double4", get_double_type(Extent(4))),
      KEYWORD_TYPE("double2x2", get_double_type(Extent(2, 2))),
      KEYWORD_TYPE("double2x3", get_double_type(Extent(2, 3))),
      KEYWORD_TYPE("double2x4", get_double_type(Extent(2, 4))),
      KEYWORD_TYPE("double3x2", get_double_type(Extent(3, 2))),
      KEYWORD_TYPE("double3x3", get_double_type(Extent(3, 3))),
      KEYWORD_TYPE("double3x4", get_double_type(Extent(3, 4))),
      KEYWORD_TYPE("double4x2", get_double_type(Extent(4, 2))),
      KEYWORD_TYPE("double4x3", get_double_type(Extent(4, 3))),
      KEYWORD_TYPE("double4x4", get_double_type(Extent(4, 4))),
      KEYWORD_TYPE("edf", edfType.get()),
      KEYWORD_TYPE("float", get_float_type()),
      KEYWORD_TYPE("float2", get_float_type(Extent(2))),
      KEYWORD_TYPE("float3", get_float_type(Extent(3))),
      KEYWORD_TYPE("float4", get_float_type(Extent(4))),
      KEYWORD_TYPE("float2x2", get_float_type(Extent(2, 2))),
      KEYWORD_TYPE("float2x3", get_float_type(Extent(2, 3))),
      KEYWORD_TYPE("float2x4", get_float_type(Extent(2, 4))),
      KEYWORD_TYPE("float3x2", get_float_type(Extent(3, 2))),
      KEYWORD_TYPE("float3x3", get_float_type(Extent(3, 3))),
      KEYWORD_TYPE("float3x4", get_float_type(Extent(3, 4))),
      KEYWORD_TYPE("float4x2", get_float_type(Extent(4, 2))),
      KEYWORD_TYPE("float4x3", get_float_type(Extent(4, 3))),
      KEYWORD_TYPE("float4x4", get_float_type(Extent(4, 4))),
      KEYWORD_TYPE("hair_bsdf", hairBsdfType.get()),
      KEYWORD_TYPE("int", get_int_type()),
      KEYWORD_TYPE("int2", get_int_type(Extent(2))),
      KEYWORD_TYPE("int3", get_int_type(Extent(3))),
      KEYWORD_TYPE("int4", get_int_type(Extent(4))),
      KEYWORD_TYPE("intensity_mode", intensityModeType.get()),
      KEYWORD_TYPE("light_profile", lightProfileType.get()),
      KEYWORD_TYPE("material_emission", materialEmissionType.get()),
      KEYWORD_TYPE("material_surface", materialSurfaceType.get()),
      KEYWORD_TYPE("material_volume", materialVolumeType.get()),
      KEYWORD_TYPE("material_geometry", materialGeometryType.get()),
      KEYWORD_TYPE("material", materialType.get()),
      EXTENDED_KEYWORD_TYPE("source_location", sourceLocationType.get()),
      KEYWORD_TYPE("string", stringType.get()),
      KEYWORD_TYPE("texture_2d", texture2DType.get()),
      KEYWORD_TYPE("texture_3d", texture3DType.get()),
      KEYWORD_TYPE("texture_cube", textureCubeType.get()),
      KEYWORD_TYPE("texture_ptex", texturePtexType.get()),
      EXTENDED_KEYWORD_TYPE("tile_2d", tile2DType.get()),
      KEYWORD_TYPE("vdf", vdfType.get()),
      KEYWORD_TYPE("void", voidType.get()),
      EXTENDED_KEYWORD_CONSTANT("null", Value::zero(get_type<void>())),
      KEYWORD_CONSTANT("intensity_radiant_exitance", RValue(intensityModeType.get(), get_compile_time_int(0))),
      KEYWORD_CONSTANT("intensity_power", RValue(intensityModeType.get(), get_compile_time_int(1))),
      EXTENDED_KEYWORD_CONSTANT("$DEBUG", get_compile_time_bool(mdl.enableDebug)),
      EXTENDED_KEYWORD_CONSTANT("$DOUBLE_MIN", get_compile_time_double(std::numeric_limits<double_t>::min())),
      EXTENDED_KEYWORD_CONSTANT("$DOUBLE_MAX", get_compile_time_double(std::numeric_limits<double_t>::max())),
      EXTENDED_KEYWORD_CONSTANT("$DOUBLE_EPS", get_compile_time_double(std::numeric_limits<double_t>::epsilon())),
      EXTENDED_KEYWORD_CONSTANT("$DOUBLE_INF", get_compile_time_double(std::numeric_limits<double_t>::infinity())),
      EXTENDED_KEYWORD_CONSTANT("$DOUBLE_NAN", get_compile_time_double(std::numeric_limits<double_t>::quiet_NaN())),
      EXTENDED_KEYWORD_CONSTANT("$FLOAT_MIN", get_compile_time_float(std::numeric_limits<float_t>::min())),
      EXTENDED_KEYWORD_CONSTANT("$FLOAT_MAX", get_compile_time_float(std::numeric_limits<float_t>::max())),
      EXTENDED_KEYWORD_CONSTANT("$FLOAT_EPS", get_compile_time_float(std::numeric_limits<float_t>::epsilon())),
      EXTENDED_KEYWORD_CONSTANT("$FLOAT_INF", get_compile_time_float(std::numeric_limits<float_t>::infinity())),
      EXTENDED_KEYWORD_CONSTANT("$FLOAT_NAN", get_compile_time_float(std::numeric_limits<float_t>::quiet_NaN())),
      EXTENDED_KEYWORD_CONSTANT("$HALF_PI", get_compile_time_float(1.57079632679489661923f)),
      EXTENDED_KEYWORD_CONSTANT("$INT_MIN", get_compile_time_int(std::numeric_limits<int_t>::min())),
      EXTENDED_KEYWORD_CONSTANT("$INT_MAX", get_compile_time_int(std::numeric_limits<int_t>::max())),
      EXTENDED_KEYWORD_CONSTANT("$PI", get_compile_time_float(3.14159265358979323846f)),
      EXTENDED_KEYWORD_CONSTANT("$TWO_PI", get_compile_time_float(6.28318530717958647692f)),
      EXTENDED_KEYWORD_CONSTANT("$WAVELENGTH_BASE_MAX", get_compile_time_int(mdl.wavelengthBaseMax)),
      EXTENDED_KEYWORD_CONSTANT("$stdout", get_compile_time_pointer(&llvm::outs())),
      EXTENDED_KEYWORD_CONSTANT("$stderr", get_compile_time_pointer(&llvm::errs()))};
#undef EXTENDED_KEYWORD_TYPE
#undef EXTENDED_KEYWORD_CONSTANT
#undef KEYWORD_TYPE
#undef KEYWORD_CONSTANT
  // Always load "::rgb"
  sanity_check_nonnull(get_builtin_module("rgb"));
}

llvm::StringRef Context::get_persistent_string(const llvm::Twine &twine) {
  if (twine.isTriviallyEmpty())
    return {};
  if (twine.isSingleStringLiteral())
    return twine.getSingleStringRef();
  if (twine.isSingleStringRef())
    return bump_duplicate(twine.getSingleStringRef());
  else
    return bump_duplicate(twine.str());
}

std::string Context::get_unique_name(llvm::StringRef name, llvm::Function *llvmFunc) {
  return name.str() + std::to_string(uniqueNames[name][llvmFunc]++);
}

bool Context::is_keyword(Module *module, llvm::StringRef name) {
  if (auto itr{keywordConstants.find(name)}; itr != keywordConstants.end())
    if (module->isSmdlSyntax || !itr->second.isSmdlSyntax)
      return true;
  return false;
}

AST::Expr *Context::parse_expression(const llvm::Twine &srcTwine) {
  auto src{get_persistent_string(srcTwine)};
  auto &expr{builtinExpressions[src]};
  if (!expr)
    expr = Parser(bumpAllocator, "(expr)", src, /*isSmdlSyntax=*/true).parse_expression();
  return expr.get();
}

AST::Decl *Context::parse_declaration(const llvm::Twine &srcTwine) {
  auto src{get_persistent_string(srcTwine)};
  auto &decl{
      builtinDeclarations.emplace_back(Parser(bumpAllocator, "(decl)", src, /*isSmdlSyntax=*/true).parse_global_declaration())};
  return decl.get();
}

ArithmeticType *Context::get_arithmetic_type(Scalar scalar, Extent extent) {
  uint64_t key{};
  key |= uint64_t(scalar) << 32;
  key |= uint64_t(extent.numRows) << 16;
  key |= uint64_t(extent.numCols);
  auto &type{arithmeticTypes[key]};
  if (!type)
    type.reset(bump_allocate<ArithmeticType>(*this, scalar, extent));
  return type.get();
}

//--{ Type relations
ConversionRule Context::get_conversion_rule(Type *srcType, Type *dstType) {
  sanity_check(srcType);
  sanity_check(dstType);
  // If the source type is equivalent to the destination type OR the destination type is pure 'auto', conversion is implicit.
  if (srcType == dstType || dstType->is_auto())
    return ConversionRule::Perfect;
  if (srcType->is_abstract())
    return ConversionRule::Explicit; // ?
  // If the source and destination types are both arithmetic ...
  if (srcType->is_arithmetic() && dstType->is_arithmetic()) {
    // If the source and destination extents are equivalent, conversion is implicit.
    if (srcType->extent == dstType->extent)
      return srcType->scalar < dstType->scalar ? ConversionRule::Implicit : ConversionRule::Explicit;
    // If the source type is a scalar and the destination type is a vector or matrix, conversion is explicit.
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // auto vecOf1s = float4(1.0);
    // auto matOf1s = float4x4(1.0); // 1s on diagonal
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (srcType->is_scalar() && (dstType->is_vector() || dstType->is_matrix()))
      return ConversionRule::Explicit;
  }
  // If the source and destination types are different enum types, conversion is explicit.
  if (srcType->is_enum() && dstType->is_enum())
    return ConversionRule::Explicit;
  // If the source and destination types are enum and int-like types, conversion is explicit.
  if ((srcType->is_enum() && dstType->is_scalar() && dstType->is_integral()) || //
      (dstType->is_enum() && srcType->is_scalar() && srcType->is_integral()))
    return ConversionRule::Explicit;
  // If the source type is a pointer ...
  if (srcType->is_pointer()) {
    // If the destination type is also a pointer, conversion is explicit. NOTE: At this point, we know the source type
    // is not equivalent to the destination type, so the pointer types are distinct.
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // auto ptrToInt = cast<&int>(ptr);
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (dstType->is_pointer()) {
      // ... unless the destination type is a pointer to 'auto', in which case conversion is perfect
      // as long as the underlying element type conversion is perfect.
      if (dstType->is_abstract())
        return get_conversion_rule(srcType->get_element_type(), dstType->get_element_type());
      return ConversionRule::Explicit;
    }
    // If the destination type is a boolean, conversion is implicit. This is a non-NULL test.
    // ~~~~~~~~~~~~~~~~~~~~~
    // bool isNonNull = ptr;
    // ~~~~~~~~~~~~~~~~~~~~~
    if (dstType == get_bool_type())
      return ConversionRule::Implicit;
    // If the destination type is a vector, color, or array type with an equivalent element type, conversion
    // is explicit.
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // &float ptr = /* ... */;
    // auto loadVec = float4(ptr);
    // auto loadColor = color(ptr);
    // auto loadArray = float[7](ptr);
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if ((dstType->is_vector() || dstType->is_color() || dstType->is_array()) &&
        srcType->get_element_type() == dstType->get_element_type())
      return ConversionRule::Explicit;
  }
  // If the source type is an array ...
  if (srcType->is_array()) {
    // If the destination type is a pointer with the same element type, conversion is implicit.
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // auto someArray = float[4](/* ... */);
    // &float somePtr = someArray;
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (dstType->is_pointer() && srcType->get_element_type() == dstType->get_element_type())
      return ConversionRule::Implicit;
    // If the destination type is an array with the same size, conversion is explicit. NOTE: At this point, we know the
    // source type is not equivalent to the destination type, so we know the element types are different.
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // auto someArray = float[4](/* ... */);
    // auto someArray2 = cast<double[4]>(someArray);
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (dstType->is_array() && srcType->get_array_size() == dstType->get_array_size() &&
        get_conversion_rule(srcType->get_element_type(), dstType->get_element_type()) != ConversionRule::NotAllowed)
      return ConversionRule::Explicit;
    // If the destination type is a size-deferred array, conversion is whatever the element conversion is.
    if (dstType->is_size_deferred_array())
      return get_conversion_rule(srcType->get_element_type(), dstType->get_element_type());
  }
  // If the source type is a struct ...
  if (srcType->is_struct()) {
    // If the source type is an instantiation of a template struct, conversion is perfect.
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // df::weighted_layer layer = df::weighted_layer(weight: 0.7, layer: someBsdf1, base: someBsdf0)
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (dstType->is_struct() && static_cast<StructType *>(srcType)->parentTemplate == static_cast<StructType *>(dstType))
      return ConversionRule::Perfect;
    // If the source type is tagged as implementing the tag, conversion is perfect.
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // bsdf someBsdf = df::diffuse_reflection_bsdf(tint: color(0.8));
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (dstType->is_tag() && static_cast<StructType *>(srcType)->has_tag(static_cast<TagType *>(dstType)))
      return ConversionRule::Perfect;
    // If the source type is a string and the destination type is a byte pointer, conversion is implicit.
    if (srcType == get_string_type() && dstType == get_pointer_type(get_byte_type()))
      return ConversionRule::Implicit;
  }
  // If the destination type is a union ...
  if (dstType->is_union()) {
    if (srcType->is_union()) {
      // If the source type is a union and the destination type has all source case types, conversion is implicit.
      if (static_cast<UnionType *>(dstType)->has_all_types(static_cast<UnionType *>(srcType)))
        return ConversionRule::Implicit;
    } else {
      // If the source type is not a union and the destination type has the source as a case type, conversion in implicit.
      if (static_cast<UnionType *>(dstType)->has_type(srcType))
        return ConversionRule::Implicit;
    }
  }
  // If the source type is a union ...
  if (srcType->is_union()) {
    // If the destination type is a tag, conversion is perfect if every type in the union has the tag.
    if (dstType->is_tag() && static_cast<UnionType *>(srcType)->always_has_tag(static_cast<TagType *>(dstType)))
      return ConversionRule::Perfect;
    // If the destination type is a struct, conversion is perfect if every type in the union is an instance of the struct.
    if (dstType->is_struct() &&
        static_cast<UnionType *>(srcType)->always_has_parent_template(static_cast<StructType *>(dstType)))
      return ConversionRule::Perfect;
  }
  // If the destination type is a color ...
  if (dstType->is_color()) {
    // If the source type is float or float3, conversion is implicit.
    if (srcType == get_float_type() || srcType == get_float_type(Extent(3)))
      return ConversionRule::Implicit;
    // If the source type is a pointer to a float, conversion is explicit. (Loads from the pointer)
    if (srcType == get_pointer_type(get_float_type()))
      return ConversionRule::Explicit;
  }
  // If the destination type is a compile-time union ...
  if (dstType->is_compile_time_union()) {
    // If the union type has the source type as an alternative, conversion is perfect.
    if (static_cast<CompileTimeUnionType *>(dstType)->unionType->has_type(srcType))
      return ConversionRule::Perfect;
  }
  // Not allowed!
  return ConversionRule::NotAllowed;
}

bool Context::is_subset_of(Type *lhsType, Type *rhsType) {
  if (lhsType == rhsType || rhsType->is_auto())
    return true;
  if (rhsType->is_tag() || (rhsType->is_struct() && rhsType->is_abstract()))
    return is_perfectly_convertible(lhsType, rhsType);
  if (rhsType->is_union())
    return lhsType->is_union() ? static_cast<UnionType *>(rhsType)->has_all_types(static_cast<UnionType *>(lhsType))
                               : static_cast<UnionType *>(rhsType)->has_type(lhsType);
  if (lhsType->is_pointer() && rhsType->is_pointer() && rhsType->is_abstract())
    return is_subset_of(lhsType->get_element_type(), rhsType->get_element_type());
  if (lhsType->is_array() && rhsType->is_array()) {
    if (static_cast<ArrayType *>(rhsType)->size != 0 &&
        static_cast<ArrayType *>(rhsType)->size != static_cast<ArrayType *>(lhsType)->size)
      return false;
    return is_subset_of(lhsType->get_element_type(), rhsType->get_element_type());
  }
  return false;
}

Type *Context::get_common_type_of_pair(Type *lhsType, Type *rhsType, bool defaultToUnion, const AST::SourceLocation &srcLoc) {
  if (lhsType == get_auto_type() || !lhsType)
    return rhsType;
  if (rhsType == get_auto_type() || !rhsType || lhsType == rhsType)
    return lhsType;
  auto scalar{std::max(lhsType->scalar, rhsType->scalar)};
  auto extent{lhsType->extent};
  if (lhsType->extent.is_scalar()) {
    extent = rhsType->extent;
  } else if (rhsType->extent.is_scalar()) {
    extent = lhsType->extent;
  } else if (lhsType->extent != rhsType->extent) {
    if (!defaultToUnion || lhsType->is_abstract() || rhsType->is_abstract())
      srcLoc.report_error(std::format("no common type between '{}' and '{}'", lhsType->name, rhsType->name));
    return get_union_type({lhsType, rhsType});
  }
  if (lhsType->is_arithmetic() && rhsType->is_arithmetic()) {
    return get_arithmetic_type(scalar, extent);
  } else if (
      (lhsType->is_arithmetic() && lhsType->extent.is_scalar() && rhsType->is_color()) ||
      (rhsType->is_arithmetic() && rhsType->extent.is_scalar() && lhsType->is_color())) {
    return get_color_type();
  } else {
    if (!defaultToUnion || lhsType->is_abstract() || rhsType->is_abstract())
      srcLoc.report_error(std::format("no common type between '{}' and '{}'", lhsType->name, rhsType->name));
    return get_union_type({lhsType, rhsType});
  }
}

Type *Context::get_common_type(llvm::ArrayRef<Type *> types, bool defaultToUnion, const AST::SourceLocation &srcLoc) {
  if (types.empty())
    return nullptr;
  if (types.size() == 1)
    return types[0];
  Type *commonType{types[0]};
  for (uint32_t i = 1; i < types.size(); i++)
    commonType = get_common_type_of_pair(commonType, types[i]);
  return commonType;
}
//--}

//--{ Compile-time constants
Value Context::get_compile_time_bool(bool value) {
  return RValue(get_bool_type(), llvm::ConstantInt::getBool(get_bool_type()->llvmType, value));
}

Value Context::get_compile_time_int(const llvm::APInt &value) {
  return RValue(get_int_type(), llvm::ConstantInt::get(get_int_type()->llvmType, value.sextOrTrunc(sizeof(int_t) * 8)));
}

Value Context::get_compile_time_FP(llvm::APFloat value, AST::Precision precision) {
  auto type{precision == AST::Precision::Double ? get_double_type() : get_float_type()};
  bool losesInfo{};
  value.convert(type->llvmType->getFltSemantics(), llvm::RoundingMode::NearestTiesToEven, &losesInfo);
  return RValue(type, llvm::ConstantFP::get(type->llvmType, value));
}

Value Context::get_compile_time_string(llvm::StringRef value) {
  auto &result{compileTimeStrings[value]};
  if (!result) {
    auto builder{llvm::IRBuilder<>(llvmContext)};
    auto ptr{builder.CreateGlobalStringPtr(value, get_unique_name("str"), 0, &llvmModule)};
    result = Value::zero(stringType.get());
    result.llvmValue = builder.CreateInsertValue(result, ptr, {0});
    result.llvmValue = builder.CreateInsertValue(result, get_compile_time_int(value.size()), {1});
  }
  return result;
}

Value Context::get_compile_time_source_location(const AST::SourceLocation &value) {
  auto result{Value::zero(sourceLocationType.get())};
  auto builder{llvm::IRBuilder<>(llvmContext)};
  result.llvmValue = builder.CreateInsertValue(result, get_compile_time_string(value.file), {0});
  result.llvmValue = builder.CreateInsertValue(result, get_compile_time_int(value.line), {1});
  return result;
}

Value Context::get_compile_time_pointer(const void *value) {
  auto type{get_pointer_type(voidType.get())};
  return RValue(
      type, llvm::IRBuilder<>(llvmContext).CreateIntToPtr(llvm_ptr_as_constant_int(llvmContext, value), type->llvmType));
}

Value Context::get_compile_time_union_index_map(UnionType *srcType, UnionType *dstType) {
  auto &indexMap{compileTimeUnionIndexMaps[std::pair(srcType, dstType)]};
  if (!indexMap) {
    indexMap = Value::zero(get_array_type(get_int_type(), srcType->types.size()));
    auto builder{llvm::IRBuilder<>(llvmContext)};
    for (unsigned i{}; i < srcType->types.size(); i++)
      indexMap.llvmValue = builder.CreateInsertValue(indexMap, get_compile_time_int(dstType->index_of(srcType->types[i])), {i});
    indexMap = LValue(
        indexMap.type, new llvm::GlobalVariable(
                           llvmModule, indexMap.type->llvmType, /*isConst=*/true, llvm::GlobalValue::PrivateLinkage,
                           static_cast<llvm::Constant *>(indexMap.llvmValue), get_unique_name("union_map")));
  }
  return indexMap;
}
//--}

Function *Context::get_function(Emitter &emitter, AST::Function *decl) {
  auto &func{functions[decl]};
  if (!func)
    func.reset(bump_allocate<Function>(emitter, *decl));
  return func.get();
}

Function *Context::get_function(Emitter &emitter, const llvm::Twine &srcTwine) {
  auto decl{sanity_check_nonnull(llvm::dyn_cast<AST::Function>(parse_declaration(srcTwine)))};
  decl->module = emitter.module;
  decl->crumb = emitter.crumb;
  return get_function(emitter, decl);
}

Module *Context::get_builtin_module(llvm::StringRef name) {
  if (auto moduleSrc{builtin::get_src(name)}) {
    auto &module{builtinModules[name]};
    if (!module) {
      module.reset(bump_allocate<Module>(name, moduleSrc));
      module->parse(*this);
    }
    module->emit(*this);
    return module.get();
  }
  return nullptr;
}

//--{ Resolve: Identifiers
Value Context::resolve(Emitter &emitter, bool isAbs, llvm::ArrayRef<llvm::StringRef> names, const AST::SourceLocation &srcLoc) {
  if (names.size() == 1) {
    auto name{names[0]};
    if (name == "$data") {
      return get_compile_time_pointer(mdl.dataLookup.get());
    } else if (name == "$state") {
      if (!emitter.state)
        srcLoc.report_error(
            !emitter.get_llvm_function() ? "'$state' is unavailable at module scope"
                                         : "'$state' is unavailable in '@(pure)' function");
      return emitter.state;
    }
    // Resolve types
    if (auto itr{keywordConstants.find(name)}; itr != keywordConstants.end())
      if (emitter.module->isSmdlSyntax || !itr->second.isSmdlSyntax)
        return itr->second.value;
    // Unqualified simple names may shadow other values
    if (!isAbs)
      if (auto crumb{Crumb::find(emitter.crumb, name, emitter.get_llvm_function())})
        return crumb->value;
  }
  if (auto crumb{Crumb::find(emitter.crumb, names, emitter.get_llvm_function())})
    return crumb->value;
  // Let absolute identifiers in builtin modules like '::math::abs' resolve even if the module has
  // not been imported yet. This may be necessary for generated material functions using '::df' if the
  // user did not import '::df::*'.
  if (names.size() == 2 && isAbs) {
    if (auto builtin{get_builtin_module(names[0])}) {
      if (auto crumb{Crumb::find(builtin->lastCrumb, names[1], emitter.get_llvm_function())}) {
        return crumb->value;
      }
    }
  }
  srcLoc.report_error(std::format("can't resolve identifier '{}'", FormatJoin(names, "::")));
  return {};
}
//--}

//--{ Resolve: Arguments
llvm::SmallVector<Value> Context::resolve_arguments(
    Emitter &emitter0, const ParamList &params, const ArgList &args, const AST::SourceLocation &srcLoc,
    llvm::SmallVector<Type *> *argParamTypes, bool dontEmit) {
  if (argParamTypes)
    argParamTypes->resize(args.size());
  // Obvious case: If there are more arguments than parameters, resolution fails.
  if (args.size() > params.size())
    srcLoc.report_error("too many arguments");
  // Obvious case: If there are argument names that do not correspond to any parameter names, resolution fails.
  if (!args.has_only_these_names(params.get_names()))
    srcLoc.report_error("invalid argument name(s)");

  // The argument resolution logic.
  auto values{llvm::SmallVector<Value>{}};
  auto isUsed{llvm::SmallVector<bool>(args.size())};
  for (auto &param : params) {
    // Attempt to resolve the argument for this parameter.
    auto arg{[&]() -> const Arg * {
      // 1. Look for an explicitly named argument.
      for (size_t i{}; i < args.size(); i++) {
        if (args[i].name == param.name) {
          // If this has already been resolved by a positional argument, resolution fails.
          if (isUsed[i])
            srcLoc.report_error("invalid argument name(s)");
          isUsed[i] = true;
          if (argParamTypes)
            (*argParamTypes)[i] = param.type;
          return &args[i];
        }
      }
      // 2. If there is no named argument, default to the next positional argument.
      for (size_t i{}; i < args.size(); i++) {
        if (args[i].name.empty() && !isUsed[i]) {
          isUsed[i] = true;
          if (argParamTypes)
            (*argParamTypes)[i] = param.type;
          return &args[i];
        }
      }
      return nullptr;
    }()};
    if (arg) {
      values.push_back(arg->value);
      if (!is_implicitly_convertible(arg->value.type, param.type))
        srcLoc.report_error("invalid argument type(s)");
    } else if (param.init) {
      values.push_back(Value()); // Initialize later
    } else {
      // There is no argument and no default!
      srcLoc.report_error(std::format("missing argument for parameter '{}'", param.name));
    }
  }
  // At this point, we should have resolved everything.
  for (size_t i{}; i < args.size(); i++) {
    sanity_check(isUsed[i]);
  }
  if (!dontEmit) {
    Emitter emitter1{&emitter0};
    emitter1.move_to(emitter0.get_insert_block());
    emitter1.module = params.module ? params.module : emitter0.module;
    emitter1.crumb = params.crumb;
    for (size_t i{}; i < params.size(); i++) {
      auto &param{params[i]};
      auto &value{values[i]};
      value = emitter1.construct(param.type, value ? value : emitter1.emit(*param.init), param.srcLoc);
      emitter1.push(value, param.name, {}, param.srcLoc);
    }
    emitter1.emit_unwind(params.crumb);
    emitter0.move_to(emitter1.get_insert_block());
  }
  return values;
}
//--}

Module *Context::resolve_module(
    Emitter &emitter, bool isAbs, llvm::ArrayRef<llvm::StringRef> importPath, const AST::SourceLocation &srcLoc) {
  // First resolve using aliases in the import path to construct the full import path.
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // using menu = "coffee shop"::drinks;
  // import menu::latte::*;
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // In this case the import path is 'menu::latte' and 'menu' is an alias for '"coffee shop"::drinks'
  // so that the full import path is actually '"coffee shop"::drinks::latte'.
  auto fullImportPath{llvm::SmallVector<llvm::StringRef>{}};
  resolve_using_aliases(emitter.crumb, importPath, fullImportPath);
  sanity_check(!fullImportPath.empty());

  // If the import path is absolute and is a single name, we prioritize builtins.
  if (isAbs && fullImportPath.size() == 1) {
    if (auto module{get_builtin_module(fullImportPath[0])})
      return module;
  }

  if (!emitter.module->is_builtin()) {
    // Add the elements of the full import path to the directory path of the current module and
    // lexically normalize the result. Suppose the current module file is '/path/to/package/current.mdl' and
    // we're resolving a slightly more interesting version of the above example.
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // import .::..::menu::latte::*;
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // The filesystem directory path we end up with is '/path/to/package/./../coffee shop/drinks', which lexically
    // normalizes to '/path/to/coffee shop/drinks'.
    //
    auto dirPath{emitter.module->directory_path()};
    for (unsigned i = 0; i + 1 < fullImportPath.size(); i++) // NOTE: Only up to the next-to-last element!
      dirPath.append(std::string_view(fullImportPath[i]));
    dirPath = dirPath.lexically_normal();

    // Now look for a module with the expected name that lives in the expected directory.
    for (auto &module : mdl.modules) {
      if (module.get() != emitter.module && module->name == fullImportPath.back() && !module->is_builtin()) {
        if (std::error_code ec{}; std::filesystem::equivalent(module->directory_path(), dirPath, ec)) {
          module->emit(*this);
          return module.get();
        }
      }
    }
  }

  if (fullImportPath.size() == 1) {
    if (auto module{get_builtin_module(fullImportPath[0])})
      return module;
  }
  srcLoc.report_error(std::format("can't resolve module '{}'", format_join(importPath, "::")));
  return nullptr;
}

void Context::resolve_using_aliases(
    Crumb *crumb, llvm::ArrayRef<llvm::StringRef> importPath, llvm::SmallVector<llvm::StringRef> &fullImportPath) {
  auto findAliasCrumb{[](Crumb *crumb, llvm::StringRef importPathName) -> Crumb * {
    for (; crumb; crumb = crumb->prev)
      if (crumb->is_ast_using_alias() && static_cast<AST::UsingAlias *>(crumb->node)->name.srcName == importPathName)
        return crumb;
    return nullptr;
  }};
  for (auto importPathName : importPath) {
    if (auto aliasCrumb{findAliasCrumb(crumb, importPathName)}) {
      resolve_using_aliases(
          aliasCrumb, static_cast<AST::UsingAlias *>(aliasCrumb->node)->path->get_string_refs(), fullImportPath);
    } else {
      fullImportPath.push_back(importPathName);
    }
  }
}

} // namespace smdl::Compiler
