#include "Context.h"

#include "builtin.h"

namespace smdl {

Context::Context(Compiler &compiler) : compiler(compiler) {
  // Initialize keywords.
  keywords = {
      {"auto", get_comptime_meta_type(get_auto_type())},
      {"bool", get_comptime_meta_type(get_bool_type())},
      {"bool2", get_comptime_meta_type(get_bool_type(Extent(2)))},
      {"bool3", get_comptime_meta_type(get_bool_type(Extent(3)))},
      {"bool4", get_comptime_meta_type(get_bool_type(Extent(4)))},
      {"color", get_comptime_meta_type(get_color_type())},
      {"double", get_comptime_meta_type(get_double_type())},
      {"double2", get_comptime_meta_type(get_double_type(Extent(2)))},
      {"double3", get_comptime_meta_type(get_double_type(Extent(3)))},
      {"double4", get_comptime_meta_type(get_double_type(Extent(4)))},
      {"double2x2", get_comptime_meta_type(get_double_type(Extent(2, 2)))},
      {"double2x3", get_comptime_meta_type(get_double_type(Extent(2, 3)))},
      {"double2x4", get_comptime_meta_type(get_double_type(Extent(2, 4)))},
      {"double3x2", get_comptime_meta_type(get_double_type(Extent(3, 2)))},
      {"double3x3", get_comptime_meta_type(get_double_type(Extent(3, 3)))},
      {"double3x4", get_comptime_meta_type(get_double_type(Extent(3, 4)))},
      {"double4x2", get_comptime_meta_type(get_double_type(Extent(4, 2)))},
      {"double4x3", get_comptime_meta_type(get_double_type(Extent(4, 3)))},
      {"double4x4", get_comptime_meta_type(get_double_type(Extent(4, 4)))},
      {"float", get_comptime_meta_type(get_float_type())},
      {"float2", get_comptime_meta_type(get_float_type(Extent(2)))},
      {"float3", get_comptime_meta_type(get_float_type(Extent(3)))},
      {"float4", get_comptime_meta_type(get_float_type(Extent(4)))},
      {"float2x2", get_comptime_meta_type(get_float_type(Extent(2, 2)))},
      {"float2x3", get_comptime_meta_type(get_float_type(Extent(2, 3)))},
      {"float2x4", get_comptime_meta_type(get_float_type(Extent(2, 4)))},
      {"float3x2", get_comptime_meta_type(get_float_type(Extent(3, 2)))},
      {"float3x3", get_comptime_meta_type(get_float_type(Extent(3, 3)))},
      {"float3x4", get_comptime_meta_type(get_float_type(Extent(3, 4)))},
      {"float4x2", get_comptime_meta_type(get_float_type(Extent(4, 2)))},
      {"float4x3", get_comptime_meta_type(get_float_type(Extent(4, 3)))},
      {"float4x4", get_comptime_meta_type(get_float_type(Extent(4, 4)))},
      {"int", get_comptime_meta_type(get_int_type())},
      {"int2", get_comptime_meta_type(get_int_type(Extent(2)))},
      {"int3", get_comptime_meta_type(get_int_type(Extent(3)))},
      {"int4", get_comptime_meta_type(get_int_type(Extent(4)))},
      {"string", get_comptime_meta_type(get_string_type())},
      {"texture_2d", get_comptime_meta_type(texture2DType.get())},
      {"$DEBUG", get_comptime_bool(compiler.enableDebug)},
      {"$DOUBLE_EPS",
       get_comptime_double(std::numeric_limits<double>::epsilon())},
      {"$DOUBLE_MAX", get_comptime_double(std::numeric_limits<double>::max())},
      {"$DOUBLE_MIN", get_comptime_double(std::numeric_limits<double>::min())},
      {"$FLOAT_EPS", get_comptime_float(std::numeric_limits<float>::epsilon())},
      {"$FLOAT_MAX", get_comptime_float(std::numeric_limits<float>::max())},
      {"$FLOAT_MIN", get_comptime_float(std::numeric_limits<float>::min())},
      {"$HALF_PI", get_comptime_float(0.5f * 3.14159265359f)},
      {"$INF", get_comptime_float(std::numeric_limits<float>::infinity())},
      {"$INT_MIN", get_comptime_int(std::numeric_limits<int>::min())},
      {"$INT_MAX", get_comptime_int(std::numeric_limits<int>::max())},
      {"$NAN", get_comptime_float(std::numeric_limits<float>::quiet_NaN())},
      {"$PI", get_comptime_float(3.14159265359f)},
      {"$stderr", get_comptime_ptr(get_void_pointer_type(), &llvm::errs())},
      {"$stdout", get_comptime_ptr(get_void_pointer_type(), &llvm::outs())},
      {"$TWO_PI", get_comptime_float(2 * 3.14159265359f)},
      {"$WAVELENGTH_BASE_MAX",
       get_comptime_int(int(compiler.wavelengthBaseMax))},
  };

  // Compile builtin `api` module and use all exports as keywords!
  // - `enum intensity_mode`
  //   - `intensity_radiant_exitance`
  //   - `intensity_power`
  // - `tag bsdf` and `struct default_bsdf`
  // - `tag vdf` and `struct default_vdf`
  // - `tag edf` and `struct default_edf`
  // - `tag hair_bsdf` and `struct default_hair_bsdf`
  // - `struct material_emission`
  // - `struct material_surface`
  // - `struct material_volume`
  // - `struct material_geometry`
  // - `struct material`
  // - Function `$wyman_1931_xyz`
  // - Function `$wyman_1931_y`
  // - Function `$color_to_rgb`
  // - Function `$rgb_to_color`
  for (auto crumb{get_builtin_module("api")->lastCrumb}; crumb;
       crumb = crumb->prev) {
    if (crumb->is_exported() && crumb->has_simple_name()) {
      auto simpleName{llvm::StringRef(crumb->name[0])};
      SMDL_SANITY_CHECK(!keywords.contains(simpleName),
                        "keyword collision in builtin 'api' module");
      keywords[simpleName] = crumb->value;
    }
  }
  materialType =
      get_keyword_value("material").get_comptime_meta_type(*this, {});
}

Module *Context::get_builtin_module(llvm::StringRef name) {
  auto sourceCode{builtin::get_source_code(name)};
  if (!sourceCode) {
    return nullptr;
  }
  auto &mod{builtinModules[name]};
  if (!mod) {
    mod = allocator.allocate<Module>(name.str(), sourceCode);
    if (auto error{mod->parse(allocator)}) {
      throw std::move(*error);
    }
  }
  if (auto error{mod->compile(*this)}) {
    throw std::move(*error);
  }
  return mod.get();
}

const AlbedoLUT *Context::get_builtin_albedo_lut(llvm::StringRef name) {
  return builtin::get_albedo_lut(name);
}

Type *Context::get_arithmetic_type(Scalar scalar, Extent extent) {
  uint64_t key{};
  key |= uint64_t(scalar.intent) << 48;
  key |= uint64_t(scalar.numBits) << 32;
  key |= uint64_t(extent.numCols) << 16;
  key |= uint64_t(extent.numRows);
  auto &type{arithmeticTypes[key]};
  if (!type)
    type = allocator.allocate<ArithmeticType>(*this, scalar, extent);
  return type.get();
}

ArrayType *Context::get_array_type(Type *elemType, uint32_t size) {
  auto &type{arrayTypes[std::pair(elemType, size)]};
  if (!type)
    type = allocator.allocate<ArrayType>(*this, elemType, size);
  return type.get();
}

InferredSizeArrayType *
Context::get_inferred_size_array_type(Type *elemType, std::string sizeName) {
  auto &type{inferredSizeArrayTypes[std::pair(elemType, sizeName)]};
  if (!type)
    type = allocator.allocate<InferredSizeArrayType>(elemType,
                                                     std::move(sizeName));
  return type.get();
}

PointerType *Context::get_pointer_type(Type *pointeeType) {
  auto &type{pointerTypes[pointeeType]};
  if (!type)
    type = allocator.allocate<PointerType>(*this, pointeeType);
  return type.get();
}

EnumType *Context::get_enum_type(AST::Enum *decl) {
  auto &type{astTypes[decl]};
  if (!type)
    type = allocator.allocate<EnumType>(*decl);
  return static_cast<EnumType *>(type.get());
}

FunctionType *Context::get_function_type(AST::Function *decl) {
  auto &type{astTypes[decl]};
  if (!type)
    type = allocator.allocate<FunctionType>(*decl);
  return static_cast<FunctionType *>(type.get());
}

StructType *Context::get_struct_type(AST::Struct *decl) {
  auto &type{astTypes[decl]};
  if (!type)
    type = allocator.allocate<StructType>(*decl);
  return static_cast<StructType *>(type.get());
}

TagType *Context::get_tag_type(AST::Tag *decl) {
  auto &type{astTypes[decl]};
  if (!type)
    type = allocator.allocate<TagType>(std::string(decl->name.srcName));
  return static_cast<TagType *>(type.get());
}

Type *Context::get_union_type(llvm::ArrayRef<Type *> types) {
  auto caseTypes{UnionType::canonicalize_types(types)};
  if (caseTypes.size() == 1)
    return caseTypes[0];
  auto &type{unionTypes[caseTypes]};
  if (!type)
    type = allocator.allocate<UnionType>(*this, std::move(caseTypes));
  return type.get();
}

ComptimeUnionType *Context::get_comptime_union_type(UnionType *unionType) {
  auto &type{comptimeUnionTypes[unionType]};
  if (!type)
    type = allocator.allocate<ComptimeUnionType>(unionType);
  return type.get();
}

Type *Context::get_common_type(llvm::ArrayRef<Type *> types,
                               bool defaultToUnion,
                               const SourceLocation &srcLoc) {
  if (types.empty())
    return get_void_type();
  if (types.size() == 1)
    return types[0];
  auto getCommonTypeOfPair{[&](Type *typeA, Type *typeB) -> Type * {
    if (typeA == get_auto_type() || !typeA)
      return typeB;
    if (typeB == get_auto_type() || !typeB || typeA == typeB)
      return typeA;
    if (typeA->is_arithmetic() && typeB->is_arithmetic()) {
      auto arithTypeA{static_cast<ArithmeticType *>(typeA)};
      auto arithTypeB{static_cast<ArithmeticType *>(typeB)};
      if (arithTypeA->extent == arithTypeB->extent ||
          arithTypeA->extent.is_scalar() || arithTypeB->extent.is_scalar())
        return arithTypeA->get_common_type(*this, arithTypeB);
    }
    if ((typeA->is_arithmetic_scalar() && typeB->is_color()) ||
        (typeB->is_arithmetic_scalar() && typeA->is_color()))
      return get_color_type();
    if (!defaultToUnion || typeA->is_abstract() || typeB->is_abstract())
      srcLoc.throw_error("no common type between ", quoted(typeA->displayName),
                         " and ", quoted(typeB->displayName));
    return get_union_type({typeA, typeB});
  }};
  Type *commonType{types[0]};
  for (uint32_t i = 1; i < types.size(); i++)
    commonType = getCommonTypeOfPair(commonType, types[i]);
  return commonType;
}

ConversionRule Context::get_conversion_rule(Type *typeA, Type *typeB) {
  // If the source type is equivalent to the destination type OR the destination
  // type is pure `auto`, conversion is perfect!
  if (typeA == typeB || typeB == get_auto_type()) {
    return ConversionRule::Perfect;
  }
  if (typeA->is_abstract())
    return ConversionRule::Explicit;
  if (typeA->is_void())
    return ConversionRule::Implicit;
  // If the source and destination types are both arithmetic ...
  if (typeA->is_arithmetic() && typeB->is_arithmetic()) {
    // If the source and destination extents are equivalent, conversion is
    // implicit.
    if (static_cast<ArithmeticType *>(typeA)->extent ==
        static_cast<ArithmeticType *>(typeB)->extent)
      return ConversionRule::Implicit;
    // If the source type is a scalar and the destination type is a vector or
    // matrix, conversion is explicit.
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // auto vecOf1s = float4(1.0);
    // auto matOf1s = float4x4(1.0); // 1s on diagonal
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (typeA->is_arithmetic_scalar() &&
        (typeB->is_arithmetic_vector() || typeB->is_arithmetic_matrix()))
      return ConversionRule::Explicit;
  }
  // If the source and destination types are different enum and/or int-like
  // types, conversion is explicit.
  if ((typeA->is_enum() && typeB->is_enum()) ||
      (typeA->is_enum() && typeB->is_arithmetic_scalar() &&
       typeB->is_arithmetic_integral()) ||
      (typeB->is_enum() && typeA->is_arithmetic_scalar() &&
       typeA->is_arithmetic_integral())) {
    return ConversionRule::Explicit;
  }
  // If the source type is a pointer ...
  if (typeA->is_pointer()) {
    // If the destination type is also a pointer, conversion is explicit. NOTE:
    // At this point, we know the source type is not equivalent to the
    // destination type, so the pointer types are distinct.
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // auto ptrToInt = cast<&int>(ptr);
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (typeB->is_pointer()) {
      // ... unless the destination type is `&void`
      if (typeB == get_void_pointer_type())
        return ConversionRule::Implicit;
      // ... unless the destination type is a pointer to `auto`, in which case
      // conversion is perfect as long as the underlying pointee type conversion
      // is perfect.
      if (typeB->is_abstract())
        return get_conversion_rule(typeA->get_pointee_type(),
                                   typeB->get_pointee_type());
      return ConversionRule::Explicit;
    }
    // If the destination type is a boolean, conversion is implicit. This is a
    // non-NULL test.
    // ~~~~~~~~~~~~~~~~~~~~~
    // bool isNonNull = ptr;
    // ~~~~~~~~~~~~~~~~~~~~~
    if (typeB == get_bool_type()) {
      return ConversionRule::Implicit;
    }
    // If the destination type is a vector with equivalent scalar type,
    // conversion is explicit (Load from the pointer!).
    if (auto arithTypeB{llvm::dyn_cast<ArithmeticType>(typeB)};
        arithTypeB->extent.is_vector() &&
        arithTypeB->get_scalar_type(*this) == typeA->get_pointee_type()) {
      return ConversionRule::Explicit;
    }
  }
  // If the source type is a struct that is an instance of the
  // destination type, conversion is perfect.
  if (auto structTypeA{llvm::dyn_cast<StructType>(typeA)};
      structTypeA && structTypeA->is_instance_of(typeB)) {
    return ConversionRule::Perfect;
  }
  // If the source type is a union that is always an instance of the
  // destination type, conversion is perfect.
  if (auto unionTypeA{llvm::dyn_cast<UnionType>(typeA)};
      unionTypeA && unionTypeA->is_always_instance_of(typeB)) {
    return ConversionRule::Perfect;
  }
  // If the destination type is a union ...
  if (auto unionTypeB{llvm::dyn_cast<UnionType>(typeB)}) {
    // If the source type is a union and the destination type has all
    // of the source case types, conversion is implicit.
    if (auto unionTypeA{llvm::dyn_cast<UnionType>(typeA)}) {
      return unionTypeB->has_all_case_types(unionTypeA)
                 ? ConversionRule::Implicit
                 : ConversionRule::Explicit;
    } else {
      // If the source type is not a union and the destination type has the
      // source as a case type, conversion is implicit.
      if (unionTypeB->has_case_type(typeA))
        return ConversionRule::Implicit;
    }
  }
  // If the destination type is a compile-time union and the source type
  // is one of the case types, conversion is perfect.
  if (auto unionTypeB{llvm::dyn_cast<ComptimeUnionType>(typeB)};
      unionTypeB && unionTypeB->unionType->has_case_type(typeA)) {
    return ConversionRule::Perfect;
  }
  // If the source type is an instance of texture 2D (with concrete
  // texel type and tile dimensions) and the destination type is the abstract
  // texture 2D type, conversion is perfect.
  if (llvm::isa<Texture2DInstanceType>(typeA) &&
      llvm::isa<Texture2DType>(typeB)) {
    return ConversionRule::Perfect;
  }
  // If the destination type is a color ...
  if (typeB->is_color()) {
    // If the source type is float or float3, conversion is implicit.
    if (typeA == get_float_type() || typeA == get_float_type(Extent(3)))
      return ConversionRule::Implicit;
    // If the source type is a pointer to a float, conversion is explicit.
    // (Loads from the pointer)
    if (typeA == get_pointer_type(get_float_type()))
      return ConversionRule::Explicit;
  }
  // If the source type is an array ...
  if (typeA->is_array()) {
    // If the destination type is an inferred-size array, conversion is whatever
    // the element conversion is.
    if (typeB->is_inferred_size_array())
      return get_conversion_rule(
          static_cast<ArrayType *>(typeA)->elemType,
          static_cast<InferredSizeArrayType *>(typeB)->elemType);
#if 0
    // If the destination type is a pointer with the same element type, conversion is implicit.
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // auto someArray = float[4](/* ... */);
    // &float somePtr = someArray;
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (typeB->is_pointer() && typeA->get_element_type() == typeB->get_element_type())
      return ConversionRule::Implicit;
    // If the destination type is an array with the same size, conversion is explicit. NOTE: At this point, we know the
    // source type is not equivalent to the destination type, so we know the element types are different.
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // auto someArray = float[4](/* ... */);
    // auto someArray2 = cast<double[4]>(someArray);
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (typeB->is_array() && typeA->get_array_size() == typeB->get_array_size() &&
        get_conversion_rule(typeA->get_element_type(), typeB->get_element_type()) != ConversionRule::NotAllowed)
      return ConversionRule::Explicit;
#endif
  }
  return ConversionRule::NotAllowed;
}

Value Context::get_comptime_union_index_map(UnionType *unionTypeA,
                                            UnionType *unionTypeB) {
  auto &indexMap{unionIndexMaps[std::pair(unionTypeA, unionTypeB)]};
  if (!indexMap) {
    indexMap = Value::zero(
        get_array_type(get_int_type(), unionTypeA->caseTypes.size()));
    auto builder{llvm::IRBuilder<>(llvmContext)};
    for (unsigned i = 0; i < unionTypeA->caseTypes.size(); i++)
      indexMap.llvmValue = builder.CreateInsertValue(
          indexMap.llvmValue,
          get_comptime_int(
              unionTypeB->get_case_type_index(unionTypeA->caseTypes[i])),
          {i});
    indexMap = LValue(
        indexMap.type,
        new llvm::GlobalVariable(
            llvmModule, indexMap.type->llvmType, /*isConstant=*/true,
            llvm::GlobalValue::PrivateLinkage,
            static_cast<llvm::Constant *>(indexMap.llvmValue), ".union_map"));
  }
  return indexMap;
}

} // namespace smdl
