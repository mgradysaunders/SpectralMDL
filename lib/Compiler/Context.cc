#include "Context.h"

#include "builtin.h"

namespace smdl {

Context::Context(Compiler &compiler) : compiler(compiler) {
  // Initialize keywords.
  mKeywords = {
      {"auto", getComptimeMetaType(getAutoType())},
      {"bool", getComptimeMetaType(getBoolType())},
      {"bool2", getComptimeMetaType(getBoolType(Extent(2)))},
      {"bool3", getComptimeMetaType(getBoolType(Extent(3)))},
      {"bool4", getComptimeMetaType(getBoolType(Extent(4)))},
      {"color", getComptimeMetaType(getColorType())},
      {"double", getComptimeMetaType(getDoubleType())},
      {"double2", getComptimeMetaType(getDoubleType(Extent(2)))},
      {"double3", getComptimeMetaType(getDoubleType(Extent(3)))},
      {"double4", getComptimeMetaType(getDoubleType(Extent(4)))},
      {"double2x2", getComptimeMetaType(getDoubleType(Extent(2, 2)))},
      {"double2x3", getComptimeMetaType(getDoubleType(Extent(2, 3)))},
      {"double2x4", getComptimeMetaType(getDoubleType(Extent(2, 4)))},
      {"double3x2", getComptimeMetaType(getDoubleType(Extent(3, 2)))},
      {"double3x3", getComptimeMetaType(getDoubleType(Extent(3, 3)))},
      {"double3x4", getComptimeMetaType(getDoubleType(Extent(3, 4)))},
      {"double4x2", getComptimeMetaType(getDoubleType(Extent(4, 2)))},
      {"double4x3", getComptimeMetaType(getDoubleType(Extent(4, 3)))},
      {"double4x4", getComptimeMetaType(getDoubleType(Extent(4, 4)))},
      {"float", getComptimeMetaType(getFloatType())},
      {"float2", getComptimeMetaType(getFloatType(Extent(2)))},
      {"float3", getComptimeMetaType(getFloatType(Extent(3)))},
      {"float4", getComptimeMetaType(getFloatType(Extent(4)))},
      {"float2x2", getComptimeMetaType(getFloatType(Extent(2, 2)))},
      {"float2x3", getComptimeMetaType(getFloatType(Extent(2, 3)))},
      {"float2x4", getComptimeMetaType(getFloatType(Extent(2, 4)))},
      {"float3x2", getComptimeMetaType(getFloatType(Extent(3, 2)))},
      {"float3x3", getComptimeMetaType(getFloatType(Extent(3, 3)))},
      {"float3x4", getComptimeMetaType(getFloatType(Extent(3, 4)))},
      {"float4x2", getComptimeMetaType(getFloatType(Extent(4, 2)))},
      {"float4x3", getComptimeMetaType(getFloatType(Extent(4, 3)))},
      {"float4x4", getComptimeMetaType(getFloatType(Extent(4, 4)))},
      {"int", getComptimeMetaType(getIntType())},
      {"int2", getComptimeMetaType(getIntType(Extent(2)))},
      {"int3", getComptimeMetaType(getIntType(Extent(3)))},
      {"int4", getComptimeMetaType(getIntType(Extent(4)))},
      {"string", getComptimeMetaType(getStringType())},
      {"$DEBUG", getComptimeBool(compiler.enableDebug)},
      {"$DOUBLE_EPS",
       getComptimeDouble(std::numeric_limits<double>::epsilon())},
      {"$DOUBLE_MAX", getComptimeDouble(std::numeric_limits<double>::max())},
      {"$DOUBLE_MIN", getComptimeDouble(std::numeric_limits<double>::min())},
      {"$FLOAT_EPS", getComptimeFloat(std::numeric_limits<float>::epsilon())},
      {"$FLOAT_MAX", getComptimeFloat(std::numeric_limits<float>::max())},
      {"$FLOAT_MIN", getComptimeFloat(std::numeric_limits<float>::min())},
      {"$HALF_PI", getComptimeFloat(0.5f * 3.14159265359f)},
      {"$INF", getComptimeFloat(std::numeric_limits<float>::infinity())},
      {"$INT_MIN", getComptimeInt(std::numeric_limits<int>::min())},
      {"$INT_MAX", getComptimeInt(std::numeric_limits<int>::max())},
      {"$NAN", getComptimeFloat(std::numeric_limits<float>::quiet_NaN())},
      {"$PI", getComptimeFloat(3.14159265359f)},
      {"$stdin", getComptimePtr(getVoidPointerType(), stdin)},
      {"$stdout", getComptimePtr(getVoidPointerType(), stdout)},
      {"$stderr", getComptimePtr(getVoidPointerType(), stderr)},
      {"$TWO_PI", getComptimeFloat(2 * 3.14159265359f)},
      {"$WAVELENGTH_BASE_MAX", getComptimeInt(int(compiler.wavelengthBaseMax))},
  };

  // Compile builtin `API` module and use all exports as keywords!
  // - `enum intensity_mode`
  //   - `intensity_radiant_exitance`
  //   - `intensity_power`
  // - `tag bsdf` and `struct _default_bsdf`
  // - `tag vdf` and `struct _default_vdf`
  // - `tag edf` and `struct _default_edf`
  // - `tag hair_bsdf` and `struct  _default_hair_bsdf`
  // - `struct material_emission`
  // - `struct material_surface`
  // - `struct material_volume`
  // - `struct material_geometry`
  // - `struct material`
  // - Function `_wyman_xyz`
  // - Function `_wyman_y`
  // - Function `_color_to_rgb`
  // - Function `_rgb_to_color`
  for (auto crumb{getBuiltinModule("api")->mLastCrumb}; crumb;
       crumb = crumb->prev) {
    if (crumb->isExported() && crumb->hasSimpleName()) {
      auto simpleName{llvm::StringRef(crumb->name[0])};
      SMDL_SANITY_CHECK(!mKeywords.contains(simpleName),
                        "keyword collision in builtin 'api' module");
      mKeywords[simpleName] = crumb->value;
    }
  }
  mMaterialType = static_cast<StructType *>(getKeywordAsType("material"));
  mTexture2DType = static_cast<StructType *>(getKeywordAsType("texture_2d"));
  mTexture3DType = static_cast<StructType *>(getKeywordAsType("texture_3d"));
  mTextureCubeType =
      static_cast<StructType *>(getKeywordAsType("texture_cube"));
  mTexturePtexType =
      static_cast<StructType *>(getKeywordAsType("texture_ptex"));
  mBSDFMeasurementType =
      static_cast<StructType *>(getKeywordAsType("bsdf_measurement"));
  mLightProfileType =
      static_cast<StructType *>(getKeywordAsType("light_profile"));
  mSpectralCurveType =
      static_cast<StructType *>(getKeywordAsType("spectral_curve"));
  mComplexType = static_cast<StructType *>(getKeywordAsType("complex"));
}

Module *Context::getBuiltinModule(llvm::StringRef name) {
  auto sourceCode{builtin::get_source_code(name)};
  if (!sourceCode) {
    return nullptr;
  }
  auto &mod{mBuiltinModules[name]};
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

const AlbedoLUT *Context::getBuiltinAlbedo(llvm::StringRef name) {
  return builtin::get_albedo(name);
}

Type *Context::getArithmeticType(Scalar scalar, Extent extent) {
  uint64_t key{};
  key |= uint64_t(scalar.intent) << 48;
  key |= uint64_t(scalar.numBits) << 32;
  key |= uint64_t(extent.numCols) << 16;
  key |= uint64_t(extent.numRows);
  auto &type{mArithmeticTypes[key]};
  if (!type)
    type = allocator.allocate<ArithmeticType>(*this, scalar, extent);
  return type.get();
}

ArrayType *Context::getArrayType(Type *elemType, uint32_t size) {
  auto &type{mArrayTypes[std::pair(elemType, size)]};
  if (!type)
    type = allocator.allocate<ArrayType>(*this, elemType, size);
  return type.get();
}

InferredSizeArrayType *Context::getInferredSizeArrayType(Type *elemType,
                                                         std::string sizeName) {
  auto &type{mInferredSizeArrayTypes[std::pair(elemType, sizeName)]};
  if (!type)
    type = allocator.allocate<InferredSizeArrayType>(elemType,
                                                     std::move(sizeName));
  return type.get();
}

PointerType *Context::getPointerType(Type *pointeeType) {
  auto &type{mPointerTypes[pointeeType]};
  if (!type)
    type = allocator.allocate<PointerType>(*this, pointeeType);
  return type.get();
}

EnumType *Context::getEnumType(AST::Enum *decl) {
  auto &type{mASTTypes[decl]};
  if (!type)
    type = allocator.allocate<EnumType>(*decl);
  return static_cast<EnumType *>(type.get());
}

FunctionType *Context::getFunctionType(AST::Function *decl) {
  auto &type{mASTTypes[decl]};
  if (!type)
    type = allocator.allocate<FunctionType>(*decl);
  return static_cast<FunctionType *>(type.get());
}

StructType *Context::getStructType(AST::Struct *decl) {
  auto &type{mASTTypes[decl]};
  if (!type)
    type = allocator.allocate<StructType>(*decl);
  return static_cast<StructType *>(type.get());
}

TagType *Context::getTagType(AST::Tag *decl) {
  auto &type{mASTTypes[decl]};
  if (!type)
    type = allocator.allocate<TagType>(std::string(decl->name.srcName));
  return static_cast<TagType *>(type.get());
}

Type *Context::getUnionType(llvm::ArrayRef<Type *> types) {
  auto caseTypes{UnionType::canonicalizeTypes(types)};
  if (caseTypes.size() == 1)
    return caseTypes[0];
  auto &type{mUnionTypes[caseTypes]};
  if (!type)
    type = allocator.allocate<UnionType>(*this, std::move(caseTypes));
  return type.get();
}

ComptimeUnionType *Context::getComptimeUnionType(UnionType *unionType) {
  auto &type{mComptimeUnionTypes[unionType]};
  if (!type)
    type = allocator.allocate<ComptimeUnionType>(unionType);
  return type.get();
}

Type *Context::getCommonType(llvm::ArrayRef<Type *> types, bool defaultToUnion,
                             const SourceLocation &srcLoc) {
  if (types.empty())
    return getVoidType();
  if (types.size() == 1)
    return types[0];
  auto getCommonTypeOfPair{[&](Type *typeA, Type *typeB) -> Type * {
    if (typeA == getAutoType() || !typeA)
      return typeB;
    if (typeB == getAutoType() || !typeB || typeA == typeB)
      return typeA;
    if (typeA->isArithmetic() && typeB->isArithmetic()) {
      auto arithTypeA{static_cast<ArithmeticType *>(typeA)};
      auto arithTypeB{static_cast<ArithmeticType *>(typeB)};
      if (arithTypeA->extent == arithTypeB->extent ||
          arithTypeA->extent.isScalar() || arithTypeB->extent.isScalar())
        return arithTypeA->getCommonType(*this, arithTypeB);
    }
    if ((typeA->isArithmeticScalar() && typeB->isColor()) ||
        (typeB->isArithmeticScalar() && typeA->isColor()))
      return getColorType();
    if (!defaultToUnion || typeA->isAbstract() || typeB->isAbstract())
      srcLoc.throwError("no common type between ", Quoted(typeA->displayName),
                        " and ", Quoted(typeB->displayName));
    return getUnionType({typeA, typeB});
  }};
  Type *commonType{types[0]};
  for (uint32_t i = 1; i < types.size(); i++)
    commonType = getCommonTypeOfPair(commonType, types[i]);
  return commonType;
}

ConversionRule Context::getConversionRule(Type *typeA, Type *typeB) {
  // If the source type is equivalent to the destination type OR the destination
  // type is pure `auto`, conversion is perfect!
  if (typeA == typeB || typeB == getAutoType()) {
    return CONVERSION_RULE_PERFECT;
  }
  if (typeA->isAbstract())
    return CONVERSION_RULE_EXPLICIT;
  if (typeA->isVoid())
    return CONVERSION_RULE_IMPLICIT;
  // If the source and destination types are both arithmetic ...
  if (typeA->isArithmetic() && typeB->isArithmetic()) {
    // If the source and destination extents are equivalent, conversion is
    // implicit.
    if (static_cast<ArithmeticType *>(typeA)->extent ==
        static_cast<ArithmeticType *>(typeB)->extent)
      return CONVERSION_RULE_IMPLICIT;
    // If the source type is a scalar and the destination type is a vector or
    // matrix, conversion is explicit.
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // auto vecOf1s = float4(1.0);
    // auto matOf1s = float4x4(1.0); // 1s on diagonal
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (typeA->isArithmeticScalar() &&
        (typeB->isArithmeticVector() || typeB->isArithmeticMatrix()))
      return CONVERSION_RULE_EXPLICIT;
  }
  // If the source and destination types are different enum and/or int-like
  // types, conversion is explicit.
  if ((typeA->isEnum() && typeB->isEnum()) ||
      (typeA->isEnum() && typeB->isArithmeticScalar() &&
       typeB->isArithmeticIntegral()) ||
      (typeB->isEnum() && typeA->isArithmeticScalar() &&
       typeA->isArithmeticIntegral())) {
    return CONVERSION_RULE_EXPLICIT;
  }
  // If the source type is a pointer ...
  if (typeA->isPointer()) {
    // If the destination type is also a pointer, conversion is explicit. NOTE:
    // At this point, we know the source type is not equivalent to the
    // destination type, so the pointer types are distinct.
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // auto ptrToInt = cast<&int>(ptr);
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (typeB->isPointer()) {
      // ... unless the destination type is `&void`
      if (typeB == getVoidPointerType())
        return CONVERSION_RULE_IMPLICIT;
      // ... unless the destination type is a pointer to `auto`, in which case
      // conversion is perfect as long as the underlying pointee type conversion
      // is perfect.
      if (typeB->isAbstract())
        return getConversionRule(typeA->getPointeeType(),
                                 typeB->getPointeeType());
      return CONVERSION_RULE_EXPLICIT;
    }
    // If the destination type is a boolean, conversion is implicit. This is a
    // non-NULL test.
    // ~~~~~~~~~~~~~~~~~~~~~
    // bool isNonNull = ptr;
    // ~~~~~~~~~~~~~~~~~~~~~
    if (typeB == getBoolType()) {
      return CONVERSION_RULE_IMPLICIT;
    }
    // If the destination type is a vector with equivalent scalar type,
    // conversion is explicit (Load from the pointer!).
    if (auto arithTypeB{llvm::dyn_cast<ArithmeticType>(typeB)};
        arithTypeB->extent.isVector() &&
        arithTypeB->getScalarType(*this) == typeA->getPointeeType()) {
      return CONVERSION_RULE_EXPLICIT;
    }
  }
  // If the source type is a struct that is an instance of the
  // destination type, conversion is perfect.
  if (auto structTypeA{llvm::dyn_cast<StructType>(typeA)};
      structTypeA && structTypeA->isInstanceOf(typeB)) {
    return CONVERSION_RULE_PERFECT;
  }
  // If the source type is a union that is always an instance of the
  // destination type, conversion is perfect.
  if (auto unionTypeA{llvm::dyn_cast<UnionType>(typeA)};
      unionTypeA && unionTypeA->isAlwaysInstanceOf(typeB)) {
    return CONVERSION_RULE_PERFECT;
  }
  // If the destination type is a union ...
  if (auto unionTypeB{llvm::dyn_cast<UnionType>(typeB)}) {
    // If the source type is a union and the destination type has all
    // of the source case types, conversion is implicit.
    if (auto unionTypeA{llvm::dyn_cast<UnionType>(typeA)}) {
      return unionTypeB->hasAllCaseTypes(unionTypeA) ? CONVERSION_RULE_IMPLICIT
                                                     : CONVERSION_RULE_EXPLICIT;
    } else {
      // If the source type is not a union and the destination type has the
      // source as a case type, conversion is implicit.
      if (unionTypeB->hasCaseType(typeA))
        return CONVERSION_RULE_IMPLICIT;
    }
  }
  // If the destination type is a compile-time union and the source type
  // is one of the case types, conversion is perfect.
  if (auto unionTypeB{llvm::dyn_cast<ComptimeUnionType>(typeB)};
      unionTypeB && unionTypeB->unionType->hasCaseType(typeA)) {
    return CONVERSION_RULE_PERFECT;
  }
  // If the destination type is a color ...
  if (typeB->isColor()) {
    // If the source type is float or float3, conversion is implicit.
    if (typeA == getFloatType() || typeA == getFloatType(Extent(3)))
      return CONVERSION_RULE_IMPLICIT;
    // If the source type is a pointer to a float, conversion is explicit.
    // (Loads from the pointer)
    if (typeA == getPointerType(getFloatType()))
      return CONVERSION_RULE_EXPLICIT;
  }
  // If the source type is an array ...
  if (auto arrayTypeA{llvm::dyn_cast<ArrayType>(typeA)}) {
    // If the destination type is an inferred-size array, conversion is whatever
    // the element conversion is.
    if (auto inferredSizeArrayTypeB{
            llvm::dyn_cast<InferredSizeArrayType>(typeB)}) {
      return getConversionRule(arrayTypeA->elemType,
                               inferredSizeArrayTypeB->elemType);
    }
    // If the destination type is a pointer with the same element type,
    // conversion is implicit.
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // auto someArray = float[4](/* ... */);
    // &float somePtr = someArray;
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (auto pointerTypeB{llvm::dyn_cast<PointerType>(typeB)};
        pointerTypeB && arrayTypeA->elemType == pointerTypeB->pointeeType) {
      return CONVERSION_RULE_IMPLICIT;
    }
    // If the destination type is an array with the same size, conversion is
    // explicit. NOTE: At this point, we know the source type is not equivalent
    // to the destination type, so we know the element types are different.
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // auto someArray = float[4](/* ... */);
    // auto someArray2 = cast<double[4]>(someArray);
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (auto arrayTypeB{llvm::dyn_cast<ArrayType>(typeB)};
        arrayTypeB && arrayTypeA->size == arrayTypeB->size &&
        getConversionRule(arrayTypeA->elemType, arrayTypeB->elemType) !=
            CONVERSION_RULE_NOT_ALLOWED) {
      return CONVERSION_RULE_EXPLICIT;
    }
  }
  return CONVERSION_RULE_NOT_ALLOWED;
}

Value Context::getComptimeUnionIndexMap(UnionType *unionTypeA,
                                        UnionType *unionTypeB) {
  auto &indexMap{mUnionIndexMaps[std::pair(unionTypeA, unionTypeB)]};
  if (!indexMap) {
    indexMap =
        Value::zero(getArrayType(getIntType(), unionTypeA->caseTypes.size()));
    auto builder{llvm::IRBuilder<>(llvmContext)};
    for (unsigned i = 0; i < unionTypeA->caseTypes.size(); i++)
      indexMap.llvmValue =
          builder.CreateInsertValue(indexMap.llvmValue,
                                    getComptimeInt(unionTypeB->getCaseTypeIndex(
                                        unionTypeA->caseTypes[i])),
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
