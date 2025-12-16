#pragma once

#include "Type.h"

namespace smdl {

/// \addtogroup Compiler
/// \{

/// The conversion rule for converting one type into another type.
enum ConversionRule : uint32_t {
  CONVERSION_RULE_NOT_ALLOWED, ///< Conversion is not allowed.
  CONVERSION_RULE_EXPLICIT, ///< Conversion is allowed but should be explicit.
  CONVERSION_RULE_IMPLICIT, ///< Conversion is allowed and may be implicit.
  CONVERSION_RULE_PERFECT,  ///< Conversion is perfect!
};

/// The compiler context.
class Context final {
public:
  explicit Context(Compiler &compiler);

  Context(const Context &) = delete;

  /// Implicit conversion to the held `LLVMContext` for convenience.
  [[nodiscard]] operator llvm::LLVMContext &() { return llvmContext; }

public:
  /// Get builtin module by name.
  Module *getBuiltinModule(llvm::StringRef name);

  /// Get builtin albedo by name.
  [[nodiscard]] const AlbedoLUT *getBuiltinAlbedo(llvm::StringRef name);

  /// Get builtin callee.
  [[nodiscard]]
  llvm::FunctionCallee getBuiltinCallee(llvm::StringRef name,
                                        llvm::FunctionType *llvmFuncTy) {
    auto &callee{mBuiltinCallees[name]};
    if (!callee) {
      auto llvmFunc{llvm::Function::Create(
          llvmFuncTy, llvm::Function::ExternalLinkage, name, llvmModule)};
      llvmFunc->setDSOLocal(false);
      callee = llvm::FunctionCallee(llvmFuncTy, llvmFunc);
    }
    return callee;
  }

  /// Get builtin callee.
  template <typename T, typename... U>
  [[nodiscard]] llvm::FunctionCallee getBuiltinCallee(const char *name) {
    return getBuiltinCallee(name, llvm_get_type<T(U...)>(llvmContext));
  }

  template <typename T, typename... U>
  [[nodiscard]] llvm::FunctionCallee getBuiltinCallee(const char *name,
                                                      T (*value)(U...)) {
    return getBuiltinCallee<T, U...>(name);
  }

  /// Get a unique name by appending a number to it, useful for
  /// generating more readable names for LLVM basic blocks.
  [[nodiscard]] std::string getUniqueName(llvm::StringRef name,
                                          llvm::Function *llvmFunc = {}) {
    return name.str() + std::to_string(mUniqueNames[name][llvmFunc]++);
  }

  /// Get keyword value. Return `Value()` if undefined.
  [[nodiscard]] Value getKeyword(llvm::StringRef name) {
    if (auto itr{mKeywords.find(name)}; itr != mKeywords.end())
      return itr->second;
    return Value();
  }

  /// Get keyword value as `Type`.
  [[nodiscard]] Type *getKeywordAsType(llvm::StringRef name,
                                       const SourceLocation &srcLoc = {}) {
    return getKeyword(name).getComptimeMetaType(*this, srcLoc);
  }

  /// Has keyword value?
  [[nodiscard]] bool hasKeywordValue(llvm::StringRef name) {
    return mKeywords.contains(name);
  }

public:
  /// Get the alignment of the given type in bytes. Return 0 if undefined.
  [[nodiscard]] uint64_t getAlignOf(Type *type) {
    return type && type->llvmType && type != mVoidType.get()
               ? uint64_t(llvmLayout.getABITypeAlign(type->llvmType).value())
               : uint64_t(0);
  }

  /// Get the size of the given type in bytes. Return 0 if undefined.
  [[nodiscard]] uint64_t getSizeOf(Type *type) {
    return type && type->llvmType && type != mVoidType.get()
               ? uint64_t(llvmLayout.getTypeAllocSize(type->llvmType))
               : uint64_t(0);
  }

public:
  /// Get arithmetic type.
  [[nodiscard]] Type *getArithmeticType(Scalar scalar,
                                        Extent extent = Extent(1));

  /// Get arithmetic `bool` type.
  [[nodiscard]] Type *getBoolType(Extent extent = Extent(1)) {
    return getArithmeticType(Scalar::getInt(1), extent);
  }

  /// Get arithmetic `int` type.
  [[nodiscard]] Type *getIntType(Extent extent = Extent(1)) {
    return getArithmeticType(Scalar::getInt(sizeof(int) * 8), extent);
  }

  /// Get arithmetic `float` type.
  [[nodiscard]] Type *getFloatType(Extent extent = Extent(1)) {
    return getArithmeticType(Scalar::getFloat(), extent);
  }

  /// Get arithmetic `double` type.
  [[nodiscard]] Type *getDoubleType(Extent extent = Extent(1)) {
    return getArithmeticType(Scalar::getDouble(), extent);
  }

  /// Get `auto` type.
  [[nodiscard]] Type *getAutoType() { return mAutoType.get(); }

  /// Get `color` type.
  [[nodiscard]] ColorType *getColorType() { return mColorType.get(); }

  /// Get `string` type.
  [[nodiscard]] Type *getStringType() { return mStringType.get(); }

  /// Get `void` type.
  [[nodiscard]] Type *getVoidType() { return mVoidType.get(); }

  /// Get `&void` type.
  [[nodiscard]] Type *getVoidPointerType() {
    return getPointerType(mVoidType.get());
  }

  /// Get meta `Module` type.
  [[nodiscard]] Type *getMetaModuleType() { return mMetaModuleType.get(); }

  /// Get meta `Type` type.
  [[nodiscard]] Type *getMetaTypeType() { return mMetaTypeType.get(); }

  /// Get meta `AST::Intrinsic` type.
  [[nodiscard]] Type *getMetaIntrinsicType() {
    return mMetaIntrinsicType.get();
  }

  /// Get meta `AST::Namespace` type.
  [[nodiscard]] Type *getMetaNamespaceType() {
    return mMetaNamespaceType.get();
  }

  /// Get array type.
  [[nodiscard]] ArrayType *getArrayType(Type *elemType, uint32_t size);

  /// Get inferred-size array type.
  [[nodiscard]] InferredSizeArrayType *
  getInferredSizeArrayType(Type *elemType, std::string sizeName = {});

  /// Get pointer type.
  [[nodiscard]] PointerType *getPointerType(Type *pointeeType);

  /// Get pointer type with the given depth, where depth is the number of
  /// pointers to add.
  [[nodiscard]] Type *getPointerType(Type *pointeeType, size_t depth) {
    while (depth-- > 0)
      pointeeType = getPointerType(pointeeType);
    return pointeeType;
  }

  /// Get enum type.
  [[nodiscard]] EnumType *getEnumType(AST::Enum *decl);

  /// Get function type.
  [[nodiscard]] FunctionType *getFunctionType(AST::Function *decl);

  /// Get state type.
  [[nodiscard]] StateType *getStateType() { return mStateType.get(); }

  /// Get struct type.
  [[nodiscard]] StructType *getStructType(AST::Struct *decl);

  /// Get tag type.
  [[nodiscard]] TagType *getTagType(AST::Tag *decl);

  /// Get the `texture_2d` type.
  [[nodiscard]] StructType *getTexture2DType() { return mTexture2DType; }

  /// Get the `texture_3d` type.
  [[nodiscard]] StructType *getTexture3DType() { return mTexture3DType; }

  /// Get the `texture_cube` type.
  [[nodiscard]] StructType *getTextureCubeType() { return mTextureCubeType; }

  /// Get the `texture_ptex` type.
  [[nodiscard]] StructType *getTexturePtexType() { return mTexturePtexType; }

  /// Get the `bsdf_measurement` type.
  [[nodiscard]] StructType *getBSDFMeasurementType() {
    return mBSDFMeasurementType;
  }

  /// Get the `light_profile` type.
  [[nodiscard]] StructType *getLightProfileType() { return mLightProfileType; }

  /// Get the `spectral_curve` type.
  [[nodiscard]] StructType *getSpectralCurveType() {
    return mSpectralCurveType;
  }

  /// Get the `complex` type.
  [[nodiscard]] StructType *getComplexType() { return mComplexType; }

  /// Get union type.
  [[nodiscard]] Type *getUnionType(llvm::ArrayRef<Type *> types);

  /// Get compile-time union type.
  [[nodiscard]] ComptimeUnionType *getComptimeUnionType(UnionType *unionType);

public:
  template <typename T> [[nodiscard]] Type *getType();

  template <typename S, typename T> [[nodiscard]] Type *getType(T S::*) {
    return getType<T>();
  }

public:
  /// Get the common type of the given types.
  [[nodiscard]] Type *getCommonType(llvm::ArrayRef<Type *> types,
                                    bool defaultToUnion = true,
                                    const SourceLocation &srcLoc = {});

  /// Get the conversion rule for converting `typeA` to `typeB`.
  [[nodiscard]] ConversionRule getConversionRule(Type *typeA, Type *typeB);

  /// Is `typeA` explicitly convertible to `typeB`?
  [[nodiscard]] bool isExplicitlyConvertible(Type *typeA, Type *typeB) {
    return getConversionRule(typeA, typeB) >= CONVERSION_RULE_EXPLICIT;
  }

  /// Is `typeA` implicitly convertible to `typeB`?
  [[nodiscard]] bool isImplicitlyConvertible(Type *typeA, Type *typeB) {
    return getConversionRule(typeA, typeB) >= CONVERSION_RULE_IMPLICIT;
  }

  /// Is `typeA` perfectly convertible to `typeB`?
  [[nodiscard]] bool isPerfectlyConvertible(Type *typeA, Type *typeB) {
    return getConversionRule(typeA, typeB) == CONVERSION_RULE_PERFECT;
  }

public:
  /// Get compile-time `bool` constant.
  [[nodiscard]] Value getComptimeBool(bool value) {
    return RValue(getBoolType(),
                  llvm::ConstantInt::getBool(getBoolType()->llvmType, value));
  }

  /// Get compile-time `int` constant.
  [[nodiscard]] Value getComptimeInt(int value) {
    return RValue(getIntType(),
                  llvm::ConstantInt::get(getIntType()->llvmType,
                                         llvm::APInt(sizeof(int) * 8, value)));
  }

  /// Get compile-time `float` constant.
  [[nodiscard]] Value getComptimeFloat(float value) {
    return RValue(
        getFloatType(),
        llvm::ConstantFP::get(getFloatType()->llvmType, llvm::APFloat(value)));
  }

  /// Get compile-time `double` constant.
  [[nodiscard]] Value getComptimeDouble(double value) {
    return RValue(
        getDoubleType(),
        llvm::ConstantFP::get(getDoubleType()->llvmType, llvm::APFloat(value)));
  }

  /// Get compile-time scalar constant.
  template <typename T> [[nodiscard]] Value getComptimeScalar(T value) {
    static_assert(std::is_arithmetic_v<T>);
    auto type{getArithmeticType(Scalar::get<T>())};
    if constexpr (std::is_integral_v<T>) {
      return RValue(type,
                    llvm::ConstantInt::get(type->llvmType,
                                           llvm::APInt(sizeof(T) * 8, value)));
    } else {
      return RValue(
          type, llvm::ConstantFP::get(type->llvmType, llvm::APFloat(value)));
    }
  }

  /// Get compile-time vector constant.
  template <typename T, size_t M>
  [[nodiscard]] Value getComptimeVector(Vector<T, M> value) {
    static_assert(std::is_arithmetic_v<T> && M > 1);
    auto builder{llvm::IRBuilder<>{llvmContext}};
    auto result{Value::zero(getArithmeticType(Scalar::get<T>(), Extent(M)))};
    for (size_t i = 0; i < M; i++)
      result.llvmValue = builder.CreateInsertElement(
          result.llvmValue, getComptimeScalar(value[i]).llvmValue, uint64_t(i));
    return result;
  }

  /// Get compile-time `string` constant.
  [[nodiscard]] Value getComptimeString(std::string_view value) {
    auto &result{mStrings[value]};
    if (!result) {
      auto builder{llvm::IRBuilder<>(llvmContext)};
      auto str{builder.CreateGlobalString(value, "str", 0, &llvmModule)};
      auto ptr{
          builder.CreateConstInBoundsGEP2_32(str->getValueType(), str, 0, 0)};
      return RValue(mStringType.get(), ptr);
    }
    return result;
  }

  /// Get compile-time meta `Module` constant.
  [[nodiscard]] Value getComptimeMetaModule(Module *value) {
    return RValue(getMetaModuleType(),
                  llvmPtrAsConstantInt(llvmContext, value));
  }

  /// Get compile-time meta `Type` constant.
  [[nodiscard]] Value getComptimeMetaType(Type *value) {
    return RValue(getMetaTypeType(), llvmPtrAsConstantInt(llvmContext, value));
  }

  /// Get compile-time meta `AST::Intrinsic` constant.
  [[nodiscard]] Value getComptimeMetaIntrinsic(AST::Intrinsic *value) {
    return RValue(getMetaIntrinsicType(),
                  llvmPtrAsConstantInt(llvmContext, value));
  }

  /// Get compile-time meta `AST::Namespace` constant.
  [[nodiscard]] Value getComptimeMetaNamespace(AST::Namespace *value) {
    return RValue(getMetaNamespaceType(),
                  llvmPtrAsConstantInt(llvmContext, value));
  }

  /// Get compile-time union index map. This is useful for converting
  /// `unionTypeA` to `unionTypeB`.
  [[nodiscard]] Value getComptimeUnionIndexMap(UnionType *unionTypeA,
                                               UnionType *unionTypeB);

  [[nodiscard]] Value getComptimePtr(Type *type, const void *value) {
    SMDL_SANITY_CHECK(type && type->llvmType);
    return RValue(type,
                  llvm::IRBuilder<>(llvmContext)
                      .CreateIntToPtr(llvmPtrAsConstantInt(llvmContext, value),
                                      type->llvmType));
  }

  [[nodiscard]] std::optional<std::string> locate(const std::string &fileName) {
    return compiler.fileLocator.locate(fileName, currentModule->getFileName());
  }

  [[nodiscard]] std::vector<FileLocator::ImagePath>
  locateImages(const std::string &fileName) {
    return compiler.fileLocator.locateImages(fileName,
                                             currentModule->getFileName());
  }

public:
  /// The compiler.
  Compiler &compiler;

  /// The bump allocator.
  BumpPtrAllocator &allocator{compiler.mAllocator};

  /// The LLVM context.
  llvm::LLVMContext &llvmContext{compiler.getLLVMContext()};

  /// The LLVM module.
  llvm::Module &llvmModule{compiler.getLLVMModule()};

  /// The LLVM data layout.
  const llvm::DataLayout &llvmLayout{llvmModule.getDataLayout()};

  /// The LLVM target library info implementation.
  llvm::TargetLibraryInfoImpl llvmTargetLibraryInfoImpl{
      llvm::Triple(llvm::StringRef(NativeTarget::get().triple))};

  /// The LLVM target library info.
  llvm::TargetLibraryInfo llvmTargetLibraryInfo{llvmTargetLibraryInfoImpl};

  /// The LLVM type used to represent an incomplete return type during
  /// function compilation.
  ///
  /// \note
  /// This makes it easy to detect invalid recursion.
  ///
  llvm::Type *const llvmIncompleteReturnTy{
      llvm::StructType::create(llvmContext, {}, "$incomplete")};

  Module *currentModule{};

private:
  /// The builtin modules. See `get_builtin_module()`
  llvm::StringMap<BumpPtr<Module>> mBuiltinModules;

  /// The LLVM callees for builtin foreign functions. See `get_builtin_callee()`
  llvm::StringMap<llvm::FunctionCallee> mBuiltinCallees;

  /// The unique name counters. See `get_unique_name()`
  llvm::StringMap<llvm::DenseMap<llvm::Function *, uint64_t>> mUniqueNames;

  /// The arithmetic types, e.g., `bool3`, `int`, `float4`, `double2x3`.
  llvm::DenseMap<uint64_t, BumpPtr<ArithmeticType>> mArithmeticTypes;

  /// The array types.
  std::map<std::pair<Type *, uint32_t>, BumpPtr<ArrayType>> mArrayTypes;

  /// The inferred-size array types.
  std::map<std::pair<Type *, std::string>, BumpPtr<InferredSizeArrayType>>
      mInferredSizeArrayTypes;

  /// The pointer types.
  llvm::DenseMap<Type *, BumpPtr<PointerType>> mPointerTypes;

  /// The union types.
  ///
  /// Each `UnionType` is keyed by its canonical case types. See
  /// `UnionType::canonicalize_types()`.
  std::map<llvm::SmallVector<Type *>, BumpPtr<UnionType>> mUnionTypes;

  /// The union index maps for remapping the union index when
  /// converting different union types.
  std::map<std::pair<UnionType *, UnionType *>, Value> mUnionIndexMaps;

  /// The `auto` type.
  const BumpPtr<AutoType> mAutoType{allocator.allocate<AutoType>()};

  /// The meta `Module` type.
  const BumpPtr<MetaType> mMetaModuleType{
      allocator.allocate<MetaType>(*this, "module")};

  /// The meta `Type` type.
  const BumpPtr<MetaType> mMetaTypeType{
      allocator.allocate<MetaType>(*this, "type")};

  /// The meta `AST::Intrinsic` type.
  const BumpPtr<MetaType> mMetaIntrinsicType{
      allocator.allocate<MetaType>(*this, "intrinsic")};

  /// The meta `AST::Namespace` type.
  const BumpPtr<MetaType> mMetaNamespaceType{
      allocator.allocate<MetaType>(*this, "namespace")};

  /// The `void` type.
  const BumpPtr<VoidType> mVoidType{allocator.allocate<VoidType>(*this)};

  /// The `string` type.
  const BumpPtr<StringType> mStringType{allocator.allocate<StringType>(*this)};

  /// The `color` type.
  const BumpPtr<ColorType> mColorType{allocator.allocate<ColorType>(*this)};

  /// The `State` type.
  const BumpPtr<StateType> mStateType{allocator.allocate<StateType>(*this)};

  /// The AST associated types.
  llvm::DenseMap<AST::Decl *, BumpPtr<Type>> mASTTypes;

  /// The compile-time union types.
  llvm::DenseMap<UnionType *, BumpPtr<ComptimeUnionType>> mComptimeUnionTypes;

  /// The keyword values.
  llvm::StringMap<Value> mKeywords;

  /// The string constants.
  ///
  /// This is a string map such that identical string literals in the source
  /// code, such as a file name that may appear many times, end up referencing
  /// the same global string constant.
  llvm::StringMap<Value> mStrings;

  /// The `material` type defined by the builtin `API` module.
  StructType *mMaterialType{};

  /// The `texture_2d` type defined by the builtin `API` module.
  StructType *mTexture2DType{};

  /// The `texture_3d` type defined by the builtin `API` module.
  StructType *mTexture3DType{};

  /// The `texture_cube` type defined by the builtin `API` module.
  StructType *mTextureCubeType{};

  /// The `texture_ptex` type defined by the builtin `API` module.
  StructType *mTexturePtexType{};

  /// The `bsdf_measurement` type defined by the builtin `API` module.
  StructType *mBSDFMeasurementType{};

  /// The `light_profile` type defined by the builtin `API` module.
  StructType *mLightProfileType{};

  /// The `spectral_curve` type defined by the builtin `API` module.
  StructType *mSpectralCurveType{};

  /// The `complex` type defined by the builtin `API` module.
  StructType *mComplexType{};

  friend class FunctionType;
};

template <typename T, typename = void> struct get_type;

template <> struct get_type<void, void> {
  [[nodiscard]] static Type *get(Context &context) {
    return context.getVoidType();
  }
};

template <typename T>
struct get_type<
    T, std::enable_if_t<std::is_arithmetic_v<T> || std::is_enum_v<T>, void>> {
  [[nodiscard]] static Type *get(Context &context) {
    if constexpr (std::is_integral_v<T> || std::is_enum_v<T>)
      return context.getArithmeticType(Scalar::getInt(sizeof(T) * 8));
    else
      return context.getArithmeticType(Scalar::getFP(sizeof(T) * 8));
  }
};

template <typename T> struct get_type<T &, void> {
  [[nodiscard]] static Type *get(Context &context) {
    return context.getPointerType(get_type<std::decay_t<T>>::get(context));
  }
};

template <typename T> struct get_type<T *, void> {
  [[nodiscard]] static Type *get(Context &context) {
    return context.getPointerType(get_type<std::decay_t<T>>::get(context));
  }
};

template <typename T, size_t M> struct get_type<T[M], void> {
  [[nodiscard]] static Type *get(Context &context) {
    return context.getArrayType(get_type<T>::get(context), M);
  }
};

template <typename T, size_t M> struct get_type<Vector<T, M>, void> {
  [[nodiscard]] static Type *get(Context &context) {
    return static_cast<ArithmeticType *>(get_type<T>::get(context))
        ->getWithDifferentExtent(context, Extent(M));
  }
};

template <typename T, size_t N, size_t M>
struct get_type<Matrix<T, N, M>, void> {
  [[nodiscard]] static Type *get(Context &context) {
    return static_cast<ArithmeticType *>(get_type<T>::get(context))
        ->getWithDifferentExtent(context, Extent(N, M));
  }
};

template <typename T> inline Type *Context::getType() {
  return smdl::get_type<T>::get(*this);
}

/// \}

} // namespace smdl
