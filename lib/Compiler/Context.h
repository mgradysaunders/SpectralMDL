/// \file
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

/// An albedo look-up table (LUT) for energy compensation in lossy BSDFs.
class AlbedoLUT final {
public:
  /// The number of samples of the cosine of the viewing angle.
  const int numCosTheta = 0;

  /// The number of samples of the roughness parameter.
  const int numRoughness = 0;

  /// The directional albedo.
  ///
  /// \note
  /// This must point to `numCosTheta` rows by `numRoughness` values.
  ///
  const float *const directionalAlbedo = nullptr;

  /// The average albedo.
  ///
  /// \note
  /// This must point to `numRoughness` values.
  ///
  const float *const averageAlbedo = nullptr;
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
    llvm::FunctionCallee &callee{builtinCallees_[name]};
    if (!callee) {
      llvm::Function *llvmFunc{llvm::Function::Create(
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

  /// Get builtin callee, and record its host address so the compiler can
  /// define it as an absolute symbol in the JIT (see
  /// `Compiler::builtinCalleeAddresses_`).
  template <typename T, typename... U>
  [[nodiscard]] llvm::FunctionCallee getBuiltinCallee(const char *name,
                                                      T (*value)(U...)) {
    compiler.builtinCalleeAddresses_[name] =
        reinterpret_cast<const void *>(value);
    return getBuiltinCallee<T, U...>(name);
  }

  /// Get a unique name by appending a number to it, useful for
  /// generating more readable names for LLVM basic blocks.
  [[nodiscard]] std::string getUniqueName(llvm::StringRef name,
                                          llvm::Function *llvmFunc = {}) {
    return name.str() + std::to_string(uniqueNames_[name][llvmFunc]++);
  }

  /// Intern the given name. The returned span and the characters it views
  /// are owned by the context, so the result is safe to store in a
  /// `Declaration` regardless of where the argument came from, and the
  /// same name always interns to the same span: full-name equality is
  /// `data()`/`size()` equality.
  [[nodiscard]] Span<const std::string_view>
  internName(Span<const std::string_view> name);

  /// Make a new `Scope`, owned by the context. Scopes must outlive the
  /// emitter that pushes them: resolution anchors captured in
  /// `ParameterList::lastScope` are used when other modules lazily emit
  /// macro expansions, function instances, and default initializers.
  [[nodiscard]] Scope *makeScope() {
    scopes_.push_back(std::make_unique<Scope>());
    return scopes_.back().get();
  }

  /// Get the next globally monotonic declaration sequence number. See
  /// `Declaration::seq`.
  [[nodiscard]] uint64_t nextDeclSeq() { return ++declSeq_; }

  /// Get the current (most recently issued) declaration sequence number.
  [[nodiscard]] uint64_t currentDeclSeq() const { return declSeq_; }

  /// Get keyword value. Return `Value()` if undefined.
  [[nodiscard]] Value getKeyword(llvm::StringRef name) {
    if (llvm::StringMapIterBase<Value, false> itr{keywords_.find(name)};
        itr != keywords_.end())
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
    return keywords_.contains(name);
  }

  /// Get every keyword name, for did-you-mean suggestions. The views are
  /// borrowed from the keyword table and live as long as the `Context`.
  [[nodiscard]] std::vector<std::string_view> getKeywordNames() const {
    std::vector<std::string_view> names{};
    names.reserve(keywords_.size());
    for (const auto &entry : keywords_)
      names.push_back(std::string_view(entry.getKey()));
    return names;
  }

public:
  /// Get the alignment of the given type in bytes. Return 0 if undefined.
  [[nodiscard]] uint64_t getAlignOf(Type *type) {
    return type && type->llvmType && type != voidType_.get()
               ? uint64_t(llvmLayout.getABITypeAlign(type->llvmType).value())
               : uint64_t(0);
  }

  /// Get the size of the given type in bytes. Return 0 if undefined.
  [[nodiscard]] uint64_t getSizeOf(Type *type) {
    return type && type->llvmType && type != voidType_.get()
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
  [[nodiscard]] Type *getAutoType() { return autoType_.get(); }

  /// Get `color` type.
  [[nodiscard]] ColorType *getColorType() { return colorType_.get(); }

  /// Get `string` type.
  [[nodiscard]] Type *getStringType() { return stringType_.get(); }

  /// Get `void` type.
  [[nodiscard]] Type *getVoidType() { return voidType_.get(); }

  /// Get `&void` type.
  [[nodiscard]] Type *getVoidPointerType() {
    return getPointerType(voidType_.get());
  }

  /// Get meta `Module` type.
  [[nodiscard]] Type *getMetaModuleType() { return metaModuleType_.get(); }

  /// Get meta `Type` type.
  [[nodiscard]] Type *getMetaTypeType() { return metaTypeType_.get(); }

  /// Get meta `ast::Intrinsic` type.
  [[nodiscard]] Type *getMetaIntrinsicType() {
    return metaIntrinsicType_.get();
  }

  /// Get meta `ast::Namespace` type.
  [[nodiscard]] Type *getMetaNamespaceType() {
    return metaNamespaceType_.get();
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
    while (depth-- > 0) pointeeType = getPointerType(pointeeType);
    return pointeeType;
  }

  /// Get enum type.
  [[nodiscard]] EnumType *getEnumType(ast::Enum *decl);

  /// Get function type.
  [[nodiscard]] FunctionType *getFunctionType(ast::Function *decl);

  /// Get a fresh lambda function type. Unlike `getFunctionType()`, this is
  /// deliberately NOT interned by declaration: a lambda inside a macro body
  /// is re-emitted at every expansion, and each evaluation must re-capture
  /// its resolution anchor. See `Emitter::emit(ast::Lambda &)`.
  [[nodiscard]] FunctionType *getLambdaFunctionType(ast::Function *decl);

  /// Get state type.
  [[nodiscard]] StateType *getStateType() { return stateType_.get(); }

  /// Get struct type.
  [[nodiscard]] StructType *getStructType(ast::Struct *decl);

  /// Get tag type.
  [[nodiscard]] TagType *getTagType(ast::Tag *decl);

  /// Get the `texture_2d` type.
  [[nodiscard]] StructType *getTexture2DType() { return texture2DType_; }

  /// Get the `texture_3d` type.
  [[nodiscard]] StructType *getTexture3DType() { return texture3DType_; }

  /// Get the `texture_cube` type.
  [[nodiscard]] StructType *getTextureCubeType() { return textureCubeType_; }

  /// Get the `texture_ptex` type.
  [[nodiscard]] StructType *getTexturePtexType() { return texturePtexType_; }

  /// Get the `bsdf_measurement` type.
  [[nodiscard]] StructType *getBSDFMeasurementType() {
    return bsdfMeasurementType_;
  }

  /// Get the `light_profile` type.
  [[nodiscard]] StructType *getLightProfileType() { return lightProfileType_; }

  /// Get the `host_resource` type.
  [[nodiscard]] StructType *getHostResourceType() { return hostResourceType_; }

  /// Get the `spectral_curve` type.
  [[nodiscard]] StructType *getSpectralCurveType() {
    return spectralCurveType_;
  }

  /// Get the `complex` type.
  [[nodiscard]] StructType *getComplexType() { return complexType_; }

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
                                    bool shouldDefaultToUnion = true,
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
  ///
  /// \note
  /// The `isSigned` flag matters: without it, a negative `value`
  /// widened to `uint64_t` no longer fits in 32 bits, and the `APInt`
  /// constructor (whose fits-check is an assert, compiled out in
  /// release LLVM) skips `clearUnusedBits()` and produces an
  /// unnormalized constant that prints correctly but compares unequal
  /// to the real constant during compile-time folding.
  [[nodiscard]] Value getComptimeInt(int value) {
    return RValue(getIntType(),
                  llvm::ConstantInt::get(getIntType()->llvmType,
                                         llvm::APInt(sizeof(int) * 8, value,
                                                     /*isSigned=*/true)));
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
    Type *type{getArithmeticType(Scalar::get<T>())};
    if constexpr (std::is_integral_v<T>) {
      // See the signedness note on 'getComptimeInt()'.
      return RValue(type, llvm::ConstantInt::get(
                              type->llvmType,
                              llvm::APInt(sizeof(T) * 8, value,
                                          /*isSigned=*/std::is_signed_v<T>)));
    } else {
      return RValue(
          type, llvm::ConstantFP::get(type->llvmType, llvm::APFloat(value)));
    }
  }

  /// Get compile-time vector constant.
  template <typename T, size_t M>
  [[nodiscard]] Value getComptimeVector(Vector<T, M> value) {
    static_assert(std::is_arithmetic_v<T> && M > 1);
    llvm::IRBuilder<> builder{llvmContext};
    Value result{Value::zero(getArithmeticType(Scalar::get<T>(), Extent(M)))};
    for (size_t i = 0; i < M; i++)
      result.llvmValue = builder.CreateInsertElement(
          result.llvmValue, getComptimeScalar(value[i]).llvmValue, uint64_t(i));
    return result;
  }

  /// Get compile-time matrix constant, of `N` columns of `M` rows.
  template <typename T, size_t N, size_t M>
  [[nodiscard]] Value getComptimeMatrix(const Matrix<T, N, M> &value) {
    static_assert(std::is_arithmetic_v<T> && N > 1 && M > 1);
    llvm::IRBuilder<> builder{llvmContext};
    Value result{
        Value::zero(getArithmeticType(Scalar::get<T>(), Extent(N, M)))};
    for (size_t j = 0; j < N; j++)
      result.llvmValue = builder.CreateInsertValue(
          result.llvmValue, getComptimeVector(value[j]).llvmValue,
          {unsigned(j)});
    return result;
  }

  /// Get compile-time `string` constant.
  [[nodiscard]] Value getComptimeString(std::string_view value) {
    Value &result{strings_[value]};
    if (!result) {
      llvm::IRBuilder<> builder{llvmContext};
      llvm::GlobalVariable *str{
          builder.CreateGlobalString(value, "str", 0, &llvmModule)};
      llvm::Value *ptr{
          builder.CreateConstInBoundsGEP2_32(str->getValueType(), str, 0, 0)};
      result = RValue(stringType_.get(), ptr);
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

  /// Get compile-time meta `ast::Intrinsic` constant.
  [[nodiscard]] Value getComptimeMetaIntrinsic(ast::Intrinsic *value) {
    return RValue(getMetaIntrinsicType(),
                  llvmPtrAsConstantInt(llvmContext, value));
  }

  /// Get compile-time meta `ast::Namespace` constant.
  [[nodiscard]] Value getComptimeMetaNamespace(ast::Namespace *value) {
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

  /// Get the external symbol naming the level 0 texels of `image`, which
  /// `Compiler::jitCompile()` defines once every image has been loaded.
  ///
  /// Unlike every other resource, an image is named rather than baked as
  /// an address, because its size is not settled until emission ends: a
  /// texture constructed later may ask for a mip chain that an earlier
  /// one did not. Naming it also makes `Compiler::dropUnusedImages_()`
  /// exact, since a symbol has a use list and an address does not.
  [[nodiscard]] Value getImageTexelBase(Type *type, const Image &image);

  /// Get a pointer to a private constant array of `int` holding
  /// `values`, materialized in the module rather than on the host so
  /// that nothing outside has to outlive the JIT'd code, and so that
  /// the optimizer can fold an index it happens to know.
  [[nodiscard]] Value getComptimeIntArray(Span<const int> values,
                                          llvm::StringRef name);

  [[nodiscard]] std::optional<std::string> locate(const std::string &fileName) {
    return compiler.fileLocator.locate(
        fileName, currentModule->getResourceAnchor(),
        FileLocator::REGULAR_FILES, currentModule->getSearchDirs());
  }

  /// Same as `locate()`, but also accepts directories.
  [[nodiscard]] std::optional<std::string>
  locateDirOrFile(const std::string &fileName) {
    return compiler.fileLocator.locate(
        fileName, currentModule->getResourceAnchor(),
        FileLocator::REGULAR_FILES | FileLocator::DIRS,
        currentModule->getSearchDirs());
  }

  [[nodiscard]] std::vector<FileLocator::ImagePath>
  locateImages(const std::string &fileName) {
    return compiler.fileLocator.locateImages(fileName,
                                             currentModule->getResourceAnchor(),
                                             currentModule->getSearchDirs());
  }

  /// The directories `locate()` and `locateImages()` search, in order.
  [[nodiscard]] std::vector<std::string> getSearchDirs() {
    return compiler.fileLocator.getSearchDirs(
        currentModule->getResourceAnchor(), currentModule->getSearchDirs());
  }

public:
  /// The compiler.
  Compiler &compiler;

  /// The bump allocator.
  BumpPtrAllocator &allocator{compiler.allocator_};

  /// The LLVM context.
  llvm::LLVMContext &llvmContext{compiler.getLLVMContext_()};

  /// The LLVM module.
  llvm::Module &llvmModule{compiler.getLLVMModule_()};

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

  /// The enclosing `namespace` names at the current point of declaration
  /// emission, maintained by `Emitter::emit(ast::Namespace &)` and reset
  /// per module by `Module::compile()`. Used to form qualified material
  /// names.
  llvm::SmallVector<std::string_view, 4> currentNamespacePath{};

  /// The modules being compiled, outermost first, each one imported by the
  /// one before it. Maintained by `Module::compile()`, and read by
  /// `Emitter::resolveModule()` to report a cyclic import.
  llvm::SmallVector<const Module *, 8> modulesInProgress{};

  /// The tags the open functions of each name are declared on, so that a
  /// function named like one can be checked against them.
  llvm::StringMap<llvm::SmallVector<TagType *, 1>> openFunctionTags{};

private:
  /// The builtin modules. See `get_builtin_module()`
  llvm::StringMap<BumpPtr<Module>> builtinModules_;

  /// The LLVM callees for builtin foreign functions. See `get_builtin_callee()`
  llvm::StringMap<llvm::FunctionCallee> builtinCallees_;

  /// The unique name counters. See `get_unique_name()`
  llvm::StringMap<llvm::DenseMap<llvm::Function *, uint64_t>> uniqueNames_;

  /// The interned names. Keyed by the name components joined with `'\0'`
  /// (identifiers cannot contain `'\0'`, so the encoding is unambiguous);
  /// the interned views point into the map entry's own key bytes. See
  /// `internName()`.
  llvm::StringMap<Span<const std::string_view>> internedNames_;

  /// The scopes. See `makeScope()`.
  std::vector<std::unique_ptr<Scope>> scopes_;

  /// The declaration sequence counter. See `nextDeclSeq()`.
  uint64_t declSeq_{};

  /// The arithmetic types, e.g., `bool3`, `int`, `float4`, `double2x3`.
  llvm::DenseMap<uint64_t, BumpPtr<ArithmeticType>> arithmeticTypes_;

  /// The array types.
  std::map<std::pair<Type *, uint32_t>, BumpPtr<ArrayType>> arrayTypes_;

  /// The inferred-size array types.
  std::map<std::pair<Type *, std::string>, BumpPtr<InferredSizeArrayType>>
      inferredSizeArrayTypes_;

  /// The pointer types.
  llvm::DenseMap<Type *, BumpPtr<PointerType>> pointerTypes_;

  /// The union types.
  ///
  /// Each `UnionType` is keyed by its canonical case types. See
  /// `UnionType::canonicalizeTypes()`.
  std::map<llvm::SmallVector<Type *>, BumpPtr<UnionType>> unionTypes_;

  /// The union index maps for remapping the union index when
  /// converting different union types.
  std::map<std::pair<UnionType *, UnionType *>, Value> unionIndexMaps_;

  /// The `auto` type.
  const BumpPtr<AutoType> autoType_{allocator.allocate<AutoType>()};

  /// The meta `Module` type.
  const BumpPtr<MetaType> metaModuleType_{
      allocator.allocate<MetaType>(*this, "module")};

  /// The meta `Type` type.
  const BumpPtr<MetaType> metaTypeType_{
      allocator.allocate<MetaType>(*this, "type")};

  /// The meta `ast::Intrinsic` type.
  const BumpPtr<MetaType> metaIntrinsicType_{
      allocator.allocate<MetaType>(*this, "intrinsic")};

  /// The meta `ast::Namespace` type.
  const BumpPtr<MetaType> metaNamespaceType_{
      allocator.allocate<MetaType>(*this, "namespace")};

  /// The `void` type.
  const BumpPtr<VoidType> voidType_{allocator.allocate<VoidType>(*this)};

  /// The `string` type.
  const BumpPtr<StringType> stringType_{allocator.allocate<StringType>(*this)};

  /// The `color` type.
  const BumpPtr<ColorType> colorType_{allocator.allocate<ColorType>(*this)};

  /// The `State` type.
  const BumpPtr<StateType> stateType_{allocator.allocate<StateType>(*this)};

  /// The AST associated types.
  llvm::DenseMap<ast::Decl *, BumpPtr<Type>> astTypes_;

  /// The lambda function types, one per evaluation of each lambda
  /// expression. This is a flat keep-alive list rather than a map keyed on
  /// the declaration, see `getLambdaFunctionType()`.
  std::vector<BumpPtr<Type>> lambdaTypes_;

  /// The compile-time union types.
  llvm::DenseMap<UnionType *, BumpPtr<ComptimeUnionType>> comptimeUnionTypes_;

  /// The keyword values.
  llvm::StringMap<Value> keywords_;

  /// The string constants.
  ///
  /// This is a string map such that identical string literals in the source
  /// code, such as a file name that may appear many times, end up referencing
  /// the same global string constant.
  llvm::StringMap<Value> strings_;

  /// The `material` type defined by the builtin `API` module.
  StructType *materialType_{};

  /// The `texture_2d` type defined by the builtin `API` module.
  StructType *texture2DType_{};

  /// The `texture_3d` type defined by the builtin `API` module.
  StructType *texture3DType_{};

  /// The `texture_cube` type defined by the builtin `API` module.
  StructType *textureCubeType_{};

  /// The `texture_ptex` type defined by the builtin `API` module.
  StructType *texturePtexType_{};

  /// The `bsdf_measurement` type defined by the builtin `API` module.
  StructType *bsdfMeasurementType_{};

  /// The `light_profile` type defined by the builtin `API` module.
  StructType *lightProfileType_{};

  /// The `host_resource` type defined by the builtin `API` module.
  StructType *hostResourceType_{};

  /// The `spectral_curve` type defined by the builtin `API` module.
  StructType *spectralCurveType_{};

  /// The `complex` type defined by the builtin `API` module.
  StructType *complexType_{};

  friend class FunctionType;
};

template <typename T, typename = void> struct GetType;

template <> struct GetType<void, void> {
  [[nodiscard]] static Type *get(Context &context) {
    return context.getVoidType();
  }
};

template <typename T>
struct GetType<
    T, std::enable_if_t<std::is_arithmetic_v<T> || std::is_enum_v<T>, void>> {
  [[nodiscard]] static Type *get(Context &context) {
    if constexpr (std::is_integral_v<T> || std::is_enum_v<T>)
      return context.getArithmeticType(Scalar::getInt(sizeof(T) * 8));
    else
      return context.getArithmeticType(Scalar::getFP(sizeof(T) * 8));
  }
};

template <typename T> struct GetType<T &, void> {
  [[nodiscard]] static Type *get(Context &context) {
    return context.getPointerType(GetType<std::decay_t<T>>::get(context));
  }
};

template <typename T> struct GetType<T *, void> {
  [[nodiscard]] static Type *get(Context &context) {
    return context.getPointerType(GetType<std::decay_t<T>>::get(context));
  }
};

template <typename T, size_t M> struct GetType<T[M], void> {
  [[nodiscard]] static Type *get(Context &context) {
    return context.getArrayType(GetType<T>::get(context), M);
  }
};

template <typename T, size_t M> struct GetType<Vector<T, M>, void> {
  [[nodiscard]] static Type *get(Context &context) {
    return static_cast<ArithmeticType *>(GetType<T>::get(context))
        ->getWithDifferentExtent(context, Extent(M));
  }
};

template <typename T, size_t N, size_t M>
struct GetType<Matrix<T, N, M>, void> {
  [[nodiscard]] static Type *get(Context &context) {
    return static_cast<ArithmeticType *>(GetType<T>::get(context))
        ->getWithDifferentExtent(context, Extent(N, M));
  }
};

template <typename T> inline Type *Context::getType() {
  return smdl::GetType<T>::get(*this);
}

/// \}

} // namespace smdl
