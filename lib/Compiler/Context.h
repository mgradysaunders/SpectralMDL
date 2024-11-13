// vim:foldmethod=marker:foldlevel=0:fmr=--{,--}
#pragma once

#include "Module.h"

namespace smdl::Compiler {

class Context final {
public:
  explicit Context(MDLInstance &mdl, llvm::BumpPtrAllocator &bumpAllocator);

  Context(const Context &) = delete;

  Context(Context &&) = delete;

public:
  [[nodiscard]] void *bump_allocate(size_t size, size_t align) { return bumpAllocator.Allocate(size, align); }

  template <typename T> [[nodiscard]] T *bump_allocate(auto &&...args) {
    return new (bump_allocate(sizeof(T), alignof(T))) T(std::forward<decltype(args)>(args)...);
  }

  [[nodiscard]] llvm::StringRef get_persistent_string(const llvm::Twine &twine);

  [[nodiscard]] llvm::StringRef get_persistent_string(const AST::Name &astName) { return astName.name; }

  [[nodiscard]] std::string get_unique_name(llvm::StringRef name, llvm::Function *llvmFunc = nullptr) {
    return name.str() + std::to_string(uniqueNames[name][llvmFunc]++);
  }

  [[nodiscard]] uint64_t get_align_of(Type *type) {
    return type && type->llvmType && !type->is_void() ? uint64_t(llvmLayout.getABITypeAlign(type->llvmType).value()) : 0;
  }

  [[nodiscard]] uint64_t get_size_of(Type *type) {
    return type && type->llvmType && !type->is_void() ? uint64_t(llvmLayout.getTypeAllocSize(type->llvmType)) : 0;
  }

  [[nodiscard]] bool is_keyword(llvm::StringRef name) {
    return keywordToType.contains(name) || keywordToConstant.contains(name);
  }

  void validate_decl_name(const char *kind, const AST::Name &name) {
    if (is_keyword(name.name))
      name.srcLoc.report_error(std::format("{} name must not be reserved keyword '{}'", kind, name.name));
  }

  [[nodiscard]] llvm::StringRef bump_duplicate(llvm::StringRef str) {
    if (str.empty())
      return {};
    auto ptr{static_cast<char *>(bump_allocate(str.size(), 1))};
    std::copy(str.begin(), str.end(), ptr);
    return llvm::StringRef(ptr, str.size());
  }

  template <typename T> [[nodiscard]] llvm::ArrayRef<T> bump_duplicate(llvm::ArrayRef<T> values) {
    if (values.empty())
      return {};
    auto ptr{static_cast<T *>(bump_allocate(sizeof(T) * values.size(), alignof(T)))};
    std::copy(values.begin(), values.end(), ptr);
    return {ptr, values.size()};
  }

  [[nodiscard]] AST::Expr *parse_expression(llvm::StringRef src);

public:
  //--{ Type getters
  [[nodiscard]] AutoType *get_auto_type() { return autoType.get(); }

  [[nodiscard]] ArithmeticType *get_arithmetic_type(Scalar scalar, Extent extent = Extent(1));

  [[nodiscard]] ArithmeticType *get_bool_type(Extent extent = Extent(1)) { return get_arithmetic_type(Scalar::Bool, extent); }

  [[nodiscard]] ArithmeticType *get_int_type(Extent extent = Extent(1)) { return get_arithmetic_type(Scalar::Int, extent); }

  [[nodiscard]] ArithmeticType *get_float_type(Extent extent = Extent(1)) { return get_arithmetic_type(Scalar::Float, extent); }

  [[nodiscard]] ArithmeticType *get_double_type(Extent extent = Extent(1)) {
    return get_arithmetic_type(Scalar::Double, extent);
  }

  [[nodiscard]] ArrayType *get_array_type(Type *elemType, uint32_t size) {
    return get_or_initialize_type<ArrayType>(arrayTypes, elemType, size);
  }

  [[nodiscard]] ArrayType *get_array_type(Type *elemType, llvm::StringRef sizeName) {
    return get_or_initialize_type<ArrayType>(sizeDeferredArrayTypes, elemType, llvm::SmallString<16>(sizeName));
  }

  [[nodiscard]] ColorType *get_color_type() { return colorType.get(); }

  [[nodiscard]] CompilerType *get_compiler_function_type() { return compilerFunctionType.get(); }

  [[nodiscard]] CompilerType *get_compiler_intrinsic_type() { return compilerIntrinsicType.get(); }

  [[nodiscard]] CompilerType *get_compiler_module_type() { return compilerModuleType.get(); }

  [[nodiscard]] CompilerType *get_compiler_type_type() { return compilerTypeType.get(); }

  [[nodiscard]] EnumType *get_enum_type(AST::Enum *decl, llvm::Function *llvmFunc) {
    return get_or_initialize_type<EnumType>(enumTypes, decl, llvmFunc);
  }

  [[nodiscard]] FunctionType *get_function_type(bool isPure, Type *returnType, llvm::ArrayRef<Type *> paramTypes) {
    return get_or_initialize_type<FunctionType>(
        functionTypes, isPure, returnType, llvm::SmallVector<Type *>(paramTypes.begin(), paramTypes.end()));
  }

  [[nodiscard]] PointerType *get_pointer_type(Type *elemType) {
    return get_or_initialize_type<PointerType>(pointerTypes, elemType);
  }

  [[nodiscard]] StructType *get_string_type() { return stringType.get(); }

  [[nodiscard]] StructType *get_struct_type(AST::Struct *decl, llvm::Function *llvmFunc) {
    return get_or_initialize_type<StructType>(structTypes, decl, llvmFunc);
  }

  [[nodiscard]] StructType *get_struct_type(StructType *structType, llvm::ArrayRef<Type *> missingTypes) {
    if (missingTypes.empty())
      return structType;
    return get_or_initialize_type<StructType>(
        structTypeInstantiations, structType, llvm::SmallVector<Type *>(missingTypes.begin(), missingTypes.end()));
  }

  [[nodiscard]] TagType *get_tag_type(AST::Tag *decl, llvm::Function *llvmFunc) {
    return get_or_initialize_type<TagType>(tagTypes, decl, llvmFunc);
  }

  [[nodiscard]] Type *get_union_type(llvm::ArrayRef<Type *> types) {
    auto canonicalTypes{UnionType::canonicalize_types(types)};
    if (canonicalTypes.size() == 1)
      return canonicalTypes[0];
    return get_or_initialize_type<UnionType>(unionTypes, canonicalTypes);
  }

  [[nodiscard]] VoidType *get_void_type() { return voidType.get(); }

  [[nodiscard]] EnumType *get_intensity_mode_type() { return intensityModeType.get(); }

  [[nodiscard]] TagType *get_bsdf_type() { return bsdfType.get(); }

  [[nodiscard]] TagType *get_edf_type() { return edfType.get(); }

  [[nodiscard]] TagType *get_vdf_type() { return vdfType.get(); }

  [[nodiscard]] TagType *get_hair_bsdf_type() { return hairBsdfType.get(); }

  [[nodiscard]] StructType *get_default_bsdf_type() { return defaultBsdfType.get(); }

  [[nodiscard]] StructType *get_default_edf_type() { return defaultEdfType.get(); }

  [[nodiscard]] StructType *get_default_vdf_type() { return defaultVdfType.get(); }

  [[nodiscard]] StructType *get_default_hair_bsdf_type() { return defaultHairBsdfType.get(); }

  [[nodiscard]] StructType *get_material_emission_type() { return materialEmissionType.get(); }

  [[nodiscard]] StructType *get_material_surface_type() { return materialSurfaceType.get(); }

  [[nodiscard]] StructType *get_material_volume_type() { return materialVolumeType.get(); }

  [[nodiscard]] StructType *get_material_geometry_type() { return materialGeometryType.get(); }

  [[nodiscard]] StructType *get_material_type() { return materialType.get(); }
  //--}

public:
  //--{ Type getter template
  template <typename T> [[nodiscard]] Type *get_type(std::in_place_type_t<T>) requires arithmetic_type<T> {
    return get_arithmetic_type(arithmetic_type_traits<T>::scalar, arithmetic_type_traits<T>::extent);
  }

  template <typename T> [[nodiscard]] Type *get_type(std::in_place_type_t<T &>) {
    return get_pointer_type(get_type(std::in_place_type<std::decay_t<T>>));
  }

  template <typename T> [[nodiscard]] Type *get_type(std::in_place_type_t<T *>) {
    return get_pointer_type(get_type(std::in_place_type<std::decay_t<T>>));
  }

  template <typename T, size_t N> [[nodiscard]] Type *get_type(std::in_place_type_t<T[N]>) {
    return get_array_type(get_type(std::in_place_type<T>), N);
  }

  [[nodiscard]] Type *get_type(std::in_place_type_t<string_t>) { return stringType.get(); }

  [[nodiscard]] Type *get_type(std::in_place_type_t<source_location_t>) { return sourceLocationType.get(); }

  [[nodiscard]] Type *get_type(std::in_place_type_t<intensity_mode_t>) { return intensityModeType.get(); }

  [[nodiscard]] Type *get_type(std::in_place_type_t<material_emission_t>) { return materialEmissionType.get(); }

  [[nodiscard]] Type *get_type(std::in_place_type_t<material_surface_t>) { return materialSurfaceType.get(); }

  [[nodiscard]] Type *get_type(std::in_place_type_t<material_volume_t>) { return materialVolumeType.get(); }

  [[nodiscard]] Type *get_type(std::in_place_type_t<material_geometry_t>) { return materialGeometryType.get(); }

  [[nodiscard]] Type *get_type(std::in_place_type_t<material_t>) { return materialType.get(); }

  [[nodiscard]] Type *get_type(std::in_place_type_t<image_t>) { return imageType.get(); }

  [[nodiscard]] Type *get_type(std::in_place_type_t<texture_2d_t>) { return texture2DType.get(); }

  [[nodiscard]] Type *get_type(std::in_place_type_t<texture_3d_t>) { return texture3DType.get(); }

  [[nodiscard]] Type *get_type(std::in_place_type_t<texture_cube_t>) { return textureCubeType.get(); }

  [[nodiscard]] Type *get_type(std::in_place_type_t<texture_ptex_t>) { return texturePtexType.get(); }

  [[nodiscard]] Type *get_type(std::in_place_type_t<state_t>) { return stateType.get(); }

  [[nodiscard]] Type *get_type(std::in_place_type_t<void>) { return voidType.get(); }

  template <typename T> [[nodiscard]] Type *get_type() { return get_type(std::in_place_type<T>); }
  //--}

public:
  //--{ Type relations
  [[nodiscard]] ConversionRule get_conversion_rule(Type *srcType, Type *dstType);

  [[nodiscard]] bool is_explicitly_convertible(Type *srcType, Type *dstType) {
    return get_conversion_rule(srcType, dstType) >= ConversionRule::Explicit;
  }

  [[nodiscard]] bool is_implicitly_convertible(Type *srcType, Type *dstType) {
    return get_conversion_rule(srcType, dstType) >= ConversionRule::Implicit;
  }

  [[nodiscard]] bool is_perfectly_convertible(Type *srcType, Type *dstType) {
    return get_conversion_rule(srcType, dstType) == ConversionRule::Perfect;
  }

  [[nodiscard]] bool is_subset_of(Type *lhsType, Type *rhsType);

  [[nodiscard]] Type *get_common_type_of_pair(
      Type *lhsType, Type *rhsType, bool defaultToUnion = true, const AST::SourceLocation &srcLoc = {});

  [[nodiscard]] Type *get_common_type(
      llvm::ArrayRef<Type *> types, bool defaultToUnion = true, const AST::SourceLocation &srcLoc = {});

  [[nodiscard]] Type *get_common_type(Type *lhsType, Type *rhsType, auto... types) {
    return get_common_type({lhsType, rhsType, types...});
  }
  //--}

public:
  //--{ Compile-time constants
  [[nodiscard]] Value get_compile_time_bool(bool value);

  [[nodiscard]] Value get_compile_time_int(const llvm::APInt &value);

  [[nodiscard]] Value get_compile_time_int(int_t value) { return get_compile_time_int(llvm::APInt(sizeof(int_t) * 8, value)); }

  [[nodiscard]] Value get_compile_time_int2(int2_t value) {
    auto result{Value::zero(get_type<int2_t>())};
    auto builder{llvm::IRBuilder<>(llvmContext)};
    result.llvmValue = builder.CreateInsertValue(result, get_compile_time_int(value.x), {0U});
    result.llvmValue = builder.CreateInsertValue(result, get_compile_time_int(value.y), {1U});
    return result;
  }

  [[nodiscard]] Value get_compile_time_FP(llvm::APFloat value, AST::Precision precision);

  [[nodiscard]] Value get_compile_time_FP(double value, AST::Precision precision) {
    return get_compile_time_FP(llvm::APFloat(value), precision);
  }

  [[nodiscard]] Value get_compile_time_float(double value) { return get_compile_time_FP(value, AST::Precision::Single); }

  [[nodiscard]] Value get_compile_time_double(double value) { return get_compile_time_FP(value, AST::Precision::Double); }

  [[nodiscard]] Value get_compile_time_string(llvm::StringRef value);

  [[nodiscard]] Value get_compile_time_source_location(const AST::SourceLocation &value);

  [[nodiscard]] Value get_compile_time_function(Function *value) {
    return RValue(get_compiler_function_type(), llvm_ptr_as_constant_int(llvmContext, value));
  }

  [[nodiscard]] Value get_compile_time_intrinsic(AST::Intrinsic *value) {
    return RValue(get_compiler_intrinsic_type(), llvm_ptr_as_constant_int(llvmContext, value));
  }

  [[nodiscard]] Value get_compile_time_module(Module *value) {
    return RValue(get_compiler_module_type(), llvm_ptr_as_constant_int(llvmContext, value));
  }

  [[nodiscard]] Value get_compile_time_type(Type *value) {
    return RValue(get_compiler_type_type(), llvm_ptr_as_constant_int(llvmContext, value));
  }

  [[nodiscard]] Value get_compile_time_pointer(const void *value);

  template <typename T, typename... U> [[nodiscard]] llvm::FunctionCallee get_compile_time_callee(T (*value)(U...)) {
    return llvm::FunctionCallee(
        llvm::FunctionType::get(get_type<T>()->llvmType, {get_type<U>()->llvmType...}, /*isVarArg=*/false),
        get_compile_time_pointer(reinterpret_cast<const void *>(value)));
  }

  [[nodiscard]] Value get_compile_time_union_index_map(UnionType *srcType, UnionType *dstType);
  //--}

public:
  [[nodiscard]] Function *get_function(Emitter &emitter, AST::Function *decl);

  [[nodiscard]] Module *get_builtin_module(llvm::StringRef name);

public:
  [[nodiscard]] Value resolve(Emitter &emitter, const AST::Identifier &identifier);

  [[nodiscard]] llvm::SmallVector<Value> resolve_arguments(
      Emitter &emitter0, const ParamList &params, const ArgList &args, const AST::SourceLocation &srcLoc,
      llvm::SmallVector<Type *> *argParamTypes = nullptr, bool dontEmit = false);

  [[nodiscard]] bool can_resolve_arguments(
      Emitter &emitter0, const ParamList &params, const ArgList &args, const AST::SourceLocation &srcLoc,
      llvm::SmallVector<Type *> *argParamTypes = nullptr) try {
    auto argValues{resolve_arguments(emitter0, params, args, srcLoc, argParamTypes, /*dontEmit=*/true)};
    return true;
  } catch (...) {
    return false;
  }

  [[nodiscard]] Module *resolve_module(
      Emitter &emitter, bool isAbs, llvm::ArrayRef<llvm::StringRef> path, const AST::SourceLocation &srcLoc);

public:
  MDLInstance &mdl;

  llvm::LLVMContext &llvmContext;

  llvm::Module &llvmModule;

  const llvm::DataLayout &llvmLayout;

  llvm::TargetLibraryInfoImpl llvmTargetLibraryInfoImpl;

  llvm::TargetLibraryInfo llvmTargetLibraryInfo;

  llvm::BumpPtrAllocator &bumpAllocator;

private:
  llvm::StringMap<llvm::DenseMap<llvm::Function *, uint64_t>> uniqueNames{};

  llvm::StringMap<unique_bump_ptr<Module>> builtinModules{};

  llvm::StringMap<unique_bump_ptr<AST::Expr>> builtinExpressions{};

  llvm::DenseMap<AST::Function *, unique_bump_ptr<Function>> functions{};

  llvm::DenseMap<uint64_t, unique_bump_ptr<ArithmeticType>> arithmeticTypes{};

  std::map<std::tuple<Type *, uint32_t>, unique_bump_ptr<ArrayType>> arrayTypes{};

  std::map<std::tuple<Type *, llvm::SmallString<16>>, unique_bump_ptr<ArrayType>> sizeDeferredArrayTypes{};

  llvm::DenseMap<std::tuple<AST::Enum *, llvm::Function *>, unique_bump_ptr<EnumType>> enumTypes{};

  std::map<std::tuple<bool, Type *, llvm::SmallVector<Type *>>, unique_bump_ptr<FunctionType>> functionTypes{};

  llvm::DenseMap<Type *, unique_bump_ptr<PointerType>> pointerTypes{};

  llvm::DenseMap<std::tuple<AST::Struct *, llvm::Function *>, unique_bump_ptr<StructType>> structTypes{};

  std::map<std::tuple<StructType *, llvm::SmallVector<Type *>>, unique_bump_ptr<StructType>> structTypeInstantiations{};

  llvm::DenseMap<std::tuple<AST::Tag *, llvm::Function *>, unique_bump_ptr<TagType>> tagTypes{};

  std::map<llvm::SmallVector<Type *>, unique_bump_ptr<UnionType>> unionTypes{};

  /// The builtin 'compiler_function' type.
  const unique_bump_ptr<CompilerType> compilerFunctionType;

  /// The builtin 'compiler_intrinsic' type.
  const unique_bump_ptr<CompilerType> compilerIntrinsicType;

  /// The builtin 'compiler_module' type.
  const unique_bump_ptr<CompilerType> compilerModuleType;

  /// The builtin 'compiler_type' type.
  const unique_bump_ptr<CompilerType> compilerTypeType;

  /// The builtin 'auto' type.
  const unique_bump_ptr<AutoType> autoType;

  /// The builtin 'void' type.
  const unique_bump_ptr<VoidType> voidType;

  /// The builtin 'color' type.
  const unique_bump_ptr<ColorType> colorType;

  /// The builtin 'intensity_mode' type.
  const unique_bump_ptr<EnumType> intensityModeType;

  /// The builtin 'image_t' type.
  const unique_bump_ptr<StructType> imageType;

  /// The builtin 'texture_2d' type.
  const unique_bump_ptr<StructType> texture2DType;

  /// The builtin 'texture_3d' type.
  const unique_bump_ptr<StructType> texture3DType;

  /// The builtin 'texture_cube' type.
  const unique_bump_ptr<StructType> textureCubeType;

  /// The builtin 'texture_ptex' type.
  const unique_bump_ptr<StructType> texturePtexType;

  /// The builtin 'state_t' type.
  const unique_bump_ptr<StructType> stateType;

  /// The builtin 'string' type.
  const unique_bump_ptr<StructType> stringType;

  /// The builtin 'source_location' type.
  const unique_bump_ptr<StructType> sourceLocationType;

  /// The builtin 'bsdf' abstract tag type.
  const unique_bump_ptr<TagType> bsdfType;

  /// The builtin 'edf' abstract tag type.
  const unique_bump_ptr<TagType> edfType;

  /// The builtin 'vdf' abstract tag type.
  const unique_bump_ptr<TagType> vdfType;

  /// The builtin 'hair_bsdf' abstract tag type.
  const unique_bump_ptr<TagType> hairBsdfType;

  /// The builtin 'default_bsdf' type. (The result of 'bsdf()')
  const unique_bump_ptr<StructType> defaultBsdfType{};

  /// The builtin 'default_edf' type. (The result of 'edf()')
  const unique_bump_ptr<StructType> defaultEdfType{};

  /// The builtin 'default_vdf' type. (The result of 'vdf()')
  const unique_bump_ptr<StructType> defaultVdfType{};

  /// The builtin 'default_hair_bsdf' type. (The result of 'hair_bsdf()')
  const unique_bump_ptr<StructType> defaultHairBsdfType{};

  /// The builtin 'material_emission' abstract struct type.
  const unique_bump_ptr<StructType> materialEmissionType;

  /// The builtin 'material_surface' abstract struct type.
  const unique_bump_ptr<StructType> materialSurfaceType;

  /// The builtin 'material_volume' abstract struct type.
  const unique_bump_ptr<StructType> materialVolumeType;

  /// The builtin 'material_geometry' abstract struct type.
  const unique_bump_ptr<StructType> materialGeometryType;

  /// The builtin 'material' abstract struct type.
  const unique_bump_ptr<StructType> materialType;

  llvm::StringMap<Type *> keywordToType{};

  llvm::StringMap<Value> keywordToConstant{};

  llvm::StringMap<Value> compileTimeStrings{};

  std::map<std::pair<UnionType *, UnionType *>, Value> compileTimeUnionIndexMaps{};

private:
  template <typename T> T *get_or_initialize_type(auto &types, auto key) {
    auto &type{types[key]};
    if (!type)
      type.reset(bump_allocate<T>(*this, key));
    return type.get();
  }

  template <typename T> T *get_or_initialize_type(auto &types, auto... keys) {
    auto &type{types[std::tuple(keys...)]};
    if (!type)
      type.reset(bump_allocate<T>(*this, keys...));
    return type.get();
  }

public:
  static void builtin_print_string(const string_t &what);

  static void builtin_print_bool(int_t what);

  static void builtin_print_int(int_t what);

  static void builtin_print_float(float_t what);

  static void builtin_print_double(double_t what);

  static void builtin_panic(const string_t &what, const source_location_t &srcLoc);
};

} // namespace smdl::Compiler
