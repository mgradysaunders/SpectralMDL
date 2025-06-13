#pragma once

#include "Type.h"

namespace smdl {

/// \addtogroup Compiler
/// \{

/// The conversion rule for converting one type into another type.
enum class ConversionRule : uint32_t {
  NotAllowed, ///< Conversion is not allowed.
  Explicit,   ///< Conversion is allowed but should be explicit.
  Implicit,   ///< Conversion is allowed and may be implicit.
  Perfect,    ///< Conversion is perfect!
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
  Module *get_builtin_module(llvm::StringRef name);

  /// Get builtin albedo LUT by name.
  [[nodiscard]] const AlbedoLUT *get_builtin_albedo_lut(llvm::StringRef name);

  /// Get builtin callee.
  [[nodiscard]]
  llvm::FunctionCallee get_builtin_callee(llvm::StringRef name,
                                          llvm::FunctionType *llvmFuncTy) {
    auto &callee{builtinCallees[name]};
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
  [[nodiscard]] llvm::FunctionCallee get_builtin_callee(const char *name) {
    return get_builtin_callee(name, llvm_get_type<T(U...)>(llvmContext));
  }

  template <typename T, typename... U>
  [[nodiscard]] llvm::FunctionCallee get_builtin_callee(const char *name,
                                                        T (*value)(U...)) {
    return get_builtin_callee<T, U...>(name);
  }

  /// Get a unique name by appending a number to it, useful for
  /// generating more readable names for LLVM basic blocks.
  [[nodiscard]] std::string get_unique_name(llvm::StringRef name,
                                            llvm::Function *llvmFunc = {}) {
    return name.str() + std::to_string(uniqueNames[name][llvmFunc]++);
  }

  /// Get keyword value. Return `Value()` if undefined.
  [[nodiscard]] Value get_keyword_value(llvm::StringRef name) {
    if (auto itr{keywords.find(name)}; itr != keywords.end())
      return itr->second;
    return Value();
  }

  /// Has keyword value?
  [[nodiscard]] bool has_keyword_value(llvm::StringRef name) {
    return keywords.contains(name);
  }

public:
  /// Get the alignment of the given type in bytes. Return 0 if undefined.
  [[nodiscard]] uint64_t get_align_of(Type *type) {
    return type && type->llvmType && type != voidType.get()
               ? uint64_t(llvmLayout.getABITypeAlign(type->llvmType).value())
               : uint64_t(0);
  }

  /// Get the size of the given type in bytes. Return 0 if undefined.
  [[nodiscard]] uint64_t get_size_of(Type *type) {
    return type && type->llvmType && type != voidType.get()
               ? uint64_t(llvmLayout.getTypeAllocSize(type->llvmType))
               : uint64_t(0);
  }

public:
  /// Get arithmetic type.
  [[nodiscard]] Type *get_arithmetic_type(Scalar scalar,
                                          Extent extent = Extent(1));

  /// Get arithmetic `bool` type.
  [[nodiscard]] Type *get_bool_type(Extent extent = Extent(1)) {
    return get_arithmetic_type(Scalar::get_int(1), extent);
  }

  /// Get arithmetic `int` type.
  [[nodiscard]] Type *get_int_type(Extent extent = Extent(1)) {
    return get_arithmetic_type(Scalar::get_int(sizeof(int) * 8), extent);
  }

  /// Get arithmetic `float` type.
  [[nodiscard]] Type *get_float_type(Extent extent = Extent(1)) {
    return get_arithmetic_type(Scalar::get_float(), extent);
  }

  /// Get arithmetic `double` type.
  [[nodiscard]] Type *get_double_type(Extent extent = Extent(1)) {
    return get_arithmetic_type(Scalar::get_double(), extent);
  }

  /// Get `auto` type.
  [[nodiscard]] Type *get_auto_type() { return autoType.get(); }

  /// Get `color` type.
  [[nodiscard]] ColorType *get_color_type() { return colorType.get(); }

  /// Get `string` type.
  [[nodiscard]] Type *get_string_type() { return stringType.get(); }

  /// Get `void` type.
  [[nodiscard]] Type *get_void_type() { return voidType.get(); }

  /// Get `&void` type.
  [[nodiscard]] Type *get_void_pointer_type() {
    return get_pointer_type(voidType.get());
  }

  /// Get meta `Module` type.
  [[nodiscard]] Type *get_meta_module_type() { return metaModuleType.get(); }

  /// Get meta `Type` type.
  [[nodiscard]] Type *get_meta_type_type() { return metaTypeType.get(); }

  /// Get meta `AST::Intrinsic` type.
  [[nodiscard]] Type *get_meta_intrinsic_type() {
    return metaIntrinsicType.get();
  }

  /// Get meta `AST::Namespace` type.
  [[nodiscard]] Type *get_meta_namespace_type() {
    return metaNamespaceType.get();
  }

  /// Get array type.
  [[nodiscard]] ArrayType *get_array_type(Type *elemType, uint32_t size);

  /// Get inferred-size array type.
  [[nodiscard]] InferredSizeArrayType *
  get_inferred_size_array_type(Type *elemType, std::string sizeName = {});

  /// Get pointer type.
  [[nodiscard]] PointerType *get_pointer_type(Type *pointeeType);

  /// Get pointer type with the given depth, where depth is the number of
  /// pointers to add.
  [[nodiscard]] Type *get_pointer_type(Type *pointeeType, size_t depth) {
    while (depth-- > 0)
      pointeeType = get_pointer_type(pointeeType);
    return pointeeType;
  }

  /// Get enum type.
  [[nodiscard]] EnumType *get_enum_type(AST::Enum *decl);

  /// Get function type.
  [[nodiscard]] FunctionType *get_function_type(AST::Function *decl);

  /// Get state type.
  [[nodiscard]] StateType *get_state_type() { return stateType.get(); }

  /// Get struct type.
  [[nodiscard]] StructType *get_struct_type(AST::Struct *decl);

  /// Get tag type.
  [[nodiscard]] TagType *get_tag_type(AST::Tag *decl);

  /// Get the `texture_2d` type.
  [[nodiscard]] Texture2DType *get_texture_2d_type() {
    return texture2DType.get();
  }

  /// Get the `texture_ptex` type.
  [[nodiscard]] TexturePtexType *get_texture_ptex_type() {
    return texturePtexType.get();
  }

  /// Get union type.
  [[nodiscard]] Type *get_union_type(llvm::ArrayRef<Type *> types);

  /// Get compile-time union type.
  [[nodiscard]] ComptimeUnionType *
  get_comptime_union_type(UnionType *unionType);

public:
  template <typename T> [[nodiscard]] Type *get_type();

  template <typename S, typename T> [[nodiscard]] Type *get_type(T S::*) {
    return get_type<T>();
  }

public:
  /// Get the common type of the given types.
  [[nodiscard]] Type *get_common_type(llvm::ArrayRef<Type *> types,
                                      bool defaultToUnion = true,
                                      const SourceLocation &srcLoc = {});

  /// Get the conversion rule for converting `typeA` to `typeB`.
  [[nodiscard]] ConversionRule get_conversion_rule(Type *typeA, Type *typeB);

  /// Is `typeA` explicitly convertible to `typeB`?
  [[nodiscard]] bool is_explicitly_convertible(Type *typeA, Type *typeB) {
    return get_conversion_rule(typeA, typeB) >= ConversionRule::Explicit;
  }

  /// Is `typeA` implicitly convertible to `typeB`?
  [[nodiscard]] bool is_implicitly_convertible(Type *typeA, Type *typeB) {
    return get_conversion_rule(typeA, typeB) >= ConversionRule::Implicit;
  }

  /// Is `typeA` perfectly convertible to `typeB`?
  [[nodiscard]] bool is_perfectly_convertible(Type *typeA, Type *typeB) {
    return get_conversion_rule(typeA, typeB) == ConversionRule::Perfect;
  }

public:
  /// Get compile-time `bool` constant.
  [[nodiscard]] Value get_comptime_bool(bool value) {
    return RValue(get_bool_type(),
                  llvm::ConstantInt::getBool(get_bool_type()->llvmType, value));
  }

  /// Get compile-time `int` constant.
  [[nodiscard]] Value get_comptime_int(int value) {
    return RValue(get_int_type(),
                  llvm::ConstantInt::get(get_int_type()->llvmType,
                                         llvm::APInt(sizeof(int) * 8, value)));
  }

  /// Get compile-time `float` constant.
  [[nodiscard]] Value get_comptime_float(float value) {
    return RValue(get_float_type(),
                  llvm::ConstantFP::get(get_float_type()->llvmType,
                                        llvm::APFloat(value)));
  }

  /// Get compile-time `double` constant.
  [[nodiscard]] Value get_comptime_double(double value) {
    return RValue(get_double_type(),
                  llvm::ConstantFP::get(get_double_type()->llvmType,
                                        llvm::APFloat(value)));
  }

  /// Get compile-time scalar constant.
  template <typename T> [[nodiscard]] Value get_comptime_scalar(T value) {
    static_assert(std::is_arithmetic_v<T>);
    auto type{get_arithmetic_type(Scalar::get<T>())};
    if constexpr (std::is_integral_v<T>)
      return RValue(type,
                    llvm::ConstantInt::get(type->llvmType,
                                           llvm::APInt(sizeof(T) * 8, value)));
    else
      return RValue(
          type, llvm::ConstantFP::get(type->llvmType, llvm::APFloat(value)));
  }

  /// Get compile-time vector constant.
  template <typename T, size_t M>
  [[nodiscard]] Value get_comptime_vector(Vector<T, M> value) {
    static_assert(std::is_arithmetic_v<T> && M > 1);
    auto builder{llvm::IRBuilder<>{llvmContext}};
    auto result{Value::zero(get_arithmetic_type(Scalar::get<T>(), Extent(M)))};
    for (size_t i = 0; i < M; i++)
      result.llvmValue = builder.CreateInsertElement(
          result.llvmValue, get_comptime_scalar(value[i]).llvmValue,
          uint64_t(i));
    return result;
  }

  /// Get compile-time `string` constant.
  [[nodiscard]] Value get_comptime_string(std::string_view value) {
    auto &result{strings[value]};
    if (!result) {
      auto builder{llvm::IRBuilder<>(llvmContext)};
      auto str{builder.CreateGlobalString(value, "str", 0, &llvmModule)};
      auto ptr{
          builder.CreateConstInBoundsGEP2_32(str->getValueType(), str, 0, 0)};
      return RValue(stringType.get(), ptr);
    }
    return result;
  }

  /// Get compile-time meta `Module` constant.
  [[nodiscard]] Value get_comptime_meta_module(Module *value) {
    return RValue(get_meta_module_type(),
                  llvm_ptr_as_constant_int(llvmContext, value));
  }

  /// Get compile-time meta `Type` constant.
  [[nodiscard]] Value get_comptime_meta_type(Type *value) {
    return RValue(get_meta_type_type(),
                  llvm_ptr_as_constant_int(llvmContext, value));
  }

  /// Get compile-time meta `AST::Intrinsic` constant.
  [[nodiscard]] Value get_comptime_meta_intrinsic(AST::Intrinsic *value) {
    return RValue(get_meta_intrinsic_type(),
                  llvm_ptr_as_constant_int(llvmContext, value));
  }

  /// Get compile-time meta `AST::Namespace` constant.
  [[nodiscard]] Value get_comptime_meta_namespace(AST::Namespace *value) {
    return RValue(get_meta_namespace_type(),
                  llvm_ptr_as_constant_int(llvmContext, value));
  }

  /// Get compile-time union index map. This is useful for converting
  /// `unionTypeA` to `unionTypeB`.
  [[nodiscard]] Value get_comptime_union_index_map(UnionType *unionTypeA,
                                                   UnionType *unionTypeB);

  [[nodiscard]] Value get_comptime_ptr(Type *type, const void *value) {
    SMDL_SANITY_CHECK(type && type->llvmType);
    return RValue(
        type, llvm::IRBuilder<>(llvmContext)
                  .CreateIntToPtr(llvm_ptr_as_constant_int(llvmContext, value),
                                  type->llvmType));
  }

public:
  /// The compiler.
  Compiler &compiler;

  /// The bump allocator.
  BumpPtrAllocator &allocator{compiler.allocator};

  /// The LLVM context.
  llvm::LLVMContext &llvmContext{compiler.get_llvm_context()};

  /// The LLVM module.
  llvm::Module &llvmModule{compiler.get_llvm_module()};

  /// The LLVM data layout.
  const llvm::DataLayout &llvmLayout{llvmModule.getDataLayout()};

  /// The LLVM target library info implementation.
  llvm::TargetLibraryInfoImpl llvmTargetLibraryInfoImpl{
      llvm::Triple(NativeTarget::get().triple)};

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

private:
  /// The builtin modules. See `get_builtin_module()`
  llvm::StringMap<BumpPtr<Module>> builtinModules{};

  /// The LLVM callees for builtin foreign functions. See `get_builtin_callee()`
  llvm::StringMap<llvm::FunctionCallee> builtinCallees{};

  /// The unique name counters. See `get_unique_name()`
  llvm::StringMap<llvm::DenseMap<llvm::Function *, uint64_t>> uniqueNames{};

  /// The arithmetic types, e.g., `bool3`, `int`, `float4`, `double2x3`.
  llvm::DenseMap<uint64_t, BumpPtr<ArithmeticType>> arithmeticTypes{};

  /// The array types.
  std::map<std::pair<Type *, uint32_t>, BumpPtr<ArrayType>> arrayTypes{};

  /// The inferred-size array types.
  std::map<std::pair<Type *, std::string>, BumpPtr<InferredSizeArrayType>>
      inferredSizeArrayTypes{};

  /// The pointer types.
  llvm::DenseMap<Type *, BumpPtr<PointerType>> pointerTypes{};

  /// The union types.
  ///
  /// Each `UnionType` is keyed by its canonical case types. See
  /// `UnionType::canonicalize_types()`.
  std::map<llvm::SmallVector<Type *>, BumpPtr<UnionType>> unionTypes{};

  /// The union index maps for remapping the union index when
  /// converting different union types.
  std::map<std::pair<UnionType *, UnionType *>, Value> unionIndexMaps{};

  /// The `auto` type.
  const BumpPtr<AutoType> autoType{allocator.allocate<AutoType>()};

  /// The meta `Module` type.
  const BumpPtr<MetaType> metaModuleType{
      allocator.allocate<MetaType>(*this, "module")};

  /// The meta `Type` type.
  const BumpPtr<MetaType> metaTypeType{
      allocator.allocate<MetaType>(*this, "type")};

  /// The meta `AST::Intrinsic` type.
  const BumpPtr<MetaType> metaIntrinsicType{
      allocator.allocate<MetaType>(*this, "intrinsic")};

  /// The meta `AST::Namespace` type.
  const BumpPtr<MetaType> metaNamespaceType{
      allocator.allocate<MetaType>(*this, "namespace")};

  /// The `void` type.
  const BumpPtr<VoidType> voidType{allocator.allocate<VoidType>(*this)};

  /// The `string` type.
  const BumpPtr<StringType> stringType{allocator.allocate<StringType>(*this)};

  /// The `color` type.
  const BumpPtr<ColorType> colorType{allocator.allocate<ColorType>(*this)};

  /// The `State` type.
  const BumpPtr<StateType> stateType{allocator.allocate<StateType>(*this)};

  /// The `texture_2d` type.
  const BumpPtr<Texture2DType> texture2DType{
      allocator.allocate<Texture2DType>()};

  /// The `texture_ptex` type.
  const BumpPtr<TexturePtexType> texturePtexType{
      allocator.allocate<TexturePtexType>(*this)};

  /// The AST associated types.
  llvm::DenseMap<AST::Decl *, BumpPtr<Type>> astTypes{};

  /// The compile-time union types.
  llvm::DenseMap<UnionType *, BumpPtr<ComptimeUnionType>> comptimeUnionTypes{};

  /// The keyword values.
  llvm::StringMap<Value> keywords{};

  /// The string constants.
  ///
  /// This is a string map such that identical string literals in the source
  /// code, such as a file name that may appear many times, end up referencing
  /// the same global string constant.
  llvm::StringMap<Value> strings{};

  /// The `material` type defined by the builtin `api` module.
  Type *materialType{};

  friend class FunctionType;
};

template <typename T, typename = void> struct get_type;

template <> struct get_type<void, void> {
  [[nodiscard]] static Type *get(Context &context) {
    return context.get_void_type();
  }
};

template <typename T>
struct get_type<T, std::enable_if_t<std::is_arithmetic_v<T>, void>> {
  [[nodiscard]] static Type *get(Context &context) {
    if constexpr (std::is_integral_v<T>)
      return context.get_arithmetic_type(Scalar::get_int(sizeof(T) * 8));
    else
      return context.get_arithmetic_type(Scalar::get_FP(sizeof(T) * 8));
  }
};

template <typename T> struct get_type<T &, void> {
  [[nodiscard]] static Type *get(Context &context) {
    return context.get_pointer_type(get_type<std::decay_t<T>>::get(context));
  }
};

template <typename T> struct get_type<T *, void> {
  [[nodiscard]] static Type *get(Context &context) {
    return context.get_pointer_type(get_type<std::decay_t<T>>::get(context));
  }
};

template <typename T, size_t M> struct get_type<T[M], void> {
  [[nodiscard]] static Type *get(Context &context) {
    return context.get_array_type(get_type<T>::get(context), M);
  }
};

template <typename T, size_t M> struct get_type<Vector<T, M>, void> {
  [[nodiscard]] static Type *get(Context &context) {
    return static_cast<ArithmeticType *>(get_type<T>::get(context))
        ->get_with_different_extent(context, Extent(M));
  }
};

template <typename T, size_t N, size_t M>
struct get_type<Matrix<T, N, M>, void> {
  [[nodiscard]] static Type *get(Context &context) {
    return static_cast<ArithmeticType *>(get_type<T>::get(context))
        ->get_with_different_extent(context, Extent(N, M));
  }
};

template <typename T> inline Type *Context::get_type() {
  return smdl::get_type<T>::get(*this);
}

/// \}

} // namespace smdl
