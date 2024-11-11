// vim:foldmethod=marker:foldlevel=0:fmr=--{,--}
#pragma once

#include "Value.h"

namespace smdl::Compiler {

enum class Scalar : uint8_t { None = 0, Bool, Int, Float, Double };

class Extent final {
public:
  constexpr Extent() = default;

  constexpr Extent(uint16_t numRows) : numRows(numRows) {}

  constexpr Extent(uint16_t numCols, uint16_t numRows) : numCols(numCols), numRows(numRows) {}

  [[nodiscard]] constexpr uint16_t get_vector_size() const { return numRows; }

  [[nodiscard]] constexpr bool is_scalar() const { return numCols == 1 && numRows == 1; }

  [[nodiscard]] constexpr bool is_vector() const { return numCols == 1 && numRows > 1; }

  [[nodiscard]] constexpr bool is_matrix() const { return numCols > 1 && numRows > 1; }

  [[nodiscard]] constexpr Extent get_transpose() const { return {numRows, numCols}; }

  [[nodiscard]] constexpr auto operator<=>(const Extent &) const = default;

  [[nodiscard]] constexpr bool operator==(const Extent &) const = default;

  [[nodiscard]] constexpr bool operator!=(const Extent &) const = default;

  [[nodiscard]] llvm::Type *apply_to_llvm_type(llvm::Type *llvmType) const {
    if (numRows > 1)
      llvmType = llvm::FixedVectorType::get(llvmType, numRows);
    if (numCols > 1)
      llvmType = llvm::ArrayType::get(llvmType, numCols);
    return llvmType;
  }

public:
  uint16_t numCols{1};

  uint16_t numRows{1};
};

template <typename T> struct arithmetic_type_traits {};

template <typename T> struct arithmetic_type_traits<const T> : arithmetic_type_traits<T> {};

template <> struct arithmetic_type_traits<int_t> {
  static constexpr Scalar scalar{Scalar::Int};
  static constexpr Extent extent{};
};

template <> struct arithmetic_type_traits<float_t> {
  static constexpr Scalar scalar{Scalar::Float};
  static constexpr Extent extent{};
};

template <> struct arithmetic_type_traits<double_t> {
  static constexpr Scalar scalar{Scalar::Double};
  static constexpr Extent extent{};
};

template <typename T, size_t M> struct arithmetic_type_traits<Vector<T, M>> {
  static constexpr Scalar scalar{arithmetic_type_traits<T>::scalar};
  static constexpr Extent extent{Extent(M)};
};

template <typename T, size_t N, size_t M> struct arithmetic_type_traits<Matrix<T, N, M>> {
  static constexpr Scalar scalar{arithmetic_type_traits<T>::scalar};
  static constexpr Extent extent{Extent(N, M)};
};

template <typename T>
concept arithmetic_type = arithmetic_type_traits<T>::scalar != Scalar::None;

enum class ConversionRule : unsigned {
  NotAllowed, ///< Conversion is not allowed.
  Explicit,   ///< Explicit conversion is allowed.
  Implicit,   ///< Implicit conversion is allowed.
  Perfect,    ///< Perfect conversion.
};

enum class TypeKind : unsigned {
  Array,
  Arithmetic,
  Auto,
  Color,
  Compiler,
  Enum,
  Function,
  Pointer,
  Struct,
  Tag,
  Union,
  Void,
};

class Context;
class Emitter;
class StructType;

class Type {
public:
  explicit Type(TypeKind kind, Context &context, Scalar scalar, Extent extent)
      : kind(kind), context(context), scalar(scalar), extent(extent) {}

  Type(const Type &) = delete;

  Type(Type &&) = delete;

  virtual ~Type() = default;

  [[nodiscard]] virtual Value insert(
      Emitter &emitter, Value value, Value elem, unsigned i, const AST::SourceLocation &srcLoc = {});

  [[nodiscard]] virtual Value access(
      Emitter &emitter, Value value, llvm::StringRef key, const AST::SourceLocation &srcLoc = {});

  [[nodiscard]] virtual Value access(Emitter &emitter, Value value, Value i, const AST::SourceLocation &srcLoc = {});

  [[nodiscard]] virtual Value construct(Emitter &emitter, const ArgList &args, const AST::SourceLocation &srcLoc = {});

  [[nodiscard]] virtual bool recursive_match(Emitter &emitter, Type *type) { return this == type; }

  [[nodiscard]] virtual bool is_zero_by_default() { return true; }

public:
  //--{ Transforms
  [[nodiscard]] Type *get_with_different_scalar(Scalar newScalar);

  [[nodiscard]] Type *get_scalar_type();

  [[nodiscard]] Type *get_column_type();

  [[nodiscard]] Type *get_transpose_type();

  [[nodiscard]] Type *get_element_type();

  [[nodiscard]] StructType *get_inline_struct_type();
  //--}

public:
  [[nodiscard]] bool is_abstract() const { return !llvmType; }

  [[nodiscard]] bool is_scalar() const { return scalar != Scalar::None && extent.is_scalar(); }

  [[nodiscard]] bool is_vector() const { return extent.is_vector() && !is_color(); }

  [[nodiscard]] bool is_matrix() const { return extent.is_matrix(); }

  [[nodiscard]] bool is_boolean() const { return scalar == Scalar::Bool; }

  [[nodiscard]] bool is_floating() const { return scalar == Scalar::Float || scalar == Scalar::Double; }

  [[nodiscard]] bool is_integral() const { return scalar == Scalar::Bool || scalar == Scalar::Int; }

  [[nodiscard]] bool is_vectorized() const { return is_scalar() || is_vector() || is_color(); }

  [[nodiscard]] uint32_t get_vector_size() const { return extent.get_vector_size(); }

  [[nodiscard]] uint32_t get_array_size() const;

  [[nodiscard]] bool is_optional() const;

  [[nodiscard]] bool is_optional_unique() const;

public:
  //--{ Kind checks
  [[nodiscard]] bool is_array() const { return kind == TypeKind::Array; }

  [[nodiscard]] bool is_size_deferred_array() const;

  [[nodiscard]] bool is_arithmetic() const { return kind == TypeKind::Arithmetic; }

  [[nodiscard]] bool is_auto() const { return kind == TypeKind::Auto; }

  [[nodiscard]] bool is_color() const { return kind == TypeKind::Color; }

  [[nodiscard]] bool is_compiler() const { return kind == TypeKind::Compiler; }

  [[nodiscard]] bool is_compiler_function() const;

  [[nodiscard]] bool is_compiler_intrinsic() const;

  [[nodiscard]] bool is_compiler_module() const;

  [[nodiscard]] bool is_compiler_type() const;

  [[nodiscard]] bool is_enum() const { return kind == TypeKind::Enum; }

  [[nodiscard]] bool is_function() const { return kind == TypeKind::Function; }

  [[nodiscard]] bool is_pointer() const { return kind == TypeKind::Pointer; }

  [[nodiscard]] bool is_struct() const { return kind == TypeKind::Struct; }

  [[nodiscard]] bool is_tag() const { return kind == TypeKind::Tag; }

  [[nodiscard]] bool is_union() const { return kind == TypeKind::Union; }

  [[nodiscard]] bool is_void() const { return kind == TypeKind::Void; }
  //--}

  //--{ Kind checks (specific structs)
  [[nodiscard]] bool is_string() const;

  [[nodiscard]] bool is_source_location() const;

  [[nodiscard]] bool is_texture_2d() const;

  [[nodiscard]] bool is_texture_3d() const;

  [[nodiscard]] bool is_texture_cube() const;

  [[nodiscard]] bool is_texture_ptex() const;

  [[nodiscard]] bool is_texture() const;
  //--}

  const TypeKind kind;

  Context &context;

  const Scalar scalar;

  const Extent extent;

  llvm::StringRef name{"(unnamed)"};

  llvm::Type *llvmType{};

protected:
  void init_name(const llvm::Twine &name);

  void init_name(const AST::Name &name) { this->name = name.name; }
};

template <TypeKind K> class TypeSubclass : public Type {
public:
  TypeSubclass(Context &context, Scalar scalar = Scalar::None, Extent extent = {1, 1}) : Type(K, context, scalar, extent) {}

  static bool classof(const Type *type) { return type->kind == K; }
};

//--{ ArrayType
class ArrayType final : public TypeSubclass<TypeKind::Array> {
public:
  ArrayType(Context &context, Type *elemType, uint32_t size);

  ArrayType(Context &context, Type *elemType, const llvm::Twine &sizeName);

  [[nodiscard]] Value insert(Emitter &emitter, Value value, Value elem, unsigned i, const AST::SourceLocation &srcLoc) final;

  [[nodiscard]] Value access(Emitter &emitter, Value value, llvm::StringRef key, const AST::SourceLocation &srcLoc) final;

  [[nodiscard]] Value access(Emitter &emitter, Value value, Value i, const AST::SourceLocation &srcLoc) final;

  [[nodiscard]] Value construct(Emitter &emitter, const ArgList &args, const AST::SourceLocation &srcLoc) final;

  [[nodiscard]] bool recursive_match(Emitter &emitter, Type *type) final;

  [[nodiscard]] bool is_zero_by_default() final { return elemType->is_zero_by_default(); }

public:
  Type *elemType{};

  uint32_t size{};

  llvm::StringRef sizeName{};
};
//--}

//--{ ArithmeticType
class ArithmeticType final : public TypeSubclass<TypeKind::Arithmetic> {
public:
  ArithmeticType(Context &context, Scalar scalar, Extent extent);

  [[nodiscard]] Value insert(Emitter &emitter, Value value, Value elem, unsigned i, const AST::SourceLocation &srcLoc) final;

  [[nodiscard]] Value access(Emitter &emitter, Value value, llvm::StringRef key, const AST::SourceLocation &srcLoc) final;

  [[nodiscard]] Value access(Emitter &emitter, Value value, Value i, const AST::SourceLocation &srcLoc) final;

  [[nodiscard]] Value construct(Emitter &emitter, const ArgList &args, const AST::SourceLocation &srcLoc) final;
};
//--}

//--{ AutoType
class AutoType final : public TypeSubclass<TypeKind::Auto> {
public:
  AutoType(Context &context) : TypeSubclass(context) { init_name("auto"); }

  [[nodiscard]] Value construct(Emitter &emitter, const ArgList &args, const AST::SourceLocation &srcLoc) final;

  [[nodiscard]] bool recursive_match(Emitter &emitter, Type *type) final { return true; }
};
//--}

//--{ ColorType
class ColorType final : public TypeSubclass<TypeKind::Color> {
public:
  ColorType(Context &context);

  [[nodiscard]] Value access(Emitter &emitter, Value value, llvm::StringRef key, const AST::SourceLocation &srcLoc = {}) final;

  [[nodiscard]] Value access(Emitter &emitter, Value value, Value i, const AST::SourceLocation &srcLoc = {}) final;

  [[nodiscard]] Value construct(Emitter &emitter, const ArgList &args, const AST::SourceLocation &srcLoc = {}) final;
};
//--}

//--{ CompilerType
class CompilerType final : public TypeSubclass<TypeKind::Compiler> {
public:
  CompilerType(Context &context, const llvm::Twine &name);
};
//--}

//--{ EnumType
template <typename T> struct builtin_enum_type_t final {};

template <typename T> constexpr auto builtin_enum_type = builtin_enum_type_t<T>{};

class EnumType final : public TypeSubclass<TypeKind::Enum> {
public:
  struct Constant final {
    llvm::StringRef name{};

    llvm::Constant *llvmConst{};

    AST::SourceLocation srcLoc{};
  };

  EnumType(Context &context, builtin_enum_type_t<intensity_mode_t>);

  EnumType(Context &context, AST::Enum *decl, llvm::Function *llvmFunc = {});

  [[nodiscard]] Value construct(Emitter &emitter, const ArgList &args, const AST::SourceLocation &srcLoc) final;

public:
  AST::Enum *const decl{};

  llvm::Function *const llvmFunc{};

  llvm::SmallVector<Constant> constants{};

  FunctionInstance *enumToString{};
};
//--}

//--{ FunctionType
class FunctionType final : public TypeSubclass<TypeKind::Function> {
public:
  FunctionType(Context &context, bool isPure, Type *returnType, llvm::SmallVector<Type *> paramTypes);

  [[nodiscard]] bool has_abstract_return_type() const { return returnType->is_abstract(); }

  [[nodiscard]] bool has_abstract_parameter_types() const {
    return detail::is_any_true(paramTypes, [](auto paramType) { return paramType->is_abstract(); });
  }

  [[nodiscard]] llvm::Function *create_default_llvm_function(const llvm::Twine &twine) const;

  bool isPure{};

  Type *returnType{};

  llvm::SmallVector<Type *> paramTypes{};
};
//--}

//--{ PointerType
class PointerType final : public TypeSubclass<TypeKind::Pointer> {
public:
  PointerType(Context &context, Type *elemTy);

  [[nodiscard]] Value access(Emitter &emitter, Value value, llvm::StringRef key, const AST::SourceLocation &srcLoc) final;

  [[nodiscard]] Value access(Emitter &emitter, Value value, Value i, const AST::SourceLocation &srcLoc) final;

  [[nodiscard]] Value construct(Emitter &emitter, const ArgList &args, const AST::SourceLocation &srcLoc) final;

  [[nodiscard]] bool recursive_match(Emitter &emitter, Type *type) final;

  Type *elemType{};
};
//--}

//--{ StructType
class TagType;

template <typename T> struct builtin_struct_type_t final {};

template <typename T> constexpr auto builtin_struct_type = builtin_struct_type_t<T>{};

class StructType final : public TypeSubclass<TypeKind::Struct> {
public:
  StructType(Context &context, AST::Struct *decl, llvm::Function *llvmFunc = {});

  StructType(Context &context, builtin_struct_type_t<default_bsdf_t>);

  StructType(Context &context, builtin_struct_type_t<default_edf_t>);

  StructType(Context &context, builtin_struct_type_t<default_vdf_t>);

  StructType(Context &context, builtin_struct_type_t<default_hair_bsdf_t>);

  StructType(Context &context, builtin_struct_type_t<material_emission_t>);

  StructType(Context &context, builtin_struct_type_t<material_surface_t>);

  StructType(Context &context, builtin_struct_type_t<material_volume_t>);

  StructType(Context &context, builtin_struct_type_t<material_geometry_t>);

  StructType(Context &context, builtin_struct_type_t<material_t>);

  StructType(Context &context, builtin_struct_type_t<image_t>);

  StructType(Context &context, builtin_struct_type_t<texture_2d_t>);

  StructType(Context &context, builtin_struct_type_t<texture_3d_t>);

  StructType(Context &context, builtin_struct_type_t<texture_cube_t>);

  StructType(Context &context, builtin_struct_type_t<texture_ptex_t>);

  StructType(Context &context, builtin_struct_type_t<state_t>);

  StructType(Context &context, builtin_struct_type_t<string_t>);

  StructType(Context &context, builtin_struct_type_t<source_location_t>);

  StructType(Context &context, StructType *parentTemplate, llvm::ArrayRef<Type *> missingTypes);

  [[nodiscard]] bool is_builtin_struct() const { return !decl; }

  [[nodiscard]] bool has_tag(TagType *tag) const { return std::find(tags.begin(), tags.end(), tag) != tags.end(); }

public:
  [[nodiscard]] Value insert(Emitter &emitter, Value value, Value elem, unsigned i, const AST::SourceLocation &srcLoc) final;

  [[nodiscard]] Value access(Emitter &emitter, Value value, llvm::StringRef key, const AST::SourceLocation &srcLoc) final;

  [[nodiscard]] Value construct(Emitter &emitter, const ArgList &args, const AST::SourceLocation &srcLoc) final;

  [[nodiscard]] bool recursive_match(Emitter &emitter, Type *type) final;

public:
  /// The parent template, if applicable.
  StructType *const parentTemplate{};

  /// The AST struct declaration, if applicable.
  AST::Struct *const decl{};

  /// The LLVM function this is declared inside, if applicable.
  llvm::Function *const llvmFunc{};

  /// The associated tags.
  llvm::SmallVector<TagType *> tags{};

  /// The fields.
  ParamList fields{};

private:
  void init_llvm_type();
};
//--}

//--{ TagType
class TagType final : public TypeSubclass<TypeKind::Tag> {
public:
  TagType(Context &context, const llvm::Twine &name) : TypeSubclass(context) { init_name(name); }

  TagType(Context &context, AST::Tag *decl, llvm::Function *llvmFunc = nullptr)
      : TypeSubclass(context), decl(decl), llvmFunc(llvmFunc) {
    init_name(*decl->name);
  }

  [[nodiscard]] Value construct(Emitter &emitter, const ArgList &args, const AST::SourceLocation &srcLoc) final;

  [[nodiscard]] bool recursive_match(Emitter &emitter, Type *type) final;

public:
  /// The AST tag declaration, if applicable.
  AST::Tag *const decl{};

  /// The LLVM function this is declared inside, if applicable.
  llvm::Function *const llvmFunc{};

  /// The default type, which is the type the default constructor this tag delegates to. If null, then it is
  /// not legal to default construct this tag.
  Type *defaultType{};

  void declare_default_type(Type *type, const AST::SourceLocation &srcLoc);
};
//--}

//--{ UnionType
class UnionType final : public TypeSubclass<TypeKind::Union> {
public:
  UnionType(Context &context, llvm::SmallVector<Type *> types);

  [[nodiscard]] bool has_type(Type *type) const { return index_of(type) != -1; }

  [[nodiscard]] bool has_all_types(UnionType *unionType) const {
    for (auto type : unionType->types)
      if (!has_type(type))
        return false;
    return true;
  }

  [[nodiscard]] int_t index_of(Type *type) const {
    if (auto itr{std::find(types.begin(), types.end(), type)}; itr != types.end())
      return itr - types.begin();
    return -1;
  }

  [[nodiscard]] Value access(Emitter &emitter, Value value, llvm::StringRef key, const AST::SourceLocation &srcLoc) final;

  [[nodiscard]] Value construct(Emitter &emitter, const ArgList &args, const AST::SourceLocation &srcLoc) final;

  [[nodiscard]] bool recursive_match(Emitter &emitter, Type *type);

public:
  llvm::SmallVector<Type *> types{};

  /// The required alignment in bytes.
  uint64_t requiredAlign{1};

  /// The required size in bytes.
  uint64_t requiredSize{1};

public:
  [[nodiscard]] static llvm::SmallVector<Type *> canonicalize_types(llvm::ArrayRef<Type *> types);
};
//--}

//--{ VoidType
class VoidType final : public TypeSubclass<TypeKind::Void> {
public:
  VoidType(Context &context);
};
//--}

} // namespace smdl::Compiler
