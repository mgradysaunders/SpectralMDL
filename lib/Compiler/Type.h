// vim:foldmethod=marker:foldlevel=0:fmr=--{,--}
#pragma once

#include "Value.h"

namespace smdl {

/// \addtogroup Compiler
/// \{

class ArithmeticType;

class TagType;

enum class TypeKind : uint32_t {
  Auto,
  Arithmetic,
  Array,
  Color,
  ComptimeUnion,
  Enum,
  Function,
  InferredSizeArray,
  Meta,
  Pointer,
  State,
  String,
  Struct,
  Tag,
  Union,
  Void
};

/// The type representation.
class Type {
public:
  explicit Type(TypeKind typeKind) : typeKind(typeKind) {}

  virtual ~Type() = default;

public:
  /// \name Virtual interface
  /// \{

  /// Is abstract?
  [[nodiscard]] virtual bool is_abstract() { return false; }

  /// Invoke.
  ///
  /// \param[inout] emitter  The emitter.
  /// \param[in]    args     The arguments.
  /// \param[in]    srcLoc   The source location if applicable.
  ///
  /// This typically constructs an instance of the `Type` that implements
  /// it. For example, `invoke()` on an `ArithmeticType` constructs an
  /// instance of that `ArithmeticType` from the given arguments.
  ///
  [[nodiscard]]
  virtual Value invoke(Emitter &emitter, const ArgumentList &args,
                       const SourceLocation &srcLoc);

  /// Has the given field?
  [[nodiscard]] virtual bool has_field(std::string_view name) {
    (void)name;
    return false;
  }

  /// Access the given field or throw an `Error` on failure.
  ///
  /// \param[inout] emitter  The emitter.
  /// \param[in]    value    The value (i.e., the instance of this type)
  /// \param[in]    name     The field name.
  /// \param[in]    srcLoc   The source location if applicable.
  ///
  [[nodiscard]]
  virtual Value access_field(Emitter &emitter, Value value,
                             std::string_view name,
                             const SourceLocation &srcLoc);

  /// Access the given index or throw an `Error` on failure.
  ///
  /// \param[inout] emitter  The emitter.
  /// \param[in]    value    The value (i.e., the instance of this type)
  /// \param[in]    i        The index, presumably an integer type.
  /// \param[in]    srcLoc   The source location if applicable.
  ///
  [[nodiscard]]
  virtual Value access_index(Emitter &emitter, Value value, Value i,
                             const SourceLocation &srcLoc);

  /// Insert element into compound value. Turns everything into rvalues.
  ///
  /// \param[inout] emitter
  [[nodiscard]]
  virtual Value insert(Emitter &emitter, Value value, Value elem, unsigned i,
                       const SourceLocation &srcLoc);

  /// \}

public:
  /// Is instance of `ArrayType`?
  [[nodiscard]] bool is_array() const { return typeKind == TypeKind::Array; }

  /// Is instance of `InferredSizeArrayType`?
  [[nodiscard]] bool is_inferred_size_array() const {
    return typeKind == TypeKind::InferredSizeArray;
  }

  /// Is instance of `ArithmeticType`?
  [[nodiscard]] bool is_arithmetic() const {
    return typeKind == TypeKind::Arithmetic;
  }

  /// Is instance of `ArithmeticType` with integral boolean scalar?
  [[nodiscard]] bool is_arithmetic_boolean() const;

  /// Is instance of `ArithmeticType` with integral scalar?
  [[nodiscard]] bool is_arithmetic_integral() const;

  /// Is instance of `ArithmeticType` with floating point scalar?
  [[nodiscard]] bool is_arithmetic_floating_point() const;

  /// Is instance of `ArithmeticType` with scalar extent?
  [[nodiscard]] bool is_arithmetic_scalar() const;

  /// Is instance of `ArithmeticType` with vector extent?
  [[nodiscard]] bool is_arithmetic_vector() const;

  /// Is instance of `ArithmeticType` with matrix extent?
  [[nodiscard]] bool is_arithmetic_matrix() const;

  /// Is arithmetic scalar int type?
  [[nodiscard]] bool is_arithmetic_scalar_int() const {
    return is_arithmetic_scalar() && is_arithmetic_integral();
  }

  /// Is instance of `ColorType`?
  [[nodiscard]] bool is_color() const { return typeKind == TypeKind::Color; }

  /// Is instance of `EnumType`?
  [[nodiscard]] bool is_enum() const { return typeKind == TypeKind::Enum; }

  /// Is instance of `FunctionType`?
  [[nodiscard]] bool is_function() const {
    return typeKind == TypeKind::Function;
  }

  /// Is instance of `MetaType`?
  [[nodiscard]] bool is_meta() const { return typeKind == TypeKind::Meta; }

  /// Is instance of `PointerType`?
  [[nodiscard]] bool is_pointer() const {
    return typeKind == TypeKind::Pointer;
  }

  /// Is instance of `StringType`?
  [[nodiscard]] bool is_string() const { return typeKind == TypeKind::String; }

  /// Is instance of `StructType`?
  [[nodiscard]] bool is_struct() const { return typeKind == TypeKind::Struct; }

  /// Is instance of `TagType`?
  [[nodiscard]] bool is_tag() const { return typeKind == TypeKind::Tag; }

  /// Is instance of `UnionType`?
  [[nodiscard]] bool is_union() const { return typeKind == TypeKind::Union; }

  /// Is instance of `UnionType` where `void` is an alternative?
  [[nodiscard]] bool is_optional_union() const;

  /// Is instance of `UnionType` or multi-level `PointerType`
  /// to instance of `UnionType`?
  [[nodiscard]] bool is_union_or_pointer_to_union() const {
    return get_first_non_pointer_type()->is_union();
  }

  /// Is instance of `ComptimeUnionType`?
  [[nodiscard]] bool is_comptime_union() const {
    return typeKind == TypeKind::ComptimeUnion;
  }

  /// Is instance of `ArithmeticType` with scalar or vector extent OR
  /// is instance of `ColorType`?
  [[nodiscard]] bool is_vectorized() const {
    return is_arithmetic_scalar() || is_arithmetic_vector() || is_color();
  }

  /// Is instance of `VoidType`?
  [[nodiscard]] bool is_void() const { return typeKind == TypeKind::Void; }

public:
  /// If `is_pointer()`, returns the `pointeeType`, else returns null.
  [[nodiscard]] Type *get_pointee_type() const;

  /// Get the first non-pointer type.
  [[nodiscard]] Type *get_first_non_pointer_type() const;

  /// Get the first non-pointer type depth.
  ///
  /// For example,
  /// - Pointer depth is 0 for `int`
  /// - Pointer depth is 1 for `&int`
  /// - Pointer depth is 2 for `&&int`
  /// - Pointer depth is 3 for `&&&int`
  ///
  [[nodiscard]] size_t get_first_non_pointer_type_depth() const;

  /// Get the display name as C-string.
  [[nodiscard]] const char *get_display_name() const {
    return displayName.c_str();
  }

public:
  /// The type kind.
  const TypeKind typeKind;

  /// The display name, not necessarily unique but useful
  /// for printing, debugging, and error messages.
  std::string displayName{};

  /// The LLVM type.
  llvm::Type *llvmType{};
};

template <TypeKind K> class TypeSubclass : public Type {
public:
  TypeSubclass() : Type(K) {}

  static bool classof(const Type *type) { return type->typeKind == K; }
};

//--{ Helper type: Scalar
class Scalar final {
public:
  /// The intent or interpretation of the scalar.
  enum class Intent : uint8_t { None = 0, Int = 1, FP = 2 };

  /// Is non-none?
  [[nodiscard]] operator bool() const { return intent != Intent::None; }

  /// Is boolean? (i.e., integral and has only 1 bit)
  [[nodiscard]] bool is_boolean() const {
    return is_integral() && numBits == 1;
  }

  /// Is integral?
  [[nodiscard]] bool is_integral() const { return intent == Intent::Int; }

  /// Is floating point?
  [[nodiscard]] bool is_floating_point() const { return intent == Intent::FP; }

  /// Get common scalar between this and the given `other` scalar.
  [[nodiscard]] Scalar get_common(const Scalar &other) const {
    auto result{Scalar{std::max(intent, other.intent),
                       std::max(numBits, other.numBits)}};
    if (result.intent == Intent::FP) {
      if (result.numBits > 80)
        result.numBits = 128; // FP 128
      else if (result.numBits > 64)
        result.numBits = 80; // FP 80
      else if (result.numBits > 32)
        result.numBits = 64; // FP 64
      else if (result.numBits > 16)
        result.numBits = 32; // FP 32
      else
        result.numBits = 16; // FP 16
    }
    return result;
  }

  /// Get the corresponding LLVM type.
  [[nodiscard]] llvm::Type *get_llvm_type(llvm::LLVMContext &context) const;

  /// Convert to string printing and debugging.
  [[nodiscard]] std::string to_string() const {
    if (intent == Intent::Int) {
      if (numBits == 1) {
        return "bool";
      } else if (numBits == sizeof(int) * 8) {
        return "int";
      } else {
        return concat("#type_int(", numBits, ")");
      }
    } else {
      if (numBits == 32) {
        return "float";
      } else if (numBits == 64) {
        return "double";
      } else {
        return concat("#type_float(", numBits, ")");
      }
    }
  }

public:
  /// The intent.
  Intent intent{Intent::None};

  /// The number of bits.
  uint8_t numBits{};

public:
  /// Get boolean scalar, i.e., `get_int(1)`.
  [[nodiscard]] static constexpr Scalar get_bool() { return {Intent::Int, 1}; }

  /// Get integral scalar with the given `numBits`.
  ///
  /// \note
  /// In SMDL, this corresponds to the `#type_int()` intrinsic.
  ///
  /// \note
  /// For integral scalars, the `numBits` is not restricted to be a power of
  /// two. That is, `get_int(11)` represents an 11-bit integer, and LLVM is
  /// capable of emulating arbitrary widths (or at least I think?)
  [[nodiscard]] static constexpr Scalar get_int(uint8_t numBits) {
    return {Intent::Int, numBits};
  }

  /// Get floating-point scalar with the given `numBits`.
  ///
  /// \note
  /// In SMDL, this corresponds to the `#type_float()` intrinsic.
  ///
  /// \note
  /// For floating point scalars, the `numBits` may only ever be
  /// - `numBits = 16` or half precision,
  /// - `numBits = 32` or single precision,
  /// - `numBits = 64` or double precision,
  /// - `numBits = 80` or extended precision (not always portable),
  /// - `numBits = 128` or quadruple precision (not always portable).
  [[nodiscard]] static constexpr Scalar get_FP(uint8_t numBits) {
    return {Intent::FP, numBits};
  }

  /// Get half-precision floating point scalar.
  [[nodiscard]] static constexpr Scalar get_half() { return {Intent::FP, 16}; }

  /// Get single-precision floating point scalar.
  [[nodiscard]] static constexpr Scalar get_float() { return {Intent::FP, 32}; }

  /// Get double-precision floating point scalar.
  [[nodiscard]] static constexpr Scalar get_double() {
    return {Intent::FP, 64};
  }

  template <typename T> [[nodiscard]] static constexpr Scalar get() {
    static_assert(std::is_arithmetic_v<T>);
    if constexpr (std::is_integral_v<T>)
      return get_int(sizeof(T) * 8);
    else
      return get_FP(sizeof(T) * 8);
  }
};
//--}

//--{ Helper type: Extent
class Extent final {
public:
  constexpr Extent() = default;

  /// The vector constructor.
  constexpr Extent(uint16_t numRows) : numRows(numRows) {}

  /// The matrix constructor.
  constexpr Extent(uint16_t numCols, uint16_t numRows)
      : numCols(numCols), numRows(numRows) {}

  /// Get the vector size.
  [[nodiscard]] constexpr uint16_t get_vector_size() const { return numRows; }

  /// Is scalar?
  [[nodiscard]] constexpr bool is_scalar() const {
    return numCols == 1 && numRows == 1;
  }

  /// Is vector?
  [[nodiscard]] constexpr bool is_vector() const {
    return numCols == 1 && numRows > 1;
  }

  /// Is matrix?
  [[nodiscard]] constexpr bool is_matrix() const {
    return numCols > 1 && numRows > 1;
  }

  /// Get the transpose extent.
  [[nodiscard]] constexpr Extent get_transpose() const {
    return {numRows, numCols};
  }

  /// Get the common extent.
  [[nodiscard]] constexpr Extent get_common(const Extent &other) const {
    return {std::max(numCols, other.numCols), std::max(numRows, other.numRows)};
  }

  /// Apply the extent to the given LLVM type.
  ///
  /// - If `is_scalar()`, returns `llvmType` unchanged.
  /// - If `is_vector()`, returns vector type of `llvmType`.
  /// - If `is_matrix()`, returns array type of column vector types
  ///   of `llvmType`.
  [[nodiscard]] llvm::Type *get_llvm_type(llvm::Type *llvmType) const {
    if (numRows > 1)
      llvmType = llvm::FixedVectorType::get(llvmType, numRows);
    if (numCols > 1)
      llvmType = llvm::ArrayType::get(llvmType, numCols);
    return llvmType;
  }

  /// Convert to string for printing or debugging.
  [[nodiscard]] std::string to_string(const Scalar &scalar) const {
    std::string str{scalar.to_string()};
    if (is_scalar()) {
      return str;
    } else if (is_vector()) {
      return str[0] != '#' && numRows <= 4
                 ? concat(str, numRows)
                 : concat("#type_vector(", str, ", ", numRows, ")");
    } else if (is_matrix()) {
      return str[0] != '#' && numCols <= 4 && numRows <= 4
                 ? concat(str, numCols, "x", numRows)
                 : concat("#type_matrix(", str, ", ", numCols, ", ", numRows,
                          ")");
    }
    SMDL_SANITY_CHECK(false);
    return {};
  }

  [[nodiscard]] constexpr bool operator==(const Extent &other) const {
    return numCols == other.numCols && numRows == other.numRows;
  }

  [[nodiscard]] constexpr bool operator!=(const Extent &other) const {
    return numCols != other.numCols || numRows != other.numRows;
  }

public:
  /// The number of columns.
  uint16_t numCols{1};

  /// The number of rows.
  uint16_t numRows{1};
};
//--}

/// An arithmetic type, includes all builtin scalar, vector, and matrix types.
class ArithmeticType final : public TypeSubclass<TypeKind::Arithmetic> {
public:
  explicit ArithmeticType(Context &context, Scalar scalar, Extent extent);

public:
  /// \name Virtual interface
  /// \{

  [[nodiscard]]
  Value invoke(Emitter &emitter, const ArgumentList &args,
               const SourceLocation &srcLoc) final;

  [[nodiscard]] bool has_field(std::string_view name) final;

  [[nodiscard]]
  Value access_field(Emitter &emitter, Value value, std::string_view name,
                     const SourceLocation &srcLoc) final;

  [[nodiscard]]
  Value access_index(Emitter &emitter, Value value, Value i,
                     const SourceLocation &srcLoc) final;

  [[nodiscard]]
  Value insert(Emitter &emitter, Value value, Value elem, unsigned i,
               const SourceLocation &srcLoc) final;

  /// \}

public:
  /// Get the `ArithmeticType` with the given `newScalar`.
  [[nodiscard]] ArithmeticType *get_with_different_scalar(Context &context,
                                                          Scalar newScalar);

  /// Get the `ArithmeticType` with the given `newExtent`.
  [[nodiscard]] ArithmeticType *get_with_different_extent(Context &context,
                                                          Extent newExtent);

  /// Get the `ArithmeticType` with the same `scalar` and `Extent(1)`.
  [[nodiscard]] ArithmeticType *get_scalar_type(Context &context) {
    return get_with_different_extent(context, Extent(1));
  }

  /// Get the `ArithmeticType` with the same `scalar` and `Extent(numRows)`.
  [[nodiscard]] ArithmeticType *get_column_type(Context &context) {
    return get_with_different_extent(context, Extent(extent.numRows));
  }

  /// Get the `ArithmeticType` with the same `scalar` and
  /// `extent.get_transpose()`.
  [[nodiscard]] ArithmeticType *get_transpose_type(Context &context) {
    return get_with_different_extent(context, extent.get_transpose());
  }

  /// Get the common type.
  [[nodiscard]] ArithmeticType *get_common_type(Context &context,
                                                ArithmeticType *otherType);

public:
  /// The scalar type.
  Scalar scalar{};

  /// The extent, determining whether this a scalar, vector, or matrix.
  Extent extent{};

private:
  /// Convert the given character to an index.
  ///
  /// - 'x' is 0
  /// - 'y' is 1
  /// - 'z' is 2
  /// - 'w' is 3
  ///
  [[nodiscard]] std::optional<uint32_t> to_index(char name) const {
    if (!extent.is_scalar()) {
      for (uint16_t i{};
           i < extent.is_vector() ? extent.numRows : extent.numCols; i++)
        if (name == "xyzw"[i])
          return i;
    }
    return std::nullopt;
  }

  /// Convert the given string to an index sequence for swizzling.
  [[nodiscard]] std::optional<llvm::SmallVector<int>>
  to_index_swizzle(llvm::StringRef name) const {
    if (extent.is_vector()) {
      llvm::SmallVector<int> swizzle{};
      for (char c : name) {
        int i{(int(c - 'w') + 3) & 3};
        if (i < 0 || unsigned(i) >= extent.numRows)
          return std::nullopt;
        swizzle.push_back(i);
      }
      return swizzle;
    }
    return std::nullopt;
  }
};

/// An array type.
class ArrayType final : public TypeSubclass<TypeKind::Array> {
public:
  explicit ArrayType(Context &context, Type *elemType, uint32_t size);

public:
  /// \name Virtual interface
  /// \{

  [[nodiscard]] bool is_abstract() final {
    return !llvmType || elemType->is_abstract();
  }

  [[nodiscard]]
  Value invoke(Emitter &emitter, const ArgumentList &args,
               const SourceLocation &srcLoc) final;

  [[nodiscard]] bool has_field(std::string_view name) final {
    return elemType->has_field(name);
  }

  [[nodiscard]]
  Value access_field(Emitter &emitter, Value value, std::string_view name,
                     const SourceLocation &srcLoc) final;

  [[nodiscard]]
  Value access_index(Emitter &emitter, Value value, Value i,
                     const SourceLocation &srcLoc) final;

  [[nodiscard]]
  Value insert(Emitter &emitter, Value value, Value elem, unsigned i,
               const SourceLocation &srcLoc) final;

  /// \}

public:
  /// Get the `ArrayType` with the given `newElemType`.
  [[nodiscard]] ArrayType *get_with_different_element_type(Context &context,
                                                           Type *newElemType);

  /// Get the `ArrayType` with the given `newSize`.
  [[nodiscard]] ArrayType *get_with_different_size(Context &context,
                                                   uint32_t newSize);

public:
  /// The element type.
  Type *elemType{};

  /// The size.
  uint32_t size{};
};

/// The auto type.
///
/// This is an abstract type (i.e., with no LLVM representation) that simply
/// behaves as an identity function constructor. So invoking `auto(x)` returns
/// `x` as an rvalue. This allows for type inference in parameter, field, and
/// variable declarations.
///
class AutoType final : public TypeSubclass<TypeKind::Auto> {
public:
  AutoType() { displayName = "auto"; }

public:
  /// \name Virtual interface
  /// \{

  [[nodiscard]] bool is_abstract() final { return true; }

  [[nodiscard]]
  Value invoke(Emitter &emitter, const ArgumentList &args,
               const SourceLocation &srcLoc) final;

  /// \}
};

/// The color type, essentially a float vector.
class ColorType final : public TypeSubclass<TypeKind::Color> {
public:
  explicit ColorType(Context &context);

public:
  /// \name Virtual interface
  /// \{

  [[nodiscard]]
  Value invoke(Emitter &emitter, const ArgumentList &args,
               const SourceLocation &srcLoc) final;

  [[nodiscard]]
  Value access_index(Emitter &emitter, Value value, Value i,
                     const SourceLocation &srcLoc) final;

  [[nodiscard]]
  Value insert(Emitter &emitter, Value value, Value elem, unsigned i,
               const SourceLocation &srcLoc) final;

  /// \}

public:
  /// Get the arithmetic scalar type, i.e., `float`.
  [[nodiscard]] ArithmeticType *get_arithmetic_scalar_type(Context &context);

  /// Get the arithmetic vector type, i.e, `floatN` where `N` is
  /// `wavelengthBaseMax`.
  [[nodiscard]] ArithmeticType *get_arithmetic_vector_type(Context &context);

public:
  /// The number of wavelengths.
  uint32_t wavelengthBaseMax{};
};

class UnionType;

/// A compile-time union type for convenient type-checking.
///
/// The syntax is, for example,
/// ~~~~~~~~~~~~~~~~~~~~~~
/// $(color | float) tint;
/// ~~~~~~~~~~~~~~~~~~~~~~
/// which indicates that `tint` may be either `color` or `float`, but
/// it is determined at compile-time.
///
class ComptimeUnionType final : public TypeSubclass<TypeKind::ComptimeUnion> {
public:
  explicit ComptimeUnionType(UnionType *unionType);

  /// \name Virtual interface
  /// \{

  [[nodiscard]] bool is_abstract() final { return true; }

  [[nodiscard]]
  Value invoke(Emitter &emitter, const ArgumentList &args,
               const SourceLocation &srcLoc) final;

  /// \}

  /// The union type.
  UnionType *unionType{};
};

/// An enum type.
class EnumType final : public TypeSubclass<TypeKind::Enum> {
public:
  explicit EnumType(AST::Enum &decl) : decl(decl) {
    displayName = std::string(decl.name.srcName);
  }

  void initialize(Emitter &emitter);

public:
  /// \name Virtual interface
  /// \{

  [[nodiscard]]
  Value invoke(Emitter &emitter, const ArgumentList &args,
               const SourceLocation &srcLoc) final;

  /// \}

public:
  /// The AST declaration.
  AST::Enum &decl;

  /// The LLVM function to convert enums to readable strings.
  llvm::Function *llvmFuncToString{};
};

/// A function type.
///
/// This is one-to-one correspondence with an `AST::Function` declaration, and
/// is used to implement function overload resolution and function calls
/// through the `invoke()` method.
///
/// Note that the idea of _function type_ here is distinct from _function type
/// signature_ or _function pointer_.
///
class FunctionType final : public TypeSubclass<TypeKind::Function> {
public:
  explicit FunctionType(AST::Function &decl) : decl(decl) {
    displayName = std::string(decl.name.srcName);
  }

  void initialize(Emitter &emitter);

public:
  /// \name Virtual interface
  /// \{

  [[nodiscard]] bool is_abstract() final { return params.is_abstract(); }

  /// Invoke. This is the entry point for function calls and function
  /// overload resolution.
  ///
  /// In the other `Type` subclasses, `invoke` generally represents a
  /// constructor for the type. Here, `invoke` is responsible for performing
  /// overload resolution and calling an LLVM function or inlining a function
  /// variant or macro.
  ///
  /// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  /// // FunctionType with name `add`
  /// int add(int a, int b) = a + b; // <-------------+
  /// //                                              |
  /// // Another FunctionType with name `add`         |
  /// int add(int a, int b, int c) = a + b + c; // ---+  prevOverload
  /// //   |
  /// //   +----------------+
  /// //                    |
  /// unit_test "Add" { //  |
  ///   // Here, the name `add` resolves to the most recently
  ///   // declared FunctionType called `add`. When invoked,
  ///   // it is able to determine that the previous overload
  ///   // is the function we actually need to call.
  ///   int s = add(1, 2);
  /// }
  /// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ///
  [[nodiscard]]
  Value invoke(Emitter &emitter, const ArgumentList &args,
               const SourceLocation &srcLoc) final;

  /// \}

public:
  /// Is marked with the attribute `@(pure)`?
  [[nodiscard]] bool is_pure() const { return decl.has_attribute("pure"); }

  /// Is marked with the attribute `@(macro)`?
  [[nodiscard]] bool is_macro() const {
    return decl.has_attribute("macro") || isMaterial;
  }

  /// Is marked with the attribute `@(foreign)`?
  [[nodiscard]] bool is_foreign() const {
    return decl.has_attribute("foreign");
  }

  /// Is function variant?
  [[nodiscard]] bool is_variant() const { return decl.is_variant(); }

  /// Has no overloads?
  [[nodiscard]] bool has_no_overloads() const {
    return !prevOverload && !nextOverload;
  }

  /// Resolve overload.
  [[nodiscard]] FunctionType *resolve_overload(Emitter &emitter,
                                               const ArgumentList &args,
                                               const SourceLocation &srcLoc);

  /// An instantiation with concrete types and a concrete LLVM function.
  struct Instance final {
    /// The concrete return type.
    Type *returnType{};

    /// The LLVM function.
    llvm::Function *llvmFunc{};

    /// Is currently compiling? This is used to sanity check that we
    /// do not accidentally enter an infinite loop.
    bool isCompiling{};
  };

  /// Instantiate a concrete LLVM function.
  Instance &instantiate(Emitter &emitter,
                        const llvm::SmallVector<Type *> paramTypes);

public:
  /// The AST declaration.
  AST::Function &decl;

  /// The AST declaration name.
  const std::string_view declName{decl.name.srcName};

  /// The prev overload by declaration order.
  FunctionType *prevOverload{};

  /// The next overload by declaration order.
  FunctionType *nextOverload{};

  /// The return type.
  Type *returnType{};

  /// The parameters.
  ParameterList params{};

  /// The function instances.
  std::map<llvm::SmallVector<Type *>, Instance> instances{};

  /// The macro recursion depth counter to detect run-away
  /// recursion at compile time.
  size_t macroRecursionDepth{};

  bool isMaterial{};

private:
  void initialize_jit_material_functions(Emitter &emitter);
};

/// An inferred-size array type.
class InferredSizeArrayType final
    : public TypeSubclass<TypeKind::InferredSizeArray> {
public:
  explicit InferredSizeArrayType(Type *elemType, std::string sizeName)
      : elemType(elemType), sizeName(std::move(sizeName)) {
    displayName = elemType->displayName;
    displayName += '[';
    if (!sizeName.empty()) {
      displayName += '<';
      displayName += sizeName;
      displayName += '>';
    }
    displayName += ']';
    sizeNameStrv = this->sizeName;
  }

  /// \name Virtual interface
  /// \{

  [[nodiscard]] bool is_abstract() final { return true; }

  [[nodiscard]]
  Value invoke(Emitter &emitter, const ArgumentList &args,
               const SourceLocation &srcLoc) final;

  /// \}

public:
  /// The element type.
  Type *elemType{};

  /// The name of the inferred size, e.g., `Size` in the expression
  /// `int[<Size>]`. This may be empty!
  std::string sizeName{};

  std::string_view sizeNameStrv{};
};

/// A meta type, essentially a compile-time pointer (represented in LLVM
/// as the equivalent int type `uintptr_t`) to a compiler type.
class MetaType final : public TypeSubclass<TypeKind::Meta> {
public:
  explicit MetaType(llvm::LLVMContext &context, std::string name) {
    displayName = std::move(name);
    llvmType = llvm_int_ptr_type(context);
  }

public:
  /// \name Virtual interface
  /// \{

  [[nodiscard]] bool has_field(std::string_view) final {
    // We consider that meta-types have all possible fields, so attempting
    // to look up any given string is legal. If no such field exists, we
    // ultimately return void without considering it an error.
    return true;
  }

  [[nodiscard]]
  Value access_field(Emitter &emitter, Value value, std::string_view name,
                     const SourceLocation &srcLoc) final;

  /// \}
};

/// A pointer type.
class PointerType final : public TypeSubclass<TypeKind::Pointer> {
public:
  explicit PointerType(Context &context, Type *pointeeType);

public:
  /// \name Virtual interface
  /// \{

  [[nodiscard]] bool is_abstract() final { return pointeeType->is_abstract(); }

  [[nodiscard]]
  Value invoke(Emitter &emitter, const ArgumentList &args,
               const SourceLocation &srcLoc) final;

  [[nodiscard]] bool has_field(std::string_view name) final {
    return pointeeType->has_field(name);
  }

  [[nodiscard]]
  Value access_field(Emitter &emitter, Value value, std::string_view name,
                     const SourceLocation &srcLoc) final;

  [[nodiscard]]
  Value access_index(Emitter &emitter, Value value, Value i,
                     const SourceLocation &srcLoc) final;

  /// \}

public:
  /// The pointee type.
  Type *pointeeType{};
};

/// The state type.
class StateType final : public TypeSubclass<TypeKind::State> {
public:
  explicit StateType(Context &context);

  /// \name Virtual interface
  /// \{

  [[nodiscard]] bool has_field(std::string_view name) final {
    for (auto &field : fields)
      if (field.name == name)
        return true;
    return false;
  }

  [[nodiscard]]
  Value access_field(Emitter &emitter, Value value, std::string_view name,
                     const SourceLocation &srcLoc) final;

  /// \}

private:
  struct Field final {
    Type *type{};

    std::string_view name{};

    uint64_t offset{};
  };

  std::vector<Field> fields{};
};

/// The string type, essentially a pointer to a null-terminated string literal.
class StringType final : public TypeSubclass<TypeKind::String> {
public:
  explicit StringType(Context &context);

  /// \name Virtual interface
  /// \{

  [[nodiscard]]
  Value invoke(Emitter &emitter, const ArgumentList &args,
               const SourceLocation &srcLoc) final;

  [[nodiscard]] bool has_field(std::string_view name) final {
    return name == "size";
  }

  [[nodiscard]]
  Value access_field(Emitter &emitter, Value value, std::string_view name,
                     const SourceLocation &srcLoc) final;

  /// \}
};

/// A struct type.
class StructType final : public TypeSubclass<TypeKind::Struct> {
public:
  explicit StructType(AST::Struct &decl) : decl(decl) {
    displayName = std::string(decl.name.srcName);
  }

  void initialize(Emitter &emitter);

  /// Instantiate a concrete struct from this presumably abstract struct.
  ///
  /// \param[in] context
  /// The context.
  ///
  /// \param[in] paramTypes
  /// The parameter types, used to replace each type in `params` in
  /// the instantiated struct type. Every type must be non-null and
  /// concrete!
  ///
  /// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  /// struct Foo {
  ///   auto bar;
  /// };
  ///
  /// unit_test "Foo" {
  ///   auto foo1 = Foo(bar: 2);
  ///   auto foo2 = Foo(bar: "Hello, world");
  /// }
  /// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ///
  /// In the example code, `foo1` and `foo2` have distinct concrete
  /// struct types instantiated from the abstract `Foo` type definition.
  ///
  [[nodiscard]]
  StructType *instantiate(Context &context,
                          const llvm::SmallVector<Type *> &paramTypes);

  /// Is instantiation of the given type?
  ///
  /// \returns
  /// Return true if
  /// - `type` is a struct type and `instanceOf == type`, OR
  /// - `type` is a tag type that appears in `tags`.
  ///
  [[nodiscard]] bool is_instance_of(Type *type) const {
    return instanceOf == type ||
           std::find(tags.begin(), tags.end(), type) != tags.end();
  }

public:
  /// \name Virtual interface
  /// \{

  [[nodiscard]] bool is_abstract() final;

  [[nodiscard]]
  Value invoke(Emitter &emitter, const ArgumentList &args,
               const SourceLocation &srcLoc) final;

  [[nodiscard]] bool has_field(std::string_view name) final;

  [[nodiscard]]
  Value access_field(Emitter &emitter, Value value, std::string_view name,
                     const SourceLocation &srcLoc) final;

  [[nodiscard]]
  Value insert(Emitter &emitter, Value value, Value elem, unsigned i,
               const SourceLocation &srcLoc) final;

  /// \}

public:
  /// If applicable, the abstract struct this is an instance of.
  StructType *instanceOf{};

  /// The AST declaration.
  AST::Struct &decl;

  /// The tags.
  llvm::SmallVector<TagType *> tags{};

  /// A constructor.
  class Constructor final {
  public:
    /// The AST constructor.
    AST::Struct::Constructor *astConstructor{};

    /// The parameters.
    ParameterList params{};

    /// Is currently invoking? This is used to prevent recursion which
    /// we do not allow for constructors.
    bool isInvoking{};
  };

  /// The constructors.
  std::vector<Constructor> constructors{};

  /// The parameters, i.e., the struct fields.
  ParameterList params{};

  /// The static fields.
  ///
  /// \note
  /// For instances of generic structs, we do not copy the static fields
  /// and instead use `instance_of().staticFields` to access the lookup
  /// table in the parent struct.
  ///
  llvm::StringMap<Value> staticFields{};

  /// If applicable, the instances of this abstract struct.
  std::map<llvm::SmallVector<Type *>, BumpPtr<StructType>> instances{};

  /// Is this the default instance of the abstract `instanceOf` struct?
  bool isDefaultInstance{};

public:
  [[nodiscard]] auto &instance_of() { return instanceOf ? *instanceOf : *this; }

  [[nodiscard]] auto &instance_of() const {
    return instanceOf ? *instanceOf : *this;
  }
};

/// A tag type.
class TagType final : public TypeSubclass<TypeKind::Tag> {
public:
  explicit TagType(std::string name) { displayName = std::move(name); }

public:
  /// \name Virtual interface
  /// \{

  [[nodiscard]] bool is_abstract() final { return true; }

  [[nodiscard]] Value invoke(Emitter &emitter, const ArgumentList &args,
                             const SourceLocation &srcLoc) final;

  /// \}

public:
  /// The default type. This may be null!
  Type *defaultType{};
};

/// A union type.
class UnionType final : public TypeSubclass<TypeKind::Union> {
public:
  explicit UnionType(Context &context, llvm::SmallVector<Type *> caseTys);

public:
  /// \name Virtual interface
  /// \{

  [[nodiscard]] Value invoke(Emitter &emitter, const ArgumentList &args,
                             const SourceLocation &srcLoc) final;

  [[nodiscard]] bool has_field(std::string_view name) final {
    for (auto caseType : caseTypes)
      if (!(caseType->is_void() || caseType->has_field(name)))
        return false;
    return true;
  }

  [[nodiscard]]
  Value access_field(Emitter &emitter, Value value, std::string_view name,
                     const SourceLocation &srcLoc) final;

  /// \}

public:
  /// Get the index of the given case type.
  [[nodiscard]] int get_case_type_index(Type *type) const {
    for (int i = 0; i < int(caseTypes.size()); i++)
      if (caseTypes[i] == type)
        return i;
    return -1;
  }

  /// Has the given case type?
  [[nodiscard]] bool has_case_type(Type *type) const {
    return std::find(caseTypes.begin(), caseTypes.end(), type) !=
           caseTypes.end();
  }

  /// Has all case types of the given union type?
  [[nodiscard]] bool has_all_case_types(UnionType *unionType) const {
    for (auto caseType : unionType->caseTypes)
      if (!has_case_type(caseType))
        return false;
    return true;
  }

  /// Is always an instance of the given type?
  ///
  /// At the moment, there are three ways this happens:
  /// - Every case type is a `StructType` that is a template instantiation of
  ///   the given type (which must be an abstract `StructType`)
  /// - Every case type is a `StructType` that is tagged as the given type
  ///   (which must be an abstract `TagType`)
  [[nodiscard]] bool is_always_instance_of(Type *type) const {
    for (auto caseType : caseTypes)
      if (!(caseType->is_struct() &&
            static_cast<StructType *>(caseType)->is_instance_of(type)))
        return false;
    return true;
  }

public:
  /// The case types.
  llvm::SmallVector<Type *> caseTypes{};

  /// The required alignment in bytes.
  uint64_t requiredAlign{1};

  /// The required size in bytes.
  uint64_t requiredSize{1};

public:
  /// Canonicalize types. This is used by `Context` to form the union
  /// type key and remove redundant union type definitions.
  [[nodiscard]]
  static llvm::SmallVector<Type *>
  canonicalize_types(llvm::ArrayRef<Type *> types);
};

/// The void type.
class VoidType final : public TypeSubclass<TypeKind::Void> {
public:
  explicit VoidType(llvm::LLVMContext &context) {
    displayName = "void";
    llvmType = llvm::Type::getVoidTy(context);
  }

  [[nodiscard]] Value invoke(Emitter &emitter, const ArgumentList &args,
                             const SourceLocation &srcLoc) final;
};

/// \}

} // namespace smdl
