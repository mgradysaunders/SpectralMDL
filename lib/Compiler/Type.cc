// vim:foldmethod=marker:foldlevel=0:fmr=--{,--}
#include "Type.h"

#include "Context.h"
#include "Emitter.h"

#if WITH_PTEX
#include "Ptexture.h"
#endif // #if WITH_PTEX

namespace smdl::Compiler {

//--{ Type
void Type::init_name(const llvm::Twine &name) { this->name = context.get_persistent_string(name); }

Type *Type::get_with_different_scalar(Scalar newScalar) { return context.get_arithmetic_type(newScalar, extent); }

Type *Type::get_scalar_type() { return context.get_arithmetic_type(scalar); }

Type *Type::get_column_type() { return context.get_arithmetic_type(scalar, Extent(extent.numRows)); }

Type *Type::get_transpose_type() {
  if (extent.is_matrix())
    return context.get_arithmetic_type(scalar, extent.get_transpose());
  else
    return this;
}

Type *Type::get_element_type() {
  if (is_vector() || is_color())
    return get_scalar_type();
  if (is_matrix())
    return get_column_type();
  if (is_array())
    return static_cast<ArrayType *>(this)->elemType;
  if (is_pointer())
    return static_cast<PointerType *>(this)->elemType;
  return nullptr;
}

Type *Type::get_most_pointed_to_type() {
  auto type{this};
  while (type->is_pointer())
    type = type->get_element_type();
  return type;
}

StructType *Type::get_inline_struct_type() { return llvm::dyn_cast<StructType>(get_most_pointed_to_type()); }

UnionType *Type::get_visit_union_type() { return llvm::dyn_cast<UnionType>(get_most_pointed_to_type()); }

uint32_t Type::get_array_size() const {
  if (is_array())
    return static_cast<const ArrayType *>(this)->size;
  return 1;
}

uint32_t Type::get_pointer_depth() const {
  uint32_t depth{};
  auto type{const_cast<Type *>(this)};
  while (type->is_pointer()) {
    type = type->get_element_type();
    depth++;
  }
  return depth;
}

bool Type::is_optional() const { return is_union() && static_cast<const UnionType *>(this)->has_type(context.get_void_type()); }

bool Type::is_optional_unique() const { return is_optional() && static_cast<const UnionType *>(this)->types.size() == 2; }

bool Type::is_size_deferred_array() const { return is_array() && static_cast<const ArrayType *>(this)->size == 0; }

bool Type::is_compiler_function() const { return this == context.get_compiler_function_type(); }

bool Type::is_compiler_intrinsic() const { return this == context.get_compiler_intrinsic_type(); }

bool Type::is_compiler_module() const { return this == context.get_compiler_module_type(); }

bool Type::is_compiler_type() const { return this == context.get_compiler_type_type(); }

bool Type::is_string() const { return this == context.get_type<string_t>(); }

bool Type::is_source_location() const { return this == context.get_type<source_location_t>(); }

bool Type::is_texture_2d() const { return this == context.get_type<texture_2d_t>(); }

bool Type::is_texture_3d() const { return this == context.get_type<texture_3d_t>(); }

bool Type::is_texture_cube() const { return this == context.get_type<texture_cube_t>(); }

bool Type::is_texture_ptex() const { return this == context.get_type<texture_ptex_t>(); }

bool Type::is_texture() const { return is_texture_2d() || is_texture_3d() || is_texture_cube() || is_texture_ptex(); }

Value Type::insert(Emitter &emitter, Value value, Value elem, unsigned i, const AST::SourceLocation &srcLoc) {
  srcLoc.report_error(std::format("invalid insert into type '{}", name));
  return {};
}

Value Type::access(Emitter &emitter, Value value, llvm::StringRef key, const AST::SourceLocation &srcLoc) {
  srcLoc.report_error(std::format("invalid field '{}' in type '{}'", key, name));
  return {};
}

Value Type::access(Emitter &emitter, Value value, Value i, const AST::SourceLocation &srcLoc) {
  srcLoc.report_error(std::format("invalid index in type '{}'", name));
  return {};
}

Value Type::construct(Emitter &emitter, const ArgList &args, const AST::SourceLocation &srcLoc) {
  srcLoc.report_error(std::format("can't construct '{}' from arguments '{}'", name, std::string(args)));
  return {};
}
//--}

//--{ ArrayType
ArrayType::ArrayType(Context &context, Type *elemType, uint32_t size) : TypeSubclass(context), elemType(elemType), size(size) {
  sanity_check(size != 0);
  if (elemType->llvmType)
    llvmType = llvm::ArrayType::get(elemType->llvmType, size);
  init_name(std::format("({})[{}]", elemType->name, size));
}

ArrayType::ArrayType(Context &context, Type *elemType, const llvm::Twine &sizeName)
    : TypeSubclass(context), elemType(elemType), sizeName(context.get_persistent_string(sizeName)) {
  if (this->sizeName.empty())
    init_name(std::format("({})[]", elemType->name));
  else
    init_name(std::format("({})[<{}>]", elemType->name, this->sizeName));
}

Value ArrayType::insert(Emitter &emitter, Value value, Value elem, unsigned i, const AST::SourceLocation &srcLoc) {
  sanity_check(!is_abstract());
  sanity_check(i < size);
  elem = emitter.rvalue(elem);
  elem = emitter.construct(elemType, elem);
  return RValue(value.type, emitter.builder.CreateInsertValue(emitter.rvalue(value), elem, {i}));
}

Value ArrayType::access(Emitter &emitter, Value value, llvm::StringRef key, const AST::SourceLocation &srcLoc) {
  sanity_check(!is_abstract());
  if (key == "size")
    return context.get_compile_time_int(size);
  return Type::access(emitter, value, key, srcLoc); // Error
}

Value ArrayType::access(Emitter &emitter, Value value, Value i, const AST::SourceLocation &srcLoc) {
  sanity_check(!is_abstract());
  i = emitter.rvalue(i);
  if (i.is_compile_time_int() && value.is_rvalue()) {
    return RValue(elemType, emitter.builder.CreateExtractValue(value, {unsigned(i.get_compile_time_int())}));
  } else {
    if (!value.is_lvalue()) {
      auto lvalue{emitter.lvalue(value, /*manageLifetime=*/false)};
      auto result{emitter.rvalue(access(emitter, lvalue, i, srcLoc))};
      emitter.emit_end_lifetime(lvalue);
      return result;
    }
    return LValue(elemType, emitter.builder.CreateGEP(llvmType, value, {emitter.builder.getInt32(0), i.llvmValue}));
  }
}

Value ArrayType::construct(Emitter &emitter, const ArgList &args, const AST::SourceLocation &srcLoc) {
  if (size > 0) {
    if (args.empty() || args.is_one_positional_null()) {
      if (elemType->is_zero_by_default()) {
        return Value::zero(this);
      } else {
        // If the element type may not be trivially constructible, explicitly default construct the element type and
        // insert it into each element in the array.
        auto value{emitter.construct(elemType, args, srcLoc)};
        auto result{Value::zero(context.get_array_type(value.type, size))};
        for (uint32_t i{}; i < size; i++)
          result = emitter.insert(result, value, i);
        return result;
      }
    }
    if (args.is_one_positional()) {
      auto value{args[0].value};
      // If the argument is an array of equivalent size and possibly different type, construct the array
      // by converting each element of the other array.
      if (value.type->is_array() && value.type->get_array_size() == size) {
        if (is_abstract())
          return emitter.rvalue(value);
        auto result{Value::zero(this)};
        for (uint32_t i{}; i < size; i++)
          result = emitter.insert(result, emitter.construct(elemType, emitter.access(value, i), srcLoc), i);
        return result;
      }
      // If the argument is a pointer to the element type, construct the array by loading from the pointer.
      if (value.type->is_pointer() && value.type->get_element_type() == elemType)
        return RValue(
            this,
            emitter.builder.CreateAlignedLoad(llvmType, emitter.rvalue(value), llvm::Align(context.get_align_of(elemType))));
    }
    if (args.is_all_positional() && args.size() == size) {
      if (is_abstract()) {
        auto argElemType{context.get_common_type(args.get_types(), /*defaultToUnion=*/true, srcLoc)};
        if (!context.is_subset_of(argElemType, elemType))
          srcLoc.report_error(
              std::format("can't construct array type '{}' from inferred element type '{}'", name, argElemType->name));
        return emitter.construct(context.get_array_type(argElemType, size), args, srcLoc);
      }
      // If we can construct all elements directly from arguments, construct the array by converting each argument.
      auto result{Value::zero(this)};
      for (uint32_t i{}; i < size; i++)
        result = emitter.insert(result, emitter.construct(elemType, args[i].value, srcLoc), i);
      return result;
    }
  } else {
    if (args.empty() || args.is_one_positional_null())
      srcLoc.report_error(std::format("can't null or default construct type '{}'", name));
    if (args.is_one_positional() && args[0].value.type->is_array()) {
      if (!pattern_match(emitter, args[0].value.type))
        srcLoc.report_error(std::format("can't construct '{}' from '{}'", name, args[0].value.type->name));
      return emitter.rvalue(args[0].value);
    } else {
      auto argElemType{context.get_common_type(args.get_types(), /*defaultToUnion=*/false, srcLoc)};
      auto value{emitter.construct(context.get_array_type(argElemType, args.size()), args, srcLoc)};
      if (!pattern_match(emitter, value.type))
        srcLoc.report_error(std::format("can't construct '{}' from '{}'", name, value.type->name));
      return value;
    }
  }
  return Type::construct(emitter, args, srcLoc); // Error
}

bool ArrayType::pattern_match(Emitter &emitter, Type *type) {
  auto arrayType{llvm::dyn_cast<ArrayType>(type)};
  if (!arrayType || (size != 0 && size != arrayType->size))
    return false;
  if (!sizeName.empty() && arrayType->size != 0)
    emitter.push(context.get_compile_time_int(arrayType->size), sizeName, {}, {});
  return elemType->pattern_match(emitter, arrayType->elemType);
}
//--}

//--{ ArithmeticType
ArithmeticType::ArithmeticType(Context &context, Scalar scalar, Extent extent) : TypeSubclass(context, scalar, extent) {
  {
    // Initialize name
    const char *baseName{};
    switch (scalar) {
    case Scalar::Bool: baseName = "bool"; break;
    case Scalar::Byte: baseName = "byte"; break;
    case Scalar::Int: baseName = "int"; break;
    case Scalar::Float: baseName = "float"; break;
    case Scalar::Double: baseName = "double"; break;
    default: assert(false); break;
    }
    if (extent.numCols > 1) {
      assert(extent.numRows > 1);
      init_name(baseName + std::format("{}x{}", extent.numCols, extent.numRows));
    } else if (extent.numRows > 1) {
      init_name(baseName + std::format("{}", extent.numRows));
    } else {
      init_name(baseName);
    }
  }
  switch (scalar) {
  case Scalar::Bool: llvmType = llvm::Type::getInt1Ty(context.llvmContext); break;
  case Scalar::Byte: llvmType = llvm::Type::getInt8Ty(context.llvmContext); break;
  case Scalar::Int: llvmType = llvm::Type::getIntNTy(context.llvmContext, sizeof(int_t) * 8); break;
  case Scalar::Float: llvmType = llvm::Type::getFloatTy(context.llvmContext); break;
  case Scalar::Double: llvmType = llvm::Type::getDoubleTy(context.llvmContext); break;
  default: assert(false); break;
  }
  llvmType = extent.apply_to_llvm_type(llvmType);
}

Value ArithmeticType::insert(Emitter &emitter, Value value, Value elem, unsigned i, const AST::SourceLocation &srcLoc) {
  if (is_vector()) {
    elem = emitter.rvalue(elem);
    elem = emitter.construct(get_scalar_type(), elem);
    return RValue(this, emitter.builder.CreateInsertElement(emitter.rvalue(value), elem, i));
  }
  if (is_matrix()) {
    elem = emitter.rvalue(elem);
    elem = emitter.construct(get_column_type(), elem);
    return RValue(this, emitter.builder.CreateInsertValue(emitter.rvalue(value), elem, {i}));
  }
  return Type::insert(emitter, value, elem, i, srcLoc); // Error
}

Value ArithmeticType::access(Emitter &emitter, Value value, llvm::StringRef key, const AST::SourceLocation &srcLoc) {
  if (key.size() == 1 && (is_vector() || is_matrix())) {
    if (auto i{to_index(key[0])})
      return access(emitter, value, context.get_compile_time_int(*i), srcLoc);
  }
  if (is_vector()) {
    if (auto iMask{to_index_swizzle(key)})
      return RValue(
          context.get_arithmetic_type(scalar, Extent(iMask->size())),
          emitter.builder.CreateShuffleVector(emitter.rvalue(value), *iMask));
    if (key == "size")
      return context.get_compile_time_int(extent.numRows);
  }
  if (is_matrix()) {
    if (key == "num_rows")
      return context.get_compile_time_int(extent.numRows);
    if (key == "num_cols" || key == "size")
      return context.get_compile_time_int(extent.numCols);
  }
  return Type::access(emitter, value, key, srcLoc); // Error
}

Value ArithmeticType::access(Emitter &emitter, Value value, Value i, const AST::SourceLocation &srcLoc) {
  i = emitter.rvalue(i);
  if (i.is_compile_time_int() && value.is_rvalue()) {
    if (is_vector())
      return RValue(get_scalar_type(), emitter.builder.CreateExtractElement(value, unsigned(i.get_compile_time_int())));
    if (is_matrix())
      return RValue(get_column_type(), emitter.builder.CreateExtractValue(value, {unsigned(i.get_compile_time_int())}));
  } else {
    if (value.is_rvalue()) {
      auto lvalue{emitter.lvalue(value, /*manageLifetime=*/false)};
      auto result{emitter.rvalue(access(emitter, lvalue, i, srcLoc))};
      emitter.emit_end_lifetime(lvalue);
      return result;
    }
    if (is_vector()) {
      auto scalarType{get_scalar_type()};
      auto llvmArrayType{llvm::ArrayType::get(scalarType->llvmType, get_vector_size())};
      return LValue(scalarType, emitter.builder.CreateGEP(llvmArrayType, value, {emitter.builder.getInt32(0), i.llvmValue}));
    }
    if (is_matrix())
      return LValue(get_column_type(), emitter.builder.CreateGEP(llvmType, value, {emitter.builder.getInt32(0), i.llvmValue}));
  }
  return Type::access(emitter, value, i, srcLoc); // Error
}

Value ArithmeticType::construct(Emitter &emitter, const ArgList &args, const AST::SourceLocation &srcLoc) {
  if (args.empty() || args.is_one_positional_null())
    return Value::zero(this); // Zero
  if (extent.is_scalar()) {
    if (args.is_one_positional()) {
      auto value{args[0].value};
      // If constructing the 'bool' type with a pointer type, check that the pointer is not NULL.
      if (value.type->is_pointer() && is_boolean())
        return RValue(this, emitter.builder.CreateIsNotNull(emitter.rvalue(value)));
      // If constructing the 'bool' type with an optional union type, check that the union is not NULL.
      if (value.type->is_optional() && is_boolean())
        return RValue(
            this, emitter.builder.CreateICmpNE(
                      emitter.rvalue(emitter.access(value, "#idx")),
                      context.get_compile_time_int(static_cast<UnionType *>(value.type)->index_of(context.get_void_type()))));
      // If constructing any scalar type from another scalar or enum type, LLVM cast the underlying representation.
      if (value.type->is_scalar() || value.type->is_enum())
        return RValue(this, llvm_emit_cast(emitter.builder, emitter.rvalue(value), llvmType));
    }
  }
  if (extent.is_vector()) {
    if (args.is_one_positional()) {
      auto value{args[0].value};
      // If the argument is a scalar type, construct a vector splat of the scalar value.
      // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      // float3(2.7) // == float3(2.7, 2.7, 2.7)
      // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if (value.type->is_arithmetic() && value.type->is_scalar())
        return RValue(
            this, emitter.builder.CreateVectorSplat(get_vector_size(), emitter.construct(get_scalar_type(), value, srcLoc)));
      // If the argument is a vector type of equivalent size, construct a vector by casting the LLVM representation.
      // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      // int4(float4(3.0, 0.2, 0.1, 5.4)) // Cast components to int
      // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if (value.type->is_arithmetic() && value.type->extent == extent)
        return RValue(this, llvm_emit_cast(emitter.builder, emitter.rvalue(value), llvmType));
      // If the argument is a pointer to the scalar type, construct a vector by loading from the pointer.
      if (value.type->is_pointer() && value.type->get_element_type() == get_scalar_type())
        return RValue(
            this, emitter.builder.CreateAlignedLoad(
                      llvmType, emitter.rvalue(value), llvm::Align(emitter.context.get_align_of(get_scalar_type()))));
      // If the argument is a color and this is a float3, delegate to '::rgb::color_to_rgb()'
      // to convert the spectrum to an RGB triple.
      if (value.type->is_color() && this == context.get_float_type(Extent(3)))
        return emitter.emit_call(context.resolve(emitter, /*isAbs=*/true, {"rgb", "color_to_rgb"}, srcLoc), args, srcLoc);
    }
    const bool canConstructFromScalars{[&] {
      if (!(get_vector_size() == args.size() && args.is_all_true([](auto &arg) { return arg.value.type->is_scalar(); })))
        return false;
      return (get_vector_size() == 2 && args.has_only_these_names({"x", "y"})) ||
             (get_vector_size() == 3 && args.has_only_these_names({"x", "y", "z"})) ||
             (get_vector_size() == 4 && args.has_only_these_names({"x", "y", "z", "w"})) || args.is_all_positional();
    }()};
    const bool canConstructFromScalarsAndVectors{[&] {
      if (!args.is_all_true([](auto &arg) { return arg.value.type->is_scalar() || arg.value.type->is_vector(); }))
        return false;
      uint32_t size{};
      for (auto arg : args)
        size += arg.value.type->extent.numRows;
      return args.is_all_positional() && size == get_vector_size();
    }()};
    if (canConstructFromScalars) {
      auto vectorSize{get_vector_size()};
      auto scalarType{get_scalar_type()};
      auto values{llvm::SmallVector<Value>{}};
      if (2 <= vectorSize && vectorSize <= 4) {
        // If vector size is 2, 3, or 4, possibly resolve the argument names.
        // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        // float4(w: 3.0, x: 5.0, y: 7.0, z: 9.0)
        // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        auto params{ParamList{}};
        params.emplace_back(scalarType, "x");
        params.emplace_back(scalarType, "y");
        if (vectorSize >= 3)
          params.emplace_back(scalarType, "z");
        if (vectorSize >= 4)
          params.emplace_back(scalarType, "w");
        values = context.resolve_arguments(emitter, params, args, srcLoc);
      } else {
        // Otherwise just add the argument values in order. (We already verifed by now that they have no names!)
        for (auto &arg : args)
          values.push_back(arg.value);
      }
      auto result{Value::zero(this)};
      for (uint32_t i{}; i < vectorSize; i++)
        result = emitter.insert(result, values[i], i);
      return result;
    }
    if (canConstructFromScalarsAndVectors) {
      auto result{Value::zero(this)};
      for (uint32_t i{}; auto &arg : args) {
        // First convert to the scalar type to avoid redundant casting later. Then
        // - If the argument is a scalar, insert it into the vector.
        // - If the argument is a vector, extract each element and insert it into the vector.
        auto value{emitter.construct(arg.value.type->get_with_different_scalar(scalar), arg, srcLoc)};
        if (arg.value.type->is_scalar()) {
          result = emitter.insert(result, value, i++);
        } else {
          sanity_check(arg.value.type->is_vector());
          for (uint32_t j{}; j < arg.value.type->get_vector_size(); j++)
            result = emitter.insert(result, emitter.access(value, j), i++);
        }
      }
      return result;
    }
  }
  if (extent.is_matrix()) {
    auto scalarType{get_scalar_type()};
    auto columnType{get_column_type()};
    if (args.is_one_positional()) {
      auto value{args[0].value};
      // If the argument is a scalar, construct the matrix by assigning to the diagonal.
      if (value.type->is_arithmetic() && value.type->is_scalar()) {
        value = emitter.construct(scalarType, value, srcLoc);
        auto result{Value::zero(this)};
        for (unsigned j{}; j < std::min(extent.numCols, extent.numRows); j++)
          result = emitter.insert(result, emitter.insert(Value::zero(columnType), value, j), j);
        return result;
      }
      // If the argument is a matrix of equivalent type, construct the matrix by casting each column.
      if (value.type->is_arithmetic() && value.type->extent == extent) {
        auto result{Value::zero(this)};
        for (uint32_t j{}; j < extent.numCols; j++)
          result = emitter.insert(result, emitter.access(value, j), j);
        return result;
      }
    }
    const bool canConstructFromColumns{args.size() == extent.numCols && args.is_all_true([&](auto &arg) {
      return arg.is_positional() && arg.value.type->is_vector() && arg.value.type->get_vector_size() == get_vector_size();
    })};
    const bool canConstructFromScalars{args.size() == extent.numCols * extent.numRows && args.is_all_true([](auto &arg) {
      return arg.is_positional() && arg.value.type->is_scalar();
    })};
    if (canConstructFromColumns) {
      auto result{Value::zero(this)};
      for (uint32_t j{}; j < extent.numCols; j++)
        result = emitter.insert(result, args[j].value, j);
      return result;
    }
    if (canConstructFromScalars) {
      auto result{Value::zero(this)};
      for (uint32_t j{}; j < extent.numCols; j++) {
        // Insert 'numRows' scalars into a temporary column value.
        auto column{Value::zero(columnType)};
        for (uint32_t i{}; i < extent.numRows; i++)
          column = emitter.insert(column, args[j * extent.numRows + i].value, i);
        // Insert the temporary column value.
        result = emitter.insert(result, column, j);
      }
      return result;
    }
  }
  return Type::construct(emitter, args, srcLoc); // Error
}

bool ArithmeticType::can_access(llvm::StringRef key) {
  if (key.size() == 1)
    return to_index(key[0]).has_value();
  if (key.size() > 1)
    return to_index_swizzle(key).has_value();
  return false;
}
//--}

//--{ AutoType
Value AutoType::construct(Emitter &emitter, const ArgList &args, const AST::SourceLocation &srcLoc) {
  if (args.empty() || !args.is_all_positional()) {
    srcLoc.report_error("'auto' constructor requires 1 or more unnamed arguments");
  }
  if (args.is_one_positional()) {
    return emitter.rvalue(args[0].value);
  }
  if (args.is_all_true([&](auto arg) { return arg.value.type->is_scalar() || arg.value.type->is_vector(); })) {
    Scalar scalar{args[0].value.type->scalar};
    size_t size{};
    for (auto arg : args) {
      size += arg.value.type->get_vector_size();
      scalar = std::max(scalar, arg.value.type->scalar);
    }
    return emitter.construct(context.get_arithmetic_type(scalar, size), args, srcLoc);
  }
  return Type::construct(emitter, args, srcLoc); // Error
}
//--}

//--{ ColorType
ColorType::ColorType(Context &context) : TypeSubclass(context, Scalar::Float, Extent(context.mdl.wavelengthBaseMax)) {
  init_name("color");
  llvmType = llvm::Type::getFloatTy(context.llvmContext);
  llvmType = extent.apply_to_llvm_type(llvmType);
}

Value ColorType::access(Emitter &emitter, Value value, llvm::StringRef key, const AST::SourceLocation &srcLoc) {
  if (key == "size")
    return context.get_compile_time_int(extent.numRows);
  return Type::access(emitter, value, key, srcLoc); // Error
}

Value ColorType::access(Emitter &emitter, Value value, Value i, const AST::SourceLocation &srcLoc) {
  i = emitter.rvalue(i);
  if (i.is_compile_time_int() && value.is_rvalue()) {
    return RValue(get_scalar_type(), emitter.builder.CreateExtractElement(value, unsigned(i.get_compile_time_int())));
  } else {
    if (!value.is_lvalue()) {
      auto lvalue{emitter.lvalue(value, /*manageLifetime=*/false)};
      auto result{emitter.rvalue(access(emitter, lvalue, i, srcLoc))};
      emitter.emit_end_lifetime(lvalue);
      return result;
    }
    auto llvmArrayType{llvm::ArrayType::get(get_scalar_type()->llvmType, get_vector_size())};
    return LValue(
        get_scalar_type(), emitter.builder.CreateGEP(llvmArrayType, value, {emitter.builder.getInt32(0), i.llvmValue}));
  }
}

Value ColorType::construct(Emitter &emitter, const ArgList &args, const AST::SourceLocation &srcLoc) {
  if (args.empty() || args.is_one_positional_null())
    return Value::zero(this);
  if (args.is_one_positional()) {
    auto value{args[0].value};
    // If the argument is a scalar, construct the color by splatting the scalar.
    if (value.type->is_arithmetic() && value.type->is_scalar())
      return RValue(
          this,
          emitter.builder.CreateVectorSplat(get_vector_size(), emitter.construct(context.get_float_type(), value, srcLoc)));
    // If the argument is a float pointer, construct the color by loading from the pointer.
    if (value.type->is_pointer() && value.type->get_element_type() == context.get_float_type())
      return RValue(this, emitter.builder.CreateAlignedLoad(llvmType, emitter.rvalue(value), llvm::Align(alignof(float_t))));
  }
  // If 1 'float3'-ish argument or 3 arguments, delegate to '::rgb::rgb_to_color' to construct the color
  // spectrum from the RGB components.
  if ((args.size() == 1 && args[0].value.type->is_vector() && args[0].value.type->extent == Extent(3)) || (args.size() == 3))
    return emitter.emit_call(context.resolve(emitter, /*isAbs=*/true, {"rgb", "rgb_to_color"}, srcLoc), args, srcLoc);
  return Type::construct(emitter, args, srcLoc); // Error
}
//--}

//--{ CompileTimeUnionType
CompileTimeUnionType::CompileTimeUnionType(Context &context, UnionType *unionType)
    : TypeSubclass(context), unionType(unionType) {
  sanity_check_nonnull(unionType);
  sanity_check(!unionType->is_abstract());
  init_name("$" + unionType->name);
}

Value CompileTimeUnionType::construct(Emitter &emitter, const ArgList &args, const AST::SourceLocation &srcLoc) {
  if (args.empty())
    srcLoc.report_error(std::format("can't default construct compile-time union type '{}'", name));
  if (args.is_one_positional() && unionType->has_type(args[0].value.type))
    return emitter.rvalue(args[0].value);
  return Type::construct(emitter, args, srcLoc);
}

bool CompileTimeUnionType::pattern_match(Emitter &emitter, Type *type) { return this == type || unionType->has_type(type); }
//--}

//--{ CompilerType
CompilerType::CompilerType(Context &context, const llvm::Twine &name) : TypeSubclass(context) {
  init_name(name);
  llvmType = llvm::Type::getIntNTy(context.llvmContext, sizeof(void *) * 8);
}
//--}

//--{ EnumType
EnumType::EnumType(Context &context, builtin_enum_type_t<intensity_mode_t>) : TypeSubclass(context, Scalar::Int) {
  init_name("intensity_mode_t");
  constants.push_back(Constant{
      "intensity_radiant_exitance",
      static_cast<llvm::ConstantInt *>(context.get_compile_time_int(int_t(intensity_mode_t::intensity_radiant_exitance)))});
  constants.push_back(Constant{
      "intensity_power",
      static_cast<llvm::ConstantInt *>(context.get_compile_time_int(int_t(intensity_mode_t::intensity_power)))});
  llvmType = context.get_int_type()->llvmType;
}

EnumType::EnumType(Context &context, AST::Enum *decl, llvm::Function *llvmFunc)
    : TypeSubclass(context, Scalar::Int), decl(decl), llvmFunc(llvmFunc) {
  init_name(decl->name);
  for (auto &declarator : decl->declarators)
    constants.push_back(Constant{declarator.name.srcName, sanity_check_nonnull(declarator.llvmConst), declarator.name.srcLoc});
  llvmType = context.get_int_type()->llvmType;
}

Value EnumType::construct(Emitter &emitter, const ArgList &args, const AST::SourceLocation &srcLoc) {
  if (args.empty())
    return constants.empty() ? Value::zero(this) : RValue(this, constants[0].llvmConst);
  if (args.is_one_positional_null())
    return Value::zero(this);
  if (args.is_one_positional()) {
    auto value{args[0].value};
    if (value.type->is_enum() || value.type == context.get_int_type())
      return RValue(this, emitter.rvalue(value).llvmValue);
  }
  return Type::construct(emitter, args, srcLoc); // Error
}
//--}

//--{ FunctionType
FunctionType::FunctionType(Context &context, bool isPure, Type *returnType, llvm::SmallVector<Type *> paramTypes)
    : TypeSubclass(context), isPure(isPure), returnType(returnType), paramTypes(std::move(paramTypes)) {
  auto paramTypeNames{llvm::SmallVector<llvm::StringRef>{}};
  for (auto &paramType : this->paramTypes)
    paramTypeNames.push_back(paramType->name);
  init_name(std::format("{}({})", returnType->name, format_join(paramTypeNames, ", ")));
  if (!has_abstract_parameter_types()) {
    auto llvmReturnType{returnType->llvmType};
    if (!llvmReturnType)
      llvmReturnType = context.get_void_type()->llvmType;
    auto llvmParamTypes{llvm::SmallVector<llvm::Type *>{}};
    if (!isPure)
      llvmParamTypes.push_back(llvm::PointerType::get(context.llvmContext, 0)); // Pointer to state
    for (auto paramType : this->paramTypes)
      llvmParamTypes.push_back(paramType->llvmType);
    llvmType = llvm::FunctionType::get(llvmReturnType, llvmParamTypes, /*isVarArg=*/false);
  }
}

llvm::Function *FunctionType::create_default_llvm_function(const llvm::Twine &twine) const {
  return llvm::Function::Create(
      static_cast<llvm::FunctionType *>(sanity_check_nonnull(llvmType)), llvm::Function::InternalLinkage, twine,
      context.llvmModule);
}
//--}

//--{ PointerType
PointerType::PointerType(Context &context, Type *elemType) : TypeSubclass(context), elemType(elemType) {
  init_name(llvm_twine("&", elemType->name));
  if (elemType->llvmType)
    llvmType = llvm::PointerType::get(context.llvmContext, 0);
  else if (llvm::isa<PointerType>(elemType))
    throw Error("multi-level pointer to abstract type is not supported");
}

Value PointerType::access(Emitter &emitter, Value value, llvm::StringRef key, const AST::SourceLocation &srcLoc) {
  sanity_check(!is_abstract());
  return elemType->access(emitter, LValue(elemType, emitter.rvalue(value)), key, srcLoc);
}

Value PointerType::access(Emitter &emitter, Value value, Value i, const AST::SourceLocation &srcLoc) {
  sanity_check(!is_abstract());
  return LValue(elemType, emitter.builder.CreateGEP(elemType->llvmType, emitter.rvalue(value), {emitter.rvalue(i).llvmValue}));
}

Value PointerType::construct(Emitter &emitter, const ArgList &args, const AST::SourceLocation &srcLoc) {
  if (args.empty() || args.is_one_positional_null()) {
    if (is_abstract())
      srcLoc.report_error(std::format("can't null or default construct abstract type '{}'", name));
    return Value::zero(this);
  }
  if (args.is_one_positional()) {
    auto value{args[0].value};
    if (!value.type->is_pointer()) {
      if (value.type->is_string() && elemType == context.get_byte_type())
        return emitter.rvalue(emitter.access(value, "ptr", srcLoc));
      srcLoc.report_error(std::format("can't construct pointer type '{}' from non-pointer type '{}'", name, value.type->name));
    }
    if (is_abstract()) {
      if (elemType->pattern_match(emitter, static_cast<PointerType *>(value.type)->elemType))
        return emitter.rvalue(value);
    } else {
      return RValue(this, value);
    }
  }
  return Type::construct(emitter, args, srcLoc); // Error
}

bool PointerType::pattern_match(Emitter &emitter, Type *type) {
  return this == type || (type->is_pointer() && elemType->pattern_match(emitter, static_cast<PointerType *>(type)->elemType));
}

bool PointerType::can_access(llvm::StringRef key) { return elemType->can_access(key); }
//--}

//--{ StructType
StructType::StructType(Context &context, AST::Struct *decl, llvm::Function *llvmFunc)
    : TypeSubclass(context), decl(decl), llvmFunc(llvmFunc) {
  init_name(decl->name);
  fields = ParamList(context, *decl);
  for (const auto &tag : decl->tags) {
    tags.push_back(sanity_check_nonnull(llvm::dyn_cast<TagType>(sanity_check_nonnull(tag.type->type))));
    if (tag.isDefault)
      tags.back()->declare_default_type(this, decl->srcLoc);
  }
  init_llvm_type();
}

StructType::StructType(Context &context, builtin_struct_type_t<default_bsdf_t>) : TypeSubclass(context) {
  init_name("default_bsdf");
  tags.push_back(sanity_check_nonnull(context.get_bsdf_type()));
  tags.back()->declare_default_type(this, {});
  init_llvm_type();
}

StructType::StructType(Context &context, builtin_struct_type_t<default_edf_t>) : TypeSubclass(context) {
  init_name("default_edf");
  tags.push_back(sanity_check_nonnull(context.get_edf_type()));
  tags.back()->declare_default_type(this, {});
  init_llvm_type();
}

StructType::StructType(Context &context, builtin_struct_type_t<default_vdf_t>) : TypeSubclass(context) {
  init_name("default_vdf");
  tags.push_back(sanity_check_nonnull(context.get_vdf_type()));
  tags.back()->declare_default_type(this, {});
  init_llvm_type();
}

StructType::StructType(Context &context, builtin_struct_type_t<default_hair_bsdf_t>) : TypeSubclass(context) {
  init_name("default_hair_bsdf");
  tags.push_back(sanity_check_nonnull(context.get_hair_bsdf_type()));
  tags.back()->declare_default_type(this, {});
  init_llvm_type();
}

static void init_builtin_field(auto &context, auto &fields, Type *type, const char *name, const char *init) {
  fields.emplace_back(type, name, context.parse_expression(init));
}

StructType::StructType(Context &context, builtin_struct_type_t<material_emission_t>) : TypeSubclass(context) {
  init_name("material_emission");
  init_builtin_field(context, fields, context.get_edf_type(), "emission", "edf()");
  init_builtin_field(context, fields, context.get_color_type(), "intensity", "color()");
  init_builtin_field(context, fields, context.get_intensity_mode_type(), "mode", "intensity_radiant_exitance");
}

StructType::StructType(Context &context, builtin_struct_type_t<material_surface_t>) : TypeSubclass(context) {
  init_name("material_surface");
  init_builtin_field(context, fields, context.get_bsdf_type(), "scattering", "bsdf()");
  init_builtin_field(context, fields, context.get_material_emission_type(), "emission", "material_emission()");
}

StructType::StructType(Context &context, builtin_struct_type_t<material_volume_t>) : TypeSubclass(context) {
  init_name("material_volume");
  init_builtin_field(context, fields, context.get_vdf_type(), "scattering", "vdf()");
  init_builtin_field(context, fields, context.get_color_type(), "absorption_coefficient", "color()"); 
  init_builtin_field(context, fields, context.get_color_type(), "scattering_coefficient", "color()"); 
}

StructType::StructType(Context &context, builtin_struct_type_t<material_geometry_t>) : TypeSubclass(context) {
  init_name("material_geometry");
  init_builtin_field(context, fields, context.get_float_type(Extent(3)), "displacement", "float3()");
  init_builtin_field(context, fields, context.get_float_type(), "cutout_opacity", "1.0");
  init_builtin_field(context, fields, context.get_float_type(Extent(3)), "normal", "$state.normal");
  init_llvm_type(); // This is actually concrete!
}

StructType::StructType(Context &context, builtin_struct_type_t<material_t>) : TypeSubclass(context) {
  init_name("material");
  init_builtin_field(context, fields, context.get_bool_type(), "thin_walled", "false");
  init_builtin_field(context, fields, context.get_material_surface_type(), "surface", "material_surface()");
  init_builtin_field(context, fields, context.get_material_surface_type(), "backface", "material_surface()");
  init_builtin_field(context, fields, context.get_color_type(), "ior", "color(1.0)"); 
  init_builtin_field(context, fields, context.get_material_volume_type(), "volume", "material_volume()");
  init_builtin_field(context, fields, context.get_material_geometry_type(), "geometry", "material_geometry()");
  init_builtin_field(context, fields, context.get_hair_bsdf_type(), "hair", "hair_bsdf()");
}

template <typename S, typename T> static void init_builtin_field(auto &context, auto &fields, const char *name, T S:: *) {
  fields.emplace_back(context.template get_type<T>(), name);
}

StructType::StructType(Context &context, builtin_struct_type_t<light_profile_t>) : TypeSubclass(context) {
  init_name("light_profile");
  init_builtin_field(context, fields, "ptr", &light_profile_t::ptr);
  init_builtin_field(context, fields, "power", &light_profile_t::power);
  init_builtin_field(context, fields, "maximum", &light_profile_t::maximum);
  init_llvm_type();
}

StructType::StructType(Context &context, builtin_struct_type_t<tile_2d_t>) : TypeSubclass(context) {
  init_name("tile_2d");
  init_builtin_field(context, fields, "extent", &tile_2d_t::extent);
  init_builtin_field(context, fields, "texels", &tile_2d_t::texels);
  init_llvm_type();
}

StructType::StructType(Context &context, builtin_struct_type_t<texture_2d_t>) : TypeSubclass(context) {
  init_name("texture_2d");
  init_builtin_field(context, fields, "gamma", &texture_2d_t::gamma);
  init_builtin_field(context, fields, "tile_count", &texture_2d_t::tile_count);
  init_builtin_field(context, fields, "tiles", &texture_2d_t::tiles);
  init_llvm_type();
}

StructType::StructType(Context &context, builtin_struct_type_t<texture_3d_t>) : TypeSubclass(context) {
  init_name("texture_3d");
  init_builtin_field(context, fields, "gamma", &texture_3d_t::gamma);
  init_builtin_field(context, fields, "extent", &texture_3d_t::extent);
  init_builtin_field(context, fields, "stride", &texture_3d_t::stride);
  init_builtin_field(context, fields, "texels", &texture_3d_t::texels);
  init_llvm_type();
}

StructType::StructType(Context &context, builtin_struct_type_t<texture_cube_t>) : TypeSubclass(context) {
  init_name("texture_cube");
  init_builtin_field(context, fields, "gamma", &texture_cube_t::gamma);
  init_builtin_field(context, fields, "extent", &texture_cube_t::extent);
  init_builtin_field(context, fields, "texels", &texture_cube_t::texels);
  init_llvm_type();
}

StructType::StructType(Context &context, builtin_struct_type_t<texture_ptex_t>) : TypeSubclass(context) {
  init_name("texture_ptex");
  init_builtin_field(context, fields, "gamma", &texture_ptex_t::gamma);
  init_builtin_field(context, fields, "ptr", &texture_ptex_t::ptr);
  init_llvm_type();
}

StructType::StructType(Context &context, builtin_struct_type_t<state_t>) : TypeSubclass(context) {
  init_name("state_t");
  init_builtin_field(context, fields, "position", &state_t::position);
  init_builtin_field(context, fields, "normal", &state_t::normal);
  init_builtin_field(context, fields, "geometry_normal", &state_t::geometry_normal);
  init_builtin_field(context, fields, "motion", &state_t::motion);
  init_builtin_field(context, fields, "texture_space_max", &state_t::texture_space_max);
  init_builtin_field(context, fields, "texture_coordinate", &state_t::texture_coordinate);
  init_builtin_field(context, fields, "texture_tangent_u", &state_t::texture_tangent_u);
  init_builtin_field(context, fields, "texture_tangent_v", &state_t::texture_tangent_v);
  init_builtin_field(context, fields, "geometry_tangent_u", &state_t::geometry_tangent_u);
  init_builtin_field(context, fields, "geometry_tangent_v", &state_t::geometry_tangent_v);
  init_builtin_field(context, fields, "object_id", &state_t::object_id);
  init_builtin_field(context, fields, "ptex_face_id", &state_t::ptex_face_id);
  init_builtin_field(context, fields, "ptex_face_uv", &state_t::ptex_face_uv);
  init_builtin_field(context, fields, "direction", &state_t::direction);
  init_builtin_field(context, fields, "animation_time", &state_t::animation_time);
  init_builtin_field(context, fields, "wavelength_base", &state_t::wavelength_base);
  init_builtin_field(context, fields, "wavelength_min", &state_t::wavelength_min);
  init_builtin_field(context, fields, "wavelength_max", &state_t::wavelength_max);
  init_builtin_field(context, fields, "meters_per_scene_unit", &state_t::meters_per_scene_unit);
  init_builtin_field(context, fields, "object_to_world_matrix_fwd", &state_t::object_to_world_matrix_fwd);
  init_builtin_field(context, fields, "object_to_world_matrix_inv", &state_t::object_to_world_matrix_inv);
  init_builtin_field(context, fields, "internal_to_object_matrix_fwd", &state_t::internal_to_object_matrix_fwd);
  init_builtin_field(context, fields, "internal_to_object_matrix_inv", &state_t::internal_to_object_matrix_inv);
  init_builtin_field(context, fields, "internal_to_world_matrix_fwd", &state_t::internal_to_world_matrix_fwd);
  init_builtin_field(context, fields, "internal_to_world_matrix_inv", &state_t::internal_to_world_matrix_inv);
  init_llvm_type();
}

StructType::StructType(Context &context, builtin_struct_type_t<string_t>) : TypeSubclass(context) {
  init_name("string");
  init_builtin_field(context, fields, "ptr", &string_t::ptr);
  init_builtin_field(context, fields, "len", &string_t::len);
  init_llvm_type();
}

StructType::StructType(Context &context, builtin_struct_type_t<source_location_t>) : TypeSubclass(context) {
  init_name("source_location");
  init_builtin_field(context, fields, "file", &source_location_t::file);
  init_builtin_field(context, fields, "line", &source_location_t::line);
  init_llvm_type();
}

StructType::StructType(Context &context, StructType *parentTemplate, llvm::ArrayRef<Type *> missingTypes)
    : TypeSubclass(context), parentTemplate(parentTemplate), decl(parentTemplate->decl), llvmFunc(parentTemplate->llvmFunc),
      fields(parentTemplate->fields), tags(parentTemplate->tags) {
  auto missingTypeItr{missingTypes.begin()};
  for (auto &field : fields) {
    if (field.type->is_abstract()) {
      sanity_check(missingTypeItr != missingTypes.end());
      sanity_check(!sanity_check_nonnull(*missingTypeItr)->is_abstract());
      field.type = *missingTypeItr++;
      if (field.type->is_void())
        field.isVoid = true;
    }
  }
  sanity_check(missingTypeItr == missingTypes.end());
  name = parentTemplate->name;
  init_llvm_type();
}

Value StructType::insert(Emitter &emitter, Value value, Value elem, unsigned i, const AST::SourceLocation &srcLoc) {
  sanity_check(llvmType != nullptr, "'StructType::insert()' called on struct with abstract fields");
  sanity_check(i < fields.size());
  if (fields[i].isVoid)
    return value;
  elem = emitter.rvalue(elem);
  elem = emitter.construct(fields[i].type, elem);
  unsigned iActual{};
  for (unsigned j{}; j < i; j++)
    if (!fields[j].isVoid)
      iActual++;
  return RValue(this, emitter.builder.CreateInsertValue(emitter.rvalue(value), elem, {iActual}));
}

Value StructType::access(Emitter &emitter, Value value, llvm::StringRef key, const AST::SourceLocation &srcLoc) {
  sanity_check(llvmType != nullptr, "'StructType::access()' called on struct with abstract fields");
  if (auto path{ParamList::InlinePath{}}; fields.get_inline_path(key, path)) {
    bool isConst{};
    for (auto [field, i] : path) {
      if (field->isVoid)
        return Value(Value::Kind::RValue, context.get_void_type(), nullptr);
      isConst |= field->isConst;
      value = value.is_lvalue() ? LValue(field->type, emitter.builder.CreateStructGEP(value.type->llvmType, value, i))
                                : RValue(field->type, emitter.builder.CreateExtractValue(value, {i}));
    }
    return isConst ? emitter.rvalue(value) : value;
  }
  return Type::access(emitter, value, key, srcLoc); // Error
}

Value StructType::construct(Emitter &emitter, const ArgList &args, const AST::SourceLocation &srcLoc) {
  if (args.is_one_positional_null()) {
    if (is_abstract())
      srcLoc.report_error(std::format("can't null construct abstract type '{}'", name));
    return Value::zero(this);
  }
  if (args.is_one_positional()) {
    auto value{args[0].value};
    if (value.type->is_struct() && static_cast<StructType *>(value.type)->parentTemplate == this) {
      sanity_check(pattern_match(emitter, value.type));
      return emitter.rvalue(value);
    }
    if (is_abstract() && value.type->is_union() && static_cast<UnionType *>(value.type)->always_has_parent_template(this)) {
      return emitter.rvalue(value);
    }
  }
  if (is_string()) {
    if (args.empty())
      return Value::zero(this);
    if (args.is_one_positional()) {
      if (args[0].value.type->is_enum()) {
        auto enumType{static_cast<EnumType *>(args[0].value.type)};
        if (!enumType->enumToString) {
          enumType->enumToString = context.bump_allocate<FunctionInstance>();
          enumType->enumToString->initialize(emitter, enumType);
        }
        return enumType->enumToString->call(emitter, {emitter.rvalue(args[0].value)}, srcLoc);
      }
    }
  } else if (is_source_location()) {
    if (args.empty())
      return context.get_compile_time_source_location(srcLoc);
  } else if (is_texture()) {
    auto values{llvm::SmallVector<Value>{}};
    if (args.empty()) {
      return Value::zero(this);
    } else if (args.size() == 1) {
      try {
        auto params{ParamList{}};
        params.emplace_back(context.get_string_type(), "name");
        values = context.resolve_arguments(emitter, params, args, srcLoc);
        values.push_back(context.get_compile_time_int(0));
      } catch (...) {
        return Type::construct(emitter, args, srcLoc); // Error
      }
    } else if (args.size() == 2) {
      try {
        auto params{ParamList{}};
        params.emplace_back(context.get_string_type(), "name");
        params.emplace_back(context.get_auto_type(), "gamma");
        values = context.resolve_arguments(emitter, params, args, srcLoc);
      } catch (...) {
        return Type::construct(emitter, args, srcLoc); // Error
      }
    } else {
      return Type::construct(emitter, args, srcLoc); // Error
    }
    auto value0{emitter.rvalue(values[0])};
    if (!value0.is_compile_time_string())
      srcLoc.report_error(std::format("expected '{}' parameter 'name' to resolve to compile-time string", name));
    auto fname{value0.get_compile_time_string()};
    if (is_texture_2d()) {
      auto imagePaths{context.mdl.fileLocator.locate_images(fname.str(), emitter.module->filename)};
      auto images{llvm::SmallVector<tile_2d_t>{}};
      uint32_t numTilesU{};
      uint32_t numTilesV{};
      for (auto &imagePath : imagePaths) {
        numTilesU = std::max(numTilesU, imagePath.iU + 1);
        numTilesV = std::max(numTilesV, imagePath.iV + 1);
        auto [itr, inserted] = context.mdl.images.try_emplace(imagePath.path.string(), Image());
        if (inserted) {
          try {
            itr->second = load_image(imagePath.path);
            itr->second.flip_vertical();
          } catch (const Error &error) {
            error.print();
          }
        }
        auto &image{images.emplace_back()};
        image.extent = itr->second.extent;
        image.texels = itr->second.texels.data();
      }
      if (images.empty()) {
        srcLoc.report_warning(std::format("can't find any image(s) for '{}'", fname));
        return Value::zero(this);
      }
      auto constImages{Value::zero(context.get_array_type(context.get_type<tile_2d_t>(), numTilesU * numTilesV))};
      auto globalImages{new llvm::GlobalVariable(
          context.llvmModule, constImages.type->llvmType, true, llvm::GlobalValue::PrivateLinkage, nullptr)};
      globalImages->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
      globalImages->setAlignment(llvm::Align(16));
      for (uint32_t iV{}; iV < numTilesV; iV++) {
        for (uint32_t iU{}; iU < numTilesU; iU++) {
          auto image{[&]() {
            for (uint32_t i{}; i < images.size(); i++)
              if (imagePaths[i].iU == iU && imagePaths[i].iV == iV)
                return images[i];
            return tile_2d_t{};
          }()};
          auto constImage{Value::zero(context.get_type<tile_2d_t>())};
          constImage = emitter.insert(constImage, context.get_compile_time_int2(image.extent), 0);
          constImage = emitter.insert(constImage, context.get_compile_time_pointer(image.texels), 1);
          constImages = emitter.insert(constImages, constImage, iV * numTilesU + iU);
        }
      }
      sanity_check(constImages.is_compile_time());
      globalImages->setInitializer(static_cast<llvm::Constant *>(constImages.llvmValue));
      auto globalImagesPtr{RValue(
          context.get_type<tile_2d_t *>(),
          emitter.builder.CreateConstInBoundsGEP2_32(constImages.type->llvmType, globalImages, 0, 0))};
      auto constTex{Value::zero(this)};
      constTex = emitter.insert(constTex, values[1], 0); // gamma
      constTex = emitter.insert(constTex, context.get_compile_time_int2(int2_t{int_t(numTilesU), int_t(numTilesV)}), 1);
      constTex = emitter.insert(constTex, globalImagesPtr, 2);
      return constTex;
    } else if (is_texture_3d()) {
      // TODO
      srcLoc.report_warning("'texture_3d' not supported yet");
      return Value::zero(this);
    } else if (is_texture_cube()) {
      // TODO
      srcLoc.report_warning("'texture_cube' not supported yet");
      return Value::zero(this);
    } else if (is_texture_ptex()) {
#if WITH_PTEX
      auto path{context.mdl.fileLocator.locate(fname.str(), emitter.module->filename)};
      if (!path) {
        srcLoc.report_warning(std::format("can't load 'texture_ptex' from '{}': file not found\n", fname));
      } else {
        auto pathStr{path->string()};
        auto [itr, inserted] = context.mdl.ptextures.try_emplace(pathStr, nullptr);
        auto &ptex{itr->second};
        if (inserted) {
          Ptex::String errorStr{};
          ptex.texture = PtexTexture::open(pathStr.c_str(), errorStr, /*premultiply=*/false);
          if (!ptex.texture) {
            srcLoc.report_warning(std::format("can't load 'texture_ptex' from '{}': {}\n", fname, errorStr.c_str()));
          } else {
            ptex.filter = PtexFilter::getFilter(
                static_cast<PtexTexture *>(ptex.texture), //
                PtexFilter::Options(PtexFilter::f_bilinear));
          }
        }
        auto constTex{Value::zero(this)};
        constTex = emitter.insert(Value::zero(this), values[1], 0); // gamma
        constTex = emitter.insert(Value::zero(this), context.get_compile_time_pointer(&ptex), 1);
        return constTex;
      }
#else
      srcLoc.report_warning(std::format("can't load 'texture_ptex' from '{}': built without Ptex support", fname));
#endif // #if WITH_PTEX
      return Value::zero(this);
    }
  } else {
    if (context.can_resolve_arguments(emitter, fields, args, srcLoc)) {
      auto values{context.resolve_arguments(emitter, fields, args, srcLoc)};
      auto resultType{this};
      if (resultType->is_abstract()) {
        auto missingTypes{llvm::SmallVector<Type *>{}};
        for (unsigned i{}; i < values.size(); i++)
          if (fields[i].type->is_abstract())
            missingTypes.push_back(values[i].type);
        resultType = context.get_struct_type(this, missingTypes);
        sanity_check(!resultType->is_abstract());
      }
      auto result{Value::zero(resultType)};
      for (unsigned i{}; auto &value : values)
        result = emitter.insert(result, value, i++, srcLoc);
      return result;
    }
  }
  return Type::construct(emitter, args, srcLoc); // Error
}

bool StructType::pattern_match(Emitter &emitter, Type *type) {
  if (this == type || (type->is_struct() && static_cast<StructType *>(type)->parentTemplate == this)) {
    for (size_t i = 0; i < fields.size(); i++)
      sanity_check(fields[i].type->pattern_match(emitter, static_cast<StructType *>(type)->fields[i].type));
    return true;
  }
  if (is_abstract() && type->is_union())
    return static_cast<UnionType *>(type)->always_has_parent_template(this);
  return false;
}

bool StructType::can_access(llvm::StringRef key) {
  ParamList::InlinePath path{};
  return fields.get_inline_path(key, path);
}

void StructType::init_llvm_type() {
  if (!fields.has_any_abstract()) {
    llvmType = llvm::StructType::create(context.llvmContext, fields.get_llvm_types(), name);
  }
}
//--}

//--{ TagType
Value TagType::construct(Emitter &emitter, const ArgList &args, const AST::SourceLocation &srcLoc) {
  if (args.empty() || args.is_one_positional_null()) {
    if (!defaultType)
      srcLoc.report_error(std::format("can't default construct tag type '{}'", name));
    return emitter.construct(defaultType, args, srcLoc);
  }
  if (args.is_one_positional()) {
    auto value{args[0].value};
    if (auto structType{llvm::dyn_cast<StructType>(value.type)}) {
      if (!structType->has_tag(this))
        srcLoc.report_error(std::format("struct type '{}' incompatible with tag type '{}'", structType->name, name));
      return emitter.rvalue(value);
    }
    if (auto unionType{llvm::dyn_cast<UnionType>(value.type)}) {
      if (!unionType->always_has_tag(this))
        srcLoc.report_error(std::format("union type '{}' incompatible with tag type '{}'", unionType->name, name));
      return emitter.rvalue(value);
    }
  }
  return Type::construct(emitter, args, srcLoc); // Error
}

bool TagType::pattern_match(Emitter &emitter, Type *type) {
  return this == type || (type->is_struct() && static_cast<StructType *>(type)->has_tag(this)) ||
         (type->is_union() && static_cast<UnionType *>(type)->always_has_tag(this));
}

void TagType::declare_default_type(Type *type, const AST::SourceLocation &srcLoc) {
  if (defaultType)
    srcLoc.report_error(std::format("tag type '{}' already has default", name));
  defaultType = type;
}
//--}

//--{ UnionType
UnionType::UnionType(Context &context, llvm::SmallVector<Type *> types) : TypeSubclass(context), types(std::move(types)) {
  assert(this->types.size() > 1);
  auto typeNames{llvm::SmallVector<llvm::StringRef>{}};
  bool hasVoid{};
  for (auto type : this->types) {
    sanity_check(type != nullptr);
    sanity_check(!type->is_abstract());
    requiredAlign = std::max(requiredAlign, uint64_t(context.get_align_of(type)));
    requiredSize = std::max(requiredSize, uint64_t(context.get_size_of(type)));
    if (type->is_void())
      hasVoid = true;
    else
      typeNames.push_back(type->name);
  }
  std::sort(typeNames.begin(), typeNames.end());
  init_name(std::format("{}({})", (hasVoid ? "?" : ""), format_join(typeNames, " | ")));
  uint64_t wordCount{requiredSize / 8};
  if (requiredSize % 8 != 0)
    wordCount++;
  llvmType = llvm::StructType::create(
      {llvm::FixedVectorType::get(llvm::Type::getInt64Ty(context.llvmContext), wordCount), context.get_int_type()->llvmType},
      context.get_unique_name("union"));
  sanity_check(requiredAlign <= uint64_t(context.get_align_of(this)));
}

Value UnionType::access(Emitter &emitter, Value value, llvm::StringRef key, const AST::SourceLocation &srcLoc) {
  if (key == "#ptr")
    return RValue(context.get_type<void *>(), emitter.builder.CreateStructGEP(llvmType, emitter.lvalue(value), 0));
  if (key == "#idx")
    return value.is_lvalue() ? LValue(context.get_int_type(), emitter.builder.CreateStructGEP(llvmType, value, 1))
                             : RValue(context.get_int_type(), emitter.builder.CreateExtractValue(value, {1U}));
  if (is_optional_unique() || can_access(key)) {
    if (value.is_rvalue()) {
      auto lvalue{emitter.lvalue(value, /*manageLifetime=*/false)};
      auto result{emitter.rvalue(access(emitter, lvalue, key, srcLoc))};
      emitter.emit_end_lifetime(lvalue);
      return result;
    }
    // Access through unique optionals unsafely without switching. This mimics the semantics of the dot operator on pointers.
    if (is_optional_unique()) {
      // The void type is guaranteed to be at the end, so we know the non-void type is 'types[0]'.
      return emitter.access(LValue(types[0], emitter.builder.CreateStructGEP(llvmType, value, 0)), key, srcLoc);
    } else {
      return emitter.emit_visit(
          value, [&](Emitter &emitter, Value value) -> Value { return emitter.access(value, key, srcLoc); });
    }
  }
  return Type::access(emitter, value, key, srcLoc); // Error
}

Value UnionType::construct(Emitter &emitter, const ArgList &args, const AST::SourceLocation &srcLoc) {
  if (args.empty() || args.is_one_positional_null()) {
    if (!is_optional())
      srcLoc.report_error(std::format("can't default construct non-optional union type '{}'", name));
    auto result{Value::zero(this)};
    result.llvmValue = emitter.builder.CreateInsertValue(
        result.llvmValue, context.get_compile_time_int(index_of(context.get_void_type())), {1U});
    return result;
  }
  if (args.is_one_positional()) {
    auto arg{args[0].value};
    if (auto argUnionType{llvm::dyn_cast<UnionType>(arg.type)}) {
      auto lvalueArg{emitter.lvalue(arg, /*manageLifetime=*/false)};
      auto lvalue{emitter.emit_alloca("tmp.union", this)};
      emitter.builder.CreateStore(Value::zero(this), lvalue); // zeroinitializer
      emitter.builder.CreateMemCpy(
          lvalue.llvmValue, llvm::Align(context.get_align_of(this)),            //
          lvalueArg.llvmValue, llvm::Align(context.get_align_of(argUnionType)), //
          std::min(requiredSize, argUnionType->requiredSize));
      if (!arg.is_lvalue())
        emitter.emit_end_lifetime(lvalueArg);
      auto indexMap{context.get_compile_time_union_index_map(argUnionType, this)};
      auto index{emitter.rvalue(emitter.access(indexMap, emitter.rvalue(emitter.access(arg, "#idx"))))};
      emitter.builder.CreateStore(index, emitter.access(lvalue, "#idx"));
      auto rvalue{emitter.rvalue(lvalue)};
      emitter.emit_end_lifetime(lvalue);
      if (!has_all_types(argUnionType)) {
        auto name{context.get_unique_name("union_conversion", emitter.get_llvm_function())};
        auto blockFail{emitter.create_block(llvm_twine(name, ".fail"))};
        auto blockPass{emitter.create_block(llvm_twine(name, ".pass"))};
        emitter.emit_br(emitter.emit_op(AST::BinaryOp::CmpLt, index, context.get_compile_time_int(0)), blockFail, blockPass);
        emitter.move_to(blockFail);
        emitter.emit_panic("union conversion failed", srcLoc);
        emitter.emit_br_and_move_to(blockPass);
      }
      return rvalue;
    } else {
      if (!has_type(arg.type))
        srcLoc.report_error(std::format("can't construct union type '{}' from type '{}'", name, arg.type->name));
      auto i{index_of(arg.type)};
      auto lvalue{emitter.emit_alloca("tmp.union", this)};
      emitter.builder.CreateStore(Value::zero(this), lvalue); // zeroinitializer
      emitter.builder.CreateStore(emitter.rvalue(arg), lvalue);
      emitter.builder.CreateStore(emitter.context.get_compile_time_int(i), emitter.access(lvalue, "#idx"));
      auto rvalue{emitter.rvalue(lvalue)};
      emitter.emit_end_lifetime(lvalue);
      return rvalue;
    }
  }
  return Type::construct(emitter, args, srcLoc); // Error
}

llvm::SmallVector<Type *> UnionType::canonicalize_types(llvm::ArrayRef<Type *> types) {
  auto canonicalTypes{llvm::SmallVector<Type *>{}};
  for (auto type : types) {
    sanity_check(type != nullptr);
    sanity_check(!type->is_abstract());
    if (auto unionType{llvm::dyn_cast<UnionType>(type)})
      canonicalTypes.insert(canonicalTypes.end(), unionType->types.begin(), unionType->types.end());
    else
      canonicalTypes.push_back(type);
  }
  std::sort(canonicalTypes.begin(), canonicalTypes.end());
  auto itr{std::unique(canonicalTypes.begin(), canonicalTypes.end())};
  canonicalTypes.erase(itr, canonicalTypes.end());
  // If void is present, sort it to the end. This guarantees an optional union has the same non-void index as
  // its non-optional version.
  std::sort(canonicalTypes.begin(), canonicalTypes.end(), [](auto lhs, auto rhs) { return rhs->is_void() || lhs < rhs; });
  return canonicalTypes;
}
//--}

//--{ VoidType
VoidType::VoidType(Context &context) : TypeSubclass(context) {
  init_name("void");
  llvmType = llvm::Type::getVoidTy(context.llvmContext);
}
//--}

} // namespace smdl::Compiler
