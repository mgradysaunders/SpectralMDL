// vim:foldmethod=marker:foldlevel=0:fmr=--{,--}
#include "Type.h"
#include "Emitter.h"

#include "smdl/Logger.h"

#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Type.h"

namespace smdl {

//--{ Type
bool Type::is_arithmetic_boolean() const {
  return is_arithmetic() &&
         static_cast<const ArithmeticType *>(this)->scalar.is_boolean();
}

bool Type::is_arithmetic_integral() const {
  return is_arithmetic() &&
         static_cast<const ArithmeticType *>(this)->scalar.is_integral();
}

bool Type::is_arithmetic_floating_point() const {
  return is_arithmetic() &&
         static_cast<const ArithmeticType *>(this)->scalar.is_floating_point();
}

bool Type::is_arithmetic_scalar() const {
  return is_arithmetic() &&
         static_cast<const ArithmeticType *>(this)->extent.is_scalar();
}

bool Type::is_arithmetic_vector() const {
  return is_arithmetic() &&
         static_cast<const ArithmeticType *>(this)->extent.is_vector();
}

bool Type::is_arithmetic_matrix() const {
  return is_arithmetic() &&
         static_cast<const ArithmeticType *>(this)->extent.is_matrix();
}

bool Type::is_optional_union() const {
  // NOTE: `Union::canonicalize_types()` always places `void` at the end!
  return is_union() &&
         static_cast<const UnionType *>(this)->caseTypes.back()->is_void();
}

Type *Type::get_pointee_type() const {
  return is_pointer() ? static_cast<const PointerType *>(this)->pointeeType
                      : nullptr;
}

Type *Type::get_first_non_pointer_type() const {
  auto type{const_cast<Type *>(this)};
  while (type->is_pointer())
    type = type->get_pointee_type();
  return type;
}

size_t Type::get_first_non_pointer_type_depth() const {
  size_t depth{};
  auto type{const_cast<Type *>(this)};
  while (type->is_pointer()) {
    type = type->get_pointee_type();
    depth++;
  }
  return depth;
}

Value Type::invoke(Emitter &emitter, const ArgumentList &args,
                   const SourceLocation &srcLoc) {
  if (args.is_one_positional(this))
    return emitter.to_rvalue(args[0].value);
  srcLoc.throw_error(
      concat("type ", quoted(displayName), " has unimplemented constructor"));
  return Value();
}

Value Type::access_field(Emitter &emitter, Value value, std::string_view name,
                         const SourceLocation &srcLoc) {
  srcLoc.throw_error(
      concat("type ", quoted(displayName), " has no field access operator"));
  return Value();
}

Value Type::access_index(Emitter &emitter, Value value, Value name,
                         const SourceLocation &srcLoc) {
  srcLoc.throw_error(
      concat("type ", quoted(displayName), " has no index access operator"));
  return Value();
}

Value Type::insert(Emitter &emitter, Value value, Value elem, unsigned pos,
                   const SourceLocation &srcLoc) {
  srcLoc.throw_error(
      concat("type ", quoted(displayName), " has unimplemented insert method"));
  return Value();
}
//--}

//--{ ArithmeticType
llvm::Type *Scalar::get_llvm_type(llvm::LLVMContext &context) const {
  if (intent == Intent::Int) {
    return llvm::Type::getIntNTy(context, numBits);
  } else if (intent == Intent::FP) {
    if (numBits == 16) {
      return llvm::Type::getHalfTy(context);
    } else if (numBits == 32) {
      return llvm::Type::getFloatTy(context);
    } else if (numBits == 64) {
      return llvm::Type::getDoubleTy(context);
    } else if (numBits == 80) {
      return llvm::Type::getX86_FP80Ty(context);
    } else if (numBits == 128) {
      return llvm::Type::getFP128Ty(context);
    } else {
      SMDL_SANITY_CHECK(false, "Invalid float type specification!");
      return nullptr;
    }
  } else {
    return llvm::Type::getVoidTy(context);
  }
}

ArithmeticType::ArithmeticType(Context &context, Scalar scalar, Extent extent)
    : scalar(scalar), extent(extent) {
  displayName = extent.to_string(scalar);
  llvmType = extent.get_llvm_type(scalar.get_llvm_type(context));
}

Value ArithmeticType::invoke(Emitter &emitter, const ArgumentList &args,
                             const SourceLocation &srcLoc) {
  if (args.empty() || args.is_null()) {
    return Value::zero(this);
  }
  if (args.is_one_positional(this)) {
    return emitter.to_rvalue(args[0].value);
  }
  if (extent.is_scalar()) {
    if (!args.is_one_positional())
      srcLoc.throw_error(concat("scalar ", quoted(displayName),
                                " constructor expects 1 positional argument"));
    auto value{args[0].value};
    // If constructing bool from pointer, check that it is non-NULL.
    if (scalar.is_boolean() && value.type->is_pointer())
      return RValue(this,
                    emitter.builder.CreateIsNotNull(emitter.to_rvalue(value)));
    // If constructing bool from optional union, check that it is non-void.
    if (scalar.is_boolean() && value.type->is_optional_union())
      return RValue(
          this,
          emitter.builder.CreateICmpNE(
              emitter.to_rvalue(emitter.access_field(value, "#idx", srcLoc)),
              emitter.context.get_comptime_int(
                  static_cast<UnionType *>(value.type)->caseTypes.size() - 1)));
    // If constructing from another scalar or enum type, cast the
    // underlying LLVM representation.
    if (value.type->is_arithmetic_scalar() || value.type->is_enum())
      return RValue(this, llvm_emit_cast(emitter.builder,
                                         emitter.to_rvalue(value), llvmType));
  } else if (extent.is_vector()) {
    auto dim{size_t(extent.get_vector_size())};
    if (args.is_one_positional()) {
      auto value{args[0].value};
      // If constructing from scalar, splat the scalar value.
      // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      // float3(2.7) // == float3(2.7, 2.7, 2.7)
      // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if (value.type->is_arithmetic_scalar())
        return RValue(this, emitter.builder.CreateVectorSplat(
                                extent.get_vector_size(),
                                emitter.invoke(get_scalar_type(emitter.context),
                                               value, srcLoc)));
      // If constructing from vector of the same size, cast the
      // underlying LLVM representation.
      // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      // int4(float4(3.0, 0.2, 0.1, 5.4)) // Cast components to int
      // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if (value.type->is_arithmetic_vector() &&
          static_cast<ArithmeticType *>(value.type)->extent == extent)
        return RValue(this, llvm_emit_cast(emitter.builder,
                                           emitter.to_rvalue(value), llvmType));
      // If constructing from a pointer to the scalar type, load from the
      // pointer. Assume the pointer is only as aligned as the scalar type
      // itself!
      if (value.type->is_pointer() &&
          value.type->get_pointee_type() == get_scalar_type(emitter.context))
        return RValue(this, emitter.builder.CreateAlignedLoad(
                                llvmType, emitter.to_rvalue(value),
                                llvm::Align(emitter.context.get_align_of(
                                    get_scalar_type(emitter.context)))));
      // If constructing from color and this is a 3-dimensional vector,
      // delegate to the `$color_to_rgb` function in the `api` module.
      if (value.type == emitter.context.get_color_type() && dim == 3)
        return invoke(emitter,
                      emitter.emit_call(
                          emitter.context.get_keyword_value("$color_to_rgb"),
                          value, srcLoc),
                      srcLoc);
    }
    // From scalars
    auto canConstructFromScalars{[&] {
      if (!(dim == args.size() && args.is_all_true([](auto &arg) {
            return arg.value.type->is_arithmetic_scalar();
          })))
        return false;
      return (dim == 2 && args.is_only_these_names({"x", "y"})) ||
             (dim == 3 && args.is_only_these_names({"x", "y", "z"})) ||
             (dim == 4 && args.is_only_these_names({"x", "y", "z", "w"})) ||
             !args.is_any_named();
    }()};
    if (canConstructFromScalars) {
      auto values{llvm::SmallVector<Value>{}};
      // If vector size is 2, 3, or 4, possibly resolve the argument names.
      // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      // float4(w: 3.0, x: 5.0, y: 7.0, z: 9.0)
      // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if (2 <= dim && dim <= 4) {
        auto scalarType{get_scalar_type(emitter.context)};
        auto params{ParameterList{}};
        params.push_back(Parameter{scalarType, "x"});
        params.push_back(Parameter{scalarType, "y"});
        if (dim >= 3)
          params.push_back(Parameter{scalarType, "z"});
        if (dim >= 4)
          params.push_back(Parameter{scalarType, "w"});
        values =
            std::move(emitter.resolve_arguments(params, args, srcLoc).values);
      } else {
        // Otherwise just add the argument values in order. (We already verifed
        // by now that they have no names!)
        for (auto &arg : args)
          values.push_back(arg.value);
      }
      auto result{Value::zero(this)};
      for (size_t i = 0; i < dim; i++)
        result = emitter.insert(result, values[i], i, srcLoc);
      return result;
    }
    // From scalars and vectors
    auto canConstructFromScalarsAndVectors{[&] {
      if (!args.is_all_true([](auto &arg) {
            return arg.is_positional() &&
                   (arg.value.type->is_arithmetic_scalar() ||
                    arg.value.type->is_arithmetic_vector());
          }))
        return false;
      size_t impliedDim{};
      for (auto &arg : args)
        impliedDim += static_cast<ArithmeticType *>(arg.value.type)
                          ->extent.get_vector_size();
      return impliedDim == dim;
    }()};
    if (canConstructFromScalarsAndVectors) {
      auto result{Value::zero(this)};
      auto i{size_t(0)};
      for (auto &arg : args) {
        // Convert the scalar to avoid redundant casting later. Then
        // - If the argument is a scalar, insert it into the vector.
        // - If the argument is a vector, extract each element and insert it
        // into the vector.
        SMDL_SANITY_CHECK(arg.value.type->is_arithmetic_scalar() ||
                          arg.value.type->is_arithmetic_vector());
        auto arithType{static_cast<ArithmeticType *>(arg.value.type)};
        auto value{emitter.invoke(
            arithType->get_with_different_scalar(emitter.context, scalar), arg,
            srcLoc)};
        if (arg.value.type->is_arithmetic_scalar()) {
          result = emitter.insert(result, value, i++, srcLoc);
        } else {
          for (size_t j = 0; j < arithType->extent.get_vector_size(); j++)
            result = emitter.insert(
                result, emitter.access_index(value, j, srcLoc), i++, srcLoc);
        }
      }
      return result;
    }
  } else if (extent.is_matrix()) {
    auto scalarType{get_scalar_type(emitter.context)};
    auto columnType{get_column_type(emitter.context)};
    auto construct{[&](auto &&func) {
      auto result{Value::zero(this)};
      for (unsigned j = 0; j < extent.numCols; j++)
        result = emitter.insert(result, std::invoke(func, j), j, srcLoc);
      return result;
    }};
    if (args.is_one_positional()) {
      const bool canConstructFromScalar{
          args[0].value.type->is_arithmetic_scalar()};
      if (canConstructFromScalar)
        return construct(
            [&, scalar = emitter.invoke(scalarType, args, srcLoc)](unsigned j) {
              auto column{Value::zero(columnType)};
              if (j < extent.numRows)
                column = emitter.insert(column, scalar, j, srcLoc);
              return column;
            });
      const bool canConstructFromMatrix{
          args[0].value.type->is_arithmetic_matrix() &&
          static_cast<ArithmeticType *>(args[0].value.type)->extent == extent};
      if (canConstructFromMatrix)
        return construct([&](unsigned j) {
          return emitter.access_index(args[0].value, j, srcLoc);
        });
    }
    if (args.is_all_positional()) {
      const bool canConstructFromColumns{
          args.size() == extent.numCols && args.is_all_true([&](auto &arg) {
            return arg.value.type->is_arithmetic_vector() &&
                   static_cast<ArithmeticType *>(arg.value.type)
                           ->extent.numRows == extent.numRows;
          })};
      if (canConstructFromColumns)
        return construct([&](unsigned j) { return args[j].value; });

      const bool canConstructFromScalars{
          args.size() == extent.numCols * extent.numRows &&
          args.is_all_true([](auto &arg) {
            return arg.value.type->is_arithmetic_scalar();
          })};
      if (canConstructFromScalars)
        return construct([&](unsigned j) {
          auto column{Value::zero(columnType)};
          for (unsigned i = 0; i < extent.numRows; i++)
            column = emitter.insert(column, args[j * extent.numRows + i].value,
                                    i, srcLoc);
          return column;
        });
    }
  }
  srcLoc.throw_error(concat("cannot construct ", quoted(displayName), " from ",
                            quoted(std::string(args))));
  return Value();
}

bool ArithmeticType::has_field(std::string_view name) {
  if (!extent.is_scalar()) {
    if (name.size() == 1)
      return to_index(name[0]).has_value();
    if (extent.is_vector())
      return name == "size" || to_index_swizzle(name).has_value();
    if (extent.is_matrix())
      return name == "size" || name == "rows" || name == "cols";
  }
  return false;
}

Value ArithmeticType::access_field(Emitter &emitter, Value value,
                                   std::string_view name,
                                   const SourceLocation &srcLoc) {
  if (extent.is_scalar()) {
    // Allow `.size` for easy implementation of `math::average()`.
    if (name == "size") {
      return emitter.context.get_comptime_int(1);
    }
    srcLoc.throw_error(concat("scalar ", quoted(displayName),
                              " has no field access operator"));
  }
  if (name.size() == 1) {
    if (auto i{to_index(name[0])}) {
      auto result{access_index(emitter, value,
                               emitter.context.get_comptime_int(*i), srcLoc)};
      // Set LLVM name for more readable LLVM-IR.
      if (value.llvmValue->hasName()) {
        auto name0{value.llvmValue->getName().str()};
        name0 += '.';
        name0 += name;
        result.llvmValue->setName(name0);
      }
      return result;
    }
  }
  if (extent.is_vector()) {
    if (auto iMask{to_index_swizzle(name)}) {
      auto result{RValue(
          emitter.context.get_arithmetic_type(scalar, Extent(iMask->size())),
          emitter.builder.CreateShuffleVector(emitter.to_rvalue(value),
                                              *iMask))};
      // Set LLVM name for more readable LLVM-IR.
      if (value.llvmValue->hasName()) {
        auto name0{value.llvmValue->getName().str()};
        name0 += '.';
        name0 += name;
        result.llvmValue->setName(name0);
      }
      return result;
    }
    if (name == "size")
      return emitter.context.get_comptime_int(extent.numRows);
  }
  if (extent.is_matrix()) {
    if (name == "rows")
      return emitter.context.get_comptime_int(extent.numRows);
    if (name == "cols" || name == "size")
      return emitter.context.get_comptime_int(extent.numCols);
  }
  srcLoc.throw_error(
      concat("no field ", quoted(name), " in ", quoted(displayName)));
  return Value();
}

Value ArithmeticType::access_index(Emitter &emitter, Value value, Value i,
                                   const SourceLocation &srcLoc) {
  if (extent.is_scalar())
    srcLoc.throw_error(concat("scalar ", quoted(displayName),
                              " has no index access operator"));
  if (value.is_rvalue()) {
    if (i.is_comptime_int()) {
      unsigned iNow{i.get_comptime_int()};
      if (extent.is_vector()) {
        return RValue(get_scalar_type(emitter.context),
                      emitter.builder.CreateExtractElement(value, iNow));
      } else {
        return RValue(get_column_type(emitter.context),
                      emitter.builder.CreateExtractValue(value, {iNow}));
      }
    } else {
      auto lv{emitter.to_lvalue(value)};
      auto rv{emitter.to_rvalue(access_index(emitter, lv, i, srcLoc))};
      emitter.create_lifetime_end(lv);
      return rv;
    }
  } else {
    if (extent.is_vector()) {
      auto scalarType{get_scalar_type(emitter.context)};
      return LValue(
          scalarType,
          emitter.builder.CreateGEP(
              llvm::ArrayType::get(scalarType->llvmType, extent.numRows), value,
              {emitter.builder.getInt32(0), i.llvmValue}));
    } else {
      return LValue(
          get_column_type(emitter.context),
          emitter.builder.CreateGEP(
              llvmType, value, {emitter.builder.getInt32(0), i.llvmValue}));
    }
  }
  return Value();
}

Value ArithmeticType::insert(Emitter &emitter, Value value, Value elem,
                             unsigned i, const SourceLocation &srcLoc) {
  if (extent.is_vector())
    return RValue(
        this,
        emitter.builder.CreateInsertElement(
            emitter.to_rvalue(value),
            emitter.invoke(get_scalar_type(emitter.context), elem, srcLoc), i));
  if (extent.is_matrix())
    return RValue(
        this, //
        emitter.builder.CreateInsertValue(
            emitter.to_rvalue(value),
            emitter.invoke(get_column_type(emitter.context), elem, srcLoc),
            {i}));
  srcLoc.throw_error(concat("cannot insert into ", quoted(displayName)));
  return Value();
}

ArithmeticType *ArithmeticType::get_with_different_scalar(Context &context,
                                                          Scalar newScalar) {
  return static_cast<ArithmeticType *>(
      context.get_arithmetic_type(newScalar, extent));
}

ArithmeticType *ArithmeticType::get_with_different_extent(Context &context,
                                                          Extent newExtent) {
  return static_cast<ArithmeticType *>(
      context.get_arithmetic_type(scalar, newExtent));
}

ArithmeticType *ArithmeticType::get_common_type(Context &context,
                                                ArithmeticType *otherType) {
  return static_cast<ArithmeticType *>(
      context.get_arithmetic_type(scalar.get_common(otherType->scalar),
                                  extent.get_common(otherType->extent)));
}
//--}

//--{ ArrayType
ArrayType::ArrayType(Context &context, Type *elemType, uint32_t size)
    : elemType(elemType), size(size) {
  SMDL_SANITY_CHECK(elemType);
  if (elemType->llvmType)
    llvmType = llvm::ArrayType::get(elemType->llvmType, size);
  displayName = concat("(", elemType->displayName, ")[", size, "]");
}

Value ArrayType::invoke(Emitter &emitter, const ArgumentList &args,
                        const SourceLocation &srcLoc) {
  if (args.empty()) {
    // The element type may not be trivially constructible, so explicitly
    // default construct the element type and insert it into each element in the
    // array.
    auto value0{emitter.invoke(elemType, args, srcLoc)};
    auto result{Value::zero(emitter.context.get_array_type(value0.type, size))};
    for (uint32_t i = 0; i < size; i++)
      result = emitter.insert(result, value0, i, srcLoc);
    return result;
  }
  if (args.is_null()) {
    if (is_abstract())
      srcLoc.throw_error(
          concat("cannot zero construct abstract array ", quoted(displayName)));
    return Value::zero(this);
  }
  if (args.is_one_positional(this)) {
    return emitter.to_rvalue(args[0].value);
  }
  if (args.is_all_positional() && args.size() == size) {
    if (is_abstract()) {
      auto argElemType{emitter.context.get_common_type(
          args.get_types(), /*defaultToUnion=*/true, srcLoc)};
      if (!emitter.context.is_perfectly_convertible(argElemType, elemType))
        srcLoc.throw_error(concat("cannot construct abstract array ",
                                  quoted(displayName), " from element ",
                                  quoted(argElemType->displayName)));
      return emitter.invoke(emitter.context.get_array_type(argElemType, size),
                            args, srcLoc);
    } else {
      // If we can construct all elements directly from arguments, construct the
      // array by converting each argument.
      auto result{Value::zero(this)};
      for (uint32_t i = 0; i < size; i++)
        result = emitter.insert(
            result, emitter.invoke(elemType, args[i].value, srcLoc), i, srcLoc);
      return result;
    }
  }
  if (args.is_one_positional()) {
    auto value{args[0].value};
    // If constructing from array type of identical size but different element
    // type, try to convert each element.
    if (auto arrType{llvm::dyn_cast<ArrayType>(value.type)};
        arrType && arrType->size == size) {
      auto elems{emitter.access_every_index(
          value, size, srcLoc, [&](unsigned, Value elem) {
            return emitter.invoke(elemType, elem, srcLoc);
          })};
      return invoke(emitter, llvm::ArrayRef<Value>(elems), srcLoc);
    }
    // If constructing from pointer to element type, load from the pointer.
    if (auto ptrType{llvm::dyn_cast<PointerType>(value.type)};
        ptrType && ptrType == elemType) {
      return RValue(this,
                    emitter.builder.CreateAlignedLoad(
                        llvmType, emitter.to_rvalue(value),
                        llvm::Align(emitter.context.get_align_of(elemType))));
    }
  }
  srcLoc.throw_error(concat("cannot construct ", quoted(displayName), " from ",
                            quoted(std::string(args))));
  return Value();
}

Value ArrayType::access_field(Emitter &emitter, Value value,
                              std::string_view name,
                              const SourceLocation &srcLoc) {
  SMDL_SANITY_CHECK(!is_abstract());
  if (has_field(name)) {
    if (!value.is_lvalue()) {
      auto lv{emitter.to_lvalue(value)};
      auto rv{emitter.to_rvalue(access_field(emitter, lv, name, srcLoc))};
      emitter.create_lifetime_end(lv);
      return rv;
    }
    // The behavior here is to construct an `auto[]` by accessing
    // the field on each of element in the array.
    auto elems{emitter.access_every_index(
        value, size, srcLoc, [&](unsigned, Value elem) {
          return emitter.access_field(elem, name, srcLoc);
        })};
    return emitter.invoke(
        emitter.context.get_array_type(emitter.context.get_auto_type(), size),
        llvm::ArrayRef<Value>(elems), srcLoc);
  }
  srcLoc.throw_error(concat("no field ", quoted(name), " in array type ",
                            quoted(displayName)));
  return Value();
}

Value ArrayType::access_index(Emitter &emitter, Value value, Value i,
                              const SourceLocation &srcLoc) {
  SMDL_SANITY_CHECK(!is_abstract());
  if (i.is_comptime_int() && value.is_rvalue()) {
    return RValue(elemType, emitter.builder.CreateExtractValue(
                                value, {unsigned(i.get_comptime_int())}));
  } else {
    if (!value.is_lvalue()) {
      auto lv{emitter.to_lvalue(value)};
      auto rv{emitter.to_rvalue(access_index(emitter, lv, i, srcLoc))};
      emitter.create_lifetime_end(lv);
      return rv;
    }
    i = emitter.to_rvalue(i);
    return LValue(elemType, emitter.builder.CreateGEP(
                                llvmType, value,
                                {emitter.builder.getInt32(0), i.llvmValue}));
  }
}

Value ArrayType::insert(Emitter &emitter, Value value, Value elem, unsigned i,
                        const SourceLocation &srcLoc) {
  SMDL_SANITY_CHECK(!is_abstract());
  SMDL_SANITY_CHECK(i < size);
  return RValue(value.type, emitter.builder.CreateInsertValue(
                                emitter.to_rvalue(value),
                                emitter.invoke(elemType, elem, srcLoc), {i}));
}

ArrayType *ArrayType::get_with_different_element_type(Context &context,
                                                      Type *newElemType) {
  return context.get_array_type(newElemType, size);
}

ArrayType *ArrayType::get_with_different_size(Context &context,
                                              uint32_t newSize) {
  return context.get_array_type(elemType, newSize);
}
//--}

//--{ AutoType
Value AutoType::invoke(Emitter &emitter, const ArgumentList &args,
                       const SourceLocation &srcLoc) {
  // If no arguments or null, return void.
  if (args.empty() || args.is_null()) {
    return RValue(emitter.context.get_void_type(), nullptr);
  }
  // If one positional argument, return it as an rvalue.
  if (args.is_one_positional()) {
    return emitter.to_rvalue(args[0]);
  }
  // If every argument is an arithmetic scalar or vector, then we
  // concatenate them all into another arithmetic vector whose size
  // is the sum of the input sizes.
  //
  // For example:
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // auto f = float2(1, 2);
  // auto g = auto(3.0, f, 4.0); // float3(3.0, 1, 2, 4.0)
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (args.is_all_true([&](auto arg) {
        return arg.is_positional() && (arg.value.type->is_arithmetic_scalar() ||
                                       arg.value.type->is_arithmetic_vector());
      })) {
    Scalar scalar{static_cast<ArithmeticType *>(args[0].value.type)->scalar};
    size_t extent{};
    for (auto arg : args) {
      auto arithType{static_cast<ArithmeticType *>(arg.value.type)};
      scalar = scalar.get_common(arithType->scalar);
      extent += arithType->extent.numRows;
    }
    return emitter.invoke(emitter.context.get_arithmetic_type(scalar, extent),
                          args, srcLoc);
  }
  // TODO Infer struct type?
  srcLoc.throw_error(
      concat("cannot construct 'auto' from ", quoted(std::string(args))));
  return Value();
}
//--}

//--{ ColorType
ColorType::ColorType(Context &context) {
  displayName = "color";
  llvmType = Scalar::get_float().get_llvm_type(context);
  llvmType = Extent(context.compiler.wavelengthBaseMax).get_llvm_type(llvmType);
  wavelengthBaseMax = context.compiler.wavelengthBaseMax;
}

Value ColorType::invoke(Emitter &emitter, const ArgumentList &args,
                        const SourceLocation &srcLoc) {
  auto &context{emitter.context};
  if (args.empty() || args.is_null()) {
    return Value::zero(this);
  }
  if (args.is_one_positional(this)) {
    return emitter.to_rvalue(args[0].value);
  }
  if (args.is_one_positional()) {
    auto value{args[0].value};
    if (value.type->is_arithmetic_scalar())
      return RValue(this, emitter.builder.CreateVectorSplat(
                              wavelengthBaseMax,
                              emitter.invoke(emitter.context.get_float_type(),
                                             value, srcLoc)));
    if (value.type->is_arithmetic_vector() &&
        static_cast<ArithmeticType *>(value.type)->extent.numRows ==
            wavelengthBaseMax)
      return RValue(this, llvm_emit_cast(emitter.builder,
                                         emitter.to_rvalue(value), llvmType));
    if (value.type->is_pointer() &&
        value.type->get_pointee_type() == emitter.context.get_float_type())
      return RValue(this, emitter.builder.CreateAlignedLoad(
                              llvmType, emitter.to_rvalue(value),
                              llvm::Align(alignof(float))));
    if (value.type == context.get_float_type(Extent(3)))
      return emitter.emit_call(context.get_keyword_value("$rgb_to_color"),
                               value, srcLoc);
  }
  if (args.size() <= 3 && args.is_only_these_names({"r", "g", "b"})) {
    auto params{ParameterList{
        Parameter{context.get_float_type(), "r", {}, {}, {}, true},
        Parameter{context.get_float_type(), "g", {}, {}, {}, true},
        Parameter{context.get_float_type(), "b", {}, {}, {}, true}}};
    auto resolved{emitter.resolve_arguments(params, args, srcLoc)};
    return emitter.emit_call(
        context.get_keyword_value("$rgb_to_color"),
        emitter.invoke(context.get_float_type(Extent(3)),
                       llvm::ArrayRef<Value>(resolved.values), srcLoc),
        srcLoc);
  }
  srcLoc.throw_error(
      concat("cannot construct 'color' from ", quoted(std::string(args))));
  return Value();
}

Value ColorType::access_index(Emitter &emitter, Value value, Value i,
                              const SourceLocation &srcLoc) {
  if (value.is_rvalue()) {
    if (i.is_comptime_int()) {
      return RValue(
          get_arithmetic_scalar_type(emitter.context),
          emitter.builder.CreateExtractElement(value, i.get_comptime_int()));
    } else {
      auto lv{emitter.to_lvalue(value)};
      auto rv{emitter.to_rvalue(access_index(emitter, lv, i, srcLoc))};
      emitter.create_lifetime_end(lv);
      return rv;
    }
  } else {
    auto scalarType{get_arithmetic_scalar_type(emitter.context)};
    return LValue(
        scalarType,
        emitter.builder.CreateGEP(
            llvm::ArrayType::get(scalarType->llvmType, wavelengthBaseMax),
            value, {emitter.builder.getInt32(0), i.llvmValue}));
  }
}

Value ColorType::insert(Emitter &emitter, Value value, Value elem, unsigned i,
                        const SourceLocation &srcLoc) {
  SMDL_SANITY_CHECK(i < wavelengthBaseMax);
  return RValue(this,
                emitter.builder.CreateInsertElement(
                    emitter.to_rvalue(value),
                    emitter.invoke(get_arithmetic_scalar_type(emitter.context),
                                   elem, srcLoc),
                    i));
}

ArithmeticType *ColorType::get_arithmetic_scalar_type(Context &context) {
  return static_cast<ArithmeticType *>(
      context.get_arithmetic_type(Scalar::get_float()));
}

ArithmeticType *ColorType::get_arithmetic_vector_type(Context &context) {
  return static_cast<ArithmeticType *>(context.get_arithmetic_type(
      Scalar::get_float(), Extent(wavelengthBaseMax)));
}
//--}

//--{ ComptimeUnionType
ComptimeUnionType::ComptimeUnionType(UnionType *unionType)
    : unionType(unionType) {
  displayName = "$" + unionType->displayName;
}

Value ComptimeUnionType::invoke(Emitter &emitter, const ArgumentList &args,
                                const SourceLocation &srcLoc) {
  if (args.empty()) {
    srcLoc.throw_error(
        concat("cannot default construct ", quoted(displayName)));
  }
  if (args.is_one_positional() &&
      unionType->has_case_type(args[0].value.type)) {
    return emitter.to_rvalue(args[0].value);
  }
  srcLoc.throw_error(concat("cannot construct ", quoted(displayName), " from ",
                            quoted(std::string(args))));
  return Value();
}
//--}

//--{ EnumType
void EnumType::initialize(Emitter &emitter) {
  auto &context{emitter.context};
  llvmType = context.get_int_type()->llvmType;
  emitter.declare_crumb(decl.name, &decl, context.get_comptime_meta_type(this));
  auto lastValue{Value()};
  for (auto &declarator : decl.declarators) {
    auto &name{declarator.name};
    auto value{[&]() {
      if (declarator.exprInit)
        return emitter.invoke(context.get_int_type(),
                              emitter.emit(declarator.exprInit), name.srcLoc);
      if (lastValue)
        return emitter.emit_op(BINOP_ADD, lastValue,
                               context.get_comptime_int(1), name.srcLoc);
      return Value::zero(context.get_int_type());
    }()};
    if (!value.is_comptime_int())
      name.srcLoc.throw_error(
          concat("expected ", quoted(name.srcName),
                 " initializer to resolve to compile-time int"));
    emitter.declare_crumb(name.srcName, &declarator, RValue(this, value));
    declarator.llvmConst = static_cast<llvm::ConstantInt *>(value.llvmValue);
    lastValue = value;
  }

  // Initialize the to-string LLVM function. This is just a big switch.
  auto returnType{emitter.context.get_string_type()};
  llvmFuncToString = emitter.create_function(
      displayName + ".to_string", /*isPure=*/true, returnType,
      {Parameter{this, "value"}}, decl.name.srcLoc, [&]() {
        auto valueName{std::string_view("value")};
        auto value{emitter.resolve_identifier(valueName, decl.srcLoc)};
        auto blockDefault{emitter.create_block("switch.default")};
        auto switchInst{emitter.builder.CreateSwitch(emitter.to_rvalue(value),
                                                     blockDefault)};
        auto switchUniq{llvm::DenseSet<llvm::Value *>{}};
        for (unsigned i = 0; i < decl.declarators.size(); i++) {
          if (auto [itr, inserted] =
                  switchUniq.insert(decl.declarators[i].llvmConst);
              !inserted)
            continue; // Skip repeats!
          auto blockCase{
              emitter.create_block("switch.case." + std::to_string(i))};
          switchInst->addCase(decl.declarators[i].llvmConst, blockCase);
          emitter.builder.SetInsertPoint(blockCase);
          emitter.returns.push_back({emitter.context.get_comptime_string(
                                         decl.declarators[i].name.srcName),
                                     blockCase,
                                     decl.declarators[i].name.srcLoc});
          emitter.builder.CreateBr(emitter.labelReturn.block);
        }
        llvm_move_block_to_end(blockDefault);
        emitter.builder.SetInsertPoint(blockDefault);
        emitter.returns.push_back({emitter.context.get_comptime_string(""),
                                   blockDefault, decl.name.srcLoc});
        emitter.builder.CreateBr(emitter.labelReturn.block);
      });
}

Value EnumType::invoke(Emitter &emitter, const ArgumentList &args,
                       const SourceLocation &srcLoc) {
  if (args.empty() || args.is_null()) {
    return Value::zero(this);
  } else if (args.is_one_positional(this)) {
    return emitter.to_rvalue(args[0].value);
  } else if (args.is_one_positional()) {
    auto value{args[0].value};
    if ((value.type->is_arithmetic_scalar() &&
         value.type->is_arithmetic_integral()) ||
        value.type->is_enum())
      return RValue(this, llvm_emit_cast(emitter.builder,
                                         emitter.to_rvalue(value), llvmType));
  }
  srcLoc.throw_error(concat("cannot construct ", quoted(displayName), " from ",
                            quoted(std::string(args))));
  return Value();
}
//--}

//--{ FunctionType
void FunctionType::initialize(Emitter &emitter) {
  auto &context{emitter.context};
  // Find previous overload.
  if (auto prev{emitter.resolve_identifier(decl.name, decl.srcLoc,
                                           /*voidByDefault=*/true)};
      !prev.is_void()) {
    auto prevType{prev.is_comptime_meta_type(context)
                      ? prev.get_comptime_meta_type(context, decl.srcLoc)
                      : nullptr};
    if (!prevType || !prevType->is_function())
      decl.srcLoc.throw_error(
          concat("function ", quoted(declName), " shadows non-function"));
    if (static_cast<FunctionType *>(prevType)->is_variant())
      decl.srcLoc.throw_error(concat("function ", quoted(declName),
                                     " must not overload function variant"));
    if (decl.is_variant())
      decl.srcLoc.throw_error(concat("function variant ", quoted(declName),
                                     " must not overload another function"));
    prevOverload = static_cast<FunctionType *>(prevType);
    prevOverload->nextOverload = this;
  }
  // Declare crumb.
  params.lastCrumb = emitter.declare_crumb(
      decl.name, &decl, context.get_comptime_meta_type(this));
  // Initialize return type and parameters.
  returnType = emitter.emit(decl.returnType)
                   .get_comptime_meta_type(context, decl.srcLoc);
  for (auto &param : decl.params)
    params.push_back(Parameter{
        .type = emitter.emit(param.type)
                    .get_comptime_meta_type(context, param.name.srcLoc),
        .name = param.name,
        .astParam = &param,
        .astField = nullptr,
        .builtinDefaultValue = {}});
  // If this is declared `@(visible)`, we compile it immediately to
  // guarantee that the symbol exists for the C++ runtime.
  if (decl.has_attribute("visible")) {
    if (!params.is_concrete())
      decl.srcLoc.throw_error(
          concat("function ", quoted(declName),
                 " declared '@(visible)' must have concrete parameters"));
    if (!decl.definition)
      decl.srcLoc.throw_error(
          concat("function ", quoted(declName),
                 " declared '@(visible)' must have definition"));
    auto paramTypes{params.get_types()};
    instantiate(emitter, llvm::SmallVector<Type *>(paramTypes.begin(),
                                                   paramTypes.end()));
  }
  // If this is a function with no parameters that returns `material`,
  // it is a material definition!
  if (params.empty() && returnType == context.materialType) {
    if (decl.has_attribute("pure"))
      decl.srcLoc.throw_error(concat("material ", quoted(declName),
                                     " must not be declared '@(pure)'"));
    if (decl.has_attribute("macro"))
      decl.srcLoc.throw_error(concat("material ", quoted(declName),
                                     " must not be declared '@(macro)'"));
    initialize_jit_material_functions(emitter);
  }
}

Value FunctionType::invoke(Emitter &emitter, const ArgumentList &args,
                           const SourceLocation &srcLoc) {
  auto func{resolve_overload(emitter, args, srcLoc)};
  if (!func->is_pure() && !emitter.state)
    srcLoc.throw_error(concat("cannot call ", quoted(func->declName),
                              " from '@(pure)' context"));
  if (func->is_variant()) {
    auto result{Value()};
    auto preserve{Preserve(emitter.crumb)};
    emitter.crumb = func->params.lastCrumb;
    emitter.handle_scope(nullptr, nullptr, [&]() {
      auto [astLet, astCall] =
          func->decl.get_variant_let_and_call_expressions();
      // If the function variant has a `let` expression, generate the variable
      // declarations.
      if (astLet) {
        for (auto &subDecl : astLet->decls) {
          emitter.emit(subDecl);
        }
      }
      // In the function variant call expression, we visit each argument in the
      // AST argument list and add it to the patched argument list but only if
      // the caller did not explicitly set it by name.
      auto patchedArgs{args};
      for (auto &astArg : astCall->args) {
        if (!patchedArgs.has_name(astArg.name.srcName)) {
          patchedArgs.push_back(Argument{astArg.name.srcName,
                                         emitter.emit(astArg.expr), &astArg});
        }
      }
      auto callee{emitter.emit(astCall->expr)};
      result = emitter.emit_call(callee, patchedArgs, srcLoc);
      result = emitter.invoke(decl.returnType->type, result, srcLoc);
    });
    return result;
  }
  auto resolved{emitter.resolve_arguments(func->params, args, srcLoc)};
  if (auto impliedVisitArgs{resolved.get_implied_visit_arguments()})
    return emitter.emit_call(emitter.context.get_comptime_meta_type(this),
                             *impliedVisitArgs, srcLoc);
  if (func->is_macro()) {
    ++macroRecursionDepth;
    if (macroRecursionDepth >= 1024)
      srcLoc.throw_error(concat("call to ", quoted(func->declName),
                                " exceeds compile-time recursion limit 1024"));
    auto preserve{Preserve(emitter.crumb)};
    emitter.crumb = func->params.lastCrumb;
    auto result{emitter.create_function_implementation(
        func->decl.name, func->is_pure(), func->returnType, func->params,
        resolved.values, srcLoc,
        [&]() { emitter.emit(func->decl.definition); })};
    --macroRecursionDepth;
    return result;
  } else {
    auto &instance{func->instantiate(emitter, resolved.get_value_types())};
    auto llvmArgs{llvm::SmallVector<llvm::Value *>{}};
    if (!func->is_pure())
      llvmArgs.push_back(emitter.state);
    llvmArgs.insert(llvmArgs.end(),          //
                    resolved.values.begin(), //
                    resolved.values.end());
    return RValue(instance.returnType, emitter.builder.CreateCall(
                                           instance.llvmFunc->getFunctionType(),
                                           instance.llvmFunc, llvmArgs));
  }
}

FunctionType *FunctionType::resolve_overload(Emitter &emitter,
                                             const ArgumentList &args,
                                             const SourceLocation &srcLoc) {
  if (is_variant()) {
    // We should have already verified the function variant does not
    // illegally overload another function by now!
    return this;
  }
  struct Overload final {
    FunctionType *func{};
    llvm::SmallVector<const Parameter *> params{};
  };
  auto overloads{std::vector<Overload>{}};
  auto getLastOverload{[&]() {
    auto func{this};
    while (func->nextOverload)
      func = func->nextOverload;
    return func;
  }};
  for (auto func{getLastOverload()}; func; func = func->prevOverload) {
    try {
      SMDL_SANITY_CHECK(!func->is_variant());
      auto resolved{emitter.resolve_arguments(func->params, args, srcLoc,
                                              /*dontEmit=*/true)};
      overloads.push_back({func, std::move(resolved.argParams)});
    } catch (const Error &error) {
      // TODO Log?
    }
  }
  // If no matching declarations, fail!
  if (overloads.empty())
    srcLoc.throw_error(concat("function ", quoted(declName),
                              " has no overload for arguments ",
                              quoted(std::string(args))));
  // The lambda to determine whether the LHS set of parameter types is
  // strictly less specific than the RHS set of parameter types. This is true
  // if each and every RHS parameter type is implicitly convertible to the
  // corresponding LHS parameter type.
  auto isLessSpecific{[&](llvm::ArrayRef<const Parameter *> paramsA,
                          llvm::ArrayRef<const Parameter *> paramsB) {
    SMDL_SANITY_CHECK(paramsA.size() == paramsB.size());
    for (size_t i = 0; i < paramsA.size(); i++)
      if (!emitter.context.is_implicitly_convertible(paramsB[i]->type,
                                                     paramsA[i]->type))
        return false;
    return true;
  }};
  // If there is more than 1 matching declaration, remove the less-specific
  // of the first 2 overloads repeatedly until 1 remains. If neither is less
  // specific, the declarations are considered to match ambiguously and
  // overload resolution fails.
  while (overloads.size() > 1) {
    auto &paramsA{overloads[0].params};
    auto &paramsB{overloads[1].params};
    if (isLessSpecific(paramsA, paramsB)) {
      overloads.erase(overloads.begin());
    } else if (isLessSpecific(paramsB, paramsA)) {
      overloads.erase(overloads.begin() + 1);
    } else {
      srcLoc.throw_error(concat("function ", quoted(declName),
                                " is ambiguous for arguments ",
                                quoted(std::string(args))));
    }
  }
  return overloads[0].func;
}

FunctionType::Instance &
FunctionType::instantiate(Emitter &emitter,
                          const llvm::SmallVector<Type *> paramTypes) {
  SMDL_SANITY_CHECK(decl.definition);
  SMDL_SANITY_CHECK(paramTypes.size() == params.size());
  auto &inst{instances[paramTypes]};
  if (!inst.llvmFunc) {
    SMDL_SANITY_CHECK(!inst.isCompiling);
    inst.isCompiling = true;
    inst.returnType = returnType;
    emitter.create_function(inst.llvmFunc, decl.name, is_pure(),
                            inst.returnType, paramTypes, params, decl.srcLoc,
                            [&] { emitter.emit(decl.definition); });
    static const std::pair<const char *, llvm::Attribute::AttrKind> attrs[] = {
        {"alwaysinline", llvm::Attribute::AlwaysInline},
        {"noinline", llvm::Attribute::NoInline},
        {"hot", llvm::Attribute::Hot},
        {"cold", llvm::Attribute::Cold},
        {"optsize", llvm::Attribute::OptimizeForSize},
        {"optnone", llvm::Attribute::OptimizeNone}};
    for (auto [attrName, attrID] : attrs)
      if (decl.has_attribute(attrName))
        inst.llvmFunc->addFnAttr(attrID);
    if (decl.has_attribute("visible"))
      inst.llvmFunc->setLinkage(llvm::Function::ExternalLinkage);
    inst.isCompiling = false;
  } else if (inst.llvmFunc->getReturnType() ==
             emitter.context.llvmIncompleteReturnTy) {
    // If the instance LLVM function has `Context::llvmIncompleteReturnTy`
    // as its return type, then the function is currently being compiled and
    // this is an attempt to invoke it recursively, which is not allowed!
    decl.srcLoc.throw_error(
        "function with inferred return type must not recurse");
  }
  return inst;
}

void FunctionType::initialize_jit_material_functions(Emitter &emitter) {
  SMDL_LOG_DEBUG(concat(std::string(decl.srcLoc), " New material ",
                        quoted(decl.name.srcName)));
  using namespace std::literals::string_view_literals;
  auto &context{emitter.context};
  auto &jitMaterial{context.compiler.jitMaterials.emplace_back()};
  jitMaterial.moduleName = std::string(decl.srcLoc.get_module_name());
  jitMaterial.moduleFileName = std::string(decl.srcLoc.get_module_file_name());
  jitMaterial.lineNo = decl.srcLoc.lineNo;
  jitMaterial.materialName = std::string(decl.name.srcName);
  auto &inst{instantiate(emitter, {})};
  SMDL_SANITY_CHECK(!inst.returnType->is_abstract() &&
                    inst.returnType->is_struct() &&
                    static_cast<StructType *>(inst.returnType)
                        ->is_instance_of(context.materialType));
  auto dfModule{context.get_builtin_module("df")};
  SMDL_SANITY_CHECK(dfModule);
  Type *materialInstanceType{};
  Type *materialInstancePtrType{};
  Type *float4PtrType{context.get_pointer_type(context.get_float_type(4))};
  Type *float3PtrType{context.get_pointer_type(context.get_float_type(3))};
  Type *floatPtrType{context.get_pointer_type(context.get_float_type())};
  Type *intPtrType{context.get_pointer_type(context.get_int_type())};
  auto constParameter{[](Type *type, std::string_view name) {
    return Parameter{.type = type,
                     .name = name,
                     .astParam = {},
                     .astField = {},
                     .builtinDefaultValue = {},
                     .builtinConst = true};
  }};
  {
    // Generate the allocate function:
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // @(visible) void "material_name.allocate"(&auto out) {
    //   *out = $material_instance(#bump(material_name()));
    // }
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    auto funcReturnType{static_cast<Type *>(context.get_void_type())};
    auto func{emitter.create_function(
        concat(declName, ".allocate"), /*isPure=*/false, funcReturnType,
        {constParameter(context.get_void_pointer_type(), "out")}, decl.srcLoc,
        [&] {
          auto material{RValue(inst.returnType,
                               emitter.builder.CreateCall(
                                   inst.llvmFunc->getFunctionType(),
                                   inst.llvmFunc, {emitter.state.llvmValue}))};
          auto materialInstance{emitter.emit_call(
              context.get_keyword_value("$material_instance"),
              emitter.emit_intrinsic("bump", material, decl.srcLoc),
              decl.srcLoc)};
          materialInstanceType = materialInstance.type;
          materialInstancePtrType =
              context.get_pointer_type(materialInstanceType);
          auto out{emitter.to_rvalue(
              emitter.resolve_identifier("out"sv, decl.srcLoc))};
          emitter.builder.CreateStore(materialInstance, out);
        })};
    func->setLinkage(llvm::Function::ExternalLinkage);
    jitMaterial.allocate.name = func->getName().str();
  }
  {
    // Generate the scatter evaluate function:
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // @(pure visible) int "material_name.scatter_evaluate"(
    //     &$material_instance self,
    //     &float3 wo,
    //     &float3 wi,
    //     &float pdf_fwd,
    //     &float pdf_rev,
    //     &float f) {
    //   return ::df::$scatter_evaluate(
    //     self.instance, wo, wi, pdf_fwd, pdf_rev, f);
    // }
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    auto funcReturnType{static_cast<Type *>(context.get_int_type())};
    auto func{emitter.create_function(
        concat(declName, ".scatter_evaluate"), /*isPure=*/true, funcReturnType,
        {constParameter(materialInstancePtrType, "self"),
         constParameter(float3PtrType, "wo"),
         constParameter(float3PtrType, "wi"),
         constParameter(floatPtrType, "pdf_fwd"),
         constParameter(floatPtrType, "pdf_rev"),
         constParameter(floatPtrType, "f")},
        decl.srcLoc, [&] {
          auto dfFunc{Crumb::find(context, "$scatter_evaluate"sv, nullptr,
                                  dfModule->lastCrumb)};
          SMDL_SANITY_CHECK(dfFunc);
          emitter.emit_return(
              emitter.emit_call(
                  dfFunc->value,
                  llvm::ArrayRef<Value>{
                      emitter.resolve_identifier("self"sv, decl.srcLoc),
                      emitter.resolve_identifier("wo"sv, decl.srcLoc),
                      emitter.resolve_identifier("wi"sv, decl.srcLoc),
                      emitter.resolve_identifier("pdf_fwd"sv, decl.srcLoc),
                      emitter.resolve_identifier("pdf_rev"sv, decl.srcLoc),
                      emitter.resolve_identifier("f"sv, decl.srcLoc)},
                  decl.srcLoc),
              decl.srcLoc);
        })};
    func->setLinkage(llvm::Function::ExternalLinkage);
    jitMaterial.scatter_evaluate.name = func->getName().str();
  }
  {
    // Generate the scatter sample function:
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // @(pure visible) int "material_name.scatter_sample"(
    //     &$material_instance self,
    //     &float4 xi,
    //     &float3 wo,
    //     &float3 wi,
    //     &float pdf_fwd,
    //     &float pdf_rev,
    //     &float f,
    //     &int is_delta) {
    //   return ::df::$scatter_sample(
    //     self.instance, xi, wo, wi, pdf_fwd, pdf_rev, f, is_delta);
    // }
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    auto funcReturnType{static_cast<Type *>(context.get_int_type())};
    auto func{emitter.create_function(
        concat(declName, ".scatter_sample"), /*isPure=*/true, funcReturnType,
        {constParameter(materialInstancePtrType, "self"),
         constParameter(float4PtrType, "xi"),
         constParameter(float3PtrType, "wo"),
         constParameter(float3PtrType, "wi"),
         constParameter(floatPtrType, "pdf_fwd"),
         constParameter(floatPtrType, "pdf_rev"),
         constParameter(floatPtrType, "f"),
         constParameter(intPtrType, "is_delta")},
        decl.srcLoc, [&] {
          auto dfFunc{Crumb::find(context, "$scatter_sample"sv, nullptr,
                                  dfModule->lastCrumb)};
          SMDL_SANITY_CHECK(dfFunc);
          emitter.emit_return(
              emitter.emit_call(
                  dfFunc->value,
                  llvm::ArrayRef<Value>{
                      emitter.resolve_identifier("self"sv, decl.srcLoc),
                      emitter.resolve_identifier("xi"sv, decl.srcLoc),
                      emitter.resolve_identifier("wo"sv, decl.srcLoc),
                      emitter.resolve_identifier("wi"sv, decl.srcLoc),
                      emitter.resolve_identifier("pdf_fwd"sv, decl.srcLoc),
                      emitter.resolve_identifier("pdf_rev"sv, decl.srcLoc),
                      emitter.resolve_identifier("f"sv, decl.srcLoc),
                      emitter.resolve_identifier("is_delta"sv, decl.srcLoc)},
                  decl.srcLoc),
              decl.srcLoc);
        })};
    func->setLinkage(llvm::Function::ExternalLinkage);
    jitMaterial.scatter_sample.name = func->getName().str();
  }
}
//--}

//--{ InferredSizeArrayType
Value InferredSizeArrayType::invoke(Emitter &emitter, const ArgumentList &args,
                                    const SourceLocation &srcLoc) {
  if (args.is_any_named())
    srcLoc.throw_error(concat(
        "unexpected named arguments in constructor of inferred-size array ",
        quoted(displayName)));

  // Infer!
  auto inferredArrayType{[&]() {
    // If there is one positional argument ...
    if (args.is_one_positional()) {
      // If the argument is an array whose element type is convertible to this
      // element type, then the inferred size should be the size of the array
      // argument.
      //
      // This is the behavior we want:
      // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      // auto arr0 = float[4](1, 2, 3, 4);
      // auto arr1 = auto[](arr0);
      // #assert(#typeof(arr0) == #typeof(arr1));
      // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      auto value{args[0].value};
      auto arrayType{llvm::dyn_cast<ArrayType>(value.type)};
      if (arrayType && emitter.context.is_explicitly_convertible(
                           arrayType->elemType, elemType))
        return emitter.context.get_array_type(elemType, arrayType->size);
    }
    // Otherwise, we attempt to construct an array that has the same number of
    // elements as there are arguments.
    return emitter.context.get_array_type(elemType, args.size());
  }()};

  if (!sizeNameStrv.empty()) {
    emitter.declare_crumb(
        sizeNameStrv, nullptr,
        emitter.context.get_comptime_int(inferredArrayType->size));
  }

  // Delegate.
  return inferredArrayType->invoke(emitter, args, srcLoc);
}
//--}

//--{ PointerType
PointerType::PointerType(Context &context, Type *pointeeType)
    : pointeeType(pointeeType) {
  displayName = "&" + pointeeType->displayName;
  llvmType = llvm::PointerType::get(context, 0);
}

Value PointerType::invoke(Emitter &emitter, const ArgumentList &args,
                          const SourceLocation &srcLoc) {
  if (is_abstract()) {
    if (args.empty() || args.is_null())
      srcLoc.throw_error(concat("cannot zero construct abstract pointer ",
                                quoted(displayName)));
    if (args.is_one_positional()) {
      auto value{args[0].value};
      if (value.type->is_pointer() &&
          emitter.context.is_perfectly_convertible(
              value.type->get_pointee_type(), pointeeType)) {
        return emitter.to_rvalue(value);
      }
    }
  } else {
    if (args.empty() || args.is_null()) {
      return Value::zero(this);
    }
    if (args.is_one_positional()) {
      auto value{args[0].value};
      if (value.type->is_pointer()) {
        return RValue(this, emitter.to_rvalue(value));
      } else if (value.type == pointeeType) {
        if (value.is_lvalue()) {
          return RValue(this, value);
        } else {
          auto lv{emitter.to_lvalue(value)};
          emitter.declare_crumb(/*name=*/{}, /*node=*/{}, lv);
          return RValue(this, lv);
        }
      }
    }
  }
  srcLoc.throw_error(concat("cannot construct ", quoted(displayName), " from ",
                            quoted(std::string(args))));
  return Value();
}

Value PointerType::access_field(Emitter &emitter, Value value,
                                std::string_view name,
                                const SourceLocation &srcLoc) {
  return pointeeType->access_field(
      emitter, LValue(pointeeType, emitter.to_rvalue(value)), name, srcLoc);
}

Value PointerType::access_index(Emitter &emitter, Value value, Value i,
                                const SourceLocation &srcLoc) {
  return LValue(pointeeType,
                emitter.builder.CreateGEP(pointeeType->llvmType,
                                          emitter.to_rvalue(value),
                                          {emitter.to_rvalue(i).llvmValue}));
}
//--}

//--{ StateType
StateType::StateType(Context &context) {
  displayName = "state";
#define ADD_FIELD(name)                                                        \
  fields.push_back({context.get_type(&State::name), #name,                     \
                    uint64_t(offsetof(State, name))})
  ADD_FIELD(allocator);
  ADD_FIELD(position);
  ADD_FIELD(normal);
  ADD_FIELD(geometry_normal);
  ADD_FIELD(motion);
  ADD_FIELD(texture_space_max);
  ADD_FIELD(texture_coordinate);
  ADD_FIELD(texture_tangent_u);
  ADD_FIELD(texture_tangent_v);
  ADD_FIELD(geometry_tangent_u);
  ADD_FIELD(geometry_tangent_v);
  ADD_FIELD(object_id);
  ADD_FIELD(ptex_face_id);
  ADD_FIELD(ptex_face_uv);
  ADD_FIELD(direction);
  ADD_FIELD(animation_time);
  ADD_FIELD(wavelength_base);
  ADD_FIELD(wavelength_min);
  ADD_FIELD(wavelength_max);
  ADD_FIELD(meters_per_scene_unit);
  ADD_FIELD(object_to_world_matrix);
  ADD_FIELD(tangent_to_object_matrix);
#undef ADD_FIELD
  auto llvmTypes{llvm::SmallVector<llvm::Type *>{}};
  for (auto &field : fields) {
    SMDL_SANITY_CHECK(field.type);
    SMDL_SANITY_CHECK(field.type->llvmType);
    llvmTypes.push_back(field.type->llvmType);
  }
  llvmType = llvm::StructType::create(context, llvmTypes, displayName);
  auto llvmLayout{context.llvmLayout.getStructLayout(
      static_cast<llvm::StructType *>(llvmType))};
  for (unsigned i = 0; i < fields.size(); i++)
    if (fields[i].offset != uint64_t(llvmLayout->getElementOffset(i)))
      throw Error(
          concat("mismatch between C++ and SMDL 'state' structures (field ",
                 quoted(fields[i].name), " is misaligned)"));
}

Value StateType::access_field(Emitter &emitter, Value value,
                              std::string_view name,
                              const SourceLocation &srcLoc) {
  SMDL_SANITY_CHECK(value.is_lvalue());
  for (unsigned i = 0; i < fields.size(); i++) {
    if (fields[i].name == name) {
      auto llvmValue{
          emitter.builder.CreateStructGEP(value.type->llvmType, value, i)};
      if (value.llvmValue->hasName())
        llvmValue->setName(concat(value.llvmValue->getName().str(), ".", name));
      return LValue(fields[i].type, llvmValue);
    }
  }
  srcLoc.throw_error(concat("no field ", quoted(name), " in 'state'"));
  return Value();
}
//--}

//--{ StringType
StringType::StringType(Context &context) {
  displayName = "string";
  llvmType = llvm::PointerType::get(context, 0);
}

Value StringType::invoke(Emitter &emitter, const ArgumentList &args,
                         const SourceLocation &srcLoc) {
  // Construct from nothing or null.
  if (args.empty() || args.is_null()) {
    return Value::zero(this);
  }
  // Construct from another string.
  if (args.is_one_positional(this)) {
    return emitter.to_rvalue(args[0].value);
  }
  // Construct from enum, call the enum-to-string conversion function.
  if (args.is_one_positional()) {
    auto value{args[0].value};
    if (auto enumType{llvm::dyn_cast<EnumType>(value.type)})
      return RValue(this, emitter.builder.CreateCall(
                              enumType->llvmFuncToString->getFunctionType(),
                              enumType->llvmFuncToString,
                              {emitter.to_rvalue(value).llvmValue}));
  }
  srcLoc.throw_error(
      concat("cannot construct 'string' from ", quoted(std::string(args))));
  return Value();
}

Value StringType::access_field(Emitter &emitter, Value value,
                               std::string_view name,
                               const SourceLocation &srcLoc) {
  if (name == "size") {
    if (value.is_comptime()) {
      return emitter.context.get_comptime_int(
          value.get_comptime_string().size());
    } else {
      return RValue(
          emitter.context.get_int_type(),
          llvm_emit_cast(
              emitter.builder,
              llvm::emitStrLen(emitter.to_rvalue(value), emitter.builder,
                               emitter.context.llvmLayout,
                               &emitter.context.llvmTargetLibraryInfo),
              emitter.context.get_int_type()->llvmType));
    }
  }
  srcLoc.throw_error(concat("no field ", quoted(name), " in 'string'"));
  return Value();
}
//--}

//--{ StructType
void StructType::initialize(Emitter &emitter) {
  params.lastCrumb = emitter.declare_crumb(
      decl.name, &decl, emitter.context.get_comptime_meta_type(this));
  // Initialize tags.
  for (auto &tag : decl.tags) {
    emitter.emit(tag.type);
    auto tagType{llvm::dyn_cast<TagType>(tag.type->type)};
    if (!tagType)
      decl.srcLoc.throw_error("unknown tag");
    if (tag.is_default()) {
      if (tagType->defaultType)
        decl.srcLoc.throw_error(concat("tag ", quoted(tagType->displayName),
                                       " already has default"));
      tagType->defaultType = this;
    }
    tags.push_back(tagType);
  }
  // Initialize fields.
  for (auto &field : decl.fields) {
    auto &param{params.emplace_back()};
    param.type =
        emitter.emit(field.type)
            .get_comptime_meta_type(emitter.context, field.name.srcLoc);
    param.name = field.name.srcName;
    param.astField = &field;
    if (param.type == this)
      field.name.srcLoc.throw_error(
          concat("struct ", quoted(displayName),
                 " cannot be type of field in its definition"));
  }
  // Initialize LLVM type. Note: We don't check `is_abstract()`
  // because it is possible that the type is abstract, but has an entirely
  // concrete LLVM definition (e.g., through abstract pointers).
  if (params.is_all_true(
          [](auto &param) { return param.type->llvmType != nullptr; }))
    llvmType = llvm::StructType::create(emitter.context,
                                        params.get_llvm_types(), displayName);
}

StructType *
StructType::instantiate(Context &context,
                        const llvm::SmallVector<Type *> &paramTypes) {
  SMDL_SANITY_CHECK(params.is_abstract());
  SMDL_SANITY_CHECK(params.size() == paramTypes.size());
  auto &structType{instances[paramTypes]};
  if (!structType) {
    structType = context.allocator.allocate<StructType>(decl);
    structType->instanceOf = this;
    structType->tags = tags;
    structType->params = params;
    for (size_t i{}; i < params.size(); i++) {
      SMDL_SANITY_CHECK(paramTypes[i]);
      SMDL_SANITY_CHECK(!paramTypes[i]->is_abstract());
      structType->params[i].type = paramTypes[i];
    }
    structType->llvmType = llvm::StructType::create(
        context, structType->params.get_llvm_types(), structType->displayName);
  }
  return structType.get();
}

bool StructType::is_abstract() {
  // If in `initialize()` we failed to construct the LLVM type
  // because at least one parameter type had no LLVM type, then
  // this type is definitely abstract!
  if (!llvmType)
    return true;
  // If at least one parameter type is abstract, then this type
  // is also abstract. However, we have to be careful here.
  for (auto &param : params) {
    // If the parameter is a pointer to an instance of this type,
    // we cannot ask it if it `is_abstract()` because that causes
    // an infinite recursion.
    //
    // Instead, we just skip it. NOTE: This is not an edge case
    // because linked lists rely on this self-referential
    // pointer behavior all the time.
    // ~~~~~~~~~~~~~~~~~~~
    // struct Foo {
    //   &Foo next = null;
    // };
    // ~~~~~~~~~~~~~~~~~~~
    if (param.type->is_pointer() &&
        param.type->get_first_non_pointer_type() == this) {
      continue;
    }
    if (param.type->is_abstract()) {
      return true;
    }
  }
  return false;
}

Value StructType::invoke(Emitter &emitter, const ArgumentList &args,
                         const SourceLocation &srcLoc) {
  if (args.is_null()) {
    if (is_abstract())
      srcLoc.throw_error(concat("cannot zero construct abstract struct type ",
                                quoted(displayName)));
    return Value::zero(this);
  }
  if (args.is_one_positional()) {
    if (auto structType{llvm::dyn_cast<StructType>(args[0].value.type)};
        structType &&
        (structType == this || structType->is_instance_of(this))) {
      return emitter.to_rvalue(args[0].value);
    }
  }
  if (emitter.can_resolve_arguments(params, args, srcLoc)) {
    auto resolved{emitter.resolve_arguments(params, args, srcLoc)};
    auto resultType{this};
    if (resultType->is_abstract()) {
      resultType = instantiate(emitter.context, resolved.get_value_types());
      SMDL_SANITY_CHECK(!resultType->is_abstract());
    }
    auto result{Value::zero(resultType)};
    auto i{size_t(0)};
    for (auto &value : resolved.values)
      result = emitter.insert(result, value, i++, srcLoc);
    if (decl.stmtFinalize) {
      auto preserve{Preserve(emitter.crumb)};
      emitter.crumb = params.lastCrumb;
      emitter.handle_scope(nullptr, nullptr, [&] {
        emitter.labelReturn = {};   // Invalidate!
        emitter.labelBreak = {};    // Invalidate!
        emitter.labelContinue = {}; // Invalidate!
        auto lv{emitter.to_lvalue(result)};
        for (auto &param : params)
          emitter.declare_crumb(param.name, &decl,
                                emitter.access_field(lv, param.name, srcLoc));
        emitter.emit(decl.stmtFinalize);
        result = emitter.to_rvalue(lv);
        emitter.create_lifetime_end(lv);
      });
    }
    return result;
  }
  srcLoc.throw_error(concat("cannot construct ", quoted(displayName), " from ",
                            quoted(std::string(args))));
  return Value();
}

Value StructType::access_field(Emitter &emitter, Value value,
                               std::string_view name,
                               const SourceLocation &srcLoc) {
  auto seq{ParameterList::LookupSeq{}};
  if (params.get_lookup_sequence(name, seq)) {
    auto hasName0{value.llvmValue->hasName()};
    auto name0{value.llvmValue->getName().str()};
    bool isConst{};
    for (auto [param, i] : seq) {
      isConst |= param->is_const();
      value = Value(
          value.kind, param->type,
          value.is_lvalue()
              ? emitter.builder.CreateStructGEP(value.type->llvmType, value, i)
              : emitter.builder.CreateExtractValue(value, {i}));
    }
    if (hasName0) {
      name0 += '.';
      name0 += name;
      value.llvmValue->setName(name0);
    }
    return isConst ? emitter.to_rvalue(value) : value;
  }
  srcLoc.throw_error(
      concat("no field ", quoted(name), " in struct ", quoted(displayName)));
  return Value();
}

Value StructType::insert(Emitter &emitter, Value value, Value elem, unsigned i,
                         const SourceLocation &srcLoc) {
  SMDL_SANITY_CHECK(i < params.size());
  if (params[i].type->is_void())
    return emitter.to_rvalue(value);
  else
    return RValue(this, emitter.builder.CreateInsertValue(
                            emitter.to_rvalue(value),
                            emitter.invoke(params[i].type, elem, srcLoc), {i}));
}
//--}

//--{ TagType
Value TagType::invoke(Emitter &emitter, const ArgumentList &args,
                      const SourceLocation &srcLoc) {
  if (args.empty() || args.is_null()) {
    if (!defaultType)
      srcLoc.throw_error(
          concat("cannot default construct tag ", quoted(displayName)));
    return defaultType->invoke(emitter, args, srcLoc);
  } else if (args.is_one_positional()) {
    auto value{args[0].value};
    if (!emitter.context.is_perfectly_convertible(value.type, this))
      srcLoc.throw_error(concat("cannot construct tag ", quoted(displayName),
                                " from ", quoted(value.type->displayName)));
    return emitter.to_rvalue(value);
  } else {
    srcLoc.throw_error(concat("cannot construct tag ", quoted(displayName),
                              " from ", quoted(std::string(args))));
    return Value();
  }
}
//--}

//--{ Texture2DInstanceType
Texture2DInstanceType::Texture2DInstanceType(Context &context, Type *texelType,
                                             int tileCountU, int tileCountV)
    : texelType(texelType), tileCountU(tileCountU), tileCountV(tileCountV) {
  SMDL_SANITY_CHECK(texelType != nullptr && !texelType->is_abstract());
  SMDL_SANITY_CHECK(tileCountU >= 0);
  SMDL_SANITY_CHECK(tileCountV >= 0);
  displayName = "texture_2d:" + texelType->displayName;
  tileExtentsType = context.get_array_type(context.get_int_type(Extent(2)),
                                           tileCountU * tileCountV);
  tileBuffersType = context.get_array_type(context.get_pointer_type(texelType),
                                           tileCountU * tileCountV);
  llvmType = llvm::StructType::create(context,
                                      {context.get_int_type()->llvmType,
                                       tileExtentsType->llvmType,
                                       tileBuffersType->llvmType},
                                      displayName.c_str());
}

Value Texture2DInstanceType::invoke(Emitter &emitter, const ArgumentList &args,
                                    const SourceLocation &srcLoc) {
  if (args.empty() || args.is_null())
    return Value::zero(this);
  if (args.is_one_positional(this))
    return emitter.to_rvalue(args[0].value);
  srcLoc.throw_error(concat("cannot construct ", quoted(displayName)));
  return Value();
}

Value Texture2DInstanceType::access_field(Emitter &emitter, Value value,
                                          std::string_view name,
                                          const SourceLocation &srcLoc) {
  auto doAccess{[&](unsigned i, Type *fieldType) {
    return Value(
        value.kind, fieldType,
        value.is_lvalue()
            ? emitter.builder.CreateStructGEP(value.type->llvmType, value, i)
            : emitter.builder.CreateExtractValue(value, {i}));
  }};
  if (name == "gamma") {
    return doAccess(0, emitter.context.get_int_type());
  } else if (name == "tile_count") {
    return emitter.context.get_comptime_vector(int2(tileCountU, tileCountV));
  } else if (name == "tile_count_u") {
    return emitter.context.get_comptime_int(tileCountU);
  } else if (name == "tile_count_v") {
    return emitter.context.get_comptime_int(tileCountV);
  } else if (name == "tile_extents") {
    return doAccess(1, tileExtentsType);
  } else if (name == "tile_buffers") {
    return doAccess(2, tileBuffersType);
  } else if (name == "texel_type") {
    return emitter.context.get_comptime_meta_type(texelType);
  }
  srcLoc.throw_error(
      concat("no field ", quoted(name), " in type ", quoted(displayName)));
  return Value();
}
//--}

//--{ Texture2DType
Texture2DInstanceType *Texture2DType::instantiate(Context &context,
                                                  Type *texelType,
                                                  int tileCountU,
                                                  int tileCountV) {
  auto &inst{instances[std::tuple(texelType, tileCountU, tileCountV)]};
  if (!inst)
    inst = context.allocator.allocate<Texture2DInstanceType>(
        context, texelType, tileCountU, tileCountV);
  return inst.get();
}

Value Texture2DType::invoke(Emitter &emitter, const ArgumentList &args,
                            const SourceLocation &srcLoc) {
  auto &context{emitter.context};
  auto nullTexture{Value::zero(
      instantiate(context, context.get_float_type(Extent(4)), 1, 1))};
  if (args.empty() || args.is_null())
    return nullTexture;
  if (args.is_one_positional() &&
      llvm::isa<Texture2DInstanceType>(args[0].value.type))
    return emitter.to_rvalue(args[0].value);
  auto params{ParameterList{
      Parameter{.type = context.get_string_type(),
                .name = "name",
                .astParam = nullptr,
                .astField = nullptr,
                .builtinDefaultValue = std::nullopt},
      Parameter{.type = context.get_auto_type(),
                .name = "gamma",
                .astParam = nullptr,
                .astField = nullptr,
                .builtinDefaultValue = context.get_comptime_int(0)}}};
  // TODO Catch and rethrow for better error message?
  auto resolvedArgs{emitter.resolve_arguments(params, args, srcLoc)};
  auto valueFileName{emitter.to_rvalue(resolvedArgs.values[0])};
  if (!valueFileName.is_comptime_string())
    srcLoc.throw_error("expected 'texture_2d' parameter 'name' to resolve to "
                       "compile-time string");
  auto fileName{valueFileName.get_comptime_string()};
  auto tileImagePaths{context.compiler.fileLocator.locate_images(
      fileName, srcLoc.get_module_file_name())};
  if (tileImagePaths.empty()) {
    srcLoc.log_warn(concat("no image(s) found for ", quoted(fileName)));
    return nullTexture;
  }
  auto tileCountU{uint32_t(1)};
  auto tileCountV{uint32_t(1)};
  auto tileImages{llvm::SmallVector<const Image *>{}};
  for (auto &[tileIndexU, tileIndexV, filePath] : tileImagePaths) {
    tileCountU = std::max(tileCountU, tileIndexU + 1);
    tileCountV = std::max(tileCountV, tileIndexV + 1);
    tileImages.push_back(&context.compiler.load_image(filePath, srcLoc));
    if (tileImages.back()->get_format() != tileImages[0]->get_format() ||
        tileImages.back()->get_num_channels() !=
            tileImages[0]->get_num_channels()) {
      srcLoc.log_warn(
          concat("inconsistent image formats for ", quoted(fileName)));
      return nullTexture;
    }
  }
  auto texelType{context.get_arithmetic_type(
      tileImages[0]->get_format() == Image::U8    ? Scalar::get_int(8)
      : tileImages[0]->get_format() == Image::U16 ? Scalar::get_int(16)
      : tileImages[0]->get_format() == Image::F16 ? Scalar::get_half()
                                                  : Scalar::get_float(),
      Extent(tileImages[0]->get_num_channels()))};
  auto textureType{instantiate(context, texelType, tileCountU, tileCountV)};
  auto valueTileExtents{Value::zero(textureType->tileExtentsType)};
  auto valueTileBuffers{Value::zero(textureType->tileBuffersType)};
  for (unsigned i = 0; i < tileImagePaths.size(); i++) {
    auto valueTileExtent{context.get_comptime_vector(int2(
        tileImages[i]->get_num_texels_x(), tileImages[i]->get_num_texels_y()))};
    auto valueTileBuffer{context.get_comptime_ptr(
        context.get_pointer_type(texelType), tileImages[i]->get_texels())};
    auto tileIndexU{tileImagePaths[i].tileIndexU};
    auto tileIndexV{tileImagePaths[i].tileIndexV};
    auto insertPos{tileIndexV * tileCountU + tileIndexU};
    valueTileExtents =
        emitter.insert(valueTileExtents, valueTileExtent, insertPos, srcLoc);
    valueTileBuffers =
        emitter.insert(valueTileBuffers, valueTileBuffer, insertPos, srcLoc);
  }
  // TODO Catch and rethrow for better error message?
  auto valueGamma{
      emitter.invoke(context.get_int_type(), resolvedArgs.values[1], srcLoc)};
  auto result{Value::zero(textureType)};
  result.llvmValue =
      emitter.builder.CreateInsertValue(result.llvmValue, valueGamma, {0});
  result.llvmValue = emitter.builder.CreateInsertValue(result.llvmValue,
                                                       valueTileExtents, {1});
  result.llvmValue = emitter.builder.CreateInsertValue(result.llvmValue,
                                                       valueTileBuffers, {2});
  return result;
}
//--}

//--{ TexturePtexType
TexturePtexType::TexturePtexType(Context &context) {
  displayName = "texture_ptex";
  llvmType = llvm::StructType::create(
      context,
      {context.get_void_pointer_type()->llvmType,
       context.get_void_pointer_type()->llvmType,
       context.get_int_type()->llvmType, context.get_int_type()->llvmType,
       context.get_int_type()->llvmType},
      "texture_ptex");
}

Value TexturePtexType::invoke(Emitter &emitter, const ArgumentList &args,
                              const SourceLocation &srcLoc) {
  auto &context{emitter.context};
  auto nullTexture{Value::zero(this)};
  if (args.empty() || args.is_null())
    return nullTexture;
  if (args.is_one_positional(this))
    return emitter.to_rvalue(args[0].value);
  auto params{ParameterList{
      Parameter{.type = context.get_string_type(),
                .name = "name",
                .astParam = nullptr,
                .astField = nullptr,
                .builtinDefaultValue = std::nullopt},
      Parameter{.type = context.get_auto_type(),
                .name = "gamma",
                .astParam = nullptr,
                .astField = nullptr,
                .builtinDefaultValue = context.get_comptime_int(0)}}};
  // TODO Catch and rethrow for better error message?
  auto resolvedArgs{emitter.resolve_arguments(params, args, srcLoc)};
  auto valueFileName{emitter.to_rvalue(resolvedArgs.values[0])};
  if (!valueFileName.is_comptime_string())
    srcLoc.throw_error("expected 'texture_2d' parameter 'name' to resolve to "
                       "compile-time string");
  auto fileName{valueFileName.get_comptime_string()};
  auto ptexture{context.compiler.load_ptexture(std::string(fileName), srcLoc)};
  // TODO Catch and rethrow for better error message?
  auto valueGamma{
      emitter.invoke(context.get_int_type(), resolvedArgs.values[1], srcLoc)};
  auto result{Value::zero(this)};
  result.llvmValue = emitter.builder.CreateInsertValue(
      result.llvmValue,
      context.get_comptime_ptr(context.get_void_pointer_type(),
                               ptexture.texture),
      {0});
  result.llvmValue = emitter.builder.CreateInsertValue(
      result.llvmValue,
      context.get_comptime_ptr(context.get_void_pointer_type(),
                               ptexture.textureFilter),
      {1});
  result.llvmValue = emitter.builder.CreateInsertValue(
      result.llvmValue, context.get_comptime_int(ptexture.channelCount), {2});
  result.llvmValue = emitter.builder.CreateInsertValue(
      result.llvmValue, context.get_comptime_int(ptexture.alphaIndex), {3});
  result.llvmValue =
      emitter.builder.CreateInsertValue(result.llvmValue, valueGamma, {4});
  return result;
}

Value TexturePtexType::access_field(Emitter &emitter, Value value,
                                    std::string_view name,
                                    const SourceLocation &srcLoc) {
  auto doAccess{[&](unsigned i, Type *fieldType) {
    return Value(
        value.kind, fieldType,
        value.is_lvalue()
            ? emitter.builder.CreateStructGEP(value.type->llvmType, value, i)
            : emitter.builder.CreateExtractValue(value, {i}));
  }};
  if (name == "texture") {
    return doAccess(0, emitter.context.get_void_pointer_type());
  } else if (name == "texture_filter") {
    return doAccess(1, emitter.context.get_void_pointer_type());
  } else if (name == "channel_count") {
    return doAccess(2, emitter.context.get_int_type());
  } else if (name == "alpha_index") {
    return doAccess(3, emitter.context.get_int_type());
  } else if (name == "gamma") {
    return doAccess(4, emitter.context.get_int_type());
  }
  srcLoc.throw_error(
      concat("no field ", quoted(name), " in type 'texture_ptex'"));
  return Value();
}
//--}

//--{ UnionType
UnionType::UnionType(Context &context, llvm::SmallVector<Type *> caseTys)
    : caseTypes(std::move(caseTys)) {
  auto caseTypeNames{llvm::SmallVector<llvm::StringRef>{}};
  for (auto caseType : caseTypes) {
    SMDL_SANITY_CHECK(caseType);
    SMDL_SANITY_CHECK(!caseType->is_abstract());
    requiredAlign = std::max(requiredAlign, context.get_align_of(caseType));
    requiredSize = std::max(requiredSize, context.get_size_of(caseType));
    if (caseType->is_void())
      displayName = "?"; // Optional begins with "?" in front of parentheses
    else
      caseTypeNames.push_back(caseType->displayName);
  }

  // Assemble type name.
  std::sort(caseTypeNames.begin(), caseTypeNames.end());
  displayName += '(';
  for (size_t i = 0; i < caseTypeNames.size(); i++) {
    displayName += caseTypeNames[i].str();
    if (i + 1 < caseTypeNames.size())
      displayName += " | ";
  }
  displayName += ')';

  // Determine LLVM type.
  uint64_t numWords{requiredSize / 8};
  if (requiredSize % 8 != 0)
    numWords++;
  llvmType = llvm::StructType::create(
      {llvm::FixedVectorType::get(llvm::Type::getInt64Ty(context), numWords),
       context.get_int_type()->llvmType},
      "union_t");
  SMDL_SANITY_CHECK(requiredAlign <= context.get_align_of(this));
}

Value UnionType::invoke(Emitter &emitter, const ArgumentList &args,
                        const SourceLocation &srcLoc) {
  if (args.empty() || args.is_null()) {
    if (!is_optional_union())
      srcLoc.throw_error(
          concat("cannot zero construct non-optional union type ",
                 quoted(displayName)));
    auto result{Value::zero(this)};
    result.llvmValue = emitter.builder.CreateInsertValue(
        result.llvmValue,
        emitter.context.get_comptime_int(caseTypes.size() - 1), {1U});
    return result;
  }
  if (args.is_one_positional(this)) {
    return emitter.to_rvalue(args[0].value);
  }
  if (args.is_one_positional()) {
    auto arg{args[0].value};
    if (auto argUnionType{llvm::dyn_cast<UnionType>(arg.type)}) {
      auto lvArg{emitter.to_lvalue(arg)};
      auto lv{emitter.create_alloca(this, "")};
      emitter.builder.CreateStore(Value::zero(this), lv);
      emitter.builder.CreateMemCpy(
          lv, llvm::Align(emitter.context.get_align_of(this)), //
          lvArg, llvm::Align(emitter.context.get_align_of(argUnionType)),
          std::min(requiredSize, argUnionType->requiredSize));
      if (!arg.is_lvalue())
        emitter.create_lifetime_end(lvArg);
      auto index{emitter.to_rvalue(emitter.access_index(
          emitter.context.get_comptime_union_index_map(argUnionType, this),
          emitter.access_field(arg, "#idx", srcLoc), srcLoc))};
      emitter.builder.CreateStore(index,
                                  emitter.access_field(lv, "#idx", srcLoc));
      auto rv{emitter.to_rvalue(lv)};
      emitter.create_lifetime_end(lv);
      if (!has_all_case_types(argUnionType)) {
        auto [blockFail, blockPass] =
            emitter.create_blocks<2>("union_conversion", {".fail", ".pass"});
        emitter.builder.CreateCondBr(
            emitter.emit_op(BINOP_CMP_LT, index,
                            emitter.context.get_comptime_int(0), srcLoc),
            blockFail, blockPass);
        emitter.builder.SetInsertPoint(blockFail);
        emitter.emit_panic(
            emitter.context.get_comptime_string("union conversion failed"),
            srcLoc);
        emitter.builder.CreateBr(blockPass);
        emitter.builder.SetInsertPoint(blockPass);
      }
      return rv;
    } else {
      if (!has_case_type(arg.type))
        srcLoc.throw_error(concat("cannot construct union ",
                                  quoted(displayName), " from ",
                                  quoted(arg.type->displayName)));
      auto i{get_case_type_index(arg.type)};
      auto lv{emitter.create_alloca(this, "union.lv")};
      emitter.create_lifetime_start(lv);
      emitter.builder.CreateStore(Value::zero(this), lv); // zeroinitializer
      emitter.builder.CreateStore(emitter.to_rvalue(arg), lv);
      emitter.builder.CreateStore(emitter.context.get_comptime_int(i),
                                  emitter.access_field(lv, "#idx", srcLoc));
      auto rv{emitter.to_rvalue(lv)};
      emitter.create_lifetime_end(lv);
      return rv;
    }
  }
  srcLoc.throw_error(concat("cannot construct union ", quoted(displayName),
                            " from ", quoted(std::string(args))));
  return Value();
}

Value UnionType::access_field(Emitter &emitter, Value value,
                              std::string_view name,
                              const SourceLocation &srcLoc) {
  if (name == "#ptr")
    return RValue(
        emitter.context.get_pointer_type(emitter.context.get_void_type()),
        emitter.builder.CreateStructGEP(llvmType, emitter.to_lvalue(value), 0));
  if (name == "#idx")
    return value.is_lvalue()
               ? LValue(emitter.context.get_int_type(),
                        emitter.builder.CreateStructGEP(llvmType, value, 1))
               : RValue(emitter.context.get_int_type(),
                        emitter.builder.CreateExtractValue(value, {1U}));
  if (has_field(name)) {
    if (value.is_rvalue()) {
      auto lv{emitter.to_lvalue(value)};
      auto rv{emitter.to_rvalue(access_field(emitter, lv, name, srcLoc))};
      emitter.create_lifetime_end(lv);
      return rv;
    }
    // Access unique optionals unsafely without switching. This mimics
    // pointer semantics. Note that the void type is guaranteed to be
    // at the end, so we know the non-void type is `caseTypes[0]`.
    if (caseTypes.size() == 2 && caseTypes.back()->is_void()) {
      return emitter.access_field(
          LValue(caseTypes[0],
                 emitter.builder.CreateStructGEP(llvmType, value, 0)),
          name, srcLoc);
    } else {
      return emitter.emit_visit(value, srcLoc, [&](Value value) {
        return emitter.access_field(value, name, srcLoc);
      });
    }
  }
  srcLoc.throw_error(
      concat("no field ", quoted(name), " in union ", quoted(displayName)));
  return Value();
}

llvm::SmallVector<Type *>
UnionType::canonicalize_types(llvm::ArrayRef<Type *> types) {
  auto caseTypes{llvm::SmallVector<Type *>{}};
  for (auto type : types) {
    SMDL_SANITY_CHECK(type);
    SMDL_SANITY_CHECK(!type->is_abstract());
    if (auto unionType{llvm::dyn_cast<UnionType>(type)})
      caseTypes.insert(caseTypes.end(), unionType->caseTypes.begin(),
                       unionType->caseTypes.end());
    else
      caseTypes.push_back(type);
  }
  std::sort(caseTypes.begin(), caseTypes.end());
  auto itr{std::unique(caseTypes.begin(), caseTypes.end())};
  caseTypes.erase(itr, caseTypes.end());
  // If void is present, sort it to the end. This guarantees an optional union
  // has the same non-void index as its non-optional version.
  std::sort(caseTypes.begin(), caseTypes.end(),
            [](auto lhs, auto rhs) { return rhs->is_void() || lhs < rhs; });
  return caseTypes;
}
//--}

Value VoidType::invoke(Emitter &emitter, const ArgumentList &args,
                       const SourceLocation &srcLoc) {
  return RValue(emitter.context.get_void_type(), nullptr);
}

} // namespace smdl
