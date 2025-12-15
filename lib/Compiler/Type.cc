// vim:foldmethod=marker:foldlevel=0:fmr=--{,--}
#include "Type.h"
#include "Emitter.h"

#include "smdl/Support/Logger.h"

#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Type.h"

namespace smdl {

//--{ Type
bool Type::isArithmeticBoolean() const {
  return isArithmetic() &&
         static_cast<const ArithmeticType *>(this)->scalar.isBoolean();
}

bool Type::isArithmeticIntegral() const {
  return isArithmetic() &&
         static_cast<const ArithmeticType *>(this)->scalar.isIntegral();
}

bool Type::isArithmeticFloatingPoint() const {
  return isArithmetic() &&
         static_cast<const ArithmeticType *>(this)->scalar.isFloatingPoint();
}

bool Type::isArithmeticScalar() const {
  return isArithmetic() &&
         static_cast<const ArithmeticType *>(this)->extent.isScalar();
}

bool Type::isArithmeticVector() const {
  return isArithmetic() &&
         static_cast<const ArithmeticType *>(this)->extent.isVector();
}

bool Type::isArithmeticMatrix() const {
  return isArithmetic() &&
         static_cast<const ArithmeticType *>(this)->extent.isMatrix();
}

bool Type::isComplex(Context &context) const {
  return typeKind == TypeKind::Struct &&
         (this == context.getComplexType() ||
          static_cast<const StructType *>(this)->instanceOf ==
              context.getComplexType());
}

bool Type::isOptionalUnion() const {
  // NOTE: `Union::canonicalize_types()` always places `void` at the end!
  return isUnion() &&
         static_cast<const UnionType *>(this)->caseTypes.back()->isVoid();
}

Type *Type::getPointeeType() const {
  return isPointer() ? static_cast<const PointerType *>(this)->pointeeType
                     : nullptr;
}

Type *Type::getFirstNonPointerType() const {
  auto type{const_cast<Type *>(this)};
  while (type->isPointer())
    type = type->getPointeeType();
  return type;
}

size_t Type::getFirstNonPointerTypeDepth() const {
  size_t depth{};
  auto type{const_cast<Type *>(this)};
  while (type->isPointer()) {
    type = type->getPointeeType();
    depth++;
  }
  return depth;
}

Value Type::invoke(Emitter &emitter, const ArgumentList &args,
                   const SourceLocation &srcLoc) {
  if (args.isOnePositional(this))
    return emitter.toRValue(args[0].value);
  srcLoc.throwError("type ", Quoted(displayName),
                     " has unimplemented constructor");
  return Value();
}

Value Type::accessField(Emitter &, Value, std::string_view,
                        const SourceLocation &srcLoc) {
  srcLoc.throwError("type ", Quoted(displayName),
                     " has no field access operator");
  return Value();
}

Value Type::accessIndex(Emitter &, Value, Value, const SourceLocation &srcLoc) {
  srcLoc.throwError("type ", Quoted(displayName),
                     " has no index access operator");
  return Value();
}

Value Type::insert(Emitter &, Value, Value, unsigned,
                   const SourceLocation &srcLoc) {
  srcLoc.throwError("type ", Quoted(displayName),
                     " has unimplemented insert method");
  return Value();
}
//--}

//--{ ArithmeticType
llvm::Type *Scalar::getLLVMType(llvm::LLVMContext &context) const {
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
  llvmType = extent.getLLVMType(scalar.getLLVMType(context));
}

Value ArithmeticType::invoke(Emitter &emitter, const ArgumentList &args,
                             const SourceLocation &srcLoc) {
  if (args.empty() || args.isNull()) {
    return Value::zero(this);
  }
  if (args.isOnePositional(this)) {
    return emitter.toRValue(args[0].value);
  }
  if (extent.isScalar()) {
    if (!args.isOnePositional())
      srcLoc.throwError("scalar ", Quoted(displayName),
                         " constructor expects 1 positional argument");
    auto value{args[0].value};
    // If constructing bool from pointer, check that it is non-NULL.
    if (scalar.isBoolean() && value.type->isPointer())
      return RValue(this,
                    emitter.builder.CreateIsNotNull(emitter.toRValue(value)));
    // If constructing bool from optional union, check that it is non-void.
    if (scalar.isBoolean() && value.type->isOptionalUnion())
      return RValue(
          this,
          emitter.builder.CreateICmpNE(
              emitter.toRValue(emitter.accessField(value, "#idx", srcLoc)),
              emitter.context.getComptimeInt(
                  int(static_cast<UnionType *>(value.type)->caseTypes.size() -
                      1))));
    // If constructing from another scalar or enum type, cast the
    // underlying LLVM representation.
    if (value.type->isArithmeticScalar() || value.type->isEnum())
      return RValue(this, llvmEmitCast(emitter.builder, emitter.toRValue(value),
                                       llvmType));
  } else if (extent.isVector()) {
    auto dim{size_t(extent.getVectorSize())};
    if (args.isOnePositional()) {
      auto value{args[0].value};
      // If constructing from scalar, splat the scalar value.
      // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      // float3(2.7) // == float3(2.7, 2.7, 2.7)
      // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if (value.type->isArithmeticScalar())
        return RValue(this, emitter.builder.CreateVectorSplat(
                                extent.getVectorSize(),
                                emitter.invoke(getScalarType(emitter.context),
                                               value, srcLoc)));
      // If constructing from vector of the same size, cast the
      // underlying LLVM representation.
      // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      // int4(float4(3.0, 0.2, 0.1, 5.4)) // Cast components to int
      // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if (value.type->isArithmeticVector() &&
          static_cast<ArithmeticType *>(value.type)->extent == extent)
        return RValue(this, llvmEmitCast(emitter.builder,
                                         emitter.toRValue(value), llvmType));
      // If constructing from a pointer to the scalar type, load from the
      // pointer. Assume the pointer is only as aligned as the scalar type
      // itself!
      if (value.type->isPointer() &&
          value.type->getPointeeType() == getScalarType(emitter.context))
        return RValue(this, emitter.builder.CreateAlignedLoad(
                                llvmType, emitter.toRValue(value),
                                llvm::Align(emitter.context.getAlignOf(
                                    getScalarType(emitter.context)))));
      // If constructing from color and this is a 3-dimensional vector,
      // delegate to the `_color_to_rgb` function in the `api` module.
      if (value.type == emitter.context.getColorType() && dim == 3)
        return invoke(
            emitter,
            emitter.emitCall(emitter.context.getKeyword("_color_to_rgb"), value,
                             srcLoc),
            srcLoc);
    }
    // From scalars
    auto canConstructFromScalars{[&] {
      if (!(dim == args.size() && args.isAllTrue([](auto &arg) {
            return arg.value.type->isArithmeticScalar();
          })))
        return false;
      return (dim == 2 && args.isOnlyTheseNames({"x", "y"})) ||
             (dim == 3 && args.isOnlyTheseNames({"x", "y", "z"})) ||
             (dim == 4 && args.isOnlyTheseNames({"x", "y", "z", "w"})) ||
             !args.isAnyNamed();
    }()};
    if (canConstructFromScalars) {
      auto values{llvm::SmallVector<Value>{}};
      // If vector size is 2, 3, or 4, possibly resolve the argument names.
      // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      // float4(w: 3.0, x: 5.0, y: 7.0, z: 9.0)
      // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if (2 <= dim && dim <= 4) {
        auto scalarType{getScalarType(emitter.context)};
        auto params{ParameterList{}};
        params.push_back(Parameter{scalarType, "x"});
        params.push_back(Parameter{scalarType, "y"});
        if (dim >= 3)
          params.push_back(Parameter{scalarType, "z"});
        if (dim >= 4)
          params.push_back(Parameter{scalarType, "w"});
        values =
            std::move(emitter.resolveArguments(params, args, srcLoc).values);
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
      if (!args.isAllTrue([](auto &arg) {
            return arg.isPositional() &&
                   (arg.value.type->isArithmeticScalar() ||
                    arg.value.type->isArithmeticVector());
          }))
        return false;
      size_t impliedDim{};
      for (auto &arg : args)
        impliedDim += static_cast<ArithmeticType *>(arg.value.type)
                          ->extent.getVectorSize();
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
        SMDL_SANITY_CHECK(arg.value.type->isArithmeticScalar() ||
                          arg.value.type->isArithmeticVector());
        auto arithType{static_cast<ArithmeticType *>(arg.value.type)};
        auto value{emitter.invoke(
            arithType->getWithDifferentScalar(emitter.context, scalar), arg,
            srcLoc)};
        if (arg.value.type->isArithmeticScalar()) {
          result = emitter.insert(result, value, i++, srcLoc);
        } else {
          for (size_t j = 0; j < arithType->extent.getVectorSize(); j++)
            result = emitter.insert(
                result, emitter.accessIndex(value, j, srcLoc), i++, srcLoc);
        }
      }
      return result;
    }
  } else if (extent.isMatrix()) {
    auto scalarType{getScalarType(emitter.context)};
    auto columnType{getColumnType(emitter.context)};
    auto construct{[&](auto &&func) {
      auto result{Value::zero(this)};
      for (unsigned j = 0; j < extent.numCols; j++)
        result = emitter.insert(result, std::invoke(func, j), j, srcLoc);
      return result;
    }};
    if (args.isOnePositional()) {
      const bool canConstructFromScalar{
          args[0].value.type->isArithmeticScalar()};
      if (canConstructFromScalar)
        return construct(
            [&, scalar = emitter.invoke(scalarType, args, srcLoc)](unsigned j) {
              auto column{Value::zero(columnType)};
              if (j < extent.numRows)
                column = emitter.insert(column, scalar, j, srcLoc);
              return column;
            });
      const bool canConstructFromMatrix{
          args[0].value.type->isArithmeticMatrix() &&
          static_cast<ArithmeticType *>(args[0].value.type)->extent == extent};
      if (canConstructFromMatrix)
        return construct([&](unsigned j) {
          return emitter.accessIndex(args[0].value, j, srcLoc);
        });
    }
    if (args.isAllPositional()) {
      const bool canConstructFromColumns{
          args.size() == extent.numCols && args.isAllTrue([&](auto &arg) {
            return arg.value.type->isArithmeticVector() &&
                   static_cast<ArithmeticType *>(arg.value.type)
                           ->extent.numRows == extent.numRows;
          })};
      if (canConstructFromColumns)
        return construct([&](unsigned j) { return args[j].value; });

      const bool canConstructFromScalars{
          args.size() == size_t(extent.numCols * extent.numRows) &&
          args.isAllTrue(
              [](auto &arg) { return arg.value.type->isArithmeticScalar(); })};
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
  srcLoc.throwError("cannot construct ", Quoted(displayName), " from ",
                     Quoted(std::string(args)));
  return Value();
}

bool ArithmeticType::hasField(std::string_view name) {
  if (!extent.isScalar()) {
    if (name.size() == 1)
      return toIndex(name[0]).has_value();
    if (extent.isVector())
      return toIndexSwizzle(name).has_value();
  }
  return false;
}

Value ArithmeticType::accessField(Emitter &emitter, Value value,
                                  std::string_view name,
                                  const SourceLocation &srcLoc) {
  if (extent.isScalar()) {
    srcLoc.throwError("scalar ", Quoted(displayName),
                       " has no field access operator");
  }
  if (name.size() == 1) {
    if (auto i{toIndex(name[0])}) {
      auto result{accessIndex(emitter, value,
                              emitter.context.getComptimeInt(int(*i)), srcLoc)};
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
  if (extent.isVector()) {
    if (auto iMask{toIndexSwizzle(name)}) {
      auto result{RValue(
          emitter.context.getArithmeticType(scalar, Extent(iMask->size())),
          emitter.builder.CreateShuffleVector(emitter.toRValue(value),
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
  }
  srcLoc.throwError("no field ", Quoted(name), " in ", Quoted(displayName));
  return Value();
}

Value ArithmeticType::accessIndex(Emitter &emitter, Value value, Value i,
                                  const SourceLocation &srcLoc) {
  if (extent.isScalar())
    srcLoc.throwError("scalar ", Quoted(displayName),
                       " has no index access operator");
  if (value.isRValue()) {
    if (i.isComptimeInt()) {
      unsigned iNow{i.getComptimeInt()};
      if (extent.isVector()) {
        return RValue(getScalarType(emitter.context),
                      emitter.builder.CreateExtractElement(value, iNow));
      } else {
        return RValue(getColumnType(emitter.context),
                      emitter.builder.CreateExtractValue(value, {iNow}));
      }
    } else {
      auto lv{emitter.toLValue(value)};
      auto rv{emitter.toRValue(accessIndex(emitter, lv, i, srcLoc))};
      emitter.createLifetimeEnd(lv);
      return rv;
    }
  } else {
    if (extent.isVector()) {
      auto scalarType{getScalarType(emitter.context)};
      return LValue(
          scalarType,
          emitter.builder.CreateGEP(
              llvm::ArrayType::get(scalarType->llvmType, extent.numRows), value,
              {emitter.builder.getInt32(0), i.llvmValue}));
    } else {
      return LValue(
          getColumnType(emitter.context),
          emitter.builder.CreateGEP(
              llvmType, value, {emitter.builder.getInt32(0), i.llvmValue}));
    }
  }
  return Value();
}

Value ArithmeticType::insert(Emitter &emitter, Value value, Value elem,
                             unsigned i, const SourceLocation &srcLoc) {
  if (extent.isVector())
    return RValue(
        this,
        emitter.builder.CreateInsertElement(
            emitter.toRValue(value),
            emitter.invoke(getScalarType(emitter.context), elem, srcLoc), i));
  if (extent.isMatrix())
    return RValue(
        this, //
        emitter.builder.CreateInsertValue(
            emitter.toRValue(value),
            emitter.invoke(getColumnType(emitter.context), elem, srcLoc), {i}));
  srcLoc.throwError("cannot insert into ", Quoted(displayName));
  return Value();
}

ArithmeticType *ArithmeticType::getWithDifferentScalar(Context &context,
                                                       Scalar newScalar) {
  return static_cast<ArithmeticType *>(
      context.getArithmeticType(newScalar, extent));
}

ArithmeticType *ArithmeticType::getWithDifferentExtent(Context &context,
                                                       Extent newExtent) {
  return static_cast<ArithmeticType *>(
      context.getArithmeticType(scalar, newExtent));
}

ArithmeticType *ArithmeticType::getCommonType(Context &context,
                                              ArithmeticType *otherType) {
  return static_cast<ArithmeticType *>(
      context.getArithmeticType(scalar.getCommon(otherType->scalar),
                                extent.getCommon(otherType->extent)));
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
    auto result{Value::zero(emitter.context.getArrayType(value0.type, size))};
    for (uint32_t i = 0; i < size; i++)
      result = emitter.insert(result, value0, i, srcLoc);
    return result;
  }
  if (args.isNull()) {
    if (isAbstract())
      srcLoc.throwError("cannot zero construct abstract array ",
                         Quoted(displayName));
    return Value::zero(this);
  }
  if (args.isOnePositional(this)) {
    return emitter.toRValue(args[0].value);
  }
  if (args.isAllPositional() && args.size() == size) {
    if (isAbstract()) {
      auto argElemType{emitter.context.getCommonType(
          args.getTypes(), /*defaultToUnion=*/true, srcLoc)};
      if (!emitter.context.isPerfectlyConvertible(argElemType, elemType))
        srcLoc.throwError("cannot construct abstract array ",
                           Quoted(displayName), " from element ",
                           Quoted(argElemType->displayName));
      return emitter.invoke(emitter.context.getArrayType(argElemType, size),
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
  if (args.isOnePositional()) {
    auto value{args[0].value};
    // If constructing from array type of identical size but different element
    // type, try to convert each element.
    if (auto arrType{llvm::dyn_cast<ArrayType>(value.type)};
        arrType && arrType->size == size) {
      auto elems{emitter.accessEveryIndex(
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
                        llvmType, emitter.toRValue(value),
                        llvm::Align(emitter.context.getAlignOf(elemType))));
    }
  }
  srcLoc.throwError("cannot construct ", Quoted(displayName), " from ",
                     Quoted(std::string(args)));
  return Value();
}

Value ArrayType::accessField(Emitter &emitter, Value value,
                             std::string_view name,
                             const SourceLocation &srcLoc) {
  SMDL_SANITY_CHECK(!isAbstract());
  if (hasField(name)) {
    if (!value.isLValue()) {
      auto lv{emitter.toLValue(value)};
      auto rv{emitter.toRValue(accessField(emitter, lv, name, srcLoc))};
      emitter.createLifetimeEnd(lv);
      return rv;
    }
    // The behavior here is to construct an `auto[]` by accessing
    // the field on each of element in the array.
    auto elems{emitter.accessEveryIndex(
        value, size, srcLoc, [&](unsigned, Value elem) {
          return emitter.accessField(elem, name, srcLoc);
        })};
    return emitter.invoke(
        emitter.context.getArrayType(emitter.context.getAutoType(), size),
        llvm::ArrayRef<Value>(elems), srcLoc);
  }
  srcLoc.throwError("no field ", Quoted(name), " in array type ",
                     Quoted(displayName));
  return Value();
}

Value ArrayType::accessIndex(Emitter &emitter, Value value, Value i,
                             const SourceLocation &srcLoc) {
  SMDL_SANITY_CHECK(!isAbstract());
  if (i.isComptimeInt() && value.isRValue()) {
    return RValue(elemType, emitter.builder.CreateExtractValue(
                                value, {unsigned(i.getComptimeInt())}));
  } else {
    if (!value.isLValue()) {
      auto lv{emitter.toLValue(value)};
      auto rv{emitter.toRValue(accessIndex(emitter, lv, i, srcLoc))};
      emitter.createLifetimeEnd(lv);
      return rv;
    }
    i = emitter.toRValue(i);
    return LValue(elemType, emitter.builder.CreateGEP(
                                llvmType, value,
                                {emitter.builder.getInt32(0), i.llvmValue}));
  }
}

Value ArrayType::insert(Emitter &emitter, Value value, Value elem, unsigned i,
                        const SourceLocation &srcLoc) {
  SMDL_SANITY_CHECK(!isAbstract());
  SMDL_SANITY_CHECK(i < size);
  return RValue(value.type, emitter.builder.CreateInsertValue(
                                emitter.toRValue(value),
                                emitter.invoke(elemType, elem, srcLoc), {i}));
}

ArrayType *ArrayType::getWithDifferentElementType(Context &context,
                                                  Type *newElemType) {
  return context.getArrayType(newElemType, size);
}

ArrayType *ArrayType::getWithDifferentSize(Context &context, uint32_t newSize) {
  return context.getArrayType(elemType, newSize);
}
//--}

//--{ AutoType
Value AutoType::invoke(Emitter &emitter, const ArgumentList &args,
                       const SourceLocation &srcLoc) {
  // If no arguments or null, return void.
  if (args.empty() || args.isNull()) {
    return RValue(emitter.context.getVoidType(), nullptr);
  }
  // If one positional argument, return it as an rvalue.
  if (args.isOnePositional()) {
    return emitter.toRValue(args[0]);
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
  if (args.isAllTrue([&](auto arg) {
        return arg.isPositional() && (arg.value.type->isArithmeticScalar() ||
                                      arg.value.type->isArithmeticVector());
      })) {
    Scalar scalar{static_cast<ArithmeticType *>(args[0].value.type)->scalar};
    size_t extent{};
    for (auto arg : args) {
      auto arithType{static_cast<ArithmeticType *>(arg.value.type)};
      scalar = scalar.getCommon(arithType->scalar);
      extent += arithType->extent.numRows;
    }
    return emitter.invoke(emitter.context.getArithmeticType(scalar, extent),
                          args, srcLoc);
  }
  // TODO Infer struct type?
  srcLoc.throwError("cannot construct 'auto' from ",
                     Quoted(std::string(args)));
  return Value();
}
//--}

//--{ ColorType
ColorType::ColorType(Context &context) {
  displayName = "color";
  llvmType = Scalar::getFloat().getLLVMType(context);
  llvmType =
      llvm::FixedVectorType::get(llvmType, context.compiler.wavelengthBaseMax);
  wavelengthBaseMax = context.compiler.wavelengthBaseMax;
}

Value ColorType::invoke(Emitter &emitter, const ArgumentList &args,
                        const SourceLocation &srcLoc) {
  auto &context{emitter.context};
  if (args.empty() || args.isNull()) {
    return Value::zero(this);
  }
  if (args.isOnePositional(this)) {
    return emitter.toRValue(args[0].value);
  }
  if (args.isOnePositional()) {
    auto value{args[0].value};
    if (value.type->isArithmeticScalar())
      return RValue(this, emitter.builder.CreateVectorSplat(
                              wavelengthBaseMax,
                              emitter.invoke(emitter.context.getFloatType(),
                                             value, srcLoc)));
    if (value.type->isArithmeticVector() &&
        static_cast<ArithmeticType *>(value.type)->extent.numRows ==
            wavelengthBaseMax)
      return RValue(this, llvmEmitCast(emitter.builder, emitter.toRValue(value),
                                       llvmType));
    if (value.type->isPointer() &&
        value.type->getPointeeType() == emitter.context.getFloatType())
      return RValue(this, emitter.builder.CreateAlignedLoad(
                              llvmType, emitter.toRValue(value),
                              llvm::Align(alignof(float))));
    if (value.type == context.getFloatType(Extent(3)))
      return emitter.emitCall(context.getKeyword("_rgb_to_color"), value,
                              srcLoc);
    if (value.type == context.getSpectralCurveType())
      return emitter.emitCall(context.getKeyword("_spectral_curve_to_color"),
                              value, srcLoc);
  }
  if (args.size() <= 3 && args.isOnlyTheseNames({"r", "g", "b"})) {
    auto params{ParameterList{
        Parameter{context.getFloatType(), "r", {}, {}, {}, true},
        Parameter{context.getFloatType(), "g", {}, {}, {}, true},
        Parameter{context.getFloatType(), "b", {}, {}, {}, true}}};
    if (emitter.canResolveArguments(params, args, srcLoc)) {
      auto resolved{emitter.resolveArguments(params, args, srcLoc)};
      return emitter.emitCall(
          context.getKeyword("_rgb_to_color"),
          emitter.invoke(context.getFloatType(Extent(3)),
                         llvm::ArrayRef<Value>(resolved.values), srcLoc),
          srcLoc);
    }
  }
  if (args.size() == 2 &&
      args.isOnlyTheseNames({"wavelengths", "amplitudes"})) {
    auto floatArrayType{
        context.getInferredSizeArrayType(context.getFloatType())};
    auto params{ParameterList{
        Parameter{floatArrayType, "wavelengths", {}, {}, {}, false},
        Parameter{floatArrayType, "amplitudes", {}, {}, {}, false}}};
    if (emitter.canResolveArguments(params, args, srcLoc)) {
      auto resolved{emitter.resolveArguments(params, args, srcLoc)};
      auto arrayType0{llvm::dyn_cast<ArrayType>(resolved.values[0].type)};
      auto arrayType1{llvm::dyn_cast<ArrayType>(resolved.values[1].type)};
      if (!(arrayType0 && arrayType1 && arrayType0 == arrayType1))
        srcLoc.throwError(
            "expected wavelength and amplitude arrays to be same size");
      return emitter.emitCall(
          context.getKeyword("_samples_to_color"),
          ArgumentList{context.getComptimeInt(int(arrayType0->size)),
                       resolved.values[0], resolved.values[1]},
          srcLoc);
    }
  }
  srcLoc.throwError("cannot construct 'color' from ",
                     Quoted(std::string(args)));
  return Value();
}

Value ColorType::accessIndex(Emitter &emitter, Value value, Value i,
                             const SourceLocation &srcLoc) {
  if (value.isRValue()) {
    if (i.isComptimeInt()) {
      return RValue(
          getArithmeticScalarType(emitter.context),
          emitter.builder.CreateExtractElement(value, i.getComptimeInt()));
    } else {
      auto lv{emitter.toLValue(value)};
      auto rv{emitter.toRValue(accessIndex(emitter, lv, i, srcLoc))};
      emitter.createLifetimeEnd(lv);
      return rv;
    }
  } else {
    auto scalarType{getArithmeticScalarType(emitter.context)};
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
                    emitter.toRValue(value),
                    emitter.invoke(getArithmeticScalarType(emitter.context),
                                   elem, srcLoc),
                    i));
}

ArithmeticType *ColorType::getArithmeticScalarType(Context &context) {
  return static_cast<ArithmeticType *>(
      context.getArithmeticType(Scalar::getFloat()));
}

ArithmeticType *ColorType::getArithmeticVectorType(Context &context) {
  return static_cast<ArithmeticType *>(
      context.getArithmeticType(Scalar::getFloat(), Extent(wavelengthBaseMax)));
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
    srcLoc.throwError("cannot default construct ", Quoted(displayName));
  }
  if (args.isOnePositional() && unionType->hasCaseType(args[0].value.type)) {
    return emitter.toRValue(args[0].value);
  }
  srcLoc.throwError("cannot construct ", Quoted(displayName), " from ",
                     Quoted(std::string(args)));
  return Value();
}
//--}

//--{ EnumType
void EnumType::initialize(Emitter &emitter) {
  auto &context{emitter.context};
  llvmType = context.getIntType()->llvmType;
  emitter.declareCrumb(decl.name, &decl, context.getComptimeMetaType(this));
  auto lastValue{Value()};
  for (auto &declarator : decl.declarators) {
    auto &name{declarator.name};
    auto value{[&]() {
      if (declarator.exprInit)
        return emitter.invoke(context.getIntType(),
                              emitter.emit(declarator.exprInit), name.srcLoc);
      if (lastValue)
        return emitter.emitOp(BINOP_ADD, lastValue, context.getComptimeInt(1),
                              name.srcLoc);
      return Value::zero(context.getIntType());
    }()};
    if (!value.isComptimeInt())
      name.srcLoc.throwError("expected ", Quoted(name),
                              " initializer to resolve to compile-time int");
    emitter.declareCrumb(name.srcName, &declarator, RValue(this, value));
    declarator.llvmConst = static_cast<llvm::ConstantInt *>(value.llvmValue);
    lastValue = value;
  }

  // Initialize the to-string LLVM function. This is just a big switch.
  auto returnType{emitter.context.getStringType()};
  llvmFuncToString = emitter.createFunction(
      displayName + ".to_string", /*isPure=*/true, returnType,
      {Parameter{this, "value"}}, decl.name.srcLoc, [&]() {
        auto valueName{std::string_view("value")};
        auto value{emitter.resolveIdentifier(valueName, decl.srcLoc)};
        auto blockDefault{emitter.createBlock("switch.default")};
        auto switchInst{emitter.builder.CreateSwitch(emitter.toRValue(value),
                                                     blockDefault)};
        auto switchUniq{llvm::DenseSet<llvm::Value *>{}};
        for (unsigned i = 0; i < decl.declarators.size(); i++) {
          if (auto [itr, inserted] =
                  switchUniq.insert(decl.declarators[i].llvmConst);
              !inserted)
            continue; // Skip repeats!
          auto blockCase{
              emitter.createBlock("switch.case." + std::to_string(i))};
          switchInst->addCase(decl.declarators[i].llvmConst, blockCase);
          emitter.builder.SetInsertPoint(blockCase);
          emitter.returns.push_back({emitter.context.getComptimeString(
                                         decl.declarators[i].name.srcName),
                                     blockCase,
                                     decl.declarators[i].name.srcLoc});
          emitter.builder.CreateBr(emitter.labelReturn.block);
        }
        llvmMoveBlockToEnd(blockDefault);
        emitter.builder.SetInsertPoint(blockDefault);
        emitter.returns.push_back({emitter.context.getComptimeString(""),
                                   blockDefault, decl.name.srcLoc});
        emitter.builder.CreateBr(emitter.labelReturn.block);
      });
}

Value EnumType::invoke(Emitter &emitter, const ArgumentList &args,
                       const SourceLocation &srcLoc) {
  if (args.empty() || args.isNull()) {
    return Value::zero(this);
  } else if (args.isOnePositional(this)) {
    return emitter.toRValue(args[0].value);
  } else if (args.isOnePositional()) {
    auto value{args[0].value};
    if ((value.type->isArithmeticScalar() &&
         value.type->isArithmeticIntegral()) ||
        value.type->isEnum())
      return RValue(this, llvmEmitCast(emitter.builder, emitter.toRValue(value),
                                       llvmType));
  }
  srcLoc.throwError("cannot construct ", Quoted(displayName), " from ",
                     Quoted(std::string(args)));
  return Value();
}
//--}

//--{ FunctionType
void FunctionType::initialize(Emitter &emitter) {
  auto &context{emitter.context};
  // Find previous overload.
  if (auto prev{emitter.resolveIdentifier(decl.name, decl.srcLoc,
                                          /*voidByDefault=*/true)};
      !prev.isVoid()) {
    auto prevType{prev.isComptimeMetaType(context)
                      ? prev.getComptimeMetaType(context, decl.srcLoc)
                      : nullptr};
    if (!prevType || !prevType->isFunction())
      decl.srcLoc.throwError("function ", Quoted(declName),
                              " shadows non-function");
    if (static_cast<FunctionType *>(prevType)->isVariant())
      decl.srcLoc.throwError("function ", Quoted(declName),
                              " must not overload function variant");
    if (static_cast<FunctionType *>(prevType)->isForeign())
      decl.srcLoc.throwError("function ", Quoted(declName),
                              " must not overload '@(foreign)' function");
    if (decl.is_variant())
      decl.srcLoc.throwError("function variant ", Quoted(declName),
                              " must not overload another function");
    if (decl.has_attribute("foreign"))
      decl.srcLoc.throwError(
          "function ", Quoted(declName),
          " declared '@(foreign)' must not overload another function");
    prevOverload = static_cast<FunctionType *>(prevType);
    prevOverload->nextOverload = this;
  }
  // Declare crumb.
  params.lastCrumb =
      emitter.declareCrumb(decl.name, &decl, context.getComptimeMetaType(this));
  // Initialize return type and parameters.
  returnType =
      emitter.emit(decl.returnType).getComptimeMetaType(context, decl.srcLoc);
  for (auto &param : decl.params)
    params.push_back(
        Parameter{.type = emitter.emit(param.type)
                              .getComptimeMetaType(context, param.name.srcLoc),
                  .name = param.name,
                  .astParam = &param,
                  .astField = nullptr,
                  .builtinDefaultValue = {}});
  if (decl.has_attribute("foreign")) {
    if (!params.isConcrete())
      decl.srcLoc.throwError(
          "function ", Quoted(declName),
          " declared '@(foreign)' must have concrete parameters");
    if (decl.definition)
      decl.srcLoc.throwError(
          "function ", Quoted(declName),
          " declared '@(foreign)' must not have definition");
    auto paramTypes{params.getTypes()};
    instantiate(emitter, llvm::SmallVector<Type *>(paramTypes.begin(),
                                                   paramTypes.end()));
  }
  // If this is declared `@(visible)`, we compile it immediately to
  // guarantee that the symbol exists for the C++ runtime.
  if (decl.has_attribute("visible")) {
    if (!params.isConcrete())
      decl.srcLoc.throwError(
          "function ", Quoted(declName),
          " declared '@(visible)' must have concrete parameters");
    if (!decl.definition)
      decl.srcLoc.throwError("function ", Quoted(declName),
                              " declared '@(visible)' must have definition");
    auto paramTypes{params.getTypes()};
    instantiate(emitter, llvm::SmallVector<Type *>(paramTypes.begin(),
                                                   paramTypes.end()));
  }
  // If this is a function with no parameters that returns `material`,
  // it is a material definition!
  if (returnType == context.materialType &&
      (params.empty() || params.allDefaultInitializers())) {
    if (decl.has_attribute("pure"))
      decl.srcLoc.throwError("material ", Quoted(declName),
                              " must not be declared '@(pure)'");
    if (decl.has_attribute("macro"))
      decl.srcLoc.throwError("material ", Quoted(declName),
                              " must not be declared '@(macro)'");
    isMaterial = true;
    initializeJitMaterialFunctions(emitter);
  }
}

Value FunctionType::invoke(Emitter &emitter, const ArgumentList &args,
                           const SourceLocation &srcLoc) {
  auto func{resolveOverload(emitter, args, srcLoc)};
  if (func->isVariant()) {
    auto result{Value()};
    SMDL_PRESERVE(emitter.crumb);
    emitter.crumb = func->params.lastCrumb;
    emitter.handleScope(nullptr, nullptr, [&]() {
      emitter.currentModule = func->decl.srcLoc.module_;
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
        if (!patchedArgs.hasName(astArg.name.srcName)) {
          patchedArgs.push_back(Argument{astArg.name.srcName,
                                         emitter.emit(astArg.expr), &astArg});
        }
      }
      auto callee{emitter.emit(astCall->expr)};
      result = emitter.emitCall(callee, patchedArgs, srcLoc);
      result = emitter.invoke(decl.returnType->type, result, srcLoc);
    });
    return result;
  }
  auto resolved{emitter.resolveArguments(func->params, args, srcLoc)};
  if (auto impliedVisitArgs{resolved.GetImpliedVisitArguments()})
    return emitter.emitCall(emitter.context.getComptimeMetaType(this),
                            *impliedVisitArgs, srcLoc);
  if (func->isMacro()) {
    ++macroRecursionDepth;
    if (macroRecursionDepth >= 1024)
      srcLoc.throwError("call to ", Quoted(func->declName),
                         " exceeds compile-time recursion limit 1024");
    SMDL_PRESERVE(emitter.crumb);
    emitter.crumb = func->params.lastCrumb;
    auto result{emitter.createFunctionImplementation(
        func->decl.name, func->isPure() || !emitter.state, func->returnType,
        func->params, resolved.values, srcLoc, [&]() {
          if (func->decl.has_attribute("fastmath"))
            emitter.builder.setFastMathFlags(llvm::FastMathFlags::getFast());
          emitter.currentModule = func->decl.srcLoc.module_;
          emitter.emit(func->decl.definition);
        })};
    --macroRecursionDepth;
    return result;
  } else {
    if (!func->isPure() && !emitter.state)
      srcLoc.throwError("cannot call ", Quoted(func->declName),
                         " from '@(pure)' context");
    auto &instance{func->instantiate(emitter, resolved.GetValueTypes())};
    auto llvmArgs{llvm::SmallVector<llvm::Value *>{}};
    if (!func->isPure())
      llvmArgs.push_back(emitter.state);
    llvmArgs.insert(llvmArgs.end(),          //
                    resolved.values.begin(), //
                    resolved.values.end());
    return RValue(instance.returnType, emitter.builder.CreateCall(
                                           instance.llvmFunc->getFunctionType(),
                                           instance.llvmFunc, llvmArgs));
  }
}

FunctionType *FunctionType::resolveOverload(Emitter &emitter,
                                            const ArgumentList &args,
                                            const SourceLocation &srcLoc) {
  if (isVariant() || isForeign()) {
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
      SMDL_SANITY_CHECK(!func->isVariant());
      auto resolved{emitter.resolveArguments(func->params, args, srcLoc,
                                             /*dontEmit=*/true)};
      overloads.push_back({func, std::move(resolved.argParams)});
    } catch (const Error &error) {
      // TODO Log?
    }
  }
  // If no matching declarations, fail!
  if (overloads.empty())
    srcLoc.throwError("function ", Quoted(declName),
                       " has no overload for arguments ",
                       Quoted(std::string(args)));
  // The lambda to determine whether the LHS set of parameter types is
  // strictly less specific than the RHS set of parameter types. This is true
  // if each and every RHS parameter type is implicitly convertible to the
  // corresponding LHS parameter type.
  auto isLessSpecific{[&](llvm::ArrayRef<const Parameter *> paramsA,
                          llvm::ArrayRef<const Parameter *> paramsB) {
    SMDL_SANITY_CHECK(paramsA.size() == paramsB.size());
    for (size_t i = 0; i < paramsA.size(); i++)
      if (!emitter.context.isImplicitlyConvertible(paramsB[i]->type,
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
      srcLoc.throwError("function ", Quoted(declName),
                         " is ambiguous for arguments ",
                         Quoted(std::string(args)));
    }
  }
  return overloads[0].func;
}

FunctionType::Instance &
FunctionType::instantiate(Emitter &emitter,
                          const llvm::SmallVector<Type *> paramTypes) {
  SMDL_SANITY_CHECK(paramTypes.size() == params.size());
  auto &inst{instances[paramTypes]};
  if (!inst.llvmFunc) {
    SMDL_SANITY_CHECK(!inst.isCompiling);
    inst.isCompiling = true;
    inst.returnType = returnType;
    if (isForeign()) {
      emitter.createFunction(inst.llvmFunc, decl.name, isPure(),
                             inst.returnType, paramTypes, params, decl.srcLoc,
                             nullptr);
    } else {
      SMDL_SANITY_CHECK(decl.definition);
      emitter.createFunction(
          inst.llvmFunc, decl.name, isPure(), inst.returnType, paramTypes,
          params, decl.srcLoc, [&] {
            if (decl.has_attribute("fastmath"))
              emitter.builder.setFastMathFlags(llvm::FastMathFlags::getFast());
            emitter.currentModule = decl.srcLoc.module_;
            emitter.emit(decl.definition);
          });
      static const std::pair<const char *, llvm::Attribute::AttrKind> attrs[] =
          {{"alwaysinline", llvm::Attribute::AlwaysInline},
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
    }
    inst.isCompiling = false;
  } else if (inst.llvmFunc->getReturnType() ==
             emitter.context.llvmIncompleteReturnTy) {
    // If the instance LLVM function has `Context::llvmIncompleteReturnTy`
    // as its return type, then the function is currently being compiled and
    // this is an attempt to invoke it recursively, which is not allowed!
    decl.srcLoc.throwError(
        "function with inferred return type must not recurse");
  }
  return inst;
}

void FunctionType::initializeJitMaterialFunctions(Emitter &emitter) {
  using namespace std::literals::string_view_literals;
  SMDL_LOG_DEBUG(std::string(decl.srcLoc), " New material ", Quoted(decl.name));
  auto &context{emitter.context};
  auto &jitMaterial{context.compiler.jitMaterials.emplace_back()};
  jitMaterial.moduleName = std::string(decl.srcLoc.getModuleName());
  jitMaterial.moduleFileName = std::string(decl.srcLoc.getModuleFileName());
  jitMaterial.lineNo = decl.srcLoc.lineNo;
  jitMaterial.materialName = std::string(decl.name.srcName);
  auto dfModule{context.getBuiltinModule("df")};
  SMDL_SANITY_CHECK(dfModule);
  Type *materialInstanceType{};
  Type *materialInstancePtrType{};
  Type *float4PtrType{context.getPointerType(context.getFloatType(4))};
  Type *float3PtrType{context.getPointerType(context.getFloatType(3))};
  Type *floatPtrType{context.getPointerType(context.getFloatType())};
  Type *intPtrType{context.getPointerType(context.getIntType())};
  auto constParameter{[](Type *type, std::string_view name) {
    return Parameter{.type = type,
                     .name = name,
                     .astParam = {},
                     .astField = {},
                     .builtinDefaultValue = {},
                     .builtinConst = true};
  }};
  {
    // Generate the evaluate function:
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // @(visible) void "material_name.evaluate"(&auto out) {
    //   *out = _material_instance(#bump_allocate(material_name()));
    // }
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    auto funcReturnType{static_cast<Type *>(context.getVoidType())};
    auto func{emitter.createFunction(
        concat(declName, ".evaluate"), /*isPure=*/false, funcReturnType,
        {constParameter(context.getVoidPointerType(), "out")}, decl.srcLoc,
        [&] {
          auto materialInstance{emitter.emitCall(
              context.getKeyword("_material_instance"),
              emitter.emitIntrinsic("bump_allocate",
                                    invoke(emitter, {}, decl.srcLoc),
                                    decl.srcLoc),
              decl.srcLoc)};
          materialInstanceType = materialInstance.type;
          materialInstancePtrType =
              context.getPointerType(materialInstanceType);
          auto out{emitter.toRValue(
              emitter.resolveIdentifier("out"sv, decl.srcLoc))};
          emitter.builder.CreateStore(materialInstance, out);
        })};
    func->setLinkage(llvm::Function::ExternalLinkage);
    jitMaterial.evaluate.name = func->getName().str();
  }
  {
    // Generate the scatter evaluate function:
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // @(pure visible) int "material_name.scatter_evaluate"(
    //     &_material_instance instance,
    //     &float3 wo,
    //     &float3 wi,
    //     &float pdf_fwd,
    //     &float pdf_rev,
    //     &float f) {
    //   return ::df::_scatter_evaluate(
    //     instance, wo, wi, pdf_fwd, pdf_rev, f);
    // }
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    auto funcReturnType{static_cast<Type *>(context.getIntType())};
    auto func{emitter.createFunction(
        concat(declName, ".scatter_evaluate"), /*isPure=*/true, funcReturnType,
        {constParameter(materialInstancePtrType, "instance"),
         constParameter(float3PtrType, "wo"),
         constParameter(float3PtrType, "wi"),
         constParameter(floatPtrType, "pdf_fwd"),
         constParameter(floatPtrType, "pdf_rev"),
         constParameter(floatPtrType, "f")},
        decl.srcLoc, [&] {
          auto dfFunc{Crumb::find(context, "_scatter_evaluate"sv, nullptr,
                                  dfModule->lastCrumb)};
          SMDL_SANITY_CHECK(dfFunc);
          emitter.emitReturn(
              emitter.emitCall(
                  dfFunc->value,
                  llvm::ArrayRef<Value>{
                      emitter.resolveIdentifier("instance"sv, decl.srcLoc),
                      emitter.resolveIdentifier("wo"sv, decl.srcLoc),
                      emitter.resolveIdentifier("wi"sv, decl.srcLoc),
                      emitter.resolveIdentifier("pdf_fwd"sv, decl.srcLoc),
                      emitter.resolveIdentifier("pdf_rev"sv, decl.srcLoc),
                      emitter.resolveIdentifier("f"sv, decl.srcLoc)},
                  decl.srcLoc),
              decl.srcLoc);
        })};
    func->setLinkage(llvm::Function::ExternalLinkage);
    jitMaterial.scatter_evaluate.name = func->getName().str();
  }
  {
    // Generate the scatter sample function:
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // @(pure visible) int "material_name.scatter_sample"(
    //     &_material_instance instance,
    //     &float4 xi,
    //     &float3 wo,
    //     &float3 wi,
    //     &float pdf_fwd,
    //     &float pdf_rev,
    //     &float f,
    //     &int is_delta) {
    //   return ::df::_scatter_sample(
    //     instance, xi, wo, wi, pdf_fwd, pdf_rev, f, is_delta);
    // }
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    auto funcReturnType{static_cast<Type *>(context.getIntType())};
    auto func{emitter.createFunction(
        concat(declName, ".scatter_sample"), /*isPure=*/true, funcReturnType,
        {constParameter(materialInstancePtrType, "instance"),
         constParameter(float4PtrType, "xi"),
         constParameter(float3PtrType, "wo"),
         constParameter(float3PtrType, "wi"),
         constParameter(floatPtrType, "pdf_fwd"),
         constParameter(floatPtrType, "pdf_rev"),
         constParameter(floatPtrType, "f"),
         constParameter(intPtrType, "is_delta")},
        decl.srcLoc, [&] {
          auto dfFunc{Crumb::find(context, "_scatter_sample"sv, nullptr,
                                  dfModule->lastCrumb)};
          SMDL_SANITY_CHECK(dfFunc);
          emitter.emitReturn(
              emitter.emitCall(
                  dfFunc->value,
                  llvm::ArrayRef<Value>{
                      emitter.resolveIdentifier("instance"sv, decl.srcLoc),
                      emitter.resolveIdentifier("xi"sv, decl.srcLoc),
                      emitter.resolveIdentifier("wo"sv, decl.srcLoc),
                      emitter.resolveIdentifier("wi"sv, decl.srcLoc),
                      emitter.resolveIdentifier("pdf_fwd"sv, decl.srcLoc),
                      emitter.resolveIdentifier("pdf_rev"sv, decl.srcLoc),
                      emitter.resolveIdentifier("f"sv, decl.srcLoc),
                      emitter.resolveIdentifier("is_delta"sv, decl.srcLoc)},
                  decl.srcLoc),
              decl.srcLoc);
        })};
    func->setLinkage(llvm::Function::ExternalLinkage);
    jitMaterial.scatter_sample.name = func->getName().str();
  }
  // TODO _volume_scatter_evaluate
  // TODO _volume_scatter_sample
}
//--}

//--{ InferredSizeArrayType
Value InferredSizeArrayType::invoke(Emitter &emitter, const ArgumentList &args,
                                    const SourceLocation &srcLoc) {
  if (args.isAnyNamed())
    srcLoc.throwError(
        "unexpected named arguments in constructor of inferred-size array ",
        Quoted(displayName));

  // Infer!
  auto inferredArrayType{[&]() {
    // If there is one positional argument ...
    if (args.isOnePositional()) {
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
      if (arrayType && emitter.context.isExplicitlyConvertible(
                           arrayType->elemType, elemType))
        return emitter.context.getArrayType(elemType, arrayType->size);
    }
    // Otherwise, we attempt to construct an array that has the same number of
    // elements as there are arguments.
    return emitter.context.getArrayType(elemType, args.size());
  }()};

  if (!sizeNameStrv.empty()) {
    emitter.declareCrumb(
        sizeNameStrv, nullptr,
        emitter.context.getComptimeInt(int(inferredArrayType->size)));
  }

  // Delegate.
  return inferredArrayType->invoke(emitter, args, srcLoc);
}
//--}

Value MetaType::accessField(Emitter &emitter, Value value,
                            std::string_view name,
                            const SourceLocation &srcLoc) {
  if (value.isComptimeMetaType(emitter.context)) {
    auto type{value.getComptimeMetaType(emitter.context, srcLoc)};
    // Make static fields available
    if (auto structType{llvm::dyn_cast<StructType>(type)}) {
      const auto &staticFields{structType->getInstanceOf().staticFields};
      if (auto itr{staticFields.find(name)}; itr != staticFields.end())
        return itr->second;
    }
  } else if (value.isComptimeMetaModule(emitter.context)) {
    // Make exported declarations available
    auto module_{value.getComptimeMetaModule(emitter.context, srcLoc)};
    if (auto crumb{Crumb::find(emitter.context, name, emitter.getLLVMFunction(),
                               module_->lastCrumb, nullptr,
                               /*ignoreIfNotExported=*/true)})
      return crumb->value;
  } else if (value.isComptimeMetaNamespace(emitter.context)) {
    // Make exported declarations available
    auto namespace_{value.getComptimeMetaNamespace(emitter.context, srcLoc)};
    if (auto crumb{Crumb::find(emitter.context, name, emitter.getLLVMFunction(),
                               namespace_->lastCrumb, namespace_->firstCrumb,
                               /*ignoreIfNotExported=*/true)})
      return crumb->value;
  }
  return RValue(emitter.context.getVoidType(), nullptr);
}

//--{ PointerType
PointerType::PointerType(Context &context, Type *pointeeType)
    : pointeeType(pointeeType) {
  displayName = "&" + pointeeType->displayName;
  llvmType = llvm::PointerType::get(context, 0);
}

Value PointerType::invoke(Emitter &emitter, const ArgumentList &args,
                          const SourceLocation &srcLoc) {
  if (isAbstract()) {
    if (args.empty() || args.isNull())
      srcLoc.throwError("cannot zero construct abstract pointer ",
                         Quoted(displayName));
    if (args.isOnePositional()) {
      auto value{args[0].value};
      if (value.type->isPointer() &&
          emitter.context.isPerfectlyConvertible(value.type->getPointeeType(),
                                                 pointeeType)) {
        return emitter.toRValue(value);
      }
    }
  } else {
    if (args.empty() || args.isNull()) {
      return Value::zero(this);
    }
    if (args.isOnePositional()) {
      auto value{args[0].value};
      if (value.type->isPointer()) {
        return RValue(this, emitter.toRValue(value));
      }
      // If the value is an instance of the pointee type or
      // if the value is an instance of an array of the pointee type,
      // decay to a pointer.
      if ((value.type == pointeeType) ||
          (value.type->isArray() &&
           llvm::dyn_cast<ArrayType>(value.type)->elemType == pointeeType)) {
        // If not an lvalue, make it an lvalue so we actually have
        // an address to work with.
        if (!value.isLValue()) {
          value = emitter.toLValue(value);
          // NOTE: The way the scopes and lifetimes work right now, this
          // does not actually work. The lifetime would implicitly end before
          // being used in argument conversions and thus leads to undefined
          // behavior, so for now we "leak" these lvalues.
          // emitter.declare_crumb(/*name=*/{}, /*node=*/{}, value);
        }
        // Do not worry about pointer cast because in modern LLVM
        // we treat all pointers as opaque anyway.
        return RValue(this, value);
      }
    }
  }
  srcLoc.throwError("cannot construct ", Quoted(displayName), " from ",
                     Quoted(std::string(args)));
  return Value();
}

Value PointerType::accessField(Emitter &emitter, Value value,
                               std::string_view name,
                               const SourceLocation &srcLoc) {
  return pointeeType->accessField(
      emitter, LValue(pointeeType, emitter.toRValue(value)), name, srcLoc);
}

Value PointerType::accessIndex(Emitter &emitter, Value value, Value i,
                               const SourceLocation &srcLoc) {
  return LValue(pointeeType, emitter.builder.CreateGEP(
                                 pointeeType->llvmType, emitter.toRValue(value),
                                 {emitter.toRValue(i).llvmValue}));
}
//--}

//--{ StateType
StateType::StateType(Context &context) {
  displayName = "state";
#define ADD_FIELD(name)                                                        \
  fields.push_back(                                                            \
      {context.getType(&State::name), #name, uint64_t(offsetof(State, name))})
  ADD_FIELD(allocator);
  ADD_FIELD(wavelength_base);
  ADD_FIELD(wavelength_min);
  ADD_FIELD(wavelength_max);
  ADD_FIELD(meters_per_scene_unit);
  ADD_FIELD(animation_time);
  ADD_FIELD(object_id);
  ADD_FIELD(ptex_face_id);
  ADD_FIELD(ptex_face_uv);
  ADD_FIELD(position);
  /* ADD_FIELD(direction); */
  ADD_FIELD(motion);
  ADD_FIELD(normal);
  ADD_FIELD(geometry_normal);
  ADD_FIELD(texture_space_max);
  ADD_FIELD(texture_coordinate);
  ADD_FIELD(texture_tangent_u);
  ADD_FIELD(texture_tangent_v);
  ADD_FIELD(geometry_tangent_u);
  ADD_FIELD(geometry_tangent_v);
  ADD_FIELD(tangent_to_object_matrix);
  ADD_FIELD(object_to_world_matrix);
  ADD_FIELD(transport);
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
                 Quoted(fields[i].name), " is misaligned)"));
}

Value StateType::accessField(Emitter &emitter, Value value,
                             std::string_view name,
                             const SourceLocation &srcLoc) {
  SMDL_SANITY_CHECK(value.isLValue());
  for (unsigned i = 0; i < fields.size(); i++) {
    if (fields[i].name == name) {
      auto llvmValue{
          emitter.builder.CreateStructGEP(value.type->llvmType, value, i)};
      if (value.llvmValue->hasName())
        llvmValue->setName(concat(value.llvmValue->getName().str(), ".", name));
      return LValue(fields[i].type, llvmValue);
    }
  }
  srcLoc.throwError("no field ", Quoted(name), " in 'state'");
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
  if (args.empty() || args.isNull()) {
    return Value::zero(this);
  }
  // Construct from another string.
  if (args.isOnePositional(this)) {
    return emitter.toRValue(args[0].value);
  }
  // Construct from enum, call the enum-to-string conversion function.
  if (args.isOnePositional()) {
    auto value{args[0].value};
    if (auto enumType{llvm::dyn_cast<EnumType>(value.type)})
      return RValue(this, emitter.builder.CreateCall(
                              enumType->llvmFuncToString->getFunctionType(),
                              enumType->llvmFuncToString,
                              {emitter.toRValue(value).llvmValue}));
  }
  srcLoc.throwError("cannot construct 'string' from ",
                     Quoted(std::string(args)));
  return Value();
}

Value StringType::accessField(Emitter &emitter, Value value,
                              std::string_view name,
                              const SourceLocation &srcLoc) {
  if (name == "size") {
    if (value.isComptime()) {
      return emitter.context.getComptimeInt(
          int(value.getComptimeString().size()));
    } else {
      return RValue(emitter.context.getIntType(),
                    llvmEmitCast(emitter.builder,
                                 llvm::emitStrLen(
                                     emitter.toRValue(value), emitter.builder,
                                     emitter.context.llvmLayout,
                                     &emitter.context.llvmTargetLibraryInfo),
                                 emitter.context.getIntType()->llvmType));
    }
  }
  srcLoc.throwError("no field ", Quoted(name), " in 'string'");
  return Value();
}
//--}

//--{ StructType
void StructType::initialize(Emitter &emitter) {
  params.lastCrumb = emitter.declareCrumb(
      decl.name, &decl, emitter.context.getComptimeMetaType(this));
  SMDL_PRESERVE(emitter.crumb);
  // Initialize tags.
  for (auto &tag : decl.tags) {
    emitter.emit(tag.type);
    auto tagType{llvm::dyn_cast<TagType>(tag.type->type)};
    if (!tagType)
      decl.srcLoc.throwError("unknown tag");
    if (tag.is_default()) {
      if (tagType->defaultType)
        decl.srcLoc.throwError("tag ", Quoted(tagType->displayName),
                                " already has default");
      tagType->defaultType = this;
    }
    tags.push_back(tagType);
  }
  for (auto &astConstructor : decl.constructors) {
    constructors.emplace_back(Constructor{
        &astConstructor, emitter.emitParameterList(astConstructor.params)});
  }
  // Initialize fields.
  for (auto &field : decl.fields) {
    auto fieldType{
        emitter.emit(field.type)
            .getComptimeMetaType(emitter.context, field.name.srcLoc)};
    if (fieldType == this)
      field.name.srcLoc.throwError(
          "struct ", Quoted(displayName),
          " cannot be type of field in its definition");
    // Handle static constant fields!
    if (field.type->has_qualifier("static")) {
      const char *reasonForError = //
          field.type->has_qualifier("inline")   ? "must not be 'inline'"
          : !field.type->has_qualifier("const") ? "must also be 'const'"
          : !field.exprInit                     ? "must have initializer"
                                                : nullptr;
      if (reasonForError)
        field.name.srcLoc.throwError("field ", Quoted(field.name),
                                      " declared 'static' ", reasonForError);
      auto value{emitter.invoke(fieldType, emitter.emit(field.exprInit),
                                field.name.srcLoc)};
      staticFields[field.name.srcName] = value;
      params.lastCrumb = emitter.declareCrumb(field.name, &field, value);
    } else {
      auto &param{params.emplace_back()};
      param.type = fieldType;
      param.name = field.name.srcName;
      param.astField = &field;
    }
  }
  // Initialize LLVM type. Note: We don't check `is_abstract()`
  // because it is possible that the type is abstract, but has an entirely
  // concrete LLVM definition (e.g., through abstract pointers).
  if (params.isAllTrue(
          [](auto &param) { return param.type->llvmType != nullptr; }))
    llvmType = llvm::StructType::create(emitter.context, params.getLLVMTypes(),
                                        displayName);
}

StructType *
StructType::instantiate(Context &context,
                        const llvm::SmallVector<Type *> &paramTypes) {
  SMDL_SANITY_CHECK(params.isAbstract());
  SMDL_SANITY_CHECK(params.size() == paramTypes.size());
  auto &structType{instances[paramTypes]};
  if (!structType) {
    structType = context.allocator.allocate<StructType>(decl);
    structType->instanceOf = this;
    structType->tags = tags;
    structType->params = params;
    for (size_t i{}; i < params.size(); i++) {
      SMDL_SANITY_CHECK(paramTypes[i]);
      SMDL_SANITY_CHECK(!paramTypes[i]->isAbstract());
      structType->params[i].type = paramTypes[i];
    }
    structType->llvmType = llvm::StructType::create(
        context, structType->params.getLLVMTypes(), structType->displayName);
  }
  return structType.get();
}

bool StructType::isAbstract() {
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
    if (param.type->isPointer() &&
        param.type->getFirstNonPointerType() == this) {
      continue;
    }
    if (param.type->isAbstract()) {
      return true;
    }
  }
  return false;
}

Value StructType::invoke(Emitter &emitter, const ArgumentList &args,
                         const SourceLocation &srcLoc) {
  if (args.isNull()) {
    if (isAbstract())
      srcLoc.throwError("cannot zero construct abstract struct type ",
                         Quoted(displayName));
    return Value::zero(this);
  }
  if (args.isOnePositional()) {
    if (auto structType{llvm::dyn_cast<StructType>(args[0].value.type)};
        structType && (structType == this || structType->isInstanceOf(this))) {
      return emitter.toRValue(args[0].value);
    }
  }
  if (emitter.canResolveArguments(params, args, srcLoc)) {
    auto resolved{emitter.resolveArguments(params, args, srcLoc)};
    auto resultType{this};
    if (resultType->isAbstract()) {
      resultType = instantiate(emitter.context, resolved.GetValueTypes());
      resultType->isDefaultInstance = args.empty();
      SMDL_SANITY_CHECK(!resultType->isAbstract());
    }
    auto result{Value::zero(resultType)};
    auto i{size_t(0)};
    for (auto &value : resolved.values)
      result = emitter.insert(result, value, i++, srcLoc);
    if (decl.stmtFinalize) {
      SMDL_PRESERVE(emitter.crumb);
      emitter.crumb = params.lastCrumb;
      emitter.handleScope(nullptr, nullptr, [&] {
        emitter.labelReturn = {};   // Invalidate!
        emitter.labelBreak = {};    // Invalidate!
        emitter.labelContinue = {}; // Invalidate!
        emitter.currentModule = decl.srcLoc.module_;
        auto lv{emitter.toLValue(result)};
        for (auto &param : params)
          emitter.declareCrumb(param.name, &decl,
                               emitter.accessField(lv, param.name, srcLoc));
        emitter.emit(decl.stmtFinalize);
        result = emitter.toRValue(lv);
        emitter.createLifetimeEnd(lv);
      });
    }
    return result;
  }
  // TODO Overload resolution?
  auto viableConstructors{llvm::SmallVector<Constructor *>{}};
  for (auto &constructor : getInstanceOf().constructors) {
    if (!constructor.isInvoking &&
        emitter.canResolveArguments(constructor.params, args, srcLoc)) {
      viableConstructors.push_back(&constructor);
    }
  }
  if (viableConstructors.size() == 1) {
    auto &constructor{*viableConstructors[0]};
    auto resolved{emitter.resolveArguments(constructor.params, args, srcLoc)};
    SMDL_PRESERVE(emitter.crumb, constructor.isInvoking);
    emitter.crumb = constructor.params.lastCrumb;
    constructor.isInvoking = true;
    return emitter.createFunctionImplementation(
        decl.name, !emitter.state, this, constructor.params, resolved.values,
        srcLoc, [&]() {
          emitter.currentModule = decl.srcLoc.module_;
          emitter.emitReturn(emitter.emit(constructor.astConstructor->expr),
                             srcLoc);
        });
  } else if (viableConstructors.size() > 1) {
    srcLoc.throwError("cannot construct ", Quoted(displayName), " from ",
                       Quoted(std::string(args)), ": ",
                       viableConstructors.size(), " ambiguous overloads");
  }
  srcLoc.throwError("cannot construct ", Quoted(displayName), " from ",
                     Quoted(std::string(args)));
  return Value();
}

bool StructType::hasField(std::string_view name) {
  if (params.isAnyTrue([&](auto &param) { return param.name == name; }))
    return true;
  return getInstanceOf().staticFields.contains(name);
}

Value StructType::accessField(Emitter &emitter, Value value,
                              std::string_view name,
                              const SourceLocation &srcLoc) {
  auto seq{ParameterList::LookupSeq{}};
  if (params.getLookupSequence(name, seq)) {
    auto hasName0{value.llvmValue->hasName()};
    auto name0{value.llvmValue->getName().str()};
    bool isConst{};
    for (auto [param, i] : seq) {
      isConst |= param->isConst();
      value = Value(
          value.kind, param->type,
          value.isLValue()
              ? emitter.builder.CreateStructGEP(value.type->llvmType, value, i)
              : emitter.builder.CreateExtractValue(value, {i}));
    }
    if (hasName0) {
      name0 += '.';
      name0 += name;
      value.llvmValue->setName(name0);
    }
    return isConst ? emitter.toRValue(value) : value;
  }
  if (auto itr{getInstanceOf().staticFields.find(name)};
      itr != getInstanceOf().staticFields.end())
    return itr->second;
  srcLoc.throwError("no field ", Quoted(name), " in struct ",
                     Quoted(displayName));
  return Value();
}

Value StructType::insert(Emitter &emitter, Value value, Value elem, unsigned i,
                         const SourceLocation &srcLoc) {
  SMDL_SANITY_CHECK(i < params.size());
  if (params[i].type->isVoid())
    return emitter.toRValue(value);
  else
    return RValue(this, emitter.builder.CreateInsertValue(
                            emitter.toRValue(value),
                            emitter.invoke(params[i].type, elem, srcLoc), {i}));
}
//--}

//--{ TagType
Value TagType::invoke(Emitter &emitter, const ArgumentList &args,
                      const SourceLocation &srcLoc) {
  if (args.empty() || args.isNull()) {
    if (!defaultType)
      srcLoc.throwError("cannot default construct tag ", Quoted(displayName));
    return defaultType->invoke(emitter, args, srcLoc);
  } else if (args.isOnePositional()) {
    auto value{args[0].value};
    if (!emitter.context.isPerfectlyConvertible(value.type, this))
      srcLoc.throwError("cannot construct tag ", Quoted(displayName), " from ",
                         Quoted(value.type->displayName));
    return emitter.toRValue(value);
  } else {
    srcLoc.throwError("cannot construct tag ", Quoted(displayName), " from ",
                       Quoted(std::string(args)));
    return Value();
  }
}
//--}

//--{ UnionType
UnionType::UnionType(Context &context, llvm::SmallVector<Type *> caseTys)
    : caseTypes(std::move(caseTys)) {
  auto caseTypeNames{llvm::SmallVector<llvm::StringRef>{}};
  for (auto caseType : caseTypes) {
    SMDL_SANITY_CHECK(caseType);
    SMDL_SANITY_CHECK(!caseType->isAbstract());
    requiredAlign = std::max(requiredAlign, context.getAlignOf(caseType));
    requiredSize = std::max(requiredSize, context.getSizeOf(caseType));
    if (caseType->isVoid())
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
       context.getIntType()->llvmType},
      "union_t");
  SMDL_SANITY_CHECK(requiredAlign <= context.getAlignOf(this));
}

Value UnionType::invoke(Emitter &emitter, const ArgumentList &args,
                        const SourceLocation &srcLoc) {
  if (args.empty() || args.isNull()) {
    if (!isOptionalUnion())
      srcLoc.throwError("cannot zero construct non-optional union type ",
                         Quoted(displayName));
    auto result{Value::zero(this)};
    result.llvmValue = emitter.builder.CreateInsertValue(
        result.llvmValue,
        emitter.context.getComptimeInt(int(caseTypes.size() - 1)), {1U});
    return result;
  }
  if (args.isOnePositional(this)) {
    return emitter.toRValue(args[0].value);
  }
  if (args.isOnePositional()) {
    auto arg{args[0].value};
    if (auto argUnionType{llvm::dyn_cast<UnionType>(arg.type)}) {
      auto lvArg{emitter.toLValue(arg)};
      auto lv{emitter.createAlloca(this, "")};
      emitter.builder.CreateStore(Value::zero(this), lv);
      emitter.builder.CreateMemCpy(
          lv, llvm::Align(emitter.context.getAlignOf(this)), //
          lvArg, llvm::Align(emitter.context.getAlignOf(argUnionType)),
          std::min(requiredSize, argUnionType->requiredSize));
      if (!arg.isLValue())
        emitter.createLifetimeEnd(lvArg);
      auto index{emitter.toRValue(emitter.accessIndex(
          emitter.context.getComptimeUnionIndexMap(argUnionType, this),
          emitter.accessField(arg, "#idx", srcLoc), srcLoc))};
      emitter.builder.CreateStore(index,
                                  emitter.accessField(lv, "#idx", srcLoc));
      auto rv{emitter.toRValue(lv)};
      emitter.createLifetimeEnd(lv);
      if (!hasAllCaseTypes(argUnionType)) {
        auto [blockFail, blockPass] =
            emitter.createBlocks<2>("union_conversion", {".fail", ".pass"});
        emitter.builder.CreateCondBr(
            emitter.emitOp(BINOP_CMP_LT, index,
                           emitter.context.getComptimeInt(0), srcLoc),
            blockFail, blockPass);
        emitter.builder.SetInsertPoint(blockFail);
        emitter.emitPanic(
            emitter.context.getComptimeString("union conversion failed"),
            srcLoc);
        emitter.builder.CreateBr(blockPass);
        emitter.builder.SetInsertPoint(blockPass);
      }
      return rv;
    } else {
      if (!hasCaseType(arg.type))
        srcLoc.throwError("cannot construct union ", Quoted(displayName),
                           " from ", Quoted(arg.type->displayName));
      auto i{getCaseTypeIndex(arg.type)};
      auto lv{emitter.createAlloca(this, "union.lv")};
      emitter.createLifetimeStart(lv);
      emitter.builder.CreateStore(Value::zero(this), lv); // zeroinitializer
      emitter.builder.CreateStore(emitter.toRValue(arg), lv);
      emitter.builder.CreateStore(emitter.context.getComptimeInt(i),
                                  emitter.accessField(lv, "#idx", srcLoc));
      auto rv{emitter.toRValue(lv)};
      emitter.createLifetimeEnd(lv);
      return rv;
    }
  }
  srcLoc.throwError("cannot construct union ", Quoted(displayName), " from ",
                     Quoted(std::string(args)));
  return Value();
}

Value UnionType::accessField(Emitter &emitter, Value value,
                             std::string_view name,
                             const SourceLocation &srcLoc) {
  if (name == "#ptr")
    return RValue(
        emitter.context.getPointerType(emitter.context.getVoidType()),
        emitter.builder.CreateStructGEP(llvmType, emitter.toLValue(value), 0));
  if (name == "#idx")
    return value.isLValue()
               ? LValue(emitter.context.getIntType(),
                        emitter.builder.CreateStructGEP(llvmType, value, 1))
               : RValue(emitter.context.getIntType(),
                        emitter.builder.CreateExtractValue(value, {1U}));
  if (hasField(name)) {
    if (value.isRValue()) {
      auto lv{emitter.toLValue(value)};
      auto rv{emitter.toRValue(accessField(emitter, lv, name, srcLoc))};
      emitter.createLifetimeEnd(lv);
      return rv;
    }
    // Access unique optionals unsafely without switching. This mimics
    // pointer semantics. Note that the void type is guaranteed to be
    // at the end, so we know the non-void type is `caseTypes[0]`.
    if (caseTypes.size() == 2 && caseTypes.back()->isVoid()) {
      return emitter.accessField(
          LValue(caseTypes[0],
                 emitter.builder.CreateStructGEP(llvmType, value, 0)),
          name, srcLoc);
    } else {
      return emitter.emitVisit(value, srcLoc, [&](Value value) {
        return emitter.accessField(value, name, srcLoc);
      });
    }
  }
  srcLoc.throwError("no field ", Quoted(name), " in union ",
                     Quoted(displayName));
  return Value();
}

llvm::SmallVector<Type *>
UnionType::canonicalizeTypes(llvm::ArrayRef<Type *> types) {
  auto caseTypes{llvm::SmallVector<Type *>{}};
  for (auto type : types) {
    SMDL_SANITY_CHECK(type);
    SMDL_SANITY_CHECK(!type->isAbstract());
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
            [](auto lhs, auto rhs) { return rhs->isVoid() || lhs < rhs; });
  return caseTypes;
}
//--}

Value VoidType::invoke(Emitter &emitter, const ArgumentList &args,
                       const SourceLocation &srcLoc) {
  return RValue(emitter.context.getVoidType(), nullptr);
}

} // namespace smdl
