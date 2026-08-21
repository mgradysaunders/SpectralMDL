// vim:foldmethod=marker:foldlevel=0:fmr=--{,--}
#include "Type.h"

#include <algorithm>

#include "Emitter.h"

#include "smdl/Support/Logger.h"
#include "smdl/Support/QualifiedName.h"

#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Type.h"

namespace smdl {

//--{ Diagnostic helpers
// Render a candidate signature like `Foo(int a, float b)` for a diagnostic
// note. Shared by every "which candidate was this?" note so that overloads
// and struct constructors describe themselves the same way.
static std::string toSignatureString(std::string_view name,
                                     const ParameterList &params) {
  auto str{std::string(name)};
  str += '(';
  for (size_t i = 0; i < params.size(); i++) {
    if (i) str += ", ";
    const auto &param{params[i]};
    str += param.type->displayName;
    if (!param.name.empty()) {
      str += ' ';
      str += param.name;
    }
  }
  if (params.isVariadic) str += params.empty() ? "..." : ", ...";
  str += ')';
  return str;
}

// Drop the leading source location that `throwError` prepends to a message
// thrown at `srcLoc`. Every candidate is probed against the same call site,
// so repeating it on every note only obscures the candidate's own location.
// This is a no-op if the message does not begin with that prefix.
static std::string dropSourceLocation(std::string message,
                                      const SourceLocation &srcLoc) {
  auto prefix{std::string(srcLoc)};
  if (!prefix.empty()) {
    prefix += ' ';
    if (llvm::StringRef(message).starts_with(prefix))
      message.erase(0, prefix.size());
  }
  return message;
}

// One "candidate rejected" note. The candidate names itself and says where
// it is declared, because the reason it was rejected is always about the one
// call site that every candidate was probed against.
static void appendCandidateNote(std::string &notes, std::string_view kind,
                                std::string_view name,
                                const ParameterList &params,
                                const SourceLocation &declSrcLoc,
                                std::string_view reason) {
  notes += "\n  candidate rejected: ";
  if (!kind.empty()) {
    notes += kind;
    notes += ' ';
  }
  notes += toSignatureString(name, params);
  if (declSrcLoc) {
    notes += " declared at ";
    notes += std::string(declSrcLoc);
  }
  notes += ": ";
  notes += reason;
}
//--}

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

bool Type::isDefault() const {
  if (auto structType{llvm::dyn_cast<StructType>(this)}) {
    if (!structType->instanceOf) {
      // The struct is considered 'default' if it is the default type for
      // its first tag.
      return !structType->tags.empty() &&
             structType->tags[0]->defaultType == structType;
    } else {
      // The struct is considered 'default' if it is the default
      // instantiation of an abstract struct.
      return structType->isDefaultInstance;
    }
  }
  return false;
}

Type *Type::getPointeeType() const {
  return isPointer() ? static_cast<const PointerType *>(this)->pointeeType
                     : nullptr;
}

Type *Type::getFirstNonPointerType() const {
  auto type{const_cast<Type *>(this)};
  while (type->isPointer()) type = type->getPointeeType();
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
  if (args.isOnePositional(this)) return emitter.rvalue(args[0].value);
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
    switch (numBits) {
    case 16:
      return llvm::Type::getHalfTy(context);
    case 32:
      return llvm::Type::getFloatTy(context);
    case 64:
      return llvm::Type::getDoubleTy(context);
    case 80:
      return llvm::Type::getX86_FP80Ty(context);
    case 128:
      return llvm::Type::getFP128Ty(context);
    default:
      break;
    }
    SMDL_SANITY_CHECK_MSG(false, "Invalid float type specification!");
    return nullptr;
  } else {
    return llvm::Type::getVoidTy(context);
  }
}

ArithmeticType::ArithmeticType(Context &context, Scalar scalar, Extent extent)
    : scalar(scalar), extent(extent) {
  displayName = extent.to_string(scalar);
  llvmType = extent.getLLVMType(scalar.getLLVMType(context));
}

std::optional<Value> Type::invokeTrivialCases(Emitter &emitter,
                                              const ArgumentList &args) {
  if (args.empty() || args.isNull()) return Value::zero(this);
  if (args.isOnePositional(this)) return emitter.rvalue(args[0].value);
  return std::nullopt;
}

// Materialize `value` in memory, apply `access` to it, and load the result
// back out as an rvalue.
//
// The slot is cached per value by `spillToMemory()`, so indexing the same
// aggregate several times costs one copy rather than one copy per access.
template <typename Access>
static Value accessViaLValue(Emitter &emitter, Value value, Access &&access) {
  return emitter.rvalue(
      std::invoke(std::forward<Access>(access), emitter.spillToMemory(value)));
}

// If `value` is a pointer to `pointeeType`, construct `resultType` by
// loading through the pointer. Assume the pointer is only as aligned as
// the pointee type itself!
static std::optional<Value> tryConstructFromPointer(Emitter &emitter,
                                                    Type *resultType,
                                                    Type *pointeeType,
                                                    const Value &value) {
  if (value.type->isPointer() && value.type->getPointeeType() == pointeeType)
    return RValue(resultType,
                  emitter.builder.CreateAlignedLoad(
                      resultType->llvmType, emitter.rvalue(value),
                      llvm::Align(emitter.context.getAlignOf(pointeeType))));
  return std::nullopt;
}

Value ArithmeticType::invoke(Emitter &emitter, const ArgumentList &args,
                             const SourceLocation &srcLoc) {
  auto &context{emitter.context};
  if (auto trivial{invokeTrivialCases(emitter, args)}) {
    return *trivial;
  }
  if (extent.isScalar()) {
    if (!args.isOnePositional())
      srcLoc.throwError("scalar ", Quoted(displayName),
                        " constructor expects 1 positional argument");
    auto value{args[0].value};
    // If constructing bool from pointer, check that it is non-NULL.
    if (scalar.isBoolean() && value.type->isPointer())
      return RValue(this,
                    emitter.builder.CreateIsNotNull(emitter.rvalue(value)));
    // If constructing bool from optional union, check that it is non-void.
    if (scalar.isBoolean() && value.type->isOptionalUnion())
      return RValue(
          this, emitter.builder.CreateICmpNE(
                    emitter.rvalue(emitter.accessField(value, "#idx", srcLoc)),
                    context.getComptimeInt(int(
                        static_cast<UnionType *>(value.type)->caseTypes.size() -
                        1))));
    // If constructing from another scalar or enum type, cast the
    // underlying LLVM representation.
    if (value.type->isArithmeticScalar() || value.type->isEnum())
      return RValue(
          this, llvmEmitCast(emitter.builder, emitter.rvalue(value), llvmType));
  } else if (extent.isVector()) {
    auto dim{size_t(extent.getVectorSize())};
    if (args.isOnePositional()) {
      auto value{args[0].value};
      // If constructing from scalar, splat the scalar value.
      // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      // float3(2.7) // == float3(2.7, 2.7, 2.7)
      // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if (value.type->isArithmeticScalar())
        return RValue(
            this, emitter.builder.CreateVectorSplat(
                      extent.getVectorSize(),
                      emitter.invoke(getScalarType(context), value, srcLoc)));
      // If constructing from vector of the same size, cast the
      // underlying LLVM representation.
      // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      // int4(float4(3.0, 0.2, 0.1, 5.4)) // Cast components to int
      // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if (value.type->isArithmeticVector() &&
          static_cast<ArithmeticType *>(value.type)->extent == extent)
        return RValue(this, llvmEmitCast(emitter.builder, emitter.rvalue(value),
                                         llvmType));
      // If constructing from a pointer to the scalar type, load from the
      // pointer.
      if (auto loaded{tryConstructFromPointer(emitter, this,
                                              getScalarType(context), value)})
        return *loaded;
      // If constructing from color and this is a 3-dimensional vector,
      // delegate to the `_colorToRgb` function in the `api` module.
      if (value.type == context.getColorType() && dim == 3)
        return invoke(
            emitter,
            emitter.emitCall(context.getKeyword("_colorToRgb"), value, srcLoc),
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
        auto scalarType{getScalarType(context)};
        auto params{ParameterList{}};
        params.push_back(Parameter{scalarType, "x"});
        params.push_back(Parameter{scalarType, "y"});
        if (dim >= 3) params.push_back(Parameter{scalarType, "z"});
        if (dim >= 4) params.push_back(Parameter{scalarType, "w"});
        values =
            std::move(emitter.resolveArguments(params, args, srcLoc).values);
      } else {
        // Otherwise just add the argument values in order. (We already verifed
        // by now that they have no names!)
        for (auto &arg : args) values.push_back(arg.value);
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
            arithType->getWithDifferentScalar(context, scalar), arg, srcLoc)};
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
    auto scalarType{getScalarType(context)};
    auto columnType{getColumnType(context)};
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
  // These constructions are a chain of shape tests rather than a list of
  // candidates, so say what the shape accepts instead of only that the
  // arguments did not fit it.
  auto accepts{std::string()};
  if (extent.isVector()) {
    accepts = concat("; ", Quoted(displayName),
                     " takes one scalar, or scalars and shorter vectors "
                     "totalling ",
                     extent.getVectorSize(), " components");
  } else if (extent.isMatrix()) {
    accepts =
        concat("; ", Quoted(displayName), " takes one scalar, ", extent.numCols,
               " column vectors of ", extent.numRows, " components, or ",
               extent.numCols * extent.numRows, " scalars");
  }
  srcLoc.throwError("cannot construct ", Quoted(displayName), " from ",
                    Quoted(std::string(args)), accepts);
  return Value();
}

bool ArithmeticType::hasField(std::string_view name) {
  if (!extent.isScalar()) {
    if (name.size() == 1) return toIndex(name[0]).has_value();
    if (extent.isVector()) return toIndexSwizzle(name).has_value();
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
  // Set LLVM name for more readable LLVM-IR.
  auto named{[&](Value result) {
    if (value.llvmValue->hasName())
      result.llvmValue->setName(
          concat(value.llvmValue->getName().str(), ".", name));
    return result;
  }};
  if (name.size() == 1) {
    if (auto i{toIndex(name[0])})
      return named(accessIndex(
          emitter, value, emitter.context.getComptimeInt(int(*i)), srcLoc));
  }
  if (extent.isVector()) {
    if (auto iMask{toIndexSwizzle(name)})
      return named(RValue(
          emitter.context.getArithmeticType(scalar, Extent(iMask->size())),
          emitter.builder.CreateShuffleVector(emitter.rvalue(value), *iMask)));
  }
  // The nameable components, so that a swizzle off the end says how far it
  // may actually go.
  auto components{std::string()};
  auto colorComponents{std::string()};
  for (uint32_t i{}; i < 4; i++) {
    if (!toIndex("xyzw"[i])) break;
    if (!components.empty()) {
      components += ", ";
      colorComponents += ", ";
    }
    components += "xyzw"[i];
    colorComponents += "rgba"[i];
  }
  srcLoc.throwError("no field ", Quoted(name), " in ", Quoted(displayName),
                    components.empty()
                        ? std::string()
                        : concat("; the components are ", components,
                                 extent.isVector()
                                     ? concat(" (or ", colorComponents, ")")
                                     : std::string()));
  return Value();
}

Value ArithmeticType::accessIndex(Emitter &emitter, Value value, Value i,
                                  const SourceLocation &srcLoc) {
  if (extent.isScalar())
    srcLoc.throwError("scalar ", Quoted(displayName),
                      " has no index access operator");
  if (i.isComptimeInt()) {
    const auto iNow{i.getComptimeSignedInt()};
    const auto count{
        int64_t(extent.isVector() ? extent.numRows : extent.numCols)};
    if (!iNow || *iNow < 0 || *iNow >= count)
      srcLoc.throwError("index out of bounds for ", Quoted(displayName));
  }
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
      return accessViaLValue(emitter, value, [&](Value lv) {
        return accessIndex(emitter, lv, i, srcLoc);
      });
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
}

Value ArithmeticType::insert(Emitter &emitter, Value value, Value elem,
                             unsigned i, const SourceLocation &srcLoc) {
  if (extent.isVector())
    return RValue(
        this,
        emitter.builder.CreateInsertElement(
            emitter.rvalue(value),
            emitter.invoke(getScalarType(emitter.context), elem, srcLoc), i));
  if (extent.isMatrix())
    return RValue(
        this, //
        emitter.builder.CreateInsertValue(
            emitter.rvalue(value),
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
    return emitter.rvalue(args[0].value);
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
    if (auto loaded{tryConstructFromPointer(emitter, this, elemType, value)})
      return *loaded;
  }
  srcLoc.throwError("cannot construct ", Quoted(displayName), " from ",
                    Quoted(std::string(args)), "; ", Quoted(displayName),
                    " takes ", size, " elements or one array of that size");
  return Value();
}

Value ArrayType::accessField(Emitter &emitter, Value value,
                             std::string_view name,
                             const SourceLocation &srcLoc) {
  SMDL_SANITY_CHECK(!isAbstract());
  if (hasField(name)) {
    if (!value.isLValue()) {
      return accessViaLValue(emitter, value, [&](Value lv) {
        return accessField(emitter, lv, name, srcLoc);
      });
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
  if (i.isComptimeInt()) {
    const auto iNow{i.getComptimeSignedInt()};
    if (!iNow || *iNow < 0 || *iNow >= int64_t(size))
      srcLoc.throwError("index out of bounds for ", Quoted(displayName));
  }
  if (i.isComptimeInt() && value.isRValue()) {
    return RValue(elemType, emitter.builder.CreateExtractValue(
                                value, {unsigned(i.getComptimeInt())}));
  } else {
    if (!value.isLValue()) {
      return accessViaLValue(emitter, value, [&](Value lv) {
        return accessIndex(emitter, lv, i, srcLoc);
      });
    }
    i = emitter.rvalue(i);
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
                                emitter.rvalue(value),
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
    return emitter.rvalue(args[0]);
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
    if (extent > 65535)
      srcLoc.throwError("cannot concatenate vector with more than 65535 "
                        "elements");
    return emitter.invoke(emitter.context.getArithmeticType(scalar, extent),
                          args, srcLoc);
  }
  // TODO Infer struct type?
  srcLoc.throwError("cannot construct 'auto' from ", Quoted(std::string(args)));
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
  if (auto trivial{invokeTrivialCases(emitter, args)}) {
    return *trivial;
  }
  if (args.isOnePositional()) {
    auto value{args[0].value};
    if (value.type->isArithmeticScalar())
      return RValue(this,
                    emitter.builder.CreateVectorSplat(
                        wavelengthBaseMax,
                        emitter.invoke(context.getFloatType(), value, srcLoc)));
    if (value.type->isArithmeticVector() &&
        static_cast<ArithmeticType *>(value.type)->extent.numRows ==
            wavelengthBaseMax)
      return RValue(
          this, llvmEmitCast(emitter.builder, emitter.rvalue(value), llvmType));
    if (auto loaded{tryConstructFromPointer(emitter, this,
                                            context.getFloatType(), value)})
      return *loaded;
    if (value.type == context.getFloatType(Extent(3)))
      return emitter.emitCall(context.getKeyword("_rgbToColor"), value, srcLoc);
    if (value.type == context.getSpectralCurveType())
      return emitter.emitCall(context.getKeyword("_spectralCurveToColor"),
                              value, srcLoc);
  }
  if (args.size() <= 3 && args.isOnlyTheseNames({"r", "g", "b"})) {
    auto params{ParameterList{
        Parameter{context.getFloatType(), "r", {}, {}, {}, true},
        Parameter{context.getFloatType(), "g", {}, {}, {}, true},
        Parameter{context.getFloatType(), "b", {}, {}, {}, true}}};
    if (emitter.canResolveArguments(params, args, srcLoc)) {
      auto resolvedArgs{emitter.resolveArguments(params, args, srcLoc)};
      return emitter.emitCall(
          context.getKeyword("_rgbToColor"),
          emitter.invoke(context.getFloatType(Extent(3)),
                         llvm::ArrayRef<Value>(resolvedArgs.values), srcLoc),
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
      auto resolvedArgs{emitter.resolveArguments(params, args, srcLoc)};
      auto arrayType0{llvm::dyn_cast<ArrayType>(resolvedArgs.values[0].type)};
      auto arrayType1{llvm::dyn_cast<ArrayType>(resolvedArgs.values[1].type)};
      if (!arrayType0 || arrayType0 != arrayType1)
        srcLoc.throwError(
            "expected wavelength and amplitude arrays to be same size");
      return emitter.emitCall(
          context.getKeyword("_samplesToColor"),
          ArgumentList{context.getComptimeInt(int(arrayType0->size)),
                       resolvedArgs.values[0], resolvedArgs.values[1]},
          srcLoc);
    }
  }
  srcLoc.throwError(
      "cannot construct 'color' from ", Quoted(std::string(args)),
      "; 'color' takes one scalar, one 'float3' or 'spectral_curve', "
      "'(r, g, b)', or '(wavelengths, amplitudes)'");
  return Value();
}

Value ColorType::accessIndex(Emitter &emitter, Value value, Value i,
                             const SourceLocation &srcLoc) {
  if (i.isComptimeInt()) {
    const auto iNow{i.getComptimeSignedInt()};
    if (!iNow || *iNow < 0 || *iNow >= int64_t(wavelengthBaseMax))
      srcLoc.throwError("index out of bounds for ", Quoted(displayName));
  }
  if (value.isRValue()) {
    if (i.isComptimeInt()) {
      return RValue(
          getArithmeticScalarType(emitter.context),
          emitter.builder.CreateExtractElement(value, i.getComptimeInt()));
    } else {
      return accessViaLValue(emitter, value, [&](Value lv) {
        return accessIndex(emitter, lv, i, srcLoc);
      });
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
                    emitter.rvalue(value),
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
    return emitter.rvalue(args[0].value);
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
  emitter.rejectSameScopeShadow(decl.name, decl.srcLoc);
  emitter.declare(decl.name, &decl, context.getComptimeMetaType(this));
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
    emitter.rejectSameScopeShadow(name.srcName, name.srcLoc);
    emitter.declare(name.srcName, &declarator, RValue(this, value));
    declarator.llvmConst = static_cast<llvm::ConstantInt *>(value.llvmValue);
    lastValue = value;
  }

  // Initialize the to-string LLVM function. This is just a big switch.
  auto returnType{context.getStringType()};
  llvmFuncToString = emitter.createFunction(
      displayName + ".to_string", /*isPure=*/true, returnType,
      {Parameter{this, "value"}}, decl.name.srcLoc, [&]() {
        auto value{
            emitter.resolveIdentifier(std::string_view("value"), decl.srcLoc)};
        auto blockDefault{emitter.createBlock("switch.default")};
        auto switchInst{
            emitter.builder.CreateSwitch(emitter.rvalue(value), blockDefault)};
        auto switchUniq{llvm::DenseSet<llvm::Value *>{}};
        for (unsigned i = 0; i < decl.declarators.size(); i++) {
          auto &declarator{decl.declarators[i]};
          if (!switchUniq.insert(declarator.llvmConst).second)
            continue; // Skip repeats!
          auto blockCase{
              emitter.createBlock("switch.case." + std::to_string(i))};
          switchInst->addCase(declarator.llvmConst, blockCase);
          emitter.builder.SetInsertPoint(blockCase);
          emitter.returns.push_back(
              {context.getComptimeString(declarator.name.srcName), blockCase,
               declarator.name.srcLoc});
          emitter.builder.CreateBr(emitter.labelReturn.block);
        }
        llvmMoveBlockToEnd(blockDefault);
        emitter.builder.SetInsertPoint(blockDefault);
        emitter.returns.push_back(
            {context.getComptimeString(""), blockDefault, decl.name.srcLoc});
        emitter.builder.CreateBr(emitter.labelReturn.block);
      });
}

Value EnumType::invoke(Emitter &emitter, const ArgumentList &args,
                       const SourceLocation &srcLoc) {
  if (auto trivial{invokeTrivialCases(emitter, args)}) {
    return *trivial;
  } else if (args.isOnePositional()) {
    auto value{args[0].value};
    if ((value.type->isArithmeticScalar() &&
         value.type->isArithmeticIntegral()) ||
        value.type->isEnum())
      return RValue(
          this, llvmEmitCast(emitter.builder, emitter.rvalue(value), llvmType));
  }
  srcLoc.throwError("cannot construct ", Quoted(displayName), " from ",
                    Quoted(std::string(args)));
  return Value();
}
//--}

//--{ FunctionType
// Reject duplicate parameter names, e.g., `foo(int a, float a)`. The
// `owner` phrase names the offending declaration in the diagnostic.
static void rejectDuplicateParameterNames(const ParameterList &params,
                                          std::string_view owner) {
  auto uniqueNames{llvm::StringSet<>()};
  for (auto &param : params)
    if (!uniqueNames.insert(param.name).second)
      param.getSourceLocation().throwError("duplicate parameter name ",
                                           Quoted(param.name), " in ", owner);
}

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
    // Only overload within a single module. A declaration over a function
    // imported from another module shadows it instead: linking would mutate
    // the imported module's interned `FunctionType` and leak this (possibly
    // non-exported) function into every other importer's overload
    // resolution.
    auto prevFunc{static_cast<FunctionType *>(prevType)};
    if (prevFunc->decl.srcLoc.module_ == decl.srcLoc.module_) {
      if (prevFunc->isVariant())
        decl.srcLoc.throwError("function ", Quoted(declName),
                               " must not overload function variant");
      if (prevFunc->isForeign())
        decl.srcLoc.throwError("function ", Quoted(declName),
                               " must not overload '@(foreign)' function");
      if (decl.isVariant())
        decl.srcLoc.throwError("function variant ", Quoted(declName),
                               " must not overload another function");
      if (decl.hasAttribute("foreign"))
        decl.srcLoc.throwError(
            "function ", Quoted(declName),
            " declared '@(foreign)' must not overload another function");
      prevOverload = prevFunc;
      prevOverload->nextOverload = this;
    }
  }
  // Declare the function name.
  emitter.declare(decl.name, &decl, context.getComptimeMetaType(this));
  emitter.captureResolutionAnchor(params);
  // Initialize return type and parameters.
  returnType =
      emitter.emit(decl.returnType).getComptimeMetaType(context, decl.srcLoc);
  for (auto &param : decl.params)
    params.push_back(
        Parameter{emitter.emit(param.type)
                      .getComptimeMetaType(context, param.name.srcLoc),
                  param.name, /*astParam=*/&param});
  rejectDuplicateParameterNames(params, concat("function ", Quoted(declName)));
  // Initialize whether parameter list is variadic.
  params.isVariadic = decl.isVariadic();
  if (decl.hasAttribute("macro") && decl.isVariadic()) {
    decl.srcLoc.throwError("function ", Quoted(declName),
                           " declared '@(macro)' must not be variadic");
  }
  auto compileNow{[&] {
    auto paramTypes{params.getTypes()};
    getInstance(emitter, llvm::SmallVector<Type *>(paramTypes.begin(),
                                                   paramTypes.end()));
  }};
  if (decl.hasAttribute("foreign")) {
    if (!params.isConcrete())
      decl.srcLoc.throwError(
          "function ", Quoted(declName),
          " declared '@(foreign)' must have concrete parameters");
    if (decl.definition)
      decl.srcLoc.throwError("function ", Quoted(declName),
                             " declared '@(foreign)' must not have definition");
    compileNow();
  }
  // If this is declared `@(visible)`, we compile it immediately to
  // guarantee that the symbol exists for the C++ runtime.
  if (decl.hasAttribute("visible")) {
    if (!params.isConcrete())
      decl.srcLoc.throwError(
          "function ", Quoted(declName),
          " declared '@(visible)' must have concrete parameters");
    if (!decl.definition)
      decl.srcLoc.throwError("function ", Quoted(declName),
                             " declared '@(visible)' must have definition");
    compileNow();
  }
  // If this is a function with no parameters that returns `material`,
  // it is a material definition!
  if (returnType == context.mMaterialType &&
      (params.empty() || params.hasAllDefaultInitializers())) {
    if (decl.hasAttribute("pure"))
      decl.srcLoc.throwError("material ", Quoted(declName),
                             " must not be declared '@(pure)'");
    if (decl.hasAttribute("macro"))
      decl.srcLoc.throwError("material ", Quoted(declName),
                             " must not be declared '@(macro)'");
    isMaterial = true;
    initializeMaterialFunctions(emitter);
  }
}

void FunctionType::initializeLambda(Emitter &emitter) {
  auto &context{emitter.context};
  // Unlike `initialize()`: no overload discovery and no name declaration,
  // because a lambda is anonymous. The resolution anchor is the lambda
  // expression itself, so the body resolves names visible at the point the
  // lambda is written.
  emitter.captureResolutionAnchor(params);
  // Initialize return type and parameters. There is no return type syntax
  // for lambdas (`decl.returnType` is null); the return type is always
  // implicitly `auto`, inferred at each expansion.
  returnType = context.getAutoType();
  for (auto &param : decl.params)
    params.push_back(
        Parameter{emitter.emit(param.type)
                      .getComptimeMetaType(context, param.name.srcLoc),
                  param.name, /*astParam=*/&param});
  rejectDuplicateParameterNames(params, "lambda");
  // The parser already rejects `...` in lambdas; belt and braces because
  // macros must not be variadic.
  if (decl.isVariadic()) decl.srcLoc.throwError("lambda must not be variadic");
}

Value FunctionType::invoke(Emitter &emitter, const ArgumentList &args,
                           const SourceLocation &srcLoc) {
  auto func{resolveOverload(emitter, args, srcLoc)};
  if (func->isVariant()) {
    auto result{Value()};
    SMDL_PRESERVE(emitter.scope, emitter.anchors);
    emitter.restoreResolutionAnchor(func->params);
    emitter.handleScope(nullptr, nullptr, [&]() {
      emitter.setCurrentModule(func->decl.srcLoc);
      auto [astLet, astCall] = func->decl.getVariantLetAndCallExpressions();
      // If the function variant has a `let` expression, generate the variable
      // declarations.
      if (astLet)
        for (auto &subDecl : astLet->decls) emitter.emit(subDecl);
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
      try {
        result = emitter.emitCall(callee, patchedArgs, srcLoc);
      } catch (const Error &error) {
        // A failure here reports the function behind the '(*)' and the
        // arguments the variant injected, none of which the caller wrote.
        // Lead with the call as it was actually written and keep the
        // underlying reason as a note.
        srcLoc.throwError(concat(
            "cannot call ", Quoted(declName), " with arguments ",
            Quoted(std::string(args)),
            "\n  forwards to: ", dropSourceLocation(error.message, srcLoc)));
      }
      // Skip the conversion when the type already matches exactly, so
      // memory-resident (lvalue) results stay in memory instead of being
      // loaded back into SSA by the pass-through conversion.
      if (result.type != decl.returnType->type)
        result = emitter.invoke(decl.returnType->type, result, srcLoc);
    });
    return result;
  }
  auto resolvedArgs{emitter.resolveArguments(
      func->params, args, srcLoc, /*dontEmit=*/false,
      /*passAggregatesIndirectly=*/func->usesIndirectParams())};
  if (auto impliedVisitArgs{resolvedArgs.getImpliedVisitArguments()})
    return emitter.emitCall(emitter.context.getComptimeMetaType(this),
                            *impliedVisitArgs, srcLoc);
  if (func->isMacro()) {
    ++macroRecursionDepth;
    if (macroRecursionDepth >= 1024)
      srcLoc.throwError("call to ", Quoted(func->declName),
                        " exceeds compile-time recursion limit 1024");
    SMDL_PRESERVE(emitter.scope, emitter.anchors);
    emitter.restoreResolutionAnchor(func->params);
    auto result{emitter.createFunctionImplementation(
        func->declName, func->isPure() || !emitter.state, func->returnType,
        func->params, resolvedArgs.values, srcLoc, [&]() {
          if (func->decl.hasAttribute("fastmath"))
            emitter.builder.setFastMathFlags(llvm::FastMathFlags::getFast());
          emitter.setCurrentModule(func->decl.srcLoc);
          emitter.emit(func->decl.definition);
        })};
    --macroRecursionDepth;
    return result;
  } else {
    if (!func->isPure() && !emitter.state)
      srcLoc.throwError("cannot call ", Quoted(func->declName),
                        " from '@(pure)' context");
    auto &instance{
        func->getInstance(emitter, resolvedArgs.getNonVariadicTypes())};
    auto llvmArgs{llvm::SmallVector<llvm::Value *>{}};
    // Indirect ('sret') returns receive a caller-provided result slot as
    // the leading argument (see 'Emitter::createFunction'), and the call
    // result is that slot as an lvalue, so the value stays in memory
    // instead of materializing as a large SSA aggregate. Foreign
    // functions always return by value.
    auto sretSlot{Value{}};
    if (!func->isForeign() && emitter.returnsIndirectly(instance.returnType)) {
      SMDL_SANITY_CHECK(instance.llvmFunc->getReturnType()->isVoidTy());
      sretSlot = emitter.createAlloca(instance.returnType, "sret.slot");
      llvmArgs.push_back(sretSlot.llvmValue);
    }
    if (!func->isPure()) llvmArgs.push_back(emitter.state);
    // Indirect ('byval') parameters are passed as pointers to the value
    // rather than as first-class aggregates. The indices are recorded while
    // building the argument list, which is what keeps them in step with the
    // callee: 'Emitter::createFunction' records its own the same way, and
    // the two must agree for the backend to lower the call consistently.
    auto indirectParams{llvm::SmallVector<std::pair<unsigned, Type *>, 4>{}};
    for (size_t i = 0; i < resolvedArgs.values.size(); i++) {
      const auto &value{resolvedArgs.values[i]};
      // A voided argument carries no data and is absent from the callee's
      // signature (see 'Emitter::createFunction'), so it contributes no
      // LLVM argument here either. The callee's parameter types are the
      // types of these very values, by way of 'getNonVariadicTypes()', so
      // the two sides drop exactly the same positions.
      if (value.isVoid()) continue;
      if (func->usesIndirectParams() && i < func->params.size() &&
          emitter.getLLVMFunction() && emitter.passesIndirectly(value.type)) {
        SMDL_SANITY_CHECK_MSG(value.isLValue(),
                              "indirect parameter must reach the call "
                              "memory-resident (see 'resolveArguments')");
        indirectParams.push_back({unsigned(llvmArgs.size()), value.type});
      }
      llvmArgs.push_back(value);
    }
    auto callInst{emitter.builder.CreateCall(
        instance.llvmFunc->getFunctionType(), instance.llvmFunc, llvmArgs)};
    for (auto [i, paramType] : indirectParams)
      emitter.addIndirectParamAttrs(paramType, i, nullptr, callInst);
    if (sretSlot) {
      emitter.addIndirectReturnAttrs(instance.returnType, nullptr, callInst);
      return LValue(instance.returnType, sretSlot.llvmValue);
    }
    return RValue(instance.returnType, callInst);
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
    while (func->nextOverload) func = func->nextOverload;
    return func;
  }};
  auto overloadErrors{std::string{}};
  for (auto func{getLastOverload()}; func; func = func->prevOverload) {
    try {
      SMDL_SANITY_CHECK(!func->isVariant());
      auto resolvedArgs{emitter.resolveArguments(func->params, args, srcLoc,
                                                 /*dontEmit=*/true)};
      overloads.push_back({func, std::move(resolvedArgs.argParams)});
    } catch (const Error &error) {
      appendCandidateNote(overloadErrors, {}, func->declName, func->params,
                          func->decl.srcLoc,
                          dropSourceLocation(error.message, srcLoc));
    }
  }
  // If no matching declarations, fail, including the reason each
  // candidate was rejected.
  if (overloads.empty())
    srcLoc.throwError("function ", Quoted(declName),
                      " has no overload for arguments ",
                      Quoted(std::string(args)), overloadErrors);
  // Candidate A beats candidate B by conversion quality if A converts every
  // argument at least as well as B and at least one argument strictly better
  // ('CONVERSION_RULE_PERFECT' beats 'CONVERSION_RULE_IMPLICIT'). This is
  // what makes an exact argument-type match win regardless of declaration
  // order.
  auto beatsByConversion{[&](const Overload &overloadA,
                             const Overload &overloadB) {
    bool anyBetter{};
    for (size_t i = 0; i < args.size(); i++) {
      if (!overloadA.params[i] || !overloadB.params[i])
        continue; // Unresolved variadic arguments do not participate
      auto ruleA{emitter.context.getConversionRule(args[i].value.type,
                                                   overloadA.params[i]->type)};
      auto ruleB{emitter.context.getConversionRule(args[i].value.type,
                                                   overloadB.params[i]->type)};
      if (ruleA < ruleB) return false;
      if (ruleA > ruleB) anyBetter = true;
    }
    return anyBetter;
  }};
  // The tiebreaker: the LHS set of parameter types is less specific than the
  // RHS set if each and every RHS parameter type is implicitly convertible
  // to the corresponding LHS parameter type. This is what makes a concrete
  // declaration win over an 'auto' template whose conversion ranks tie.
  auto isLessSpecific{[&](llvm::ArrayRef<const Parameter *> paramsA,
                          llvm::ArrayRef<const Parameter *> paramsB) {
    SMDL_SANITY_CHECK(paramsA.size() == paramsB.size());
    for (size_t i = 0; i < paramsA.size(); i++) {
      if (!paramsA[i] || !paramsB[i])
        continue; // Unresolved variadic arguments do not participate
      if (!emitter.context.isImplicitlyConvertible(paramsB[i]->type,
                                                   paramsA[i]->type))
        return false;
    }
    return true;
  }};
  auto beatsBySpecificity{
      [&](const Overload &overloadA, const Overload &overloadB) {
        return isLessSpecific(overloadB.params, overloadA.params) &&
               !isLessSpecific(overloadA.params, overloadB.params);
      }};
  // Remove every candidate beaten by another candidate, first by conversion
  // quality, then by specificity among what remains. Both relations are
  // strict partial orders, so at least one candidate always survives. If
  // more than one survives, the call is genuinely ambiguous: fail loudly
  // instead of picking by declaration order.
  auto filterBeaten{[&](auto &&beats) {
    auto beaten{llvm::SmallVector<bool>(overloads.size(), false)};
    for (size_t a = 0; a < overloads.size(); a++)
      for (size_t b = 0; b < overloads.size(); b++)
        if (a != b && beats(overloads[a], overloads[b])) beaten[b] = true;
    size_t keep{};
    for (size_t i = 0; i < overloads.size(); i++) {
      if (!beaten[i]) {
        if (keep != i) overloads[keep] = std::move(overloads[i]);
        keep++;
      }
    }
    overloads.resize(keep);
  }};
  filterBeaten(beatsByConversion);
  filterBeaten(beatsBySpecificity);
  if (overloads.size() > 1) {
    auto candidateNotes{std::string{}};
    for (auto &overload : overloads) {
      candidateNotes += "\n  ambiguous candidate: ";
      candidateNotes +=
          toSignatureString(overload.func->declName, overload.func->params);
      candidateNotes += " declared at ";
      candidateNotes += std::string(overload.func->decl.srcLoc);
    }
    srcLoc.throwError("function ", Quoted(declName),
                      " is ambiguous for arguments ", Quoted(std::string(args)),
                      candidateNotes);
  }
  return overloads[0].func;
}

FunctionType::Instance &
FunctionType::getInstance(Emitter &emitter,
                          const llvm::SmallVector<Type *> &paramTypes) {
  SMDL_SANITY_CHECK(paramTypes.size() == params.size());
  auto &inst{instances[paramTypes]};
  if (!inst.llvmFunc) {
    SMDL_SANITY_CHECK(!inst.isCompiling);
    inst.isCompiling = true;
    inst.returnType = returnType;
    if (isForeign()) {
      emitter.createFunction(inst.llvmFunc, declName, isPure(), inst.returnType,
                             paramTypes, params, decl.srcLoc, nullptr);
    } else {
      SMDL_SANITY_CHECK(decl.definition);
      // A lambda instance is always compiled pure: lambdas ordinarily
      // macro-expand, and materialization exists to hand the compiled
      // function across an ABI with no state channel (see the
      // 'tabulateAlbedo' intrinsic). A lambda body that references
      // '$state' fails with the usual pure-context error, and a lambda
      // body that captures run-time locals fails with the usual
      // cross-function reference error.
      emitter.createFunction(
          inst.llvmFunc, declName, isPure() || isLambda, inst.returnType,
          paramTypes, params, decl.srcLoc,
          [&] {
            if (decl.hasAttribute("fastmath"))
              emitter.builder.setFastMathFlags(llvm::FastMathFlags::getFast());
            emitter.setCurrentModule(decl.srcLoc);
            emitter.emit(decl.definition);
          },
          usesIndirectParams());
      static const std::pair<const char *, llvm::Attribute::AttrKind> attrs[] =
          {{"alwaysinline", llvm::Attribute::AlwaysInline},
           {"noinline", llvm::Attribute::NoInline},
           {"hot", llvm::Attribute::Hot},
           {"cold", llvm::Attribute::Cold},
           {"optsize", llvm::Attribute::OptimizeForSize},
           {"optnone", llvm::Attribute::OptimizeNone}};
      for (auto [attrName, attrID] : attrs)
        if (decl.hasAttribute(attrName)) inst.llvmFunc->addFnAttr(attrID);
      if (decl.hasAttribute("visible"))
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

// Verify that the C++ `JIT::Material::Instance` layout matches the api
// `_MaterialInstance` struct emitted by the compiler. The JIT boundary
// reinterprets one as the other, so any drift is silent undefined
// behavior at render time; fail the compile loudly instead.
static void verifyMaterialInstanceLayout(Context &context, Type *type,
                                         const SourceLocation &srcLoc) {
  auto llvmStructType{
      llvm::dyn_cast_if_present<llvm::StructType>(type->llvmType)};
  if (!llvmStructType)
    srcLoc.throwError("'_MaterialInstance' is not a struct type");
  using Instance = JIT::Material::Instance;
  const std::pair<std::string_view, uint64_t> fields[]{
      {"ptr", offsetof(Instance, ptr)},
      {"geometry", offsetof(Instance, geometry)},
      {"ior", offsetof(Instance, ior)},
      {"exterior_ior", offsetof(Instance, exterior_ior)},
      {"temperature", offsetof(Instance, temperature)},
      {"absorption_coefficient", offsetof(Instance, absorption_coefficient)},
      {"scattering_coefficient", offsetof(Instance, scattering_coefficient)},
      {"max_absorption_coefficient",
       offsetof(Instance, max_absorption_coefficient)},
      {"max_scattering_coefficient",
       offsetof(Instance, max_scattering_coefficient)},
      {"volume_density_resource", offsetof(Instance, volume_density_resource)},
      {"volume_density_bound_min",
       offsetof(Instance, volume_density_bound_min)},
      {"volume_density_bound_max",
       offsetof(Instance, volume_density_bound_max)},
      {"volume_emission_intensity",
       offsetof(Instance, volume_emission_intensity)},
      {"surface_emission_intensity",
       offsetof(Instance, surface_emission_intensity)},
      {"backface_emission_intensity",
       offsetof(Instance, backface_emission_intensity)},
      {"wavelength_base_max", offsetof(Instance, wavelength_base_max)},
      {"flags", offsetof(Instance, flags)},
      {"df_lobes_surface", offsetof(Instance, df_lobes_surface)},
      {"df_lobes_backface", offsetof(Instance, df_lobes_backface)},
      {"emission_modes", offsetof(Instance, emission_modes)},
      {"seed", offsetof(Instance, seed)},
      {"tangent_to_world_space", offsetof(Instance, tangent_to_world_space)},
  };
  auto llvmLayout{context.llvmLayout.getStructLayout(llvmStructType)};
  if (llvmStructType->getNumElements() != std::size(fields) ||
      uint64_t(llvmLayout->getSizeInBytes()) > sizeof(Instance))
    srcLoc.throwError("mismatch between C++ 'JIT::Material::Instance' and "
                      "SMDL '_MaterialInstance' structures");
  for (size_t i = 0; i < std::size(fields); i++) {
    const auto &[fieldName, fieldOffset] = fields[i];
    if (uint64_t(llvmLayout->getElementOffset(i)) != fieldOffset)
      srcLoc.throwError(
          concat("mismatch between C++ 'JIT::Material::Instance' and SMDL "
                 "'_MaterialInstance' structures (field ",
                 Quoted(fieldName), " is misaligned)"));
  }
}

void FunctionType::initializeMaterialFunctions(Emitter &emitter) {
  using namespace std::literals::string_view_literals;
  auto &context{emitter.context};
  auto &compiler{context.compiler};
  // Build the qualified material name from the module identity, the
  // enclosing namespace names, and the material name.
  auto module_{decl.srcLoc.module_};
  SMDL_SANITY_CHECK(module_);
  auto qualifiedName{std::string(module_->getQualifiedName())};
  if (qualifiedName.empty()) {
    // Builtin modules have no search root; use the bare name.
    qualifiedName += "::";
    qualifiedName += module_->getName();
  }
  for (const auto &namespaceName : context.currentNamespacePath) {
    qualifiedName += "::";
    qualifiedName += namespaceName;
  }
  qualifiedName += "::";
  qualifiedName += decl.name.srcName;
  // If the host named the materials it wants (see
  // `Compiler::setDesiredMaterials()`), skip everything else: no JIT
  // entry points are emitted, so nothing this material alone references
  // is instantiated or loaded. The function itself still exists, so
  // other materials may instantiate it.
  if (!compiler.mDesiredMaterialNames.empty() &&
      std::none_of(
          compiler.mDesiredMaterialNames.begin(),
          compiler.mDesiredMaterialNames.end(), [&](const auto &desiredName) {
            return Compiler::matchesMaterialName(desiredName, qualifiedName);
          })) {
    SMDL_LOG_DEBUG(std::string(decl.srcLoc), " Skipping material ",
                   Quoted(decl.name), ": undesired by host program");
    compiler.mSkippedMaterialNames.push_back(std::move(qualifiedName));
    return;
  }
  SMDL_LOG_DEBUG(std::string(decl.srcLoc), " New material ", Quoted(decl.name));
  auto &jitMaterial{compiler.mMaterials.emplace_back()};
  jitMaterial.moduleName = std::string(decl.srcLoc.getModuleName());
  jitMaterial.moduleFileName = std::string(decl.srcLoc.getModuleFileName());
  jitMaterial.moduleDisplayName =
      std::string(decl.srcLoc.getModuleDisplayName());
  jitMaterial.lineNo = decl.srcLoc.lineNo;
  jitMaterial.materialName = std::string(decl.name.srcName);
  jitMaterial.qualifiedName = std::move(qualifiedName);
  jitMaterial.moduleIsShadowed = module_->isShadowed();
  // The JIT symbol base name is the dotted qualified name, plus a
  // disambiguating ordinal for duplicates, which only arise when the
  // module is shadowed by an equally named module under an earlier
  // search root. This keeps the symbols deterministic instead of
  // relying on LLVM's load-order '.N' uniquing.
  auto symbolBase{std::string()};
  for (auto component : splitQualifiedName(jitMaterial.qualifiedName)) {
    if (!symbolBase.empty()) symbolBase += '.';
    symbolBase += component;
  }
  if (auto numDuplicates{std::count_if(
          compiler.mMaterials.begin(), compiler.mMaterials.end(),
          [&](const auto &other) {
            return &other != &jitMaterial &&
                   other.qualifiedName == jitMaterial.qualifiedName;
          })};
      numDuplicates > 0)
    symbolBase += concat(".", numDuplicates);
  auto dfModule{context.getBuiltinModule("df")};
  SMDL_SANITY_CHECK(dfModule);
  Type *materialType{};
  Type *materialInstanceType{};
  Type *materialInstancePtrType{};
  Type *float3PtrType{context.getPointerType(context.getFloatType(3))};
  Type *floatPtrType{context.getPointerType(context.getFloatType())};
  auto constParameter{[](Type *type, std::string_view name) {
    return Parameter{type, name, {}, {}, {}, /*builtinConst=*/true};
  }};
  // The '@(visible)' entry points are called by the renderer through the
  // C++ 'JIT::Material' API, which passes distinct, sufficiently aligned,
  // dereferenceable pointers (see 'include/smdl/JIT.h'). LLVM cannot infer
  // caller-side contracts for externally visible functions, so state them
  // explicitly.
  auto markPointerParam{[&](llvm::Function *func, unsigned argIndex,
                            Type *pointeeType, uint64_t count = 1,
                            bool noAlias = true) {
    auto attrs{llvm::AttrBuilder(context.llvmContext)};
    if (noAlias) attrs.addAttribute(llvm::Attribute::NoAlias);
    attrs.addAttribute(llvm::Attribute::NonNull);
    attrs.addAttribute(llvm::Attribute::NoUndef);
    attrs.addAlignmentAttr(llvm::Align(context.getAlignOf(pointeeType)));
    attrs.addDereferenceableAttr(count * context.getSizeOf(pointeeType));
    func->addParamAttrs(argIndex, attrs);
  }};
  {
    // Generate the evaluate function:
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // @(visible) void "material_name.evaluate"(&auto out) {
    //   *out = _MaterialInstance(#bump(material_name()));
    // }
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    auto funcReturnType{context.getVoidType()};
    auto func{emitter.createFunction(
        concat(symbolBase, ".evaluate"), /*isPure=*/false, funcReturnType,
        {constParameter(context.getVoidPointerType(), "out")}, decl.srcLoc,
        [&] {
          auto materialValue{invoke(emitter, {}, decl.srcLoc)};
          materialType = materialValue.type;
          auto materialInstance{emitter.emitCall(
              context.getKeyword("_MaterialInstance"),
              emitter.emitIntrinsic("bump", materialValue, decl.srcLoc),
              decl.srcLoc)};
          materialInstanceType = materialInstance.type;
          materialInstancePtrType =
              context.getPointerType(materialInstanceType);
          auto out{
              emitter.rvalue(emitter.resolveIdentifier("out"sv, decl.srcLoc))};
          emitter.createStore(materialInstance, out);
        })};
    func->setLinkage(llvm::Function::ExternalLinkage);
    // '%state' is deliberately not 'noalias': the bump arena written
    // during evaluation is reachable through 'state.allocator'.
    markPointerParam(func, 0, context.getStateType(), 1, /*noAlias=*/false);
    markPointerParam(func, 1, materialInstanceType);
    jitMaterial.evaluate.name = func->getName().str();
  }
  verifyMaterialInstanceLayout(context, materialInstanceType, decl.srcLoc);
  // Generate the scatter and emission entry points, which all have the
  // same shape: a '@(pure visible)' wrapper that forwards the material
  // instance and its remaining parameters to the like-named function in
  // the 'df' module:
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // @(pure visible) int "material_name.scatterEvaluate"(
  //     &_MaterialInstance instance,
  //     &float3 wo,
  //     &float3 wi,
  //     int lobeMask,
  //     &float pdfFwd,
  //     &float pdfRev,
  //     &float f) {
  //   return ::df::_scatterEvaluate(
  //     instance, wo, wi, lobeMask, pdfFwd, pdfRev, f);
  // }
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // A wrapper parameter passes by pointer unless it is marked
  // 'byValue'. Pointers are the default because the aggregates and the
  // SIMD vectors have no stable by-value ABI across the JIT boundary and
  // the outputs have to be written through; a scalar input has neither
  // problem and passes in a register.
  struct WrapperParam final {
    std::string_view name{};
    Type *type{};
    uint64_t count{1};
    bool byValue{};
  };
  auto makeDfWrapper{[&](auto &jitFunc, std::string_view suffix,
                         Type *funcReturnType,
                         std::initializer_list<WrapperParam> wrapperParams) {
    auto params{ParameterList{}};
    params.push_back(constParameter(materialInstancePtrType, "instance"));
    for (const auto &wrapperParam : wrapperParams)
      params.push_back(constParameter(
          wrapperParam.byValue ? wrapperParam.type
                               : context.getPointerType(wrapperParam.type),
          wrapperParam.name));
    auto func{emitter.createFunction(
        concat(symbolBase, ".", suffix), /*isPure=*/true, funcReturnType,
        params, decl.srcLoc, [&] {
          const auto dfName{concat("_", suffix)};
          auto dfFunc{Declaration::findInModule(
              context, std::string_view(dfName), nullptr, dfModule,
              /*ignoreIfNotExported=*/false)};
          SMDL_SANITY_CHECK(dfFunc);
          auto callArgs{llvm::SmallVector<Value>{}};
          for (const auto &param : params)
            callArgs.push_back(
                emitter.resolveIdentifier(param.name, decl.srcLoc));
          emitter.emitReturn(emitter.emitCall(dfFunc->value,
                                              llvm::ArrayRef<Value>(callArgs),
                                              decl.srcLoc),
                             decl.srcLoc);
        })};
    func->setLinkage(llvm::Function::ExternalLinkage);
    markPointerParam(func, 0, materialInstanceType);
    auto argIndex{1U};
    for (const auto &wrapperParam : wrapperParams) {
      if (!wrapperParam.byValue)
        markPointerParam(func, argIndex, wrapperParam.type, wrapperParam.count);
      argIndex++;
    }
    jitFunc.name = func->getName().str();
  }};
  auto floatType{context.getFloatType()};
  auto float3Type{context.getFloatType(3)};
  auto float4Type{context.getFloatType(4)};
  auto intType{context.getIntType()};
  const auto colorSize{uint64_t(context.getColorType()->wavelengthBaseMax)};
  makeDfWrapper(jitMaterial.scatterEvaluate, "scatterEvaluate", intType,
                {{"wo", float3Type},
                 {"wi", float3Type},
                 {"lobeMask", intType, 1, /*byValue=*/true},
                 {"pdfFwd", floatType},
                 {"pdfRev", floatType},
                 {"f", floatType, colorSize}});
  makeDfWrapper(jitMaterial.scatterSample, "scatterSample", intType,
                {{"xi", float4Type},
                 {"wo", float3Type},
                 {"lobeMask", intType, 1, /*byValue=*/true},
                 {"wi", float3Type},
                 {"pdfFwd", floatType},
                 {"pdfRev", floatType},
                 {"f", floatType, colorSize},
                 {"sampledLobe", intType},
                 {"lobeChance", floatType}});
  makeDfWrapper(
      jitMaterial.emissionEvaluate, "emissionEvaluate", intType,
      {{"wi", float3Type}, {"pdf", floatType}, {"Le", floatType, colorSize}});
  makeDfWrapper(jitMaterial.emissionSample, "emissionSample", intType,
                {{"xi", float4Type},
                 {"wi", float3Type},
                 {"pdf", floatType},
                 {"Le", floatType, colorSize}});
  makeDfWrapper(jitMaterial.volumeScatterEvaluate, "volumeScatterEvaluate",
                floatType, {{"wo", float3Type}, {"wi", float3Type}});
  makeDfWrapper(jitMaterial.volumeScatterSample, "volumeScatterSample",
                floatType,
                {{"xi", float4Type}, {"wo", float3Type}, {"wi", float3Type}});
  makeDfWrapper(jitMaterial.hairScatterEvaluate, "hairScatterEvaluate", intType,
                {{"wo", float3Type},
                 {"wi", float3Type},
                 {"pdfFwd", floatType},
                 {"pdfRev", floatType},
                 {"f", floatType, colorSize}});
  makeDfWrapper(jitMaterial.hairScatterSample, "hairScatterSample", intType,
                {{"xi", float4Type},
                 {"wo", float3Type},
                 {"wi", float3Type},
                 {"pdfFwd", floatType},
                 {"pdfRev", floatType},
                 {"f", floatType, colorSize}});
  {
    // Generate the evaluate opacity function:
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // @(visible) float "material_name.evaluateOpacity"() {
    //   return material_name().geometry.cutout_opacity;
    // }
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // This evaluates only the cutout opacity: no '_MaterialInstance' and
    // no '#bump', so 'state.allocator' may be null and everything not
    // feeding the opacity is dead-code eliminated. After optimization,
    // 'deriveStaticMaterialFlags' in 'Compiler.cc' also inspects whether
    // the body folded to a constant to derive the static
    // 'MATERIAL_HAS_CUTOUT' flag.
    auto funcReturnType{context.getFloatType()};
    auto func{emitter.createFunction(
        concat(symbolBase, ".evaluateOpacity"), /*isPure=*/false,
        funcReturnType, {}, decl.srcLoc, [&] {
          emitter.emitReturn(
              emitter.accessField(
                  emitter.accessField(invoke(emitter, {}, decl.srcLoc),
                                      "geometry"sv, decl.srcLoc),
                  "cutout_opacity"sv, decl.srcLoc),
              decl.srcLoc);
        })};
    func->setLinkage(llvm::Function::ExternalLinkage);
    markPointerParam(func, 0, context.getStateType(), 1, /*noAlias=*/false);
    jitMaterial.evaluateOpacity.name = func->getName().str();
  }
  {
    // Generate the displacement evaluate function:
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // @(visible) void "material_name.displacementEvaluate"(
    //     &float3 displacement) {
    //   *displacement = material_name().geometry.displacement;
    // }
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // Like 'evaluateOpacity', this evaluates only the displacement: no
    // '_MaterialInstance' and no '#bump', so 'state.allocator' may be
    // null and everything not feeding 'geometry.displacement' is
    // dead-code eliminated. This is the per-vertex query for hosts
    // that apply displacement to geometry at load time.
    auto funcReturnType{context.getVoidType()};
    auto func{emitter.createFunction(
        concat(symbolBase, ".displacementEvaluate"), /*isPure=*/false,
        funcReturnType, {constParameter(float3PtrType, "displacement")},
        decl.srcLoc, [&] {
          auto value{emitter.accessField(
              emitter.accessField(invoke(emitter, {}, decl.srcLoc),
                                  "geometry"sv, decl.srcLoc),
              "displacement"sv, decl.srcLoc)};
          auto out{emitter.rvalue(
              emitter.resolveIdentifier("displacement"sv, decl.srcLoc))};
          emitter.createStore(emitter.rvalue(value), out);
        })};
    func->setLinkage(llvm::Function::ExternalLinkage);
    markPointerParam(func, 0, context.getStateType(), 1, /*noAlias=*/false);
    markPointerParam(func, 1, context.getFloatType(3)); // displacement
    jitMaterial.displacementEvaluate.name = func->getName().str();
  }
  {
    // Generate the volume evaluate function:
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // @(visible) void "material_name.volumeEvaluate"(
    //     &float sigma_a,
    //     &float sigma_s,
    //     &float emission) {
    //   _volumeEvaluate(material_name(), sigma_a, sigma_s, emission);
    // }
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // Like 'evaluateOpacity', this evaluates only the volume
    // coefficient expressions: no '_MaterialInstance' and no '#bump',
    // so 'state.allocator' may be null and everything not feeding the
    // coefficients is dead-code eliminated. Unlike instance
    // evaluation, renderers call this at arbitrary points inside a
    // heterogeneous medium with a partial object-space state (see
    // 'JIT::Material::volumeEvaluate'). After optimization,
    // 'deriveStaticMaterialFlags' in 'Compiler.cc' inspects whether
    // the body still reads the state to derive the static
    // 'MATERIAL_HAS_HETEROGENEOUS_VOLUME' flag.
    auto funcReturnType{context.getVoidType()};
    auto func{emitter.createFunction(
        concat(symbolBase, ".volumeEvaluate"), /*isPure=*/false, funcReturnType,
        {constParameter(floatPtrType, "sigma_a"),
         constParameter(floatPtrType, "sigma_s"),
         constParameter(floatPtrType, "emission")},
        decl.srcLoc, [&] {
          emitter.emitCall(
              context.getKeyword("_volumeEvaluate"),
              llvm::ArrayRef<Value>{
                  invoke(emitter, {}, decl.srcLoc),
                  emitter.resolveIdentifier("sigma_a"sv, decl.srcLoc),
                  emitter.resolveIdentifier("sigma_s"sv, decl.srcLoc),
                  emitter.resolveIdentifier("emission"sv, decl.srcLoc)},
              decl.srcLoc);
        })};
    func->setLinkage(llvm::Function::ExternalLinkage);
    markPointerParam(func, 0, context.getStateType(), 1, /*noAlias=*/false);
    markPointerParam(func, 1, context.getFloatType(), // sigma_a
                     context.getColorType()->wavelengthBaseMax);
    markPointerParam(func, 2, context.getFloatType(), // sigma_s
                     context.getColorType()->wavelengthBaseMax);
    markPointerParam(func, 3, context.getFloatType(), // emission
                     context.getColorType()->wavelengthBaseMax);
    jitMaterial.volumeEvaluate.name = func->getName().str();
  }
  {
    // Generate the thin-walled probe, compile-time scaffolding that
    // 'deriveStaticMaterialFlags' in 'Compiler.cc' inspects for a
    // constant 'thin_walled' and then erases; it is never a host entry
    // point.
    auto funcReturnType{context.getIntType()};
    auto func{emitter.createFunction(
        concat(symbolBase, ".thinWalledProbe"), /*isPure=*/false,
        funcReturnType, {}, decl.srcLoc, [&] {
          emitter.emitReturn(
              emitter.accessField(invoke(emitter, {}, decl.srcLoc),
                                  "thin_walled"sv, decl.srcLoc),
              decl.srcLoc);
        })};
    func->setLinkage(llvm::Function::ExternalLinkage);
  }
  {
    // Generate the displacement probe, compile-time scaffolding in the
    // mold of the thin-walled probe: it returns 'geometry.displacement'
    // itself, so 'deriveStaticMaterialFlags' in 'Compiler.cc' can
    // inspect whether the body folded to a constant vector, settling
    // 'MATERIAL_HAS_DISPLACEMENT'. Erased after inspection; never a
    // host entry point.
    auto funcReturnType{context.getFloatType(3)};
    auto func{emitter.createFunction(
        concat(symbolBase, ".displacementProbe"), /*isPure=*/false,
        funcReturnType, {}, decl.srcLoc, [&] {
          emitter.emitReturn(
              emitter.accessField(
                  emitter.accessField(invoke(emitter, {}, decl.srcLoc),
                                      "geometry"sv, decl.srcLoc),
                  "displacement"sv, decl.srcLoc),
              decl.srcLoc);
        })};
    func->setLinkage(llvm::Function::ExternalLinkage);
  }
  {
    // Compute the structural static flags, which are type-level facts:
    // 'Type::isDefault()' is the same definition the '#isDefault'
    // intrinsic uses in the api '_MaterialInstance.flags' initializer,
    // so the invariant '(instance.flags & staticFlagsKnown) ==
    // staticFlags' holds by construction. The value-dependent bits
    // ('MATERIAL_THIN_WALLED', 'MATERIAL_HAS_CUTOUT') are filled in
    // later by 'deriveStaticMaterialFlags' in 'Compiler.cc'.
    auto fieldTypeAtPath{
        [&](std::initializer_list<std::string_view> path) -> Type * {
          Type *type{materialType};
          for (auto name : path) {
            auto structType{llvm::dyn_cast_if_present<StructType>(type)};
            SMDL_SANITY_CHECK(structType);
            auto seq{ParameterList::LookupSequence{}};
            SMDL_SANITY_CHECK_MSG(
                structType->params.getLookupSequence(name, seq) && !seq.empty(),
                "cannot resolve material field");
            type = seq.back().first->type;
          }
          return type;
        }};
    auto addStaticFlag{
        [&](int flag, std::initializer_list<std::string_view> path) {
          jitMaterial.staticFlagsKnown |= flag;
          if (!fieldTypeAtPath(path)->isDefault()) {
            jitMaterial.staticFlags |= flag;
          }
        }};
    addStaticFlag(JIT::MATERIAL_HAS_SURFACE, {"surface"});
    addStaticFlag(JIT::MATERIAL_HAS_BACKFACE, {"backface"});
    addStaticFlag(JIT::MATERIAL_HAS_SURFACE_EMISSION,
                  {"surface", "emission", "emission"});
    addStaticFlag(JIT::MATERIAL_HAS_BACKFACE_EMISSION,
                  {"backface", "emission", "emission"});
    addStaticFlag(JIT::MATERIAL_HAS_VOLUME, {"volume"});
    addStaticFlag(JIT::MATERIAL_HAS_HAIR, {"hair"});
  }
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
      // #assert(#typeOf(arr0) == #typeOf(arr1));
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

  if (!sizeName.empty()) {
    const auto size{int64_t(inferredArrayType->size)};
    // The same scope may already bind the size name: a previous parameter
    // in `(const float[<N>] a, const auto[<N>] b)` binds `N` for its own
    // argument first. A binding that agrees is shared; a binding that
    // disagrees is a hard error, never a silent rebind, because the body
    // indexes every array with whichever binding survived.
    if (auto existing{
            emitter.findSameScopeDeclaration(std::string_view(sizeName))}) {
      const auto existingSize{existing->value.getComptimeSignedInt()};
      if (!existingSize)
        srcLoc.throwError("inferred array size name ", Quoted(sizeName),
                          " conflicts with a declaration of ", Quoted(sizeName),
                          " in the same scope");
      if (*existingSize != size)
        srcLoc.throwError(
            "inferred array size ", Quoted(sizeName), " = ",
            std::to_string(size), " conflicts with ", Quoted(sizeName), " = ",
            std::to_string(*existingSize), " already bound in the same scope");
    } else {
      // The temporary view is safe: 'declare' interns the name.
      emitter.declare(std::string_view(sizeName), nullptr,
                      emitter.context.getComptimeInt(int(size)));
    }
  }

  // Delegate.
  return inferredArrayType->invoke(emitter, args, srcLoc);
}
//--}

//--{ MetaType
bool MetaType::hasNonVoidField(Emitter &emitter, Value value,
                               std::string_view name,
                               const SourceLocation &srcLoc) {
  // Meta-types resolve their fields from the value, so there is no
  // type-level answer to give: ask 'accessField()', which is the only
  // implementation that never emits instructions and reports a missing
  // name by returning void.
  return value && !accessField(emitter, value, name, srcLoc).isVoid();
}

Value MetaType::accessField(Emitter &emitter, Value value,
                            std::string_view name,
                            const SourceLocation &srcLoc) {
  auto &context{emitter.context};
  if (value.isComptimeMetaType(context)) {
    auto type{value.getComptimeMetaType(context, srcLoc)};
    // Make static fields available
    if (auto structType{llvm::dyn_cast<StructType>(type)}) {
      const auto &staticFields{structType->getInstanceOf().staticFields};
      if (auto itr{staticFields.find(name)}; itr != staticFields.end())
        return itr->second;
    }
  } else if (value.isComptimeMetaModule(context)) {
    // Make exported declarations available
    auto module_{value.getComptimeMetaModule(context, srcLoc)};
    if (auto declaration{Declaration::findInModule(
            context, name, emitter.getLLVMFunction(), module_)})
      return declaration->value;
  } else if (value.isComptimeMetaNamespace(context)) {
    // Make declarations available. 'export' only gates access from other
    // modules, not from the namespace's own module.
    auto namespace_{value.getComptimeMetaNamespace(context, srcLoc)};
    if (auto declaration{Declaration::resolveInScope(
            context, name, emitter.getLLVMFunction(), namespace_->scope,
            /*ignoreIfNotExported=*/
            namespace_->srcLoc.module_ != emitter.currentModule,
            std::numeric_limits<uint64_t>::max(), nullptr)})
      return declaration->value;
  }
  return RValue(context.getVoidType(), nullptr);
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
  if (isAbstract()) {
    if (args.empty() || args.isNull())
      srcLoc.throwError("cannot zero construct abstract pointer ",
                        Quoted(displayName));
    if (args.isOnePositional()) {
      auto value{args[0].value};
      if (value.type->isPointer() &&
          emitter.context.isPerfectlyConvertible(value.type->getPointeeType(),
                                                 pointeeType)) {
        return emitter.rvalue(value);
      }
    }
  } else {
    if (args.empty() || args.isNull()) {
      return Value::zero(this);
    }
    if (args.isOnePositional()) {
      auto value{args[0].value};
      if (value.type->isPointer()) {
        return RValue(this, emitter.rvalue(value));
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
          value = emitter.lvalue(value);
          // NOTE: The way the scopes and lifetimes work right now, this
          // does not actually work. The lifetime would implicitly end before
          // being used in argument conversions and thus leads to undefined
          // behavior, so for now we "leak" these lvalues.
          // emitter.declare(/*name=*/{}, /*node=*/{}, value);
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
      emitter, LValue(pointeeType, emitter.rvalue(value)), name, srcLoc);
}

Value PointerType::accessIndex(Emitter &emitter, Value value, Value i,
                               const SourceLocation &srcLoc) {
  if (pointeeType->isVoid())
    srcLoc.throwError("cannot index into ", Quoted(displayName));
  return LValue(pointeeType, emitter.builder.CreateGEP(
                                 pointeeType->llvmType, emitter.rvalue(value),
                                 {emitter.rvalue(i).llvmValue}));
}
//--}

//--{ StateType
StateType::StateType(Context &context) {
  displayName = "state";
#define ADD_FIELD(name) \
  mFields.push_back(    \
      {context.getType(&State::name), #name, uint64_t(offsetof(State, name))})
  ADD_FIELD(allocator);
  ADD_FIELD(user_data);
  ADD_FIELD(wavelength_base);
  ADD_FIELD(wavelength_min);
  ADD_FIELD(wavelength_max);
  ADD_FIELD(wavelength_weight);
  ADD_FIELD(meters_per_scene_unit);
  ADD_FIELD(animation_time);
  ADD_FIELD(object_id);
  ADD_FIELD(ptex_face_id);
  ADD_FIELD(ptex_face_uv);
  ADD_FIELD(position);
  ADD_FIELD(direction);
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
  ADD_FIELD(rng);
  ADD_FIELD(transport);
  ADD_FIELD(scattering_order);
  ADD_FIELD(travel_distance);
  ADD_FIELD(cone_angle);
  ADD_FIELD(cone_width);
  ADD_FIELD(texture_density);
#undef ADD_FIELD
  auto llvmTypes{llvm::SmallVector<llvm::Type *>{}};
  for (auto &field : mFields) {
    SMDL_SANITY_CHECK(field.type);
    SMDL_SANITY_CHECK(field.type->llvmType);
    llvmTypes.push_back(field.type->llvmType);
  }
  llvmType = llvm::StructType::create(context, llvmTypes, displayName);
  auto llvmLayout{context.llvmLayout.getStructLayout(
      static_cast<llvm::StructType *>(llvmType))};
  for (unsigned i = 0; i < mFields.size(); i++)
    if (mFields[i].offset != uint64_t(llvmLayout->getElementOffset(i)))
      throw Error(
          concat("mismatch between C++ and SMDL 'state' structures (field ",
                 Quoted(mFields[i].name), " is misaligned)"));
}

Value StateType::accessField(Emitter &emitter, Value value,
                             std::string_view name,
                             const SourceLocation &srcLoc) {
  SMDL_SANITY_CHECK(value.isLValue());
  for (unsigned i = 0; i < mFields.size(); i++) {
    const auto &field{mFields[i]};
    if (field.name == name) {
      auto llvmValue{
          emitter.builder.CreateStructGEP(value.type->llvmType, value, i)};
      if (value.llvmValue->hasName())
        llvmValue->setName(concat(value.llvmValue->getName().str(), ".", name));
      return LValue(field.type, llvmValue);
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
  // Construct from nothing, null, or another string.
  if (auto trivial{invokeTrivialCases(emitter, args)}) {
    return *trivial;
  }
  // Construct from enum, call the enum-to-string conversion function.
  if (args.isOnePositional()) {
    auto value{args[0].value};
    if (auto enumType{llvm::dyn_cast<EnumType>(value.type)})
      return RValue(this, emitter.builder.CreateCall(
                              enumType->llvmFuncToString->getFunctionType(),
                              enumType->llvmFuncToString,
                              {emitter.rvalue(value).llvmValue}));
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
      return RValue(
          emitter.context.getIntType(),
          llvmEmitCast(emitter.builder,
                       llvm::emitStrLen(emitter.rvalue(value), emitter.builder,
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
  emitter.rejectSameScopeShadow(decl.name, decl.srcLoc);
  emitter.declare(decl.name, &decl, emitter.context.getComptimeMetaType(this));
  emitter.captureResolutionAnchor(params);
  // The interior declarations (static fields, inferred sizes in field
  // types) are popped from the chain on exit; the transparent scope pops
  // their map entries with them.
  SMDL_PRESERVE(emitter.scope);
  emitter.scope = emitter.pushScope(/*transparent=*/true);
  // Initialize tags.
  for (auto &tag : decl.tags) {
    emitter.emit(tag.type);
    auto tagType{llvm::dyn_cast<TagType>(tag.type->type)};
    if (!tagType) decl.srcLoc.throwError("unknown tag");
    if (tag.isDefault()) {
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
    if (field.type->hasQualifier("static")) {
      const char *reasonForError = //
          field.type->hasQualifier("inline")   ? "must not be 'inline'"
          : !field.type->hasQualifier("const") ? "must also be 'const'"
          : !field.exprInit                    ? "must have initializer"
                                               : nullptr;
      if (reasonForError)
        field.name.srcLoc.throwError("field ", Quoted(field.name),
                                     " declared 'static' ", reasonForError);
      auto value{emitter.invoke(fieldType, emitter.emit(field.exprInit),
                                field.name.srcLoc)};
      staticFields[field.name.srcName] = value;
      emitter.declare(field.name, &field, value);
      emitter.captureResolutionAnchor(params);
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
    llvmType = llvm::StructType::create(
        emitter.context, params.getLLVMFieldTypes(), displayName);
}

StructType *
StructType::getInstance(Context &context,
                        const llvm::SmallVector<Type *> &paramTypes,
                        llvm::ArrayRef<llvm::Constant *> paramConstants) {
  SMDL_SANITY_CHECK(params.isAbstract());
  SMDL_SANITY_CHECK(params.size() == paramTypes.size());
  SMDL_SANITY_CHECK(paramConstants.empty() ||
                    paramConstants.size() == paramTypes.size());
  auto key{llvm::SmallVector<std::pair<Type *, llvm::Constant *>>{}};
  key.reserve(paramTypes.size());
  for (size_t i{}; i < paramTypes.size(); i++) {
    auto paramConstant{paramConstants.empty() ? nullptr : paramConstants[i]};
    SMDL_SANITY_CHECK(!paramConstant || params[i].isConst());
    key.push_back({paramTypes[i], paramConstant});
  }
  auto &structType{instances[key]};
  if (!structType) {
    structType = context.allocator.allocate<StructType>(decl);
    structType->instanceOf = this;
    structType->tags = tags;
    structType->params = params;
    for (size_t i{}; i < params.size(); i++) {
      SMDL_SANITY_CHECK(paramTypes[i]);
      SMDL_SANITY_CHECK(!paramTypes[i]->isAbstract());
      structType->params[i].type = paramTypes[i];
      structType->params[i].bakedConstant = key[i].second;
    }
    structType->llvmType = llvm::StructType::create(
        context, structType->params.getLLVMFieldTypes(),
        structType->displayName);
  }
  return structType.get();
}

StructType *StructType::getCommonSiblingInstance(Context &context, Type *typeA,
                                                 Type *typeB) {
  auto structTypeA{llvm::dyn_cast_if_present<StructType>(typeA)};
  auto structTypeB{llvm::dyn_cast_if_present<StructType>(typeB)};
  if (!structTypeA || !structTypeB || !structTypeA->instanceOf ||
      structTypeA->instanceOf != structTypeB->instanceOf)
    return nullptr;
  SMDL_SANITY_CHECK(structTypeA->params.size() == structTypeB->params.size());
  auto paramTypes{llvm::SmallVector<Type *>{}};
  auto paramConstants{llvm::SmallVector<llvm::Constant *>{}};
  paramTypes.reserve(structTypeA->params.size());
  paramConstants.reserve(structTypeA->params.size());
  for (size_t i{}; i < structTypeA->params.size(); i++) {
    auto &paramA{structTypeA->params[i]};
    auto &paramB{structTypeB->params[i]};
    if (paramA.type != paramB.type) return nullptr;
    paramTypes.push_back(paramA.type);
    paramConstants.push_back(paramA.bakedConstant == paramB.bakedConstant
                                 ? paramA.bakedConstant
                                 : nullptr);
  }
  return structTypeA->instanceOf->getInstance(context, paramTypes,
                                              paramConstants);
}

bool StructType::isAbstract() {
  // If in `initialize()` we failed to construct the LLVM type
  // because at least one parameter type had no LLVM type, then
  // this type is definitely abstract!
  if (!llvmType) return true;
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
    if (auto structType{llvm::dyn_cast<StructType>(args[0].value.type)}) {
      if (structType == this || structType->isInstanceOf(this)) {
        return emitter.rvalue(args[0].value);
      }
      // A sibling instance converts to this instance if this instance
      // bakes a subset of its constants: the demoted constants
      // materialize into storage. This is the conversion the merges of
      // 'getCommonSiblingInstance' resolve through, so e.g. a '?:' over
      // two constructions differing only in a 'const' field value stays
      // one struct type instead of becoming a union.
      if (instanceOf && structType->instanceOf == instanceOf &&
          getCommonSiblingInstance(emitter.context, structType, this) == this) {
        auto value{emitter.rvalue(args[0].value)};
        auto result{Value::zero(this)};
        for (size_t i{}; i < params.size(); i++) {
          if (params[i].isBaked() || params[i].type->isVoid()) continue;
          auto &sourceParam{structType->params[i]};
          auto elem{RValue(
              sourceParam.type,
              sourceParam.isBaked()
                  ? sourceParam.bakedConstant
                  : emitter.builder.CreateExtractValue(
                        value, {structType->params.getLLVMFieldIndex(i)}))};
          result = emitter.insert(result, elem, i, srcLoc);
        }
        return result;
      }
    }
  }
  // Why each candidate was rejected, so that a failure to construct can say
  // why instead of only that it failed. Mirrors the candidate notes in
  // 'FunctionType::resolveOverload'.
  auto candidateNotes{std::string{}};
  auto addCandidateNote{
      [&](std::string_view kind, const ParameterList &candidateParams,
          const SourceLocation &declSrcLoc, std::string_view reason) {
        appendCandidateNote(candidateNotes, kind, displayName, candidateParams,
                            declSrcLoc, reason);
      }};
  auto whyNot{std::string{}};
  // Explicit constructors are applied first, so a struct with constructors
  // controls its own construction. Field-wise construction below is the
  // fallback; callers can always force it by naming the fields explicitly,
  // provided no constructor parameter names collide exactly.
  // TODO Overload resolution?
  auto viableConstructors{llvm::SmallVector<Constructor *>{}};
  for (auto &constructor : getInstanceOf().constructors) {
    const auto declSrcLoc{constructor.astConstructor->name.srcLoc};
    if (constructor.isInvoking) {
      addCandidateNote("constructor", constructor.params, declSrcLoc,
                       "constructor is already being invoked (a constructor "
                       "must not construct its own type)");
      continue;
    }
    whyNot.clear();
    if (emitter.canResolveArguments(constructor.params, args, srcLoc,
                                    &whyNot)) {
      viableConstructors.push_back(&constructor);
    } else {
      addCandidateNote("constructor", constructor.params, declSrcLoc,
                       dropSourceLocation(std::move(whyNot), srcLoc));
    }
  }
  if (viableConstructors.size() == 1) {
    auto &constructor{*viableConstructors[0]};
    auto resolvedArgs{
        emitter.resolveArguments(constructor.params, args, srcLoc)};
    SMDL_PRESERVE(emitter.scope, emitter.anchors, constructor.isInvoking);
    emitter.restoreResolutionAnchor(constructor.params);
    constructor.isInvoking = true;
    return emitter.createFunctionImplementation(
        decl.name, !emitter.state, this, constructor.params,
        resolvedArgs.values, srcLoc, [&]() {
          emitter.setCurrentModule(decl.srcLoc);
          emitter.emitReturn(emitter.emit(constructor.astConstructor->expr),
                             srcLoc);
        });
  } else if (viableConstructors.size() > 1) {
    auto ambiguousNotes{std::string{}};
    for (auto constructor : viableConstructors) {
      ambiguousNotes += "\n  ambiguous candidate: constructor ";
      ambiguousNotes += toSignatureString(displayName, constructor->params);
      ambiguousNotes += " declared at ";
      ambiguousNotes += std::string(constructor->astConstructor->name.srcLoc);
    }
    srcLoc.throwError("cannot construct ", Quoted(displayName), " from ",
                      Quoted(std::string(args)), ambiguousNotes);
  }
  whyNot.clear();
  if (emitter.canResolveArguments(params, args, srcLoc, &whyNot)) {
    auto resolvedArgs{emitter.resolveArguments(params, args, srcLoc)};
    // Constructing a value instance directly (e.g. '#typeOf(x)(...)'): a
    // baked field is part of the type and cannot be overridden, but
    // explicitly passing the identical constant is allowed so generic
    // code can round-trip.
    if (instanceOf) {
      for (size_t iArg{}; iArg < args.size(); iArg++) {
        if (auto param{resolvedArgs.argParams[iArg]};
            param && param->isBaked()) {
          const auto &value{resolvedArgs.values[size_t(param - &params[0])]};
          if (!(value.isComptime() && value.llvmValue == param->bakedConstant))
            srcLoc.throwError(
                "cannot construct ", Quoted(displayName), ": field ",
                Quoted(param->name),
                " is baked to a compile-time constant in this instance");
        }
      }
    }
    auto resultType{this};
    if (resultType->isAbstract()) {
      // Constant-field elimination: a 'const' field whose resolved value
      // is a compile-time constant is baked into the instantiated type,
      // where it occupies no storage and reads as the constant. 'const'
      // is already not addressable, so only type identity can observe
      // the elimination.
      auto paramConstants{llvm::SmallVector<llvm::Constant *>{}};
      paramConstants.reserve(params.size());
      for (size_t i{}; i < params.size(); i++) {
        const auto &value{resolvedArgs.values[i]};
        paramConstants.push_back(
            params[i].isConst() && value.isComptime() && !value.isVoid()
                ? static_cast<llvm::Constant *>(value.llvmValue)
                : nullptr);
      }
      resultType = getInstance(
          emitter.context, resolvedArgs.getNonVariadicTypes(), paramConstants);
      // Sticky: a later construction landing on the same instance with
      // explicit arguments must not clear the flag.
      resultType->isDefaultInstance |= args.empty();
      SMDL_SANITY_CHECK(!resultType->isAbstract());
    }
    // Void values count as compile-time: they contribute no runtime data
    // ('insert()' and the in-place stores below both skip them), and
    // counting them as runtime would push an otherwise-constant aggregate
    // through the in-place path, where it can no longer fold, e.g., as a
    // module-scope initializer of a large struct with void fields.
    auto allComptime{true};
    for (const auto &value : resolvedArgs.values)
      allComptime &= value.isVoid() || value.isComptime();
    auto result{Value()};
    if (!allComptime && emitter.getLLVMFunction() &&
        emitter.returnsIndirectly(resultType)) {
      // Construct large structs in place: allocate a slot, store the
      // fields through it, and return it as an lvalue. Building them as
      // first-class 'insertvalue' chains makes the optimizer juggle
      // whole-struct SSA values, which lower poorly. Small structs and
      // compile-time constants keep the by-value construction below so
      // module scope and constant folding continue to work.
      auto lv{emitter.createAlloca(resultType, "struct.lv")};
      SMDL_SANITY_CHECK(resolvedArgs.values.size() ==
                        resultType->params.size());
      for (size_t i{}; i < resolvedArgs.values.size(); i++) {
        auto &value{resolvedArgs.values[i]};
        // NOTE: 'i' is the field index, which is the LLVM element index
        // only when no preceding field is voided or baked.
        const auto j{resultType->params.getLLVMFieldIndex(i)};
        if (j == ParameterList::NO_LLVM_FIELD) {
          SMDL_SANITY_CHECK(value.isVoid() || resultType->params[i].isBaked());
          continue;
        }
        SMDL_SANITY_CHECK(!value.isVoid());
        emitter.createStore(
            value, LValue(value.type, emitter.builder.CreateStructGEP(
                                          resultType->llvmType, lv, j)));
      }
      result = LValue(resultType, lv.llvmValue);
    } else {
      result = Value::zero(resultType);
      auto i{size_t(0)};
      for (auto &value : resolvedArgs.values)
        result = emitter.insert(result, value, i++, srcLoc);
    }
    if (decl.stmtFinalize) {
      SMDL_PRESERVE(emitter.scope, emitter.anchors);
      emitter.restoreResolutionAnchor(params);
      emitter.handleScope(nullptr, nullptr, [&] {
        emitter.labelReturn = {};   // Invalidate!
        emitter.labelBreak = {};    // Invalidate!
        emitter.labelContinue = {}; // Invalidate!
        emitter.setCurrentModule(decl.srcLoc);
        const auto inPlace{result.isLValue()};
        auto lv{emitter.lvalue(result)};
        for (auto &param : params)
          emitter.declare(param.name, &decl,
                          emitter.accessField(lv, param.name, srcLoc));
        emitter.emit(decl.stmtFinalize);
        if (inPlace) {
          result = lv;
        } else {
          result = emitter.rvalue(lv);
          emitter.createLifetimeEnd(lv);
        }
      });
    }
    return result;
  }
  addCandidateNote("field-wise", params, decl.name.srcLoc,
                   dropSourceLocation(std::move(whyNot), srcLoc));
  srcLoc.throwError("cannot construct ", Quoted(displayName), " from ",
                    Quoted(std::string(args)), candidateNotes);
  return {};
}

bool StructType::hasField(std::string_view name) {
  // Resolve through 'getLookupSequence()' rather than scanning 'params'
  // flat, so that this agrees with 'accessField()' below about fields
  // reached through an 'inline' parameter.
  auto seq{ParameterList::LookupSequence{}};
  if (params.getLookupSequence(name, seq)) return true;
  return getInstanceOf().staticFields.contains(name);
}

bool StructType::hasNonVoidField(Emitter &, Value, std::string_view name,
                                 const SourceLocation &) {
  auto seq{ParameterList::LookupSequence{}};
  if (params.getLookupSequence(name, seq))
    return !seq.back().first->type->isVoid();
  if (auto itr{getInstanceOf().staticFields.find(name)};
      itr != getInstanceOf().staticFields.end())
    return !itr->second.isVoid();
  return false;
}

Value StructType::accessField(Emitter &emitter, Value value,
                              std::string_view name,
                              const SourceLocation &srcLoc) {
  auto seq{ParameterList::LookupSequence{}};
  if (params.getLookupSequence(name, seq)) {
    const auto name0{value.llvmValue->getName().str()};
    bool isConst{};
    for (auto [param, i] : seq) {
      isConst |= param->isConst();
      if (i == ParameterList::NO_LLVM_FIELD) {
        // A baked field occupies no storage and reads as its compile-time
        // constant. Unlike a voided field it may be an interior step of
        // the sequence: the walk continues by extraction, which folds on
        // constants.
        if (param->isBaked()) {
          value = RValue(param->type, param->bakedConstant);
          continue;
        }
        // A voided field occupies no storage, so there is nothing to
        // address: it resolves to the void value itself, as an rvalue. It
        // can only ever be the last step of a sequence, because 'void' has
        // no fields of its own to descend into. Returning here also skips
        // the name decoration below, which would dereference the null
        // 'llvmValue' that a void value carries.
        return RValue(param->type, nullptr);
      }
      value = Value(
          value.kind, param->type,
          value.isLValue()
              ? emitter.builder.CreateStructGEP(value.type->llvmType, value, i)
              : emitter.builder.CreateExtractValue(value, {i}));
    }
    // Skip the decoration when the walk ended on a constant (a baked
    // field, or an extraction that folded): constants cannot carry names.
    if (!name0.empty() && !value.isLLVMConstant())
      value.llvmValue->setName(concat(name0, ".", name));
    return isConst ? emitter.rvalue(value) : value;
  }
  if (auto itr{getInstanceOf().staticFields.find(name)};
      itr != getInstanceOf().staticFields.end())
    return itr->second;
  auto fieldNames{params.getNames()};
  for (const auto &[staticName, staticValue] : getInstanceOf().staticFields)
    fieldNames.push_back(staticName);
  auto suggestion{std::string()};
  if (auto similar{suggestNearestName(name, fieldNames)}; !similar.empty())
    suggestion = concat("; did you mean ", Quoted(similar), "?");
  srcLoc.throwError("no field ", Quoted(name), " in struct ",
                    Quoted(displayName), suggestion);
  return {};
}

Value StructType::insert(Emitter &emitter, Value value, Value elem, unsigned i,
                         const SourceLocation &srcLoc) {
  SMDL_SANITY_CHECK(i < params.size());
  // NOTE: 'i' is the field index, which is the LLVM element index only
  // when no preceding field is voided or baked. Neither has storage to
  // insert into; construction is the only writer of a baked field and
  // always passes its own constant.
  const auto j{params.getLLVMFieldIndex(i)};
  if (j == ParameterList::NO_LLVM_FIELD) return emitter.rvalue(value);
  return RValue(this, emitter.builder.CreateInsertValue(
                          emitter.rvalue(value),
                          emitter.invoke(params[i].type, elem, srcLoc), {j}));
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
    return emitter.rvalue(value);
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
    if (i) displayName += " | ";
    displayName += caseTypeNames[i].str();
  }
  displayName += ')';

  // Determine LLVM type. The payload is an array of alignment-carrying
  // chunks rather than a single wide vector. The chunk vector type
  // guarantees the alignment of the most-aligned case type even when the
  // union nests inside other types, while the array bounds how much of
  // the payload SROA promotes into any one SSA value: a single
  // union-sized vector otherwise reappears in optimized code as giant
  // byte shuffles and unmergeable scalar stores.
  uint64_t chunkSize{std::max<uint64_t>(requiredAlign, 8)};
  uint64_t numChunks{(requiredSize + chunkSize - 1) / chunkSize};
  auto i64Type{llvm::Type::getInt64Ty(context)};
  auto chunkType{chunkSize == 8
                     ? i64Type
                     : static_cast<llvm::Type *>(
                           llvm::FixedVectorType::get(i64Type, chunkSize / 8))};
  llvmType =
      llvm::StructType::create({llvm::ArrayType::get(chunkType, numChunks),
                                context.getIntType()->llvmType},
                               "union_t");
  SMDL_SANITY_CHECK(requiredAlign <= context.getAlignOf(this));
}

Value UnionType::invoke(Emitter &emitter, const ArgumentList &args,
                        const SourceLocation &srcLoc) {
  auto &context{emitter.context};
  if (args.empty() || args.isNull()) {
    if (!isOptionalUnion())
      srcLoc.throwError("cannot zero construct non-optional union type ",
                        Quoted(displayName));
    auto result{Value::zero(this)};
    result.llvmValue = emitter.builder.CreateInsertValue(
        result.llvmValue, context.getComptimeInt(int(caseTypes.size() - 1)),
        {1U});
    return result;
  }
  if (args.isOnePositional(this)) {
    return emitter.rvalue(args[0].value);
  }
  if (args.isOnePositional()) {
    auto arg{args[0].value};
    if (auto argUnionType{llvm::dyn_cast<UnionType>(arg.type)}) {
      auto lvArg{emitter.lvalue(arg)};
      auto lv{emitter.createAlloca(this, "")};
      emitter.builder.CreateStore(Value::zero(this), lv);
      emitter.builder.CreateMemCpy(
          lv, llvm::Align(context.getAlignOf(this)), //
          lvArg, llvm::Align(context.getAlignOf(argUnionType)),
          std::min(requiredSize, argUnionType->requiredSize));
      if (!arg.isLValue()) emitter.createLifetimeEnd(lvArg);
      auto index{emitter.rvalue(emitter.accessIndex(
          context.getComptimeUnionIndexMap(argUnionType, this),
          emitter.accessField(arg, "#idx", srcLoc), srcLoc))};
      emitter.builder.CreateStore(index,
                                  emitter.accessField(lv, "#idx", srcLoc));
      // Large unions stay memory-resident: return the slot as an lvalue
      // instead of loading the whole payload into an SSA value.
      auto result{Value()};
      if (emitter.returnsIndirectly(this)) {
        result = lv;
      } else {
        result = emitter.rvalue(lv);
        emitter.createLifetimeEnd(lv);
      }
      if (!hasAllCaseTypes(argUnionType)) {
        auto [blockFail, blockPass] =
            emitter.createBlocks<2>("union_conversion", {".fail", ".pass"});
        emitter.builder.CreateCondBr(emitter.emitOp(BINOP_CMP_LT, index,
                                                    context.getComptimeInt(0),
                                                    srcLoc),
                                     blockFail, blockPass);
        emitter.builder.SetInsertPoint(blockFail);
        emitter.emitPanic(context.getComptimeString("union conversion failed"),
                          srcLoc);
        emitter.builder.CreateBr(blockPass);
        emitter.builder.SetInsertPoint(blockPass);
      }
      return result;
    } else {
      if (!hasCaseType(arg.type))
        srcLoc.throwError("cannot construct union ", Quoted(displayName),
                          " from ", Quoted(arg.type->displayName));
      auto i{getCaseTypeIndex(arg.type)};
      auto lv{emitter.createAlloca(this, "union.lv")};
      emitter.createLifetimeStart(lv);
      emitter.builder.CreateStore(Value::zero(this), lv); // zeroinitializer
      emitter.createStore(arg, LValue(arg.type, lv.llvmValue));
      emitter.builder.CreateStore(context.getComptimeInt(i),
                                  emitter.accessField(lv, "#idx", srcLoc));
      // Large unions stay memory-resident: return the slot as an lvalue
      // instead of loading the whole payload into an SSA value.
      if (emitter.returnsIndirectly(this)) return lv;
      auto rv{emitter.rvalue(lv)};
      emitter.createLifetimeEnd(lv);
      return rv;
    }
  }
  srcLoc.throwError("cannot construct union ", Quoted(displayName), " from ",
                    Quoted(std::string(args)));
  return Value();
}

bool UnionType::hasNonVoidField(Emitter &emitter, Value, std::string_view name,
                                const SourceLocation &srcLoc) {
  if (name == "#ptr" || name == "#idx") return true;
  // Every non-void case must have the field, else 'accessField()' below
  // throws; and at least one of them must carry something, else the visit
  // it emits yields void from every case.
  bool anyNonVoid{};
  for (auto caseType : caseTypes) {
    if (caseType->isVoid()) continue;
    if (!caseType->hasField(name)) return false;
    anyNonVoid |= caseType->hasNonVoidField(emitter, Value(), name, srcLoc);
  }
  return anyNonVoid;
}

Value UnionType::accessField(Emitter &emitter, Value value,
                             std::string_view name,
                             const SourceLocation &srcLoc) {
  if (name == "#ptr")
    return RValue(
        emitter.context.getPointerType(emitter.context.getVoidType()),
        emitter.builder.CreateStructGEP(llvmType, emitter.lvalue(value), 0));
  if (name == "#idx")
    return value.isLValue()
               ? LValue(emitter.context.getIntType(),
                        emitter.builder.CreateStructGEP(llvmType, value, 1))
               : RValue(emitter.context.getIntType(),
                        emitter.builder.CreateExtractValue(value, {1U}));
  if (hasField(name)) {
    if (value.isRValue()) {
      return accessViaLValue(emitter, value, [&](Value lv) {
        return accessField(emitter, lv, name, srcLoc);
      });
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
  caseTypes.erase(std::unique(caseTypes.begin(), caseTypes.end()),
                  caseTypes.end());
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
