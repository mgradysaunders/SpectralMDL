// vim:foldmethod=marker:foldlevel=0:fmr=--{,--}
#include "Emitter.h"

#include "smdl/BSDFMeasurement.h"
#include "smdl/Support/Logger.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/Parallel.h"
#include <atomic>

#if SMDL_HAS_PTEX
#include "Ptexture.h"
#endif // #if SMDL_HAS_PTEX

namespace smdl {

Value Emitter::createFunctionImplementation(
    std::string_view name, bool isPure, Type *returnType,
    const ParameterList &params, llvm::ArrayRef<Value> paramValues,
    const SourceLocation &srcLoc, const std::function<void()> &callback) {
  SMDL_SANITY_CHECK(params.size() == paramValues.size());
  SMDL_SANITY_CHECK(callback != nullptr);
  auto fmf{builder.getFastMathFlags()};
  SMDL_DEFER([&] { builder.setFastMathFlags(fmf); });
  SMDL_PRESERVE(labelReturn, returns, crumb);
  if (params.lastCrumb)
    crumb = params.lastCrumb;
  labelReturn.crumb = crumb;
  labelReturn.block = createBlock(llvm::StringRef(name) + ".return");
  returns.clear();
  handleScope(nullptr, nullptr, [&] {
    auto crumbsToWarnAboutSize{crumbsToWarnAbout.size()};
    labelBreak = labelContinue = {}; // Invalidate
    inDefer = false;
    for (size_t i = 0; i < params.size(); ++i)
      declareParameter(params[i], paramValues[i]);
    callback();
    if (!hasTerminator()) {
      returns.push_back(Result{RValue(context.getVoidType(), nullptr),
                               getInsertBlock(), srcLoc});
      unwind(labelReturn.crumb);
      builder.CreateBr(labelReturn.block);
    }
    for (size_t i = crumbsToWarnAboutSize; i < crumbsToWarnAbout.size(); i++)
      crumbsToWarnAbout[i]->maybeWarnAboutUnusedValue();
    crumbsToWarnAbout.resize(crumbsToWarnAboutSize);
  });
  llvmMoveBlockToEnd(labelReturn.block);
  builder.SetInsertPoint(labelReturn.block);
  return createResult(returnType, returns, srcLoc);
}

void Emitter::createFunction(llvm::Function *&llvmFunc, std::string_view name,
                             bool isPure, Type *&returnType,
                             llvm::ArrayRef<Type *> paramTypes,
                             const ParameterList &params,
                             const SourceLocation &srcLoc,
                             const std::function<void()> &callback) {
  SMDL_SANITY_CHECK(returnType && params.size() == paramTypes.size());
  auto llvmParamTys{std::vector<llvm::Type *>{}};
  if (!isPure)
    llvmParamTys.push_back(llvm::PointerType::get(context, 0));
  for (const auto &paramType : paramTypes) {
    SMDL_SANITY_CHECK(paramType);
    SMDL_SANITY_CHECK(paramType->llvmType);
    llvmParamTys.push_back(paramType->llvmType);
  }
  // Interpret null callback as foreign.
  if (!callback) {
    SMDL_SANITY_CHECK(!returnType->isAbstract());
    auto llvmFuncTy{llvm::FunctionType::get(returnType->llvmType, llvmParamTys,
                                            /*isVarArg=*/false)};
    auto llvmCallee{context.getBuiltinCallee(name, llvmFuncTy)};
    if (llvmFuncTy != llvmCallee.getFunctionType()) {
      srcLoc.throwError("conflicting definitions of '@(foreign)' function ",
                        Quoted(name));
    }
    llvmFunc = static_cast<llvm::Function *>(llvmCallee.getCallee());
    return;
  }
  // Initialize LLVM function with a placeholder. If we have a return type
  // with a well-defined LLVM type, then use that as the placeholder return
  // type in order to permit recursion. Otherwise use
  // `Context::llvmIncompleteReturnTy`.
  llvmFunc = llvm::Function::Create(
      llvm::FunctionType::get(returnType->llvmType
                                  ? returnType->llvmType
                                  : context.llvmIncompleteReturnTy,
                              llvmParamTys,
                              /*isVarArg=*/false),
      llvm::Function::InternalLinkage, "", context.llvmModule);

  auto lastIP{builder.saveIP()};
  {
    SMDL_PRESERVE(state, inlines);
    state = {}; // Invalidate
    inlines.clear();
    builder.SetInsertPoint(
        llvm::BasicBlock::Create(context, "entry", llvmFunc));
    auto llvmArg{llvmFunc->arg_begin()};
    if (!isPure) {
      llvmArg->setName("state");
      state =
          RValue(context.getPointerType(context.getStateType()), &*llvmArg++);
    }
    auto paramValues{llvm::SmallVector<Value>()};
    for (size_t i = 0; i < params.size(); i++) {
      llvmArg->setName(params[i].name);
      paramValues.push_back(RValue(paramTypes[i], &*llvmArg++));
    }
    auto result{createFunctionImplementation(name, isPure, returnType, params,
                                             paramValues, srcLoc, callback)};
    if (!result || result.type->isVoid()) {
      builder.CreateRetVoid();
    } else {
      builder.CreateRet(result);
    }
    returnType = result.type;
    SMDL_SANITY_CHECK(returnType);
    SMDL_SANITY_CHECK(!returnType->isAbstract());
    if (returnType->llvmType != llvmFunc->getFunctionType()->getReturnType()) {
      auto llvmFunc0{llvmFunc};
      llvmFunc = llvm::Function::Create(
          llvm::FunctionType::get(returnType->llvmType,
                                  llvmFunc->getFunctionType()->params(),
                                  /*isVarArg=*/false),
          llvm::Function::InternalLinkage, "", context.llvmModule);
      llvmFunc->splice(llvmFunc->begin(), llvmFunc0);
      auto llvmArg0{llvmFunc0->arg_begin()};
      auto llvmArg1{llvmFunc->arg_begin()};
      while (llvmArg1 != llvmFunc->arg_end()) {
        llvmArg1->setName(llvmArg0->getName());
        llvmArg0->replaceAllUsesWith(&*llvmArg1);
        ++llvmArg0;
        ++llvmArg1;
      }
      llvmFunc0->replaceAllUsesWith(llvmFunc);
      llvmFunc0->eraseFromParent();
    }
    llvmFunc->setName(llvm::StringRef(name));
    // Verify.
    llvm::EliminateUnreachableBlocks(*llvmFunc);
    std::string message{};
    if (llvm::raw_string_ostream os{message};
        llvm::verifyFunction(*llvmFunc, &os))
      srcLoc.throwError("function ", Quoted(name),
                        " LLVM-IR verification failed: ", message);
    // Inline.
    for (auto &inlineReq : inlines) {
      auto result{llvmForceInline(inlineReq.value, //
                                  inlineReq.isRecursive)};
      if (!result.isSuccess())
        inlineReq.srcLoc.logWarn(std::string("cannot force inline: ") +
                                 result.getFailureReason());
    }
  }
  builder.restoreIP(lastIP);
}

Value Emitter::createAlloca(Type *type, const llvm::Twine &name) {
  auto llvmFunc{getLLVMFunction()};
  SMDL_SANITY_CHECK(llvmFunc, "tried to alloca outside of LLVM function");
  auto blockEntry{&llvmFunc->getEntryBlock()};
  auto ip{builder.saveIP()};
  builder.SetInsertPoint(blockEntry, blockEntry->getFirstNonPHIOrDbgOrAlloca());
  SMDL_SANITY_CHECK(type);
  SMDL_SANITY_CHECK(type->llvmType);
  auto value{LValue(type, builder.CreateAlloca(type->llvmType, nullptr, name))};
  builder.restoreIP(ip);
  return value;
}

void Emitter::declareParameter(const Parameter &param, Value value) {
  value = invoke(param.type, value, param.getSourceLocation());
  value = param.isConst() || value.isVoid() ? value : toLValue(value);
  crumbsToWarnAbout.push_back(
      declareCrumb(param.name,
                   param.astParam   ? static_cast<AST::Node *>(param.astParam)
                   : param.astField ? static_cast<AST::Node *>(param.astField)
                                    : nullptr,
                   value));
  if (param.isInline())
    declareParameterInline(crumb->value);
}

void Emitter::declareParameterInline(Value value) {
  if (auto structType{
          llvm::dyn_cast<StructType>(value.type->getFirstNonPointerType())}) {
    for (auto &param : structType->params) {
      auto srcLoc{param.getSourceLocation()};
      declareCrumb(param.name, /*node=*/{},
                   accessField(value, param.name, srcLoc));
      if (param.isInline())
        declareParameterInline(crumb->value);
    }
  }
}

void Emitter::declareImport(Span<const std::string_view> importPath, bool isAbs,
                            AST::Decl &decl) {
  if (importPath.size() < 2)
    decl.srcLoc.throwError("invalid import path (missing '::*'?)");
  auto isDots{[](auto elem) { return elem == "." || elem == ".."; }};
  auto importedModule{
      resolveModule(importPath.drop_back(), isAbs, decl.srcLoc.module_)};
  if (!importedModule)
    decl.srcLoc.throwError("cannot resolve import identifier ",
                           Quoted(join(importPath, "::")));
  if (importPath.back() == "*") {
    importPath = importPath.drop_back();
    importPath = importPath.drop_front_while(isDots);
    declareCrumb(importPath, &decl,
                 context.getComptimeMetaModule(importedModule));
  } else {
    auto importedCrumb{Crumb::find(context, importPath.back(),
                                   getLLVMFunction(), importedModule->lastCrumb,
                                   nullptr, /*ignoreIfNotExported=*/true)};
    if (!importedCrumb)
      decl.srcLoc.throwError("cannot resolve import identifier ",
                             Quoted(join(importPath, "::")));
    declareCrumb(importPath.drop_front_while(isDots), &decl,
                 importedCrumb->value);
  }
}

void Emitter::unwind(Crumb *lastCrumb) {
  SMDL_SANITY_CHECK(!hasTerminator(), "tried to unwind after terminator");
  for (; crumb && crumb != lastCrumb; crumb = crumb->prev) {
    if (crumb->isASTDefer()) {
      handleScope(nullptr, nullptr, [&] {
        labelReturn = {};   // Invalidate!
        labelBreak = {};    // Invalidate!
        labelContinue = {}; // Invalidate!
        inDefer = true;     // More specific error messages
        emit(static_cast<AST::Defer *>(crumb->node)->stmt);
      });
    } else if (crumb->isASTPreserve()) {
      builder.CreateStore(crumb->valueToPreserve, crumb->value);
    } else if (crumb->value) {
      createLifetimeEnd(crumb->value);
    }
  }
  SMDL_SANITY_CHECK(crumb == lastCrumb, "inconsistent unwind");
}

Value Emitter::createResult(Type *type, llvm::ArrayRef<Result> results,
                            const SourceLocation &srcLoc) {
  SMDL_SANITY_CHECK(type);
  if (type->isAbstract()) {
    auto resultTypes{llvm::SmallVector<Type *>{}};
    for (auto &result : results)
      resultTypes.push_back(result.value.type ? result.value.type
                                              : context.getVoidType());
    auto resultType{
        context.getCommonType(resultTypes, /*defaultToUnion=*/true, srcLoc)};
    if (context.getConversionRule(resultType, type) ==
        CONVERSION_RULE_NOT_ALLOWED)
      srcLoc.throwError("inferred result type ",
                        Quoted(resultType->displayName),
                        " is not convertible to ", Quoted(type->displayName));
    type = resultType;
    SMDL_SANITY_CHECK(!type->isAbstract());
  }
  if (type->isVoid()) {
    return RValue(type, nullptr);
  }
  bool isAllIdenticalLValues{[&]() {
    for (auto &result : results)
      if (!(result.value.isLValue() &&
            result.value.type == results[0].value.type))
        return false;
    return true;
  }()};
  auto crumb0{crumb};
  auto phiInst{builder.CreatePHI(isAllIdenticalLValues
                                     ? context.getPointerType(type)->llvmType
                                     : type->llvmType,
                                 results.size())};
  auto ip{builder.saveIP()};
  for (auto &result : results) {
    auto value{result.value};
    auto block{result.block};
    SMDL_SANITY_CHECK(block->getTerminator());
    if (!isAllIdenticalLValues) {
      builder.restoreIP(
          llvm::IRBuilderBase::InsertPoint(block, std::prev(block->end())));
      value = invoke(type, value, srcLoc);
      block = getInsertBlock();
    }
    phiInst->addIncoming(value, block);
  }
  builder.restoreIP(ip);
  crumb = crumb0;
  return isAllIdenticalLValues ? LValue(type, phiInst) : RValue(type, phiInst);
}

Value Emitter::emit(AST::Node &node) {
  return emitTypeSwitch<AST::File, AST::Decl, AST::Expr, AST::Stmt>(node);
}

//--{ Emit: Decl
Value Emitter::emit(AST::Decl &decl) {
  return emitTypeSwitch< //
      AST::AnnotationDecl, AST::Enum, AST::Exec, AST::Function, AST::Import,
      AST::Namespace, AST::Struct, AST::Tag, AST::Typedef, AST::UnitTest,
      AST::UsingAlias, AST::UsingImport, AST::Variable>(decl);
}

Value Emitter::emit(AST::Exec &decl) {
  auto returnType{context.getVoidType()};
  auto llvmFunc{createFunction( //
      ".exec", /*isPure=*/false, returnType, ParameterList(), decl.srcLoc,
      [&] { emit(decl.stmt); })};
  llvmFunc->setLinkage(llvm::Function::ExternalLinkage);
  context.compiler.jitExecs.emplace_back(llvmFunc->getName().str());
  return Value();
}

Value Emitter::emit(AST::UnitTest &decl) {
  if (context.compiler.enableUnitTests) {
    auto returnType{context.getVoidType()};
    auto llvmFunc{createFunction( //
        ".unit_test", /*isPure=*/false, returnType, ParameterList(),
        decl.srcLoc, [&] { emit(decl.stmt); })};
    llvmFunc->setLinkage(llvm::Function::ExternalLinkage);
    auto &jitTest{context.compiler.jitUnitTests.emplace_back()};
    jitTest.moduleName = std::string(decl.srcLoc.getModuleName());
    jitTest.moduleFileName = std::string(decl.srcLoc.getModuleFileName());
    jitTest.lineNo = decl.srcLoc.lineNo;
    jitTest.testName = decl.name->value;
    jitTest.test.name = llvmFunc->getName().str();
    jitTest.test.func = nullptr; // Lookup later
  }
  return Value();
}

Value Emitter::emit(AST::UsingImport &decl) {
  auto importPath{decl.importPath.elementViews};
  for (auto &name : decl.names) {
    importPath.push_back(name.srcName);
    auto importPathPtr{static_cast<std::string_view *>(
        context.allocator.allocate(sizeof(std::string_view) * importPath.size(),
                                   alignof(std::string_view)))};
    std::uninitialized_copy(importPath.begin(), importPath.end(),
                            importPathPtr);
    declareImport(Span(importPathPtr, importPath.size()),
                  decl.importPath.isAbsolute(), decl);
    importPath.pop_back();
  }
  return Value();
}

Value Emitter::emit(AST::Variable &decl) {
  auto type{emit(decl.type).getComptimeMetaType(context, decl.srcLoc)};
  if (type->isFunction())
    decl.srcLoc.throwError("variable must not have function type ",
                           Quoted(type->displayName));
  const bool isConst{decl.type->has_qualifier("const")};
  const bool isStatic{decl.type->has_qualifier("static")};
  const bool isInline{decl.type->has_qualifier("inline")};
  if (!getLLVMFunction() && !isConst)
    decl.srcLoc.throwError(
        "variables declared at module scope must be 'const'");
  if (isStatic && !isConst)
    decl.srcLoc.throwError(
        "variables declared 'static' must be 'const' (at least for now)");
  if (isInline)
    decl.srcLoc.throwError("variables must not be declared 'inline'");
  for (auto &declarator : decl.declarators) {
    if (declarator.isDestructure() && type != context.getAutoType())
      declarator.srcLoc.throwError(
          "destructure declarator must have 'auto' type");
    auto crumb0{crumb};
    auto args{[&]() -> ArgumentList {
      if (declarator.exprInit) {
        return emit(*declarator.exprInit);
      } else if (declarator.argsInit) {
        return emit(*declarator.argsInit);
      } else {
        return ArgumentList();
      }
    }()};
    unwind(crumb0);
    auto value{invoke(type, args, declarator.srcLoc)};
    if (declarator.isDestructure()) {
      if (isStatic)
        declarator.srcLoc.throwError(
            "cannot destructure 'static' variables (yet)");
      SMDL_SANITY_CHECK(declarator.names.size() >= 1);
      if (auto structType{llvm::dyn_cast<StructType>(value.type)}) {
        if (structType->params.size() != declarator.names.size())
          declarator.srcLoc.throwError(
              "cannot destructure ", Quoted(structType->displayName),
              ", expected ", structType->params.size(), " names");
        if (!isConst) {
          auto valueAlloca{createAlloca(value.type)};
          createLifetimeStart(valueAlloca);
          builder.CreateStore(value, valueAlloca);
          value = LValue(value.type, valueAlloca);
        }
        for (size_t i = 0; i < structType->params.size(); i++) {
          declareCrumb(declarator.names[i].name, &declarator,
                       accessField(value, structType->params[i].name,
                                   declarator.srcLoc));
        }
      } else {
        declarator.srcLoc.throwError("unsupported destructure");
      }
    } else {
      // NOTE: This must be a reference for `declare_crumb` below
      SMDL_SANITY_CHECK(declarator.names.size() == 1);
      const auto &name{declarator.names[0].name};
      if (!value.isVoid()) {
        if (isStatic) {
          if (!value.isComptime())
            declarator.srcLoc.throwError(
                "variable ", Quoted(name.srcName),
                " declared 'static' requires compile-time initializer");
          auto llvmGlobal{new llvm::GlobalVariable(
              context.llvmModule, value.type->llvmType, /*isConstant=*/true,
              llvm::GlobalValue::PrivateLinkage,
              static_cast<llvm::Constant *>(value.llvmValue))};
          llvmGlobal->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
          llvmGlobal->setName(name.srcName);
          value = LValue(value.type, llvmGlobal);
        } else if (isConst) {
          value.llvmValue->setName(name.srcName);
        } else {
          auto valueAlloca{createAlloca(value.type, name.srcName)};
          createLifetimeStart(valueAlloca);
          builder.CreateStore(value, valueAlloca);
          value = LValue(value.type, valueAlloca);
        }
      }
      declareCrumb(name, &declarator, value);
      if (getLLVMFunction()) {
        crumbsToWarnAbout.push_back(crumb);
      }
    }
  }
  return Value();
}
//--}

//--{ Emit: Expr
Value Emitter::emit(AST::Expr &expr) {
  return emitTypeSwitch< //
      AST::AccessField, AST::AccessIndex, AST::Binary, AST::Call,
      AST::Identifier, AST::Intrinsic, AST::Let, AST::LiteralBool,
      AST::LiteralFloat, AST::LiteralInt, AST::LiteralString, AST::Parens,
      AST::ReturnFrom, AST::Select, AST::Type, AST::TypeCast, AST::Unary>(expr);
}

Value Emitter::emit(AST::AccessIndex &expr) {
  auto value{emit(expr.expr)};
  if (!value.isComptimeMetaType(context)) {
    for (auto &index : expr.indexes) {
      if (!index.expr)
        expr.srcLoc.throwError("expected non-empty '[]'");
      value = accessIndex(
          value, invoke(context.getIntType(), emit(index.expr), expr.srcLoc),
          expr.srcLoc);
    }
    return value;
  } else {
    auto type{value.getComptimeMetaType(context, expr.srcLoc)};
    for (auto itr{expr.indexes.rbegin()}; itr != expr.indexes.rend(); ++itr) {
      auto &index{*itr};
      if (!index.expr) {
        type = context.getInferredSizeArrayType(type); // Empty
      } else if (auto sizeName{
                     llvm::dyn_cast<AST::SizeName>(index.expr.get())}) {
        type = context.getInferredSizeArrayType(
            type, std::string(sizeName->name.srcName));
      } else {
        auto size{invoke(context.getIntType(), emit(index.expr), expr.srcLoc)};
        if (!size.isComptimeInt())
          expr.srcLoc.throwError(
              "expected array size expression to resolve to compile-time int");
        type = context.getArrayType(type, size.getComptimeInt());
      }
    }
    return context.getComptimeMetaType(type);
  }
}

Value Emitter::emit(AST::Binary &expr) {
  // Temporary let.
  if (expr.op == BINOP_LET) {
    auto ident{llvm::dyn_cast<AST::Identifier>(&*expr.exprLhs)};
    if (!ident) // || !ident->is_simple_name())
      expr.srcLoc.throwError(
          "expected lhs of operator ':=' to be an identifier");
    auto rv{toRValue(emit(expr.exprRhs))};
    declareCrumb(*ident, ident, rv);
    return rv;
  }
  // Short-circuit logic conditions.
  if (expr.op == BINOP_LOGIC_AND || expr.op == BINOP_LOGIC_OR) {
    auto boolType{context.getBoolType()};
    auto valueLhs{invoke(boolType, emit(expr.exprLhs), expr.srcLoc)};
    if (valueLhs.isComptimeInt()) {
      auto valueLhsNow{valueLhs.getComptimeInt()};
      if ((valueLhsNow != 0 && expr.op == BINOP_LOGIC_AND) ||
          (valueLhsNow == 0 && expr.op == BINOP_LOGIC_OR))
        return invoke(boolType, emit(expr.exprRhs), expr.srcLoc);
      return valueLhs;
    } else {
      auto blockLhs{getInsertBlock()};
      auto [blockRhs, blockEnd] = createBlocks<2>(
          expr.op == BINOP_LOGIC_AND ? "and" : "or", {".rhs", ".end"});
      builder.CreateCondBr(valueLhs,
                           expr.op == BINOP_LOGIC_AND ? blockRhs : blockEnd,
                           expr.op == BINOP_LOGIC_AND ? blockEnd : blockRhs);
      builder.SetInsertPoint(blockRhs);
      auto valueRhs{invoke(boolType, emit(expr.exprRhs), expr.srcLoc)};
      blockRhs = getInsertBlock();
      builder.CreateBr(blockEnd);
      builder.SetInsertPoint(blockEnd);
      auto phiInst{builder.CreatePHI(boolType->llvmType, 2)};
      phiInst->addIncoming(context.getComptimeBool(expr.op == BINOP_LOGIC_OR),
                           blockLhs);
      phiInst->addIncoming(valueRhs, blockRhs);
      return RValue(boolType, phiInst);
    }
  }
  // Short-circuit else.
  if (expr.op == BINOP_ELSE) {
    auto valueLhs{emit(expr.exprLhs)};
    auto valueLhsCond{invoke(context.getBoolType(), valueLhs, expr.srcLoc)};
    if (valueLhsCond.isComptimeInt()) {
      return valueLhsCond.getComptimeInt() ? valueLhs : emit(expr.exprRhs);
    } else {
      auto [blockLhs, blockRhs, blockEnd] =
          createBlocks<3>("else", {".lhs", ".rhs", ".end"});
      builder.CreateCondBr(valueLhsCond, blockLhs, blockRhs);
      builder.SetInsertPoint(blockLhs);
      auto valueLhsIP{builder.saveIP()};
      llvmMoveBlockToEnd(blockRhs);
      builder.SetInsertPoint(blockRhs);
      auto valueRhs{emit(expr.exprRhs)};
      auto valueRhsIP{builder.saveIP()};
      auto commonType{context.getCommonType({valueLhs.type, valueRhs.type},
                                            /*defaultToUnion=*/true,
                                            expr.srcLoc)};
      builder.restoreIP(valueLhsIP);
      valueLhs = invoke(commonType, valueLhs, expr.exprLhs->srcLoc);
      blockLhs = getInsertBlock();
      builder.CreateBr(blockEnd);
      builder.restoreIP(valueRhsIP);
      valueRhs = invoke(commonType, valueRhs, expr.exprRhs->srcLoc);
      blockRhs = getInsertBlock();
      builder.CreateBr(blockEnd);
      // Create PHI instruction.
      llvmMoveBlockToEnd(blockEnd);
      builder.SetInsertPoint(blockEnd);
      auto phiInst{builder.CreatePHI(commonType->llvmType, 2)};
      phiInst->addIncoming(valueLhs, blockLhs);
      phiInst->addIncoming(valueRhs, blockRhs);
      return RValue(commonType, phiInst);
    }
  }
  // Default.
  auto lhs{emit(expr.exprLhs)};
  auto rhs{emit(expr.exprRhs)};
  if (expr.op == BINOP_APPROX_CMP_EQ || //
      expr.op == BINOP_APPROX_CMP_NE) {
    // Approximate comparison operators
    // `lhs ~== [eps] rhs`
    // `lhs ~!= [eps] rhs`
    auto res{emitOp(BINOP_SUB, lhs, rhs, expr.srcLoc)};
    res = emitIntrinsic("abs", res, expr.srcLoc);
    res = emitOp(BINOP_CMP_LT, res, emit(expr.exprEps), expr.srcLoc);
    if (expr.op == BINOP_APPROX_CMP_NE)
      res = emitOp(UNOP_LOGIC_NOT, res, expr.srcLoc);
    return res;
  }
  return emitOp(expr.op, lhs, rhs, expr.srcLoc);
}

Value Emitter::emit(AST::Parens &expr) {
  if (expr.isComptime()) {
    auto value{emit(expr.expr)};
    if (!value.isComptime()) {
      expr.srcLoc.throwError("expected compile-time constant");
    }
    if (value.isComptimeMetaType(context)) {
      auto type{value.getComptimeMetaType(context, expr.srcLoc)};
      if (auto unionType{llvm::dyn_cast<UnionType>(type)}) {
        return context.getComptimeMetaType(
            context.getComptimeUnionType(unionType));
      }
    }
    return value;
  } else {
    return emit(expr.expr);
  }
}

Value Emitter::emit(AST::ReturnFrom &expr) {
  auto [blockBegin, blockEnd] =
      createBlocks<2>("return_from", {".begin", ".end"});
  SMDL_PRESERVE(returns);
  returns.clear();
  builder.CreateBr(blockBegin);
  builder.SetInsertPoint(blockBegin);
  handleScope(blockBegin, blockEnd, [&, blockEnd = blockEnd] {
    labelReturn = {crumb, blockEnd};
    labelBreak = {};
    labelContinue = {};
    emit(expr.stmt);
  });
  llvmMoveBlockToEnd(blockEnd);
  builder.SetInsertPoint(blockEnd);
  return createResult(context.getAutoType(), returns, expr.srcLoc);
}

Value Emitter::emit(AST::Select &expr) {
  auto cond{invoke(context.getBoolType(), emit(expr.exprCond), expr.srcLoc)};
  if (cond.isComptimeInt())
    return emit(cond.getComptimeInt() ? expr.exprThen : expr.exprElse);
  auto [blockThen, blockElse, blockEnd] =
      createBlocks<3>("select", {".then", ".else", ".end"});
  builder.CreateCondBr(cond, blockThen, blockElse);
  builder.SetInsertPoint(blockThen);
  auto valueThen{emit(expr.exprThen)};
  auto valueThenIP{builder.saveIP()};
  llvmMoveBlockToEnd(blockElse);
  builder.SetInsertPoint(blockElse);
  auto valueElse{emit(expr.exprElse)};
  auto valueElseIP{builder.saveIP()};
  auto commonType{context.getCommonType({valueThen.type, valueElse.type},
                                        /*defaultToUnion=*/true, expr.srcLoc)};
  builder.restoreIP(valueThenIP);
  valueThen = invoke(commonType, valueThen, expr.exprThen->srcLoc);
  blockThen = getInsertBlock();
  builder.CreateBr(blockEnd);
  builder.restoreIP(valueElseIP);
  valueElse = invoke(commonType, valueElse, expr.exprElse->srcLoc);
  blockElse = getInsertBlock();
  builder.CreateBr(blockEnd);
  // Create PHI instruction.
  llvmMoveBlockToEnd(blockEnd);
  builder.SetInsertPoint(blockEnd);
  auto phiInst{builder.CreatePHI(commonType->llvmType, 2)};
  phiInst->addIncoming(valueThen, blockThen);
  phiInst->addIncoming(valueElse, blockElse);
  return RValue(commonType, phiInst);
}
//--}

//--{ Emit: Stmt
Value Emitter::emit(AST::Stmt &stmt) {
  return emitTypeSwitch< //
      AST::Break, AST::Compound, AST::Continue, AST::DeclStmt, AST::Defer,
      AST::DoWhile, AST::ExprStmt, AST::For, AST::If, AST::Preserve,
      AST::Return, AST::Switch, AST::Unreachable, AST::Visit, AST::While>(stmt);
}

Value Emitter::emit(AST::Compound &stmt) {
  handleScope(nullptr, nullptr, [&] {
    for (auto &subStmt : stmt.stmts)
      if (emit(subStmt); hasTerminator())
        break;
  });
  return Value();
}

Value Emitter::emit(AST::DoWhile &stmt) {
  auto [blockLoop, blockCond, blockEnd] =
      createBlocks<3>("do_while", {".loop", ".cond", ".end"});
  handleScope(blockLoop, blockCond,
              [&, blockCond = blockCond, blockEnd = blockEnd] {
                labelBreak = {crumb, blockEnd};
                labelContinue = {crumb, blockCond};
                inDefer = false;
                emit(stmt.stmt);
              });
  builder.SetInsertPoint(blockCond);
  builder.CreateCondBr(
      invoke(context.getBoolType(), emit(stmt.expr), stmt.expr->srcLoc),
      blockLoop, blockEnd);
  handleBlockEnd(blockEnd);
  return Value();
}

Value Emitter::emit(AST::For &stmt) {
  auto crumb0{crumb};
  auto [blockCond, blockLoop, blockNext, blockEnd] =
      createBlocks<4>("for", {".cond", ".loop", ".next", ".end"});
  if (stmt.stmtInit)
    emit(stmt.stmtInit);
  builder.CreateBr(blockCond);
  llvmMoveBlockToEnd(blockCond);
  builder.SetInsertPoint(blockCond);
  builder.CreateCondBr(
      invoke(context.getBoolType(), emit(stmt.exprCond), stmt.exprCond->srcLoc),
      blockLoop, blockEnd);
  handleScope(blockLoop, blockNext,
              [&, blockNext = blockNext, blockEnd = blockEnd] {
                labelBreak = {crumb, blockEnd};
                labelContinue = {crumb, blockNext};
                inDefer = false;
                if (stmt.stmtLoop)
                  emit(stmt.stmtLoop);
              });
  builder.SetInsertPoint(blockNext);
  if (stmt.exprNext)
    emit(stmt.exprNext);
  builder.CreateBr(blockCond);
  handleBlockEnd(blockEnd);
  if (!hasTerminator())
    unwind(crumb0);
  crumb = crumb0;
  return Value();
}

Value Emitter::emit(AST::If &stmt) {
  auto cond{invoke(context.getBoolType(), emit(stmt.expr), stmt.srcLoc)};
  if (cond.isComptimeInt()) {
    handleScope(nullptr, nullptr, [&] {
      if (cond.getComptimeInt())
        emit(stmt.stmtThen);
      else if (stmt.stmtElse)
        emit(stmt.stmtElse);
    });
    return Value();
  }
  auto [blockThen, blockElse, blockEnd] =
      createBlocks<3>("if", {".then", ".else", ".end"});
  if (!stmt.stmtElse)
    blockElse->removeFromParent();
  builder.CreateCondBr(cond, blockThen, stmt.stmtElse ? blockElse : blockEnd);
  if (stmt.stmtThen)
    handleScope(blockThen, blockEnd, [&] { emit(stmt.stmtThen); });
  if (stmt.stmtElse)
    handleScope(blockElse, blockEnd, [&] { emit(stmt.stmtElse); });
  handleBlockEnd(blockEnd);
  return Value();
}

Value Emitter::emit(AST::Switch &stmt) {
  auto switchName{context.getUniqueName("switch", getLLVMFunction())};
  auto switchNameRef{llvm::StringRef(switchName)};
  auto blockEnd{createBlock(switchNameRef + ".end")};
  auto blockDefault{static_cast<llvm::BasicBlock *>(nullptr)};
  struct SwitchCase final {
    AST::Switch::Case *astCase{};
    llvm::ConstantInt *llvmConst{};
    llvm::BasicBlock *block{};
  };
  auto switchCases{llvm::SmallVector<SwitchCase>{}};
  for (auto &astCase : stmt.cases) {
    if (astCase.is_default()) {
      if (blockDefault)
        stmt.srcLoc.throwError("expected at most 1 'default' case in 'switch'");
      switchCases.push_back(SwitchCase{
          &astCase, nullptr, createBlock(switchNameRef + ".default")});
      blockDefault = switchCases.back().block;
    } else {
      auto value{emit(astCase.expr)};
      auto llvmConst{llvm::dyn_cast<llvm::ConstantInt>(value.llvmValue)};
      if (!llvmConst)
        astCase.expr->srcLoc.throwError(
            "expected 'case' expression to resolve to compile-time int");
      switchCases.push_back(SwitchCase{
          &astCase, llvmConst,
          createBlock(concat(switchName, ".case.", switchCases.size()))});
    }
  }
  if (!blockDefault)
    blockDefault = blockEnd;
  auto switchInst{builder.CreateSwitch(
      invoke(context.getIntType(), emit(stmt.expr), stmt.srcLoc),
      blockDefault)};
  handleScope(nullptr, blockEnd, [&] {
    auto i{size_t(0)};
    auto crumb0{crumb};
    labelBreak = {crumb, blockEnd};
    for (auto &[astCase, llvmConst, block] : switchCases) {
      if (llvmConst)
        switchInst->addCase(llvmConst, block);
      builder.SetInsertPoint(block);
      for (auto &subStmt : astCase->stmts)
        if (emit(subStmt); hasTerminator())
          break;
      if (hasTerminator())
        crumb = crumb0; // Reset after `return`, `break`, or `continue`
      if (++i < switchCases.size()) {
        if (!hasTerminator())
          builder.CreateBr(switchCases[i].block);
        llvmMoveBlockToEnd(switchCases[i].block);
      }
    }
  });
  handleBlockEnd(blockEnd);
  return Value();
}

Value Emitter::emit(AST::While &stmt) {
  auto [blockCond, blockLoop, blockEnd] =
      createBlocks<3>("while", {".cond", ".loop", ".end"});
  builder.CreateBr(blockCond);
  builder.SetInsertPoint(blockCond);
  builder.CreateCondBr(
      invoke(context.getBoolType(), emit(stmt.expr), stmt.expr->srcLoc),
      blockLoop, blockEnd);
  handleScope(blockLoop, blockCond,
              [&, blockCond = blockCond, blockEnd = blockEnd] {
                labelBreak = {crumb, blockEnd};
                labelContinue = {crumb, blockCond};
                inDefer = false;
                emit(stmt.stmt);
              });
  handleBlockEnd(blockEnd);
  return Value();
}
//--}

//--{ emit_op (AST::UnaryOp)
Value Emitter::emitOp(AST::UnaryOp op, Value value,
                      const SourceLocation &srcLoc) {
  if (value.isComptimeMetaType(context)) {
    auto type{value.getComptimeMetaType(context, srcLoc)};
    switch (op) {
    // Add pointer type, e.g., `&int`
    case UNOP_ADDR:
      return context.getComptimeMetaType(context.getPointerType(type));
    // Remove pointer type, e.g., `*(&int)`
    case UNOP_DEREF:
      if (type->isPointer())
        return context.getComptimeMetaType(type->getPointeeType());
      srcLoc.throwError("cannot dereference ", Quoted(type->displayName));
      break;
    // Union with `void`, e.g., `?int`
    case UNOP_MAYBE:
      if (type->isVoid())
        srcLoc.throwError("cannot optionalize 'void'");
      if (type->isAbstract())
        srcLoc.throwError("cannot optionalize abstract type ",
                          Quoted(type->displayName));
      return context.getComptimeMetaType(
          context.getUnionType({context.getVoidType(), type}));
    default:
      break;
    }
  } else {
    // If the operator is a postfix increment or decrement,
    // follow the C convention of returning the value before the
    // operation is applied.
    if ((op & UNOP_POSTFIX) == UNOP_POSTFIX) {
      auto result{toRValue(value)}; // Save
      emitOp(op & ~UNOP_POSTFIX, value, srcLoc);
      return result;
    }
    // Unary increment or decrement, e.g., `++value` or `--value`
    if (op == UNOP_INC || op == UNOP_DEC) {
      /// We can increment or decrement if the value is an arithmetic scalar,
      /// arithmetic vector, color, or pointer.
      if (value.type->isArithmeticScalar() ||
          value.type->isArithmeticVector() || value.type->isColor() ||
          value.type->isPointer())
        return emitOp(op == UNOP_INC ? BINOP_EQ_ADD : BINOP_EQ_SUB, value,
                      context.getComptimeInt(1), srcLoc);
      srcLoc.throwError("cannot increment or decrement ",
                        Quoted(value.type->displayName));
    }
    // Unary positive, e.g., `+value`
    if (op == UNOP_POS) {
      return toRValue(value);
    }
    // Unary negative, e.g., `-value`
    if (op == UNOP_NEG) {
      if (value.type->isVectorized() && value.type->isArithmeticIntegral()) {
        return RValue(value.type, builder.CreateNeg(toRValue(value)));
      } else if (value.type->isVectorized()) {
        return RValue(value.type, builder.CreateFNeg(toRValue(value)));
      } else if (value.type->isArithmeticMatrix()) {
        return emitOpColumnwise(value.type, [&](unsigned j) {
          return emitOp(op, accessIndex(value, j, srcLoc), srcLoc);
        });
      } else if (value.type->isComplex(context)) {
        return invoke("_complex_neg", value, srcLoc);
      }
    }
    // Unary not, e.g., `~value`
    if (op == UNOP_NOT) {
      if ((value.type->isArithmeticScalar() ||
           value.type->isArithmeticVector()) &&
          value.type->isArithmeticIntegral()) {
        return RValue(value.type, builder.CreateNot(toRValue(value)));
      }
    }
    // Unary logic not, e.g., `!value`
    if (op == UNOP_LOGIC_NOT) {
      if (value.isVoid())
        return context.getComptimeBool(true);
      if (value.type->isArithmetic() || value.type->isColor() ||
          value.type->isPointer() || value.type->isEnum())
        return emitOp(BINOP_CMP_EQ, value, Value::zero(value.type), srcLoc);
      if (value.type->isOptionalUnion())
        return emitOp(UNOP_LOGIC_NOT,
                      invoke(context.getBoolType(), value, srcLoc), srcLoc);
    }
    // Unary address, e.g., `&value`
    if (op == UNOP_ADDR) {
      if (!value.isLValue())
        srcLoc.throwError("cannot take address of rvalue");
      return RValue(context.getPointerType(value.type), value);
    }
    // Unary dereference, e.g., `*value`
    if (op == UNOP_DEREF) {
      if (value.type->isPointer())
        return LValue(value.type->getPointeeType(), toRValue(value));
      if (value.type->isOptionalUnion()) {
        if (value.isRValue()) {
          auto lv{toLValue(value)};
          auto rv{toRValue(emitOp(op, lv, srcLoc))};
          createLifetimeEnd(lv);
          return rv;
        }
        return LValue(
            context.getUnionType(llvm::ArrayRef(
                static_cast<UnionType *>(value.type)->caseTypes.data(),
                static_cast<UnionType *>(value.type)->caseTypes.size() - 1)),
            value.llvmValue);
      }
    }
  }
  srcLoc.throwError("unimplemented unary operator ", Quoted(to_string(op)),
                    " for type ", Quoted(value.type->displayName));
  return Value();
}
//--}

//--{ Helper: llvm_arith_op
[[nodiscard]] static std::optional<llvm::Instruction::BinaryOps>
llvm_arith_op(Scalar::Intent intent, AST::BinaryOp op) {
  if (intent == Scalar::Intent::Int) {
    switch (op) {
    case BINOP_ADD:
      return llvm::Instruction::Add;
    case BINOP_SUB:
      return llvm::Instruction::Sub;
    case BINOP_MUL:
      return llvm::Instruction::Mul;
    case BINOP_DIV:
      return llvm::Instruction::SDiv;
    case BINOP_REM:
      return llvm::Instruction::SRem;
    case BINOP_AND:
      return llvm::Instruction::And;
    case BINOP_OR:
      return llvm::Instruction::Or;
    case BINOP_XOR:
      return llvm::Instruction::Xor;
    case BINOP_SHL:
      return llvm::Instruction::Shl;
    case BINOP_ASHR:
      return llvm::Instruction::AShr;
    case BINOP_LSHR:
      return llvm::Instruction::LShr;
    default:
      break;
    }
  } else if (intent == Scalar::Intent::FP) {
    switch (op) {
    case BINOP_ADD:
      return llvm::Instruction::FAdd;
    case BINOP_SUB:
      return llvm::Instruction::FSub;
    case BINOP_MUL:
      return llvm::Instruction::FMul;
    case BINOP_DIV:
      return llvm::Instruction::FDiv;
    case BINOP_REM:
      return llvm::Instruction::FRem;
    default:
      break;
    }
  }
  return std::nullopt;
}
//--}

//--{ Helper: llvm_cmp_op
[[nodiscard]] static std::optional<llvm::CmpInst::Predicate>
llvm_cmp_op(Scalar::Intent intent, AST::BinaryOp op, bool isSigned = true) {
  if (intent == Scalar::Intent::Int) {
    switch (op) {
    case BINOP_CMP_EQ:
      return llvm::CmpInst::ICMP_EQ;
    case BINOP_CMP_NE:
      return llvm::CmpInst::ICMP_NE;
    case BINOP_CMP_LT:
      return !isSigned ? llvm::CmpInst::ICMP_ULT : llvm::CmpInst::ICMP_SLT;
    case BINOP_CMP_LE:
      return !isSigned ? llvm::CmpInst::ICMP_ULE : llvm::CmpInst::ICMP_SLE;
    case BINOP_CMP_GT:
      return !isSigned ? llvm::CmpInst::ICMP_UGT : llvm::CmpInst::ICMP_SGT;
    case BINOP_CMP_GE:
      return !isSigned ? llvm::CmpInst::ICMP_UGE : llvm::CmpInst::ICMP_SGE;
    default:
      break;
    }
  } else if (intent == Scalar::Intent::FP) {
    switch (op) {
    case BINOP_CMP_EQ:
      return llvm::CmpInst::FCMP_OEQ;
    case BINOP_CMP_NE:
      return llvm::CmpInst::FCMP_ONE;
    case BINOP_CMP_LT:
      return llvm::CmpInst::FCMP_OLT;
    case BINOP_CMP_LE:
      return llvm::CmpInst::FCMP_OLE;
    case BINOP_CMP_GT:
      return llvm::CmpInst::FCMP_OGT;
    case BINOP_CMP_GE:
      return llvm::CmpInst::FCMP_OGE;
    default:
      break;
    }
  }
  return std::nullopt;
}
//--}

//--{ emit_op (AST::BinaryOp)
Value Emitter::emitOp(AST::BinaryOp op, Value lhs, Value rhs,
                      const SourceLocation &srcLoc) {
  if (op == BINOP_COMMA)
    return rhs;
  if ((op & BINOP_EQ) == BINOP_EQ) {
    if (!lhs.isLValue())
      srcLoc.throwError("cannot apply ", Quoted(to_string(op)), " to rvalue");
    if (op != BINOP_EQ)
      rhs = emitOp(op & ~BINOP_EQ, toRValue(lhs), rhs, srcLoc);
    builder.CreateStore(invoke(lhs.type, rhs, srcLoc), lhs);
    return lhs;
  }
  lhs = toRValue(lhs);
  rhs = toRValue(rhs);
  // Numbers
  if (lhs.type->isArithmetic() && rhs.type->isArithmetic()) {
    auto lhsType{static_cast<ArithmeticType *>(lhs.type)};
    auto rhsType{static_cast<ArithmeticType *>(rhs.type)};
    // Scalar and vector operations
    if ((lhsType->extent.isScalar() && rhsType->extent.isScalar()) ||
        (lhsType->extent.isScalar() && rhsType->extent.isVector()) ||
        (lhsType->extent.isVector() && rhsType->extent.isScalar()) ||
        (lhsType->extent.isVector() && lhsType->extent == rhsType->extent)) {
      auto commonType{lhsType->getCommonType(context, rhsType)};
      lhs = invoke(commonType, lhs, srcLoc);
      rhs = invoke(commonType, rhs, srcLoc);
      if (auto llvmOp{llvm_arith_op(commonType->scalar.intent, op)})
        return RValue(commonType, builder.CreateBinOp(*llvmOp, lhs, rhs));
      if (auto llvmOp{llvm_cmp_op(commonType->scalar.intent, op)})
        return RValue(
            commonType->getWithDifferentScalar(context, Scalar::getBool()),
            builder.CreateCmp(*llvmOp, lhs, rhs));
    }
    // Matrix-Matrix (ADD, SUB)
    if ((op == BINOP_ADD || op == BINOP_SUB) && //
        lhsType->extent.isMatrix() &&           //
        lhsType->extent == rhsType->extent) {
      return emitOpColumnwise(lhsType->getCommonType(context, rhsType),
                              [&](unsigned j) {
                                auto lhsColumn{accessIndex(lhs, j, srcLoc)};
                                auto rhsColumn{accessIndex(rhs, j, srcLoc)};
                                return emitOp(op, lhsColumn, rhsColumn, srcLoc);
                              });
    }
    // Matrix-Scalar (MUL, DIV, REM)
    if ((op == BINOP_MUL || op == BINOP_DIV || op == BINOP_REM) && //
        lhsType->extent.isMatrix() &&                              //
        rhsType->extent.isScalar()) {
      auto commonType{lhsType->getCommonType(context, rhsType)};
      auto scalarAsColumn{
          invoke(commonType->getColumnType(context), rhs, srcLoc)};
      return emitOpColumnwise(commonType, [&](unsigned j) {
        return emitOp(op, accessIndex(lhs, j, srcLoc), scalarAsColumn, srcLoc);
      });
    }
    // Matrix-Matrix or Matrix-Vector (MUL)
    if ((op == BINOP_MUL) && lhsType->extent.isMatrix() &&
        (rhsType->extent.isMatrix() || rhsType->extent.isVector()) &&
        (lhsType->extent.numCols == rhsType->extent.numRows)) {
      auto scalar{lhsType->scalar.getCommon(rhsType->scalar)};
      lhs =
          invoke(lhsType->getWithDifferentScalar(context, scalar), lhs, srcLoc);
      rhs =
          invoke(rhsType->getWithDifferentScalar(context, scalar), rhs, srcLoc);
      auto extentM{lhsType->extent.numRows};
      auto extentN{lhsType->extent.numCols};
      auto extentP{rhsType->extent.numCols};
      auto result{Value::zero(
          context.getArithmeticType(scalar, Extent(extentP, extentM)))};
      auto lhsColumns{llvm::SmallVector<Value>(size_t(extentN))};
      for (unsigned k = 0; k < extentN; k++)
        lhsColumns[k] = accessIndex(lhs, k, srcLoc);
      for (unsigned j = 0; j < extentP; j++) {
        auto rhsColumn{extentP == 1 ? rhs : accessIndex(rhs, j, srcLoc)};
        auto resColumnTerm{[&](unsigned k) {
          return emitOp(BINOP_MUL, lhsColumns[k],
                        accessIndex(rhsColumn, k, srcLoc), srcLoc);
        }};
        auto resColumn{resColumnTerm(0)};
        for (unsigned k = 1; k < extentN; k++)
          resColumn = emitOp(BINOP_ADD, resColumn, resColumnTerm(k), srcLoc);
        result = extentP == 1 //
                     ? resColumn
                     : insert(result, resColumn, j, srcLoc);
      }
      return result;
    }
    // Vector-Matrix (MUL)
    if ((op == BINOP_MUL) && lhsType->extent.isVector() &&
        rhsType->extent.isMatrix() &&
        lhsType->extent.numRows == rhsType->extent.numRows) {
      auto scalar{lhsType->scalar.getCommon(rhsType->scalar)};
      lhs =
          invoke(lhsType->getWithDifferentScalar(context, scalar), lhs, srcLoc);
      rhs =
          invoke(rhsType->getWithDifferentScalar(context, scalar), rhs, srcLoc);
      /* auto extentN{rhsType->extent.numRows}; */
      auto extentP{rhsType->extent.numCols};
      auto result{
          Value::zero(context.getArithmeticType(scalar, Extent(extentP)))};
      // Row-vector times matrix, equivalent to `#transpose(matrix) * vector`
      for (unsigned j = 0; j < extentP; j++) {
        auto resColumn{accessIndex(rhs, j, srcLoc)};
        resColumn = emitOp(BINOP_MUL, lhs, resColumn, srcLoc);
        resColumn = emitIntrinsic("sum", resColumn, srcLoc);
        result = insert(result, resColumn, j, srcLoc);
      }
      return result;
    }
  }
  // Enums
  if (lhs.type->isEnum() && rhs.type->isEnum()) {
    if (auto llvmOp{llvm_arith_op(Scalar::Intent::Int, op)};
        llvmOp && lhs.type == rhs.type)
      return RValue(lhs.type, builder.CreateBinOp(*llvmOp, lhs, rhs));
    if (auto llvmOp{llvm_cmp_op(Scalar::Intent::Int, op)})
      return RValue(context.getBoolType(),
                    builder.CreateCmp(*llvmOp, lhs, rhs));
  }
  // Colors
  if ((lhs.type->isColor() &&
       (rhs.type->isColor() || rhs.type->isArithmeticScalar())) ||
      (lhs.type->isArithmeticScalar() && rhs.type->isColor())) {
    if (auto llvmOp{llvm_arith_op(Scalar::Intent::FP, op)}) {
      lhs = invoke(context.getColorType(), lhs, srcLoc);
      rhs = invoke(context.getColorType(), rhs, srcLoc);
      return RValue(context.getColorType(),
                    builder.CreateBinOp(*llvmOp, lhs, rhs));
    }
    if (auto llvmOp{llvm_cmp_op(Scalar::Intent::FP, op)}) {
      lhs = invoke(context.getColorType(), lhs, srcLoc);
      rhs = invoke(context.getColorType(), rhs, srcLoc);
      return RValue(context.getColorType()
                        ->getArithmeticVectorType(context)
                        ->getWithDifferentScalar(context, Scalar::getBool()),
                    builder.CreateCmp(*llvmOp, lhs, rhs));
    }
  }
  // Complex numbers
  if (lhs.type->isComplex(context) || rhs.type->isComplex(context)) {
    // Promote both to `complex`
    lhs = invoke(context.getComplexType(), lhs, srcLoc);
    rhs = invoke(context.getComplexType(), rhs, srcLoc);
    const char *funcName{};
    if (op == BINOP_ADD) {
      funcName = "_complex_add";
    } else if (op == BINOP_SUB) {
      funcName = "_complex_sub";
    } else if (op == BINOP_MUL) {
      funcName = "_complex_mul";
    } else if (op == BINOP_DIV) {
      funcName = "_complex_div";
    }
    if (funcName) {
      return invoke(funcName, {lhs, rhs}, srcLoc);
    }
  }
  // Strings
  if (lhs.type->isString() && rhs.type->isString() &&
      (op == BINOP_CMP_EQ || op == BINOP_CMP_NE)) {
    auto result{llvm::emitStrNCmp(
        lhs, rhs,
        llvmEmitCast(builder, accessField(lhs, "size", srcLoc),
                     builder.getInt64Ty()),
        builder, context.llvmLayout, &context.llvmTargetLibraryInfo)};
    return RValue(context.getBoolType(), op == BINOP_CMP_EQ
                                             ? builder.CreateIsNull(result)
                                             : builder.CreateIsNotNull(result));
  }
  // Pointers
  if ((op == BINOP_ADD || op == BINOP_SUB) && lhs.type->isPointer() &&
      rhs.type->isArithmeticIntegral()) {
    if (op == BINOP_SUB)
      rhs = emitOp(UNOP_NEG, rhs, srcLoc);
    return RValue(lhs.type,
                  builder.CreateGEP(lhs.type->getPointeeType()->llvmType, lhs,
                                    {rhs.llvmValue}));
  }
  if (lhs.type->isPointer() && rhs.type->isPointer()) {
    if (op == BINOP_SUB) {
      if (lhs.type != rhs.type)
        srcLoc.throwError(
            "pointer subtraction requires both pointers to be the same type");
      return RValue(context.getIntType(),
                    builder.CreateIntCast(
                        builder.CreatePtrDiff(
                            lhs.type->getPointeeType()->llvmType, lhs, rhs),
                        context.getIntType()->llvmType, /*isSigned=*/true));
    }
    if (auto llvmOp{llvm_cmp_op(Scalar::Intent::Int, op, /*isSigned=*/false)}) {
      auto intPtrTy{builder.getIntNTy(sizeof(void *) * 8)};
      return RValue(context.getBoolType(),
                    builder.CreateCmp(*llvmOp,                               //
                                      builder.CreatePtrToInt(lhs, intPtrTy), //
                                      builder.CreatePtrToInt(rhs, intPtrTy)));
    }
  }
  // Types
  if (lhs.type == context.getMetaTypeType() && //
      rhs.type == context.getMetaTypeType()) {
    if (auto llvmOp{llvm_cmp_op(Scalar::Intent::Int, op)})
      return RValue(context.getBoolType(),
                    builder.CreateCmp(*llvmOp, lhs, rhs));
    if (lhs.isComptime() && rhs.isComptime()) {
      auto lhsTy{lhs.getComptimeMetaType(context, srcLoc)};
      auto rhsTy{rhs.getComptimeMetaType(context, srcLoc)};
      if (op == BINOP_OR)
        return context.getComptimeMetaType(
            context.getUnionType({lhsTy, rhsTy}));
      if (op == BINOP_SUBSET)
        return context.getComptimeBool(
            context.isPerfectlyConvertible(lhsTy, rhsTy));
    }
  }
  // Type-checking
  if (lhs.type != context.getMetaTypeType() && //
      rhs.type == context.getMetaTypeType() && op == BINOP_SUBSET) {
    if (rhs.isComptime()) {
      auto lhsTy{lhs.type};
      auto rhsTy{rhs.getComptimeMetaType(context, srcLoc)};
      return context.getComptimeBool(
          context.isPerfectlyConvertible(lhsTy, rhsTy));
    }
  }
  srcLoc.throwError("unimplemented binary operator ", Quoted(to_string(op)),
                    " for argument types ", Quoted(lhs.type->displayName),
                    " and ", Quoted(rhs.type->displayName));
  return Value();
}
//--}

extern "C" {

SMDL_EXPORT void *smdl_bump_allocate(void *state, int size, int align) {
  SMDL_SANITY_CHECK(state != nullptr && align > 0);
  if (size <= 0)
    return nullptr;
  auto allocator{
      static_cast<BumpPtrAllocator *>(static_cast<State *>(state)->allocator)};
  SMDL_SANITY_CHECK(allocator != nullptr, "allocator cannot be null!");
  return allocator->allocate(size, align);
}

SMDL_EXPORT void *smdl_ofile_open(const char *fname) {
  std::error_code ec{};
  auto result{new llvm::raw_fd_ostream(std::string_view(fname), ec)};
  if (ec) {
    SMDL_LOG_WARN("cannot open ", Quoted(fname), ": ", Quoted(ec.message()));
    delete result;
    return nullptr;
  }
  return result;
}

SMDL_EXPORT void smdl_ofile_close(void *ofile) {
  if (ofile)
    delete static_cast<llvm::raw_fd_ostream *>(ofile);
}

// TODO This is a specific solution to a specific problem of calculating
// tabulated albedos conveniently and efficiently. There might be a more
// general way of addressing this in the future.
SMDL_EXPORT void smdl_tabulate_albedo(const char *name, int num_cos_theta,
                                      int num_roughness, const void *func) {
  SMDL_SANITY_CHECK(num_cos_theta > 1);
  SMDL_SANITY_CHECK(num_roughness > 1);
  SMDL_SANITY_CHECK(func);
  auto numCalculationsTodo{num_cos_theta * num_roughness};
  auto numCalculationsDone{std::atomic_int(0)};
  auto directionalAlbedo{std::vector<float>(size_t(numCalculationsTodo), 0.0f)};
  llvm::parallelFor(0, num_cos_theta, [&](size_t i) {
    float cos_theta{i / float(num_cos_theta - 1)};
    for (int j = 0; j < num_roughness; j++) {
      float roughness{j / float(num_roughness - 1)};
      directionalAlbedo[int(i) * num_roughness + j] =
          reinterpret_cast<float (*)(float, float)>(const_cast<void *>(func))(
              cos_theta, roughness);
      llvm::errs() << llvm::format("\rTabulating '%s': %2.1f%%", name,
                                   float(double(++numCalculationsDone) * 100.0 /
                                         double(numCalculationsTodo)));
    }
  });
  llvm::errs() << '\n';
  {
    std::error_code ec{};
    auto outputFileName{std::string(name) + ".inl"};
    auto outputFile{llvm::raw_fd_stream{outputFileName, ec}};
    outputFile << llvm::format(
        "static const std::array<float, %d * %d> %s_directional_albedo = {\n",
        num_cos_theta, num_roughness, name);
    for (int i = 0; i < num_cos_theta; i++) {
      for (int j = 0; j < num_roughness; j++) {
        outputFile << llvm::format("%1.7ef, ",
                                   directionalAlbedo[i * num_roughness + j]);
      }
      outputFile << '\n';
    }
    outputFile << "};\n";
    outputFile << llvm::format(
        "static const std::array<float, %d> %s_average_albedo = {\n",
        num_roughness, name);
    for (int j = 0; j < num_roughness; j++) {
      double numer = 0.0;
      double denom = 0.0;
      for (int i = 0; i < num_cos_theta; i++) {
        float cos_theta{i / float(num_cos_theta - 1)};
        numer += 2 * cos_theta * directionalAlbedo[i * num_roughness + j];
        denom += 1;
      }
      outputFile << llvm::format("%1.7ef, ", numer / denom);
    }
    outputFile << "};\n";
    outputFile << llvm::format(
        "static const AlbedoLUT %s = {%d, %d, %s_directional_albedo.data(), "
        "%s_average_albedo.data()};\n",
        name, num_cos_theta, num_roughness, name, name);
  }
  {
    std::error_code ec{};
    auto outputFileName{std::string(name) + ".txt"};
    auto outputFile{llvm::raw_fd_stream{outputFileName, ec}};
    for (int i = 0; i < num_cos_theta; i++) {
      for (int j = 0; j < num_roughness; j++) {
        outputFile << llvm::format("%1.7e ",
                                   directionalAlbedo[i * num_roughness + j]);
      }
      outputFile << '\n';
    }
  }
}

} // extern "C"

//--{ emit_intrinsic
Value Emitter::emitIntrinsic(std::string_view name, const ArgumentList &args,
                             const SourceLocation &srcLoc) {
  if (args.isAnyNamed())
    srcLoc.throwError("intrinsics expect only unnamed arguments");
  auto expectOne{[&]() {
    if (args.size() != 1)
      srcLoc.throwError("intrinsic ", Quoted(name), " expects 1 argument");
    return args[0].value;
  }};
  auto expectOneVectorized{[&]() {
    if (args.size() != 1 || !args[0].value.type->isVectorized())
      srcLoc.throwError("intrinsic ", Quoted(name),
                        " expects 1 vectorized argument");
    return args[0].value;
  }};
  auto expectOneIntOrIntVector{[&]() {
    if (args.size() != 1 ||                    //
        !args[0].value.type->isVectorized() || //
        !args[0].value.type->isArithmeticIntegral())
      srcLoc.throwError("intrinsic ", Quoted(name),
                        " expects 1 int or int vector argument");
    return args[0].value;
  }};
  auto expectOneType{[&]() {
    auto value{toRValue(expectOne())};
    return value.isComptimeMetaType(context)
               ? value.getComptimeMetaType(context, srcLoc)
               : value.type;
  }};
  auto scalarTypeOf{[&](Type *type) -> ArithmeticType * {
    if (type->isColor())
      return static_cast<ColorType *>(type)->getArithmeticScalarType(context);
    if (type->isArithmetic())
      return static_cast<ArithmeticType *>(type)->getScalarType(context);
    SMDL_SANITY_CHECK(false);
    return nullptr;
  }};
  switch (name[0]) {
  default:
    break;
  case 'a': {
    if (name == "alignof") {
      return context.getComptimeInt(int(context.getAlignOf(expectOneType())));
    }
    if (name == "abs") {
      // Intercept instances of `complex` and forward appropriately.
      if (args.size() == 1 && args[0].value.type->isComplex(context)) {
        return invoke("_complex_abs", args, srcLoc);
      }
      auto value{toRValue(expectOneVectorized())};
      return RValue(
          value.type,
          value.type->isArithmeticIntegral()
              ? builder.CreateBinaryIntrinsic(llvm::Intrinsic::abs, value,
                                              context.getComptimeBool(false))
              : builder.CreateUnaryIntrinsic(llvm::Intrinsic::fabs, value));
    }
    if (name == "any" || name == "all") {
      auto value{expectOne()};
      if (!value.type->isArithmeticScalar() &&
          !value.type->isArithmeticVector())
        srcLoc.throwError("intrinsic ", Quoted(name),
                          " expects 1 scalar or vector argument");
      value = invoke(static_cast<ArithmeticType *>(value.type)
                         ->getWithDifferentScalar(context, Scalar::getBool()),
                     value, srcLoc);
      if (value.type->isArithmeticScalar())
        return value;
      return RValue(context.getBoolType(),
                    builder.CreateUnaryIntrinsic(
                        name == "any" ? llvm::Intrinsic::vector_reduce_or
                                      : llvm::Intrinsic::vector_reduce_and,
                        value));
    }
    if (name == "assert") {
      if (!((args.size() == 1 && args[0].value.type == context.getBoolType()) ||
            (args.size() == 2 && args[0].value.type == context.getBoolType() &&
             args[1].value.type == context.getStringType())))
        srcLoc.throwError("intrinsic 'assert' expects 1 bool argument and 1 "
                          "optional string argument");
      auto [blockPanic, blockOk] = createBlocks<2>("assert", {".panic", ".ok"});
      builder.CreateCondBr(toRValue(args[0].value), blockOk, blockPanic);
      builder.SetInsertPoint(blockPanic);
      handleScope(nullptr, nullptr, [&] {
        if (args.size() == 1) {
          std::string message{"assertion failed"};
          if (!args[0].getSource().empty()) {
            message += ": ";
            message += args[0].getSource();
          }
          emitPanic(context.getComptimeString(message), srcLoc);
        } else {
          emitPanic(toRValue(args[1].value), srcLoc);
        }
      });
      builder.CreateBr(blockOk);
      builder.SetInsertPoint(blockOk);
      return Value();
    }
    if (name == "atan2") {
      if (!(args.size() == 2 &&                   //
            args[0].value.type->isVectorized() && //
            args[1].value.type->isVectorized()))
        srcLoc.throwError("intrinsic 'atan2' expects 2 vectorized arguments");
      auto commonType{context.getCommonType(
          {args[0].value.type, args[1].value.type, context.getFloatType()},
          /*defaultToUnion=*/false, srcLoc)};
      SMDL_SANITY_CHECK(commonType->isArithmeticFloatingPoint());
      auto value0{invoke(commonType, args[0].value, srcLoc)};
      auto value1{invoke(commonType, args[1].value, srcLoc)};
      return RValue(commonType, builder.CreateBinaryIntrinsic(
                                    llvm::Intrinsic::atan2, value0, value1));
    }
    if (name == "albedo_lut") {
      if (!(args.size() == 1 && args[0].value.isComptimeString()))
        srcLoc.throwError(
            "intrinsic 'albedo_lut' expects 1 compile-time string argument");
      auto lutName{args[0].value.getComptimeString()};
      auto lutType{context.getKeyword("_albedo_lut")
                       .getComptimeMetaType(context, srcLoc)};
      auto lut{context.getBuiltinAlbedo(lutName)};
      if (!lut)
        srcLoc.throwError(
            "intrinsic 'albedo_lut' passed invalid name ", Quoted(lutName),
            " that does not identify any known look-up table at compile time");
      auto args{ArgumentList{}};
      args.push_back(Argument{"num_cos_theta",
                              context.getComptimeInt(lut->num_cos_theta)});
      args.push_back(Argument{"num_roughness",
                              context.getComptimeInt(lut->num_roughness)});
      args.push_back(Argument{
          "directional_albedo",
          context.getComptimePtr(context.getPointerType(context.getFloatType()),
                                 lut->directional_albedo)});
      args.push_back(Argument{
          "average_albedo",
          context.getComptimePtr(context.getPointerType(context.getFloatType()),
                                 lut->average_albedo)});
      return invoke(lutType, args, srcLoc);
    }
    break;
  }
  case 'b': {
    if (name == "bitreverse") {
      auto value{toRValue(expectOneIntOrIntVector())};
      return RValue(value.type, builder.CreateUnaryIntrinsic(
                                    llvm::Intrinsic::bitreverse, value));
    }
    if (name == "breakpoint") {
      if (!args.empty())
        srcLoc.throwError("intrinsic 'breakpoint' expects no arguments");
      builder.CreateIntrinsic(context.getVoidType()->llvmType,
                              llvm::Intrinsic::debugtrap, {});
      return RValue(context.getVoidType(), nullptr);
    }
    if (name == "bump_allocate") {
      auto value{toRValue(expectOne())};
      auto callee{
          context.getBuiltinCallee("smdl_bump_allocate", &smdl_bump_allocate)};
      if (auto func{llvm::dyn_cast<llvm::Function>(callee.getCallee())})
        func->setReturnDoesNotAlias();
      auto callInst{builder.CreateCall(
          callee,
          {state.llvmValue,
           context.getComptimeInt(int(context.getSizeOf(value.type))).llvmValue,
           context.getComptimeInt(int(context.getAlignOf(value.type)))
               .llvmValue})};
      builder.CreateStore(value, callInst);
      return RValue(context.getPointerType(value.type), callInst);
    }
    break;
  }
  case 'c': {
    if (name == "ctlz") {
      auto value{toRValue(expectOneIntOrIntVector())};
      return RValue(value.type, builder.CreateBinaryIntrinsic(
                                    llvm::Intrinsic::ctlz, value,
                                    context.getComptimeBool(false)));
    }
    if (name == "cttz") {
      auto value{toRValue(expectOneIntOrIntVector())};
      return RValue(value.type, builder.CreateBinaryIntrinsic(
                                    llvm::Intrinsic::cttz, value,
                                    context.getComptimeBool(false)));
    }
    if (name == "ctpop") {
      auto value{toRValue(expectOneIntOrIntVector())};
      return RValue(value.type, builder.CreateUnaryIntrinsic(
                                    llvm::Intrinsic::ctpop, value));
    }
    break;
  }
  case 'i': {
    if (name == "isfpclass") {
      if (!(args.size() == 2 &&                                 //
            (args[0].value.type->isArithmeticFloatingPoint() || //
             args[0].value.type->isColor()) &&                  //
            args[1].value.isComptimeInt()))
        srcLoc.throwError(
            "intrinsic 'isfpclass' expects 1 vectorized floating point "
            "argument and 1 compile-time int argument");
      auto value{toRValue(args[0].value)};
      return RValue(
          static_cast<ArithmeticType *>(value.type)
              ->getWithDifferentScalar(context, Scalar::getBool()),
          builder.createIsFPClass(value, args[1].value.getComptimeInt()));
    }
    if (startsWith(name, "is_")) {
      auto type{expectOneType()};
      auto result{std::optional<bool>{}};
      if (name == "is_array") {
        result = type->isArray();
      } else if (name == "is_arithmetic") {
        result = type->isArithmetic();
      } else if (name == "is_arithmetic_integral") {
        result = type->isArithmeticIntegral();
      } else if (name == "is_arithmetic_floating_point") {
        result = type->isArithmeticFloatingPoint();
      } else if (name == "is_arithmetic_scalar") {
        result = type->isArithmeticScalar();
      } else if (name == "is_arithmetic_vector") {
        result = type->isArithmeticVector();
      } else if (name == "is_arithmetic_matrix") {
        result = type->isArithmeticMatrix();
      } else if (name == "is_enum") {
        result = type->isEnum();
      } else if (name == "is_optional_union") {
        result = type->isOptionalUnion();
      } else if (name == "is_pointer") {
        result = type->isPointer();
      } else if (name == "is_struct") {
        result = type->isStruct();
      } else if (name == "is_tag") {
        result = type->isTag();
      } else if (name == "is_union") {
        result = type->isUnion();
      } else if (name == "is_void") {
        result = type->isVoid();
      } else if (name == "is_default") {
        if (auto structType{llvm::dyn_cast<StructType>(type)}) {
          if (!structType->instanceOf) {
            // The struct is considered 'default' if it is
            // the default type for its first tag.
            result = structType->tags.size() >= 1 &&
                     structType->tags[0]->defaultType == structType;
          } else {
            // The struct is considered 'default' if it is
            // the default instantiation of an abstract struct.
            result = structType->isDefaultInstance;
          }
        } else {
          result = false;
        }
      }
      if (result) {
        return context.getComptimeBool(*result);
      }
    }
    break;
  }
  case 'l': {
    if (startsWith(name, "load_")) {
      auto expectOneComptimeString{[&]() {
        if (!(args.size() == 1 && args[0].value.isComptimeString()))
          srcLoc.throwError("intrinsic ", Quoted(name),
                            " expects 1 compile-time string argument");
        return std::string(args[0].value.getComptimeString());
      }};
      auto expectOneComptimeStringAndOneInt{[&]() {
        if (!(args.size() == 2 &&                 //
              args[0].value.isComptimeString() && //
              args[1].value.type->isArithmeticScalarInt()))
          srcLoc.throwError(
              "intrinsic ", Quoted(name),
              " expects 1 compile-time string argument and 1 int argument");
        return std::make_pair(std::string(args[0].value.getComptimeString()),
                              toRValue(args[1].value));
      }};
      if (name == "load_texture_2d") {
        auto texture2DType{context.getTexture2DType()};
        auto [fileName, valueGammaAsInt] = expectOneComptimeStringAndOneInt();
        auto resolvedImagePaths{context.locateImages(fileName)};
        if (resolvedImagePaths.empty()) {
          srcLoc.logWarn(concat("no image(s) found for ", Quoted(fileName)));
          return invoke(texture2DType, {}, srcLoc);
        }
        auto tileCountU{uint32_t(1)};
        auto tileCountV{uint32_t(1)};
        auto images{llvm::SmallVector<const Image *>{}};
        for (auto &[tileIndexU, tileIndexV, filePath] : resolvedImagePaths) {
          tileCountU = std::max(tileCountU, tileIndexU + 1);
          tileCountV = std::max(tileCountV, tileIndexV + 1);
          images.push_back(&context.compiler.loadImage(filePath, srcLoc));
          if (images.back()->getFormat() != images.front()->getFormat() ||
              images.back()->getNumChannels() !=
                  images.front()->getNumChannels()) {
            srcLoc.logWarn(
                concat("inconsistent image formats for ", Quoted(fileName)));
            return invoke(texture2DType, {}, srcLoc);
          }
        }
        auto texelPtrType{context.getPointerType(context.getArithmeticType(
            images[0]->getFormat() == Image::UINT8     ? Scalar::getInt(8)
            : images[0]->getFormat() == Image::UINT16  ? Scalar::getInt(16)
            : images[0]->getFormat() == Image::FLOAT16 ? Scalar::getHalf()
                                                       : Scalar::getFloat(),
            Extent(images[0]->getNumChannels())))};
        auto valueTileExtents{Value::zero(context.getArrayType(
            context.getIntType(2), tileCountU * tileCountV))};
        auto valueTileBuffers{Value::zero(
            context.getArrayType(texelPtrType, tileCountU * tileCountV))};
        for (unsigned int i = 0; i < resolvedImagePaths.size(); i++) {
          auto &imagePath{resolvedImagePaths[i]};
          auto &image{images[i]};
          auto insertPos{imagePath.tileIndexV * tileCountU +
                         imagePath.tileIndexU};
          valueTileExtents = insert(
              valueTileExtents,
              context.getComptimeVector(int2(int(image->getNumTexelsX()),
                                             int(image->getNumTexelsY()))),
              insertPos, srcLoc);
          valueTileBuffers =
              insert(valueTileBuffers,
                     context.getComptimePtr(texelPtrType, image->getTexels()),
                     insertPos, srcLoc);
        }
        return invoke(
            texture2DType,
            {Argument{"tile_count", context.getComptimeVector(int2(
                                        int(tileCountU), int(tileCountV)))},
             Argument{"tile_extents", valueTileExtents},
             Argument{"tile_buffers", valueTileBuffers},
             Argument{"gamma", valueGammaAsInt}},
            srcLoc);
      }
      if (name == "load_texture_3d") {
        auto texture3DType{context.getTexture3DType()};
        auto [fileName, valueGammaAsInt] = expectOneComptimeStringAndOneInt();
        // TODO
        return invoke(texture3DType, {}, srcLoc);
      }
      if (name == "load_texture_cube") {
        auto textureCubeType{context.getTextureCubeType()};
        auto [fileName, valueGammaAsInt] = expectOneComptimeStringAndOneInt();
        // TODO
        return invoke(textureCubeType, {}, srcLoc);
      }
      if (name == "load_texture_ptex") {
        auto texturePtexType{context.getTexturePtexType()};
        auto [fileName, valueGammaAsInt] = expectOneComptimeStringAndOneInt();
        auto resolvedFileName{context.locate(fileName)};
        if (!resolvedFileName) {
          srcLoc.logWarn(
              concat("cannot load ", Quoted(fileName), ": file not found"));
          return invoke(texturePtexType, {}, srcLoc);
        }
        auto &ptexture{
            context.compiler.loadPtexture(*resolvedFileName, srcLoc)};
        auto valuePtr{
            context.getComptimePtr(context.getVoidPointerType(),
                                   ptexture.texture ? &ptexture : nullptr)};
        return invoke(
            texturePtexType,
            {Argument{"ptr", valuePtr}, Argument{"gamma", valueGammaAsInt}},
            srcLoc);
      }
      if (name == "load_bsdf_measurement") {
        auto bsdfMeasurementType{context.getBSDFMeasurementType()};
        auto fileName{expectOneComptimeString()};
        auto resolvedFileName{context.locate(fileName)};
        if (!resolvedFileName) {
          srcLoc.logWarn(
              concat("cannot load ", Quoted(fileName), ": file not found"));
          return invoke(bsdfMeasurementType, {}, srcLoc);
        }
        auto &bsdfMeasurement{
            context.compiler.loadBSDFMeasurement(*resolvedFileName, srcLoc)};
        auto bufferPtrType{context.getPointerType(context.getFloatType(
            bsdfMeasurement.type == BSDFMeasurement::TYPE_FLOAT ? 1 : 3))};
        return invoke(
            bsdfMeasurementType,
            {Argument{"mode", context.getComptimeInt(
                                  bsdfMeasurement.kind ==
                                          BSDFMeasurement::KIND_REFLECTION
                                      ? /* scatter_reflect  */ 1
                                      : /* scatter_transmit */ 2)},
             Argument{"num_theta",
                      context.getComptimeInt(int(bsdfMeasurement.numTheta))},
             Argument{"num_phi",
                      context.getComptimeInt(int(bsdfMeasurement.numPhi))},
             Argument{"buffer", context.getComptimePtr(
                                    bufferPtrType, bsdfMeasurement.buffer)}},
            srcLoc);
      }
      if (name == "load_light_profile") {
        auto lightProfileType{context.getLightProfileType()};
        auto fileName{expectOneComptimeString()};
        auto resolvedFileName{context.locate(fileName)};
        if (!resolvedFileName) {
          srcLoc.logWarn(
              concat("cannot load ", Quoted(fileName), ": file not found"));
          return invoke(lightProfileType, {}, srcLoc);
        }
        auto &lightProfile{
            context.compiler.loadLightProfile(*resolvedFileName, srcLoc)};
        return invoke(
            lightProfileType,
            {Argument{"ptr",
                      context.getComptimePtr(
                          context.getVoidPointerType(),
                          lightProfile.isValid() ? &lightProfile : nullptr)},
             Argument{"max_intensity",
                      context.getComptimeFloat(lightProfile.maxIntensity())},
             Argument{"power", context.getComptimeFloat(lightProfile.power())}},
            srcLoc);
      }
      if (name == "load_spectral_curve") {
        auto spectralCurveType{context.getSpectralCurveType()};
        if (args.size() == 1) {
          if (!args[0].value.isComptimeString()) {
            srcLoc.throwError("intrinsic ", Quoted(name),
                              " expects 1 compile-time string argument");
          }
        } else if (args.size() == 2) {
          if (!(args[0].value.isComptimeString() &&
                (args[1].value.isComptimeString() ||
                 args[1].value.isComptimeInt()))) {
            srcLoc.throwError("intrinsic ", Quoted(name),
                              " expects 1 compile-time string argument and 1 "
                              "compile-time string or int argument");
          }
        } else {
          srcLoc.throwError("intrinsic ", Quoted(name),
                            " expects 1 or 2 arguments");
        }
        auto fileName{std::string(args[0].value.getComptimeString())};
        auto resolvedFileName{context.locate(fileName)};
        if (!resolvedFileName) {
          srcLoc.logWarn(
              concat("cannot load ", Quoted(fileName), ": file not found"));
          return invoke(spectralCurveType, {}, srcLoc);
        }
        auto spectrumView{SpectrumView{}};
        if (args.size() == 1) {
          spectrumView =
              context.compiler.loadSpectrum(*resolvedFileName, srcLoc);
        } else if (args[1].value.isComptimeString()) {
          spectrumView = context.compiler.loadSpectrum(
              *resolvedFileName, std::string(args[1].value.getComptimeString()),
              srcLoc);
        } else if (args[1].value.isComptimeInt()) {
          spectrumView = context.compiler.loadSpectrum(
              *resolvedFileName, int(args[1].value.getComptimeInt()), srcLoc);
        }
        if (spectrumView.curveValues.empty()) {
          return invoke(spectralCurveType, {}, srcLoc);
        }
        auto floatPtrType{context.getPointerType(context.getFloatType())};
        return invoke(
            spectralCurveType,
            {Argument{"count",
                      context.getComptimeInt(spectrumView.wavelengths.size())},
             Argument{"wavelengths",
                      context.getComptimePtr(floatPtrType,
                                             spectrumView.wavelengths.data())},
             Argument{"amplitudes",
                      context.getComptimePtr(floatPtrType,
                                             spectrumView.curveValues.data())}},
            srcLoc);
      }
    }
    break;
  }
  case 'm': {
    if (name == "memcpy") {
      if (!(args.size() == 3 &&                           //
            args[0].value.type->isPointer() &&            //
            args[1].value.type->isPointer() &&            //
            args[2].value.type->isArithmeticIntegral() && //
            args[2].value.type->isArithmeticScalar()))
        srcLoc.throwError("intrinsic 'memcpy' expects 2 pointer arguments "
                          "and 1 int argument");
      auto dst{toRValue(args[0].value)};
      auto src{toRValue(args[1].value)};
      builder.CreateMemCpy(dst, std::nullopt, src, std::nullopt,
                           llvmEmitCast(builder, toRValue(args[2].value),
                                        llvm::Type::getInt64Ty(context)));
      return Value();
    }
    if (name == "max" || name == "min") {
      if (args.size() != 2 || !args.isAllTrue([](auto &arg) {
            return arg.value.type->isVectorized();
          }))
        srcLoc.throwError("intrinsic ", Quoted(name),
                          " expects 2 vectorized arguments");
      auto value0{args[0].value};
      auto value1{args[1].value};
      auto type{context.getCommonType({value0.type, value1.type},
                                      /*defaultToUnion=*/false, srcLoc)};
      value0 = invoke(type, value0, srcLoc);
      value1 = invoke(type, value1, srcLoc);
      auto intrID =
          name == "max"
              ? (type->isArithmeticBoolean()    ? llvm::Intrinsic::umax
                 : type->isArithmeticIntegral() ? llvm::Intrinsic::smax
                                                : llvm::Intrinsic::maxnum)
              : (type->isArithmeticBoolean()    ? llvm::Intrinsic::umin
                 : type->isArithmeticIntegral() ? llvm::Intrinsic::smin
                                                : llvm::Intrinsic::minnum);
      return RValue(type,
                    builder.CreateBinaryIntrinsic(intrID, value0, value1));
    }
    if (name == "max_value" || name == "min_value") {
      auto value{toRValue(expectOneVectorized())};
      if (value.type->isArithmeticScalar())
        return value;
      auto intrID = name == "max_value"
                        ? (value.type->isArithmeticBoolean()
                               ? llvm::Intrinsic::vector_reduce_umax
                           : value.type->isArithmeticIntegral()
                               ? llvm::Intrinsic::vector_reduce_smax
                               : llvm::Intrinsic::vector_reduce_fmax)
                        : (value.type->isArithmeticBoolean()
                               ? llvm::Intrinsic::vector_reduce_umin
                           : value.type->isArithmeticIntegral()
                               ? llvm::Intrinsic::vector_reduce_smin
                               : llvm::Intrinsic::vector_reduce_fmin);
      return RValue(scalarTypeOf(value.type),
                    builder.CreateUnaryIntrinsic(intrID, value));
    }
    break;
  }
  case 'n': {
    if (name == "num") {
      auto type{expectOneType()};
      auto size{[&]() -> int {
        if (type->isArithmeticVector()) {
          return static_cast<ArithmeticType *>(type)->extent.numRows;
        } else if (type->isArithmeticMatrix()) {
          return static_cast<ArithmeticType *>(type)->extent.numCols;
        } else if (type->isColor()) {
          return static_cast<ColorType *>(type)->wavelengthBaseMax;
        } else if (type->isArray()) {
          return static_cast<ArrayType *>(type)->size;
        } else {
          return 1;
        }
      }()};
      return context.getComptimeInt(size);
    }
    if (name == "num_rows" || name == "num_cols") {
      auto type{expectOneType()};
      if (!type->isArithmeticMatrix())
        srcLoc.throwError("intrinsic ", Quoted(name),
                          " expects 1 matrix argument");
      return context.getComptimeInt(
          name == "num_rows"
              ? int(static_cast<ArithmeticType *>(type)->extent.numRows)
              : int(static_cast<ArithmeticType *>(type)->extent.numCols));
    }
    break;
  }
  case 'o': {
    if (name == "ofile_open") {
      auto value{toRValue(expectOne())};
      return RValue(context.getVoidPointerType(),
                    builder.CreateCall(context.getBuiltinCallee(
                                           "smdl_ofile_open", &smdl_ofile_open),
                                       {value.llvmValue}));
    }
    if (name == "ofile_close") {
      auto value{toRValue(expectOne())};
      builder.CreateCall(
          context.getBuiltinCallee("smdl_ofile_close", &smdl_ofile_close),
          {value.llvmValue});
      return Value();
    }
    if (name == "ofile_print" || name == "ofile_println") {
      if (args.size() < 2)
        srcLoc.throwError("intrinsic 'ofile_print' expects 1 file pointer "
                          "argument and 1 or more printable arguments");
      auto os{toRValue(args[0].value)};
      for (size_t i = 1; i < args.size(); i++)
        emitPrint(os, args[i].value, srcLoc);
      if (name == "ofile_println")
        emitPrint(os, context.getComptimeString("\n"), srcLoc);
      return {};
    }
    break;
  }
  case 'p': {
    if (name == "panic") {
      if (args.size() != 1 || args[0].value.type != context.getStringType())
        srcLoc.throwError("intrinsic 'panic' expects 1 string argument");
      emitPanic(args[0].value, srcLoc);
      return Value();
    }
    if (name == "pow") {
      if (args.size() != 2 ||                    //
          !args[0].value.type->isVectorized() || //
          !args[1].value.type->isVectorized())
        srcLoc.throwError("intrinsic 'pow' expects 2 vectorized arguments");
      auto value0{toRValue(args[0].value)};
      auto value1{toRValue(args[1].value)};
      if (value1.isComptimeInt()) {
        auto power{value1.getComptimeInt()};
        if (power == 1)
          return value0;
        if (power == 2)
          return emitOp(BINOP_MUL, value0, value0, srcLoc);
      }
      auto resultType{context.getCommonType(
          {value0.type, value1.type, context.getFloatType()},
          /*defaultToUnion=*/false, srcLoc)};
      value0 = invoke(resultType, value0, srcLoc);
      return RValue(resultType, value1.type->isArithmeticScalarInt()
                                    ? llvmEmitPowi(builder, value0, value1)
                                    : builder.CreateBinaryIntrinsic(
                                          llvm::Intrinsic::pow, value0,
                                          invoke(resultType, value1, srcLoc)));
    }
    if (name == "print" || name == "println") {
      auto os{
          context.getComptimePtr(context.getVoidPointerType(), &llvm::errs())};
      for (auto &arg : args)
        emitPrint(os, arg.value, srcLoc);
      if (name == "println")
        emitPrint(os, context.getComptimeString("\n"), srcLoc);
      return Value();
    }
    if (name == "prod") {
      auto value{toRValue(expectOneVectorized())};
      if (value.type->isArithmeticScalar())
        return value;
      auto scalarType{scalarTypeOf(value.type)};
      return RValue(
          scalarType,
          scalarType->isArithmeticIntegral()
              ? builder.CreateMulReduce(value)
              : builder.CreateFMulReduce(
                    invoke(scalarType, context.getComptimeFloat(1), srcLoc),
                    value));
    }
    break;
  }
  case 'r': {
    if (name == "rotl" || name == "rotr") {
      if (!(args.size() == 2 && //
            args[0].value.type->isArithmeticIntegral() &&
            args[1].value.type->isArithmeticIntegral())) {
        srcLoc.throwError("intrinsic ", Quoted(name),
                          " expects 2 integer or vectorized integer arguments");
      }
      auto intType{
          context.getCommonType({args[0].value.type, args[1].value.type},
                                /*defaultToUnion=*/false, srcLoc)};
      auto value0{invoke(intType, args[0].value, srcLoc)};
      auto value1{invoke(intType, args[1].value, srcLoc)};
      auto intrID{name == "rotl" ? llvm::Intrinsic::fshl
                                 : llvm::Intrinsic::fshr};
      return RValue(intType,
                    builder.CreateIntrinsic(intType->llvmType, intrID,
                                            {value0.llvmValue, value0.llvmValue,
                                             value1.llvmValue}));
    }
    break;
  }
  case 's': {
    if (name == "sizeof") {
      return context.getComptimeInt(int(context.getSizeOf(expectOneType())));
    }
    if (name == "sum") {
      auto value{toRValue(expectOneVectorized())};
      if (value.type->isArithmeticScalar())
        return value;
      auto scalarType{scalarTypeOf(value.type)};
      return RValue(scalarType, scalarType->isArithmeticIntegral()
                                    ? builder.CreateAddReduce(value)
                                    : builder.CreateFAddReduce(
                                          Value::zero(scalarType), value));
    }
    if (name == "sign") {
      auto value{toRValue(expectOneVectorized())};
      if (value.type->isArithmeticIntegral()) {
        return RValue(
            value.type,
            builder.CreateSelect(
                emitOp(BINOP_CMP_LT, value, context.getComptimeInt(0), srcLoc),
                invoke(value.type, context.getComptimeInt(-1), srcLoc),
                invoke(value.type, context.getComptimeInt(+1), srcLoc)));
      } else {
        return RValue(
            value.type,
            builder.CreateBinaryIntrinsic(
                llvm::Intrinsic::copysign,
                invoke(value.type, context.getComptimeFloat(1), srcLoc),
                value));
      }
    }
    if (name == "select") {
      if (args.size() != 3 || !args[0].value.type->isArithmeticBoolean() ||
          !args.isAllTrue(
              [](auto arg) { return arg.value.type->isVectorized(); }))
        srcLoc.throwError("intrinsic 'select' expects 1 vectorized boolean "
                          "argument and 2 vectorized selection arguments");
      auto valueCond{toRValue(args[0].value)};
      auto valueThen{args[1].value};
      auto valueElse{args[2].value};
      auto type{context.getCommonType(
          {valueThen.type, valueElse.type,
           valueCond.type->isArithmeticVector() ? valueCond.type : nullptr},
          /*defaultToUnion=*/false, srcLoc)};
      valueThen = invoke(type, valueThen, srcLoc);
      valueElse = invoke(type, valueElse, srcLoc);
      return RValue(type,
                    builder.CreateSelect(valueCond, valueThen, valueElse));
    }
    break;
  }
  case 't': {
    // TODO This is a specific solution to a specific problem of calculating
    // tabulated albedos conveniently and efficiently. There might be a more
    // general way of addressing this in the future.
    if (name == "tabulate_albedo") {
      if (!(args.size() == 4 && args[0].value.type == context.getStringType() &&
            args[1].value.type->isArithmeticScalarInt() &&
            args[2].value.type->isArithmeticScalarInt() &&
            args[3].value.isComptimeMetaType(context)))
        srcLoc.throwError("intrinsic 'tabulate_albedo' expects 1 compile-time "
                          "string argument, 2 compile-time integer arguments, "
                          "and 1 function argument");
      auto funcType{llvm::dyn_cast<FunctionType>(
          args[3].value.getComptimeMetaType(context, srcLoc))};
      if (!(funcType &&               //
            funcType->isPure() &&     //
            !funcType->isMacro() &&   //
            !funcType->isVariant() && //
            funcType->hasNoOverloads() &&
            funcType->returnType == context.getFloatType() &&
            funcType->params.size() == 2 &&
            funcType->params[0].type == context.getFloatType() &&
            funcType->params[1].type == context.getFloatType())) {
        srcLoc.throwError("intrinsic 'tabulate_albedo' function argument must "
                          "have signature '@(pure) float(float, float)'");
      }
      auto &funcInst{funcType->instantiate(
          *this, {context.getFloatType(), context.getFloatType()})};
      auto callee{context.getBuiltinCallee("smdl_tabulate_albedo",
                                           &smdl_tabulate_albedo)};
      auto callInst{
          builder.CreateCall(callee, {toRValue(args[0].value).llvmValue, //
                                      toRValue(args[1].value).llvmValue, //
                                      toRValue(args[2].value).llvmValue, //
                                      funcInst.llvmFunc})};
      return RValue(context.getVoidType(), callInst);
    }
    if (name == "typeof") {
      return context.getComptimeMetaType(expectOne().type);
    }
    if (name == "typename") {
      return context.getComptimeString(expectOneType()->displayName);
    }
    if (name == "type_int") {
      auto value{toRValue(expectOne())};
      if (!value.isComptimeInt())
        srcLoc.throwError("intrinsic 'type_int' expects 1 compile-time int");
      return context.getComptimeMetaType(context.getArithmeticType(
          Scalar::getInt(value.getComptimeInt()), Extent(1)));
    }
    if (name == "type_float") {
      auto value{toRValue(expectOne())};
      if (!value.isComptimeInt())
        srcLoc.throwError("intrinsic 'type_float' expects 1 compile-time int");
      return context.getComptimeMetaType(context.getArithmeticType(
          Scalar::getFP(value.getComptimeInt()), Extent(1)));
    }
    if (name == "type_vector") {
      auto reportError{[&] {
        srcLoc.throwError("intrinsic 'type_vector' expects 1 compile-time "
                          "scalar type and 1 compile-time positive int");
      }};
      if (!(args.size() == 2 &&                          //
            args[0].value.isComptimeMetaType(context) && //
            args[1].value.isComptimeInt()))
        reportError();
      auto type{args[0].value.getComptimeMetaType(context, srcLoc)};
      auto size{args[1].value.getComptimeInt()};
      if (!type->isArithmeticScalar() || size < 1)
        reportError();
      return context.getComptimeMetaType(context.getArithmeticType(
          static_cast<ArithmeticType *>(type)->scalar, Extent(size)));
    }
    if (name == "type_matrix") {
      auto reportError{[&] {
        srcLoc.throwError("intrinsic 'type_matrix' expects 1 compile-time "
                          "scalar type and 2 compile-time positive ints");
      }};
      if (!(args.size() == 3 &&                          //
            args[0].value.isComptimeMetaType(context) && //
            args[1].value.isComptimeInt() &&             //
            args[2].value.isComptimeInt()))
        reportError();
      auto type{args[0].value.getComptimeMetaType(context, srcLoc)};
      auto numCols{args[1].value.getComptimeInt()};
      auto numRows{args[2].value.getComptimeInt()};
      if (!type->isArithmeticScalar() || numCols < 1 || numRows < 1)
        reportError();
      return context.getComptimeMetaType(
          context.getArithmeticType(static_cast<ArithmeticType *>(type)->scalar,
                                    Extent(numCols, numRows)));
    }
    if (name == "transpose") {
      if (!(args.size() == 1 && args[0].value.type->isArithmeticMatrix()))
        srcLoc.throwError("intrinsic 'transpose' expects 1 matrix argument");
      auto value{args[0].value};
      auto valueCols{llvm::SmallVector<Value>{}};
      for (unsigned j = 0;
           j < static_cast<ArithmeticType *>(value.type)->extent.numCols; j++)
        valueCols.push_back(toRValue(accessIndex(value, j, srcLoc)));
      auto resultType{
          static_cast<ArithmeticType *>(value.type)->getTransposeType(context)};
      auto result{Value::zero(resultType)};
      for (unsigned j = 0; j < resultType->extent.numCols; j++) {
        auto resultCol{Value::zero(resultType->getColumnType(context))};
        for (unsigned i = 0; i < resultType->extent.numRows; i++)
          resultCol = insert(resultCol, accessIndex(valueCols[i], j, srcLoc), i,
                             srcLoc);
        result = insert(result, resultCol, j, srcLoc);
      }
      return result;
    }
    break;
  }
  case 'u': {
    if (name == "unsigned_to_fp") {
      if (!(args.size() == 2 && args[0].value.type->isArithmeticIntegral() &&
            args[1].value.isComptimeMetaType(context))) {
        srcLoc.throwError(
            "intrinsic 'unsigned_to_fp' expects 1 int argument and "
            "1 type argument");
      }
      auto floatType{args[1].value.getComptimeMetaType(context, srcLoc)};
      if (!floatType->isArithmeticFloatingPoint())
        srcLoc.throwError(
            "intrinsic 'unsigned_to_fp' expects 1 int argument and "
            "1 floating point type argument");
      return RValue(floatType, builder.CreateUIToFP(toRValue(args[0].value),
                                                    floatType->llvmType));
    }
    if (name == "unpack_float4") {
      auto value{toRValue(expectOne())};
      if (!value.type->isArithmeticScalar() &&
          !value.type->isArithmeticVector())
        srcLoc.throwError(
            "intrinsic 'unpack_float4' expects 1 scalar or vector argument");
      auto texelType{static_cast<ArithmeticType *>(value.type)};
      value.type =
          texelType->getWithDifferentScalar(context, Scalar::getFloat());
      value.llvmValue =
          texelType->isArithmeticIntegral()
              ? builder.CreateUIToFP(value.llvmValue, value.type->llvmType)
              : builder.CreateFPCast(value.llvmValue, value.type->llvmType);
      if (texelType->isArithmeticIntegral())
        value =
            emitOp(BINOP_MUL, value,
                   context.getComptimeFloat(
                       1.0f / float((1ULL << texelType->scalar.numBits) - 1)),
                   srcLoc);
      if (value.type == context.getFloatType(Extent(4))) // Early out?
        return value;
      auto result{Value::zero(context.getFloatType(Extent(4)))};
      if (value.type->isArithmeticScalar()) {
        result.llvmValue = builder.CreateVectorSplat(4, value.llvmValue);
      } else {
        for (uint64_t i = 0; i < texelType->extent.numRows; i++)
          result.llvmValue = builder.CreateInsertElement(
              result.llvmValue,
              builder.CreateExtractElement(value.llvmValue, i), i);
      }
      result.llvmValue = builder.CreateInsertElement(
          result.llvmValue, context.getComptimeFloat(1).llvmValue, uint64_t(3));
      return result;
    }
    break;
  }
  }
  // Unary floating-point math intrinsics
  if (args.size() == 1 && name.size() <= 5) {
    // Intercept instances of `complex` and forward appropriately.
    {
      auto value{args[0].value};
      auto valueIsComplex{value.type->isComplex(context)};
      if (name == "conj") {
        // NOTE: Conjugate of real number is no-op
        return valueIsComplex ? invoke("_complex_conj", value, srcLoc)
                              : toRValue(value);
      } else if (name == "real") {
        // NOTE: Real part of real number is identity
        return valueIsComplex ? accessField(value, "a", srcLoc) : value;
      } else if (name == "imag") {
        // NOTE: Imag part of real number is zero
        return valueIsComplex ? accessField(value, "b", srcLoc)
                              : invoke(value.type, {}, srcLoc);
      } else if (name == "norm") {
        // NOTE: Norm of real number is square
        return valueIsComplex ? invoke("_complex_norm", value, srcLoc)
                              : emitOp(BINOP_MUL, value, value, srcLoc);
      } else if (valueIsComplex) {
        if (name == "exp") {
          return invoke("_complex_exp", value, srcLoc);
        } else if (name == "log") {
          return invoke("_complex_log", value, srcLoc);
        } else if (name == "sqrt") {
          return invoke("_complex_sqrt", value, srcLoc);
        }
      }
    }
    static const std::pair<std::string_view, llvm::Intrinsic::ID> intrs[] = {
        {"floor", llvm::Intrinsic::floor}, {"ceil", llvm::Intrinsic::ceil},
        {"trunc", llvm::Intrinsic::trunc}, {"round", llvm::Intrinsic::round},
        {"sqrt", llvm::Intrinsic::sqrt},   {"sin", llvm::Intrinsic::sin},
        {"cos", llvm::Intrinsic::cos},     {"tan", llvm::Intrinsic::tan},
        {"asin", llvm::Intrinsic::asin},   {"acos", llvm::Intrinsic::acos},
        {"atan", llvm::Intrinsic::atan},   {"sinh", llvm::Intrinsic::sinh},
        {"cosh", llvm::Intrinsic::cosh},   {"tanh", llvm::Intrinsic::tanh},
        {"exp", llvm::Intrinsic::exp},     {"exp2", llvm::Intrinsic::exp2},
        {"exp10", llvm::Intrinsic::exp10}, {"log", llvm::Intrinsic::log},
        {"log2", llvm::Intrinsic::log2},   {"log10", llvm::Intrinsic::log10}};
    for (auto [intrName, intrID] : intrs) {
      if (name == intrName) {
        auto value{toRValue(expectOneVectorized())};
        if (value.type->isArithmeticIntegral())
          value =
              invoke(static_cast<ArithmeticType *>(value.type)
                         ->getWithDifferentScalar(context, Scalar::getFloat()),
                     value, srcLoc);
        return RValue(value.type, builder.CreateUnaryIntrinsic(intrID, value));
      }
    }
  }
  srcLoc.throwError("unimplemented intrinsic ", Quoted(name));
  return Value();
}
//--}

Value Emitter::emitCall(Value callee, const ArgumentList &args,
                        const SourceLocation &srcLoc) {
  if (args.isAnyVisited()) {
    auto visitedIndex{args.indexOfFirstVisited()};
    return emitVisit(args[visitedIndex].value, srcLoc, [&](Value value) {
      auto visitedArgs{args};
      visitedArgs[visitedIndex].value = value;
      return emitCall(callee, visitedArgs, srcLoc);
    });
  } else {
    if (callee.isComptimeMetaType(context)) {
      return invoke(callee.getComptimeMetaType(context, srcLoc), args, srcLoc);
    }
    if (callee.isComptimeMetaIntrinsic(context)) {
      return emitIntrinsic(
          callee.getComptimeMetaIntrinsic(context, srcLoc)->srcName.substr(1),
          args, srcLoc);
    }
  }
  srcLoc.throwError("unimplemented or invalid call");
  return Value();
}

Value Emitter::emitVisit(Value value, const SourceLocation &srcLoc,
                         const std::function<Value(Value)> &visitor) {
  if (!value.type->isUnionOrPointerToUnion()) {
    Value result{};
    handleScope(nullptr, nullptr, [&]() { result = visitor(value); });
    return result;
  } else {
    auto results{llvm::SmallVector<Result>{}};
    auto unionType{
        static_cast<UnionType *>(value.type->getFirstNonPointerType())};
    auto ptr{value.type->isUnion() ? toRValue(accessField(value, "#ptr", {}))
                                   : Value()};
    auto visitName{context.getUniqueName("visit", getLLVMFunction())};
    auto visitNameRef{llvm::StringRef(visitName)};
    auto blockUnreachable{createBlock(visitNameRef + ".unreachable")};
    auto blockEnd{createBlock(visitNameRef + ".end")};
    auto switchInst{builder.CreateSwitch(
        toRValue(accessField(value, "#idx", {})), blockUnreachable)};
    for (size_t i = 0; i < unionType->caseTypes.size(); i++) {
      auto blockCase{createBlock(concat(visitName, ".case.", i))};
      switchInst->addCase(builder.getInt32(i), blockCase);
      handleScope(blockCase, blockEnd, [&] {
        auto caseType{unionType->caseTypes[i]};
        auto caseValue{Value()};
        if (value.type->isUnion()) {
          caseValue = LValue(caseType, ptr);
          caseValue = value.isRValue() ? toRValue(caseValue) : caseValue;
        } else {
          caseValue = value;
          caseValue.type = context.getPointerType(
              caseType, value.type->getFirstNonPointerTypeDepth());
        }
        caseValue = visitor(caseValue);
        results.push_back({caseValue, getInsertBlock(), {}});
      });
    }
    llvmMoveBlockToEnd(blockUnreachable);
    builder.SetInsertPoint(blockUnreachable);
    builder.CreateUnreachable();
    handleBlockEnd(blockEnd);
    return createResult(context.getAutoType(), results, srcLoc);
  }
}

extern "C" {

SMDL_EXPORT void smdl_panic(const char *message) {
  throw Error(std::string(message));
}

} // extern "C"

void Emitter::emitPanic(Value message, const SourceLocation &srcLoc) {
  SMDL_SANITY_CHECK(message);
  SMDL_SANITY_CHECK(message.type == context.getStringType());
  message = toRValue(message);
  auto callInst{
      builder.CreateCall(context.getBuiltinCallee("smdl_panic", &smdl_panic),
                         {message.llvmValue})};
  callInst->setIsNoInline();
  callInst->setDoesNotReturn();
}

extern "C" {

SMDL_EXPORT void smdl_print_string(void *ptr, const char *value) {
  *static_cast<llvm::raw_ostream *>(ptr) << std::string_view(value);
}

SMDL_EXPORT void smdl_print_quoted_string(void *ptr, const char *value) {
  auto &os{*static_cast<llvm::raw_ostream *>(ptr)};
  os << '"';
  for (char ch : std::string_view(value)) {
    switch (ch) {
    case '\\':
      os << "\\";
      break;
    case '\a':
      os << "\\a";
      break;
    case '\b':
      os << "\\b";
      break;
    case '\f':
      os << "\\f";
      break;
    case '\n':
      os << "\\n";
      break;
    case '\r':
      os << "\\r";
      break;
    case '\t':
      os << "\\t";
      break;
    case '\v':
      os << "\\v";
      break;
    case '"':
      os << "\\\"";
      break;
    default: {
      if (std::isgraph(static_cast<uint8_t>(ch))) {
        os << ch;
      } else {
        os << "\\x";
        os << "0123456789abcdef"[static_cast<uint8_t>(ch) / 16];
        os << "0123456789abcdef"[static_cast<uint8_t>(ch) % 16];
      }
      break;
    }
    }
  }
  os << '"';
}

SMDL_EXPORT void smdl_print_bool(void *ptr, int value) {
  *static_cast<llvm::raw_ostream *>(ptr) << (value != 0 ? "true" : "false");
}

SMDL_EXPORT void smdl_print_int(void *ptr, int64_t value) {
  *static_cast<llvm::raw_ostream *>(ptr) << value;
}

SMDL_EXPORT void smdl_print_float(void *ptr, float value) {
  *static_cast<llvm::raw_ostream *>(ptr) << value;
}

SMDL_EXPORT void smdl_print_double(void *ptr, double value) {
  *static_cast<llvm::raw_ostream *>(ptr) << value;
}

SMDL_EXPORT void smdl_print_pointer(void *ptr, const void *value) {
  *static_cast<llvm::raw_ostream *>(ptr) << value;
}

} // extern "C"

void Emitter::emitPrint(Value os, Value value, const SourceLocation &srcLoc,
                        bool quoteStrings) {
  SMDL_SANITY_CHECK(os.type->isPointer());
  os = toRValue(os);
  if (value.isComptimeMetaType(context)) {
    emitPrint(os, value.getComptimeMetaType(context, srcLoc)->displayName,
              srcLoc);
  } else if (value.isVoid()) {
    emitPrint(os, "none", srcLoc);
  } else if (value.type->isPointer()) {
    builder.CreateCall(
        context.getBuiltinCallee("smdl_print_pointer", &smdl_print_pointer),
        {os.llvmValue, toRValue(value).llvmValue});
  } else if (value.type->isString()) {
    auto call{quoteStrings
                  ? context.getBuiltinCallee("smdl_print_quoted_string",
                                             &smdl_print_quoted_string)
                  : context.getBuiltinCallee("smdl_print_string",
                                             &smdl_print_string)};
    builder.CreateCall(call, {os.llvmValue, toRValue(value).llvmValue});
  } else if (value.type->isEnum()) {
    emitPrint(os, invoke(context.getStringType(), value, srcLoc), srcLoc);
  } else if (auto arithType{llvm::dyn_cast<ArithmeticType>(value.type)}) {
    if (arithType->extent.isScalar()) {
      auto type{static_cast<Type *>(nullptr)};
      auto call{llvm::FunctionCallee()};
      if (arithType->scalar.isBoolean()) {
        type = context.getIntType();
        call = context.getBuiltinCallee("smdl_print_bool", &smdl_print_bool);
      } else if (arithType->scalar.isIntegral()) {
        type = context.getArithmeticType(Scalar::getInt(64), Extent(1));
        call = context.getBuiltinCallee("smdl_print_int", &smdl_print_int);
      } else {
        type = arithType->scalar.numBits < 64
                   ? context.getArithmeticType(Scalar::getFP(32), Extent(1))
                   : context.getArithmeticType(Scalar::getFP(64), Extent(1));
        call = arithType->scalar.numBits < 64
                   ? context.getBuiltinCallee("smdl_print_float",
                                              &smdl_print_float)
                   : context.getBuiltinCallee("smdl_print_double",
                                              &smdl_print_double);
      }
      builder.CreateCall(call,
                         {os.llvmValue, invoke(type, value, srcLoc).llvmValue});
    } else if (arithType->extent.isVector()) {
      emitPrint(os, "<", srcLoc);
      for (uint32_t i = 0; i < arithType->extent.numRows; i++) {
        emitPrint(os, accessIndex(value, i, srcLoc), srcLoc);
        if (i + 1 < arithType->extent.numRows) {
          emitPrint(os, ", ", srcLoc);
        }
      }
      emitPrint(os, ">", srcLoc);
    } else if (arithType->extent.isMatrix()) {
      emitPrint(os, "[", srcLoc);
      for (uint32_t i = 0; i < arithType->extent.numCols; i++) {
        emitPrint(os, accessIndex(value, i, srcLoc), srcLoc);
        if (i + 1 < arithType->extent.numCols) {
          emitPrint(os, ", ", srcLoc);
        }
      }
      emitPrint(os, "]", srcLoc);
    }
  } else if (auto arrayType{llvm::dyn_cast<ArrayType>(value.type)}) {
    emitPrint(os, "[", srcLoc);
    for (uint32_t i = 0; i < arrayType->size; i++) {
      emitPrint(os, accessIndex(value, i, srcLoc), srcLoc,
                /*quoteStrings=*/true);
      if (i + 1 < arrayType->size) {
        emitPrint(os, ", ", srcLoc);
      }
    }
    emitPrint(os, "]", srcLoc);
  } else if (auto colorType{llvm::dyn_cast<ColorType>(value.type)}) {
    emitPrint(os, "<", srcLoc);
    for (uint32_t i = 0; i < colorType->wavelengthBaseMax; i++) {
      emitPrint(os, accessIndex(value, i, srcLoc), srcLoc);
      if (i + 1 < colorType->wavelengthBaseMax) {
        emitPrint(os, ", ", srcLoc);
      }
    }
    emitPrint(os, ">", srcLoc);
  } else if (auto structType{llvm::dyn_cast<StructType>(value.type)}) {
    emitPrint(os, value.type->displayName + "(", srcLoc);
    for (uint32_t i = 0; i < structType->params.size(); i++) {
      auto paramName{std::string(structType->params[i].name)};
      emitPrint(os, paramName + ": ", srcLoc);
      emitPrint(os, accessField(value, paramName, srcLoc), srcLoc,
                /*quoteStrings=*/true);
      if (i + 1 < structType->params.size()) {
        emitPrint(os, ", ", srcLoc);
      }
    }
    emitPrint(os, ")", srcLoc);
  } else if (value.type->isUnion()) {
    emitVisit(value, srcLoc, [&](Value value) {
      emitPrint(os, value, srcLoc, quoteStrings);
      return Value();
    });
  }
}

Value Emitter::resolveIdentifier(Span<const std::string_view> names,
                                 const SourceLocation &srcLoc,
                                 bool voidByDefault) {
  SMDL_SANITY_CHECK(!names.empty());
  if (auto crumb0{Crumb::find(context, names, getLLVMFunction(), crumb)}) {
    return crumb0->value;
  }
  if (names.size() == 1) {
    if (!currentModule || currentModule->isSMDLSyntax()) {
      if (names[0] == "$state") {
        if (!state)
          srcLoc.throwError(
              "cannot resolve identifier '$state' in pure context");
        return state;
      } else if (names[0] == "$scene_data") {
        return context.getComptimePtr(context.getVoidPointerType(),
                                      &context.compiler.sceneData);
      } else if (names[0] == "i8") {
        return context.getComptimeMetaType(
            context.getArithmeticType(Scalar::getInt(8)));
      } else if (names[0] == "i16") {
        return context.getComptimeMetaType(
            context.getArithmeticType(Scalar::getInt(16)));
      } else if (names[0] == "i32") {
        return context.getComptimeMetaType(
            context.getArithmeticType(Scalar::getInt(32)));
      } else if (names[0] == "i64") {
        return context.getComptimeMetaType(
            context.getArithmeticType(Scalar::getInt(64)));
      } else if (names[0] == "void") {
        return context.getComptimeMetaType(context.getVoidType());
      } else if (names[0] == "none") {
        return RValue(context.getVoidType(), nullptr);
      }
    }
    if (auto value{context.getKeyword(names[0])}) {
      return value;
    }
  }
  if (voidByDefault) {
    return RValue(context.getVoidType(), nullptr);
  }
  srcLoc.throwError("cannot resolve identifier ", Quoted(join(names, "::")));
  return Value();
}

Emitter::ResolvedArguments
Emitter::resolveArguments(const ParameterList &params, const ArgumentList &args,
                          const SourceLocation &srcLoc, bool dontEmit) {
  // Obvious case: If there are more arguments than parameters, resolution
  // fails.
  if (args.size() > params.size()) {
    srcLoc.throwError("too many arguments");
  }
  // Obvious case: If there are argument names that do not correspond to any
  // parameter names, resolution fails.
  if (!args.isOnlyTheseNames(params.getNames())) {
    srcLoc.throwError("invalid argument name(s)");
  }
  // The primary resolution logic.
  ResolvedArguments resolved{args};
  resolved.argParams.resize(args.size());
  resolved.values.resize(params.size());
  auto isResolved{
      [&](size_t argIndex) { return resolved.argParams[argIndex] != nullptr; }};
  for (size_t paramIndex = 0; paramIndex < params.size(); paramIndex++) {
    auto &param{params[paramIndex]};
    auto arg{[&]() -> const Argument * {
      // 1. Look for an explicitly named argument.
      for (size_t argIndex = 0; argIndex < args.size(); argIndex++) {
        if (auto &arg{args[argIndex]}; arg.name == param.name) {
          // If this has already been resolved by a positional argument,
          // resolution fails.
          if (isResolved(argIndex))
            srcLoc.throwError("named argument ", Quoted(arg.name),
                              " already resolved by positional argument");
          resolved.argParams[argIndex] = &param;
          return &arg;
        }
      }
      // 2. If there is no named argument, default to the next positional
      // argument.
      for (size_t argIndex = 0; argIndex < args.size(); argIndex++) {
        if (auto &arg{args[argIndex]};
            arg.isPositional() && !isResolved(argIndex)) {
          resolved.argParams[argIndex] = &param;
          return &arg;
        }
      }
      return nullptr;
    }()};
    if (arg) {
      resolved.values[paramIndex] = arg->value;
      if (!context.isImplicitlyConvertible(arg->value.type, param.type))
        srcLoc.throwError("argument type ",
                          Quoted(arg->value.type->displayName),
                          " is not implicitly convertible to type ",
                          Quoted(param.type->displayName), " of parameter ",
                          Quoted(param.name));
    } else if (!param.getASTInitializer() && !param.builtinDefaultValue) {
      srcLoc.throwError("missing argument for parameter ", Quoted(param.name),
                        " without default initializer");
    }
  }
  // At this point, every argument should be resolved.
  for (size_t argIndex = 0; argIndex < args.size(); argIndex++) {
    SMDL_SANITY_CHECK(isResolved(argIndex));
  }
  if (!dontEmit) {
    SMDL_PRESERVE(crumb);
    crumb = params.lastCrumb;
    handleScope(nullptr, nullptr, [&] {
      for (size_t paramIndex = 0; paramIndex < params.size(); paramIndex++) {
        auto &param{params[paramIndex]};
        auto &value{resolved.values[paramIndex]};
        if (!value) {
          if (auto expr{param.getASTInitializer()}) {
            currentModule = expr->srcLoc.module_;
            value = emit(expr);
          } else {
            value = *param.builtinDefaultValue;
          }
        }
        value = invoke(param.type, value, param.getSourceLocation());
        declareCrumb(param.name, /*node=*/nullptr, value);
      }
    });
  }
  return resolved;
}

Module *Emitter::resolveModule(Span<const std::string_view> importPath,
                               bool isAbs, Module *thisModule) {
  llvm::SmallVector<std::string_view> resolvedImportPath{};
  resolveImportUsingAliases(crumb, importPath, resolvedImportPath);
  SMDL_SANITY_CHECK(!resolvedImportPath.empty());

  auto findModuleInDirectory{[&](std::string dirPath) -> Module * {
    for (auto resolvedImportDirPath :
         Span<const std::string_view>(resolvedImportPath.data(), //
                                      resolvedImportPath.size() - 1)) {
      dirPath = joinPaths(dirPath, resolvedImportDirPath);
    }
    for (auto &otherModule : context.compiler.modules) {
      if (otherModule.get() != thisModule && !otherModule->isBuiltin() &&
          otherModule->getName() == resolvedImportPath.back() &&
          isPathEquivalent(dirPath, otherModule->getDirectory())) {
        if (auto error{otherModule->compile(context)}) {
          throw std::move(*error);
        }
        return otherModule.get();
      }
    }
    return nullptr;
  }};
  auto searchRelativeToCurrentModule{[&]() -> Module * {
    if (!thisModule->isBuiltin())
      if (auto module_{findModuleInDirectory(thisModule->getDirectory())})
        return module_;
    return nullptr;
  }};
  auto searchCompilerDirPaths{[&]() -> Module * {
    if (!thisModule->isBuiltin())
      for (const auto &dirPath : context.compiler.moduleDirSearchPaths)
        if (auto module_{findModuleInDirectory(dirPath)})
          return module_;
    return nullptr;
  }};
  auto searchCompilerBuiltins{[&]() -> Module * {
    if (resolvedImportPath.size() == 1)
      if (auto module_{context.getBuiltinModule(resolvedImportPath[0])})
        return module_;
    return nullptr;
  }};

  if (isAbs) {
    // If the import path is absolute, meaning it starts with `::`,
    // prioritize compiler builtins first and compiler dir paths
    // second. This guarantees that `::df` always gets the builtin
    // df module for example.
    if (auto module_{searchCompilerBuiltins()})
      return module_;
    if (auto module_{searchCompilerDirPaths()})
      return module_;
  } else {
    // If the import path is relative, meaning it does NOT starts with `::`,
    // prioritize modules relative to the current module first.
    if (auto module_{searchRelativeToCurrentModule()})
      return module_;
    // If the import path is not explicitly relative, meaning it does
    // not start with `.` or `..`, also search the compiler dir paths
    // first and then lastly default to compiler builtins.
    if (bool isExplicitlyRelative{importPath[0] == "." ||
                                  importPath[0] == ".."};
        !isExplicitlyRelative) {
      if (auto module_{searchCompilerDirPaths()})
        return module_;
      if (auto module_{searchCompilerBuiltins()})
        return module_;
    }
  }
  return nullptr;
}

void Emitter::resolveImportUsingAliases(
    Crumb *crumbStart, Span<const std::string_view> importPath,
    llvm::SmallVector<std::string_view> &resolvedImportPath) {
  for (auto &importPathElem : importPath) {
    auto crumbItr{crumbStart};
    for (; crumbItr; crumbItr = crumbItr->prev) {
      if (auto decl{llvm::dyn_cast_if_present<AST::UsingAlias>(crumbItr->node)};
          decl && decl->name.srcName == importPathElem) {
        break;
      }
    }
    if (crumbItr) {
      resolveImportUsingAliases(
          crumbItr, static_cast<AST::UsingAlias *>(crumbItr->node)->importPath,
          resolvedImportPath);
    } else {
      resolvedImportPath.push_back(importPathElem);
    }
  }
}

} // namespace smdl
