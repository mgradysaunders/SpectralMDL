// vim:foldmethod=marker:foldlevel=0:fmr=--{,--}
#include "Emitter.h"

#include "smdl/BSDFMeasurement.h"
#include "smdl/Logger.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/Parallel.h"
#include <atomic>

#include "filesystem.h"

#if SMDL_HAS_PTEX
#include "Ptexture.h"
#endif // #if SMDL_HAS_PTEX

namespace smdl {

Value Emitter::create_function_implementation(
    std::string_view name, bool isPure, Type *returnType,
    const ParameterList &params, llvm::ArrayRef<Value> paramValues,
    const SourceLocation &srcLoc, const std::function<void()> &callback) {
  SMDL_SANITY_CHECK(params.size() == paramValues.size());
  SMDL_SANITY_CHECK(callback != nullptr);
  auto fmf{builder.getFastMathFlags()};
  auto fmfDefer{Defer([&] { builder.setFastMathFlags(fmf); })};
  auto preserve{Preserve(labelReturn, returns, crumb)};
  if (params.lastCrumb)
    crumb = params.lastCrumb;
  labelReturn.crumb = crumb;
  labelReturn.block = create_block(llvm::StringRef(name) + ".return");
  returns.clear();
  handle_scope(nullptr, nullptr, [&] {
    auto crumbsToWarnAboutSize{crumbsToWarnAbout.size()};
    labelBreak = labelContinue = {}; // Invalidate
    inDefer = false;
    for (size_t i = 0; i < params.size(); ++i)
      declare_parameter(params[i], paramValues[i]);
    callback();
    if (!has_terminator()) {
      returns.push_back(Result{RValue(context.get_void_type(), nullptr),
                               get_insert_block(), srcLoc});
      unwind(labelReturn.crumb);
      builder.CreateBr(labelReturn.block);
    }
    for (size_t i = crumbsToWarnAboutSize; i < crumbsToWarnAbout.size(); i++)
      crumbsToWarnAbout[i]->maybe_warn_about_unused_value();
    crumbsToWarnAbout.resize(crumbsToWarnAboutSize);
  });
  llvm_move_block_to_end(labelReturn.block);
  builder.SetInsertPoint(labelReturn.block);
  return create_result(returnType, returns, srcLoc);
}

void Emitter::create_function(llvm::Function *&llvmFunc, std::string_view name,
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
    SMDL_SANITY_CHECK(!returnType->is_abstract());
    auto llvmFuncTy{llvm::FunctionType::get(returnType->llvmType, llvmParamTys,
                                            /*isVarArg=*/false)};
    auto llvmCallee{context.get_builtin_callee(name, llvmFuncTy)};
    if (llvmFuncTy != llvmCallee.getFunctionType()) {
      srcLoc.throw_error("conflicting definitions of '@(foreign)' function ",
                         quoted(name));
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
    auto preserve{Preserve(state, inlines)};
    state = {}; // Invalidate
    inlines.clear();
    builder.SetInsertPoint(
        llvm::BasicBlock::Create(context, "entry", llvmFunc));
    auto llvmArg{llvmFunc->arg_begin()};
    if (!isPure) {
      llvmArg->setName("state");
      state = RValue(context.get_pointer_type(context.get_state_type()),
                     &*llvmArg++);
    }
    auto paramValues{llvm::SmallVector<Value>()};
    for (size_t i = 0; i < params.size(); i++) {
      llvmArg->setName(params[i].name);
      paramValues.push_back(RValue(paramTypes[i], &*llvmArg++));
    }
    auto result{create_function_implementation(name, isPure, returnType, params,
                                               paramValues, srcLoc, callback)};
    if (!result || result.type->is_void()) {
      builder.CreateRetVoid();
    } else {
      builder.CreateRet(result);
    }
    returnType = result.type;
    SMDL_SANITY_CHECK(returnType);
    SMDL_SANITY_CHECK(!returnType->is_abstract());
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
      srcLoc.throw_error("function ", quoted(name),
                         " LLVM-IR verification failed: ", message);
    // Inline.
    for (auto &inlineReq : inlines) {
      auto result{llvm_force_inline(inlineReq.value, //
                                    inlineReq.isRecursive)};
      if (!result.isSuccess())
        inlineReq.srcLoc.log_warn(std::string("cannot force inline: ") +
                                  result.getFailureReason());
    }
  }
  builder.restoreIP(lastIP);
}

Value Emitter::create_alloca(Type *type, const llvm::Twine &name) {
  auto llvmFunc{get_llvm_function()};
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

void Emitter::declare_parameter(const Parameter &param, Value value) {
  value = invoke(param.type, value, param.get_source_location());
  value = param.is_const() || value.is_void() ? value : to_lvalue(value);
  crumbsToWarnAbout.push_back(
      declare_crumb(param.name,
                    param.astParam   ? static_cast<AST::Node *>(param.astParam)
                    : param.astField ? static_cast<AST::Node *>(param.astField)
                                     : nullptr,
                    value));
  if (param.is_inline())
    declare_parameter_inline(crumb->value);
}

void Emitter::declare_parameter_inline(Value value) {
  if (auto structType{llvm::dyn_cast<StructType>(
          value.type->get_first_non_pointer_type())}) {
    for (auto &param : structType->params) {
      auto srcLoc{param.get_source_location()};
      declare_crumb(param.name, /*node=*/{},
                    access_field(value, param.name, srcLoc));
      if (param.is_inline())
        declare_parameter_inline(crumb->value);
    }
  }
}

void Emitter::declare_import(Span<std::string_view> importPath, bool isAbs,
                             AST::Decl &decl) {
  if (importPath.size() < 2)
    decl.srcLoc.throw_error("invalid import path (missing '::*'?)");
  auto isDots{[](auto elem) { return elem == "." || elem == ".."; }};
  auto importedModule{
      resolve_module(importPath.drop_back(), isAbs, decl.srcLoc.module_)};
  if (!importedModule)
    decl.srcLoc.throw_error("cannot resolve import identifier ",
                            quoted(join(importPath, "::")));
  if (importPath.back() == "*") {
    importPath = importPath.drop_back();
    importPath = importPath.drop_front_while(isDots);
    declare_crumb(importPath, &decl,
                  context.get_comptime_meta_module(importedModule));
  } else {
    auto importedCrumb{Crumb::find(
        context, importPath.back(), get_llvm_function(),
        importedModule->lastCrumb, nullptr, /*ignoreIfNotExported=*/true)};
    if (!importedCrumb)
      decl.srcLoc.throw_error("cannot resolve import identifier ",
                              quoted(join(importPath, "::")));
    declare_crumb(importPath.drop_front_while(isDots), &decl,
                  importedCrumb->value);
  }
}

void Emitter::unwind(Crumb *lastCrumb) {
  SMDL_SANITY_CHECK(!has_terminator(), "tried to unwind after terminator");
  for (; crumb && crumb != lastCrumb; crumb = crumb->prev) {
    if (crumb->is_ast_defer()) {
      handle_scope(nullptr, nullptr, [&] {
        labelReturn = {};   // Invalidate!
        labelBreak = {};    // Invalidate!
        labelContinue = {}; // Invalidate!
        inDefer = true;     // More specific error messages
        emit(static_cast<AST::Defer *>(crumb->node)->stmt);
      });
    } else if (crumb->is_ast_preserve()) {
      builder.CreateStore(crumb->valueToPreserve, crumb->value);
    } else if (crumb->value) {
      create_lifetime_end(crumb->value);
    }
  }
  SMDL_SANITY_CHECK(crumb == lastCrumb, "inconsistent unwind");
}

Value Emitter::create_result(Type *type, llvm::ArrayRef<Result> results,
                             const SourceLocation &srcLoc) {
  SMDL_SANITY_CHECK(type);
  if (type->is_abstract()) {
    auto resultTypes{llvm::SmallVector<Type *>{}};
    for (auto &result : results)
      resultTypes.push_back(result.value.type ? result.value.type
                                              : context.get_void_type());
    auto resultType{
        context.get_common_type(resultTypes, /*defaultToUnion=*/true, srcLoc)};
    if (context.get_conversion_rule(resultType, type) ==
        ConversionRule::NotAllowed)
      srcLoc.throw_error("inferred result type ",
                         quoted(resultType->displayName),
                         " is not convertible to ", quoted(type->displayName));
    type = resultType;
    SMDL_SANITY_CHECK(!type->is_abstract());
  }
  if (type->is_void()) {
    return RValue(type, nullptr);
  }
  bool isAllIdenticalLValues{[&]() {
    for (auto &result : results)
      if (!(result.value.is_lvalue() &&
            result.value.type == results[0].value.type))
        return false;
    return true;
  }()};
  auto crumb0{crumb};
  auto phiInst{builder.CreatePHI(isAllIdenticalLValues
                                     ? context.get_pointer_type(type)->llvmType
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
      block = get_insert_block();
    }
    phiInst->addIncoming(value, block);
  }
  builder.restoreIP(ip);
  crumb = crumb0;
  return isAllIdenticalLValues ? LValue(type, phiInst) : RValue(type, phiInst);
}

Value Emitter::emit(AST::Node &node) {
  return emit_type_switch<AST::File, AST::Decl, AST::Expr, AST::Stmt>(node);
}

//--{ Emit: Decl
Value Emitter::emit(AST::Decl &decl) {
  return emit_type_switch< //
      AST::Enum, AST::Exec, AST::Function, AST::Import, AST::Namespace,
      AST::Struct, AST::Tag, AST::Typedef, AST::UnitTest, AST::UsingAlias,
      AST::UsingImport, AST::Variable>(decl);
}

Value Emitter::emit(AST::Exec &decl) {
  auto returnType{context.get_void_type()};
  auto llvmFunc{create_function( //
      ".exec", /*isPure=*/false, returnType, ParameterList(), decl.srcLoc,
      [&] { emit(decl.stmt); })};
  llvmFunc->setLinkage(llvm::Function::ExternalLinkage);
  context.compiler.jitExecs.emplace_back(llvmFunc->getName().str());
  return Value();
}

Value Emitter::emit(AST::UnitTest &decl) {
  if (context.compiler.enableUnitTests) {
    auto returnType{context.get_void_type()};
    auto llvmFunc{create_function( //
        ".unit_test", /*isPure=*/false, returnType, ParameterList(),
        decl.srcLoc, [&] { emit(decl.stmt); })};
    llvmFunc->setLinkage(llvm::Function::ExternalLinkage);
    auto &jitTest{context.compiler.jitUnitTests.emplace_back()};
    jitTest.moduleName = std::string(decl.srcLoc.get_module_name());
    jitTest.moduleFileName = std::string(decl.srcLoc.get_module_file_name());
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
    declare_import(Span(importPathPtr, importPath.size()),
                   decl.importPath.is_absolute(), decl);
    importPath.pop_back();
  }
  return Value();
}

Value Emitter::emit(AST::Variable &decl) {
  auto type{emit(decl.type).get_comptime_meta_type(context, decl.srcLoc)};
  if (type->is_function())
    decl.srcLoc.throw_error("variable must not have function type ",
                            quoted(type->displayName));
  const bool isConst{decl.type->has_qualifier("const")};
  const bool isStatic{decl.type->has_qualifier("static")};
  const bool isInline{decl.type->has_qualifier("inline")};
  if (!get_llvm_function() && !isConst)
    decl.srcLoc.throw_error(
        "variables declared at module scope must be 'const'");
  if (isStatic && !isConst)
    decl.srcLoc.throw_error(
        "variables declared 'static' must be 'const' (at least for now)");
  if (isInline)
    decl.srcLoc.throw_error("variables must not be declared 'inline'");
  for (auto &declarator : decl.declarators) {
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
    auto value{invoke(type, args, declarator.name.srcLoc)};
    if (!value.is_void()) {
      if (isStatic) {
        if (!value.is_comptime())
          declarator.name.srcLoc.throw_error(
              "static variable ", quoted(declarator.name.srcName),
              " requires compile-time initializer");
        auto llvmGlobal{new llvm::GlobalVariable(
            context.llvmModule, value.type->llvmType, /*isConstant=*/true,
            llvm::GlobalValue::PrivateLinkage,
            static_cast<llvm::Constant *>(value.llvmValue))};
        llvmGlobal->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
        llvmGlobal->setName(declarator.name.srcName);
        value = LValue(value.type, llvmGlobal);
      } else if (isConst) {
        value.llvmValue->setName(declarator.name.srcName);
      } else {
        auto valueAlloca{create_alloca(value.type, declarator.name.srcName)};
        create_lifetime_start(valueAlloca);
        builder.CreateStore(value, valueAlloca);
        value = LValue(value.type, valueAlloca);
      }
    }
    declare_crumb(declarator.name, &declarator, value);
    if (get_llvm_function())
      crumbsToWarnAbout.push_back(crumb);
  }
  return Value();
}
//--}

//--{ Emit: Expr
Value Emitter::emit(AST::Expr &expr) {
  return emit_type_switch< //
      AST::AccessField, AST::AccessIndex, AST::Binary, AST::Call,
      AST::Identifier, AST::Intrinsic, AST::Let, AST::LiteralBool,
      AST::LiteralFloat, AST::LiteralInt, AST::LiteralString, AST::Parens,
      AST::ReturnFrom, AST::Select, AST::Type, AST::TypeCast, AST::Unary>(expr);
}

Value Emitter::emit(AST::AccessIndex &expr) {
  auto value{emit(expr.expr)};
  if (!value.is_comptime_meta_type(context)) {
    for (auto &index : expr.indexes) {
      if (!index.expr)
        expr.srcLoc.throw_error("expected non-empty '[]'");
      value = access_index(
          value, invoke(context.get_int_type(), emit(index.expr), expr.srcLoc),
          expr.srcLoc);
    }
    return value;
  } else {
    auto type{value.get_comptime_meta_type(context, expr.srcLoc)};
    for (auto itr{expr.indexes.rbegin()}; itr != expr.indexes.rend(); ++itr) {
      auto &index{*itr};
      if (!index.expr) {
        type = context.get_inferred_size_array_type(type); // Empty
      } else if (auto sizeName{
                     llvm::dyn_cast<AST::SizeName>(index.expr.get())}) {
        type = context.get_inferred_size_array_type(
            type, std::string(sizeName->name.srcName));
      } else {
        auto size{
            invoke(context.get_int_type(), emit(index.expr), expr.srcLoc)};
        if (!size.is_comptime_int())
          expr.srcLoc.throw_error(
              "expected array size expression to resolve to compile-time int");
        type = context.get_array_type(type, size.get_comptime_int());
      }
    }
    return context.get_comptime_meta_type(type);
  }
}

Value Emitter::emit(AST::Binary &expr) {
  // Temporary let.
  if (expr.op == BINOP_LET) {
    auto ident{llvm::dyn_cast<AST::Identifier>(&*expr.exprLhs)};
    if (!ident) // || !ident->is_simple_name())
      expr.srcLoc.throw_error(
          "expected lhs of operator ':=' to be an identifier");
    auto rv{to_rvalue(emit(expr.exprRhs))};
    declare_crumb(*ident, ident, rv);
    return rv;
  }
  // Short-circuit logic conditions.
  if (expr.op == BINOP_LOGIC_AND || expr.op == BINOP_LOGIC_OR) {
    auto boolType{context.get_bool_type()};
    auto valueLhs{invoke(boolType, emit(expr.exprLhs), expr.srcLoc)};
    if (valueLhs.is_comptime_int()) {
      auto valueLhsNow{valueLhs.get_comptime_int()};
      if ((valueLhsNow != 0 && expr.op == BINOP_LOGIC_AND) ||
          (valueLhsNow == 0 && expr.op == BINOP_LOGIC_OR))
        return invoke(boolType, emit(expr.exprRhs), expr.srcLoc);
      return valueLhs;
    } else {
      auto blockLhs{get_insert_block()};
      auto [blockRhs, blockEnd] = create_blocks<2>(
          expr.op == BINOP_LOGIC_AND ? "and" : "or", {".rhs", ".end"});
      builder.CreateCondBr(valueLhs,
                           expr.op == BINOP_LOGIC_AND ? blockRhs : blockEnd,
                           expr.op == BINOP_LOGIC_AND ? blockEnd : blockRhs);
      builder.SetInsertPoint(blockRhs);
      auto valueRhs{invoke(boolType, emit(expr.exprRhs), expr.srcLoc)};
      blockRhs = get_insert_block();
      builder.CreateBr(blockEnd);
      builder.SetInsertPoint(blockEnd);
      auto phiInst{builder.CreatePHI(boolType->llvmType, 2)};
      phiInst->addIncoming(context.get_comptime_bool(expr.op == BINOP_LOGIC_OR),
                           blockLhs);
      phiInst->addIncoming(valueRhs, blockRhs);
      return RValue(boolType, phiInst);
    }
  }
  // Short-circuit else.
  if (expr.op == BINOP_ELSE) {
    auto valueLhs{emit(expr.exprLhs)};
    auto valueLhsCond{invoke(context.get_bool_type(), valueLhs, expr.srcLoc)};
    if (valueLhsCond.is_comptime_int()) {
      return valueLhsCond.get_comptime_int() ? valueLhs : emit(expr.exprRhs);
    } else {
      auto [blockLhs, blockRhs, blockEnd] =
          create_blocks<3>("else", {".lhs", ".rhs", ".end"});
      builder.CreateCondBr(valueLhsCond, blockLhs, blockRhs);
      builder.SetInsertPoint(blockLhs);
      auto valueLhsIP{builder.saveIP()};
      llvm_move_block_to_end(blockRhs);
      builder.SetInsertPoint(blockRhs);
      auto valueRhs{emit(expr.exprRhs)};
      auto valueRhsIP{builder.saveIP()};
      auto commonType{context.get_common_type({valueLhs.type, valueRhs.type},
                                              /*defaultToUnion=*/true,
                                              expr.srcLoc)};
      builder.restoreIP(valueLhsIP);
      valueLhs = invoke(commonType, valueLhs, expr.exprLhs->srcLoc);
      blockLhs = get_insert_block();
      builder.CreateBr(blockEnd);
      builder.restoreIP(valueRhsIP);
      valueRhs = invoke(commonType, valueRhs, expr.exprRhs->srcLoc);
      blockRhs = get_insert_block();
      builder.CreateBr(blockEnd);
      // Create PHI instruction.
      llvm_move_block_to_end(blockEnd);
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
    auto res{emit_op(BINOP_SUB, lhs, rhs, expr.srcLoc)};
    res = emit_intrinsic("abs", res, expr.srcLoc);
    res = emit_op(BINOP_CMP_LT, res, emit(expr.exprEps), expr.srcLoc);
    if (expr.op == BINOP_APPROX_CMP_NE)
      res = emit_op(UNOP_LOGIC_NOT, res, expr.srcLoc);
    return res;
  }
  return emit_op(expr.op, lhs, rhs, expr.srcLoc);
}

Value Emitter::emit(AST::Parens &expr) {
  if (expr.is_comptime()) {
    auto value{emit(expr.expr)};
    if (!value.is_comptime()) {
      expr.srcLoc.throw_error("expected compile-time constant");
    }
    if (value.is_comptime_meta_type(context)) {
      auto type{value.get_comptime_meta_type(context, expr.srcLoc)};
      if (auto unionType{llvm::dyn_cast<UnionType>(type)}) {
        return context.get_comptime_meta_type(
            context.get_comptime_union_type(unionType));
      }
    }
    return value;
  } else {
    return emit(expr.expr);
  }
}

Value Emitter::emit(AST::ReturnFrom &expr) {
  auto [blockBegin, blockEnd] =
      create_blocks<2>("return_from", {".begin", ".end"});
  auto preserve{Preserve(returns)};
  returns.clear();
  builder.CreateBr(blockBegin);
  builder.SetInsertPoint(blockBegin);
  handle_scope(blockBegin, blockEnd, [&, blockEnd = blockEnd] {
    labelReturn = {crumb, blockEnd};
    labelBreak = {};
    labelContinue = {};
    emit(expr.stmt);
  });
  llvm_move_block_to_end(blockEnd);
  builder.SetInsertPoint(blockEnd);
  return create_result(context.get_auto_type(), returns, expr.srcLoc);
}

Value Emitter::emit(AST::Select &expr) {
  auto cond{invoke(context.get_bool_type(), emit(expr.exprCond), expr.srcLoc)};
  if (cond.is_comptime_int())
    return emit(cond.get_comptime_int() ? expr.exprThen : expr.exprElse);
  auto [blockThen, blockElse, blockEnd] =
      create_blocks<3>("select", {".then", ".else", ".end"});
  builder.CreateCondBr(cond, blockThen, blockElse);
  builder.SetInsertPoint(blockThen);
  auto valueThen{emit(expr.exprThen)};
  auto valueThenIP{builder.saveIP()};
  llvm_move_block_to_end(blockElse);
  builder.SetInsertPoint(blockElse);
  auto valueElse{emit(expr.exprElse)};
  auto valueElseIP{builder.saveIP()};
  auto commonType{context.get_common_type({valueThen.type, valueElse.type},
                                          /*defaultToUnion=*/true,
                                          expr.srcLoc)};
  builder.restoreIP(valueThenIP);
  valueThen = invoke(commonType, valueThen, expr.exprThen->srcLoc);
  blockThen = get_insert_block();
  builder.CreateBr(blockEnd);
  builder.restoreIP(valueElseIP);
  valueElse = invoke(commonType, valueElse, expr.exprElse->srcLoc);
  blockElse = get_insert_block();
  builder.CreateBr(blockEnd);
  // Create PHI instruction.
  llvm_move_block_to_end(blockEnd);
  builder.SetInsertPoint(blockEnd);
  auto phiInst{builder.CreatePHI(commonType->llvmType, 2)};
  phiInst->addIncoming(valueThen, blockThen);
  phiInst->addIncoming(valueElse, blockElse);
  return RValue(commonType, phiInst);
}
//--}

//--{ Emit: Stmt
Value Emitter::emit(AST::Stmt &stmt) {
  return emit_type_switch< //
      AST::Break, AST::Compound, AST::Continue, AST::DeclStmt, AST::Defer,
      AST::DoWhile, AST::ExprStmt, AST::For, AST::If, AST::Preserve,
      AST::Return, AST::Switch, AST::Unreachable, AST::Visit, AST::While>(stmt);
}

Value Emitter::emit(AST::Compound &stmt) {
  handle_scope(nullptr, nullptr, [&] {
    for (auto &subStmt : stmt.stmts)
      if (emit(subStmt); has_terminator())
        break;
  });
  return Value();
}

Value Emitter::emit(AST::DoWhile &stmt) {
  auto [blockLoop, blockCond, blockEnd] =
      create_blocks<3>("do_while", {".loop", ".cond", ".end"});
  handle_scope(blockLoop, blockCond,
               [&, blockCond = blockCond, blockEnd = blockEnd] {
                 labelBreak = {crumb, blockEnd};
                 labelContinue = {crumb, blockCond};
                 inDefer = false;
                 emit(stmt.stmt);
               });
  builder.SetInsertPoint(blockCond);
  builder.CreateCondBr(
      invoke(context.get_bool_type(), emit(stmt.expr), stmt.expr->srcLoc),
      blockLoop, blockEnd);
  handle_block_end(blockEnd);
  return Value();
}

Value Emitter::emit(AST::For &stmt) {
  auto crumb0{crumb};
  auto [blockCond, blockLoop, blockNext, blockEnd] =
      create_blocks<4>("for", {".cond", ".loop", ".next", ".end"});
  if (stmt.stmtInit)
    emit(stmt.stmtInit);
  builder.CreateBr(blockCond);
  llvm_move_block_to_end(blockCond);
  builder.SetInsertPoint(blockCond);
  builder.CreateCondBr(invoke(context.get_bool_type(), emit(stmt.exprCond),
                              stmt.exprCond->srcLoc),
                       blockLoop, blockEnd);
  handle_scope(blockLoop, blockNext,
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
  handle_block_end(blockEnd);
  if (!has_terminator())
    unwind(crumb0);
  crumb = crumb0;
  return Value();
}

Value Emitter::emit(AST::If &stmt) {
  auto cond{invoke(context.get_bool_type(), emit(stmt.expr), stmt.srcLoc)};
  if (cond.is_comptime_int()) {
    handle_scope(nullptr, nullptr, [&] {
      if (cond.get_comptime_int())
        emit(stmt.stmtThen);
      else if (stmt.stmtElse)
        emit(stmt.stmtElse);
    });
    return Value();
  }
  auto [blockThen, blockElse, blockEnd] =
      create_blocks<3>("if", {".then", ".else", ".end"});
  if (!stmt.stmtElse)
    blockElse->removeFromParent();
  builder.CreateCondBr(cond, blockThen, stmt.stmtElse ? blockElse : blockEnd);
  if (stmt.stmtThen)
    handle_scope(blockThen, blockEnd, [&] { emit(stmt.stmtThen); });
  if (stmt.stmtElse)
    handle_scope(blockElse, blockEnd, [&] { emit(stmt.stmtElse); });
  handle_block_end(blockEnd);
  return Value();
}

Value Emitter::emit(AST::Switch &stmt) {
  auto switchName{context.get_unique_name("switch", get_llvm_function())};
  auto switchNameRef{llvm::StringRef(switchName)};
  auto blockEnd{create_block(switchNameRef + ".end")};
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
        stmt.srcLoc.throw_error(
            "expected at most 1 'default' case in 'switch'");
      switchCases.push_back(SwitchCase{
          &astCase, nullptr, create_block(switchNameRef + ".default")});
      blockDefault = switchCases.back().block;
    } else {
      auto value{emit(astCase.expr)};
      auto llvmConst{llvm::dyn_cast<llvm::ConstantInt>(value.llvmValue)};
      if (!llvmConst)
        astCase.expr->srcLoc.throw_error(
            "expected 'case' expression to resolve to compile-time int");
      switchCases.push_back(SwitchCase{
          &astCase, llvmConst,
          create_block(concat(switchName, ".case.", switchCases.size()))});
    }
  }
  if (!blockDefault)
    blockDefault = blockEnd;
  auto switchInst{builder.CreateSwitch(
      invoke(context.get_int_type(), emit(stmt.expr), stmt.srcLoc),
      blockDefault)};
  handle_scope(nullptr, blockEnd, [&] {
    auto i{size_t(0)};
    auto crumb0{crumb};
    labelBreak = {crumb, blockEnd};
    for (auto &[astCase, llvmConst, block] : switchCases) {
      if (llvmConst)
        switchInst->addCase(llvmConst, block);
      builder.SetInsertPoint(block);
      for (auto &subStmt : astCase->stmts)
        if (emit(subStmt); has_terminator())
          break;
      if (has_terminator())
        crumb = crumb0; // Reset after `return`, `break`, or `continue`
      if (++i < switchCases.size()) {
        if (!has_terminator())
          builder.CreateBr(switchCases[i].block);
        llvm_move_block_to_end(switchCases[i].block);
      }
    }
  });
  handle_block_end(blockEnd);
  return Value();
}

Value Emitter::emit(AST::While &stmt) {
  auto [blockCond, blockLoop, blockEnd] =
      create_blocks<3>("while", {".cond", ".loop", ".end"});
  builder.CreateBr(blockCond);
  builder.SetInsertPoint(blockCond);
  builder.CreateCondBr(
      invoke(context.get_bool_type(), emit(stmt.expr), stmt.expr->srcLoc),
      blockLoop, blockEnd);
  handle_scope(blockLoop, blockCond,
               [&, blockCond = blockCond, blockEnd = blockEnd] {
                 labelBreak = {crumb, blockEnd};
                 labelContinue = {crumb, blockCond};
                 inDefer = false;
                 emit(stmt.stmt);
               });
  handle_block_end(blockEnd);
  return Value();
}
//--}

//--{ emit_op (AST::UnaryOp)
Value Emitter::emit_op(AST::UnaryOp op, Value value,
                       const SourceLocation &srcLoc) {
  if (value.is_comptime_meta_type(context)) {
    auto type{value.get_comptime_meta_type(context, srcLoc)};
    switch (op) {
    // Add pointer type, e.g., `&int`
    case UNOP_ADDR:
      return context.get_comptime_meta_type(context.get_pointer_type(type));
    // Remove pointer type, e.g., `*(&int)`
    case UNOP_DEREF:
      if (type->is_pointer())
        return context.get_comptime_meta_type(type->get_pointee_type());
      srcLoc.throw_error("cannot dereference ", quoted(type->displayName));
      break;
    // Union with `void`, e.g., `?int`
    case UNOP_MAYBE:
      if (type->is_void())
        srcLoc.throw_error("cannot optionalize 'void'");
      if (type->is_abstract())
        srcLoc.throw_error("cannot optionalize abstract type ",
                           quoted(type->displayName));
      return context.get_comptime_meta_type(
          context.get_union_type({context.get_void_type(), type}));
    default:
      break;
    }
  } else {
    // If the operator is a postfix increment or decrement,
    // follow the C convention of returning the value before the
    // operation is applied.
    if ((op & UNOP_POSTFIX) == UNOP_POSTFIX) {
      auto result{to_rvalue(value)}; // Save
      emit_op(op & ~UNOP_POSTFIX, value, srcLoc);
      return result;
    }
    // Unary increment or decrement, e.g., `++value` or `--value`
    if (op == UNOP_INC || op == UNOP_DEC) {
      /// We can increment or decrement if the value is an arithmetic scalar,
      /// arithmetic vector, color, or pointer.
      if (value.type->is_arithmetic_scalar() ||
          value.type->is_arithmetic_vector() || value.type->is_color() ||
          value.type->is_pointer())
        return emit_op(op == UNOP_INC ? BINOP_EQ_ADD : BINOP_EQ_SUB, value,
                       context.get_comptime_int(1), srcLoc);
      srcLoc.throw_error("cannot increment or decrement ",
                         quoted(value.type->displayName));
    }
    // Unary positive, e.g., `+value`
    if (op == UNOP_POS) {
      return to_rvalue(value);
    }
    // Unary negative, e.g., `-value`
    if (op == UNOP_NEG) {
      if (value.type->is_vectorized() && value.type->is_arithmetic_integral()) {
        return RValue(value.type, builder.CreateNeg(to_rvalue(value)));
      } else if (value.type->is_vectorized()) {
        return RValue(value.type, builder.CreateFNeg(to_rvalue(value)));
      } else if (value.type->is_arithmetic_matrix()) {
        return emit_op_columnwise(value.type, [&](unsigned j) {
          return emit_op(op, access_index(value, j, srcLoc), srcLoc);
        });
      }
    }
    // Unary not, e.g., `~value`
    if (op == UNOP_NOT) {
      if ((value.type->is_arithmetic_scalar() ||
           value.type->is_arithmetic_vector()) &&
          value.type->is_arithmetic_integral())
        return RValue(value.type, builder.CreateNot(to_rvalue(value)));
    }
    // Unary logic not, e.g., `!value`
    if (op == UNOP_LOGIC_NOT) {
      if (value.is_void())
        return context.get_comptime_bool(true);
      if (value.type->is_arithmetic() || value.type->is_color() ||
          value.type->is_pointer() || value.type->is_enum())
        return emit_op(BINOP_CMP_EQ, value, Value::zero(value.type), srcLoc);
      if (value.type->is_optional_union())
        return emit_op(UNOP_LOGIC_NOT,
                       invoke(context.get_bool_type(), value, srcLoc), srcLoc);
    }
    // Unary address, e.g., `&value`
    if (op == UNOP_ADDR) {
      if (!value.is_lvalue())
        srcLoc.throw_error("cannot take address of rvalue");
      return RValue(context.get_pointer_type(value.type), value);
    }
    // Unary dereference, e.g., `*value`
    if (op == UNOP_DEREF) {
      if (value.type->is_pointer())
        return LValue(value.type->get_pointee_type(), to_rvalue(value));
      if (value.type->is_optional_union()) {
        if (value.is_rvalue()) {
          auto lv{to_lvalue(value)};
          auto rv{to_rvalue(emit_op(op, lv, srcLoc))};
          create_lifetime_end(lv);
          return rv;
        }
        return LValue(
            context.get_union_type(llvm::ArrayRef(
                static_cast<UnionType *>(value.type)->caseTypes.data(),
                static_cast<UnionType *>(value.type)->caseTypes.size() - 1)),
            value.llvmValue);
      }
    }
  }
  srcLoc.throw_error("unimplemented unary operator ", quoted(to_string(op)),
                     " for type ", quoted(value.type->displayName));
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
Value Emitter::emit_op(AST::BinaryOp op, Value lhs, Value rhs,
                       const SourceLocation &srcLoc) {
  if (op == BINOP_COMMA)
    return rhs;
  if ((op & BINOP_EQ) == BINOP_EQ) {
    if (!lhs.is_lvalue())
      srcLoc.throw_error("cannot apply ", quoted(to_string(op)), " to rvalue");
    if (op != BINOP_EQ)
      rhs = emit_op(op & ~BINOP_EQ, to_rvalue(lhs), rhs, srcLoc);
    builder.CreateStore(invoke(lhs.type, rhs, srcLoc), lhs);
    return lhs;
  }
  lhs = to_rvalue(lhs);
  rhs = to_rvalue(rhs);
  // Numbers
  if (lhs.type->is_arithmetic() && rhs.type->is_arithmetic()) {
    auto lhsType{static_cast<ArithmeticType *>(lhs.type)};
    auto rhsType{static_cast<ArithmeticType *>(rhs.type)};
    // Scalar and vector operations
    if ((lhsType->extent.is_scalar() && rhsType->extent.is_scalar()) ||
        (lhsType->extent.is_scalar() && rhsType->extent.is_vector()) ||
        (lhsType->extent.is_vector() && rhsType->extent.is_scalar()) ||
        (lhsType->extent.is_vector() && lhsType->extent == rhsType->extent)) {
      auto commonType{lhsType->get_common_type(context, rhsType)};
      lhs = invoke(commonType, lhs, srcLoc);
      rhs = invoke(commonType, rhs, srcLoc);
      if (auto llvmOp{llvm_arith_op(commonType->scalar.intent, op)})
        return RValue(commonType, builder.CreateBinOp(*llvmOp, lhs, rhs));
      if (auto llvmOp{llvm_cmp_op(commonType->scalar.intent, op)})
        return RValue(
            commonType->get_with_different_scalar(context, Scalar::get_bool()),
            builder.CreateCmp(*llvmOp, lhs, rhs));
    }
    // Matrix-Matrix (ADD, SUB)
    if ((op == BINOP_ADD || op == BINOP_SUB) && //
        lhsType->extent.is_matrix() &&          //
        lhsType->extent == rhsType->extent) {
      return emit_op_columnwise(
          lhsType->get_common_type(context, rhsType), [&](unsigned j) {
            auto lhsColumn{access_index(lhs, j, srcLoc)};
            auto rhsColumn{access_index(rhs, j, srcLoc)};
            return emit_op(op, lhsColumn, rhsColumn, srcLoc);
          });
    }
    // Matrix-Scalar (MUL, DIV, REM)
    if ((op == BINOP_MUL || op == BINOP_DIV || op == BINOP_REM) && //
        lhsType->extent.is_matrix() &&                             //
        rhsType->extent.is_scalar()) {
      auto commonType{lhsType->get_common_type(context, rhsType)};
      auto scalarAsColumn{
          invoke(commonType->get_column_type(context), rhs, srcLoc)};
      return emit_op_columnwise(commonType, [&](unsigned j) {
        return emit_op(op, access_index(lhs, j, srcLoc), scalarAsColumn,
                       srcLoc);
      });
    }
    // Matrix-Matrix or Matrix-Vector (MUL)
    if ((op == BINOP_MUL) && lhsType->extent.is_matrix() &&
        (rhsType->extent.is_matrix() || rhsType->extent.is_vector()) &&
        (lhsType->extent.numCols == rhsType->extent.numRows)) {
      auto scalar{lhsType->scalar.get_common(rhsType->scalar)};
      lhs = invoke(lhsType->get_with_different_scalar(context, scalar), lhs,
                   srcLoc);
      rhs = invoke(rhsType->get_with_different_scalar(context, scalar), rhs,
                   srcLoc);
      auto extentM{lhsType->extent.numRows};
      auto extentN{lhsType->extent.numCols};
      auto extentP{rhsType->extent.numCols};
      auto result{Value::zero(
          context.get_arithmetic_type(scalar, Extent(extentP, extentM)))};
      auto lhsColumns{llvm::SmallVector<Value>(size_t(extentN))};
      for (unsigned k = 0; k < extentN; k++)
        lhsColumns[k] = access_index(lhs, k, srcLoc);
      for (unsigned j = 0; j < extentP; j++) {
        auto rhsColumn{extentP == 1 ? rhs : access_index(rhs, j, srcLoc)};
        auto resColumnTerm{[&](unsigned k) {
          return emit_op(BINOP_MUL, lhsColumns[k],
                         access_index(rhsColumn, k, srcLoc), srcLoc);
        }};
        auto resColumn{resColumnTerm(0)};
        for (unsigned k = 1; k < extentN; k++)
          resColumn = emit_op(BINOP_ADD, resColumn, resColumnTerm(k), srcLoc);
        result = extentP == 1 //
                     ? resColumn
                     : insert(result, resColumn, j, srcLoc);
      }
      return result;
    }
    // Vector-Matrix (MUL)
    if ((op == BINOP_MUL) && lhsType->extent.is_vector() &&
        rhsType->extent.is_matrix() &&
        lhsType->extent.numRows == rhsType->extent.numRows) {
      auto scalar{lhsType->scalar.get_common(rhsType->scalar)};
      lhs = invoke(lhsType->get_with_different_scalar(context, scalar), lhs,
                   srcLoc);
      rhs = invoke(rhsType->get_with_different_scalar(context, scalar), rhs,
                   srcLoc);
      /* auto extentN{rhsType->extent.numRows}; */
      auto extentP{rhsType->extent.numCols};
      auto result{
          Value::zero(context.get_arithmetic_type(scalar, Extent(extentP)))};
      // Row-vector times matrix, equivalent to `#transpose(matrix) * vector`
      for (unsigned j = 0; j < extentP; j++) {
        auto resColumn{access_index(rhs, j, srcLoc)};
        resColumn = emit_op(BINOP_MUL, lhs, resColumn, srcLoc);
        resColumn = emit_intrinsic("sum", resColumn, srcLoc);
        result = insert(result, resColumn, j, srcLoc);
      }
      return result;
    }
  }
  // Enums
  if (lhs.type->is_enum() && rhs.type->is_enum()) {
    if (auto llvmOp{llvm_arith_op(Scalar::Intent::Int, op)};
        llvmOp && lhs.type == rhs.type)
      return RValue(lhs.type, builder.CreateBinOp(*llvmOp, lhs, rhs));
    if (auto llvmOp{llvm_cmp_op(Scalar::Intent::Int, op)})
      return RValue(context.get_bool_type(),
                    builder.CreateCmp(*llvmOp, lhs, rhs));
  }
  // Colors
  if ((lhs.type->is_color() &&
       (rhs.type->is_color() || rhs.type->is_arithmetic_scalar())) ||
      (lhs.type->is_arithmetic_scalar() && rhs.type->is_color())) {
    if (auto llvmOp{llvm_arith_op(Scalar::Intent::FP, op)}) {
      lhs = invoke(context.get_color_type(), lhs, srcLoc);
      rhs = invoke(context.get_color_type(), rhs, srcLoc);
      return RValue(context.get_color_type(),
                    builder.CreateBinOp(*llvmOp, lhs, rhs));
    }
    if (auto llvmOp{llvm_cmp_op(Scalar::Intent::FP, op)}) {
      lhs = invoke(context.get_color_type(), lhs, srcLoc);
      rhs = invoke(context.get_color_type(), rhs, srcLoc);
      return RValue(
          context.get_color_type()
              ->get_arithmetic_vector_type(context)
              ->get_with_different_scalar(context, Scalar::get_bool()),
          builder.CreateCmp(*llvmOp, lhs, rhs));
    }
  }
  // Strings
  if (lhs.type->is_string() && rhs.type->is_string() &&
      (op == BINOP_CMP_EQ || op == BINOP_CMP_NE)) {
    auto result{llvm::emitStrNCmp(
        lhs, rhs,
        llvm_emit_cast(builder, access_field(lhs, "size", srcLoc),
                       builder.getInt64Ty()),
        builder, context.llvmLayout, &context.llvmTargetLibraryInfo)};
    return RValue(context.get_bool_type(),
                  op == BINOP_CMP_EQ ? builder.CreateIsNull(result)
                                     : builder.CreateIsNotNull(result));
  }
  // Pointers
  if ((op == BINOP_ADD || op == BINOP_SUB) && lhs.type->is_pointer() &&
      rhs.type->is_arithmetic_integral()) {
    if (op == BINOP_SUB)
      rhs = emit_op(UNOP_NEG, rhs, srcLoc);
    return RValue(lhs.type,
                  builder.CreateGEP(lhs.type->get_pointee_type()->llvmType, lhs,
                                    {rhs.llvmValue}));
  }
  if (lhs.type->is_pointer() && rhs.type->is_pointer()) {
    if (op == BINOP_SUB) {
      if (lhs.type != rhs.type)
        srcLoc.throw_error(
            "pointer subtraction requires both pointers to be the same type");
      return RValue(context.get_int_type(),
                    builder.CreateIntCast(
                        builder.CreatePtrDiff(
                            lhs.type->get_pointee_type()->llvmType, lhs, rhs),
                        context.get_int_type()->llvmType, /*isSigned=*/true));
    }
    if (auto llvmOp{llvm_cmp_op(Scalar::Intent::Int, op, /*isSigned=*/false)}) {
      auto intPtrTy{builder.getIntNTy(sizeof(void *) * 8)};
      return RValue(context.get_bool_type(),
                    builder.CreateCmp(*llvmOp,                               //
                                      builder.CreatePtrToInt(lhs, intPtrTy), //
                                      builder.CreatePtrToInt(rhs, intPtrTy)));
    }
  }
  // Types
  if (lhs.type == context.get_meta_type_type() && //
      rhs.type == context.get_meta_type_type()) {
    if (auto llvmOp{llvm_cmp_op(Scalar::Intent::Int, op)})
      return RValue(context.get_bool_type(),
                    builder.CreateCmp(*llvmOp, lhs, rhs));
    if (lhs.is_comptime() && rhs.is_comptime()) {
      auto lhsTy{lhs.get_comptime_meta_type(context, srcLoc)};
      auto rhsTy{rhs.get_comptime_meta_type(context, srcLoc)};
      if (op == BINOP_OR)
        return context.get_comptime_meta_type(
            context.get_union_type({lhsTy, rhsTy}));
      if (op == BINOP_SUBSET)
        return context.get_comptime_bool(
            context.is_perfectly_convertible(lhsTy, rhsTy));
    }
  }
  // Type-checking
  if (lhs.type != context.get_meta_type_type() && //
      rhs.type == context.get_meta_type_type() && op == BINOP_SUBSET) {
    if (rhs.is_comptime()) {
      auto lhsTy{lhs.type};
      auto rhsTy{rhs.get_comptime_meta_type(context, srcLoc)};
      return context.get_comptime_bool(
          context.is_perfectly_convertible(lhsTy, rhsTy));
    }
  }
  srcLoc.throw_error("unimplemented binary operator ", quoted(to_string(op)),
                     " for argument types ", quoted(lhs.type->displayName),
                     " and ", quoted(rhs.type->displayName));
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
    SMDL_LOG_WARN("cannot open ", quoted(fname), ": ", quoted(ec.message()));
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

SMDL_EXPORT void smdl_bsdf_measurement_evaluate(const void *bsdfMeasurement,
                                                const float3 &wo,
                                                const float3 &wi,
                                                float3 &result) {
  if (!bsdfMeasurement)
    result = float3(0);
  else
    result = static_cast<const BSDFMeasurement *>(bsdfMeasurement)
                 ->interpolate(wo, wi);
}

} // extern "C"

//--{ emit_intrinsic
Value Emitter::emit_intrinsic(std::string_view name, const ArgumentList &args,
                              const SourceLocation &srcLoc) {
  if (args.is_any_named())
    srcLoc.throw_error("intrinsics expect only unnamed arguments");
  auto expectOne{[&]() {
    if (args.size() != 1)
      srcLoc.throw_error("intrinsic ", quoted(name), " expects 1 argument");
    return args[0].value;
  }};
  auto expectOneVectorized{[&]() {
    if (args.size() != 1 || !args[0].value.type->is_vectorized())
      srcLoc.throw_error("intrinsic ", quoted(name),
                         " expects 1 vectorized argument");
    return args[0].value;
  }};
  auto expectOneIntOrIntVector{[&]() {
    if (args.size() != 1 ||                     //
        !args[0].value.type->is_vectorized() || //
        !args[0].value.type->is_arithmetic_integral())
      srcLoc.throw_error("intrinsic ", quoted(name),
                         " expects 1 int or int vector argument");
    return args[0].value;
  }};
  auto expectOneType{[&]() {
    auto value{to_rvalue(expectOne())};
    return value.is_comptime_meta_type(context)
               ? value.get_comptime_meta_type(context, srcLoc)
               : value.type;
  }};
  auto scalarTypeOf{[&](Type *type) -> ArithmeticType * {
    if (type->is_color())
      return static_cast<ColorType *>(type)->get_arithmetic_scalar_type(
          context);
    if (type->is_arithmetic())
      return static_cast<ArithmeticType *>(type)->get_scalar_type(context);
    SMDL_SANITY_CHECK(false);
    return nullptr;
  }};
  switch (name[0]) {
  default:
    break;
  case 'a': {
    if (name == "alignof") {
      return context.get_comptime_int(
          int(context.get_align_of(expectOneType())));
    }
    if (name == "abs") {
      auto value{to_rvalue(expectOneVectorized())};
      return RValue(
          value.type,
          value.type->is_arithmetic_integral()
              ? builder.CreateBinaryIntrinsic(llvm::Intrinsic::abs, value,
                                              context.get_comptime_bool(false))
              : builder.CreateUnaryIntrinsic(llvm::Intrinsic::fabs, value));
    }
    if (name == "any" || name == "all") {
      auto value{expectOne()};
      if (!value.type->is_arithmetic_scalar() &&
          !value.type->is_arithmetic_vector())
        srcLoc.throw_error("intrinsic ", quoted(name),
                           " expects 1 scalar or vector argument");
      value =
          invoke(static_cast<ArithmeticType *>(value.type)
                     ->get_with_different_scalar(context, Scalar::get_bool()),
                 value, srcLoc);
      if (value.type->is_arithmetic_scalar())
        return value;
      return RValue(context.get_bool_type(),
                    builder.CreateUnaryIntrinsic(
                        name == "any" ? llvm::Intrinsic::vector_reduce_or
                                      : llvm::Intrinsic::vector_reduce_and,
                        value));
    }
    if (name == "assert") {
      if (!((args.size() == 1 &&
             args[0].value.type == context.get_bool_type()) ||
            (args.size() == 2 &&
             args[0].value.type == context.get_bool_type() &&
             args[1].value.type == context.get_string_type())))
        srcLoc.throw_error("intrinsic 'assert' expects 1 bool argument and 1 "
                           "optional string argument");
      auto [blockPanic, blockOk] =
          create_blocks<2>("assert", {".panic", ".ok"});
      builder.CreateCondBr(to_rvalue(args[0].value), blockOk, blockPanic);
      builder.SetInsertPoint(blockPanic);
      handle_scope(nullptr, nullptr, [&] {
        if (args.size() == 1) {
          std::string message{"assertion failed"};
          if (!args[0].get_source().empty()) {
            message += ": ";
            message += args[0].get_source();
          }
          emit_panic(context.get_comptime_string(message), srcLoc);
        } else {
          emit_panic(to_rvalue(args[1].value), srcLoc);
        }
      });
      builder.CreateBr(blockOk);
      builder.SetInsertPoint(blockOk);
      return Value();
    }
    if (name == "atan2") {
      if (!(args.size() == 2 &&                    //
            args[0].value.type->is_vectorized() && //
            args[1].value.type->is_vectorized()))
        srcLoc.throw_error("intrinsic 'atan2' expects 2 vectorized arguments");
      auto commonType{context.get_common_type(
          {args[0].value.type, args[1].value.type, context.get_float_type()},
          /*defaultToUnion=*/false, srcLoc)};
      SMDL_SANITY_CHECK(commonType->is_arithmetic_floating_point());
      auto value0{invoke(commonType, args[0].value, srcLoc)};
      auto value1{invoke(commonType, args[1].value, srcLoc)};
      return RValue(commonType, builder.CreateBinaryIntrinsic(
                                    llvm::Intrinsic::atan2, value0, value1));
    }
    if (name == "albedo_lut") {
      if (!(args.size() == 1 && args[0].value.is_comptime_string()))
        srcLoc.throw_error(
            "intrinsic 'albedo_lut' expects 1 compile-time string argument");
      auto lutName{args[0].value.get_comptime_string()};
      auto lutType{context.get_keyword_value("$albedo_lut")
                       .get_comptime_meta_type(context, srcLoc)};
      auto lut{context.get_builtin_albedo_lut(lutName)};
      if (!lut)
        srcLoc.throw_error(
            "intrinsic 'albedo_lut' passed invalid name ", quoted(lutName),
            " that does not identify any known look-up table at compile time");
      auto args{ArgumentList{}};
      args.push_back(Argument{"num_cos_theta",
                              context.get_comptime_int(lut->num_cos_theta)});
      args.push_back(Argument{"num_roughness",
                              context.get_comptime_int(lut->num_roughness)});
      args.push_back(
          Argument{"directional_albedo",
                   context.get_comptime_ptr(
                       context.get_pointer_type(context.get_float_type()),
                       lut->directional_albedo)});
      args.push_back(
          Argument{"average_albedo",
                   context.get_comptime_ptr(
                       context.get_pointer_type(context.get_float_type()),
                       lut->average_albedo)});
      return invoke(lutType, args, srcLoc);
    }
    break;
  }
  case 'b': {
    if (name == "bitreverse") {
      auto value{to_rvalue(expectOneIntOrIntVector())};
      return RValue(value.type, builder.CreateUnaryIntrinsic(
                                    llvm::Intrinsic::bitreverse, value));
    }
    if (name == "breakpoint") {
      if (!args.empty())
        srcLoc.throw_error("intrinsic 'breakpoint' expects no arguments");
      builder.CreateIntrinsic(context.get_void_type()->llvmType,
                              llvm::Intrinsic::debugtrap, {});
      return RValue(context.get_void_type(), nullptr);
    }
    if (name == "bump_allocate") {
      auto value{to_rvalue(expectOne())};
      auto callee{context.get_builtin_callee("smdl_bump_allocate",
                                             &smdl_bump_allocate)};
      if (auto func{llvm::dyn_cast<llvm::Function>(callee.getCallee())})
        func->setReturnDoesNotAlias();
      auto callInst{builder.CreateCall(
          callee,
          {state.llvmValue,
           context.get_comptime_int(int(context.get_size_of(value.type)))
               .llvmValue,
           context.get_comptime_int(int(context.get_align_of(value.type)))
               .llvmValue})};
      builder.CreateStore(value, callInst);
      return RValue(context.get_pointer_type(value.type), callInst);
    }
    break;
  }
  case 'c': {
    if (name == "ctlz") {
      auto value{to_rvalue(expectOneIntOrIntVector())};
      return RValue(value.type, builder.CreateBinaryIntrinsic(
                                    llvm::Intrinsic::ctlz, value,
                                    context.get_comptime_bool(false)));
    }
    if (name == "cttz") {
      auto value{to_rvalue(expectOneIntOrIntVector())};
      return RValue(value.type, builder.CreateBinaryIntrinsic(
                                    llvm::Intrinsic::cttz, value,
                                    context.get_comptime_bool(false)));
    }
    if (name == "ctpop") {
      auto value{to_rvalue(expectOneIntOrIntVector())};
      return RValue(value.type, builder.CreateUnaryIntrinsic(
                                    llvm::Intrinsic::ctpop, value));
    }
    break;
  }
  case 'i': {
    if (name == "isfpclass") {
      if (!(args.size() == 2 &&                                    //
            (args[0].value.type->is_arithmetic_floating_point() || //
             args[0].value.type->is_color()) &&                    //
            args[1].value.is_comptime_int()))
        srcLoc.throw_error(
            "intrinsic 'isfpclass' expects 1 vectorized floating point "
            "argument and 1 compile-time int argument");
      auto value{to_rvalue(args[0].value)};
      return RValue(
          static_cast<ArithmeticType *>(value.type)
              ->get_with_different_scalar(context, Scalar::get_bool()),
          builder.createIsFPClass(value, args[1].value.get_comptime_int()));
    }
    if (starts_with(name, "is_")) {
      auto type{expectOneType()};
      auto result{std::optional<bool>{}};
      if (name == "is_array") {
        result = type->is_array();
      } else if (name == "is_arithmetic") {
        result = type->is_arithmetic();
      } else if (name == "is_arithmetic_integral") {
        result = type->is_arithmetic_integral();
      } else if (name == "is_arithmetic_floating_point") {
        result = type->is_arithmetic_floating_point();
      } else if (name == "is_arithmetic_scalar") {
        result = type->is_arithmetic_scalar();
      } else if (name == "is_arithmetic_vector") {
        result = type->is_arithmetic_vector();
      } else if (name == "is_arithmetic_matrix") {
        result = type->is_arithmetic_matrix();
      } else if (name == "is_enum") {
        result = type->is_enum();
      } else if (name == "is_optional_union") {
        result = type->is_optional_union();
      } else if (name == "is_pointer") {
        result = type->is_pointer();
      } else if (name == "is_struct") {
        result = type->is_struct();
      } else if (name == "is_tag") {
        result = type->is_tag();
      } else if (name == "is_union") {
        result = type->is_union();
      }
      if (result) {
        return context.get_comptime_bool(*result);
      }
    }
    break;
  }
  case 'l': {
    if (name == "load_bsdf_measurement") {
      auto fileName{expectOne()};
      if (!fileName.is_comptime_string()) {
        srcLoc.throw_error("intrinsic 'load_bsdf_measurement' expects 1 "
                           "compile-time string argument");
      }
      return context.get_comptime_ptr(
          context.get_void_pointer_type(),
          context.compiler.load_bsdf_measurement(
              std::string(fileName.get_comptime_string()), srcLoc));
    }
    if (name == "load_ptexture") {
      auto fileName{expectOne()};
      if (!fileName.is_comptime_string()) {
        srcLoc.throw_error("intrinsic 'load_ptexture' expects 1 "
                           "compile-time string argument");
      }
      return context.get_comptime_ptr(
          context.get_void_pointer_type(),
          context.compiler.load_ptexture(
              std::string(fileName.get_comptime_string()), srcLoc));
    }
    break;
  }
  case 'm': {
    if (name == "memcpy") {
      if (!(args.size() == 3 &&                             //
            args[0].value.type->is_pointer() &&             //
            args[1].value.type->is_pointer() &&             //
            args[2].value.type->is_arithmetic_integral() && //
            args[2].value.type->is_arithmetic_scalar()))
        srcLoc.throw_error("intrinsic 'memcpy' expects 2 pointer arguments "
                           "and 1 int argument");
      auto dst{to_rvalue(args[0].value)};
      auto src{to_rvalue(args[1].value)};
      builder.CreateMemCpy(dst, std::nullopt, src, std::nullopt,
                           llvm_emit_cast(builder, to_rvalue(args[2].value),
                                          llvm::Type::getInt64Ty(context)));
      return Value();
    }
    if (name == "max" || name == "min") {
      if (args.size() != 2 || !args.is_all_true([](auto &arg) {
            return arg.value.type->is_vectorized();
          }))
        srcLoc.throw_error("intrinsic ", quoted(name),
                           " expects 2 vectorized arguments");
      auto value0{args[0].value};
      auto value1{args[1].value};
      auto type{context.get_common_type({value0.type, value1.type},
                                        /*defaultToUnion=*/false, srcLoc)};
      value0 = invoke(type, value0, srcLoc);
      value1 = invoke(type, value1, srcLoc);
      auto intrID =
          name == "max"
              ? (type->is_arithmetic_boolean()    ? llvm::Intrinsic::umax
                 : type->is_arithmetic_integral() ? llvm::Intrinsic::smax
                                                  : llvm::Intrinsic::maxnum)
              : (type->is_arithmetic_boolean()    ? llvm::Intrinsic::umin
                 : type->is_arithmetic_integral() ? llvm::Intrinsic::smin
                                                  : llvm::Intrinsic::minnum);
      return RValue(type,
                    builder.CreateBinaryIntrinsic(intrID, value0, value1));
    }
    if (name == "max_value" || name == "min_value") {
      auto value{to_rvalue(expectOneVectorized())};
      if (value.type->is_arithmetic_scalar())
        return value;
      auto intrID = name == "max_value"
                        ? (value.type->is_arithmetic_boolean()
                               ? llvm::Intrinsic::vector_reduce_umax
                           : value.type->is_arithmetic_integral()
                               ? llvm::Intrinsic::vector_reduce_smax
                               : llvm::Intrinsic::vector_reduce_fmax)
                        : (value.type->is_arithmetic_boolean()
                               ? llvm::Intrinsic::vector_reduce_umin
                           : value.type->is_arithmetic_integral()
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
        if (type->is_arithmetic_vector()) {
          return static_cast<ArithmeticType *>(type)->extent.numRows;
        } else if (type->is_arithmetic_matrix()) {
          return static_cast<ArithmeticType *>(type)->extent.numCols;
        } else if (type->is_color()) {
          return static_cast<ColorType *>(type)->wavelengthBaseMax;
        } else if (type->is_array()) {
          return static_cast<ArrayType *>(type)->size;
        } else {
          return 1;
        }
      }()};
      return context.get_comptime_int(size);
    }
    if (name == "num_rows" || name == "num_cols") {
      auto type{expectOneType()};
      if (!type->is_arithmetic_matrix())
        srcLoc.throw_error("intrinsic ", quoted(name),
                           " expects 1 matrix argument");
      return context.get_comptime_int(
          name == "num_rows"
              ? int(static_cast<ArithmeticType *>(type)->extent.numRows)
              : int(static_cast<ArithmeticType *>(type)->extent.numCols));
    }
    break;
  }
  case 'o': {
    if (name == "ofile_open") {
      auto value{to_rvalue(expectOne())};
      return RValue(context.get_void_pointer_type(),
                    builder.CreateCall(context.get_builtin_callee(
                                           "smdl_ofile_open", &smdl_ofile_open),
                                       {value.llvmValue}));
    }
    if (name == "ofile_close") {
      auto value{to_rvalue(expectOne())};
      builder.CreateCall(
          context.get_builtin_callee("smdl_ofile_close", &smdl_ofile_close),
          {value.llvmValue});
      return Value();
    }
    if (name == "ofile_print" || name == "ofile_println") {
      if (args.size() < 2)
        srcLoc.throw_error("intrinsic 'ofile_print' expects 1 file pointer "
                           "argument and 1 or more printable arguments");
      auto os{to_rvalue(args[0].value)};
      for (size_t i = 1; i < args.size(); i++)
        emit_print(os, args[i].value, srcLoc);
      if (name == "ofile_println")
        emit_print(os, context.get_comptime_string("\n"), srcLoc);
      return {};
    }
    break;
  }
  case 'p': {
    if (name == "panic") {
      if (args.size() != 1 || args[0].value.type != context.get_string_type())
        srcLoc.throw_error("intrinsic 'panic' expects 1 string argument");
      emit_panic(args[0].value, srcLoc);
      return Value();
    }
    if (name == "pow") {
      if (args.size() != 2 ||                     //
          !args[0].value.type->is_vectorized() || //
          !args[1].value.type->is_vectorized())
        srcLoc.throw_error("intrinsic 'pow' expects 2 vectorized arguments");
      auto value0{to_rvalue(args[0].value)};
      auto value1{to_rvalue(args[1].value)};
      if (value1.is_comptime_int()) {
        auto power{value1.get_comptime_int()};
        if (power == 1)
          return value0;
        if (power == 2)
          return emit_op(BINOP_MUL, value0, value0, srcLoc);
      }
      auto resultType{context.get_common_type(
          {value0.type, value1.type, context.get_float_type()},
          /*defaultToUnion=*/false, srcLoc)};
      value0 = invoke(resultType, value0, srcLoc);
      return RValue(resultType, value1.type->is_arithmetic_scalar_int()
                                    ? llvm_emit_powi(builder, value0, value1)
                                    : builder.CreateBinaryIntrinsic(
                                          llvm::Intrinsic::pow, value0,
                                          invoke(resultType, value1, srcLoc)));
    }
    if (name == "print" || name == "println") {
      auto os{context.get_comptime_ptr(context.get_void_pointer_type(),
                                       &llvm::errs())};
      for (auto &arg : args)
        emit_print(os, arg.value, srcLoc);
      if (name == "println")
        emit_print(os, context.get_comptime_string("\n"), srcLoc);
      return Value();
    }
    if (name == "prod") {
      auto value{to_rvalue(expectOneVectorized())};
      if (value.type->is_arithmetic_scalar())
        return value;
      auto scalarType{scalarTypeOf(value.type)};
      return RValue(
          scalarType,
          scalarType->is_arithmetic_integral()
              ? builder.CreateMulReduce(value)
              : builder.CreateFMulReduce(
                    invoke(scalarType, context.get_comptime_float(1), srcLoc),
                    value));
    }
    break;
  }
  case 'r': {
    if (name == "rotl" || name == "rotr") {
      if (!(args.size() == 2 && //
            args[0].value.type->is_arithmetic_integral() &&
            args[1].value.type->is_arithmetic_integral())) {
        srcLoc.throw_error(
            "intrinsic ", quoted(name),
            " expects 2 integer or vectorized integer arguments");
      }
      auto intType{
          context.get_common_type({args[0].value.type, args[1].value.type},
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
      return context.get_comptime_int(
          int(context.get_size_of(expectOneType())));
    }
    if (name == "sum") {
      auto value{to_rvalue(expectOneVectorized())};
      if (value.type->is_arithmetic_scalar())
        return value;
      auto scalarType{scalarTypeOf(value.type)};
      return RValue(scalarType, scalarType->is_arithmetic_integral()
                                    ? builder.CreateAddReduce(value)
                                    : builder.CreateFAddReduce(
                                          Value::zero(scalarType), value));
    }
    if (name == "sign") {
      auto value{to_rvalue(expectOneVectorized())};
      if (value.type->is_arithmetic_integral()) {
        return RValue(
            value.type,
            builder.CreateSelect(
                emit_op(BINOP_CMP_LT, value, context.get_comptime_int(0),
                        srcLoc),
                invoke(value.type, context.get_comptime_int(-1), srcLoc),
                invoke(value.type, context.get_comptime_int(+1), srcLoc)));
      } else {
        return RValue(
            value.type,
            builder.CreateBinaryIntrinsic(
                llvm::Intrinsic::copysign,
                invoke(value.type, context.get_comptime_float(1), srcLoc),
                value));
      }
    }
    if (name == "select") {
      if (args.size() != 3 || !args[0].value.type->is_arithmetic_boolean() ||
          !args.is_all_true(
              [](auto arg) { return arg.value.type->is_vectorized(); }))
        srcLoc.throw_error("intrinsic 'select' expects 1 vectorized boolean "
                           "argument and 2 vectorized selection arguments");
      auto valueCond{to_rvalue(args[0].value)};
      auto valueThen{args[1].value};
      auto valueElse{args[2].value};
      auto type{context.get_common_type(
          {valueThen.type, valueElse.type,
           valueCond.type->is_arithmetic_vector() ? valueCond.type : nullptr},
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
      if (!(args.size() == 4 &&
            args[0].value.type == context.get_string_type() &&
            args[1].value.type->is_arithmetic_scalar_int() &&
            args[2].value.type->is_arithmetic_scalar_int() &&
            args[3].value.is_comptime_meta_type(context)))
        srcLoc.throw_error("intrinsic 'tabulate_albedo' expects 1 compile-time "
                           "string argument, 2 compile-time integer arguments, "
                           "and 1 function argument");
      auto funcType{llvm::dyn_cast<FunctionType>(
          args[3].value.get_comptime_meta_type(context, srcLoc))};
      if (!(funcType &&                //
            funcType->is_pure() &&     //
            !funcType->is_macro() &&   //
            !funcType->is_variant() && //
            funcType->has_no_overloads() &&
            funcType->returnType == context.get_float_type() &&
            funcType->params.size() == 2 &&
            funcType->params[0].type == context.get_float_type() &&
            funcType->params[1].type == context.get_float_type())) {
        srcLoc.throw_error("intrinsic 'tabulate_albedo' function argument must "
                           "have signature '@(pure) float(float, float)'");
      }
      auto &funcInst{funcType->instantiate(
          *this, {context.get_float_type(), context.get_float_type()})};
      auto callee{context.get_builtin_callee("smdl_tabulate_albedo",
                                             &smdl_tabulate_albedo)};
      auto callInst{
          builder.CreateCall(callee, {to_rvalue(args[0].value).llvmValue, //
                                      to_rvalue(args[1].value).llvmValue, //
                                      to_rvalue(args[2].value).llvmValue, //
                                      funcInst.llvmFunc})};
      return RValue(context.get_void_type(), callInst);
    }
    if (name == "typeof") {
      return context.get_comptime_meta_type(expectOne().type);
    }
    if (name == "typename") {
      return context.get_comptime_string(expectOneType()->displayName);
    }
    if (name == "type_int") {
      auto value{to_rvalue(expectOne())};
      if (!value.is_comptime_int())
        srcLoc.throw_error("intrinsic 'type_int' expects 1 compile-time int");
      return context.get_comptime_meta_type(context.get_arithmetic_type(
          Scalar::get_int(value.get_comptime_int()), Extent(1)));
    }
    if (name == "type_float") {
      auto value{to_rvalue(expectOne())};
      if (!value.is_comptime_int())
        srcLoc.throw_error("intrinsic 'type_float' expects 1 compile-time int");
      return context.get_comptime_meta_type(context.get_arithmetic_type(
          Scalar::get_FP(value.get_comptime_int()), Extent(1)));
    }
    if (name == "type_vector") {
      auto reportError{[&] {
        srcLoc.throw_error("intrinsic 'type_vector' expects 1 compile-time "
                           "scalar type and 1 compile-time positive int");
      }};
      if (!(args.size() == 2 &&                             //
            args[0].value.is_comptime_meta_type(context) && //
            args[1].value.is_comptime_int()))
        reportError();
      auto type{args[0].value.get_comptime_meta_type(context, srcLoc)};
      auto size{args[1].value.get_comptime_int()};
      if (!type->is_arithmetic_scalar() || size < 1)
        reportError();
      return context.get_comptime_meta_type(context.get_arithmetic_type(
          static_cast<ArithmeticType *>(type)->scalar, Extent(size)));
    }
    if (name == "type_matrix") {
      auto reportError{[&] {
        srcLoc.throw_error("intrinsic 'type_matrix' expects 1 compile-time "
                           "scalar type and 2 compile-time positive ints");
      }};
      if (!(args.size() == 3 &&                             //
            args[0].value.is_comptime_meta_type(context) && //
            args[1].value.is_comptime_int() &&              //
            args[2].value.is_comptime_int()))
        reportError();
      auto type{args[0].value.get_comptime_meta_type(context, srcLoc)};
      auto numCols{args[1].value.get_comptime_int()};
      auto numRows{args[2].value.get_comptime_int()};
      if (!type->is_arithmetic_scalar() || numCols < 1 || numRows < 1)
        reportError();
      return context.get_comptime_meta_type(context.get_arithmetic_type(
          static_cast<ArithmeticType *>(type)->scalar,
          Extent(numCols, numRows)));
    }
    if (name == "transpose") {
      if (!(args.size() == 1 && args[0].value.type->is_arithmetic_matrix()))
        srcLoc.throw_error("intrinsic 'transpose' expects 1 matrix argument");
      auto value{args[0].value};
      auto valueCols{llvm::SmallVector<Value>{}};
      for (unsigned j = 0;
           j < static_cast<ArithmeticType *>(value.type)->extent.numCols; j++)
        valueCols.push_back(to_rvalue(access_index(value, j, srcLoc)));
      auto resultType{static_cast<ArithmeticType *>(value.type)
                          ->get_transpose_type(context)};
      auto result{Value::zero(resultType)};
      for (unsigned j = 0; j < resultType->extent.numCols; j++) {
        auto resultCol{Value::zero(resultType->get_column_type(context))};
        for (unsigned i = 0; i < resultType->extent.numRows; i++)
          resultCol = insert(resultCol, access_index(valueCols[i], j, srcLoc),
                             i, srcLoc);
        result = insert(result, resultCol, j, srcLoc);
      }
      return result;
    }
    break;
  }
  case 'u': {
    if (name == "unsigned_to_fp") {
      if (!(args.size() == 2 && args[0].value.type->is_arithmetic_integral() &&
            args[1].value.is_comptime_meta_type(context))) {
        srcLoc.throw_error(
            "intrinsic 'unsigned_to_fp' expects 1 int argument and "
            "1 type argument");
      }
      auto floatType{args[1].value.get_comptime_meta_type(context, srcLoc)};
      if (!floatType->is_arithmetic_floating_point())
        srcLoc.throw_error(
            "intrinsic 'unsigned_to_fp' expects 1 int argument and "
            "1 floating point type argument");
      return RValue(floatType, builder.CreateUIToFP(to_rvalue(args[0].value),
                                                    floatType->llvmType));
    }
    if (name == "unpack_float4") {
      auto value{to_rvalue(expectOne())};
      if (!value.type->is_arithmetic_scalar() &&
          !value.type->is_arithmetic_vector())
        srcLoc.throw_error(
            "intrinsic 'unpack_float4' expects 1 scalar or vector argument");
      auto texelType{static_cast<ArithmeticType *>(value.type)};
      value.type =
          texelType->get_with_different_scalar(context, Scalar::get_float());
      value.llvmValue =
          texelType->is_arithmetic_integral()
              ? builder.CreateUIToFP(value.llvmValue, value.type->llvmType)
              : builder.CreateFPCast(value.llvmValue, value.type->llvmType);
      if (texelType->is_arithmetic_integral())
        value =
            emit_op(BINOP_MUL, value,
                    context.get_comptime_float(
                        1.0f / float((1ULL << texelType->scalar.numBits) - 1)),
                    srcLoc);
      if (value.type == context.get_float_type(Extent(4))) // Early out?
        return value;
      auto result{Value::zero(context.get_float_type(Extent(4)))};
      if (value.type->is_arithmetic_scalar()) {
        result.llvmValue = builder.CreateVectorSplat(4, value.llvmValue);
      } else {
        for (uint64_t i = 0; i < texelType->extent.numRows; i++)
          result.llvmValue = builder.CreateInsertElement(
              result.llvmValue,
              builder.CreateExtractElement(value.llvmValue, i), i);
      }
      result.llvmValue = builder.CreateInsertElement(
          result.llvmValue, context.get_comptime_float(1).llvmValue,
          uint64_t(3));
      return result;
    }
    break;
  }
  }
  // Unary floating-point math intrinsics
  if (name.size() <= 5) {
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
        auto value{to_rvalue(expectOneVectorized())};
        if (value.type->is_arithmetic_integral())
          value = invoke(
              static_cast<ArithmeticType *>(value.type)
                  ->get_with_different_scalar(context, Scalar::get_float()),
              value, srcLoc);
        return RValue(value.type, builder.CreateUnaryIntrinsic(intrID, value));
      }
    }
  }
  srcLoc.throw_error("unimplemented intrinsic ", quoted(name));
  return Value();
}
//--}

Value Emitter::emit_call(Value callee, const ArgumentList &args,
                         const SourceLocation &srcLoc) {
  if (args.is_any_visited()) {
    auto visitedIndex{args.index_of_first_visited()};
    return emit_visit(args[visitedIndex].value, srcLoc, [&](Value value) {
      auto visitedArgs{args};
      visitedArgs[visitedIndex].value = value;
      return emit_call(callee, visitedArgs, srcLoc);
    });
  } else {
    if (callee.is_comptime_meta_type(context)) {
      return invoke(callee.get_comptime_meta_type(context, srcLoc), args,
                    srcLoc);
    }
    if (callee.is_comptime_meta_intrinsic(context)) {
      return emit_intrinsic(callee.get_comptime_meta_intrinsic(context, srcLoc)
                                ->srcName.substr(1),
                            args, srcLoc);
    }
  }
  srcLoc.throw_error("unimplemented or invalid call");
  return Value();
}

Value Emitter::emit_visit(Value value, const SourceLocation &srcLoc,
                          const std::function<Value(Value)> &visitor) {
  if (!value.type->is_union_or_pointer_to_union()) {
    Value result{};
    handle_scope(nullptr, nullptr, [&]() { result = visitor(value); });
    return result;
  } else {
    auto results{llvm::SmallVector<Result>{}};
    auto unionType{
        static_cast<UnionType *>(value.type->get_first_non_pointer_type())};
    auto ptr{value.type->is_union() ? to_rvalue(access_field(value, "#ptr", {}))
                                    : Value()};
    auto visitName{context.get_unique_name("visit", get_llvm_function())};
    auto visitNameRef{llvm::StringRef(visitName)};
    auto blockUnreachable{create_block(visitNameRef + ".unreachable")};
    auto blockEnd{create_block(visitNameRef + ".end")};
    auto switchInst{builder.CreateSwitch(
        to_rvalue(access_field(value, "#idx", {})), blockUnreachable)};
    for (size_t i = 0; i < unionType->caseTypes.size(); i++) {
      auto blockCase{create_block(concat(visitName, ".case.", i))};
      switchInst->addCase(builder.getInt32(i), blockCase);
      handle_scope(blockCase, blockEnd, [&] {
        auto caseType{unionType->caseTypes[i]};
        auto caseValue{Value()};
        if (value.type->is_union()) {
          caseValue = LValue(caseType, ptr);
          caseValue = value.is_rvalue() ? to_rvalue(caseValue) : caseValue;
        } else {
          caseValue = value;
          caseValue.type = context.get_pointer_type(
              caseType, value.type->get_first_non_pointer_type_depth());
        }
        caseValue = visitor(caseValue);
        results.push_back({caseValue, get_insert_block(), {}});
      });
    }
    llvm_move_block_to_end(blockUnreachable);
    builder.SetInsertPoint(blockUnreachable);
    builder.CreateUnreachable();
    handle_block_end(blockEnd);
    return create_result(context.get_auto_type(), results, srcLoc);
  }
}

extern "C" {

SMDL_EXPORT void smdl_panic(const char *message) {
  throw Error(std::string(message));
}

} // extern "C"

void Emitter::emit_panic(Value message, const SourceLocation &srcLoc) {
  SMDL_SANITY_CHECK(message);
  SMDL_SANITY_CHECK(message.type == context.get_string_type());
  message = to_rvalue(message);
  auto callInst{
      builder.CreateCall(context.get_builtin_callee("smdl_panic", &smdl_panic),
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

void Emitter::emit_print(Value os, Value value, const SourceLocation &srcLoc,
                         bool quoteStrings) {
  SMDL_SANITY_CHECK(os.type->is_pointer());
  os = to_rvalue(os);
  if (value.is_comptime_meta_type(context)) {
    emit_print(os, value.get_comptime_meta_type(context, srcLoc)->displayName,
               srcLoc);
  } else if (value.is_void()) {
    emit_print(os, "null", srcLoc);
  } else if (value.type->is_pointer()) {
    builder.CreateCall(
        context.get_builtin_callee("smdl_print_pointer", &smdl_print_pointer),
        {os.llvmValue, to_rvalue(value).llvmValue});
  } else if (value.type->is_string()) {
    auto call{quoteStrings
                  ? context.get_builtin_callee("smdl_print_quoted_string",
                                               &smdl_print_quoted_string)
                  : context.get_builtin_callee("smdl_print_string",
                                               &smdl_print_string)};
    builder.CreateCall(call, {os.llvmValue, to_rvalue(value).llvmValue});
  } else if (value.type->is_enum()) {
    emit_print(os, invoke(context.get_string_type(), value, srcLoc), srcLoc);
  } else if (auto arithType{llvm::dyn_cast<ArithmeticType>(value.type)}) {
    if (arithType->extent.is_scalar()) {
      auto type{static_cast<Type *>(nullptr)};
      auto call{llvm::FunctionCallee()};
      if (arithType->scalar.is_boolean()) {
        type = context.get_int_type();
        call = context.get_builtin_callee("smdl_print_bool", &smdl_print_bool);
      } else if (arithType->scalar.is_integral()) {
        type = context.get_arithmetic_type(Scalar::get_int(64), Extent(1));
        call = context.get_builtin_callee("smdl_print_int", &smdl_print_int);
      } else {
        type = arithType->scalar.numBits < 64
                   ? context.get_arithmetic_type(Scalar::get_FP(32), Extent(1))
                   : context.get_arithmetic_type(Scalar::get_FP(64), Extent(1));
        call = arithType->scalar.numBits < 64
                   ? context.get_builtin_callee("smdl_print_float",
                                                &smdl_print_float)
                   : context.get_builtin_callee("smdl_print_double",
                                                &smdl_print_double);
      }
      builder.CreateCall(call,
                         {os.llvmValue, invoke(type, value, srcLoc).llvmValue});
    } else if (arithType->extent.is_vector()) {
      emit_print(os, "<", srcLoc);
      for (uint32_t i = 0; i < arithType->extent.numRows; i++) {
        emit_print(os, access_index(value, i, srcLoc), srcLoc);
        if (i + 1 < arithType->extent.numRows) {
          emit_print(os, ", ", srcLoc);
        }
      }
      emit_print(os, ">", srcLoc);
    } else if (arithType->extent.is_matrix()) {
      emit_print(os, "[", srcLoc);
      for (uint32_t i = 0; i < arithType->extent.numCols; i++) {
        emit_print(os, access_index(value, i, srcLoc), srcLoc);
        if (i + 1 < arithType->extent.numCols) {
          emit_print(os, ", ", srcLoc);
        }
      }
      emit_print(os, "]", srcLoc);
    }
  } else if (auto arrayType{llvm::dyn_cast<ArrayType>(value.type)}) {
    emit_print(os, "[", srcLoc);
    for (uint32_t i = 0; i < arrayType->size; i++) {
      emit_print(os, access_index(value, i, srcLoc), srcLoc,
                 /*quoteStrings=*/true);
      if (i + 1 < arrayType->size) {
        emit_print(os, ", ", srcLoc);
      }
    }
    emit_print(os, "]", srcLoc);
  } else if (auto colorType{llvm::dyn_cast<ColorType>(value.type)}) {
    emit_print(os, "<", srcLoc);
    for (uint32_t i = 0; i < colorType->wavelengthBaseMax; i++) {
      emit_print(os, access_index(value, i, srcLoc), srcLoc);
      if (i + 1 < colorType->wavelengthBaseMax) {
        emit_print(os, ", ", srcLoc);
      }
    }
    emit_print(os, ">", srcLoc);
  } else if (auto structType{llvm::dyn_cast<StructType>(value.type)}) {
    emit_print(os, value.type->displayName + "(", srcLoc);
    for (uint32_t i = 0; i < structType->params.size(); i++) {
      auto paramName{std::string(structType->params[i].name)};
      emit_print(os, paramName + ": ", srcLoc);
      emit_print(os, access_field(value, paramName, srcLoc), srcLoc,
                 /*quoteStrings=*/true);
      if (i + 1 < structType->params.size()) {
        emit_print(os, ", ", srcLoc);
      }
    }
    emit_print(os, ")", srcLoc);
  } else if (value.type->is_union()) {
    emit_visit(value, srcLoc, [&](Value value) {
      emit_print(os, value, srcLoc, quoteStrings);
      return Value();
    });
  }
}

Value Emitter::resolve_identifier(Span<std::string_view> names,
                                  const SourceLocation &srcLoc,
                                  bool voidByDefault) {
  SMDL_SANITY_CHECK(!names.empty());
  if (auto crumb0{Crumb::find(context, names, get_llvm_function(), crumb)}) {
    return crumb0->value;
  }
  if (names.size() == 1) {
    if (names[0] == "$state") {
      if (!state)
        srcLoc.throw_error(
            "cannot resolve identifier '$state' in pure context");
      return state;
    } else if (names[0] == "$scene_data") {
      return context.get_comptime_ptr(context.get_void_pointer_type(),
                                      &context.compiler.sceneData);
    } else if (names[0] == "void") {
      // TODO Only if in extended syntax mode?
      return context.get_comptime_meta_type(context.get_void_type());
    } else if (names[0] == "null") {
      // TODO Only if in extended syntax mode?
      return RValue(context.get_void_type(), nullptr);
    } else if (auto value{context.get_keyword_value(names[0])}) {
      return value;
    }
  }
  if (voidByDefault) {
    return RValue(context.get_void_type(), nullptr);
  }
  srcLoc.throw_error("cannot resolve identifier ", quoted(join(names, "::")));
  return Value();
}

Emitter::ResolvedArguments
Emitter::resolve_arguments(const ParameterList &params,
                           const ArgumentList &args,
                           const SourceLocation &srcLoc, bool dontEmit) {
  // Obvious case: If there are more arguments than parameters, resolution
  // fails.
  if (args.size() > params.size()) {
    srcLoc.throw_error("too many arguments");
  }
  // Obvious case: If there are argument names that do not correspond to any
  // parameter names, resolution fails.
  if (!args.is_only_these_names(params.get_names())) {
    srcLoc.throw_error("invalid argument name(s)");
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
            srcLoc.throw_error("named argument ", quoted(arg.name),
                               " already resolved by positional argument");
          resolved.argParams[argIndex] = &param;
          return &arg;
        }
      }
      // 2. If there is no named argument, default to the next positional
      // argument.
      for (size_t argIndex = 0; argIndex < args.size(); argIndex++) {
        if (auto &arg{args[argIndex]};
            arg.is_positional() && !isResolved(argIndex)) {
          resolved.argParams[argIndex] = &param;
          return &arg;
        }
      }
      return nullptr;
    }()};
    if (arg) {
      resolved.values[paramIndex] = arg->value;
      if (!context.is_implicitly_convertible(arg->value.type, param.type))
        srcLoc.throw_error("argument type ",
                           quoted(arg->value.type->displayName),
                           " is not implicitly convertible to type ",
                           quoted(param.type->displayName), " of parameter ",
                           quoted(param.name));
    } else if (!param.get_ast_initializer() && !param.builtinDefaultValue) {
      srcLoc.throw_error("missing argument for parameter ", quoted(param.name),
                         " without default initializer");
    }
  }
  // At this point, every argument should be resolved.
  for (size_t argIndex = 0; argIndex < args.size(); argIndex++) {
    SMDL_SANITY_CHECK(isResolved(argIndex));
  }
  if (!dontEmit) {
    auto preserve{Preserve(crumb)};
    crumb = params.lastCrumb;
    handle_scope(nullptr, nullptr, [&] {
      for (size_t paramIndex = 0; paramIndex < params.size(); paramIndex++) {
        auto &param{params[paramIndex]};
        auto &value{resolved.values[paramIndex]};
        if (!value) {
          if (auto expr{param.get_ast_initializer()})
            value = emit(expr);
          else
            value = *param.builtinDefaultValue;
        }
        value = invoke(param.type, value, param.get_source_location());
        declare_crumb(param.name, /*node=*/nullptr, value);
      }
    });
  }
  return resolved;
}

Module *Emitter::resolve_module(Span<std::string_view> importPath, bool isAbs,
                                Module *thisModule) {
  // First resolve using aliases in the import path. That is, make
  // sense of declarations like this:
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // using menu = "coffee shop"::drinks;
  // import menu::latte::*;
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // In this case the import path is `menu::latte` and `menu` is an alias for
  // `"coffee shop"::drinks` so that the final import path is actually
  // `"coffee shop"::drinks::latte`.
  auto finalImportPath{llvm::SmallVector<std::string_view>{}};
  resolve_import_using_aliases(crumb, importPath, finalImportPath);
  SMDL_SANITY_CHECK(!finalImportPath.empty());

  // If the import path is absolute and is a single name, we prioritize
  // builtins.
  if (isAbs && finalImportPath.size() == 1) {
    if (auto mod{context.get_builtin_module(finalImportPath[0])}) {
      return mod;
    }
  }

  if (!thisModule->is_builtin()) {
    // Add the elements of the import path to the directory path of the
    // current module and lexically normalize. If the current module
    // file is '/path/to/package/current.mdl', and we are resolving
    // this import:
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // import .::..::menu::latte::*;
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // the filesystem directory path we end up with is
    // '/path/to/package/./../coffee shop/drinks', which lexically normalizes to
    // '/path/to/coffee shop/drinks'.
    auto thisDir{fs_make_path(thisModule->get_file_name()).parent_path()};
    for (size_t i = 0; i + 1 < finalImportPath.size(); i++) // Next-to-last
      thisDir /= fs_make_path(finalImportPath[i]);
    thisDir = thisDir.lexically_normal();
    // Now look for the module with the expected name that lives in
    // the expected directory.
    for (auto &otherModule : context.compiler.modules) {
      if (otherModule.get() != thisModule && !otherModule->is_builtin() &&
          otherModule->get_name() == finalImportPath.back()) {
        auto otherDir{fs_make_path(otherModule->get_file_name()).parent_path()};
        auto ec{fs_error_code()};
        if (fs::equivalent(thisDir, otherDir, ec)) {
          if (auto error{otherModule->compile(context)})
            throw std::move(*error);
          return otherModule.get();
        }
      }
    }
  }

  if (finalImportPath.size() == 1) {
    if (auto mod{context.get_builtin_module(finalImportPath[0])}) {
      return mod;
    }
  }
  return nullptr;
}

void Emitter::resolve_import_using_aliases(
    Crumb *crumbStart, Span<std::string_view> importPath,
    llvm::SmallVector<std::string_view> &finalImportPath) {
  for (auto &importPathElem : importPath) {
    auto crumbItr{crumbStart};
    for (; crumbItr; crumbItr = crumbItr->prev) {
      if (auto decl{llvm::dyn_cast_if_present<AST::UsingAlias>(crumbItr->node)};
          decl && decl->name.srcName == importPathElem) {
        break;
      }
    }
    if (crumbItr) {
      resolve_import_using_aliases(
          crumbItr, static_cast<AST::UsingAlias *>(crumbItr->node)->importPath,
          finalImportPath);
    } else {
      finalImportPath.push_back(importPathElem);
    }
  }
}

} // namespace smdl
