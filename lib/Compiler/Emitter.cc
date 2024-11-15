// vim:foldmethod=marker:foldlevel=0:fmr=--{,--}
#include "Emitter.h"

namespace smdl::Compiler {

//--{ Helpers
Value Emitter::emit_alloca(const llvm::Twine &name, Type *type) {
  sanity_check(type != nullptr);
  sanity_check(type->llvmType != nullptr);
  auto ip{builder.saveIP()};
  auto entry{get_llvm_function_entry_block()};
  sanity_check(entry, "can't alloca outside of LLVM function");
  builder.SetInsertPoint(entry, entry->getFirstNonPHIOrDbgOrAlloca());
  auto value{LValue(type, builder.CreateAlloca(type->llvmType, nullptr, name))};
  builder.restoreIP(ip);
  builder.CreateLifetimeStart(value, builder.getInt64(context.get_size_of(type)));
  return value;
}

Value Emitter::lvalue(Value value, bool manageLifetime) {
  if (value.is_rvalue()) {
    auto lv{emit_alloca({}, value.type)};
    builder.CreateStore(value, lv);
    if (manageLifetime)
      push(lv, {}, {}, {});
    return lv;
  }
  return value;
}

Value Emitter::rvalue(Value value) {
  if (value.is_lvalue())
    return RValue(value.type, builder.CreateLoad(value.type->llvmType, value));
  return value;
}

void Emitter::emit_unwind(Crumb *crumb0) {
  sanity_check(!has_terminator());
  for (; crumb && crumb != crumb0; crumb = crumb->prev) {
    if (crumb->value) {
      emit_end_lifetime(crumb->value);
    } else if (crumb->is_ast_defer()) {
      auto defer{static_cast<AST::Defer *>(crumb->node)};
      auto block{create_block(context.get_unique_name("defer", get_llvm_function()))};
      emit_br(block);
      Emitter emitter{this};
      emitter.crumb = defer->crumb;
      emitter.afterBreak = {};    // Not allowed to break!
      emitter.afterContinue = {}; // Not allowed to continue!
      emitter.afterReturn = {};   // Not allowed to return!
      emitter.move_to(block);
      emitter.emit(*defer->stmt);
      emitter.emit_unwind(defer->crumb);
      move_to(emitter.get_insert_block());
    } else if (crumb->is_ast_preserve()) {
      for (auto &[crumb, llvmValue] : static_cast<AST::Preserve *>(crumb->node)->backups)
        builder.CreateStore(rvalue(crumb->value), llvmValue);
    }
  }
  sanity_check(crumb == crumb0);
}

void Emitter::emit_panic(Value message, const AST::SourceLocation &srcLoc) {
  sanity_check(message);
  sanity_check(message.type == context.get_string_type());
  auto inst{builder.CreateCall(
      context.get_compile_time_callee(&Context::builtin_panic),
      {lvalue(message).llvmValue, lvalue(context.get_compile_time_source_location(srcLoc)).llvmValue})};
  inst->setIsNoInline();
  inst->setDoesNotReturn();
}
//--}

//--{ Fundamental operations
void Emitter::declare_function_parameter(const Param &param, Value value) {
  value = construct(param.type, value, param.srcLoc);
  if (!param.isConst) {
    value = lvalue(value, /*manageLifetime=*/false); // We push below
    value.llvmValue->setName(llvm_twine("lv.", param.name));
  }
  // We assume 'param' is a reference to a persistent 'Function' parameter, so we can directly
  // construct an 'llvm::ArrayRef' without needing to 'context.bump_duplicate()'
  push(value, param.name, {}, param.srcLoc);
  if (param.isInline)
    declare_function_parameter_inline(crumb->value);
}

void Emitter::declare_function_parameter_inline(Value value) {
  if (auto structType{value.type->get_inline_struct_type()}) {
    for (auto &field : structType->fields) {
      push(access(value, field.name, field.srcLoc), field.name, {}, field.srcLoc);
      crumb->value.llvmValue->setName(llvm_twine("inl.", field.name));
      if (field.isInline)
        declare_function_parameter_inline(crumb->value);
    }
  }
}

void Emitter::declare_import(bool isAbs, llvm::ArrayRef<llvm::StringRef> path, AST::Decl &decl) {
  if (path.size() < 2)
    decl.srcLoc.report_error("invalid import path (missing '::*'?)");
  auto importedModule{context.resolve_module(*this, isAbs, path.drop_back(1), decl.srcLoc)};
  if (path.back() == "*") { // Import all?
    push(context.get_compile_time_module(importedModule), context.bump_duplicate(path.drop_back(1)), &decl, decl.srcLoc);
  } else {
    auto importedCrumb{Crumb::find(importedModule->lastCrumb, path.back(), get_llvm_function(), 1)};
    if (!importedCrumb)
      decl.srcLoc.report_error(std::format("can't resolve import path '{}'", format_join(path, "::")));
    path = path.drop_while([](llvm::StringRef part) { return part == "." || part == ".."; });
    sanity_check(!path.empty());
    push(importedCrumb->value, context.bump_duplicate(path), &decl, decl.srcLoc);
  }
}

Value Emitter::emit_call(Value value, const ArgList &args, const AST::SourceLocation &srcLoc) {
  // Do we need to automatically visit any arguments?
  if (args.has_any_visited()) {
    auto name{context.get_unique_name("visit-call", get_llvm_function())};
    auto blockBegin{create_block(llvm_twine(name, ".begin"))};
    auto blockEnd{create_block(llvm_twine(name, ".end"))};
    auto localReturns{llvm::SmallVector<Return>{}};
    emit_br(blockBegin);
    emit_scope(blockBegin, blockEnd, [&](auto &emitter) {
      emitter.afterReturn = {crumb, blockEnd};
      emitter.returns = &localReturns;
      auto i{args.index_of_first_visited()};
      auto argsCopy{args};
      emitter.emit_visit(args[i].value, [&](Emitter &emitter, Value argValue) {
        argsCopy[i].value = argValue;
        argsCopy[i].isVisited = false;
        emitter.emit_return(emitter.emit_call(value, argsCopy, srcLoc), srcLoc);
      });
    });
    llvm_move_block_to_end(blockEnd);
    move_to(blockEnd);
    return emit_final_return_phi(context.get_auto_type(), localReturns);
  } else {
    if (value.is_compile_time_intrinsic())
      return emit_intrinsic(*value.get_compile_time_intrinsic(), args);
    if (value.is_compile_time_type())
      return construct(value.get_compile_time_type(), args, srcLoc);
    if (value.is_compile_time_function())
      return value.get_compile_time_function()->call(*this, args, srcLoc);
    srcLoc.report_error("invalid call");
    return {};
  }
}

void Emitter::emit_visit(Value value, const std::function<void(Emitter &emitter, Value value)> &visitor) {
  if (!value.type->is_union_or_pointer_to_union()) {
    emit_scope([&](auto &emitter) { visitor(emitter, value); });
  } else {
    auto name{context.get_unique_name("visit", get_llvm_function())};
    auto blockBegin{create_block(llvm_twine(name, ".begin"))};
    auto blockEnd{create_block(llvm_twine(name, ".end"))};
    emit_br_and_move_to(blockBegin);
    auto idx{rvalue(access(value, "#idx"))};
    auto ptr{value.type->is_union() ? rvalue(access(value, "#ptr")) : Value()};
    auto blockUnreachable{create_block(llvm_twine(name, ".unreachable"))};
    auto inst{builder.CreateSwitch(idx, blockUnreachable)};
    for (unsigned i{}; auto type : static_cast<UnionType *>(value.type->get_most_pointed_to_type())->types) {
      auto blockCase{create_block(std::format("{}.case.{}", name, i))};
      inst->addCase(static_cast<llvm::ConstantInt *>(context.get_compile_time_int(i)), blockCase);
      emit_scope(blockCase, blockEnd, [&](auto &emitter) {
        if (!type->is_void()) {
          Value valueCast{};
          if (value.type->is_union()) {
            valueCast = LValue(type, ptr);
            valueCast = value.is_rvalue() ? emitter.rvalue(valueCast) : valueCast;
          } else {
            sanity_check(value.type->is_pointer());
            valueCast = value;
            valueCast.type = context.get_pointer_type(type, value.type->get_pointer_depth());
          }
          visitor(emitter, valueCast);
        }
      });
      i++;
    }
    llvm_move_block_to_end(blockUnreachable);
    builder.SetInsertPoint(blockUnreachable);
    builder.CreateUnreachable();
    llvm_move_block_to_end(blockEnd);
    move_to_or_erase(blockEnd);
  }
}

void Emitter::emit_return(Value value, const AST::SourceLocation &srcLoc) {
  sanity_check(!has_terminator());
  sanity_check(returns);
  returns->push_back({rvalue(value), get_insert_block(), srcLoc});
  emit_unwind_and_br(afterReturn);
}

Value Emitter::emit_final_return_phi(Type *type, llvm::ArrayRef<Return> returns, const AST::SourceLocation &srcLoc) {
  sanity_check(type);
  sanity_check(!returns.empty());
  if (type->is_abstract()) {
    auto returnTypes{llvm::SmallVector<Type *>{}};
    for (auto &[value, block, srcLoc] : returns)
      returnTypes.push_back(value.type);
    auto returnType{context.get_common_type(returnTypes, /*defaultToUnion=*/true, srcLoc)};
    if (context.get_conversion_rule(returnType, type) == ConversionRule::NotAllowed)
      srcLoc.report_error(std::format("inferred return type '{}' is not convertible to '{}'", returnType->name, type->name));
    type = returnType;
  }
  auto phi{builder.CreatePHI(type->llvmType, returns.size())};
  for (auto [value, block, srcLoc] : returns) {
    Emitter emitter{this};
    emitter.builder.restoreIP(llvm_insert_point_before_terminator(block));
    auto value2{emitter.construct(type, value, srcLoc)};
    phi->addIncoming(value2, emitter.get_insert_block());
  }
  return RValue(type, phi);
}

Type *Emitter::emit_final_return(Type *type, llvm::ArrayRef<Return> returns, const AST::SourceLocation &srcLoc) {
  llvm_move_block_to_end(afterReturn);
  move_to(afterReturn);
  if (type == context.get_void_type()) {
    for (auto &returnInfo : returns)
      if (!returnInfo.value.is_void())
        srcLoc.report_error("non-'void' return statement in 'void' function");
    builder.CreateRetVoid();
    return type;
  } else {
    auto returnValue{emit_final_return_phi(type, returns, srcLoc)};
    builder.CreateRet(returnValue);
    return returnValue.type;
  }
}
//--}

static void unimplemented_emit(AST::Node &node) { node.srcLoc.report_error("unimplemented"); }

template <typename... Ts> [[nodiscard]] static Value emit_type_switch(Emitter &emitter, auto &node) {
  return llvm::TypeSwitch<decltype(&node), Value>(&node)
      .template Case<Ts...>([&]<typename T>(T *each) { return emitter.emit(*each); })
      .Default([&]<typename T>(T *each) {
        unimplemented_emit(*each);
        return Value();
      });
}

Value Emitter::emit(AST::Node &node) { return emit_type_switch<AST::Decl, AST::Expr, AST::File, AST::Stmt>(*this, node); }

//--{ Emit: Decls
Value Emitter::emit(AST::Decl &decl) {
  return emit_type_switch<
      AST::Enum, AST::Function, AST::Import, AST::Struct, AST::Tag, AST::Typedef, AST::UnitTest, AST::UsingAlias,
      AST::UsingImport, AST::Variable>(*this, decl);
}

Value Emitter::emit(AST::Enum &decl) {
  context.validate_decl_name("enum", *decl.name);
  for (auto lastValue{Value()}; auto &declVal : decl.constants) {
    context.validate_decl_name("enum constant", *declVal.name);
    auto value{
        declVal.init ? emit(declVal.init)
        : lastValue  ? emit_op(AST::BinaryOp::Add, lastValue, context.get_compile_time_int(1))
                     : Value::zero(context.get_int_type())};
    if (!value.is_compile_time_int())
      declVal.name->srcLoc.report_error(
          std::format("expected '{}' initializer to resolve to compile-time int", declVal.name->name));
    declVal.llvmConst = static_cast<llvm::ConstantInt *>(value);
    lastValue = value;
  }
  auto type{context.get_enum_type(&decl, get_llvm_function())};
  decl.module = module;
  decl.crumb = crumb;
  declare(*decl.name, context.get_compile_time_type(type));
  for (auto &declVal : decl.constants)
    declare(*declVal.name, RValue(type, declVal.llvmConst), &decl);
  return {};
}

Value Emitter::emit(AST::Function &decl) {
  decl.module = module;
  decl.crumb = crumb;
  declare(*decl.name, context.get_compile_time_function(context.get_function(*this, &decl)), &decl);
  return {};
}

Value Emitter::emit(AST::Import &decl) {
  if (decl.isExport)
    decl.srcLoc.report_error("can't re-export qualified 'import'");
  decl.module = module;
  decl.crumb = crumb;
  for (auto &path : decl.paths)
    declare_import(path->isAbsolute, path->get_string_refs(), decl);
  return {};
}

Value Emitter::emit(AST::Struct &decl) {
  context.validate_decl_name("struct", *decl.name);
  for (auto &astField : decl.fields)
    emit(astField.type);
  for (auto &astTag : decl.tags)
    emit(astTag.type);
  decl.module = module;
  decl.crumb = crumb;
  declare(*decl.name, context.get_compile_time_type(context.get_struct_type(&decl, get_llvm_function())), &decl);
  return {};
}

Value Emitter::emit(AST::Tag &decl) {
  context.validate_decl_name("tag", *decl.name);
  decl.module = module;
  decl.crumb = crumb;
  declare(*decl.name, context.get_compile_time_type(context.get_tag_type(&decl, get_llvm_function())), &decl);
  return {};
}

Value Emitter::emit(AST::Typedef &decl) {
  context.validate_decl_name("typedef", *decl.name);
  decl.module = module;
  decl.crumb = crumb;
  declare(*decl.name, emit(decl.type), &decl);
  return {};
}

Value Emitter::emit(AST::UnitTest &decl) {
  decl.module = module;
  decl.crumb = crumb;
  if (context.mdl.enableUnitTests) {
    auto func{FunctionInstance{*this, decl}};
    context.mdl.unitTests.push_back(
        MDLInstance::UnitTest{.llvmName = func.llvmFunc->getName().str(), .name = decl.name.str().str()});
  }
  return {};
}

Value Emitter::emit(AST::UsingAlias &decl) {
  // TODO
  unimplemented_emit(decl);
  return {};
}

Value Emitter::emit(AST::UsingImport &decl) {
  auto path{decl.path->get_string_refs()};
  if (decl.is_import_all()) {
    path.push_back("*");
    declare_import(decl.path->isAbsolute, path, decl);
  } else {
    for (auto &name : decl.names) {
      path.push_back(name->name);
      declare_import(decl.path->isAbsolute, path, decl);
      path.pop_back();
    }
  }
  return {};
}

Value Emitter::emit(AST::Variable &decl) {
  auto type{emit(decl.type).get_compile_time_type()};
  auto isConst{bool(decl.type->attrs.isConst)};
  auto isStatic{bool(decl.type->attrs.isStatic)};
  if (!get_llvm_function() && !isConst)
    decl.srcLoc.report_error("global variables must be 'const'");
  if (isStatic && !isConst)
    decl.srcLoc.report_error("variable declared 'static' must also be 'const' (at least for now)");
  if (decl.type->attrs.isInline)
    decl.srcLoc.report_error("variable must not be declared 'inline'");
  decl.module = module;
  decl.crumb = crumb;
  for (auto &declVal : decl.values) {
    auto &declValName{*declVal.name};
    context.validate_decl_name("variable", declValName);
    Value value{};
    if (declVal.init) {
      emit_scope([&](auto &emitter) { value = emitter.emit(declVal.init); });
      value = construct(type, value, declValName.srcLoc);
    } else if (declVal.args) {
      value = construct(type, emit(declVal.args), declValName.srcLoc);
    } else {
      if (type->is_abstract())
        declValName.srcLoc.report_error(
            std::format("variable '{}' declare with type '{}' requires initializer", declValName.name, type->name));
      value = Value::zero(type);
    }
    if (isStatic) {
      if (!value.is_compile_time())
        declValName.srcLoc.report_error(
            std::format("variable '{}' declared 'static' requires compile-time initializer", declValName.name));
      auto llvmGlobal{new llvm::GlobalVariable(
          context.llvmModule, value.type->llvmType, /*isConst=*/true,
          decl.isExport ? llvm::GlobalValue::ExternalLinkage : llvm::GlobalValue::PrivateLinkage,
          static_cast<llvm::Constant *>(value.llvmValue))};
      llvmGlobal->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
      llvmGlobal->setName(llvm_twine("gv.", declValName.name));
      value = LValue(value.type, llvmGlobal);
    } else if (!isConst) {
      auto valueAlloca{emit_alloca(declValName.name, value.type)};
      builder.CreateStore(value, valueAlloca);
      value = LValue(value.type, valueAlloca);
    }
    declare(declValName, value, &decl);
  }
  return {};
}
//--}

//--{ Emit: Exprs
Value Emitter::emit(AST::Expr &expr) {
  return emit_type_switch<
      AST::Binary, AST::Call, AST::Cast, AST::CompileTime, AST::Conditional, AST::GetField, AST::GetIndex, AST::Identifier,
      AST::Intrinsic, AST::Let, AST::LiteralBool, AST::LiteralFloat, AST::LiteralInt, AST::LiteralString, AST::ReturnFrom,
      AST::SizeName, AST::Type, AST::Unary>(*this, expr);
}

Value Emitter::emit(AST::Binary &expr) {
  // Temporary definition.
  if (expr.op == AST::BinaryOp::Def) {
    auto identifier{llvm::dyn_cast<AST::Identifier>(&*expr.lhs)};
    if (!identifier || !identifier->is_simple_name())
      expr.srcLoc.report_error("expected lhs of definition operator ':=' to be a simple name");
    context.validate_decl_name("temporary", *identifier->names[0]);
    auto rv{rvalue(emit(expr.rhs))};
    declare(*identifier->names[0], rv);
    return rv;
  }
  // Short circuit logical and/or.
  if (expr.op == AST::BinaryOp::LogicalAnd || expr.op == AST::BinaryOp::LogicalOr) {
    auto boolType{context.get_bool_type()};
    auto valueLhs{construct(boolType, emit(expr.lhs), expr.srcLoc)};
    if (valueLhs.is_compile_time_int()) {
      auto valueLhsNow{valueLhs.get_compile_time_int()};
      if ((valueLhsNow != 0 && expr.op == AST::BinaryOp::LogicalAnd) || //
          (valueLhsNow == 0 && expr.op == AST::BinaryOp::LogicalOr))
        return construct(boolType, emit(expr.rhs), expr.srcLoc);
      return valueLhs;
    }
    auto name{context.get_unique_name(expr.op == AST::BinaryOp::LogicalAnd ? "and" : "or", get_llvm_function())};
    auto blockLhs{get_insert_block()};
    auto blockRhs{create_block(llvm_twine(name, ".rhs"))};
    auto blockEnd{create_block(llvm_twine(name, ".end"))};
    emit_br(
        valueLhs,
        expr.op == AST::BinaryOp::LogicalOr ? blockEnd : blockRhs, //
        expr.op == AST::BinaryOp::LogicalOr ? blockRhs : blockEnd);
    move_to(blockRhs);
    auto valueRhs{construct(boolType, emit(expr.rhs), expr.srcLoc)};
    emit_br_and_move_to(blockEnd);
    return emit_phi(
        boolType, {{context.get_compile_time_bool(expr.op == AST::BinaryOp::LogicalOr), blockLhs}, {valueRhs, blockRhs}});
  }
  return emit_op(expr.op, emit(expr.lhs), emit(expr.rhs), expr.srcLoc);
}

Value Emitter::emit(AST::Conditional &expr) {
  auto cond{construct(context.get_bool_type(), emit(expr.cond))};
  if (cond.is_compile_time_int())
    return rvalue(emit(cond.get_compile_time_int() != 0 ? *expr.ifPass : *expr.ifFail));

  auto name{context.get_unique_name("select", get_llvm_function())};
  auto blockPass{create_block(llvm_twine(name, ".pass"))};
  auto blockFail{create_block(llvm_twine(name, ".fail"))};
  auto blockEnd{create_block(llvm_twine(name, ".end"))};
  emit_br(cond, blockPass, blockFail);
  move_to(blockPass);
  auto valuePass{rvalue(emit(expr.ifPass))};
  auto valuePassIP{builder.saveIP()};
  llvm_move_block_to_end(blockFail);
  move_to(blockFail);
  auto valueFail{rvalue(emit(expr.ifFail))};
  auto valueFailIP{builder.saveIP()};

  // Back-track and cast results to common type.
  auto type{context.get_common_type({valuePass.type, valueFail.type}, /*defaultToUnion=*/true, expr.srcLoc)};
  builder.restoreIP(valuePassIP), valuePass = construct(type, valuePass), blockPass = get_insert_block(), emit_br(blockEnd);
  builder.restoreIP(valueFailIP), valueFail = construct(type, valueFail), blockFail = get_insert_block(), emit_br(blockEnd);
  llvm_move_block_to_end(blockEnd);
  move_to(blockEnd);
  return emit_phi(type, {{valuePass, blockPass}, {valueFail, blockFail}});
}

Value Emitter::emit(AST::GetIndex &expr) {
  if (auto value{emit(expr.expr)}; value.is_compile_time_type()) {
    // Array type construction
    auto type{value.get_compile_time_type()};
    for (auto itr{expr.indices.rbegin()}; itr != expr.indices.rend(); ++itr) {
      auto &index{*itr};
      if (!index) {
        type = context.get_array_type(type, ""); // Empty
      } else if (auto sizeName{llvm::dyn_cast<AST::SizeName>(index.get())}) {
        type = context.get_array_type(type, sizeName->name->name);
      } else {
        auto size{construct(context.get_int_type(), emit(index), expr.srcLoc)};
        if (!size.is_compile_time_int())
          expr.srcLoc.report_error("expected array size expression to resolve to compile-time int");
        type = context.get_array_type(type, size.get_compile_time_int());
      }
    }
    return context.get_compile_time_type(type);
  } else {
    for (auto &index : expr.indices) {
      if (!index)
        expr.srcLoc.report_error("expected non-empty '[]'");
      value = access(value, construct(context.get_int_type(), emit(index), expr.srcLoc), expr.srcLoc);
    }
    return value;
  }
}

Value Emitter::emit(AST::Let &expr) {
  auto name{context.get_unique_name("let", get_llvm_function())};
  auto blockBegin{create_block(llvm_twine(name, ".begin"))};
  auto blockEnd{create_block(llvm_twine(name, ".end"))};
  llvm::BasicBlock *block{};
  Value value{};
  emit_br(blockBegin);
  emit_scope(blockBegin, blockEnd, [&](auto &emitter) {
    for (auto &var : expr.vars)
      emitter.emit(var);
    value = emitter.rvalue(emitter.emit(expr.expr));
    block = emitter.get_insert_block();
  });
  llvm_move_block_to_end(blockEnd);
  move_to(blockEnd);
  if (!value.is_void())
    return emit_phi(value.type, {{value.llvmValue, block}});
  return {};
}

Value Emitter::emit(AST::ReturnFrom &expr) {
  auto name{context.get_unique_name("return_from", get_llvm_function())};
  auto blockBegin{create_block(llvm_twine(name, ".begin"))};
  auto blockEnd{create_block(llvm_twine(name, ".end"))};
  auto localReturns{llvm::SmallVector<Return>{}};
  emit_br(blockBegin);
  emit_scope(blockBegin, blockEnd, [&](auto &emitter) {
    emitter.afterBreak = {};
    emitter.afterContinue = {};
    emitter.afterEndScope = {};
    emitter.afterReturn = {crumb, blockEnd};
    emitter.returns = &localReturns;
    emitter.emit(expr.stmt);
  });
  llvm_move_block_to_end(blockEnd);
  move_to(blockEnd);
  return emit_final_return_phi(context.get_auto_type(), localReturns);
}

Value Emitter::emit(AST::SizeName &expr) {
  // TODO
  unimplemented_emit(expr);
  return {};
}

Value Emitter::emit(AST::Type &expr) {
  auto value{emit(expr.expr)};
  if (!value.is_compile_time_type())
    expr.srcLoc.report_error("expected expression to resolve to compile-time type");
  expr.type = value.get_compile_time_type(); // Remember type in AST expression
  return value;
}
//--}

//--{ Emit: Stmts
Value Emitter::emit(AST::Stmt &stmt) {
  return emit_type_switch<
      AST::Break, AST::Compound, AST::Continue, AST::DeclStmt, AST::Defer, AST::DoWhile, AST::ExprStmt, AST::For, AST::If,
      AST::Preserve, AST::Return, AST::Switch, AST::Unreachable, AST::Visit, AST::While>(*this, stmt);
}

Value Emitter::emit(AST::Compound &stmt) {
  auto name{context.get_unique_name("compound", get_llvm_function())};
  auto blockBegin{create_block(llvm_twine(name, ".begin"))};
  auto blockEnd{create_block(llvm_twine(name, ".end"))};
  emit_br(blockBegin);
  emit_scope(blockBegin, blockEnd, [&](auto &emitter) {
    for (auto &subStmt : stmt.stmts)
      if (emitter.emit(subStmt); emitter.has_terminator())
        break;
  });
  llvm_move_block_to_end(blockEnd);
  move_to_or_erase(blockEnd);
  return {};
}

Value Emitter::emit(AST::DoWhile &stmt) {
  auto name{context.get_unique_name("do_while", get_llvm_function())};
  auto blockBody{create_block(llvm_twine(name, ".body"))};
  auto blockCond{create_block(llvm_twine(name, ".cond"))};
  auto blockEnd{create_block(llvm_twine(name, ".end"))};
  emit_br(blockBody);
  emit_scope(blockBody, blockCond, [&](auto &emitter) {
    emitter.afterBreak = {crumb, blockEnd};
    emitter.afterContinue = {crumb, blockCond};
    emitter.emit(stmt.body);
  });
  llvm_move_block_to_end(blockCond);
  move_to(blockCond);
  emit_br(emit_cond(*stmt.cond), blockBody, blockEnd);
  llvm_move_block_to_end(blockEnd);
  move_to_or_erase(blockEnd);
  return {};
}

Value Emitter::emit(AST::For &stmt) {
  auto name{context.get_unique_name("for", get_llvm_function())};
  auto blockBegin{create_block(llvm_twine(name, ".begin"))};
  auto blockCond{create_block(llvm_twine(name, ".cond"))};
  auto blockBody{create_block(llvm_twine(name, ".body"))};
  auto blockIncr{create_block(llvm_twine(name, ".incr"))};
  auto blockEnd{create_block(llvm_twine(name, ".end"))};
  emit_br_and_move_to(blockBegin);
  emit_scope([&](auto &emitter1) {
    emitter1.emit(stmt.init);
    auto crumb1{emitter1.crumb};
    llvm_move_block_to_end(blockCond);
    emitter1.emit_br_and_move_to(blockCond);
    emitter1.emit_br(emitter1.emit_cond(*stmt.cond), blockBody, blockEnd);
    emitter1.emit_scope(blockCond, blockIncr, [&](auto &emitter2) {
      emitter2.afterBreak = {crumb1, blockEnd};
      emitter2.afterContinue = {crumb1, blockIncr};
      llvm_move_block_to_end(blockBody);
      emitter2.move_to(blockBody);
      emitter2.emit(stmt.body);
    });
    llvm_move_block_to_end(blockIncr);
    emitter1.move_to(blockIncr);
    emitter1.emit(stmt.incr);
    emitter1.emit_br(blockCond);
    emitter1.move_to(blockEnd);
  });
  llvm_move_block_to_end(blockEnd);
  move_to_or_erase(blockEnd);
  return {};
}

Value Emitter::emit(AST::If &stmt) {
  auto cond{emit_cond(*stmt.cond)};
  if (cond.is_compile_time_int()) {
    auto pass{cond.get_compile_time_int() != 0};
    if ((pass && !stmt.ifPass) || (!pass && !stmt.ifFail))
      return {};
    auto name{context.get_unique_name("if", get_llvm_function())};
    auto blockThen{create_block(llvm_twine(name, pass ? ".pass" : ".fail"))};
    auto blockEnd{create_block(llvm_twine(name, ".end"))};
    emit_br(blockThen);
    emit_scope(blockThen, blockEnd, [&](auto &emitter) { emitter.emit(pass ? *stmt.ifPass : *stmt.ifFail); });
    llvm_move_block_to_end(blockEnd);
    move_to_or_erase(blockEnd);
  } else {
    auto name{context.get_unique_name("if", get_llvm_function())};
    auto blockPass{stmt.ifPass.get() ? create_block(llvm_twine(name, ".pass")) : nullptr};
    auto blockFail{stmt.ifFail.get() ? create_block(llvm_twine(name, ".fail")) : nullptr};
    auto blockEnd{create_block(llvm_twine(name, ".end"))};
    emit_br(cond, blockPass ? blockPass : blockEnd, blockFail ? blockFail : blockEnd);
    if (blockPass)
      emit_scope(blockPass, blockEnd, [&](auto &emitter) { emitter.emit(stmt.ifPass); });
    if (blockFail) {
      llvm_move_block_to_end(blockFail);
      emit_scope(blockFail, blockEnd, [&](auto &emitter) { emitter.emit(stmt.ifFail); });
    }
    llvm_move_block_to_end(blockEnd);
    move_to_or_erase(blockEnd);
  }
  return {};
}

Value Emitter::emit(AST::Preserve &stmt) {
  stmt.backups.clear();
  for (auto &expr : stmt.exprs) {
    auto value{emit(expr)};
    if (!value.is_lvalue())
      stmt.srcLoc.report_error("can't apply 'preserve' to constant or temporary");
    push(emit_alloca("local.preserve", value.type), {}, {}, stmt.srcLoc);
    stmt.backups.emplace_back(crumb, value);
    builder.CreateStore(rvalue(value), crumb->value);
  }
  push({}, {}, &stmt, stmt.srcLoc);
  return {};
}

Value Emitter::emit(AST::Switch &stmt) {
  struct SwitchCase final {
    AST::Switch::Case *astCase{};
    llvm::ConstantInt *cond{};
    llvm::BasicBlock *block{};
  };
  auto name{context.get_unique_name("switch", get_llvm_function())};
  auto blockBegin{create_block(llvm_twine(name, ".begin"))};
  auto blockEnd{create_block(llvm_twine(name, ".end"))};
  auto blockDefault{static_cast<llvm::BasicBlock *>(nullptr)};
  auto cases{llvm::SmallVector<SwitchCase>{}};
  for (auto &astCase : stmt.cases) {
    if (astCase.isDefault()) {
      if (blockDefault)
        stmt.srcLoc.report_error("expected at most 1 'default' case in 'switch'");
      cases.push_back(SwitchCase{&astCase, nullptr, create_block(llvm_twine(name, ".default"))});
      blockDefault = cases.back().block;
    } else {
      auto cond{emit(astCase.cond)};
      if (!cond.is_compile_time_int())
        astCase.cond->srcLoc.report_error("expected 'case' expression to resolve to compile-time integer");
      cases.push_back(SwitchCase{&astCase, static_cast<llvm::ConstantInt *>(cond), create_block(llvm_twine(name, ".case"))});
    }
  }
  if (!blockDefault)
    blockDefault = blockEnd;
  emit_br_and_move_to(blockBegin);
  auto what{construct(context.get_int_type(), emit(stmt.what), stmt.srcLoc)};
  auto inst{builder.CreateSwitch(what, blockDefault)};
  emit_scope(blockBegin, blockEnd, [&](auto &emitter) {
    emitter.afterBreak = {crumb, blockEnd};
    for (size_t i{}; auto &[astCase, cond, block] : cases) {
      emitter.move_to(block);
      for (auto &subStmt : astCase->stmts)
        if (emitter.emit(subStmt); emitter.has_terminator())
          break;
      if (cond)
        inst->addCase(cond, block);
      if (++i < cases.size()) {
        if (!emitter.has_terminator())
          emitter.emit_br(cases[i].block);
        llvm_move_block_to_end(cases[i].block);
      }
    }
  });
  llvm_move_block_to_end(blockEnd);
  move_to_or_erase(blockEnd);
  return {};
}

Value Emitter::emit(AST::While &stmt) {
  auto name{context.get_unique_name("while", get_llvm_function())};
  auto blockCond{create_block(llvm_twine(name, ".cond"))};
  auto blockBody{create_block(llvm_twine(name, ".body"))};
  auto blockEnd{create_block(llvm_twine(name, ".end"))};
  emit_br_and_move_to(blockCond);
  emit_br(emit_cond(*stmt.cond), blockBody, blockEnd);
  emit_scope(blockBody, blockCond, [&](auto &emitter) {
    emitter.afterBreak = {crumb, blockEnd};
    emitter.afterContinue = {crumb, blockCond};
    emitter.emit(*stmt.body);
  });
  llvm_move_block_to_end(blockEnd);
  move_to_or_erase(blockEnd);
  return {};
}
//--}

ArgList Emitter::emit(const AST::ArgList &astArgs) {
  ArgList args{};
  for (auto &astArg : astArgs.args) {
    auto &arg{args.emplace_back()};
    if (astArg.name)
      arg.name = astArg.name->name;
    arg.value = emit(astArg.expr);
    arg.srcLoc = astArg.srcLoc;
    arg.src = astArg.src;
    arg.isVisited = astArg.isVisited && arg.value.type->is_union_or_pointer_to_union();
  }
  args.guarantee_valid_names(astArgs.srcLoc);
  return args;
}

//--{ Emit: Ops

Value Emitter::emit_op(AST::UnaryOp op, Value val, const AST::SourceLocation &srcLoc) {
  using UnOp = AST::UnaryOp;
  using BinOp = AST::BinaryOp;
  if (val.is_compile_time_type()) {
    auto type{val.get_compile_time_type()};
    if (op == UnOp::Address) {
      // &int
      return context.get_compile_time_type(context.get_pointer_type(type));
    } else if (op == UnOp::Deref) {
      // *(?int)   = int
      // *(int[4]) = int
      if (type->is_optional()) {
        auto unionType{static_cast<UnionType *>(type)};
        return context.get_compile_time_type(
            context.get_union_type(llvm::ArrayRef(unionType->types.data(), unionType->types.size() - 1)));
      }
      if (!type->is_pointer() && !type->is_array())
        srcLoc.report_error(std::format("can't dereference type '{}'", type->name));
      return context.get_compile_time_type(type->get_element_type());
    } else if (op == UnOp::Maybe) {
      // ?int = int | void
      auto type{val.get_compile_time_type()};
      if (type == context.get_void_type())
        srcLoc.report_error("can't optionalize 'void'");
      if (type->is_abstract())
        srcLoc.report_error(std::format("can't optionalize abstract type '{}'", type->name));
      return context.get_compile_time_type(context.get_union_type({context.get_void_type(), type}));
    }
  } else {
    if ((op & UnOp::Postfix) == UnOp::Postfix) {
      auto result{rvalue(val)};
      return emit_op(op & ~UnOp::Postfix, val, srcLoc), result;
    }
    if (op == UnOp::Incr && (val.type->is_vectorized() || val.type->is_pointer()))
      return emit_op(BinOp::EqAdd, val, context.get_compile_time_int(1), srcLoc);
    if (op == UnOp::Decr && (val.type->is_vectorized() || val.type->is_pointer()))
      return emit_op(BinOp::EqSub, val, context.get_compile_time_int(1), srcLoc);
    if (op == UnOp::Pos && (val.type->is_vectorized() || val.type->is_matrix()))
      return rvalue(val);
    if (op == UnOp::Neg && (val.type->is_vectorized() || val.type->is_matrix())) {
      if (val.type->is_vectorized()) {
        val = rvalue(val);
        val = RValue(val.type, val.type->is_floating() ? builder.CreateFNeg(val) : builder.CreateNeg(val));
        return val;
      } else {
        sanity_check(val.type->is_matrix());
        auto result{Value::zero(val.type)};
        for (uint32_t j{}; j < val.type->extent.numCols; j++)
          result = insert(result, emit_op(op, access(val, j)), j);
        return result;
      }
    }
    if (op == UnOp::Not && val.type->is_vectorized() && val.type->is_integral())
      return RValue(val.type, builder.CreateNot(rvalue(val)));
    if (op == UnOp::LogicalNot && (val.type->is_scalar() || val.type->is_pointer()))
      return emit_op(UnOp::Not, construct(context.get_bool_type(), val, srcLoc), srcLoc);
    if (op == UnOp::LogicalNot && (val.type->is_vector()))
      return emit_op(UnOp::Not, construct(val.type->get_with_different_scalar(Scalar::Bool), val, srcLoc), srcLoc);
    if (op == UnOp::Address) {
      if (!val.is_lvalue())
        srcLoc.report_error("can't take address of rvalue");
      return RValue(context.get_pointer_type(val.type), val);
    }
    if (op == UnOp::Deref) {
      if (val.type->is_optional()) {
        auto unionType{static_cast<UnionType *>(val.type)};
        if (unionType->types.size() > 2) {
          val.type = context.get_union_type(llvm::ArrayRef(unionType->types.data(), unionType->types.size() - 1));
          return val;
        } else {
          if (val.is_rvalue()) {
            auto lv{lvalue(val, /*manageLifetime=*/false)};
            auto rv{rvalue(LValue(unionType->types[0], access(lv, "#ptr", srcLoc)))};
            emit_end_lifetime(lv);
            return rv;
          } else {
            return LValue(unionType->types[0], access(val, "#ptr", srcLoc));
          }
        }
      }
      if (!val.type->is_pointer())
        srcLoc.report_error(std::format("can't dereference type '{}'", val.type->name));
      return LValue(val.type->get_element_type(), rvalue(val));
    }
  }
  srcLoc.report_error(std::format("unimplemented unary operator '{}' for type '{}'", AST::to_string(op), val.type->name));
  return {};
}

//--{ Helper: 'llvm_arithmetic_op'
[[nodiscard]] static std::optional<llvm::Instruction::BinaryOps> llvm_arithmetic_op(Scalar scalar, AST::BinaryOp op) {
  if (scalar == Scalar::Bool || scalar == Scalar::Int) {
    switch (op) {
    case AST::BinaryOp::Add: return llvm::Instruction::Add;
    case AST::BinaryOp::Sub: return llvm::Instruction::Sub;
    case AST::BinaryOp::Mul: return llvm::Instruction::Mul;
    case AST::BinaryOp::Div: return llvm::Instruction::SDiv;
    case AST::BinaryOp::Rem: return llvm::Instruction::SRem;
    case AST::BinaryOp::And: return llvm::Instruction::And;
    case AST::BinaryOp::Or: return llvm::Instruction::Or;
    case AST::BinaryOp::Xor: return llvm::Instruction::Xor;
    case AST::BinaryOp::Shl: return llvm::Instruction::Shl;
    case AST::BinaryOp::AShr: return llvm::Instruction::AShr;
    case AST::BinaryOp::LShr: return llvm::Instruction::LShr;
    default: break;
    }
  } else if (scalar == Scalar::Float || scalar == Scalar::Double) {
    switch (op) {
    case AST::BinaryOp::Add: return llvm::Instruction::FAdd;
    case AST::BinaryOp::Sub: return llvm::Instruction::FSub;
    case AST::BinaryOp::Mul: return llvm::Instruction::FMul;
    case AST::BinaryOp::Div: return llvm::Instruction::FDiv;
    case AST::BinaryOp::Rem: return llvm::Instruction::FRem;
    default: break;
    }
  }
  return std::nullopt;
}
//--}
//--{ Helper: 'llvm_compare_op'
[[nodiscard]] static std::optional<llvm::CmpInst::Predicate> llvm_compare_op(
    Scalar scalar, AST::BinaryOp op, bool isSigned = true) {
  if (scalar == Scalar::Bool || scalar == Scalar::Int) {
    switch (op) {
    case AST::BinaryOp::CmpEq: return llvm::CmpInst::ICMP_EQ;
    case AST::BinaryOp::CmpNe: return llvm::CmpInst::ICMP_NE;
    case AST::BinaryOp::CmpLt: return !isSigned ? llvm::CmpInst::ICMP_ULT : llvm::CmpInst::ICMP_SLT;
    case AST::BinaryOp::CmpLe: return !isSigned ? llvm::CmpInst::ICMP_ULE : llvm::CmpInst::ICMP_SLE;
    case AST::BinaryOp::CmpGt: return !isSigned ? llvm::CmpInst::ICMP_UGT : llvm::CmpInst::ICMP_SGT;
    case AST::BinaryOp::CmpGe: return !isSigned ? llvm::CmpInst::ICMP_UGE : llvm::CmpInst::ICMP_SGE;
    default: break;
    }
  } else if (scalar == Scalar::Float || scalar == Scalar::Double) {
    switch (op) {
    case AST::BinaryOp::CmpEq: return llvm::CmpInst::FCMP_OEQ;
    case AST::BinaryOp::CmpNe: return llvm::CmpInst::FCMP_ONE;
    case AST::BinaryOp::CmpLt: return llvm::CmpInst::FCMP_OLT;
    case AST::BinaryOp::CmpLe: return llvm::CmpInst::FCMP_OLE;
    case AST::BinaryOp::CmpGt: return llvm::CmpInst::FCMP_OGT;
    case AST::BinaryOp::CmpGe: return llvm::CmpInst::FCMP_OGE;
    default: break;
    }
  }
  return std::nullopt;
}
//--}

Value Emitter::emit_op(AST::BinaryOp op, Value lhs, Value rhs, const AST::SourceLocation &srcLoc) {
  using UnOp = AST::UnaryOp;
  using BinOp = AST::BinaryOp;
  if (op == BinOp::Comma)
    return rhs;
  //--{ Assignment
  if ((op & BinOp::Eq) == BinOp::Eq) {
    if (!lhs.is_lvalue())
      srcLoc.report_error(std::format("can't apply '{}' to rvalue", AST::to_string(op)));
    if (op != BinOp::Eq)
      rhs = emit_op(op & ~BinOp::Eq, rvalue(lhs), rhs, srcLoc);
    builder.CreateStore(construct(lhs.type, rhs, srcLoc), lhs);
    return lhs;
  }
  //--}
  //--{ Logical
  if ((op & BinOp::Logical) == BinOp::Logical) {
    op = op & ~BinOp::Logical;
    lhs = construct(context.get_bool_type(), lhs, srcLoc);
    rhs = construct(context.get_bool_type(), rhs, srcLoc);
  }
  //--}

  const auto lhsType{lhs.type};
  const auto rhsType{rhs.type};
  lhs = rvalue(lhs);
  rhs = rvalue(rhs);
  if (AST::is_compare_op(op)) {
    //--{ Comparison: Enum
    if (lhsType->is_enum() && rhsType->is_enum()) {
      return RValue(context.get_bool_type(), builder.CreateCmp(*llvm_compare_op(Scalar::Int, op), lhs, rhs));
    }
    //--}
    //--{ Comparison: String
    if (lhsType == context.get_string_type() && rhsType == context.get_string_type()) {
      // If both compile-time strings, do the comparison at compile time. Otherwise, build a
      // call to 'strncmp' with `llvm::emitStrNCmp'.
      if (lhs.is_compile_time_string() && rhs.is_compile_time_string()) {
        auto lhsNow{lhs.get_compile_time_string()};
        auto rhsNow{rhs.get_compile_time_string()};
        bool result{};
        switch (op) {
        case BinOp::CmpEq: result = (lhsNow == rhsNow); break;
        case BinOp::CmpNe: result = (lhsNow != rhsNow); break;
        case BinOp::CmpLt: result = (lhsNow < rhsNow); break;
        case BinOp::CmpGt: result = (lhsNow > rhsNow); break;
        case BinOp::CmpLe: result = (lhsNow <= rhsNow); break;
        case BinOp::CmpGe: result = (lhsNow >= rhsNow); break;
        default: sanity_check(false); break;
        }
        return context.get_compile_time_bool(result);
      } else {
        auto llvmSizeType{llvm::Type::getIntNTy(context.llvmContext, sizeof(size_t) * 8)};
        auto llvmValue{llvm::emitStrNCmp(
            access(lhs, "ptr", srcLoc), //
            access(rhs, "ptr", srcLoc), //
            builder.CreateIntCast(
                builder.CreateBinaryIntrinsic(llvm::Intrinsic::umax, access(lhs, "len", srcLoc), access(rhs, "len", srcLoc)),
                llvmSizeType, /*isSigned=*/false),
            builder, context.llvmLayout, &context.llvmTargetLibraryInfo)};
        llvmValue = builder.CreateIntCast(llvmValue, context.get_int_type()->llvmType, /*isSigned=*/true);
        llvmValue = builder.CreateCmp(*llvm_compare_op(Scalar::Int, op), llvmValue, context.get_compile_time_int(0));
        return RValue(context.get_bool_type(), llvmValue);
      }
    }
    //--}
    //--{ Comparison: Struct
    if (lhsType->is_struct() && rhsType == lhsType) {
      const auto structType{static_cast<StructType *>(lhsType)};
      auto name{context.get_unique_name("compare", get_llvm_function())};
      auto blockLtGt{llvm::SmallVector<std::pair<llvm::BasicBlock *, llvm::BasicBlock *>>{}};
      auto phiInputs{llvm::SmallVector<std::pair<llvm::Value *, llvm::BasicBlock *>>{}};
      for (unsigned i{}; i < structType->fields.size(); i++) {
        auto blockLt{create_block(std::format("{}.field{}.lt", name, i))};
        auto blockGt{create_block(std::format("{}.field{}.gt", name, i))};
        blockLtGt.emplace_back(blockLt, blockGt);
        phiInputs.emplace_back(context.get_compile_time_int(-1).llvmValue, blockLt);
        phiInputs.emplace_back(context.get_compile_time_int(+1).llvmValue, blockGt);
      }
      auto blockEqual{create_block(llvm_twine(name, ".equal"))};
      auto blockEnd{create_block(llvm_twine(name, ".end"))};
      phiInputs.emplace_back(context.get_compile_time_int(0).llvmValue, blockEqual);
      for (unsigned i{}; i < structType->fields.size(); i++) {
        auto [blockLt, blockGt] = blockLtGt[i];
        llvm_move_block_to_end(blockLt);
        if (i == 0)
          emit_br(blockLt);
        move_to(blockLt);
        auto &field{structType->fields[i]};
        auto lhsField{access(lhs, field.name, srcLoc)};
        auto rhsField{access(rhs, field.name, srcLoc)};
        emit_br(construct(context.get_bool_type(), emit_op(BinOp::CmpLt, lhsField, rhsField, srcLoc)), blockEnd, blockGt);
        llvm_move_block_to_end(blockGt);
        move_to(blockGt);
        emit_br(
            construct(context.get_bool_type(), emit_op(BinOp::CmpGt, lhsField, rhsField, srcLoc)), blockEnd,
            i + 1 < structType->fields.size() ? blockLtGt[i + 1].first : blockEqual);
      }
      llvm_move_block_to_end(blockEqual);
      move_to(blockEqual);
      emit_br(blockEnd);
      llvm_move_block_to_end(blockEnd);
      move_to(blockEnd);
      return RValue(
          context.get_bool_type(),
          builder.CreateCmp(
              *llvm_compare_op(Scalar::Int, op), emit_phi(context.get_int_type(), phiInputs), context.get_compile_time_int(0)));
    }
    //--}
    //--{ Comparison: Union
    if (lhsType->is_union() && rhsType->is_union()) {
      // TODO
    }
    //--}
  }
  //--{ Operators: Scalar, Vector, Matrix
  if (lhsType->is_arithmetic() && rhsType->is_arithmetic()) {
    // Scalar-Vector
    if ((lhsType->is_scalar() && rhsType->is_scalar()) || (lhsType->is_scalar() && rhsType->is_vector()) ||
        (lhsType->is_vector() && rhsType->is_scalar()) || (lhsType->is_vector() && lhsType->extent == rhsType->extent)) {
      auto type{context.get_common_type(lhsType, rhsType)};
      lhs = construct(type, lhs, srcLoc);
      rhs = construct(type, rhs, srcLoc);
      if (auto llvmOp{llvm_arithmetic_op(type->scalar, op)})
        return RValue(type, builder.CreateBinOp(*llvmOp, lhs, rhs));
      if (auto llvmOp{llvm_compare_op(type->scalar, op)})
        return RValue(type->get_with_different_scalar(Scalar::Bool), builder.CreateCmp(*llvmOp, lhs, rhs));
    }
    // Matrix-Matrix addition
    if ((op == BinOp::Add || op == BinOp::Sub) && lhsType->is_matrix() && rhsType->is_matrix() &&
        lhsType->extent == rhsType->extent) {
      auto type{context.get_common_type(lhsType, rhsType)};
      auto result{Value::zero(type)};
      for (unsigned j{}; j < type->extent.numCols; j++)
        result = insert(result, emit_op(op, access(lhs, j), access(rhs, j)), j);
      return result;
    }
    // Matrix-Scalar multiplication
    if ((op == BinOp::Mul || op == BinOp::Div) && lhsType->is_matrix() && rhsType->is_scalar()) {
      auto type{context.get_common_type(lhsType, rhsType)};
      auto result{Value::zero(type)};
      auto scalar{construct(type->get_column_type(), rhs, srcLoc)};
      for (unsigned j{}; j < type->extent.numCols; j++)
        result = insert(result, emit_op(op, access(lhs, j), scalar), j);
      return result;
    }
    // Scalar-Matrix multiplication
    if ((op == BinOp::Mul) && lhsType->is_scalar() && rhsType->is_matrix())
      return emit_op(op, rhs, lhs, srcLoc);
    // Matrix-Matrix or Matrix-Vector multiplication
    if ((op == BinOp::Mul) && lhsType->is_matrix() && (rhsType->is_matrix() || rhsType->is_vector()) &&
        lhsType->extent.numCols == rhsType->extent.numRows) {
      auto scalar{std::max(lhsType->scalar, rhsType->scalar)};
      auto extentM{lhsType->extent.numRows};
      auto extentN{lhsType->extent.numCols};
      auto extentP{rhsType->extent.numCols};
      auto result{Value::zero(context.get_arithmetic_type(scalar, Extent(extentP, extentM)))};
      lhs = construct(lhsType->get_with_different_scalar(scalar), lhs, srcLoc);
      rhs = construct(rhsType->get_with_different_scalar(scalar), rhs, srcLoc);
      auto lhsCols{llvm::SmallVector<Value>{}};
      auto rhsCols{llvm::SmallVector<Value>{}};
      lhsCols.resize(extentN);
      rhsCols.resize(extentP);
      for (unsigned k{0}; k < extentN; k++)
        lhsCols[k] = access(lhs, k);
      for (unsigned j{0}; j < extentP; j++)
        rhsCols[j] = extentP == 1 ? rhs : access(rhs, j);
      for (unsigned j{0}; j < extentP; j++) {
        auto resCols{llvm::SmallVector<Value>{}};
        resCols.resize(extentN);
        for (unsigned k{0}; k < extentN; k++)
          resCols[k] = emit_op(BinOp::Mul, lhsCols[k], access(rhsCols[j], k), srcLoc);
        for (unsigned k{1}; k < extentN; k++)
          resCols[0] = emit_op(BinOp::Add, resCols[0], resCols[k], srcLoc);
        result = extentP == 1 ? resCols[0] : insert(result, resCols[0], j);
      }
      return result;
    }
  }
  //--}
  //--{ Operators: Color
  if ((lhsType->is_color() && (rhsType->is_color() || rhsType->is_scalar())) || (lhsType->is_scalar() && rhsType->is_color())) {
    if (auto llvmOp{llvm_arithmetic_op(Scalar::Float, op)}) {
      lhs = construct(context.get_color_type(), lhs, srcLoc);
      rhs = construct(context.get_color_type(), rhs, srcLoc);
      return RValue(context.get_color_type(), builder.CreateBinOp(*llvmOp, lhs, rhs));
    }
  }
  //--}
  //--{ Operators: Pointer
  if ((op == BinOp::Add || op == BinOp::Sub) && lhsType->is_pointer() && rhsType == context.get_int_type()) {
    rhs = construct(context.get_int_type(), rhs, srcLoc);
    if (op == BinOp::Sub)
      rhs = emit_op(UnOp::Neg, rhs);
    return RValue(lhsType, builder.CreateGEP(lhsType->get_element_type()->llvmType, lhs, {rhs.llvmValue}));
  }
  if (lhsType->is_pointer() && rhsType->is_pointer()) {
    auto lhsElemType{lhsType->get_element_type()};
    auto rhsElemType{rhsType->get_element_type()};
    if (op == BinOp::Sub) {
      if (lhsElemType != rhsElemType)
        srcLoc.report_error("pointer subtraction requires both pointers to be the same type");
      return RValue(
          context.get_int_type(),
          builder.CreateIntCast(
              builder.CreatePtrDiff(lhsElemType->llvmType, lhs, rhs), context.get_int_type()->llvmType, /*isSigned=*/true));
    }
    if (auto llvmOp{llvm_compare_op(Scalar::Int, op, /*isSigned=*/false)}) {
      auto intPtrTy{builder.getIntNTy(sizeof(void *) * 8)};
      return RValue(
          context.get_bool_type(),
          builder.CreateCmp(
              *llvmOp,                               //
              builder.CreatePtrToInt(lhs, intPtrTy), //
              builder.CreatePtrToInt(rhs, intPtrTy)));
    }
  }
  //--}
  //--{ Operators: Type
  if (lhsType->is_compiler_type() && rhsType->is_compiler_type()) {
    if (auto llvmOp{llvm_compare_op(Scalar::Int, op)})
      return RValue(context.get_bool_type(), builder.CreateCmp(*llvmOp, lhs, rhs));
    if (lhs.is_compile_time() && rhs.is_compile_time()) {
      auto lhsTy{lhs.get_compile_time_type()};
      auto rhsTy{rhs.get_compile_time_type()};
      if (op == BinOp::Or)
        return context.get_compile_time_type(context.get_union_type({lhsTy, rhsTy}));
      if (op == BinOp::Subset)
        return context.get_compile_time_bool(context.is_subset_of(lhsTy, rhsTy));
    }
  }
  //--}
  srcLoc.report_error(std::format(
      "unimplemented binary operator '{}' between types '{}' and '{}'", AST::to_string(op), lhsType->name, rhsType->name));
  return {};
}
//--}

//--{ Emit: Intrinsics
Value Emitter::emit_intrinsic(const AST::Intrinsic &intr, const ArgList &args) {
  namespace Intr = llvm::Intrinsic;
  auto name{intr.name};
  if (!args.is_all_positional())
    intr.srcLoc.report_error("intrinsics expect only unnamed arguments");
  auto expectOne{[&]() {
    if (args.size() != 1)
      intr.srcLoc.report_error(std::format("intrinsic '#{}' expects 1 argument", name));
    return args[0].value;
  }};
  auto expectOneVectorized{[&]() {
    if (args.size() != 1 || !args[0].value.type->is_vectorized())
      intr.srcLoc.report_error(std::format("intrinsic '#{}' expects 1 vectorized argument", name));
    return args[0].value;
  }};
  auto expectOneIntOrIntVector{[&]() {
    if (args.size() != 1 || !args[0].value.type->is_vectorized() || args[0].value.type->scalar != Scalar::Int)
      intr.srcLoc.report_error(std::format("intrinsic '#{}' expects 1 int or int vector argument", name));
    return args[0].value;
  }};
  auto expectOneType{[&]() {
    auto value{rvalue(expectOne())};
    return value.is_compile_time_type() ? value.get_compile_time_type() : value.type;
  }};
  if (name == "panic") {
    if (args.size() != 1 || args[0].value.type != context.get_string_type())
      intr.srcLoc.report_error("intrinsic '#panic' expects 1 string argument");
    emit_panic(args[0].value, intr.srcLoc);
    return {};
  }
  //--{ Compile-time
  if (name == "alignof")
    return context.get_compile_time_int(context.get_align_of(expectOneType()));
  if (name == "sizeof")
    return context.get_compile_time_int(context.get_size_of(expectOneType()));
  if (name == "typeof")
    return context.get_compile_time_type(expectOne().type);
  if (name == "typename")
    return context.get_compile_time_string(expectOneType()->name);
  if (name == "comptime")
    return context.get_compile_time_bool(expectOne().is_compile_time());
  if (name == "is_array")
    return context.get_compile_time_bool(expectOneType()->is_array());
  if (name == "is_enum")
    return context.get_compile_time_bool(expectOneType()->is_enum());
  if (name == "is_matrix")
    return context.get_compile_time_bool(expectOneType()->is_matrix());
  if (name == "is_pointer")
    return context.get_compile_time_bool(expectOneType()->is_pointer());
  if (name == "is_scalar")
    return context.get_compile_time_bool(expectOneType()->is_scalar());
  if (name == "is_struct")
    return context.get_compile_time_bool(expectOneType()->is_struct());
  if (name == "is_union")
    return context.get_compile_time_bool(expectOneType()->is_union());
  if (name == "is_vector")
    return context.get_compile_time_bool(expectOneType()->is_vector());
  //--}
  //--{ Bit manipulation
  if (name == "bitreverse") {
    auto value{rvalue(expectOneIntOrIntVector())};
    return RValue(value.type, builder.CreateUnaryIntrinsic(Intr::bitreverse, value));
  }
  if (name == "ctpop") {
    auto value{rvalue(expectOneIntOrIntVector())};
    return RValue(value.type, builder.CreateUnaryIntrinsic(Intr::ctpop, value));
  }
  if (name == "ctlz") {
    auto value{rvalue(expectOneIntOrIntVector())};
    return RValue(value.type, builder.CreateBinaryIntrinsic(Intr::ctlz, value, context.get_compile_time_bool(false)));
  }
  if (name == "cttz") {
    auto value{rvalue(expectOneIntOrIntVector())};
    return RValue(value.type, builder.CreateBinaryIntrinsic(Intr::cttz, value, context.get_compile_time_bool(false)));
  }
  //--}
  //--{ Vector reduction
  if (auto intrID{[&]() -> std::optional<Intr::ID> {
        if (name == "all")
          return Intr::vector_reduce_and;
        if (name == "any")
          return Intr::vector_reduce_or;
        return std::nullopt;
      }()}) {
    auto value{expectOneVectorized()};
    value = construct(value.type->get_with_different_scalar(Scalar::Bool), value, intr.srcLoc);
    if (value.type->is_scalar())
      return value;
    return RValue(context.get_bool_type(), builder.CreateUnaryIntrinsic(*intrID, value));
  }
  if (auto intrIDs{[&]() -> std::optional<std::tuple<Intr::ID, Intr::ID>> {
        if (name == "min_value")
          return std::tuple(Intr::vector_reduce_fmin, Intr::vector_reduce_smin);
        if (name == "max_value")
          return std::tuple(Intr::vector_reduce_fmax, Intr::vector_reduce_smax);
        return std::nullopt;
      }()}) {
    auto value{rvalue(expectOneVectorized())};
    if (value.type->scalar == Scalar::Bool)
      value = construct(value.type->get_with_different_scalar(Scalar::Int), value);
    if (value.type->is_scalar())
      return value;
    return RValue(
        value.type->get_scalar_type(),
        builder.CreateUnaryIntrinsic(value.type->is_floating() ? std::get<0>(*intrIDs) : std::get<1>(*intrIDs), value));
  }
  if (auto intrIDs{[&]() -> std::optional<std::tuple<Intr::ID, Intr::ID, Intr::ID>> {
        if (name == "min")
          return std::tuple(Intr::minnum, Intr::umin, Intr::smin);
        if (name == "max")
          return std::tuple(Intr::maxnum, Intr::umax, Intr::smax);
        return std::nullopt;
      }()}) {
    if (args.size() != 2 || !args.is_all_true([](auto &arg) { return arg.value.type->is_vectorized(); }))
      intr.srcLoc.report_error(std::format("intrinsic '#{}' expects 2 vectorized arguments", name));
    auto lhs{args[0].value};
    auto rhs{args[1].value};
    auto type{context.get_common_type({lhs.type, rhs.type}, /*defaultToUnion=*/false, intr.srcLoc)};
    lhs = construct(type, lhs);
    rhs = construct(type, rhs);
    auto intrID{
        type->is_floating() ? std::get<0>(*intrIDs)
                            : (type->scalar == Scalar::Bool ? std::get<1>(*intrIDs) : std::get<2>(*intrIDs))};
    return RValue(type, builder.CreateBinaryIntrinsic(intrID, lhs, rhs));
  }
  if (name == "sum") {
    auto value{rvalue(expectOneVectorized())};
    if (value.type->scalar == Scalar::Bool)
      value = construct(value.type->get_with_different_scalar(Scalar::Int), value);
    if (value.type->is_scalar())
      return value;
    return RValue(
        value.type->get_scalar_type(), value.type->is_floating()
                                           ? builder.CreateFAddReduce(Value::zero(value.type->get_scalar_type()), value)
                                           : builder.CreateAddReduce(value));
  }
  if (name == "product") {
    auto value{rvalue(expectOneVectorized())};
    if (value.type->scalar == Scalar::Bool)
      value = construct(value.type->get_with_different_scalar(Scalar::Int), value);
    if (value.type->is_scalar())
      return value;
    return RValue(
        value.type->get_scalar_type(),
        value.type->is_floating()
            ? builder.CreateFMulReduce(construct(value.type->get_scalar_type(), context.get_compile_time_float(1)), value)
            : builder.CreateMulReduce(value));
  }
  //--}
  //--{ Math
  if (name == "isfpclass") {
    if (args.size() != 2 || !args[0].value.type->is_vectorized() || !args[1].value.is_compile_time_int())
      intr.srcLoc.report_error("intrinsic '#isfpclass' expects 1 vectorized argument and 1 compile-time integer argument");
    auto value{rvalue(args[0].value)};
    return RValue(
        value.type->get_with_different_scalar(Scalar::Bool),
        builder.createIsFPClass(value, args[1].value.get_compile_time_int()));
  }
  if (name == "frexp") {
    // TODO
  }
  if (name == "ldexp") {
    if (!(args.size() == 2 &&
          args.is_all_true([&](auto &arg) { return arg.value.type->is_scalar() || arg.value.type->is_vector(); }) &&
          args[0].value.type->get_vector_size() == args[1].value.type->get_vector_size()))
      intr.srcLoc.report_error(std::format("intrinsic '#{}' expects 2 same-size scalar or vector arguments", name));
    auto value0{rvalue(args[0].value)};
    auto value1{rvalue(args[1].value)};
    if (!value0.type->is_floating())
      value0 = construct(value0.type->get_with_different_scalar(Scalar::Float), value0, intr.srcLoc);
    value1 = construct(value1.type->get_with_different_scalar(Scalar::Int), value1, intr.srcLoc);
    return RValue(value0.type, llvm_emit_ldexp(builder, value0, value1)); // CreateBinaryIntrinsic doesn't work for ldexp!
  }
  if (name == "abs") {
    auto value{rvalue(expectOneVectorized())};
    return RValue(
        value.type, value.type->is_floating()
                        ? builder.CreateUnaryIntrinsic(Intr::fabs, value)
                        : builder.CreateBinaryIntrinsic(Intr::abs, value, context.get_compile_time_bool(false)));
  }
  for (auto [intrName, intrID] :
       std::array{std::pair("floor", Intr::floor), std::pair("ceil", Intr::ceil),                                    //
                  std::pair("trunc", Intr::trunc), std::pair("round", Intr::round), std::pair("sqrt", Intr::sqrt),   //
                  std::pair("sin", Intr::sin),     std::pair("cos", Intr::cos),     std::pair("tan", Intr::tan),     //
                  std::pair("asin", Intr::asin),   std::pair("acos", Intr::acos),   std::pair("atan", Intr::atan),   //
                  std::pair("sinh", Intr::sinh),   std::pair("cosh", Intr::cosh),   std::pair("tanh", Intr::tanh),   //
                  std::pair("exp", Intr::exp),     std::pair("exp2", Intr::exp2),   std::pair("exp10", Intr::exp10), //
                  std::pair("log", Intr::log),     std::pair("log2", Intr::log2),   std::pair("log10", Intr::log10)}) {
    if (name == intrName) {
      auto value{rvalue(expectOneVectorized())};
      if (!value.type->is_color())
        value = construct(value.type->get_with_different_scalar(Scalar::Float), value);
      return RValue(value.type, builder.CreateUnaryIntrinsic(intrID, value));
    }
  }
  if (name == "sign") {
    auto value{rvalue(expectOneVectorized())};
    if (!value.type->is_floating()) {
      return RValue(
          value.type, builder.CreateSelect(
                          emit_op(AST::BinaryOp::CmpLt, value, context.get_compile_time_int(0)),
                          construct(value.type, context.get_compile_time_int(-1)),
                          construct(value.type, context.get_compile_time_int(+1))));
    } else {
      return RValue(
          value.type,
          builder.CreateBinaryIntrinsic(Intr::copysign, construct(value.type, context.get_compile_time_float(1)), value));
    }
  }
  if (name == "atan2") {
    // TODO
  }
  if (name == "pow") {
    if (args.size() != 2 || !args[0].value.type->is_vectorized() || !args[1].value.type->is_vectorized())
      intr.srcLoc.report_error(std::format("intrinsic '#{}' expects 2 vectorized arguments", name));
    auto value0{rvalue(args[0].value)};
    auto value1{rvalue(args[1].value)};
    auto type{
        context.get_common_type({value0.type, value1.type, context.get_float_type()}, /*defaultToUnion=*/false, intr.srcLoc)};
    value0 = construct(type, value0, intr.srcLoc);
    return RValue(
        type, value1.type == context.get_int_type()
                  ? llvm_emit_powi(builder, value0, value1) // CreateBinaryIntrinsic doesn't work for powi!
                  : builder.CreateBinaryIntrinsic(Intr::pow, value0, construct(type, value1, intr.srcLoc)));
  }
  //--}
  //--{ Debug
  if (name == "assert") {
    if (!((args.size() == 1 && args[0].value.type == context.get_bool_type()) ||
          (args.size() == 2 && args[0].value.type == context.get_bool_type() &&
           args[1].value.type == context.get_string_type())))
      intr.srcLoc.report_error("intrinsic '#assert' expects 1 bool argument and 1 optional string argument");
    auto name{context.get_unique_name("assert", get_llvm_function())};
    auto blockFail{create_block(llvm_twine(name, ".fail"))};
    auto blockPass{create_block(llvm_twine(name, ".pass"))};
    emit_br(rvalue(args[0].value), blockPass, blockFail);
    move_to(blockFail);
    if (args.size() == 1) {
      std::string message{"assertion failed"};
      if (!args[0].src.empty()) {
        message += ": ";
        message += args[0].src.str();
      }
      emit_panic(message, intr.srcLoc);
    } else
      emit_panic(args[1].value, intr.srcLoc);
    emit_br(blockPass); // Never actually happens
    move_to(blockPass);
    return {};
  }
  if (name == "breakpoint") {
    if (!args.empty())
      intr.srcLoc.report_error("intrinsic '#breakpoint' expects no arguments");
    if (context.mdl.enableDebug)
      builder.CreateIntrinsic(context.get_type<void>()->llvmType, Intr::debugtrap, {});
    return context.get_compile_time_bool(true);
  }
  if (name == "print") {
    // TODO
  }
  //--}
  if (name == "select") {
    if (args.size() != 3 || !args.is_all_true([](auto arg) { return arg.value.type->is_vectorized(); }))
      intr.srcLoc.report_error("intrinsic '#select' expects 3 vectorized arguments");
    auto cond{construct(args[0].value.type->get_with_different_scalar(Scalar::Bool), args[0].value, intr.srcLoc)};
    auto ifPass{args[1].value};
    auto ifFail{args[2].value};
    auto type{context.get_common_type(
        {ifPass.type, ifFail.type, cond.type->is_vector() ? cond.type : nullptr}, /*defaultToUnion=*/false, intr.srcLoc)};
    ifPass = construct(type, ifPass, intr.srcLoc);
    ifFail = construct(type, ifFail, intr.srcLoc);
    return RValue(type, builder.CreateSelect(cond, ifPass, ifFail));
  }
  if (name == "inline" || name == "flatten") {
    auto value{expectOne()};
    inlines->push_back({value, intr.srcLoc, name == "flatten"});
    return value;
  }
  if (name == "print") {
    for (auto &arg : args)
      emit_print(arg.value);
    return context.get_compile_time_bool(true);
  }
  intr.srcLoc.report_error(std::format("unimplemented intrinsic '#{}'", name));
  return {};
}
//--}

void Emitter::emit_print(Value value) {
  if (value.type->is_string()) {
    auto callee{context.get_compile_time_callee(&Context::builtin_print_string)};
    builder.CreateCall(callee, {lvalue(value).llvmValue}); // Passes by pointer
  } else if (value.type->is_enum()) {
    emit_print(construct(context.get_string_type(), value));
  } else if (value.type->is_scalar()) {
    auto callee{llvm::FunctionCallee()};
    switch (value.type->scalar) {
    case Scalar::Bool: callee = context.get_compile_time_callee(&Context::builtin_print_bool); break;
    case Scalar::Int: callee = context.get_compile_time_callee(&Context::builtin_print_int); break;
    case Scalar::Float: callee = context.get_compile_time_callee(&Context::builtin_print_float); break;
    case Scalar::Double: callee = context.get_compile_time_callee(&Context::builtin_print_double); break;
    default: sanity_check(false); break;
    }
    if (value.type->scalar == Scalar::Bool)
      value = construct(context.get_int_type(), value);
    builder.CreateCall(callee, {rvalue(value).llvmValue});
  } else if (value.type->is_vector() || value.type->is_color()) {
    emit_print(context.get_compile_time_string(std::format("{}(", value.type->name)));
    emit_print(access(value, 0));
    for (uint32_t i{1}; i < value.type->get_vector_size(); i++) {
      emit_print(context.get_compile_time_string(", "));
      emit_print(access(value, i));
    }
    emit_print(context.get_compile_time_string(")"));
  } else if (value.type->is_matrix()) {
    // TODO
  } else if (value.type->is_struct()) {
    emit_print(context.get_compile_time_string(std::format("{}(", value.type->name)));
    auto structType{static_cast<StructType *>(value.type)};
    for (uint32_t i{}; i < structType->fields.size(); i++) {
      // TODO Quote strings
      emit_print(context.get_compile_time_string(std::format("{}: ", structType->fields[i].name)));
      emit_print(access(value, structType->fields[i].name));
      if (i + 1 < structType->fields.size())
        emit_print(context.get_compile_time_string(", "));
    }
    emit_print(context.get_compile_time_string(")"));
  } else if (value.type->is_union()) {
    // TODO
  }
}

} // namespace smdl::Compiler
