// vim:foldmethod=marker:foldlevel=0:fmr=--{,--}
#include "Function.h"
#include "Emitter.h"

namespace smdl::Compiler {

[[nodiscard]] static Function *find_prev_function(Crumb *crumb, const AST::Function &decl) {
  for (; crumb; crumb = crumb->prev) {
    if (crumb->matches_name(decl.name->name)) {
      if (crumb->value.is_compile_time_function())
        break;
      decl.srcLoc.report_error(std::format("function '{}' shadows non-function", decl.name->name));
    }
  }
  return crumb ? crumb->value.get_compile_time_function() : nullptr;
}

Function::Function(Emitter &emitter0, AST::Function &decl) : prev(find_prev_function(emitter0.crumb, decl)), decl(decl) {
  emitter0.context.validate_decl_name("function", *decl.name);
  emitter0.emit(*decl.returnType);
  for (auto &param : decl.params)
    emitter0.emit(*param.type);
  params = ParamList(emitter0.context, decl);
  validate_attributes();
  if (has_unique_concrete_instance())
    instances[params.get_types()] = FunctionInstance(emitter0, decl, params);
}

//--{ Construct 'FunctionInstance' from 'AST::Function' and 'ParamList'
FunctionInstance::FunctionInstance(Emitter &emitter0, AST::Function &decl, const ParamList &params)
    : name(decl.name->name), srcLoc(decl.srcLoc) {
  sanity_check(decl.params.size() == params.size());
  auto &context{emitter0.context};
  type = context.get_function_type(decl.attrs.isPure, decl.returnType->type, params.get_types());
  llvmFunction = type->create_default_llvm_function("");
  if (decl.definition) {
    llvm::SmallVector<Return> returns{};
    llvm::SmallVector<Inline> inlines{};
    Emitter emitter{context, decl.module, emitter0.crumb, &returns, &inlines, llvmFunction};
    {
      auto llvmArgItr{llvmFunction->arg_begin()};
      emitter.state = type->isPure ? Value() : RValue(context.get_type<state_t *>(), &*llvmArgItr++);
      for (auto &param : params)
        emitter.declare(param, &*llvmArgItr++);
    }
    emitter.emit(*decl.definition);
    if (!emitter.has_terminator()) {
      if (type->returnType != context.get_void_type())
        decl.srcLoc.report_error(std::format("function '{}' is missing 'return' statement", name));
      emitter.emit_unwind_and_br(emitter.afterReturn);
    }
    auto returnType{emitter.emit_return(type->returnType, returns, decl.srcLoc)};
    if (type->has_abstract_return_type()) // If necessary, patch concrete return type
      patch_return_type(returnType);
    force_inline(inlines);
  }
  llvmFunction->setName(name);
  if (decl.attrs.isAlwaysInline)
    llvmFunction->addFnAttr(llvm::Attribute::AlwaysInline);
  if (decl.attrs.isNoInline)
    llvmFunction->addFnAttr(llvm::Attribute::NoInline);
  if (decl.attrs.isHot)
    llvmFunction->addFnAttr(llvm::Attribute::Hot);
  if (decl.attrs.isCold)
    llvmFunction->addFnAttr(llvm::Attribute::Cold);
  if (decl.attrs.isOptSize)
    llvmFunction->addFnAttr(llvm::Attribute::OptimizeForSize);
  if (decl.attrs.isOptNone)
    llvmFunction->addFnAttr(llvm::Attribute::OptimizeNone);
  if (decl.attrs.isVisible)
    llvmFunction->setLinkage(llvm::Function::ExternalLinkage);
  if (decl.attrs.isForeign) {
    llvmFunction->setLinkage(llvm::Function::ExternalLinkage);
    llvmFunction->setDSOLocal(false);
  }
  {
    auto llvmArgItr{llvmFunction->arg_begin()};
    if (!type->isPure) {
      llvmArgItr->setName("state");
      llvmArgItr->addAttr(llvm::Attribute::ReadOnly);
      llvmArgItr->addAttr(llvm::Attribute::NoAlias);
      llvmArgItr++;
    }
    for (auto &param : decl.params) {
      llvmArgItr->setName(param.name->name);
      llvmArgItr++;
    }
  }
  if (decl.definition) {
    eliminate_unreachable();
    verify();
  }
}
//--}

//--{ Construct 'FunctionInstance' from 'AST::UnitTest'
FunctionInstance::FunctionInstance(Emitter &emitter0, AST::UnitTest &decl) : name(decl.desc), srcLoc(decl.srcLoc) {
  auto &context{emitter0.context};
  type = context.get_function_type(/*isPure=*/false, context.get_void_type(), {});
  llvmFunction = type->create_default_llvm_function(context.get_unique_name("unit-test"));
  llvmFunction->setLinkage(llvm::Function::ExternalLinkage);
  auto returns{llvm::SmallVector<Return>{}};
  auto inlines{llvm::SmallVector<Inline>{}};
  auto emitter{Emitter{context, decl.module, emitter0.crumb, &returns, &inlines, llvmFunction}};
  {
    // Initialize arguments.
    auto llvmArgItr{llvmFunction->arg_begin()};
    llvmArgItr->setName("state");
    llvmArgItr->addAttr(llvm::Attribute::ReadOnly);
    llvmArgItr->addAttr(llvm::Attribute::NoAlias);
    emitter.state = RValue(context.get_type<state_t *>(), &*llvmArgItr++);
  }
  emitter.emit(*decl.body);
  emitter.emit_unwind_and_br(emitter.afterReturn);
  emitter.emit_return(context.get_void_type(), returns, decl.srcLoc);
  force_inline(inlines);
  eliminate_unreachable();
  verify();
}
//--}

//--{ Construct 'FunctionInstance' from 'AST::Expr'
FunctionInstance::FunctionInstance(Emitter &emitter0, AST::Expr &expr) : name("(expr)"), srcLoc(expr.srcLoc) {
  auto &context{emitter0.context};
  type = context.get_function_type(/*isPure=*/true, context.get_auto_type(), {});
  llvmFunction = type->create_default_llvm_function("");
  auto returns{llvm::SmallVector<Return>{}};
  auto inlines{llvm::SmallVector<Inline>{}};
  auto emitter{Emitter{context, emitter0.module, emitter0.crumb, &returns, &inlines, llvmFunction}};
  auto returnValue{emitter.rvalue(emitter.emit(expr))};
  returns.push_back({returnValue, emitter.get_insert_block(), expr.srcLoc});
  emitter.emit_unwind_and_br(emitter.afterReturn);
  emitter.emit_return(returnValue.type, returns, expr.srcLoc);
  patch_return_type(returnValue.type);
  force_inline(inlines);
  eliminate_unreachable();
  verify();
}
//--}

//--{ Construct 'FunctionInstance' from 'EnumType' constant strings
FunctionInstance::FunctionInstance(Emitter &emitter0, EnumType *enumType) {
  auto &context{emitter0.context};
  type = context.get_function_type(/*isPure=*/true, context.get_string_type(), {enumType});
  llvmFunction = type->create_default_llvm_function("enum-to-string");
  auto returns{llvm::SmallVector<Return>{}};
  auto inlines{llvm::SmallVector<Inline>{}};
  auto emitter{Emitter{context, emitter0.module, emitter0.crumb, &returns, &inlines, llvmFunction}};
  {
    auto blockDefault{emitter.create_block("switch.default")};
    auto inst{emitter.builder.CreateSwitch(llvmFunction->getArg(0), blockDefault)};
    for (size_t i{}; i < enumType->constants.size(); i++) {
      auto blockCase{emitter.create_block(std::format("switch.case.{}", i))};
      inst->addCase(llvm::dyn_cast<llvm::ConstantInt>(enumType->constants[i].llvmConst), blockCase);
      returns.push_back(
          {context.get_compile_time_string(enumType->constants[i].name), blockCase, enumType->constants[i].srcLoc});
      emitter.move_to(blockCase);
      emitter.emit_unwind_and_br(emitter.afterReturn);
    }
    llvm_move_block_to_end(blockDefault);
    returns.push_back({context.get_compile_time_string(""), blockDefault, {}});
    emitter.move_to(blockDefault);
    emitter.emit_unwind_and_br(emitter.afterReturn);
  }
  emitter.emit_return(context.get_string_type(), returns);
  eliminate_unreachable();
  verify();
}
//--}

void FunctionInstance::patch_return_type(Type *returnType) {
  auto &context{type->context};
  sanity_check(returnType);
  sanity_check(!returnType->is_abstract());
  auto llvmFuncPrev{llvmFunction};
  type = context.get_function_type(type->isPure, returnType, type->paramTypes);
  llvmFunction = type->create_default_llvm_function("");
  llvmFunction->splice(llvmFunction->begin(), llvmFuncPrev);
  auto llvmArgItr0{llvmFuncPrev->arg_begin()};
  auto llvmArgItr1{llvmFunction->arg_begin()};
  while (llvmArgItr1 != llvmFunction->arg_end()) {
    llvmArgItr0->replaceAllUsesWith(&*llvmArgItr1);
    ++llvmArgItr0;
    ++llvmArgItr1;
  }
  llvmFuncPrev->replaceAllUsesWith(llvmFunction);
  llvmFuncPrev->eraseFromParent();
}

void FunctionInstance::force_inline(llvm::ArrayRef<Inline> inlines) {
  for (auto &[value, srcLoc, isRecursive] : inlines) {
    if (auto call{llvm::dyn_cast<llvm::CallBase>(value.llvmValue)}) {
      auto result{llvm_force_inline(*call, isRecursive)};
      if (!result.isSuccess())
        srcLoc.report_warning(llvm::errs(), std::format("can't force inline: {}", result.getFailureReason()));
    } else {
      srcLoc.report_warning(llvm::errs(), "can't force inline: expected 'llvm::CallBase'");
    }
  }
}

Value FunctionInstance::call(Emitter &emitter, llvm::ArrayRef<Value> argValues, const AST::SourceLocation &srcLoc) {
  sanity_check(llvmFunction);
  auto llvmArgs{llvm::SmallVector<llvm::Value *>{}};
  if (!type->isPure) {
    if (!emitter.state)
      srcLoc.report_error(std::format("call to function '{}' from '@(pure)' context", name));
    llvmArgs.push_back(emitter.state);
  }
  llvmArgs.insert(llvmArgs.end(), argValues.begin(), argValues.end());
  return RValue(
      type->returnType, emitter.builder.CreateCall(static_cast<llvm::FunctionType *>(type->llvmType), llvmFunction, llvmArgs));
}

Value Function::call(Emitter &emitter0, const ArgList &args, const AST::SourceLocation &srcLoc) {
  auto &context{emitter0.context};
  // Get all overloads.
  llvm::SmallVector<std::pair<Function *, llvm::SmallVector<Type *>>> overloads{};
  for (auto func{this}; func; func = func->prev) {
    llvm::SmallVector<Type *> argParamTypes{};
    if (context.can_resolve_arguments(emitter0, func->params, args, srcLoc, &argParamTypes))
      overloads.emplace_back(func, std::move(argParamTypes));
  }

  // If no matching declarations, overload resolution fails.
  if (overloads.empty())
    srcLoc.report_error(std::format("call to function '{}' has no viable overloads", get_name()));

  // The lambda to determine whether the LHS set of parameter types is strictly less specific than
  // the RHS set of parameter types. This is true if each and every RHS parameter type is implicitly
  // convertible to the corresponding LHS parameter type.
  auto isLessSpecific{[&](llvm::ArrayRef<Type *> lhsParamTypes, //
                          llvm::ArrayRef<Type *> rhsParamTypes) {
    sanity_check(lhsParamTypes.size() == rhsParamTypes.size());
    for (size_t i{}; i < lhsParamTypes.size(); i++)
      if (!context.is_implicitly_convertible(rhsParamTypes[i], lhsParamTypes[i]))
        return false;
    return true;
  }};

  // If there is more than 1 matching declaration, remove the less-specific of the first 2 overloads repeatedly
  // until 1 remains. If neither is less specific, the declarations are considered to match ambiguously and overload
  // resolution fails.
  while (overloads.size() > 1) {
    auto &paramTypes0{overloads[0].second};
    auto &paramTypes1{overloads[1].second};
    if (isLessSpecific(paramTypes0, paramTypes1)) {
      overloads.erase(overloads.begin());
    } else if (isLessSpecific(paramTypes1, paramTypes0)) {
      overloads.erase(overloads.begin() + 1);
    } else {
      srcLoc.report_error(std::format("call to function '{}' has ambiguous overloads", get_name()));
    }
  }

  auto &overload{*overloads[0].first};
  if (!overload.is_pure() && !emitter0.state)
    srcLoc.report_error(std::format("call to function '{}' from '@(pure)' context", get_name()));
  auto argValues{context.resolve_arguments(emitter0, overload.params, args, srcLoc)};
  auto argParams{[&]() -> ParamList {
    sanity_check(overload.params.size() == argValues.size());
    auto params{overload.params};
    for (size_t i = 0; i < params.size(); i++) {
      sanity_check(params[i].type->is_abstract() || params[i].type == argValues[i].type);
      params[i].type = argValues[i].type;
      params[i].init = nullptr;
    }
    return params;
  }()};

  if (overload.is_macro()) {
    auto name{context.get_unique_name("macro", emitter0.get_llvm_function())};
    auto blockBegin{emitter0.create_block(llvm_twine(name, ".begin"))};
    auto blockEnd{emitter0.create_block(llvm_twine(name, ".end"))};
    emitter0.emit_br(blockBegin);
    llvm::SmallVector<Return> returns{};
    {
      Emitter emitter1{&emitter0};
      emitter1.crumb = decl.crumb;
      emitter1.state = overload.is_pure() ? Value() : emitter0.state;
      emitter1.returns = &returns;
      emitter1.afterBreak = {};    // Reset 'break'
      emitter1.afterContinue = {}; // Reset 'continue'
      emitter1.afterEndScope = {}; // Reset end-scope
      emitter1.afterReturn = {decl.crumb, blockEnd};
      emitter1.move_to(blockBegin);
      for (size_t i = 0; i < argValues.size(); i++)
        emitter1.declare(argParams[i], argValues[i].llvmValue);
      emitter1.emit(overload.decl.definition);
      if (!emitter1.has_terminator()) {
        if (overload.decl.returnType->type != context.get_void_type())
          overload.decl.srcLoc.report_error(std::format("function '{}' is missing 'return' statement", get_name()));
        emitter1.emit_unwind_and_br(emitter1.afterReturn);
      }
    }
    emitter0.move_to(blockEnd);
    return emitter0.emit_return_phi(overload.decl.returnType->type, returns, overload.decl.returnType->srcLoc);
  } else {
    auto &instance{overload.instances[argParams.get_types()]};
    if (!instance)
      instance = FunctionInstance(emitter0, overload.decl, argParams);
    return instance.call(emitter0, argValues, srcLoc);
  }
}

Value Function::compile_time_evaluate(Emitter &emitter, AST::Expr &expr) {
  if (llvm::isa<AST::Identifier>(&expr) ||                                         //
      llvm::isa<AST::LiteralBool>(&expr) || llvm::isa<AST::LiteralFloat>(&expr) || //
      llvm::isa<AST::LiteralInt>(&expr) || llvm::isa<AST::LiteralString>(&expr)) {
    auto result{emitter.emit(expr)};
    if (!result.is_compile_time())
      return {};
    return result;
  }
  // Compile the expression into a temporary function with no arguments, then
  // flatten everything and aggressively optimize.
  auto &context{emitter.context};
  FunctionInstance instance{emitter, expr};
  instance.force_inline_flatten();
  instance.optimize(llvm::OptimizationLevel::O3);
  // Then feed to the compile-time evaluator.
  Value resultValue{};
  llvm::Constant *result{};
  llvm::SmallVector<llvm::Constant *> args{};
  if (llvm::Evaluator eval{context.llvmLayout, &context.llvmTargetLibraryInfo};
      eval.EvaluateFunction(instance.llvmFunction, result, args))
    resultValue = RValue(instance.type->returnType, result);
  instance.llvmFunction->eraseFromParent(); // And finally erase the temporary function.
  return resultValue;
}

void Function::validate_attributes() {
  // '@(foreign)' ...
  if (decl.attrs.isForeign) {
    // ... must not have definition
    if (decl.definition)
      decl.srcLoc.report_error(std::format("function '{}' declared '@(foreign)' must not have definition", get_name()));
    // ... must not use abstract (inferred) types
    if (params.has_any_abstract() || decl.returnType->type->is_abstract())
      decl.srcLoc.report_error(std::format("function '{}' declared '@(foreign)' must not use abstract types", get_name()));
    auto checkNoCollision{[&](bool attr, const char *name) {
      if (attr)
        decl.srcLoc.report_error(
            std::format("function '{}' declared '@(foreign)' must not be declared '@({})'", get_name(), name));
    }};
    checkNoCollision(decl.attrs.isMacro, "macro");               // ... must not be '@(macro)'
    checkNoCollision(decl.attrs.isNoInline, "noinline");         // ... must not be '@(noinline)' (it always is?)
    checkNoCollision(decl.attrs.isAlwaysInline, "alwaysinline"); // ... must not be '@(alwaysinline)'
    checkNoCollision(decl.attrs.isHot, "hot");                   // ... must not be '@(hot)
    checkNoCollision(decl.attrs.isCold, "cold");                 // ... must not be '@(cold)'
    checkNoCollision(decl.attrs.isOptNone, "optnone");           // ... must not be '@(optnone)'
    checkNoCollision(decl.attrs.isOptSize, "optsize");           // ... must not be '@(optsize)'
    checkNoCollision(decl.attrs.isVisible, "visible");           // ... must not be '@(visible)'
  }
  // '@(macro)' ...
  if (decl.attrs.isMacro) {
    // ... must have definition
    if (!decl.definition)
      decl.srcLoc.report_error(std::format("function '{}' declared '@(macro)' must have definition", get_name()));
    auto checkNoCollision{[&](bool attr, const char *name) {
      if (attr)
        decl.srcLoc.report_error(
            std::format("function '{}' declared '@(macro)' must not be declared '@({})'", get_name(), name));
    }};
    checkNoCollision(decl.attrs.isNoInline, "noinline");         // ... must not be '@(noinline)'
    checkNoCollision(decl.attrs.isAlwaysInline, "alwaysinline"); // ... must not be '@(alwaysinline)' (it always is?)
    checkNoCollision(decl.attrs.isHot, "hot");                   // ... must not be '@(hot)
    checkNoCollision(decl.attrs.isCold, "cold");                 // ... must not be '@(cold)'
    checkNoCollision(decl.attrs.isOptNone, "optnone");           // ... must not be '@(optnone)'
    checkNoCollision(decl.attrs.isOptSize, "optsize");           // ... must not be '@(optsize)'
    checkNoCollision(decl.attrs.isVisible, "visible");           // ... must not be '@(visible)'
  }

  {
    auto checkNoCollision{[&](bool attr0, bool attr1, const char *name0, const char *name1) {
      if (attr0 && attr1)
        decl.srcLoc.report_error(
            std::format("function '{}' must not be declared both '@({})' and '@({})'", get_name(), name0, name1));
    }};
    // Verify no '@(hot cold)'
    // Verify no '@(alwaysinline noinline)'
    // Verify no '@(alwaysinline visible)'
    // Verify no '@(optnone optsize)'
    checkNoCollision(decl.attrs.isHot, decl.attrs.isCold, "hot", "cold");
    checkNoCollision(decl.attrs.isAlwaysInline, decl.attrs.isNoInline, "alwaysinline", "noinline");
    checkNoCollision(decl.attrs.isAlwaysInline, decl.attrs.isVisible, "alwaysinline", "visible");
    checkNoCollision(decl.attrs.isOptNone, decl.attrs.isOptSize, "optnone", "optsize");
  }
}

} // namespace smdl::Compiler
