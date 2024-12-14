// vim:foldmethod=marker:foldlevel=0:fmr=--{,--}
#include "Function.h"
#include "Emitter.h"

namespace smdl::Compiler {

void FunctionInstance::initialize(
    Emitter &emitter0, AST::Function &decl, const ParamList &params, llvm::ArrayRef<Type *> argTypes) {
  name = decl.name.srcName;
  srcLoc = decl.srcLoc;
  sanity_check(decl.params.size() == params.size());
  sanity_check(decl.params.size() == argTypes.size());
  auto &context{emitter0.context};
  type = context.get_function_type(decl.has_attr("pure"), decl.returnType->type, argTypes);
  // If the function is declared `@(foreign)`, first try to look it up in the LLVM module to see
  // if it was already declared earlier. Redundant declarations like this should be OK and should
  // resolve to the same function.
  if (decl.has_attr("foreign")) {
    if (llvmFunc = context.mdl.get_llvm_module().getFunction(name); llvmFunc) {
      // However, if the declaration clashes with the earlier declaration, that is a problem! Foreign
      // C functions must be uniqued by type signature without overloads.
      if (llvmFunc->getFunctionType() != type->llvmType)
        srcLoc.report_error(
            std::format("redundant '@(foreign)' function '{}' is not compatible with earlier declaration", name));
      return;
    }
  }
  // TODO Declarations of non-foreign functions don't work yet?

  // Now create the LLVM function. NOTE: We don't bother naming it yet because we might re-create it
  // if we need to infer the return type. (See `patch_return_type()`)
  llvmFunc = type->create_default_llvm_function("");

  if (decl.definition) {
    // If we have a definition, it is time to compile the LLVM function.
    auto returns{llvm::SmallVector<Return>{}};
    auto inlines{llvm::SmallVector<Inline>{}};
    auto emitter{Emitter{context, decl.module, decl.crumb, &returns, &inlines, llvmFunc}};

    // Now initialize the function arguments, and keep track of whether there is any implied `visit`
    // type switching.
    auto impliedVisit{false};
    auto impliedVisitArgs{ArgList{}};
    {
      auto llvmArgItr{llvmFunc->arg_begin()};
      // If the function is `@(pure)` then it does NOT have an implicit state argument. In which case,
      // we set the `emitter.state` to the null `Value()`. Otherwise, the implicit state argument is
      // always the first argument of the function.
      emitter.state = type->isPure ? Value() : RValue(context.get_type<state_t *>(), &*llvmArgItr++);
      for (size_t i{}; i < params.size(); i++) {
        auto argValue{RValue(argTypes[i], &*llvmArgItr++)};
        emitter.declare_function_parameter(params[i], argValue);
        impliedVisitArgs.emplace_back(params[i].name, argValue, params[i].srcLoc);

        // To understand when implied type switching occurs, consider the following setup:
        // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        // tag Animal;
        // struct Cat: Animal { /* ... */ };
        // struct Dog: Animal { /* ... */ };
        // void do_something(Animal animal) {
        //   /* ... */
        // }
        // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        // In this example, `do_something` is a generic function of an `Animal`. This means that
        // inside the function body, the user expects to be processing some concrete instance of a
        // struct that is tagged `Animal`. Consider the runtime union `(Cat | Dog)`, which is always
        // either `Cat` or `Dog`, both of which are definitely `Animal`s. By the SMDL typing paradigm,
        // it is legal to invoke `do_something()` with such a union and expect it to automatically
        // type switch to `Cat` or `Dog`.
        //
        // So the condition is: Is the parameter type abstract and is the argument type a
        // union or a pointer to a union? (And at this point everything should be guaranteed
        // to be semantically valid).
        if (params[i].type->is_abstract() && argTypes[i]->is_union_or_pointer_to_union()) {
          impliedVisit = true;
          impliedVisitArgs.args.back().isVisit = true;
        }
      }
    }
    // If there is implied `visit` type switching, we recurse to `emitter.emit_call` again with `impliedVisitArgs`.
    if (impliedVisit) {
      emitter.emit_return(
          emitter.emit_call(context.get_compile_time_function(context.get_function(emitter, &decl)), impliedVisitArgs, srcLoc),
          srcLoc);
    } else {
      emitter.emit(*decl.definition);
      if (!emitter.has_terminator()) {
        if (type->returnType != context.get_void_type())
          srcLoc.report_error(std::format("function '{}' is missing 'return' statement", name));
        emitter.emit_unwind_and_br(emitter.afterReturn);
      }
    }
    auto returnType{emitter.emit_final_return(type->returnType, returns, srcLoc)};
    if (type->has_abstract_return_type()) // If necessary, patch concrete return type
      patch_return_type(returnType);
    force_inline(inlines);
  }

  llvmFunc->setName(name);
  if (decl.has_attr("alwaysinline"))
    llvmFunc->addFnAttr(llvm::Attribute::AlwaysInline);
  if (decl.has_attr("noinline"))
    llvmFunc->addFnAttr(llvm::Attribute::NoInline);
  if (decl.has_attr("hot"))
    llvmFunc->addFnAttr(llvm::Attribute::Hot);
  if (decl.has_attr("cold"))
    llvmFunc->addFnAttr(llvm::Attribute::Cold);
  if (decl.has_attr("optsize"))
    llvmFunc->addFnAttr(llvm::Attribute::OptimizeForSize);
  if (decl.has_attr("optnone"))
    llvmFunc->addFnAttr(llvm::Attribute::OptimizeNone);
  if (decl.has_attr("visible"))
    llvmFunc->setLinkage(llvm::Function::ExternalLinkage);
  if (decl.has_attr("foreign")) {
    llvmFunc->setLinkage(llvm::Function::ExternalLinkage);
    llvmFunc->setDSOLocal(false);
  }
  {
    auto llvmArgItr{llvmFunc->arg_begin()};
    if (!type->isPure) {
      llvmArgItr->setName("state");
      llvmArgItr->addAttr(llvm::Attribute::ReadOnly);
      llvmArgItr->addAttr(llvm::Attribute::NoAlias);
      llvmArgItr++;
    }
    for (auto &param : decl.params) {
      llvmArgItr->setName(param.name.srcName);
      llvmArgItr++;
    }
  }
  if (decl.definition) {
    eliminate_unreachable();
    verify();
  }
}

void FunctionInstance::initialize(Emitter &emitter0, AST::UnitTest &decl) {
  name = decl.name->value;
  srcLoc = decl.srcLoc;
  auto &context{emitter0.context};
  type = context.get_function_type(/*isPure=*/false, context.get_void_type(), {});
  llvmFunc = type->create_default_llvm_function(context.get_unique_name("unit-test"));
  llvmFunc->setLinkage(llvm::Function::ExternalLinkage);
  auto returns{llvm::SmallVector<Return>{}};
  auto inlines{llvm::SmallVector<Inline>{}};
  auto emitter{Emitter{context, decl.module, decl.crumb, &returns, &inlines, llvmFunc}};
  {
    // Initialize arguments.
    auto llvmArgItr{llvmFunc->arg_begin()};
    llvmArgItr->setName("state");
    llvmArgItr->addAttr(llvm::Attribute::ReadOnly);
    llvmArgItr->addAttr(llvm::Attribute::NoAlias);
    emitter.state = RValue(context.get_type<state_t *>(), &*llvmArgItr++);
  }
  emitter.emit(*decl.body);
  emitter.emit_unwind_and_br(emitter.afterReturn);
  emitter.emit_final_return(context.get_void_type(), returns, decl.srcLoc);
  force_inline(inlines);
  eliminate_unreachable();
  verify();
}

void FunctionInstance::initialize(Emitter &emitter0, AST::Expr &expr) {
  name = "(expr)";
  srcLoc = expr.srcLoc;
  auto &context{emitter0.context};
  type = context.get_function_type(/*isPure=*/true, context.get_auto_type(), {});
  llvmFunc = type->create_default_llvm_function("");
  auto returns{llvm::SmallVector<Return>{}};
  auto inlines{llvm::SmallVector<Inline>{}};
  auto emitter{Emitter{context, emitter0.module, emitter0.crumb, &returns, &inlines, llvmFunc}};
  auto returnValue{emitter.rvalue(emitter.emit(expr))};
  returns.push_back({returnValue, emitter.get_insert_block(), expr.srcLoc});
  emitter.emit_unwind_and_br(emitter.afterReturn);
  emitter.emit_final_return(returnValue.type, returns, expr.srcLoc);
  patch_return_type(returnValue.type);
  force_inline(inlines);
  eliminate_unreachable();
  verify();
}

void FunctionInstance::initialize(Emitter &emitter0, EnumType *enumType) {
  name = "(enum to string)";
  auto &context{emitter0.context};
  type = context.get_function_type(/*isPure=*/true, context.get_string_type(), {enumType});
  llvmFunc = type->create_default_llvm_function("enum-to-string");
  auto returns{llvm::SmallVector<Return>{}};
  auto inlines{llvm::SmallVector<Inline>{}};
  auto emitter{Emitter{context, emitter0.module, emitter0.crumb, &returns, &inlines, llvmFunc}};
  {
    auto blockDefault{emitter.create_block("switch.default")};
    auto inst{emitter.builder.CreateSwitch(llvmFunc->getArg(0), blockDefault)};
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
  emitter.emit_final_return(context.get_string_type(), returns);
  eliminate_unreachable();
  verify();
}

void FunctionInstance::patch_return_type(Type *returnType) {
  auto &context{type->context};
  sanity_check(returnType);
  sanity_check(!returnType->is_abstract());
  auto llvmFuncPrev{llvmFunc};
  type = context.get_function_type(type->isPure, returnType, type->paramTypes);
  llvmFunc = type->create_default_llvm_function("");
  llvmFunc->splice(llvmFunc->begin(), llvmFuncPrev);
  auto llvmArgItr0{llvmFuncPrev->arg_begin()};
  auto llvmArgItr1{llvmFunc->arg_begin()};
  while (llvmArgItr1 != llvmFunc->arg_end()) {
    llvmArgItr0->replaceAllUsesWith(&*llvmArgItr1);
    ++llvmArgItr0;
    ++llvmArgItr1;
  }
  llvmFuncPrev->replaceAllUsesWith(llvmFunc);
  llvmFuncPrev->eraseFromParent();
}

void FunctionInstance::force_inline(llvm::ArrayRef<Inline> inlines) {
  for (auto &[value, srcLoc, isRecursive] : inlines) {
    if (auto call{llvm::dyn_cast<llvm::CallBase>(value.llvmValue)}) {
      auto result{llvm_force_inline(*call, isRecursive)};
      if (!result.isSuccess())
        srcLoc.report_warning(std::format("can't force inline: {}", result.getFailureReason()));
    } else {
      srcLoc.report_warning("can't force inline: expected 'llvm::CallBase'");
    }
  }
}

Value FunctionInstance::call(Emitter &emitter, llvm::ArrayRef<Value> argValues, const AST::SourceLocation &srcLoc) {
  auto llvmArgs{llvm::SmallVector<llvm::Value *>{}};
  if (!type->isPure) {
    if (!emitter.state)
      srcLoc.report_error(std::format("call to function '{}' from '@(pure)' context", name));
    llvmArgs.push_back(emitter.state);
  }
  llvmArgs.insert(llvmArgs.end(), argValues.begin(), argValues.end());
  sanity_check_nonnull(llvmFunc);
  sanity_check(llvmFunc->arg_size() == llvmArgs.size());
  return RValue(
      type->returnType, emitter.builder.CreateCall(static_cast<llvm::FunctionType *>(type->llvmType), llvmFunc, llvmArgs));
}

Function::Function(Emitter &emitter0, AST::Function &decl) : decl(decl) {
  emitter0.emit(*decl.returnType);
  for (const auto &param : decl.params)
    emitter0.emit(*param.type);
  params = ParamList(emitter0.context, decl);
  validate_attributes();
  if (auto prevCrumb{Crumb::find(emitter0.crumb, decl.name.srcName, nullptr)}) {
    if (prevCrumb->value.is_compile_time_function())
      prev = prevCrumb->value.get_compile_time_function();
    else
      decl.srcLoc.report_error(std::format("function '{}' shadows non-function by the same name", decl.name.srcName));
  }
  // Only forward-link prev to this if the declaration is in the same module. This prevents
  // overloads of functions in other modules from interfering with code outside of the
  // current module.
  if (prev && prev->decl.module == decl.module)
    prev->next = this;
  if ((is_variant() && prev) || (prev && prev->is_variant()))
    decl.srcLoc.report_error(std::format("function variant '{}' must not be overloaded", get_name()));
  if (is_variant()) {
    letAndCall = decl.get_variant_let_and_call_expressions();
    sanity_check(has_definition());
  }
  // NOTE: This is kind of ugly, but for now we have to do function declaration here so that
  // recursion works, i.e., the function knows about itself. Probably a way to clean this up.
  emitter0.declare(decl.name, emitter0.context.get_compile_time_function(this), &decl);
  decl.crumb = emitter0.crumb;

  // If this function has a unique concrete instance and is marked `@(visible)` or
  // is marked `@(foreign)` or represents a concrete material, then compile it immediately.
  if ((has_unique_concrete_instance() && is_visible()) || is_foreign() || represents_material()) {
    auto argTypes{params.get_types()};
    instances[argTypes].initialize(emitter0, decl, params, argTypes);
  }
}

bool Function::represents_material() const {
  return has_unique_concrete_instance() && params.empty() &&
         get_return_type() == get_return_type()->context.get_material_type();
}

Function *Function::resolve_overload(Emitter &emitter0, const ArgList &args, const AST::SourceLocation &srcLoc) {
  auto &context{emitter0.context};
  if (is_variant())
    return this;
  // Get all overloads.
  auto overloads{llvm::SmallVector<std::pair<Function *, llvm::SmallVector<Type *>>>{}};
  for (auto func{get_bottom_overload()}; func; func = func->prev) {
    sanity_check(!func->is_variant());
    llvm::SmallVector<Type *> argParamTypes{};
    if (context.can_resolve_arguments(emitter0, func->params, args, srcLoc, &argParamTypes))
      overloads.emplace_back(func, std::move(argParamTypes));
  }

  // If no matching declarations, overload resolution fails.
  if (overloads.empty())
    srcLoc.report_error(std::format("call to function '{}' has no viable overloads for '{}'", get_name(), std::string(args)));

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
  return overloads[0].first;
}

Value Function::call(Emitter &emitter0, const ArgList &args, const AST::SourceLocation &srcLoc) {
  auto &context{emitter0.context};
  if (is_variant()) {
    sanity_check_nonnull(letAndCall.call);
    auto name{context.get_unique_name("function-variant", emitter0.get_llvm_function())};
    auto blockBegin{emitter0.create_block(llvm_twine(name, ".begin"))};
    auto blockEnd{emitter0.create_block(llvm_twine(name, ".end"))};
    emitter0.emit_br(blockBegin);
    Emitter emitter1{&emitter0};
    emitter1.module = decl.module;
    emitter1.crumb = decl.crumb;
    emitter1.state = is_pure() ? Value() : emitter0.state;
    emitter1.afterEndScope = {decl.crumb, blockEnd};
    emitter1.move_to(blockBegin);

    // If the function variant has a `let` expression, generate the variable declarations.
    if (letAndCall.let)
      for (auto &var : letAndCall.let->vars)
        emitter1.emit(var);
    // In the function variant call expression, we visit each argument in the AST argument list and add it
    // to the patched argument list but only if the caller did not explicitly set it by name.
    auto patchedArgs{args};
    for (auto &astArg : letAndCall.call->args.args)
      if (!patchedArgs.has_name(astArg.name.srcName))
        patchedArgs.emplace_back(astArg.name.srcName, emitter1.emit(astArg.expr), astArg.srcLoc, astArg.src);

    auto callee{emitter1.emit(letAndCall.call->expr)};
    auto result{emitter1.emit_call(callee, patchedArgs, srcLoc)};
    sanity_check(!emitter1.has_terminator());
    emitter1.emit_unwind_and_br(emitter1.afterEndScope);
    emitter0.move_to(blockEnd);
    return emitter0.construct(decl.returnType->type, result);
  } else {
    auto &overload{*resolve_overload(emitter0, args, srcLoc)};
    if (!overload.is_pure() && !emitter0.state)
      srcLoc.report_error(std::format("call to function '{}' from '@(pure)' context", get_name()));
    auto argValues{context.resolve_arguments(emitter0, overload.params, args, srcLoc)};

    if (overload.is_macro()) {
      auto name{context.get_unique_name("macro", emitter0.get_llvm_function())};
      auto blockBegin{emitter0.create_block(llvm_twine(name, ".begin"))};
      auto blockEnd{emitter0.create_block(llvm_twine(name, ".end"))};
      emitter0.emit_br(blockBegin);
      llvm::SmallVector<Return> returns{};
      {
        Emitter emitter1{&emitter0};
        emitter1.module = overload.decl.module;
        emitter1.crumb = overload.decl.crumb;
        emitter1.state = overload.is_pure() ? Value() : emitter0.state;
        emitter1.returns = &returns;
        emitter1.afterBreak = {};    // Reset break
        emitter1.afterContinue = {}; // Reset continue
        emitter1.afterEndScope = {}; // Reset end-scope
        emitter1.afterReturn = {overload.decl.crumb, blockEnd};
        emitter1.macroRecursionDepth++;
        if (emitter1.macroRecursionDepth > 4096)
          srcLoc.report_error(std::format("call to function '{}' exceeds macro recursion limit of 4096", get_name()));
        emitter1.move_to(blockBegin);
        if (!emitter1.state && !overload.is_pure())
          srcLoc.report_error(std::format("call to function '{}' from '@(pure)' context", get_name()));

        // Declare function parameters
        auto impliedVisit{false};
        auto impliedVisitArgs{ArgList{}};
        for (size_t i = 0; i < argValues.size(); i++) {
          emitter1.declare_function_parameter(overload.params[i], argValues[i]);
          impliedVisitArgs.emplace_back(overload.params[i].name, argValues[i], overload.params[i].srcLoc);
          if (overload.params[i].type->is_abstract() && argValues[i].type->is_union_or_pointer_to_union()) {
            impliedVisit = true;
            impliedVisitArgs.args.back().isVisit = true;
          }
        }
        if (impliedVisit) {
          emitter1.emit_return(
              emitter1.emit_call(context.get_compile_time_function(&overload), impliedVisitArgs, srcLoc), srcLoc);
        } else {
          emitter1.emit(overload.decl.definition);
          if (!emitter1.has_terminator()) {
            if (overload.decl.returnType->type != context.get_void_type())
              overload.decl.srcLoc.report_error(std::format("function '{}' is missing 'return' statement", get_name()));
            emitter1.emit_unwind_and_br(emitter1.afterReturn);
          }
        }
      }
      llvm_move_block_to_end(blockEnd);
      emitter0.move_to(blockEnd);
      return emitter0.emit_final_return_phi(overload.decl.returnType->type, returns, overload.decl.returnType->srcLoc);
    } else {
      llvm::SmallVector<Type *> argTypes{};
      for (auto argValue : argValues)
        argTypes.push_back(argValue.type);
      auto &instance{overload.instances[argTypes]};
      if (!instance)
        instance.initialize(emitter0, overload.decl, overload.params, argTypes);
      return instance.call(emitter0, argValues, srcLoc);
    }
  }
}

Value Function::compile_time_evaluate(Emitter &emitter, AST::Expr &expr) {
  if (llvm::isa<AST::Parens>(&expr))
    return compile_time_evaluate(emitter, *static_cast<AST::Parens *>(&expr)->expr);
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
  auto instance{FunctionInstance{}};
  instance.initialize(emitter, expr);
  instance.force_inline_flatten();
  instance.optimize(llvm::OptimizationLevel::O3);
  // Then feed to the compile-time evaluator.
  Value resultValue{};
  llvm::Constant *result{};
  llvm::SmallVector<llvm::Constant *> args{};
  if (llvm::Evaluator eval{context.llvmLayout, &context.llvmTargetLibraryInfo};
      eval.EvaluateFunction(instance.llvmFunc, result, args))
    resultValue = RValue(instance.type->returnType, result);
  instance.llvmFunc->eraseFromParent(); // And finally erase the temporary function.
  return resultValue;
}

void Function::validate_attributes() {
  // `@(foreign)` ...
  if (decl.has_attr("foreign")) {
    // ... must not have definition
    if (decl.definition)
      decl.srcLoc.report_error(std::format("function '{}' declared '@(foreign)' must not have definition", get_name()));
    // ... must not use abstract (inferred) types
    if (params.has_any_abstract() || decl.returnType->type->is_abstract())
      decl.srcLoc.report_error(std::format("function '{}' declared '@(foreign)' must not use abstract types", get_name()));
    auto checkNoCollision{[&](const char *name) {
      if (decl.has_attr(name))
        decl.srcLoc.report_error(
            std::format("function '{}' declared '@(foreign)' must not be declared '@({})'", get_name(), name));
    }};
    checkNoCollision("macro");        // ... must not be `@(macro)`
    checkNoCollision("noinline");     // ... must not be `@(noinline)` (it always is?)
    checkNoCollision("alwaysinline"); // ... must not be `@(alwaysinline)`
    checkNoCollision("hot");          // ... must not be `@(hot)`
    checkNoCollision("cold");         // ... must not be `@(cold)`
    checkNoCollision("optnone");      // ... must not be `@(optnone)`
    checkNoCollision("optsize");      // ... must not be `@(optsize)`
    checkNoCollision("visible");      // ... must not be `@(visible)`
  }
  // `@(macro)` ...
  if (decl.has_attr("macro")) {
    // ... must have definition
    if (!decl.definition)
      decl.srcLoc.report_error(std::format("function '{}' declared '@(macro)' must have definition", get_name()));
    auto checkNoCollision{[&](const char *name) {
      if (decl.has_attr(name))
        decl.srcLoc.report_error(
            std::format("function '{}' declared '@(macro)' must not be declared '@({})'", get_name(), name));
    }};
    checkNoCollision("noinline");     // ... must not be `@(noinline)`
    checkNoCollision("alwaysinline"); // ... must not be `@(alwaysinline)` (it always is?)
    checkNoCollision("hot");          // ... must not be `@(hot)`
    checkNoCollision("cold");         // ... must not be `@(cold)`
    checkNoCollision("optnone");      // ... must not be `@(optnone)`
    checkNoCollision("optsize");      // ... must not be `@(optsize)`
    checkNoCollision("visible");      // ... must not be `@(visible)`
  }

  {
    auto checkNoCollision{[&](const char *name0, const char *name1) {
      if (decl.has_attr(name0) && decl.has_attr(name1))
        decl.srcLoc.report_error(
            std::format("function '{}' must not be declared both '@({})' and '@({})'", get_name(), name0, name1));
    }};
    // Verify no `@(hot cold)`
    // Verify no `@(alwaysinline noinline)`
    // Verify no `@(alwaysinline visible)`
    // Verify no `@(optnone optsize)`
    checkNoCollision("hot", "cold");
    checkNoCollision("alwaysinline", "noinline");
    checkNoCollision("alwaysinline", "visible");
    checkNoCollision("optnone", "optsize");
  }
}

} // namespace smdl::Compiler
