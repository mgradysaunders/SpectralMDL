// vim:foldmethod=marker:foldlevel=0:fmr=--{,--}
#include "Emitter.h"

#include "smdl/Resource/BSDFMeasurement.h"
#include "smdl/Support/Filesystem.h"
#include "smdl/Support/Logger.h"
#include "smdl/Support/PBRMaps.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/Parallel.h"
#include <atomic>
#include <cstdlib>
#include <exception>
#include <filesystem>
#include <mutex>

#if defined(_WIN32)
#include <malloc.h> // for '_aligned_malloc' and '_aligned_free'
#endif              // #if defined(_WIN32)

#if SMDL_HAS_PTEX
#include "Ptexture.h"
#endif // #if SMDL_HAS_PTEX

namespace smdl {

// Fold the result PHI of an inline expansion whose incoming values are all
// the same constant.
//
// `createResult` always merges through a PHI, even for a lone `return`, and
// leaves collapsing it to the optimizer. That is fine inside a function, but
// an expansion at module scope has to come out as a constant here and now,
// with no optimizer to run and no function to keep the PHI in.
static llvm::Value *foldConstantPHI(llvm::Value *llvmValue) {
  auto phiInst{llvm::dyn_cast_if_present<llvm::PHINode>(llvmValue)};
  if (!phiInst || phiInst->getNumIncomingValues() == 0) return llvmValue;
  auto llvmConst{llvm::dyn_cast<llvm::Constant>(phiInst->getIncomingValue(0))};
  if (!llvmConst) return llvmValue;
  for (unsigned i = 1; i < phiInst->getNumIncomingValues(); i++)
    if (phiInst->getIncomingValue(i) != llvmConst) return llvmValue;
  return llvmConst;
}

Value Emitter::createFunctionImplementation(
    std::string_view name, bool isPure, Type *returnType,
    const ParameterList &params, llvm::ArrayRef<Value> paramValues,
    const SourceLocation &srcLoc, const std::function<void()> &callback) {
  SMDL_SANITY_CHECK(params.size() == paramValues.size());
  SMDL_SANITY_CHECK(callback != nullptr);
  // Inline expansion needs an LLVM function to hold its blocks, but module
  // scope has none: an initializer there is emitted with no insert point at
  // all, on the understanding that it folds away entirely. So expand into a
  // scratch function and discard it below, once the result has been checked
  // to be a compile-time constant and therefore not to refer into it.
  auto scratchFunc{static_cast<llvm::Function *>(nullptr)};
  if (!getLLVMFunction()) {
    scratchFunc = llvm::Function::Create(
        llvm::FunctionType::get(llvm::Type::getVoidTy(context.llvmContext),
                                /*isVarArg=*/false),
        llvm::Function::PrivateLinkage, ".module_scope_call",
        &context.llvmModule);
    builder.SetInsertPoint(
        llvm::BasicBlock::Create(context.llvmContext, "entry", scratchFunc));
  }
  SMDL_DEFER([&] {
    if (scratchFunc) {
      builder.ClearInsertionPoint();
      scratchFunc->eraseFromParent();
      // Any slot allocated in the scratch function dies with it, and a
      // later function could be allocated at the same address.
      spillSlots.clear();
    }
  });
  auto fmf{builder.getFastMathFlags()};
  SMDL_DEFER([&] { builder.setFastMathFlags(fmf); });
  SMDL_PRESERVE(labelReturn, returns, scope, anchors);
  if (params.lastScope) restoreResolutionAnchor(params);
  labelReturn.depth = unwindStack.size();
  labelReturn.block = createBlock(llvm::StringRef(name) + ".return");
  returns.clear();
  auto declarationsToWarnAboutSize{declarationsToWarnAbout.size()};
  handleScope(nullptr, nullptr, [&] {
    labelBreak = labelContinue = {}; // Invalidate
    inDefer = false;
    for (size_t i = 0; i < params.size(); ++i)
      declareParameter(params[i], paramValues[i]);
    callback();
    if (!hasTerminator()) {
      recordReturn(RValue(context.getVoidType(), nullptr), srcLoc);
    }
  });
  llvmMoveBlockToEnd(labelReturn.block);
  builder.SetInsertPoint(labelReturn.block);
  auto result{createResult(returnType, returns, srcLoc)};
  // If the result is a lambda, its body has not been emitted yet and may
  // still legitimately use this expansion's parameters and locals when it
  // is called later. Leave the unused-value warnings in the list for the
  // enclosing expansion to flush: by then the lambda has either been
  // called, marking its captures as used, or is itself unused.
  const bool resultIsLambda{[&]() {
    if (!result.isComptimeMetaType(context)) return false;
    auto *type{llvm::dyn_cast<FunctionType>(
        result.getComptimeMetaType(context, srcLoc))};
    return type && type->isLambda;
  }()};
  if (!resultIsLambda) {
    for (size_t i = declarationsToWarnAboutSize;
         i < declarationsToWarnAbout.size(); i++)
      declarationsToWarnAbout[i]->maybeWarnAboutUnusedValue();
    declarationsToWarnAbout.resize(declarationsToWarnAboutSize);
  }
  if (scratchFunc && result) {
    if (result.isRValue())
      result = RValue(result.type, foldConstantPHI(result.llvmValue));
    if (!result.isComptime())
      srcLoc.throwError("call to ", Quoted(name),
                        " does not resolve to a compile-time constant and "
                        "cannot be used at module scope");
  }
  return result;
}

// Is this an aggregate large enough to move through memory rather than as a
// first-class LLVM value? Shared by the indirect return ('sret') and
// indirect parameter ('byval') conventions so the two cannot drift apart.
//
// NOTE: The threshold is 'greater than', not 'at least'. At
// '$WAVELENGTH_BASE_MAX' of 16 a 'color' and a 'float[16]' are both exactly
// 64 bytes, and the spectral inner loops are full of them: moving those to
// memory would cost far more than the aggregate traffic it saves.
bool Emitter::isIndirectAggregate(Type *type) {
  return type && (type->isStruct() || type->isUnion() || type->isArray()) &&
         context.getSizeOf(type) > 64;
}

void Emitter::addIndirectParamAttrs(Type *paramType, unsigned i,
                                    llvm::Function *llvmFunc,
                                    llvm::CallBase *callInst) {
  // 'byval' is what makes the convention safe: it states that the pointer is
  // transport and the callee's copy is private, which is exactly SMDL's
  // by-value parameter semantics. Without it a callee that writes to a
  // mutable parameter would write through to the caller's memory, because
  // the parameter binds directly to the incoming pointer (see
  // 'createFunction') and 'lvalue()' returns an existing lvalue untouched.
  auto attrs{llvm::AttrBuilder(context.llvmContext)};
  attrs.addByValAttr(paramType->llvmType);
  attrs.addAlignmentAttr(llvm::Align(context.getAlignOf(paramType)));
  if (llvmFunc) llvmFunc->addParamAttrs(i, attrs);
  if (callInst)
    callInst->setAttributes(callInst->getAttributes().addParamAttributes(
        context.llvmContext, i, attrs));
}

void Emitter::addIndirectReturnAttrs(Type *returnType, llvm::Function *llvmFunc,
                                     llvm::CallBase *callInst) {
  auto attrs{llvm::AttrBuilder(context.llvmContext)};
  attrs.addStructRetAttr(returnType->llvmType);
  attrs.addAttribute(llvm::Attribute::NoAlias);
  attrs.addAttribute(llvm::Attribute::NoUndef);
  attrs.addAlignmentAttr(llvm::Align(context.getAlignOf(returnType)));
  if (llvmFunc) llvmFunc->addParamAttrs(0, attrs);
  if (callInst)
    callInst->setAttributes(callInst->getAttributes().addParamAttributes(
        context.llvmContext, 0, attrs));
}

void Emitter::createFunction(llvm::Function *&llvmFunc, std::string_view name,
                             bool isPure, Type *&returnType,
                             llvm::ArrayRef<Type *> paramTypes,
                             const ParameterList &params,
                             const SourceLocation &srcLoc,
                             const std::function<void()> &callback,
                             bool useIndirectParams) {
  SMDL_SANITY_CHECK(returnType && params.size() == paramTypes.size());
  SMDL_SANITY_CHECK_MSG(!useIndirectParams || callback,
                        "'@(foreign)' must not use the indirect parameter "
                        "convention: it has to match the C ABI exactly");
  auto llvmParamTys{std::vector<llvm::Type *>{}};
  if (!isPure) llvmParamTys.push_back(llvm::PointerType::get(context, 0));
  // The parameters that travel as 'byval' pointers, as (LLVM parameter
  // index, type) pairs. Recorded once so that the signature built here, the
  // parameter binding below, and the 'sret' rebuild all agree: the rebuild
  // creates a fresh function and does not inherit parameter attributes.
  auto indirectParams{llvm::SmallVector<std::pair<unsigned, Type *>, 4>{}};
  for (const auto &paramType : paramTypes) {
    SMDL_SANITY_CHECK(paramType);
    SMDL_SANITY_CHECK(paramType->llvmType);
    // A voided parameter carries no data, so it is absent from the
    // signature entirely, exactly as a voided field is absent from a
    // struct layout. The call site drops it the same way (see
    // 'FunctionType::invoke'), and both sides derive it from the same
    // type vector, so the two cannot disagree.
    if (paramType->isVoid()) continue;
    if (useIndirectParams && passesIndirectly(paramType)) {
      indirectParams.push_back({unsigned(llvmParamTys.size()), paramType});
      llvmParamTys.push_back(llvm::PointerType::get(context, 0));
    } else {
      llvmParamTys.push_back(paramType->llvmType);
    }
  }
  // Interpret null callback as foreign.
  if (!callback) {
    SMDL_SANITY_CHECK(!returnType->isAbstract());
    auto llvmFuncTy{llvm::FunctionType::get(returnType->llvmType, llvmParamTys,
                                            params.isVariadic)};
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
                              llvmParamTys, params.isVariadic),
      llvm::Function::InternalLinkage, "", context.llvmModule);
  for (auto [i, paramType] : indirectParams)
    addIndirectParamAttrs(paramType, i, llvmFunc, nullptr);

  llvm::IRBuilderBase::InsertPointGuard ipGuard{builder};
  {
    // Spill slots belong to the function being emitted. Emitting this body
    // may recurse into another function's body (an instantiation triggered
    // from inside it), so the map is saved and restored rather than merely
    // cleared, and no slot can outlive the function it was allocated in.
    SMDL_PRESERVE(state, spillSlots);
    spillSlots.clear();
    state = {}; // Invalidate
    builder.SetInsertPoint(
        llvm::BasicBlock::Create(context, "entry", llvmFunc));
    auto llvmArg{llvmFunc->arg_begin()};
    if (!isPure) {
      llvmArg->setName("state");
      state =
          RValue(context.getPointerType(context.getStateType()), &*llvmArg++);
    }
    auto paramValues{llvm::SmallVector<Value>()};
    auto indirectItr{indirectParams.begin()};
    for (size_t i = 0; i < params.size(); i++) {
      // A voided parameter has no LLVM argument to consume, so do not
      // advance past one. It still binds, as the void value, so that the
      // name resolves in the body like any other parameter.
      if (paramTypes[i]->isVoid()) {
        paramValues.push_back(RValue(paramTypes[i], nullptr));
        continue;
      }
      llvmArg->setName(params[i].name);
      auto llvmParam{&*llvmArg++};
      // An indirect parameter arrives as a 'byval' pointer to storage the
      // callee owns privately, which is exactly what an lvalue is. Binding
      // it directly is the whole point of the convention: the parameter's
      // storage *is* the incoming pointer, so the re-spill that a by-value
      // aggregate needs before it can be indexed at runtime disappears
      // rather than being replaced by a different copy.
      if (indirectItr != indirectParams.end() &&
          indirectItr->first == llvmParam->getArgNo()) {
        paramValues.push_back(LValue(paramTypes[i], llvmParam));
        ++indirectItr;
      } else {
        paramValues.push_back(RValue(paramTypes[i], llvmParam));
      }
    }
    auto result{createFunctionImplementation(name, isPure, returnType, params,
                                             paramValues, srcLoc, callback)};
    returnType = result.type;
    SMDL_SANITY_CHECK(returnType);
    SMDL_SANITY_CHECK(!returnType->isAbstract());
    if (!result || result.type->isVoid()) {
      builder.CreateRetVoid();
    } else if (!returnsIndirectly(returnType)) {
      builder.CreateRet(result);
    }
    if (result && !result.type->isVoid() && returnsIndirectly(returnType)) {
      // Rebuild the function to return indirectly: 'void' return with a
      // leading 'sret' pointer parameter that the caller provides (see
      // 'FunctionType::invoke'). The result value stays in memory on both
      // sides instead of materializing as a large SSA aggregate.
      auto llvmFunc0{llvmFunc};
      auto llvmSretParamTys{std::vector<llvm::Type *>{}};
      llvmSretParamTys.push_back(llvm::PointerType::get(context, 0));
      llvmSretParamTys.insert(llvmSretParamTys.end(), llvmParamTys.begin(),
                              llvmParamTys.end());
      llvmFunc = llvm::Function::Create(
          llvm::FunctionType::get(llvm::Type::getVoidTy(context.llvmContext),
                                  llvmSretParamTys, params.isVariadic),
          llvm::Function::InternalLinkage, "", context.llvmModule);
      llvmFunc->splice(llvmFunc->begin(), llvmFunc0);
      auto llvmArg0{llvmFunc0->arg_begin()};
      auto llvmArg1{llvmFunc->arg_begin()};
      auto sretArg{&*llvmArg1++};
      sretArg->setName("sret");
      while (llvmArg1 != llvmFunc->arg_end()) {
        llvmArg1->setName(llvmArg0->getName());
        llvmArg0->replaceAllUsesWith(&*llvmArg1);
        ++llvmArg0;
        ++llvmArg1;
      }
      addIndirectReturnAttrs(returnType, llvmFunc, nullptr);
      // The rebuilt function does not inherit parameter attributes, and
      // every index shifts by one for the leading 'sret' slot.
      for (auto [i, paramType] : indirectParams)
        addIndirectParamAttrs(paramType, i + 1, llvmFunc, nullptr);
      builder.CreateStore(rvalue(result), sretArg);
      builder.CreateRetVoid();
      // Rewrite recursive calls emitted against the by-value placeholder,
      // giving each call site its own result slot in its entry block.
      for (auto user : llvm::make_early_inc_range(llvmFunc0->users())) {
        auto callInst{llvm::dyn_cast<llvm::CallInst>(user)};
        SMDL_SANITY_CHECK_MSG(
            callInst && callInst->getCalledOperand() == llvmFunc0,
            "unexpected use of function during 'sret' rebuild");
        auto blockEntry{&callInst->getFunction()->getEntryBlock()};
        builder.SetInsertPoint(blockEntry,
                               blockEntry->getFirstNonPHIOrDbgOrAlloca());
        auto slot{builder.CreateAlloca(returnType->llvmType)};
        builder.SetInsertPoint(callInst);
        auto llvmArgs{llvm::SmallVector<llvm::Value *>{}};
        llvmArgs.push_back(slot);
        for (auto &arg : callInst->args()) llvmArgs.push_back(arg.get());
        auto newCall{builder.CreateCall(llvmFunc->getFunctionType(), llvmFunc,
                                        llvmArgs)};
        addIndirectReturnAttrs(returnType, nullptr, newCall);
        for (auto [i, paramType] : indirectParams)
          addIndirectParamAttrs(paramType, i + 1, nullptr, newCall);
        callInst->replaceAllUsesWith(
            builder.CreateLoad(returnType->llvmType, slot));
        callInst->eraseFromParent();
      }
      llvmFunc0->eraseFromParent();
    } else if (returnType->llvmType !=
               llvmFunc->getFunctionType()->getReturnType()) {
      auto llvmFunc0{llvmFunc};
      llvmFunc = llvm::Function::Create(
          llvm::FunctionType::get(returnType->llvmType,
                                  llvmFunc->getFunctionType()->params(),
                                  params.isVariadic),
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
  }
}

Value Emitter::createAlloca(Type *type, const llvm::Twine &name) {
  auto llvmFunc{getLLVMFunction()};
  SMDL_SANITY_CHECK_MSG(llvmFunc, "tried to alloca outside of LLVM function");
  auto blockEntry{&llvmFunc->getEntryBlock()};
  auto ip{builder.saveIP()};
  builder.SetInsertPoint(blockEntry, blockEntry->getFirstNonPHIOrDbgOrAlloca());
  SMDL_SANITY_CHECK(type);
  SMDL_SANITY_CHECK(type->llvmType);
  auto value{LValue(type, builder.CreateAlloca(type->llvmType, nullptr, name))};
  builder.restoreIP(ip);
  return value;
}

Value Emitter::spillToMemory(Value value) {
  if (!value || value.isLValue() || value.isVoid()) return value;
  auto llvmFunc{getLLVMFunction()};
  // With no function there is nowhere to put the slot. This is module scope,
  // where the initializer has to fold to a constant anyway.
  if (!llvmFunc) return lvalue(value);
  const auto key{std::pair(value.llvmValue, value.type)};
  if (auto itr{spillSlots.find(key)}; itr != spillSlots.end())
    return itr->second;
  auto slot{createAlloca(value.type, value.llvmValue->hasName()
                                         ? value.llvmValue->getName() + ".spill"
                                         : "spill")};
  // Emit the copy immediately after the definition of the value, not here at
  // the point of access. The alloca is already in the entry block, so it
  // dominates everything; placing the store at the definition makes the slot
  // valid wherever the value itself is, which is what lets a later access --
  // in another block, or on a later iteration of a loop -- reuse it.
  {
    llvm::IRBuilderBase::InsertPointGuard ipGuard{builder};
    if (auto inst{llvm::dyn_cast<llvm::Instruction>(value.llvmValue)}) {
      // A PHI must keep every PHI in its block contiguous at the top, so
      // insert past them rather than directly after the node.
      if (llvm::isa<llvm::PHINode>(inst)) {
        builder.SetInsertPoint(inst->getParent(),
                               inst->getParent()->getFirstInsertionPt());
      } else {
        SMDL_SANITY_CHECK_MSG(!inst->isTerminator(),
                              "cannot spill the result of a terminator");
        // The value is very often the last instruction emitted so far, in a
        // block that has no terminator yet, in which case there is no 'next'
        // to insert before and the store simply appends.
        if (auto llvmNext{inst->getNextNode()}) {
          builder.SetInsertPoint(llvmNext);
        } else {
          builder.SetInsertPoint(inst->getParent());
        }
      }
    } else {
      // An argument or a constant is available from the top of the function.
      auto blockEntry{&llvmFunc->getEntryBlock()};
      builder.SetInsertPoint(blockEntry,
                             blockEntry->getFirstNonPHIOrDbgOrAlloca());
    }
    builder.CreateStore(value, slot);
  }
  spillSlots[key] = slot;
  return slot;
}

void Emitter::declareParameter(const Parameter &param, Value value) {
  // Skip the conversion when the type already matches exactly. It is an
  // identity that nonetheless loads a memory-resident value back into SSA
  // (see 'ArrayType::invoke'), which would undo the indirect parameter
  // binding in 'createFunction' one line after it was established.
  if (value.type != param.type)
    value = invoke(param.type, value, param.getSourceLocation());
  // NOTE: 'lvalue()' returns an existing lvalue untouched, so a mutable
  // indirect parameter does NOT get a fresh copy here. That is correct
  // only because 'byval' guarantees the incoming pointer already denotes
  // the callee's private copy. If that attribute ever stops being applied
  // to every indirect parameter, mutable parameters silently start writing
  // through to the caller.
  value = param.isConst() || value.isVoid() ? value : lvalue(value);
  auto declaration{
      declare(param.name,
              param.astParam   ? static_cast<AST::Node *>(param.astParam)
              : param.astField ? static_cast<AST::Node *>(param.astField)
                               : nullptr,
              value)};
  declarationsToWarnAbout.push_back(declaration);
  if (param.isInline()) declareParameterInline(declaration->value);
}

void Emitter::declareParameterInline(Value value) {
  if (auto structType{
          llvm::dyn_cast<StructType>(value.type->getFirstNonPointerType())}) {
    for (auto &param : structType->params) {
      auto srcLoc{param.getSourceLocation()};
      auto declaration{declare(param.name, /*node=*/{},
                               accessField(value, param.name, srcLoc))};
      if (param.isInline()) declareParameterInline(declaration->value);
    }
  }
}

Declaration *
Emitter::findSameScopeDeclaration(Span<const std::string_view> name) {
  const auto internedName{context.internName(name)};
  // Probe the scope index: nearest non-exempt target in the current scope,
  // continuing through transparent scopes into the first real boundary.
  for (auto s{scope}; s; s = s->parent) {
    if (auto itr{s->decls.find(internedName.data())}; itr != s->decls.end())
      for (auto c{itr->second}; c; c = c->prevSameNameInScope)
        if (!c->isSameScopeShadowExempt()) return c;
    if (!s->transparent) break;
  }
  return nullptr;
}

void Emitter::rejectSameScopeShadow(Span<const std::string_view> name,
                                    const SourceLocation &srcLoc) {
  if (auto c{findSameScopeDeclaration(name)}) {
    if (auto prevSrcLoc{c->getSourceLocation()})
      srcLoc.throwError("redeclaration of ", Quoted(join(name, "::")),
                        " in the same scope, previously declared at ",
                        std::string(prevSrcLoc));
    srcLoc.throwError("redeclaration of ", Quoted(join(name, "::")),
                      " in the same scope");
  }
}

Declaration *Emitter::probeName(Span<const std::string_view> name,
                                llvm::Function *llvmFunc,
                                Declaration **unusableMatch) {
  auto seqLimit{std::numeric_limits<uint64_t>::max()};
  for (auto s{scope}; s; s = s->parent) {
    for (const auto &[anchorScope, anchorSeq] : anchors)
      if (s == anchorScope) seqLimit = std::min(seqLimit, anchorSeq);
    if (auto found{Declaration::resolveInScope(context, name, llvmFunc, s,
                                               /*ignoreIfNotExported=*/false,
                                               seqLimit, unusableMatch)})
      return found;
  }
  return nullptr;
}

void Emitter::declareImport(Span<const std::string_view> importPath, bool isAbs,
                            AST::Decl &decl) {
  if (importPath.size() < 2)
    decl.srcLoc.throwError("invalid import path (missing '::*'?)");
  auto isDots{[](auto elem) { return elem == "." || elem == ".."; }};
  auto importedModule{
      resolveModule(importPath.dropBack(), isAbs, decl.srcLoc.module_)};
  if (!importedModule)
    decl.srcLoc.throwError("cannot resolve import identifier ",
                           Quoted(join(importPath, "::")));
  if (importPath.back() == "*") {
    importPath = importPath.dropBack();
    importPath = importPath.dropFrontWhile(isDots);
    declare(importPath, &decl, context.getComptimeMetaModule(importedModule));
  } else {
    auto importedDeclaration{Declaration::findInModule(
        context, importPath.back(), getLLVMFunction(), importedModule)};
    if (!importedDeclaration)
      decl.srcLoc.throwError("cannot resolve import identifier ",
                             Quoted(join(importPath, "::")));
    declare(importPath.dropFrontWhile(isDots), &decl,
            importedDeclaration->value);
  }
}

void Emitter::unwind(size_t depth) {
  SMDL_SANITY_CHECK_MSG(!hasTerminator(), "tried to unwind after terminator");
  SMDL_SANITY_CHECK_MSG(depth <= unwindStack.size(), "inconsistent unwind");
  for (auto i{unwindStack.size()}; i-- > depth;) {
    // Copy the action: a defer body may push actions of its own, which can
    // reallocate the stack (its `handleScope` truncates them again, so the
    // indices below `i` stay meaningful).
    const auto action{unwindStack[i]};
    switch (action.kind) {
    case UnwindAction::Kind::Defer:
      handleScope(nullptr, nullptr, [&] {
        labelReturn = {};   // Invalidate!
        labelBreak = {};    // Invalidate!
        labelContinue = {}; // Invalidate!
        inDefer = true;     // More specific error messages
        // The body must not see declarations made after the defer
        // statement, so push an anchor at the innermost live scope
        // ('scope->parent': 'scope' itself is the defer body's own
        // scope, just pushed by 'handleScope', and stays unfiltered).
        // Pushing (not replacing) preserves any outer definition-site
        // anchor, e.g. a defer inside a macro expansion.
        anchors.push_back({scope->parent, action.seq});
        emit(action.astDefer->stmt);
      });
      break;
    case UnwindAction::Kind::Preserve:
      builder.CreateStore(action.valueToPreserve, action.value);
      break;
    case UnwindAction::Kind::LifetimeEnd:
      createLifetimeEnd(action.value);
      break;
    }
  }
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
  if (type->isVoid()) return RValue(type, nullptr);
  // A single compile-time constant result passes through unchanged: the
  // PHI below would needlessly demote it to a run-time value, and
  // compile-time-only values like function handles must survive being
  // returned from macros and 'return_from' blocks.
  if (results.size() == 1 && results[0].value.isComptime() &&
      results[0].value.type == type)
    return results[0].value;
  bool isAllIdenticalLValues{[&]() {
    for (auto &result : results)
      if (!(result.value.isLValue() &&
            result.value.type == results[0].value.type))
        return false;
    return true;
  }()};
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
      // Blame the 'return' that produced this value rather than the merge:
      // the conversion below is what rejects a return of the wrong type,
      // and 'srcLoc' here is typically the function declaration. Results
      // with no statement behind them (see 'emitVisit') fall back to it.
      const auto &valueSrcLoc{result.srcLoc ? result.srcLoc : srcLoc};
      // The conversion may emit control flow (e.g. union narrowing), so
      // detach the terminator, convert with the block open, then re-attach
      // the terminator to whichever block emission ends in.
      auto terminator{block->getTerminator()};
      terminator->removeFromParent();
      builder.SetInsertPoint(block);
      // The 'rvalue' matters: conversions may come back memory-resident
      // (see e.g. 'UnionType::invoke'), and the PHI merges values here.
      value = rvalue(invoke(type, value, valueSrcLoc));
      block = getInsertBlock();
      terminator->insertInto(block, block->end());
    }
    phiInst->addIncoming(value, block);
  }
  builder.restoreIP(ip);
  return isAllIdenticalLValues ? LValue(type, phiInst) : RValue(type, phiInst);
}

Value Emitter::emit(AST::Node &node) {
  return emitTypeSwitch<AST::File, AST::Decl, AST::Expr, AST::Stmt>(node);
}

//--{ Emit: Decl
Value Emitter::emit(AST::Decl &decl) {
  return emitTypeSwitch<AST::AnnotationDecl, AST::Enum, AST::Exec,
                        AST::Function, AST::Import, AST::Namespace, AST::Struct,
                        AST::Tag, AST::Typedef, AST::UnitTest, AST::UsingAlias,
                        AST::UsingImport, AST::Variable>(decl);
}

Value Emitter::emit(AST::Exec &decl) {
  auto returnType{context.getVoidType()};
  // Execs must be pure: the C++ side invokes them as 'void()' (see
  // 'JIT::Function<void()>' in Compiler.h), so a non-pure exec would read
  // a garbage '$state' pointer from a register that was never passed.
  auto llvmFunc{createFunction(".exec", /*isPure=*/true, returnType,
                               ParameterList(), decl.srcLoc,
                               [&] { emit(decl.stmt); })};
  llvmFunc->setLinkage(llvm::Function::ExternalLinkage);
  context.compiler.mExecs.emplace_back(llvmFunc->getName().str());
  return Value();
}

Value Emitter::emit(AST::UnitTest &decl) {
  if (context.compiler.enableUnitTests) {
    auto returnType{context.getVoidType()};
    auto llvmFunc{createFunction(".unit_test", /*isPure=*/false, returnType,
                                 ParameterList(), decl.srcLoc,
                                 [&] { emit(decl.stmt); })};
    llvmFunc->setLinkage(llvm::Function::ExternalLinkage);
    auto &unitTest{context.compiler.mUnitTests.emplace_back()};
    unitTest.moduleName = std::string(decl.srcLoc.getModuleName());
    unitTest.moduleFileName = std::string(decl.srcLoc.getModuleFileName());
    unitTest.lineNo = decl.srcLoc.lineNo;
    unitTest.testName = decl.name->value;
    unitTest.test.name = llvmFunc->getName().str();
    unitTest.test.func = nullptr; // Lookup later
  }
  return Value();
}

Value Emitter::emit(AST::UsingImport &decl) {
  // The local path buffer is safe here: 'declare' interns the name it
  // ultimately stores.
  auto importPath{decl.importPath.elementViews};
  for (auto &name : decl.names) {
    importPath.push_back(name.srcName);
    declareImport(
        Span<const std::string_view>(importPath.data(), importPath.size()),
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
  const bool isConst{decl.type->hasQualifier("const")};
  const bool isStatic{decl.type->hasQualifier("static")};
  const bool isInline{decl.type->hasQualifier("inline")};
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
    const auto depth0{unwindStack.size()};
    auto scope0{scope};
    // The initializer's bindings (e.g. ':=') go out of scope below, before
    // the declared variable itself enters the enclosing scope, hence a
    // transparent scope whose entries pop with the initializer region.
    scope = pushScope(/*transparent=*/true);
    auto args{[&]() -> ArgumentList {
      if (declarator.exprInit) {
        return emit(*declarator.exprInit);
      } else if (declarator.argsInit) {
        return emit(*declarator.argsInit);
      } else {
        return ArgumentList();
      }
    }()};
    unwind(depth0);
    unwindStack.resize(depth0);
    scope = scope0;
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
          createStore(value, valueAlloca);
          value = LValue(value.type, valueAlloca);
        }
        for (size_t i = 0; i < structType->params.size(); i++) {
          rejectSameScopeShadow(declarator.names[i].name,
                                declarator.names[i].name.srcLoc);
          declare(declarator.names[i].name, &declarator,
                  accessField(value, structType->params[i].name,
                              declarator.srcLoc));
        }
      } else {
        declarator.srcLoc.throwError("unsupported destructure");
      }
    } else {
      // NOTE: This must be a reference for `declare` below
      SMDL_SANITY_CHECK(declarator.names.size() == 1);
      const auto &name{declarator.names[0].name};
      rejectSameScopeShadow(name, name.srcLoc);
      // A function value is a compile-time constant handle. Storing it in
      // memory (a 'static' global or a mutable alloca) loses the
      // compile-time-ness that calling it requires, so fail here at the
      // declaration instead of confusingly at the call.
      if (value.isComptimeMetaType(context) &&
          value.getComptimeMetaType(context, declarator.srcLoc)->isFunction() &&
          (isStatic || !isConst))
        declarator.srcLoc.throwError(
            "variable ", Quoted(name.srcName),
            " holding a function must be declared 'const'",
            isStatic ? " without 'static'" : "");
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
          createStore(value, valueAlloca);
          value = LValue(value.type, valueAlloca);
        }
      }
      auto declaration{declare(name, &declarator, value)};
      if (getLLVMFunction()) declarationsToWarnAbout.push_back(declaration);
    }
  }
  return Value();
}
//--}

//--{ Emit: Expr
Value Emitter::emit(AST::Expr &expr) {
  return emitTypeSwitch<
      AST::AccessField, AST::AccessIndex, AST::Binary, AST::Call,
      AST::Identifier, AST::Intrinsic, AST::Lambda, AST::Let, AST::LiteralBool,
      AST::LiteralFloat, AST::LiteralInt, AST::LiteralString, AST::Parens,
      AST::ReturnFrom, AST::Select, AST::Type, AST::TypeCast, AST::Unary>(expr);
}

Value Emitter::emit(AST::AccessIndex &expr) {
  auto value{emit(expr.expr)};
  if (!value.isComptimeMetaType(context)) {
    for (auto &index : expr.indexes) {
      if (!index.expr) expr.srcLoc.throwError("expected non-empty '[]'");
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
        auto sizeNow{size.getComptimeSignedInt()};
        if (!sizeNow || *sizeNow < 0)
          expr.srcLoc.throwError("expected array size expression to resolve "
                                 "to non-negative compile-time int");
        type = context.getArrayType(type, uint32_t(*sizeNow));
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
    auto rv{rvalue(emit(expr.exprRhs))};
    declare(*ident, ident, rv);
    return rv;
  }
  // Short-circuit logic conditions.
  if (expr.op == BINOP_LOGIC_AND || expr.op == BINOP_LOGIC_OR) {
    auto boolType{context.getBoolType()};
    auto valueLhs{invoke(boolType, emit(expr.exprLhs), expr.srcLoc)};
    if (valueLhs.isComptimeInt()) {
      auto valueLhsNow{valueLhs.getComptimeInt()};
      if ((valueLhsNow != 0 && expr.op == BINOP_LOGIC_AND) ||
          (valueLhsNow == 0 && expr.op == BINOP_LOGIC_OR)) {
        // Contain rhs declarations, matching the runtime two-arm merge:
        // whether ':=' in the rhs is visible afterward must not depend on
        // whether the lhs constant-folded.
        SMDL_PRESERVE(scope);
        scope = pushScope(/*transparent=*/true);
        return invoke(boolType, emit(expr.exprRhs), expr.srcLoc);
      }
      return valueLhs;
    } else {
      // One arm evaluates the right-hand side (converted to bool inside
      // its own block, so the PHI type stays bool); the other arm is the
      // short-circuit constant.
      auto emitRhs{[&]() -> Value {
        return invoke(boolType, emit(expr.exprRhs), expr.srcLoc);
      }};
      auto emitConstant{[&]() -> Value {
        return context.getComptimeBool(expr.op == BINOP_LOGIC_OR);
      }};
      return expr.op == BINOP_LOGIC_AND
                 ? emitTwoArmMerge(valueLhs, "and", emitRhs,
                                   expr.exprRhs->srcLoc, emitConstant,
                                   expr.srcLoc, expr.srcLoc)
                 : emitTwoArmMerge(valueLhs, "or", emitConstant, expr.srcLoc,
                                   emitRhs, expr.exprRhs->srcLoc, expr.srcLoc);
    }
  }
  // Short-circuit else.
  if (expr.op == BINOP_ELSE) {
    auto valueLhs{emit(expr.exprLhs)};
    auto valueLhsCond{invoke(context.getBoolType(), valueLhs, expr.srcLoc)};
    if (valueLhsCond.isComptimeInt()) {
      if (valueLhsCond.getComptimeInt()) return valueLhs;
      // Contain rhs declarations, matching the runtime two-arm merge.
      SMDL_PRESERVE(scope);
      scope = pushScope(/*transparent=*/true);
      return emit(expr.exprRhs);
    } else {
      return emitTwoArmMerge(
          valueLhsCond, "else", [&] { return valueLhs; }, expr.exprLhs->srcLoc,
          [&] { return emit(expr.exprRhs); }, expr.exprRhs->srcLoc,
          expr.srcLoc);
    }
  }
  // Default.
  auto lhs{emit(expr.exprLhs)};
  auto rhs{emit(expr.exprRhs)};
  if (expr.op == BINOP_APPROX_CMP_EQ || //
      expr.op == BINOP_APPROX_CMP_NE) {
    // Approximate comparison operators
    // lhs ~== |eps| rhs: #abs(lhs - rhs) <= eps
    // lhs ~== (eps) rhs: #abs(lhs - rhs) <= eps * #max(#abs(lhs), #abs(rhs))
    // and ~!= is the logical negation of ~==.
    auto res{emitOp(BINOP_SUB, lhs, rhs, expr.srcLoc)};
    res = emitIntrinsic("abs", res, expr.srcLoc);
    auto eps{emit(expr.exprEps)};
    if (expr.isRelativeEps()) {
      auto scale{emitIntrinsic("max",
                               {emitIntrinsic("abs", lhs, expr.srcLoc),
                                emitIntrinsic("abs", rhs, expr.srcLoc)},
                               expr.srcLoc)};
      eps = emitOp(BINOP_MUL, eps, scale, expr.srcLoc);
    }
    res = emitOp(BINOP_CMP_LE, res, eps, expr.srcLoc);
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
    labelReturn = {unwindStack.size(), blockEnd};
    labelBreak = {};
    labelContinue = {};
    emit(expr.stmt);
    // With no recorded results the expression would silently evaluate to
    // 'none', which is more likely a mistake than an intent.
    if (returns.empty())
      expr.srcLoc.throwError("'return_from' must contain at least one "
                             "reachable 'return' statement");
    // A fallthrough path would reach 'blockEnd' without a recorded result
    // and leave the PHI in 'createResult' short one incoming edge.
    if (!hasTerminator())
      expr.srcLoc.throwError("'return_from' must 'return' on all control "
                             "paths");
  });
  llvmMoveBlockToEnd(blockEnd);
  builder.SetInsertPoint(blockEnd);
  return createResult(context.getAutoType(), returns, expr.srcLoc);
}

Value Emitter::emit(AST::Select &expr) {
  auto cond{invoke(context.getBoolType(), emit(expr.exprCond), expr.srcLoc)};
  if (cond.isComptimeInt()) {
    // Contain arm declarations, matching the runtime two-arm merge. (A ':='
    // in the condition still leaks, in both paths; it dominates the merge.)
    SMDL_PRESERVE(scope);
    scope = pushScope(/*transparent=*/true);
    return emit(cond.getComptimeInt() ? expr.exprThen : expr.exprElse);
  }
  return emitTwoArmMerge(
      cond, "select", [&] { return emit(expr.exprThen); },
      expr.exprThen->srcLoc, [&] { return emit(expr.exprElse); },
      expr.exprElse->srcLoc, expr.srcLoc);
}

Value Emitter::emitTwoArmMerge(Value cond, const char *name,
                               const std::function<Value()> &emitArm0,
                               const SourceLocation &srcLoc0,
                               const std::function<Value()> &emitArm1,
                               const SourceLocation &srcLoc1,
                               const SourceLocation &srcLoc) {
  // Contain declarations emitted inside either arm (e.g. the ':=' operator):
  // an arm executes only on its own control path, so a binding introduced
  // there must not be visible to the other arm or after the merge: its
  // value would not dominate those uses. Each arm gets its own transparent
  // scope: resolution inside arm 1 must not see arm 0's bindings.
  SMDL_PRESERVE(scope);
  const auto scope0{scope};
  auto [blockArm0, blockArm1, blockEnd] =
      createBlocks<3>(name, {".then", ".else", ".end"});
  builder.CreateCondBr(cond, blockArm0, blockArm1);
  builder.SetInsertPoint(blockArm0);
  scope = pushScope(/*transparent=*/true);
  auto value0{emitArm0()};
  auto value0IP{builder.saveIP()};
  scope = scope0;
  llvmMoveBlockToEnd(blockArm1);
  builder.SetInsertPoint(blockArm1);
  scope = pushScope(/*transparent=*/true);
  auto value1{emitArm1()};
  auto value1IP{builder.saveIP()};
  auto commonType{context.getCommonType({value0.type, value1.type},
                                        /*defaultToUnion=*/true, srcLoc)};
  // If both arms are already lvalues of the common type, merge the
  // pointers and stay an lvalue (mirroring 'createResult') so large
  // values (unions, structs) are not loaded into SSA just to be merged.
  const bool mergeAsLValues{value0.isLValue() && value1.isLValue() &&
                            value0.type == commonType &&
                            value1.type == commonType};
  builder.restoreIP(value0IP);
  if (!mergeAsLValues) value0 = rvalue(invoke(commonType, value0, srcLoc0));
  blockArm0 = getInsertBlock();
  builder.CreateBr(blockEnd);
  builder.restoreIP(value1IP);
  if (!mergeAsLValues) value1 = rvalue(invoke(commonType, value1, srcLoc1));
  blockArm1 = getInsertBlock();
  builder.CreateBr(blockEnd);
  // Create PHI instruction.
  llvmMoveBlockToEnd(blockEnd);
  builder.SetInsertPoint(blockEnd);
  auto phiInst{builder.CreatePHI(
      mergeAsLValues ? context.getPointerType(commonType)->llvmType
                     : commonType->llvmType,
      2)};
  phiInst->addIncoming(value0, blockArm0);
  phiInst->addIncoming(value1, blockArm1);
  return mergeAsLValues ? LValue(commonType, phiInst)
                        : RValue(commonType, phiInst);
}
//--}

//--{ Emit: Stmt
Value Emitter::emit(AST::Stmt &stmt) {
  return emitTypeSwitch<AST::Break, AST::Compound, AST::Continue, AST::DeclStmt,
                        AST::Defer, AST::DoWhile, AST::ExprStmt, AST::For,
                        AST::If, AST::Preserve, AST::Return, AST::Switch,
                        AST::Unreachable, AST::Visit, AST::While>(stmt);
}

Value Emitter::emit(AST::Compound &stmt) {
  handleScope(nullptr, nullptr, [&] {
    for (auto &subStmt : stmt.stmts)
      if (emit(subStmt); hasTerminator()) break;
  });
  return Value();
}

Value Emitter::emit(AST::DoWhile &stmt) {
  auto [blockLoop, blockCond, blockEnd] =
      createBlocks<3>("do_while", {".loop", ".cond", ".end"});
  builder.CreateBr(blockLoop);
  handleLoopScope(blockLoop, blockCond, blockEnd, blockCond,
                  [&] { emit(stmt.stmt); });
  builder.SetInsertPoint(blockCond);
  builder.CreateCondBr(
      invoke(context.getBoolType(), emit(stmt.expr), stmt.expr->srcLoc),
      blockLoop, blockEnd);
  handleBlockEnd(blockEnd);
  return Value();
}

Value Emitter::emit(AST::For &stmt) {
  // The init statement opens its own scope, so 'for (int i = ...' may
  // shadow an 'i' in the enclosing scope.
  SMDL_PRESERVE(scope);
  const auto depth0{unwindStack.size()};
  scope = pushScope(/*transparent=*/false);
  auto [blockCond, blockLoop, blockNext, blockEnd] =
      createBlocks<4>("for", {".cond", ".loop", ".next", ".end"});
  if (stmt.stmtInit) emit(stmt.stmtInit);
  builder.CreateBr(blockCond);
  llvmMoveBlockToEnd(blockCond);
  builder.SetInsertPoint(blockCond);
  if (stmt.exprCond)
    builder.CreateCondBr(invoke(context.getBoolType(), emit(stmt.exprCond),
                                stmt.exprCond->srcLoc),
                         blockLoop, blockEnd);
  else
    builder.CreateBr(blockLoop); // Empty condition is always true
  handleLoopScope(blockLoop, blockNext, blockEnd, blockNext, [&] {
    if (stmt.stmtLoop) emit(stmt.stmtLoop);
  });
  builder.SetInsertPoint(blockNext);
  if (stmt.exprNext) emit(stmt.exprNext);
  builder.CreateBr(blockCond);
  handleBlockEnd(blockEnd);
  if (!hasTerminator()) unwind(depth0);
  unwindStack.resize(depth0);
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
  if (!stmt.stmtElse) blockElse->eraseFromParent();
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
  auto seenCaseValues{llvm::SmallDenseSet<int64_t>{}};
  for (auto &astCase : stmt.cases) {
    if (astCase.isDefault()) {
      if (blockDefault)
        stmt.srcLoc.throwError("expected at most 1 'default' case in 'switch'");
      switchCases.push_back(SwitchCase{
          &astCase, nullptr, createBlock(switchNameRef + ".default")});
      blockDefault = switchCases.back().block;
    } else {
      auto value{rvalue(emit(astCase.expr))};
      // Convert bool/int-typed constants to 'int' so every case value has
      // the switch condition's type, which the LLVM switch requires.
      if (value.type->isArithmeticScalarInt())
        value = invoke(context.getIntType(), value, astCase.expr->srcLoc);
      auto llvmConst{
          llvm::dyn_cast_if_present<llvm::ConstantInt>(value.llvmValue)};
      if (!llvmConst)
        astCase.expr->srcLoc.throwError(
            "expected 'case' expression to resolve to compile-time int");
      if (!seenCaseValues.insert(llvmConst->getValue().getSExtValue()).second)
        astCase.expr->srcLoc.throwError("duplicate 'case' value in 'switch'");
      switchCases.push_back(SwitchCase{
          &astCase, llvmConst,
          createBlock(concat(switchName, ".case.", switchCases.size()))});
    }
  }
  if (!blockDefault) blockDefault = blockEnd;
  auto switchInst{builder.CreateSwitch(
      invoke(context.getIntType(), emit(stmt.expr), stmt.srcLoc),
      blockDefault)};
  handleScope(nullptr, blockEnd, [&] {
    labelBreak = {unwindStack.size(), blockEnd};
    for (size_t i = 0; i < switchCases.size(); i++) {
      auto &switchCase{switchCases[i]};
      if (switchCase.llvmConst)
        switchInst->addCase(switchCase.llvmConst, switchCase.block);
      builder.SetInsertPoint(switchCase.block);
      // Each case is its own scope: the dispatch may jump directly to any
      // case, past the declarations of the cases before it, so a
      // declaration must not be visible beyond its own case, not even on
      // fallthrough. Falling through unwinds the case scope like any other
      // scope exit (and `handleScope` emits the fallthrough branch).
      auto blockNext{i + 1 < switchCases.size() ? switchCases[i + 1].block
                                                : blockEnd};
      handleScope(nullptr, blockNext, [&] {
        for (auto &subStmt : switchCase.astCase->stmts)
          if (emit(subStmt); hasTerminator()) break;
      });
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
  handleLoopScope(blockLoop, blockCond, blockEnd, blockCond,
                  [&] { emit(stmt.stmt); });
  handleBlockEnd(blockEnd);
  return Value();
}
//--}

//--{ emitOp (AST::UnaryOp)
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
      if (type->isVoid()) srcLoc.throwError("cannot optionalize 'void'");
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
      auto result{rvalue(value)}; // Save
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
      return rvalue(value);
    }
    // Unary negative, e.g., `-value`
    if (op == UNOP_NEG) {
      if (value.type->isVectorized() && value.type->isArithmeticIntegral()) {
        return RValue(value.type, builder.CreateNeg(rvalue(value)));
      } else if (value.type->isVectorized()) {
        return RValue(value.type, builder.CreateFNeg(rvalue(value)));
      } else if (value.type->isArithmeticMatrix()) {
        return emitOpColumnwise(value.type, [&](unsigned j) {
          return emitOp(op, accessIndex(value, j, srcLoc), srcLoc);
        });
      } else if (value.type->isComplex(context)) {
        return invoke("_complexNeg", value, srcLoc);
      }
    }
    // Unary not, e.g., `~value`
    if (op == UNOP_NOT) {
      if ((value.type->isArithmeticScalar() ||
           value.type->isArithmeticVector()) &&
          value.type->isArithmeticIntegral()) {
        return RValue(value.type, builder.CreateNot(rvalue(value)));
      }
    }
    // Unary logic not, e.g., `!value`
    if (op == UNOP_LOGIC_NOT) {
      if (value.isVoid()) return context.getComptimeBool(true);
      if (value.type->isArithmetic() || value.type->isColor() ||
          value.type->isPointer() || value.type->isEnum())
        return emitOp(BINOP_CMP_EQ, value, Value::zero(value.type), srcLoc);
      if (value.type->isOptionalUnion())
        return emitOp(UNOP_LOGIC_NOT,
                      invoke(context.getBoolType(), value, srcLoc), srcLoc);
    }
    // Unary address, e.g., `&value`
    if (op == UNOP_ADDR) {
      // This must come first: after it, the value is an rvalue anyway, and
      // the rvalue message below says nothing about why.
      if (value.isVoid())
        srcLoc.throwError("cannot take address of 'void'; a voided field "
                          "occupies no storage");
      if (!value.isLValue()) srcLoc.throwError("cannot take address of rvalue");
      return RValue(context.getPointerType(value.type), value);
    }
    // Unary dereference, e.g., `*value`
    if (op == UNOP_DEREF) {
      if (value.type->isPointer())
        return LValue(value.type->getPointeeType(), rvalue(value));
      if (value.type->isOptionalUnion()) {
        if (value.isRValue()) {
          auto lv{lvalue(value)};
          auto rv{rvalue(emitOp(op, lv, srcLoc))};
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
}
//--}

//--{ Helper: llvmArithOp
[[nodiscard]] static std::optional<llvm::Instruction::BinaryOps>
llvmArithOp(Scalar::Intent intent, AST::BinaryOp op) {
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

//--{ Helper: llvmCmpOp
[[nodiscard]] static std::optional<llvm::CmpInst::Predicate>
llvmCmpOp(Scalar::Intent intent, AST::BinaryOp op, bool isSigned = true) {
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

//--{ emitOp (AST::BinaryOp)
Value Emitter::emitOp(AST::BinaryOp op, Value lhs, Value rhs,
                      const SourceLocation &srcLoc) {
  if (op == BINOP_COMMA) return rhs;
  if ((op & BINOP_EQ) == BINOP_EQ) {
    if (!lhs.isLValue())
      srcLoc.throwError("cannot apply ", Quoted(to_string(op)), " to rvalue");
    if (op != BINOP_EQ) rhs = emitOp(op & ~BINOP_EQ, rvalue(lhs), rhs, srcLoc);
    createStore(invoke(lhs.type, rhs, srcLoc), lhs);
    return lhs;
  }
  lhs = rvalue(lhs);
  rhs = rvalue(rhs);
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
      if (auto llvmOp{llvmArithOp(commonType->scalar.intent, op)})
        return RValue(commonType, builder.CreateBinOp(*llvmOp, lhs, rhs));
      if (auto llvmOp{llvmCmpOp(commonType->scalar.intent, op)})
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
      // Row-vector times matrix is 'transpose(matrix) * vector'. Emitting
      // the transpose (a few shuffles, CSE'd across transforms by the same
      // matrix) lets the multiply lower to vertical splat-multiply-adds
      // like Matrix-Vector above, instead of one horizontal '#sum'
      // reduction per output component.
      return emitOp(op, emitIntrinsic("transpose", rhs, srcLoc), lhs, srcLoc);
    }
  }
  // Enums
  if (lhs.type->isEnum() && rhs.type->isEnum()) {
    if (auto llvmOp{llvmArithOp(Scalar::Intent::Int, op)};
        llvmOp && lhs.type == rhs.type)
      // NOTE: The result stays enum-typed even though it need not be a
      // declared enumerator; flag-mask idioms like '(mode & m) == flag'
      // depend on this.
      return RValue(lhs.type, builder.CreateBinOp(*llvmOp, lhs, rhs));
    if (auto llvmOp{llvmCmpOp(Scalar::Intent::Int, op)}) {
      if (lhs.type != rhs.type)
        srcLoc.throwError("cannot compare different enum types ",
                          Quoted(lhs.type->displayName), " and ",
                          Quoted(rhs.type->displayName));
      return RValue(context.getBoolType(),
                    builder.CreateCmp(*llvmOp, lhs, rhs));
    }
  }
  // Colors
  if ((lhs.type->isColor() &&
       (rhs.type->isColor() || rhs.type->isArithmeticScalar())) ||
      (lhs.type->isArithmeticScalar() && rhs.type->isColor())) {
    if (auto llvmOp{llvmArithOp(Scalar::Intent::FP, op)}) {
      lhs = invoke(context.getColorType(), lhs, srcLoc);
      rhs = invoke(context.getColorType(), rhs, srcLoc);
      return RValue(context.getColorType(),
                    builder.CreateBinOp(*llvmOp, lhs, rhs));
    }
    if (auto llvmCmpOpResult{llvmCmpOp(Scalar::Intent::FP, op)}) {
      lhs = invoke(context.getColorType(), lhs, srcLoc);
      rhs = invoke(context.getColorType(), rhs, srcLoc);
      auto resultType{context.getColorType()
                          ->getArithmeticVectorType(context)
                          ->getWithDifferentScalar(context, Scalar::getBool())};
      llvm::Value *llvmResult{builder.CreateCmp(*llvmCmpOpResult, lhs, rhs)};
      // A 1-wide 'color' still lowers to '<1 x float>', but the type
      // system has no 1-wide arithmetic vector, so the result type here
      // is scalar 'bool' and the '<1 x i1>' must be reduced to match.
      if (resultType->extent.isScalar())
        llvmResult = builder.CreateExtractElement(llvmResult, uint64_t(0));
      return RValue(resultType, llvmResult);
    }
  }
  // Complex numbers
  if (lhs.type->isComplex(context) || rhs.type->isComplex(context)) {
    // Promote both to `complex`
    lhs = invoke(context.getComplexType(), lhs, srcLoc);
    rhs = invoke(context.getComplexType(), rhs, srcLoc);
    const char *funcName{};
    if (op == BINOP_ADD) {
      funcName = "_complexAdd";
    } else if (op == BINOP_SUB) {
      funcName = "_complexSub";
    } else if (op == BINOP_MUL) {
      funcName = "_complexMul";
    } else if (op == BINOP_DIV) {
      funcName = "_complexDiv";
    }
    if (funcName) {
      return invoke(funcName, {lhs, rhs}, srcLoc);
    }
  }
  // Strings
  if (lhs.type->isString() && rhs.type->isString() &&
      (op == BINOP_CMP_EQ || op == BINOP_CMP_NE)) {
    // Compare size+1 bytes so the NUL terminator participates; otherwise
    // a prefix would compare equal to a longer string.
    auto result{llvm::emitStrNCmp(
        lhs, rhs,
        builder.CreateAdd(llvmEmitCast(builder,
                                       accessField(lhs, "size", srcLoc),
                                       builder.getInt64Ty()),
                          builder.getInt64(1)),
        builder, context.llvmLayout, &context.llvmTargetLibraryInfo)};
    return RValue(context.getBoolType(), op == BINOP_CMP_EQ
                                             ? builder.CreateIsNull(result)
                                             : builder.CreateIsNotNull(result));
  }
  // Pointers
  if ((op == BINOP_ADD || op == BINOP_SUB) && lhs.type->isPointer() &&
      rhs.type->isArithmeticScalarInt()) {
    if (op == BINOP_SUB) rhs = emitOp(UNOP_NEG, rhs, srcLoc);
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
    if (auto llvmOp{llvmCmpOp(Scalar::Intent::Int, op, /*isSigned=*/false)}) {
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
    if (auto llvmOp{llvmCmpOp(Scalar::Intent::Int, op)})
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
}
//--}

// Round an alignment request up to something every aligned allocator
// accepts: a power of two that is at least 'sizeof(void *)'. Over-aligning
// is always safe for the caller, who asked for *at least* this alignment,
// and '#alloc(size, align)' takes both arguments straight from user source,
// so neither is guaranteed to be sane on the way in.
[[nodiscard]] static size_t normalizeAlignment(size_t align) noexcept {
  auto result{sizeof(void *)};
  while (result < align) result <<= 1;
  return result;
}

// NOTE: Deliberately not 'std::aligned_alloc'. The Microsoft STL does not
// provide it at all (their 'free()' cannot release an aligned allocation),
// Apple only declares it for deployment targets of macOS 10.15 or later,
// and C11 additionally requires the size to be an integral multiple of the
// alignment, which nothing here guarantees.
[[nodiscard]] static void *alignedAllocate(size_t size, size_t align) noexcept {
#if defined(_WIN32)
  return _aligned_malloc(size, align);
#else
  void *ptr{};
  return ::posix_memalign(&ptr, align, size) == 0 ? ptr : nullptr;
#endif // #if defined(_WIN32)
}

static void alignedFree(void *ptr) noexcept {
#if defined(_WIN32)
  _aligned_free(ptr);
#else
  std::free(ptr);
#endif // #if defined(_WIN32)
}

extern "C" {

SMDL_EXPORT void *smdlAllocate(int size, int align) {
  SMDL_SANITY_CHECK(align > 0);
  if (!(size > 0)) return nullptr;
  auto ptr{alignedAllocate(size_t(size), normalizeAlignment(size_t(align)))};
  if (!ptr) throw std::bad_alloc();
  std::memset(ptr, 0x0, size);
  return ptr;
}

// NOTE: Must pair with 'alignedAllocate' above. On Windows in particular,
// 'std::free' cannot release memory from '_aligned_malloc'.
SMDL_EXPORT void smdlFree(void *ptr) { alignedFree(ptr); }

SMDL_EXPORT void *smdlBumpAllocate(void *state, int size, int align) {
  SMDL_SANITY_CHECK(state != nullptr && align > 0);
  if (size <= 0) return nullptr;
  auto allocator{
      static_cast<BumpPtrAllocator *>(static_cast<State *>(state)->allocator)};
  SMDL_SANITY_CHECK_MSG(allocator != nullptr, "allocator cannot be null!");
  return allocator->allocate(size, align);
}

// TODO This is a specific solution to a specific problem of calculating
// tabulated albedos conveniently and efficiently. There might be a more
// general way of addressing this in the future.
SMDL_EXPORT void smdlTabulateAlbedo(const char *name, int num_cos_theta,
                                    int num_roughness, const void *func) {
  SMDL_SANITY_CHECK(num_cos_theta > 1);
  SMDL_SANITY_CHECK(num_roughness > 1);
  SMDL_SANITY_CHECK(func);
  auto numCalculationsTodo{size_t(num_cos_theta) * size_t(num_roughness)};
  auto numCalculationsDone{std::atomic_int(0)};
  auto directionalAlbedo{std::vector<float>(numCalculationsTodo, 0.0f)};
  // The JIT'd function may throw (e.g. '#panic'), and an exception must
  // not unwind into LLVM's '-fno-exceptions' thread pool. Capture the
  // first one and rethrow it on the calling thread.
  auto firstException{std::exception_ptr{}};
  auto firstExceptionMutex{std::mutex{}};
  llvm::parallelFor(0, num_cos_theta, [&](size_t i) {
    try {
      float cos_theta{float(i) / float(num_cos_theta - 1)};
      for (int j = 0; j < num_roughness; j++) {
        float roughness{float(j) / float(num_roughness - 1)};
        directionalAlbedo[i * size_t(num_roughness) + size_t(j)] =
            reinterpret_cast<float (*)(float, float)>(const_cast<void *>(func))(
                cos_theta, roughness);
        llvm::errs() << llvm::format(
            "\rTabulating '%s': %2.1f%%", name,
            float(double(++numCalculationsDone) * 100.0 /
                  double(numCalculationsTodo)));
      }
    } catch (...) {
      auto lock{std::lock_guard<std::mutex>(firstExceptionMutex)};
      if (!firstException) firstException = std::current_exception();
    }
  });
  if (firstException) std::rethrow_exception(firstException);
  llvm::errs() << '\n';
  {
    std::error_code ec{};
    auto outputFileName{std::string(name) + ".inl"};
    auto outputFile{llvm::raw_fd_stream{outputFileName, ec}};
    if (ec) {
      llvm::errs() << "cannot open '" << outputFileName
                   << "' for writing: " << ec.message() << '\n';
      return;
    }
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
        float cos_theta{float(i) / float(num_cos_theta - 1)};
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

//--{ emitIntrinsic
IntrinsicID Emitter::resolveIntrinsic(std::string_view name,
                                      const SourceLocation &srcLoc) {
  // The parser accepts any '#word', so this is where a misspelling is caught.
  // The registry makes it cheap to guess what was meant.
  auto intrinsicID{getIntrinsicByName(name)};
  if (intrinsicID == IntrinsicID::Invalid) {
    auto similar{getSimilarIntrinsicName(name)};
    if (!similar.empty())
      srcLoc.throwError("unimplemented intrinsic ", Quoted(name),
                        "; did you mean ", Quoted(similar), "?");
    srcLoc.throwError("unimplemented intrinsic ", Quoted(name));
  }
  return intrinsicID;
}

Value Emitter::emitIntrinsic(std::string_view name, const ArgumentList &args,
                             const SourceLocation &srcLoc) {
  return emitIntrinsic(resolveIntrinsic(name, srcLoc), args, srcLoc);
}

Value Emitter::emitIntrinsic(IntrinsicID intrinsicID, const ArgumentList &args,
                             const SourceLocation &srcLoc) {
  const auto name{getIntrinsicName(intrinsicID)};
  if (args.isAnyNamed())
    srcLoc.throwError("intrinsics expect only unnamed arguments");
  if (!getLLVMFunction()) {
    // At module scope there is no function to emit instructions into. Run
    // the intrinsic in a scratch function so the IRBuilder cannot crash,
    // then require that everything constant-folded away.
    auto scratchFunc{llvm::Function::Create(
        llvm::FunctionType::get(llvm::Type::getVoidTy(context.llvmContext),
                                /*isVarArg=*/false),
        llvm::Function::PrivateLinkage, ".module_scope_intrinsic",
        &context.llvmModule)};
    auto blockEntry{
        llvm::BasicBlock::Create(context.llvmContext, "entry", scratchFunc)};
    auto ip{builder.saveIP()};
    SMDL_DEFER([&] {
      builder.restoreIP(ip);
      scratchFunc->eraseFromParent();
    });
    builder.SetInsertPoint(blockEntry);
    auto value{emitIntrinsic(intrinsicID, args, srcLoc)};
    if (scratchFunc->size() > 1 || !blockEntry->empty())
      srcLoc.throwError("intrinsic ", Quoted(name),
                        " does not resolve to a compile-time constant and "
                        "cannot be used at module scope");
    return value;
  }
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
    auto value{rvalue(expectOne())};
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
  // '#heapAllocate' and '#bumpAllocate' expect '(size)' or '(size, align)'
  // int arguments.
  auto expectSizeAndAlign{[&]() {
    auto arg0{args.size() >= 1 ? args[0].value : context.getComptimeInt(0)};
    auto arg1{args.size() == 2 ? args[1].value : context.getComptimeInt(1)};
    if (!(args.size() == 1 || args.size() == 2) ||
        !arg0.type->isArithmeticScalarInt() ||
        !arg1.type->isArithmeticScalarInt())
      srcLoc.throwError("intrinsic ", Quoted(name),
                        " expects 1 or 2 int arguments");
    auto intType{context.getIntType()};
    return std::pair{invoke(intType, rvalue(arg0), srcLoc),
                     invoke(intType, rvalue(arg1), srcLoc)};
  }};
  auto emitAllocateCall{[&](llvm::FunctionCallee callee) {
    if (auto func{llvm::dyn_cast<llvm::Function>(callee.getCallee())})
      func->setReturnDoesNotAlias();
    auto [size, align] = expectSizeAndAlign();
    return RValue(
        context.getVoidPointerType(),
        builder.CreateCall(callee, {size.llvmValue, align.llvmValue}));
  }};
  // Shared by '#bump' and '#bumpAllocate'.
  auto emitBumpAllocate{[&](Value size, Value align) {
    if (!state)
      srcLoc.throwError("intrinsic ", Quoted(name),
                        " requires '$state' and cannot be used in '@(pure)' "
                        "context");
    auto callee{
        context.getBuiltinCallee("smdlBumpAllocate", &smdlBumpAllocate)};
    if (auto func{llvm::dyn_cast<llvm::Function>(callee.getCallee())})
      func->setReturnDoesNotAlias();
    return RValue(
        context.getVoidPointerType(),
        builder.CreateCall(callee, {rvalue(state).llvmValue, size.llvmValue,
                                    align.llvmValue}));
  }};
  switch (intrinsicID) {
  case IntrinsicID::Invalid:
    break;
  case IntrinsicID::AlignOf: {
    return context.getComptimeInt(int(context.getAlignOf(expectOneType())));
  }
  case IntrinsicID::Abs: {
    // Intercept instances of `complex` and forward appropriately.
    if (args.size() == 1 && args[0].value.type->isComplex(context)) {
      return invoke("_complexAbs", args, srcLoc);
    }
    auto value{rvalue(expectOneVectorized())};
    return RValue(
        value.type,
        value.type->isArithmeticIntegral()
            ? builder.CreateBinaryIntrinsic(llvm::Intrinsic::abs, value,
                                            context.getComptimeBool(false))
            : builder.CreateUnaryIntrinsic(llvm::Intrinsic::fabs, value));
  }
  case IntrinsicID::Any:
  case IntrinsicID::All: {
    auto value{expectOne()};
    if (!value.type->isArithmeticScalar() && !value.type->isArithmeticVector())
      srcLoc.throwError("intrinsic ", Quoted(name),
                        " expects 1 scalar or vector argument");
    value = invoke(static_cast<ArithmeticType *>(value.type)
                       ->getWithDifferentScalar(context, Scalar::getBool()),
                   value, srcLoc);
    if (value.type->isArithmeticScalar()) return value;
    return RValue(
        context.getBoolType(),
        builder.CreateUnaryIntrinsic(intrinsicID == IntrinsicID::Any
                                         ? llvm::Intrinsic::vector_reduce_or
                                         : llvm::Intrinsic::vector_reduce_and,
                                     value));
  }
  case IntrinsicID::Assert: {
    if (!((args.size() == 1 && args[0].value.type == context.getBoolType()) ||
          (args.size() == 2 && args[0].value.type == context.getBoolType() &&
           args[1].value.type == context.getStringType())))
      srcLoc.throwError("intrinsic 'assert' expects 1 bool argument and 1 "
                        "optional string argument");
    auto cond{rvalue(args[0].value)};
    // A condition that folds is decided here and now: a true assertion
    // emits nothing at all, and a false one is a compile-time error
    // naming the offending source location.
    //
    // Deciding it here is what makes the false case reportable. Left to
    // the runtime path below, the panic is only as good as the program's
    // reaching it: it loses the source location, and in a compile-time
    // context (a macro expanded into a module-scope initializer, say)
    // the whole branch folds away and the assertion says nothing at all.
    //
    // This only fires for code that is actually emitted, so an assertion
    // in the untaken branch of an 'if $(...)' still costs nothing and
    // says nothing.
    if (cond.isComptimeInt()) {
      if (cond.getComptimeInt()) return Value();
      // The message is only usable here if it is itself compile-time.
      // A runtime string falls back to the source of the condition,
      // the same text the runtime panic below would report.
      if (args.size() == 2 && args[1].value.isComptimeString())
        srcLoc.throwError(std::string(args[1].value.getComptimeString()));
      if (!args[0].getSource().empty())
        srcLoc.throwError("assertion failed: ", args[0].getSource());
      srcLoc.throwError("assertion failed");
    }
    auto [blockPanic, blockOk] = createBlocks<2>("assert", {".panic", ".ok"});
    builder.CreateCondBr(cond, blockOk, blockPanic);
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
        emitPanic(rvalue(args[1].value), srcLoc);
      }
    });
    builder.CreateBr(blockOk);
    builder.SetInsertPoint(blockOk);
    return Value();
  }
  case IntrinsicID::Atan2: {
    if (!(args.size() == 2 &&                   //
          args[0].value.type->isVectorized() && //
          args[1].value.type->isVectorized()))
      srcLoc.throwError("intrinsic 'atan2' expects 2 vectorized arguments");
    auto commonType{context.getCommonType(
        {args[0].value.type, args[1].value.type, context.getFloatType()},
        /*defaultToUnion=*/false, srcLoc)};
    SMDL_SANITY_CHECK(commonType->isArithmeticFloatingPoint() ||
                      commonType->isColor());
    auto value0{invoke(commonType, args[0].value, srcLoc)};
    auto value1{invoke(commonType, args[1].value, srcLoc)};
    return RValue(commonType, builder.CreateBinaryIntrinsic(
                                  llvm::Intrinsic::atan2, value0, value1));
  }
  case IntrinsicID::AlbedoLUT: {
    if (!(args.size() == 1 && args[0].value.isComptimeString()))
      srcLoc.throwError(
          "intrinsic 'albedoLUT' expects 1 compile-time string argument");
    auto lutName{args[0].value.getComptimeString()};
    auto lutType{
        context.getKeyword("_AlbedoLUT").getComptimeMetaType(context, srcLoc)};
    auto lut{context.getBuiltinAlbedo(lutName)};
    if (!lut)
      srcLoc.throwError(
          "intrinsic 'albedoLUT' passed invalid name ", Quoted(lutName),
          " that does not identify any known look-up table at compile time");
    auto floatPtrType{context.getPointerType(context.getFloatType())};
    auto args{ArgumentList{}};
    args.emplace_back("num_cos_theta",
                      context.getComptimeInt(lut->num_cos_theta));
    args.emplace_back("num_roughness",
                      context.getComptimeInt(lut->num_roughness));
    args.emplace_back(
        "directional_albedo",
        context.getComptimePtr(floatPtrType, lut->directional_albedo));
    args.emplace_back("average_albedo", context.getComptimePtr(
                                            floatPtrType, lut->average_albedo));
    return invoke(lutType, args, srcLoc);
  }
  case IntrinsicID::BitCast: {
    if (!(args.size() == 2 &&                          //
          args[0].value.isComptimeMetaType(context) && //
          args[1].value.llvmValue != nullptr))
      srcLoc.throwError("intrinsic 'bitCast' expects 1 type argument and 1 "
                        "value argument");
    auto targetType{args[0].value.getComptimeMetaType(context, srcLoc)};
    if (targetType->isAbstract())
      srcLoc.throwError("intrinsic 'bitCast' expects concrete type argument");
    auto value{args[1].value};
    if (context.getSizeOf(targetType) != context.getSizeOf(value.type))
      srcLoc.throwError("intrinsic 'bitCast' expects source and target type "
                        "with identical size");
    // If the alignment is compatible and the value is already an lvalue,
    // we can just load it into an rvalue after transparently changing the
    // type.
    if (context.getAlignOf(targetType) <= context.getAlignOf(value.type) &&
        value.isLValue())
      return rvalue(LValue(targetType, value.llvmValue));
    // Otherwise, we guarantee the value is an lvalue and then create a
    // temporary alloca to perform the cast. We memcpy the bytes from the
    // lvalue
    auto lv{lvalue(value)};
    auto lvResult{createAlloca(targetType, value.getName() + ".bitcast")};
    createLifetimeStart(lvResult);
    builder.CreateMemCpyInline(lvResult, //
                               llvm::Align(context.getAlignOf(targetType)), lv,
                               llvm::Align(context.getAlignOf(value.type)),
                               builder.getInt64(context.getSizeOf(targetType)));
    auto rvResult{rvalue(lvResult)};
    createLifetimeEnd(lvResult);
    if (value.isRValue()) createLifetimeEnd(lv);
    return rvResult;
  }
  case IntrinsicID::Breakpoint: {
    if (!args.empty())
      srcLoc.throwError("intrinsic 'breakpoint' expects no arguments");
    builder.CreateIntrinsic(context.getVoidType()->llvmType,
                            llvm::Intrinsic::debugtrap, {});
    return RValue(context.getVoidType(), nullptr);
  }
  case IntrinsicID::BumpAllocate: {
    auto [size, align] = expectSizeAndAlign();
    return emitBumpAllocate(size, align);
  }
  case IntrinsicID::Bump: {
    auto value{expectOne()};
    auto ptr{emitBumpAllocate(
        context.getComptimeInt(int(context.getSizeOf(value.type))),
        context.getComptimeInt(int(context.getAlignOf(value.type))))};
    if (value.isLValue()) {
      // Copy directly out of memory (e.g. an 'sret' result slot)
      // instead of round-tripping the value through an SSA aggregate.
      builder.CreateMemCpy(ptr, llvm::Align(context.getAlignOf(value.type)),
                           value, llvm::Align(context.getAlignOf(value.type)),
                           context.getSizeOf(value.type));
    } else {
      builder.CreateStore(rvalue(value), ptr);
    }
    return RValue(context.getPointerType(value.type), ptr.llvmValue);
  }
  case IntrinsicID::Clock: {
    if (!args.empty())
      srcLoc.throwError("intrinsic 'clock' expects no arguments");
    // Lowers to the CPU cycle counter (RDTSC on x86-64, CNTVCT_EL0 on
    // AArch64). Units are reference cycles, only meaningful as deltas;
    // targets without a cycle counter lower this to constant 0.
    auto int64Type{context.getArithmeticType(Scalar::getInt(64))};
    return RValue(int64Type, builder.CreateIntrinsic(
                                 int64Type->llvmType,
                                 llvm::Intrinsic::readcyclecounter, {}));
  }
  case IntrinsicID::Fprint:
  case IntrinsicID::Fprintln: {
    if (args.size() < 2)
      srcLoc.throwError("intrinsic 'fprint' expects 1 file argument and 1 or "
                        "more printable arguments");
    auto file{rvalue(args[0].value)};
    for (size_t i = 1; i < args.size(); i++)
      emitPrint(file, args[i].value, srcLoc);
    if (intrinsicID == IntrinsicID::Fprintln)
      emitPrint(file, context.getComptimeString("\n"), srcLoc);
    return RValue(context.getVoidType(), nullptr);
  }
  case IntrinsicID::GetIntType: {
    auto value{rvalue(expectOne())};
    auto numBits{value.getComptimeSignedInt()};
    if (!numBits || *numBits < 1 || *numBits > 255)
      srcLoc.throwError("intrinsic 'getIntType' expects 1 compile-time int "
                        "between 1 and 255");
    return context.getComptimeMetaType(context.getArithmeticType(
        Scalar::getInt(uint8_t(*numBits)), Extent(1)));
  }
  case IntrinsicID::GetFloatType: {
    auto value{rvalue(expectOne())};
    auto numBits{value.getComptimeSignedInt()};
    if (!numBits || !(*numBits == 16 || *numBits == 32 || *numBits == 64 ||
                      *numBits == 80 || *numBits == 128))
      srcLoc.throwError("intrinsic 'getFloatType' expects 1 compile-time int "
                        "in {16, 32, 64, 80, 128}");
    return context.getComptimeMetaType(
        context.getArithmeticType(Scalar::getFP(uint8_t(*numBits)), Extent(1)));
  }
  case IntrinsicID::GetVectorType: {
    auto reportError{[&] {
      srcLoc.throwError("intrinsic 'getVectorType' expects 1 compile-time "
                        "scalar type and 1 compile-time positive int");
    }};
    if (!(args.size() == 2 &&                          //
          args[0].value.isComptimeMetaType(context) && //
          args[1].value.isComptimeInt()))
      reportError();
    auto type{args[0].value.getComptimeMetaType(context, srcLoc)};
    auto size{args[1].value.getComptimeSignedInt()};
    if (!type->isArithmeticScalar() || !size || *size < 1 || *size > 65535)
      reportError();
    return context.getComptimeMetaType(context.getArithmeticType(
        static_cast<ArithmeticType *>(type)->scalar, Extent(uint16_t(*size))));
  }
  case IntrinsicID::GetMatrixType: {
    auto reportError{[&] {
      srcLoc.throwError("intrinsic 'getMatrixType' expects 1 compile-time "
                        "scalar type and 2 compile-time positive ints");
    }};
    if (!(args.size() == 3 &&                          //
          args[0].value.isComptimeMetaType(context) && //
          args[1].value.isComptimeInt() &&             //
          args[2].value.isComptimeInt()))
      reportError();
    auto type{args[0].value.getComptimeMetaType(context, srcLoc)};
    auto numCols{args[1].value.getComptimeSignedInt()};
    auto numRows{args[2].value.getComptimeSignedInt()};
    if (!type->isArithmeticScalar() ||                  //
        !numCols || *numCols < 1 || *numCols > 65535 || //
        !numRows || *numRows < 1 || *numRows > 65535)
      reportError();
    return context.getComptimeMetaType(context.getArithmeticType(
        static_cast<ArithmeticType *>(type)->scalar,
        Extent(uint16_t(*numCols), uint16_t(*numRows))));
  }
  case IntrinsicID::HasField: {
    if (!(args.size() == 2 && args[1].value.isComptimeString()))
      srcLoc.throwError("intrinsic 'hasField' expects 1 argument and 1 "
                        "compile-time string argument");
    auto fieldName{args[1].value.getComptimeString()};
    auto value{args[0].value};
    if (value.isVoid()) return context.getComptimeBool(false);
    // A compile-time type asks about instances of that type, so that
    // '#hasField(value, ...)' and '#hasField(#typeOf(value), ...)' agree.
    // Only meta-*types* unwrap: a module or namespace stays a value,
    // because 'MetaType::accessField()' resolves its fields from it.
    if (value.isComptimeMetaType(context)) {
      auto type{value.getComptimeMetaType(context, srcLoc)};
      if (type->isAbstract())
        srcLoc.throwError("intrinsic 'hasField' cannot query abstract type ",
                          Quoted(type->displayName));
      return context.getComptimeBool(
          type->hasNonVoidField(*this, Value(), fieldName, srcLoc));
    }
    // NOTE: The value is deliberately not 'rvalue()'d. Only its type
    // matters, and forcing the load would leave dead IR behind.
    return context.getComptimeBool(
        value.type->hasNonVoidField(*this, value, fieldName, srcLoc));
  }
  case IntrinsicID::HeapAllocate: {
    return emitAllocateCall(
        context.getBuiltinCallee("smdlAllocate", &smdlAllocate));
  }
  case IntrinsicID::HeapFree: {
    auto value{expectOne()};
    if (!value.type->isPointer())
      srcLoc.throwError("intrinsic ", Quoted(name),
                        " expects 1 pointer argument");
    auto callee{context.getBuiltinCallee("smdlFree", &smdlFree)};
    return RValue(context.getVoidType(),
                  builder.CreateCall(callee, {rvalue(value).llvmValue}));
  }
  case IntrinsicID::TestFPClass: {
    if (!(args.size() == 2 &&                                 //
          (args[0].value.type->isArithmeticFloatingPoint() || //
           args[0].value.type->isColor()) &&                  //
          args[1].value.isComptimeInt()))
      srcLoc.throwError(
          "intrinsic 'testFPClass' expects 1 vectorized floating point "
          "argument and 1 compile-time int argument");
    auto value{rvalue(args[0].value)};
    auto arithType{value.type->isColor()
                       ? static_cast<ColorType *>(value.type)
                             ->getArithmeticVectorType(context)
                       : static_cast<ArithmeticType *>(value.type)};
    llvm::Value *llvmResult{
        builder.createIsFPClass(value, args[1].value.getComptimeInt())};
    // As with color comparison: a 1-wide 'color' is '<1 x float>' but
    // its arithmetic type is scalar, so reduce the '<1 x i1>' to match.
    if (value.type->isColor() && arithType->extent.isScalar())
      llvmResult = builder.CreateExtractElement(llvmResult, uint64_t(0));
    return RValue(arithType->getWithDifferentScalar(context, Scalar::getBool()),
                  llvmResult);
  }
  case IntrinsicID::IsArray:
  case IntrinsicID::IsArithmetic:
  case IntrinsicID::IsArithmeticIntegral:
  case IntrinsicID::IsArithmeticFloatingPoint:
  case IntrinsicID::IsArithmeticScalar:
  case IntrinsicID::IsArithmeticVector:
  case IntrinsicID::IsArithmeticMatrix:
  case IntrinsicID::IsEnum:
  case IntrinsicID::IsOptionalUnion:
  case IntrinsicID::IsPointer:
  case IntrinsicID::IsStruct:
  case IntrinsicID::IsTag:
  case IntrinsicID::IsUnion:
  case IntrinsicID::IsVoid:
  case IntrinsicID::IsDefault: {
    // Every type predicate is a compile-time query of the 'Type' member
    // function it is named for, so the table is the whole implementation.
    static constexpr struct {
      IntrinsicID intrinsicID;
      bool (Type::*predicate)() const;
    } typePredicates[]{
        {IntrinsicID::IsArray, &Type::isArray},
        {IntrinsicID::IsArithmetic, &Type::isArithmetic},
        {IntrinsicID::IsArithmeticIntegral, &Type::isArithmeticIntegral},
        {IntrinsicID::IsArithmeticFloatingPoint,
         &Type::isArithmeticFloatingPoint},
        {IntrinsicID::IsArithmeticScalar, &Type::isArithmeticScalar},
        {IntrinsicID::IsArithmeticVector, &Type::isArithmeticVector},
        {IntrinsicID::IsArithmeticMatrix, &Type::isArithmeticMatrix},
        {IntrinsicID::IsEnum, &Type::isEnum},
        {IntrinsicID::IsOptionalUnion, &Type::isOptionalUnion},
        {IntrinsicID::IsPointer, &Type::isPointer},
        {IntrinsicID::IsStruct, &Type::isStruct},
        {IntrinsicID::IsTag, &Type::isTag},
        {IntrinsicID::IsUnion, &Type::isUnion},
        {IntrinsicID::IsVoid, &Type::isVoid},
        {IntrinsicID::IsDefault, &Type::isDefault},
    };
    auto type{expectOneType()};
    for (auto [predicateID, predicate] : typePredicates)
      if (predicateID == intrinsicID)
        return context.getComptimeBool((type->*predicate)());
    break;
  }
  case IntrinsicID::LoadTexture2D:
  case IntrinsicID::LoadTexture3D:
  case IntrinsicID::LoadTextureCube:
  case IntrinsicID::LoadTexturePtex:
  case IntrinsicID::LoadPBRMaps:
  case IntrinsicID::LoadBSDFMeasurement:
  case IntrinsicID::LoadLightProfile:
  case IntrinsicID::LoadSpectralCurve:
    return emitIntrinsicLoad(intrinsicID, args, srcLoc);
  case IntrinsicID::Memset: {
    if (!(args.size() == 3 &&                            //
          args[0].value.type->isPointer() &&             //
          args[1].value.type->isArithmeticScalarInt() && //
          args[2].value.type->isArithmeticScalarInt()))
      srcLoc.throwError("intrinsic 'memset' expects 1 pointer argument "
                        "and 2 int arguments");
    auto ptr{rvalue(args[0].value)};
    auto val{llvmEmitCast(builder, rvalue(args[1].value), builder.getInt8Ty())};
    auto count{
        llvmEmitCast(builder, rvalue(args[2].value), builder.getInt64Ty())};
    return RValue(context.getVoidType(),
                  builder.CreateMemSet(ptr, val, count, std::nullopt));
  }
  case IntrinsicID::Memcpy: {
    if (!(args.size() == 3 &&                //
          args[0].value.type->isPointer() && //
          args[1].value.type->isPointer() && //
          args[2].value.type->isArithmeticScalarInt()))
      srcLoc.throwError("intrinsic 'memcpy' expects 2 pointer arguments "
                        "and 1 int argument");
    auto dst{rvalue(args[0].value)};
    auto src{rvalue(args[1].value)};
    auto count{
        llvmEmitCast(builder, rvalue(args[2].value), builder.getInt64Ty())};
    return RValue(
        context.getVoidType(),
        builder.CreateMemCpy(dst, std::nullopt, src, std::nullopt, count));
  }
  case IntrinsicID::Max:
  case IntrinsicID::Min: {
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
        intrinsicID == IntrinsicID::Max
            ? (type->isArithmeticBoolean()    ? llvm::Intrinsic::umax
               : type->isArithmeticIntegral() ? llvm::Intrinsic::smax
                                              : llvm::Intrinsic::maxnum)
            : (type->isArithmeticBoolean()    ? llvm::Intrinsic::umin
               : type->isArithmeticIntegral() ? llvm::Intrinsic::smin
                                              : llvm::Intrinsic::minnum);
    return RValue(type, builder.CreateBinaryIntrinsic(intrID, value0, value1));
  }
  case IntrinsicID::Num: {
    auto type{expectOneType()};
    auto size{[&]() -> int {
      if (type->isArithmeticVector()) {
        return static_cast<ArithmeticType *>(type)->extent.numRows;
      } else if (type->isArithmeticMatrix()) {
        return static_cast<ArithmeticType *>(type)->extent.numCols;
      } else if (type->isColor()) {
        return int(static_cast<ColorType *>(type)->wavelengthBaseMax);
      } else if (type->isArray()) {
        return int(static_cast<ArrayType *>(type)->size);
      } else {
        return 1;
      }
    }()};
    return context.getComptimeInt(size);
  }
  case IntrinsicID::NumRows:
  case IntrinsicID::NumCols: {
    auto type{expectOneType()};
    if (!type->isArithmeticMatrix())
      srcLoc.throwError("intrinsic ", Quoted(name),
                        " expects 1 matrix argument");
    return context.getComptimeInt(
        intrinsicID == IntrinsicID::NumRows
            ? int(static_cast<ArithmeticType *>(type)->extent.numRows)
            : int(static_cast<ArithmeticType *>(type)->extent.numCols));
  }
  case IntrinsicID::Panic: {
    if (args.size() != 1 || args[0].value.type != context.getStringType())
      srcLoc.throwError("intrinsic 'panic' expects 1 string argument");
    emitPanic(args[0].value, srcLoc);
    return Value();
  }
  case IntrinsicID::Pow: {
    if (args.size() != 2 ||                    //
        !args[0].value.type->isVectorized() || //
        !args[1].value.type->isVectorized())
      srcLoc.throwError("intrinsic 'pow' expects 2 vectorized arguments");
    auto value0{rvalue(args[0].value)};
    auto value1{rvalue(args[1].value)};
    if (value1.isComptimeInt()) {
      auto power{value1.getComptimeInt()};
      if (power == 1) return value0;
      if (power == 2) return emitOp(BINOP_MUL, value0, value0, srcLoc);
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
  case IntrinsicID::Print:
  case IntrinsicID::Println: {
    auto os{context.getComptimePtr(context.getVoidPointerType(), stderr)};
    for (auto &arg : args) emitPrint(os, arg.value, srcLoc);
    if (intrinsicID == IntrinsicID::Println)
      emitPrint(os, context.getComptimeString("\n"), srcLoc);
    return Value();
  }
  case IntrinsicID::Rotl:
  case IntrinsicID::Rotr: {
    if (!(args.size() == 2 && //
          args[0].value.type->isArithmeticIntegral() &&
          args[1].value.type->isArithmeticIntegral())) {
      srcLoc.throwError("intrinsic ", Quoted(name),
                        " expects 2 integer or vectorized integer arguments");
    }
    auto intType{context.getCommonType({args[0].value.type, args[1].value.type},
                                       /*defaultToUnion=*/false, srcLoc)};
    auto value0{invoke(intType, args[0].value, srcLoc)};
    auto value1{invoke(intType, args[1].value, srcLoc)};
    auto llvmIntrID{intrinsicID == IntrinsicID::Rotl ? llvm::Intrinsic::fshl
                                                     : llvm::Intrinsic::fshr};
    return RValue(intType,
                  builder.CreateIntrinsic(
                      intType->llvmType, llvmIntrID,
                      {value0.llvmValue, value0.llvmValue, value1.llvmValue}));
  }
  case IntrinsicID::SizeOf: {
    return context.getComptimeInt(int(context.getSizeOf(expectOneType())));
  }
  case IntrinsicID::Sign: {
    auto value{rvalue(expectOneVectorized())};
    if (value.type->isArithmeticIntegral()) {
      return RValue(
          value.type,
          builder.CreateSelect(
              emitOp(BINOP_CMP_LT, value, context.getComptimeInt(0), srcLoc),
              invoke(value.type, context.getComptimeInt(-1), srcLoc),
              invoke(value.type, context.getComptimeInt(+1), srcLoc)));
    } else {
      return RValue(value.type,
                    builder.CreateBinaryIntrinsic(
                        llvm::Intrinsic::copysign,
                        invoke(value.type, context.getComptimeFloat(1), srcLoc),
                        value));
    }
  }
  case IntrinsicID::Select: {
    if (args.size() != 3 || !args[0].value.type->isArithmeticBoolean() ||
        !args.isAllTrue(
            [](auto arg) { return arg.value.type->isVectorized(); }))
      srcLoc.throwError("intrinsic 'select' expects 1 vectorized boolean "
                        "argument and 2 vectorized selection arguments");
    auto valueCond{rvalue(args[0].value)};
    auto valueThen{args[1].value};
    auto valueElse{args[2].value};
    // The number of components a vectorized type selects over.
    auto numComponents{[](Type *type) -> uint32_t {
      if (type->isArithmeticVector())
        return static_cast<ArithmeticType *>(type)->extent.numRows;
      if (type->isColor())
        return static_cast<ColorType *>(type)->wavelengthBaseMax;
      return 1;
    }};
    // The condition fixes the number of components of the result, but takes
    // no part in its value type, so it joins the common type only when both
    // selection arguments are scalars and neither can supply that number
    // itself. Folding it in unconditionally fails on 'color', which has no
    // common type with the boolean vector that comparing two colors yields.
    auto type{
        context.getCommonType({valueThen.type, valueElse.type,
                               valueThen.type->isArithmeticScalar() &&
                                       valueElse.type->isArithmeticScalar() &&
                                       valueCond.type->isArithmeticVector()
                                   ? valueCond.type
                                   : nullptr},
                              /*defaultToUnion=*/false, srcLoc)};
    // The common type no longer sees the condition in every case, so the
    // component counts must be reconciled here instead of falling out of it.
    if (valueCond.type->isArithmeticVector() &&
        numComponents(valueCond.type) != numComponents(type))
      srcLoc.throwError("intrinsic 'select' expects the condition ",
                        Quoted(valueCond.type->displayName),
                        " to have the same number of components as the "
                        "selection arguments ",
                        Quoted(type->displayName));
    valueThen = invoke(type, valueThen, srcLoc);
    valueElse = invoke(type, valueElse, srcLoc);
    return RValue(type, builder.CreateSelect(valueCond, valueThen, valueElse));
  }
    // TODO This is a specific solution to a specific problem of calculating
    // tabulated albedos conveniently and efficiently. There might be a more
    // general way of addressing this in the future.
  case IntrinsicID::TabulateAlbedo: {
    if (!(args.size() == 4 && args[0].value.type == context.getStringType() &&
          args[1].value.type->isArithmeticScalarInt() &&
          args[2].value.type->isArithmeticScalarInt() &&
          args[3].value.isComptimeMetaType(context)))
      srcLoc.throwError("intrinsic 'tabulateAlbedo' expects 1 compile-time "
                        "string argument, 2 compile-time integer arguments, "
                        "and 1 function argument");
    auto funcType{llvm::dyn_cast<FunctionType>(
        args[3].value.getComptimeMetaType(context, srcLoc))};
    // Named functions must be declared '@(pure) float(float, float)'.
    // Lambdas declare neither purity nor a return type, so accept two
    // 'float' parameters and let materialization validate the rest: the
    // instance is compiled pure (see 'FunctionType::getInstance') and
    // the inferred return type is checked below.
    if (!(funcType && !funcType->isVariant() && funcType->hasNoOverloads() &&
          funcType->params.size() == 2 &&
          funcType->params[0].type == context.getFloatType() &&
          funcType->params[1].type == context.getFloatType() &&
          (funcType->isLambda ||
           (funcType->isPure() && !funcType->isMacro() &&
            funcType->returnType == context.getFloatType())))) {
      srcLoc.throwError("intrinsic 'tabulateAlbedo' function argument must "
                        "have signature '@(pure) float(float, float)' or be "
                        "a lambda with two 'float' parameters");
    }
    auto &funcInst{funcType->getInstance(
        *this, {context.getFloatType(), context.getFloatType()})};
    if (funcInst.returnType != context.getFloatType())
      srcLoc.throwError("intrinsic 'tabulateAlbedo' function argument must "
                        "return 'float'");
    auto callee{
        context.getBuiltinCallee("smdlTabulateAlbedo", &smdlTabulateAlbedo)};
    auto callInst{
        builder.CreateCall(callee, {rvalue(args[0].value).llvmValue, //
                                    rvalue(args[1].value).llvmValue, //
                                    rvalue(args[2].value).llvmValue, //
                                    funcInst.llvmFunc})};
    return RValue(context.getVoidType(), callInst);
  }
  case IntrinsicID::TypeOf: {
    return context.getComptimeMetaType(expectOne().type);
  }
  case IntrinsicID::TypeName: {
    return context.getComptimeString(expectOneType()->displayName);
  }
  case IntrinsicID::Transpose: {
    if (!(args.size() == 1 && args[0].value.type->isArithmeticMatrix()))
      srcLoc.throwError("intrinsic 'transpose' expects 1 matrix argument");
    auto value{args[0].value};
    auto valueCols{llvm::SmallVector<Value>{}};
    for (unsigned j = 0;
         j < static_cast<ArithmeticType *>(value.type)->extent.numCols; j++)
      valueCols.push_back(rvalue(accessIndex(value, j, srcLoc)));
    auto resultType{
        static_cast<ArithmeticType *>(value.type)->getTransposeType(context)};
    auto result{Value::zero(resultType)};
    for (unsigned j = 0; j < resultType->extent.numCols; j++) {
      auto resultCol{Value::zero(resultType->getColumnType(context))};
      for (unsigned i = 0; i < resultType->extent.numRows; i++)
        resultCol =
            insert(resultCol, accessIndex(valueCols[i], j, srcLoc), i, srcLoc);
      result = insert(result, resultCol, j, srcLoc);
    }
    return result;
  }
  case IntrinsicID::Uitofp: {
    if (!(args.size() == 2 &&                           //
          args[0].value.type->isArithmeticIntegral() && //
          args[1].value.isComptimeMetaType(context))) {
      srcLoc.throwError("intrinsic 'uitofp' expects 1 int argument and "
                        "1 type argument");
    }
    auto floatType{args[1].value.getComptimeMetaType(context, srcLoc)};
    if (!floatType->isArithmeticFloatingPoint())
      srcLoc.throwError("intrinsic 'uitofp' expects 1 int argument and "
                        "1 floating point type argument");
    return RValue(floatType, builder.CreateUIToFP(rvalue(args[0].value),
                                                  floatType->llvmType));
  }
  case IntrinsicID::UnpackFloat4: {
    auto value{rvalue(expectOne())};
    if (!value.type->isArithmeticScalar() && !value.type->isArithmeticVector())
      srcLoc.throwError(
          "intrinsic 'unpackFloat4' expects 1 scalar or vector argument");
    auto texelType{static_cast<ArithmeticType *>(value.type)};
    value.type = texelType->getWithDifferentScalar(context, Scalar::getFloat());
    value.llvmValue =
        texelType->isArithmeticIntegral()
            ? builder.CreateUIToFP(value.llvmValue, value.type->llvmType)
            : builder.CreateFPCast(value.llvmValue, value.type->llvmType);
    if (texelType->isArithmeticIntegral())
      value =
          emitOp(BINOP_MUL, value,
                 // Compute in double via ldexp: '1ULL << numBits' is
                 // undefined for 64-bit texel types.
                 context.getComptimeFloat(float(
                     1.0 / (std::ldexp(1.0, texelType->scalar.numBits) - 1.0))),
                 srcLoc);
    if (value.type == context.getFloatType(Extent(4))) // Early out?
      return value;
    auto result{Value::zero(context.getFloatType(Extent(4)))};
    if (value.type->isArithmeticScalar()) {
      result.llvmValue = builder.CreateVectorSplat(4, value.llvmValue);
    } else {
      for (uint64_t i = 0; i < texelType->extent.numRows; i++)
        result.llvmValue = builder.CreateInsertElement(
            result.llvmValue, builder.CreateExtractElement(value.llvmValue, i),
            i);
    }
    result.llvmValue = builder.CreateInsertElement(
        result.llvmValue, context.getComptimeFloat(1).llvmValue, uint64_t(3));
    return result;
  }
  // '#bitReverse', '#ctlz', '#cttz', and '#ctpop' differ only in the LLVM
  // intrinsic and whether it takes the trailing is-zero-poison flag.
  case IntrinsicID::BitReverse:
  case IntrinsicID::Ctlz:
  case IntrinsicID::Cttz:
  case IntrinsicID::Ctpop: {
    auto hasIsZeroPoisonFlag{intrinsicID == IntrinsicID::Ctlz ||
                             intrinsicID == IntrinsicID::Cttz};
    auto llvmIntrID{
        intrinsicID == IntrinsicID::BitReverse ? llvm::Intrinsic::bitreverse
        : intrinsicID == IntrinsicID::Ctlz     ? llvm::Intrinsic::ctlz
        : intrinsicID == IntrinsicID::Cttz     ? llvm::Intrinsic::cttz
                                               : llvm::Intrinsic::ctpop};
    auto value{rvalue(expectOneIntOrIntVector())};
    return RValue(value.type,
                  hasIsZeroPoisonFlag
                      ? builder.CreateBinaryIntrinsic(
                            llvmIntrID, value, context.getComptimeBool(false))
                      : builder.CreateUnaryIntrinsic(llvmIntrID, value));
  }
  // '#sum', '#prod', '#maxValue', and '#minValue' share one vector
  // reduction implementation.
  case IntrinsicID::Sum:
  case IntrinsicID::Prod:
  case IntrinsicID::MaxValue:
  case IntrinsicID::MinValue: {
    auto value{rvalue(expectOneVectorized())};
    if (value.type->isArithmeticScalar()) return value;
    // Widen bool vectors before '#sum': add-reducing '<N x i1>' computes
    // the parity of the lanes, not the count of true lanes.
    if (intrinsicID == IntrinsicID::Sum && value.type->isArithmeticBoolean())
      value = invoke(static_cast<ArithmeticType *>(value.type)
                         ->getWithDifferentScalar(context, Scalar::getInt(32)),
                     value, srcLoc);
    auto scalarType{scalarTypeOf(value.type)};
    if (intrinsicID == IntrinsicID::MaxValue ||
        intrinsicID == IntrinsicID::MinValue) {
      auto llvmIntrID = intrinsicID == IntrinsicID::MaxValue
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
      return RValue(scalarType,
                    builder.CreateUnaryIntrinsic(llvmIntrID, value));
    }
    if (scalarType->isArithmeticIntegral())
      return RValue(scalarType, intrinsicID == IntrinsicID::Sum
                                    ? builder.CreateAddReduce(value)
                                    : builder.CreateMulReduce(value));
    return RValue(
        scalarType,
        intrinsicID == IntrinsicID::Sum
            ? builder.CreateFAddReduce(Value::zero(scalarType), value)
            : builder.CreateFMulReduce(
                  invoke(scalarType, context.getComptimeFloat(1), srcLoc),
                  value));
  }
  // The complex accessors are defined on real numbers too, where they
  // degenerate to identity, zero, or a square.
  case IntrinsicID::Conj: {
    auto value{expectOne()};
    // NOTE: Conjugate of real number is no-op
    return value.type->isComplex(context)
               ? invoke("_complexConj", value, srcLoc)
               : rvalue(value);
  }
  case IntrinsicID::Real: {
    auto value{expectOne()};
    // NOTE: Real part of real number is identity
    return value.type->isComplex(context) ? accessField(value, "a", srcLoc)
                                          : value;
  }
  case IntrinsicID::Imag: {
    auto value{expectOne()};
    // NOTE: Imag part of real number is zero
    return value.type->isComplex(context) ? accessField(value, "b", srcLoc)
                                          : invoke(value.type, {}, srcLoc);
  }
  case IntrinsicID::Norm: {
    auto value{expectOne()};
    // NOTE: Norm of real number is square
    return value.type->isComplex(context)
               ? invoke("_complexNorm", value, srcLoc)
               : emitOp(BINOP_MUL, value, value, srcLoc);
  }
  // Unary floating-point math intrinsics. '#exp', '#log', and '#sqrt' accept
  // 'complex' as well and forward to the corresponding implementation.
  case IntrinsicID::Exp:
  case IntrinsicID::Log:
  case IntrinsicID::Sqrt:
    if (expectOne().type->isComplex(context))
      return invoke(intrinsicID == IntrinsicID::Exp   ? "_complexExp"
                    : intrinsicID == IntrinsicID::Log ? "_complexLog"
                                                      : "_complexSqrt",
                    args[0].value, srcLoc);
    [[fallthrough]];
  case IntrinsicID::Floor:
  case IntrinsicID::Ceil:
  case IntrinsicID::Trunc:
  case IntrinsicID::Round:
  case IntrinsicID::Sin:
  case IntrinsicID::Cos:
  case IntrinsicID::Tan:
  case IntrinsicID::Asin:
  case IntrinsicID::Acos:
  case IntrinsicID::Atan:
  case IntrinsicID::Sinh:
  case IntrinsicID::Cosh:
  case IntrinsicID::Tanh:
  case IntrinsicID::Exp2:
  case IntrinsicID::Exp10:
  case IntrinsicID::Log2:
  case IntrinsicID::Log10: {
    static const std::pair<IntrinsicID, llvm::Intrinsic::ID>
        unaryFPIntrinsics[]{
            {IntrinsicID::Floor, llvm::Intrinsic::floor},
            {IntrinsicID::Ceil, llvm::Intrinsic::ceil},
            {IntrinsicID::Trunc, llvm::Intrinsic::trunc},
            {IntrinsicID::Round, llvm::Intrinsic::round},
            {IntrinsicID::Sqrt, llvm::Intrinsic::sqrt},
            {IntrinsicID::Sin, llvm::Intrinsic::sin},
            {IntrinsicID::Cos, llvm::Intrinsic::cos},
            {IntrinsicID::Tan, llvm::Intrinsic::tan},
            {IntrinsicID::Asin, llvm::Intrinsic::asin},
            {IntrinsicID::Acos, llvm::Intrinsic::acos},
            {IntrinsicID::Atan, llvm::Intrinsic::atan},
            {IntrinsicID::Sinh, llvm::Intrinsic::sinh},
            {IntrinsicID::Cosh, llvm::Intrinsic::cosh},
            {IntrinsicID::Tanh, llvm::Intrinsic::tanh},
            {IntrinsicID::Exp, llvm::Intrinsic::exp},
            {IntrinsicID::Exp2, llvm::Intrinsic::exp2},
            {IntrinsicID::Exp10, llvm::Intrinsic::exp10},
            {IntrinsicID::Log, llvm::Intrinsic::log},
            {IntrinsicID::Log2, llvm::Intrinsic::log2},
            {IntrinsicID::Log10, llvm::Intrinsic::log10},
        };
    auto value{rvalue(expectOneVectorized())};
    if (value.type->isArithmeticIntegral())
      value = invoke(static_cast<ArithmeticType *>(value.type)
                         ->getWithDifferentScalar(context, Scalar::getFloat()),
                     value, srcLoc);
    for (auto [mathID, llvmIntrID] : unaryFPIntrinsics)
      if (mathID == intrinsicID)
        return RValue(value.type,
                      builder.CreateUnaryIntrinsic(llvmIntrID, value));
    break;
  }
  }
  srcLoc.throwError("unimplemented intrinsic ", Quoted(name));
}

/// Emit one of the `#load...` intrinsics.
///
/// These are separated out from `emitIntrinsic()` because they are a third of
/// it by volume and because they share the argument checking below; splitting
/// them keeps the main dispatch flat enough to read.
Value Emitter::emitIntrinsicLoad(IntrinsicID intrinsicID,
                                 const ArgumentList &args,
                                 const SourceLocation &srcLoc) {
  const auto name{getIntrinsicName(intrinsicID)};
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
                          rvalue(args[1].value));
  }};
  // The third argument must be compile-time, unlike the gamma: it is
  // baked into the texture as its level count at emission time, which
  // no runtime value can influence.
  auto expectOneComptimeStringOneIntAndOneComptimeBool{[&]() {
    if (!(args.size() == 3 &&                 //
          args[0].value.isComptimeString() && //
          args[1].value.type->isArithmeticScalarInt() &&
          args[2].value.isComptimeInt()))
      srcLoc.throwError("intrinsic ", Quoted(name),
                        " expects 1 compile-time string argument, 1 int "
                        "argument, and 1 compile-time bool argument");
    return std::make_tuple(std::string(args[0].value.getComptimeString()),
                           rvalue(args[1].value),
                           args[2].value.getComptimeInt() != 0);
  }};
  // Every flag must be compile-time for the same reason: together they
  // decide which images are loaded at all, which fields the resulting
  // struct has, and what each texture bakes for its level count. The
  // names are passed in so that the diagnostic can blame the parameter
  // the user actually wrote instead of an argument position.
  auto expectOneComptimeStringAndComptimeBools{
      [&](std::initializer_list<const char *> flagNames) {
        if (!(args.size() == 1 + flagNames.size() &&
              args[0].value.isComptimeString()))
          srcLoc.throwError("intrinsic ", Quoted(name),
                            " expects 1 compile-time string argument and ",
                            flagNames.size(), " compile-time bool arguments");
        auto flags{llvm::SmallVector<bool, 8>{}};
        for (size_t i = 0; i < flagNames.size(); i++) {
          if (!args[i + 1].value.isComptimeInt())
            srcLoc.throwError("intrinsic ", Quoted(name), " argument ",
                              Quoted(flagNames.begin()[i]),
                              " must be compile-time");
          flags.push_back(args[i + 1].value.getComptimeInt() != 0);
        }
        return std::make_pair(std::string(args[0].value.getComptimeString()),
                              std::move(flags));
      }};
  // The '#load_*' resource intrinsics share one locate-or-warn
  // prologue: a file that cannot be found is a warning, and the
  // resource type is default-constructed.
  //
  // The warning goes through 'logMissingFileOnce' rather than
  // 'srcLoc.logWarn' because this runs at emission time and a material
  // body is emitted three times (see 'Type.cc': the 'evaluate',
  // 'evaluateOpacity' and 'thinWalledProbe' functions each inline it),
  // which would otherwise report the same missing file three times over.
  // Resources that are found but fail to load already report once,
  // memoized by file hash in 'loadResource'.
  auto withLocatedFile{[&](const std::string &fileName, Type *resultType,
                           auto &&buildFromFile) -> Value {
    auto resolvedFileName{context.locate(fileName)};
    if (!resolvedFileName) {
      context.compiler.logResourceWarningOnce(
          srcLoc, fileName,
          concat("cannot load ", Quoted(fileName), ": file not found"));
      return invoke(resultType, {}, srcLoc);
    }
    return buildFromFile(*resolvedFileName);
  }};
  switch (intrinsicID) {
  case IntrinsicID::LoadTexture2D: {
    auto texture2DType{context.getTexture2DType()};
    auto [fileName, valueGammaAsInt, withMipLevels] =
        expectOneComptimeStringOneIntAndOneComptimeBool();
    auto resolvedImagePaths{context.locateImages(fileName)};
    if (resolvedImagePaths.empty()) {
      context.compiler.logResourceWarningOnce(
          srcLoc, fileName, concat("no image(s) found for ", Quoted(fileName)));
      return invoke(texture2DType, {}, srcLoc);
    }
    auto tileCountU{uint32_t(1)};
    auto tileCountV{uint32_t(1)};
    auto images{llvm::SmallVector<const Image *>{}};
    for (auto &[tileIndexU, tileIndexV, filePath] : resolvedImagePaths) {
      tileCountU = std::max(tileCountU, tileIndexU + 1);
      tileCountV = std::max(tileCountV, tileIndexV + 1);
      images.push_back(
          &context.compiler.loadImage(filePath, srcLoc, withMipLevels));
      if (images.back()->getFormat() != images.front()->getFormat() ||
          images.back()->getNumChannels() != images.front()->getNumChannels()) {
        context.compiler.logResourceWarningOnce(
            srcLoc, fileName,
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
    // Only the level 0 pointer of each tile is baked; the higher
    // levels live contiguously behind it and 'tex.smdl' recomputes
    // their offsets from the tile extent (see the layout note on
    // 'texture_2d' in 'api.smdl'). Baking pointer tables instead
    // would grow the struct past the by-value ABI sweet spot.
    //
    // The level count is per texture, not per image: the images are
    // shared with every other reference to the same files, so a texture
    // that opted out of mip levels bakes 1 here and never reads past
    // level 0, whatever the images behind it happen to hold.
    auto numLevels{1};
    if (withMipLevels)
      for (auto &image : images)
        numLevels = std::max(numLevels, image->getNumLevels());
    auto valueTileExtents{Value::zero(
        context.getArrayType(context.getIntType(2), tileCountU * tileCountV))};
    auto valueTileBuffers{Value::zero(
        context.getArrayType(texelPtrType, tileCountU * tileCountV))};
    for (unsigned int i = 0; i < resolvedImagePaths.size(); i++) {
      auto &imagePath{resolvedImagePaths[i]};
      auto &image{images[i]};
      auto insertPos{imagePath.tileIndexV * tileCountU + imagePath.tileIndexU};
      valueTileExtents =
          insert(valueTileExtents,
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
        {Argument{"tile_count", context.getComptimeVector(
                                    int2(int(tileCountU), int(tileCountV)))},
         Argument{"num_levels", context.getComptimeInt(numLevels)},
         Argument{"tile_extents", valueTileExtents},
         Argument{"tile_buffers", valueTileBuffers},
         Argument{"gamma", valueGammaAsInt}},
        srcLoc);
  }
  case IntrinsicID::LoadTexture3D: {
    auto texture3DType{context.getTexture3DType()};
    // The selector must be compile-time like the file name: together
    // they identify the resource, whose pointers are baked into the
    // compiled code at emission time.
    if (!(args.size() == 3 &&                 //
          args[0].value.isComptimeString() && //
          args[1].value.type->isArithmeticScalarInt() &&
          args[2].value.isComptimeString()))
      srcLoc.throwError("intrinsic ", Quoted(name),
                        " expects 1 compile-time string argument, 1 int "
                        "argument, and 1 compile-time string argument");
    auto fileName{std::string(args[0].value.getComptimeString())};
    auto valueGammaAsInt{rvalue(args[1].value)};
    auto gridName{std::string(args[2].value.getComptimeString())};
    return withLocatedFile(
        fileName, texture3DType, [&](const std::string &resolvedFileName) {
          auto &voxelGrid{context.compiler.loadVoxelGrid(resolvedFileName,
                                                         gridName, srcLoc)};
          // A load failure was already reported as a warning by
          // 'loadVoxelGrid', so quietly return the default (invalid)
          // texture, matching the missing-file behavior.
          if (!voxelGrid.isValid()) return invoke(texture3DType, {}, srcLoc);
          const auto extent{voxelGrid.getExtent()};
          const auto brickCount{voxelGrid.getBrickCount()};
          return invoke(
              texture3DType,
              {Argument{"extent", context.getComptimeVector(extent)},
               Argument{"brick_count", context.getComptimeVector(brickCount)},
               Argument{"brick_table",
                        context.getComptimePtr(
                            context.getPointerType(context.getIntType()),
                            voxelGrid.getBrickTable())},
               Argument{"brick_buffer",
                        context.getComptimePtr(
                            context.getPointerType(context.getFloatType()),
                            voxelGrid.getBrickData())},
               Argument{"background",
                        context.getComptimeFloat(voxelGrid.getBackground())},
               Argument{"min_value",
                        context.getComptimeFloat(voxelGrid.getMinValue())},
               Argument{"max_value",
                        context.getComptimeFloat(voxelGrid.getMaxValue())},
               Argument{"gamma", valueGammaAsInt},
               Argument{"resource",
                        context.getComptimePtr(context.getVoidPointerType(),
                                               &voxelGrid)}},
              srcLoc);
        });
  }
  case IntrinsicID::LoadTextureCube: {
    auto textureCubeType{context.getTextureCubeType()};
    auto [fileName, valueGammaAsInt] = expectOneComptimeStringAndOneInt();
    // TODO Actually load the texture
    context.compiler.logResourceWarningOnce(
        srcLoc, fileName,
        "intrinsic 'loadTextureCube' is not implemented yet; returning "
        "default texture");
    return invoke(textureCubeType, {}, srcLoc);
  }
  case IntrinsicID::LoadTexturePtex: {
    auto texturePtexType{context.getTexturePtexType()};
    auto fileNameAndGamma{expectOneComptimeStringAndOneInt()};
    return withLocatedFile(
        fileNameAndGamma.first, texturePtexType,
        [&](const std::string &resolvedFileName) {
          auto &ptexture{
              context.compiler.loadPtexture(resolvedFileName, srcLoc)};
          auto valuePtr{
              context.getComptimePtr(context.getVoidPointerType(),
                                     ptexture.texture ? &ptexture : nullptr)};
          return invoke(texturePtexType,
                        {Argument{"ptr", valuePtr},
                         Argument{"gamma", fileNameAndGamma.second}},
                        srcLoc);
        });
  }
  case IntrinsicID::LoadPBRMaps: {
    // The 'pbr_maps' and 'pbr_map' structs live in the non-standard
    // '::extras::pbr' module rather than the keyword-seeded 'api'
    // module, precisely so they do not masquerade as official MDL.
    // This intrinsic only ever appears in the 'pbr_maps' constructor
    // in that same module, so both types resolve by name right here.
    auto resolveStructType{[&](std::string_view typeName) {
      return static_cast<StructType *>(
          resolveIdentifier(typeName, srcLoc)
              .getComptimeMetaType(context, srcLoc));
    }};
    auto pbrMapsType{resolveStructType("pbr_maps")};
    auto pbrMapType{resolveStructType("pbr_map")};
    auto [fileName, flags] = expectOneComptimeStringAndComptimeBools(
        {"use_mipmap", "no_basecolor", "no_class_map", "no_height",
         "no_parallax"});
    const bool useMipmap{flags[0]};
    const bool noBasecolor{flags[1]};
    const bool noClassMap{flags[2]};
    const bool noHeight{flags[3]};
    // Dropping the height map drops the relief pipeline with it, so
    // 'no_height' subsumes 'no_parallax' and the effective flag is what
    // gets baked below.
    const bool noParallax{flags[4] || noHeight};
    auto resolvedPath{context.locateDirOrFile(fileName)};
    if (!resolvedPath) {
      context.compiler.logResourceWarningOnce(
          srcLoc, fileName,
          concat("cannot load ", Quoted(fileName),
                 ": file or directory not found"));
      return invoke(pbrMapsType, {}, srcLoc);
    }
    // Once the pack path is located, a broken pack is an authoring
    // bug: an ambiguous or absent '.pbr' manifest and a manifest
    // that fails to parse are errors, not warnings.
    const PBRMaps *manifest{};
    auto manifestFileName{std::string()};
    try {
      manifestFileName = PBRMaps::resolveFileName(*resolvedPath);
      manifest = &context.compiler.loadPBRMaps(manifestFileName, srcLoc);
    } catch (const Error &error) {
      srcLoc.throwError(error.message);
      return invoke(pbrMapsType, {}, srcLoc); // Unreachable
    }
    // An ambiguous manifest still loads, so report what it said and
    // which reading won. The key is the manifest and the message
    // together, so a manifest with two independent warnings reports
    // both, while a pack loaded by ten materials warns once.
    for (const auto &warning : manifest->warnings)
      context.compiler.logResourceWarningOnce(
          srcLoc, concat(manifestFileName, ": ", warning), warning);
    auto manifestDirName{parentPathOf(manifestFileName)};
    auto args{ArgumentList{}};
    if (!manifest->name.empty())
      args.elems.push_back(
          Argument{"name", context.getComptimeString(manifest->name)});
    if (manifest->physicalExtent)
      args.elems.push_back(Argument{
          "physical_extent",
          context.getComptimeVector(float2((*manifest->physicalExtent)[0],
                                           (*manifest->physicalExtent)[1]))});
    if (manifest->physicalRelief)
      args.elems.push_back(
          Argument{"physical_relief",
                   context.getComptimeFloat(*manifest->physicalRelief)});
    // The resolved UV-space relief scale, from either spelling: the
    // one relief quantity the builtin module consumes.
    if (auto reliefScale{manifest->effectiveReliefScale()})
      args.elems.push_back(
          Argument{"relief_scale", context.getComptimeFloat(*reliefScale)});
    args.elems.push_back(Argument{
        "hex_rotation", context.getComptimeFloat(manifest->hexRotation)});
    // The effective relief switch, which the module reads to gate the
    // whole relief pipeline. The height map may well be present and
    // sampled anyway; only the march, the horizon slopes, and the
    // height-implied normal are off.
    args.elems.push_back(
        Argument{"no_parallax", context.getComptimeBool(noParallax)});
    // Build one 'pbr_map' per declared map: a single-tile
    // 'texture_2d' through the image cache (so a file shared between
    // roles or with a plain 'texture_2d' decodes exactly once), plus
    // the folded storage facts. A map whose image fails to load stays
    // void, and 'loadImage' has warned.
    auto loadMap{[&](PBRMaps::Role role, std::string_view fieldName) -> bool {
      const auto &map{(*manifest)[role]};
      if (!map) return false;
      // Height and horizon maps never read mip levels, whatever
      // 'use_mipmap' says: box filtering is wrong for both (averaged
      // heights flatten the relief the parallax march and
      // finite-difference normals reconstruct; a horizon tangent's
      // meaningful minification is a max, not a mean), so 'num_levels'
      // bakes as 1 and every lod-aware lookup stays at level 0. The
      // image itself may still carry a chain if something else
      // references the same file and asks for one, which costs nothing
      // here.
      auto withMipLevels{useMipmap && role != PBRMaps::ROLE_HEIGHT &&
                         role != PBRMaps::ROLE_HORIZON};
      auto &image{context.compiler.loadImage(
          joinPaths(manifestDirName, map.file), srcLoc, withMipLevels)};
      if (!image.getTexels()) return false;
      auto texelPtrType{context.getPointerType(context.getArithmeticType(
          image.getFormat() == Image::UINT8     ? Scalar::getInt(8)
          : image.getFormat() == Image::UINT16  ? Scalar::getInt(16)
          : image.getFormat() == Image::FLOAT16 ? Scalar::getHalf()
                                                : Scalar::getFloat(),
          Extent(image.getNumChannels())))};
      auto numLevels{withMipLevels ? image.getNumLevels() : 1};
      auto valueTileExtents{
          Value::zero(context.getArrayType(context.getIntType(2), 1))};
      auto valueTileBuffers{Value::zero(context.getArrayType(texelPtrType, 1))};
      valueTileExtents =
          insert(valueTileExtents,
                 context.getComptimeVector(int2(int(image.getNumTexelsX()),
                                                int(image.getNumTexelsY()))),
                 0, srcLoc);
      valueTileBuffers = insert(
          valueTileBuffers,
          context.getComptimePtr(texelPtrType, image.getTexels()), 0, srcLoc);
      auto valueTex{invoke(
          context.getTexture2DType(),
          {Argument{"tile_count", context.getComptimeVector(int2(1, 1))},
           Argument{"num_levels", context.getComptimeInt(numLevels)},
           Argument{"tile_extents", valueTileExtents},
           Argument{"tile_buffers", valueTileBuffers},
           Argument{"gamma",
                    context.getComptimeInt(map.effectiveColorSpace() ==
                                                   PBRMaps::COLOR_SPACE_SRGB
                                               ? 1
                                               : 0)}},
          srcLoc)};
      auto convention{0};
      if (role == PBRMaps::ROLE_NORMAL) convention = int(map.normalConvention);
      args.elems.push_back(Argument{
          fieldName,
          invoke(pbrMapType,
                 {Argument{"tex", valueTex},
                  Argument{"channel", context.getComptimeInt(int(map.channel))},
                  Argument{"convention", context.getComptimeInt(convention)},
                  Argument{"max_slope", context.getComptimeFloat(map.maxSlope)},
                  Argument{"factor", context.getComptimeFloat(map.factor)},
                  Argument{"range", context.getComptimeVector(
                                        float2(map.range[0], map.range[1]))}},
                 srcLoc)});
      return true;
    }};
    // A map the call site opted out of is simply not loaded, so the
    // field stays void and every presence gate in the module reads the
    // set exactly as if the manifest had never declared it. The horizon
    // map goes with the relief pipeline: nothing else consumes it.
    if (!noBasecolor) loadMap(PBRMaps::ROLE_BASECOLOR, "basecolor");
    loadMap(PBRMaps::ROLE_NORMAL, "normal");
    loadMap(PBRMaps::ROLE_ROUGHNESS, "roughness");
    loadMap(PBRMaps::ROLE_METALLIC, "metallic");
    if (!noHeight) loadMap(PBRMaps::ROLE_HEIGHT, "height");
    loadMap(PBRMaps::ROLE_EMISSION, "emission");
    if (!noParallax) loadMap(PBRMaps::ROLE_HORIZON, "horizon");
    if (!noClassMap) loadMap(PBRMaps::ROLE_CLASS, "class_map");
    return invoke(pbrMapsType, args, srcLoc);
  }
  case IntrinsicID::LoadBSDFMeasurement: {
    auto bsdfMeasurementType{context.getBSDFMeasurementType()};
    auto fileName{expectOneComptimeString()};
    return withLocatedFile(
        fileName, bsdfMeasurementType,
        [&](const std::string &resolvedFileName) {
          auto &bsdfMeasurement{
              context.compiler.loadBSDFMeasurement(resolvedFileName, srcLoc)};
          auto bufferPtrType{context.getPointerType(context.getFloatType(
              bsdfMeasurement.type == BSDFMeasurement::TYPE_FLOAT ? 1 : 3))};
          return invoke(
              bsdfMeasurementType,
              {Argument{"ptr", context.getComptimePtr(
                                   context.getVoidPointerType(),
                                   bsdfMeasurement.buffer ? &bsdfMeasurement
                                                          : nullptr)},
               Argument{"mode", context.getComptimeInt(
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
        });
  }
  case IntrinsicID::LoadLightProfile: {
    auto lightProfileType{context.getLightProfileType()};
    auto fileName{expectOneComptimeString()};
    // Register the light profile runtime callees backing the
    // '@(pure foreign)' declarations in 'df.smdl' (which implement
    // 'df::measured_edf'), so they resolve as absolute JIT symbols
    // even when the host process does not export its own dynamic
    // symbols. Any 'measured_edf' necessarily loads a light profile
    // first, so this registration is a safe superset.
    (void)context.getBuiltinCallee("smdlLightProfileInterpolate",
                                   &smdlLightProfileInterpolate);
    (void)context.getBuiltinCallee("smdlLightProfileDirectionPDF",
                                   &smdlLightProfileDirectionPDF);
    (void)context.getBuiltinCallee("smdlLightProfileDirectionSample",
                                   &smdlLightProfileDirectionSample);
    return withLocatedFile(
        fileName, lightProfileType, [&](const std::string &resolvedFileName) {
          auto &lightProfile{
              context.compiler.loadLightProfile(resolvedFileName, srcLoc)};
          return invoke(
              lightProfileType,
              {Argument{"ptr",
                        context.getComptimePtr(
                            context.getVoidPointerType(),
                            lightProfile.isValid() ? &lightProfile : nullptr)},
               Argument{"max_intensity",
                        context.getComptimeFloat(lightProfile.maxIntensity())},
               Argument{"power",
                        context.getComptimeFloat(lightProfile.power())}},
              srcLoc);
        });
  }
  case IntrinsicID::LoadSpectralCurve: {
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
    return withLocatedFile(
        fileName, spectralCurveType,
        [&](const std::string &resolvedFileName) -> Value {
          auto spectrumView{SpectrumView{}};
          if (args.size() == 1) {
            spectrumView =
                context.compiler.loadSpectrum(resolvedFileName, srcLoc);
          } else if (args[1].value.isComptimeString()) {
            spectrumView = context.compiler.loadSpectrum(
                resolvedFileName,
                std::string(args[1].value.getComptimeString()), srcLoc);
          } else if (args[1].value.isComptimeInt()) {
            spectrumView = context.compiler.loadSpectrum(
                resolvedFileName, int(args[1].value.getComptimeInt()), srcLoc);
          }
          if (spectrumView.curveValues.empty()) {
            return invoke(spectralCurveType, {}, srcLoc);
          }
          auto floatPtrType{context.getPointerType(context.getFloatType())};
          return invoke(
              spectralCurveType,
              {Argument{"count", context.getComptimeInt(
                                     int(spectrumView.wavelengths.size()))},
               Argument{"wavelengths",
                        context.getComptimePtr(
                            floatPtrType, spectrumView.wavelengths.data())},
               Argument{"amplitudes",
                        context.getComptimePtr(
                            floatPtrType, spectrumView.curveValues.data())}},
              srcLoc);
        });
  }
  default:
    break;
  }
  // Unreachable: 'emitIntrinsic()' only routes the '#load...' identifiers here.
  SMDL_SANITY_CHECK(false);
  return Value();
}
//--}

void Emitter::expandInlineArgument(ArgumentList &args, AST::Argument &astArg) {
  const auto &srcLoc{astArg.srcLoc};
  if (astArg.isNamed())
    srcLoc.throwError("argument marked 'inline' must not be named");
  auto value{emit(astArg.expr)};
  auto *type{value.type};
  if (type->isArithmeticVector()) {
    const auto count{
        unsigned(static_cast<ArithmeticType *>(type)->extent.getVectorSize())};
    for (unsigned i = 0; i < count; i++)
      args.push_back(Argument{{}, accessIndex(value, i, srcLoc), &astArg});
  } else if (type->isArray() && !type->isAbstract()) {
    const auto count{static_cast<ArrayType *>(type)->size};
    for (unsigned i = 0; i < count; i++)
      args.push_back(Argument{{}, accessIndex(value, i, srcLoc), &astArg});
  } else if (type->isStruct() && !type->isAbstract()) {
    for (auto &param : static_cast<StructType *>(type)->params)
      args.push_back(Argument{param.name,
                              accessField(value, param.name, srcLoc), &astArg});
  } else if (type->isPointer()) {
    srcLoc.throwError("cannot expand 'inline' argument of pointer type ",
                      Quoted(type->displayName), "; dereference it first");
  } else {
    srcLoc.throwError("cannot expand 'inline' argument of type ",
                      Quoted(type->displayName),
                      "; expected a vector, array, or concrete struct");
  }
}

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
    // A meta-type value that is not a compile-time constant: a function or
    // type handle that was smuggled through a parameter of a non-macro
    // function or loaded back out of a mutable variable.
    if (callee.type == context.getMetaTypeType())
      srcLoc.throwError(
          "function and type values are compile-time only; they can only be "
          "called through 'const' bindings and parameters of '@(macro)' "
          "functions");
  }
  srcLoc.throwError("unimplemented or invalid call");
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
    auto ptr{value.type->isUnion() ? rvalue(accessField(value, "#ptr", {}))
                                   : Value()};
    auto visitName{context.getUniqueName("visit", getLLVMFunction())};
    auto visitNameRef{llvm::StringRef(visitName)};
    auto blockUnreachable{createBlock(visitNameRef + ".unreachable")};
    auto blockEnd{createBlock(visitNameRef + ".end")};
    auto switchInst{builder.CreateSwitch(rvalue(accessField(value, "#idx", {})),
                                         blockUnreachable)};
    for (size_t i = 0; i < unionType->caseTypes.size(); i++) {
      auto blockCase{createBlock(concat(visitName, ".case.", i))};
      switchInst->addCase(builder.getInt32(i), blockCase);
      handleScope(blockCase, blockEnd, [&] {
        auto caseType{unionType->caseTypes[i]};
        auto caseValue{Value()};
        if (value.type->isUnion()) {
          caseValue = LValue(caseType, ptr);
          caseValue = value.isRValue() ? rvalue(caseValue) : caseValue;
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

SMDL_EXPORT void smdlPanic(const char *message) {
  throw Error(std::string(message));
}

} // extern "C"

void Emitter::emitPanic(Value message, const SourceLocation &srcLoc) {
  SMDL_SANITY_CHECK(message);
  SMDL_SANITY_CHECK(message.type == context.getStringType());
  message = rvalue(message);
  auto callInst{builder.CreateCall(
      context.getBuiltinCallee("smdlPanic", &smdlPanic), {message.llvmValue})};
  callInst->setIsNoInline();
  callInst->setDoesNotReturn();
}

extern "C" {

SMDL_EXPORT void smdlPrintString(void *ptr, const char *value) {
  std::fputs(value, static_cast<std::FILE *>(ptr));
}

SMDL_EXPORT void smdlPrintQuotedString(void *ptr, const char *value) {
  auto *file{static_cast<std::FILE *>(ptr)};
  std::fputc('"', file);
  for (char ch : std::string_view(value)) {
    switch (ch) {
    case '\\':
      std::fputs("\\", file);
      break;
    case '\a':
      std::fputs("\\a", file);
      break;
    case '\b':
      std::fputs("\\b", file);
      break;
    case '\f':
      std::fputs("\\f", file);
      break;
    case '\n':
      std::fputs("\\n", file);
      break;
    case '\r':
      std::fputs("\\r", file);
      break;
    case '\t':
      std::fputs("\\t", file);
      break;
    case '\v':
      std::fputs("\\v", file);
      break;
    case '"':
      std::fputs("\\\"", file);
      break;
    default: {
      if (std::isgraph(static_cast<uint8_t>(ch))) {
        std::fputc(ch, file);
      } else {
        std::fputs("\\x", file);
        std::fputc("0123456789abcdef"[static_cast<uint8_t>(ch) / 16], file);
        std::fputc("0123456789abcdef"[static_cast<uint8_t>(ch) % 16], file);
      }
      break;
    }
    }
  }
  std::fputc('"', file);
}

SMDL_EXPORT void smdlPrintBool(void *ptr, int value) {
  std::fputs(value != 0 ? "true" : "false", static_cast<std::FILE *>(ptr));
}

SMDL_EXPORT void smdlPrintInt(void *ptr, int64_t value) {
  std::fprintf(static_cast<std::FILE *>(ptr), "%lld",
               static_cast<long long>(value));
}

SMDL_EXPORT void smdlPrintFloat(void *ptr, float value) {
  std::fprintf(static_cast<std::FILE *>(ptr), "%f", value);
}

SMDL_EXPORT void smdlPrintDouble(void *ptr, double value) {
  std::fprintf(static_cast<std::FILE *>(ptr), "%g", value);
}

SMDL_EXPORT void smdlPrintPointer(void *ptr, const void *value) {
  std::fprintf(static_cast<std::FILE *>(ptr), "%p", value);
}

} // extern "C"

void Emitter::emitPrint(Value file, Value value, const SourceLocation &srcLoc,
                        bool quoteStrings) {
  SMDL_SANITY_CHECK(file.type->isPointer());
  file = rvalue(file);
  if (value.isComptimeMetaType(context)) {
    emitPrint(file, value.getComptimeMetaType(context, srcLoc)->displayName,
              srcLoc);
  } else if (value.isVoid()) {
    emitPrint(file, "none", srcLoc);
  } else if (value.type->isPointer()) {
    builder.CreateCall(
        context.getBuiltinCallee("smdlPrintPointer", &smdlPrintPointer),
        {file.llvmValue, rvalue(value).llvmValue});
  } else if (value.type->isString()) {
    auto call{quoteStrings ? context.getBuiltinCallee("smdlPrintQuotedString",
                                                      &smdlPrintQuotedString)
                           : context.getBuiltinCallee("smdlPrintString",
                                                      &smdlPrintString)};
    builder.CreateCall(call, {file.llvmValue, rvalue(value).llvmValue});
  } else if (value.type->isEnum()) {
    emitPrint(file, invoke(context.getStringType(), value, srcLoc), srcLoc);
  } else if (auto arithType{llvm::dyn_cast<ArithmeticType>(value.type)}) {
    if (arithType->extent.isScalar()) {
      auto type{static_cast<Type *>(nullptr)};
      auto call{llvm::FunctionCallee()};
      if (arithType->scalar.isBoolean()) {
        type = context.getIntType();
        call = context.getBuiltinCallee("smdlPrintBool", &smdlPrintBool);
      } else if (arithType->scalar.isIntegral()) {
        type = context.getArithmeticType(Scalar::getInt(64), Extent(1));
        call = context.getBuiltinCallee("smdlPrintInt", &smdlPrintInt);
      } else {
        type = arithType->scalar.numBits < 64
                   ? context.getArithmeticType(Scalar::getFP(32), Extent(1))
                   : context.getArithmeticType(Scalar::getFP(64), Extent(1));
        call =
            arithType->scalar.numBits < 64
                ? context.getBuiltinCallee("smdlPrintFloat", &smdlPrintFloat)
                : context.getBuiltinCallee("smdlPrintDouble", &smdlPrintDouble);
      }
      builder.CreateCall(
          call, {file.llvmValue, invoke(type, value, srcLoc).llvmValue});
    } else if (arithType->extent.isVector()) {
      emitPrint(file, "<", srcLoc);
      for (uint32_t i = 0; i < arithType->extent.numRows; i++) {
        emitPrint(file, accessIndex(value, i, srcLoc), srcLoc);
        if (i + 1 < arithType->extent.numRows) {
          emitPrint(file, ", ", srcLoc);
        }
      }
      emitPrint(file, ">", srcLoc);
    } else if (arithType->extent.isMatrix()) {
      emitPrint(file, "[", srcLoc);
      for (uint32_t i = 0; i < arithType->extent.numCols; i++) {
        emitPrint(file, accessIndex(value, i, srcLoc), srcLoc);
        if (i + 1 < arithType->extent.numCols) {
          emitPrint(file, ", ", srcLoc);
        }
      }
      emitPrint(file, "]", srcLoc);
    }
  } else if (auto arrayType{llvm::dyn_cast<ArrayType>(value.type)}) {
    emitPrint(file, "[", srcLoc);
    for (uint32_t i = 0; i < arrayType->size; i++) {
      emitPrint(file, accessIndex(value, i, srcLoc), srcLoc,
                /*quoteStrings=*/true);
      if (i + 1 < arrayType->size) {
        emitPrint(file, ", ", srcLoc);
      }
    }
    emitPrint(file, "]", srcLoc);
  } else if (auto colorType{llvm::dyn_cast<ColorType>(value.type)}) {
    emitPrint(file, "<", srcLoc);
    for (uint32_t i = 0; i < colorType->wavelengthBaseMax; i++) {
      emitPrint(file, accessIndex(value, i, srcLoc), srcLoc);
      if (i + 1 < colorType->wavelengthBaseMax) {
        emitPrint(file, ", ", srcLoc);
      }
    }
    emitPrint(file, ">", srcLoc);
  } else if (auto structType{llvm::dyn_cast<StructType>(value.type)}) {
    emitPrint(file, value.type->displayName + "(", srcLoc);
    for (uint32_t i = 0; i < structType->params.size(); i++) {
      auto paramName{std::string(structType->params[i].name)};
      emitPrint(file, paramName + ": ", srcLoc);
      emitPrint(file, accessField(value, paramName, srcLoc), srcLoc,
                /*quoteStrings=*/true);
      if (i + 1 < structType->params.size()) {
        emitPrint(file, ", ", srcLoc);
      }
    }
    emitPrint(file, ")", srcLoc);
  } else if (value.type->isUnion()) {
    emitVisit(value, srcLoc, [&](Value value) {
      emitPrint(file, value, srcLoc, quoteStrings);
      return Value();
    });
  }
}

Value Emitter::resolveIdentifier(Span<const std::string_view> names,
                                 const SourceLocation &srcLoc,
                                 bool voidByDefault) {
  SMDL_SANITY_CHECK(!names.empty());
  // Interning the lookup makes the full-name comparisons in the scope
  // index pointer comparisons against interned declaration names.
  names = context.internName(names);
  Declaration *unusableMatch{};
  Declaration *declaration{probeName(names, getLLVMFunction(), &unusableMatch)};
  if (unusableMatch) {
    if (auto prevSrcLoc{unusableMatch->getSourceLocation()})
      srcLoc.throwError("cannot reference run-time value of ",
                        Quoted(join(names, "::")), " declared at ",
                        std::string(prevSrcLoc), " from a different function");
    srcLoc.throwError("cannot reference run-time value of ",
                      Quoted(join(names, "::")), " from a different function");
  }
  if (declaration) {
    return declaration->value;
  }
  if (names.size() == 1) {
    if (currentModule == nullptr || currentModule->isSMDLSyntax()) {
      if (names[0] == "$state") {
        if (state == nullptr)
          srcLoc.throwError("cannot resolve '$state' in pure context");
        return state;
      }
      if (names[0] == "none") {
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
                          const SourceLocation &srcLoc, bool dontEmit,
                          bool passAggregatesIndirectly) {
  // Obvious case: If there are more arguments than parameters, resolution
  // fails.
  if (args.size() > params.size() && !params.isVariadic) {
    srcLoc.throwError("too many arguments");
  }
  // Obvious case: If there are argument names that do not correspond to any
  // parameter names, resolution fails.
  if (!args.isOnlyTheseNames(params.getNames())) {
    srcLoc.throwError("invalid argument name(s)");
  }
  // The primary resolution logic.
  ResolvedArguments resolvedArgs{params, args};
  // The named array sizes deduced so far across the argument list, so that
  // `(const float[<N>] a, const auto[<N>] b)` rejects arguments of
  // different sizes instead of silently rebinding `N` (see
  // `InferredSizeArrayType::invoke`). This runs during overload probing,
  // so a size-inconsistent candidate is rejected like any other
  // conversion failure.
  auto deducedSizes{
      llvm::SmallVector<std::pair<std::string_view, uint32_t>, 2>{}};
  for (size_t iParam{}; iParam < params.size(); iParam++) {
    auto &param{params[iParam]};
    auto arg{[&]() -> const Argument * {
      // 1. Look for an explicitly named argument.
      for (size_t iArg{}; iArg < args.size(); iArg++) {
        if (const auto &arg{args[iArg]}; arg.name == param.name) {
          // If this has already been resolved by a positional argument,
          // resolution fails.
          if (resolvedArgs.isResolved(iArg))
            srcLoc.throwError("named argument ", Quoted(arg.name),
                              " already resolved by positional argument");
          resolvedArgs.argParams[iArg] = &param;
          return &arg;
        }
      }
      // 2. If no named argument, default to the next positional argument.
      for (size_t iArg{}; iArg < args.size(); iArg++) {
        if (const auto &arg{args[iArg]};
            arg.isPositional() && !resolvedArgs.isResolved(iArg)) {
          resolvedArgs.argParams[iArg] = &param;
          return &arg;
        }
      }
      return nullptr;
    }()};
    if (arg) {
      resolvedArgs.values[iParam] = arg->value;
      if (!context.isImplicitlyConvertible(arg->value.type, param.type))
        srcLoc.throwError("argument type ",
                          Quoted(arg->value.type->displayName),
                          " is not implicitly convertible to type ",
                          Quoted(param.type->displayName), " of parameter ",
                          Quoted(param.name));
      // Deduce named array sizes, descending through nested arrays.
      auto paramType{param.type};
      auto argType{arg->value.type};
      while (true) {
        auto inferredType{llvm::dyn_cast<InferredSizeArrayType>(paramType)};
        auto arrayType{llvm::dyn_cast<ArrayType>(argType)};
        if (!inferredType || !arrayType) break;
        if (!inferredType->sizeName.empty()) {
          auto deduced{[&]() -> std::pair<std::string_view, uint32_t> * {
            for (auto &entry : deducedSizes)
              if (entry.first == inferredType->sizeName) return &entry;
            return nullptr;
          }()};
          if (!deduced) {
            deducedSizes.push_back({inferredType->sizeName, arrayType->size});
          } else if (deduced->second != arrayType->size) {
            srcLoc.throwError(
                "argument type ", Quoted(arg->value.type->displayName),
                " of parameter ", Quoted(param.name), " deduces array size ",
                Quoted(inferredType->sizeName), " = ",
                std::to_string(arrayType->size),
                " but an earlier argument deduced ",
                Quoted(inferredType->sizeName), " = ",
                std::to_string(deduced->second));
          }
        }
        paramType = inferredType->elemType;
        argType = arrayType->elemType;
      }
    } else if (!param.getASTInitializer() && !param.builtinDefaultValue) {
      srcLoc.throwError("missing argument for parameter ", Quoted(param.name),
                        " without default initializer");
    }
  }
  // At this point, every parameter should either have a resolved argument
  // or a default initializer.
  auto resolvedArgsCount{size_t(0)};
  for (size_t iArg{}; iArg < args.size(); iArg++) {
    if (resolvedArgs.isResolved(iArg)) {
      resolvedArgsCount++;
    } else {
      // An argument that did not resolve is appended to the end
      // as variadic. After this for loop finishes, it should be
      // true that the number of values is equal to whichever is
      // greater between the number of arguments and the number
      // of parameters.
      resolvedArgs.values.emplace_back(args[iArg]);
    }
  }
  SMDL_SANITY_CHECK(resolvedArgs.values.size() ==
                    std::max(args.size(), params.size()));
  // If there are more arguments than parameters, i.e., the parameter list is
  // variadic, the number of resolved arguments should equal the number of
  // parameters. If there are fewer arguments than parameters, the number
  // of resolved arguments should equal the number of arguments.
  SMDL_SANITY_CHECK(resolvedArgsCount == std::min(args.size(), params.size()));

  if (!dontEmit) {
    SMDL_PRESERVE(scope, anchors);
    restoreResolutionAnchor(params);
    handleScope(nullptr, nullptr, [&] {
      for (size_t iParam{}; iParam < params.size(); iParam++) {
        auto &param{params[iParam]};
        auto &value{resolvedArgs.values[iParam]};
        if (!value) {
          if (auto expr{param.getASTInitializer()}) {
            setCurrentModule(expr->srcLoc);
            value = emit(expr);
          } else {
            SMDL_SANITY_CHECK(param.builtinDefaultValue);
            value = *param.builtinDefaultValue;
          }
        }
        // Skip a same-type conversion, which is an identity that would
        // nonetheless load a memory-resident value into SSA (see
        // 'ArrayType::invoke'): exactly the traffic the indirect
        // convention exists to avoid. The 'rvalue'/'lvalue' below supplies
        // whichever form is actually wanted either way.
        if (value.type != param.type)
          value = invoke(param.type, value, param.getSourceLocation());
        // An indirect parameter is passed as a pointer, so its value must
        // reach the call memory-resident. Everything else must reach it as
        // an rvalue: conversions may come back memory-resident (see e.g.
        // 'UnionType::invoke'), and these values are passed directly as
        // LLVM call arguments by 'FunctionType::invoke'.
        //
        // NOTE: The predicate is applied to the *converted value's* type,
        // not to 'param.type'. A declared parameter type may be abstract
        // ('auto', 'float[<N>]'), and it is the concrete resolved type that
        // the callee is instantiated with (see 'getNonVariadicTypes'), so
        // testing the declared type would disagree with the callee for
        // exactly the generic functions that pass big arrays around.
        //
        // The 'getLLVMFunction()' guard is for module scope, where there is
        // no function to allocate the argument slot in. A call cannot be
        // emitted there either (it needs an insert point), so the compile
        // fails regardless and both sides agree to skip the convention;
        // this only keeps the failure the one module scope already
        // produces rather than an alloca sanity check from here.
        const auto indirect{passAggregatesIndirectly && getLLVMFunction() &&
                            passesIndirectly(value.type)};
        // 'lvalue()' passes an existing lvalue through, so an argument that
        // is already memory-resident is passed without a copy; 'byval'
        // makes that safe by giving the callee its own copy regardless.
        value = indirect ? lvalue(value) : rvalue(value);
        // NOT this scope's storage to end: the call these values feed is
        // emitted after this scope closes, and an indirect argument may be
        // the caller's own variable passed through without a copy.
        declare(param.name, /*node=*/nullptr, value, /*ownsStorage=*/false);
      }
      if (params.isVariadic) {
        for (size_t iArg{}; iArg < args.size(); iArg++) {
          if (!resolvedArgs.argParams[iArg]) {
            auto &value{resolvedArgs.values[iArg]};
            value = rvalue(resolvedArgs.values[iArg]);
            // The C-ABI always promotes to double?
            if (value.type == context.getFloatType())
              value = invoke(context.getDoubleType(), value, srcLoc);
          }
        }
      }
    });
  }
  return resolvedArgs;
}

Module *Emitter::resolveModule(Span<const std::string_view> importPath,
                               bool isAbs, Module *thisModule) {
  llvm::SmallVector<std::string_view> resolvedImportPath{};
  resolveImportUsingAliases(std::numeric_limits<uint64_t>::max(), importPath,
                            resolvedImportPath);
  SMDL_SANITY_CHECK(!resolvedImportPath.empty());

  auto findModuleInDirectory{[&](std::string dirPath) -> Module * {
    for (auto resolvedImportDirPath :
         Span<const std::string_view>(resolvedImportPath.data(), //
                                      resolvedImportPath.size() - 1)) {
      dirPath = joinPaths(dirPath, resolvedImportDirPath);
    }
    // NOTE: The lexical comparison is a fallback for modules extracted
    // from archives: their pseudo file paths route through the '.mdr'
    // file, so the filesystem-level equivalence check cannot
    // canonicalize away '.' and '..' components. Trailing separators
    // are stripped because 'lexically_normal()' of a path ending in
    // '..' keeps one.
    auto normalizePath{[](std::string somePath) {
      auto normal{std::filesystem::path(std::move(somePath))
                      .lexically_normal()
                      .string()};
      while (normal.size() > 1 &&
             (normal.back() == '/' || normal.back() == '\\')) {
        normal.pop_back();
      }
      return normal;
    }};
    auto lexicalDirPath{normalizePath(dirPath)};
    for (auto &otherModule : context.compiler.mModules) {
      if (otherModule.get() != thisModule && !otherModule->isBuiltin() &&
          otherModule->getName() == resolvedImportPath.back() &&
          (isPathEquivalent(dirPath, otherModule->getDirectory()) ||
           lexicalDirPath ==
               normalizePath(std::string(otherModule->getDirectory())))) {
        if (auto error{otherModule->compile(context)}) throw std::move(*error);
        return otherModule.get();
      }
    }
    return nullptr;
  }};
  auto searchRelativeToCurrentModule{[&]() -> Module * {
    if (!thisModule->isBuiltin())
      if (auto mod{findModuleInDirectory(thisModule->getDirectory())})
        return mod;
    return nullptr;
  }};
  auto searchCompilerDirPaths{[&]() -> Module * {
    if (thisModule->isBuiltin()) return nullptr;
    // Look up by qualified name, so modules extracted from archives
    // resolve as if the archive were extracted at the top level of its
    // search root, and so first-root-wins shadowing applies exactly as
    // it does in the public lookup API. Note the index is populated in
    // add-order, which makes this equivalent to searching the roots in
    // the order they were added.
    auto qualifiedName{std::string()};
    for (const auto &element : resolvedImportPath) {
      qualifiedName += "::";
      qualifiedName += element;
    }
    auto &modules{context.compiler.mModulesByQualifiedName};
    if (auto itr{modules.find(qualifiedName)};
        itr != modules.end() && itr->second != thisModule) {
      auto otherModule{itr->second};
      if (auto error{otherModule->compile(context)}) {
        throw std::move(*error);
      }
      return otherModule;
    }
    return nullptr;
  }};
  auto searchCompilerBuiltins{[&]() -> Module * {
    // Builtin modules may be nested under builtin packages, e.g.,
    // `::models::prospect`, so the whole path joined with `::` forms
    // the builtin lookup key.
    auto key{std::string()};
    for (const auto &element : resolvedImportPath) {
      if (!key.empty()) key += "::";
      key += element;
    }
    if (auto mod{context.getBuiltinModule(key)}) return mod;
    return nullptr;
  }};

  if (isAbs) {
    // If the import path is absolute, meaning it starts with `::`,
    // prioritize compiler builtins first and compiler dir paths
    // second. This guarantees that `::df` always gets the builtin
    // df module for example.
    if (auto mod{searchCompilerBuiltins()}) return mod;
    if (auto mod{searchCompilerDirPaths()}) return mod;
  } else {
    // If the import path is relative, meaning it does NOT starts with `::`,
    // prioritize modules relative to the current module first.
    if (auto mod{searchRelativeToCurrentModule()}) return mod;
    // If the import path is not explicitly relative, meaning it does
    // not start with `.` or `..`, also search the compiler dir paths
    // first and then lastly default to compiler builtins.
    if (bool isExplicitlyRelative{importPath[0] == "." ||
                                  importPath[0] == ".."};
        !isExplicitlyRelative) {
      if (auto mod{searchCompilerDirPaths()}) return mod;
      if (auto mod{searchCompilerBuiltins()}) return mod;
    }
  }
  return nullptr;
}

void Emitter::resolveImportUsingAliases(
    uint64_t seqLimit, Span<const std::string_view> importPath,
    llvm::SmallVector<std::string_view> &resolvedImportPath) {
  for (const auto &importPathElem : importPath) {
    AST::UsingAlias *foundAlias{};
    uint64_t foundAliasSeq{};
    for (auto s{scope}; s && !foundAlias; s = s->parent) {
      for (auto itr{s->usingAliases.rbegin()}; itr != s->usingAliases.rend();
           ++itr) {
        if (itr->second < seqLimit &&
            itr->first->name.srcName == importPathElem) {
          foundAlias = itr->first;
          foundAliasSeq = itr->second;
          break;
        }
      }
    }
    if (foundAlias) {
      // Resolve the alias's own path against the aliases declared before
      // it, including the alias itself would recurse forever on
      // `using foo = foo::bar;`.
      resolveImportUsingAliases(foundAliasSeq, foundAlias->importPath,
                                resolvedImportPath);
    } else {
      resolvedImportPath.push_back(importPathElem);
    }
  }
}

} // namespace smdl
