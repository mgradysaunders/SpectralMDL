#include "llvm.h"

namespace smdl {

void llvm_throw_if_error(llvm::Error error) {
  uint32_t numErrors{};
  std::string message{};
  llvm::raw_string_ostream os{message};
  llvm::cantFail(llvm::handleErrors(std::move(error),
                                    [&](const llvm::ErrorInfoBase &errorInfo) {
                                      errorInfo.log(os);
                                      numErrors++;
                                    }));
  if (numErrors > 0)
    throw Error(std::move(message));
}

llvm::Value *llvm_emit_cast(llvm::IRBuilderBase &builder, llvm::Value *value,
                            llvm::Type *dstType) {
  auto srcType{value->getType()};
  if (srcType == dstType)
    return value;
  bool isSrcFP{srcType->isFPOrFPVectorTy()};
  bool isDstFP{dstType->isFPOrFPVectorTy()};
  // float => float
  if (isSrcFP && isDstFP)
    return builder.CreateFPCast(value, dstType);
  bool isSrcInt{srcType->isIntOrIntVectorTy()};
  bool isDstInt{dstType->isIntOrIntVectorTy()};
  bool isSrcBool{isSrcInt && srcType->getScalarSizeInBits() == 1};
  bool isDstBool{isDstInt && dstType->getScalarSizeInBits() == 1};
  // bool => int
  if (isSrcBool && isDstInt)
    return builder.CreateIntCast(value, dstType, /*isSigned=*/false);
  // int => bool
  if (isSrcInt && isDstBool)
    return builder.CreateICmpNE(value, llvm::Constant::getNullValue(srcType));
  // bool => float
  if (isSrcBool && isDstFP)
    return builder.CreateUIToFP(value, dstType);
  // float => bool
  if (isSrcFP && isDstBool)
    return builder.CreateFCmpONE(value, llvm::Constant::getNullValue(srcType));
  // int => float
  if (isSrcInt && isDstFP)
    return builder.CreateSIToFP(value, dstType);
  // float => int
  if (isSrcFP && isDstInt) {
    if (srcType->getScalarSizeInBits() != dstType->getScalarSizeInBits()) {
      auto dstTypeSameSize =
          dstType->getWithNewBitWidth(srcType->getScalarSizeInBits());
      value = builder.CreateFPToSI(value, dstTypeSameSize);
      return builder.CreateIntCast(value, dstType, /*isSigned=*/true);
    } else {
      return builder.CreateFPToSI(value, dstType);
    }
  }
  // int => int
  if (isSrcInt && isDstInt)
    return builder.CreateIntCast(value, dstType, /*isSigned=*/true);
  return nullptr;
}

llvm::Value *llvm_emit_powi(llvm::IRBuilderBase &builder, llvm::Value *lhs,
                            llvm::Value *rhs) {
  auto func{llvm::Intrinsic::getOrInsertDeclaration(
      builder.GetInsertBlock()->getModule(), llvm::Intrinsic::powi,
      {lhs->getType(), rhs->getType()})};
  return builder.CreateCall(func, {lhs, rhs});
}

llvm::Value *llvm_emit_ldexp(llvm::IRBuilderBase &builder, llvm::Value *lhs,
                             llvm::Value *rhs) {
  auto func{llvm::Intrinsic::getOrInsertDeclaration(
      builder.GetInsertBlock()->getModule(), llvm::Intrinsic::ldexp,
      {lhs->getType(), rhs->getType()})};
  return builder.CreateCall(func, {lhs, rhs});
}

llvm::InlineResult llvm_force_inline(llvm::Value *value, bool isRecursive) {
  auto call{llvm::dyn_cast_if_present<llvm::CallBase>(value)};
  if (!call)
    return llvm::InlineResult::failure("expected 'llvm::CallBase'");
  auto resultInfo{llvm::InlineFunctionInfo{}};
  auto result{llvm::InlineFunction(*call, resultInfo)};
  if (result.isSuccess() && isRecursive) {
    auto todo{llvm::SmallVector<llvm::CallBase *>{}};
    todo.insert(todo.end(), resultInfo.InlinedCallSites.begin(),
                resultInfo.InlinedCallSites.end());
    while (!todo.empty()) {
      auto next{todo.back()};
      todo.pop_back();
      auto info{llvm::InlineFunctionInfo{}};
      if (llvm::InlineFunction(*next, info).isSuccess()) {
        todo.insert(todo.end(), info.InlinedCallSites.begin(),
                    info.InlinedCallSites.end());
      }
    }
  }
  return result;
}

void llvm_force_inline_flatten(llvm::Function &func) {
  auto calls{llvm::SmallVector<llvm::CallBase *>{}};
  for (auto &block : func) {
    for (auto &inst : block) {
      if (auto call{llvm::dyn_cast<llvm::CallBase>(&inst)}) {
        calls.push_back(call);
      }
    }
  }
  for (auto *call : calls) {
    llvm_force_inline(call, /*isRecursive=*/true);
  }
}

void llvm_move_block_to_end(llvm::BasicBlock *block) {
  auto func{block->getParent()};
  SMDL_SANITY_CHECK(func);
  block->removeFromParent();
  func->insert(func->end(), block);
}

} // namespace smdl
