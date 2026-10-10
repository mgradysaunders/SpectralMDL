#include "llvm.h"

#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/StringSaver.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/TargetParser/Host.h"

#include "smdl/Support/Logger.h"

#include "Support/Environment.h"

namespace smdl {

void llvmThrowIfError(llvm::Error error) {
  uint32_t numErrors{};
  std::string message{};
  llvm::raw_string_ostream os{message};
  llvm::cantFail(llvm::handleErrors(std::move(error),
                                    [&](const llvm::ErrorInfoBase &errorInfo) {
                                      errorInfo.log(os);
                                      numErrors++;
                                    }));
  if (numErrors > 0) throw Error(std::move(message));
}

namespace {
// Give LLVM the options in 'SMDL_LLVM_ARGS', as if they were the command
// line of a program built on it. LLVM keeps its options for the whole
// process, so this runs once, before anything reads them: the target
// machine reads its code generation options as it is made, and each
// 'LLVMOptimizer' reads the rest as it is set up.
//
// NOTE: The parse makes the top-level command the active one, so a host
// that parses its own command line through LLVM must ask which of its
// subcommands is active before the first compile, not after.
void parseLLVMArgs() {
  const std::string &args{Environment::get().llvmArgs};
  if (args.empty()) return;
  SMDL_LOG_WARN("Passing LLVM its own options, because SMDL_LLVM_ARGS is ",
                SpellQuoted(args));
  llvm::BumpPtrAllocator allocator{};
  llvm::StringSaver saver{allocator};
  // LLVM's messages name the program after the first argument.
  llvm::SmallVector<const char *> argv{"SMDL_LLVM_ARGS"};
  llvm::cl::TokenizeGNUCommandLine(args, saver, argv);
  // Given a stream to write them to, the parser reports errors there
  // rather than exiting the process. It still applies every option it
  // understood. Each line it writes begins with the program name, so it
  // stands as a warning on its own.
  std::string errors{};
  llvm::raw_string_ostream os{errors};
  if (!llvm::cl::ParseCommandLineOptions(int(argv.size()), argv.data(),
                                         /*Overview=*/"", &os)) {
    llvm::SmallVector<llvm::StringRef> lines{};
    llvm::StringRef(errors).split(lines, '\n', /*MaxSplit=*/-1,
                                  /*KeepEmpty=*/false);
    for (llvm::StringRef line : lines) SMDL_LOG_WARN(std::string_view(line));
  }
}
} // namespace

const NativeTarget &NativeTarget::get() noexcept {
  // Lazy magic static: initializing LLVM at static-initialization time
  // would run before 'main' in every process linking the library and be
  // exposed to static-init-order hazards.
  static const NativeTarget nativeTarget{[]() {
    // Both of these return true on failure, which happens if the LLVM we
    // linked has no code generator for this machine. CMake is supposed to
    // have guaranteed otherwise, so say so plainly here instead of letting it
    // resurface as a baffling 'lookupTarget' failure below.
    if (llvm::InitializeNativeTarget() ||
        llvm::InitializeNativeTargetAsmPrinter())
      llvm::report_fatal_error("LLVM has no code generator for this machine");
    parseLLVMArgs();
    std::string name{llvm::sys::getHostCPUName()};
    std::string triple{llvm::sys::getDefaultTargetTriple()};
    std::string targetError{};
    const llvm::Target *target{
        llvm::TargetRegistry::lookupTarget(llvm::Triple(triple), targetError)};
    if (!target) llvm::report_fatal_error(targetError.c_str());
    llvm::TargetOptions opts{};
    return NativeTarget{name, triple,
                        target->createTargetMachine(llvm::Triple(triple), name,
                                                    "", opts,
                                                    llvm::Reloc::PIC_)};
  }()};
  return nativeTarget;
}

llvm::Value *llvmEmitCast(llvm::IRBuilderBase &builder, llvm::Value *value,
                          llvm::Type *dstType) {
  llvm::Type *srcType{value->getType()};
  if (srcType == dstType) return value;
  bool isSrcFP{srcType->isFPOrFPVectorTy()};
  bool isDstFP{dstType->isFPOrFPVectorTy()};
  // float => float
  if (isSrcFP && isDstFP) return builder.CreateFPCast(value, dstType);
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
  if (isSrcBool && isDstFP) return builder.CreateUIToFP(value, dstType);
  // float => bool
  if (isSrcFP && isDstBool)
    return builder.CreateFCmpONE(value, llvm::Constant::getNullValue(srcType));
  // int => float
  if (isSrcInt && isDstFP) return builder.CreateSIToFP(value, dstType);
  // float => int
  if (isSrcFP && isDstInt) {
    if (srcType->getScalarSizeInBits() != dstType->getScalarSizeInBits()) {
      llvm::Type *dstTypeSameSize =
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

llvm::Value *llvmEmitPowi(llvm::IRBuilderBase &builder, llvm::Value *lhs,
                          llvm::Value *rhs) {
  llvm::Function *func{llvm::Intrinsic::getOrInsertDeclaration(
      builder.GetInsertBlock()->getModule(), llvm::Intrinsic::powi,
      {lhs->getType(), rhs->getType()})};
  return builder.CreateCall(func, {lhs, rhs});
}

llvm::Value *llvmEmitLdexp(llvm::IRBuilderBase &builder, llvm::Value *lhs,
                           llvm::Value *rhs) {
  llvm::Function *func{llvm::Intrinsic::getOrInsertDeclaration(
      builder.GetInsertBlock()->getModule(), llvm::Intrinsic::ldexp,
      {lhs->getType(), rhs->getType()})};
  return builder.CreateCall(func, {lhs, rhs});
}

llvm::InlineResult llvmForceInline(llvm::Value *value, bool isRecursive) {
  llvm::CallBase *call{llvm::dyn_cast_if_present<llvm::CallBase>(value)};
  if (!call) return llvm::InlineResult::failure("expected 'llvm::CallBase'");
  llvm::InlineFunctionInfo resultInfo{};
  llvm::InlineResult result{llvm::InlineFunction(*call, resultInfo)};
  if (result.isSuccess() && isRecursive) {
    llvm::SmallVector<llvm::CallBase *> todo{};
    todo.insert(todo.end(), resultInfo.InlinedCallSites.begin(),
                resultInfo.InlinedCallSites.end());
    while (!todo.empty()) {
      llvm::CallBase *next{todo.back()};
      todo.pop_back();
      llvm::InlineFunctionInfo info{};
      if (llvm::InlineFunction(*next, info).isSuccess()) {
        todo.insert(todo.end(), info.InlinedCallSites.begin(),
                    info.InlinedCallSites.end());
      }
    }
  }
  return result;
}

void llvmForceInlineFlatten(llvm::Function &func) {
  llvm::SmallVector<llvm::CallBase *> calls{};
  for (auto &block : func) {
    for (auto &inst : block) {
      if (llvm::CallBase * call{llvm::dyn_cast<llvm::CallBase>(&inst)}) {
        calls.push_back(call);
      }
    }
  }
  for (auto *call : calls) {
    llvmForceInline(call, /*isRecursive=*/true);
  }
}

void llvmMoveBlockToEnd(llvm::BasicBlock *block) {
  llvm::Function *func{block->getParent()};
  SMDL_SANITY_CHECK(func);
  block->removeFromParent();
  func->insert(func->end(), block);
}

} // namespace smdl
