#include "llvm.h"

#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/TargetParser/Host.h"

namespace smdl {

struct LLVMNativeTarget final {
  std::string triple{};
  llvm::TargetMachine *machine{};
};

static LLVMNativeTarget llvmNativeTarget{};

void init_or_exit() {
  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmPrinter();
  std::string err;
  auto triple{llvm::sys::getDefaultTargetTriple()};
  auto target{llvm::TargetRegistry::lookupTarget(triple, err)};
  if (!target) {
    llvm::errs() << err;
    std::exit(EXIT_FAILURE);
  }
  llvm::TargetOptions opts{};
  llvmNativeTarget.triple = triple;
  llvmNativeTarget.machine = target->createTargetMachine(triple, llvm::sys::getHostCPUName(), "", opts, llvm::Reloc::PIC_);
}

void Error::print() const {
  llvm::WithColor(llvm::errs(), llvm::HighlightColor::Error) << "[error] ";
  if (!file.empty() && line > 0)
    llvm::WithColor(llvm::errs(), llvm::HighlightColor::Address) << '[' << file << ':' << line << "] ";
  llvm::errs() << message << '\n';
}

llvm::StringRef llvm_get_native_target_triple() { return llvmNativeTarget.triple; }

llvm::TargetMachine *llvm_get_native_target_machine() { return llvmNativeTarget.machine; }

void sanity_check_failure(const char *file, int line, const char *cond, std::string_view more) {
  std::string message{std::format(
      "Sanity check failure!\n"
      "  File = {}\n"
      "  Line = {}\n"
      "  Cond = {}\n",
      file, line, cond)};
  if (!more.empty())
    message += std::format("\n...\n\n{}\n", more);
  llvm::report_fatal_error(message.c_str());
}

void llvm_throw_if_error(llvm::Error error) {
  uint32_t numErrors{};
  std::string message{};
  llvm::raw_string_ostream os{message};
  llvm::cantFail(llvm::handleErrors(std::move(error), [&](const llvm::ErrorInfoBase &errorInfo) {
    errorInfo.log(os);
    numErrors++;
  }));
  if (numErrors > 0)
    throw Error(std::move(message));
}

} // namespace smdl
