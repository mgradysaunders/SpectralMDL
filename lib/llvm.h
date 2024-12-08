#pragma once

#include <cassert>
#include <functional>
#include <limits>
#include <locale>
#include <string>

#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseMapInfo.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/ADT/Twine.h"
#include "llvm/ADT/TypeSwitch.h"
#include "llvm/ExecutionEngine/Orc/LLJIT.h"
#include "llvm/ExecutionEngine/Orc/ThreadSafeModule.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Support/Allocator.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/CodeGen.h"
#include "llvm/Support/ConvertUTF.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/WithColor.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Transforms/Utils/BuildLibCalls.h"
#include "llvm/Transforms/Utils/Cloning.h"
#include "llvm/Transforms/Utils/Evaluator.h"

#include "smdl/MDLInstance.h"

#define sanity_check(cond, ...)                                                           \
  do {                                                                                    \
    if (!(cond))                                                                          \
      ::smdl::sanity_check_failure(__FILE__, __LINE__, #cond __VA_OPT__(, ) __VA_ARGS__); \
  } while (false)

#define sanity_check_nonnull(expr)   \
  ([&]() {                           \
    sanity_check((expr) != nullptr); \
    return expr;                     \
  }())

namespace smdl {

[[noreturn]] void sanity_check_failure(const char *file, int line, const char *cond, std::string_view more = {});

[[nodiscard]] inline std::string to_lower(llvm::StringRef str) { return str.lower(); }

[[nodiscard]] inline std::string to_upper(llvm::StringRef str) { return str.upper(); }

llvm::StringRef llvm_get_native_target_triple();

llvm::TargetMachine *llvm_get_native_target_machine();

template <typename Func> [[nodiscard]] std::optional<Error> catch_and_return_error(Func &&func) try {
  std::invoke(std::forward<Func>(func));
  return std::nullopt;
} catch (const Error &error) {
  return error;
} catch (const std::exception &error) {
  return Error(error.what());
} catch (...) {
  return Error("unknown error converted from unknown exception type");
}

void llvm_throw_if_error(llvm::Error error);

template <typename T> [[nodiscard]] inline T llvm_throw_if_error(llvm::Expected<T> valueOrError) {
  if (!valueOrError)
    llvm_throw_if_error(valueOrError.takeError());
  return std::move(*valueOrError);
}

template <typename T> [[nodiscard]] inline T llvm_throw_if_error(llvm::ErrorOr<T> valueOrError) {
  if (!valueOrError)
    llvm_throw_if_error(llvm::make_error<llvm::StringError>(
        std::format("STL error: {}", valueOrError.getError().message()), valueOrError.getError()));
  return std::move(*valueOrError);
}

template <typename T> [[nodiscard]] inline T *llvm_force_cast(llvm::Value *llvmValue) {
  sanity_check(llvmValue);
  sanity_check(llvm::isa<T>(llvmValue));
  return static_cast<T *>(llvmValue);
}

[[nodiscard]] inline auto llvm_twine(const std::string &name) { return llvm::Twine(name); }

[[nodiscard]] inline auto llvm_twine(std::string_view name) { return llvm::Twine(name); }

[[nodiscard]] inline auto llvm_twine(llvm::StringRef name) { return llvm::Twine(name); }

[[nodiscard]] inline auto llvm_twine(const char *name) { return llvm::Twine(name); }

[[nodiscard]] inline auto llvm_twine(const auto &name0, const auto &name1, const auto &...names) {
  return llvm_twine(name0) + llvm_twine(name1, names...);
}

[[nodiscard]] inline auto llvm_int_ptr_type(llvm::LLVMContext &context) {
  return llvm::Type::getIntNTy(context, sizeof(void *) * 8);
}

[[nodiscard]] inline llvm::Constant *llvm_ptr_as_constant_int(llvm::LLVMContext &context, const void *ptr) {
  uintptr_t addr{};
  std::memcpy(&addr, &ptr, sizeof(ptr));
  static_assert(sizeof(addr) == sizeof(ptr));
  return llvm::ConstantInt::get(llvm_int_ptr_type(context), llvm::APInt(sizeof(addr) * 8, addr));
}

template <typename T> [[nodiscard]] inline T *llvm_constant_int_as_ptr(llvm::Value *value) {
  if (auto constantInt{llvm::dyn_cast<llvm::ConstantInt>(value)}) {
    uintptr_t addr = constantInt->getValue().getLimitedValue(std::numeric_limits<uintptr_t>::max());
    T *ptr{};
    std::memcpy(&ptr, &addr, sizeof(void *));
    return ptr;
  }
  return nullptr;
}

inline void llvm_move_block_to_end(llvm::BasicBlock *block) {
  auto function{block->getParent()};
  block->removeFromParent();
  function->insert(function->end(), block);
}

[[nodiscard]] inline llvm::IRBuilderBase::InsertPoint llvm_insert_point_before_terminator(llvm::BasicBlock *block) {
  sanity_check(block->getTerminator());
  auto itr{block->end()};
  --itr;
  return llvm::IRBuilderBase::InsertPoint(block, itr);
}

[[nodiscard]] inline llvm::Value *llvm_emit_cast(llvm::IRBuilderBase &builder, llvm::Value *value, llvm::Type *dstType) {
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
  bool isSrcBool{isSrcInt && srcType->getScalarType()->getPrimitiveSizeInBits() == 1};
  bool isDstBool{isDstInt && dstType->getScalarType()->getPrimitiveSizeInBits() == 1};
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
  if (isSrcFP && isDstInt)
    return builder.CreateFPToSI(value, dstType);
  // int => int
  if (isSrcInt && isDstInt)
    return builder.CreateIntCast(value, dstType, /*isSigned=*/true);
  return nullptr;
}

[[nodiscard]] inline llvm::Value *llvm_emit_powi(llvm::IRBuilderBase &builder, llvm::Value *lhs, llvm::Value *rhs) {
  auto func{llvm::Intrinsic::getDeclaration(
      builder.GetInsertBlock()->getModule(), llvm::Intrinsic::powi, {lhs->getType(), rhs->getType()})};
  return builder.CreateCall(func, {lhs, rhs});
}

[[nodiscard]] inline llvm::Value *llvm_emit_ldexp(llvm::IRBuilderBase &builder, llvm::Value *lhs, llvm::Value *rhs) {
  auto func{llvm::Intrinsic::getDeclaration(
      builder.GetInsertBlock()->getModule(), llvm::Intrinsic::ldexp, {lhs->getType(), rhs->getType()})};
  return builder.CreateCall(func, {lhs, rhs});
}

inline llvm::InlineResult llvm_force_inline(llvm::CallBase &call, bool isRecursive = false) {
  auto resultInfo{llvm::InlineFunctionInfo{}};
  auto result{llvm::InlineFunction(call, resultInfo)};
  if (result.isSuccess() && isRecursive) {
    auto todo{llvm::SmallVector<llvm::CallBase *>{}};
    todo.insert(todo.end(), resultInfo.InlinedCallSites.begin(), resultInfo.InlinedCallSites.end());
    while (!todo.empty()) {
      auto next{todo.back()};
      todo.pop_back();
      auto info{llvm::InlineFunctionInfo{}};
      if (llvm::InlineFunction(*next, info).isSuccess())
        todo.insert(todo.end(), info.InlinedCallSites.begin(), info.InlinedCallSites.end());
    }
  }
  return result;
}

inline void llvm_force_inline_flatten(llvm::Function &func) {
  auto calls{llvm::SmallVector<llvm::CallBase *>{}};
  for (auto &block : func)
    for (auto &inst : block)
      if (auto call{llvm::dyn_cast<llvm::CallBase>(&inst)})
        calls.push_back(call);
  for (auto call : calls)
    llvm_force_inline(*call, /*isRecursive=*/true);
}

struct LLVMOptimizer final {
public:
  LLVMOptimizer() : passBuilder(llvm_get_native_target_machine()) {
    passBuilder.registerModuleAnalyses(moduleAnalysis);
    passBuilder.registerCGSCCAnalyses(cgsccAnalysis);
    passBuilder.registerFunctionAnalyses(funcAnalysis);
    passBuilder.registerLoopAnalyses(loopAnalysis);
    passBuilder.crossRegisterProxies(loopAnalysis, funcAnalysis, cgsccAnalysis, moduleAnalysis);
  }

  void run(llvm::Module &module, llvm::OptimizationLevel level) {
    auto pipeline{passBuilder.buildPerModuleDefaultPipeline(level)};
    pipeline.run(module, moduleAnalysis);
  }
  void run(llvm::Function &function, llvm::OptimizationLevel level) {
    auto pipeline{passBuilder.buildFunctionSimplificationPipeline(level, llvm::ThinOrFullLTOPhase::None)};
    pipeline.run(function, funcAnalysis);
  }

  llvm::PassBuilder passBuilder;
  llvm::LoopAnalysisManager loopAnalysis;
  llvm::FunctionAnalysisManager funcAnalysis;
  llvm::CGSCCAnalysisManager cgsccAnalysis;
  llvm::ModuleAnalysisManager moduleAnalysis;
};

struct unique_bump_ptr_deleter final {
public:
  template <typename T> void operator()(T *ptr) const { std::destroy_at(ptr); }
};

template <typename T> using unique_bump_ptr = std::unique_ptr<T, unique_bump_ptr_deleter>;

namespace detail {

[[nodiscard]] inline bool is_all_true(auto &&range, auto &&pred) {
  for (auto &&each : std::forward<decltype(range)>(range))
    if (!std::invoke(pred, each))
      return false;
  return true;
}

[[nodiscard]] inline bool is_any_true(auto &&range, auto &&pred) {
  for (auto &&each : std::forward<decltype(range)>(range))
    if (std::invoke(pred, each))
      return true;
  return false;
}

} // namespace detail

template <typename T> struct FormatJoin final {
  constexpr FormatJoin() = default;

  constexpr FormatJoin(llvm::ArrayRef<T> values, llvm::StringRef delim) : values(values), delim(delim) {}

  llvm::ArrayRef<T> values{};

  llvm::StringRef delim{};
};

template <typename T> //
[[nodiscard]] inline auto format_join(llvm::ArrayRef<T> values, llvm::StringRef delim) {
  return FormatJoin<T>(values, delim);
}

template <typename T> [[nodiscard]] inline auto format_join(const llvm::SmallVectorImpl<T> &values, llvm::StringRef delim) {
  return FormatJoin<T>(values, delim);
}

[[nodiscard]] inline std::string load_file_to_string(llvm::StringRef fname) {
  auto buffer{llvm_throw_if_error(llvm::MemoryBuffer::getFile(fname, /*isText=*/true))};
  return buffer->getBuffer().str();
}

} // namespace smdl

template <> struct std::formatter<llvm::StringRef, char> {
public:
  constexpr auto parse(auto &ctx) { return ctx.begin(); }

  constexpr auto format(llvm::StringRef str, auto &ctx) const {
    auto itr{ctx.out()};
    for (auto &ch : str)
      *itr++ = ch;
    return itr;
  }
};

template <unsigned N> struct std::formatter<llvm::SmallString<N>, char> {
  constexpr auto parse(auto &ctx) { return ctx.begin(); }

  auto format(const llvm::SmallString<N> &str, auto &ctx) const {
    auto itr{ctx.out()};
    for (auto &ch : str)
      *itr++ = ch;
    return itr;
  }
};

template <> struct std::formatter<llvm::Twine, char> {
  constexpr auto parse(auto &ctx) { return ctx.begin(); }

  auto format(const llvm::Twine &str, auto &ctx) const {
    auto itr{ctx.out()};
    if (!str.isTriviallyEmpty()) {
      if (str.isSingleStringRef()) {
        for (auto &ch : str.getSingleStringRef())
          *itr++ = ch;
      } else {
        llvm::SmallString<64> buf{};
        str.toVector(buf);
        for (auto &ch : buf)
          *itr++ = ch;
      }
    }
    return itr;
  }
};

template <typename T> struct std::formatter<smdl::FormatJoin<T>, char> {
  constexpr auto parse(auto &ctx) { return ctx.begin(); }

  auto format(const smdl::FormatJoin<T> &join, auto &ctx) const {
    auto itr{ctx.out()};
    for (size_t i{}; i < join.values.size(); i++) {
      itr = std::format_to(itr, "{}", join.values[i]);
      if (i + 1 < join.values.size())
        for (auto &ch : join.delim)
          *itr++ = ch;
    }
    return itr;
  }
};
