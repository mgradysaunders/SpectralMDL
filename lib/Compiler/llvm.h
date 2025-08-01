#pragma once

#include <map>

#include "smdl/AST.h"
#include "smdl/Compiler.h"
#include "smdl/Support/Profiler.h"

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/ADT/Twine.h"
#include "llvm/ADT/TypeSwitch.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Support/Casting.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Transforms/Utils/BuildLibCalls.h"
#include "llvm/Transforms/Utils/Cloning.h"
#include "llvm/Transforms/Utils/Evaluator.h"

template <typename T> struct llvm::simplify_type<smdl::BumpPtr<T>> {
  using SimpleType = T *;
  static SimpleType &getSimplifiedValue(smdl::BumpPtr<T> &Val) {
    return Val.ptr;
  }
};

namespace smdl {

/// \defgroup Compiler Compiler Implementation
/// \{

void llvm_throw_if_error(llvm::Error error);

template <typename T>
[[nodiscard]] inline T llvm_throw_if_error(llvm::Expected<T> valueOrError) {
  if (!valueOrError)
    llvm_throw_if_error(valueOrError.takeError());
  return std::move(*valueOrError);
}

template <typename T>
[[nodiscard]] inline T llvm_throw_if_error(llvm::ErrorOr<T> valueOrError) {
  if (!valueOrError)
    llvm_throw_if_error(llvm::make_error<llvm::StringError>(
        "STL error: " + valueOrError.getError().message(),
        valueOrError.getError()));
  return std::move(*valueOrError);
}

class LLVMOptimizer final {
public:
  LLVMOptimizer() : passBuilder(NativeTarget::get().machine) {
    passBuilder.registerModuleAnalyses(moduleAnalysis);
    passBuilder.registerCGSCCAnalyses(cgsccAnalysis);
    passBuilder.registerFunctionAnalyses(funcAnalysis);
    passBuilder.registerLoopAnalyses(loopAnalysis);
    passBuilder.crossRegisterProxies(loopAnalysis, funcAnalysis, cgsccAnalysis,
                                     moduleAnalysis);
  }

  void run(llvm::Module &mod, llvm::OptimizationLevel level) {
    auto pipeline{passBuilder.buildPerModuleDefaultPipeline(level)};
    pipeline.run(mod, moduleAnalysis);
  }
  void run(llvm::Function &function, llvm::OptimizationLevel level) {
    auto pipeline{passBuilder.buildFunctionSimplificationPipeline(
        level, llvm::ThinOrFullLTOPhase::None)};
    pipeline.run(function, funcAnalysis);
  }

  llvm::PassBuilder passBuilder;
  llvm::LoopAnalysisManager loopAnalysis;
  llvm::FunctionAnalysisManager funcAnalysis;
  llvm::CGSCCAnalysisManager cgsccAnalysis;
  llvm::ModuleAnalysisManager moduleAnalysis;
};

[[nodiscard]] inline auto llvm_int_ptr_type(llvm::LLVMContext &context) {
  return llvm::Type::getIntNTy(context, sizeof(void *) * 8);
}

[[nodiscard]] inline auto llvm_ptr_as_constant_int(llvm::LLVMContext &context,
                                                   const void *ptr) {
  uintptr_t addr{};
  std::memcpy(&addr, &ptr, sizeof(ptr));
  static_assert(sizeof(addr) == sizeof(ptr));
  return llvm::ConstantInt::get(llvm_int_ptr_type(context),
                                llvm::APInt(sizeof(addr) * 8, addr));
}

template <typename T>
[[nodiscard]] inline T *llvm_constant_int_as_ptr(llvm::Value *value) {
  if (auto constantInt{llvm::dyn_cast<llvm::ConstantInt>(value)}) {
    uintptr_t addr = constantInt->getValue().getLimitedValue(
        std::numeric_limits<uintptr_t>::max());
    T *ptr{};
    std::memcpy(&ptr, &addr, sizeof(void *));
    return ptr;
  }
  return nullptr;
}

llvm::Value *llvm_emit_cast(llvm::IRBuilderBase &builder, llvm::Value *value,
                            llvm::Type *dstType);

llvm::Value *llvm_emit_powi(llvm::IRBuilderBase &builder, llvm::Value *lhs,
                            llvm::Value *rhs);

llvm::Value *llvm_emit_ldexp(llvm::IRBuilderBase &builder, llvm::Value *lhs,
                             llvm::Value *rhs);

llvm::InlineResult llvm_force_inline(llvm::Value *value,
                                     bool isRecursive = false);

void llvm_force_inline_flatten(llvm::Function &func);

void llvm_move_block_to_end(llvm::BasicBlock *block);

template <typename, typename = void> struct llvm_get_type_template;

template <> struct llvm_get_type_template<void, void> {
  [[nodiscard]] static llvm::Type *get(llvm::LLVMContext &context) {
    return llvm::Type::getVoidTy(context);
  }
};

template <typename T>
struct llvm_get_type_template<T,
                              std::enable_if_t<std::is_integral_v<T>, void>> {
  [[nodiscard]] static llvm::Type *get(llvm::LLVMContext &context) {
    return llvm::Type::getIntNTy(context, sizeof(T) * 8);
  }
};

template <> struct llvm_get_type_template<float, void> {
  [[nodiscard]] static llvm::Type *get(llvm::LLVMContext &context) {
    return llvm::Type::getFloatTy(context);
  }
};

template <> struct llvm_get_type_template<double, void> {
  [[nodiscard]] static llvm::Type *get(llvm::LLVMContext &context) {
    return llvm::Type::getDoubleTy(context);
  }
};

template <typename T> struct llvm_get_type_template<T *, void> {
  [[nodiscard]] static llvm::Type *get(llvm::LLVMContext &context) {
    return llvm::PointerType::get(context, 0);
  }
};

template <typename T> struct llvm_get_type_template<T &, void> {
  [[nodiscard]] static llvm::Type *get(llvm::LLVMContext &context) {
    return llvm::PointerType::get(context, 0);
  }
};

template <typename T, typename... U>
struct llvm_get_type_template<T(U...), void> {
  [[nodiscard]] static llvm::FunctionType *get(llvm::LLVMContext &context) {
    return llvm::FunctionType::get(llvm_get_type_template<T>::get(context),
                                   {llvm_get_type_template<U>::get(context)...},
                                   /*isVarArg=*/false);
  }
};

template <typename T>
[[nodiscard]] auto *llvm_get_type(llvm::LLVMContext &context) {
  return llvm_get_type_template<T>::get(context);
}

template <typename T> class SmallVectorOf {
public:
  // Only use `llvm::SmallVector` if `T` is reasonably small.
  [[nodiscard]] static constexpr auto preferred_vector_type() noexcept {
    if constexpr (sizeof(T) <= 256) {
      return llvm::SmallVector<T>();
    } else {
      return std::vector<T>();
    }
  }

  using vector_type = std::decay_t<decltype(preferred_vector_type())>;

  SmallVectorOf() = default;

  template <typename Iterator>
  SmallVectorOf(Iterator itr0, Iterator itrN) : elems(itr0, itrN) {}

  /// Is empty?
  [[nodiscard]] bool empty() const noexcept { return elems.empty(); }

  /// The size.
  [[nodiscard]] size_t size() const noexcept { return elems.size(); }

  /// The begin iterator.
  [[nodiscard]] auto begin() noexcept { return elems.begin(); }

  /// The begin iterator, const variant.
  [[nodiscard]] auto begin() const noexcept { return elems.begin(); }

  /// The end iterator.
  [[nodiscard]] auto end() noexcept { return elems.end(); }

  /// The end iterator, const variant.
  [[nodiscard]] auto end() const noexcept { return elems.end(); }

  /// The index access operator.
  [[nodiscard]] auto operator[](size_t i) noexcept -> T & { return elems[i]; }

  /// The index access operator, const variant.
  [[nodiscard]] auto operator[](size_t i) const noexcept -> const T & {
    return elems[i];
  }

public:
  /// Emplace back.
  template <typename... Args> T &emplace_back(Args &&...args) {
    return elems.emplace_back(std::forward<Args>(args)...);
  }

  /// Push back.
  void push_back(T elem) { elems.push_back(std::move(elem)); }

  /// Clear.
  void clear() { elems.clear(); }

public:
  /// Is the given predicate true for any element? Returns false if empty.
  template <typename Pred> [[nodiscard]] bool is_any_true(Pred &&pred) const {
    for (auto &elem : elems)
      if (std::invoke(pred, elem))
        return true;
    return false;
  }

  /// Is the given predicate true for all elements? Returns true if empty.
  template <typename Pred> [[nodiscard]] bool is_all_true(Pred &&pred) const {
    for (auto &elem : elems)
      if (!std::invoke(pred, elem))
        return false;
    return true;
  }

  /// Transform into another `std::vector` by passing each element to
  /// the given predicate.
  template <typename U, typename Pred>
  [[nodiscard]] auto transform(Pred &&pred) const {
    auto results{std::vector<U>(size_t(elems.size()))};
    auto resultItr{results.begin()};
    for (auto &elem : elems)
      *resultItr++ = std::invoke(pred, elem);
    return results;
  }

public:
  vector_type elems{};
};

/// \}

} // namespace smdl
