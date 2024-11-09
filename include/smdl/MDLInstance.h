#pragma once

#include <map>
#include <set>

#include "smdl/FileLocator.h"
#include "smdl/types.h"

namespace llvm {

class LLVMContext;
class Module;
class DataLayout;

namespace orc {

class ThreadSafeModule;
class LLJIT;

} // namespace orc

} // namespace llvm

namespace smdl::Compiler {

class Context;
class Emitter;
class Module;

} // namespace smdl::Compiler

namespace smdl {

struct Ptex_t final {
  void *texture{};

  void *filter{};
};

enum class OptLevel : unsigned { None, O1, O2, O3 };

enum class OutputFormat : unsigned { IR, Assembly, Object };

class SMDL_EXPORT MDLInstance final {
public:
  MDLInstance(unsigned numWavelens = 16);

  MDLInstance(const MDLInstance &) = delete;

  ~MDLInstance();

public:
  [[nodiscard]] llvm::LLVMContext &get_llvm_context();

  [[nodiscard]] llvm::Module &get_llvm_module();

  [[nodiscard]] std::optional<Error> load_all_modules(std::filesystem::path path);

  [[nodiscard]] std::optional<Error> load_module(std::filesystem::path path);

  [[nodiscard]] std::optional<Error> compile(OptLevel optLevel = OptLevel::O2);

  [[nodiscard]] std::string dump(OutputFormat outputFormat);

  [[nodiscard]] std::optional<Error> compile_jit();

  [[nodiscard]] std::optional<Error> execute_jit_unit_tests(const state_t &state);

  [[nodiscard]] void *lookup_jit_symbol(std::string_view name);

  template <typename T> [[nodiscard]] T *lookup_jit_symbol(std::string_view name) {
    return reinterpret_cast<T *>(lookup_jit_symbol(name));
  }

public:
  FileLocator fileLocator{};

  /// This represents the primary API for loading image file formats. By default, SpectralMDL does not
  /// want to enforce the usage of any specific library, i.e., we do not require OpenImageIO or OpenEXR.
  ///
  /// This repository does include another CMake library target `SpectralMDL::DefaultImageLoader` that
  /// uses STB and TinyEXR to support all relevant image formats.
  using ImageLoader = std::function<Image(const std::filesystem::path &fname)>;

  ImageLoader imageLoader{};

  bool enableDebug{false};

  bool enableUnitTests{false};

  typedef void (*unit_test_t)(const state_t &);

  struct UnitTest final {
    std::string llvmName{};

    std::string name{};

    unit_test_t test{};
  };

  std::vector<UnitTest> unitTests{};

  unsigned numWavelens{16};

private:
  std::map<std::string, Image, std::less<>> images{};

  std::map<std::string, Ptex_t, std::less<>> ptexs{};

  std::set<std::string> modulePaths{};

  std::vector<std::unique_ptr<Compiler::Module>> modules;

  std::unique_ptr<llvm::orc::ThreadSafeModule> llvmJITModule;

  std::unique_ptr<llvm::orc::LLJIT> llvmJIT;

  friend class Compiler::Context;

  friend class Compiler::Emitter;
};

} // namespace smdl
