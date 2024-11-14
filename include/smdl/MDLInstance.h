#pragma once

#include <map>
#include <set>

#include "smdl/FileLocator.h"
#include "smdl/ImageLoader.h"
#include "smdl/type.h"

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

class DataLookup;

struct PtexTexture final {
  void *texture{};

  void *filter{};
};

enum class OptLevel : uint32_t { None, O1, O2, O3 };

enum class OutputFormat : uint32_t { IR, Assembly, Object };

using eval_opacity_t = float_t (*)(state_t &state);

using eval_bsdf_t = int_t (*)( //
    state_t &state, const float3_t &wo, const float3_t &wi, float_t &pdf_fwd, float_t &pdf_rev, float_t *f);

using eval_bsdf_sample_t = int_t (*)(
    state_t &state, const float4_t &xi, const float3_t &wo, float3_t &wi, float_t &pdf_fwd, float_t &pdf_rev, float_t *f,
    int_t &is_delta);

template <typename F> struct FunctionJIT final {
  std::string linkName{};

  F func{};
};

struct MaterialJIT final {
  std::string moduleName{};

  std::string name{};

  FunctionJIT<eval_opacity_t> evalOpacity{};

  FunctionJIT<eval_bsdf_t> evalBsdf{};

  FunctionJIT<eval_bsdf_sample_t> evalBsdfSample{};
};

class SMDL_EXPORT MDLInstance final {
public:
  MDLInstance(uint32_t numWavelens = 16);

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

  template <typename T> //
  [[nodiscard]] auto *lookup_jit_symbol(std::string_view name) {
    return reinterpret_cast<T *>(lookup_jit_symbol(name));
  }

public:
  void set_data_int(std::string_view name, int_t value);

  void set_data_int2(std::string_view name, int2_t value);

  void set_data_int3(std::string_view name, int3_t value);

  void set_data_int4(std::string_view name, int4_t value);

  void set_data_float(std::string_view name, float_t value);

  void set_data_float2(std::string_view name, float2_t value);

  void set_data_float3(std::string_view name, float3_t value);

  void set_data_float4(std::string_view name, float4_t value);

  void set_data_color(std::string_view name, std::span<const float_t> value);

public:
  FileLocator fileLocator{};

  ImageLoader imageLoader{default_image_loader};

  bool enableDebug{false};

  bool enableUnitTests{false};

  uint32_t numWavelens{16};

  typedef void (*unit_test_t)(const state_t &);

  struct UnitTest final {
    std::string llvmName{};

    std::string name{};

    unit_test_t test{};
  };

  std::vector<UnitTest> unitTests{};

  std::map<std::string, Image, std::less<>> images{};

  std::map<std::string, PtexTexture, std::less<>> ptexTextures{};

private:
  std::set<std::string> modulePaths{};

  std::vector<std::unique_ptr<Compiler::Module>> modules;

  std::unique_ptr<llvm::orc::ThreadSafeModule> llvmJITModule;

  std::unique_ptr<llvm::orc::LLJIT> llvmJIT;

  std::unique_ptr<DataLookup> dataLookup;

  std::vector<MaterialJIT> materialJITs{};

  friend class Compiler::Context;

  friend class Compiler::Emitter;
};

} // namespace smdl
