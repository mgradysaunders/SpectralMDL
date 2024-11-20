#pragma once

#include <map>
#include <set>

#include "smdl/FileLocator.h"
#include "smdl/Image.h"

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
class StructType;

} // namespace smdl::Compiler

namespace smdl {

class DataLookup;

enum class OptLevel : uint32_t { None, O1, O2, O3 };

enum class OutputFormat : uint32_t { IR, Assembly, Object };

namespace jit {

template <typename> struct Function;

template <typename R, typename... Args> struct Function<R(Args...)> final {
public:
  using function_pointer = R (*)(Args...);

  std::string name{};

  function_pointer ptr{};

  R operator()(Args... args) const { return std::invoke(ptr, std::forward<Args>(args)...); }

  [[nodiscard]] operator bool() const { return ptr != nullptr; }
};

struct Material final {
public:
  std::string moduleName{};

  std::string name{};

  Function<float_t(const state_t &state)> evalOpacity{};

  Function<int_t(
      const state_t &state, const float3_t &wo, const float3_t &wi, //
      float_t &pdf_fwd, float_t &pdf_rev,                           //
      float_t *f)>
      evalBsdf{};

  Function<int_t(
      const state_t &state, const float4_t &xi, const float3_t &wo, //
      float3_t &wi, float_t &pdf_fwd, float_t &pdf_rev,             //
      float_t *f, int_t &is_delta)>
      evalBsdfSample{};
};

struct UnitTest final {
  std::string moduleName{};

  std::string name{};

  Function<void(const state_t &state)> exec{};
};

} // namespace jit

class SMDL_EXPORT MDLInstance final {
public:
  MDLInstance(uint32_t wavelengthBaseMax = 16);

  MDLInstance(const MDLInstance &) = delete;

  ~MDLInstance();

public:
  [[nodiscard]] llvm::LLVMContext &get_llvm_context() noexcept;

  [[nodiscard]] llvm::Module &get_llvm_module() noexcept;

  [[nodiscard]] std::optional<Error> load_all_modules(std::filesystem::path path);

  [[nodiscard]] std::optional<Error> load_module(std::filesystem::path path);

  [[nodiscard]] std::optional<Error> compile(OptLevel optLevel = OptLevel::O2) noexcept;

  [[nodiscard]] std::string dump(OutputFormat outputFormat);

  [[nodiscard]] std::optional<Error> compile_jit() noexcept;

  [[nodiscard]] std::optional<Error> execute_jit_unit_tests(const state_t &state) noexcept;

  [[nodiscard]] void *lookup_jit(std::string_view name) noexcept;

  template <typename T> bool lookup_jit(jit::Function<T> &func) noexcept {
    return (func.ptr = reinterpret_cast<typename jit::Function<T>::function_pointer>(lookup_jit(func.name))) != nullptr;
  }

  template <typename T> void lookup_jit_or_throw(jit::Function<T> &func) {
    if (!lookup_jit(func))
      throw Error(std::format("can't lookup JIT function '{}'", func.name));
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
  [[nodiscard]] std::span<const jit::Material> all_materials() const noexcept { return materials; }

  [[nodiscard]] const jit::Material *find_material(std::string_view name) const noexcept;

  [[nodiscard]] const jit::Material *find_material(std::string_view moduleName, std::string_view name) const noexcept;

  [[nodiscard]] float3_t color_to_rgb(const state_t &state, std::span<const float_t> color) const noexcept;

  void rgb_to_color(const state_t &state, const float3_t &rgb, std::span<float_t> color) const noexcept;

public:
  FileLocator fileLocator{};

  bool enableDebug{false};

  bool enableUnitTests{false};

  uint32_t wavelengthBaseMax{16};

private:
  std::map<std::string, Image, std::less<>> images{};

  std::map<std::string, Ptexture_t, std::less<>> ptextures{};

  std::set<std::string> modulePaths{};

  std::vector<std::unique_ptr<Compiler::Module>> modules;

  std::unique_ptr<llvm::orc::ThreadSafeModule> llvmJitModule;

  std::unique_ptr<llvm::orc::LLJIT> llvmJit;

  std::unique_ptr<DataLookup> dataLookup;

  jit::Function<void(const state_t &state, const float_t *color, float3_t &rgb)> colorToRgb{"color_to_rgb_jit", nullptr};

  jit::Function<void(const state_t &state, const float3_t &rgb, float_t *color)> rgbToColor{"rgb_to_color_jit", nullptr};

  std::vector<jit::Material> materials{};

  std::vector<jit::UnitTest> unitTests{};

  friend class Compiler::Context;

  friend class Compiler::Emitter;

  friend class Compiler::Module;

  friend class Compiler::StructType;
};

} // namespace smdl
