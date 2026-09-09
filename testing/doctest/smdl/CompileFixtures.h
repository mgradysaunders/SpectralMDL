/// \file
/// What the library suite needs beyond `Fixtures.h`: compiling a snippet
/// of source, looking up a material, a state to evaluate one through,
/// reading the emitted LLVM-IR, and collecting what a compile logs.
///
/// The division of labor this serves: the SMDL-language suite pins what
/// the language means, and this suite pins what the host sees. A compile
/// that must fail and the words it fails with, the IR that must be
/// emitted, and the entry points the host calls are all things a
/// `unit_test` block cannot reach, because a failing compile aborts the
/// run and a running one cannot see its own IR.
#pragma once

#include "Fixtures.h"

#include <cstdint>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "smdl/Compiler.h"
#include "smdl/Support/Logger.h"

//--{ Compiling

/// Compile `sourceCode` as one host-supplied module and return the error
/// it fails with, or an error saying it did not fail.
///
/// The sentinel rather than an optional, so that a test asserting on the
/// message needs no null check and a compile that unexpectedly succeeds
/// fails on the same assertion as a wrong message.
[[nodiscard]] inline smdl::Error compileError(std::string sourceCode) {
  smdl::Compiler compiler{};
  if (auto error{compiler.addCode("::diag", std::move(sourceCode))})
    return *error;
  if (auto error{compiler.compile(smdl::OPT_LEVEL_NONE)}) return *error;
  return smdl::Error("compiled without error");
}

/// Add every path, compile, and JIT-compile, returning the first error
/// message or the empty string on success.
[[nodiscard]] inline std::string
buildAll(smdl::Compiler &compiler,
         const std::vector<std::filesystem::path> &paths,
         std::vector<std::string> *addedNames = nullptr,
         smdl::OptLevel optLevel = smdl::OPT_LEVEL_NONE) {
  for (const auto &path : paths)
    if (auto error{compiler.add(path.string(), addedNames)})
      return error->message;
  if (auto error{compiler.compile(optLevel)}) return error->message;
  if (auto error{compiler.jitCompile()}) return error->message;
  return {};
}

/// Write `sourceCode` into `tmpDir` as one module, compile it, and return
/// the error message or the empty string on success.
///
/// This stops before `jitCompile()` on purpose: what the file form is for
/// is a diagnostic that needs a real path, and unit-test bodies are
/// emitted, since otherwise nothing inside one would be diagnosed at all.
[[nodiscard]] inline std::string compileSource(const TempDir &tmpDir,
                                               std::string_view sourceCode) {
  const auto path{tmpDir.write("main.smdl", sourceCode)};
  smdl::Compiler compiler{};
  compiler.shouldEmitUnitTests = true;
  if (auto error{compiler.add(path.string())}) return error->message;
  if (auto error{compiler.compile(smdl::OPT_LEVEL_NONE)}) return error->message;
  return {};
}

/// The material named `name`, requiring that there is one.
[[nodiscard]] inline const smdl::JIT::MaterialDef *
requireMaterial(smdl::Compiler &compiler, std::string_view name) {
  const auto *materialDef{compiler.findMaterial(name)};
  REQUIRE_MESSAGE(materialDef != nullptr, "no material named ", name);
  return materialDef;
}

/// A minimal named material definition.
[[nodiscard]] inline std::string minimalMaterial(std::string_view name) {
  auto text{std::string("material ")};
  text += name;
  text += "() = material(\n"
          "  surface: material_surface(\n"
          "    scattering: df::diffuse_reflection_bsdf(tint: 0.5)),\n"
          ");\n";
  return text;
}

//--}

//--{ Emitted code

/// Write `sourceCode` into `tmpDir` as one module and return its
/// unoptimized LLVM-IR.
[[nodiscard]] inline std::string compileToIR(const TempDir &tmpDir,
                                             std::string_view sourceCode) {
  const auto path{tmpDir.write("main.smdl", sourceCode)};
  smdl::Compiler compiler{};
  REQUIRE_OK(compiler.add(path.string()));
  REQUIRE_OK(compiler.compile(smdl::OPT_LEVEL_NONE));
  auto ir{std::string()};
  REQUIRE_OK(compiler.dump(smdl::DUMP_FORMAT_IR, ir));
  return ir;
}

/// The parameter list of the LLVM definition of `name`, i.e. the text
/// between the parentheses of its `define` line.
[[nodiscard]] inline std::string llvmParamsOf(const std::string &ir,
                                              std::string_view name) {
  const auto marker{std::string("@") + std::string(name) + "("};
  auto i{ir.find("define")};
  while (i != std::string::npos) {
    const auto lineEnd{ir.find('\n', i)};
    const auto line{ir.substr(i, lineEnd - i)};
    if (const auto j{line.find(marker)}; j != std::string::npos) {
      const auto open{j + marker.size()};
      const auto close{line.rfind(')')};
      REQUIRE(close != std::string::npos);
      REQUIRE(close >= open);
      return line.substr(open, close - open);
    }
    i = ir.find("define", i + 1);
  }
  FAIL("no LLVM definition of " << name << " in:\n" << ir);
  return {};
}

/// Count the image symbol declarations in an LLVM-IR dump. A declaration
/// starts a line; every other mention of the symbol is a use.
[[nodiscard]] inline size_t countImageSymbols(std::string_view ir) {
  auto count{size_t(0)};
  for (size_t pos{}; (pos = ir.find("\n@smdl.image.", pos)) != ir.npos; pos++)
    count++;
  return count;
}

//--}

//--{ Evaluating

/// The wavelength grid and the bump allocator that a `smdl::State` points
/// into, which must outlive every state made from it and everything
/// evaluated through one.
class StateStorage final {
public:
  explicit StateStorage(const smdl::Compiler &compiler)
      : mWavelengths(compiler.wavelengthBaseMax) {
    const auto numBands{mWavelengths.size()};
    for (size_t i = 0; i < numBands; i++) {
      // The midpoint at one band, rather than a division by zero.
      const float fac{numBands > 1 ? float(i) / float(numBands - 1) : 0.5f};
      mWavelengths[i] = (1 - fac) * WAVELENGTH_MIN + fac * WAVELENGTH_MAX;
    }
  }

  StateStorage(const StateStorage &) = delete;

  StateStorage &operator=(const StateStorage &) = delete;

  /// A state over this storage: the wavelength grid and the allocator,
  /// and nothing geometric. Call `finalize()` on it where the entry
  /// point being tested reads the space conventions.
  [[nodiscard]] smdl::State makeState() noexcept {
    auto state{smdl::State()};
    state.allocator = &mAllocator;
    state.wavelengthMin = WAVELENGTH_MIN;
    state.wavelengthMax = WAVELENGTH_MAX;
    state.wavelengthBase = mWavelengths.data();
    return state;
  }

  /// The wavelengths the state points at, in nanometers.
  [[nodiscard]] const std::vector<float> &wavelengths() const noexcept {
    return mWavelengths;
  }

  static constexpr float WAVELENGTH_MIN{380.0f};
  static constexpr float WAVELENGTH_MAX{720.0f};

private:
  smdl::BumpPtrAllocator mAllocator{};
  std::vector<float> mWavelengths{};
};

//--}

//--{ Diagnostics

/// Every logged message mentioning `needle`, for as long as this lives.
///
/// The sink goes on the process-wide logger and the destructor takes it
/// off again, so a throwing `REQUIRE` cannot leave it behind. The logger
/// has no targeted removal, so only one of these may be alive at a time.
/// Set `shouldCollectDebug` to see debug messages, since the logger drops
/// a message below its minimum level before any sink is offered it.
class CollectedLog final {
public:
  explicit CollectedLog(std::string needle, bool shouldCollectDebug = false)
      : mSink(smdl::Logger::get().addSink<Sink>(std::move(needle))) {
    if (shouldCollectDebug)
      smdl::Logger::get().setMinLevel(smdl::LOG_LEVEL_DEBUG);
  }

  CollectedLog(const CollectedLog &) = delete;

  CollectedLog &operator=(const CollectedLog &) = delete;

  ~CollectedLog() {
    smdl::Logger::get().reset();
    smdl::Logger::get().setMinLevel(smdl::LOG_LEVEL_INFO);
  }

  /// The messages collected so far, in the order they were logged.
  [[nodiscard]] const std::vector<std::string> &messages() const noexcept {
    return mSink.messages;
  }

  /// How many of them were logged as warnings.
  [[nodiscard]] int warningCount() const noexcept { return mSink.warningCount; }

  /// How many of them mention `needle`, for telling apart the messages a
  /// broader needle collected together.
  [[nodiscard]] int count(std::string_view needle) const {
    auto n{0};
    for (const auto &message : mSink.messages)
      if (message.find(needle) != std::string::npos) n++;
    return n;
  }

private:
  struct Sink final : smdl::LogSink {
    explicit Sink(std::string needle) : needle(std::move(needle)) {}

    void logMessage(smdl::LogLevel level, std::string_view message) override {
      if (message.find(needle) == std::string_view::npos) return;
      messages.emplace_back(message);
      if (level == smdl::LOG_LEVEL_WARN) warningCount++;
    }

    std::string needle{};
    std::vector<std::string> messages{};
    int warningCount{};
  };

  Sink &mSink;
};

//--}

//--{ Archives

/// Write a minimal ZIP with stored (uncompressed) entries, which is
/// enough for the miniz-based archive reader to load as an `.mdr`.
inline void
writeZip(const std::filesystem::path &path,
         const std::vector<std::pair<std::string, std::string>> &entries) {
  // CRC-32 (polynomial 0xEDB88320) as the ZIP format requires.
  const auto crc32{[](std::string_view data) {
    uint32_t crc{0xFFFFFFFFu};
    for (auto ch : data) {
      crc ^= uint8_t(ch);
      for (int i = 0; i < 8; i++)
        crc = (crc >> 1) ^ (0xEDB88320u & (0u - (crc & 1u)));
    }
    return ~crc;
  }};
  auto out{std::string()};
  const auto putU16{[&](uint32_t value) {
    out += char(value & 0xFF);
    out += char((value >> 8) & 0xFF);
  }};
  const auto putU32{[&](uint32_t value) {
    putU16(value & 0xFFFF);
    putU16(value >> 16);
  }};
  auto offsets{std::vector<uint32_t>()};
  for (const auto &[name, data] : entries) {
    offsets.push_back(uint32_t(out.size()));
    putU32(0x04034B50u); // Local file header
    putU16(20), putU16(0), putU16(0), putU16(0), putU16(0);
    putU32(crc32(data));
    putU32(uint32_t(data.size())), putU32(uint32_t(data.size()));
    putU16(uint32_t(name.size())), putU16(0);
    out += name, out += data;
  }
  const auto centralOffset{uint32_t(out.size())};
  for (size_t i = 0; i < entries.size(); i++) {
    const auto &[name, data]{entries[i]};
    putU32(0x02014B50u); // Central directory header
    putU16(20), putU16(20), putU16(0), putU16(0), putU16(0), putU16(0);
    putU32(crc32(data));
    putU32(uint32_t(data.size())), putU32(uint32_t(data.size()));
    putU16(uint32_t(name.size())), putU16(0), putU16(0), putU16(0), putU16(0);
    putU32(0);
    putU32(offsets[i]);
    out += name;
  }
  const auto centralSize{uint32_t(out.size()) - centralOffset};
  putU32(0x06054B50u); // End of central directory
  putU16(0), putU16(0);
  putU16(uint32_t(entries.size())), putU16(uint32_t(entries.size()));
  putU32(centralSize), putU32(centralOffset);
  putU16(0);
  std::filesystem::create_directories(path.parent_path());
  std::ofstream(path, std::ios::binary) << out;
}

//--}
