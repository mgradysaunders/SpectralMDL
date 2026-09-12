/// \file
/// The vocabulary the two programs' command lines share: the `llvm::cl`
/// glue that teaches it to carry vector-valued options, the `Flag` that
/// remembers whether an option was given at all, and the hand parsers
/// for the settings both programs spell the same way.
///
/// The `cl::opt` objects themselves never leave either program's
/// `Options.cc`. What crosses this header is the machinery to declare
/// them and the plain values they lower into.
#pragma once

#include <cassert>
#include <cctype>
#include <cmath>
#include <cstdlib>
#include <fstream>
#include <iterator>
#include <optional>
#include <string>
#include <type_traits>
#include <vector>

#include "smdl/Common.h"
#include "smdl/Support/Error.h"
#include "smdl/Support/Logger.h"
#include "smdl/Support/Strings.h"

#include "llvm/Support/CommandLine.h"
#include "llvm/Support/raw_ostream.h"

namespace cl = llvm::cl;

/// A command line value, and whether the command line actually gave it.
///
/// `given` is the whole point: the value alone cannot distinguish "the
/// user asked for 50" from "50 is what it defaults to". That matters
/// wherever a flag left at its default must not override a scene file
/// that spoke, and wherever a flag's own default is not a value at all.
template <typename T> struct Flag final {
  /// The value, which is the flag's own default when `given` is false.
  T value{};

  /// Did the command line actually give it?
  bool wasGiven{};
};

/// Resolve one of the merged settings: the command line if it spoke,
/// else the scene file if it did, else the flag's own default.
template <typename T, typename U>
[[nodiscard]] T pick(const Flag<T> &cli, const std::optional<U> &file) {
  return !cli.wasGiven && file ? T(*file) : cli.value;
}

/// A `cl::opt` lowered: the value, and whether the command line gave it.
template <typename T> [[nodiscard]] Flag<T> flag(const cl::opt<T> &option) {
  return Flag<T>{T(option), option.getNumOccurrences() > 0};
}

/// Store the default value of a vector option.
///
/// `cl::OptionValue` only keeps a default for non-class types; for a
/// class it falls back to a stub whose `hasValue()` is always false and
/// whose `getValue()` is `llvm_unreachable`. Without this specialization
/// `-print-options` reports every vector option as `*no default*` and,
/// because the stub `compare()` also answers false, prints them all even
/// when they sit at their defaults.
///
/// This mirrors `cl::OptionValueCopy`, which cannot simply be inherited:
/// its `compare()` is `Value == V`, and `smdl::Vector`'s `operator==` is
/// component-wise and yields a `Vector<bool, N>` rather than a `bool`.
template <typename T, size_t N>
struct cl::OptionValue<smdl::Vector<T, N>> final : cl::GenericOptionValue {
  // Mirrors the non-class `OptionValueBase`. Nothing in LLVM reads it,
  // but the other specializations all declare it.
  using WrapperType = smdl::Vector<T, N>;

  OptionValue() = default;

  OptionValue(const smdl::Vector<T, N> &value) { setValue(value); }

  // `opt_storage` records the default by assigning the bare vector.
  template <typename DT> OptionValue &operator=(const DT &value) {
    setValue(value);
    return *this;
  }

  [[nodiscard]] bool hasValue() const { return mIsValid; }

  [[nodiscard]] const smdl::Vector<T, N> &getValue() const {
    assert(mIsValid && "invalid option value");
    return mValue;
  }

  void setValue(const smdl::Vector<T, N> &value) {
    mValue = value;
    mIsValid = true;
  }

  /// Does this hold `value`? Compared component by component, since the
  /// vector `operator==` returns a vector of results.
  [[nodiscard]] bool compare(const smdl::Vector<T, N> &value) const {
    if (!mIsValid) return false;
    for (size_t i{}; i < N; i++)
      if (!(mValue[i] == value[i])) return false;
    return true;
  }

  [[nodiscard]] bool compare(const GenericOptionValue &value) const override {
    const OptionValue &other{static_cast<const OptionValue &>(value)};
    return other.hasValue() && compare(other.getValue());
  }

private:
  smdl::Vector<T, N> mValue{};
  bool mIsValid{};
};

/// A vector in the comma-separated syntax an option is typed in, so that
/// what `-print-options` shows, and what a diagnostic quotes back, can be
/// pasted onto a command line. `smdl::Brief` rather than `concat`'s bare
/// arithmetic path, which is six decimal places and would print a frame
/// width as `1280.000000`.
template <typename T, size_t N>
[[nodiscard]] inline std::string spellVector(const smdl::Vector<T, N> &value) {
  std::string result{};
  for (size_t i{}; i < N; i++) {
    if (i != 0) result += ',';
    if constexpr (std::is_floating_point_v<T>) {
      smdl::Brief(value[i]).appendTo(result);
    } else {
      result += std::to_string(value[i]);
    }
  }
  return result;
}

template <typename T, size_t N>
class cl::parser<smdl::Vector<T, N>>
    : public cl::basic_parser<smdl::Vector<T, N>> {
public:
  using base = basic_parser<smdl::Vector<T, N>>;

  parser(Option &O) : base(O) {}

  // NOLINTNEXTLINE
  bool parse(Option &O, StringRef ArgName, StringRef Arg,
             smdl::Vector<T, N> &Val) {
    SmallVector<StringRef> tokens{};
    Arg.split(tokens, ",");
    if (tokens.size() != N) {
      O.error("'" + Arg + "' value invalid for " + getValueName());
      return true;
    }
    for (size_t i{}; i < N; i++) {
      if constexpr (std::is_floating_point_v<T>) {
        double result{};
        if (tokens[i].getAsDouble(result)) {
          O.error("'" + Arg + "' value invalid for " + getValueName());
          return true;
        }
        Val[i] = result;
      } else {
        unsigned result{};
        if (tokens[i].getAsInteger(10, result)) {
          O.error("'" + Arg + "' value invalid for " + getValueName());
          return true;
        }
        Val[i] = result;
      }
    }
    return false;
  }

  [[nodiscard]] StringRef getValueName() const override {
    if constexpr (std::is_same_v<T, float>) {
      if constexpr (N == 2) return "float2";
      if constexpr (N == 3) return "float3";
      if constexpr (N == 4) return "float4";
    }
    if constexpr (std::is_same_v<T, int>) {
      if constexpr (N == 2) return "int2";
      if constexpr (N == 3) return "int3";
      if constexpr (N == 4) return "int4";
    }
    return "...";
  }

  void printOptionDiff(const Option &O, smdl::Vector<T, N> V,
                       typename base::OptVal Default,
                       size_t GlobalWidth) const {
    this->printOptionName(O, GlobalWidth);
    const std::string value{spellVector(V)};
    outs() << "= " << value;
    // The value column is padded to 8 before the default, matching the
    // scalar parsers in LLVM's `CommandLine.cpp`.
    outs().indent(value.size() < 8 ? 8 - value.size() : 0) << " (default: ";
    // `OptionValue` only tracks a default for non-class types, so a
    // vector option never has one to report even when `cl::init` gave it
    // a value. Asking anyway is not allowed: `getValue()` on the class
    // specialization is `llvm_unreachable`.
    if (Default.hasValue())
      outs() << spellVector(Default.getValue());
    else
      outs() << "*no default*";
    outs() << ")\n";
  }
};

/// The default wavelength range in nanometers, spanning the visible.
constexpr float WAVELENGTH_MIN = 380.0f;
constexpr float WAVELENGTH_MAX = 720.0f;

/// A uniform wavelength grid, as `-wavelength-range` spells one.
struct WavelengthRange final {
  /// The endpoints in nanometers, inclusive.
  smdl::float2 range{};

  /// The number of bands spanning them.
  unsigned bandCount{};
};

/// The '-log-level' name as `smdl::Logger` spells it.
///
/// \throws smdl::Error  If the name is not recognized.
///
[[nodiscard]] inline smdl::LogLevel parseLogLevel(const std::string &flagStr) {
  if (flagStr == "debug") return smdl::LOG_LEVEL_DEBUG;
  if (flagStr == "info") return smdl::LOG_LEVEL_INFO;
  if (flagStr == "warn") return smdl::LOG_LEVEL_WARN;
  if (flagStr == "error") return smdl::LOG_LEVEL_ERROR;
  throw smdl::Error(smdl::concat("Expected -log-level to be 'debug', 'info', "
                                 "'warn', or 'error', got ",
                                 smdl::Quoted(flagStr)));
}

/// The '-unicode' flag as `smdl::UnicodeMode` spells it: unset leaves the
/// choice to autodetection, and either value overrides it.
[[nodiscard]] inline smdl::UnicodeMode
lowerUnicodeMode(cl::boolOrDefault value) {
  return value == cl::boolOrDefault::BOU_TRUE    ? smdl::UNICODE_MODE_ALWAYS
         : value == cl::boolOrDefault::BOU_FALSE ? smdl::UNICODE_MODE_NEVER
                                                 : smdl::UNICODE_MODE_AUTO;
}

/// Parse the '-wavelengths' flag: wavelengths in nanometers separated by
/// commas or whitespace, or the name of a text file of the same, which
/// wins whenever the value opens as a file. NOT '@file': LLVM's command
/// line expands '@'-prefixed argv tokens as response files before any
/// option sees them. Returns empty when the flag was not given; anything
/// else must be a finite, positive, strictly increasing list.
///
/// \throws smdl::Error  If the list is malformed or out of order.
///
[[nodiscard]] inline std::vector<float>
parseWavelengths(const std::string &flagStr) {
  std::vector<float> values{};
  if (flagStr.empty()) return values;
  std::string text{flagStr};
  if (std::ifstream file{flagStr}; file) {
    text.assign(std::istreambuf_iterator<char>(file), {});
    if (text.empty())
      throw smdl::Error(smdl::concat("-wavelengths file ",
                                     smdl::Quoted(flagStr), " is empty"));
  }
  const char *ptr{text.c_str()};
  while (*ptr) {
    if (*ptr == ',' || std::isspace(static_cast<unsigned char>(*ptr))) {
      ptr++;
      continue;
    }
    char *numEnd{};
    const float value{std::strtof(ptr, &numEnd)};
    if (numEnd == ptr)
      throw smdl::Error(smdl::concat("Cannot parse -wavelengths near ",
                                     smdl::Quoted(std::string(ptr, 0, 12))));
    ptr = numEnd;
    values.push_back(value);
  }
  if (values.empty())
    throw smdl::Error("Expected -wavelengths to name at least 1 wavelength");
  for (size_t i = 0; i < values.size(); i++) {
    if (!(std::isfinite(values[i]) && values[i] > 0))
      throw smdl::Error(
          "Expected every -wavelengths value to be positive and finite");
    if (i > 0 && !(values[i] > values[i - 1]))
      throw smdl::Error("Expected -wavelengths to be strictly increasing");
  }
  return values;
}

/// Parse the '-wavelength-range' flag: 'A,B:N' for N uniform bands
/// spanning A to B nm, with ':N' optional. Returns the default grid when
/// the flag was not given.
///
/// A band count of 1 is allowed here, which is a grid of one wavelength
/// at the midpoint; a caller that needs a band width to speak of rejects
/// it itself.
///
/// \throws smdl::Error  If the value is malformed or out of range.
///
[[nodiscard]] inline WavelengthRange
parseWavelengthRange(const std::string &flagStr) {
  WavelengthRange result{smdl::float2{WAVELENGTH_MIN, WAVELENGTH_MAX}, 16U};
  if (flagStr.empty()) return result;
  const char *ptr{flagStr.c_str()};
  char *numEnd{};
  result.range.x = std::strtof(ptr, &numEnd);
  if (numEnd == ptr || *numEnd != ',')
    throw smdl::Error(smdl::concat("Cannot parse -wavelength-range near ",
                                   smdl::Quoted(std::string(ptr, 0, 12))));
  ptr = numEnd + 1;
  result.range.y = std::strtof(ptr, &numEnd);
  if (numEnd == ptr)
    throw smdl::Error(smdl::concat("Cannot parse -wavelength-range near ",
                                   smdl::Quoted(std::string(ptr, 0, 12))));
  ptr = numEnd;
  if (*ptr == ':') {
    ptr++;
    if (!std::isdigit(static_cast<unsigned char>(*ptr)))
      throw smdl::Error(smdl::concat("Cannot parse -wavelength-range near ",
                                     smdl::Quoted(std::string(ptr, 0, 12))));
    result.bandCount = unsigned(std::strtoul(ptr, &numEnd, 10));
    ptr = numEnd;
  }
  if (*ptr != '\0')
    throw smdl::Error(smdl::concat("Cannot parse -wavelength-range near ",
                                   smdl::Quoted(std::string(ptr, 0, 12))));
  if (!(std::isfinite(result.range.x) && std::isfinite(result.range.y) &&
        result.range.x > 0 && result.range.x < result.range.y))
    throw smdl::Error(
        "Expected -wavelength-range 'A,B' to be positive and increasing");
  if (result.bandCount < 1)
    throw smdl::Error("Expected -wavelength-range ':N' to be at least 1");
  return result;
}
