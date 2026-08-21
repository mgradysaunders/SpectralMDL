#pragma once

#include "smdl/Common.h"

#include "llvm/Support/CommandLine.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/raw_ostream.h"

#include <cassert>
#include <cstdio>
#include <string>
#include <type_traits>

namespace cl = llvm::cl;

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

  [[nodiscard]] bool hasValue() const { return mValid; }

  [[nodiscard]] const smdl::Vector<T, N> &getValue() const {
    assert(mValid && "invalid option value");
    return mValue;
  }

  void setValue(const smdl::Vector<T, N> &value) {
    mValue = value;
    mValid = true;
  }

  /// Does this hold `value`? Compared component by component, since the
  /// vector `operator==` returns a vector of results.
  [[nodiscard]] bool compare(const smdl::Vector<T, N> &value) const {
    if (!mValid) return false;
    for (size_t i{}; i < N; i++)
      if (!(mValue[i] == value[i])) return false;
    return true;
  }

  bool compare(const GenericOptionValue &value) const override {
    const auto &other{static_cast<const OptionValue &>(value)};
    return other.hasValue() && compare(other.getValue());
  }

private:
  smdl::Vector<T, N> mValue{};
  bool mValid{};
};

template <typename T, size_t N>
class cl::parser<smdl::Vector<T, N>>
    : public cl::basic_parser<smdl::Vector<T, N>> {
public:
  using base = basic_parser<smdl::Vector<T, N>>;

  parser(Option &O) : base(O) {}

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

  StringRef getValueName() const override {
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
    const auto value{spell(V)};
    outs() << "= " << value;
    // The value column is padded to 8 before the default, matching the
    // scalar parsers in LLVM's `CommandLine.cpp`.
    outs().indent(value.size() < 8 ? 8 - value.size() : 0) << " (default: ";
    // `OptionValue` only tracks a default for non-class types, so a
    // vector option never has one to report even when `cl::init` gave it
    // a value. Asking anyway is not allowed: `getValue()` on the class
    // specialization is `llvm_unreachable`.
    if (Default.hasValue())
      outs() << spell(Default.getValue());
    else
      outs() << "*no default*";
    outs() << ")\n";
  }

private:
  /// The value in the comma-separated syntax the option is typed in, so
  /// that what `-print-options` shows can be pasted back onto a command
  /// line. `%g` rather than `raw_ostream`'s own float formatting, which
  /// is fixed-exponent and would print a frame width as `1.280000e+03`.
  [[nodiscard]] static std::string spell(const smdl::Vector<T, N> &value) {
    std::string result{};
    for (size_t i{}; i < N; i++) {
      if (i != 0) result += ',';
      if constexpr (std::is_floating_point_v<T>) {
        char buffer[32]{};
        std::snprintf(buffer, sizeof(buffer), "%g", double(value[i]));
        result += buffer;
      } else {
        result += std::to_string(value[i]);
      }
    }
    return result;
  }
};
