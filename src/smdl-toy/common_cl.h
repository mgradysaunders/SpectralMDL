#pragma once

#include "smdl/common.h"
#include "llvm/Support/CommandLine.h"

namespace cl = llvm::cl;

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
      if constexpr (N == 2)
        return "float2";
      if constexpr (N == 3)
        return "float3";
      if constexpr (N == 4)
        return "float4";
    }
    if constexpr (std::is_same_v<T, int>) {
      if constexpr (N == 2)
        return "int2";
      if constexpr (N == 3)
        return "int3";
      if constexpr (N == 4)
        return "int4";
    }
    return "...";
  }

  void printOptionDiff(const Option &O, smdl::Vector<T, N> V,
                       typename base::OptVal Default,
                       size_t GlobalWidth) const {
    //
  }

  void anchor() override {}
};
