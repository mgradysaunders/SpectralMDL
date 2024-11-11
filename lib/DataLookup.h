#pragma once

#include "llvm.h"

namespace smdl {

class DataLookup final {
public:
  using color_t = llvm::SmallVector<float_t>;

  using value_t = std::variant<int_t, int2_t, int3_t, int4_t, float_t, float2_t, float3_t, float4_t, color_t>;

  [[nodiscard]] bool isvalid(const string_t &name) const { return values.contains(std::string_view(name)); }

  template <typename T> [[nodiscard]] const T *lookup(const string_t &name) const {
    if (auto itr{values.find(std::string_view(name))}; itr != values.end())
      return std::get_if<T>(&itr->second);
    return nullptr;
  }

  llvm::StringMap<value_t> values{};
};

} // namespace smdl
