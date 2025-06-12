#pragma once

#include <unordered_map>

#include "smdl/common.h"

namespace smdl {

class SMDL_EXPORT MeasuredBSDF final {
public:
  enum Kind { KIND_BRDF, KIND_BTDF };

  enum Type { TYPE_FLOAT, TYPE_FLOAT3 };

  [[nodiscard]] static constexpr size_t size_of(Type type) noexcept {
    switch (type) {
    case TYPE_FLOAT:
      static_assert(sizeof(float) == 4);
      return sizeof(float);
    case TYPE_FLOAT3:
      static_assert(sizeof(float3) == 16);
      return sizeof(float3);
    default:
      return 0;
    }
  }

  MeasuredBSDF() = default;

  MeasuredBSDF(const MeasuredBSDF &) = delete;

  ~MeasuredBSDF() { clear(); }

  [[nodiscard]] static std::unique_ptr<MeasuredBSDF>
  load_from_memory(const std::string &file);

  [[nodiscard]] static std::unique_ptr<MeasuredBSDF>
  load_from_file(const std::string &fileName);

  void clear() noexcept;

  [[nodiscard]] float3 fetch(size_t indexThetaO, size_t indexThetaI,
                             size_t indexPhi) const noexcept;

  [[nodiscard]] float3 interpolate(const float3 &wo,
                                   const float3 &wi) const noexcept;

public:
  Kind kind{KIND_BRDF};

  Type type{TYPE_FLOAT};

  size_t numTheta{};

  size_t numPhi{};

  void *buffer{};

  std::unordered_map<std::string, std::string> metaData{};
};

} // namespace smdl
