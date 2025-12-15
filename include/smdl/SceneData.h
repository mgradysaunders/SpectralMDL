/// \file
#pragma once

#include "smdl/Common.h"

namespace smdl {

/// \addtogroup Main
/// \{

/// The scene data.
///
/// This is the arbitrary data held by the compiler that is
/// made available to MDL code at runtime through the `scene::`
/// module.
///
class SMDL_EXPORT SceneData final {
public:
  enum class Kind : int {
    Int = 0,
    Float = 1,
    Color = 2,
  };

  // TODO Doc
  using Getter =
      std::function<void(State *state, Kind kind, int size, void *out)>;

  SceneData();

  /// Non-copyable and non-movable!
  SceneData(const SceneData &) = delete;

  ~SceneData();

public:
  void clear();

  void set(std::string_view name, Getter getter);

  /// Associate the given name with a constant `int`.
  void set_int(std::string_view name, int var);

  /// Associate the given name with a constant `int2`.
  void set_int2(std::string_view name, int2 var);

  /// Associate the given name with a constant `int3`.
  void set_int3(std::string_view name, int3 var);

  /// Associate the given name with a constant `int4`.
  void set_int4(std::string_view name, int4 var);

  /// Associate the given name with a constant `float`.
  void set_float(std::string_view name, float var);

  /// Associate the given name with a constant `float2`.
  void set_float2(std::string_view name, float2 var);

  /// Associate the given name with a constant `float3`.
  void set_float3(std::string_view name, float3 var);

  /// Associate the given name with a constant `float4`.
  void set_float4(std::string_view name, float4 var);

  /// Associate the given name with a `color`.
  ///
  /// \param[in] getter
  /// The getter to calculate the color which must generally depend on
  /// the wavelengths in the `State`.
  ///
  void set_color(std::string_view name,
                 std::function<void(State &, float *)> getter);

  [[nodiscard]] const Getter *get(std::string_view name) const;

private:
  void *ptr{};
};

/// \}

} // namespace smdl
