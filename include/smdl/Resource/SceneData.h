/// \file
#pragma once

#include "smdl/Common.h"

namespace smdl {

/// \addtogroup resource
/// \{

/// The scene data.
///
/// This is the arbitrary data held by the compiler that is
/// made available to MDL code at runtime through the `scene::`
/// module: `scene::data_lookup_*` calls the getter registered under a
/// name, and `scene::data_isvalid` asks its presence predicate.
///
class SMDL_EXPORT SceneData final {
public:
  enum class Kind : int {
    Int = 0,
    Float = 1,
    Color = 2,
  };

  /// A getter: write `size` values of `kind` (`int` or `float`; a color is
  /// `wavelengthBaseMax` floats) to `out`, which arrives holding the
  /// lookup's default, or leave it alone to keep that default. The state
  /// is the shading point being asked about, so per-vertex data reads
  /// whatever the renderer put on the state.
  using Getter =
      std::function<void(State *state, Kind kind, int size, void *out)>;

  /// A presence predicate, for `scene::data_isvalid`: is the data present
  /// at this shading point? A name registered without one is present
  /// wherever it is registered.
  using Exists = std::function<bool(const State *state)>;

  SceneData();

  /// Non-copyable and non-movable!
  SceneData(const SceneData &) = delete;

  ~SceneData();

public:
  void clear();

  /// Associate the given name with a getter, and optionally with a
  /// presence predicate.
  void set(std::string_view name, Getter getter, Exists exists = {});

  /// Associate the given name with a constant `int`.
  void setInt(std::string_view name, int var);

  /// Associate the given name with a constant `int2`.
  void setInt2(std::string_view name, int2 var);

  /// Associate the given name with a constant `int3`.
  void setInt3(std::string_view name, int3 var);

  /// Associate the given name with a constant `int4`.
  void setInt4(std::string_view name, int4 var);

  /// Associate the given name with a constant `float`.
  void setFloat(std::string_view name, float var);

  /// Associate the given name with a constant `float2`.
  void setFloat2(std::string_view name, float2 var);

  /// Associate the given name with a constant `float3`.
  void setFloat3(std::string_view name, float3 var);

  /// Associate the given name with a constant `float4`.
  void setFloat4(std::string_view name, float4 var);

  /// Associate the given name with a constant `float4x4`, stored as
  /// 16 floats in column-major order.
  void setFloat4x4(std::string_view name, const float4x4 &var);

  /// Associate the given name with a `color`.
  ///
  /// \param[in] name
  /// The name to associate.
  ///
  /// \param[in] getter
  /// The getter to calculate the color which must generally depend on
  /// the wavelengths in the `State`.
  ///
  void setColor(std::string_view name,
                std::function<void(State &, float *)> getter);

  /// The getter registered under `name`, or null.
  [[nodiscard]] const Getter *get(std::string_view name) const;

  /// Is data registered under `name` present at `state`? False for an
  /// unregistered name; the predicate's answer when one was given; true
  /// otherwise.
  [[nodiscard]] bool exists(std::string_view name, const State *state) const;

private:
  void *mPtr{};
};

/// \}

} // namespace smdl
