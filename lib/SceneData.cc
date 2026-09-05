#include "smdl/SceneData.h"

#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringRef.h"

namespace smdl {

namespace {

struct Entry final {
  SceneData::Getter getter{};
  SceneData::Exists exists{};
};

using Lookup = llvm::StringMap<Entry>;

} // namespace

SceneData::SceneData() : mPtr(new Lookup()) {}

SceneData::~SceneData() {
  delete static_cast<Lookup *>(mPtr);
  mPtr = nullptr;
}

void SceneData::clear() { static_cast<Lookup *>(mPtr)->clear(); }

void SceneData::set(std::string_view name, Getter getter, Exists exists) {
  auto &lookup{*static_cast<Lookup *>(mPtr)};
  lookup[llvm::StringRef(name)] = Entry{std::move(getter), std::move(exists)};
}

void SceneData::setInt(std::string_view name, int var) {
  set(name, [var](State *, Kind kind, int size, void *out) {
    if (kind == Kind::Int)
      for (int i = 0; i < size; i++) static_cast<int *>(out)[i] = var;
  });
}

void SceneData::setInt2(std::string_view name, int2 var) {
  set(name, [var](State *, Kind kind, int size, void *out) {
    if (kind == Kind::Int && size == 2)
      for (int i = 0; i < 2; i++) static_cast<int *>(out)[i] = var[i];
  });
}

void SceneData::setInt3(std::string_view name, int3 var) {
  set(name, [var](State *, Kind kind, int size, void *out) {
    if (kind == Kind::Int && size == 3)
      for (int i = 0; i < 3; i++) static_cast<int *>(out)[i] = var[i];
  });
}

void SceneData::setInt4(std::string_view name, int4 var) {
  set(name, [var](State *, Kind kind, int size, void *out) {
    if (kind == Kind::Int && size <= 4)
      for (int i = 0; i < size; i++) static_cast<int *>(out)[i] = var[i];
  });
}

void SceneData::setFloat(std::string_view name, float var) {
  set(name, [var](State *, Kind kind, int size, void *out) {
    if (kind == Kind::Float || kind == Kind::Color)
      for (int i = 0; i < size; i++) static_cast<float *>(out)[i] = var;
  });
}

void SceneData::setFloat2(std::string_view name, float2 var) {
  set(name, [var](State *, Kind kind, int size, void *out) {
    if (kind == Kind::Float && size == 2)
      for (int i = 0; i < 2; i++) static_cast<float *>(out)[i] = var[i];
  });
}

void SceneData::setFloat3(std::string_view name, float3 var) {
  set(name, [var](State *, Kind kind, int size, void *out) {
    if (kind == Kind::Float && size == 3)
      for (int i = 0; i < 3; i++) static_cast<float *>(out)[i] = var[i];
  });
}

void SceneData::setFloat4(std::string_view name, float4 var) {
  set(name, [var](State *, Kind kind, int size, void *out) {
    if (kind == Kind::Float && size <= 4)
      for (int i = 0; i < size; i++) static_cast<float *>(out)[i] = var[i];
  });
}

void SceneData::setFloat4x4(std::string_view name, const float4x4 &var) {
  set(name, [var](State *, Kind kind, int size, void *out) {
    if (kind == Kind::Float && size == 16)
      for (int j = 0; j < 4; j++)
        for (int i = 0; i < 4; i++)
          static_cast<float *>(out)[4 * j + i] = var[j][i];
  });
}

void SceneData::setColor(std::string_view name,
                         std::function<void(State &, float *)> getter) {
  SMDL_SANITY_CHECK(getter != nullptr);
  set(name,
      [getter = std::move(getter)](State *state, Kind kind, int, void *out) {
        if (kind == Kind::Color) getter(*state, static_cast<float *>(out));
      });
}

const SceneData::Getter *SceneData::get(std::string_view name) const {
  auto &lookup{*static_cast<const Lookup *>(mPtr)};
  if (auto itr{lookup.find(llvm::StringRef(name))}; itr != lookup.end())
    return &itr->getValue().getter;
  return nullptr;
}

bool SceneData::exists(std::string_view name, const State *state) const {
  auto &lookup{*static_cast<const Lookup *>(mPtr)};
  auto itr{lookup.find(llvm::StringRef(name))};
  if (itr == lookup.end()) return false;
  const auto &entry{itr->getValue()};
  return entry.exists ? entry.exists(state) : true;
}

} // namespace smdl

extern "C" {

SMDL_EXPORT int smdlDataExists(void *state, void *sceneData, // NOLINT
                               const char *name) {
  return static_cast<smdl::SceneData *>(sceneData)->exists(
      name, static_cast<const smdl::State *>(state));
}

SMDL_EXPORT void smdlDataLookup(void *state, void *sceneData, // NOLINT
                                const char *name, int kind, int size,
                                void *ptr) {
  if (auto getter{static_cast<smdl::SceneData *>(sceneData)->get(name)})
    (*getter)(static_cast<smdl::State *>(state), smdl::SceneData::Kind(kind),
              size, ptr);
}

} // extern "C"
