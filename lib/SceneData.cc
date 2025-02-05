#include "smdl/SceneData.h"

#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringRef.h"

namespace smdl {

SceneData::SceneData() : ptr(new llvm::StringMap<Getter>()) {}

SceneData::~SceneData() {
  delete static_cast<llvm::StringMap<Getter> *>(ptr);
  ptr = nullptr;
}

void SceneData::clear() {
  auto &lookup{*static_cast<llvm::StringMap<Getter> *>(ptr)};
  lookup.clear();
}

void SceneData::set(std::string_view name, Getter getter) {
  auto &lookup{*static_cast<llvm::StringMap<Getter> *>(ptr)};
  lookup[llvm::StringRef(name)] = std::move(getter);
}

void SceneData::set_int(std::string_view name, int var) {
  set(name, [var](State *, Kind kind, int size, void *out) {
    if (kind == Kind::Int)
      for (int i = 0; i < size; i++)
        static_cast<int *>(out)[i] = var;
  });
}

void SceneData::set_int2(std::string_view name, int2 var) {
  set(name, [var](State *, Kind kind, int size, void *out) {
    if (kind == Kind::Int && size == 2)
      for (int i = 0; i < 2; i++)
        static_cast<int *>(out)[i] = var[i];
  });
}

void SceneData::set_int3(std::string_view name, int3 var) {
  set(name, [var](State *, Kind kind, int size, void *out) {
    if (kind == Kind::Int && size == 3)
      for (int i = 0; i < 3; i++)
        static_cast<int *>(out)[i] = var[i];
  });
}

void SceneData::set_int4(std::string_view name, int4 var) {
  set(name, [var](State *, Kind kind, int size, void *out) {
    if (kind == Kind::Int && size <= 4)
      for (int i = 0; i < size; i++)
        static_cast<int *>(out)[i] = var[i];
  });
}

void SceneData::set_float(std::string_view name, float var) {
  set(name, [var](State *, Kind kind, int size, void *out) {
    if (kind == Kind::Float || kind == Kind::Color)
      for (int i = 0; i < size; i++)
        static_cast<float *>(out)[i] = var;
  });
}

void SceneData::set_float2(std::string_view name, float2 var) {
  set(name, [var](State *, Kind kind, int size, void *out) {
    if (kind == Kind::Float && size == 2)
      for (int i = 0; i < 2; i++)
        static_cast<float *>(out)[i] = var[i];
  });
}

void SceneData::set_float3(std::string_view name, float3 var) {
  set(name, [var](State *, Kind kind, int size, void *out) {
    if (kind == Kind::Float && size == 3)
      for (int i = 0; i < 3; i++)
        static_cast<float *>(out)[i] = var[i];
  });
}

void SceneData::set_float4(std::string_view name, float4 var) {
  set(name, [var](State *, Kind kind, int size, void *out) {
    if (kind == Kind::Float && size <= 4)
      for (int i = 0; i < size; i++)
        static_cast<float *>(out)[i] = var[i];
  });
}

const SceneData::Getter *SceneData::get(std::string_view name) const {
  auto &lookup{*static_cast<const llvm::StringMap<Getter> *>(ptr)};
  if (auto itr{lookup.find(llvm::StringRef(name))}; itr != lookup.end())
    return &itr->getValue();
  return nullptr;
}

} // namespace smdl
