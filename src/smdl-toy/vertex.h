#pragma once

#include "raytracing.h"

class Camera;

class Light;

class Vertex final {
public:
  float3 point{};

  Color beta{};

  const Camera *camera{};

  const Light *light{};

  smdl::JIT::MaterialInstance materialInstance{};

  float3 wNext{};

  float pdfFwd{QUIET_NAN};

  float pdfRev{QUIET_NAN};

  bool isDeltaBounce{};

  bool isInfiniteLight{};
};

[[nodiscard]] uint64_t random_walk(const Scene &scene, const RandomFP &random,
                                   const Color &wavelengths,
                                   smdl::BumpPtrAllocator &allocator,
                                   smdl::Transport transport, Vertex path0,
                                   float wpdfFwd, uint64_t maxDepth,
                                   Vertex *path);
