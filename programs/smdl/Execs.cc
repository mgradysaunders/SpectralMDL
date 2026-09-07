#include <iostream>
#include <vector>

#include "smdl/Support/BumpPtrAllocator.h"

#include "Execs.h"
#include "Options.h"

void runExecs(const Options &opts, smdl::Compiler &compiler) {
  if (auto error{compiler.compile(opts.compile.optLevel)})
    error->printAndExit();
  if (auto error{compiler.jitCompile()}) error->printAndExit();
  if (auto error{compiler.runExecs()}) error->printAndExit();
}

void runUnitTests(const Options &opts, smdl::Compiler &compiler) {
  // The grid is the compiler's own, so a unit test sees exactly the
  // wavelengths the material code was emitted for. It is copied because
  // `State::wavelength_base` is a mutable view into it.
  auto wavelengths{opts.compile.wavelengths};
  auto allocator{smdl::BumpPtrAllocator{}};
  auto state{smdl::State{}};
  state.allocator = &allocator;
  state.texture_coordinate[0][0] = opts.state.texCoord.x;
  state.texture_coordinate[0][1] = opts.state.texCoord.y;
  state.texture_coordinate[0][2] = opts.state.texCoord.z;
  state.animation_time = opts.state.time;
  state.object_id = opts.state.objectID;
  state.ptex_face_id = opts.state.ptexFaceID;
  state.ptex_face_uv[0] = opts.state.ptexFaceUV.x;
  state.ptex_face_uv[1] = opts.state.ptexFaceUV.y;
  state.wavelength_min = opts.compile.wavelengthRange.x;
  state.wavelength_max = opts.compile.wavelengthRange.y;
  state.wavelength_base = wavelengths.data();
  if (auto error{compiler.runUnitTests(state)}) {
    std::cerr << '\n';
    error->printAndExit();
  }
}
