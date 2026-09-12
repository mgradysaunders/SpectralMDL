#include <iostream>
#include <vector>

#include "smdl/Support/BumpPtrAllocator.h"

#include "Execs.h"
#include "Options.h"

void runExecs(const Options &opts, smdl::Compiler &compiler) {
  if (std::optional<smdl::Error> error{compiler.compile(opts.compile.optLevel)})
    error->printAndExit();
  if (std::optional<smdl::Error> error{compiler.jitCompile()})
    error->printAndExit();
  if (std::optional<smdl::Error> error{compiler.runExecs()})
    error->printAndExit();
}

void runUnitTests(const Options &opts, smdl::Compiler &compiler) {
  // The grid is the compiler's own, so a unit test sees exactly the
  // wavelengths the material code was emitted for. It is copied because
  // `State::wavelengthBase` is a mutable view into it.
  std::vector<float> wavelengths{opts.compile.wavelengths};
  smdl::BumpPtrAllocator allocator{};
  smdl::State state{};
  state.allocator = &allocator;
  state.textureCoordinate[0][0] = opts.state.texCoord.x;
  state.textureCoordinate[0][1] = opts.state.texCoord.y;
  state.textureCoordinate[0][2] = opts.state.texCoord.z;
  state.animationTime = opts.state.time;
  state.objectId = opts.state.objectID;
  state.ptexFaceId = opts.state.ptexFaceID;
  state.ptexFaceUV[0] = opts.state.ptexFaceUV.x;
  state.ptexFaceUV[1] = opts.state.ptexFaceUV.y;
  state.wavelengthMin = opts.compile.wavelengthRange.x;
  state.wavelengthMax = opts.compile.wavelengthRange.y;
  state.wavelengthBase = wavelengths.data();
  if (std::optional<smdl::Error> error{compiler.runUnitTests(state)}) {
    std::cerr << '\n';
    error->printAndExit();
  }
}
