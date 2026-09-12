/// \file
/// The contexts a renderer test walks in: a `RenderContext` over a
/// committed scene, with no lights and the default options, and a
/// `PathContext` with its own allocator, sampler, medium view, states
/// and scratch, so a test can run the visibility walks and the chain
/// discoveries the way the path walk runs them. Everything is owned
/// here for the duration of the test.
#pragma once

#include "Fixtures.h"

#include "Color.h"
#include "Render/Context.h"
#include "Render/Light.h"
#include "smdl/RenderUtil/OpticalGlass.h"

class PathHarness final {
public:
  PathHarness(smdl::Compiler &compiler, const Scene &scene,
              const Color &wavelengths)
      : mLights(compiler, scene, nullptr, {}, wavelengths),
        mGatherState(makeRenderState(wavelengths, &allocator)),
        mWalkState(makeRenderState(wavelengths, &allocator)),
        mShadeState(makeRenderState(wavelengths, &allocator)),
        mLightState(makeRenderState(wavelengths, &allocator)),
        render{compiler, scene, mLights, mneeOptions, pathOptions},
        path{allocator,
             sampler,
             mMedium,
             mSkyBasis,
             mGatherState,
             mWalkState,
             mShadeState,
             mLightState,
             mGatherSample,
             mGatherBlocker,
             wavelengths,
             PathTime{0.0f},
             smdl::FRAUNHOFER_D_LINE} {}

  PathHarness(const PathHarness &) = delete;

  PathHarness &operator=(const PathHarness &) = delete;

  /// Begin one path on sample `sampleIndex` of pixel 0: the allocator
  /// emptied, the medium view's key dropped, and the sampler positioned.
  void beginPath(uint32_t sampleIndex) {
    allocator.reset();
    mMedium.beginPath();
    sampler.startPixelSample(0, sampleIndex);
  }

  smdl::BumpPtrAllocator allocator{};
  Sampler sampler{};
  MNEEOptions mneeOptions{};
  PathOptions pathOptions{};

private:
  LightSampler mLights;
  Medium mMedium{};
  smdl::SkyBasis mSkyBasis{};
  smdl::State mGatherState;
  smdl::State mWalkState;
  smdl::State mShadeState;
  smdl::State mLightState;
  LightSample mGatherSample{};
  Hit mGatherBlocker{};

public:
  RenderContext render;
  PathContext path;
};
