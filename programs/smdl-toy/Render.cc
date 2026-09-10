#include <algorithm>
#include <chrono>
#include <cmath>
#include <ctime>
#include <filesystem>
#include <memory>
#include <mutex>
#include <optional>
#include <string>
#include <vector>

#include "../CommandLine.h"
#include "llvm/Support/raw_ostream.h"

#include "smdl/Support/Denormals.h"
#include "smdl/Support/Filesystem.h"
#include "smdl/Support/Logger.h"
#include "smdl/Support/Parallel.h"
#include "smdl/Support/Strings.h"

#include "MedianFilter.h"
#include "Options.h"
#include "Progress.h"
#include "Render.h"
#include "Render/Guiding.h"
#include "Render/Manifold.h"
#include "Render/PathStats.h"
#include "Render/PathTracing.h"
#include "Render/Sampler.h"
#include "Response.h"
#include "Resume.h"
#include "Stage.h"
#include "Tonemap.h"

namespace {

// The process CPU time in seconds, summed over every thread, so a render
// on N cores accrues about N seconds per second of wall clock. Zero when
// the platform has no way to ask, which the caller sees as a session that
// took no compute time rather than as an error.
//
// NOTE: 'std::clock()' is only the right answer where nothing better is
// available: it measures process CPU time on POSIX but wall clock since
// process start on MSVC.
[[nodiscard]] double cpuTimeSeconds() {
#if defined(_WIN32)
  FILETIME creationTime{}, exitTime{}, kernelTime{}, userTime{};
  if (!GetProcessTimes(GetCurrentProcess(), &creationTime, &exitTime,
                       &kernelTime, &userTime))
    return 0.0;
  // Both are 100-nanosecond tick counts in a split 64-bit integer.
  const auto toSeconds{[](const FILETIME &fileTime) {
    return 1e-7 * double((uint64_t(fileTime.dwHighDateTime) << 32) |
                         uint64_t(fileTime.dwLowDateTime));
  }};
  return toSeconds(kernelTime) + toSeconds(userTime);
#elif defined(CLOCK_PROCESS_CPUTIME_ID)
  timespec time{};
  if (clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time) != 0) return 0.0;
  return double(time.tv_sec) + 1e-9 * double(time.tv_nsec);
#else
  return double(std::clock()) / double(CLOCKS_PER_SEC);
#endif
}

// How the sample budget is split into passes.
//
// Without guiding there is a single pass of the whole budget. With guiding,
// passes grow geometrically (1, 2, 4, ... spp) and the remainder is dumped
// into the final pass, so it always holds at least half the budget. Solved
// up front rather than as the loop runs so that the progress bar can say
// which pass of how many.
[[nodiscard]]
std::vector<size_t> solveSamplePasses(size_t spp, bool useGuiding,
                                      size_t trainedSpp) {
  // The geometric warmup exists to bound the samples spent while the
  // tree is immature, so a session that resumed a saved tree skips it:
  // the first pass starts at the largest power of two at or below what
  // already trained the tree, and the refine threshold keeps scaling
  // with the pass size.
  size_t firstPass{1};
  while (useGuiding && firstPass * 2 <= trainedSpp) firstPass *= 2;
  auto passes{std::vector<size_t>()};
  for (size_t sppDone{0}; sppDone < spp;) {
    size_t thisPass{
        useGuiding ? std::min(firstPass << passes.size(), spp - sppDone) : spp};
    if (useGuiding && (spp - sppDone) < 2 * thisPass) thisPass = spp - sppDone;
    passes.push_back(thisPass);
    sppDone += thisPass;
  }
  return passes;
}

} // namespace

bool savesGuideTree(const Options &opts, const Frame &frame,
                    const std::string &outputSpectrum) {
  return opts.render.guide.isEnabled && frame.spp > 0 &&
         !outputSpectrum.empty();
}

void renderSamples(const Options &opts, const Frame &frame,
                   const ResolvedGrid &grid, smdl::Compiler &compiler,
                   const StagedScene &staged, ResumedSequence &resumed,
                   smdl::SpectralFilm &film, const Response *response,
                   smdl::SpectralFilm *bandFilm,
                   smdl::SpectralFilm *bandSquares,
                   const std::string &outputSpectrum,
                   std::unique_ptr<STree> &sdtree) {
  SMDL_SANITY_CHECK(!bandFilm || response);
  SMDL_SANITY_CHECK(!bandFilm == !bandSquares);
  const auto &wavelengths{grid.wavelengths};
  const auto &scene{*staged.scene};
  const auto &lights{*staged.lights};
  const auto *envLight{staged.envLight.get()};
  const auto *haze{staged.haze.get()};
  const auto *exteriorMedium{staged.exteriorMedium};
  const auto &guideBound{staged.guideBound};
  const bool hasValidGuideBounds{staged.hasValidGuideBounds};
  const auto &camera{frame.camera};
  const auto numPixelsX{frame.numPixelsX};
  const auto numPixelsY{frame.numPixelsY};
  const auto numWindowPixels{frame.numWindowPixels};
  const auto window{frame.window};
  const auto spp{frame.spp};
  const bool isSavingTree{savesGuideTree(opts, frame, outputSpectrum)};
  auto progressOptions{opts.utility.progress};
  // How many samples per pixel trained the resumed tree, 0 without one:
  // what the pass schedule continues from.
  size_t guideTrainedSpp{0};
  if (opts.render.guide.isEnabled && resumed.wasLoaded) {
    // Resume the guide tree saved beside the accumulation, so this
    // session starts guided by everything the sequence has learned. The
    // tree only steers sampling, so a missing or unreadable one is
    // never fatal: retraining from scratch is always safe, just slower
    // to converge.
    const auto treeName{opts.image.resume + std::string(GUIDE_TREE_EXTENSION)};
    if (smdl::exists(treeName)) {
      try {
        uint64_t treeSpp{};
        sdtree = std::make_unique<STree>(STree::readFile(treeName, treeSpp));
        guideTrainedSpp = size_t(treeSpp);
        SMDL_LOG_INFO("Resuming guide tree: ", smdl::Quoted(treeName), ", ",
                      sdtree->leafCount(), " spatial leaves trained by ",
                      treeSpp, " spp");
        if (treeSpp != resumed.info.samplesPerPixel)
          SMDL_LOG_WARN("The guide tree was trained by ", treeSpp,
                        " spp against the accumulation's ",
                        resumed.info.samplesPerPixel,
                        "; using it anyway, since a tree that is behind "
                        "still guides");
      } catch (const smdl::Error &error) {
        SMDL_LOG_WARN("Cannot resume guide tree, retraining from scratch: ",
                      error.message);
      }
    } else {
      SMDL_LOG_INFO("No guide tree at ", smdl::Quoted(treeName),
                    ", retraining from scratch");
    }
  }
  if (opts.render.guide.isEnabled && !sdtree) {
    // With a ground plane, guide over the actual geometry padded by half
    // its own size, so the plane's enormous backdrop extent does not
    // dilute the spatial resolution; vertices on the far plane clamp
    // into the border cells, where there is nothing worth guiding
    // anyway. Without one, the scene bounds are the geometry bounds.
    auto center{scene.boundCenter};
    auto r{scene.boundRadius};
    if (hasValidGuideBounds) {
      center = guideBound.center();
      r = 0.75f * smdl::length(guideBound.extent());
    }
    sdtree = std::make_unique<STree>(center - float3(r, r, r),
                                     center + float3(r, r, r));
    SMDL_LOG_INFO("Guide bounds: center (", center.x, ", ", center.y, ", ",
                  center.z, "), radius ", r);
  }
  // The combination of the guided passes, which also maintains the
  // ADRRS pixel estimates between passes. Null without guiding, where
  // the single pass accumulates straight into `film`.
  auto combiner{std::unique_ptr<PassCombiner>()};
  if (opts.render.guide.isEnabled) {
    combiner = std::make_unique<PassCombiner>(numPixelsX, numPixelsY, window);
    // Seed with the prior session's accumulation, so resolve() below
    // reproduces the full merged image (the unguided path adds it into
    // the accumulation instead, just below) and the first pass's ADRRS
    // starts from the resumed estimates rather than zero.
    if (resumed.wasLoaded) {
      combiner->seed(resumed.film);
      combiner->rebuildPixelEstimates();
    }
  }
  // Merge a resumed session's samples in before rendering rather than
  // after it, so that the previews written along the way already stand on
  // every sample taken and the image is never displayed noisier than it
  // is. One image-level add, which is exactly the merge the
  // sums-plus-count invariant makes safe; every read below divides by
  // the combined count.
  if (resumed.wasLoaded && !combiner) film.add(resumed.film);
  // The band films accumulate straight through, guided or not, so the
  // resumed ones merge the same way on both paths.
  if (bandFilm && resumed.bandFilm.getNumSamples() > 0) {
    bandFilm->add(resumed.bandFilm);
    bandSquares->add(resumed.bandSquares);
  }
  // Nothing reads them again, and they are the size of the films being
  // rendered into.
  resumed.film.clear();
  resumed.bandFilm.clear();
  resumed.bandSquares.clear();
  // Progress is counted in samples rather than pixels, so that the
  // geometrically growing passes below read as one bar that only ever
  // moves forward. The counters still show pixels, which is the number a
  // person pictures. Nothing is drawn unless stderr is a terminal, where
  // the summary below takes the bar's place.
  // The radiance the renderer estimates, as linear RGB: what the floating
  // point output holds, and what every tonemap displays. Resolved here
  // rather than at the outputs because a checkpoint image runs the same
  // path mid-render.
  // Rewriting the tone mapped output while the render runs, so that a tool
  // watching the file sees the image converge. The sums-plus-count film
  // is a valid mean at every moment, so a checkpoint is the finished write
  // with fewer samples behind it, and nothing about the estimator changes.
  // Written beside the output and renamed into place: a watcher polling
  // the path never opens a half-written PNG.
  const double previewEvery{std::max(double(opts.utility.previewEvery), 0.0)};
  const bool isCheckpointing{previewEvery > 0.0 &&
                             !opts.image.outputRGB.empty()};
  const auto writeDisplayImage{[&] {
    // Resolve first, so that a guided preview stands on every pass folded
    // so far, the resumed seed included, instead of the newest pass
    // alone. Guided checkpoints only happen on pass boundaries, where the
    // pass just rendered is already folded in.
    if (combiner) combiner->resolve(film);
    const auto path{std::filesystem::path(opts.image.outputRGB)};
    auto partPath{path};
    partPath.replace_extension("part" + path.extension().string());
    auto rgb{resolveRGB(compiler, film, wavelengths, opts.image.rgbPolicy)};
    // Filtered like the final write, so that a checkpoint differs from
    // it only in how many samples stand behind it.
    (void)medianFilterRGB(opts.image.medianFilter, rgb, numPixelsX, window);
    const auto ldr{tonemap(opts.image.tonemap, rgb, film, wavelengths)};
    if (auto error{smdl::write8bitImage(partPath.string(), //
                                        int(numPixelsX), int(numPixelsY), 3,
                                        ldr.data())}) {
      error->print();
      return;
    }
    // A checkpoint that loses the rename is one missed preview, not a
    // reason to stop the render.
    (void)smdl::tryRenameOnto(partPath.string(), path.string());
  }};
  auto lastCheckpoint{std::chrono::steady_clock::now()};
  const auto checkpoint{[&] {
    if (!isCheckpointing) return;
    const auto now{std::chrono::steady_clock::now()};
    if (std::chrono::duration<double>(now - lastCheckpoint).count() <
        previewEvery)
      return;
    writeDisplayImage();
    // Timed from the end of the write, so that an image expensive to tone
    // map spaces its checkpoints out instead of running back to back.
    lastCheckpoint = std::chrono::steady_clock::now();
  }};

  const auto passes{
      solveSamplePasses(spp, opts.render.guide.isEnabled, guideTrainedSpp)};
  // The manifold-NEE chain depth `tracePath()` runs with, 0 when
  // disabled.
  auto mneeOptions{opts.render.mnee};
  // The reflective gather searches this in place of the straight shadow
  // segment, so it is built once per render: the layout's marked casters,
  // with what each claims.
  auto mneeCasters{MNEECasterSet()};
  if (opts.render.useMNEE) {
    mneeCasters = MNEECasterSet(scene, wavelengths, mneeOptions.maxRoughness);
    mneeOptions.casters = &mneeCasters;
    SMDL_LOG_DEBUG("MNEE casters: ", mneeCasters.casters.size(),
                   " instance(s)");
    if (opts.render.useMNEESunOnly && envLight)
      mneeOptions.isSunOnly =
          envLight->sunCone(mneeOptions.sunDirection, mneeOptions.cosSunRadius);
  }
  // The default walk is terminated by Russian roulette, with the bounce
  // bound set high enough that clipping it is negligible even for
  // high-albedo transport; giving -max-bounces makes the bound the whole
  // termination rule, so the estimate is the fixed-depth truncation.
  const auto &pathOptions{opts.render.path};
  // What every path of this render is traced against; see
  // `RenderContext`. Built once, shared by every worker thread.
  const RenderContext render{compiler,    scene, lights,        mneeOptions,
                             pathOptions, haze,  exteriorMedium};
  // The tally behind -report: every block adds its own into this one at
  // the block's end, under the mutex, so the walk itself never shares
  // a write. Empty when nobody asked, and then no block tallies at all.
  std::optional<PathStats> stats;
  std::mutex statsMutex;
  if (opts.render.shouldReportStats) stats.emplace();
  // Whether every sample draws its own wavelength grid; see
  // `WavelengthGrid::bandEdges` and `jitterWavelengths()`.
  const bool shouldJitterWavelength{!gRenderGrid.bandEdges.empty()};
  // The window row length, which turns a window pixel index into a frame
  // pixel index below.
  const size_t windowWidth{size_t(window[2] - window[0])};
  // The parallel loop runs over blocks of pixels rather than over single
  // pixels, so that everything a pixel needs but does not own (the
  // allocator, the sampler, the guide record buffer, the jitter buffer)
  // is built once per block: on a guiding pass the record buffer alone
  // is `maxBounces + 1` records of runtime-sized spectra. Nothing about
  // the estimate depends on how the pixels are grouped, since each one
  // writes its own film cell from a sampler that is deterministic in
  // (pixel, sample index) and the guide accumulator counts in integers.
  //
  // The size is a balance: large enough to amortize that setup, small
  // enough that one block of expensive pixels cannot become the tail the
  // rest of the pool waits on. Taken from the work available so a
  // thumbnail still spreads over every thread.
  constexpr size_t MAX_PIXELS_PER_BLOCK{64};
  constexpr size_t BLOCKS_PER_THREAD{8};
  const size_t blockSize{std::clamp<size_t>(
      numWindowPixels / (BLOCKS_PER_THREAD * smdl::getThreadCount()), 1,
      MAX_PIXELS_PER_BLOCK)};
  const size_t numBlocks{(numWindowPixels + blockSize - 1) / blockSize};
  progressOptions.total = numWindowPixels * spp;
  progressOptions.displayScale = std::max<size_t>(spp, 1);
  progressOptions.summary =
      opts.image.cropWindow.wasGiven
          ? smdl::concat("Rendered window ", spellVector(window), " of ",
                         numPixelsX, "x", numPixelsY, " at ", spp, " spp")
          : smdl::concat("Rendered ", numPixelsX, "x", numPixelsY, " at ", spp,
                         " spp");
  // The render window the cumulative times above measure: the sample
  // passes and the previews written between them, but none of the setup
  // that came before or the outputs that come after, so that the number
  // means the same thing in every session of a resumed sequence.
  // The sun-sky resolved onto the render-wide grid, which every path of
  // every block shares because the grid holds still. Read-only once the
  // threads start. A jittering render cannot use it: its grid moves with
  // every sample, so each block resolves its own below.
  smdl::SkyBasis renderSkyBasis;
  if (!shouldJitterWavelength && lights.env())
    lights.env()->resolve(wavelengths, renderSkyBasis);
  const auto renderStartWall{std::chrono::steady_clock::now()};
  const double renderStartCompute{cpuTimeSeconds()};
  ProgressBar progress{progressOptions};
  size_t sppDone{0};
  size_t chunkSpp{1};
  for (size_t passIndex = 0; passIndex < passes.size(); passIndex++) {
    const size_t thisPass{passes[passIndex]};
    const bool isFinal{passIndex + 1 == passes.size()};
    if (opts.render.guide.isEnabled)
      progress.setNote(
          smdl::concat("pass ", passIndex + 1, "/", passes.size()));
    // Pre-final passes train the SD-tree; every pass contributes to the
    // output through the pass combination below. When the tree will be
    // saved the final pass trains too: its training is no longer wasted,
    // it is what the next session of the sequence inherits.
    const bool shouldRecordPass{opts.render.guide.isEnabled &&
                                (!isFinal || isSavingTree)};
    // The per-thread training mirrors for this pass, absorbed into the
    // tree after the pass renders and before it refines; the tree
    // structure the layout mirrors is frozen in between.
    auto guideAccumulator{std::unique_ptr<GuideAccumulator>()};
    if (shouldRecordPass)
      guideAccumulator = std::make_unique<GuideAccumulator>(*sdtree);
    // Without guiding the whole budget is one pass, so checkpointing has
    // to split it; the chunk starts at one sample, so the first image
    // lands almost immediately, and then grows toward the interval asked
    // for. With guiding the passes are the chunks: they already grow
    // geometrically, and splitting one would change what the combiner
    // weights.
    const bool isChunked{isCheckpointing && !combiner};
    for (size_t passDone{0}; passDone < thisPass;) {
      const size_t chunk{isChunked ? std::min(chunkSpp, thisPass - passDone)
                                   : thisPass - passDone};
      const size_t chunkBase{passDone};
      const auto chunkStart{std::chrono::steady_clock::now()};
      smdl::parallelFor(0, numBlocks, [&](size_t block) {
        // Denormals are worth flushing for the whole task: the material
        // code the walk runs produces them, and the microcode assist each one
        // costs is a measurable fraction of the render.
        const smdl::ScopedFlushDenormals flushDenormals{};
        // The scratch the block's pixels take in turn. The allocator is
        // rewound after every sample and the sampler restarted at every
        // one, so what a block shares is the memory, never the state.
        smdl::BumpPtrAllocator allocator;
        Sampler sampler;
        // The medium view every path of the block resolves through, with
        // the haze it stands in for the vacuum set once: its component
        // storage and its resolution both carry from path to path, see
        // `PathContext::medium`.
        Medium medium;
        medium.setHaze(haze);
        // Training records for `trainGuiding()`, one per vertex the walk
        // may reach, sized only on the pre-final guiding passes that fill
        // them: at a runtime band count every record holds sized vectors,
        // too much to pay per pixel of a non-guiding render. The walk
        // resets every record it appends, so one buffer serves the block.
        std::vector<GuideRecord> guideRecords;
        if (shouldRecordPass) guideRecords.resize(pathOptions.maxBounces + 1);
        GuideRecord *const records{shouldRecordPass ? guideRecords.data()
                                                    : nullptr};
        // The sample's own wavelength grid, rewritten in place once per
        // sample: a `Color` past `SpectralColor::INLINE_CAPACITY` bands
        // heaps, and every state built from it holds the pointer rather
        // than a copy, so one buffer serves the block.
        std::optional<Color> jittered;
        if (shouldJitterWavelength) jittered.emplace(wavelengths);
        // The sun-sky resolved onto this sample's own grid, rewritten
        // below once per sample, which still amortizes over the many
        // evaluations one sample makes. Empty and unused when the
        // wavelengths hold still, where `renderSkyBasis` serves instead.
        smdl::SkyBasis jitteredSkyBasis;
        const smdl::SkyBasis &skyBasis{shouldJitterWavelength ? jitteredSkyBasis
                                                              : renderSkyBasis};
        // The four states every path of the block works in, built here
        // rather than per path: only the animation time below tells one
        // path's from another's, and the jittered grid is rewritten in
        // place, so the wavelength pointer holds still too. See
        // `PathContext`.
        const Color &blockWavelengths{shouldJitterWavelength ? *jittered
                                                             : wavelengths};
        smdl::State gatherState{makeRenderState(blockWavelengths, &allocator)};
        smdl::State walkState{makeRenderState(blockWavelengths, &allocator)};
        smdl::State shadeState{makeRenderState(blockWavelengths, &allocator)};
        smdl::State lightState{makeRenderState(blockWavelengths, &allocator)};
        // The gather scratch, bought here for the same reason.
        LightSample gatherSample{};
        Hit gatherBlocker{};
        Guiding guiding{};
        guiding.tree = sdtree.get();
        guiding.bsdfFraction =
            std::clamp(opts.render.guide.bsdfFraction.value, 0.0f, 1.0f);
        guiding.isBSDFFractionFixed = opts.render.guide.bsdfFraction.wasGiven;
        // The context and the walker of the block's paths; the time is
        // the open key until each path sets its own.
        PathContext path{allocator,     sampler,          medium,
                         skyBasis,      gatherState,      walkState,
                         shadeState,    lightState,       gatherSample,
                         gatherBlocker, blockWavelengths, PathTime{0.0f},
                         &guiding,      records};
        // The block's own tally, which the walk fills and the end of the
        // block folds into the render's.
        std::optional<PathStats> blockStats;
        if (stats) {
          blockStats.emplace();
          path.stats = &*blockStats;
        }
        const auto walk{makePathWalk(render, path)};
        // The block's per-pixel band sums and their squares, when there
        // is a response to project onto.
        std::vector<double> bandSums(bandFilm ? response->filmBandCount() : 0);
        std::vector<double> bandSquareSums(bandSums.size());
        const size_t kBegin{block * blockSize};
        const size_t kEnd{std::min(numWindowPixels, kBegin + blockSize)};
        for (size_t k = kBegin; k < kEnd; k++) {
          // The pixel index in the whole frame, which seeds the sampler and
          // addresses every per-pixel buffer, so a window renders the same
          // pixels the whole frame would.
          const size_t i{(size_t(window[1]) + k / windowWidth) * numPixelsX +
                         (size_t(window[0]) + k % windowWidth)};
          const size_t x{i % numPixelsX};
          const size_t y{i / numPixelsX};
          Color Lsum{};
          std::fill(bandSums.begin(), bandSums.end(), 0.0);
          std::fill(bandSquareSums.begin(), bandSquareSums.end(), 0.0);
          PassCombiner::PixelHalves halves{};
          guiding.pixelEstimate = combiner && opts.render.guide.useADRRS
                                      ? combiner->pixelEstimate(i)
                                      : 0.0f;
          for (size_t s = 0; s < chunk; s++) {
            const uint32_t sampleIndex =
                resumed.sampleIndexBase + sppDone + chunkBase + s;
            sampler.startPixelSample(uint32_t(i), sampleIndex);
            if (shouldJitterWavelength) {
              jitterWavelengths(
                  *jittered, wavelengthJitterOffset(uint32_t(i), sampleIndex));
              if (render.lights.env())
                render.lights.env()->resolve(*jittered, jitteredSkyBasis);
            }
            Color Lsample{};
            // A fully vignetted sample contributes nothing, so skip the
            // walk but let it still count in the average below, keeping the
            // darkening unbiased.
            uint64_t numRecords{0};
            if (auto cameraSample{camera->sample(x, y, sampler)};
                cameraSample.weight > 0) {
              // The path's time: the sample's draw within its line's
              // exposure, taken only when there is one to draw within,
              // matching the lens-point precedent so a default render's
              // sampler sequence is unchanged, and mapped onto the frame
              // by the shutter, which is where a rolling readout enters.
              // The camera ray is placed in the world only now, at that
              // time.
              float xi{};
              if (gRenderShutter.hasExposure()) xi = float(sampler);
              const PathTime time{gRenderShutter.fractionAt(x, y, xi)};
              camera->toWorld(cameraSample, time.fraction);
              gatherState.animationTime = time.seconds;
              walkState.animationTime = time.seconds;
              shadeState.animationTime = time.seconds;
              lightState.animationTime = time.seconds;
              path.time = time;
              Lsample = tracePath(*walk, cameraSample);
              numRecords = path.numRecords;
            }
            // Train the SD-tree on the records the walk retained.
            if (shouldRecordPass && numRecords > 0)
              trainGuiding(*sdtree, *guideAccumulator, sampler,
                           guideRecords.data(), numRecords);
            Lsum += Lsample;
            if (bandFilm)
              response->accumulate(smdl::Span<const float>(blockWavelengths),
                                   smdl::Span<const float>(Lsample), x, y,
                                   bandSums.data(), bandSquareSums.data());
            if (combiner) {
              // Split the samples into two half images so the combination can
              // cross-weight each half by the other's variance estimate.
              float value{Lsample.average()};
              if ((chunkBase + s) % 2 == 0) {
                halves.halfA += Lsample;
                halves.squaresA += value * value;
              } else {
                halves.halfB += Lsample;
                halves.squaresB += value * value;
              }
            }
            allocator.reset();
          }
          // With guiding the combination owns the film and resolves into
          // it, pass by pass; without, the accumulation is the film.
          if (combiner) {
            combiner->deposit(i, halves);
          } else {
            film.addTotals(x, y, Lsum.data());
          }
          if (bandFilm) {
            bandFilm->addTotals(x, y, bandSums.data());
            bandSquares->addTotals(x, y, bandSquareSums.data());
          }
        }
        if (blockStats) {
          blockStats->addSamples(chunk * (kEnd - kBegin));
          const std::lock_guard<std::mutex> lock{statsMutex};
          stats->add(*blockStats);
        }
        // Counted where the work is finished rather than where it starts,
        // which at thumbnail sizes is a whole pool's worth of pixels.
        progress.advance(chunk * (kEnd - kBegin));
      });
      // Every pixel of the window took the same samples, so the count
      // belongs to the film rather than to each pixel, and is recorded
      // once here where the chunk is finished. It has to land before the
      // checkpoint below, which divides by it.
      if (!combiner) film.addSamples(chunk);
      if (bandFilm) {
        bandFilm->addSamples(chunk);
        bandSquares->addSamples(chunk);
      }
      passDone += chunk;
      if (isChunked) {
        // Aim the next chunk at the interval from what this one cost,
        // and never more than quadruple it at once: the first chunk is
        // one sample, and a scene that is cheap at one sample and dear at
        // sixty-four should not overshoot the whole way there.
        const double seconds{std::chrono::duration<double>(
                                 std::chrono::steady_clock::now() - chunkStart)
                                 .count()};
        const double perSample{seconds / double(chunk)};
        const size_t wanted{
            perSample > 0.0 ? size_t(std::max(previewEvery / perSample, 1.0))
                            : thisPass};
        chunkSpp = std::clamp<size_t>(wanted, 1, chunk * 4);
        checkpoint();
      }
    }
    if (combiner) combiner->foldPass(thisPass);
    if (shouldRecordPass) {
      guideAccumulator->absorbInto(*sdtree);
      combiner->rebuildPixelEstimates();
      // Refine: split spatial leaves past c*sqrt(2^k) records (k this
      // pass's index), rebuild the directional quadtrees with the 1% flux
      // threshold.
      sdtree->refine(
          uint32_t(double(opts.render.guide.split) * std::sqrt(thisPass)),
          0.01f, 20);
      float minAlpha{}, meanAlpha{};
      sdtree->alphaStats(minAlpha, meanAlpha);
      SMDL_LOG_INFO("Guide pass ", passIndex + 1, "/", passes.size(),
                    " done: ", thisPass, " spp, ", sdtree->leafCount(),
                    " spatial leaves, alpha min ", minAlpha, " mean ",
                    meanAlpha);
    }
    sppDone += thisPass;
    // A guided render checkpoints on its own pass boundaries, which is
    // where its image changes anyway.
    if (!isFinal) checkpoint();
  }
  progress.finish();
  // A '-spp 0' re-run of the output stage rendered nothing, so it is not
  // a session and must leave the totals it rewrites exactly as they were.
  if (spp > 0) {
    resumed.header.seconds +=
        std::chrono::duration<double>(std::chrono::steady_clock::now() -
                                      renderStartWall)
            .count();
    resumed.header.cpuSeconds +=
        std::max(cpuTimeSeconds() - renderStartCompute, 0.0);
    resumed.header.sessions++;
  }
  if (stats) {
    const PathStatsSession session{window, spp, resumed.sampleIndexBase};
    if (opts.utility.useJSON)
      stats->printJSON(llvm::outs(), session, pathOptions, mneeOptions);
    else
      stats->print(llvm::outs(), session, pathOptions, mneeOptions);
    llvm::outs().flush();
  }
  // Resolve the pass combination back into the film every downstream
  // output reads from. A resumed session's samples are already in there,
  // through the seeded combination or the add before the render.
  if (combiner) combiner->resolve(film);
}
