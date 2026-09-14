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

#include "smdl/RenderUtil/OpticalGlass.h"
#include "smdl/Support/Denormals.h"
#include "smdl/Support/Filesystem.h"
#include "smdl/Support/Logger.h"
#include "smdl/Support/Parallel.h"
#include "smdl/Support/Strings.h"

#include "MedianFilter.h"
#include "Options.h"
#include "Output.h"
#include "Progress.h"
#include "Render.h"
#include "Render/Guiding.h"
#include "Render/Manifold.h"
#include "Render/PathStats.h"
#include "Render/PathTracing.h"
#include "Render/Sampler.h"
#include "RenderFilm.h"
#include "Resume.h"
#include "Sensor/Meter.h"
#include "Sensor/Response.h"
#include "Stage.h"

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

// The meter's tally of one call of the kernel: each block's sum of the
// projections of its samples and their count, folded in block order
// afterward, so the reading is the same on any thread count.
struct MeterTally final {
  const MeterProjection *projection{};
  std::vector<double> sums{};
  std::vector<uint64_t> counts{};
};

// One call of the render kernel: what it draws, which pixels, and what
// it feeds. The render feeds the film, the band film, the combiner, the
// guide accumulator, the tally, and the progress bar, a chunk of the
// window at a time; the metering pass feeds the meter alone, over its
// lattice. A null sink is not fed.
struct PassTarget final {
  // The samples per pixel this call draws, from `sampleIndexBase`.
  size_t spp{};
  size_t sampleIndexBase{};

  // The pass's offset of the first sample, which decides which half a
  // guided sample lands in.
  size_t passOffset{};

  // The pixels: `numPixels` of them, as frame indices in `pixels`, or
  // the window's when `pixels` is empty.
  size_t numPixels{};
  smdl::Span<const size_t> pixels{};

  size_t blockSize{};

  smdl::SpectralFilm *film{};
  smdl::SpectralFilm *bandFilm{};
  PassCombiner *combiner{};
  GuideAccumulator *guideAccumulator{};
  PathStats *stats{};
  ProgressBar *progress{};
  MeterTally *meter{};
};

// The pixels one call of the kernel hands a thread, and how many such
// blocks each thread gets.
//
// The parallel loop runs over blocks rather than single pixels so that
// everything a pixel needs but does not own (the allocator, the sampler,
// the guide record buffer, the jitter buffer) is built once per block: on
// a guiding pass the record buffer alone is `maxBounces + 1` records of
// runtime-sized spectra. Nothing about the estimate depends on how the
// pixels are grouped, since each one writes its own film cell from a
// sampler deterministic in (pixel, sample index) and the guide
// accumulator counts in integers.
//
// The size is a balance: large enough to amortize that setup, small enough
// that one block of expensive pixels cannot become the tail the rest of
// the pool waits on. Taken from the work available, so a thumbnail still
// spreads over every thread.
//
// \{
constexpr size_t MAX_PIXELS_PER_BLOCK{64};
constexpr size_t BLOCKS_PER_THREAD{8};
// \}

// The SD-tree this session guides by, into `sdtree`, and how many samples
// per pixel already trained it, which the pass schedule continues from.
//
// A resumed session takes the tree saved beside the accumulation, so it
// starts guided by everything the sequence has learned. The tree only
// steers sampling, so a missing or unreadable one is never fatal:
// retraining from scratch is always safe, just slower to converge.
//
// Leaves `sdtree` null, and answers 0, when guiding is off.
[[nodiscard]] size_t openGuideTree(const Options &opts,
                                   const StagedScene &staged,
                                   const ResumedSequence &resumed,
                                   std::unique_ptr<STree> &sdtree) {
  if (!opts.render.guide.isEnabled) return 0;
  size_t trainedSpp{0};
  if (resumed.wasLoaded) {
    const std::string treeName{opts.image.resume +
                               std::string(GUIDE_TREE_EXTENSION)};
    if (smdl::exists(treeName)) {
      try {
        uint64_t treeSpp{};
        sdtree = std::make_unique<STree>(STree::readFile(treeName, treeSpp));
        trainedSpp = size_t(treeSpp);
        SMDL_LOG_INFO("Resuming guide tree: ", SpellQuoted(treeName), ", ",
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
      SMDL_LOG_INFO("No guide tree at ", SpellQuoted(treeName),
                    ", retraining from scratch");
    }
  }
  if (sdtree) return trainedSpp;
  // With a ground plane, guide over the actual geometry padded by half its
  // own size, so the plane's enormous backdrop extent does not dilute the
  // spatial resolution; vertices on the far plane clamp into the border
  // cells, where there is nothing worth guiding anyway. Without one, the
  // scene bounds are the geometry bounds.
  float3 center{staged.scene->boundCenter};
  float r{staged.scene->boundRadius};
  if (staged.hasValidGuideBounds) {
    center = staged.guideBound.center();
    r = 0.75f * smdl::length(staged.guideBound.extent());
  }
  sdtree = std::make_unique<STree>(center - float3(r, r, r),
                                   center + float3(r, r, r));
  SMDL_LOG_INFO("Guide bounds: center (", center.x, ", ", center.y, ", ",
                center.z, "), radius ", r);
  return trainedSpp;
}

// The combination of the guided passes, which also maintains the ADRRS
// pixel estimates between passes. Null without guiding, where the single
// pass accumulates straight into the film; and null through a sensor
// unless ADRRS wants the estimates, since the band film accumulates
// straight through, guided or not, and there is no spectral film to
// resolve into.
[[nodiscard]] std::unique_ptr<PassCombiner>
makePassCombiner(const Options &opts, const Frame &frame,
                 const RenderFilm &target, const ResumedSequence &resumed) {
  if (!opts.render.guide.isEnabled ||
      (target.hasResponse() && !opts.render.guide.useADRRS))
    return {};
  auto combiner{std::make_unique<PassCombiner>(frame.numPixelsX,
                                               frame.numPixelsY, frame.window)};
  // Seed with the prior session's accumulation, so resolve() reproduces
  // the full merged image (the unguided path adds it into the accumulation
  // instead) and the first pass's ADRRS starts from the resumed estimates
  // rather than zero.
  if (resumed.wasLoaded && target.spectralFilm()) {
    combiner->seed(resumed.film);
    combiner->rebuildPixelEstimates();
  }
  return combiner;
}

// Which manifold estimators run for this render, and the caster set the
// gathers search in place of the straight shadow segment.
//
// The set is built once per render from the layout's marked casters, with
// what each claims; the options point into `casters`, which the caller
// owns for as long as anything traces.
[[nodiscard]] MNEEOptions makeMNEEOptions(const Options &opts,
                                          const Scene &scene,
                                          const Color &wavelengths,
                                          const EnvLight *envLight,
                                          MNEECasterSet &casters) {
  MNEEOptions mneeOptions{opts.render.mnee};
  if (!opts.render.useMNEE) return mneeOptions;
  casters = MNEECasterSet(scene, wavelengths);
  mneeOptions.casters = &casters;
  SMDL_LOG_DEBUG("MNEE casters: ",
                 SpellCounted(casters.casters.size(), "instance"));
  if (opts.render.useMNEESunOnly && envLight)
    mneeOptions.isSunOnly =
        envLight->sunCone(mneeOptions.sunDirection, mneeOptions.cosSunRadius);
  return mneeOptions;
}

// The RGB checkpoints written while the render runs, so that a tool
// watching the file sees the image converge.
//
// The sums-plus-count film is a valid mean at every moment, so a
// checkpoint is the finished write with fewer samples behind it, and
// nothing about the estimator changes. Each is written beside the output
// and renamed into place, so a watcher polling the path never opens a
// half-written image.
class Checkpointer final {
public:
  Checkpointer(const Options &opts, const Frame &frame,
               smdl::Compiler &compiler, const RenderHeader &header,
               RenderFilm &target, PassCombiner *combiner)
      : mOpts(opts), mFrame(frame), mCompiler(compiler), mHeader(header),
        mTarget(target), mCombiner(combiner),
        mEvery(std::max(double(opts.utility.previewEvery), 0.0)),
        mIsEnabled(mEvery > 0.0 && !opts.image.outputRGB.empty()) {}

  // Is anything written at all? A render whose budget is one pass has to
  // split it into chunks to checkpoint within it, so the loop asks.
  [[nodiscard]] bool isEnabled() const noexcept { return mIsEnabled; }

  // The interval asked for, in seconds.
  [[nodiscard]] double every() const noexcept { return mEvery; }

  // Write one if the interval has passed since the last, and nothing
  // otherwise.
  void writeIfDue() {
    if (!mIsEnabled) return;
    const auto now{std::chrono::steady_clock::now()};
    if (std::chrono::duration<double>(now - mLast).count() < mEvery) return;
    write();
    // Timed from the end of the write, so that an image expensive to tone
    // map spaces its checkpoints out instead of running back to back.
    mLast = std::chrono::steady_clock::now();
  }

private:
  void write() {
    // Resolve first, so that a guided preview stands on every pass folded
    // so far, the resumed seed included, instead of the newest pass alone.
    // Guided checkpoints only happen on pass boundaries, where the pass
    // just rendered is already folded in.
    if (mCombiner && mTarget.spectralFilm())
      mCombiner->resolve(*mTarget.spectralFilm());
    // Developed as the final picture is, a physical sensor's without its
    // noise, and filtered as the final write is, so that a checkpoint
    // differs from it only in how many samples stand behind it.
    std::vector<float> rgb{
        developPreview(mOpts, mFrame, mCompiler, mHeader, mTarget)};
    (void)medianFilterRGB(mOpts.image.medianFilter, rgb, mFrame.numPixelsX,
                          mFrame.window);
    for (const auto &fileName : mOpts.image.outputRGB) {
      const std::filesystem::path path{fileName};
      std::filesystem::path partPath{path};
      partPath.replace_extension("part" + path.extension().string());
      if (std::optional<smdl::Error> error{
              writeRGBImage(mOpts, mTarget, partPath.string(), rgb,
                            mFrame.numPixelsX, mFrame.numPixelsY)}) {
        error->print();
        continue;
      }
      // A checkpoint that loses the rename is one missed preview, not a
      // reason to stop the render.
      (void)smdl::tryRenameOnto(partPath.string(), path.string());
    }
  }

  const Options &mOpts;
  const Frame &mFrame;
  smdl::Compiler &mCompiler;
  const RenderHeader &mHeader;
  RenderFilm &mTarget;
  PassCombiner *mCombiner{};
  double mEvery{};
  bool mIsEnabled{};
  std::chrono::steady_clock::time_point mLast{std::chrono::steady_clock::now()};
};

// Meter the scene when the shot needs it, then say what the readout's ISO
// will be.
//
// `renderBlocks` is the kernel, which the metering pass calls once over its
// own sparse lattice rather than over the window.
template <typename RenderBlocks>
void meterAndLogISO(const Frame &frame, ResumedSequence &resumed,
                    RenderBlocks &&renderBlocks) {
  // The meter, before the shutter opens: a sensor whose ISO nothing states
  // is metered once for the sequence, from one sample at each pixel of a
  // sparse lattice over the window, read through the sensor's most
  // sensitive band, and every later session reads the rung back. The
  // samples are thrown away; they overlap the render's first sample at
  // those pixels, which costs the lattice's worth of paths, a fraction of
  // a sample per pixel. Not timed as the render is: a resumed session
  // pays nothing for it.
  const CameraModel &model{frame.model};
  const bool isMetering{model.sensor &&
                        needsMeter(*model.sensor, resumed.header)};
  if (isMetering) {
    const Sensor &sensor{*model.sensor};
    const MeterProjection projection{sensor};
    const MeterLattice lattice{frame.window, frame.numPixelsX,
                               gRenderGrid.tileColumns, gRenderGrid.tileRows,
                               projection.countingCells()};
    MeterTally tally{};
    tally.projection = &projection;
    PassTarget pass{};
    pass.spp = 1;
    pass.sampleIndexBase = resumed.sampleIndexBase;
    pass.numPixels = lattice.pixels().size();
    pass.pixels = smdl::Span<const size_t>(lattice.pixels().data(),
                                           lattice.pixels().size());
    // A fixed block size, so the fold below associates the same way on
    // any thread count and the rung stamped in the header is the same on
    // any machine.
    pass.blockSize = MAX_PIXELS_PER_BLOCK;
    pass.meter = &tally;
    renderBlocks(pass);
    MeterReading reading{};
    for (size_t b = 0; b < tally.sums.size(); b++) {
      reading.sum += tally.sums[b];
      reading.count += tally.counts[b];
    }
    SMDL_LOG_DEBUG("Meter: ", SpellCounted(reading.count, "sample"),
                   " through ", SpellQuoted(projection.bandName()),
                   " pixels, one per ", lattice.stride(), " tiles");
    // Under -ideal the film is the observer's radiance, which the model's
    // preview scale turns into the irradiance the sensor would have
    // metered.
    const double irradianceScale{
        model.hasSensor() ? 1.0 : model.previewIrradianceScale};
    recordMeter(sensor, reading, gRenderShutter.exposure * irradianceScale,
                resumed.header);
  }
  if (model.sensor)
    logISO(*model.sensor, model.iso, resumed.header, gRenderShutter.exposure,
           frame.camera->fNumber(), !isMetering);
}

// Everything one call of the render kernel reads beside its `PassTarget`:
// what the render settled before the first sample and every block of every
// pass then shares.
//
// Named rather than captured because this set is the kernel's interface.
// Everything here is read-only for the render's duration except the tally
// mutex and the tree the guided passes train, so nothing a block does to
// it can be seen by another; that is what lets the blocks run in any
// order on any thread count.
//
// Held by reference where a block takes the address of what it finds (the
// sky basis a sample resolves through, the haze of its pixel's grid, the
// wavelengths its states carry), so that every block sees the one the
// render resolved rather than a copy of it.
struct RenderKernel final {
  const Options &opts;
  const RenderContext &render;
  const PathOptions &pathOptions;
  const std::optional<Camera> &camera;
  const Response *response;
  const Color &wavelengths;
  const smdl::SkyBasis &renderSkyBasis;
  const smdl::Haze *haze;
  const std::vector<smdl::Haze> &hazes;
  const std::unique_ptr<STree> &sdtree;
  std::mutex &statsMutex;
  size_t numPixelsX;
  size_t windowWidth;
  int4 window;
  bool disperses;
  bool hasMovingGrid;
  bool hasPixelGrids;
  bool shouldJitterWavelength;

  // Draw `pass.spp` samples at each of the pass's pixels and feed what it
  // names, a block of pixels per task. See `PassTarget`.
  void operator()(const PassTarget &pass) const;
};

void RenderKernel::operator()(const PassTarget &pass) const {
  const size_t numBlocks{(pass.numPixels + pass.blockSize - 1) /
                         pass.blockSize};
  if (pass.meter) {
    pass.meter->sums.assign(numBlocks, 0.0);
    pass.meter->counts.assign(numBlocks, 0);
  }
  smdl::parallelFor(0, numBlocks, [&](size_t block) {
    // Denormals are worth flushing for the whole task: the material
    // code the walk runs produces them, and the microcode assist each one
    // costs is a measurable fraction of the render.
    const smdl::ScopedFlushDenormals flushDenormals{};
    // The scratch the block's pixels take in turn. The allocator is
    // rewound after every sample and the samplers restarted at every
    // one, so what a block shares is the memory, never the state.
    smdl::BumpPtrAllocator allocator;
    // The sampler the block's paths draw from, which every walk
    // reaches through `PathContext`. Each sample copies its own
    // batch sampler in before walking, so the walk goes on drawing
    // from exactly where the camera ray left off.
    Sampler sampler;
    // One sampler per sample of a batch, each reseeded from its own
    // (pixel, sample index).
    std::array<Sampler, Camera::TRACE_WIDTH> batchSamplers{};
    std::array<CameraSample, Camera::TRACE_WIDTH> batchSamples{};
    std::array<float, Camera::TRACE_WIDTH> batchWavelengths{};
    // The medium view every path of the block resolves through, with
    // the haze it stands in for the vacuum set once: its component
    // storage and its resolution both carry from path to path, see
    // `PathContext::medium`.
    Medium medium;
    medium.setHaze(haze);
    medium.setGridIndex(0);
    // Training records for `trainGuiding()`, one per vertex the walk
    // may reach, sized only on the pre-final guiding passes that fill
    // them: at a runtime band count every record holds sized vectors,
    // too much to pay per pixel of a non-guiding render. The walk
    // resets every record it appends, so one buffer serves the block.
    std::vector<GuideRecord> guideRecords;
    if (pass.guideAccumulator) guideRecords.resize(pathOptions.maxBounces + 1);
    GuideRecord *const records{pass.guideAccumulator ? guideRecords.data()
                                                     : nullptr};
    // The sample's own wavelength grid, rewritten in place once per
    // sample under the jitter and once per pixel under a tile: a
    // `Color` past `SpectralColor::INLINE_CAPACITY` bands heaps, and
    // every state built from it holds the pointer rather than a
    // copy, so one buffer serves the block.
    std::optional<Color> jittered;
    if (hasMovingGrid) jittered.emplace(wavelengths);
    // The sun-sky resolved onto this sample's own grid, rewritten
    // below once per sample, which still amortizes over the many
    // evaluations one sample makes. Empty and unused when the
    // wavelengths hold still, where `renderSkyBasis` serves instead.
    smdl::SkyBasis jitteredSkyBasis;
    const smdl::SkyBasis &skyBasis{hasMovingGrid ? jitteredSkyBasis
                                                 : renderSkyBasis};
    // The four states every path of the block works in, built here
    // rather than per path; see `PathStates`. The jittered grid is
    // rewritten in place, so the wavelength pointer holds still too.
    const Color &blockWavelengths{hasMovingGrid ? *jittered : wavelengths};
    PathStates states{blockWavelengths, allocator};
    // The gather scratch, bought here for the same reason.
    LightSample gatherSample{};
    Hit gatherBlocker{};
    Guiding guiding{};
    guiding.tree = sdtree.get();
    guiding.bsdfFraction =
        std::clamp(opts.render.guide.bsdfFraction.value, 0.0f, 1.0f);
    guiding.isBSDFFractionFixed = opts.render.guide.bsdfFraction.wasGiven;
    // The context and the walker of the block's paths; the time is
    // the open key and the hero wavelength the d line until each path
    // sets its own.
    PathContext path{allocator,
                     sampler,
                     medium,
                     skyBasis,
                     states,
                     gatherSample,
                     gatherBlocker,
                     blockWavelengths,
                     PathTime{0.0f},
                     smdl::FRAUNHOFER_D_LINE,
                     0,
                     &guiding,
                     records};
    // The block's own tally, which the walk fills and the end of the
    // block folds into the render's.
    std::optional<PathStats> blockStats;
    if (pass.stats) {
      blockStats.emplace();
      path.stats = &*blockStats;
    }
    const std::unique_ptr<PathWalk, PathWalkDeleter> walk{
        makePathWalk(render, path)};
    // The block's per-pixel band sums, when there is a response to
    // project onto.
    std::vector<double> bandSums(pass.bandFilm ? response->filmBandCount() : 0);
    const size_t kBegin{block * pass.blockSize};
    const size_t kEnd{std::min(pass.numPixels, kBegin + pass.blockSize)};
    for (size_t k = kBegin; k < kEnd; k++) {
      // The pixel index in the whole frame, which seeds the sampler and
      // addresses every per-pixel buffer, so a window renders the same
      // pixels the whole frame would.
      const size_t i{pass.pixels.empty()
                         ? (size_t(window[1]) + k / windowWidth) * numPixelsX +
                               (size_t(window[0]) + k % windowWidth)
                         : pass.pixels[k]};
      const size_t x{i % numPixelsX};
      const size_t y{i / numPixelsX};
      // Under a tile, the pixel's own grid: what its states carry,
      // what its baked spectra are read by, and, held still, what
      // its samples are evaluated at.
      const size_t gridIndex{gRenderGrid.gridIndexAt(x, y)};
      const WavelengthGrid &grid{gRenderGrid.grids[gridIndex]};
      if (hasPixelGrids) {
        states.applyGrid(grid);
        path.gridIndex = gridIndex;
        medium.setHaze(hazes.empty() ? nullptr : &hazes[gridIndex]);
        medium.setGridIndex(gridIndex);
        if (!shouldJitterWavelength) {
          for (size_t b = 0; b < jittered->size(); b++)
            (*jittered)[b] = grid.wavelengths[b];
          if (render.lights.env())
            render.lights.env()->resolve(*jittered, jitteredSkyBasis);
        }
      }
      Color Lsum{};
      std::fill(bandSums.begin(), bandSums.end(), 0.0);
      PassCombiner::PixelHalves halves{};
      guiding.pixelEstimate = pass.combiner && opts.render.guide.useADRRS
                                  ? pass.combiner->pixelEstimate(i)
                                  : 0.0f;
      // The samples of a pixel are drawn in batches so that the
      // camera rays can be traced together: a real lens traces
      // several times faster on a batch than on one ray at a time,
      // being latency bound on one. Nothing about the estimate
      // depends on the grouping, since `startPixelSample()` reseeds
      // from the (pixel, sample index) pair and each sample's draws
      // follow from that alone, so a batched sample draws exactly
      // what it would have drawn on its own.
      for (size_t sBase = 0; sBase < pass.spp; sBase += Camera::TRACE_WIDTH) {
        const size_t batch{
            std::min(pass.spp - sBase, size_t(Camera::TRACE_WIDTH))};
        // Draw the batch, stopping short of the glass. The
        // wavelength a dispersing lens is traced at comes from the
        // pixel's band and the sample index, and reads none of the
        // per-sample state the walk below sets up.
        for (size_t j = 0; j < batch; j++) {
          const uint32_t sampleIndex = pass.sampleIndexBase + sBase + j;
          batchSamplers[j].startPixelSample(uint32_t(i), sampleIndex);
          batchWavelengths[j] =
              disperses
                  ? response->traceWavelengthAt(
                        x, y, lensWavelengthOffset(uint32_t(i), sampleIndex))
                  : 0.0f;
          camera->sampleDeferred(x, y, batchSamplers[j], batchWavelengths[j],
                                 batchSamples[j]);
        }
        camera->traceDeferred(
            smdl::Span<CameraSample>(batchSamples.data(), batch),
            smdl::Span<const float>(batchWavelengths.data(), batch));

        for (size_t j = 0; j < batch; j++) {
          const size_t s{sBase + j};
          const uint32_t sampleIndex = pass.sampleIndexBase + s;
          sampler = batchSamplers[j];
          if (shouldJitterWavelength) {
            jitterWavelengths(*jittered, grid,
                              wavelengthJitterOffset(uint32_t(i), sampleIndex));
            if (render.lights.env())
              render.lights.env()->resolve(*jittered, jitteredSkyBasis);
          }
          Color Lsample{};
          // A fully vignetted sample contributes nothing, so skip the
          // walk but let it still count in the average below, keeping the
          // darkening unbiased.
          uint64_t numRecords{0};
          const float lensWavelength{batchWavelengths[j]};
          // The same wavelength reaches every material of the path as
          // `State::wavelengthHero`, so that a material refracts at the
          // index the picture is being formed at. A lens that does not
          // disperse, and a band the illuminant leaves dark, draw
          // nothing and leave every material at the d line, which is
          // the reference a glass catalog states `nd` at.
          const float wavelengthHero{
              lensWavelength > 0 ? lensWavelength : smdl::FRAUNHOFER_D_LINE};
          if (CameraSample cameraSample{batchSamples[j]};
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
            states.beginPath(time.seconds, wavelengthHero);
            path.time = time;
            path.wavelengthHero = wavelengthHero;
            Lsample = tracePath(*walk, cameraSample);
            numRecords = path.numRecords;
          }
          // Train the SD-tree on the records the walk retained.
          if (pass.guideAccumulator && numRecords > 0)
            trainGuiding(*sdtree, *pass.guideAccumulator, sampler,
                         guideRecords.data(), numRecords);
          Lsum += Lsample;
          if (pass.bandFilm)
            response->accumulate(smdl::Span<const float>(blockWavelengths),
                                 smdl::Span<const float>(Lsample), x, y,
                                 bandSums.data());
          if (pass.meter) {
            // A poisoned sample reads as black, as `filmMean()` reads
            // a poisoned pixel.
            const double value{pass.meter->projection->project(
                smdl::Span<const float>(blockWavelengths),
                smdl::Span<const float>(Lsample))};
            pass.meter->sums[block] += std::isfinite(value) ? value : 0.0;
            pass.meter->counts[block]++;
          }
          if (pass.combiner) {
            // Split the samples into two half images so the combination can
            // cross-weight each half by the other's variance estimate.
            float value{Lsample.average()};
            if ((pass.passOffset + s) % 2 == 0) {
              halves.halfA += Lsample;
              halves.squaresA += value * value;
            } else {
              halves.halfB += Lsample;
              halves.squaresB += value * value;
            }
          }
          allocator.reset();
        }
      }
      // With guiding the combination owns the film and resolves into
      // it, pass by pass; without, the accumulation is the film.
      if (pass.combiner) {
        pass.combiner->deposit(i, halves);
      } else if (pass.film) {
        pass.film->addTotals(x, y, Lsum.data());
      }
      if (pass.bandFilm) pass.bandFilm->addTotals(x, y, bandSums.data());
    }
    if (blockStats) {
      blockStats->addSamples(pass.spp * (kEnd - kBegin));
      const std::lock_guard<std::mutex> lock{statsMutex};
      pass.stats->add(*blockStats);
    }
    // Counted where the work is finished rather than where it starts,
    // which at thumbnail sizes is a whole pool's worth of pixels.
    if (pass.progress) pass.progress->advance(pass.spp * (kEnd - kBegin));
  });
}

} // namespace

std::vector<size_t> solveSamplePasses(size_t spp, bool useGuiding,
                                      size_t trainedSpp) {
  // The refine threshold scales with the pass size, so a first pass that
  // skipped the warmup refines where the saved tree left off.
  size_t firstPass{1};
  while (useGuiding && firstPass * 2 <= trainedSpp) firstPass *= 2;
  std::vector<size_t> passes{};
  for (size_t sppDone{0}; sppDone < spp;) {
    size_t thisPass{
        useGuiding ? std::min(firstPass << passes.size(), spp - sppDone) : spp};
    if (useGuiding && (spp - sppDone) < 2 * thisPass) thisPass = spp - sppDone;
    passes.push_back(thisPass);
    sppDone += thisPass;
  }
  return passes;
}

bool savesGuideTree(const Options &opts, const Frame &frame,
                    const std::string &outputBands) {
  return opts.render.guide.isEnabled && frame.spp > 0 && !outputBands.empty();
}

void renderSamples(const Options &opts, const Frame &frame,
                   smdl::Compiler &compiler, const StagedScene &staged,
                   ResumedSequence &resumed, RenderFilm &target,
                   const std::string &outputBands,
                   std::unique_ptr<STree> &sdtree) {
  // The film seen both ways, exactly one of which is there: the pass
  // combination and the resumed merge below speak about the observer's,
  // the response's accumulation about a sensor's. See `RenderFilm`.
  smdl::SpectralFilm *const film{target.spectralFilm()};
  smdl::SpectralFilm *const bandFilm{target.bandFilm()};
  const Response *const response{target.response()};
  const Color wavelengths{gRenderGrid.wavelengths()};
  const Scene &scene{*staged.scene};
  const LightSampler &lights{*staged.lights};
  const EnvLight *envLight{staged.envLight.get()};
  const std::vector<smdl::Haze> &hazes{staged.hazes};
  const smdl::Haze *haze{hazes.empty() ? nullptr : &hazes.front()};
  const smdl::JIT::MaterialDef *exteriorMediumDef{staged.exteriorMediumDef};
  const BoundBox3 &guideBound{staged.guideBound};
  const bool hasValidGuideBounds{staged.hasValidGuideBounds};
  const std::optional<Camera> &camera{frame.camera};
  const size_t numPixelsX{frame.numPixelsX};
  const size_t numPixelsY{frame.numPixelsY};
  const size_t numWindowPixels{frame.numWindowPixels};
  const int4 window{frame.window};
  const size_t spp{frame.spp};
  const bool isSavingTree{savesGuideTree(opts, frame, outputBands)};
  ProgressOptions progressOptions{opts.utility.progress};
  // How many samples per pixel trained the resumed tree, 0 without one:
  // what the pass schedule continues from.
  const size_t guideTrainedSpp{openGuideTree(opts, staged, resumed, sdtree)};
  const std::unique_ptr<PassCombiner> combiner{
      makePassCombiner(opts, frame, target, resumed)};
  // Merge a resumed session's samples in before rendering rather than
  // after it, so that the previews written along the way already stand on
  // every sample taken and the image is never displayed noisier than it
  // is. One image-level add, which is exactly the merge the
  // sums-plus-count invariant makes safe; every read below divides by the
  // combined count.
  if (resumed.wasLoaded && film && !combiner) film->add(resumed.film);
  if (resumed.wasLoaded && bandFilm) bandFilm->add(resumed.film);
  // Nothing reads it again, and it is the size of the film being rendered
  // into.
  resumed.film.clear();
  Checkpointer checkpointer{opts,           frame,  compiler,
                            resumed.header, target, combiner.get()};
  const std::vector<size_t> passes{
      solveSamplePasses(spp, opts.render.guide.isEnabled, guideTrainedSpp)};
  // The caster set the manifold gathers search, which the options below
  // point into, so it outlives every trace.
  MNEECasterSet mneeCasters{};
  const MNEEOptions mneeOptions{
      makeMNEEOptions(opts, scene, wavelengths, envLight, mneeCasters)};
  // The default walk is terminated by Russian roulette, with the bounce
  // bound set high enough that clipping it is negligible even for
  // high-albedo transport; giving -max-bounces makes the bound the whole
  // termination rule, so the estimate is the fixed-depth truncation.
  const PathOptions &pathOptions{opts.render.path};
  // What every path of this render is traced against; see
  // `RenderContext`. Built once, shared by every worker thread.
  const RenderContext render{compiler,    scene, lights,           mneeOptions,
                             pathOptions, haze,  exteriorMediumDef};
  // The tally behind -report: every block adds its own into this one at
  // the block's end, under the mutex, so the walk itself never shares
  // a write. Empty when nobody asked, and then no block tallies at all.
  std::optional<PathStats> stats;
  std::mutex statsMutex;
  if (opts.render.shouldReportStats) stats.emplace();
  // Whether every sample draws its own wavelength grid; see
  // `WavelengthGrid::bandEdges` and `jitterWavelengths()`. And whether
  // every pixel evaluates on the grid of its own tile band, in which
  // case the grid moves per pixel where the jitter moves it per sample.
  const bool shouldJitterWavelength{gRenderGrid.isJittering};
  const bool hasPixelGrids{gRenderGrid.hasTile()};
  const bool hasMovingGrid{shouldJitterWavelength || hasPixelGrids};
  // Whether every sample traces the lens at a wavelength of its own, drawn
  // from its pixel's band; see `Response::traceWavelengthAt()`. Only a
  // tile sets the range a camera needs to disperse, so there is always a
  // tile to draw from.
  const bool disperses{camera->disperses()};
  SMDL_SANITY_CHECK(!disperses || (response && response->hasTile()));
  if (disperses) response->logTracedSpans();
  // The window row length, which turns a window pixel index into a frame
  // pixel index below.
  const size_t windowWidth{size_t(window[2] - window[0])};
  // See `MAX_PIXELS_PER_BLOCK`.
  const size_t blockSize{std::clamp<size_t>(
      numWindowPixels / (BLOCKS_PER_THREAD * smdl::getThreadCount()), 1,
      MAX_PIXELS_PER_BLOCK)};
  // Progress is counted in samples rather than pixels, so that the
  // geometrically growing passes below read as one bar that only ever
  // moves forward. The counters still show pixels, which is the number a
  // person pictures. Nothing is drawn unless stderr is a terminal, where
  // the summary below takes the bar's place.
  progressOptions.total = numWindowPixels * spp;
  progressOptions.displayScale = std::max<size_t>(spp, 1);
  progressOptions.summary =
      opts.image.cropWindow.wasGiven
          ? smdl::concat("Rendered window ", spellVector(window), " of ",
                         SpellDimensions(numPixelsX, numPixelsY), " at ", spp,
                         " spp")
          : smdl::concat("Rendered ", SpellDimensions(numPixelsX, numPixelsY),
                         " at ", spp, " spp");
  // The sun-sky resolved onto the render-wide grid, which every path of
  // every block shares because the grid holds still. Read-only once the
  // threads start. A jittering render cannot use it: its grid moves with
  // every sample, so each block resolves its own below.
  smdl::SkyBasis renderSkyBasis;
  if (!hasMovingGrid && lights.env())
    lights.env()->resolve(wavelengths, renderSkyBasis);
  // The kernel: one call draws `target.spp` samples at each of its pixels
  // and feeds what it names. The render calls it a chunk at a time below,
  // and the meter calls it once first.
  // The kernel every pass below calls, a chunk of the window at a time,
  // and the metering pass calls once over its own lattice. The arguments
  // are in the order `RenderKernel` declares its members.
  const RenderKernel renderBlocks{
      opts,           render,        pathOptions,
      camera,         response,      wavelengths,
      renderSkyBasis, haze,          hazes,
      sdtree,         statsMutex,    numPixelsX,
      windowWidth,    window,        disperses,
      hasMovingGrid,  hasPixelGrids, shouldJitterWavelength};
  meterAndLogISO(frame, resumed, renderBlocks);
  // The render window the header's cumulative times measure: the sample
  // passes and the previews written between them, but none of the setup
  // that came before or the outputs that come after, so that the number
  // means the same thing in every session of a resumed sequence.
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
    std::unique_ptr<GuideAccumulator> guideAccumulator{};
    if (shouldRecordPass)
      guideAccumulator = std::make_unique<GuideAccumulator>(*sdtree);
    // Without guiding the whole budget is one pass, so checkpointing has
    // to split it; the chunk starts at one sample, so the first image
    // lands almost immediately, and then grows toward the interval asked
    // for. With guiding the passes are the chunks: they already grow
    // geometrically, and splitting one would change what the combiner
    // weights.
    const bool isChunked{checkpointer.isEnabled() && !combiner};
    for (size_t passDone{0}; passDone < thisPass;) {
      const size_t chunk{isChunked ? std::min(chunkSpp, thisPass - passDone)
                                   : thisPass - passDone};
      const size_t chunkBase{passDone};
      const auto chunkStart{std::chrono::steady_clock::now()};
      PassTarget pass{};
      pass.spp = chunk;
      pass.sampleIndexBase = resumed.sampleIndexBase + sppDone + chunkBase;
      pass.passOffset = chunkBase;
      pass.numPixels = numWindowPixels;
      pass.blockSize = blockSize;
      pass.film = film;
      pass.bandFilm = bandFilm;
      pass.combiner = combiner.get();
      pass.guideAccumulator = guideAccumulator.get();
      pass.stats = stats ? &*stats : nullptr;
      pass.progress = &progress;
      renderBlocks(pass);
      // Every pixel of the window took the same samples, so the count
      // belongs to the film rather than to each pixel, and is recorded
      // once here where the chunk is finished. It has to land before the
      // checkpoint below, which divides by it.
      if (film && !combiner) film->addSamples(chunk);
      if (bandFilm) bandFilm->addSamples(chunk);
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
            perSample > 0.0
                ? size_t(std::max(checkpointer.every() / perSample, 1.0))
                : thisPass};
        chunkSpp = std::clamp<size_t>(wanted, 1, chunk * 4);
        checkpointer.writeIfDue();
      }
    }
    if (combiner) combiner->foldPass(thisPass);
    if (shouldRecordPass) {
      guideAccumulator->absorbInto(*sdtree);
      if (combiner) combiner->rebuildPixelEstimates();
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
    if (!isFinal) checkpointer.writeIfDue();
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
  if (combiner && film) combiner->resolve(*film);
}
