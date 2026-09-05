// vim:foldmethod=marker:foldlevel=0:fmr=--{,--}
#include <cstdlib>
#include <iostream>
#include <memory>
#include <string>

#include "CommandLine.h"
#include "llvm/Support/InitLLVM.h"

#include "IO/PlacesFile.h"
#include "Layout/LayoutTables.h"
#include "Options.h"
#include "Output.h"
#include "Progress.h"
#include "Render.h"
#include "Render/Guiding.h"
#include "Render/Manifold.h"
#include "Resume.h"
#include "Stage.h"

#include "smdl/Compiler.h"
#include "smdl/RenderUtil/SpectralFilm.h"
#include "smdl/Support/Error.h"
#include "smdl/Support/Logger.h"
#include "smdl/Support/Parallel.h"
#include "smdl/Support/Profiler.h"

int main(int argc, char **argv) try {
  llvm::InitLLVM X(argc, argv);
  // Prints exactly like 'print_to_cerr', except that it knows to step
  // around a progress bar while one is on screen.
  smdl::Logger::get().addSink<ProgressLogSink>();
  const auto opts{parseCommandLine(argc, argv)};
  // Before anything parallel: the thread pool is built by whichever
  // parallel operation runs first (the compile's image loads, usually)
  // and cannot be resized afterward. Embree keeps its own pool for
  // building acceleration structures, and `Scene` bounds that one from
  // `smdl::getThreadCount()`.
  smdl::setThreadCount(opts.sampling.threads);
  // The '.places' utilities bow out before anything else: they touch
  // nothing but the named files.
  if (!opts.utility.dumpPlaces.empty()) {
    dumpPlaces(opts.utility.dumpPlaces);
    return EXIT_SUCCESS;
  }
  if (!opts.utility.dumpCurves.empty()) {
    dumpCurves(opts.utility.dumpCurves);
    return EXIT_SUCCESS;
  }
  if (!opts.utility.packPlaces.empty()) {
    packPlaces(opts.utility.packPlaces, opts.utility.outputPlaces);
    return EXIT_SUCCESS;
  }
  // The positional scene argument is required for everything that
  // remains; see the note on its declaration.
  if (opts.scene.inputSceneFile.empty())
    throw smdl::Error("expected an <input scene> argument");
  // The profiler covers everything from here to just before the render
  // loop: layout parsing, MDL compilation, scene import, and the
  // acceleration structures. The library's own entries (module parse, IR
  // emission, LLVM optimization, image loads) only record once this is
  // initialized. NOTE: The LLVM time-trace instance is thread-local, so
  // entries are only ever begun on this thread; parallel work is timed by
  // hand and reported through logging instead.
  const bool profiling{opts.output.profiling};
  const auto profileFileName{opts.output.profile.empty()
                                 ? std::string("smdl-toy.trace.json")
                                 : opts.output.profile};
  if (profiling) smdl::profilerInitialize();
  auto frame{resolveFrame(opts)};
  auto resumed{resumeSequence(opts, frame.resolution, frame.window)};
  const auto grid{resolveWavelengthGrid(opts, frame, resumed)};
  // The compiler outlives every render below it, because the JIT'd
  // material code embeds absolute pointers into the data it owns.
  auto compiler{smdl::Compiler{}};
  setUpCompiler(opts, frame, grid, compiler);
  if (opts.utility.listObjects) {
    if (opts.utility.json) {
      printObjectTableJSON(frame.layout);
    } else {
      printObjectTable(frame.layout);
    }
    if (profiling) smdl::profilerFinalize(profileFileName.c_str());
    return EXIT_SUCCESS;
  } else if (opts.utility.listMaterials) {
    // The table reports how every name resolves, so it must see the
    // unfiltered material list; and it never calls JIT'd code, so
    // compile() alone is enough.
    const smdl::Compiler *compilerOrNull{};
    if (!opts.scene.inputMDLFiles.empty()) {
      if (auto error{compiler.compile(smdl::OPT_LEVEL_O2)})
        error->printAndExit();
      compilerOrNull = &compiler;
    }
    if (opts.utility.json) {
      printMaterialTableJSON(compilerOrNull, frame.layout);
    } else {
      printMaterialTable(compilerOrNull, frame.layout);
    }
    if (profiling) smdl::profilerFinalize(profileFileName.c_str());
    return EXIT_SUCCESS;
  }
  StagedScene staged{opts, frame, grid, compiler};
  // The self-test bows out here rather than after the render setup: it
  // asks the committed scene one question and answers it.
  if (opts.mneeTestNormalHook) {
    std::cout << "Checking the geometry-normal hook against the meshes:\n";
    const int failures{runMNEETestNormalHook(*staged.scene)};
    if (failures == 0)
      std::cout << "All unmapped instances agree\n";
    else
      std::cout << failures << " instance(s) disagree\n";
    return failures == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
  }
  // The render loop is deliberately outside the trace; see -profile.
  if (profiling) smdl::profilerFinalize(profileFileName.c_str());
  auto film{smdl::SpectralFilm(grid.wavelengths.size(), frame.numPixelsX,
                               frame.numPixelsY)};
  // -resume implies writing back to the file being resumed, so one
  // command line re-runs to keep accumulating; an explicitly given
  // -output-spectrum wins verbatim, redirecting or (when empty)
  // suppressing the write.
  const auto outputSpectrum{opts.output.spectrumGiven || !resumed.requested
                                ? opts.output.spectrum
                                : opts.output.resume};
  auto sdtree{std::unique_ptr<STree>()};
  renderSamples(opts, frame, grid, compiler, staged, resumed, film,
                outputSpectrum, sdtree);
  writeOutputs(opts, frame, grid, compiler, film, resumed, outputSpectrum,
               savesGuideTree(opts, frame, outputSpectrum) ? sdtree.get()
                                                           : nullptr);
  return EXIT_SUCCESS;
} catch (const smdl::Error &error) {
  error.print();
  return EXIT_FAILURE;
} catch (const std::exception &error) {
  std::cerr << error.what() << '\n';
  return EXIT_FAILURE;
}
