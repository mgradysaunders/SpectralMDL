#include <cstdlib>
#include <iostream>

#include "llvm/Support/InitLLVM.h"

#include "smdl/Compiler.h"
#include "smdl/Support/Error.h"
#include "smdl/Support/Logger.h"
#include "smdl/Support/Parallel.h"
#include "smdl/Support/Profiler.h"

#include "Doc.h"
#include "Execs.h"
#include "Options.h"
#include "Session.h"
#include "Volume.h"

int main(int argc, char **argv) try {
  llvm::InitLLVM X(argc, argv);
  auto &logSink{smdl::Logger::get().addSink<smdl::LogSinks::PrintToCerr>()};
  const auto opts{parseCommandLine(argc, argv)};
  // Before anything is logged: the sink above is already in place, and
  // the parse itself says nothing, so this is the first point at which a
  // message could be filtered and labeled as asked, and the last at which
  // nothing has been missed.
  smdl::Logger::get().setMinLevel(opts.utility.logLevel);
  logSink.setUnicodeMode(opts.utility.unicodeMode);
  logSink.setColorMode(opts.utility.ansiColorMode);
  // Before anything parallel: the thread pool is built by whichever
  // parallel operation runs first (the compile's image loads, usually)
  // and cannot be resized afterward.
  smdl::setThreadCount(opts.utility.threads);
  // The voxel grids bow out before the compiler, which would reject one
  // as a source file and which this subcommand has no use for anyway.
  if (opts.subcommand == Subcommand::VOLUME) {
    runVolume(opts);
    return EXIT_SUCCESS;
  }
  if (opts.utility.isProfiling) smdl::profilerInitialize();
  // The compiler outlives every use of the code it emits, because the
  // JIT'd material code embeds absolute pointers into the data it owns.
  auto compiler{smdl::Compiler{}};
  setUpCompiler(opts, compiler);
  switch (opts.subcommand) {
  case Subcommand::DUMP:
    runDump(opts, compiler);
    break;
  case Subcommand::LIST:
    runList(opts, compiler);
    break;
  case Subcommand::FORMAT:
    runFormat(opts, compiler);
    break;
  case Subcommand::DOC:
    runDoc(opts, compiler);
    break;
  case Subcommand::RUN:
    runExecs(opts, compiler);
    break;
  case Subcommand::TEST:
    runExecs(opts, compiler);
    runUnitTests(opts, compiler);
    break;
  case Subcommand::VOLUME:
    break; // Handled above
  }
  if (opts.utility.isProfiling)
    smdl::profilerFinalize(opts.utility.profile.c_str());
  return EXIT_SUCCESS;
} catch (const smdl::Error &error) {
  error.print();
  return EXIT_FAILURE;
} catch (const std::exception &error) {
  std::cerr << error.what() << '\n';
  return EXIT_FAILURE;
}
