#include "smdl/Support/Parallel.h"
#include "smdl/Support/Logger.h"

#include "llvm/Support/Parallel.h"

namespace smdl {

// Set by the first 'parallelFor()', by which point LLVM has built the
// thread pool and the strategy no longer has any effect. This only sees
// the calls that come through here, which is everything in the library
// and in the programs that ship with it, but not a host that reaches for
// 'llvm::parallelFor' itself.
static bool hasRunParallelWork{};

void setThreadCount(unsigned numThreads) {
  if (hasRunParallelWork) {
    SMDL_LOG_WARN("ignoring setThreadCount(", numThreads,
                  "): parallel work has already started");
    return;
  }
  llvm::parallel::strategy.ThreadsRequested = numThreads;
  // Clamp to the hardware thread count, so that an absurd request cannot
  // spawn an absurd number of threads.
  llvm::parallel::strategy.Limit = true;
}

unsigned getThreadCount() {
  // Deliberately not 'llvm::parallel::getThreadCount()', which asks the
  // pool and thereby creates it.
  return llvm::parallel::strategy.compute_thread_count();
}

void parallelFor(size_t begin, size_t end,
                 const std::function<void(size_t)> &func) {
  hasRunParallelWork = true;
  llvm::parallelFor(begin, end, func);
}

} // namespace smdl
