#include "smdl/Support/Parallel.h"
#include "smdl/Support/Logger.h"

#include "llvm/Support/Parallel.h"

#ifndef SMDL_DYNAMIC_SCHEDULING
#define SMDL_DYNAMIC_SCHEDULING 0
#endif // #ifndef SMDL_DYNAMIC_SCHEDULING

#if SMDL_DYNAMIC_SCHEDULING
#include <algorithm>
#include <atomic>
#endif // #if SMDL_DYNAMIC_SCHEDULING

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

#if SMDL_DYNAMIC_SCHEDULING

// How many chunks each thread is expected to get through, which is what
// sets the grain below. Both ends of the range bite: too few chunks and
// the idle tail is a large share of one thread's work, too many and the
// shared counter becomes the bottleneck. Measured on 12 threads against a
// cost field where 2 percent of the rows cost 400 times the rest, 16
// chunks per thread balances no better than 36 percent and 256 reaches
// 99; raising it to 1024 buys a few points more on small ranges and costs
// three to four times the counter traffic.
static constexpr size_t CHUNKS_PER_THREAD = 256;

#endif // #if SMDL_DYNAMIC_SCHEDULING

void parallelFor(size_t begin, size_t end,
                 const std::function<void(size_t)> &func) {
  hasRunParallelWork = true;
#if SMDL_DYNAMIC_SCHEDULING
  if (begin >= end) return;
  const size_t numThreads{getThreadCount()};
  const size_t grain{
      std::max<size_t>(1, (end - begin) / (CHUNKS_PER_THREAD * numThreads))};
  if (numThreads <= 1 || end - begin <= grain) {
    for (size_t i = begin; i < end; i++) func(i);
    return;
  }
  // One task per thread, each taking 'grain' indices at a time off the
  // shared counter, so that the number of chunks tracks the thread count
  // rather than being capped and the work lands where a thread is free to
  // take it. The counter carries no data between threads, so the ordering
  // can be relaxed; the join is what publishes the results.
  //
  // The nesting rule survives: spawned from inside another parallel loop
  // the tasks below run inline, the first drains the counter, and the
  // rest find it empty.
  std::atomic<size_t> next{begin};
  llvm::parallelFor(0, numThreads, [&](size_t) {
    while (true) {
      const size_t first{next.fetch_add(grain, std::memory_order_relaxed)};
      if (first >= end) return;
      for (size_t i = first, last = std::min(first + grain, end); i < last; i++)
        func(i);
    }
  });
#else
  llvm::parallelFor(begin, end, func);
#endif // #if SMDL_DYNAMIC_SCHEDULING
}

} // namespace smdl
