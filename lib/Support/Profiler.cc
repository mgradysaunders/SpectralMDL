#include "smdl/Support/Profiler.h"
#include "smdl/Support/Logger.h"
#include "smdl/Support/StringHelpers.h"

#include "llvm/Support/TimeProfiler.h"

namespace smdl {

static bool isProfilerRunning{};

void profiler_initialize(unsigned granularityMicroseconds,
                         const char *processName) {
  SMDL_SANITY_CHECK(!isProfilerRunning,
                    "Must only call profiler_initialize() once");
  llvm::timeTraceProfilerInitialize(granularityMicroseconds, processName);
  isProfilerRunning = true;
}

ProfilerEntry *profiler_entry_begin(const char *name, const char *detail) {
  return reinterpret_cast<ProfilerEntry *>(
      llvm::timeTraceProfilerBegin(name, detail));
}

void profiler_entry_end(ProfilerEntry *entry) {
  llvm::timeTraceProfilerEnd(
      reinterpret_cast<llvm::TimeTraceProfilerEntry *>(entry));
}

void profiler_finalize(const char *outputFilename) {
  SMDL_SANITY_CHECK(isProfilerRunning,
                    "Must only call profiler_finalize() if initialized");
  auto error{llvm::timeTraceProfilerWrite(outputFilename, "-")};
  if (error) {
    SMDL_LOG_ERROR("cannot write profiler time-trace file ",
                   quoted_path(outputFilename));
  }
  llvm::timeTraceProfilerCleanup();
  isProfilerRunning = false;
}

} // namespace smdl
