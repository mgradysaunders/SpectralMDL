#include "smdl/Support/Profiler.h"
#include "smdl/Support/Logger.h"
#include "smdl/Support/StringHelpers.h"

#include "llvm/Support/TimeProfiler.h"

namespace smdl {

static bool isProfilerRunning{};

void profilerInitialize(unsigned granularityMicroseconds,
                        const char *processName) {
  SMDL_SANITY_CHECK(!isProfilerRunning,
                    "Must only call profiler_initialize() once");
  llvm::timeTraceProfilerInitialize(granularityMicroseconds, processName);
  isProfilerRunning = true;
}

ProfilerEntry *profilerEntryBegin(const char *name, const char *detail) {
  return reinterpret_cast<ProfilerEntry *>(
      llvm::timeTraceProfilerBegin(name, detail));
}

void profilerEntryEnd(ProfilerEntry *entry) {
  llvm::timeTraceProfilerEnd(
      reinterpret_cast<llvm::TimeTraceProfilerEntry *>(entry));
}

void profilerFinalize(const char *outputFilename) {
  SMDL_SANITY_CHECK(isProfilerRunning,
                    "Must only call profiler_finalize() if initialized");
  auto error{llvm::timeTraceProfilerWrite(outputFilename, "-")};
  if (error) {
    SMDL_LOG_ERROR("cannot write profiler time-trace file ",
                   QuotedPath(outputFilename));
  }
  llvm::timeTraceProfilerCleanup();
  isProfilerRunning = false;
}

} // namespace smdl
