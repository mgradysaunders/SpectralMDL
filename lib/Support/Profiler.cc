#include "smdl/Support/Profiler.h"
#include "smdl/Support/Logger.h"
#include "smdl/Support/Strings.h"

#include "llvm/Support/TimeProfiler.h"

namespace smdl {

namespace {
bool isProfilerRunning{};
} // namespace

void profilerInitialize(unsigned granularityMicroseconds,
                        const char *processName) {
  SMDL_SANITY_CHECK_MSG(!isProfilerRunning,
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
  SMDL_SANITY_CHECK_MSG(isProfilerRunning,
                        "Must only call profiler_finalize() if initialized");
  if (llvm::Error error{llvm::timeTraceProfilerWrite(outputFilename, "-")}) {
    // Consumed out here, since the log macro only evaluates its arguments
    // when the message is enabled.
    const std::string reason{llvm::toString(std::move(error))};
    SMDL_LOG_ERROR("cannot write profiler time-trace file ",
                   QuotedPath(outputFilename), ": ", reason);
  }
  llvm::timeTraceProfilerCleanup();
  isProfilerRunning = false;
}

} // namespace smdl
