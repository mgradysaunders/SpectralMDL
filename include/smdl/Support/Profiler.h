/// \file
#pragma once

#include "smdl/Export.h"
#include "smdl/Support/MacroHelpers.h"

namespace smdl {

/// \addtogroup Support
/// \{

/// \name Functions (profiler)
/// \{

SMDL_EXPORT void profilerInitialize(unsigned granularityMicroseconds = 100,
                                    const char *processName = "");

struct ProfilerEntry;

[[nodiscard]]
SMDL_EXPORT ProfilerEntry *profilerEntryBegin(const char *name,
                                              const char *detail = "");

SMDL_EXPORT void profilerEntryEnd(ProfilerEntry *entry);

SMDL_EXPORT void
profilerFinalize(const char *outputFilename = "profiler_output.json");

/// \}

class ProfilerEntryScope final {
public:
  ProfilerEntryScope(const char *name, const char *detail = "")
      : entry(profilerEntryBegin(name, detail)) {}

  ProfilerEntryScope(const ProfilerEntryScope &) = delete;

  ~ProfilerEntryScope() { profilerEntryEnd(entry); }

private:
  ProfilerEntry *entry{};
};

#define SMDL_PROFILER_ENTRY(...)                                               \
  const auto SMDL_CAT(__profilerEntry, __LINE__) =                             \
      ::smdl::ProfilerEntryScope(__VA_ARGS__)

/// \}

} // namespace smdl
