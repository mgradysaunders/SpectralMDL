/// \file
#pragma once

#include "smdl/Export.h"
#include "smdl/Support/MacroHelpers.h"

namespace smdl {

/// \addtogroup Support
/// \{

/// \name Functions (profiler)
/// \{

SMDL_EXPORT void profiler_initialize(unsigned granularityMicroseconds = 100,
                                     const char *processName = "");

struct ProfilerEntry;

[[nodiscard]]
SMDL_EXPORT ProfilerEntry *profiler_entry_begin(const char *name,
                                                const char *detail = "");

SMDL_EXPORT void profiler_entry_end(ProfilerEntry *entry);

SMDL_EXPORT void
profiler_finalize(const char *outputFilename = "profiler_output.json");

/// \}

class ProfilerEntryScope final {
public:
  ProfilerEntryScope(const char *name, const char *detail = "")
      : entry(profiler_entry_begin(name, detail)) {}

  ProfilerEntryScope(const ProfilerEntryScope &) = delete;

  ~ProfilerEntryScope() { profiler_entry_end(entry); }

private:
  ProfilerEntry *entry{};
};

#define SMDL_PROFILER_ENTRY(...)                                               \
  const auto SMDL_CAT(__profilerEntry, __LINE__) =                             \
      ::smdl::ProfilerEntryScope(__VA_ARGS__)

/// \}

} // namespace smdl
