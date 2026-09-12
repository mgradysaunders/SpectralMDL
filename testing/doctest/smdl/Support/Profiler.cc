#include "CompileFixtures.h"

#include "smdl/Support/Profiler.h"

TEST_CASE("Profiler: a trace that cannot be written") {
  TempDir tmpDir{"profiler"};
  const std::string fileName{(tmpDir / "missing" / "trace.json").string()};
  const CollectedLog logged{"write profiler time-trace file"};
  smdl::profilerInitialize();
  smdl::profilerFinalize(fileName.c_str());
  // Said once, and with the reason, which is also what consumes the error
  // LLVM hands back rather than leaving it to abort a checked build.
  REQUIRE(logged.messages().size() == 1);
  CHECK_CONTAINS(logged.messages()[0], "trace.json': ");
}
