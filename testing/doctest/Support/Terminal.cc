#include "Fixtures.h"

#include <string>

#include "smdl/Support/Terminal.h"

namespace {

using smdl::ANSIColorMode;

// The two variables `shouldUseColors()` reads, pinned for a scope. An empty
// value counts as unset.
class ScopedColorEnv final {
public:
  ScopedColorEnv(const std::string &term, const std::string &noColor)
      : mTerm("TERM", term), mNoColor("NO_COLOR", noColor) {}

private:
  ScopedEnv mTerm;

  ScopedEnv mNoColor;
};

} // namespace

TEST_CASE("shouldUseColors: what a color mode means for a stream") {
  SUBCASE("Always and never ignore the terminal and the environment") {
    for (const char *term : {"xterm-256color", "dumb", ""}) {
      for (const char *noColor : {"", "1"}) {
        const ScopedColorEnv env{term, noColor};
        for (const bool isTerminal : {false, true}) {
          CHECK(smdl::shouldUseColors(ANSIColorMode::ALWAYS, isTerminal));
          CHECK_FALSE(smdl::shouldUseColors(ANSIColorMode::NEVER, isTerminal));
        }
      }
    }
  }
  SUBCASE("Auto needs a terminal") {
    const ScopedColorEnv env{"xterm-256color", ""};
    CHECK(smdl::shouldUseColors(ANSIColorMode::AUTO, true));
    CHECK_FALSE(smdl::shouldUseColors(ANSIColorMode::AUTO, false));
  }
  SUBCASE("Auto honors NO_COLOR whatever its value, unless it is empty") {
    for (const char *noColor : {"1", "0", "false"}) {
      const ScopedColorEnv env{"xterm-256color", noColor};
      CHECK_FALSE_MESSAGE(smdl::shouldUseColors(ANSIColorMode::AUTO, true),
                          noColor);
    }
  }
  SUBCASE("Auto wants a TERM that is set and is not dumb") {
    for (const char *term : {"dumb", ""}) {
      const ScopedColorEnv env{term, ""};
      CHECK_FALSE_MESSAGE(smdl::shouldUseColors(ANSIColorMode::AUTO, true),
                          term);
    }
  }
}
