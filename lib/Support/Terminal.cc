#include "smdl/Support/Terminal.h"

#include <cstdlib>
#include <cstring>

namespace smdl {

namespace {

// Does the environment allow colors on a terminal? See
// `shouldUseColors()`.
[[nodiscard]] bool environmentAllowsColors() noexcept {
  const char *noColor{std::getenv("NO_COLOR")};
  const char *term{std::getenv("TERM")};
  return !(noColor && *noColor) && term && *term &&
         std::strcmp(term, "dumb") != 0;
}

} // namespace

bool shouldUseColors(ANSIColorMode mode, bool isTerminal) noexcept {
  return mode == ANSIColorMode::ALWAYS ||
         (mode == ANSIColorMode::AUTO && isTerminal &&
          environmentAllowsColors());
}

} // namespace smdl
