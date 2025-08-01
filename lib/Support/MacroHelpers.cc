#include "smdl/Support/MacroHelpers.h"

#include <string>

#include "llvm/Support/Error.h"

namespace smdl {

namespace detail {

void sanity_check_failed(const char *condition, const char *file, int line,
                         const char *more) {
  std::string message{"Sanity check failed! "};
  message += condition, message += '\n';
  message += "  File = ", message += file, message += '\n';
  message += "  Line = ", message += std::to_string(line), message += '\n';
  if (more) {
    message += "\n...\n\n";
    message += more;
    message += "\n";
  }
  llvm::report_fatal_error(message.c_str());
}

} // namespace detail

} // namespace smdl
