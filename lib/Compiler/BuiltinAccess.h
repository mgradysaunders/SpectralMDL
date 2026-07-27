/// \file
#pragma once

#include <string_view>

#include "smdl/Support/Span.h"

namespace smdl::builtin {

/// The accessors implemented in `Context.cc`, which is the only
/// translation unit that includes the generated `Builtin.h` (it embeds
/// large constant tables that must not be duplicated).

/// Get the names of all builtin modules, e.g., `df`.
[[nodiscard]] Span<const std::string_view> getAllNames();

/// Get the source code of the builtin module with the given name, or
/// null if there is no such builtin module.
[[nodiscard]] const char *getSourceCode(std::string_view name);

} // namespace smdl::builtin
