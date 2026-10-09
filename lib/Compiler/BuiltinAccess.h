/// \file
#pragma once

#include <string>
#include <string_view>

#include "smdl/Support/Span.h"

namespace smdl::builtin {

/// The accessors implemented in `Context.cc`, which is the only
/// translation unit that includes the generated `Builtin.h` (it embeds
/// large constant tables that must not be duplicated).

/// Get the names of all builtin modules, e.g., `df`.
[[nodiscard]] Span<const std::string_view> getAllNames();

/// Get the source code of the builtin module with the given name,
/// decompressed from the representation embedded in the binary, or
/// the empty string if there is no such builtin module.
[[nodiscard]] std::string getSourceCode(std::string_view name);

} // namespace smdl::builtin
