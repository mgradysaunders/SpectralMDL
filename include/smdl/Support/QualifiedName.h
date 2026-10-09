/// \file
#pragma once

#include <string>
#include <string_view>
#include <vector>

#include "smdl/Export.h"
#include "smdl/Support/Span.h"

namespace smdl {

/// \addtogroup support
/// \{

/// Split a qualified name into its `::`-delimited components.
///
/// A leading `::` is ignored, so `"::a::b"` and `"a::b"` both split
/// into `{"a", "b"}`. The returned views point into the given
/// `qualifiedName`.
[[nodiscard]] SMDL_EXPORT std::vector<std::string_view>
splitQualifiedName(std::string_view qualifiedName);

/// Join components into an absolute qualified name.
///
/// For example, `{"a", "b"}` joins into `"::a::b"`. Joining no
/// components yields the empty string.
[[nodiscard]] SMDL_EXPORT std::string
joinQualifiedName(Span<const std::string_view> components);

/// Is `name` a suffix of `qualifiedName` on `::` component boundaries?
///
/// For example, `"steel::brushed"` and `"brushed"` are suffixes of
/// `"::vendor::metals::steel::brushed"`, but `"shed"` is not. A
/// leading `::` on either argument is ignored, and an empty `name`
/// is never a suffix.
[[nodiscard]] SMDL_EXPORT bool
isQualifiedNameSuffix(std::string_view name, std::string_view qualifiedName);

/// \}

} // namespace smdl
