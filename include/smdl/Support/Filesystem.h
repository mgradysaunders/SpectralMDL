/// \file
#pragma once

#include <fstream>
#include <ios>
#include <string>
#include <string_view>

#include "smdl/Export.h"

namespace smdl {

/// \addtogroup support
/// \{

/// \name Functions (filesystem)
/// \{

/// Has extension?
[[nodiscard]] SMDL_EXPORT bool
hasExtension(std::string_view path, std::string_view extension) noexcept;

/// Exists?
[[nodiscard]] SMDL_EXPORT bool exists(const std::string &path) noexcept;

/// Is file?
[[nodiscard]] SMDL_EXPORT bool isFile(const std::string &path) noexcept;

/// Is directory?
[[nodiscard]] SMDL_EXPORT bool isDirectory(const std::string &path) noexcept;

/// Is path equivalent?
[[nodiscard]]
SMDL_EXPORT bool isPathEquivalent(const std::string &path0,
                                  const std::string &path1) noexcept;

/// Is path0 a parent path of path1?
[[nodiscard]]
SMDL_EXPORT bool isParentPathOf(const std::string &path0,
                                const std::string &path1) noexcept;

/// Join paths.
[[nodiscard]] SMDL_EXPORT std::string joinPaths(std::string_view path0,
                                                std::string_view path1);

/// Expand environment variables of the form `$VAR` or `${VAR}`.
///
/// \note
/// A `$` not followed by a valid variable name is preserved verbatim.
/// Tilde expansion is not performed here, as `makePathCanonical()`
/// already handles it.
///
/// \throw Error if a referenced environment variable is undefined.
///
[[nodiscard]] SMDL_EXPORT std::string
expandPathVariables(std::string_view path);

/// Make path canonical.
///
/// \note
/// This does not throw. If the implementation fails for any reason, the input
/// path is returned unchanged.
///
[[nodiscard]] SMDL_EXPORT std::string
makePathCanonical(std::string path) noexcept;

/// Make path absolute.
///
/// \note
/// This does not throw. If the implementation fails for any reason, the input
/// path is returned unchanged.
///
[[nodiscard]] SMDL_EXPORT std::string
makePathAbsolute(std::string path) noexcept;

/// Make path relative to working directory.
///
/// \note
/// This does not throw. If the implementation fails for any reason, the input
/// path is returned unchanged.
///
[[nodiscard]] SMDL_EXPORT std::string
makePathRelative(std::string path) noexcept;

/// Convert to the best path for printing: relative path, absolute path, or
/// the input path unchanged, whichever is shortest.
///
/// \note
/// This does not throw. If the implementation fails for any reason, the input
/// path is returned unchanged.
///
[[nodiscard]] SMDL_EXPORT std::string
bestPathForPrinting(std::string path) noexcept;

/// Determine parent path.
///
/// \note
/// This does not throw. If the implementation fails for any reason, the input
/// path is returned unchanged.
///
[[nodiscard]] SMDL_EXPORT std::string parentPathOf(std::string path) noexcept;

/// Rename `from` onto `to`, replacing whatever `to` held.
///
/// This is the second half of the write-through-a-temporary discipline a
/// file that something else may be reading has to follow: write to
/// `to + ".part"`, then rename. A reader polling `to` never sees a
/// partial file, and an interrupted write cannot destroy what `to`
/// already held, which matters most when `to` is the very file the
/// writer is resuming from.
///
/// \throws Error if the rename fails.
SMDL_EXPORT void renameOnto(const std::string &from, const std::string &to);

/// `renameOnto()` reporting failure by return value, for a caller whose
/// file is not worth failing over: a progress line a tool polls is
/// better stale than fatal.
SMDL_EXPORT bool tryRenameOnto(const std::string &from,
                               const std::string &to) noexcept;

/// Open file or throw an `Error`.
[[nodiscard]] SMDL_EXPORT std::fstream openOrThrow(const std::string &path,
                                                   std::ios::openmode mode);

/// Read file or throw an `Error`.
[[nodiscard]] SMDL_EXPORT std::string readOrThrow(const std::string &path);

/// \}

/// \}

} // namespace smdl
