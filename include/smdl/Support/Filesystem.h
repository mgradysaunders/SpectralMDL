/// \file
#pragma once

#include <fstream>
#include <ios>
#include <string>
#include <string_view>

#include "smdl/Export.h"

namespace smdl {

/// \addtogroup Support
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

/// Make path canonical.
///
/// \note
/// This does not throw. If the implementation fails for any reason, the input
/// path is returned unchanged.
///
[[nodiscard]] SMDL_EXPORT std::string makePathCanonical(std::string path) noexcept;

/// Make path relative to working directory.
///
/// \note
/// This does not throw. If the implementation fails for any reason, the input
/// path is returned unchanged.
///
[[nodiscard]] SMDL_EXPORT std::string makePathRelative(std::string path) noexcept;

/// Determine parent path.
///
/// \note
/// This does not throw. If the implementation fails for any reason, the input
/// path is returned unchanged.
///
[[nodiscard]] SMDL_EXPORT std::string parentPathOf(std::string path) noexcept;

/// Open file or throw an `Error`.
[[nodiscard]] SMDL_EXPORT std::fstream openOrThrow(const std::string &path,
                                                   std::ios::openmode mode);

/// Read file or throw an `Error`.
[[nodiscard]] SMDL_EXPORT std::string readOrThrow(const std::string &path);

/// \}

/// \}

} // namespace smdl
