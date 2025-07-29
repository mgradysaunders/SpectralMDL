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
has_extension(std::string_view path, std::string_view extension) noexcept;

/// Exists?
[[nodiscard]] SMDL_EXPORT bool exists(const std::string &path) noexcept;

/// Is file?
[[nodiscard]] SMDL_EXPORT bool is_file(const std::string &path) noexcept;

/// Is directory?
[[nodiscard]] SMDL_EXPORT bool is_directory(const std::string &path) noexcept;

/// Is path equivalent?
[[nodiscard]]
SMDL_EXPORT bool is_path_equivalent(const std::string &path0,
                                    const std::string &path1) noexcept;

/// Is path0 a parent path of path1?
[[nodiscard]]
SMDL_EXPORT bool is_parent_path_of(const std::string &path0,
                                   const std::string &path1) noexcept;

/// Join paths.
[[nodiscard]] SMDL_EXPORT std::string join_paths(std::string_view path0,
                                                 std::string_view path1);

/// Make path canonical.
///
/// \note
/// This does not throw. If the implementation fails for any reason, the input
/// path is returned unchanged.
///
[[nodiscard]] SMDL_EXPORT std::string canonical(std::string path) noexcept;

/// Make path relative to working directory.
///
/// \note
/// This does not throw. If the implementation fails for any reason, the input
/// path is returned unchanged.
///
[[nodiscard]] SMDL_EXPORT std::string relative(std::string path) noexcept;

/// Determine parent path.
///
/// \note
/// This does not throw. If the implementation fails for any reason, the input
/// path is returned unchanged.
///
[[nodiscard]] SMDL_EXPORT std::string parent_path(std::string path) noexcept;

/// Open file or throw an `Error`.
[[nodiscard]] SMDL_EXPORT std::fstream open_or_throw(const std::string &path,
                                                     std::ios::openmode mode);

/// Read file or throw an `Error`.
[[nodiscard]] SMDL_EXPORT std::string read_or_throw(const std::string &path);

/// \}

/// \}

} // namespace smdl
