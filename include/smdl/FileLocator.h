/// \file
#pragma once

#include "smdl/Common.h"

namespace smdl {

/// \addtogroup scene
/// \{

/// The file locator.
class SMDL_EXPORT FileLocator final {
public:
  FileLocator() = default;

public:
  /// Set whether or not the locator should search the present working
  /// directory.
  void setSearchPwd(bool yes) { mSearchPwd = yes; }

  /// Add search directory.
  ///
  /// \param[in] dir
  /// The directory path.
  ///
  /// \param[in] isRecursive
  /// Search recursively?
  ///
  /// \return
  /// Returns true if successful, and false if the given `dir`
  /// did not resolve to a valid path.
  ///
  bool addSearchDir(const std::string &dir, bool isRecursive = false) {
    if (isDirectory(dir)) {
      mSearchDirs.emplace_back(SearchDir{makePathCanonical(dir), isRecursive});
      return true;
    }
    return false;
  }

  /// Get all active search directories.
  ///
  /// \param[in] relativeTo
  /// If not empty, understood to be the most relevant file or
  /// directory path to search relative to. This is useful for
  /// resolving files included by other files.
  ///
  /// \param[in] priorityDirs
  /// If not empty, directory paths searched before everything else,
  /// in order. This is used for the `#search_dir` declarations of the
  /// module a resource is referenced by.
  ///
  /// \return
  /// A vector of valid canonical directory paths, in the order
  /// in which the directories were initially added, with redundant
  /// paths removed.
  ///
  [[nodiscard]] std::vector<std::string>
  getSearchDirs(std::string_view relativeTo = {},
                Span<const std::string> priorityDirs = {}) const;

  /// The bitwise-or of the flags that select which kinds of paths
  /// `locate()` is allowed to return.
  using LocateFlags = uint32_t;

  /// Flag to consider paths to regular files.
  static constexpr LocateFlags REGULAR_FILES = 0x1;

  /// Flag to consider paths to directories.
  static constexpr LocateFlags DIRS = 0x2;

  /// Attempt to locate a file subject to all active search directories.
  ///
  /// \param[in] fileName
  /// The file name to locate.
  ///
  /// \param[in] relativeTo
  /// If not empty, understood to be the most relevant file or
  /// directory path to search relative to. This is useful for
  /// resolving files included by other files.
  ///
  /// \param[in] flags
  /// The bitwise-or of `REGULAR_FILES` and `DIRS` selecting which kinds
  /// of paths may be returned.
  ///
  /// \param[in] priorityDirs
  /// If not empty, directory paths searched before everything else,
  /// in order. See `getSearchDirs()`.
  ///
  /// \return
  /// The canonical path of the located file, or `std::nullopt` if
  /// not found.
  ///
  [[nodiscard]] std::optional<std::string>
  locate(std::string_view fileName, std::string_view relativeTo = {},
         LocateFlags flags = REGULAR_FILES,
         Span<const std::string> priorityDirs = {}) const;

  /// An image path with parsed tile indexes in U and V.
  class SMDL_EXPORT ImagePath final {
  public:
    [[nodiscard]] bool operator==(const ImagePath &other) const {
      return tileIndexU == other.tileIndexU && tileIndexV == other.tileIndexV &&
             path == other.path;
    }

    [[nodiscard]] bool operator!=(const ImagePath &other) const {
      return !operator==(other);
    }

    [[nodiscard]] bool operator<(const ImagePath &other) const {
      return std::make_tuple(tileIndexU, tileIndexV, path) <
             std::make_tuple(other.tileIndexU, other.tileIndexV, other.path);
    }

  public:
    /// The tile index in U.
    uint32_t tileIndexU{};

    /// The tile index in V.
    uint32_t tileIndexV{};

    /// The file path.
    std::string path;
  };

  /// Attempt to locate one or more images subject to all active search
  /// directories, with support for the tile markers `<UDIM>`, `<UVTILE0>`,
  /// and `<UVTILE1>` from the MDL specification.
  ///
  /// \param[in] fileName
  /// The file name to locate, which may contain a tile marker in its
  /// final path component.
  ///
  /// \param[in] relativeTo
  /// If not empty, understood to be the most relevant file or
  /// directory path to search relative to. This is useful for
  /// resolving files included by other files.
  ///
  /// If `fileName` contains no tile marker, this behaves like `locate()`:
  /// the result is either empty or contains exactly one `ImagePath` with
  /// tile indexes `(0, 0)`.
  ///
  /// Otherwise, the final path component of `fileName` is understood
  /// as a pattern of the form `prefix<MARKER>suffix`, split at the
  /// last occurrence of the marker. If more than one kind of marker
  /// is present, `<UDIM>` takes precedence over `<UVTILE0>`, which
  /// takes precedence over `<UVTILE1>`. If `fileName` is absolute,
  /// the only candidate directory is its parent directory. If
  /// `fileName` is relative, there is one candidate directory per
  /// active search directory returned by `getSearchDirs()`, formed
  /// by joining the search directory with the directory portion of
  /// `fileName`, if any. The candidate directories are scanned in
  /// order, and the first directory that contains at least one match
  /// provides all of the results, which guarantees that every tile
  /// resides in the same directory. A file matches if its name is
  /// exactly `prefix`, then a marker expansion, then `suffix`, where
  /// the marker expands as follows:
  /// - `<UDIM>` expands to exactly four decimal digits understood as
  ///   the number `1001 + U + 10 * V` for tile indexes `U` in
  ///   `[0, 10)` and `V` in `[0, 900)`, i.e., `1001` up to `9999`.
  /// - `<UVTILE0>` expands to `_u<U>_v<V>` for zero-based tile
  ///   indexes, so the first tile is `_u0_v0`.
  /// - `<UVTILE1>` expands to `_u<U>_v<V>` for one-based tile
  ///   indexes, so the first tile is `_u1_v1`. The returned tile
  ///   indexes are normalized to be zero-based, so `_u1_v1` yields
  ///   tile indexes `(0, 0)`.
  ///
  /// \note
  /// As a sanity check against absurd allocations downstream, tile
  /// indexes parsed from `<UVTILE0>` and `<UVTILE1>` names must be
  /// less than `1000` in each axis after normalization, or else the
  /// file is ignored. Negative tile indexes are not supported.
  ///
  /// \return
  /// The canonical paths and parsed tile indexes of all matched
  /// images, sorted by tile index in U, then tile index in V, then
  /// by path. If nothing matches, the vector is empty.
  ///
  [[nodiscard]] std::vector<ImagePath>
  locateImages(std::string_view fileName, std::string_view relativeTo = {},
               Span<const std::string> priorityDirs = {}) const;

private:
  /// Always search the present working directory?
  bool mSearchPwd{true};

  /// A search directory.
  class SearchDir final {
  public:
    /// The canonical directory path.
    std::string dir;

    /// Search recursively?
    bool isRecursive{};
  };

  /// The search directories.
  std::vector<SearchDir> mSearchDirs;
};

/// \}

} // namespace smdl
