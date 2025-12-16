/// \file
#pragma once

#include "smdl/Common.h"

namespace smdl {

/// \addtogroup Main
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
  /// \returns
  /// Returns true if successful, and false if the given `dirPath`
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
  /// \returns
  /// A vector of valid canonical directory paths, in the order
  /// in which the directories were initially added, with redundant
  /// paths removed.
  ///
  [[nodiscard]] std::vector<std::string>
  getSearchDirs(std::string_view relativeTo = {}) const;

  typedef uint32_t LocateFlags;

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
  /// \returns
  /// The canonical path of the located file, or `std::nullopt` if
  /// not found.
  ///
  [[nodiscard]] std::optional<std::string>
  locate(std::string_view fileName, std::string_view relativeTo = {},
         LocateFlags flags = REGULAR_FILES) const;

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

  // TODO Document
  /// Locate images according to the MDL specification for UV tile filenames.
  [[nodiscard]] std::vector<ImagePath>
  locateImages(std::string_view fileName,
               std::string_view relativeTo = {}) const;

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
