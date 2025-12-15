#include "smdl/FileLocator.h"

#include <filesystem>
#include <set>

#include "llvm/ADT/StringSet.h"

namespace smdl {

std::vector<std::string>
FileLocator::get_search_dirs(std::string_view relativeTo) const {
  auto results{std::vector<std::string>()};
  auto resultSet{llvm::StringSet()};
  auto add{[&](std::string dir) {
    if (auto [itr, inserted] = resultSet.insert(dir); inserted) {
      results.push_back(std::move(dir));
    }
  }};
  if (!relativeTo.empty()) {
    auto fileOrDir{canonical(std::string(relativeTo))};
    if (is_directory(fileOrDir)) {
      add(fileOrDir);
    } else {
      add(parent_path(fileOrDir));
    }
  }
  if (searchPwd) {
    auto ec{std::error_code()};
    if (auto pwd{std::filesystem::current_path(ec)}; !ec) {
      add(std::move(pwd));
    }
  }
  for (const auto &[dir, isRecursive] : searchDirs) {
    add(dir);
    if (isRecursive) {
      for (auto &&entry : std::filesystem::recursive_directory_iterator(dir)) {
        if (auto subDir{entry.path().string()}; is_directory(subDir)) {
          add(std::move(subDir));
        }
      }
    }
  }
  return results;
}

std::optional<std::string> FileLocator::locate(std::string_view fileName,
                                               std::string_view relativeTo,
                                               LocateFlags flags) const {
  auto result{std::string()};
  auto accept{[&](std::filesystem::path attempt) {
    try {
      auto ec{std::error_code()};
      if (((flags & REGULAR_FILES) != 0 &&
           std::filesystem::is_regular_file(attempt, ec)) ||
          ((flags & DIRS) != 0 && std::filesystem::is_directory(attempt, ec))) {
        result = canonical(attempt.string());
        return true;
      }
    } catch (...) {
      // Do nothing
    }
    return false;
  }};
  auto fname{std::filesystem::path(fileName)};
  if (fname.is_absolute() && accept(fname)) {
    return result;
  }
  if (fname.is_relative()) {
    for (auto &&dir : get_search_dirs(relativeTo)) {
      if (accept(std::filesystem::path(dir) / fname)) {
        return result;
      }
    }
  }
  return std::nullopt;
}

std::vector<FileLocator::ImagePath>
FileLocator::locate_images(std::string_view fileName,
                           std::string_view relativeTo) const {
  const auto fileNameStrRef{llvm::StringRef(fileName)};
  // Look for tile placeholders:
  // - "<UDIM>"
  // - "<UVTILEO>"
  // - "<UVTILE1>"
  const bool hasUDIM{fileNameStrRef.contains("<UDIM>")};
  const bool hasUVTILE0{fileNameStrRef.contains("<UVTILE0>")};
  const bool hasUVTILE1{fileNameStrRef.contains("<UVTILE1>")};
  if (!hasUDIM && !hasUVTILE0 && !hasUVTILE1) {
    // If no tile placeholders, this is an ordinary filename
    // meant to identify just 1 tile.
    auto result{locate(fileName, relativeTo)};
    if (!result)
      return {};
    return {ImagePath{0, 0, std::move(*result)}};
  } else {
    // Else, split the filename into the substrings before and
    // after the placeholder. NOTE: No structured bindings here
    // because we capture in the lambda below and that is not
    // OK in C++17.
    auto fileNameBeforeAndAfter =
        fileNameStrRef.rsplit(hasUDIM      ? "<UDIM>"
                              : hasUVTILE0 ? "<UVTILE0>"
                                           : "<UVTILE1>");
    const auto &fileNameBefore{fileNameBeforeAndAfter.first};
    const auto &fileNameAfter{fileNameBeforeAndAfter.second};
    // Try to match the given path and parse the tile indices.
    auto tryMatch{[&](llvm::StringRef path, uint32_t &tileIndexU,
                      uint32_t &tileIndexV) -> bool {
      if (auto pos{path.rfind(fileNameBefore)}; pos != path.npos)
        path = path.substr(pos + fileNameBefore.size());
      else
        return false;
      if (hasUDIM) {
        uint32_t num{};
        if (path.consumeInteger(10, num) || num < 1001)
          return false;
        num -= 1001;
        tileIndexU = num % 10;
        tileIndexV = num / 10;
        if (tileIndexU >= 10 || tileIndexV >= 10)
          return false;
      } else {
        if (!path.consume_front("_u") || path.consumeInteger(10, tileIndexU) ||
            !path.consume_front("_v") || path.consumeInteger(10, tileIndexV))
          return false;
        if (hasUVTILE1 && (tileIndexU == 0 || tileIndexV == 0))
          return false;
      }
      return path == fileNameAfter;
    }};
    // Collect results in an STL set so we do not duplicate image paths somehow.
    auto results{std::set<ImagePath>{}};
    for (auto &&dir : get_search_dirs(relativeTo)) {
      // TODO
    }
    return std::vector<ImagePath>(results.begin(), results.end());
  }
}

#if 0
  auto ec{std::error_code()};
  std::optional<std::filesystem::path> pathDir0{};
  auto searchForFiles{[&](auto &&dirIterator) {
    for (const auto &dirEntry : dirIterator) {
      ImagePath imagePath{};
      imagePath.path = dirEntry.path();
      if (match(imagePath)) {
        if (auto path{std::filesystem::canonical(imagePath.path, ec)}; !ec && (!pathDir0 || *pathDir0 == path.parent_path())) {
          if (!pathDir0)
            pathDir0 = path.parent_path();
          imagePath.path = std::move(path);
          imagePaths.insert(std::move(imagePath));
        }
      }
    }
  }};
  if (!fname0.empty()) {
    if (fname0 = std::filesystem::canonical(fname0, ec); !ec) {
      if (std::filesystem::is_regular_file(fname0, ec))
        fname0 = fname0.parent_path();
      if (std::filesystem::is_directory(fname0, ec))
        searchForFiles(std::filesystem::directory_iterator(fname0));
    }
  }
  if (searchPwd)
    if (auto pwd{std::filesystem::current_path(ec)}; !ec)
      searchForFiles(std::filesystem::directory_iterator(pwd));
  for (const auto &[dir, isRecursive] : searchDirs) {
    if (isRecursive)
      searchForFiles(std::filesystem::recursive_directory_iterator(dir));
    else
      searchForFiles(std::filesystem::directory_iterator(dir));
  }
#endif

} // namespace smdl
