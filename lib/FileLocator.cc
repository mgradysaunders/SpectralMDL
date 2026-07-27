#include "smdl/FileLocator.h"

#include <filesystem>

#include "llvm/ADT/StringSet.h"

namespace smdl {

std::vector<std::string>
FileLocator::getSearchDirs(std::string_view relativeTo) const {
  auto results{std::vector<std::string>()};
  auto resultsSet{llvm::StringSet()};
  auto add{[&](std::string dir) {
    if (auto [itr, inserted] = resultsSet.insert(dir); inserted) {
      results.push_back(std::move(dir));
    }
  }};
  if (!relativeTo.empty()) {
    auto fileOrDir{makePathCanonical(std::string(relativeTo))};
    if (isDirectory(fileOrDir)) {
      add(fileOrDir);
    } else {
      add(parentPathOf(fileOrDir));
    }
  }
  if (mSearchPwd) {
    auto ec{std::error_code()};
    if (auto pwd{std::filesystem::current_path(ec)}; !ec) {
      add(pwd.string());
    }
  }
  for (const auto &[dir, isRecursive] : mSearchDirs) {
    add(dir);
    if (isRecursive) {
      auto ec{std::error_code()};
      auto itr{std::filesystem::recursive_directory_iterator(
          dir, std::filesystem::directory_options::skip_permission_denied, ec)};
      for (; !ec && itr != std::filesystem::recursive_directory_iterator();
           itr.increment(ec)) {
        if (auto subDir{itr->path().string()}; isDirectory(subDir)) {
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
        result = makePathCanonical(attempt.string());
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
    for (auto &&dir : getSearchDirs(relativeTo)) {
      if (accept(std::filesystem::path(dir) / fname)) {
        return result;
      }
    }
  }
  return std::nullopt;
}

std::vector<FileLocator::ImagePath>
FileLocator::locateImages(std::string_view fileName,
                          std::string_view relativeTo) const {
  const auto pattern{std::filesystem::path(std::string(fileName))};
  const auto patternName{pattern.filename().string()};
  const auto patternNameStrRef{llvm::StringRef(patternName)};
  // Look for tile placeholders in the final path component:
  // - "<UDIM>"
  // - "<UVTILE0>"
  // - "<UVTILE1>"
  const bool hasUDIM{patternNameStrRef.contains("<UDIM>")};
  const bool hasUVTILE0{patternNameStrRef.contains("<UVTILE0>")};
  const bool hasUVTILE1{patternNameStrRef.contains("<UVTILE1>")};
  if (!hasUDIM && !hasUVTILE0 && !hasUVTILE1) {
    // If no tile placeholders, this is an ordinary filename
    // meant to identify just 1 tile.
    auto result{locate(fileName, relativeTo)};
    if (!result) return {};
    return {ImagePath{0, 0, std::move(*result)}};
  }
  // Else, split the final path component into the substrings before
  // and after the placeholder. NOTE: No structured bindings here
  // because we capture in the lambda below and that is not OK
  // in C++17.
  auto patternNameBeforeAndAfter =
      patternNameStrRef.rsplit(hasUDIM      ? "<UDIM>"
                               : hasUVTILE0 ? "<UVTILE0>"
                                            : "<UVTILE1>");
  const auto &patternNameBefore{patternNameBeforeAndAfter.first};
  const auto &patternNameAfter{patternNameBeforeAndAfter.second};
  // Try to match the given filename and parse the tile indexes.
  auto tryMatch{[&](llvm::StringRef name, uint32_t &tileIndexU,
                    uint32_t &tileIndexV) -> bool {
    if (!name.consume_front(patternNameBefore)) return false;
    if (hasUDIM) {
      // "<UDIM>" stands for exactly 4 decimal digits.
      auto digits{name.take_front(4)};
      uint32_t num{};
      if (digits.size() != 4 || digits.consumeInteger(10, num) ||
          !digits.empty() || num < 1001)
        return false;
      name = name.drop_front(4);
      num -= 1001;
      tileIndexU = num % 10;
      tileIndexV = num / 10;
    } else {
      if (!name.consume_front("_u") || name.consumeInteger(10, tileIndexU) ||
          !name.consume_front("_v") || name.consumeInteger(10, tileIndexV))
        return false;
      if (hasUVTILE1) {
        // "<UVTILE1>" is 1-based, so normalize to be 0-based.
        if (tileIndexU == 0 || tileIndexV == 0) return false;
        tileIndexU -= 1;
        tileIndexV -= 1;
      }
      // Sanity check against absurd allocations downstream.
      if (tileIndexU >= 1000 || tileIndexV >= 1000) return false;
    }
    return name == patternNameAfter;
  }};
  // Determine the candidate directories to scan. If the pattern is
  // absolute, the only candidate is its parent directory. Otherwise,
  // join each active search directory with the directory portion of
  // the pattern, if any.
  auto scanDirs{std::vector<std::string>()};
  if (pattern.is_absolute()) {
    scanDirs.push_back(pattern.parent_path().string());
  } else {
    auto patternDir{pattern.parent_path().string()};
    for (auto &&dir : getSearchDirs(relativeTo)) {
      scanDirs.push_back(joinPaths(dir, patternDir));
    }
  }
  // Scan the candidate directories in order. The first directory that
  // contains at least 1 match provides all of the results, so that
  // every tile is guaranteed to reside in the same directory.
  for (const auto &scanDir : scanDirs) {
    auto results{std::vector<ImagePath>()};
    auto ec{std::error_code()};
    auto itr{std::filesystem::directory_iterator(
        scanDir, std::filesystem::directory_options::skip_permission_denied,
        ec)};
    for (; !ec && itr != std::filesystem::directory_iterator();
         itr.increment(ec)) {
      auto entryEc{std::error_code()};
      ImagePath imagePath{};
      if (!itr->is_regular_file(entryEc) ||
          !tryMatch(itr->path().filename().string(), imagePath.tileIndexU,
                    imagePath.tileIndexV))
        continue;
      if (auto path{std::filesystem::canonical(itr->path(), entryEc)};
          !entryEc) {
        imagePath.path = path.string();
        results.emplace_back(std::move(imagePath));
      }
    }
    if (!results.empty()) {
      std::sort(results.begin(), results.end());
      return results;
    }
  }
  return {};
}

} // namespace smdl
