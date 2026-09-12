#include "smdl/Support/FileLocator.h"
#include "smdl/Support/Logger.h"

#include <cstdlib>
#include <filesystem>
#include <mutex>

#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Support/Program.h"

namespace smdl {

namespace {
// The default search directories are read on every lookup, so an entry
// that names no directory is reported once per process, not per lookup.
void warnAboutDefaultSearchDir(llvm::StringRef entry) {
  static std::mutex mutex{};
  static llvm::StringSet<> warnedEntries{};
  {
    std::scoped_lock guard{mutex};
    if (!warnedEntries.insert(entry).second) return;
  }
  SMDL_LOG_WARN("SMDL_DEFAULT_SEARCH_DIRS names ", Quoted(entry),
                ", which is not a directory");
}
} // namespace

std::vector<std::string>
FileLocator::getSearchDirs(std::string_view relativeTo,
                           Span<const std::string> priorityDirs) const {
  std::vector<std::string> results{};
  llvm::StringSet<> resultsSet{};
  auto add{[&](std::string dir) {
    if (auto [itr, inserted] = resultsSet.insert(dir); inserted) {
      results.push_back(std::move(dir));
    }
  }};
  for (const auto &dir : priorityDirs) {
    add(makePathCanonical(dir));
  }
  if (!relativeTo.empty()) {
    std::string fileOrDir{makePathCanonical(std::string(relativeTo))};
    if (isDirectory(fileOrDir)) {
      add(fileOrDir);
    } else {
      add(parentPathOf(fileOrDir));
    }
  }
  if (mShouldSearchPwd) {
    std::error_code ec{};
    if (std::filesystem::path pwd{std::filesystem::current_path(ec)}; !ec) {
      add(pwd.string());
    }
  }
  for (const auto &[dir, isRecursive] : mSearchDirs) {
    add(dir);
    if (isRecursive) {
      std::error_code ec{};
      std::filesystem::recursive_directory_iterator itr{
          dir, std::filesystem::directory_options::skip_permission_denied, ec};
      for (; !ec && itr != std::filesystem::recursive_directory_iterator();
           itr.increment(ec)) {
        if (std::string subDir{itr->path().string()}; isDirectory(subDir)) {
          add(std::move(subDir));
        }
      }
    }
  }
  if (mShouldSearchDefaultDirs) {
    if (const char *value{std::getenv("SMDL_DEFAULT_SEARCH_DIRS")}) {
      llvm::SmallVector<llvm::StringRef> entries{};
      llvm::StringRef(value).split(entries, llvm::sys::EnvPathSeparator,
                                   /*MaxSplit=*/-1, /*KeepEmpty=*/false);
      for (auto entry : entries) {
        if (std::string dir{makePathCanonical(entry.str())}; isDirectory(dir)) {
          add(std::move(dir));
        } else {
          warnAboutDefaultSearchDir(entry);
        }
      }
    }
  }
  return results;
}

std::optional<std::string>
FileLocator::locate(std::string_view fileName, std::string_view relativeTo,
                    LocateFlags flags,
                    Span<const std::string> priorityDirs) const {
  std::string result{};
  auto accept{[&](std::filesystem::path attempt) {
    try {
      std::error_code ec{};
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
  std::filesystem::path fname{fileName};
  if (fname.is_absolute() && accept(fname)) {
    return result;
  }
  if (fname.is_relative()) {
    for (auto &&dir : getSearchDirs(relativeTo, priorityDirs)) {
      if (accept(std::filesystem::path(dir) / fname)) {
        return result;
      }
    }
  }
  return std::nullopt;
}

std::vector<FileLocator::ImagePath>
FileLocator::locateImages(std::string_view fileName,
                          std::string_view relativeTo,
                          Span<const std::string> priorityDirs) const {
  const std::filesystem::path pattern{std::string(fileName)};
  const std::string patternName{pattern.filename().string()};
  const llvm::StringRef patternNameStrRef{patternName};
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
    std::optional<std::string> result{
        locate(fileName, relativeTo, REGULAR_FILES, priorityDirs)};
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
  const llvm::StringRef &patternNameBefore{patternNameBeforeAndAfter.first};
  const llvm::StringRef &patternNameAfter{patternNameBeforeAndAfter.second};
  // Try to match the given filename and parse the tile indexes.
  auto tryMatch{[&](llvm::StringRef name, uint32_t &tileIndexU,
                    uint32_t &tileIndexV) -> bool {
    if (!name.consume_front(patternNameBefore)) return false;
    if (hasUDIM) {
      // "<UDIM>" stands for exactly 4 decimal digits.
      llvm::StringRef digits{name.take_front(4)};
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
  std::vector<std::string> scanDirs{};
  if (pattern.is_absolute()) {
    scanDirs.push_back(pattern.parent_path().string());
  } else {
    std::string patternDir{pattern.parent_path().string()};
    for (auto &&dir : getSearchDirs(relativeTo, priorityDirs)) {
      scanDirs.push_back(joinPaths(dir, patternDir));
    }
  }
  // Scan the candidate directories in order. The first directory that
  // contains at least 1 match provides all of the results, so that
  // every tile is guaranteed to reside in the same directory.
  for (const auto &scanDir : scanDirs) {
    std::vector<ImagePath> results{};
    std::error_code ec{};
    std::filesystem::directory_iterator itr{
        scanDir, std::filesystem::directory_options::skip_permission_denied,
        ec};
    for (; !ec && itr != std::filesystem::directory_iterator();
         itr.increment(ec)) {
      std::error_code entryEc{};
      ImagePath imagePath{};
      if (!itr->is_regular_file(entryEc) ||
          !tryMatch(itr->path().filename().string(), imagePath.tileIndexU,
                    imagePath.tileIndexV))
        continue;
      if (std::filesystem::path path{
              std::filesystem::canonical(itr->path(), entryEc)};
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
