#include "smdl/FileLocator.h"

#include "llvm.h"

#include <set>

namespace smdl {

void FileLocator::add_search_dir(std::filesystem::path dir, bool isRecursive) {
  auto ec{std::error_code()};
  dir = std::filesystem::canonical(dir, ec);
  if (!ec)
    searchDirs.emplace_back(std::move(dir), isRecursive);
}

std::optional<std::filesystem::path> FileLocator::locate(
    const std::filesystem::path &fname, std::filesystem::path fname0) const {
  auto ec{std::error_code()};
  auto result{std::filesystem::path()};
  auto accept{[&](std::filesystem::path attempt) {
    attempt = std::filesystem::canonical(attempt, ec);
    if (!ec && std::filesystem::is_regular_file(attempt, ec)) {
      result = std::move(attempt);
      return true;
    } else {
      return false;
    }
  }};
  if (fname.is_absolute() && accept(fname))
    return result;
  if (fname.is_relative()) {
    if (!fname0.empty()) {
      if (fname0 = std::filesystem::canonical(fname0, ec); !ec) {
        if (std::filesystem::is_regular_file(fname0, ec))
          fname0 = fname0.parent_path();
        if (std::filesystem::is_directory(fname0, ec))
          if (accept(fname0 / fname))
            return result;
      }
    }
    if (searchPwd)
      if (auto pwd{std::filesystem::current_path(ec)}; !ec && accept(pwd / fname))
        return result;
    for (const auto &[dir, isRecursive] : searchDirs) {
      if (accept(dir / fname))
        return result;
      if (isRecursive)
        for (const auto &dirEntry : std::filesystem::recursive_directory_iterator(dir))
          if (dirEntry.is_directory(ec) && accept(dirEntry.path() / fname))
            return result;
    }
  }
  return std::nullopt;
}

std::vector<FileLocator::ImagePath> FileLocator::locate_images(
    std::filesystem::path fname, std::filesystem::path fname0) const {
  auto ec{std::error_code()};
  auto fnameStr{fname.string()};
  auto fnameStrRef{llvm::StringRef(fnameStr)};
  bool hasUDIM{fnameStrRef.contains("<UDIM>")};
  bool hasUVTILE0{fnameStrRef.contains("<UVTILE0>")};
  bool hasUVTILE1{fnameStrRef.contains("<UVTILE1>")};
  if (!hasUDIM && !hasUVTILE0 && !hasUVTILE1) {
    auto path{locate(fname, std::move(fname0))};
    if (!path)
      return {};
    ImagePath imagePath{};
    imagePath.path = std::move(*path);
    imagePath.iU = 0;
    imagePath.iV = 0;
    return {imagePath};
  }
  auto fnameBeforeAndAfter{fnameStrRef.rsplit(hasUDIM ? "<UDIM>" : hasUVTILE0 ? "<UVTILE0>" : "<UVTILE1>")};
  auto match{[&](ImagePath &imagePath) -> bool {
    auto pathStr{imagePath.path.string()};
    auto s{llvm::StringRef(pathStr)};
    if (auto pos{s.rfind(fnameBeforeAndAfter.first)}; pos != s.npos)
      s = s.substr(pos + fnameBeforeAndAfter.first.size());
    else
      return false;
    if (hasUDIM) {
      if (s.size() < 4 ||                                    //
          !std::isdigit(static_cast<unsigned char>(s[0])) || //
          !std::isdigit(static_cast<unsigned char>(s[1])) || //
          !std::isdigit(static_cast<unsigned char>(s[2])) || //
          !std::isdigit(static_cast<unsigned char>(s[3])))
        return false;
      int num{};
      num += int(s[0] - '0') * 1000;
      num += int(s[1] - '0') * 100;
      num += int(s[2] - '0') * 10;
      num += int(s[3] - '0') * 1;
      if (num < 1001)
        return false;
      num -= 1001;
      imagePath.iU = num % 10;
      imagePath.iV = num / 10;
      if (imagePath.iU >= 10 || imagePath.iV >= 10)
        return false;
    } else {
      if (!s.consume_front("_u") || s.consumeInteger(10, imagePath.iU) || // NOTE: consumeInteger() returns true on error
          !s.consume_front("_v") || s.consumeInteger(10, imagePath.iV))
        return false;
      if (hasUVTILE1 && (imagePath.iU == 0 || imagePath.iV == 0))
        return false;
    }
    if (s != fnameBeforeAndAfter.second)
      return false;
    return true;
  }};
  std::set<ImagePath> imagePaths{};
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
  return std::vector<ImagePath>(imagePaths.begin(), imagePaths.end());
}

} // namespace smdl
