#include "smdl/Support/Filesystem.h"
#include "smdl/Common.h"

#include <cerrno>
#include <filesystem>
#include <streambuf>

#include "llvm/Support/FileSystem.h"

namespace smdl {

bool hasExtension(std::string_view path, std::string_view extension) noexcept {
  return llvm::StringRef(path).ends_with_insensitive(extension);
}

bool exists(const std::string &path) noexcept try {
  return std::filesystem::exists(path);
} catch (...) {
  return false;
}

bool isFile(const std::string &path) noexcept try {
  return std::filesystem::is_regular_file(path);
} catch (...) {
  return false;
}

bool isDirectory(const std::string &path) noexcept try {
  return std::filesystem::is_directory(path);
} catch (...) {
  return false;
}

bool isPathEquivalent(const std::string &path0,
                      const std::string &path1) noexcept try {
  return makePathCanonical(path0) == makePathCanonical(path1);
} catch (...) {
  return false;
}

bool isParentPathOf(const std::string &path0, const std::string &path1) noexcept
    try {
  return std::filesystem::relative(makePathCanonical(path1),
                                   makePathCanonical(path0)) !=
         std::filesystem::path();
} catch (...) {
  return false;
}

std::string joinPaths(std::string_view path0, std::string_view path1) {
  if (path1.empty())
    return std::string(path0);
  if (path0.empty())
    return std::string(path1);
  return (std::filesystem::path(path0) / std::filesystem::path(path1)).string();
}

std::string makePathCanonical(std::string path) noexcept try {
  if (!path.empty() && path[0] == '~') {
    llvm::SmallString<128> pathTmp{};
    llvm::sys::fs::expand_tilde(path, pathTmp);
    path = pathTmp.str();
  }
  return std::filesystem::weakly_canonical(path).string();
} catch (...) {
  return path;
}

std::string makePathRelative(std::string path) noexcept try {
  return std::filesystem::relative(path).string();
} catch (...) {
  return path;
}

std::string parentPathOf(std::string path) noexcept try {
  return std::filesystem::path(path).parent_path().string();
} catch (...) {
  return path;
}

std::fstream openOrThrow(const std::string &path, std::ios::openmode mode) {
  auto stream{std::fstream(path, mode)};
  if (!stream.is_open())
    throw Error(
        concat("cannot open ", QuotedPath(path), ": ", std::strerror(errno)));
  return stream;
}

std::string readOrThrow(const std::string &path) {
  auto stream{openOrThrow(path, std::ios::in | std::ios::binary)};
  return std::string((std::istreambuf_iterator<char>(stream)),
                     std::istreambuf_iterator<char>());
}

} // namespace smdl
