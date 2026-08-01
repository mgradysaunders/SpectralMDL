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

std::string expandPathVariables(std::string_view path) {
  auto isNameStart{[](char ch) {
    return ch == '_' || (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z');
  }};
  auto isNameContinue{[&](char ch) {
    return isNameStart(ch) || (ch >= '0' && ch <= '9');
  }};
  const auto fullPath{path};
  auto result{std::string()};
  result.reserve(path.size());
  while (!path.empty()) {
    if (path[0] != '$') {
      result += path[0];
      path.remove_prefix(1);
      continue;
    }
    auto name{std::string_view()};
    auto lenConsumed{size_t(0)};
    if (path.size() > 1 && path[1] == '{') {
      if (auto pos{path.find('}', 2)}; pos != std::string_view::npos) {
        name = path.substr(2, pos - 2);
        lenConsumed = pos + 1;
      }
    } else if (path.size() > 1 && isNameStart(path[1])) {
      auto pos{size_t(2)};
      while (pos < path.size() && isNameContinue(path[pos])) {
        pos++;
      }
      name = path.substr(1, pos - 1);
      lenConsumed = pos;
    }
    if (name.empty()) {
      result += path[0];
      path.remove_prefix(1);
      continue;
    }
    auto value{std::getenv(std::string(name).c_str())};
    if (!value)
      throw Error(concat("undefined environment variable ", Quoted(name),
                         " in path ", Quoted(fullPath)));
    result += value;
    path.remove_prefix(lenConsumed);
  }
  return result;
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
