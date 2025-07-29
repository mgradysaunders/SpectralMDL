#include "smdl/Support/Filesystem.h"

#include "llvm/Support/FileSystem.h"

#include <cerrno>
#include <streambuf>

#include "../filesystem.h"

namespace smdl {

bool has_extension(std::string_view path, std::string_view extension) noexcept {
  return llvm::StringRef(path).ends_with_insensitive(extension);
}

bool exists(const std::string &path) noexcept try {
  return fs::exists(path);
} catch (...) {
  return false;
}

bool is_file(const std::string &path) noexcept try {
  return fs::is_regular_file(path);
} catch (...) {
  return false;
}

bool is_directory(const std::string &path) noexcept try {
  return fs::is_directory(path);
} catch (...) {
  return false;
}

bool is_path_equivalent(const std::string &path0,
                        const std::string &path1) noexcept try {
  return canonical(path0) == canonical(path1);
} catch (...) {
  return false;
}

bool is_parent_path_of(const std::string &path0,
                       const std::string &path1) noexcept try {
  return fs::relative(canonical(path1), canonical(path0)) != fs::path();
} catch (...) {
  return false;
}

std::string join_paths(std::string_view path0, std::string_view path1) {
  if (path1.empty())
    return std::string(path0);
  if (path0.empty())
    return std::string(path1);
  return (fs_make_path(path0) / fs_make_path(path1)).string();
}

std::string canonical(std::string path) noexcept try {
  if (starts_with(path, "~")) {
    llvm::SmallString<128> pathTmp{};
    llvm::sys::fs::expand_tilde(path, pathTmp);
    path = pathTmp.str();
  }
  return fs::weakly_canonical(path).string();
} catch (...) {
  return path;
}

std::string relative(std::string path) noexcept try {
  return fs::relative(path).string();
} catch (...) {
  return path;
}

std::string parent_path(std::string path) noexcept try {
  return fs::path(path).parent_path().string();
} catch (...) {
  return path;
}

std::fstream open_or_throw(const std::string &path, std::ios::openmode mode) {
  auto stream{std::fstream(path, mode)};
  if (!stream.is_open())
    throw Error(
        concat("cannot open ", quoted_path(path), ": ", std::strerror(errno)));
  return stream;
}

std::string read_or_throw(const std::string &path) {
  auto stream{open_or_throw(path, std::ios::in | std::ios::binary)};
  return std::string((std::istreambuf_iterator<char>(stream)),
                     std::istreambuf_iterator<char>());
}

} // namespace smdl
