#pragma once

#include <cerrno>
#include <cctype>
#include <cstring>
#include <fstream>
#include <streambuf>
#include <string>

#if SMDL_USE_BOOST_FILESYSTEM
#include "boost/filesystem.hpp"
namespace fs = boost::filesystem;
using fs_error_code = boost::system::error_code;
#else
#include <filesystem>
namespace fs = std::filesystem;
using fs_error_code = std::error_code;
#endif // #if SMDL_USE_BOOST_FILESYSTEM

#include "smdl/common.h"

namespace smdl {

[[nodiscard]] inline fs::path fs_make_path(std::string_view str) {
  // NOTE: Boost doesn't have `std::string_view` constructor
  return fs::path(str.begin(), str.end());
}

[[nodiscard]] inline fs::path fs_make_path(const std::string &str) {
  return fs::path(str);
}

[[nodiscard]] inline std::fstream fs_open_file(const fs::path &path,
                                               std::ios::openmode mode) {
#if SMDL_USE_BOOST_FILESYSTEM
  auto stream{std::fstream(path.string(), mode)};
#else
  auto stream{std::fstream(path, mode)};
#endif // #if SMDL_USE_BOOST_FILESYSTEM
  if (!stream.is_open())
    throw Error(concat("cannot open ", quoted(path.string()), ": ",
                       std::strerror(errno)));
  return stream;
}

[[nodiscard]] inline std::string fs_read_file(const fs::path &path) {
  auto stream{fs_open_file(path, std::ios::in | std::ios::binary)};
  return std::string((std::istreambuf_iterator<char>(stream)),
                     std::istreambuf_iterator<char>());
}

[[nodiscard]] inline std::string fs_extension(const fs::path &path) try {
  auto extension{path.extension().string()};
  for (char &ch : extension)
    ch = std::tolower(static_cast<unsigned char>(ch));
  return extension;
} catch (...) {
  return {};
}

[[nodiscard]] inline std::string fs_abbreviate(const fs::path &path) {
  auto pathStr{path.string()};
  try {
    if (auto s{fs::relative(path).string()}; s.size() < pathStr.size())
      return s;
  } catch (...) {
  }
  return pathStr;
}

} // namespace smdl
