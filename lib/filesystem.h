#pragma once

#include <cctype>
#include <cerrno>
#include <fstream>
#include <streambuf>

#if SMDL_USE_BOOST_FILESYSTEM
#include "boost/filesystem.hpp"
namespace fs = boost::filesystem;
using fs_fstream = boost::filesystem::fstream;
using fs_error_code = boost::system::error_code;
#else
#include <filesystem>
namespace fs = std::filesystem;
using fs_fstream = std::fstream;
using fs_error_code = std::error_code;
#endif // #if SMDL_USE_BOOST_FILESYSTEM

#include "smdl/common.h"

namespace smdl {

[[nodiscard]] inline fs::path fs_make_path(const std::string &str) {
  return fs::path(str);
}

[[nodiscard]] inline fs::path fs_make_path(std::string_view str) {
  // NOTE: Boost doesn't have `std::string_view` constructor
  return fs::path(str.begin(), str.end());
}

[[nodiscard]] inline std::string fs_abbreviate_path(const fs::path &path) {
  auto pathStr{path.string()};
  try {
    if (auto str{fs::relative(path).string()}; str.size() < pathStr.size())
      return str;
  } catch (...) {
    // Ignore on failure
  }
  return pathStr;
}

[[nodiscard]] inline std::string fs_extension(const fs::path &path) try {
  auto extension{path.extension().string()};
  for (char &ch : extension)
    ch = std::tolower(static_cast<unsigned char>(ch));
  return extension;
} catch (...) {
  return {};
}

[[nodiscard]] inline fs_fstream fs_open(const fs::path &path,
                                        std::ios::openmode mode) {
  auto stream{fs_fstream(path, mode)};
  if (!stream.is_open())
    throw Error(concat("cannot open ", quoted(path.string()), ": ",
                       std::strerror(errno)));
  return stream;
}

[[nodiscard]] std::string fs_read(const fs::path &path);

[[nodiscard]] std::string fs_read_thru_archive(const fs::path &path,
                                               bool &isExtractedFromArchive);

template <size_t N>
[[nodiscard]] inline std::array<char, N> fs_read_header(const fs::path &path) {
  auto buffer{std::array<char, N>{}};
  auto stream{fs_open(path, std::ios::in | std::ios::binary)};
  stream.read(buffer.data(), buffer.size());
  return buffer;
}

} // namespace smdl
