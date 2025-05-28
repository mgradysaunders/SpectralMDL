#pragma once

#include <cctype>
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
