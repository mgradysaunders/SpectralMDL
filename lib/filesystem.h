#pragma once

#include <filesystem>
namespace fs = std::filesystem;
using fs_error_code = std::error_code;

#include "smdl/common.h"

namespace smdl {

[[nodiscard]] inline fs::path fs_make_path(const std::string &str) {
  return fs::path(str);
}

[[nodiscard]] inline fs::path fs_make_path(std::string_view str) {
  // NOTE: Boost doesn't have `std::string_view` constructor
  return fs::path(str.begin(), str.end());
}

} // namespace smdl
