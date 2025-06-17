#pragma once

#if SMDL_USE_BOOST_FILESYSTEM
#include "boost/filesystem.hpp"
namespace fs = boost::filesystem;
using fs_error_code = boost::system::error_code;
#else
#include <filesystem>
namespace fs = std::filesystem;
using fs_error_code = std::error_code;
#endif // #if SMDL_USE_BOOST_FILESYSTEM

#include "thirdparty/miniz.h"

#include "smdl/common.h"

namespace smdl {

[[nodiscard]] inline fs::path fs_make_path(const std::string &str) {
  return fs::path(str);
}

[[nodiscard]] inline fs::path fs_make_path(std::string_view str) {
  // NOTE: Boost doesn't have `std::string_view` constructor
  return fs::path(str.begin(), str.end());
}

/// A zip archive.
class SMDL_EXPORT Archive final {
public:
  Archive() { mz_zip_zero_struct(&zip); }

  Archive(const std::string &fileName) {
    mz_zip_zero_struct(&zip);
    if (!mz_zip_reader_init_file(&zip, fileName.c_str(), /*flags=*/0))
      throw Error(concat("cannot load ", quoted(fileName), ": ",
                         mz_zip_get_error_string(mz_zip_get_last_error(&zip))));
  }

  Archive(const Archive &) = delete;

  ~Archive() { close(); }

public:
  /// Get file count.
  [[nodiscard]] int get_file_count() {
    return mz_zip_reader_get_num_files(&zip);
  }

  /// Get file index, or return `-1` if not found.
  [[nodiscard]] int get_file_index(const std::string &fileName) {
    return mz_zip_reader_locate_file(&zip, fileName.c_str(), nullptr,
                                     /*flags=*/0);
  }

  /// Get file name.
  [[nodiscard]] std::string get_file_name(int fileIndex) {
    auto buffer{std::array<char, 512>{}};
    if (!mz_zip_reader_get_filename(&zip, fileIndex, buffer.data(),
                                    buffer.size())) {
      throw Error(mz_zip_get_error_string(mz_zip_get_last_error(&zip)));
    }
    return std::string(buffer.data());
  }

  /// Get file stat.
  [[nodiscard]] mz_zip_archive_file_stat file_stat(int fileIndex) {
    auto stat{mz_zip_archive_file_stat{}};
    if (!mz_zip_reader_file_stat(&zip, fileIndex, &stat)) {
      throw Error(mz_zip_get_error_string(mz_zip_get_last_error(&zip)));
    }
    return stat;
  }

  /// Get file.
  [[nodiscard]] std::string extract_file(int fileIndex) {
    auto stat{file_stat(fileIndex)};
    auto file{std::string()};
    file.resize(stat.m_uncomp_size);
    if (!mz_zip_reader_extract_to_mem(&zip, fileIndex, file.data(), file.size(),
                                      /*flags=*/0)) {
      throw Error(mz_zip_get_error_string(mz_zip_get_last_error(&zip)));
    }
    return file;
  }

  /// Close.
  void close() {
    mz_zip_end(&zip);
    mz_zip_zero_struct(&zip);
  }

private:
  mz_zip_archive zip{};
};

} // namespace smdl
