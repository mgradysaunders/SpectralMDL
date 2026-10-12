/// \file
#pragma once

#include "smdl/Common.h"

#include "thirdparty/miniz/miniz.h"

namespace smdl {

/// A zip archive.
class SMDL_EXPORT Archive final {
public:
  Archive() { mz_zip_zero_struct(&zip_); }

  Archive(const std::string &fileName) {
    mz_zip_zero_struct(&zip_);
    if (!mz_zip_reader_init_file(&zip_, fileName.c_str(), /*flags=*/0))
      throw Error(
          concat("Cannot load ", SpellFilePath(fileName), ": ",
                 mz_zip_get_error_string(mz_zip_get_last_error(&zip_))));
  }

  Archive(const Archive &) = delete;

  ~Archive() { close(); }

public:
  /// Get file count.
  [[nodiscard]] int get_file_count() {
    return mz_zip_reader_get_num_files(&zip_);
  }

  /// Get file index, or return `-1` if not found.
  [[nodiscard]] int get_file_index(const std::string &fileName) {
    return mz_zip_reader_locate_file(&zip_, fileName.c_str(), nullptr,
                                     /*flags=*/0);
  }

  /// Get file name.
  [[nodiscard]] std::string get_file_name(int fileIndex) {
    std::array<char, 512> buffer{};
    if (!mz_zip_reader_get_filename(&zip_, fileIndex, buffer.data(),
                                    buffer.size())) {
      throw Error(mz_zip_get_error_string(mz_zip_get_last_error(&zip_)));
    }
    return std::string(buffer.data());
  }

  /// Get file stat.
  [[nodiscard]] mz_zip_archive_file_stat file_stat(int fileIndex) {
    mz_zip_archive_file_stat stat{};
    if (!mz_zip_reader_file_stat(&zip_, fileIndex, &stat)) {
      throw Error(mz_zip_get_error_string(mz_zip_get_last_error(&zip_)));
    }
    return stat;
  }

  /// Get file.
  [[nodiscard]] std::string extract_file(int fileIndex) {
    mz_zip_archive_file_stat stat{file_stat(fileIndex)};
    std::string file{};
    file.resize(stat.m_uncomp_size);
    if (!mz_zip_reader_extract_to_mem(&zip_, fileIndex, file.data(),
                                      file.size(),
                                      /*flags=*/0)) {
      throw Error(mz_zip_get_error_string(mz_zip_get_last_error(&zip_)));
    }
    return file;
  }

  /// Close.
  void close() {
    mz_zip_end(&zip_);
    mz_zip_zero_struct(&zip_);
  }

private:
  mz_zip_archive zip_{};
};

} // namespace smdl
