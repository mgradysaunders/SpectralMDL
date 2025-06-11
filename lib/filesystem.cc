#include "filesystem.h"

#include "thirdparty/miniz.h"

namespace smdl {

std::string fs_read(const fs::path &path) {
  auto stream{fs_open(path, std::ios::in | std::ios::binary)};
  return std::string((std::istreambuf_iterator<char>(stream)),
                     std::istreambuf_iterator<char>());
}

std::string fs_read_thru_archive(const fs::path &path,
                                 bool &isExtractedFromArchive) {
  isExtractedFromArchive = false;
  fs_error_code ec{};
  if (fs::exists(path, ec)) {
    return fs_read(path);
  }
  bool zipPathFound{};
  fs::path zipPath{};
  for (const auto &elem : path) {
    if (zipPath /= elem; fs::is_regular_file(zipPath, ec)) {
      zipPathFound = true;
      break;
    }
  }
  if (!zipPathFound) {
    throw Error(
        concat("cannot load ", quoted(path.string()), ": file not found"));
  }
  auto zipFileName{zipPath.string()};
  auto zip{mz_zip_archive{}};
  auto zipDefer{Defer([&] { mz_zip_reader_end(&zip); })};
  auto zipError{
      [&] { return mz_zip_get_error_string(mz_zip_get_last_error(&zip)); }};
  mz_zip_zero_struct(&zip);
  if (!mz_zip_reader_init_file(&zip, zipFileName.c_str(), /*flags=*/0)) {
    throw Error(concat("cannot load ", quoted(zipFileName), ": ", zipError()));
  }
  auto fileName{path.lexically_relative(zipPath).lexically_normal().string()};
  auto fileIndex{mz_zip_reader_locate_file(&zip, fileName.c_str(), nullptr,
                                           /*flags=*/0)};
  if (fileIndex < 0) {
    throw Error(concat("cannot load ", quoted(path.string()),
                       ": file not found in ZIP archive"));
  }
  auto fileStat{mz_zip_archive_file_stat{}};
  if (!mz_zip_reader_file_stat(&zip, fileIndex, &fileStat)) {
    throw Error(
        concat("cannot load ", quoted(path.string()), ": ", zipError()));
  }
  auto file{std::string()};
  file.resize(fileStat.m_uncomp_size);
  if (!mz_zip_reader_extract_to_mem(&zip, fileIndex, file.data(), file.size(),
                                    /*flags=*/0)) {
    throw Error(
        concat("cannot load ", quoted(path.string()), ": ", zipError()));
  }
  isExtractedFromArchive = true;
  return file;
}

} // namespace smdl
