#include "filesystem.h"

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
  try {
    isExtractedFromArchive = true;
    auto archive{Archive{zipPath.string()}};
    auto fileName{path.lexically_relative(zipPath).lexically_normal().string()};
    auto fileIndex{archive.get_file_index(fileName)};
    return archive.extract_file(fileIndex);
  } catch (const Error &error) {
    throw Error(
        concat("cannot load ", quoted(path.string()), ": ", error.message));
  }
  return {};
}

} // namespace smdl
