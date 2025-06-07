#include "smdl/Archive.h"
#include "smdl/Logger.h"

#include "filesystem.h"

#include "thirdparty/miniz.h"

namespace smdl {

bool is_file_MDLE(const std::string &fileName) noexcept try {
  return fs_test_header<4>(fileName, {0x4D, 0x44, 0x4C, 0x45});
} catch (...) {
  return false;
}

bool is_file_MDR(const std::string &fileName) noexcept try {
  return fs_test_header<4>(fileName, {0x4D, 0x44, 0x52, 0x00});
} catch (...) {
  return false;
}

std::unique_ptr<ArchiveMDLE>
ArchiveMDLE::load_from_file(const std::string &fileName) {
  if (!fs_test_header<4>(fileName, {0x4D, 0x44, 0x4C, 0x45})) {
    throw Error(concat("cannot load ", quoted(fileName), ": not an MDLE file"));
  }
  auto archive{std::make_unique<ArchiveMDLE>()};
  auto zip{mz_zip_archive{}};
  auto zipDefer{Defer([&] { mz_zip_reader_end(&zip); })};
  auto zipError{
      [&] { return mz_zip_get_error_string(mz_zip_get_last_error(&zip)); }};
  auto zipReadFile{[&](int i) -> std::string {
    auto file{std::string()};
    auto fileReader{[](void *ptr, mz_uint64 offset, const void *buf, size_t n) {
      auto &file{*static_cast<std::string *>(ptr)};
      if (file.size() < offset)
        file.resize(offset);
      file.insert(file.begin() + offset,          //
                  static_cast<const char *>(buf), //
                  static_cast<const char *>(buf) + n);
      return n;
    }};
    if (!mz_zip_reader_extract_to_callback(&zip, i, fileReader, &file,
                                           /*flags=*/0))
      throw Error(concat("cannot load file ", i, " from ", quoted(fileName),
                         ": ", zipError()));
    return file;
  }};
  mz_zip_zero_struct(&zip);
  if (!mz_zip_reader_init_file(&zip, fileName.c_str(), /*flags=*/0)) {
    throw Error(concat("cannot load ", quoted(fileName), ": ", zipError()));
  }
  auto zipFileCount{int(mz_zip_reader_get_num_files(&zip))};
  for (int zipFileIndex = 0; zipFileIndex < zipFileCount; zipFileIndex++) {
    auto zipFileNameBuffer{std::vector<char>(size_t(
        mz_zip_reader_get_filename(&zip, zipFileIndex, nullptr, 0) + 1))};
    mz_zip_reader_get_filename(&zip, zipFileIndex,       //
                               zipFileNameBuffer.data(), //
                               zipFileNameBuffer.size());
    auto zipFileName{std::string_view(zipFileNameBuffer.data())};
    if (zipFileName == "main.mdl") {
      SMDL_LOG_DEBUG("loading ", quoted(fileName), ": found 'main.mdl'");
      archive->mainFile = zipReadFile(zipFileIndex);
    } else if (starts_with(zipFileName, "resources/")) {
      SMDL_LOG_DEBUG("loading ", quoted(fileName), ": found resource ",
                     quoted(zipFileName));
      auto &resource{archive->resources.emplace_back()};
      resource.fileName = zipFileName;
      resource.file = zipReadFile(zipFileIndex);
    } else {
      SMDL_LOG_DEBUG("loading ", quoted(fileName), ": ignoring ",
                     quoted(zipFileName));
    }
  }
  return archive;
}

} // namespace smdl
