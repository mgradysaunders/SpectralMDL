#include "smdl/Support/MD5Hash.h"
#include "smdl/Support/Filesystem.h"

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/MD5.h"

namespace smdl {

MD5Hash MD5Hash::hashFile(const std::string &fileName) noexcept try {
  llvm::MD5 hasher{};
  std::array<char, 128> buffer{};
  std::fstream stream{openOrThrow(fileName, std::ios::in | std::ios::binary)};
  while (!stream.eof()) {
    stream.read(buffer.data(), buffer.size());
    hasher.update(llvm::StringRef(buffer.data(), stream.gcount()));
  }
  return MD5Hash{hasher.result().words()};
} catch (...) {
  return MD5Hash{}; // Zero
}

MD5Hash MD5Hash::hashMemory(const void *mem, size_t memSize) noexcept {
  llvm::MD5::MD5Result result{llvm::MD5::hash(
      llvm::ArrayRef<uint8_t>{static_cast<const uint8_t *>(mem), memSize})};
  return MD5Hash{result.words()};
}

MD5Hash::operator std::string() const {
  std::array<uint8_t, 16> bytes{};
  llvm::support::endian::write64le(&bytes[0], getLowerBits());
  llvm::support::endian::write64le(&bytes[8], getUpperBits());
  return llvm::toHex(llvm::ArrayRef<uint8_t>{bytes.data(), 16},
                     /*LowerCase=*/true);
}

const MD5FileHash *MD5FileHasher::operator[](const std::string &fileName) {
  std::string canonicalFileName{makePathCanonical(fileName)};
  auto [nameItr, nameInserted] =
      mFileHashesByName.try_emplace(canonicalFileName);
  if (nameInserted) {
    MD5Hash hash{MD5Hash::hashFile(canonicalFileName)};
    // Identical files at different paths share one entry. The zero hash
    // means the file was unreadable; keep those per-path so distinct
    // broken files are not conflated.
    std::unique_ptr<MD5FileHash> &fileHash{mFileHashes[std::pair(
        hash, !hash ? canonicalFileName : std::string())]};
    if (!fileHash) fileHash = std::make_unique<MD5FileHash>();
    fileHash->hash = hash;
    fileHash->canonicalFileNames.push_back(canonicalFileName);
    nameItr->second = fileHash.get();
  }
  return nameItr->second;
}

} // namespace smdl
