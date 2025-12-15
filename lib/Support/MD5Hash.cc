#include "smdl/Support/MD5Hash.h"
#include "smdl/Support/Filesystem.h"

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/MD5.h"

namespace smdl {

MD5Hash MD5Hash::hashFile(const std::string &fileName) noexcept try {
  auto hasher{llvm::MD5()};
  auto buffer{std::array<char, 128>{}};
  auto stream{openOrThrow(fileName, std::ios::in | std::ios::binary)};
  while (!stream.eof()) {
    stream.read(buffer.data(), buffer.size());
    hasher.update(llvm::StringRef(buffer.data(), stream.gcount()));
  }
  return MD5Hash{hasher.result().words()};
} catch (...) {
  return MD5Hash{}; // Zero
}

MD5Hash MD5Hash::hashMemory(const void *mem, size_t memSize) noexcept {
  auto result{llvm::MD5::hash(
      llvm::ArrayRef<uint8_t>{static_cast<const uint8_t *>(mem), memSize})};
  return MD5Hash{result.words()};
}

MD5Hash::operator std::string() const {
  auto bytes{std::array<uint8_t, 16>{}};
  llvm::support::endian::write64le(&bytes[0], getLowerBits());
  llvm::support::endian::write64le(&bytes[8], getUpperBits());
  return llvm::toHex(llvm::ArrayRef<uint8_t>{bytes.data(), 16},
                     /*LowerCase=*/true);
}

const MD5FileHash *MD5FileHasher::operator[](const std::string &fileName) {
  auto canonicalFileName{makePathCanonical(fileName)};
  auto [itr, inserted] = fileHashes.try_emplace(canonicalFileName);
  auto &fileHash{itr->second};
  if (inserted) {
    fileHash.hash = MD5Hash::hashFile(canonicalFileName);
    fileHash.canonicalFileNames.push_back(canonicalFileName);
  }
  return &fileHash;
}

} // namespace smdl
