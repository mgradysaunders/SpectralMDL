#include "smdl/MD5Hash.h"

#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/MD5.h"

namespace smdl {

MD5Hash MD5Hash::hash_file(const std::string &fileName) noexcept try {
  auto hasher{llvm::MD5()};
  auto buffer{std::array<char, 128>{}};
  auto stream{open_or_throw(fileName, std::ios::in | std::ios::binary)};
  while (!stream.eof()) {
    stream.read(buffer.data(), buffer.size());
    hasher.update(llvm::StringRef(buffer.data(), stream.gcount()));
  }
  return MD5Hash{hasher.result().words()};
} catch (...) {
  return MD5Hash{}; // Zero
}

MD5Hash MD5Hash::hash_memory(const void *mem, size_t memSize) noexcept {
  auto result{llvm::MD5::hash(
      llvm::ArrayRef<uint8_t>{static_cast<const uint8_t *>(mem), memSize})};
  return MD5Hash{result.words()};
}

MD5Hash::operator std::string() const {
  auto bytes{std::array<uint8_t, 16>{}};
  llvm::support::endian::write64le(&bytes[0], lower_bits());
  llvm::support::endian::write64le(&bytes[8], upper_bits());
  return llvm::toHex(llvm::ArrayRef<uint8_t>{bytes.data(), 16},
                     /*LowerCase=*/true);
}

const MD5FileHash *MD5FileHasher::operator[](const std::string &fileName) {
  auto canonicalFileName{canonical(fileName)};
  auto [itr, inserted] = fileHashes.try_emplace(canonicalFileName);
  auto &fileHash{itr->second};
  if (inserted) {
    fileHash.hash = MD5Hash::hash_file(canonicalFileName);
    fileHash.canonicalFileNames.push_back(canonicalFileName);
  }
  return &fileHash;
}

} // namespace smdl
