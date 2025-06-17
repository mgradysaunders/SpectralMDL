#pragma once

#include <map>

#include "smdl/common.h"

namespace smdl {

/// \addtogroup Support
/// \{

/// A 128-bit MD5 hash.
class SMDL_EXPORT MD5Hash final {
public:
  /// Hash file on disk. Returns zero if there is an error.
  [[nodiscard]] static MD5Hash hash_file(const std::string &fileName) noexcept;

  /// Hash memory.
  [[nodiscard]] static MD5Hash hash_memory(const void *mem,
                                           size_t memSize) noexcept;

  /// Hash memory.
  [[nodiscard]] static MD5Hash hash_memory(std::string_view mem) noexcept {
    return hash_memory(mem.data(), mem.size());
  }

public:
  /// Construct zero.
  constexpr MD5Hash() = default;

  /// Construct from hash code.
  constexpr MD5Hash(std::pair<uint64_t, uint64_t> hash) : hash(hash) {}

  /// Get the upper or most significant bits.
  [[nodiscard]] constexpr uint64_t upper_bits() const noexcept {
    return hash.first;
  }

  /// Get the lower or least significant bits.
  [[nodiscard]] constexpr uint64_t lower_bits() const noexcept {
    return hash.second;
  }

  /// Wrap `operator==`.
  [[nodiscard]] constexpr bool operator==(const MD5Hash &other) const noexcept {
    return hash == other.hash;
  }

  /// Wrap `operator!=`.
  [[nodiscard]] constexpr bool operator!=(const MD5Hash &other) const noexcept {
    return hash != other.hash;
  }

  /// Wrap `operator<`.
  [[nodiscard]] constexpr bool operator<(const MD5Hash &other) const noexcept {
    return hash < other.hash;
  }

  /// Wrap `operator>`.
  [[nodiscard]] constexpr bool operator>(const MD5Hash &other) const noexcept {
    return hash > other.hash;
  }

  /// Wrap `operator<=`.
  [[nodiscard]] constexpr bool operator<=(const MD5Hash &other) const noexcept {
    return hash <= other.hash;
  }

  /// Wrap `operator>=`.
  [[nodiscard]] constexpr bool operator>=(const MD5Hash &other) const noexcept {
    return hash >= other.hash;
  }

  /// Wrap `operator!`.
  [[nodiscard]] constexpr bool operator!() const noexcept {
    return hash == std::pair<uint64_t, uint64_t>();
  }

  /// Stringify for display.
  [[nodiscard]] operator std::string() const;

public:
  /// The hash code.
  std::pair<uint64_t, uint64_t> hash{};
};

/// An MD5 file hash.
class SMDL_EXPORT MD5FileHash final {
public:
  /// The hash code.
  MD5Hash hash{};

  /// The file names that produced this hash code (presumably all duplicates of
  /// the same file).
  std::vector<std::string> canonicalFileNames{};
};

/// An MD5 file hasher.
///
/// This caches the `MD5FileHash` for every file that is hashed, so that we
/// do not have to calculate hashes redundantly.
///
class SMDL_EXPORT MD5FileHasher final {
public:
  MD5FileHasher() = default;

  MD5FileHasher(const MD5FileHasher &) = delete;

  /// Hash.
  [[nodiscard]] const MD5FileHash *operator[](const std::string &fileName);

private:
  /// The file hashes.
  std::map<std::string, MD5FileHash> fileHashes{};
};

/// \}

} // namespace smdl
