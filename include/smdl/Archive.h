#pragma once

#include "smdl/common.h"

namespace smdl {

/// Test if the given file is an MDLE.
[[nodiscard]] SMDL_EXPORT bool
is_file_MDLE(const std::string &fileName) noexcept;

/// Test if the given file is an MDR.
[[nodiscard]] SMDL_EXPORT bool
is_file_MDR(const std::string &fileName) noexcept;

/// An encapsulated material with extension `.mdle`.
class SMDL_EXPORT ArchiveMDLE final {
public:
  ArchiveMDLE() = default;

  ArchiveMDLE(const ArchiveMDLE &) = delete;

  /// Load from file.
  ///
  /// \note
  /// This is not exception-safe, and there are no guarantees about
  /// what it may or may not throw on failure.
  ///
  [[nodiscard]] static std::unique_ptr<ArchiveMDLE>
  load_from_file(const std::string &fileName);

  class Resource final {
  public:
    /// The file name in the archive, must start with `"resources/"`.
    std::string fileName{};

    /// The file memory.
    std::string file{};
  };

public:
  /// The main file `main.mdl`.
  std::string mainFile{};

  /// The resources.
  std::vector<Resource> resources{};
};

/// An archive with extension `.mdr`.
class SMDL_EXPORT ArchiveMDR final {
public:
  ArchiveMDR() = default;

  ArchiveMDR(const ArchiveMDR &) = delete;

private:
  // TODO
};

} // namespace smdl
