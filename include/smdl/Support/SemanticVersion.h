/// \file
#pragma once

#include <cstdint>
#include <string>

#include "smdl/Export.h"

namespace smdl {

/// \addtogroup Support
/// \{

/// A semantic version per [Semantic Versioning 2.0.0](https://semver.org).
class SMDL_EXPORT SemanticVersion final {
public:
  /// Parse from string representation or throw an `Error`.
  [[nodiscard]] static SemanticVersion parse(const std::string &versionStr);

  /// Has pre-release version?
  [[nodiscard]] bool has_pre_release() const noexcept {
    return !preRelease.empty();
  }

  /// Has build metadata?
  [[nodiscard]] bool has_build_metadata() const noexcept {
    return !buildMetadata.empty();
  }

  /// Compare result.
  enum CompareResult : int {
    OLDER_BY_MAJOR_VERSION = -4,
    OLDER_BY_MINOR_VERSION = -3,
    OLDER_BY_PATCH_VERSION = -2,
    OLDER_BY_PRE_RELEASE = -1,
    EXACTLY_EQUAL = 0,
    NEWER_BY_PRE_RELEASE = +1,
    NEWER_BY_PATCH_VERSION = +2,
    NEWER_BY_MINOR_VERSION = +3,
    NEWER_BY_MAJOR_VERSION = +4,
  };

  /// Compare.
  [[nodiscard]] CompareResult
  compare(const SemanticVersion &other) const noexcept;

  /// \name Comparison operators
  /// \{

  [[nodiscard]] bool operator==(const SemanticVersion &other) const noexcept {
    return compare(other) == EXACTLY_EQUAL;
  }

  [[nodiscard]] bool operator!=(const SemanticVersion &other) const noexcept {
    return compare(other) != EXACTLY_EQUAL;
  }

  [[nodiscard]] bool operator<(const SemanticVersion &other) const noexcept {
    return compare(other) < EXACTLY_EQUAL;
  }

  [[nodiscard]] bool operator>(const SemanticVersion &other) const noexcept {
    return compare(other) > EXACTLY_EQUAL;
  }

  [[nodiscard]] bool operator<=(const SemanticVersion &other) const noexcept {
    return compare(other) <= EXACTLY_EQUAL;
  }

  [[nodiscard]] bool operator>=(const SemanticVersion &other) const noexcept {
    return compare(other) >= EXACTLY_EQUAL;
  }

  /// \}

  /// Convert to string representation.
  [[nodiscard]] operator std::string() const;

public:
  /// The major version number.
  uint32_t major{};

  /// The minor version number.
  uint32_t minor{};

  /// The patch version number.
  uint32_t patch{};

  /// The pre-release version.
  ///
  /// > A pre-release version MAY be denoted by appending a hyphen
  /// > and a series of dot separated identifiers immediately following
  /// > the patch version.
  ///
  std::string preRelease{};

  /// The build metadata.
  ///
  /// > Build metadata MAY be denoted by appending a plus sign and a
  /// > series of dot separated identifiers immediately following the
  /// > patch or pre-release version.
  ///
  std::string buildMetadata{};
};

/// \}

} // namespace smdl
