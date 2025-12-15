#include "smdl/Support/SemanticVersion.h"

#include "smdl/Common.h"

#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"

namespace smdl {

SemanticVersion SemanticVersion::parse(const std::string &versionStr) {
  auto version{SemanticVersion{}};
  auto success{[&]() {
    auto src{llvm::StringRef(versionStr).trim()};
    if (src.consumeInteger(10, version.major) || !src.consume_front(".") ||
        src.consumeInteger(10, version.minor) || !src.consume_front(".") ||
        src.consumeInteger(10, version.patch))
      return false;
    if (src.consume_front("-")) {
      auto [src0, src1] = src.split('+');
      version.preRelease = std::string(src0);
      version.buildMetadata = std::string(src1);
    } else if (src.consume_front("+")) {
      version.buildMetadata = std::string(src);
    }
    return true;
  }()};
  if (!success)
    throw Error(concat("invalid version string ", Quoted(versionStr)));
  return version;
}

SemanticVersion::CompareResult
SemanticVersion::compare(const SemanticVersion &other) const noexcept {
  const SemanticVersion &lhs{*this};
  const SemanticVersion &rhs{other};
  // 1. Precedence MUST be calculated by separating the version into major,
  //    minor, patch, and pre-release identifiers in that order (Build
  //    metadata does not figure into precedence).
  // 2. Precedence is determined by the first difference when comparing each
  //    of these identifiers from left to right as follows: Major, minor, and
  //    patch version are always compared numerically
  if (lhs.major != rhs.major)
    return lhs.major > rhs.major ? NEWER_BY_MAJOR_VERSION
                                 : OLDER_BY_MAJOR_VERSION;
  if (lhs.minor != rhs.minor)
    return lhs.minor > rhs.minor ? NEWER_BY_MINOR_VERSION
                                 : OLDER_BY_MINOR_VERSION;
  if (lhs.patch != rhs.patch)
    return lhs.patch > rhs.patch ? NEWER_BY_PATCH_VERSION
                                 : OLDER_BY_PATCH_VERSION;
  if (lhs.has_pre_release() || rhs.has_pre_release()) {
    // 3. When major, minor, and patch are equal, a pre-release version
    //    has lower precedence than a normal version.
    if (!lhs.has_pre_release())
      return NEWER_BY_PRE_RELEASE;
    if (!rhs.has_pre_release())
      return OLDER_BY_PRE_RELEASE;
    // 4. Precedence for two pre-release versions with the same major,
    //    minor, and patch version MUST be determined by comparing each
    //    dot separated identifier from left to right until a difference is
    //    found as follows:
    //    1. Identifiers consisting of only digits are compared
    //       numerically
    //    2. Identifiers with letters or hyphens are compared lexically in
    //       ASCII sort order
    //    3. Numeric identifiers always have lower precedence than non-numeric
    //       identifiers
    //    4. A larger set of pre-release fields has a higher precedence than
    //       a smaller set, if all of the preceding identifiers are equal.
    auto lhsIdents{llvm::SmallVector<llvm::StringRef>{}};
    auto rhsIdents{llvm::SmallVector<llvm::StringRef>{}};
    llvm::StringRef(lhs.preRelease).split(lhsIdents, '.');
    llvm::StringRef(rhs.preRelease).split(rhsIdents, '.');
    for (size_t i = 0; i < size_t(std::min(lhsIdents.size(), rhsIdents.size()));
         i++) {
      uint32_t lhsNum{};
      uint32_t rhsNum{};
      bool lhsIsNumeric{!lhsIdents[i].getAsInteger(10, lhsNum)};
      bool rhsIsNumeric{!rhsIdents[i].getAsInteger(10, rhsNum)};
      if (lhsIsNumeric && rhsIsNumeric) {
        if (lhsNum != rhsNum)
          return lhsNum > rhsNum ? NEWER_BY_PRE_RELEASE : OLDER_BY_PRE_RELEASE;
      } else if (!lhsIsNumeric && rhsIsNumeric) {
        return NEWER_BY_PRE_RELEASE;
      } else if (lhsIsNumeric && !rhsIsNumeric) {
        return OLDER_BY_PRE_RELEASE;
      } else {
        int result{lhsIdents[i].compare(rhsIdents[i])};
        if (result != 0)
          return result > 0 ? NEWER_BY_PRE_RELEASE : OLDER_BY_PRE_RELEASE;
      }
    }
    if (lhsIdents.size() != rhsIdents.size()) {
      return lhsIdents.size() > rhsIdents.size() ? NEWER_BY_PRE_RELEASE
                                                 : OLDER_BY_PRE_RELEASE;
    }
  }
  return EXACTLY_EQUAL;
}

SemanticVersion::operator std::string() const {
  auto str{std::string()};
  str += std::to_string(major), str += '.';
  str += std::to_string(minor), str += '.';
  str += std::to_string(patch);
  if (!preRelease.empty()) {
    str += '-';
    str += preRelease;
  }
  if (!buildMetadata.empty()) {
    str += '+';
    str += buildMetadata;
  }
  return str;
}

} // namespace smdl
