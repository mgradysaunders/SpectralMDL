#include "Intrinsics.h"

#include <algorithm>
#include <iterator>

#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringSwitch.h"

#include "smdl/Support/StringHelpers.h"

namespace smdl {

IntrinsicID getIntrinsicByName(std::string_view name) {
  return llvm::StringSwitch<IntrinsicID>(llvm::StringRef(name))
#define SMDL_INTRINSIC(Enumerator, Spelling) \
  .Case(Spelling, IntrinsicID::Enumerator)
#include "Intrinsics.def"
      .Default(IntrinsicID::Invalid);
}

std::string_view getIntrinsicName(IntrinsicID intrID) {
  switch (intrID) {
  case IntrinsicID::Invalid:
    return {};
#define SMDL_INTRINSIC(Enumerator, Spelling) \
  case IntrinsicID::Enumerator:              \
    return Spelling;
#include "Intrinsics.def"
  }
  return {};
}

Span<const std::string_view> getAllIntrinsicNames() {
  static constexpr std::string_view names[]{
#define SMDL_INTRINSIC(Enumerator, Spelling) Spelling,
#include "Intrinsics.def"
  };
  return {names, std::size(names)};
}

std::string_view getSimilarIntrinsicName(std::string_view name) {
  // Only ever suggest a name that is a small number of edits away, so that a
  // wildly wrong spelling produces no hint instead of a misleading one.
  return suggestNearestName(name, getAllIntrinsicNames());
}

} // namespace smdl
