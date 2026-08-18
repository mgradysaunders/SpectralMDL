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

std::string_view getSimilarIntrinsicName(std::string_view name) {
  static constexpr std::string_view names[]{
#define SMDL_INTRINSIC(Enumerator, Spelling) Spelling,
#include "Intrinsics.def"
  };
  // Only ever suggest a name that is a small number of edits away, so that a
  // wildly wrong spelling produces no hint instead of a misleading one. The
  // threshold scales with the length of what was typed, because one edit in
  // '#abs' is a much bigger relative error than one edit in
  // '#loadBSDFMeasurement'.
  return suggestNearest(name, {names, std::size(names)},
                        std::min<size_t>(1 + name.size() / 4, 4));
}

} // namespace smdl
