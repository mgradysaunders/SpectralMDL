/// \file
#pragma once

#include <string_view>

#include "smdl/Support/Span.h"

namespace smdl {

/// \addtogroup Compiler
/// \{

/// The identity of an SMDL intrinsic, i.e., a `#name` expression.
///
/// The enumerators are generated from `Intrinsics.def`, which is the single
/// registry of intrinsic names. Dispatch in `Emitter::emitIntrinsic()` is by
/// enumerator rather than by string, so that renaming an intrinsic is a
/// one-line edit to the table and so that the set of intrinsics is
/// enumerable, which the string dispatch it replaced was not.
enum class IntrinsicID {
  /// Not an intrinsic. The result of looking up an unknown name.
  Invalid,
#define SMDL_INTRINSIC(Enumerator, Spelling) Enumerator,
#include "Intrinsics.def"
};

/// Look up the intrinsic with the given name, which must not include the
/// leading `#`. Returns `IntrinsicID::Invalid` if there is no such intrinsic.
[[nodiscard]] IntrinsicID getIntrinsicByName(std::string_view name);

/// The canonical source spelling of the given intrinsic, without the leading
/// `#`. Returns an empty view for `IntrinsicID::Invalid`.
[[nodiscard]] std::string_view getIntrinsicName(IntrinsicID intrID);

/// Every intrinsic's canonical source spelling, without the leading `#`,
/// for did-you-mean suggestions that weigh intrinsics against the other
/// names in scope.
[[nodiscard]] Span<const std::string_view> getAllIntrinsicNames();

/// The name of the intrinsic most similar to `name`, for use in a
/// "did you mean" hint after `getIntrinsicByName()` fails. Returns an empty
/// view if nothing is close enough to be worth suggesting.
[[nodiscard]] std::string_view getSimilarIntrinsicName(std::string_view name);

/// \}

} // namespace smdl
