#pragma once

#include "smdl/type.h"

namespace smdl {

[[nodiscard]] SMDL_EXPORT Image ImageLoader(const std::filesystem::path &fname);

} // namespace smdl
