#pragma once

#include "smdl/types.h"

namespace smdl {

[[nodiscard]] SMDL_EXPORT Image ImageLoader(const std::filesystem::path &fname);

} // namespace smdl
