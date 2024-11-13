#pragma once

#include "smdl/type.h"

namespace smdl {

/// This represents the primary API for loading image file formats. By default, SpectralMDL does not
/// want to enforce the usage of any specific library, i.e., we do not require OpenImageIO or OpenEXR.
using ImageLoader = std::function<Image(const std::filesystem::path &fname)>;

[[nodiscard]] SMDL_EXPORT Image default_image_loader(const std::filesystem::path &fname);

} // namespace smdl
