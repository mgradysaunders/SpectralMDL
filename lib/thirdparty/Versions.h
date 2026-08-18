#pragma once

// Hand-maintained version strings for the vendored single-file libraries
// that publish no usable version macro: the stb headers state their version
// only in a leading comment, and tinyexr states nothing at all. Re-vendoring
// a file must update its macro here in the same change.
#define SMDL_STB_IMAGE_VERSION "2.30"
#define SMDL_STB_IMAGE_WRITE_VERSION "1.16"
#define SMDL_STB_IMAGE_RESIZE_VERSION "2.18"
#define SMDL_TINYEXR_VERSION "unversioned (vendored 2025-05)"
