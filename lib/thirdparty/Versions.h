#pragma once

// Hand-maintained version strings for the vendored libraries
// that publish no usable version macro: the stb headers state their version
// only in a leading comment, and tinyexr's 'EXR_VERSION_*' macros still
// read 3.0.0 in its v3.2.0 release. Re-vendoring a file must update its
// macro here in the same change.
#define SMDL_STB_IMAGE_VERSION "2.30"
#define SMDL_STB_IMAGE_WRITE_VERSION "1.16"
#define SMDL_STB_IMAGE_RESIZE_VERSION "2.18"
#define SMDL_STB_SPRINTF_VERSION "1.10"
#define SMDL_TINYEXR_VERSION "3.2.0"
