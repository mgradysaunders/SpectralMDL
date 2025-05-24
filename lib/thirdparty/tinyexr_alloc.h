#pragma once

#if !defined(TINYEXR_MALLOC) && !defined(TINYEXR_CALLOC) &&                    \
    !defined(TINYEXR_FREE)
#include <cstdlib>
#define TINYEXR_MALLOC(sz) std::malloc(sz)
#define TINYEXR_CALLOC(n, sz) std::calloc(n, sz)
#define TINYEXR_FREE(p) std::free(p)
#elif defined(TINYEXR_MALLOC) && defined(TINYEXR_CALLOC) &&                    \
    defined(TINYEXR_FREE)
// OK
#else
#error "Define all or none of TINYEXR_MALLOC, TINYEXR_CALLOC, and TINYEXR_FREE"
#endif
