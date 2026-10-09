/// \file
#pragma once

#include <cstddef>
#include <vector>

#include "smdl/Export.h"
#include "smdl/Support/Span.h"

namespace smdl {

/// \addtogroup support
/// \{

/// \name Functions (compression)
/// \{

/// Deflate into a zlib stream.
///
/// \param[in] bytes  The bytes to compress.
/// \param[in] level  The deflate level, 0 (stored) through 9 (smallest).
///
/// \throws Error  If the level is out of range or the deflate fails.
///
[[nodiscard]] SMDL_EXPORT std::vector<std::byte>
compressBytes(Span<const std::byte> bytes, int level = 6);

/// Inflate a zlib stream produced by `compressBytes()`.
///
/// There is no size-discovering form on purpose: every caller reaches
/// here holding a header that says exactly how many bytes the stream
/// must inflate to, so `dest` is sized by the caller and a stream that
/// says otherwise is corrupt.
///
/// \param[out] dest   The destination, sized to the exact inflated size.
/// \param[in]  bytes  The zlib stream.
///
/// \throws Error  If the stream is corrupt or inflates to any other size.
///
SMDL_EXPORT void decompressBytesInto(Span<std::byte> dest,
                                     Span<const std::byte> bytes);

/// \}

/// \}

} // namespace smdl
