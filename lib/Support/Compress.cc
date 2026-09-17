#include "smdl/Support/Compress.h"
#include "smdl/Support/Error.h"
#include "smdl/Support/Strings.h"

#include "../thirdparty/miniz.h"

// The one-shot zlib codec every format in the project deflates through.
// The buffers are whole files held in memory anyway, so there is no
// streaming form and no partial progress to report: a call either
// produces the bytes or throws.

namespace smdl {

std::vector<std::byte> compressBytes(Span<const std::byte> bytes, int level) {
  if (level < 0 || level > 9)
    throw Error(concat("Deflate level ", level, " is out of range [0, 9]"));
  std::vector<std::byte> result(mz_compressBound(mz_ulong(bytes.size())));
  mz_ulong resultSize{mz_ulong(result.size())};
  const int status{mz_compress2(
      reinterpret_cast<unsigned char *>(result.data()), &resultSize,
      reinterpret_cast<const unsigned char *>(bytes.data()),
      mz_ulong(bytes.size()), level)};
  if (status != MZ_OK)
    throw Error(concat("Cannot deflate ", SpellByteSize(bytes.size()), ": ",
                       mz_error(status)));
  result.resize(resultSize);
  return result;
}

void decompressBytesInto(Span<std::byte> dest, Span<const std::byte> bytes) {
  mz_ulong destSize{mz_ulong(dest.size())};
  // miniz reads a zero-length destination as an empty buffer whatever
  // the pointer is, so the empty case needs only the stream itself to
  // be well formed, which an empty deflate stream cannot be.
  if (bytes.empty()) throw Error("Cannot inflate an empty stream");
  const int status{
      mz_uncompress(reinterpret_cast<unsigned char *>(dest.data()), &destSize,
                    reinterpret_cast<const unsigned char *>(bytes.data()),
                    mz_ulong(bytes.size()))};
  if (status != MZ_OK)
    throw Error(concat("Cannot inflate ", SpellByteSize(bytes.size()), ": ",
                       mz_error(status)));
  if (destSize != dest.size())
    throw Error(concat("Cannot inflate ", SpellByteSize(bytes.size()),
                       ": it holds ", SpellByteSize(destSize), " where ",
                       SpellByteSize(dest.size()), " were expected"));
}

} // namespace smdl
