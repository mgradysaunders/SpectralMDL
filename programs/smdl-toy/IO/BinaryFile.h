/// \file
/// The shared mechanics of the renderer's own binary formats.
///
/// `.places`, `.curves` and `.sdtree` are three different formats with
/// three different specs (see `PlacesFile.h`, `CurvesFile.h` and
/// `STree::writeFile()`), but they are read and written the same way:
/// a fixed-layout header beginning with an eight-byte magic, then
/// records and bulk arrays written as the host's own bytes.
///
/// That last part is why the little-endian requirement is a hard check
/// rather than a byte-order conversion: writing the host's bytes is what
/// lets a million-point groom read in one `memcpy` instead of a million
/// field decodes, and the price is that a big-endian port has to fail
/// loudly here. Every format states the same requirement, so it is
/// stated once.
#pragma once

#include <cstdint>
#include <cstring>
#include <istream>
#include <ostream>
#include <string_view>
#include <vector>

#include "smdl/Support/Error.h"
#include "smdl/Support/Macros.h"
#include "smdl/Support/Strings.h"

/// Throw unless the host is little-endian; `what` names the format in
/// the message, e.g. `"'.curves'"`.
///
/// \throws smdl::Error on a big-endian host.
inline void requireLittleEndianHost(std::string_view what) {
  const uint32_t probe{1};
  if (*reinterpret_cast<const unsigned char *>(&probe) != 1)
    throw smdl::Error(smdl::concat(what, " I/O requires a little-endian host"));
}

/// Write one fixed-layout record. The caller's `static_assert` on its
/// size is what pins the layout down; nothing here can check it.
template <typename T>
inline void putRecord(std::ostream &stream, const T &record) {
  stream.write(reinterpret_cast<const char *>(&record), sizeof(record));
}

/// Read one fixed-layout record. The stream's failure bit is the only
/// report; the caller checks it where its own diagnostic belongs.
template <typename T> inline void getRecord(std::istream &stream, T &record) {
  stream.read(reinterpret_cast<char *>(&record), sizeof(record));
}

/// Write a whole array as bulk bytes.
template <typename T>
inline void putArray(std::ostream &stream, const std::vector<T> &values) {
  if (values.empty()) return;
  stream.write(reinterpret_cast<const char *>(values.data()),
               std::streamsize(sizeof(T) * values.size()));
}

/// Read `count` elements into `values`, which is sized to match.
///
/// \note
/// `count` comes off a header, so a corrupt one allocates whatever it
/// says before the read fails. Bound it first where the format gives
/// something to bound it against, as the guide tree's node count is.
template <typename T>
inline void getArray(std::istream &stream, std::vector<T> &values,
                     size_t count) {
  values.resize(count);
  if (values.empty()) return;
  stream.read(reinterpret_cast<char *>(values.data()),
              std::streamsize(sizeof(T) * values.size()));
}

/// Does the header's magic field hold `magic`?
template <size_t N>
[[nodiscard]] inline bool hasMagic(const char (&field)[N],
                                   std::string_view magic) noexcept {
  return magic.size() == N &&
         std::memcmp(field, magic.data(), magic.size()) == 0;
}

/// Stamp `magic` into a header's magic field.
template <size_t N>
inline void setMagic(char (&field)[N], std::string_view magic) noexcept {
  SMDL_SANITY_CHECK(magic.size() == N);
  std::memcpy(field, magic.data(), magic.size());
}
