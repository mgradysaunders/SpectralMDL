/// \file
/// The shared mechanics of the renderer's own binary formats.
///
/// `.places`, `.curves` and `.sdtree` are three different formats with
/// three different specs (see `PlacesFile.h`, `CurvesFile.h` and
/// `STree::writeFile()`), but they are read and written the same way:
/// a fixed-layout header beginning with an eight-byte magic, then
/// records and bulk arrays written as the host's own bytes.
///
/// `.places` and `.curves` may deflate everything after their header,
/// which streaming cannot do, so those two go through the buffer half
/// below: the whole file is read at once, the payload is inflated to the
/// size its header states, and `ByteReader` walks it. The stream half
/// above it is what `.sdtree`, which has no compressed form, still
/// uses.
///
/// That last part is why the little-endian requirement is a hard check
/// rather than a byte-order conversion: writing the host's bytes is what
/// lets a million-point groom read in one `memcpy` instead of a million
/// field decodes, and the price is that a big-endian port has to fail
/// loudly here. Every format states the same requirement, so it is
/// stated once.
#pragma once

#include <cstddef>
#include <cstdint>
#include <cstring>
#include <istream>
#include <ostream>
#include <string>
#include <string_view>
#include <vector>

#include "smdl/Support/Compress.h"
#include "smdl/Support/Error.h"
#include "smdl/Support/Macros.h"
#include "smdl/Support/Span.h"
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

/// Append one fixed-layout record. The caller's `static_assert` on its
/// size is what pins the layout down, exactly as for `putRecord()`.
template <typename T>
inline void pushRecord(std::vector<std::byte> &bytes, const T &record) {
  const auto *first{reinterpret_cast<const std::byte *>(&record)};
  bytes.insert(bytes.end(), first, first + sizeof(record));
}

/// Append a whole array as bulk bytes.
template <typename T>
inline void pushArray(std::vector<std::byte> &bytes,
                      const std::vector<T> &values) {
  if (values.empty()) return;
  const auto *first{reinterpret_cast<const std::byte *>(values.data())};
  bytes.insert(bytes.end(), first, first + sizeof(T) * values.size());
}

/// A cursor over a payload already in memory: the buffer counterpart of
/// `getRecord()` and `getArray()`.
///
/// A read past the end leaves the cursor failed and every later read a
/// no-op, so a caller checks `ok()` once at the end where its own
/// diagnostic belongs, exactly as it checks a stream's failure bit.
class ByteReader final {
public:
  explicit ByteReader(smdl::Span<const std::byte> bytes) : mBytes(bytes) {}

  /// Read one fixed-layout record.
  template <typename T> void takeRecord(T &record) {
    if (const std::byte *first{take(sizeof(T))})
      std::memcpy(&record, first, sizeof(T));
  }

  /// Read `count` elements into `values`, which is sized to match.
  ///
  /// \note
  /// `count` comes off a header, so the caller must have checked it
  /// against the payload the header also states before allocating here.
  template <typename T> void takeArray(std::vector<T> &values, size_t count) {
    if (const std::byte *first{take(sizeof(T) * count)}) {
      values.resize(count);
      if (count != 0) std::memcpy(values.data(), first, sizeof(T) * count);
    }
  }

  /// Did every read fit?
  [[nodiscard]] bool ok() const noexcept { return mIsOk; }

  /// Is everything read, with nothing left over?
  [[nodiscard]] bool empty() const noexcept {
    return mIsOk && mOffset == mBytes.size();
  }

private:
  /// The next `size` bytes, or null once the cursor has failed.
  [[nodiscard]] const std::byte *take(size_t size) noexcept {
    if (!mIsOk || size > mBytes.size() - mOffset) {
      mIsOk = false;
      return nullptr;
    }
    const std::byte *first{mBytes.data() + mOffset};
    mOffset += size;
    return first;
  }

  smdl::Span<const std::byte> mBytes{};
  size_t mOffset{};
  bool mIsOk{true};
};

/// The payload of a compressible binary format: everything in `contents`
/// after `offset`, inflated when `isCompressed` says it is deflated, and
/// required to be exactly `size` bytes either way.
///
/// An uncompressed payload is a view into the contents the caller still
/// owns, so the common case costs no second copy of a groom. `what`
/// names the file in any message.
///
/// \throws smdl::Error  If the file is short, holds trailing bytes, or
///                      the stream does not inflate to `size`.
///
class BinaryPayload final {
public:
  BinaryPayload(std::string_view contents, size_t offset, size_t size,
                bool isCompressed, const std::string &what) {
    const auto fail{[&](auto &&...args) {
      throw smdl::Error(
          smdl::concat("Cannot read ", smdl::QuotedPath(what), ": ", args...));
    }};
    if (offset > contents.size()) fail("truncated (the header does not fit)");
    const std::string_view rest{contents.substr(offset)};
    const auto *first{reinterpret_cast<const std::byte *>(rest.data())};
    if (!isCompressed) {
      if (rest.size() != size)
        fail("it holds ", smdl::Bytes(rest.size()),
             " where its header "
             "promises ",
             smdl::Bytes(size));
      mBytes = smdl::Span<const std::byte>(first, size);
      return;
    }
    mInflated.resize(size);
    try {
      smdl::decompressBytesInto(
          mInflated, smdl::Span<const std::byte>(first, rest.size()));
    } catch (const smdl::Error &error) {
      fail(error.message);
    }
    mBytes = mInflated;
  }

  [[nodiscard]] smdl::Span<const std::byte> bytes() const noexcept {
    return mBytes;
  }

private:
  std::vector<std::byte> mInflated{};
  smdl::Span<const std::byte> mBytes{};
};
