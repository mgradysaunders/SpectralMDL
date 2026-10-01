#if defined(_WIN32)
#ifndef NOMINMAX
#define NOMINMAX
#endif
#endif

#include "smdl/Resource/Image.h"

#include <array>
#include <fstream>
#include <memory>
#include <mutex>

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-function"
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

extern "C" {
#define STBI_ASSERT(X) ((void)0)
#define STBI_MALLOC(sz) ::smdl::Image::image_malloc(sz)
#define STBI_REALLOC(p, newsz) ::smdl::Image::image_realloc(p, newsz)
#define STBI_FREE(p) ::smdl::Image::image_free(p)
#define STBI_ONLY_JPEG 1
#define STBI_ONLY_PNG 1
#define STBI_ONLY_TGA 1
#define STBI_ONLY_BMP 1
#define STBI_ONLY_PNM 1
#define STBI_ONLY_HDR 1
#define STB_IMAGE_STATIC 1
#define STB_IMAGE_IMPLEMENTATION 1
#include "thirdparty/stb/stb_image.h"

#define STBIW_ASSERT(X) ((void)0)
#define STBIW_MALLOC(sz) ::smdl::Image::image_malloc(sz)
#define STBIW_REALLOC(p, newsz) ::smdl::Image::image_realloc(p, newsz)
#define STBIW_FREE(p) ::smdl::Image::image_free(p)
#define STB_IMAGE_WRITE_STATIC 1
#define STB_IMAGE_WRITE_IMPLEMENTATION 1
#include "thirdparty/stb/stb_image_write.h"
} // extern "C"

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic pop
#endif

// NOTE: Outside the 'extern "C"' block above because the implementation
// pulls in SIMD intrinsics headers.
#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-function"
#endif
#define STBIR_ASSERT(X) ((void)0)
#define STBIR_MALLOC(sz, user) ::smdl::Image::image_malloc(sz)
#define STBIR_FREE(p, user) ::smdl::Image::image_free(p)
#define STB_IMAGE_RESIZE_STATIC 1
#define STB_IMAGE_RESIZE_IMPLEMENTATION 1
#include "thirdparty/stb/stb_image_resize2.h"
#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic pop
#endif

#include "thirdparty/tinyexr/exr.h"

// NOTE: Not in 'exr.h', but tinyexr's own test hook for the B44 tables,
// and the only way to set them up ahead of a decode. See 'warmUpEXR()'.
extern "C" void exr_b44_debug_tables(const uint16_t **exp_tbl,
                                     const uint16_t **log_tbl);

namespace smdl {

namespace {
// The reason stb_image recorded for its last failure on this thread.
[[nodiscard]] std::string stbFailureReason() {
  const char *reason{stbi_failure_reason()};
  return reason ? reason : "unknown error";
}

// Routes tinyexr's allocations through the same hooks as stb's.
const exr_allocator EXRAllocator{
    /*user=*/nullptr,
    [](void *, size_t size) { return Image::image_malloc(size); },
    [](void *, void *ptr) { Image::image_free(ptr); }};

void throwIfEXRFailed(exr_result result, std::string_view what) {
  if (!EXR_OK(result))
    throw Error(concat(what, ": ", exr_result_string(result)));
}

// Whether the file starts with the EXR magic number. A file that cannot be
// opened, or is too short to hold one, is not an EXR.
[[nodiscard]] bool isEXRFile(const std::string &fileName) {
  char magic[4]{};
  std::ifstream stream{fileName, std::ios::binary};
  return stream.read(magic, sizeof(magic)) &&
         exr_is_exr_memory(magic, sizeof(magic));
}

struct EXRReaderDeleter final {
  void operator()(exr_reader *reader) const noexcept {
    exr_reader_close(reader);
  }
};

using EXRReaderPtr = std::unique_ptr<exr_reader, EXRReaderDeleter>;

// Open the file and parse its header. NOTE: The reader holds the whole
// file in memory until it closes.
[[nodiscard]] EXRReaderPtr openEXR(const std::string &fileName) {
  exr_reader *ptr{};
  throwIfEXRFailed(exr_reader_open_file(fileName.c_str(), &EXRAllocator, &ptr),
                   "Cannot open EXR");
  EXRReaderPtr reader{ptr};
  throwIfEXRFailed(exr_reader_parse_header(reader.get()),
                   "Cannot parse EXR header");
  return reader;
}

// Set up, exactly once, the tables tinyexr otherwise sets up lazily and
// without synchronization the first time a decode or encode needs them,
// since 'finishLoad()' may be called from many threads at once. Those are
// the SIMD dispatch table (with the CPU detection behind it) and the B44
// tables. The only other such tables serve the HTJ2K encoder, which never
// runs here.
void warmUpEXR() {
  static std::once_flag once;
  std::call_once(once, [] {
    exr_half_to_float(nullptr, nullptr, 0);
    exr_b44_debug_tables(nullptr, nullptr);
  });
}

// The channels of an EXR that load into an image, and what they load as.
struct EXRChannels final {
  // The number of image channels, 1 or 4.
  int numChannels{};

  // The EXR channel index for each image channel, or -1 for a missing
  // alpha channel.
  std::array<int, 4> indices{-1, -1, -1, -1};

  // The pixel type all of them share.
  exr_pixel_type pixelType{};
};

// A 1-channel EXR loads its one channel whatever its name, and any other
// loads 'R', 'G', 'B', and, if present, 'A'.
[[nodiscard]] EXRChannels selectEXRChannels(const exr_header &header) {
  const auto findChannel{[&](std::string_view name) {
    for (int i = 0; i < header.num_channels; i++)
      if (header.channels[i].name == name) return i;
    return -1;
  }};
  EXRChannels result{};
  if (header.num_channels == 1) {
    result.numChannels = 1;
    result.indices[0] = 0;
  } else {
    result.numChannels = 4;
    result.indices = {findChannel("R"), findChannel("G"), findChannel("B"),
                      findChannel("A")};
    if (result.indices[0] < 0)
      throw Error("Expected EXR channel 'R' is missing");
    if (result.indices[1] < 0)
      throw Error("Expected EXR channel 'G' is missing");
    if (result.indices[2] < 0)
      throw Error("Expected EXR channel 'B' is missing");
    // NOTE: We allow missing 'A' channel!
  }
  result.pixelType = header.channels[result.indices[0]].pixel_type;
  for (int index : result.indices) {
    if (index < 0) continue;
    const exr_channel &channel{header.channels[index]};
    // NOTE: A subsampled channel decodes to fewer samples than the data
    // window has pixels.
    if (channel.x_sampling != 1 || channel.y_sampling != 1)
      throw Error("Subsampled EXR channels are not supported");
    if (channel.pixel_type != result.pixelType)
      throw Error("Inconsistent EXR pixel types");
  }
  if (result.pixelType == EXR_PIXEL_UINT)
    throw Error("Uint EXR is not supported");
  return result;
}

} // namespace

float unpackHalf(const void *ptr) noexcept {
#if __clang__
  return float(*static_cast<const _Float16 *>(ptr));
#else
  uint16_t h = *static_cast<const uint16_t *>(ptr);
  uint32_t f = 0;
  int32_t exponent = (h >> 10) & 0x001F;
  int32_t negative = (h >> 15) & 0x0001;
  int32_t mantissa = h & 0x03FF;
  f = negative << 31;
  if (exponent == 0) {
    if (mantissa != 0) {
      exponent = 113;
      while (!(mantissa & 0x0400)) exponent--, mantissa <<= 1;
      f |= (exponent << 23) | ((mantissa & ~0x0400) << 13); // Subnormal
    }
  } else {
    f |= mantissa << 13;
    f |= exponent == 31 ? 0x7F800000 : ((exponent + 112) << 23);
  }
  float result{};
  std::memcpy(&result, &f, sizeof(result));
  return result;
#endif // #if __clang__
}

void *(*Image::image_malloc)(size_t) = &std::malloc;

void *(*Image::image_calloc)(size_t, size_t) = &std::calloc;

void *(*Image::image_realloc)(void *, size_t) = &std::realloc;

void (*Image::image_free)(void *) = &std::free;

std::string_view Image::getFormatName(Format format) noexcept {
  switch (format) {
  case UINT8:
    return "uint8";
  case UINT16:
    return "uint16";
  case FLOAT16:
    return "float16";
  case FLOAT32:
    return "float32";
  }
  return "unknown";
}

void Image::clear() {
  mFormat = UINT8;
  mNumTexelsX = 0;
  mNumTexelsY = 0;
  mNumChannels = 1;
  mTexelSize = 1;
  mNumLevels = 1;
  mHasRequestedMipLevels = false;
  mHasGeneratedMipLevels = false;
  mMipFilter = MIP_MEAN;
  mLevelOffsets.assign(1, 0);
  mSizeInBytes = 0;
  mTexels.reset();
  mFinishLoad = nullptr;
}

std::optional<Error> Image::startLoad(const std::string &fileName) noexcept {
  clear();
  std::optional<Error> error{catchAndReturnError([&] {
    if (stbi_info(fileName.c_str(), &mNumTexelsX, &mNumTexelsY,
                  &mNumChannels)) {
      // If the number of channels is 3, i.e., RGB, round it up to 4
      // so all of our alignment assumptions work.
      if (mNumChannels == 3) mNumChannels = 4;
      // Determine whether we should load 32-bit float, 16-bit unsigned int, or
      // 8-bit unsigned int.
      if (stbi_is_hdr(fileName.c_str())) {
        mFormat = FLOAT32;
        mTexelSize = 4 * mNumChannels;
      } else if (stbi_is_16_bit(fileName.c_str())) {
        mFormat = UINT16;
        mTexelSize = 2 * mNumChannels;
      } else {
        mFormat = UINT8;
        mTexelSize = 1 * mNumChannels;
      }
      // Defer the actual load until later!
      mFinishLoad = [this, fileName]() {
        // NOTE: The thread-local variant because 'finishLoad()' may be
        // called from many threads at once.
        stbi_set_flip_vertically_on_load_thread(0);
        int nX{};
        int nY{};
        int nChannels{};
        void *ptr{};
        switch (mFormat) {
        default:
        case Format::UINT8:
          // Load 8-bit unsigned int.
          ptr = stbi_load(fileName.c_str(), &nX, &nY, &nChannels, mNumChannels);
          break;
        case Format::UINT16:
          // Load 16-bit unsigned int.
          ptr = stbi_load_16(fileName.c_str(), &nX, &nY, &nChannels,
                             mNumChannels);
          break;
        case Format::FLOAT32:
          // Load 32-bit float.
          ptr =
              stbi_loadf(fileName.c_str(), &nX, &nY, &nChannels, mNumChannels);
          break;
        }
        if (!ptr) throw Error(stbFailureReason());
        // Copy into the pre-allocated texel buffer, then free the pointer.
        SMDL_SANITY_CHECK(mTexels != nullptr);
        SMDL_SANITY_CHECK(mNumTexelsX == nX);
        SMDL_SANITY_CHECK(mNumTexelsY == nY);
        std::memcpy(mTexels.get(), ptr,
                    size_t(mNumTexelsX) * size_t(mNumTexelsY) *
                        size_t(mTexelSize));
        stbi_image_free(ptr);
      };
    } else if (isEXRFile(fileName)) {
      EXRReaderPtr reader{openEXR(fileName)};
      const exr_header &header{*exr_reader_part_header(reader.get(), 0)};
      // Fail if deep or multipart!
      if (exr_reader_num_parts(reader.get()) != 1 ||
          header.part_type == EXR_PART_DEEP_SCANLINE ||
          header.part_type == EXR_PART_DEEP_TILED)
        throw Error("Deep or multipart EXR is not supported");
      int nX{header.data_window.max_x - header.data_window.min_x + 1};
      int nY{header.data_window.max_y - header.data_window.min_y + 1};
      if (nX < 0 || nY < 0)
        throw Error("Cannot parse EXR header: invalid data window");
      const EXRChannels channels{selectEXRChannels(header)};
      mNumChannels = channels.numChannels;
      if (channels.pixelType == EXR_PIXEL_HALF)
        mFormat = FLOAT16, mTexelSize = 2 * mNumChannels;
      else
        mFormat = FLOAT32, mTexelSize = 4 * mNumChannels;
      mNumTexelsX = nX;
      mNumTexelsY = nY;
      // NOTE: The reader closes here and the finish function below opens
      // another, rather than holding the whole file in memory until then.
      mFinishLoad = [this, fileName]() {
        warmUpEXR();
        EXRReaderPtr reader{openEXR(fileName)};
        // NOTE: A tiled EXR decodes to the same planar channels as a
        // scanline EXR, taking the first level of a mipmapped one.
        exr_part part{};
        SMDL_DEFER([&part]() { exr_part_free(&EXRAllocator, &part); });
        throwIfEXRFailed(exr_reader_read_part(reader.get(), 0, &part),
                         "Cannot decode EXR");
        // Select again from the decoded header rather than trusting what
        // 'startLoad()' saw, which a file rewritten in between invalidates.
        const EXRChannels channels{selectEXRChannels(part.header)};
        if (channels.numChannels != mNumChannels ||
            (channels.pixelType == EXR_PIXEL_HALF) != (mFormat == FLOAT16) ||
            part.width != mNumTexelsX || part.height != mNumTexelsY)
          throw Error("EXR changed since its header was parsed");
        const size_t channelSize{size_t(mTexelSize / mNumChannels)};
        const size_t numTexels{size_t(mNumTexelsX) * size_t(mNumTexelsY)};
        for (int iC = 0; iC < mNumChannels; iC++) {
          if (channels.indices[iC] < 0) continue;
          const std::byte *src{static_cast<const std::byte *>(
              part.images[channels.indices[iC]])};
          std::byte *dst{mTexels.get() + channelSize * size_t(iC)};
          for (size_t i = 0; i < numTexels; i++)
            std::memcpy(dst + size_t(mTexelSize) * i, src + channelSize * i,
                        channelSize);
        }
        // If the alpha channel is missing, fill with 1.
        if (mNumChannels == 4 && channels.indices[3] < 0) {
          if (mFormat == FLOAT16) {
            uint16_t one{0x3C00};
            std::byte *itr{mTexels.get() + 6};
            for (size_t i = 0; i < numTexels; i++) {
              std::memcpy(itr, &one, 2);
              itr += mTexelSize;
            }
          } else {
            float one{1.0f};
            std::byte *itr{mTexels.get() + 12};
            for (size_t i = 0; i < numTexels; i++) {
              std::memcpy(itr, &one, 4);
              itr += mTexelSize;
            }
          }
        }
      };
    } else {
      // What stb_image said on the way in speaks for tinyexr too: it is
      // "can't fopen" exactly when tinyexr could not open the file either,
      // and otherwise "unknown image type".
      throw Error(stbFailureReason());
    }
    // How many levels the extent implies, which is all of the layout
    // that can be settled here: whether anything wants them is not known
    // until every reference has been seen, so 'finishLoad()' does the
    // rest once the answer is in.
    mNumLevels = 1;
    while ((std::max(mNumTexelsX, mNumTexelsY) >> (mNumLevels - 1)) > 1)
      mNumLevels++;
  })};
  if (error) {
    clear();
    error->message =
        concat("Cannot load ", SpellFilePath(fileName), ": ", error->message);
  }
  return error;
}

void Image::allocate() {
  const int numLevels{getNumLevels()};
  mLevelOffsets.assign(size_t(numLevels), 0);
  size_t offset{0};
  for (int level = 0; level < numLevels; level++) {
    mLevelOffsets[size_t(level)] = offset;
    offset += size_t(getNumTexelsX(level)) * size_t(getNumTexelsY(level)) *
              size_t(mTexelSize);
  }
  mSizeInBytes = offset;
  mTexels.reset(static_cast<std::byte *>(
      ::operator new(mSizeInBytes, std::align_val_t(TEXEL_ALIGNMENT))));
  // Level 0 only: the rest is written by 'generateMipLevels()' before
  // anything can read it, and zeroing here is so that a decode failure
  // leaves well-defined zero texels.
  std::memset(mTexels.get(), 0,
              size_t(mNumTexelsX) * size_t(mNumTexelsY) * size_t(mTexelSize));
}

void Image::finishLoad() {
  if (!mFinishLoad) return;
  allocate();
  // NOTE: Move into a local and null the member before invoking, so
  // that a throw cannot leave a stale finish function behind to be
  // invoked again.
  std::function<void()> finishLoad{std::move(mFinishLoad)};
  mFinishLoad = nullptr;
  try {
    finishLoad();
  } catch (...) {
    // Only level 0 was zeroed by 'allocate()', so zero all of it here: a
    // partially decoded image and an ungenerated chain must still read
    // as well-defined zero texels.
    std::memset(mTexels.get(), 0, mSizeInBytes);
    throw;
  }
  if (mHasRequestedMipLevels) {
    generateMipLevels();
    mHasGeneratedMipLevels = true;
  }
}

void Image::generateMeanMipLevels() noexcept {
  const stbir_datatype dataType{mFormat == UINT8     ? STBIR_TYPE_UINT8
                                : mFormat == UINT16  ? STBIR_TYPE_UINT16
                                : mFormat == FLOAT16 ? STBIR_TYPE_HALF_FLOAT
                                                     : STBIR_TYPE_FLOAT};
  // The plain N-channel layouts, so no channel gets alpha semantics:
  // the chain averages stored values as-is (see the class doc comment
  // for why filtering is gamma-agnostic).
  const stbir_pixel_layout layout{mNumChannels == 1   ? STBIR_1CHANNEL
                                  : mNumChannels == 2 ? STBIR_2CHANNEL
                                                      : STBIR_4CHANNEL};
  for (int level = 1; level < mNumLevels; level++) {
    // Each level halves the previous one, so the box filter is an
    // area average whenever the parent extent is even, and blends the
    // straddling texels with the correct fractional weights when it
    // is odd. Averaging level to level keeps the whole chain
    // mean-preserving.
    stbir_resize(mTexels.get() + mLevelOffsets[size_t(level - 1)],
                 getNumTexelsX(level - 1), getNumTexelsY(level - 1),
                 getNumTexelsX(level - 1) * mTexelSize,
                 mTexels.get() + mLevelOffsets[size_t(level)],
                 getNumTexelsX(level), getNumTexelsY(level),
                 getNumTexelsX(level) * mTexelSize, layout, dataType,
                 STBIR_EDGE_CLAMP, STBIR_FILTER_BOX);
  }
}

void Image::generateMaxMipLevels() noexcept {
  const int channelSize{mTexelSize / mNumChannels};
  // Compare in single precision and copy the winning channel's stored
  // bytes, so every format reduces without a pack step.
  const auto valueOf{[&](const void *ptr) -> float {
    switch (mFormat) {
    case UINT8:
      return *static_cast<const uint8_t *>(ptr);
    case UINT16:
      return *static_cast<const uint16_t *>(ptr);
    case FLOAT16:
      return unpackHalf(ptr);
    default:
      return *static_cast<const float *>(ptr);
    }
  }};
  const auto wrap{[](int i, int n) { return ((i % n) + n) % n; }};
  for (int level = 1; level < mNumLevels; level++) {
    const int prevX{getNumTexelsX(level - 1)};
    const int prevY{getNumTexelsY(level - 1)};
    const int numX{getNumTexelsX(level)};
    const int numY{getNumTexelsY(level)};
    std::byte *prevTexels{mTexels.get() + mLevelOffsets[size_t(level - 1)]};
    std::byte *texels{mTexels.get() + mLevelOffsets[size_t(level)]};
    // Level 1 alone adds the one-texel border around the pair it
    // covers; the higher levels inherit it from their children. The
    // last texel of an odd extent widens to cover the remainder.
    const int border{level == 1 ? 1 : 0};
    for (int j = 0; j < numY; j++) {
      const int y0{2 * j - border};
      const int y1{(j == numY - 1 ? prevY : std::min(2 * j + 2, prevY)) +
                   border};
      for (int i = 0; i < numX; i++) {
        const int x0{2 * i - border};
        const int x1{(i == numX - 1 ? prevX : std::min(2 * i + 2, prevX)) +
                     border};
        const uint8_t *best[4]{};
        float bestValue[4]{};
        for (int y = y0; y < y1; y++) {
          for (int x = x0; x < x1; x++) {
            std::byte *src{prevTexels +
                           size_t(mTexelSize) *
                               (size_t(wrap(x, prevX)) +
                                size_t(prevX) * size_t(wrap(y, prevY)))};
            for (int c = 0; c < mNumChannels; c++) {
              const uint8_t *ptr{reinterpret_cast<const uint8_t *>(src) +
                                 size_t(c) * size_t(channelSize)};
              const float value{valueOf(ptr)};
              if (!best[c] || value > bestValue[c]) {
                best[c] = ptr;
                bestValue[c] = value;
              }
            }
          }
        }
        uint8_t *dst{reinterpret_cast<uint8_t *>(
            texels +
            size_t(mTexelSize) * (size_t(i) + size_t(numX) * size_t(j)))};
        for (int c = 0; c < mNumChannels; c++)
          std::memcpy(dst + size_t(c) * size_t(channelSize), best[c],
                      size_t(channelSize));
      }
    }
  }
}

void Image::flipVertically() noexcept {
  // The levels in memory, not the levels the extent implies: a chain
  // that was never requested was never allocated either.
  for (int level = 0; level < getNumLevelsInMemory(); level++) {
    std::byte *levelTexels{mTexels.get() + mLevelOffsets[size_t(level)]};
    int numTexelsY{getNumTexelsY(level)};
    size_t rowSize{size_t(mTexelSize) * size_t(getNumTexelsX(level))};
    for (int iY = 0; iY < numTexelsY / 2; iY++) {
      std::swap_ranges(levelTexels + rowSize * size_t(iY),
                       levelTexels + rowSize * size_t(iY + 1),
                       levelTexels + rowSize * size_t(numTexelsY - iY - 1));
    }
  }
}

float4 Image::fetch(int x, int y, int level) const noexcept {
  SMDL_SANITY_CHECK(mTexels != nullptr);
  SMDL_SANITY_CHECK(0 <= level && level < getNumLevels());
  SMDL_SANITY_CHECK(0 <= x && x < getNumTexelsX(level));
  SMDL_SANITY_CHECK(0 <= y && y < getNumTexelsY(level));
  return fetchUnsafe(x, y, level);
}

float4 Image::fetchUnsafe(int x, int y, int level) const noexcept {
  float4 texel{std::numeric_limits<float>::quiet_NaN(),
               std::numeric_limits<float>::quiet_NaN(),
               std::numeric_limits<float>::quiet_NaN(),
               std::numeric_limits<float>::quiet_NaN()};
  std::byte *texelPtr{
      mTexels.get() + mLevelOffsets[size_t(level)] +
      size_t(mTexelSize) *
          (size_t(x) + size_t(getNumTexelsX(level)) * size_t(y))};
  for (int i = 0; i < mNumChannels; i++) {
    switch (mFormat) {
    case UINT8:
      texel[i] = float(*reinterpret_cast<const uint8_t *>(texelPtr)) / 255.0f;
      texelPtr += 1;
      break;
    case UINT16:
      texel[i] =
          float(*reinterpret_cast<const uint16_t *>(texelPtr)) / 65535.0f;
      texelPtr += 2;
      break;
    case FLOAT16:
      texel[i] = unpackHalf(texelPtr);
      texelPtr += 2;
      break;
    case FLOAT32:
      texel[i] = *reinterpret_cast<const float *>(texelPtr);
      texelPtr += 4;
      break;
    default:
      SMDL_SANITY_CHECK_MSG(false, "Unexpected texel format!");
      break;
    }
  }
  return texel;
}

std::optional<Error> write8bitImage(const std::string &fileName, int numTexelsX,
                                    int numTexelsY, int numChannels,
                                    const void *ptr) {
  SMDL_SANITY_CHECK(0 < numTexelsX);
  SMDL_SANITY_CHECK(0 < numTexelsY);
  SMDL_SANITY_CHECK(0 < numChannels && numChannels <= 4);
  SMDL_SANITY_CHECK(ptr);
  int result{};
  if (hasExtension(fileName, ".png")) {
    result = stbi_write_png(fileName.c_str(), numTexelsX, numTexelsY,
                            numChannels, ptr, 0);
  } else if (hasExtension(fileName, ".jpeg") ||
             hasExtension(fileName, ".jpg")) {
    result = stbi_write_jpg(fileName.c_str(), numTexelsX, numTexelsY,
                            numChannels, ptr, 90);
  } else if (hasExtension(fileName, ".bmp")) {
    result = stbi_write_bmp(fileName.c_str(), numTexelsX, numTexelsY,
                            numChannels, ptr);
  } else if (hasExtension(fileName, ".tga")) {
    result = stbi_write_tga(fileName.c_str(), numTexelsX, numTexelsY,
                            numChannels, ptr);
  } else if (hasExtension(fileName, ".pgm") || hasExtension(fileName, ".ppm")) {
    return catchAndReturnError([&] {
      bool isGray{hasExtension(fileName, ".pgm")};
      std::fstream stream{
          openOrThrow(fileName, std::ios::binary | std::ios::out)};
      stream << (isGray ? "P5 " : "P6 ");
      stream << numTexelsX << ' ';
      stream << numTexelsY << " 255\n";
      const char *texelPtr{static_cast<const char *>(ptr)};
      const char *texelPtrEnd{texelPtr + size_t(numChannels) *
                                             size_t(numTexelsX) *
                                             size_t(numTexelsY)};
      if (isGray) {
        for (; texelPtr < texelPtrEnd; texelPtr += numChannels) {
          stream.write(texelPtr, 1);
        }
      } else {
        int effNumChannels{std::min(numChannels, 3)};
        for (; texelPtr < texelPtrEnd; texelPtr += numChannels) {
          stream.write(texelPtr, effNumChannels);
          stream.write("\0\0\0", 3 - effNumChannels);
        }
      }
    });
  } else {
    return Error(concat("Cannot write ", SpellFilePath(fileName),
                        ": unrecognized extension"));
  }
  if (result == 0) {
    return Error(concat("Cannot write ", SpellFilePath(fileName)));
  }
  return std::nullopt;
}

Span<const std::string_view> write8bitImageExtensions() noexcept {
  static constexpr std::array<std::string_view, 7> EXTENSIONS{
      ".png", ".jpg", ".jpeg", ".bmp", ".tga", ".pgm", ".ppm"};
  return EXTENSIONS;
}

std::optional<Error> writeFloatImage(const std::string &fileName,
                                     int numTexelsX, int numTexelsY,
                                     int numChannels, const float *ptr) {
  SMDL_SANITY_CHECK(0 < numTexelsX);
  SMDL_SANITY_CHECK(0 < numTexelsY);
  SMDL_SANITY_CHECK(numChannels == 1 || numChannels == 3 || numChannels == 4);
  SMDL_SANITY_CHECK(ptr);
  if (hasExtension(fileName, ".exr")) {
    warmUpEXR();
    exr_part part{};
    SMDL_DEFER([&part]() { exr_part_free(&EXRAllocator, &part); });
    exr_result result{exr_rgba_float_to_part(&EXRAllocator, ptr, numTexelsX,
                                             numTexelsY, numChannels,
                                             EXR_PIXEL_FLOAT, &part)};
    if (EXR_OK(result)) {
      const exr_image image{/*num_parts=*/1, &part, EXRAllocator};
      result = exr_save_to_file(fileName.c_str(), &image, EXR_COMPRESSION_ZIP);
    }
    if (!EXR_OK(result))
      return Error(concat("Cannot write ", SpellFilePath(fileName), ": ",
                          exr_result_string(result)));
    return std::nullopt;
  } else if (hasExtension(fileName, ".hdr")) {
    if (stbi_write_hdr(fileName.c_str(), numTexelsX, numTexelsY, numChannels,
                       ptr) == 0) {
      return Error(concat("Cannot write ", SpellFilePath(fileName)));
    }
    return std::nullopt;
  } else {
    return Error(concat("Cannot write ", SpellFilePath(fileName),
                        ": unrecognized extension"));
  }
}

Span<const std::string_view> writeFloatImageExtensions() noexcept {
  static constexpr std::array<std::string_view, 2> EXTENSIONS{".exr", ".hdr"};
  return EXTENSIONS;
}

} // namespace smdl
