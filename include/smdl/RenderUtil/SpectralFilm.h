/// \file
#pragma once

#include <cstdint>
#include <map>
#include <memory>
#include <optional>
#include <string>
#include <vector>

#include "smdl/Export.h"
#include "smdl/Support/Macros.h"
#include "smdl/Support/Span.h"
#include "smdl/Support/VectorMath.h"

namespace smdl {

/// \addtogroup renderutil
/// \{

/// A spectral film: the per-pixel spectral accumulator a render
/// integrates into.
///
/// Every pixel accumulates the same number of samples, which the film
/// tracks as a whole: the per-pixel totals are raw sums over
/// `getNumSamples()` samples, and `mean()` is where the division
/// happens.
///
/// Accumulation is unsynchronized: threads may accumulate concurrently
/// only into disjoint pixels, and no thread may read the film while
/// another is accumulating into it.
class SMDL_EXPORT SpectralFilm final {
public:
  SpectralFilm() = default;

  explicit SpectralFilm(size_t nBands, size_t nPixelsX, size_t nPixelsY) {
    resize(nBands, nPixelsX, nPixelsY);
  }

  /// Clear.
  void clear() noexcept;

  /// Resize.
  void resize(size_t nBands, size_t nPixelsX, size_t nPixelsY);

  /// The number of spectral bands.
  [[nodiscard]] size_t getNumBands() const noexcept { return mNumBands; }

  /// The number of pixels in X.
  [[nodiscard]] size_t getNumPixelsX() const noexcept { return mNumPixelsX; }

  /// The number of pixels in Y.
  [[nodiscard]] size_t getNumPixelsY() const noexcept { return mNumPixelsY; }

  /// The number of samples accumulated into every pixel.
  [[nodiscard]] uint64_t getNumSamples() const noexcept { return mNumSamples; }

  /// Add the contents of another film pixel-by-pixel.
  void add(const SpectralFilm &other) noexcept;

  /// Add an already-summed contribution to one pixel's totals. The
  /// buffer invariant this maintains is that the totals are always the
  /// raw sums over `getNumSamples()` samples, and the mean is taken
  /// only at the edges (see `mean()`).
  ///
  /// \param[in] totalPtr
  /// The pointer to `getNumBands()` summed contribution values.
  ///
  template <typename T>
  void addTotals(size_t iX, size_t iY, const T *totalPtr) noexcept {
    SMDL_SANITY_CHECK(iX < mNumPixelsX);
    SMDL_SANITY_CHECK(iY < mNumPixelsY);
    auto totalsPtr{&mTotals[(mNumPixelsX * iY + iX) * mNumBands]};
    for (size_t i = 0; i < mNumBands; i++) {
      totalsPtr[i] += static_cast<double>(totalPtr[i]);
    }
  }

  /// The raw per-band sums of one pixel, for a caller that must
  /// re-weight them exactly. Anything reading radiance wants `mean()`.
  [[nodiscard]] Span<const double> totals(size_t iX, size_t iY) const noexcept {
    SMDL_SANITY_CHECK(iX < mNumPixelsX);
    SMDL_SANITY_CHECK(iY < mNumPixelsY);
    return {&mTotals[(mNumPixelsX * iY + iX) * mNumBands], mNumBands};
  }

  /// The mean of band `iBand` of one pixel: the accumulated total
  /// divided by the sample count, or 0 with no samples. This is the
  /// single source of radiance values; every output must read the
  /// buffer through it.
  [[nodiscard]] double mean(size_t iX, size_t iY, size_t iBand) const noexcept {
    SMDL_SANITY_CHECK(iX < mNumPixelsX);
    SMDL_SANITY_CHECK(iY < mNumPixelsY);
    SMDL_SANITY_CHECK(iBand < mNumBands);
    return mNumSamples > 0
               ? mTotals[(mNumPixelsX * iY + iX) * mNumBands + iBand] /
                     static_cast<double>(mNumSamples)
               : 0.0;
  }

  /// Record that every pixel has accumulated `numSamples` more samples.
  void addSamples(uint64_t numSamples) noexcept { mNumSamples += numSamples; }

  /// The header information returned by `readENVIFile()`: everything
  /// the loader does not fold into the film itself.
  struct ENVIFileInfo final {
    /// The wavelengths in nanometers, empty if the header has none.
    std::vector<float> wavelengths{};

    /// The value of the `render spp` header field, or 0 if the
    /// header does not carry one (a foreign or legacy file). The film
    /// reads back with `getNumSamples()` equal to this clamped to 1, so
    /// means stay meaningful even when the true count is unknown. It
    /// applies inside `window` only.
    uint64_t samplesPerPixel{};

    /// The pixel rectangle the sample count applies to, from the
    /// `render crop window` header field, or the whole frame when the
    /// header carries none. Pixels outside it read back empty, so a caller
    /// always compares whole rectangles and never has to ask whether
    /// the field was there.
    int4 cropWindow{};

    /// Every header field this loader has no specific handling for,
    /// keyed by the lower-cased field name, values verbatim.
    std::map<std::string, std::string> fields{};
  };

  /// Read an ENVI Standard image file and header, as written by
  /// `writeENVIFile()`, replacing this film's contents. The totals and
  /// sample count are reconstructed from the stored means and the
  /// `smdl spp` header field inside the window the file records, with
  /// zero totals outside it.
  ///
  /// The header is read from `fileName + ".hdr"`. Only the exact
  /// format the writer emits is accepted (`data type = 5`, `interleave
  /// = bip`); a byte order other than the native one is swapped on
  /// load.
  ///
  /// \return
  /// The header information that does not become part of the film.
  ///
  /// \throws Error if either file is missing or malformed, in which
  /// case this film is left cleared rather than partly read.
  ///
  ENVIFileInfo readENVIFile(const std::string &fileName);

  /// Write an ENVI Standard image file and header.
  ///
  /// The binary file holds the per-pixel per-band MEAN, i.e., the
  /// accumulated totals divided by the sample count, so the pixel
  /// values are physically meaningful radiance no matter how many
  /// samples were folded in. The header records the count as
  /// `smdl spp = N`, which is what `readENVIFile()` uses to reconstruct
  /// the totals so that a later render can keep accumulating.
  ///
  /// \param[in] wavelengths
  /// The wavelengths in nanometers.
  ///
  /// \param[in] fileName
  /// The filename of the image. The header is written alongside as
  /// `fileName + ".hdr"`.
  ///
  /// \param[in] extraHeaderLines
  /// Additional `key = value` lines appended to the header verbatim,
  /// for application metadata. Lines whose keys ENVI readers do not
  /// recognize are ignored by convention.
  ///
  /// \param[in] window
  /// The pixel rectangle `{x0, y0, x1, y1}` (`x0 <= x < x1` and
  /// `y0 <= y < y1`) the accumulation covers, or nothing for the whole
  /// frame. This is the caller's declaration of what it rendered, and
  /// is recorded as `smdl window = {x0, y0, x1, y1}` when it narrows
  /// the frame, so that `readENVIFile()` knows which pixels the count
  /// belongs to and drops the totals outside them. Every pixel is
  /// written as its mean either way.
  ///
  /// \throws Error if the window is empty or out of bounds.
  ///
  void writeENVIFile(Span<const float> wavelengths, const std::string &fileName,
                     Span<const std::string> extraHeaderLines = {},
                     std::optional<int4> window = {}) const;

private:
  size_t mNumBands{};

  size_t mNumPixelsX{};

  size_t mNumPixelsY{};

  uint64_t mNumSamples{};

  /// The per-band totals of each pixel, `mNumBands` per pixel.
  std::unique_ptr<double[]> mTotals{};
};

/// \}

} // namespace smdl
