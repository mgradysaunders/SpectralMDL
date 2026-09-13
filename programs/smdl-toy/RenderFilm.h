/// \file
/// What the render accumulates into: one film, and what fills it.
#pragma once

#include <cstddef>

#include "smdl/RenderUtil/SpectralFilm.h"

class Response;

/// The film the render draws into: the observer's spectral film, or a
/// physical sensor's band film with the response that projects every
/// sample onto its bands. Exactly one of the two, which is why this is a
/// type rather than a pair of pointers and an invariant restated at every
/// entry point that takes them.
///
/// `spectralFilm()` and `bandFilm()` are the same storage seen two ways,
/// each null for the kind it is not: a caller that means the observer's
/// film specifically (the pass combination, the night tone map, the
/// spectral ENVI write) asks for the first, one that means a sensor's
/// (the response's accumulation, the detector readout) asks for the
/// second, and one that means whichever film this is (the sample count,
/// a resumed merge) asks for `film()`.
///
/// The film is owned here. The response is borrowed and must outlive
/// this, which in `main()` it does, being built before the film and
/// destroyed after it.
class RenderFilm final {
public:
  /// The film `response` fills, one value per band it projects onto, or
  /// the observer's film of `numBands` spectral radiances when `response`
  /// is null. `numBands` is the render grid's band count and is
  /// meaningless with a response, which states its own.
  ///
  /// Whether the response is there is the whole of the choice, which is
  /// why it is one constructor: there is no way to ask for a band film
  /// with nothing to fill it, or for the observer's film through a
  /// sensor. Out of line, this header not seeing what a response is.
  RenderFilm(const Response *response, size_t numBands, size_t numPixelsX,
             size_t numPixelsY);

  RenderFilm(const RenderFilm &) = delete;

  RenderFilm &operator=(const RenderFilm &) = delete;

  /// Does a sensor's response fill this, rather than the observer? The
  /// one question every branch on the film's kind asks.
  [[nodiscard]] bool hasResponse() const noexcept {
    return mResponse != nullptr;
  }

  /// The response, or null for the observer's film.
  [[nodiscard]] const Response *response() const noexcept { return mResponse; }

  /// The film, whichever kind it is.
  ///
  /// \{
  [[nodiscard]] smdl::SpectralFilm &film() noexcept { return mFilm; }
  [[nodiscard]] const smdl::SpectralFilm &film() const noexcept {
    return mFilm;
  }
  /// \}

  /// The film as the observer's, or null through a sensor.
  ///
  /// \{
  [[nodiscard]] smdl::SpectralFilm *spectralFilm() noexcept {
    return mResponse ? nullptr : &mFilm;
  }
  [[nodiscard]] const smdl::SpectralFilm *spectralFilm() const noexcept {
    return mResponse ? nullptr : &mFilm;
  }
  /// \}

  /// The film as a sensor's, or null through the observer.
  ///
  /// \{
  [[nodiscard]] smdl::SpectralFilm *bandFilm() noexcept {
    return mResponse ? &mFilm : nullptr;
  }
  [[nodiscard]] const smdl::SpectralFilm *bandFilm() const noexcept {
    return mResponse ? &mFilm : nullptr;
  }
  /// \}

private:
  smdl::SpectralFilm mFilm{};

  const Response *mResponse{};
};
