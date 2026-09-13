/// \file
/// The DNG writer: a detector readout in the raw format a photographic
/// pipeline takes, which `-output-raw` writes when the file is named
/// `.dng`. A TIFF-EP container of DNG 1.4 tags, uncompressed and little
/// endian whatever the host, holding one strip of 16-bit digital
/// numbers and the calibration that develops them.
///
/// This product includes DNG technology under license by Adobe, which is
/// the notice Adobe's royalty-free patent license asks of anything that
/// reads or writes the format.
///
/// Nothing here knows what a sensor is: the caller resolves the shapes a
/// camera has, and the writer refuses anything else. That is the line
/// the format draws too. DNG carries at most four color planes, keyed to
/// a fixed enumeration of colors, so a band that is not red, green, or
/// blue has no spelling here and belongs in the ENVI pair, which carries
/// any number of bands under any name.
#pragma once

#include <array>
#include <cstdint>
#include <string>
#include <string_view>

#include "smdl/Support/Span.h"

#include "Common.h"

/// The extension that marks a DNG.
constexpr std::string_view DNG_EXTENSION = ".dng";

/// The readout to write, and everything a raw developer needs to take
/// it: what the numbers are, which of the three colors each pixel holds,
/// how the three see color, and what the shot was.
///
/// The two calibrations are the pair every camera profile carries: one
/// matrix under a warm illuminant and one under a cool one, which a
/// developer interpolates between by the temperature `asShotNeutral`
/// reads as, exactly as it does for a real body. `ColorMatrix1` and
/// `ColorMatrix2` are what the spec requires; the forward matrices are
/// what a developer prefers when they are there, and they pin the
/// develop to the same numbers rather than to the reader's own white
/// point solve.
struct DNGImage final {
  /// The frame in pixels, which is the whole sensor.
  ///
  /// \{
  size_t pixelCountX{};
  size_t pixelCountY{};
  /// \}

  /// The digital numbers row by row: one per pixel under `cfa`, and
  /// three per pixel without one, in red, green, blue order.
  smdl::Span<const uint16_t> digitalNumbers{};

  /// The 2 by 2 color filter array, row by row, each entry 0 for red, 1
  /// for green, and 2 for blue; empty when every pixel holds all three,
  /// which is written as `LinearRaw` instead.
  std::array<uint8_t, 4> cfa{};

  bool hasCFA{};

  /// The digital number a pixel with no charge reads, and the one a
  /// saturated pixel reads, which is what a develop takes as white.
  ///
  /// \{
  double blackLevel{};
  uint16_t whiteLevel{};
  /// \}

  /// `ColorMatrix1` and `ColorMatrix2`: XYZ to the camera's own three,
  /// under the first and the second calibration illuminant.
  ///
  /// \{
  double3x3 xyzToCamera1{};
  double3x3 xyzToCamera2{};
  /// \}

  /// `ForwardMatrix1` and `ForwardMatrix2`: the camera's own three,
  /// balanced so that the neutral reads (1, 1, 1), to XYZ under D50.
  ///
  /// \{
  double3x3 cameraToXYZ1{};
  double3x3 cameraToXYZ2{};
  /// \}

  /// `CalibrationIlluminant1` and `CalibrationIlluminant2`, as the Exif
  /// light source codes: 17 is CIE illuminant A, 20 is D55, 21 is D65.
  ///
  /// \{
  uint16_t illuminant1{};
  uint16_t illuminant2{};
  /// \}

  /// `AsShotNeutral`: what a neutral under the shot's own light reads in
  /// the camera's three, which is the white balance.
  double3 asShotNeutral{};

  /// `BaselineExposure`: the stops a develop adds, so that a developer's
  /// default rendering lands a metered neutral where this one does.
  double baselineExposure{};

  /// `NoiseProfile`: the scale and the offset of each plane, in the
  /// model `sigma(x) = sqrt(S x + O)` over a signal `x` in [0, 1]
  /// between the black level and the white level.
  std::array<double2, 3> noiseProfile{};

  /// The rendered part of the frame as `DefaultCropOrigin` and
  /// `DefaultCropSize`, so that a crop opens as the picture it is; the
  /// whole frame writes neither tag.
  int4 window{};

  /// `Make`, `Model`, and `UniqueCameraModel`, which a developer indexes
  /// its profiles by, `Software`, and `ImageDescription`.
  ///
  /// \{
  std::string make{};
  std::string model{};
  std::string uniqueCameraModel{};
  std::string software{};
  std::string description{};
  /// \}

  /// The shot, for the Exif directory: the exposure in seconds, the
  /// f-number, and the ISO.
  ///
  /// \{
  double exposureTime{};
  double fNumber{};
  double iso{};
  /// \}

  /// The samples one pixel holds: one under a tile, and three without.
  [[nodiscard]] size_t sampleCount() const noexcept { return hasCFA ? 1 : 3; }
};

/// Write `image` to `fileName`.
///
/// \throws smdl::Error
/// If the numbers do not fill the frame, the tile names a plane that is
/// not one of the three, the levels do not increase, or the window falls
/// outside the frame.
///
void writeDNGFile(const std::string &fileName, const DNGImage &image);
