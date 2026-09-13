/// \file
/// The meter: what a reflected-light meter reads of the scene before the
/// first sample, and the pixels it reads it at. A camera meters and then
/// exposes, so the ISO is decided once, before the shutter opens, from a
/// metering pass over the window, recorded in the film's header, and
/// held for every session of the sequence.
#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <vector>

#include "Color.h"
#include "IO/RenderHeader.h"
#include "Sensor/Response.h"
#include "Sensor/Sensor.h"

/// How many pixels the meter reads at least, where the window has that
/// many: a meter has fewer cells than the sensor, and a fixed count
/// makes the reading's noise the same at every resolution and the pass
/// about this many paths, a fraction of a sample per pixel of the
/// render. Enough that a lamp in view, whose few direct hits are most
/// of the frame's mean, moves the reading by less than a third stop
/// from one seed to the next.
constexpr size_t METER_PIXEL_BUDGET{size_t(1) << 18};

/// The band the meter reads through, on the grid its pixels evaluate on:
/// the sensor's most sensitive band, the one the well and the base ISO
/// rest on, which is what a body that meters through its own sensor
/// reads. Under a tile the band's own grid and its own pixels; else the
/// one grid and every pixel. Built from the sensor alone, so the
/// observer's preview of a sensor meters the same way.
class MeterProjection final {
public:
  explicit MeterProjection(const Sensor &sensor);

  [[nodiscard]] const std::string &bandName() const noexcept {
    return mBandName;
  }

  /// Which cells of the render's tile hold the band, row by row as
  /// `tileIndexAt()` indexes them; empty without a tile, where every
  /// pixel holds every band.
  [[nodiscard]] const std::vector<bool> &countingCells() const noexcept {
    return mCountingCells;
  }

  /// One sample's projection onto the band, in electrons per square
  /// meter and second; see `BandProjection::project()`.
  [[nodiscard]] double project(smdl::Span<const float> wavelengths,
                               smdl::Span<const float> E) const noexcept {
    return mProjection.project(wavelengths, E);
  }

private:
  std::string mBandName{};

  BandProjection mProjection{};

  std::vector<bool> mCountingCells{};
};

/// The pixels the meter reads, as frame pixel indices in row-major frame
/// order: one whole tile, or one pixel without a tile, at the origin of
/// every cell of a square lattice over the window, `stride()` tiles
/// apart, keeping the pixels of the meter's band. The stride is the
/// largest that leaves at least `METER_PIXEL_BUDGET` of them, at least
/// 1, so a window smaller than the budget is read at every pixel of the
/// band. Empty when no pixel holds the band, which the meter reads as a
/// dark frame.
class MeterLattice final {
public:
  MeterLattice(int4 window, size_t numPixelsX, size_t tileColumns,
               size_t tileRows, const std::vector<bool> &countingCells);

  [[nodiscard]] size_t stride() const noexcept { return mStride; }

  [[nodiscard]] const std::vector<size_t> &pixels() const noexcept {
    return mPixels;
  }

private:
  size_t mStride{1};

  std::vector<size_t> mPixels{};
};

/// What the metering pass tallied: the projections of every sample it
/// drew, and how many.
struct MeterReading final {
  double sum{};

  uint64_t count{};

  /// The mean projection in electrons per square meter and second, 0 for
  /// no samples.
  [[nodiscard]] double meanElectronRate() const noexcept {
    return count > 0 ? sum / double(count) : 0.0;
  }
};

/// Does the sequence still need metering? Not with a stated gain, whose
/// speed is the instrument's, and not once the header carries the rung
/// the meter chose, which every later session reads back.
[[nodiscard]] bool needsMeter(const Sensor &sensor,
                              const RenderHeader &header) noexcept;

/// Record what the meter read in `header`: the exposure `reading` is at
/// `seconds` and the rung it snaps to. `seconds` is the shutter, times
/// the scale that turns a previewed observer's radiance into the
/// sensor's irradiance under `-ideal`.
void recordMeter(const Sensor &sensor, const MeterReading &reading,
                 double seconds, RenderHeader &header) noexcept;

/// The ISO a shot is read out at and where it came from.
struct ShotISO final {
  double iso{};

  /// Did the meter choose it, rather than the gain or a stated number?
  bool wasMetered{};
};

/// The ISO the shot is read out at: the stated gain's speed, else the
/// `stated` number, else the rung `header` records, which must be there
/// by then.
[[nodiscard]] ShotISO resolveShotISO(const Sensor &sensor,
                                     const std::optional<float> &stated,
                                     const RenderHeader &header) noexcept;

/// Log where the ISO came from, once per session: the meter's reading
/// against the base and the top, the shutter or the stop that would
/// meter into range when the frame is outside it, and where a stated
/// ISO or a stated gain's speed sits against the meter. `exposure` and
/// `fNumber` spell that fix; `wasRead` says the reading came off the
/// resumed file rather than this session's pass.
void logISO(const Sensor &sensor, const std::optional<float> &stated,
            const RenderHeader &header, double exposure, double fNumber,
            bool wasRead);
