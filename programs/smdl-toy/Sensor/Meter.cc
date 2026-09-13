#include <algorithm>
#include <cmath>

#include "smdl/Support/Logger.h"
#include "smdl/Support/Strings.h"

#include "Sensor/Meter.h"

namespace {

// A shutter for the log, in seconds and as a reciprocal.
[[nodiscard]] std::string spellShutter(double seconds) {
  return seconds > 0 && seconds < 1
             ? smdl::concat(smdl::Brief(seconds, 4), " s (1/",
                            smdl::Brief(1.0 / seconds, 4), ")")
             : smdl::concat(smdl::Brief(seconds, 4), " s");
}

} // namespace

MeterProjection::MeterProjection(const Sensor &sensor) {
  const ResponseSettings &response{sensor.settings().response};
  const size_t peak{sensor.peakBand()};
  SMDL_SANITY_CHECK(peak < response.bands.size());
  const ResponseBand &band{response.bands[peak]};
  mBandName = band.name;
  const RenderGrid &grids{gRenderGrid};
  // The band's grid: under a tile the grid of the cells that hold the
  // band, which are the cells that count; else the one grid.
  const WavelengthGrid *grid{&grids.first()};
  if (grids.hasTile()) {
    SMDL_SANITY_CHECK(grids.tileColumns == response.cfaColumns &&
                      grids.tileGrids.size() == response.cfa.size());
    mCountingCells.assign(response.cfa.size(), false);
    for (size_t cell = 0; cell < response.cfa.size(); cell++) {
      if (response.cfa[cell] != peak) continue;
      mCountingCells[cell] = true;
      grid = &grids.grids[grids.tileGrids[cell]];
    }
    if (std::none_of(mCountingCells.begin(), mCountingCells.end(),
                     [](bool counts) { return counts; }))
      SMDL_LOG_WARN("The tile lays down no ", smdl::Quoted(mBandName),
                    " pixel, the sensor's most sensitive band, so the meter "
                    "reads nothing");
  }
  mProjection =
      BandProjection(band, response.qeScale(), *grid, grids.isJittering);
}

MeterLattice::MeterLattice(int4 window, size_t numPixelsX, size_t tileColumns,
                           size_t tileRows,
                           const std::vector<bool> &countingCells) {
  const size_t columns{std::max<size_t>(tileColumns, 1)};
  const size_t rows{std::max<size_t>(tileRows, 1)};
  SMDL_SANITY_CHECK(countingCells.empty() ||
                    countingCells.size() == columns * rows);
  const size_t x0{size_t(std::max(window[0], 0))};
  const size_t y0{size_t(std::max(window[1], 0))};
  const size_t x1{size_t(std::max(window[2], 0))};
  const size_t y1{size_t(std::max(window[3], 0))};
  if (!(x1 > x0 && y1 > y0)) return;
  // The stride in tiles: the largest that leaves the budget's worth of
  // counting pixels, from the cells the window holds and the counting
  // pixels each tile holds.
  const size_t cellsX{(x1 - x0 + columns - 1) / columns};
  const size_t cellsY{(y1 - y0 + rows - 1) / rows};
  const size_t countingPerTile{
      countingCells.empty() ? columns * rows
                            : size_t(std::count(countingCells.begin(),
                                                countingCells.end(), true))};
  const double budgetCells{double(METER_PIXEL_BUDGET) /
                           double(std::max<size_t>(countingPerTile, 1))};
  mStride = std::max<size_t>(
      size_t(std::floor(std::sqrt(double(cellsX * cellsY) / budgetCells))), 1);
  for (size_t cy = 0; y0 + cy * mStride * rows < y1; cy++) {
    for (size_t dy = 0; dy < rows; dy++) {
      const size_t y{y0 + cy * mStride * rows + dy};
      if (y >= y1) break;
      for (size_t cx = 0; x0 + cx * mStride * columns < x1; cx++) {
        for (size_t dx = 0; dx < columns; dx++) {
          const size_t x{x0 + cx * mStride * columns + dx};
          if (x >= x1) break;
          if (!countingCells.empty() &&
              !countingCells[tileIndexAt(columns, rows, x, y)])
            continue;
          mPixels.push_back(y * numPixelsX + x);
        }
      }
    }
  }
}

bool needsMeter(const Sensor &sensor, const RenderHeader &header) noexcept {
  return !sensor.hasFixedGain() && !(header.meteredISO > 0);
}

void recordMeter(const Sensor &sensor, const MeterReading &reading,
                 double seconds, RenderHeader &header) noexcept {
  const MeteredExposure metered{
      sensor.meter(sensor.luxSecondsOf(reading.meanElectronRate(), seconds))};
  header.meteredLuxSeconds = metered.luxSeconds;
  header.meteredISO = metered.iso;
}

ShotISO resolveShotISO(const Sensor &sensor, const std::optional<float> &stated,
                       const RenderHeader &header) noexcept {
  if (sensor.hasFixedGain()) return ShotISO{sensor.fixedGainISO(), false};
  if (stated) return ShotISO{double(*stated), false};
  SMDL_SANITY_CHECK(header.meteredISO > 0);
  return ShotISO{header.meteredISO, true};
}

void logISO(const Sensor &sensor, const std::optional<float> &stated,
            const RenderHeader &header, double exposure, double fNumber,
            bool wasRead) {
  const ShotISO shot{resolveShotISO(sensor, stated, header)};
  if (sensor.hasFixedGain()) {
    SMDL_LOG_INFO("ISO: ", smdl::Brief(shot.iso, 5),
                  ", the saturation speed of the stated gain, so nothing is "
                  "metered");
    return;
  }
  const MeteredExposure metered{sensor.meter(header.meteredLuxSeconds)};
  const bool isDark{!(metered.luxSeconds > 0)};
  const std::string reading{
      isDark
          ? std::string("the frame is dark, so the meter reads nothing")
          : smdl::concat("the meter reads ", smdl::Brief(metered.luxSeconds, 4),
                         " lux-seconds over the window and asks for ISO ",
                         smdl::Brief(metered.wantedISO, 5))};
  const char *source{wasRead ? " off the resumed file" : ""};
  // Where a given ISO leaves the frame against the meter's wish.
  const auto against{[&](double iso) {
    if (isDark) return std::string();
    const double stops{std::log2(double(iso) / metered.wantedISO)};
    return smdl::concat(", so the frame comes out ",
                        smdl::Brief(std::abs(stops), 3), " stops ",
                        stops >= 0 ? "brighter" : "darker", " than metered");
  }};
  if (stated) {
    SMDL_LOG_INFO("ISO: ", smdl::Brief(shot.iso, 6), " stated; ", reading,
                  source, against(shot.iso));
    if (shot.iso < sensor.baseISO())
      SMDL_LOG_WARN("ISO ", smdl::Brief(shot.iso, 6),
                    " is below the base ISO of ",
                    smdl::Brief(sensor.baseISO(), 5),
                    ", so the well clips before the ADC does");
    else if (shot.iso > sensor.maxISO())
      SMDL_LOG_WARN("ISO ", smdl::Brief(shot.iso, 6),
                    " is above the top ISO of ",
                    smdl::Brief(sensor.maxISO(), 6));
    return;
  }
  if (metered.stopsOff != 0) {
    // The exposure that would meter to the end of the range the frame
    // ran past: the shutter scaled by the ISO ratio, or the stop by its
    // square root.
    const bool isOver{metered.isOverexposed()};
    const double end{isOver ? sensor.baseISO() : sensor.maxISO()};
    const std::string fix{
        isDark
            ? std::string()
            : smdl::concat(
                  ": a shutter of ",
                  spellShutter(exposure * metered.wantedISO / end), ", or f/",
                  smdl::Brief(fNumber * std::sqrt(end / metered.wantedISO), 4),
                  ", would meter to it")};
    SMDL_LOG_WARN(
        "ISO ", smdl::Brief(shot.iso, 6),
        isOver ? ", the base; " : ", the top; ", reading, source,
        ", so the frame is ",
        isDark ? std::string("dark")
               : smdl::concat(smdl::Brief(std::abs(metered.stopsOff), 3),
                              " stops ", isOver ? "over" : "under", "exposed"),
        " at the ", isOver ? "base" : "top", " ISO", fix);
    return;
  }
  SMDL_LOG_INFO("ISO: ", smdl::Brief(shot.iso, 5), " metered", source,
                ", the nearest third stop to the ",
                smdl::Brief(metered.wantedISO, 5), " that ",
                smdl::Brief(metered.luxSeconds, 4),
                " lux-seconds over the window asks for");
}
