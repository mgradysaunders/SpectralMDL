#include "RenderFixtures.h"

#include <cmath>
#include <vector>

#include "Sensor/Meter.h"

// The meter reads the scene through the sensor's most sensitive band at
// a sparse lattice of pixels before the first sample. What matters is
// that the lattice reads whole tiles of the band's pixels at the stride
// that keeps the budget, that the projection is the peak band's on the
// grid of its own pixels, that a reading records the rung the shot is
// read out at, and that the ISO comes from the gain, a stated number, or
// the record, in that order.

namespace {

constexpr double PHOTONS_PER_JOULE_NM{1e-9 / (PLANCK * SPEED_OF_LIGHT)};

// A sensor of 4 um pixels with one flat `qe` band of 0.5 from 400 to 700
// nm, base ISO 100, top 6400.
[[nodiscard]] SensorSettings flatSensor() {
  SensorSettings value{};
  value.pixels = int2(4, 4);
  value.pitchUM = float2(4.0f, 4.0f);
  value.response.kind = ResponseKind::QE;
  ResponseBand &band{value.response.bands.emplace_back()};
  band.name = "L";
  band.wavelengths = {400.0f, 700.0f};
  band.values = {0.5f, 0.5f};
  value.detector.bits = 16;
  value.detector.blackLevel = 0.0f;
  value.detector.baseISO = 100.0f;
  value.detector.maxISO = 6400.0f;
  return value;
}

// The flat sensor with two `qe` bands under a 2 by 2 tile, R G / G B
// with B the R band again: a dim red over 550 to 700 nm and a bright
// green over 400 to 500 nm, so that green is the peak. Values exact in
// a float, so a hand integral in double agrees to the last bit.
[[nodiscard]] SensorSettings tiledSensor() {
  SensorSettings value{flatSensor()};
  value.response.bands.clear();
  ResponseBand &red{value.response.bands.emplace_back()};
  red.name = "R";
  red.wavelengths = {550.0f, 700.0f};
  red.values = {0.125f, 0.125f};
  ResponseBand &green{value.response.bands.emplace_back()};
  green.name = "G";
  green.wavelengths = {400.0f, 500.0f};
  green.values = {0.75f, 0.75f};
  value.response.cfaColumns = 2;
  value.response.cfa = {0, 1, 1, 0};
  return value;
}

[[nodiscard]] std::vector<float> fineGrid() {
  std::vector<float> grid{};
  for (float w = 400; w <= 700; w += 10) grid.push_back(w);
  return grid;
}

[[nodiscard]] WavelengthCells listOf(std::vector<float> list) {
  return WavelengthCells::fromWavelengths(
      smdl::Span<const float>(list.data(), list.size()));
}

// The hand integral of a flat `qe` under a flat irradiance `E` over
// `grid` held still.
[[nodiscard]] double flatOn(const WavelengthGrid &grid, double qe, double E) {
  double expected{};
  for (size_t i = 0; i < grid.size(); i++)
    expected += qe * E * grid.widths[i] * double(grid.wavelengths[i]) *
                PHOTONS_PER_JOULE_NM;
  return expected;
}

} // namespace

TEST_CASE("Meter: the lattice") {
  SUBCASE("A window under the budget is read at every pixel, in frame order") {
    const MeterLattice lattice{int4{2, 1, 6, 4}, 8, 0, 0, {}};
    CHECK(lattice.stride() == 1);
    REQUIRE(lattice.pixels().size() == 12);
    CHECK(lattice.pixels()[0] == 1 * 8 + 2);
    CHECK(lattice.pixels()[3] == 1 * 8 + 5);
    CHECK(lattice.pixels()[4] == 2 * 8 + 2);
    CHECK(lattice.pixels()[11] == 3 * 8 + 5);
  }
  SUBCASE("A window over the budget is read one pixel per cell of the "
          "stride that keeps the budget") {
    // A million pixels over a budget of 262144 is a stride of 2, which
    // reads exactly the budget.
    const MeterLattice lattice{int4{0, 0, 1024, 1024}, 1024, 0, 0, {}};
    CHECK(lattice.stride() == 2);
    REQUIRE(lattice.pixels().size() == METER_PIXEL_BUDGET);
    CHECK(lattice.pixels()[0] == 0);
    CHECK(lattice.pixels()[1] == 2);
    CHECK(lattice.pixels()[512] == 2 * 1024);
    // Half a stride more of window rounds the stride down, so the budget
    // is always met.
    const MeterLattice wider{int4{0, 0, 1500, 1024}, 1500, 0, 0, {}};
    CHECK(wider.stride() == 2);
    CHECK(wider.pixels().size() >= METER_PIXEL_BUDGET);
  }
  SUBCASE("Under a tile a whole tile is read per cell, keeping the band's "
          "pixels") {
    // The band at the tile's two G cells, over a window whose origin is
    // not on a tile: the tile read at each cell origin covers every tile
    // position once, and the band's positions are frame-aligned.
    const std::vector<bool> counting{false, true, true, false};
    const MeterLattice lattice{int4{1, 1, 9, 9}, 16, 2, 2, counting};
    CHECK(lattice.stride() == 1);
    REQUIRE(lattice.pixels().size() == 32);
    for (const auto index : lattice.pixels()) {
      const size_t x{index % 16};
      const size_t y{index / 16};
      CHECK(x >= 1);
      CHECK(x < 9);
      CHECK(y >= 1);
      CHECK(y < 9);
      CHECK(counting[tileIndexAt(2, 2, x, y)]);
    }
    // The stride counts the band's pixels, two per tile, toward the
    // budget: a frame of 1024 by 1024 holds 512 by 512 tiles and 524288
    // of those pixels, twice the budget, so the stride is 1, since 2
    // would read half the budget; a frame of 2048 by 2048 holds four
    // times as many and strides by 2.
    const MeterLattice large{int4{0, 0, 1024, 1024}, 1024, 2, 2, counting};
    CHECK(large.stride() == 1);
    CHECK(large.pixels().size() == 2 * METER_PIXEL_BUDGET);
    const MeterLattice larger{int4{0, 0, 2048, 2048}, 2048, 2, 2, counting};
    CHECK(larger.stride() == 2);
    CHECK(larger.pixels().size() == 2 * METER_PIXEL_BUDGET);
  }
  SUBCASE("A window narrower than a tile still reads what it holds") {
    const MeterLattice lattice{int4{0, 0, 1, 1}, 4, 2, 2, {}};
    REQUIRE(lattice.pixels().size() == 1);
    CHECK(lattice.pixels()[0] == 0);
  }
  SUBCASE("No cell of the band reads nothing, and so does no window") {
    const MeterLattice none{
        int4{0, 0, 8, 8}, 8, 2, 2, {false, false, false, false}};
    CHECK(none.pixels().empty());
    const MeterLattice empty{int4{0, 0, 0, 0}, 8, 0, 0, {}};
    CHECK(empty.pixels().empty());
  }
}

TEST_CASE("Meter: the projection and the record") {
  const Sensor sensor{flatSensor()};
  ScopedGrid scoped{fineGrid(), false};
  const MeterProjection projection{sensor};
  const Color &wavelengths{scoped.wavelengths()};
  Color flat{};
  for (size_t i = 0; i < flat.size(); i++) flat[i] = 1;
  SUBCASE("Without a tile the projection is the band on the one grid, and "
          "every pixel counts") {
    CHECK(projection.bandName() == "L");
    CHECK(projection.countingCells().empty());
    CHECK(projection.project(smdl::Span<const float>(wavelengths),
                             smdl::Span<const float>(flat)) ==
          doctest::Approx(flatOn(gRenderGrid.first(), 0.5, 1.0)).epsilon(1e-9));
  }
  SUBCASE("A reading records the exposure and the rung, and the sequence "
          "then needs no meter") {
    RenderHeader header{};
    CHECK(needsMeter(sensor, header));
    // A D55 field of 2.03 lux for 10 ms: 0.0203 lux-seconds, ISO 400.
    const SensorSpectrum d55{daylightSpectrum(D55_KELVIN)};
    const double rate{sensor.electronRate(0, d55) * 2.03 /
                      Sensor::illuminance(d55)};
    MeterReading reading{};
    reading.sum = 7.0 * rate;
    reading.count = 7;
    CHECK(reading.meanElectronRate() == doctest::Approx(rate));
    recordMeter(sensor, reading, 0.01, header);
    CHECK(header.meteredLuxSeconds == doctest::Approx(0.0203).epsilon(1e-9));
    CHECK(header.meteredISO == 400.0);
    CHECK(!needsMeter(sensor, header));
    // Nothing read is a dark frame, which asks for everything and gets
    // the top.
    RenderHeader dark{};
    recordMeter(sensor, MeterReading{}, 0.01, dark);
    CHECK(dark.meteredLuxSeconds == 0.0);
    CHECK(dark.meteredISO == 6400.0);
    CHECK(!needsMeter(sensor, dark));
  }
  SUBCASE("The shot's ISO is the gain's, else the stated one, else the "
          "record's") {
    RenderHeader header{};
    header.meteredLuxSeconds = 0.0203;
    header.meteredISO = 400.0;
    const ShotISO metered{resolveShotISO(sensor, std::nullopt, header)};
    CHECK(metered.iso == 400.0);
    CHECK(metered.wasMetered);
    const ShotISO stated{resolveShotISO(sensor, 800.0f, header)};
    CHECK(stated.iso == 800.0);
    CHECK(!stated.wasMetered);
    SensorSettings fixed{flatSensor()};
    fixed.detector.gain = 0.5f;
    const Sensor fixedSensor{fixed};
    CHECK(!needsMeter(fixedSensor, RenderHeader{}));
    const ShotISO gained{resolveShotISO(fixedSensor, std::nullopt, header)};
    CHECK(gained.iso == doctest::Approx(fixedSensor.fixedGainISO()));
    CHECK(!gained.wasMetered);
  }
}

TEST_CASE("Meter: under a tile the projection is the band's own grid over "
          "its own cells") {
  // Two grids of seven bands, the red band's over 550 to 700 nm and the
  // green band's over 400 to 500 nm, laid over the tile as the sensor's
  // bands are. Green is the peak, so the meter reads the G cells on the
  // second grid.
  const std::vector<WavelengthCells> family{
      listOf({550, 575, 600, 625, 650, 675, 700}),
      listOf({400, 417, 433, 450, 467, 483, 500})};
  const ScopedGrid tiled{family, 2, {0, 1, 1, 0}, false};
  const Sensor sensor{tiledSensor()};
  REQUIRE(sensor.peakBand() == 1);
  const MeterProjection projection{sensor};
  CHECK(projection.bandName() == "G");
  CHECK(projection.countingCells() ==
        std::vector<bool>{false, true, true, false});
  const WavelengthGrid &grid{gRenderGrid.grids[1]};
  Color flat{};
  for (size_t i = 0; i < flat.size(); i++) flat[i] = 1;
  CHECK(projection.project(
            smdl::Span<const float>(grid.wavelengths.data(), grid.size()),
            smdl::Span<const float>(flat)) ==
        doctest::Approx(flatOn(grid, 0.75, 1.0)).epsilon(1e-9));
}
