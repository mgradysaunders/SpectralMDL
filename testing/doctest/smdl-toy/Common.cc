#include "Fixtures.h"

#include <limits>
#include <string>

#include "Common.h"

TEST_CASE("Spelling: the dimensions") {
  SUBCASE("Whole sizes are written whole, whatever integer they come in") {
    CHECK(smdl::concat(SpellDimensions(1280, 720)) == "1280x720");
    CHECK(smdl::concat(SpellDimensions(size_t(4368), size_t(2912))) ==
          "4368x2912");
    CHECK(smdl::concat(SpellDimensions(int2(600, 400))) == "600x400");
    CHECK(smdl::concat(SpellDimensions(2, size_t(2))) == "2x2");
  }
  SUBCASE("Measured sizes take the decimal places asked for") {
    CHECK(smdl::concat(SpellDimensions(float2(35.816f, 23.877f), 2)) ==
          "35.82x23.88");
    CHECK(smdl::concat(SpellDimensions(float2(35.816f, 23.877f), 3)) ==
          "35.816x23.877");
    CHECK(smdl::concat(SpellDimensions(double2(8.2, 8.2), 2)) == "8.20x8.20");
    // The places stay as asked, trailing zeros and all, so that a pair
    // reads as one measurement however the two differ.
    CHECK(smdl::concat(SpellDimensions(36.0, 24.0)) == "36.0x24.0");
    CHECK(smdl::concat(SpellDimensions(1.12f, 1.4f)) == "1.1x1.4");
  }
  SUBCASE("A size that is not a number is written as what it is") {
    const double inf{std::numeric_limits<double>::infinity()};
    CHECK(smdl::concat(SpellDimensions(inf, -inf)) == "infx-inf");
    CHECK(smdl::concat(SpellDimensions(-2.5, 0.0)) == "-2.5x0.0");
  }
}

TEST_CASE("Spelling: the wavelength range") {
  SUBCASE("The range carries its unit and one decimal place") {
    CHECK(smdl::concat(SpellWavelengthRange(380.0, 780.0)) == "380.0-780.0 nm");
    CHECK(smdl::concat(SpellWavelengthRange(559.5f, 700.25f)) ==
          "559.5-700.2 nm");
    CHECK(smdl::concat(SpellWavelengthRange(380.0, 780.0, 0)) == "380-780 nm");
    CHECK(smdl::concat(SpellWavelengthRange(1.0 / 3.0, 2.0 / 3.0, 3)) ==
          "0.333-0.667 nm");
  }
  SUBCASE("The range composes with the rest of a message") {
    CHECK(smdl::concat("over ", SpellWavelengthRange(380.0, 780.0),
                       ", from ") == "over 380.0-780.0 nm, from ");
  }
}
