#include "Fixtures.h"

#include "Output.h"

TEST_CASE("Output: the RGB picture's format follows its extension") {
  SUBCASE("An OpenEXR or Radiance name holds floats, whatever its case") {
    CHECK(hasFloatImageExtension("out.exr"));
    CHECK(hasFloatImageExtension("out.hdr"));
    CHECK(hasFloatImageExtension("OUT.EXR"));
    CHECK(hasFloatImageExtension("frames/shot.part.hdr"));
  }
  SUBCASE("Any other name is tone mapped to 8 bits") {
    CHECK_FALSE(hasFloatImageExtension("out.png"));
    CHECK_FALSE(hasFloatImageExtension("out.jpg"));
    CHECK_FALSE(hasFloatImageExtension("out"));
    CHECK_FALSE(hasFloatImageExtension("exr"));
    CHECK_FALSE(hasFloatImageExtension(""));
  }
}
