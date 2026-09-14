#include "Common.h"

#include <cstdio>

namespace {

// The one way either spelling writes a number: the decimal places asked
// for, trailing zeros and all, so that the two halves of a measurement
// line up however their magnitudes differ. This is what `SpellFixed`
// would write if its digits were places rather than significant digits.
void appendPlaces(std::string &result, double value, int decimals) {
  // NOLINTNEXTLINE
  char buffer[512]{};
  std::snprintf(buffer, sizeof(buffer), "%.*f", std::clamp(decimals, 0, 9),
                value);
  result += buffer;
}

} // namespace

void SpellDimensions::appendTo(std::string &result) const {
  appendPlaces(result, mX, mDecimals);
  result += 'x';
  appendPlaces(result, mY, mDecimals);
}

void SpellWavelengthRange::appendTo(std::string &result) const {
  appendPlaces(result, mWavelengthMin, mDecimals);
  result += '-';
  appendPlaces(result, mWavelengthMax, mDecimals);
  result += " nm";
}
