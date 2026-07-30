/// \file
#pragma once

#include "smdl/Export.h"
#include "smdl/Support/VectorMath.h"

namespace smdl {

/// \addtogroup Support
/// \{

extern "C" {

/// Convert correlated color temperature to CIE 1931 chromaticity.
///
/// This evaluates the daylight locus defined in CIE 15, which is only valid
/// for temperatures between 4000K and 25000K, so the given temperature in
/// `kelvin` is clamped to this range. For reference, some standard
/// temperatures and the associated chromaticities:
/// - D50: 5003K => (0.34567, 0.35850)
/// - D55: 5503K => (0.33242, 0.34743)
/// - D65: 6504K => (0.31272, 0.32903)
/// - D75: 7504K => (0.29902, 0.31485)
///
SMDL_EXPORT void smdlKelvinToChromaticity(float kelvin, float2 *xy);

/// Evaluate CIE standard illuminant D for the chromaticity `xy` on the
/// daylight locus, e.g., as computed by `smdlKelvinToChromaticity()`.
///
/// The evaluation linearly interpolates the S0, S1, and S2 daylight
/// components from CIE 15, which are tabulated in 10nm steps between 300nm
/// and 830nm. The wavelengths in `wavelens` must be in nanometers, and
/// wavelengths outside the table evaluate to zero. The resulting relative
/// spectral power in `illum` is normalized to be 1 at 560nm.
///
SMDL_EXPORT void smdlEvalIlluminantD(int numWavelens, const float *wavelens,
                                     float *illum, const float2 &xy);

/// Evaluate CIE standard fluorescent illuminant F1 through F12, as
/// indicated by `number` being 1 through 12.
///
/// The evaluation linearly interpolates the fluorescent tables from CIE 15,
/// which are tabulated in 5nm steps between 380nm and 780nm. The wavelengths
/// in `wavelens` must be in nanometers, and wavelengths outside the table
/// evaluate to zero. The resulting relative spectral power in `illum` is
/// scaled by 1/100 to be consistent with the illuminant D normalization. If
/// `number` is out of range, `illum` is filled with zeros.
///
SMDL_EXPORT void smdlEvalIlluminantF(int numWavelens, const float *wavelens,
                                     float *illum, int number = 1);
}

/// \}

} // namespace smdl
