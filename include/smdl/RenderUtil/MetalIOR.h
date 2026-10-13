/// \file
#pragma once

#include "smdl/Export.h"
#include "smdl/Support/VectorMath.h"

namespace smdl {

/// \addtogroup renderutil
/// \{

/// The metals with builtin complex IOR tables.
enum Metal : int {
  METAL_AG = 0,           ///< Silver.
  METAL_AL,               ///< Aluminum.
  METAL_AU,               ///< Gold.
  METAL_CO,               ///< Cobalt.
  METAL_CU,               ///< Copper.
  METAL_CUZN,             ///< Copper+Zinc, also known as brass.
  METAL_FE,               ///< Iron.
  METAL_HG,               ///< Mercury.
  METAL_LI,               ///< Lithium.
  METAL_MG,               ///< Magnesium.
  METAL_NA,               ///< Sodium.
  METAL_NI,               ///< Nickel.
  METAL_PB,               ///< Lead.
  METAL_PT,               ///< Platinum.
  METAL_SN,               ///< Tin.
  METAL_TI,               ///< Titanium.
  METAL_ZN,               ///< Zinc.
  METAL_FIRST = METAL_AG, ///< The first valid metal.
  METAL_LAST = METAL_ZN   ///< The last valid metal.
};

/// An entry in a metal IOR lookup table.
struct MetalIORTableEntry final {
  /// The wavelength in nanometers.
  float wavelen{};

  /// The complex index of refraction \f$ n + ik \f$.
  float2 ior{};
};

/// A metal IOR lookup table, see `smdlFindMetalIOR()`.
struct MetalIOR final {
  /// The lookup table sorted by increasing wavelength.
  const MetalIORTableEntry *table{};

  /// The number of lookup table entries. If non-zero, must be at least two.
  int tableSize{};
};

extern "C" {

/// Find the builtin IOR lookup table for the given metal.
///
/// Returns 1 on success. If `metal` is invalid, zeroes out `metalIOR` and
/// returns 0. The returned table points to static storage, so it is valid
/// forever and must not be freed. See `MetalIOR.cc` in the library
/// implementation for the data sources.
SMDL_EXPORT int smdlFindMetalIOR(Metal metal, MetalIOR *metalIOR);

/// Evaluate the complex IOR \f$ n + ik \f$ of the given metal.
///
/// The evaluation linearly interpolates the builtin lookup table of the
/// given metal. The wavelengths in `wavelens` are in nanometers and need
/// not be in any particular order. Wavelengths outside the table domain
/// clamp to the nearest table entry. Either of `iorN` and `iorK` may be
/// null to skip that output. If `metal` is invalid, the outputs are filled
/// with zeros.
SMDL_EXPORT void smdlEvalMetalIOR(Metal metal, int numWavelens,
                                  const float *wavelens, float *iorN,
                                  float *iorK);
}

/// \}

} // namespace smdl
