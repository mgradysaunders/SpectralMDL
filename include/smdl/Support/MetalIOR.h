/// \file
#pragma once

#include "smdl/Export.h"
#include "smdl/Support/VectorMath.h"

namespace smdl {

/// \addtogroup support
/// \{

/// The metals with builtin complex IOR tables.
enum class Metal : int {
  Ag = 0,     ///< Silver.
  Al,         ///< Aluminum.
  Au,         ///< Gold.
  Co,         ///< Cobalt.
  Cu,         ///< Copper.
  CuZn,       ///< Copper+Zinc, also known as brass.
  Fe,         ///< Iron.
  Hg,         ///< Mercury.
  Li,         ///< Lithium.
  Mg,         ///< Magnesium.
  Na,         ///< Sodium.
  Ni,         ///< Nickel.
  Pb,         ///< Lead.
  Pt,         ///< Platinum.
  Sn,         ///< Tin.
  Ti,         ///< Titanium.
  Zn,         ///< Zinc.
  First = Ag, ///< The first valid metal.
  Last = Zn   ///< The last valid metal.
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
/// given metal. The wavelengths in `wavelens` must be in nanometers and,
/// like `State::wavelength_base`, must be sorted in increasing order.
/// Wavelengths outside the table domain clamp to the nearest table entry.
/// Either of `iorN` and `iorK` may be null to skip that output. If `metal`
/// is invalid, the outputs are filled with zeros.
SMDL_EXPORT void smdlEvalMetalIOR(Metal metal, int numWavelens,
                                  const float *wavelens, float *iorN,
                                  float *iorK);
}

/// \}

} // namespace smdl
