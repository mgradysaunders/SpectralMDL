/// \file
/// The layout toolchain's commands beyond rendering: the object and
/// material tables behind `-list-objects` and `-list-materials`, and
/// the `.places` and `.curves` utilities behind `-pack-places`,
/// `-dump-places`, and `-dump-curves`.
#pragma once

#include <string>

#include "Layout/Layout.h"

namespace smdl {
class Compiler;
}

/// Print what each scene file offers to `select`.
///
/// Every file is listed once and in full, with no selection applied,
/// since this is what a user reads in order to write a selection in the
/// first place.
void printObjectTable(const Layout &layout);

/// Print what each scene file offers to `select`, as JSON: the
/// machine-readable form of `printObjectTable()`. This is how asset
/// preparation tooling learns object paths, pivots, and material names;
/// assimp is the authority on all three, since it is what `select`
/// matches against and what the renderer places.
void printObjectTableJSON(const Layout &layout);

/// Print the material names that the scene needs, and how each one
/// resolves.
///
/// `compiler` is null if no MDL modules were given, in which case only
/// the names and the identifier check are reported, which is the point
/// of running this before any MDL has been written.
void printMaterialTable(const smdl::Compiler *compiler, const Layout &layout);

/// Print the material names that the scene needs, and how each one
/// resolves, as JSON. Same content as `printMaterialTable()`.
void printMaterialTableJSON(const smdl::Compiler *compiler,
                            const Layout &layout);

/// Pack a layout's top-level `place` statements into a `.places`
/// buffer: each place becomes one record carrying its transform, and
/// the distinct per-place override sets become the variant table, which
/// is printed as the `variant` blocks of a suggested `place <name> *`
/// wrapper. The layout must place exactly one asset or group name.
///
/// \throws smdl::Error  On parse errors, mixed content, or write
///                      failure.
void packPlaces(const std::string &layoutFileName, std::string outputFileName);

/// Print a `.places` buffer as the one-line place text `packPlaces()`
/// consumes, one `place` per record with its variant's overrides
/// inline, under the placeholder name `thing`. Dump, edit, re-pack.
///
/// \throws smdl::Error  If the buffer cannot be read.
void dumpPlaces(const std::string &fileName);

/// Print a `.curves` summary: version, basis, counts, bounds, radii,
/// and whether the root UV column is present. The `-dump-curves` body.
///
/// \throws smdl::Error  If the file cannot be read.
void dumpCurves(const std::string &fileName);
