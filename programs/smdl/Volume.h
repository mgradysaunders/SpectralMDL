/// \file
/// The `volume` subcommand.
#pragma once

struct Options;

/// Convert the voxel grid inputs to `-output`, whose extension picks the
/// format, or describe what they hold when there is no output.
///
/// This is the one subcommand that needs no `smdl::Compiler` at all: a
/// voxel grid is a resource that stands on its own.
void runVolume(const Options &opts);
