/// \file
/// The compiler the subcommands share, and the three that are thin
/// enough to be nothing but a call into it.
#pragma once

#include <string_view>

#include "smdl/Compiler.h"

struct Options;

/// Apply the settings a compile depends on and add every input the
/// command line named.
///
/// The compiler must outlive every use of the code it emits, because the
/// JIT'd material code embeds absolute pointers into the data it owns.
void setUpCompiler(const Options &opts, smdl::Compiler &compiler);

/// Write to `-output`, or to standard output when it was not given.
void writeOutput(const Options &opts, std::string_view text);

/// The `dump` subcommand. Compiles first: `dump()` reads the module
/// after optimization and before the JIT takes it.
void runDump(const Options &opts, smdl::Compiler &compiler);

/// The `list` subcommand.
void runList(const Options &opts, smdl::Compiler &compiler);

/// The `format` subcommand, which formats the sources as they were
/// parsed and never compiles them.
void runFormat(const Options &opts, smdl::Compiler &compiler);
