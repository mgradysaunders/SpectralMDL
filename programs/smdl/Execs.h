/// \file
/// Running what the JIT produced.
#pragma once

#include "smdl/Compiler.h"

struct Options;

/// The `run` subcommand, which is also the first half of `test`. JIT
/// compiles, then runs every `exec` the inputs declare.
void runExecs(const Options &opts, smdl::Compiler &compiler);

/// The second half of `test`: run every `unit_test` against the state
/// the command line describes. Call after `runExecs()`, which is what
/// JIT compiles.
void runUnitTests(const Options &opts, smdl::Compiler &compiler);
