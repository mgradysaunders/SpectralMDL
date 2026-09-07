/// \file
/// The `doc` subcommand.
#pragma once

#include "smdl/Compiler.h"

struct Options;

/// Extract the documentation from the inputs, add whatever builtin
/// modules the queries or `-builtins` ask for, and print it.
///
/// The whole database prints as JSON or Markdown; a `::`-prefixed query
/// prints as colored plain text unless `-f` asked for one of the other
/// two, which have no per-symbol form.
void runDoc(const Options &opts, smdl::Compiler &compiler);
