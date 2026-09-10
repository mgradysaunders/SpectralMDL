/// \file
/// Everything the command line asks for, lowered into plain values.
///
/// The `cl::opt` objects themselves live in `Options.cc` and go no
/// further: every other translation unit takes an `Options` and never
/// knows a command line was involved.
///
/// The groups below are the `cl::OptionCategory` groups the help text
/// prints, one struct apiece, so that where a setting lives here and
/// where a user finds it are the same question. Which of them a given
/// subcommand fills is the same question again, since a category is
/// registered on exactly the subcommands that read it.
#pragma once

#include <string>
#include <vector>

#include "smdl/Common.h"
#include "smdl/Compiler.h"
#include "smdl/Support/Logger.h"

#include "../CommandLine.h"

/// Which subcommand ran. There is always exactly one; `cl` rejects a
/// command line naming none.
enum class Subcommand {
  DUMP,
  LIST,
  RUN,
  TEST,
  FORMAT,
  DOC,
  VOLUME,
};

//--{ Compile Options
/// What the compiler bakes into the material code.
struct CompileOptions final {
  smdl::OptLevel optLevel{smdl::OPT_LEVEL_O2};

  bool isDebugEnabled{};

  /// The wavelength grid the material code is compiled for, whose size
  /// is `Compiler::wavelengthBaseMax`. Resolved from
  /// `-wavelength-range`, or taken verbatim from `-wavelengths`.
  std::vector<float> wavelengths{};

  /// The endpoints `state::wavelength_min()` and `wavelength_max()`
  /// report. These are what `-wavelength-range` states rather than the
  /// ends of the grid above, which differ for a single band: one
  /// wavelength sits at the midpoint of the range it samples.
  smdl::float2 wavelengthRange{WAVELENGTH_MIN, WAVELENGTH_MAX};
};
//--}

//--{ Doc Options
/// How the `doc` subcommand renders what it extracted.
enum class DocFormat {
  /// Plain text, colored, for symbol and module queries. Whole-database
  /// output has no plain text form and falls back to Markdown.
  TEXT,
  JSON,
  MARKDOWN,
};

struct DocOptions final {
  DocFormat format{DocFormat::TEXT};

  /// Show declarations not marked `export` and those named with a
  /// leading underscore.
  bool shouldIncludeHidden{};

  /// Document every builtin module, rather than only those a query
  /// names.
  bool allBuiltins{};
};
//--}

//--{ Output Options
/// Where the one thing a subcommand produces goes, and in what form.
struct OutputOptions final {
  smdl::DumpFormat dumpFormat{smdl::DUMP_FORMAT_IR};

  /// The destination file. `given` is what distinguishes a file from
  /// standard output, which is where an unstated `-output` goes.
  Flag<std::string> fileName{};
};
//--}

//--{ State Options
/// The `smdl::State` a unit test is evaluated against. The wavelength
/// grid is not here: it is a compile input first, and lives with the
/// rest of those.
struct StateOptions final {
  float time{};

  int objectID{};

  smdl::float3 texCoord{};

  int ptexFaceID{};

  smdl::float2 ptexFaceUV{};
};
//--}

//--{ Utility Options
/// How the run is presented and scheduled. Shared, name for name, with
/// the renderer's group of the same name.
struct UtilityOptions final {
  unsigned threads{};

  /// The lowest level of log message to print, which `main()` hands the
  /// logger before anything else can say anything.
  smdl::LogLevel logLevel{smdl::LOG_LEVEL_INFO};

  /// Whether the unit test report and the documentation text colorize,
  /// which they decide for themselves when this is `COLOR_MODE_AUTO`.
  smdl::ColorMode colorMode{smdl::COLOR_MODE_AUTO};

  /// Whether log messages are labeled with Unicode symbols, which the
  /// log sink decides for itself when this is `UNICODE_MODE_AUTO`.
  smdl::UnicodeMode unicodeMode{smdl::UNICODE_MODE_AUTO};

  /// The time-trace file, and whether `-profile` was given at all,
  /// since it takes an optional value.
  std::string profile{};

  bool isProfiling{};
};
//--}

//--{ Volume Options
/// The voxel grid conversion, which needs no compiler at all.
struct VolumeOptions final {
  /// The grid to read from each NanoVDB input and to name in a NanoVDB
  /// output: one per input, or empty for none at all.
  std::vector<std::string> gridNames{};

  /// The destination, whose extension picks the format. Empty describes
  /// the inputs instead of converting them.
  std::string fileName{};
};
//--}

/// Everything the command line asked for, grouped as the help text
/// groups it.
struct Options final {
  Subcommand subcommand{};

  /// The positional arguments the compiler is given, which are file and
  /// directory names for every subcommand.
  std::vector<std::string> inputs{};

  /// The `::`-prefixed positional arguments, which `doc` alone accepts
  /// and reads as symbol and module queries rather than file names.
  std::vector<std::string> docQueries{};

  CompileOptions compile{};

  DocOptions doc{};

  /// The library's own type, filled straight from the `format` flags.
  smdl::FormatOptions format{};

  OutputOptions output{};

  StateOptions state{};

  UtilityOptions utility{};

  VolumeOptions volume{};
};

/// Parse and validate the command line.
///
/// Everything knowable without reading an input is checked here, so that
/// a typo or an out-of-range value fails before anything is loaded or
/// compiled.
///
/// \throws smdl::Error  If any flag is malformed or out of range.
///
[[nodiscard]] Options parseCommandLine(int argc, char **argv);
