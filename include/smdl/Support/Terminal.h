/// \file
#pragma once

#include "smdl/Export.h"

namespace smdl {

/// \addtogroup support
/// \{

/// \name Functions (terminal)
/// \{

/// Whether output is colored with ANSI escape codes.
enum class ANSIColorMode : int {
  AUTO,   ///< Colorize a terminal, if the environment allows.
  ALWAYS, ///< Colorize even if the stream is redirected.
  NEVER   ///< Never colorize.
};

/// Resolve `mode` for a stream, given whether the stream is a terminal.
///
/// `ANSIColorMode::AUTO` also wants the environment to allow colors:
/// `NO_COLOR` unset or empty (the no-color.org convention), and `TERM`
/// set to something other than `dumb`. The explicit modes override
/// both, as that convention asks.
///
/// This is the whole of the library's color policy, and it is public
/// so that a host coloring its own output for a different stream
/// resolves `-color` (or whatever it calls the option) the same way.
[[nodiscard]] SMDL_EXPORT bool shouldUseColors(ANSIColorMode mode,
                                               bool isTerminal) noexcept;

/// \}

/// \}

} // namespace smdl
