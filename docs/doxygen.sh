#!/bin/sh
# Generates the Doxygen API reference into docs/doxygen/. The Doxyfile takes
# its PROJECT_NUMBER from SMDL_VERSION, which this reads from CMakeLists.txt,
# so the published version follows the build's. Quarto runs this before it
# renders the site, and it can be run by hand from any directory.
set -eu
cd "$(dirname "$0")/.."

part() { sed -nE "s/^set\(SMDL_VERSION_$1 ([0-9]+) .*/\1/p" CMakeLists.txt; }
SMDL_VERSION="$(part MAJOR).$(part MINOR).$(part PATCH)"
if ! printf '%s\n' "$SMDL_VERSION" | grep -Eqx '[0-9]+\.[0-9]+\.[0-9]+'; then
  echo "doxygen.sh: cannot read the version from CMakeLists.txt" >&2
  exit 1
fi
export SMDL_VERSION

# Progress goes to stdout and warnings to stderr, so this keeps the warnings.
doxygen >/dev/null
