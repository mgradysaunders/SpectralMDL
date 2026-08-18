#!/usr/bin/env bash
#
# Build, validate, or symlink the SpectralMDL layout add-on.
#
#   build_addon.sh                 validate and build the zip
#   build_addon.sh -o /tmp         build it somewhere else
#   build_addon.sh --link          symlink it for development
#   build_addon.sh --unlink        remove that symlink
#
# The zip is for installing the add-on somewhere else. For working on it,
# use --link instead: it symlinks the source directory into Blender's local
# extension repository, so Blender loads the working tree directly and there
# is no zip to rebuild and no reinstall between edits. Enable it once under
# Edit > Preferences > Add-ons, then after each edit use Reload Scripts
# (F3 > "Reload Scripts", or the Blender icon menu in the top bar).
#
# Rebuild the zip only to hand the add-on to someone else, or to test that
# what installs from a zip is what you have been developing against.

set -euo pipefail

BLENDER="${BLENDER:-blender}"
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SOURCE="${HERE}/smdl_layout"
OUTPUT="$(cd "${HERE}/.." && pwd)/build"
ACTION="build"

while [[ $# -gt 0 ]]; do
  case "$1" in
    -o|--output-dir) OUTPUT="$2"; shift 2 ;;
    --link) ACTION="link"; shift ;;
    --unlink) ACTION="unlink"; shift ;;
    -h|--help) sed -n '2,20p' "${BASH_SOURCE[0]}" | sed 's/^# \?//'; exit 0 ;;
    *) echo "unknown argument $1 (see --help)" >&2; exit 2 ;;
  esac
done

if ! command -v "${BLENDER}" >/dev/null 2>&1; then
  echo "cannot find Blender as '${BLENDER}'; set BLENDER to its path" >&2
  exit 1
fi

# Blender keeps its local extension repository beside its configuration, one
# directory per version, so the link has to go in the one that is running.
version="$("${BLENDER}" --version | head -1 | cut -d' ' -f2 | cut -d. -f1,2)"
repository="${HOME}/.config/blender/${version}/extensions/user_default"
link="${repository}/smdl_layout"

case "${ACTION}" in
  link)
    mkdir -p "${repository}"
    ln -sfn "${SOURCE}" "${link}"
    echo "linked ${link} -> ${SOURCE}"
    echo "enable it once under Edit > Preferences > Add-ons, then use"
    echo "Reload Scripts after each edit"
    ;;
  unlink)
    if [[ -L "${link}" ]]; then
      rm "${link}"
      echo "removed ${link}"
    else
      echo "nothing linked at ${link}"
    fi
    ;;
  build)
    mkdir -p "${OUTPUT}"
    # Validation is separate from the build and catches manifest mistakes
    # with a better message than the build does.
    "${BLENDER}" --command extension validate "${SOURCE}" 2>&1 |
      grep -E "^(Success|Error|Warning)" || true
    "${BLENDER}" --command extension build \
      --source-dir "${SOURCE}" --output-dir "${OUTPUT}" 2>&1 |
      grep -E "^(created|complete|Error)" || true
    ;;
esac
