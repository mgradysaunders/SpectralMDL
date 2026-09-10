#!/usr/bin/env python3
"""Convert a measured camera sensitivity into the renderer's '.response'
format.

    response_convert.py IN.json [-o OUT.response] [--name "Canon EOS 5D"]
                        [--peak-qe 0.5]

Nobody measures a sensor for this renderer: the curves come from a
published data set, and this script covers the mechanical half of
getting one in, so the '.response' parser never has to be general.

With --peak-qe the output is a 'qe' response instead: the same curves
scaled so that their largest value is the stated peak quantum
efficiency, electrons per photon, which is what a detector readout
counts electrons from. A published relative curve carries no absolute
scale, so the peak is a guess the user states, and the result is no
better than that guess.

Input format
------------
PhysLight JSON (Winquist and Thurston, "Physlight - Camera Spectral
Sensitivity Curves", 2022, https://doi.org/10.5281/zenodo.6590768,
Apache-2.0), one file per camera body:

    header.manufacturer, header.model     the body, which names the output
    spectral_data.units                   "relative": peak-normalized
    spectral_data.index.main              the channel names, ["R", "G", "B"]
    spectral_data.data.main               wavelength in nm -> one value per
                                          channel, 380 to 780 at 5 nm

What does not convert
---------------------
A file whose units are not "relative", since the format's other kind
(quantum efficiency, electrons per photon) is an absolute measurement
these are not; and a file with no channels. Each is refused rather than
guessed at.
"""

import argparse
import json
import os
import re
import sys


class ConvertError(Exception):
    """A file that cannot be represented, reported as one message."""


def read_physlight(path):
    """The name, the channel names, and the rows of (wavelength, values)."""
    with open(path) as f:
        document = json.load(f)
    header = document.get("header", {})
    spectral = document.get("spectral_data", {})
    units = spectral.get("units")
    if units != "relative":
        raise ConvertError(
            f"{path}: units are {units!r}, and only 'relative' converts")
    channels = spectral.get("index", {}).get("main", [])
    if not channels:
        raise ConvertError(f"{path}: no channels in spectral_data.index.main")
    table = spectral.get("data", {}).get("main", {})
    if not table:
        raise ConvertError(f"{path}: no rows in spectral_data.data.main")
    rows = []
    for key, values in table.items():
        wavelength = float(key)
        if len(values) != len(channels):
            raise ConvertError(
                f"{path}: {len(values)} values at {key} nm for "
                f"{len(channels)} channels")
        rows.append((wavelength, [float(v) for v in values]))
    rows.sort()
    for (a, _), (b, _) in zip(rows, rows[1:]):
        if not b > a:
            raise ConvertError(f"{path}: wavelengths repeat at {b} nm")
    for _, values in rows:
        for v in values:
            if not (v >= 0) or v != v or v in (float("inf"), float("-inf")):
                raise ConvertError(f"{path}: a value is negative or not finite")
    manufacturer = str(header.get("manufacturer", "")).replace("_", " ")
    model = str(header.get("model", "")).replace("_", " ")
    name = (manufacturer + " " + model).strip()
    return name, channels, rows


def band_name(channel):
    """A channel name as the format's identifier: letters, digits, and
    underscores, beginning with a letter."""
    cleaned = re.sub(r"[^A-Za-z0-9_]", "_", str(channel))
    if not cleaned or not (cleaned[0].isalpha() or cleaned[0] == "_"):
        cleaned = "b_" + cleaned
    return cleaned


def file_stem(name):
    """The output stem: the name lower-cased with runs of anything but
    letters and digits as one hyphen."""
    return re.sub(r"[^a-z0-9]+", "-", name.lower()).strip("-")


def write_response(out, name, channels, rows, source_name, peak_qe=None):
    lo = rows[0][0]
    hi = rows[-1][0]
    step = rows[1][0] - rows[0][0] if len(rows) > 1 else 0
    scale = 1.0
    if peak_qe is not None:
        peak = max(v for _, values in rows for v in values)
        if not peak > 0:
            raise ConvertError("every value is zero, so there is no peak to scale")
        scale = peak_qe / peak
    lines = []
    lines.append(f"# {name}: spectral sensitivity as measured by Weta Digital's")
    lines.append("# lightsaber system (Winquist and Thurston, \"Physlight - Camera")
    lines.append("# Spectral Sensitivity Curves\", 2022,")
    lines.append("# https://doi.org/10.5281/zenodo.6590768, Apache-2.0). Relative and")
    lines.append(f"# peak-normalized, {lo:g} to {hi:g} nm at {step:g} nm.")
    if peak_qe is not None:
        lines.append(f"# Scaled to a stated peak quantum efficiency of {peak_qe:g}")
        lines.append("# electrons per photon, which the measurement does not carry.")
    lines.append(f"# Converted by response_convert.py from {source_name}.")
    lines.append("response {")
    lines.append(f"  name \"{name}\"")
    lines.append("  kind qe" if peak_qe is not None else "  kind relative")
    width = max(len(f"{w:g}") for w, _ in rows)
    for c, channel in enumerate(channels):
        lines.append(f"  band {band_name(channel)} {{")
        for wavelength, values in rows:
            lines.append(f"    {wavelength:>{width}g} {scale * values[c]:.8g}")
        lines.append("  }")
    lines.append("}")
    out.write("\n".join(lines) + "\n")


def main():
    parser = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    parser.add_argument("input", help="the PhysLight '.json' to convert")
    parser.add_argument("-o", "--output",
                        help="the '.response' to write (default: named from "
                             "the body, beside the input)")
    parser.add_argument("--name", help="the name to write instead of the "
                                       "manufacturer and model")
    parser.add_argument("--peak-qe", type=float,
                        help="write a 'qe' response with the curves scaled "
                             "to this peak quantum efficiency in electrons "
                             "per photon")
    args = parser.parse_args()
    if args.peak_qe is not None and not args.peak_qe > 0:
        print("error: --peak-qe must be positive", file=sys.stderr)
        return 1
    try:
        name, channels, rows = read_physlight(args.input)
    except (OSError, ValueError, ConvertError) as error:
        print(f"error: {error}", file=sys.stderr)
        return 1
    if args.name:
        name = args.name
    output = args.output
    if not output:
        output = os.path.join(os.path.dirname(args.input),
                              file_stem(name) + ".response")
    try:
        with open(output, "w") as out:
            write_response(out, name, channels, rows,
                           os.path.basename(args.input), args.peak_qe)
    except ConvertError as error:
        print(f"error: {error}", file=sys.stderr)
        return 1
    kind = "qe" if args.peak_qe is not None else "relative"
    print(f"{output}: {name}, {len(channels)} {kind} band(s) of "
          f"{len(rows)} knots")
    return 0


if __name__ == "__main__":
    sys.exit(main())
