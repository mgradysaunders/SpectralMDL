#!/usr/bin/env python3
"""Convert a measured camera sensitivity and the body's published facts
into the renderer's '.sensor' format.

    sensor_convert.py IN.json --pixels W H (--pitch UM | --size W H)
                      [--bits N] [--base-iso N] [--peak-qe QE]
                      [--cfa rggb|none] [--readout S] [--name NAME]
                      [--comment TEXT] [-o OUT.sensor]

Nobody measures a sensor for this renderer: the curves come from a
published data set, and this script covers the mechanical half of
getting one in, so the '.sensor' parser never has to be general.

The curves are written as they were measured, 'relative' and
peak-normalized, and the file states the peak quantum efficiency the
renderer scales them to. A published relative curve carries no absolute
scale, so the peak is a guess the user states, and a readout is no
better than that guess. The pixel count, the pitch or the frame size,
the bit depth, and the rated base ISO are the maker's published figures;
the rest of the detector is left to the format's generic defaults, which
the file does not restate.

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


def write_sensor(out, name, channels, rows, source_name, body):
    """Write one '.sensor' file. `body` holds the published facts:
    'pixels' (a pair), 'pitch' (a number) or 'size' (a pair), and the
    optional 'bits', 'base_iso', 'peak_qe', 'cfa', 'readout', and
    'comment'."""
    lo = rows[0][0]
    hi = rows[-1][0]
    step = rows[1][0] - rows[0][0] if len(rows) > 1 else 0
    names = [band_name(c) for c in channels]
    cfa = body.get("cfa", "rggb" if names == ["R", "G", "B"] else "none")
    if cfa == "rggb" and names != ["R", "G", "B"]:
        raise ConvertError("an RGGB tile needs the bands R, G, and B")
    lines = []
    lines.append(f"# {name}: spectral sensitivity as measured by Weta Digital's")
    lines.append("# lightsaber system (Winquist and Thurston, \"Physlight - Camera")
    lines.append("# Spectral Sensitivity Curves\", 2022,")
    lines.append("# https://doi.org/10.5281/zenodo.6590768, Apache-2.0). Relative and")
    lines.append(f"# peak-normalized, {lo:g} to {hi:g} nm at {step:g} nm, scaled by the")
    lines.append("# renderer to a stated peak quantum efficiency the measurement does")
    lines.append("# not carry.")
    lines.append("# The pixels, the pitch, the bit depth, and the rated base ISO are")
    lines.append("# the maker's published figures; the rest of the detector is the")
    lines.append("# format's generic default.")
    if body.get("comment"):
        for line in body["comment"].split("\n"):
            lines.append(f"# {line}".rstrip())
    lines.append(f"# Converted by sensor_convert.py from {source_name}.")
    lines.append("sensor {")
    lines.append(f"  name \"{name}\"")
    lines.append(f"  pixels {body['pixels'][0]} {body['pixels'][1]}")
    if "pitch" in body:
        lines.append(f"  pitch {body['pitch']:g}")
    else:
        lines.append(f"  size {body['size'][0]:g} {body['size'][1]:g}")
    lines.append("  response {")
    lines.append("    kind relative")
    lines.append(f"    peak_qe {body.get('peak_qe', 0.5):g}")
    width = max(len(f"{w:g}") for w, _ in rows)
    for c, band in enumerate(names):
        lines.append(f"    band {band} {{")
        for wavelength, values in rows:
            lines.append(f"      {wavelength:>{width}g} {values[c]:.8g}")
        lines.append("    }")
    if cfa == "rggb":
        lines.append("    cfa { row R G  row G B }")
    lines.append("  }")
    detector = []
    if body.get("base_iso") is not None:
        detector.append(f"    base_iso {body['base_iso']:g}")
    if body.get("bits") is not None:
        detector.append(f"    bits {body['bits']}")
    if detector:
        lines.append("  detector {")
        lines.extend(detector)
        lines.append("  }")
    if body.get("readout") is not None:
        lines.append(f"  readout {body['readout']:g}")
    lines.append("}")
    out.write("\n".join(lines) + "\n")


def main():
    parser = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    parser.add_argument("input", help="the PhysLight '.json' to convert")
    parser.add_argument("-o", "--output",
                        help="the '.sensor' to write (default: named from "
                             "the body, beside the input)")
    parser.add_argument("--name", help="the name to write instead of the "
                                       "manufacturer and model")
    parser.add_argument("--pixels", type=int, nargs=2, required=True,
                        metavar=("W", "H"), help="the columns and rows")
    parser.add_argument("--pitch", type=float,
                        help="the pixel pitch in micrometers")
    parser.add_argument("--size", type=float, nargs=2, metavar=("W", "H"),
                        help="the frame in millimeters, instead of the pitch")
    parser.add_argument("--bits", type=int, help="the ADC depth")
    parser.add_argument("--base-iso", type=float, help="the rated base ISO")
    parser.add_argument("--peak-qe", type=float, default=0.5,
                        help="the peak quantum efficiency to scale the "
                             "curves to (default: 0.5, generic)")
    parser.add_argument("--cfa", choices=["rggb", "none"],
                        help="the tile (default: rggb for R, G, and B bands)")
    parser.add_argument("--readout", type=float,
                        help="the rolling readout in seconds")
    parser.add_argument("--comment", help="a comment line to add to the "
                                          "file's header")
    args = parser.parse_args()
    if (args.pitch is None) == (args.size is None):
        print("error: give exactly one of --pitch and --size",
              file=sys.stderr)
        return 1
    if not args.peak_qe > 0 or args.peak_qe > 1:
        print("error: --peak-qe must be in (0, 1]", file=sys.stderr)
        return 1
    try:
        name, channels, rows = read_physlight(args.input)
    except (OSError, ValueError, ConvertError) as error:
        print(f"error: {error}", file=sys.stderr)
        return 1
    if args.name:
        name = args.name
    body = {"pixels": args.pixels, "peak_qe": args.peak_qe}
    if args.pitch is not None:
        body["pitch"] = args.pitch
    else:
        body["size"] = args.size
    for key in ("bits", "base_iso", "cfa", "readout", "comment"):
        value = getattr(args, key)
        if value is not None:
            body[key] = value
    output = args.output
    if not output:
        output = os.path.join(os.path.dirname(args.input),
                              file_stem(name) + ".sensor")
    try:
        with open(output, "w") as out:
            write_sensor(out, name, channels, rows,
                         os.path.basename(args.input), body)
    except ConvertError as error:
        print(f"error: {error}", file=sys.stderr)
        return 1
    print(f"{output}: {name}, {len(channels)} band(s) of {len(rows)} knots, "
          f"{args.pixels[0]}x{args.pixels[1]} pixels")
    return 0


if __name__ == "__main__":
    sys.exit(main())
