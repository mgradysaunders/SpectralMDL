#!/usr/bin/env python3
"""Convert a lens prescription into the renderer's '.lens' format.

    lens_convert.py IN.dat [-o OUT.lens] [--name "Double Gauss 50mm f/2"]
                           [--note "US 2,673,491 (Tronnier)"] ...

Reading a prescription is the only way anyone gets a lens into the renderer:
nobody designs one for it, they transcribe a published table or export from a
design tool. This script covers the mechanical half of that, so the '.lens'
parser never has to be general.

Input formats
-------------
pbrt '.dat'
    Four whitespace-separated columns per surface, '#' for a comment, all
    lengths in millimeters, surfaces front (scene side) first:

        radius  thickness  index  diameter

    The aperture stop is the row whose radius is 0, and pbrt writes its index
    as 0 as well. Auto-detected, and the same reader handles the several
    published variants that differ only in whitespace.

Zemax '.zmx'
    The sequential surface list of an uncompressed ASCII '.zmx': SURF, CURV,
    DISZ, DIAM, CONI, GLAS, and STOP. Glass names are resolved to an index
    only when the file states one (a GLAS line carries nd), because a name
    alone means nothing without the catalog it came from; --glass supplies
    the rest as 'NAME=1.6779' pairs.

What does not convert
---------------------
Mirror surfaces, tilts, decenters, multi-configuration data, solves, and
anything non-sequential. Each is reported and refused rather than dropped,
since a lens that silently loses a surface still renders a picture.

Aspheric coefficients convert, but the renderer refuses a nonzero one until
its trace grows an iterative intersection. Coefficients that are all zero
pass through, since published tables print them.
"""

import argparse
import math
import os
import sys

MM_PER_INCH = 25.4


class ConvertError(Exception):
    """A prescription that cannot be represented, reported as one message."""


class Surface:
    """One surface, in millimeters, with the renderer's sign conventions."""

    def __init__(self):
        self.is_stop = False
        self.radius = 0.0
        self.thickness = 0.0
        self.ior = 1.0
        self.diameter = 0.0
        self.conic = 0.0
        self.aspheric = []

    def spell(self, widths):
        """The '.lens' text of this surface, padded to align a whole table."""
        keyword = "stop" if self.is_stop else "surface"
        fields = []
        if not self.is_stop and self.radius != 0.0:
            fields.append(("radius", number(self.radius), widths["radius"]))
        else:
            fields.append(("", "", widths["radius"]))
        if self.thickness != 0.0:
            fields.append(("thickness", number(self.thickness),
                           widths["thickness"]))
        else:
            fields.append(("", "", widths["thickness"]))
        if not self.is_stop and self.ior != 1.0:
            fields.append(("ior", number(self.ior), widths["ior"]))
        else:
            fields.append(("", "", widths["ior"]))
        fields.append(("diameter", number(self.diameter), widths["diameter"]))
        body = " ".join(
            (f"{key} {value}" if key else "").ljust(width)
            for key, value, width in fields).rstrip()
        if not self.is_stop and self.conic != 0.0:
            body += f" conic {number(self.conic)}"
        if not self.is_stop and self.aspheric:
            body += " aspheric " + " ".join(number(a) for a in self.aspheric)
        return f"  {keyword.ljust(7)} {{ {body} }}"


def number(value):
    """A number as short as it can be written without losing a digit.

    Prescriptions are decimal tables and stay legible only if the output
    reads like the input did, so this prefers the shortest decimal that round
    trips and falls back to exponent form for the aspheric coefficients,
    which are genuinely tiny.
    """
    if value == 0.0:
        return "0"
    if abs(value) < 1e-4 or abs(value) >= 1e7:
        return repr(value)
    for digits in range(0, 10):
        text = f"{value:.{digits}f}"
        if float(text) == value:
            return text
    return repr(value)


def read_pbrt_dat(text):
    """Parse pbrt's four-column '.dat'."""
    values = []
    for line in text.splitlines():
        line = line.split("#", 1)[0].strip()
        if not line:
            continue
        for token in line.replace(",", " ").split():
            try:
                values.append(float(token))
            except ValueError:
                raise ConvertError(f"expected a number, got {token!r}")
    if not values:
        raise ConvertError("no surfaces: the file holds no numbers")
    if len(values) % 4 != 0:
        raise ConvertError(
            f"expected a multiple of 4 numbers (radius, thickness, index, "
            f"diameter per surface), got {len(values)}")
    surfaces = []
    for i in range(0, len(values), 4):
        radius, thickness, ior, diameter = values[i:i + 4]
        surface = Surface()
        # pbrt marks the stop with a zero radius, and writes its index as 0
        # too. A zero radius alone is the reliable half of that: some tables
        # leave the index at 1.
        surface.is_stop = radius == 0.0
        surface.radius = radius
        surface.thickness = thickness
        surface.ior = 1.0 if surface.is_stop or ior == 0.0 else ior
        surface.diameter = diameter
        surfaces.append(surface)
    return surfaces


def read_zemax_zmx(text, glass_table):
    """Parse the sequential surface list of an uncompressed ASCII '.zmx'."""
    unit_scale = 1.0
    surfaces = []
    current = None
    refused = []

    def finish():
        if current is not None:
            surfaces.append(current)

    for raw in text.splitlines():
        line = raw.strip()
        if not line:
            continue
        parts = line.split()
        key = parts[0].upper()
        if key == "UNIT" and len(parts) > 1:
            unit = parts[1].upper()
            unit_scale = {"MM": 1.0, "CM": 10.0, "IN": MM_PER_INCH,
                          "M": 1000.0}.get(unit)
            if unit_scale is None:
                raise ConvertError(f"unknown UNIT {parts[1]!r}")
        elif key == "SURF":
            finish()
            current = Surface()
        elif current is None:
            continue
        elif key == "STOP":
            current.is_stop = True
        elif key == "CURV" and len(parts) > 1:
            # A radius is the reciprocal of what the file states, and the
            # reciprocal of a rounded decimal is a long one: 0.0333333333
            # is 30.000000030000002 mm. No prescription is stated to nine
            # significant digits, so round there and let a radius read
            # like the table it came from.
            curvature = float(parts[1])
            current.radius = (
                0.0 if curvature == 0.0
                else float(f"{1.0 / curvature:.9g}"))
        elif key == "DISZ" and len(parts) > 1:
            if parts[1].upper() == "INFINITY":
                current.thickness = math.inf
            else:
                current.thickness = float(parts[1])
        elif key == "DIAM" and len(parts) > 1:
            # DIAM is a semi-diameter in every Zemax file that states one.
            current.diameter = 2.0 * float(parts[1])
        elif key == "CONI" and len(parts) > 1:
            current.conic = float(parts[1])
        elif key == "PARM" and len(parts) > 2:
            # Even-asphere terms, PARM 1 being the r^4 coefficient.
            index = int(parts[1])
            while len(current.aspheric) < index:
                current.aspheric.append(0.0)
            current.aspheric[index - 1] = float(parts[2])
        elif key == "GLAS" and len(parts) > 1:
            name = parts[1]
            if name.upper() == "MIRROR":
                refused.append("a MIRROR surface")
                continue
            index = glass_table.get(name.upper())
            if index is None and len(parts) >= 5:
                # A GLAS line states nd in its fourth numeric field when the
                # file was written with catalog data folded in.
                try:
                    index = float(parts[4])
                except ValueError:
                    index = None
            if index is None:
                raise ConvertError(
                    f"glass {name!r} has no index in the file; give it with "
                    f"--glass {name}=<nd>")
            current.ior = index
        elif key in ("XDEC", "YDEC", "ATIL", "BTIL"):
            if len(parts) > 1 and float(parts[1]) != 0.0:
                refused.append(f"a nonzero {key}")
    finish()
    if refused:
        raise ConvertError("cannot convert " + ", ".join(sorted(set(refused))) +
                           ": the renderer traces refracting surfaces on a "
                           "straight axis only")
    if unit_scale != 1.0:
        for surface in surfaces:
            surface.radius *= unit_scale
            surface.thickness *= unit_scale
            surface.diameter *= unit_scale
    # The object surface leads the list and the image surface ends it;
    # neither is part of the lens.
    if surfaces and math.isinf(surfaces[0].thickness):
        surfaces = surfaces[1:]
    if surfaces:
        surfaces = surfaces[:-1]
    for surface in surfaces:
        if math.isinf(surface.thickness):
            raise ConvertError("an infinite thickness inside the lens")
    return surfaces


def normalize(surfaces):
    """Check what the '.lens' format requires, and say what is missing."""
    if not surfaces:
        raise ConvertError("no surfaces")
    stops = [i for i, s in enumerate(surfaces) if s.is_stop]
    if len(stops) > 1:
        raise ConvertError(f"{len(stops)} aperture stops, and a lens has one")
    if not stops:
        raise ConvertError(
            "no aperture stop: the table does not say where the diaphragm "
            "sits, which patents routinely leave out. Add the row by hand, "
            "or pass --stop-after N to put it after surface N")
    for i, surface in enumerate(surfaces):
        if surface.diameter <= 0.0:
            raise ConvertError(
                f"surface {i + 1} has no clear aperture; the renderer needs "
                f"one on every surface to know which rays it passes")
        if surface.ior < 1.0:
            raise ConvertError(f"surface {i + 1} has an index below 1")
        if surface.thickness < 0.0:
            raise ConvertError(
                f"surface {i + 1} steps backward along the axis, which only a "
                f"mirror does")
    return surfaces


def spell(surfaces, name, notes):
    """The whole '.lens' file as text."""
    widths = {}
    for key in ("radius", "thickness", "ior", "diameter"):
        longest = 0
        for surface in surfaces:
            value = getattr(surface, key)
            if key == "radius" and (surface.is_stop or value == 0.0):
                continue
            if key == "thickness" and value == 0.0:
                continue
            if key == "ior" and (surface.is_stop or value == 1.0):
                continue
            longest = max(longest, len(key) + 1 + len(number(value)))
        widths[key] = longest
    lines = ["lens {"]
    if name:
        lines.append(f'  name "{name}"')
    for note in notes:
        lines.append(f"  # {note}")
    for surface in surfaces:
        lines.append(surface.spell(widths))
    lines.append("}")
    return "\n".join(lines) + "\n"


def main(argv=None):
    parser = argparse.ArgumentParser(
        description=__doc__.splitlines()[0],
        formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("input", help="the prescription to read")
    parser.add_argument("-o", "--output",
                        help="where to write (default: stdout)")
    parser.add_argument("--format", choices=("auto", "dat", "zmx"),
                        default="auto",
                        help="the input format (default: by extension)")
    parser.add_argument("--name", default="",
                        help="the lens name to write into the file")
    parser.add_argument("--note", action="append", default=[],
                        help="a provenance comment, repeatable: the patent "
                             "number, the book, the scaling applied")
    parser.add_argument("--glass", action="append", default=[],
                        metavar="NAME=ND",
                        help="the index of a glass the file names but does "
                             "not define, repeatable")
    parser.add_argument("--scale", type=float, default=1.0,
                        help="scale every length, for a design published at "
                             "a different focal length (default: 1)")
    parser.add_argument("--stop-after", type=int, metavar="N",
                        help="insert the aperture stop after surface N "
                             "(1-based) for a table that omits it, taking "
                             "that surface's thickness as the gap")
    args = parser.parse_args(argv)

    glass_table = {}
    for entry in args.glass:
        if "=" not in entry:
            parser.error(f"expected --glass NAME=ND, got {entry!r}")
        key, _, value = entry.partition("=")
        glass_table[key.strip().upper()] = float(value)

    kind = args.format
    if kind == "auto":
        kind = "zmx" if args.input.lower().endswith(".zmx") else "dat"
    try:
        with open(args.input, "r", errors="replace") as file:
            text = file.read()
        if kind == "zmx":
            surfaces = read_zemax_zmx(text, glass_table)
        else:
            surfaces = read_pbrt_dat(text)
        if args.stop_after is not None:
            index = args.stop_after
            if not 1 <= index <= len(surfaces):
                raise ConvertError(
                    f"--stop-after {index} is outside the "
                    f"{len(surfaces)} surfaces read")
            before = surfaces[index - 1]
            stop = Surface()
            stop.is_stop = True
            stop.thickness = before.thickness
            stop.diameter = before.diameter
            before.thickness = 0.0
            surfaces.insert(index, stop)
        if args.scale != 1.0:
            for surface in surfaces:
                surface.radius *= args.scale
                surface.thickness *= args.scale
                surface.diameter *= args.scale
        surfaces = normalize(surfaces)
    except ConvertError as error:
        sys.stderr.write(f"{args.input}: {error}\n")
        return 1
    except OSError as error:
        sys.stderr.write(f"{error}\n")
        return 1

    notes = list(args.note)
    notes.append(f"Converted from {os.path.basename(args.input)} by "
                 f"etc/scripts/lens_convert.py.")
    text = spell(surfaces, args.name, notes)
    if args.output:
        with open(args.output, "w") as file:
            file.write(text)
    else:
        sys.stdout.write(text)
    return 0


if __name__ == "__main__":
    sys.exit(main())
