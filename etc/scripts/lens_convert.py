#!/usr/bin/env python3
"""Convert a lens prescription into the renderer's '.lens' format.

    lens_convert.py IN.dat [-o OUT.lens] [--name "Double Gauss 50mm f/2"]
                           [--note "US 2,673,491 (Tronnier)"] ...
    lens_convert.py IN.zmx [--agf CATALOG.agf ...] [--glass NAME=ND,VD ...]

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
    published variants that differ only in whitespace. An index is all the
    format states, so every glass converts to an 'ior', which disperses
    nothing.

Zemax '.zmx'
    The sequential surface list of an uncompressed '.zmx', in ASCII or in
    UTF-16, both of which Zemax writes: SURF, TYPE, CURV, DISZ, DIAM, CONI,
    PARM, GLAS, and STOP. A surface is a STANDARD conic or an EVENASPH one,
    whose PARM n is the coefficient of r^(2n).

Glasses
-------
A GLAS line names the glass after its surface. The name resolves through,
in order:

1. The renderer's built-in catalog, whose names convert as 'glass NAME'.
2. --glass NAME=ND, which converts as one index at every wavelength, or
   --glass NAME=ND,VD, which converts as a definition by nd and Vd.
3. Each --agf catalog, in the order given. A glass its maker fits with the
   three-term Sellmeier (AGF formula 2) converts as a copy of it. One fitted
   with the Schott power series (formula 1), as most makers besides SCHOTT
   publish, converts as its printed nd and Vd with the partial dispersion
   its formula gives, since refitting one formula to the other would move
   the index between the lines its maker measured. Other formulas are
   refused.

A model glass (Zemax's ___BLANK) states its nd, Vd, and departure from the
normal line on its GLAS line, and converts as a definition named by its
glass code. A catalog glass's GLAS line carries the same fields, but they
are never read: Zemax fills them with a placeholder 1.5 and 40 as often as
with the catalog's values.

What does not convert
---------------------
Mirror surfaces, tilts, decenters, multi-configuration data, solves, and
anything non-sequential. Each is reported and refused rather than dropped,
since a lens that silently loses a surface still renders a picture. So is an
aperture stop on a surface that refracts, as Zemax files often put it on a
lens's front surface, and glass behind the last surface: the renderer's stop
is an opening that bends nothing, and its film is in air.

So is a surface stating an r^2 aspheric term, since the renderer's
polynomial starts at r^4: that term only restates the curvature near the
axis, and designs leave it at zero.
"""

import argparse
import decimal
import math
import os
import re
import sys

MM_PER_INCH = 25.4

# The renderer's built-in glasses and their aliases, as the table in
# lib/RenderUtil/OpticalGlass.cc spells them. A name added there belongs here
# too: one missing here converts to a definition under the built-in name,
# which the renderer then refuses loudly rather than reading silently.
BUILTIN_GLASSES = frozenset((
    "CAF2", "N-FK51A", "N-PK52A", "FUSED-SILICA", "N-BK7", "N-SK16", "N-K5",
    "N-BAK1", "N-BAK4", "N-LAK9", "N-BAF10", "N-KZFS4", "LF5", "N-F2", "F2",
    "SF2", "N-SF2", "N-SF5", "N-LASF9", "N-SF10", "N-SF11", "N-SF6", "N-SF57",
    "BK7", "SILICA", "FLUORITE"))

# The Fraunhofer lines a partial dispersion is stated between, in
# micrometers.
LINE_G = 0.4358343
LINE_F = 0.4861327
LINE_C = 0.6562725

# A '.lens' glass name: a letter, then letters, digits, '-', and '_'.
GLASS_NAME = re.compile(r"[A-Za-z][A-Za-z0-9_-]*")


class ConvertError(Exception):
    """A prescription that cannot be represented, reported as one message."""


class Surface:
    """One surface, in millimeters, with the renderer's sign conventions."""

    def __init__(self):
        self.is_stop = False
        self.radius = 0.0
        self.thickness = 0.0
        self.ior = 1.0
        self.glass = ""
        self.glass_line = None
        self.diameter = 0.0
        self.conic = 0.0
        self.aspheric = []
        self.kind = "STANDARD"
        self.parms = {}

    def scale(self, factor):
        """Scale every length by `factor`, aspheric coefficients included:
        the term `a r^p` is a length, so `a` is one to the power `1 - p`."""
        self.radius *= factor
        self.thickness *= factor
        self.diameter *= factor
        self.aspheric = [a * factor ** (-3 - 2 * i)
                         for i, a in enumerate(self.aspheric)]

    def medium(self):
        """The '.lens' key stating the space after this surface, or '' for
        air."""
        if self.glass:
            return f"glass {self.glass}"
        if self.ior != 1.0:
            return f"ior {number(self.ior)}"
        return ""

    def columns(self):
        """The columns of this surface's '.lens' text that align in a table."""
        radius = ("" if self.is_stop or self.radius == 0.0
                  else f"radius {number(self.radius)}")
        thickness = ("" if self.thickness == 0.0
                     else f"thickness {number(self.thickness)}")
        # The stop sits in the space before it, which normalize() has
        # checked is the space after it too.
        medium = "" if self.is_stop else self.medium()
        return [radius, thickness, medium, f"diameter {number(self.diameter)}"]

    def spell(self, widths):
        """The '.lens' text of this surface, padded to align a whole table."""
        keyword = "stop" if self.is_stop else "surface"
        body = " ".join(column.ljust(width) for column, width in
                        zip(self.columns(), widths)).rstrip()
        if not self.is_stop and self.conic != 0.0:
            body += f" conic {number(self.conic)}"
        if not self.is_stop and self.aspheric:
            body += " aspheric " + " ".join(number(a) for a in self.aspheric)
        return f"  {keyword.ljust(7)} {{ {body} }}"


class Definition:
    """A glass the '.lens' file defines, since the renderer does not know
    it."""

    def __init__(self, name, source, nd, abbe, partial=None, sellmeier=None):
        self.name = name
        self.source = source
        self.nd = nd
        self.abbe = abbe
        self.partial = partial
        self.sellmeier = sellmeier

    def spell(self):
        """The '.lens' text of this definition, under a comment saying where
        its numbers came from."""
        lines = [f"  # {self.source}"]
        printed = f"ior {number(self.nd)} abbe {number(self.abbe)}"
        if self.sellmeier:
            b, c = self.sellmeier
            lines += [f"  glass {self.name} {{",
                      "    sellmeier { b " + " ".join(number(x) for x in b),
                      "                c " + " ".join(number(x) for x in c) +
                      " }",
                      f"    {printed}",
                      "  }"]
        else:
            if self.partial is not None:
                printed += f" partial_dispersion {number(self.partial)}"
            lines.append(f"  glass {self.name} {{ {printed} }}")
        return lines


class Glasses:
    """What each glass a prescription names becomes in the '.lens' file, and
    the definitions the file needs for those the renderer does not know."""

    def __init__(self, given, catalogs):
        self.given = given
        self.catalogs = catalogs
        self.definitions = []
        self.by_key = {}
        self.taken = set(BUILTIN_GLASSES)

    def resolve(self, surface):
        """Give `surface` the medium its GLAS line names."""
        name, fields = surface.glass_line
        upper = name.upper()
        if upper == "___BLANK":
            self.resolve_model(surface, fields)
        elif upper in BUILTIN_GLASSES:
            surface.glass = upper
        elif upper in self.given:
            nd, abbe = self.given[upper]
            if abbe is None:
                surface.ior = nd
            else:
                surface.glass = self.define(
                    ("name", upper), name,
                    "By the nd and Vd given to the converter.", nd, abbe)
        else:
            for path, records in self.catalogs:
                if upper in records:
                    surface.glass = self.define_from_agf(
                        name, path, records[upper])
                    return
            raise ConvertError(
                f"glass {name!r} is neither built in nor in an --agf "
                f"catalog; give its maker's catalog with --agf, or its nd "
                f"and Vd with --glass {name}=<nd>,<Vd>")

    def resolve_model(self, surface, fields):
        """Give `surface` the model glass its GLAS line states."""
        # After the name: the solve type, the surface a pickup copies, nd,
        # Vd, and the departure from the normal line.
        try:
            nd, abbe = float(fields[2]), float(fields[3])
            departure = float(fields[4]) if len(fields) > 4 else 0.0
        except (IndexError, ValueError):
            raise ConvertError(
                "a model glass (___BLANK) without its nd and Vd")
        if abbe == 0.0:
            # Zemax reads a model glass with no Abbe number as one index at
            # every wavelength.
            surface.ior = nd
            return
        # The departure is from Schott's normal line, which a definition by
        # nd and Vd alone takes as its partial dispersion.
        partial = None
        source = "A Zemax model glass, by its nd and Vd."
        if departure != 0.0:
            partial = round(0.6438 - 0.001682 * abbe + departure, 4)
            source = ("A Zemax model glass, by its nd, Vd, and partial "
                      "dispersion.")
        surface.glass = self.define(("model", nd, abbe, partial),
                                    f"MODEL-{glass_code(nd, abbe)}", source,
                                    nd, abbe, partial)

    def define_from_agf(self, name, path, record):
        """The definition of the glass `record` of the catalog at `path`."""
        catalog = os.path.basename(path)
        formula, cd = record["formula"], record["cd"]
        if formula not in (1, 2):
            raise ConvertError(
                f"glass {name!r} in {catalog} is fitted by AGF formula "
                f"{formula}, and the converter reads only formula 2 (the "
                f"three-term Sellmeier) and formula 1 (the Schott power "
                f"series); give it with --glass {name}=<nd>,<Vd> from its "
                f"datasheet")
        if len(cd) < 6:
            raise ConvertError(
                f"glass {name!r} in {catalog} has no dispersion coefficients")
        key = ("name", name.upper())
        if formula == 2:
            # The CD line interleaves the terms: B1 C1 B2 C2 B3 C3.
            return self.define(
                key, name,
                f"The three-term Sellmeier of {catalog}, with its printed nd "
                f"and Vd.", record["nd"], record["abbe"],
                sellmeier=((cd[0], cd[2], cd[4]), (cd[1], cd[3], cd[5])))

        def index(wavelength):
            w2 = wavelength * wavelength
            return math.sqrt(cd[0] + cd[1] * w2 + cd[2] / w2 +
                             cd[3] / w2 ** 2 + cd[4] / w2 ** 3 +
                             cd[5] / w2 ** 4)

        partial = ((index(LINE_G) - index(LINE_F)) /
                   (index(LINE_F) - index(LINE_C)))
        return self.define(
            key, name,
            f"The printed nd and Vd of {catalog}, and the partial dispersion "
            f"of its Schott formula.", record["nd"], record["abbe"],
            round(partial, 4))

    def define(self, key, name, source, nd, abbe, partial=None,
               sellmeier=None):
        """The name of the definition `key` stands for, made on first use."""
        if key in self.by_key:
            return self.by_key[key].name
        spelled = name
        if not GLASS_NAME.fullmatch(spelled):
            spelled = re.sub(r"[^A-Za-z0-9_-]", "_", spelled)
            if not GLASS_NAME.fullmatch(spelled):
                spelled = "G" + spelled
            source = f"Named {name} in the prescription. {source}"
        unique, count = spelled, 1
        while unique.upper() in self.taken:
            count += 1
            unique = f"{spelled}-{count}"
        self.taken.add(unique.upper())
        definition = Definition(unique, source, nd, abbe, partial, sellmeier)
        self.by_key[key] = definition
        self.definitions.append(definition)
        return unique


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


def glass_code(nd, abbe):
    """The six digits catalogs code a glass by: nd's first three decimals,
    then Vd to one decimal, each rounded half up as the decimal reads, so
    that a Vd of 33.65 codes as 337."""
    def rounded(value, exponent):
        return decimal.Decimal(repr(value)).scaleb(exponent).quantize(
            decimal.Decimal(1), rounding=decimal.ROUND_HALF_UP)
    return f"{rounded(nd, 3) - 1000:03}{rounded(abbe, 1):03}"


def read_text(path):
    """A file's text, whether it is UTF-16 or ASCII, as Zemax writes both."""
    with open(path, "rb") as file:
        data = file.read()
    for bom, encoding in ((b"\xff\xfe", "utf-16-le"),
                          (b"\xfe\xff", "utf-16-be"),
                          (b"\xef\xbb\xbf", "utf-8")):
        if data.startswith(bom):
            return data[len(bom):].decode(encoding, errors="replace")
    if b"\x00" in data[:4096]:
        encoding = "utf-16-le" if data[1:2] == b"\x00" else "utf-16-be"
        return data.decode(encoding, errors="replace")
    return data.decode("utf-8", errors="replace")


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


def read_zemax_shape(number, surface):
    """Read surface `number`'s parameters as its type gives them meaning.

    A STANDARD conic and an EVENASPH polynomial are the shapes that trace on
    a straight axis, and a STANDARD surface's parameters mean nothing, here
    or in Zemax.
    """
    if surface.kind == "EVENASPH":
        # PARM n is the coefficient of r^(2n), so the '.lens' polynomial,
        # which starts at r^4, begins at PARM 2.
        if surface.parms.get(1, 0.0) != 0.0:
            raise ConvertError(
                f"surface {number} states an r^2 aspheric term (PARM 1), and "
                f"the renderer's polynomial starts at r^4")
        terms = [surface.parms.get(n, 0.0)
                 for n in range(2, max(surface.parms, default=1) + 1)]
        while terms and terms[-1] == 0.0:
            terms.pop()
        surface.aspheric = terms
    elif surface.kind != "STANDARD":
        raise ConvertError(
            f"surface {number} is a Zemax {surface.kind} surface, and the "
            f"renderer traces STANDARD and EVENASPH ones: conics on a "
            f"straight axis, with an even polynomial")


def read_zemax_zmx(text):
    """Parse the sequential surface list of an uncompressed '.zmx'.

    A surface's GLAS line is kept as it reads, and resolved by `Glasses` once
    the object and image surfaces, whose glasses mean nothing here, are gone.
    """
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
        elif key == "TYPE" and len(parts) > 1:
            current.kind = parts[1].upper()
        elif key == "PARM" and len(parts) > 2:
            # What a parameter means is the surface type's to say, and the
            # type may be stated after it.
            current.parms[int(parts[1])] = float(parts[2])
        elif key == "GLAS" and len(parts) > 1:
            if parts[1].upper() == "MIRROR":
                refused.append("a MIRROR surface")
                continue
            current.glass_line = (parts[1], parts[2:])
        elif key in ("XDEC", "YDEC", "ATIL", "BTIL"):
            if len(parts) > 1 and float(parts[1]) != 0.0:
                refused.append(f"a nonzero {key}")
    finish()
    if refused:
        raise ConvertError("cannot convert " + ", ".join(sorted(set(refused))) +
                           ": the renderer traces refracting surfaces on a "
                           "straight axis only")
    # The object surface leads the list and the image surface ends it;
    # neither is part of the lens.
    if surfaces and math.isinf(surfaces[0].thickness):
        surfaces = surfaces[1:]
    if surfaces:
        surfaces = surfaces[:-1]
    for i, surface in enumerate(surfaces):
        if math.isinf(surface.thickness):
            raise ConvertError("an infinite thickness inside the lens")
        read_zemax_shape(i + 1, surface)
        if unit_scale != 1.0:
            surface.scale(unit_scale)
    return surfaces


def read_agf(path):
    """The glasses of a Zemax '.agf' catalog, by uppercase name: each its
    formula, its printed nd and Vd, and its coefficients."""
    records = {}
    record = None
    for line in read_text(path).splitlines():
        parts = line.split()
        if not parts:
            continue
        try:
            if parts[0] == "NM":
                # NM name formula code nd Vd ...
                record = {"formula": int(float(parts[2])),
                          "nd": float(parts[4]), "abbe": float(parts[5]),
                          "cd": []}
                records.setdefault(parts[1].upper(), record)
            elif parts[0] == "CD" and record is not None:
                record["cd"] = [float(value) for value in parts[1:]]
        except (IndexError, ValueError):
            raise ConvertError(f"{path}: cannot read {line.strip()!r}")
    if not records:
        raise ConvertError(f"{path}: no glasses, so not an '.agf' catalog")
    return records


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
    stop = stops[0]
    before = surfaces[stop - 1].medium() if stop > 0 else ""
    if surfaces[stop].medium() != before:
        raise ConvertError(
            f"surface {stop + 1}, the aperture stop, refracts, and the "
            f"renderer's stop is an opening that bends nothing; move STOP to "
            f"a flat surface of its own, in the air in front of the lens or "
            f"behind it")
    if surfaces[-1].medium():
        raise ConvertError(
            "the last surface has glass behind it, and the renderer puts the "
            "film in air")
    return surfaces


def spell(surfaces, name, notes, definitions):
    """The whole '.lens' file as text."""
    columns = [surface.columns() for surface in surfaces]
    widths = [max(len(text) for text in column) for column in zip(*columns)]
    lines = ["lens {"]
    if name:
        lines.append(f'  name "{name}"')
    for note in notes:
        lines.append(f"  # {note}")
    for definition in definitions:
        lines.extend(definition.spell())
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
                        metavar="NAME=ND[,VD]",
                        help="a glass the file names that is neither built "
                             "in nor in an --agf catalog: one index at every "
                             "wavelength, or an index and an Abbe number for "
                             "a glass that disperses; repeatable")
    parser.add_argument("--agf", action="append", default=[],
                        metavar="CATALOG.agf",
                        help="a Zemax glass catalog to find the file's "
                             "glasses in, searched in the order given; "
                             "repeatable")
    parser.add_argument("--scale", type=float, default=1.0,
                        help="scale every length, for a design published at "
                             "a different focal length (default: 1)")
    parser.add_argument("--stop-after", type=int, metavar="N",
                        help="insert the aperture stop after surface N "
                             "(1-based) for a table that omits it, taking "
                             "that surface's thickness as the gap")
    args = parser.parse_args(argv)

    kind = args.format
    if kind == "auto":
        kind = "zmx" if args.input.lower().endswith(".zmx") else "dat"
    if kind == "dat" and (args.glass or args.agf):
        parser.error("--glass and --agf resolve the glasses a '.zmx' names, "
                     "and a pbrt '.dat' names none")

    given = {}
    for entry in args.glass:
        key, sep, value = entry.partition("=")
        key = key.strip()
        fields = value.split(",")
        try:
            if not sep or not key or len(fields) > 2:
                raise ValueError(entry)
            nd = float(fields[0])
            abbe = float(fields[1]) if len(fields) == 2 else None
        except ValueError:
            parser.error(f"expected --glass NAME=ND or NAME=ND,VD, got "
                         f"{entry!r}")
        if key.upper() in BUILTIN_GLASSES:
            parser.error(f"{key} is built in, so the renderer knows it "
                         f"already; drop --glass {key}")
        given[key.upper()] = (nd, abbe)

    definitions = []
    try:
        text = read_text(args.input)
        if kind == "zmx":
            glasses = Glasses(given,
                              [(path, read_agf(path)) for path in args.agf])
            surfaces = read_zemax_zmx(text)
            for surface in surfaces:
                if surface.glass_line:
                    glasses.resolve(surface)
            definitions = glasses.definitions
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
            stop.ior = before.ior
            stop.glass = before.glass
            before.thickness = 0.0
            surfaces.insert(index, stop)
        if args.scale != 1.0:
            for surface in surfaces:
                surface.scale(args.scale)
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
    text = spell(surfaces, args.name, notes, definitions)
    if args.output:
        with open(args.output, "w") as file:
            file.write(text)
    else:
        sys.stdout.write(text)
    return 0


if __name__ == "__main__":
    sys.exit(main())
