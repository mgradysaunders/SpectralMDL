#!/usr/bin/env python3
"""Convert the rawtoaces training reflectances into the table the
renderer's color fit trains on.

    training_convert.py IN.json [-o OUT.inl]

The develop fits a body's color matrix over reflectances whose license
lets them ship with the renderer: the 190 patches of rawtoaces-data
(https://github.com/AcademySoftwareFoundation/rawtoaces-data, the file
data/training/training_spectral.json, Apache-2.0). This writes them as
a C++ table that keeps the attribution and the license notice, so the
renderer reads no data file at run time. The values are copied as they
are.

Input format
------------
The rawtoaces spectral JSON, schema 1.0.0:

    header.license              "Apache-2.0", checked
    spectral_data.units         "relative": reflectance, 1 for a white
    spectral_data.index.main    the patch names, in order
    spectral_data.data.main     wavelength in nm -> one value per patch

What does not convert
---------------------
A file under another license, one whose wavelengths are not evenly
spaced, and one whose rows disagree on the patch count. Each is refused
rather than guessed at.
"""

import argparse
import json
import sys

HEADER = """\
// The rawtoaces training reflectances, which the color fit of
// `Sensor/Sensor.cc` trains on: {count} patches from {lo:g} to {hi:g} nm at
// {step:g} nm, "{description}" by {creator} ({date}), from
// data/training/training_spectral.json of rawtoaces-data,
// https://github.com/AcademySoftwareFoundation/rawtoaces-data. Generated
// by etc/scripts/training_convert.py, which wrote the values as a C++
// table and changed none of them; regenerate rather than editing.
//
// Licensed under the Apache License, Version 2.0 (the "License"); you
// may not use this file except in compliance with the License. You may
// obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied. See the License for the specific language governing
// permissions and limitations under the License.

constexpr size_t TRAINING_PATCH_COUNT{{{count}}};
constexpr double TRAINING_WAVELENGTH_MIN{{{lo:.1f}}};
constexpr double TRAINING_WAVELENGTH_STEP{{{step:.1f}}};
constexpr size_t TRAINING_WAVELENGTH_COUNT{{{numWavelengths}}};

// Patch by patch, each at the wavelengths above.
constexpr float TRAINING_REFLECTANCES[TRAINING_PATCH_COUNT]
                                     [TRAINING_WAVELENGTH_COUNT]{{
"""

VALUES_PER_LINE = 8


class ConvertError(Exception):
    """A file that cannot be represented, reported as one message."""


def read_training(path):
    """The header, the wavelengths, and the patches as rows of values."""
    with open(path) as f:
        document = json.load(f)
    header = document.get("header", {})
    if header.get("license") != "Apache-2.0":
        raise ConvertError(
            f"expected the Apache-2.0 license, got {header.get('license')!r}"
        )
    spectral = document.get("spectral_data", {})
    if spectral.get("units") != "relative":
        raise ConvertError(
            f"expected relative reflectances, got {spectral.get('units')!r}"
        )
    names = spectral.get("index", {}).get("main", [])
    rows = spectral.get("data", {}).get("main", {})
    wavelengths = sorted(float(key) for key in rows)
    if len(wavelengths) < 2:
        raise ConvertError("expected at least two wavelengths")
    step = wavelengths[1] - wavelengths[0]
    for a, b in zip(wavelengths, wavelengths[1:]):
        if abs(b - a - step) > 1e-6:
            raise ConvertError(f"expected even spacing, got {a:g} then {b:g}")
    table = [rows[key] for key in sorted(rows, key=float)]
    for wavelength, row in zip(wavelengths, table):
        if len(row) != len(names):
            raise ConvertError(
                f"expected {len(names)} values at {wavelength:g} nm, "
                f"got {len(row)}"
            )
    patches = [[row[j] for row in table] for j in range(len(names))]
    return header, wavelengths, step, patches


def write_table(out, header, wavelengths, step, patches):
    out.write(
        HEADER.format(
            count=len(patches),
            lo=wavelengths[0],
            hi=wavelengths[-1],
            step=step,
            numWavelengths=len(wavelengths),
            description=header.get("description", ""),
            creator=header.get("document_creator", ""),
            date=header.get("document_creation_date", "")[:10],
        )
    )
    for patch in patches:
        spelled = [f"{value:.6g}" for value in patch]
        lines = [
            ", ".join(spelled[i : i + VALUES_PER_LINE])
            for i in range(0, len(spelled), VALUES_PER_LINE)
        ]
        out.write("    {" + (",\n     ".join(lines)) + "},\n")
    out.write("};\n")


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("input", help="training_spectral.json")
    parser.add_argument("-o", "--output", help="the .inl to write (stdout)")
    args = parser.parse_args()
    try:
        header, wavelengths, step, patches = read_training(args.input)
    except (ConvertError, OSError, ValueError) as error:
        sys.exit(f"{args.input}: {error}")
    if args.output:
        with open(args.output, "w") as out:
            write_table(out, header, wavelengths, step, patches)
    else:
        write_table(sys.stdout, header, wavelengths, step, patches)


if __name__ == "__main__":
    main()
