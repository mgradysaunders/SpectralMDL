#!/usr/bin/env python3
"""Precompute a horizon map from a height map.

Produces the four directional horizon slopes (+U, -U, +V, -V) that
``::extras::parallax::horizon_slopes_with()`` computes at runtime with
24-36+ height taps. The output is consumed by the ``horizon`` role of
the ``.pbr`` manifest schema (see ``smdl::PBRMaps``), through which
``::extras::pbr`` — ``horizon_slopes()`` and the ``prepared()``
pipeline — replaces the runtime search with a single mip-filtered
lookup. The manifest must declare the same relief scale given here,
either directly as ``relief_scale`` or as the ``physical_extent`` and
``physical_relief`` pair whose ratio it is.

Semantics match the runtime: for each texel, each channel is the
steepest blocker slope ``rise / run`` toward that axis direction,
clamped below at zero. The slopes are baked at the maps' own relief
scale, and — unlike the runtime, whose single ``scale`` cannot honor
anisotropic physical extents — per axis:

    slope(+U) = max over t > 0 of
        scale_u * (h(u + t, v) - h(u, v)) / t

with heights ``h`` in [0, 1] (1 = un-displaced surface, matching the
SMDL convention; only differences matter here), ``t`` in UV units, and
``scale_u`` the relief as a fraction of one UV unit along U: either
``--relief-scale`` (isotropic, for bespoke maps with no meaningful
physical extent) or ``physical_relief / extent_u``.

Encoding: ``stored = min(slope, max_slope) / max_slope`` in a 16-bit
RGBA PNG, R = +U, G = -U, B = +V, A = -V. The default ``max_slope`` of 8
mirrors ``PARALLAX_MAX_SLOPE``. The map is linear data: a manifest hook
must declare (or default) ``color_space: linear``. A consumer decodes
``slope = stored * max_slope`` and, when shading at a relief scale
other than the maps' own (e.g. under a detail fade), multiplies by
``scale / relief_scale``.

Orientation: MDL texture space is v-up, so v = 0 is the *bottom* row of
the displayed image and +V points up the image. (SMDL stores images in
file order, top row first, and mirrors the row at lookup; see the
storage note in ``tex.smdl``.) The directional scans run in that v-up
frame, and the output file is written in ordinary image orientation, so
the height map and the horizon map stay in register row for row.

Search strategy: exact single-texel steps out to ``--near-exact``
texels, then geometrically growing windows (``--ratio``) out to
``--max-distance`` (in UV units, like the runtime's default 0.25). Each
window contributes its windowed height maximum divided by the window's
far edge, which can never overestimate a horizon and underestimates by
at most the ratio — unlike sparse taps, a thin distant spike is never
missed entirely.

Example:

    python3 etc/horizon_map.py textures/ganges_river_pebbles_disp_4k.png \\
        --extent 2.1558 --relief 0.16 -o textures/ganges_river_pebbles_hrz_4k.png

    python3 etc/horizon_map.py bespoke_rock_disp.png \\
        --relief-scale 0.031 -o bespoke_rock_hrz.png
"""
import argparse
import math
import os
import struct
import sys
import time
import zlib

import numpy as np
from PIL import Image

Image.MAX_IMAGE_PIXELS = None  # trust local asset downloads


def load_height(path, channel):
    """Load a height map as float32 in [0, 1], in file orientation."""
    img = Image.open(path)
    arr = np.asarray(img)
    if arr.ndim == 3:
        arr = arr[:, :, "rgba".index(channel)]
    arr = arr.astype(np.float32)
    if np.issubdtype(np.asarray(img).dtype, np.integer):
        arr /= np.float32(np.iinfo(np.asarray(img).dtype).max)
    return np.clip(arr, 0.0, 1.0)


def resize_box(arr, size):
    """Downsample so the longer dimension is `size`, keeping aspect."""
    height, width = arr.shape
    if max(height, width) <= size:
        return arr
    scale = size / max(height, width)
    new_size = (max(1, round(width * scale)), max(1, round(height * scale)))
    return np.asarray(Image.fromarray(arr, mode="F").resize(new_size, Image.BOX))


def write_png16_rgba(path, arr):
    """Write float [0, 1] (H, W, 4) as a 16-bit RGBA PNG."""
    quantized = np.round(np.clip(arr, 0.0, 1.0) * 65535.0).astype(">u2")
    height, width = quantized.shape[:2]
    rows = quantized.reshape(height, -1).view(np.uint8)
    raw = np.zeros((height, rows.shape[1] + 1), np.uint8)  # filter 0 per row
    raw[:, 1:] = rows

    def chunk(tag, payload):
        body = tag + payload
        return struct.pack(">I", len(payload)) + body + struct.pack(">I", zlib.crc32(body))

    with open(path, "wb") as fp:
        fp.write(b"\x89PNG\r\n\x1a\n")
        fp.write(chunk(b"IHDR", struct.pack(">IIBBBBB", width, height, 16, 6, 0, 0, 0)))
        fp.write(chunk(b"IDAT", zlib.compress(raw.tobytes(), 6)))
        fp.write(chunk(b"IEND", b""))


def window_schedule(near_exact, max_d, ratio):
    """The pooled windows [d0, d1) covering (near_exact, max_d].

    Widths are non-decreasing — the invariant the doubling table in
    `scan_last_axis` relies on — so a final window clamped shorter by
    `max_d` is merged into its predecessor.
    """
    windows = []
    d0 = near_exact + 1
    while d0 <= max_d:
        d1 = min(max(math.ceil(d0 * ratio), d0 + 1), max_d + 1)
        windows.append((d0, d1))
        d0 = d1
    if len(windows) >= 2 and windows[-1][1] - windows[-1][0] < windows[-2][1] - windows[-2][0]:
        last = windows.pop()
        windows[-1] = (windows[-1][0], last[1])
    return windows


def scan_last_axis(h, step_uv, scale, max_d, near_exact, ratio, wrap):
    """Steepest blocker slope toward increasing last-axis index.

    ``scale`` is the relief as a fraction of one UV unit along this
    axis and ``step_uv`` the UV distance of one texel, so the slopes
    come out in the UV-space units the runtime march works in.

    Exact for distances 1..near_exact; beyond that, each geometric
    window [d0, d1) contributes its windowed height maximum divided by
    the far edge d1 (conservative: never overestimates, underestimates
    by at most d1/d0 <= ~ratio). Windowed maxima come from a doubling
    table, which covers each window with two shifts exactly because the
    schedule's widths never decrease.
    """
    n = h.shape[-1]

    def shifted(a, d):
        if wrap:
            return np.roll(a, -d, axis=-1)
        out = np.full_like(a, -np.inf)
        if d < n:
            out[..., : n - d] = a[..., d:]
        return out

    slopes = np.zeros_like(h)
    for d in range(1, min(near_exact, max_d) + 1):
        np.maximum(slopes, (shifted(h, d) - h) * (scale / (d * step_uv)), out=slopes)
    table, width = h, 1
    for d0, d1 in window_schedule(min(near_exact, max_d), max_d, ratio):
        if not wrap and d0 >= n:
            break
        while width * 2 <= d1 - d0:
            table = np.maximum(table, shifted(table, width))
            width *= 2
        window_max = shifted(table, d0)
        if d1 - width != d0:
            np.maximum(window_max, shifted(table, d1 - width), out=window_max)
        np.maximum(slopes, (window_max - h) * (scale / (d1 * step_uv)), out=slopes)
    return slopes


def main():
    parser = argparse.ArgumentParser(
        description="Precompute a (+U, -U, +V, -V) horizon map from a height map.",
        epilog="See the module docstring for the encoding and orientation contract.",
    )
    parser.add_argument("height", help="height map image (1 = un-displaced surface)")
    parser.add_argument("-o", "--output", help="output PNG (default: <height>_horizon.png)")
    parser.add_argument("--extent", type=float, nargs="+", metavar="METERS",
                        help="physical extent per repeat in U [and V] (like .pbr physical_extent)")
    parser.add_argument("--relief", type=float, metavar="METERS",
                        help="physical relief spanned by heights 0..1 (like .pbr physical_relief)")
    parser.add_argument("--relief-scale", type=float, metavar="UV",
                        help="relief spanned by heights 0..1 as a fraction of one UV unit "
                             "(like .pbr relief_scale); the isotropic alternative to "
                             "--extent/--relief, for maps with no meaningful physical extent")
    parser.add_argument("--max-slope", type=float, default=8.0,
                        help="slope cap and encoding scale (default 8, PARALLAX_MAX_SLOPE)")
    parser.add_argument("--max-distance", type=float, default=0.25,
                        help="search distance in UV units (default 0.25, like the runtime)")
    parser.add_argument("--near-exact", type=int, default=32,
                        help="texel distance searched exactly before pooling (default 32)")
    parser.add_argument("--ratio", type=float, default=1.1,
                        help="geometric growth of pooled search windows (default 1.1)")
    parser.add_argument("--no-wrap", action="store_true",
                        help="clamp instead of repeat (for non-tiling atlas maps)")
    parser.add_argument("--size", type=int,
                        help="downsample the height map so its longer side is SIZE first")
    parser.add_argument("--channel", choices="rgba", default="r",
                        help="channel of a multi-channel height map (default r)")
    args = parser.parse_args()
    # Exactly one spelling of the relief scale, matching the manifest's
    # own rule that 'relief_scale' overrides the physical pair.
    if args.relief_scale is not None:
        if args.extent is not None or args.relief is not None:
            parser.error("--relief-scale is an alternative to --extent and --relief, "
                         "not an addition to them")
        if args.relief_scale <= 0:
            parser.error("--relief-scale must be positive")
        scale_u = scale_v = args.relief_scale
    else:
        if args.extent is None or args.relief is None:
            parser.error("give --extent and --relief, or --relief-scale")
        if len(args.extent) not in (1, 2) or min(args.extent) <= 0:
            parser.error("--extent takes one or two positive reals")
        if args.relief <= 0:
            parser.error("--relief must be positive")
        scale_u = args.relief / args.extent[0]
        scale_v = args.relief / args.extent[-1]
    if args.max_slope <= 0 or args.max_distance <= 0:
        parser.error("--max-slope and --max-distance must be positive")
    if args.near_exact < 1 or args.ratio <= 1.0:
        parser.error("--near-exact must be >= 1 and --ratio must be > 1")

    h_file = load_height(args.height, args.channel)
    if args.size:
        h_file = resize_box(h_file, args.size)
    h = h_file[::-1]  # v-up texel frame: +V is +row (MDL texture space is v-up)
    height, width = h.shape
    wrap = not args.no_wrap

    start = time.time()
    directions = {}
    for key, view, scale, limit in [
        ("+U", h, scale_u, width),
        ("-U", h[:, ::-1], scale_u, width),
        ("+V", h.T, scale_v, height),
        ("-V", h.T[:, ::-1], scale_v, height),
    ]:
        view = np.ascontiguousarray(view)
        max_d = max(1, round(args.max_distance * limit))
        slopes = scan_last_axis(view, 1.0 / limit, scale, max_d,
                                args.near_exact, args.ratio, wrap)
        if key.startswith("-"):
            slopes = slopes[:, ::-1]
        if key.endswith("V"):
            slopes = slopes.T
        directions[key] = np.minimum(slopes, args.max_slope)
        print(f"  {key}: max slope {directions[key].max():.3f}, "
              f"mean {directions[key].mean():.4f} [{time.time() - start:.1f}s]")

    encoded = np.stack([directions[k] for k in ("+U", "-U", "+V", "-V")], axis=-1)
    encoded /= args.max_slope
    output = args.output or (args.height.rsplit(".", 1)[0] + "_horizon.png")
    write_png16_rgba(output, encoded[::-1])  # back to file orientation
    print(f"Wrote {output} ({width}x{height}, max_slope {args.max_slope}) "
          f"[{time.time() - start:.1f}s]")
    print("Manifest entry (adjust the path):")
    if args.relief_scale is not None:
        print(f"  relief_scale: {args.relief_scale:g}")
    print("  horizon:")
    print(f"    file: {os.path.basename(output)}")
    if args.max_slope != 8.0:
        print(f"    max_slope: {args.max_slope:g}")


if __name__ == "__main__":
    main()
