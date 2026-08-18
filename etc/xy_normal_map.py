#!/usr/bin/env python3
"""Convert a 3-channel tangent-space normal map to a 2-channel XY map.

Drops the reconstructible Z channel: the output stores X and Y only in
an 8-bit gray+alpha PNG, and the runtime reconstructs
``z = sqrt(1 - x^2 - y^2)``. SMDL loads a gray+alpha PNG as a
2-channel texture whose texels unpack positionally, so ``texel.x`` is
the gray channel (X) and ``texel.y`` is the alpha channel (Y): exactly
what ``::extras::pbr::decode_normal_texel()`` reads for the
``NORMAL_OPENGL_XY`` and ``NORMAL_DIRECTX_XY`` conventions.

The handedness of the input is preserved: an ``opengl`` map becomes an
``opengl_xy`` map and a ``directx`` map becomes a ``directx_xy`` map,
so the ``.pbr`` manifest hook is the input's convention with ``_xy``
appended:

    normal:
      file: ground_nor_xy_4k.png
      convention: opengl_xy

Pass ``--flip-y`` to negate Y, which converts handedness on the way
through (an ``opengl`` input becomes a ``directx_xy`` output and vice
versa).

By default each texel is renormalized in 3D before Z is dropped, which
projects sloppy (non-unit) inputs onto the unit sphere the
reconstruction assumes; on well-formed maps the effect is below
quantization. Pass ``--no-renormalize`` to copy X and Y through
untouched instead, which is byte-exact for 8-bit inputs.

Tangent-space normal maps have non-negative Z by construction; a
back-facing texel (Z < 0) cannot be represented in two channels, so it
is reported and written with its Z sign lost.

Example:

    python3 etc/xy_normal_map.py textures/ground_nor_gl_4k.png \\
        -o textures/ground_nor_xy_4k.png
"""
import argparse
import os
import sys

import numpy as np
from PIL import Image

Image.MAX_IMAGE_PIXELS = None  # trust local asset downloads


def load_normals(path):
    """Load a normal map as float32 vectors in [-1, 1], file orientation."""
    img = Image.open(path)
    if img.mode == "P":
        img = img.convert("RGB")
    arr = np.asarray(img)
    if arr.ndim != 3 or arr.shape[2] < 3:
        channels = 1 if arr.ndim == 2 else arr.shape[2]
        sys.exit(f"error: '{path}' has {channels} channel(s); "
                 "expected a 3- or 4-channel normal map")
    dtype = arr.dtype
    arr = arr[:, :, :3].astype(np.float32)
    if np.issubdtype(dtype, np.integer):
        arr /= np.float32(np.iinfo(dtype).max)
    return 2.0 * np.clip(arr, 0.0, 1.0) - 1.0


def main():
    parser = argparse.ArgumentParser(
        description="Convert a 3-channel normal map to a 2-channel XY map "
                    "(8-bit gray+alpha PNG, gray = X, alpha = Y).")
    parser.add_argument("input", help="the 3-channel normal map")
    parser.add_argument("-o", "--output",
                        help="the output PNG (default: <input>_xy.png)")
    parser.add_argument("--flip-y", action="store_true",
                        help="negate Y, converting between the OpenGL and "
                             "DirectX handedness")
    parser.add_argument("--no-renormalize", dest="renormalize",
                        action="store_false",
                        help="copy X and Y through untouched instead of "
                             "renormalizing in 3D first")
    args = parser.parse_args()

    normals = load_normals(args.input)
    lengths = np.linalg.norm(normals, axis=2)
    backfacing = int(np.count_nonzero(normals[:, :, 2] < 0.0))
    if args.renormalize:
        normals /= np.maximum(lengths, 1e-12)[:, :, None]
    if args.flip_y:
        normals[:, :, 1] = -normals[:, :, 1]

    xy = np.round(np.clip(0.5 * normals[:, :, :2] + 0.5, 0.0, 1.0)
                  * 255.0).astype(np.uint8)
    output = args.output or os.path.splitext(args.input)[0] + "_xy.png"
    Image.fromarray(xy, mode="LA").save(output)

    height, width = xy.shape[:2]
    print(f"{output}: {width}x{height}, 8-bit gray+alpha (gray = X, alpha = Y)")
    print(f"  input vector length: mean {lengths.mean():.4f}, "
          f"max deviation {np.abs(lengths - 1.0).max():.4f}")
    if backfacing:
        print(f"  warning: {backfacing} back-facing texel(s) (Z < 0), "
              "which two channels cannot represent; their Z sign is lost")
    flipped = " (flipped from the input by --flip-y)" if args.flip_y else ""
    print("  declare 'convention: opengl_xy' or 'directx_xy' to match the "
          f"output handedness{flipped}")


if __name__ == "__main__":
    main()
