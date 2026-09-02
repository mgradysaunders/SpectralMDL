#!/usr/bin/env python3
"""Compare two linear-radiance renders numerically.

Reads the '.exr' or '.hdr' files that 'smdl-toy -output-rgbf' writes and
reports, over an optional crop, the ratio of means, the RMSE of the
difference relative to the reference mean, and, when each side was rendered
twice with different '-sample-offset' values, the z-score of the mean
difference and the fraction of pixels whose difference exceeds z = 3 against
a locally estimated per-pixel variance. Exit status 0 when every threshold
given on the command line holds, 1 when one fails, 2 on a usage error.

    render_compare.py test.exr ref.exr [--test2 t2.exr --ref2 r2.exr]
        [--crop x0,y0,x1,y1] [--max-mean-diff 0.01] [--max-rel-rmse 0.05]
        [--max-z-mean 3] [--max-z-frac 0.01]

Needs numpy and cv2 (OpenCV reads EXR when OPENCV_IO_ENABLE_OPENEXR is set,
which this script does before importing it).
"""
import argparse
import os
import sys

os.environ.setdefault("OPENCV_IO_ENABLE_OPENEXR", "1")

import numpy as np  # noqa: E402

try:
    import cv2  # noqa: E402
except ImportError:  # pragma: no cover
    sys.stderr.write("render_compare.py: needs cv2 (OpenCV) to read images\n")
    sys.exit(2)


def read_linear(path):
    """Read a float image as (H, W, C) float64 RGB, or None on failure."""
    image = cv2.imread(path, cv2.IMREAD_UNCHANGED)
    if image is None:
        return None
    image = np.asarray(image, dtype=np.float64)
    if image.ndim == 2:
        image = image[:, :, None]
    elif image.shape[2] >= 3:
        image = image[:, :, :3][:, :, ::-1]  # BGR to RGB
    return image


def luminance(image):
    """Per-pixel channel average, the scalar the statistics run on."""
    return image.mean(axis=2)


def box_filter(image, radius):
    """Mean over a (2r+1)^2 window, edges clamped; a variance smoother."""
    if radius <= 0:
        return image
    k = 2 * radius + 1
    return cv2.blur(image, (k, k), borderType=cv2.BORDER_REPLICATE)


def main():
    parser = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    parser.add_argument("test")
    parser.add_argument("ref")
    parser.add_argument("--test2", help="second seed of the test render")
    parser.add_argument("--ref2", help="second seed of the reference render")
    parser.add_argument("--crop", help="x0,y0,x1,y1 pixel rectangle, half-open")
    parser.add_argument("--max-mean-diff", type=float,
                        help="fail if |mean(test)/mean(ref) - 1| exceeds this")
    parser.add_argument("--max-rel-rmse", type=float,
                        help="fail if RMSE(test - ref)/mean(ref) exceeds this")
    parser.add_argument("--max-z-mean", type=float,
                        help="fail if |z| of the crop-mean difference exceeds "
                             "this (needs --test2 and --ref2)")
    parser.add_argument("--max-z-frac", type=float,
                        help="fail if the fraction of pixels with |z| > 3 "
                             "exceeds this (needs --test2 and --ref2)")
    parser.add_argument("--z-radius", type=int, default=2,
                        help="box radius for the per-pixel variance estimate "
                             "(default 2)")
    args = parser.parse_args()

    images = {}
    for name in ("test", "ref", "test2", "ref2"):
        path = getattr(args, name)
        if path is None:
            continue
        image = read_linear(path)
        if image is None:
            sys.stderr.write(f"render_compare.py: cannot read {path!r}\n")
            return 2
        images[name] = image
    shape = images["test"].shape
    for name, image in images.items():
        if image.shape != shape:
            sys.stderr.write(f"render_compare.py: {name} has shape "
                             f"{image.shape}, test has {shape}\n")
            return 2
    if ("test2" in images) != ("ref2" in images):
        sys.stderr.write("render_compare.py: give both --test2 and --ref2 "
                         "or neither\n")
        return 2

    height, width = shape[:2]
    x0, y0, x1, y1 = 0, 0, width, height
    if args.crop:
        try:
            x0, y0, x1, y1 = (int(v) for v in args.crop.split(","))
        except ValueError:
            sys.stderr.write("render_compare.py: --crop wants x0,y0,x1,y1\n")
            return 2
        x0, y0 = max(x0, 0), max(y0, 0)
        x1, y1 = min(x1, width), min(y1, height)
        if x1 <= x0 or y1 <= y0:
            sys.stderr.write("render_compare.py: empty crop\n")
            return 2

    def crop(image):
        return luminance(image)[y0:y1, x0:x1]

    have_seeds = "test2" in images
    if have_seeds:
        test_a, test_b = crop(images["test"]), crop(images["test2"])
        ref_a, ref_b = crop(images["ref"]), crop(images["ref2"])
        test = 0.5 * (test_a + test_b)
        ref = 0.5 * (ref_a + ref_b)
    else:
        test, ref = crop(images["test"]), crop(images["ref"])

    mean_test = float(test.mean())
    mean_ref = float(ref.mean())
    diff = test - ref
    rmse = float(np.sqrt(np.mean(diff * diff)))
    failures = []
    print(f"crop            x {x0}..{x1}, y {y0}..{y1}, {diff.size} pixels")
    print(f"mean test       {mean_test:.6g}")
    print(f"mean ref        {mean_ref:.6g}")
    if mean_ref > 0:
        mean_ratio = mean_test / mean_ref
        rel_rmse = rmse / mean_ref
        print(f"mean ratio - 1  {mean_ratio - 1:+.4%}")
        print(f"rel RMSE        {rel_rmse:.4%}")
        if args.max_mean_diff is not None and abs(mean_ratio - 1) > args.max_mean_diff:
            failures.append("mean ratio")
        if args.max_rel_rmse is not None and rel_rmse > args.max_rel_rmse:
            failures.append("rel RMSE")
    else:
        print("mean ref is zero; ratios undefined")
        if args.max_mean_diff is not None or args.max_rel_rmse is not None:
            failures.append("mean ref is zero")

    if have_seeds:
        # Each side's mean of two seeds has per-pixel variance about
        # (a - b)^2 / 4, and the crop mean's variance is the sum over pixels
        # of that divided by N^2.
        var_test = 0.25 * (test_a - test_b) ** 2
        var_ref = 0.25 * (ref_a - ref_b) ** 2
        n = float(diff.size)
        var_mean = (var_test.sum() + var_ref.sum()) / (n * n)
        z_mean = float(diff.mean() / np.sqrt(var_mean)) if var_mean > 0 else 0.0
        # The standard error of each side's crop mean, relative to the
        # reference mean: what the comparison can resolve at all. A mean
        # ratio inside a few of these is not a disagreement, and a reference
        # whose error is larger than the tolerance asked for cannot pass or
        # fail the mean test, only report that it needs more samples.
        se_test = float(np.sqrt(var_test.sum()) / n)
        se_ref = float(np.sqrt(var_ref.sum()) / n)
        if mean_ref > 0:
            print(f"std err of mean test {se_test / mean_ref:.4%}, "
                  f"ref {se_ref / mean_ref:.4%} (relative to mean ref)")
            if args.max_mean_diff is not None and \
                    np.hypot(se_test, se_ref) > 0.5 * args.max_mean_diff * mean_ref:
                print("note: the mean test is inconclusive at this sample "
                      "count; the standard error exceeds half the tolerance")
        print(f"z of mean diff  {z_mean:+.2f}")
        # Per pixel, one seed pair is a one-degree-of-freedom variance
        # estimate, far too noisy for a z test; smooth it over a box first.
        var_local = box_filter(var_test + var_ref, args.z_radius)
        with np.errstate(divide="ignore", invalid="ignore"):
            z = np.where(var_local > 0, diff / np.sqrt(var_local), 0.0)
        z_frac = float(np.mean(np.abs(z) > 3.0))
        print(f"frac |z| > 3    {z_frac:.4%} (box radius {args.z_radius})")
        if args.max_z_mean is not None and abs(z_mean) > args.max_z_mean:
            failures.append("z of mean")
        if args.max_z_frac is not None and z_frac > args.max_z_frac:
            failures.append("z fraction")
    elif args.max_z_mean is not None or args.max_z_frac is not None:
        sys.stderr.write("render_compare.py: z thresholds need --test2 and "
                         "--ref2\n")
        return 2

    if failures:
        print("FAIL: " + ", ".join(failures))
        return 1
    print("PASS" if any(v is not None for v in (
        args.max_mean_diff, args.max_rel_rmse, args.max_z_mean,
        args.max_z_frac)) else "(no thresholds given)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
