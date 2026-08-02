#!/usr/bin/env python3
"""Colorimetric clustering of a PBR albedo map into a 4-class spectral driver map.

Partitions an albedo texture into up to 4 perceptual color classes and writes an
RGBA map encoding per-pixel soft membership in those classes, for driving a
mixture of spectral signatures in a shading language.

Encoding
--------
    R, G, B  membership in classes 0, 1, 2
    (1-R-G-B) membership in class 3, implicit via the sum-to-one constraint
    A        lightness, L* rescaled over the range recorded in the sidecar

Classes are ordered by descending total membership mass, so R is the most
abundant class and the implicit channel the least. Quantization uses largest
remainder rounding, so the four channels sum to exactly the full-scale value and
the implicit channel is recovered without accumulated error.

Memberships behave like area fractions: a pixel midway between two class centers
splits 50/50. Averaging them (mipmapping) and renormalizing therefore stays
physically meaningful.

The sidecar JSON carries the class centers -- in CIELAB, linear RGB and sRGB hex
-- so each class can be matched to a spectral model downstream.
"""
from __future__ import annotations

import argparse
import json
from pathlib import Path

import numpy as np
from PIL import Image

Image.MAX_IMAGE_PIXELS = None

# color

M_RGB2XYZ = np.array([[0.4124564, 0.3575761, 0.1804375],
                      [0.2126729, 0.7151522, 0.0721750],
                      [0.0193339, 0.1191920, 0.9503041]])
WP_D65 = np.array([0.9504559, 1.0000000, 1.0890578])

# OKLab, Bjorn Ottosson's matrices.
M_OK1 = np.array([[0.4122214708, 0.5363325363, 0.0514459929],
                  [0.2119034982, 0.6806995451, 0.1073969566],
                  [0.0883024619, 0.2817188376, 0.6299787005]])
M_OK2 = np.array([[0.2104542553, +0.7936177850, -0.0040720468],
                  [1.9779984951, -2.4285922050, +0.4505937099],
                  [0.0259040371, +0.7827717662, -0.8086757660]])


def srgb_to_linear(c):
    return np.where(c <= 0.04045, c / 12.92, ((np.maximum(c, 0.0) + 0.055) / 1.055) ** 2.4)


def linear_to_srgb(c):
    c = np.clip(c, 0.0, 1.0)
    return np.where(c <= 0.0031308, c * 12.92, 1.055 * c ** (1 / 2.4) - 0.055)


def linear_to_cielab(rgb):
    t = (rgb @ M_RGB2XYZ.T) / WP_D65
    d = 6.0 / 29.0
    f = np.where(t > d ** 3, np.cbrt(np.maximum(t, 1e-12)), t / (3 * d * d) + 4.0 / 29.0)
    return np.stack([116 * f[..., 1] - 16,
                     500 * (f[..., 0] - f[..., 1]),
                     200 * (f[..., 1] - f[..., 2])], axis=-1)


def cielab_to_linear(lab):
    fy = (lab[..., 0] + 16) / 116
    f = np.stack([fy + lab[..., 1] / 500, fy, fy - lab[..., 2] / 200], axis=-1)
    d = 6.0 / 29.0
    t = np.where(f > d, f ** 3, 3 * d * d * (f - 4.0 / 29.0))
    return (t * WP_D65) @ np.linalg.inv(M_RGB2XYZ).T


def linear_to_oklab(rgb):
    """Scaled by 100 so distances are on roughly the same footing as dE*ab."""
    lms = np.cbrt(np.maximum(rgb @ M_OK1.T, 0.0))
    return (lms @ M_OK2.T) * 100.0


def oklab_to_linear(lab):
    lms = (lab / 100.0) @ np.linalg.inv(M_OK2).T
    return (lms ** 3) @ np.linalg.inv(M_OK1).T


SPACES = {"cielab": (linear_to_cielab, cielab_to_linear),
          "oklab": (linear_to_oklab, oklab_to_linear)}


def srgb_hex(lin):
    v = np.clip(np.round(linear_to_srgb(np.asarray(lin)) * 255), 0, 255).astype(int)
    return "#%02x%02x%02x" % tuple(v)


# preprocessing

def add_noise(lin, sigma, rng):
    """Perturb each linear channel independently, in place, clamped to [0,1].

    Chroma subsampling and DCT ringing in lossy textures collapse the
    chromaticity plane into banded streaks. Dithering with a little noise
    spreads those bands back into a smooth distribution, so the fit sees the
    underlying chromaticity sweep rather than the codec's staircase.

    Works in row chunks so peak memory stays bounded on large textures.
    """
    rows = max(1, (1 << 22) // max(lin.shape[1], 1))
    for y0 in range(0, len(lin), rows):
        chunk = lin[y0:y0 + rows]
        chunk += rng.standard_normal(chunk.shape, dtype=np.float32) * sigma
        np.clip(chunk, 0.0, 1.0, out=chunk)
    return lin


# clustering

def fit_centers(sample, k, l_weight, seed):
    """GMM with full covariance, fitted in the weighted metric.

    Centers come back as responsibility-weighted means of the *unweighted*
    samples, so every class has a meaningful L* even when l_weight is 0 and the
    lightness axis carries no weight in the fit itself.
    """
    from sklearn.mixture import GaussianMixture

    weighted = sample * weight_scale(l_weight)
    gmm = GaussianMixture(n_components=k, covariance_type="full", n_init=4,
                          random_state=seed, reg_covar=1e-4)
    resp = gmm.fit(weighted).predict_proba(weighted)
    mass = resp.sum(axis=0)
    keep = mass > 1e-6
    centers = (resp[:, keep].T @ sample) / mass[keep][:, None]
    return centers, mass[keep] / mass[keep].sum()


def weight_scale(l_weight):
    return np.array([l_weight, 1.0, 1.0])


def merge_close(centers, weights, min_sep, l_weight):
    """Greedily merge the closest pair while any pair is nearer than min_sep.

    Separation is measured in the same weighted metric used for clustering, so
    the threshold stays consistent with how the classes were found.
    """
    centers, weights = centers.copy(), weights.copy()
    while len(centers) > 1:
        d = pairwise(centers, l_weight)
        np.fill_diagonal(d, np.inf)
        i, j = np.unravel_index(np.argmin(d), d.shape)
        if d[i, j] >= min_sep:
            break
        w = weights[i] + weights[j]
        centers[i] = (centers[i] * weights[i] + centers[j] * weights[j]) / w
        weights[i] = w
        centers = np.delete(centers, j, axis=0)
        weights = np.delete(weights, j)
    return centers, weights


def pairwise(centers, l_weight):
    """Pairwise center distances in the weighted clustering metric."""
    d = (centers[:, None] - centers[None, :]) * weight_scale(l_weight)
    return np.linalg.norm(d, axis=-1)


def memberships(lab, centers, sigma, l_weight):
    """Softmax over -dE^2 / 2 sigma^2. Shape (..., K), rows sum to 1."""
    d2 = ((lab[..., None, :] - centers) * weight_scale(l_weight)) ** 2
    d2 = d2.sum(axis=-1)
    z = -d2 / (2.0 * sigma * sigma)
    z -= z.max(axis=-1, keepdims=True)
    e = np.exp(z)
    return e / e.sum(axis=-1, keepdims=True)


# encoding

def write_rgba(arr, path):
    """Write an RGBA array. PIL cannot encode 16-bit RGBA PNG, so defer to cv2."""
    if arr.dtype == np.uint8:
        Image.fromarray(arr, mode="RGBA").save(path)
        return
    import cv2
    if not cv2.imwrite(str(path), arr[..., [2, 1, 0, 3]]):
        raise RuntimeError(f"cv2 failed to write {path}")


def largest_remainder(u, full_scale):
    """Quantize rows of u (summing to 1) to integers summing to exactly full_scale."""
    scaled = u * full_scale
    base = np.floor(scaled).astype(np.int32)
    deficit = full_scale - base.sum(axis=-1)
    rem = scaled - base
    # Rank remainders descending; the top `deficit` entries each get one more.
    order = np.argsort(-rem, axis=-1, kind="stable")
    rank = np.empty_like(order)
    np.put_along_axis(rank, order, np.arange(u.shape[-1])[None, :], axis=-1)
    return base + (rank < deficit[:, None]).astype(np.int32)


# main

def main():
    p = argparse.ArgumentParser(description=__doc__,
                                formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("image", type=Path)
    p.add_argument("-o", "--output", type=Path, help="output RGBA PNG (default: <image>_cls.png)")
    p.add_argument("-k", "--classes", type=int, default=4, help="max classes, 1-4 (default 4)")
    p.add_argument("--space", choices=sorted(SPACES), default="cielab")
    p.add_argument("--l-weight", type=float, default=0.25,
                   help="weight on the lightness axis when clustering (default 0.25). "
                        "This is the main knob: it encodes whether lightness variation "
                        "separates materials (raise it) or varies within one material "
                        "(lower it). At 1.0 classes tend to become lightness strata, "
                        "which duplicates what the alpha channel already carries.")
    p.add_argument("--min-separation", type=float, default=2.5,
                   help="merge class centers closer than this, measured in the weighted "
                        "clustering metric (default 2.5)")
    p.add_argument("--blend-sigma", type=float, default=None,
                   help="membership blend width in dE (default: 0.4x the smallest "
                        "pairwise center separation)")
    p.add_argument("--noise", type=float, default=0.0, metavar="SIGMA",
                   help="std deviation of gaussian noise added independently to each "
                        "linear R, G, B channel after sRGB decode, clamped to [0,1] "
                        "(default 0, off). Dithers away the chromaticity banding that "
                        "lossy compression leaves behind; 0.002-0.01 is a usual range. "
                        "Note the sigma is in linear reflectance, so it perturbs dark "
                        "pixels more in perceptual terms than bright ones.")
    p.add_argument("--samples", type=int, default=300_000, help="pixels sampled to fit centers")
    p.add_argument("--bits", type=int, choices=(8, 16), default=8)
    p.add_argument("--lightness-range", nargs=2, type=float, metavar=("LO", "HI"),
                   help="explicit L* range for the alpha channel (default: 1st/99th pct)")
    p.add_argument("--seed", type=int, default=0)
    p.add_argument("--diagnostics", action="store_true", help="also write a diagnostic figure")
    args = p.parse_args()

    if not 1 <= args.classes <= 4:
        p.error("--classes must be between 1 and 4")
    if args.noise < 0:
        p.error("--noise must be non-negative")

    to_lab, from_lab = SPACES[args.space]
    out_path = args.output or args.image.with_name(args.image.stem + "_cls.png")

    img = Image.open(args.image)
    if img.mode != "RGB":
        img = img.convert("RGB")
    W, H = img.size
    print(f"{args.image.name}: {W}x{H}")

    lin = srgb_to_linear(np.asarray(img, dtype=np.float32) / 255.0).astype(np.float32)

    rng = np.random.default_rng(args.seed)
    if args.noise > 0:
        add_noise(lin, args.noise, rng)
        print(f"  dithered with sigma {args.noise:g} (linear)")

    flat = lin.reshape(-1, 3)
    n = min(args.samples, len(flat))
    sample = to_lab(flat[rng.choice(len(flat), n, replace=False)].astype(np.float64))

    centers, weights = fit_centers(sample, args.classes, args.l_weight, args.seed)
    if args.classes > 1:
        centers, weights = merge_close(centers, weights, args.min_separation, args.l_weight)
    k = len(centers)
    print(f"  {args.classes} requested -> {k} classes after merging at dE {args.min_separation}")

    if k > 1:
        d = pairwise(centers, args.l_weight)
        np.fill_diagonal(d, np.inf)
        sep = d.min()
        print(f"  pairwise center separations (dE): min {sep:.2f}")
        for row in np.where(np.isinf(d), 0.0, d):
            print("    " + "  ".join(f"{v:6.2f}" for v in row))
    else:
        sep = 10.0
    sigma = args.blend_sigma if args.blend_sigma is not None else 0.4 * sep
    print(f"  blend sigma: {sigma:.2f} dE")

    # Order classes by total membership mass over the sampled pixels.
    mass = memberships(sample, centers, sigma, args.l_weight).sum(axis=0)
    order = np.argsort(-mass)
    centers, mass = centers[order], mass[order]
    shares = mass / mass.sum()
    print("  class shares: " + "  ".join(f"{s:.1%}" for s in shares))

    # Lightness range for the alpha channel.
    if args.lightness_range:
        lo, hi = args.lightness_range
    else:
        Ls = sample[:, 0]
        lo, hi = float(np.percentile(Ls, 1)), float(np.percentile(Ls, 99))
    if hi - lo < 1e-6:
        hi = lo + 1.0
    print(f"  lightness range: L* [{lo:.2f}, {hi:.2f}]")

    # Full-resolution pass, in row tiles to bound peak memory.
    full_scale = (1 << args.bits) - 1
    dtype = np.uint8 if args.bits == 8 else np.uint16
    out = np.empty((H, W, 4), dtype=dtype)
    resid = np.zeros(H, dtype=np.float64)
    tile = max(1, (1 << 22) // max(W, 1))
    for y0 in range(0, H, tile):
        y1 = min(y0 + tile, H)
        lab = to_lab(lin[y0:y1].reshape(-1, 3).astype(np.float64))
        u = memberships(lab, centers, sigma, args.l_weight)

        # Pad to 4 classes so the sum-to-one encoding is uniform across textures.
        if k < 4:
            u = np.concatenate([u, np.zeros((len(u), 4 - k))], axis=-1)
        q = largest_remainder(u, full_scale)
        out[y0:y1, :, :3] = q[:, :3].reshape(y1 - y0, W, 3).astype(dtype)

        a = (lab[:, 0] - lo) / (hi - lo)
        out[y0:y1, :, 3] = np.round(np.clip(a, 0, 1) * full_scale).reshape(y1 - y0, W).astype(dtype)

        # Diagnostic only: how far the class-center mixture lands from the pixel.
        mix = u[:, :k] @ centers
        resid[y0:y1] = np.linalg.norm(mix - lab, axis=-1).reshape(y1 - y0, W).mean(axis=1)

    write_rgba(out, out_path)
    print(f"  wrote {out_path}  ({args.bits}-bit RGBA)")
    print(f"  mean mixture residual: {resid.mean():.2f} dE  (diagnostic, not an objective)")

    centers_lin = np.clip(from_lab(centers), 0.0, None)
    meta = {
        "source": args.image.name,
        "resolution": [W, H],
        "space": args.space,
        "classes": int(k),
        "encoding": {
            "rgb": "membership in classes 0,1,2",
            "implicit": "membership in class 3 = 1 - R - G - B",
            "alpha": "L* = lightness_range[0] + A * (lightness_range[1] - lightness_range[0])",
            "bits": args.bits,
            "ordering": "descending total membership mass",
        },
        "lightness_range": [lo, hi],
        "blend_sigma": float(sigma),
        "l_weight": args.l_weight,
        "min_separation": args.min_separation,
        "noise": args.noise,
        "mean_mixture_residual_dE": float(resid.mean()),
        "class": [
            {
                "index": i,
                "channel": "RGB"[i] if i < 3 else "implicit",
                "share": float(shares[i]),
                "lab": [float(v) for v in centers[i]],
                "linear_rgb": [float(v) for v in centers_lin[i]],
                "srgb_hex": srgb_hex(centers_lin[i]),
            }
            for i in range(k)
        ],
    }
    meta_path = out_path.with_suffix(".json")
    meta_path.write_text(json.dumps(meta, indent=2) + "\n")
    print(f"  wrote {meta_path}")
    for c in meta["class"]:
        lab = c["lab"]
        print(f"    {c['channel']:>8}  {c['srgb_hex']}  share {c['share']:5.1%}  "
              f"L*={lab[0]:6.2f} a*={lab[1]:+6.2f} b*={lab[2]:+6.2f}")

    if args.diagnostics:
        write_diagnostics(out_path, img, out, sample, centers, shares, args, full_scale)


def write_diagnostics(out_path, img, out, sample, centers, shares, args, full_scale):
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    k = len(centers)
    _, from_lab = SPACES[args.space]
    cols = np.clip(linear_to_srgb(np.clip(from_lab(centers), 0, None)), 0, 1)

    thumb = np.asarray(img.resize((512, 512), Image.LANCZOS))
    disp = out if out.dtype == np.uint8 else (out >> 8).astype(np.uint8)
    small = np.asarray(Image.fromarray(disp, "RGBA").resize((512, 512), Image.LANCZOS))
    u = small[..., :3].astype(np.float64) / 255.0
    u = np.concatenate([u, np.clip(1.0 - u.sum(axis=-1, keepdims=True), 0, 1)], axis=-1)

    fig, ax = plt.subplots(2, 4, figsize=(17, 9))
    ax[0, 0].imshow(thumb); ax[0, 0].set_title("albedo")
    for i in range(4):
        a = ax[0, 1 + i] if i < 3 else ax[1, 0]
        a.imshow(u[..., i], cmap="magma", vmin=0, vmax=1)
        lbl = "RGB"[i] if i < 3 else "implicit"
        a.set_title(f"class {i} ({lbl})" + (f"  {shares[i]:.1%}" if i < k else "  unused"))
    ax[1, 1].imshow(small[..., 3], cmap="gray"); ax[1, 1].set_title("A: lightness")

    # False-color composite: memberships blended with each class's own color.
    comp = np.einsum("...k,kc->...c", u[..., :k], cols)
    ax[1, 2].imshow(np.clip(comp, 0, 1)); ax[1, 2].set_title("class composite")

    ax[1, 3].scatter(sample[::37, 1], sample[::37, 2], s=1, alpha=0.05,
                     c=np.clip(linear_to_srgb(np.clip(from_lab(sample[::37]), 0, None)), 0, 1))
    ax[1, 3].scatter(centers[:, 1], centers[:, 2], s=260, c=cols,
                     edgecolors="k", linewidths=2, zorder=5)
    ax[1, 3].set_xlabel("a*"); ax[1, 3].set_ylabel("b*")
    ax[1, 3].set_title("chromaticity + centers"); ax[1, 3].set_aspect("equal")

    for a in ax.flat[:-1]:
        a.set_xticks([]); a.set_yticks([])
    fig.suptitle(out_path.stem)
    fig.tight_layout()
    dpath = out_path.with_name(out_path.stem + "_diag.png")
    fig.savefig(dpath, dpi=110)
    print(f"  wrote {dpath}")


if __name__ == "__main__":
    main()
