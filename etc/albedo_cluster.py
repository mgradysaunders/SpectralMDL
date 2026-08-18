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

How literally that holds depends on --encoding. The default reads each pixel as
a soft classification, which is only an area fraction near the midpoint and
saturates quickly on either side; --encoding pairwise reads it as a mixture of
the two classes that best explain it, blended linearly in reflectance, and does
hold across the range.

The classes themselves are found by a fit that discounts how much of the texture
each color covers (see --rarity), so a material present over a small fraction of
the pixels can still claim a class of its own rather than losing it to a second
shade of the substrate.

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


def hex_color(s):
    """argparse type: '#rrggbb' or the 3-digit short form -> linear RGB."""
    t = s.lstrip("#")
    if len(t) == 3:
        t = "".join(c * 2 for c in t)
    if len(t) != 6 or any(c not in "0123456789abcdefABCDEF" for c in t):
        raise argparse.ArgumentTypeError(f"not an sRGB hex color: {s!r}")
    return srgb_to_linear(np.array([int(t[i:i + 2], 16) for i in (0, 2, 4)]) / 255.0)


# preprocessing

def row_chunks(arr):
    """Iterate over row blocks of an image array, bounding peak working memory."""
    rows = max(1, (1 << 22) // max(arr.shape[1], 1))
    for y0 in range(0, len(arr), rows):
        yield arr[y0:y0 + rows]


def add_noise(lin, sigma, rng):
    """Perturb each linear channel independently, in place, clamped to [0,1].

    Chroma subsampling and DCT ringing in lossy textures collapse the
    chromaticity plane into banded streaks. Dithering with a little noise
    spreads those bands back into a smooth distribution, so the fit sees the
    underlying chromaticity sweep rather than the codec's staircase.
    """
    for chunk in row_chunks(lin):
        chunk += rng.standard_normal(chunk.shape, dtype=np.float32) * sigma
        np.clip(chunk, 0.0, 1.0, out=chunk)
    return lin


def stretch_contrast(lin, pct, rng):
    """Map the luminance percentile range onto [0,1] in place, clamped.

    One gain and offset shared by all three channels, so an overall color cast
    survives -- per-channel endpoints would white balance it away, and for an
    albedo map that cast is usually the material, not an artifact. Subtracting
    the black point does lift chroma along with contrast, which is the point:
    a hazy, low-contrast scan spreads back out in a* b* as well as in L*.

    Returns the linear endpoints it mapped from, or None if they are too close
    together to divide by, in which case the image is left untouched.
    """
    n = min(1 << 20, lin.shape[0] * lin.shape[1])
    y = lin.reshape(-1, 3)[rng.integers(0, lin.shape[0] * lin.shape[1], n)] @ M_RGB2XYZ[1]
    lo, hi = (float(v) for v in np.percentile(y, [pct, 100.0 - pct]))
    if hi - lo < 1e-6:
        return None
    for chunk in row_chunks(lin):
        chunk -= lo
        chunk /= hi - lo
        np.clip(chunk, 0.0, 1.0, out=chunk)
    return lo, hi


# interactive

def pick_seeds(lin, k, radius):
    """Collect up to k seed colors by clicking on the image. Blocks on a window.

    Samples the preprocessed linear image rather than the source file, so what
    is picked is what the fit will see. Each click averages the (2*radius+1)^2
    neighborhood at full resolution, because a single pixel of a lossy texture
    is as likely to be DCT ringing as it is to be the material.

    Left click samples, right click undoes, enter closes. Returns linear RGB
    triples in pick order, possibly empty if the window is closed with none.
    """
    import matplotlib
    if matplotlib.get_backend().lower().startswith("agg"):
        raise RuntimeError(f"--pick needs an interactive matplotlib backend, "
                           f"got {matplotlib.get_backend()!r}")
    import matplotlib.pyplot as plt
    from matplotlib import patheffects
    outline = [patheffects.withStroke(linewidth=2.5, foreground="k")]

    # Decimate for display only; clicks map back through `step` to full res.
    step = max(1, min(lin.shape[:2]) // 900)
    seeds, marks = [], []

    fig, ax = plt.subplots(figsize=(10, 8))
    ax.imshow(np.clip(linear_to_srgb(lin[::step, ::step]), 0, 1), interpolation="nearest")
    ax.set_xticks([]); ax.set_yticks([])

    def retitle():
        ax.set_title(f"[{len(seeds)}/{k}]  left click: sample   "
                     f"right click: undo   enter: done")
        fig.canvas.draw_idle()

    def sample_at(x, y):
        cy, cx = int(round(y)) * step, int(round(x)) * step
        y0, y1 = max(cy - radius, 0), min(cy + radius + 1, lin.shape[0])
        x0, x1 = max(cx - radius, 0), min(cx + radius + 1, lin.shape[1])
        return lin[y0:y1, x0:x1].reshape(-1, 3).mean(axis=0).astype(np.float64)

    def on_click(ev):
        if ev.inaxes is not ax or ev.xdata is None:
            return
        if ev.button == 3 and seeds:
            seeds.pop()
            for artist in marks.pop():
                artist.remove()
        elif ev.button == 1 and len(seeds) < k:
            c = sample_at(ev.xdata, ev.ydata)
            seeds.append(c)
            print(f"    seed {len(seeds)}: {srgb_hex(c)}")
            marks.append([
                ax.scatter([ev.xdata], [ev.ydata], s=300, zorder=5, linewidths=2,
                           c=[np.clip(linear_to_srgb(c), 0, 1)], edgecolors="k"),
                ax.annotate(str(len(seeds)), (ev.xdata, ev.ydata), zorder=6,
                            color="w", ha="center", va="center", fontsize=8,
                            path_effects=outline)])
        else:
            return
        retitle()

    def on_key(ev):
        if ev.key in ("enter", "escape"):
            plt.close(fig)

    fig.canvas.mpl_connect("button_press_event", on_click)
    fig.canvas.mpl_connect("key_press_event", on_key)
    retitle()
    fig.tight_layout()
    plt.show()
    return seeds


# clustering

REG_COVAR = 1e-4


def fit_centers(sample, k, l_weight, seed, seeds=None):
    """GMM with full covariance, fitted in the weighted metric.

    Centers come back as responsibility-weighted means of the *unweighted*
    samples, so every class has a meaningful L* even when l_weight is 0 and the
    lightness axis carries no weight in the fit itself.

    Optional `seeds` (in the same space as `sample`) initialize the component
    means, so the fit refines the classes that were pointed at instead of
    searching from k-means restarts. Restarts are pointless once the init is
    explicit -- they would all start from the same place -- so n_init drops to 1.

    Seeding the means alone is not enough. GaussianMixture runs its k-means
    initializer regardless, and takes the starting covariances and mixture
    weights from that partition while overriding only the means, so a seed is
    paired with the shape and the prior of whichever k-means cluster happened to
    land in the same slot. A component seeded on a thin material and handed a
    quarter of the gamut to cover claims a piece of the substrate on the first
    step and never comes back. Deriving all three from the seeds keeps them
    consistent.
    """
    from sklearn.mixture import GaussianMixture

    scale = weight_scale(l_weight)
    weighted = sample * scale
    init = {}
    if seeds is not None:
        init = dict(zip(("weights_init", "precisions_init"),
                        seeded_init(weighted, seeds * scale, REG_COVAR)))
    gmm = GaussianMixture(n_components=k, covariance_type="full",
                          n_init=1 if seeds is not None else 4,
                          means_init=None if seeds is None else seeds * scale,
                          random_state=seed, reg_covar=REG_COVAR, **init)
    resp = gmm.fit(weighted).predict_proba(weighted)
    mass = resp.sum(axis=0)
    keep = mass > 1e-6
    centers = (resp[:, keep].T @ sample) / mass[keep][:, None]
    return centers, mass[keep] / mass[keep].sum()


def seeded_init(x, seeds, reg):
    """Starting mixture weight and precision for each seed, from its own cell.

    Each seed is given the prior and the shape of the samples that are nearer to
    it than to any other seed, so it starts out describing the material it was
    pointed at rather than a quarter of the whole gamut.
    """
    cell = np.argmin(((x[:, None, :] - seeds) ** 2).sum(axis=-1), axis=1)
    d = x.shape[1]
    weights = np.empty(len(seeds))
    precisions = np.empty((len(seeds), d, d))
    for i in range(len(seeds)):
        m = cell == i
        weights[i] = max(int(m.sum()), 1)
        cov = np.cov(x[m].T) if m.sum() > d else np.eye(d) * 4.0
        precisions[i] = np.linalg.inv(cov + np.eye(d) * reg)
    return weights / weights.sum(), precisions


def weight_scale(l_weight):
    return np.array([l_weight, 1.0, 1.0])


def rarity_multiplicity(lab, alpha, bin_size, floor, budget):
    """How many times each sample should count in the fit, given how rare its color is.

    A maximum likelihood fit weights every pixel equally, so a class's pull on
    the centers is proportional to the area it covers. A material over a few
    percent of the texture holds a class comfortably; one under a couple of
    percent does not, and the component that should have described it walks off
    to split the substrate into two shades of the same thing instead. That
    happens whether the class was found by the fit or seeded on the color by
    hand, which is why pointing at a thin material does not make it stick.

    Repeating a sample is exactly how a fit is told to weight it, and unlike
    drawing a weighted resample it adds no noise of its own -- which matters
    here, because the classes at stake are the ones with the fewest pixels to
    draw from. Samples are binned on a coarse Lab lattice, and a bin holding a
    fraction f of the pixels of the most populated bin is repeated f**-alpha
    times, so alpha=0 leaves the fit area-proportional and alpha=1 equalizes the
    occupied bins outright, clustering the palette rather than the pixels.

    Bins holding fewer than `floor` samples are left at their natural weight
    rather than boosted, so the flattening reaches thin materials and not the
    stray pixels of compression ringing between them. `budget` caps the total
    sample count, since a texture with many rare colors could otherwise inflate
    the fitting set without bound.
    """
    key = np.floor(lab / bin_size).astype(np.int64)
    key -= key.min(axis=0)
    flat = (key[:, 0] * (key[:, 1].max() + 1) + key[:, 1]) * (key[:, 2].max() + 1) + key[:, 2]
    _, inv, counts = np.unique(flat, return_inverse=True, return_counts=True)
    c = counts[inv].astype(np.float64)
    busiest = counts[counts >= floor].max() if (counts >= floor).any() else counts.max()
    m = np.where(c >= floor, np.maximum(busiest / np.maximum(c, 1.0), 1.0) ** alpha, 1.0)
    m = np.maximum(np.round(m), 1).astype(np.int64)
    # Spend at most `budget` samples: shrink the boost, never the sample itself.
    excess = m.sum() - len(lab)
    if excess > budget - len(lab) and excess > 0:
        s = max(budget - len(lab), 0) / excess
        m = np.maximum(np.round(1 + (m - 1) * s), 1).astype(np.int64)
    return m


def chromatic_endmembers(sample, centers, q, l_weight):
    """Move each center out from the mean toward the edge of its own class.

    A cluster center is the average of everything the class owns, and most of
    what it owns is mixed with something. The center is therefore not the color
    of the pure material but the color of the material with the mixing already
    averaged in, biased inward toward whatever it neighbors. A mixture encoding
    wants the unmixed color, so the bias is worth undoing: the centroid is the
    contaminated estimate, not the conservative one.

    Each class is pushed along the direction leading away from the mean of all
    the centers, out to the q-th percentile of how far its own members reach in
    that direction. Reach is measured in the weighted metric, so lightness
    counts for as little in placing an endmember as it did in finding the class.
    The shift is taken relative to the class's own median, which leaves q = 50
    exactly where it started, and it is a percentile rather than an extreme so
    that one stray pixel cannot drag a class across the gamut.

    Pushing indefinitely does not pay: past the point where the endmembers stop
    resembling the material, ordinary pixels have further to travel to be
    explained and the reconstruction gets worse again.
    """
    scale = weight_scale(l_weight)
    hub = centers.mean(axis=0)
    cell = np.argmin((((sample[:, None, :] - centers) * scale) ** 2).sum(axis=-1), axis=1)
    out = centers.copy()
    for i in range(len(centers)):
        members = sample[cell == i]
        if len(members) < 32:
            continue
        d = (centers[i] - hub) * scale
        norm = float(np.linalg.norm(d))
        if norm < 1e-9:  # a class sitting on the hub has no outward direction
            continue
        axis = d / norm
        reach = ((members - hub) * scale) @ axis
        beyond = float(np.percentile(reach, q) - np.percentile(reach, 50.0))
        # Back out of the weighted metric. An axis carrying no weight gets no
        # movement, rather than an infinite one.
        out[i] = centers[i] + np.divide(axis * beyond, scale,
                                        out=np.zeros(len(scale)), where=scale > 0)
    return out


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
    """Softmax over -dE^2 / 2 sigma^2. Shape (..., K), rows sum to 1.

    This answers "which class is this pixel", not "what is this pixel made of".
    The distinction matters, because sigma is one number for the whole texture
    and is derived from the closest pair of centers: the blend between any two
    classes has width 2 sigma^2 / D, so the further apart a pair is the harder
    the switch between them, and introducing a single pair of similar classes
    sharpens every other pair at once. On a ramp between two of four classes,
    two of which sit 2 dE apart, the blend occupies about 1% of the ramp and the
    rest reads as pure one class or the other. See pairwise_memberships for the
    mixture reading of the same data.
    """
    d2 = ((lab[..., None, :] - centers) * weight_scale(l_weight)) ** 2
    d2 = d2.sum(axis=-1)
    z = -d2 / (2.0 * sigma * sigma)
    z -= z.max(axis=-1, keepdims=True)
    e = np.exp(z)
    return e / e.sum(axis=-1, keepdims=True)


def pairwise_memberships(lab, lin, centers, centers_lin, tau, l_weight):
    """Mixture of the two classes that best account for the pixel. Rows sum to 1.

    Every pair of classes offers an explanation: a fraction along the segment
    joining them, and a residual saying how well that reproduces the color.
    Taking the best explanation would jump wherever the winning pair changes, so
    the explanations are averaged, weighted by exp(-excess residual^2 / 2 tau^2).

    Which pair is plausible is a question about chromatic character, and is
    judged in the same weighted Lab metric the classes were found in. How much
    of each is a question about areal mixing, which is linear in reflectance,
    and so the fraction is measured there. The two deliberately differ.

    `tau` is a residual scale: how much worse a rival explanation has to be
    before it stops counting. Unlike the softmax sigma it says nothing about the
    distance between classes, so similar classes no longer sharpen unrelated
    pairs. Only ever two classes mix at a time, so three channels remain enough
    however many classes there are, and no pixel is required to lie inside the
    simplex the classes span.

    Costs K(K-1)/2 evaluations per pixel. The hypotheses are recomputed rather
    than kept, because holding every pair's fraction and residual for a whole
    row tile costs more memory than recomputing them costs time.
    """
    n, k = len(lab), len(centers)
    if k == 1:
        return np.ones((n, 1))
    scale = weight_scale(l_weight)
    pairs = [(i, j) for i in range(k) for j in range(i + 1, k)]

    def hypothesis(i, j):
        d = centers_lin[j] - centers_lin[i]
        denom = float(d @ d)
        # Coincident centers explain nothing beyond their own color.
        t = (np.clip(((lin - centers_lin[i]) @ d) / denom, 0.0, 1.0)
             if denom > 1e-18 else np.zeros(n))
        model = centers[i] + t[:, None] * (centers[j] - centers[i])
        return t, np.linalg.norm((model - lab) * scale, axis=-1)

    best = np.full(n, np.inf)
    for i, j in pairs:
        np.minimum(best, hypothesis(i, j)[1], out=best)

    u = np.zeros((n, k))
    total = np.zeros(n)
    for i, j in pairs:
        t, err = hypothesis(i, j)
        w = np.exp(-(err ** 2 - best ** 2) / (2.0 * tau * tau))
        u[:, i] += w * (1.0 - t)
        u[:, j] += w * t
        total += w
    return u / total[:, None]


def triangle_memberships(lab, lin, centers, centers_lin, tau, l_weight, min_det=1e-24):
    """As pairwise_memberships, but each hypothesis is a triple rather than a pair.

    Letting three classes mix at once buys reconstruction wherever the classes
    span real area in reflectance, and buys nothing where they sit on a line.
    Which of those a texture is varies, hence the separate encoding rather than a
    replacement.

    Each hypothesis is the closest point on the CLOSED triangle: the barycentric
    fit where that lands inside, and the nearest of the three edges where it does
    not. Closing the triangle is what keeps the encoding continuous. Letting a
    hypothesis abstain outside itself instead looks equivalent and is not, since
    it both vanishes abruptly at the boundary and, just inside, double counts an
    edge that another hypothesis already covers; measured on a path crossing
    between two triples, abstaining jumps about twenty times as far.

    Every edge belongs to some triangle, so pairs are not enumerated separately;
    doing so would reintroduce exactly that double counting. Triples spanning
    less than `min_det` are dropped as slivers, whose barycentric coordinates
    swing wildly on noise, and if that leaves nothing the pairwise encoding
    stands in.
    """
    n, k = len(lab), len(centers)
    scale = weight_scale(l_weight)
    triples = [(i, j, m) for i in range(k) for j in range(i + 1, k)
               for m in range(j + 1, k)]

    def edge(p, q):
        v = centers_lin[q] - centers_lin[p]
        vv = float(v @ v)
        t = (np.clip(((lin - centers_lin[p]) @ v) / vv, 0.0, 1.0)
             if vv > 1e-18 else np.zeros(n))
        w = np.zeros((n, k))
        w[:, p] = 1.0 - t
        w[:, q] = t
        return w

    def hypothesis(i, j, m):
        e1, e2 = centers_lin[j] - centers_lin[i], centers_lin[m] - centers_lin[i]
        g11, g12, g22 = float(e1 @ e1), float(e1 @ e2), float(e2 @ e2)
        det = g11 * g22 - g12 * g12
        if abs(det) < min_det:
            return None
        d = lin - centers_lin[i]
        r1, r2 = d @ e1, d @ e2
        b = (g22 * r1 - g12 * r2) / det
        c = (g11 * r2 - g12 * r1) / det
        inside = (b >= 0) & (c >= 0) & (b + c <= 1)

        w = np.zeros((n, k))
        w[:, i] = 1.0 - b - c
        w[:, j] = b
        w[:, m] = c
        if not inside.all():
            # Outside the triangle the closest point lies on one of its edges.
            best_r = np.full(n, np.inf)
            best_w = np.zeros((n, k))
            for p, q in ((i, j), (i, m), (j, m)):
                ew = edge(p, q)
                r = np.linalg.norm(ew @ centers_lin - lin, axis=-1)
                take = r < best_r
                best_r = np.where(take, r, best_r)
                best_w = np.where(take[:, None], ew, best_w)
            w = np.where(inside[:, None], w, best_w)
        return w, np.linalg.norm((w @ centers - lab) * scale, axis=-1)

    live = [t for t in triples if hypothesis(*t) is not None]
    if not live:
        return pairwise_memberships(lab, lin, centers, centers_lin, tau, l_weight)

    best = np.full(n, np.inf)
    for t in live:
        np.minimum(best, hypothesis(*t)[1], out=best)

    u = np.zeros((n, k))
    total = np.zeros(n)
    for t in live:
        w, err = hypothesis(*t)
        g = np.exp(-(err ** 2 - best ** 2) / (2.0 * tau * tau))
        u += w * g[:, None]
        total += g
    return u / total[:, None]


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
    p.add_argument("--seed-color", nargs="+", type=hex_color, metavar="HEX",
                   help="seed the class centers with explicit sRGB colors, '#rrggbb' or "
                        "'#rgb', up to 4. The count overrides --classes: naming the "
                        "colors is also naming how many there are. The fit refines the "
                        "centers from these unless --lock-seeds is given, and seeded "
                        "centers are never merged by --min-separation.")
    p.add_argument("--pick", action="store_true",
                   help="pick the seed colors interactively, as with an eyedropper: "
                        "opens a window on the preprocessed image where left click "
                        "samples, right click undoes, and enter closes. Prints the "
                        "equivalent --seed-color line so the session can be replayed "
                        "headlessly. Needs an interactive matplotlib backend.")
    p.add_argument("--pick-radius", type=int, default=2, metavar="R",
                   help="eyedropper sample size for --pick: the mean over a (2R+1)^2 "
                        "neighborhood at full resolution (default 2, i.e. 5x5). Single "
                        "pixels are as likely to be compression ringing as material.")
    p.add_argument("--lock-seeds", action="store_true",
                   help="use the seed colors verbatim as class centers, skipping the "
                        "mixture fit entirely")
    p.add_argument("--l-weight", type=float, default=0.25,
                   help="weight on the lightness axis when clustering (default 0.25). "
                        "This is the main knob: it encodes whether lightness variation "
                        "separates materials (raise it) or varies within one material "
                        "(lower it). At 1.0 classes tend to become lightness strata, "
                        "which duplicates what the alpha channel already carries.")
    p.add_argument("--endmember", type=float, default=50.0, metavar="Q",
                   help="place each class at the Qth percentile of how far its own "
                        "members reach outward, rather than at their average "
                        "(default 50, which is the average and so leaves this off). "
                        "A class average is biased inward, because most of what a "
                        "class owns is mixed with its neighbors and the mixing is "
                        "averaged in with it; a mixture encoding wants the unmixed "
                        "color. Around 90 measures best, and pushing further makes "
                        "the reconstruction worse again, not better. Ignored under "
                        "--lock-seeds, which asks for the seed colors verbatim.")
    p.add_argument("--min-separation", type=float, default=2.5,
                   help="merge class centers closer than this, measured in the weighted "
                        "clustering metric (default 2.5)")
    p.add_argument("--encoding", choices=("softmax", "pairwise", "triangle"),
                   default="softmax",
                   help="how per-pixel memberships are computed. 'softmax' (default) "
                        "reads each pixel as a soft classification, which switches "
                        "between distant classes sharply and sharpens further still "
                        "when any two classes are similar. 'pairwise' reads it as a "
                        "mixture of the two classes that best explain it, blending "
                        "linearly in reflectance, which is what the memberships are "
                        "meant to drive downstream. 'triangle' lets three classes mix "
                        "at once, which reconstructs better where the class colors "
                        "span real area and no better where they lie on a line, so it "
                        "is worth trying per texture rather than assuming.")
    p.add_argument("--blend-sigma", type=float, default=None,
                   help="membership blend width in dE for --encoding softmax "
                        "(default: 0.4x the smallest pairwise center separation)")
    p.add_argument("--blend-residual", type=float, default=2.0, metavar="TAU",
                   help="for --encoding pairwise, how much worse a rival pair of "
                        "classes has to reproduce a pixel before it stops "
                        "contributing, in dE (default 2.0). Small values approach "
                        "picking a single best pair, which is discontinuous where the "
                        "winner changes; large values average unrelated pairs together "
                        "and wash the fractions out. Unlike --blend-sigma this is a "
                        "residual, not a distance between classes, so it does not "
                        "couple to how far apart the classes happen to be.")
    p.add_argument("--noise", type=float, default=0.0, metavar="SIGMA",
                   help="std deviation of gaussian noise added independently to each "
                        "linear R, G, B channel after sRGB decode, clamped to [0,1] "
                        "(default 0, off). Dithers away the chromaticity banding that "
                        "lossy compression leaves behind; 0.002-0.01 is a usual range. "
                        "Note the sigma is in linear reflectance, so it perturbs dark "
                        "pixels more in perceptual terms than bright ones.")
    p.add_argument("--stretch", nargs="?", type=float, const=1.0, default=None, metavar="PCT",
                   help="contrast-stretch the linear image so the PCT to (100-PCT) "
                        "luminance percentile range spans [0,1] (PCT defaults to 1 when "
                        "the flag is given, off otherwise). One gain and offset shared by "
                        "all three channels, so a genuine color cast survives; chroma "
                        "widens along with the tonal range, which pushes hazy or "
                        "low-contrast scans further apart in a* b*.")
    p.add_argument("--rarity", type=float, default=0.5, metavar="ALPHA",
                   help="flatten the fit's sensitivity to how much of the texture a "
                        "color covers, by repeating each sampled pixel in inverse "
                        "proportion to how populated its region of color space is "
                        "(default 0.5; 0 weights by area as before, 1 equalizes "
                        "outright, clustering the palette rather than the pixels). "
                        "Without it a material under a couple of percent cannot hold a "
                        "class against the substrate even when seeded on it by hand. "
                        "Reported class shares are unaffected: they are always measured "
                        "on the sample as drawn.")
    p.add_argument("--rarity-bin", type=float, default=4.0, metavar="DE",
                   help="edge of the Lab bins --rarity measures color density over "
                        "(default 4.0)")
    p.add_argument("--rarity-floor", type=int, default=8, metavar="N",
                   help="leave color bins holding fewer than N sampled pixels at their "
                        "natural weight, so --rarity lifts thin materials and not the "
                        "stray pixels of compression ringing (default 8)")
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
    if args.stretch is not None and not 0 <= args.stretch < 50:
        p.error("--stretch percentile must be in [0, 50)")
    if args.seed_color and args.pick:
        p.error("--seed-color and --pick are mutually exclusive")
    if args.seed_color and len(args.seed_color) > 4:
        p.error("at most 4 --seed-color values")
    if args.lock_seeds and not (args.seed_color or args.pick):
        p.error("--lock-seeds requires --seed-color or --pick")
    if args.pick_radius < 0:
        p.error("--pick-radius must be non-negative")
    if not 0.0 <= args.rarity <= 1.0:
        p.error("--rarity must be in [0, 1]")
    if args.rarity_bin <= 0:
        p.error("--rarity-bin must be positive")
    if args.rarity_floor < 1:
        p.error("--rarity-floor must be at least 1")
    if args.blend_residual <= 0:
        p.error("--blend-residual must be positive")
    if not 50.0 <= args.endmember < 100.0:
        p.error("--endmember must be in [50, 100)")

    to_lab, from_lab = SPACES[args.space]
    out_path = args.output or args.image.with_name(args.image.stem + "_cls.png")

    img = Image.open(args.image)
    if img.mode != "RGB":
        img = img.convert("RGB")
    W, H = img.size
    print(f"{args.image.name}: {W}x{H}")

    lin = srgb_to_linear(np.asarray(img, dtype=np.float32) / 255.0).astype(np.float32)

    rng = np.random.default_rng(args.seed)
    # Dither before stretching, so the sigma stays in source-linear units and is
    # amplified by exactly the same gain as the banding it is meant to smear.
    if args.noise > 0:
        add_noise(lin, args.noise, rng)
        print(f"  dithered with sigma {args.noise:g} (linear)")

    stretch = None
    if args.stretch is not None:
        stretch = stretch_contrast(lin, args.stretch, rng)
        if stretch:
            print(f"  stretched linear [{stretch[0]:.4f}, {stretch[1]:.4f}] -> [0, 1] "
                  f"({args.stretch:g}/{100 - args.stretch:g} pct luminance)")
        else:
            print("  stretch skipped: luminance percentile range is degenerate")

    seed_lin = list(args.seed_color) if args.seed_color else []
    if args.pick:
        seed_lin = pick_seeds(lin, args.classes, args.pick_radius)
        if seed_lin:
            print("  replay with: --seed-color " + " ".join(srgb_hex(c) for c in seed_lin))
        else:
            print("  nothing picked, falling back to an unseeded fit")
    if seed_lin:
        args.classes = len(seed_lin)

    flat = lin.reshape(-1, 3)
    n = min(args.samples, len(flat))
    sample_lin = flat[rng.choice(len(flat), n, replace=False)].astype(np.float64)
    sample = to_lab(sample_lin)

    # The fit sees rare colors repeated; everything downstream (class shares, the
    # L* range, the diagnostic scatter) reads the sample as drawn, so reported
    # proportions stay true to the texture.
    fit_sample = sample
    if args.rarity > 0:
        m = rarity_multiplicity(sample, args.rarity, args.rarity_bin,
                                args.rarity_floor, 4 * n)
        fit_sample = np.repeat(sample, m, axis=0)
        print(f"  rarity weighting (alpha {args.rarity:g}, bin {args.rarity_bin:g} dE): "
              f"{n} samples -> {len(fit_sample)}, thinnest color counted "
              f"{m.max()}x against the busiest")

    seeds_lab = to_lab(np.asarray(seed_lin, dtype=np.float64)) if seed_lin else None
    if seeds_lab is not None and args.lock_seeds:
        centers = seeds_lab
        weights = np.full(len(centers), 1.0 / len(centers))
        print(f"  {len(centers)} seed colors locked, no fit")
    else:
        centers, weights = fit_centers(fit_sample, args.classes, args.l_weight,
                                       args.seed, seeds_lab)
        # Merging seeded centers would collapse classes named apart on purpose.
        if args.classes > 1 and seeds_lab is None:
            centers, weights = merge_close(centers, weights, args.min_separation, args.l_weight)
    k = len(centers)
    if seeds_lab is None:
        print(f"  {args.classes} requested -> {k} classes after merging at dE {args.min_separation}")
    elif k < len(seeds_lab):
        # A seeded component can still lose all its responsibility to a neighbor.
        print(f"  {len(seeds_lab)} seeded -> {k} classes, {len(seeds_lab) - k} collapsed "
              f"into others during the fit (--lock-seeds keeps them as picked)")
    else:
        print(f"  {k} seeded classes, merging disabled")

    if args.endmember > 50.0:
        if args.lock_seeds:
            print("  --endmember ignored: --lock-seeds asks for the seeds verbatim")
        else:
            moved = chromatic_endmembers(sample, centers, args.endmember, args.l_weight)
            delta = np.linalg.norm((moved - centers) * weight_scale(args.l_weight), axis=1)
            centers = moved
            print(f"  endmembers at the {args.endmember:g}th percentile: "
                  f"centers moved {delta.mean():.2f} dE on average, "
                  f"{delta.max():.2f} at most")

    if k > 1:
        d = pairwise(centers, args.l_weight)
        np.fill_diagonal(d, np.inf)
        sep = d.min()
        if seeds_lab is not None and sep < args.min_separation:
            print(f"  warning: centers are closer than --min-separation "
                  f"({sep:.2f} < {args.min_separation}), kept as seeded")
        print(f"  pairwise center separations (dE): min {sep:.2f}")
        for row in np.where(np.isinf(d), 0.0, d):
            print("    " + "  ".join(f"{v:6.2f}" for v in row))
    else:
        sep = 10.0
    sigma = args.blend_sigma if args.blend_sigma is not None else 0.4 * sep
    if args.encoding == "softmax":
        print(f"  blend sigma: {sigma:.2f} dE")
    else:
        print(f"  {args.encoding} mixing, blend residual: {args.blend_residual:.2f} dE")

    # The pairwise encoding mixes in reflectance, so the centers are needed there
    # too. Kept alongside `centers` from here on, reordered with them.
    centers_lin = np.clip(from_lab(centers), 0.0, None)

    def encode(lab, lin_rgb):
        if args.encoding == "pairwise":
            return pairwise_memberships(lab, lin_rgb, centers, centers_lin,
                                        args.blend_residual, args.l_weight)
        if args.encoding == "triangle":
            return triangle_memberships(lab, lin_rgb, centers, centers_lin,
                                        args.blend_residual, args.l_weight)
        return memberships(lab, centers, sigma, args.l_weight)

    # Order classes by total membership mass over the sampled pixels.
    mass = encode(sample, sample_lin).sum(axis=0)
    order = np.argsort(-mass)
    centers, centers_lin, mass = centers[order], centers_lin[order], mass[order]
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
    if args.encoding in ("pairwise", "triangle"):
        # Every hypothesis needs its own scratch, so take smaller bites.
        tile = max(1, tile // 4)
    for y0 in range(0, H, tile):
        y1 = min(y0 + tile, H)
        tile_lin = lin[y0:y1].reshape(-1, 3).astype(np.float64)
        lab = to_lab(tile_lin)
        u = encode(lab, tile_lin)

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
    objective = "diagnostic, not an objective" if args.encoding == "softmax" \
        else f"what the {args.encoding} encoding minimizes"
    print(f"  mean mixture residual: {resid.mean():.2f} dE  ({objective})")

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
            "membership": args.encoding,
        },
        "lightness_range": [lo, hi],
        "blend_sigma": float(sigma) if args.encoding == "softmax" else None,
        "blend_residual": float(args.blend_residual) if args.encoding != "softmax" else None,
        "l_weight": args.l_weight,
        "endmember": args.endmember,
        "rarity": args.rarity,
        "rarity_bin": args.rarity_bin,
        "rarity_floor": args.rarity_floor,
        "min_separation": args.min_separation,
        "noise": args.noise,
        "stretch_percentile": args.stretch,
        "stretch_range": list(stretch) if stretch else None,
        "seed_colors": [srgb_hex(c) for c in seed_lin] if seed_lin else None,
        "seed_colors_locked": bool(args.lock_seeds) if seed_lin else None,
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
        write_diagnostics(out_path, lin, out, sample, centers, shares, args, seeds_lab)


def write_diagnostics(out_path, lin, out, sample, centers, shares, args, seeds_lab):
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    k = len(centers)
    _, from_lab = SPACES[args.space]
    cols = np.clip(linear_to_srgb(np.clip(from_lab(centers), 0, None)), 0, 1)

    # Show what clustering actually saw, i.e. after dithering and stretching.
    step = max(1, min(lin.shape[:2]) // 512)
    thumb = np.clip(linear_to_srgb(lin[::step, ::step]) * 255, 0, 255).astype(np.uint8)
    thumb = np.asarray(Image.fromarray(thumb, "RGB").resize((512, 512), Image.LANCZOS))
    disp = out if out.dtype == np.uint8 else (out >> 8).astype(np.uint8)
    small = np.asarray(Image.fromarray(disp, "RGBA").resize((512, 512), Image.LANCZOS))
    u = small[..., :3].astype(np.float64) / 255.0
    u = np.concatenate([u, np.clip(1.0 - u.sum(axis=-1, keepdims=True), 0, 1)], axis=-1)

    fig, ax = plt.subplots(2, 4, figsize=(17, 9))
    ax[0, 0].imshow(thumb)
    ax[0, 0].set_title("albedo" + (" (stretched)" if args.stretch is not None else ""))
    for i in range(4):
        a = ax[0, 1 + i] if i < 3 else ax[1, 0]
        a.imshow(u[..., i], cmap="magma", vmin=0, vmax=1)
        lbl = "RGB"[i] if i < 3 else "implicit"
        a.set_title(f"class {i} ({lbl})" + (f"  {shares[i]:.1%}" if i < k else "  unused"))
    ax[1, 1].imshow(small[..., 3], cmap="gray"); ax[1, 1].set_title("A: lightness")

    # False-color composite: memberships blended with each class's own color.
    comp = np.einsum("...k,kc->...c", u[..., :k], cols)
    ax[1, 2].imshow(np.clip(comp, 0, 1)); ax[1, 2].set_title("class composite")
    #ax[1, 2].imshow(np.clip(u[...,:3], 0, 1)); ax[1, 2].set_title("class composite")

    ax[1, 3].scatter(sample[::37, 1], sample[::37, 2], s=1, alpha=0.05,
                     c=np.clip(linear_to_srgb(np.clip(from_lab(sample[::37]), 0, None)), 0, 1))
    ax[1, 3].scatter(centers[:, 1], centers[:, 2], s=260, c=cols,
                     edgecolors="k", linewidths=2, zorder=5)
    # Seeds against fitted centers: how far the fit drifted from what was picked.
    if seeds_lab is not None:
        ax[1, 3].scatter(seeds_lab[:, 1], seeds_lab[:, 2], s=150, marker="x",
                         c="k", linewidths=2, zorder=6, label="seeds")
        ax[1, 3].legend(loc="best", fontsize=8)
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
