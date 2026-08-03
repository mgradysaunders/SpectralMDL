"""Empirical soil reflectance model, 400-2298nm.

A representative spectral model of soil reflectance, fit to 1894 measured
spectra drawn from 8 published reflectance databases. Reflectance is generated
through a logistic link, so every parameter combination yields a spectrum
strictly inside (0, 1).

The model is driven by four pedogenic parameters, all normalized to [0, 1],
which describe a soil by cause rather than by appearance:

  humus     organic matter content. 0 = mineral soil, 1 = humus-rich.
            Darkens the whole spectrum and mutes the iron pigmentation.
  iron      ferric oxide pigment load. 0 = unpigmented, 1 = maximal. Drives
            the chroma of the visible ramp and the ferric absorption near
            900nm.
  aridity   hot/dry versus cool/moist pedogenesis, i.e. the hematite:goethite
            ratio. 0 = humid (goethite, yellow), 1 = arid (hematite, red).
  moisture  water content. 0 = bone dry, 1 = the darkest wet state the soil
            reaches (see moisture_to_smc for the approximate gravimetric
            equivalent). Perceptually warped, so dragging it darkens the soil
            at a constant rate.

humus = iron = 0 is the pale, weakly-colored quartz/carbonate end member.

Internally the spectrum is a linear model in logit space,

    logit R(lambda) = [1, b, r, v] . BB(lambda) + u M1(lambda) + u^2 M2(lambda)

where (b, r, v) are dry-state color coordinates -- log luminance under D65,
and the along-arc and transverse positions of the dry chromaticity on the
empirical soil color curve -- and u is optical water. The pedogenic parameters
map onto (b, r, v, u) bijectively, so they reach exactly the same spectra as
the underlying color coordinates; nothing is traded for the change of
variables. Coefficients live in empirical_soil.npz beside this file.

Usage:
    import empirical_soil
    wvl = empirical_soil.WAVELENGTHS
    R = empirical_soil.evaluate(wvl, humus=0.4, iron=0.6, aridity=0.8)
    rgb = empirical_soil.srgb(humus=0.4, iron=0.6, aridity=0.8)
    params = empirical_soil.fit(wvl_measured, R_measured)

Run `python empirical_soil.py` for an interactive explorer with sliders.
"""
import pathlib

import numpy as np

__all__ = ["MIN_WAVELENGTH", "MAX_WAVELENGTH", "NUM_WAVELENGTHS",
           "WAVELENGTHS", "PARAMETERS", "evaluate", "fit", "to_xyz",
           "to_srgb", "srgb", "moisture_to_smc"]

MIN_WAVELENGTH = 400.0
MAX_WAVELENGTH = 2298.0
NUM_WAVELENGTHS = 1899
WAVELENGTHS = np.linspace(MIN_WAVELENGTH, MAX_WAVELENGTH, NUM_WAVELENGTHS)

PARAMETERS = ("humus", "iron", "aridity", "moisture")

_D = dict(np.load(pathlib.Path(__file__).with_name("empirical_soil.npz")))


# ------------------------------------------------------------------------------
# Colorimetry: Wyman et al. (2013) fits to the CIE 1931 CMFs, D65 illuminant.
# ------------------------------------------------------------------------------
def _cmfs(wvl_nm):
    lam = np.asarray(wvl_nm, dtype=float) / 1000.0

    def pg(c, s_lo, s_hi):
        s = np.where(lam < c, s_lo, s_hi)
        return np.exp(-0.5 * ((lam - c) * s) ** 2)

    x = 3.62 * pg(0.4420, 62.4, 37.4) + 10.56 * pg(0.5998, 26.4, 32.3) \
        - 0.65 * pg(0.5011, 49.0, 38.2)
    y = 8.21 * pg(0.5688, 21.3, 24.7) + 2.86 * pg(0.5309, 61.3, 32.2)
    z = 12.17 * pg(0.4370, 84.5, 27.8) + 6.81 * pg(0.4590, 38.5, 72.5)
    return np.stack([x, y, z])


_D65_WVL = np.arange(400.0, 781.0, 10.0)
_D65_SPD = np.array([
    82.75, 91.49, 93.43, 86.68, 104.86, 117.01, 117.81, 114.86, 115.92,
    108.81, 109.35, 107.80, 104.79, 107.69, 104.41, 104.05, 100.00, 96.33,
    95.79, 88.69, 90.01, 89.60, 87.70, 83.29, 83.70, 80.03, 80.21, 82.28,
    78.28, 69.72, 71.61, 74.35, 61.60, 69.89, 75.09, 63.59, 46.42, 66.81,
    63.38])

_XYZ_TO_SRGB = np.array([[+3.2406, -1.5372, -0.4986],
                         [-0.9689, +1.8758, +0.0415],
                         [+0.0557, -0.2040, +1.0570]])


def _xyz_weights(wvl_nm):
    """Trapezoidal integration weights for CIE XYZ under D65 on an arbitrary
    wavelength grid, normalized so a perfect reflector has Y = 1."""
    wvl = np.asarray(wvl_nm, dtype=float)
    dl = np.gradient(wvl) if wvl.size > 1 else np.ones(1)
    w = _cmfs(wvl) * np.interp(wvl, _D65_WVL, _D65_SPD, left=0.0, right=0.0) * dl
    norm = w[1].sum()
    if norm <= 0.0:
        raise ValueError("wavelengths do not cover the visible range "
                         "(400-780nm), so no color can be computed")
    return w / norm


def to_xyz(wavelengths, reflectance):
    """CIE 1931 XYZ tristimulus of a reflectance spectrum under D65.

    Normalized so a perfect diffuse reflector gives Y = 1. Samples outside the
    visible range contribute nothing, so passing a full VNIR/SWIR spectrum is
    fine.
    """
    return _xyz_weights(wavelengths) @ np.asarray(reflectance, dtype=float)


def to_srgb(wavelengths, reflectance):
    """sRGB color of a reflectance spectrum under D65: 3 gamma-encoded floats
    in [0, 1]. Out-of-gamut colors are clipped."""
    c = np.clip(_XYZ_TO_SRGB @ to_xyz(wavelengths, reflectance), 0.0, 1.0)
    return np.where(c <= 0.0031308, 12.92 * c, 1.055 * c ** (1 / 2.4) - 0.055)


# The visible-band integration weights on the native grid, precomputed for the
# common case of coloring a model spectrum.
_XYZ_W = _xyz_weights(WAVELENGTHS)


# ------------------------------------------------------------------------------
# Pedogenic parameters -> the fitted color and water coordinates.
#
# humus and aridity are relabelings (humus darkens; the hematite:goethite ratio
# is the hue), and iron maps to chroma through a power law whose exponent
# encodes two real effects: humus masks pigment (raises the exponent) and
# hematite tints ~10x harder per gram than goethite (lowers it). Because the
# exponent depends only on humus and aridity -- which map across directly --
# the transform is triangular, hence a bijection of the unit cube.
#
# The signs and the ratio between these two constants are fit to the databases;
# the magnitude is deliberately damped, since the evidence is a partial
# correlation only -- the marginal correlation between chroma and hue across
# soils is ~0 -- and an undamped coupling makes the iron parameter behave
# unpredictably.
# ------------------------------------------------------------------------------
_G_HUMUS = +0.45   # humus masking
_G_ARIDITY = -0.55  # hematite tinting strength


def _warp(moisture):
    """Perceptual moisture warp: map moisture to optical water u/u_max so that
    CIE L* falls at a constant rate. Polynomial g(m) = c1 m + ... + ck m^k with
    ascending-power coefficients, pinned g(0) = 0 and g(1) = 1, evaluated by
    Horner so any degree works."""
    m = np.clip(moisture, 0.0, 1.0)
    acc = 0.0
    for ck in _D["wet_warp"][::-1]:
        acc = acc * m + ck
    return acc * m


def _lerp(t, lo, hi):
    return lo + t * (hi - lo)


def _coords(humus, iron, aridity, moisture):
    """Map the pedogenic parameters onto the fitted design vector [1, b, r, v]
    and the optical water u."""
    h = np.clip(humus, 0.0, 1.0)
    f = np.clip(iron, 0.0, 1.0)
    a = np.clip(aridity, 0.0, 1.0)
    # Chroma: iron load, gamma-shaped by masking (humus) and tinting (aridity).
    chroma = f ** np.exp(_G_HUMUS * (h - 0.5) + _G_ARIDITY * (a - 0.5))
    b = _lerp(1.0 - h, *_D["b_range"])  # humus darkens
    r = _lerp(chroma, *_D["r_range"])
    # +transverse = toward yellow, so aridity runs from the yellow end down.
    v = np.interp(a, [0.0, 0.5, 1.0],
                  [_D["yel_range"][1], 0.0, _D["yel_range"][0]])
    return np.array([1.0, b, r, v]), _D["u_max"][0] * _warp(moisture)


def _logit_spectrum(humus, iron, aridity, moisture):
    """The model's logit reflectance on the native wavelength grid."""
    base, u = _coords(humus, iron, aridity, moisture)
    return base @ _D["BB"] + u * _D["M1"] + u * u * _D["M2"]


# ------------------------------------------------------------------------------
# Evaluation.
# ------------------------------------------------------------------------------
def evaluate(wavelengths, *, humus=0.5, iron=0.5, aridity=0.5, moisture=0.5):
    """Model soil reflectance at the given wavelengths (nm).

    All four parameters are in [0, 1] and are clamped to it; see the module
    docstring. The model is built on a 1nm grid spanning MIN_WAVELENGTH to
    MAX_WAVELENGTH and resampled onto the requested wavelengths; requests
    outside that range are held at the nearest endpoint. Returns an array
    shaped like wavelengths, with values strictly inside (0, 1).
    """
    y = _logit_spectrum(humus, iron, aridity, moisture)
    wvl = np.asarray(wavelengths, dtype=float)
    # Resample before the link, not after: linear interpolation of the logit
    # keeps the result strictly inside (0, 1) and tracks the model's curvature
    # a little better than interpolating reflectance directly.
    return 1.0 / (1.0 + np.exp(-np.interp(wvl, WAVELENGTHS, y)))


def srgb(*, humus=0.5, iron=0.5, aridity=0.5, moisture=0.5):
    """sRGB color of the modeled soil under D65: 3 gamma-encoded floats in
    [0, 1]."""
    R = evaluate(WAVELENGTHS, humus=humus, iron=iron, aridity=aridity,
                 moisture=moisture)
    c = np.clip(_XYZ_TO_SRGB @ (_XYZ_W @ R), 0.0, 1.0)
    return np.where(c <= 0.0031308, 12.92 * c, 1.055 * c ** (1 / 2.4) - 0.055)


def moisture_to_smc(moisture):
    """Convert the moisture parameter to gravimetric soil moisture content (%),
    using the population-mean saturation capacity. The same perceptual warp is
    applied, so moisture = 1 corresponds to the saturation capacity."""
    return _warp(moisture) * _D["smc_cap"][0]


# ------------------------------------------------------------------------------
# Fitting.
# ------------------------------------------------------------------------------
def fit(wavelengths, reflectance):
    """Estimate the pedogenic parameters of a measured reflectance spectrum.

    Fitting is a bounded least-squares solve in logit space, evaluated at the
    measurement's own wavelengths -- the measurement is never resampled. Samples
    outside MIN_WAVELENGTH..MAX_WAVELENGTH are dropped.

    Returns a dict of the four parameters plus "rmse", the reflectance-space
    root-mean-square residual of the fit over the samples used.
    """
    from scipy.optimize import least_squares

    wvl = np.asarray(wavelengths, dtype=float)
    R = np.asarray(reflectance, dtype=float)
    if wvl.shape != R.shape:
        raise ValueError(f"wavelengths {wvl.shape} and reflectance {R.shape} "
                         "must have the same shape")
    inside = (wvl >= MIN_WAVELENGTH) & (wvl <= MAX_WAVELENGTH)
    if inside.sum() < len(PARAMETERS):
        raise ValueError(f"need at least {len(PARAMETERS)} samples within "
                         f"{MIN_WAVELENGTH:.0f}-{MAX_WAVELENGTH:.0f}nm, "
                         f"got {int(inside.sum())}")
    wvl, R = wvl[inside], np.clip(R[inside], 1e-3, 1 - 1e-3)
    y_obs = np.log(R / (1 - R))

    # Interpolate the basis onto the measurement grid once, so each residual
    # evaluation is a single small matrix product.
    basis = np.stack([np.interp(wvl, WAVELENGTHS, c)
                      for c in (*_D["BB"], _D["M1"], _D["M2"])])

    def model(p):
        base, u = _coords(*p)
        return np.concatenate([base, [u, u * u]]) @ basis

    sol = least_squares(lambda p: model(p) - y_obs,
                        x0=np.full(len(PARAMETERS), 0.5), bounds=(0.0, 1.0))
    out = {k: float(v) for k, v in zip(PARAMETERS, sol.x)}
    Rfit = 1.0 / (1.0 + np.exp(-model(sol.x)))
    out["rmse"] = float(np.sqrt(np.mean((Rfit - R) ** 2)))
    return out


# ------------------------------------------------------------------------------
# Interactive explorer.
# ------------------------------------------------------------------------------
if __name__ == "__main__":
    from matplotlib import pyplot as plt
    from matplotlib.widgets import Slider

    INIT = dict.fromkeys(PARAMETERS, 0.5)

    fig = plt.figure(figsize=(11, 6.5))
    fig.canvas.manager.set_window_title("empirical soil model")

    ax = fig.add_axes([0.07, 0.30, 0.63, 0.63])
    ax.set_xlabel("Wavelength (nm)")
    ax.set_ylabel("Reflectance")
    ax.set_xlim([MIN_WAVELENGTH, MAX_WAVELENGTH])
    ax.set_ylim([0.0, 1.0])
    ax.grid(color="0.9", linewidth=0.5)
    ax.set_axisbelow(True)
    line, = ax.plot(WAVELENGTHS, evaluate(WAVELENGTHS, **INIT), color="C0",
                    linewidth=1.5)

    swatch = fig.add_axes([0.77, 0.52, 0.16, 0.41])
    swatch.set_xticks([])
    swatch.set_yticks([])
    swatch.set_title("sRGB (D65)")
    info = fig.text(0.85, 0.44, "", ha="center", va="center", fontsize=9,
                    family="monospace")

    sliders = []
    for k, name in enumerate(PARAMETERS):
        sax = fig.add_axes([0.17, 0.17 - 0.045 * k, 0.60, 0.026])
        sliders.append(Slider(sax, name, 0.0, 1.0, valinit=INIT[name]))

    def update(_=None):
        p = {name: s.val for name, s in zip(PARAMETERS, sliders)}
        line.set_ydata(evaluate(WAVELENGTHS, **p))
        rgb = srgb(**p)
        swatch.set_facecolor(tuple(rgb))
        hexcode = "".join(f"{int(round(255 * c)):02x}" for c in rgb)
        info.set_text(f"#{hexcode}\nSMC = {moisture_to_smc(p['moisture']):.1f}%")
        fig.canvas.draw_idle()

    for s in sliders:
        s.on_changed(update)
    update()
    plt.show()
