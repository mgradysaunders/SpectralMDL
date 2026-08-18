"""PROSPECT leaf optical properties model, 400-2500nm.

A Python port of `lib/Compiler/Builtin/models/prospect.smdl`, including its
analytic fits, so what this plots is what the renderer computes. The tabulated
optical constants are read out of the SMDL source at import time, which is
therefore the single source of truth for both.

The leaf is a stack of `num_layers` identical absorbing plates. Every
constituent enters through one absorption coefficient k = sum_j K_j(lambda) C_j,
so the model returns the hemispherical reflectance and transmittance of a single
leaf given its biochemistry per unit leaf area.

Unlike the pedogenic parameters of `empirical_soil.py`, these are physical
contents in physical units, so each one has its own range. Only
`xanthophyll_cycle` is normalized, and only `browns` is in arbitrary units:

  num_layers           1..4         leaf structure parameter N, need not be an
                                    integer; higher means more internal
                                    scattering, hence a brighter NIR plateau.
  incident_cone_angle  10..90 deg   half-angle of the illumination cone, in
                                    radians. The classic PROSPECT value is 40
                                    degrees (0.7 rad). Outside 10..90 degrees
                                    the model's polynomial fit is extrapolating.
  chlorophylls         0..100       Cab, micrograms per square centimeter.
  carotenoids          0..30        Car, micrograms per square centimeter.
  xanthophyll_cycle    0..1         de-epoxidation state: 0 is violaxanthin, 1
                                    is zeaxanthin. Reshapes the carotenoid pool
                                    over 500..564nm without resizing it.
  anthocyanins         0..40        Canth, micrograms per square centimeter.
  browns               0..1         Cbrown, arbitrary units (senescence).
  water                0..0.06      Cw, equivalent water thickness in cm.
  dry_matter           0..25        Cm, milligrams per square centimeter.
  proteins             0..6         Cp, milligrams per square centimeter.
  carbons              0..20        CBC, milligrams per square centimeter.

`proteins` and `carbons` are the PROSPECT-PRO split of dry matter: they are an
alternative to the lumped `dry_matter`, not an addition to it. Pass one or the
other, or the dry matter is counted twice.

Usage:
    import prospect
    wvl = prospect.WAVELENGTHS
    R, T = prospect.evaluate(wvl, chlorophylls=45.0, water=0.012)
    rgb_r, rgb_t = prospect.srgb(chlorophylls=45.0)
    params = prospect.fit(wvl_measured, R_measured, T_measured)

Run `python prospect.py` for an interactive explorer with sliders.
"""
import pathlib
import re
from collections import namedtuple

import numpy as np

__all__ = ["MIN_WAVELENGTH", "MAX_WAVELENGTH", "NUM_WAVELENGTHS",
           "WAVELENGTHS", "Parameter", "PARAMETERS", "DEFAULTS",
           "CONSTITUENTS", "FITTABLE", "ProspectResult", "evaluate", "fit",
           "to_xyz", "to_srgb", "srgb"]

SOURCE = (pathlib.Path(__file__).resolve().parents[1] /
          "lib/Compiler/Builtin/models/prospect.smdl")


# ------------------------------------------------------------------------------
# The tabulated optical constants, lifted straight out of the SMDL source so the
# two implementations cannot drift apart. The tables are machine-generated
# between `smdl format off`/`on` markers, so their layout is stable; all this
# needs is the array literal that follows a given name.
# ------------------------------------------------------------------------------
_NUMBER = re.compile(r"[-+]?(?:\d+\.\d*|\.\d+|\d+)(?:[eE][-+]?\d+)?")


def _read_source():
    try:
        return SOURCE.read_text(encoding="utf-8")
    except OSError as exc:
        raise RuntimeError(
            f"cannot read the PROSPECT tables from {SOURCE}: {exc}. This script "
            "reads them out of the SMDL source, so it must live in the smdl "
            "repository next to the lib/ tree.") from exc


_SRC = _read_source()


def _table(name, columns=1):
    """The array literal bound to `name` in the SMDL source, as an array shaped
    (rows,) for one column or (rows, columns) for several."""
    m = re.search(rf"\b{name}\s*=\s*(?:float|auto)\[(\d+)\]\((.*?)\);",
                  _SRC, re.DOTALL)
    if m is None:
        raise RuntimeError(f"{SOURCE.name} has no table named {name}")
    rows = int(m.group(1))
    values = np.array([float(v) for v in _NUMBER.findall(m.group(2))])
    if values.size != rows * columns:
        raise RuntimeError(f"{name} has {values.size} values, expected "
                           f"{rows * columns}")
    return values if columns == 1 else values.reshape(rows, columns)


def _constant(name):
    m = re.search(rf"\b{name}\s*=\s*([-+0-9.eE]+)\s*;", _SRC)
    if m is None:
        raise RuntimeError(f"{SOURCE.name} has no constant named {name}")
    return float(m.group(1))


MIN_WAVELENGTH = _constant("PROSPECT_MIN_WAVELENGTH")
MAX_WAVELENGTH = _constant("PROSPECT_MAX_WAVELENGTH")
NUM_WAVELENGTHS = 2101
WAVELENGTHS = np.linspace(MIN_WAVELENGTH, MAX_WAVELENGTH, NUM_WAVELENGTHS)

# The columns of the specific absorption table, in the order the SMDL packs
# them. `contents` below must match this order.
CONSTITUENTS = ("chlorophylls", "carotenoids", "anthocyanins", "browns",
                "water", "dry_matter", "proteins", "carbons")

_TABLE_IOR = _table("PROSPECT_TABLE_IOR")
_TABLE_K = _table("PROSPECT_TABLE_K", columns=len(CONSTITUENTS))
_CX_MIN_WAVELENGTH = _constant("PROSPECT_CX_MIN_WAVELENGTH")
_CX_MAX_WAVELENGTH = _constant("PROSPECT_CX_MAX_WAVELENGTH")
_CX_TABLE = _table("PROSPECT_CX_TABLE")


def _lerp_table(table, wavelengths, wmin, wmax):
    """Linear interpolation of a uniformly sampled table, mirroring
    `_uniform_lerp_index_and_fraction` in api.smdl: the position is clamped to
    the table, so wavelengths outside wmin..wmax hold the endpoint value."""
    t = (len(table) - 1) * np.clip((wavelengths - wmin) / (wmax - wmin),
                                   0.0, 1.0)
    i = np.minimum(np.floor(t).astype(int), len(table) - 2)
    f = t - i
    if table.ndim == 2:
        f = f[..., None]
    return table[i] + f * (table[i + 1] - table[i])


# ------------------------------------------------------------------------------
# Colorimetry: Wyman et al. (2013) fits to the CIE 1931 CMFs, D65 illuminant.
# The same block as in empirical_soil.py, kept local so this file stands alone.
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


def to_xyz(wavelengths, spectrum):
    """CIE 1931 XYZ tristimulus of a reflectance or transmittance spectrum under
    D65, normalized so a perfect diffuse reflector gives Y = 1. Samples outside
    the visible range contribute nothing."""
    return _xyz_weights(wavelengths) @ np.asarray(spectrum, dtype=float)


def to_srgb(wavelengths, spectrum):
    """sRGB color of a reflectance or transmittance spectrum under D65: 3
    gamma-encoded floats in [0, 1]. Out-of-gamut colors are clipped."""
    c = np.clip(_XYZ_TO_SRGB @ to_xyz(wavelengths, spectrum), 0.0, 1.0)
    return np.where(c <= 0.0031308, 12.92 * c, 1.055 * c ** (1 / 2.4) - 0.055)


# The visible-band integration weights on the native grid, precomputed for the
# common case of coloring a model spectrum.
_XYZ_W = _xyz_weights(WAVELENGTHS)


# ------------------------------------------------------------------------------
# Parameters. Every one carries its own unit and plausible range, since only
# xanthophyll_cycle is normalized.
# ------------------------------------------------------------------------------
Parameter = namedtuple("Parameter", "name unit lo hi default")

PARAMETERS = (
    Parameter("num_layers", "", 1.0, 4.0, 1.5),
    Parameter("incident_cone_angle", "rad", np.pi / 18, np.pi / 2, 0.7),
    Parameter("chlorophylls", "ug/cm2", 0.0, 100.0, 30.0),
    Parameter("carotenoids", "ug/cm2", 0.0, 30.0, 1.5),
    Parameter("xanthophyll_cycle", "", 0.0, 1.0, 0.0),
    Parameter("anthocyanins", "ug/cm2", 0.0, 40.0, 1.0),
    Parameter("browns", "a.u.", 0.0, 1.0, 0.0),
    Parameter("water", "cm", 0.0, 0.06, 0.01),
    Parameter("dry_matter", "mg/cm2", 0.0, 25.0, 5.0),
    Parameter("proteins", "mg/cm2", 0.0, 6.0, 0.0),
    Parameter("carbons", "mg/cm2", 0.0, 20.0, 0.0),
)

DEFAULTS = {p.name: p.default for p in PARAMETERS}
_BY_NAME = {p.name: p for p in PARAMETERS}


# ------------------------------------------------------------------------------
# Evaluation. This follows prospect.smdl line for line, analytic fits included,
# so that the curves here are the ones the renderer sees.
# ------------------------------------------------------------------------------
ProspectResult = namedtuple("ProspectResult", "reflectance transmittance")


def evaluate(wavelengths, **params):
    """Model the hemispherical reflectance and transmittance of a single leaf at
    the given wavelengths (nm).

    Keyword parameters are those of PARAMETERS; see the module docstring for
    units and ranges. Wavelengths outside MIN_WAVELENGTH..MAX_WAVELENGTH hold
    the nearest tabulated endpoint. Returns a ProspectResult of two arrays
    shaped like wavelengths.
    """
    unknown = set(params) - set(DEFAULTS)
    if unknown:
        raise TypeError(f"unknown parameters {sorted(unknown)}")
    p = {**DEFAULTS, **params}
    wvl = np.asarray(wavelengths, dtype=float)

    # The plate model stacks num_layers copies of one absorbing layer, so the
    # whole-leaf contents are spread over the layers.
    num_layers = max(float(p["num_layers"]), 1.0)
    contents = np.array([
        p["chlorophylls"], p["carotenoids"], p["anthocyanins"], p["browns"],
        p["water"], 1e-3 * p["dry_matter"], 1e-3 * p["proteins"],
        1e-3 * p["carbons"]]) / num_layers
    # How much violaxanthin to relax back out of the tabulated zeaxanthin.
    xanthophylls = (p["carotenoids"] *
                    (1.0 - np.clip(p["xanthophyll_cycle"], 0.0, 1.5)) /
                    num_layers)

    ior = _lerp_table(_TABLE_IOR, wvl, MIN_WAVELENGTH, MAX_WAVELENGTH)
    k = (_lerp_table(_TABLE_K, wvl, MIN_WAVELENGTH, MAX_WAVELENGTH) @ contents
         - xanthophylls * _lerp_table(_CX_TABLE, wvl, _CX_MIN_WAVELENGTH,
                                      _CX_MAX_WAVELENGTH))

    # tau = (1 - k) exp(-k) + k^2 E1(k), the layer transmittance, via the
    # rational fit the SMDL uses in place of the exponential integral.
    num = (1.236150246012 * k + 3.672877420834) * k + 1.0
    den = ((0.618075123006 * k + 3.664716300259) * k + 4.621903634050) * k + 1.0
    tau = np.clip(np.exp(-k) * num / den, 0.0, 0.999)

    # Average transmittance across the air/leaf interface for diffuse (90
    # degree) incidence, as a fifth-order polynomial in the refractive index.
    t12 = (((((-0.17369388 * ior + 1.31899730) * ior - 4.02936997) * ior
             + 6.21265658) * ior - 4.99648418) * ior + 2.66515836)
    t12 = np.clip(t12, 0.0, 1.0)
    r12 = 1 - t12
    t21 = t12 / (ior * ior)
    r21 = 1 - t21

    # ... and the same for the incident cone, whose coefficients are themselves
    # a polynomial in the cone half-angle.
    cone = float(p["incident_cone_angle"])
    c = np.array([+5.9796905e-01, -1.9041080e+00, +1.6576156e+00])
    for row in ((-4.1001221e+00, +1.2956352e+01, -1.1049849e+01),
                (+1.1477769e+01, -3.6044872e+01, +3.0242981e+01),
                (-1.7172335e+01, +5.3666636e+01, -4.4411331e+01),
                (+1.5069425e+01, -4.6911094e+01, +3.8289770e+01),
                (-7.8923812e+00, +2.4474973e+01, -1.9667279e+01),
                (+2.4020134e+00, -7.4210148e+00, +5.8397553e+00),
                (-3.8620638e-01, +1.1877490e+00, -9.0387653e-01),
                (-4.8754145e-02, +1.6941738e-02, +1.0405082e+00)):
        c = c * cone + np.array(row)
    t_alpha = np.clip(c[0] * ior * ior + c[1] * ior + c[2], 0.0, 1.0)
    r_alpha = 1 - t_alpha

    # One layer, summing the interreflections between its two interfaces.
    with np.errstate(divide="ignore", invalid="ignore", over="ignore"):
        tau_r21 = tau * r21
        tmp0 = tau * t21 / (1 - tau_r21 ** 2)
        tA = t_alpha * tmp0
        rA = r_alpha + tau_r21 * tA
        t = t12 * tmp0
        r = r12 + tau_r21 * t

        # Stokes' closed form for the remaining num_layers - 1 layers.
        add_r_t, sub_r_t = r + t, r - t
        sub_r2_t2 = r * r - t * t
        d = np.sqrt(np.maximum((1 + add_r_t) * (1 + sub_r_t) *
                               (1 - add_r_t) * (1 - sub_r_t), 0.0))
        a = (1 + d + sub_r2_t2) / (2 * r)
        b = (1 + d - sub_r2_t2) / (2 * t)
        b_nm1 = b ** (num_layers - 1)
        tmp1 = (a * b_nm1) ** 2 - 1
        t_sub = b_nm1 * (a * a - 1) / tmp1
        r_sub = a * (b_nm1 * b_nm1 - 1) / tmp1

        # Degenerate cases: a non-absorbing stack (r + t > 1, where the closed
        # form's discriminant collapses) reduces to the transparent-plate limit,
        # and anything left non-finite is treated as a perfect reflector.
        clear = add_r_t > 1
        t_clear = t / (t + (1 - t) * (num_layers - 1))
        t_sub = np.where(clear, t_clear, t_sub)
        r_sub = np.where(clear, 1 - t_clear, r_sub)
        bad = ~clear & ~(np.isfinite(r_sub) & np.isfinite(t_sub))
        t_sub = np.where(bad, 0.0, t_sub)
        r_sub = np.where(bad, 1.0, r_sub)

        one_minus_r_sub_r = 1 - r_sub * r
        return ProspectResult(
            reflectance=rA + tA * r_sub * t / one_minus_r_sub_r,
            transmittance=tA * t_sub / one_minus_r_sub_r)


def srgb(**params):
    """sRGB colors of the modeled leaf under D65, as a ProspectResult of two
    triples of gamma-encoded floats in [0, 1]: the color seen in reflection, and
    the color seen with the leaf held up to the light."""
    spectra = evaluate(WAVELENGTHS, **params)

    def encode(spectrum):
        c = np.clip(_XYZ_TO_SRGB @ (_XYZ_W @ spectrum), 0.0, 1.0)
        return np.where(c <= 0.0031308, 12.92 * c,
                        1.055 * c ** (1 / 2.4) - 0.055)

    return ProspectResult(*(encode(s) for s in spectra))


# ------------------------------------------------------------------------------
# Fitting.
# ------------------------------------------------------------------------------
# Cone angle is an instrument property rather than a leaf property, and the
# PROSPECT-PRO split is degenerate with the lumped dry matter, so none of the
# three is inverted unless asked for by name.
FITTABLE = ("num_layers", "chlorophylls", "carotenoids", "anthocyanins",
            "browns", "water", "dry_matter")


def fit(wavelengths, reflectance, transmittance=None, free=FITTABLE, **fixed):
    """Estimate leaf biochemistry from a measured spectrum.

    Fitting is a bounded least-squares solve against reflectance, and against
    transmittance too when it is given -- which is what actually pins down the
    structure parameter, so pass it if you have it. Parameters named in `free`
    are inverted within their PARAMETERS bounds; everything else is held at its
    default unless overridden by a keyword.

    Returns a dict of the fitted parameters plus "rmse", the residual over the
    samples used.
    """
    from scipy.optimize import least_squares

    unknown = set(free) - set(DEFAULTS)
    if unknown:
        raise ValueError(f"unknown parameters {sorted(unknown)}")
    free = list(free)
    if not free:
        raise ValueError("nothing to fit")

    wvl = np.asarray(wavelengths, dtype=float)
    obs = [np.asarray(reflectance, dtype=float)]
    if transmittance is not None:
        obs.append(np.asarray(transmittance, dtype=float))
    for o in obs:
        if o.shape != wvl.shape:
            raise ValueError(f"wavelengths {wvl.shape} and spectra {o.shape} "
                             "must have the same shape")
    inside = (wvl >= MIN_WAVELENGTH) & (wvl <= MAX_WAVELENGTH)
    if inside.sum() < len(free):
        raise ValueError(f"need at least {len(free)} samples within "
                         f"{MIN_WAVELENGTH:.0f}-{MAX_WAVELENGTH:.0f}nm, "
                         f"got {int(inside.sum())}")
    wvl = wvl[inside]
    y_obs = np.concatenate([o[inside] for o in obs])

    # Solve in normalized coordinates: the contents span five orders of
    # magnitude in their natural units, which no single step size suits.
    lo = np.array([_BY_NAME[n].lo for n in free])
    hi = np.array([_BY_NAME[n].hi for n in free])
    x0 = np.array([np.clip(fixed.get(n, DEFAULTS[n]), l, h)
                   for n, l, h in zip(free, lo, hi)])

    def model(x):
        p = {**DEFAULTS, **fixed, **dict(zip(free, lo + x * (hi - lo)))}
        out = evaluate(wvl, **p)
        return np.concatenate(out[:len(obs)])

    sol = least_squares(lambda x: model(x) - y_obs,
                        x0=(x0 - lo) / (hi - lo), bounds=(0.0, 1.0))
    values = lo + sol.x * (hi - lo)
    out = {**DEFAULTS, **fixed, **{n: float(v) for n, v in zip(free, values)}}
    out["rmse"] = float(np.sqrt(np.mean((model(sol.x) - y_obs) ** 2)))
    return out


# ------------------------------------------------------------------------------
# Interactive explorer.
# ------------------------------------------------------------------------------
if __name__ == "__main__":
    from matplotlib import pyplot as plt
    from matplotlib.widgets import RadioButtons, Slider

    # The lumped dry matter and the PROSPECT-PRO split are alternatives, so the
    # explorer offers them as modes and zeroes whichever set is not in use --
    # otherwise it is far too easy to count the dry matter twice. The PRO
    # sliders start at roughly the same total as the lumped default.
    MODES = {"lumped dry matter": ("dry_matter",),
             "proteins + carbons": ("proteins", "carbons")}
    INIT = {**DEFAULTS, "proteins": 1.0, "carbons": 4.0}
    PAR = (WAVELENGTHS >= 400.0) & (WAVELENGTHS <= 700.0)

    fig = plt.figure(figsize=(11.5, 8.0))
    fig.canvas.manager.set_window_title("PROSPECT leaf model")

    ax = fig.add_axes([0.075, 0.45, 0.60, 0.46])
    ax.set_xlabel("Wavelength (nm)")
    ax.set_ylabel("Reflectance")
    ax.set_xlim([MIN_WAVELENGTH, MAX_WAVELENGTH])
    ax.set_ylim([0.0, 1.0])
    ax.grid(color="0.9", linewidth=0.5)
    ax.set_axisbelow(True)
    # The literature convention: reflectance up from 0, transmittance down from
    # 1, so the gap between the curves is the absorptance.
    mirror = ax.secondary_yaxis("right", functions=(lambda y: 1 - y,
                                                    lambda y: 1 - y))
    mirror.set_ylabel("Transmittance")

    spectra = evaluate(WAVELENGTHS, **{k: INIT[k] for k in DEFAULTS
                                       if k not in ("proteins", "carbons")})
    r_line, = ax.plot(WAVELENGTHS, spectra.reflectance, color="C0",
                      linewidth=1.5, label="reflectance")
    t_line, = ax.plot(WAVELENGTHS, 1 - spectra.transmittance, color="C1",
                      linewidth=1.5, label="transmittance")
    absorbed = ax.fill_between(WAVELENGTHS, spectra.reflectance,
                               1 - spectra.transmittance, color="0.88",
                               linewidth=0, label="absorptance")
    # Above the axes rather than inside them: the curves sweep through every
    # corner of the plot as the sliders move, so there is no safe inside corner.
    ax.legend(loc="lower center", bbox_to_anchor=(0.5, 1.005), ncol=3,
              frameon=False, fontsize=9)

    swatches = []
    for k, title in enumerate(("reflected", "transmitted")):
        sw = fig.add_axes([0.735 + 0.130 * k, 0.69, 0.115, 0.20])
        sw.set_xticks([])
        sw.set_yticks([])
        sw.set_title(title, fontsize=9)
        swatches.append(sw)
    fig.text(0.855, 0.945, "sRGB (D65)", ha="center", fontsize=10)
    info = fig.text(0.855, 0.595, "", ha="center", va="center", fontsize=9,
                    family="monospace")

    sliders = {}
    for k, p in enumerate(PARAMETERS):
        sax = fig.add_axes([0.20, 0.375 - 0.032 * k, 0.45, 0.018])
        step = "%.3f" if p.hi <= 1.0 else "%.2f" if p.hi <= 5.0 else "%.1f"
        sliders[p.name] = Slider(sax, p.name.replace("_", " "), p.lo, p.hi,
                                 valinit=INIT[p.name],
                                 valfmt=f"{step} {p.unit}".rstrip())

    fig.text(0.755, 0.375, "dry matter", fontsize=10)
    radio = RadioButtons(fig.add_axes([0.755, 0.245, 0.21, 0.115]),
                         tuple(MODES), active=0)

    def set_active(slider, on):
        slider.set_active(on)
        slider.label.set_color("black" if on else "0.6")
        slider.valtext.set_color("black" if on else "0.6")
        slider.poly.set_alpha(1.0 if on else 0.25)

    def update(_=None):
        global absorbed
        inactive = set().union(*(v for k, v in MODES.items()
                                 if k != radio.value_selected))
        for name, s in sliders.items():
            set_active(s, name not in inactive)
        p = {name: 0.0 if name in inactive else s.val
             for name, s in sliders.items()}

        R, T = evaluate(WAVELENGTHS, **p)
        r_line.set_ydata(R)
        t_line.set_ydata(1 - T)
        absorbed.remove()
        absorbed = ax.fill_between(WAVELENGTHS, R, 1 - T, color="0.88",
                                   linewidth=0)

        hexes = []
        for sw, rgb in zip(swatches, srgb(**p)):
            sw.set_facecolor(tuple(rgb))
            hexes.append("".join(f"{int(round(255 * c)):02x}" for c in rgb))
        par = float(np.mean(1 - R[PAR] - T[PAR]))
        info.set_text(f"R  #{hexes[0]}\n"
                      f"T  #{hexes[1]}\n\n"
                      f"PAR absorptance {par:.3f}\n"
                      f"cone angle {np.degrees(p['incident_cone_angle']):.0f}"
                      "\N{DEGREE SIGN}")
        fig.canvas.draw_idle()

    for s in sliders.values():
        s.on_changed(update)
    radio.on_clicked(update)
    update()
    plt.show()
