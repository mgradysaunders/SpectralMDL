# Sensors

Camera bodies in the renderer's `.sensor` format, for `smdl-toy` to land
a picture on: the pixel array and its pitch, the measured spectral
sensitivity of each band and the Bayer tile that lays them over the
pixels, and the detector that turns electrons into digital numbers. A
camera file names one beside itself, or the command line does:

```
camera {
  sensor "../../etc/sensors/sony-ilce-7m3.sensor"
  shutter 0.004  fstop 8
}
```

```
smdl-toy shot.layout -sensor etc/sensors/sony-ilce-7m3.sensor \
    -shutter 0.004 -fstop 8 -output-spectrum out.img -output-dn out-dn.img
```

A body decides the picture: the render is exactly its pixels (leave
`-resolution` out, or use `-crop-window` for part of the frame), the
spectral film holds the irradiance at the sensor rather than the scene
radiance, the wavelength grid spans the body's curves with the jitter on
unless the flags say otherwise, and the band film beside the spectral
one (`out-bands.img`) holds the mosaic the tile reads, in photoelectrons
per square meter and second. `-output-dn` reads it out through the
detector as a 16-bit ENVI pair of digital numbers, with every factor in
its header. `-describe-camera` prints what a camera resolves to before
anything renders.

Every curve here runs from 380 to 780 nm at 5 nm.

## What is measured and what is not

The curves are Weta Digital's measurements, relative and
peak-normalized, and carry no absolute scale. Each file states
`peak_qe 0.5`, a generic peak quantum efficiency the renderer scales the
set to, keeping the ratios the measurement carries; a readout is no
better than that guess. The pixel count, the pitch, the bit depth, and
the rated base ISO are the makers' published figures. The rest of the
detector (the read noise, the dark current, the black level) is the
format's generic default, which the files do not restate.

The well is derived from the rated base ISO: the exposure the well
fills at is the saturation exposure ISO 12232 gives that speed, `78 /
base_iso` lux-seconds of D55, counted through the body's most sensitive
band at the stated peak. The full-frame bodies land in the tens of
thousands of electrons (the a7 III at 52,000), which is the right order
against the measured wells, and no better than the peak they rest on.
The log and `-describe-camera` state every derived number as derived.

An unstated ISO is metered from the rendered film as a reflected-light
meter would set it, never below the base, and the log says what the
meter asked for and, when the frame ran past the base or the top, the
shutter or the stop that would bring it back; `iso` in the camera file
or `-iso` states one instead. The gain follows the ISO from the same
line: at the base it fills the well to the top code, and above the base
the ADC clips before the well does.

The Hasselblad L1D-20c is stated at 12 bits; its own raw files are
16-bit DNG containers.

The rolling readout times measured by Horshack
(https://horshack-dpreview.github.io/RollingShutter/) are not stated
in the files, since they are one enthusiast's measurements rather than
the makers' figures; for reference, at their electronic shutters the
a6400 reads out in about 46 ms, the a7 III in 62 ms, the a7R III in 70
ms, the a9 in 6.6 ms, and the R5 in 16 ms. State one with `readout` in
a copy of the file, or with `-readout`.

## Reading a sensor out

`-output-dn` needs an exposure (`shutter`) and a pupil (`fstop` or
`aperture` with the thin lens, or a `.lens`). A saved film reads out
again with `-spp 0 -resume out.img -output-dn ...`, as many times as
there are realizations to draw, and at any ISO; `-detector-seed` picks
the realization and `-detector-noise none|shot|all` isolates a term.
The readout's header carries the ISO, the base ISO, whether the ISO was
metered, and the white level beside the gain and the black level.

The noise model assumes a converged film. It takes the band film's mean
as the exact signal and draws the shot noise on it in full, so the
render's own noise adds to the sensor's rather than standing in for any
of it: render until the film's error is well under the shot noise, which
is 1% at 10,000 electrons, before reading it out. A firefly or an
unconverged caustic reads out as signal.

## Developing a sensor

A camera that names one of these bodies writes a developed picture to
`-output-rgb` with no flag asked for: the readout, noise and all, goes
through the steps a raw developer takes. The digital numbers over the
black level, balanced to the camera's `white_balance` (`D65` unless
stated, so that the develop agrees with the `-sensor human` preview;
`daylight`, `cloudy`, `shade`, `tungsten`, `fluorescent`, a temperature
in kelvin, or `auto` for the frame's gray world), held where a saturated
white would lose its color, demosaicked (Hamilton-Adams on these RGGB
tiles), and mapped to linear sRGB by a matrix fitted to the body's own
curves. A metered neutral develops to 0.18, middle gray. `-output-rgbf`
holds the same linear picture, and the tone map and `-exposure` apply to
it as they do to the observer's.

The matrix is a white-preserving least-squares fit over the 190
training reflectances of rawtoaces-data
(https://github.com/AcademySoftwareFoundation/rawtoaces-data,
Apache-2.0), under the white balance's illuminant, against the CIE
observer the renderer's own RGB conversion uses. `-describe-camera`
reports each body's fit. Under D65 these bodies fit to a mean of 1.2 to
2.0 CIEDE2000 and an index, `100 - 5.5` times the mean CIE 1976
difference, of 80 (the 5D) to 87 (the 200D): a training error over this
set, which ISO 17321 computes over the ColorChecker instead, so it is
optimistic and not comparable to a published one. A body whose bands
fit the observer to worse than 10 CIEDE2000 develops as false color,
each band on its own channel, and one with fewer than three bands as
gray; both say so.

## Source

Winquist and Thurston, "Physlight - Camera Spectral Sensitivity
Curves" (Weta Digital, 2022), https://doi.org/10.5281/zenodo.6590768,
measured with Weta's "lightsaber" system: 17 bodies, relative and
peak-normalized, pinned to the Zenodo v1.0.0 record. Converted by
`etc/scripts/sensor_convert.py` from the record's JSON files, which the
header comment of each file names, with the body facts given as flags.
The data set is Apache License 2.0, a copy of which is `LICENSE` beside
this file.

A published curve is a starting point rather than a calibration: the one
validation of a rendered sensor against a real phone in the literature
had to transform the sensor's published curves to match it. Treat a
band's absolute scale with suspicion and its shape with less.
