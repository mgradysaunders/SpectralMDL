# Sensors

Measured spectral sensitivities of camera bodies, in the renderer's
`.response` format, for `smdl-toy` to read a scene through. A camera
file names one beside itself, or the command line does:

```
camera {
  response "../../etc/sensors/canon-eos-5d-mark-ii.response"
}
```

```
smdl-toy shot.layout -response etc/sensors/canon-eos-5d-mark-ii.response \
    -wavelength-range 380,780 -output-spectrum out.img
```

The render then writes a second ENVI pair beside the spectral one,
`out-bands.img`, holding each band's integral of the radiance against
its curve, with the bands named in the header. Add
`cfa { row R G  row G B }` to a copy of the file to get the Bayer mosaic
a real sensor reads instead.

Every curve here runs from 380 to 780 nm at 5 nm, so a render wants
`-wavelength-range 380,780`; the default grid stops at 720 nm and the
render says so.

## Reading a sensor out

A readout counts electrons, which a relative curve cannot do: it needs
a `qe` response, the curve in electrons per photon. The measurement
carries no absolute scale, so the converter takes the peak quantum
efficiency as a stated guess and writes the same curves scaled to it:

```
etc/scripts/response_convert.py canon-eos-5d-mark-ii.json --peak-qe 0.5 \
    -o canon-eos-5d-mark-ii-qe.response
```

The result is no better than that peak. With it, a camera file that
states the exposure, the f-number, the sensor size, and what it reads
out with, and a command line that asks for the readout, get a 16-bit
ENVI pair of the digital numbers the instrument would write, with every
factor in its header:

```
camera {
  response "canon-eos-5d-mark-ii-qe.response"
  shutter 0.001  fstop 8  sensor 36 24
  detector { full_well 60000 read_noise 3 bits 14 }
}
```

```
smdl-toy shot.layout -resolution 1800,1200 -output-spectrum out.img \
    -output-dn out-dn.img
```

Every field of `detector` has a generic default and the block may be
left out; `-detector-seed` picks the noise realization and
`-detector-noise none|shot|all` isolates a term. A saved film reads out
again with `-spp 0 -resume out.img -output-dn ...`, as many times as
there are realizations to draw.

The noise model assumes a converged film. It takes the band film's mean
as the exact signal and draws the shot noise on it in full, so the
render's own noise adds to the sensor's rather than standing in for any
of it: render until the film's error is well under the shot noise, which
is 1% at 10,000 electrons, before reading it out. A firefly or an
unconverged caustic reads out as signal.

## Source

Winquist and Thurston, "Physlight - Camera Spectral Sensitivity
Curves" (Weta Digital, 2022), https://doi.org/10.5281/zenodo.6590768,
measured with Weta's "lightsaber" system: 17 bodies, relative and
peak-normalized. Converted by `etc/scripts/response_convert.py` from the
record's JSON files, which the header comment of each file names. The
data set is Apache License 2.0, a copy of which is `LICENSE` beside this
file.

A published curve is a starting point rather than a calibration: the one
validation of a rendered sensor against a real phone in the literature
had to transform the sensor's published curves to match it. Treat a
band's absolute scale with suspicion and its shape with less.
