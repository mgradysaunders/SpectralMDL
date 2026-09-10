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
