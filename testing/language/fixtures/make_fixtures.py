#!/usr/bin/env python3
"""Write the height-field fixtures the relief tests march through.

Both are 8-bit gray PNGs, so every height is a multiple of 1/255 and the
tests can state their expectations in closed form:

    ridge_16.png   16x16 at 51 (height 0.2) with the column x = 8 at 255
                   (height 1.0): a one-texel wall a layer march can step
                   over, repeating every 16 texels under wrap_repeat.
    bumps_32.png   32x32 of deterministic pseudo-random heights in
                   [20, 235]/255, for the first-crossing property test.

Run from this directory; PIL is the only dependency.
"""
from PIL import Image


def write_gray(name, width, height, value_at):
    image = Image.new("L", (width, height))
    image.putdata([value_at(x, y) for y in range(height) for x in range(width)])
    image.save(name, optimize=True)


def main():
    write_gray("ridge_16.png", 16, 16, lambda x, y: 255 if x == 8 else 51)
    # A small linear congruential generator, so the field never depends on
    # the Python version's random module.
    state = [12345]

    def next_value():
        state[0] = (1103515245 * state[0] + 12345) % (1 << 31)
        return 20 + (state[0] >> 16) % 216

    write_gray("bumps_32.png", 32, 32, lambda x, y: next_value())


if __name__ == "__main__":
    main()
