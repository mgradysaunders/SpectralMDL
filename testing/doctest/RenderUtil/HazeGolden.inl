// Golden data for the haze extinction spectrum, generated from the
// same extraction as HazeRural.h by gen_haze_table.py in
// Empirical-Atm; regenerate rather than editing.
constexpr std::size_t GOLDEN_WAVELENGTH_COUNT = 9;
constexpr float GOLDEN_WAVELENGTHS[GOLDEN_WAVELENGTH_COUNT] = {
    380.0f, 412.5f, 550.0f, 683.0f, 940.0f, 1377.0f, 1998.5f, 2500.0f, 2600.0f};

constexpr std::size_t GOLDEN_CASE_COUNT = 3;
constexpr float GOLDEN_VISIBILITY[GOLDEN_CASE_COUNT] = {5.0f, 23.0f, 100.0f};

// clang-format off
// Extinction at the base height in inverse meters, one scene unit to the
// meter
constexpr float GOLDEN_EXTINCTION[GOLDEN_CASE_COUNT][GOLDEN_WAVELENGTH_COUNT] = {
    {0.00108392083f, 0.00104913523f, 0.00078240002f, 0.000600579719f, 0.000389424706f,
     0.000225583382f, 0.000122583064f, 9.94118745e-05f, 9.94118745e-05f},
    {0.000256302883f, 0.000245207077f, 0.000170086962f, 0.000127405598f, 8.12340368e-05f,
     4.66889542e-05f, 2.53014459e-05f, 2.04986281e-05f, 2.04986281e-05f},
    {7.9284604e-05f, 7.32557819e-05f, 3.9120001e-05f, 2.61989026e-05f, 1.53154779e-05f,
     8.42542249e-06f, 4.49399113e-06f, 3.61996035e-06f, 3.61996035e-06f}
};

// Single-scattering albedo, which the height does not change
constexpr float GOLDEN_ALBEDO[GOLDEN_CASE_COUNT][GOLDEN_WAVELENGTH_COUNT] = {
    {0.958486319f, 0.958026707f, 0.954611421f, 0.947856367f, 0.905463517f,
     0.852364182f, 0.896257579f, 0.862131953f, 0.862131953f},
    {0.963837087f, 0.963008702f, 0.956993639f, 0.94936955f, 0.906650245f,
     0.853069127f, 0.896469176f, 0.862277329f, 0.862277329f},
    {0.979483366f, 0.978269577f, 0.967184246f, 0.956788957f, 0.913104296f,
     0.857106268f, 0.897703528f, 0.863131285f, 0.863131285f}
};
// clang-format on
