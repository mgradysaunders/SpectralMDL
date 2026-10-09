// Include this inside an anonymous namespace: the two golden tables in
// this directory name some of the same constants, and internal linkage
// is what keeps them apart.
// Golden data for the haze extinction spectrum, generated from the
// same extraction as HazeRural.h by gen_haze_table.py in
// Empirical-Atm; regenerate rather than editing.
constexpr std::size_t GOLDEN_WAVELENGTH_COUNT = 10;
constexpr float GOLDEN_WAVELENGTHS[GOLDEN_WAVELENGTH_COUNT] = {
    370.0f, 382.5f,  412.5f,  550.0f,  683.0f,
    940.0f, 1377.0f, 1998.5f, 2500.0f, 2600.0f};

constexpr std::size_t GOLDEN_CASE_COUNT = 3;
constexpr float GOLDEN_VISIBILITY[GOLDEN_CASE_COUNT] = {5.0f, 23.0f, 100.0f};

// clang-format off
// Extinction at the base height in inverse meters, one scene unit to the
// meter
constexpr float GOLDEN_EXTINCTION[GOLDEN_CASE_COUNT][GOLDEN_WAVELENGTH_COUNT] = {
    {0.00114555669f, 0.00113750377f, 0.00104917784f, 0.00078240002f, 0.00060057838f,
     0.000389424938f, 0.000225583994f, 0.000122583428f, 9.94121074e-05f, 9.94121074e-05f},
    {0.000276872073f, 0.000274134654f, 0.000245249685f, 0.000170086962f, 0.00012740423f,
     8.12342769e-05f, 4.66895654e-05f, 2.53018134e-05f, 2.04988573e-05f, 2.04988573e-05f},
    {9.10700765e-05f, 8.94696132e-05f, 7.32983826e-05f, 3.9120001e-05f, 2.6197542e-05f,
     1.5315718e-05f, 8.42603549e-06f, 4.49435765e-06f, 3.62019e-06f, 3.62019e-06f}
};

// Single-scattering albedo, which the height does not change
constexpr float GOLDEN_ALBEDO[GOLDEN_CASE_COUNT][GOLDEN_WAVELENGTH_COUNT] = {
    {0.959247053f, 0.959144175f, 0.958028436f, 0.954611421f, 0.947856247f,
     0.905463576f, 0.8523646f, 0.896257937f, 0.862132251f, 0.862132251f},
    {0.965268373f, 0.965080202f, 0.963015139f, 0.956993639f, 0.949369013f,
     0.906650543f, 0.853071094f, 0.896470666f, 0.862278879f, 0.862278879f},
    {0.981468678f, 0.98122251f, 0.978282213f, 0.967184246f, 0.956786692f,
     0.913105607f, 0.85711664f, 0.897711873f, 0.863139987f, 0.863139987f}
};
// clang-format on
