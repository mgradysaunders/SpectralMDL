#include "doctest.h"

#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>

#include "smdl/RenderUtil/MetalIOR.h"

namespace {
smdl::float2 evalMetalIOR(smdl::Metal metal, float wavelen) {
  float n{}, k{};
  smdl::smdlEvalMetalIOR(metal, 1, &wavelen, &n, &k);
  return {n, k};
}

// The normal-incidence Fresnel reflectance.
float reflectance(smdl::float2 ior) {
  const float n{ior[0]}, k{ior[1]};
  return ((n - 1) * (n - 1) + k * k) / ((n + 1) * (n + 1) + k * k);
}

// The straightforward search this module used before it gained the bucket
// index, kept here so the fast path can be held to returning the same bits.
void searchEvalMetalIOR(smdl::Metal metal, int numWavelens,
                        const float *wavelens, float *iorN, float *iorK) {
  smdl::MetalIOR metalIOR{};
  if (!smdl::smdlFindMetalIOR(metal, &metalIOR)) return;
  const smdl::MetalIORTableEntry *tableBegin{metalIOR.table};
  const smdl::MetalIORTableEntry *tableEnd{metalIOR.table + metalIOR.tableSize};
  for (int i = 0; i < numWavelens; i++) {
    const float wavelen{wavelens[i]};
    const smdl::MetalIORTableEntry *itr{std::lower_bound(
        tableBegin, tableEnd, wavelen,
        [](const smdl::MetalIORTableEntry &entry, float wavelen) {
          return entry.wavelen < wavelen;
        })};
    if (itr != tableBegin) --itr;
    if (itr == tableEnd - 1) --itr;
    float t{(wavelen - itr[0].wavelen) / (itr[1].wavelen - itr[0].wavelen)};
    t = std::clamp(t, 0.0f, 1.0f);
    const smdl::float2 ior{(1 - t) * itr[0].ior + t * itr[1].ior};
    iorN[i] = ior[0], iorK[i] = ior[1];
  }
}

// Bit-identical, and NaN compares equal to NaN so the out-of-domain results
// are held to the same bits as everything else.
bool hasSameBits(float a, float b) {
  return (std::isnan(a) && std::isnan(b)) || a == b;
}

// Every wavelength worth asking about: each table entry and its two nearest
// neighbors, every interval midpoint, a sweep across and well past the bucket
// domain, and the exceptional values.
std::vector<float> probeWavelengths(const smdl::MetalIOR &metalIOR) {
  std::vector<float> wavelens{};
  for (int i = 0; i < metalIOR.tableSize; i++) {
    const float wavelen{metalIOR.table[i].wavelen};
    wavelens.push_back(wavelen);
    wavelens.push_back(std::nextafter(wavelen, 0.0f));
    wavelens.push_back(std::nextafter(wavelen, 1e30f));
    if (i + 1 < metalIOR.tableSize)
      wavelens.push_back(0.5f * (wavelen + metalIOR.table[i + 1].wavelen));
  }
  for (double wavelen = 1.0; wavelen < 20000.0; wavelen *= 1.0005)
    wavelens.push_back(float(wavelen));
  const float inf{std::numeric_limits<float>::infinity()};
  for (float wavelen : {0.0f, -1.0f, -1e30f, 299.0f, 299.999f, 300.0f, 300.001f,
                        13999.9f, 14000.0f, 14000.1f, 1e6f, inf, -inf,
                        std::numeric_limits<float>::quiet_NaN()})
    wavelens.push_back(wavelen);
  std::sort(wavelens.begin(), wavelens.end());
  return wavelens;
}
} // namespace

TEST_CASE("MetalIOR") {
  SUBCASE("smdlFindMetalIOR") {
    // Every valid metal must have a table that respects the documented
    // invariants: at least two entries, wavelengths positive, at most
    // 14000nm, and strictly increasing, with n > 0 and k >= 0.
    for (int i = int(smdl::Metal::First); i <= int(smdl::Metal::Last); i++) {
      smdl::MetalIOR metalIOR{};
      REQUIRE(smdl::smdlFindMetalIOR(smdl::Metal(i), &metalIOR) == 1);
      REQUIRE(metalIOR.table != nullptr);
      REQUIRE(metalIOR.tableSize >= 2);
      int violations{};
      for (int j = 0; j < metalIOR.tableSize; j++) {
        const auto &entry{metalIOR.table[j]};
        violations += !(entry.wavelen > 0.0f);
        violations += !(entry.wavelen <= 14000.0f);
        violations += !(entry.ior[0] > 0.0f);
        violations += !(entry.ior[1] >= 0.0f);
        if (j > 0)
          violations += !(metalIOR.table[j - 1].wavelen < entry.wavelen);
      }
      CAPTURE(i);
      CHECK(violations == 0);
    }

    // An invalid metal must zero the table and return 0, and a null table
    // pointer must return 0 without crashing.
    smdl::MetalIOR metalIOR{};
    CHECK(smdl::smdlFindMetalIOR(smdl::Metal(-1), &metalIOR) == 0);
    CHECK(metalIOR.table == nullptr);
    CHECK(metalIOR.tableSize == 0);
    CHECK(smdl::smdlFindMetalIOR(smdl::Metal(17), &metalIOR) == 0);
    CHECK(smdl::smdlFindMetalIOR(smdl::Metal::Au, nullptr) == 0);
  }
  SUBCASE("smdlEvalMetalIOR at table wavelengths") {
    // Evaluating all table wavelengths at once must reproduce the table
    // exactly. This exercises the sorted-wavelength scan over each entire
    // table, including both endpoints.
    for (int i = int(smdl::Metal::First); i <= int(smdl::Metal::Last); i++) {
      smdl::MetalIOR metalIOR{};
      REQUIRE(smdl::smdlFindMetalIOR(smdl::Metal(i), &metalIOR) == 1);
      std::vector<float> wavelens(metalIOR.tableSize);
      std::vector<float> iorN(metalIOR.tableSize);
      std::vector<float> iorK(metalIOR.tableSize);
      for (int j = 0; j < metalIOR.tableSize; j++)
        wavelens[j] = metalIOR.table[j].wavelen;
      smdl::smdlEvalMetalIOR(smdl::Metal(i), metalIOR.tableSize,
                             wavelens.data(), iorN.data(), iorK.data());
      int violations{};
      for (int j = 0; j < metalIOR.tableSize; j++) {
        violations += !(iorN[j] == metalIOR.table[j].ior[0]);
        violations += !(iorK[j] == metalIOR.table[j].ior[1]);
      }
      CAPTURE(i);
      CHECK(violations == 0);
    }
  }
  SUBCASE("smdlEvalMetalIOR interpolation and clamping") {
    // The evaluation must interpolate linearly between table entries.
    smdl::MetalIOR metalIOR{};
    REQUIRE(smdl::smdlFindMetalIOR(smdl::Metal::Au, &metalIOR) == 1);
    const auto &entry0{metalIOR.table[10]};
    const auto &entry1{metalIOR.table[11]};
    auto ior =
        evalMetalIOR(smdl::Metal::Au, 0.5f * (entry0.wavelen + entry1.wavelen));
    CHECK(ior[0] == doctest::Approx(0.5f * (entry0.ior[0] + entry1.ior[0])));
    CHECK(ior[1] == doctest::Approx(0.5f * (entry0.ior[1] + entry1.ior[1])));

    // Wavelengths outside the table domain must clamp to the first and
    // last entries instead of extrapolating.
    const auto &entryFirst{metalIOR.table[0]};
    const auto &entryLast{metalIOR.table[metalIOR.tableSize - 1]};
    CHECK(evalMetalIOR(smdl::Metal::Au, 10.0f)[0] == entryFirst.ior[0]);
    CHECK(evalMetalIOR(smdl::Metal::Au, 10.0f)[1] == entryFirst.ior[1]);
    CHECK(evalMetalIOR(smdl::Metal::Au, 100000.0f)[0] == entryLast.ior[0]);
    CHECK(evalMetalIOR(smdl::Metal::Au, 100000.0f)[1] == entryLast.ior[1]);
  }
  SUBCASE("smdlEvalMetalIOR argument handling") {
    // Null arguments must not crash.
    smdl::smdlEvalMetalIOR(smdl::Metal::Au, 1, nullptr, nullptr, nullptr);

    // An invalid metal must fill the outputs with zeros.
    float wavelens[2] = {400.0f, 700.0f};
    float iorN[2] = {7.0f, 7.0f};
    float iorK[2] = {7.0f, 7.0f};
    smdl::smdlEvalMetalIOR(smdl::Metal(17), 2, wavelens, iorN, iorK);
    CHECK(iorN[0] == 0.0f);
    CHECK(iorN[1] == 0.0f);
    CHECK(iorK[0] == 0.0f);
    CHECK(iorK[1] == 0.0f);

    // Either output may be null to skip it.
    smdl::smdlEvalMetalIOR(smdl::Metal::Au, 2, wavelens, iorN, nullptr);
    smdl::smdlEvalMetalIOR(smdl::Metal::Au, 2, wavelens, nullptr, iorK);
    CHECK(iorN[0] == evalMetalIOR(smdl::Metal::Au, 400.0f)[0]);
    CHECK(iorK[0] == evalMetalIOR(smdl::Metal::Au, 400.0f)[1]);
    CHECK(iorN[1] == evalMetalIOR(smdl::Metal::Au, 700.0f)[0]);
    CHECK(iorK[1] == evalMetalIOR(smdl::Metal::Au, 700.0f)[1]);
  }
  SUBCASE("smdlEvalMetalIOR against published values") {
    // Spot check against the underlying refractiveindex.info datasets. The
    // tables are downsampled, so interpolation may deviate from dropped
    // source entries by up to about half a percent.
    struct {
      smdl::Metal metal{};
      float wavelen{};
      smdl::float2 expectedIOR{};
    } static const SPOTS[] = {
        {smdl::Metal::Ag, 649.9f, {0.06061f, 4.283f}},      // Yang
        {smdl::Metal::Au, 650.0f, {0.1546f, 3.647f}},       // Olmon
        {smdl::Metal::Cu, 650.0f, {0.326f, 3.4f}},          // Querry
        {smdl::Metal::CuZn, 10000.0f, {16.878f, 51.601f}}}; // Querry
    for (const auto &spot : SPOTS) {
      auto ior = evalMetalIOR(spot.metal, spot.wavelen);
      CHECK(ior[0] == doctest::Approx(spot.expectedIOR[0]).epsilon(0.01));
      CHECK(ior[1] == doctest::Approx(spot.expectedIOR[1]).epsilon(0.01));
    }

    // Physical sanity of normal-incidence reflectance: silver is highly
    // reflective across the visible range, and gold is much more
    // reflective in red than in blue, which is why it looks yellow.
    CHECK(reflectance(evalMetalIOR(smdl::Metal::Ag, 550.0f)) > 0.9f);
    CHECK(reflectance(evalMetalIOR(smdl::Metal::Au, 650.0f)) > 0.9f);
    CHECK(reflectance(evalMetalIOR(smdl::Metal::Au, 450.0f)) < 0.5f);

    // The tin table below 730nm is a Drude-Lorentz extrapolation with
    // roughly 0.1 uncertainty in reflectance, so only pin down that it
    // stays a plausible silvery metal through the visible range.
    for (float wavelen : {380.0f, 550.0f, 700.0f}) {
      auto ior = evalMetalIOR(smdl::Metal::Sn, wavelen);
      CHECK(ior[0] > 0.0f);
      CHECK(ior[1] > ior[0]);
      CHECK(reflectance(ior) > 0.65f);
      CHECK(reflectance(ior) < 0.9f);
    }
  }

  SUBCASE("smdlEvalMetalIOR matches the search it replaced") {
    // The bucket index is an acceleration, not a re-derivation: it must land
    // on the same bracketing pair the search does, so the interpolation is
    // the same arithmetic on the same two entries and the result agrees to
    // the bit. Anything less would shift rendered appearance.
    for (int i = int(smdl::Metal::First); i <= int(smdl::Metal::Last); i++) {
      const auto metal{smdl::Metal(i)};
      smdl::MetalIOR metalIOR{};
      REQUIRE(smdl::smdlFindMetalIOR(metal, &metalIOR) == 1);
      const auto wavelens{probeWavelengths(metalIOR)};
      const int numWavelens(wavelens.size());
      std::vector<float> iorN(numWavelens), iorK(numWavelens);
      std::vector<float> expectN(numWavelens), expectK(numWavelens);
      searchEvalMetalIOR(metal, numWavelens, wavelens.data(), expectN.data(),
                         expectK.data());
      smdl::smdlEvalMetalIOR(metal, numWavelens, wavelens.data(), iorN.data(),
                             iorK.data());
      int numMismatched{};
      for (int j = 0; j < numWavelens; j++)
        if (!hasSameBits(iorN[j], expectN[j]) ||
            !hasSameBits(iorK[j], expectK[j]))
          numMismatched++;
      CHECK(numMismatched == 0);
    }
  }

  SUBCASE("smdlEvalMetalIOR does not depend on wavelength order") {
    // The search used to carry its lower bound forward from the previous
    // wavelength, which quietly returned wrong values for unsorted input
    // rather than merely being slower. The bucket index reaches each
    // wavelength independently, so order cannot matter.
    for (int i = int(smdl::Metal::First); i <= int(smdl::Metal::Last); i++) {
      const auto metal{smdl::Metal(i)};
      smdl::MetalIOR metalIOR{};
      REQUIRE(smdl::smdlFindMetalIOR(metal, &metalIOR) == 1);
      auto wavelens{probeWavelengths(metalIOR)};
      wavelens.erase(std::remove_if(wavelens.begin(), wavelens.end(),
                                    [](float w) { return std::isnan(w); }),
                     wavelens.end());
      const int numWavelens(wavelens.size());
      std::vector<float> iorN(numWavelens), iorK(numWavelens);
      smdl::smdlEvalMetalIOR(metal, numWavelens, wavelens.data(), iorN.data(),
                             iorK.data());
      auto reversed{wavelens};
      std::reverse(reversed.begin(), reversed.end());
      std::vector<float> reversedN(numWavelens), reversedK(numWavelens);
      smdl::smdlEvalMetalIOR(metal, numWavelens, reversed.data(),
                             reversedN.data(), reversedK.data());
      int numMismatched{};
      for (int j = 0; j < numWavelens; j++)
        if (!hasSameBits(iorN[j], reversedN[numWavelens - 1 - j]) ||
            !hasSameBits(iorK[j], reversedK[numWavelens - 1 - j]))
          numMismatched++;
      CHECK(numMismatched == 0);
    }
  }
}
