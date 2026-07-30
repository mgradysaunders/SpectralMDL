#include "doctest.h"

#include <vector>

#include "smdl/Support/MetalIOR.h"

static smdl::float2 evalMetalIOR(smdl::Metal metal, float wavelen) {
  float n{}, k{};
  smdl::smdlEvalMetalIOR(metal, 1, &wavelen, &n, &k);
  return {n, k};
}

/// The normal-incidence Fresnel reflectance.
static float reflectance(smdl::float2 ior) {
  const float n{ior[0]}, k{ior[1]};
  return ((n - 1) * (n - 1) + k * k) / ((n + 1) * (n + 1) + k * k);
}

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
        {smdl::Metal::Ag, 659.5f, {0.05f, 4.483f}},         // Johnson & Christy
        {smdl::Metal::Au, 659.5f, {0.14f, 3.697f}},         // Johnson & Christy
        {smdl::Metal::Cu, 659.5f, {0.22f, 3.747f}},         // Johnson & Christy
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
  }
}
