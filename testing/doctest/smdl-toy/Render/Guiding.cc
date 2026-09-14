#include "Fixtures.h"

#include <cstring>
#include <string>
#include <vector>

#include "Render/Guiding.h"
#include "Render/Sampler.h"

// The '.sdtree' sidecar a guided render leaves beside its accumulation
// and a resuming session reads back, so that the training already paid
// for is not paid for twice. The layout is the spec table on
// `STree::writeFile()`; the offsets the refusals patch below are read
// off it.

namespace {

// The corner of the bound every record below is splatted near, and a
// direction that is exactly unit without normalizing.
constexpr float3 LOBE_DIRECTION{0.6f, 0.0f, 0.8f};

// A tree with training in it, left where `writeFile()` is meant to find
// it: records absorbed and `refine()` run, so the structure has split
// and every leaf's sampling quadtree is built from flux.
[[nodiscard]] STree trainedTree() {
  STree tree{float3(-1.0f, -1.0f, -1.0f), float3(1.0f, 1.0f, 1.0f)};
  Sampler sampler{};
  for (uint32_t pass = 0; pass < 3; pass++) {
    // The counter layout is fixed until `refine()` rebuilds it, so each
    // pass gets its own mirror, exactly as a render's threads do.
    std::vector<uint64_t> mirror(tree.counterCount(), 0);
    for (uint32_t i = 0; i < 2048; i++) {
      sampler.startPixelSample(i, pass);
      const float t{float(i) / 2048.0f};
      // A bright streak across one face of the bound, so that the
      // spatial leaves have something to split over and the quadtrees
      // something to resolve.
      const float3 position{-0.9f + 1.8f * t, -0.9f, 0.5f};
      tree.record(sampler, position, LOBE_DIRECTION, 1.0f + t, 0.5f, 0.25f,
                  mirror.data());
    }
    tree.absorb({mirror.data()});
    tree.refine(64, 0.01f, 20);
  }
  return tree;
}

// The positions and directions a round trip is compared over: a lattice
// through the bound, and both the trained lobe and directions away from
// it.
[[nodiscard]] std::vector<float3> probePositions() {
  std::vector<float3> points{};
  for (int z = 0; z < 4; z++)
    for (int y = 0; y < 4; y++)
      for (int x = 0; x < 4; x++)
        points.push_back(float3(-0.9f + 0.6f * float(x),
                                -0.9f + 0.6f * float(y),
                                -0.9f + 0.6f * float(z)));
  return points;
}

[[nodiscard]] std::vector<float3> probeDirections() {
  return {LOBE_DIRECTION,           float3(-0.6f, 0.0f, -0.8f),
          float3(0.0f, 0.0f, 1.0f), float3(0.0f, 0.0f, -1.0f),
          float3(1.0f, 0.0f, 0.0f), float3(0.0f, 1.0f, 0.0f),
          float3(0.0f, -0.8f, 0.6f)};
}

} // namespace

TEST_CASE("STree: the guide tree file round trip") {
  const TempDir dir{"sdtree-round-trip"};
  const std::string fileName{(dir / "tree.sdtree").string()};
  const STree written{trainedTree()};
  // The training must have split the tree, or the round trip below
  // proves nothing about the structure.
  REQUIRE(written.leafCount() > 1);
  written.writeFile(fileName, 137);
  uint64_t samplesPerPixel{};
  const STree read{STree::readFile(fileName, samplesPerPixel)};

  SUBCASE("The samples that trained it come back") {
    CHECK(samplesPerPixel == 137);
  }
  SUBCASE("The spatial structure comes back") {
    CHECK(read.leafCount() == written.leafCount());
  }
  SUBCASE("The learned mixture weights come back") {
    float minWritten{}, meanWritten{}, minRead{}, meanRead{};
    written.alphaStats(minWritten, meanWritten);
    read.alphaStats(minRead, meanRead);
    CHECK(minRead == minWritten);
    CHECK(meanRead == meanWritten);
  }
  SUBCASE("Every leaf samples the same directions to the same density") {
    // What the tree is for: the density the sampler draws by. It goes
    // through fixed-point flux units, so it comes back bit for bit.
    size_t nonUniform{};
    for (const auto &position : probePositions())
      for (const auto &direction : probeDirections()) {
        const float pdfWritten{written.samplingAt(position).pdf(direction)};
        const float pdfRead{read.samplingAt(position).pdf(direction)};
        CHECK_MESSAGE(hasSameBits(pdfRead, pdfWritten), "at ", position,
                      " toward ", direction, ": ", pdfRead, " and not ",
                      pdfWritten);
        if (pdfWritten != 0.25f / float(smdl::PI)) nonUniform++;
      }
    // And the tree really did learn something, so the equality above is
    // not the equality of two uniform trees.
    CHECK(nonUniform > 0);
  }
  SUBCASE("A rewrite of what was read is the same file") {
    const std::string again{(dir / "again.sdtree").string()};
    read.writeFile(again, samplesPerPixel);
    CHECK(dir.read("again.sdtree") == dir.read("tree.sdtree"));
  }
}

TEST_CASE("STree::readFile: what it refuses") {
  const TempDir dir{"sdtree-refusals"};
  const std::string fileName{(dir / "tree.sdtree").string()};
  trainedTree().writeFile(fileName, 8);
  const std::string good{dir.read("tree.sdtree")};
  uint64_t samplesPerPixel{};

  // The bytes of `good` with `count` bytes at `offset` replaced, written
  // where `readFile()` will be pointed at it.
  const auto patched{[&](size_t offset, const void *bytes, size_t count) {
    std::string text{good};
    REQUIRE(offset + count <= text.size());
    std::memcpy(&text[offset], bytes, count);
    return dir.write("patched.sdtree", text).string();
  }};
  // The message the read refuses `name` with, empty if it accepts it.
  const auto refusalOf{[&](const std::string &name) {
    try {
      STree::readFile(name, samplesPerPixel);
    } catch (const smdl::Error &error) {
      return error.message;
    }
    return std::string();
  }};

  SUBCASE("A file that is not there") {
    CHECK_CONTAINS(refusalOf((dir / "absent.sdtree").string()),
                   "Cannot open guide tree");
  }
  SUBCASE("Another format under the same name") {
    CHECK_CONTAINS(
        refusalOf(
            dir.write("wrong.sdtree", "not a guide tree at all").string()),
        "bad magic");
  }
  SUBCASE("A version this build does not read") {
    const uint16_t version{2};
    CHECK_CONTAINS(refusalOf(patched(8, &version, sizeof(version))),
                   "version 2 (this build reads version 1)");
  }
  SUBCASE("A reserved field that is not 0") {
    const uint16_t reserved{1};
    CHECK_CONTAINS(refusalOf(patched(10, &reserved, sizeof(reserved))),
                   "the reserved field is not 0");
  }
  SUBCASE("A node count that cannot be right") {
    const uint32_t none{0};
    CHECK_CONTAINS(refusalOf(patched(12, &none, sizeof(none))),
                   "implausible spatial node count");
    const uint32_t world{1u << 20};
    CHECK_CONTAINS(refusalOf(patched(12, &world, sizeof(world))),
                   "implausible spatial node count");
  }
  SUBCASE("A bound with no extent") {
    const float flat{0.0f};
    CHECK_CONTAINS(refusalOf(patched(36, &flat, sizeof(flat))),
                   "degenerate bounds");
  }
  SUBCASE("A file that stops in the middle") {
    CHECK_CONTAINS(
        refusalOf(dir.write("short.sdtree", good.substr(0, good.size() / 2))
                      .string()),
        "truncated");
  }
  SUBCASE("A file that is only its header") {
    CHECK_CONTAINS(
        refusalOf(dir.write("head.sdtree", good.substr(0, 48)).string()),
        "truncated");
  }
}
