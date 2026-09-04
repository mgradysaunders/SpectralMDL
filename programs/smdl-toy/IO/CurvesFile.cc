#include "IO/CurvesFile.h"

#include "smdl/Support/Error.h"
#include "smdl/Support/Strings.h"

#include <cstring>
#include <fstream>

// The `.curves` reader, writer, and basis math: explicit-width
// little-endian I/O over the layout documented in `CurvesFile.h`,
// exactly the `PlacesFile.cc` discipline, plus the closed-form window
// evaluation the hit path shares with the tests. The Embree build lives
// in `Curves.cc`, so that this file stays Embree-free for the doctest
// binary.
//
// Measured at the scale this is for (a one-million-point, 100k-strand
// Catmull-Rom groom, 16.4 MB): the raw read is ~6 ms, and the whole
// `Scene::add()` including validation, the strand tables, and the
// Embree BVH is ~240 ms, so the file format is never the slow part.

namespace {

// The fixed-size header, exactly as it sits in the file.
class CurvesHeader final {
public:
  char magic[8]{};
  uint16_t version{};
  uint16_t basis{};
  uint16_t flags{};
  uint16_t reserved0{};
  uint32_t strandCount{};
  uint32_t pointCount{};
  uint32_t reserved1{};
};

static_assert(sizeof(CurvesHeader) == 28, "the header is 28 bytes");
static_assert(sizeof(float4) == 16, "points are packed float4s");
static_assert(sizeof(float2) == 8, "root UVs are packed float2s");

constexpr uint16_t FLAG_ROOT_UVS = 1;

// This code assumes a little-endian host outright, like the rest of the
// renderer's binary I/O; the check is here so a big-endian port fails
// loudly instead of scattering garbage.
[[nodiscard]] bool hostIsLittleEndian() noexcept {
  const uint32_t probe{1};
  return *reinterpret_cast<const unsigned char *>(&probe) == 1;
}

// The shared shape validation, so that reading a bad file and being
// asked to write one fail with the same words. `fileName` names the
// file in the message; `verb` is "read" or "write".
void validateCurvesShape(const CurvesFile &curves, const std::string &fileName,
                         std::string_view verb) {
  auto fail{[&](auto &&...args) {
    throw smdl::Error(smdl::concat("cannot ", verb, " curves ",
                                   smdl::QuotedPath(fileName), ": ", args...));
  }};
  if (curves.basis != CurvesFile::Basis::LINEAR &&
      curves.basis != CurvesFile::Basis::BSPLINE &&
      curves.basis != CurvesFile::Basis::CATMULL_ROM)
    fail("unknown basis ", uint16_t(curves.basis),
         " (this build knows 0 = linear, 1 = b-spline, 2 = catmull-rom)");
  if (curves.strandOffsets.empty() || curves.strandOffsets.front() != 0 ||
      curves.strandOffsets.back() != curves.points.size())
    fail("the offset table must start at 0 and end at the point count");
  const auto minPoints{CurvesFile::minPointsPerStrand(curves.basis)};
  for (size_t i = 0; i + 1 < curves.strandOffsets.size(); i++) {
    if (curves.strandOffsets[i] >= curves.strandOffsets[i + 1])
      fail("the offset table must strictly increase (strand ", i,
           " is empty or out of order)");
    if (curves.strandOffsets[i + 1] - curves.strandOffsets[i] < minPoints)
      fail("strand ", i, " has ",
           curves.strandOffsets[i + 1] - curves.strandOffsets[i],
           " point(s), but the ", CurvesFile::basisName(curves.basis),
           " basis needs at least ", minPoints);
  }
  if (!curves.rootUVs.empty() && curves.rootUVs.size() != curves.strandCount())
    fail("the root UV column must be empty or one entry per strand");
}

} // namespace

CurvesFile readCurvesFile(const std::string &fileName) {
  if (!hostIsLittleEndian())
    throw smdl::Error("'.curves' I/O requires a little-endian host");
  auto stream{std::ifstream(fileName, std::ios::binary)};
  if (!stream)
    throw smdl::Error(
        smdl::concat("cannot open curves ", smdl::QuotedPath(fileName)));
  auto header{CurvesHeader()};
  stream.read(reinterpret_cast<char *>(&header), sizeof(header));
  if (!stream ||
      std::memcmp(header.magic, CURVES_MAGIC.data(), CURVES_MAGIC.size()) != 0)
    throw smdl::Error(smdl::concat(
        smdl::QuotedPath(fileName),
        " is not a '.curves' file (bad magic; expected it to begin "
        "with \"SMDLCRVS\")"));
  if (header.version != 1)
    throw smdl::Error(smdl::concat(
        "cannot read curves ", smdl::QuotedPath(fileName), ": version ",
        header.version, " (this build reads version 1)"));
  if (header.reserved0 != 0 || header.reserved1 != 0)
    throw smdl::Error(smdl::concat("cannot read curves ",
                                   smdl::QuotedPath(fileName),
                                   ": a reserved field is non-zero (must be 0 "
                                   "in version 1)"));
  auto curves{CurvesFile()};
  curves.version = header.version;
  curves.basis = CurvesFile::Basis(header.basis);
  curves.strandOffsets.resize(size_t(header.strandCount) + 1);
  stream.read(reinterpret_cast<char *>(curves.strandOffsets.data()),
              std::streamsize(sizeof(uint32_t) * curves.strandOffsets.size()));
  curves.points.resize(header.pointCount);
  stream.read(reinterpret_cast<char *>(curves.points.data()),
              std::streamsize(sizeof(float4) * curves.points.size()));
  if (header.flags & FLAG_ROOT_UVS) {
    curves.rootUVs.resize(header.strandCount);
    stream.read(reinterpret_cast<char *>(curves.rootUVs.data()),
                std::streamsize(sizeof(float2) * curves.rootUVs.size()));
  }
  if (!stream)
    throw smdl::Error(
        smdl::concat("cannot read curves ", smdl::QuotedPath(fileName),
                     ": truncated (the header promises ", header.strandCount,
                     " strand(s) and ", header.pointCount, " point(s))"));
  validateCurvesShape(curves, fileName, "read");
  return curves;
}

void writeCurvesFile(const std::string &fileName, const CurvesFile &curves) {
  if (!hostIsLittleEndian())
    throw smdl::Error("'.curves' I/O requires a little-endian host");
  validateCurvesShape(curves, fileName, "write");
  auto stream{std::ofstream(fileName, std::ios::binary)};
  if (!stream)
    throw smdl::Error(
        smdl::concat("cannot write curves ", smdl::QuotedPath(fileName)));
  auto header{CurvesHeader()};
  std::memcpy(header.magic, CURVES_MAGIC.data(), CURVES_MAGIC.size());
  header.version = 1;
  header.basis = uint16_t(curves.basis);
  header.flags = curves.hasRootUVs() ? FLAG_ROOT_UVS : 0;
  header.strandCount = curves.strandCount();
  header.pointCount = uint32_t(curves.points.size());
  stream.write(reinterpret_cast<const char *>(&header), sizeof(header));
  stream.write(reinterpret_cast<const char *>(curves.strandOffsets.data()),
               std::streamsize(sizeof(uint32_t) * curves.strandOffsets.size()));
  stream.write(reinterpret_cast<const char *>(curves.points.data()),
               std::streamsize(sizeof(float4) * curves.points.size()));
  if (curves.hasRootUVs())
    stream.write(reinterpret_cast<const char *>(curves.rootUVs.data()),
                 std::streamsize(sizeof(float2) * curves.rootUVs.size()));
  if (!stream)
    throw smdl::Error(
        smdl::concat("cannot write curves ", smdl::QuotedPath(fileName)));
}

CurveAxis evalCurveAxis(CurvesFile::Basis basis, const float4 *window,
                        float u) {
  const auto *p{window};
  auto axis{CurveAxis()};
  auto finish{[&](const float4 &value, const float4 &derivative) {
    axis.point = float3(value);
    axis.tangent = float3(derivative);
    axis.radius = value.w;
  }};
  switch (basis) {
  case CurvesFile::Basis::LINEAR:
    finish((1.0f - u) * p[0] + u * p[1], p[1] - p[0]);
    break;
  case CurvesFile::Basis::BSPLINE: {
    // The uniform cubic B-spline window, matching Embree's cardinal
    // basis with the implicit equidistant knot vector.
    const float v{1.0f - u};
    const float b0{v * v * v / 6.0f};
    const float b1{(3.0f * u * u * u - 6.0f * u * u + 4.0f) / 6.0f};
    const float b2{(-3.0f * u * u * u + 3.0f * u * u + 3.0f * u + 1.0f) / 6.0f};
    const float b3{u * u * u / 6.0f};
    const float d0{-0.5f * v * v};
    const float d1{0.5f * (3.0f * u * u - 4.0f * u)};
    const float d2{0.5f * (-3.0f * u * u + 2.0f * u + 1.0f)};
    const float d3{0.5f * u * u};
    finish(b0 * p[0] + b1 * p[1] + b2 * p[2] + b3 * p[3],
           d0 * p[0] + d1 * p[1] + d2 * p[2] + d3 * p[3]);
    break;
  }
  case CurvesFile::Basis::CATMULL_ROM: {
    // Uniform Catmull-Rom through p1 and p2 with tangents (p2 - p0) / 2
    // and (p3 - p1) / 2, exactly Embree's definition.
    const auto c0{2.0f * p[1]};
    const auto c1{p[2] - p[0]};
    const auto c2{2.0f * p[0] - 5.0f * p[1] + 4.0f * p[2] - p[3]};
    const auto c3{-1.0f * p[0] + 3.0f * p[1] - 3.0f * p[2] + p[3]};
    finish(0.5f * (c0 + u * (c1 + u * (c2 + u * c3))),
           0.5f * (c1 + u * (2.0f * c2 + u * 3.0f * c3)));
    break;
  }
  }
  return axis;
}
