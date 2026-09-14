#include "IO/CurvesFile.h"
#include "IO/BinaryFile.h"

#include "smdl/Support/Error.h"
#include "smdl/Support/Filesystem.h"
#include "smdl/Support/Strings.h"

#include <cmath>
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
  uint16_t keyCount{};
  uint32_t strandCount{};
  uint32_t pointCount{};
};

static_assert(sizeof(CurvesHeader) == 24, "the header is 24 bytes");
static_assert(sizeof(float4) == 16, "points are packed float4s");
static_assert(sizeof(float2) == 8, "root UVs are packed float2s");

constexpr uint16_t FLAG_ROOT_UVS = 1;
constexpr uint16_t FLAG_COMPRESSED = 2;
constexpr uint16_t FLAG_ALL = FLAG_ROOT_UVS | FLAG_COMPRESSED;

// The shared shape validation, so that reading a bad file and being
// asked to write one fail with the same words. `fileName` names the
// file in the message; `verb` is "read" or "write".
void validateCurvesShape(const CurvesFile &curves, const std::string &fileName,
                         std::string_view verb) {
  auto fail{[&](auto &&...args) {
    throw smdl::Error(smdl::concat("Cannot ", verb, " curves ",
                                   SpellFilePath(fileName), ": ", args...));
  }};
  if (curves.basis != CurvesFile::Basis::LINEAR &&
      curves.basis != CurvesFile::Basis::BSPLINE &&
      curves.basis != CurvesFile::Basis::CATMULL_ROM)
    fail("unknown basis ", uint16_t(curves.basis),
         " (this build knows 0 = linear, 1 = b-spline, 2 = catmull-rom)");
  if (curves.keyTimes.empty()) fail("a groom must have at least one key time");
  for (size_t i = 0; i < curves.keyTimes.size(); i++) {
    if (!std::isfinite(curves.keyTimes[i]))
      fail("key time ", i, " is not finite");
    if (i > 0 && !(curves.keyTimes[i] > curves.keyTimes[i - 1]))
      fail("the key times must strictly increase (key ", i, " is at ",
           curves.keyTimes[i], " s, after ", curves.keyTimes[i - 1], " s)");
  }
  if (curves.points.size() % curves.keyTimes.size() != 0)
    fail("the point block holds ",
         SpellCounted(curves.points.size(), "entry", "entries"),
         ", which is "
         "not a whole number of points at ",
         SpellCounted(curves.keyTimes.size(), "key"), " each");
  if (curves.strandOffsets.empty() || curves.strandOffsets.front() != 0 ||
      curves.strandOffsets.back() != curves.pointCount())
    fail("the offset table must start at 0 and end at the point count");
  const uint32_t minPoints{CurvesFile::minPointsPerStrand(curves.basis)};
  for (size_t i = 0; i + 1 < curves.strandOffsets.size(); i++) {
    if (curves.strandOffsets[i] >= curves.strandOffsets[i + 1])
      fail("the offset table must strictly increase (strand ", i,
           " is empty or out of order)");
    if (curves.strandOffsets[i + 1] - curves.strandOffsets[i] < minPoints)
      fail("strand ", i, " has ",
           SpellCounted(curves.strandOffsets[i + 1] - curves.strandOffsets[i],
                        "point"),
           ", but the ", CurvesFile::basisName(curves.basis),
           " basis needs at least ", minPoints);
  }
  if (!curves.rootUVs.empty() && curves.rootUVs.size() != curves.strandCount())
    fail("the root UV column must be empty or one entry per strand");
}

} // namespace

CurvesKeyBlend CurvesFile::blendAt(float seconds) const noexcept {
  if (keyTimes.size() < 2) return {};
  if (!(seconds > keyTimes.front())) return {};
  if (!(seconds < keyTimes.back())) {
    const uint32_t last{uint32_t(keyTimes.size() - 1)};
    return {last, last, 0.0f};
  }
  size_t i{1};
  while (i + 1 < keyTimes.size() && keyTimes[i] < seconds) i++;
  const float lo{keyTimes[i - 1]}, hi{keyTimes[i]};
  // A key's own time answers that key, with no blend and no rounding,
  // exactly as `MotionTrack::at()` does.
  if (seconds == lo) return {uint32_t(i - 1), uint32_t(i - 1), 0.0f};
  if (seconds == hi) return {uint32_t(i), uint32_t(i), 0.0f};
  return {uint32_t(i - 1), uint32_t(i), (seconds - lo) / (hi - lo)};
}

bool CurvesFile::hasKeyBetween(float open, float shut) const noexcept {
  for (const float time : keyTimes)
    if (time > open && time < shut) return true;
  return false;
}

CurvesFile readCurvesFile(const std::string &fileName) {
  requireLittleEndianHost("'.curves'");
  const auto fail{[&](auto &&...args) {
    throw smdl::Error(smdl::concat("Cannot read curves ",
                                   SpellFilePath(fileName), ": ", args...));
  }};
  const std::string contents{smdl::readOrThrow(fileName)};
  CurvesHeader header{};
  if (contents.size() >= sizeof(header))
    std::memcpy(&header, contents.data(), sizeof(header));
  if (!hasMagic(header.magic, CURVES_MAGIC))
    throw smdl::Error(smdl::concat(
        SpellFilePath(fileName),
        " is not a '.curves' file (bad magic; expected it to begin "
        "with \"SMDLCRVS\")"));
  if (header.version != 1)
    fail("version ", header.version, " (this build reads version 1)");
  if (header.flags & ~FLAG_ALL)
    fail("its flags hold unknown bits (", header.flags & ~uint16_t(FLAG_ALL),
         ")");
  if (header.keyCount == 0) fail("it declares no key times");
  CurvesFile curves{};
  curves.version = header.version;
  curves.basis = CurvesFile::Basis(header.basis);
  curves.isCompressed = (header.flags & FLAG_COMPRESSED) != 0;
  const size_t timesOffset{sizeof(header)};
  const size_t timesSize{header.keyCount * sizeof(float)};
  if (contents.size() < timesOffset + timesSize)
    fail("truncated (the key times do not fit)");
  curves.keyTimes.resize(header.keyCount);
  std::memcpy(curves.keyTimes.data(), contents.data() + timesOffset, timesSize);
  const size_t payloadSize{
      (size_t(header.strandCount) + 1) * sizeof(uint32_t) +
      size_t(header.pointCount) * header.keyCount * sizeof(float4) +
      ((header.flags & FLAG_ROOT_UVS) ? header.strandCount * sizeof(float2)
                                      : 0)};
  const BinaryPayload payload{contents, timesOffset + timesSize, payloadSize,
                              curves.isCompressed, fileName};
  ByteReader reader{payload.bytes()};
  reader.takeArray(curves.strandOffsets, size_t(header.strandCount) + 1);
  reader.takeArray(curves.points, size_t(header.pointCount) * header.keyCount);
  if (header.flags & FLAG_ROOT_UVS)
    reader.takeArray(curves.rootUVs, header.strandCount);
  if (!reader.empty())
    fail("truncated (the header promises ",
         SpellCounted(header.strandCount, "strand"), " and ",
         SpellCounted(header.pointCount, "point"), " at ",
         SpellCounted(header.keyCount, "key"), ")");
  validateCurvesShape(curves, fileName, "read");
  return curves;
}

void writeCurvesFile(const std::string &fileName, const CurvesFile &curves) {
  requireLittleEndianHost("'.curves'");
  validateCurvesShape(curves, fileName, "write");
  if (curves.keyTimes.size() > 0xFFFF)
    throw smdl::Error(
        smdl::concat("Cannot write curves ", SpellFilePath(fileName), ": ",
                     SpellCounted(curves.keyTimes.size(), "key time"),
                     " exceeds the 65535 the header can state"));
  std::vector<std::byte> payload{};
  payload.reserve(curves.strandOffsets.size() * sizeof(uint32_t) +
                  curves.points.size() * sizeof(float4) +
                  curves.rootUVs.size() * sizeof(float2));
  pushArray(payload, curves.strandOffsets);
  pushArray(payload, curves.points);
  if (curves.hasRootUVs()) pushArray(payload, curves.rootUVs);
  if (curves.isCompressed) payload = smdl::compressBytes(payload);
  std::ofstream stream{fileName, std::ios::binary};
  if (!stream)
    throw smdl::Error(
        smdl::concat("Cannot write curves ", SpellFilePath(fileName)));
  CurvesHeader header{};
  setMagic(header.magic, CURVES_MAGIC);
  header.version = 1;
  header.basis = uint16_t(curves.basis);
  header.flags = uint16_t((curves.hasRootUVs() ? FLAG_ROOT_UVS : 0) |
                          (curves.isCompressed ? FLAG_COMPRESSED : 0));
  header.keyCount = uint16_t(curves.keyCount());
  header.strandCount = curves.strandCount();
  header.pointCount = curves.pointCount();
  putRecord(stream, header);
  putArray(stream, curves.keyTimes);
  stream.write(reinterpret_cast<const char *>(payload.data()),
               std::streamsize(payload.size()));
  if (!stream)
    throw smdl::Error(
        smdl::concat("Cannot write curves ", SpellFilePath(fileName)));
}

CurveAxis evalCurveAxis(CurvesFile::Basis basis, const float4 *window,
                        float u) {
  const float4 *p{window};
  CurveAxis axis{};
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
    const float4 c0{2.0f * p[1]};
    const float4 c1{p[2] - p[0]};
    const float4 c2{2.0f * p[0] - 5.0f * p[1] + 4.0f * p[2] - p[3]};
    const float4 c3{-1.0f * p[0] + 3.0f * p[1] - 3.0f * p[2] + p[3]};
    finish(0.5f * (c0 + u * (c1 + u * (c2 + u * c3))),
           0.5f * (c1 + u * (2.0f * c2 + u * 3.0f * c3)));
    break;
  }
  }
  return axis;
}
