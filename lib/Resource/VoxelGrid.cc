#include "smdl/Resource/VoxelGrid.h"

#include <algorithm>
#include <cmath>

#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Endian.h"

#include "smdl/Support/Filesystem.h"

// NanoVDB is header-only and used here exclusively to load and save: grids
// are flattened into the brick structure at load time and rebuilt from it at
// save time, so nothing outside this file ever sees a NanoVDB type, which is
// what keeps SMDL_HAS_NANOVDB a one-file concern. The build tools come along
// with `CreateNanoGrid.h`, and pull in OpenVDB only under
// `NANOVDB_USE_OPENVDB`, which is never defined here.
#if SMDL_HAS_NANOVDB
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wdeprecated-copy"
#pragma GCC diagnostic ignored "-Wunused-private-field"
#include "nanovdb/NanoVDB.h"
#include "nanovdb/io/IO.h"
#include "nanovdb/tools/CreateNanoGrid.h"
#pragma GCC diagnostic pop
#endif // #if SMDL_HAS_NANOVDB

namespace smdl {

static constexpr int B{VoxelGrid::BRICK_EXTENT};

// The number of voxels in a brick.
static constexpr int64_t BRICK_VOLUME{int64_t(B) * B * B};

// The flattened form every loader produces, moved into the `VoxelGrid`
// members on success so that a failed load leaves the grid cleared.
struct FlatGrid final {
  int3 extent{};
  int3 brickCount{};
  float background{0.0f};
  float minValue{0.0f};
  float maxValue{0.0f};
  float3 worldBoundMin{};
  float3 worldBoundMax{};
  std::vector<int32_t> brickTable{};
  std::vector<float> brickData{};
};

[[nodiscard]] static int64_t brickTableSize(const int3 &brickCount) {
  return int64_t(brickCount.x) * brickCount.y * brickCount.z;
}

// Initialize the extent-derived fields of `flat` and the all-empty
// brick table.
static void initFlatGrid(FlatGrid &flat, int3 extent, float background) {
  if (!(extent.x > 0 && extent.y > 0 && extent.z > 0))
    throw Error(concat("invalid voxel grid extent (", extent.x, ", ", extent.y,
                       ", ", extent.z, ")"));
  flat.extent = extent;
  flat.brickCount = int3((extent.x + B - 1) / B, //
                         (extent.y + B - 1) / B, //
                         (extent.z + B - 1) / B);
  flat.background = background;
  flat.brickTable.assign(size_t(brickTableSize(flat.brickCount)), -1);
}

// Allocate the brick data blocks for every occupied brick, assigning
// table indices in x-fastest brick order. Every block starts out
// filled with the background, which is what the padding voxels of
// partial bricks at the high boundary must hold anyway.
static void allocateBricks(FlatGrid &flat, const std::vector<char> &occupied) {
  int32_t numOccupied{0};
  for (size_t i = 0; i < flat.brickTable.size(); i++)
    if (occupied[i]) flat.brickTable[i] = numOccupied++;
  flat.brickData.assign(size_t(numOccupied) * BRICK_VOLUME, flat.background);
}

// Finalize the global value bounds: `fillValue` has been called for
// every in-extent voxel of every occupied brick, so all that is left
// is folding in the background if any empty brick remains.
static void finalizeValueBounds(FlatGrid &flat, bool sawAnyValue) {
  const bool anyEmptyBrick{std::find(flat.brickTable.begin(),
                                     flat.brickTable.end(),
                                     -1) != flat.brickTable.end()};
  if (!sawAnyValue) {
    flat.minValue = flat.maxValue = flat.background;
  } else if (anyEmptyBrick) {
    flat.minValue = std::min(flat.minValue, flat.background);
    flat.maxValue = std::max(flat.maxValue, flat.background);
  }
}

//--{ NanoVDB loading
// Mark every brick overlapping the voxel range `[lo, lo + span)` as
// occupied, clamping the range against the extent. The range is in
// grid-local voxel coordinates and may hang off either end, because
// NanoVDB node origins are aligned in index space while the local
// origin is the corner of the active bounding box.
static void markRange(const FlatGrid &flat, std::vector<char> &occupied,
                      int3 lo, int span) {
  const int3 hi{std::min(lo.x + span, flat.extent.x),
                std::min(lo.y + span, flat.extent.y),
                std::min(lo.z + span, flat.extent.z)};
  lo = int3(std::max(lo.x, 0), std::max(lo.y, 0), std::max(lo.z, 0));
  for (int bz = lo.z / B; bz * B < hi.z; bz++)
    for (int by = lo.y / B; by * B < hi.y; by++)
      for (int bx = lo.x / B; bx * B < hi.x; bx++)
        occupied[size_t(bx + flat.brickCount.x *
                                 (by + int64_t(flat.brickCount.y) * bz))] = 1;
}

#if SMDL_HAS_NANOVDB

// Flatten the NanoVDB grid in `handle` if its build type is `BuildT`,
// widening values to `float`. Returns false if the build type does not
// match, so the caller can try the next one.
template <typename BuildT>
[[nodiscard]] static bool
tryFlattenNanoGrid(const nanovdb::GridHandle<nanovdb::HostBuffer> &handle,
                   FlatGrid &flat) {
  const auto *grid{handle.template grid<BuildT>()};
  if (!grid) return false;
  const auto &tree{grid->tree()};
  const auto &bbox{grid->indexBBox()};
  if (bbox.empty()) throw Error("NanoVDB grid has no active voxels");
  const auto bboxMin{bbox.min()};
  const auto bboxDim{bbox.dim()};
  initFlatGrid(flat, int3(bboxDim[0], bboxDim[1], bboxDim[2]),
               float(tree.root().background()));
  const auto &worldBBox{grid->worldBBox()};
  flat.worldBoundMin =
      float3(float(worldBBox.min()[0]), float(worldBBox.min()[1]),
             float(worldBBox.min()[2]));
  flat.worldBoundMax =
      float3(float(worldBBox.max()[0]), float(worldBBox.max()[1]),
             float(worldBBox.max()[2]));
  // Mark occupancy exactly, without visiting every voxel of the
  // bounding box: the linearized leaf array covers all fine values, and
  // the active tiles of the internal levels cover constant regions. The
  // spans are those of the fixed NanoVDB configuration (8, 128) plus
  // the leaf span itself.
  auto occupied{std::vector<char>(flat.brickTable.size(), 0)};
  const auto localCoord{[&](const nanovdb::Coord &ijk) {
    return int3(ijk[0] - bboxMin[0], ijk[1] - bboxMin[1], ijk[2] - bboxMin[2]);
  }};
  for (uint32_t i = 0; i < tree.nodeCount(0); i++)
    markRange(flat, occupied,
              localCoord(tree.template getFirstNode<0>()[i].origin()),
              /*span=*/8);
  for (uint32_t i = 0; i < tree.nodeCount(1); i++)
    for (auto it{tree.template getFirstNode<1>()[i].cbeginValueOn()}; it; ++it)
      markRange(flat, occupied, localCoord(it.getOrigin()), /*span=*/8);
  for (uint32_t i = 0; i < tree.nodeCount(2); i++)
    for (auto it{tree.template getFirstNode<2>()[i].cbeginValueOn()}; it; ++it)
      markRange(flat, occupied, localCoord(it.getOrigin()), /*span=*/128);
  // Active tiles directly in the root cover such enormous regions that
  // precise marking is not worth the awkward API: mark everything. In
  // practice fog volumes never have them.
  if (tree.root().cbeginValueOn())
    std::fill(occupied.begin(), occupied.end(), char(1));
  allocateBricks(flat, occupied);
  // Fill the occupied bricks through a cached read accessor, tracking
  // the value bounds over the in-extent voxels. Reads of inactive
  // voxels return the background, which is exactly the field the grid
  // represents.
  auto accessor{grid->getAccessor()};
  bool sawAnyValue{false};
  for (int bz = 0; bz < flat.brickCount.z; bz++)
    for (int by = 0; by < flat.brickCount.y; by++)
      for (int bx = 0; bx < flat.brickCount.x; bx++) {
        const auto tableIndex{size_t(
            bx + flat.brickCount.x * (by + int64_t(flat.brickCount.y) * bz))};
        const auto brickIndex{flat.brickTable[tableIndex]};
        if (brickIndex < 0) continue;
        float *block{&flat.brickData[size_t(brickIndex) * BRICK_VOLUME]};
        const int3 lo{bx * B, by * B, bz * B};
        const int3 hi{std::min(lo.x + B, flat.extent.x),
                      std::min(lo.y + B, flat.extent.y),
                      std::min(lo.z + B, flat.extent.z)};
        for (int z = lo.z; z < hi.z; z++)
          for (int y = lo.y; y < hi.y; y++)
            for (int x = lo.x; x < hi.x; x++) {
              const float value{float(accessor.getValue(nanovdb::Coord(
                  x + bboxMin[0], y + bboxMin[1], z + bboxMin[2])))};
              block[(x - lo.x) + B * ((y - lo.y) + int64_t(B) * (z - lo.z))] =
                  value;
              if (!sawAnyValue) {
                flat.minValue = flat.maxValue = value;
                sawAnyValue = true;
              } else {
                flat.minValue = std::min(flat.minValue, value);
                flat.maxValue = std::max(flat.maxValue, value);
              }
            }
      }
  finalizeValueBounds(flat, sawAnyValue);
  return true;
}

#endif // #if SMDL_HAS_NANOVDB

static void loadNanoVDB(const std::string &fileName,
                        const std::string &gridName, FlatGrid &flat) {
#if SMDL_HAS_NANOVDB
  auto handle{
      gridName.empty()
          ? nanovdb::io::readGrid<nanovdb::HostBuffer>(fileName)
          : nanovdb::io::readGrid<nanovdb::HostBuffer>(fileName, gridName)};
  if (!handle)
    throw Error(gridName.empty() ? std::string("no grid in NanoVDB file")
                                 : concat("no grid named ", Quoted(gridName),
                                          " in NanoVDB file"));
  if (!(tryFlattenNanoGrid<float>(handle, flat) ||
        tryFlattenNanoGrid<nanovdb::Fp4>(handle, flat) ||
        tryFlattenNanoGrid<nanovdb::Fp8>(handle, flat) ||
        tryFlattenNanoGrid<nanovdb::Fp16>(handle, flat) ||
        tryFlattenNanoGrid<nanovdb::FpN>(handle, flat)))
    throw Error(concat("unsupported NanoVDB grid type ", int(handle.gridType()),
                       "; expected a float or quantized-float grid"));
#else
  (void)fileName, (void)gridName, (void)flat;
  throw Error("built without NanoVDB!");
#endif // #if SMDL_HAS_NANOVDB
}
//--}

//--{ NanoVDB saving
#if SMDL_HAS_NANOVDB

// Build a NanoVDB grid from the brick storage. Only the voxels that
// differ from the background are set, which is what leaves the result
// sparse, and the index-to-world map is recovered from the extent and
// the world bounds.
[[nodiscard]] static nanovdb::GridHandle<nanovdb::HostBuffer>
buildNanoGrid(const VoxelGrid &voxelGrid, const std::string &gridName) {
  const auto extent{voxelGrid.getExtent()};
  const auto background{voxelGrid.getBackground()};
  auto srcGrid{nanovdb::tools::build::Grid<float>(
      background, gridName, nanovdb::GridClass::FogVolume)};
  auto accessor{srcGrid.getAccessor()};
  for (int z = 0; z < extent.z; z++)
    for (int y = 0; y < extent.y; y++)
      for (int x = 0; x < extent.x; x++) {
        const float value{voxelGrid.fetch(x, y, z)};
        if (value != background)
          accessor.setValue(nanovdb::Coord(x, y, z), value);
      }
  // Anchor the extent at both ends. NanoVDB stores the ACTIVE index
  // bounding box, and `loadFromFile()` takes the extent from it, so a
  // grid whose boundary voxels hold the background would come back
  // trimmed. Texture space spans the extent, so a trim would silently
  // rescale every lookup: the two opposite corners pin all three axes,
  // and setting them to what they already hold changes no value.
  accessor.setValue(nanovdb::Coord(0, 0, 0), voxelGrid.fetch(0, 0, 0));
  accessor.setValue(nanovdb::Coord(extent.x - 1, extent.y - 1, extent.z - 1),
                    voxelGrid.fetch(extent.x - 1, extent.y - 1, extent.z - 1));
  // The map puts index 0 at the low corner of the world bounds rather
  // than at the first voxel center, because NanoVDB derives the world
  // bounding box it stores as the map applied to the index bounds with
  // the upper corner offset by one voxel. Writing it this way is what
  // makes `getWorldBoundMin()` and `getWorldBoundMax()` survive a round
  // trip exactly instead of drifting half a voxel per conversion.
  const auto boundMin{voxelGrid.getWorldBoundMin()};
  const auto boundMax{voxelGrid.getWorldBoundMax()};
  const double voxelSize[3]{double(boundMax.x - boundMin.x) / extent.x,
                            double(boundMax.y - boundMin.y) / extent.y,
                            double(boundMax.z - boundMin.z) / extent.z};
  if (voxelSize[0] > 0 && voxelSize[1] > 0 && voxelSize[2] > 0) {
    const double mat[3][3]{{voxelSize[0], 0.0, 0.0},
                           {0.0, voxelSize[1], 0.0},
                           {0.0, 0.0, voxelSize[2]}};
    const double invMat[3][3]{{1.0 / voxelSize[0], 0.0, 0.0},
                              {0.0, 1.0 / voxelSize[1], 0.0},
                              {0.0, 0.0, 1.0 / voxelSize[2]}};
    const double translate[3]{boundMin.x, boundMin.y, boundMin.z};
    srcGrid.mMap.set(mat, invMat, translate);
  }
  return nanovdb::tools::createNanoGrid(srcGrid);
}

#endif // #if SMDL_HAS_NANOVDB

static void saveNanoVDB(const std::string &fileName,
                        const std::vector<const VoxelGrid *> &voxelGrids,
                        const std::vector<std::string> &gridNames) {
#if SMDL_HAS_NANOVDB
  // Declared without braces on purpose: `GridHandle` has a greedy
  // constructor template, so a braced initializer resolves to the
  // vector's `initializer_list` constructor and tries to copy a handle,
  // which is deleted.
  std::vector<nanovdb::GridHandle<nanovdb::HostBuffer>> handles;
  handles.reserve(voxelGrids.size());
  for (size_t i = 0; i < voxelGrids.size(); i++)
    handles.push_back(buildNanoGrid(*voxelGrids[i], gridNames[i]));
  // Only the uncompressed codec is available: the NanoVDB dependency is
  // fetched without OpenVDB, and with it without the ZIP and Blosc that
  // the other codecs need. The file is still sparse, which is the point.
  nanovdb::io::writeGrids(fileName, handles, nanovdb::io::Codec::NONE);
#else
  (void)fileName, (void)voxelGrids, (void)gridNames;
  throw Error("built without NanoVDB!");
#endif // #if SMDL_HAS_NANOVDB
}
//--}

//--{ Mitsuba volume loading
// Load a Mitsuba `.vol` volume: a 48-byte header of magic `VOL`,
// version 3, the encoding, the extent, the channel count, and a
// world-space bounding box, followed by the values x-fastest. Only the
// single-channel `float32` encoding is supported.
static void loadMitsubaVol(const std::string &fileName, FlatGrid &flat) {
  const auto file{readOrThrow(fileName)};
  const auto mem{llvm::StringRef(file)};
  if (!(mem.size() >= 48 && mem.starts_with("VOL") && mem[3] == 3))
    throw Error("not a version-3 Mitsuba volume");
  const auto *header{reinterpret_cast<const unsigned char *>(mem.data())};
  const auto readInt32{[&](size_t offset) {
    return int32_t(llvm::support::endian::read32le(header + offset));
  }};
  const auto readFloat{[&](size_t offset) {
    const auto bits{llvm::support::endian::read32le(header + offset)};
    float value{};
    std::memcpy(&value, &bits, sizeof(value));
    return value;
  }};
  const auto encoding{readInt32(4)};
  const auto extent{int3(readInt32(8), readInt32(12), readInt32(16))};
  const auto numChannels{readInt32(20)};
  if (encoding != 1)
    throw Error(concat("unsupported Mitsuba volume encoding ", encoding,
                       "; expected 1 (float32)"));
  if (numChannels != 1)
    throw Error(concat("unsupported Mitsuba volume channel count ", numChannels,
                       "; expected 1"));
  initFlatGrid(flat, extent, /*background=*/0.0f);
  flat.worldBoundMin = float3(readFloat(24), readFloat(28), readFloat(32));
  flat.worldBoundMax = float3(readFloat(36), readFloat(40), readFloat(44));
  const auto numValues{int64_t(extent.x) * extent.y * extent.z};
  if (mem.size() < 48 + size_t(numValues) * 4)
    throw Error("Mitsuba volume file is truncated");
  const auto *values{header + 48};
  const auto fetchDense{[&](int x, int y, int z) {
    const auto bits{llvm::support::endian::read32le(
        values + 4 * (x + int64_t(extent.x) * (y + int64_t(extent.y) * z)))};
    float value{};
    std::memcpy(&value, &bits, sizeof(value));
    return value;
  }};
  // Every brick that is not uniformly background is occupied.
  auto occupied{std::vector<char>(flat.brickTable.size(), 0)};
  bool sawAnyValue{false};
  for (int bz = 0; bz < flat.brickCount.z; bz++)
    for (int by = 0; by < flat.brickCount.y; by++)
      for (int bx = 0; bx < flat.brickCount.x; bx++) {
        const int3 lo{bx * B, by * B, bz * B};
        const int3 hi{std::min(lo.x + B, flat.extent.x),
                      std::min(lo.y + B, flat.extent.y),
                      std::min(lo.z + B, flat.extent.z)};
        bool uniform{true};
        for (int z = lo.z; z < hi.z; z++)
          for (int y = lo.y; y < hi.y; y++)
            for (int x = lo.x; x < hi.x; x++) {
              const float value{fetchDense(x, y, z)};
              uniform &= value == flat.background;
              if (!sawAnyValue) {
                flat.minValue = flat.maxValue = value;
                sawAnyValue = true;
              } else {
                flat.minValue = std::min(flat.minValue, value);
                flat.maxValue = std::max(flat.maxValue, value);
              }
            }
        occupied[size_t(bx + flat.brickCount.x *
                                 (by + int64_t(flat.brickCount.y) * bz))] =
            uniform ? 0 : 1;
      }
  allocateBricks(flat, occupied);
  for (int bz = 0; bz < flat.brickCount.z; bz++)
    for (int by = 0; by < flat.brickCount.y; by++)
      for (int bx = 0; bx < flat.brickCount.x; bx++) {
        const auto tableIndex{size_t(
            bx + flat.brickCount.x * (by + int64_t(flat.brickCount.y) * bz))};
        const auto brickIndex{flat.brickTable[tableIndex]};
        if (brickIndex < 0) continue;
        float *block{&flat.brickData[size_t(brickIndex) * BRICK_VOLUME]};
        const int3 lo{bx * B, by * B, bz * B};
        const int3 hi{std::min(lo.x + B, flat.extent.x),
                      std::min(lo.y + B, flat.extent.y),
                      std::min(lo.z + B, flat.extent.z)};
        for (int z = lo.z; z < hi.z; z++)
          for (int y = lo.y; y < hi.y; y++)
            for (int x = lo.x; x < hi.x; x++)
              block[(x - lo.x) + B * ((y - lo.y) + int64_t(B) * (z - lo.z))] =
                  fetchDense(x, y, z);
      }
  finalizeValueBounds(flat, sawAnyValue);
}
//--}

//--{ Mitsuba volume saving
// Save a Mitsuba `.vol` volume, the exact form `loadMitsubaVol()` reads
// back: the 48-byte header, then every voxel of the extent x-fastest.
// The format is dense and has no background, so an empty brick is
// written out in full as the background value.
static void saveMitsubaVol(const std::string &fileName,
                           const VoxelGrid &voxelGrid) {
  auto stream{openOrThrow(fileName, std::ios::out | std::ios::binary)};
  const auto extent{voxelGrid.getExtent()};
  const auto boundMin{voxelGrid.getWorldBoundMin()};
  const auto boundMax{voxelGrid.getWorldBoundMax()};
  unsigned char header[48]{'V', 'O', 'L', 3};
  const auto writeInt32{[&](size_t offset, int32_t value) {
    llvm::support::endian::write32le(header + offset, uint32_t(value));
  }};
  const auto writeFloat{[&](size_t offset, float value) {
    uint32_t bits{};
    std::memcpy(&bits, &value, sizeof(bits));
    llvm::support::endian::write32le(header + offset, bits);
  }};
  writeInt32(4, /*encoding=*/1);
  writeInt32(8, extent.x), writeInt32(12, extent.y), writeInt32(16, extent.z);
  writeInt32(20, /*numChannels=*/1);
  writeFloat(24, boundMin.x), writeFloat(28, boundMin.y);
  writeFloat(32, boundMin.z), writeFloat(36, boundMax.x);
  writeFloat(40, boundMax.y), writeFloat(44, boundMax.z);
  stream.write(reinterpret_cast<const char *>(header), sizeof(header));
  // One row at a time, so that the buffer is the row rather than the
  // whole grid: a dense 512^3 field is half a gigabyte.
  auto row{std::vector<uint32_t>(size_t(extent.x))};
  for (int z = 0; z < extent.z; z++)
    for (int y = 0; y < extent.y; y++) {
      for (int x = 0; x < extent.x; x++) {
        const float value{voxelGrid.fetch(x, y, z)};
        uint32_t bits{};
        std::memcpy(&bits, &value, sizeof(bits));
        row[size_t(x)] =
            llvm::support::endian::byte_swap(bits, llvm::endianness::little);
      }
      stream.write(reinterpret_cast<const char *>(row.data()),
                   std::streamsize(row.size() * sizeof(uint32_t)));
    }
  if (!stream)
    throw Error(concat("cannot write ", QuotedPath(fileName), ": ",
                       std::strerror(errno)));
}
//--}

void VoxelGrid::clear() noexcept {
  mExtent = int3();
  mBrickCount = int3();
  mBackground = 0.0f;
  mMinValue = 0.0f;
  mMaxValue = 0.0f;
  mWorldBoundMin = float3();
  mWorldBoundMax = float3();
  mBrickTable.clear();
  mBrickData.clear();
  mBrickMinValues.clear();
  mBrickMaxValues.clear();
}

std::optional<Error>
VoxelGrid::loadFromFile(const std::string &fileName,
                        const std::string &gridName) noexcept {
  clear();
  auto error{catchAndReturnError([&] {
    auto flat{FlatGrid{}};
    const auto fileNameRef{llvm::StringRef(fileName)};
    if (fileNameRef.ends_with_insensitive(".nvdb")) {
      loadNanoVDB(fileName, gridName, flat);
    } else if (fileNameRef.ends_with_insensitive(".vol")) {
      if (!gridName.empty())
        throw Error(concat("Mitsuba volumes have no named grids, cannot "
                           "select ",
                           Quoted(gridName)));
      loadMitsubaVol(fileName, flat);
    } else {
      throw Error("unrecognized volume file extension");
    }
    mExtent = flat.extent;
    mBrickCount = flat.brickCount;
    mBackground = flat.background;
    mMinValue = flat.minValue;
    mMaxValue = flat.maxValue;
    mWorldBoundMin = flat.worldBoundMin;
    mWorldBoundMax = flat.worldBoundMax;
    mBrickTable = std::move(flat.brickTable);
    mBrickData = std::move(flat.brickData);
    // Compute the per-brick value bounds over the brick voxels dilated
    // by one on every side, so that the per-brick maximum bounds every
    // trilinear interpolation whose support touches the brick. This
    // goes through 'fetch()', which already resolves empty bricks and
    // out-of-extent coordinates to the background.
    mBrickMinValues.assign(mBrickTable.size(), mBackground);
    mBrickMaxValues.assign(mBrickTable.size(), mBackground);
    for (int bz = 0; bz < mBrickCount.z; bz++)
      for (int by = 0; by < mBrickCount.y; by++)
        for (int bx = 0; bx < mBrickCount.x; bx++) {
          float brickMin{fetch(bx * B - 1, by * B - 1, bz * B - 1)};
          float brickMax{brickMin};
          for (int z = bz * B - 1; z <= bz * B + B; z++)
            for (int y = by * B - 1; y <= by * B + B; y++)
              for (int x = bx * B - 1; x <= bx * B + B; x++) {
                const float value{fetch(x, y, z)};
                brickMin = std::min(brickMin, value);
                brickMax = std::max(brickMax, value);
              }
          const auto tableIndex{
              size_t(bx + mBrickCount.x * (by + int64_t(mBrickCount.y) * bz))};
          mBrickMinValues[tableIndex] = brickMin;
          mBrickMaxValues[tableIndex] = brickMax;
        }
  })};
  if (error) {
    clear();
    error->message =
        concat("cannot load ", QuotedPath(fileName), ": ", error->message);
  }
  return error;
}

std::optional<Error>
VoxelGrid::saveToFile(const std::string &fileName,
                      const std::string &gridName) const noexcept {
  auto error{catchAndReturnError([&] {
    if (!isValid()) throw Error("the grid is empty");
    const auto fileNameRef{llvm::StringRef(fileName)};
    if (fileNameRef.ends_with_insensitive(".nvdb")) {
      saveNanoVDB(fileName, {this},
                  {gridName.empty() ? std::string("density") : gridName});
    } else if (fileNameRef.ends_with_insensitive(".vol")) {
      if (!gridName.empty())
        throw Error(concat("Mitsuba volumes have no named grids, cannot name "
                           "one ",
                           Quoted(gridName)));
      saveMitsubaVol(fileName, *this);
    } else {
      throw Error("unrecognized volume file extension");
    }
  })};
  if (error)
    error->message =
        concat("cannot save ", QuotedPath(fileName), ": ", error->message);
  return error;
}

std::optional<Error>
VoxelGrid::saveToFile(const std::string &fileName,
                      const std::vector<const VoxelGrid *> &voxelGrids,
                      const std::vector<std::string> &gridNames) noexcept {
  auto error{catchAndReturnError([&] {
    if (!llvm::StringRef(fileName).ends_with_insensitive(".nvdb"))
      throw Error("several named grids need a '.nvdb' file");
    if (voxelGrids.empty()) throw Error("no grids to save");
    if (voxelGrids.size() != gridNames.size())
      throw Error(concat("have ", voxelGrids.size(), " grid(s) but ",
                         gridNames.size(), " name(s)"));
    for (size_t i = 0; i < voxelGrids.size(); i++) {
      if (!voxelGrids[i] || !voxelGrids[i]->isValid())
        throw Error(concat("grid ", i, " is empty"));
      if (gridNames[i].empty()) throw Error(concat("grid ", i, " has no name"));
      for (size_t j = 0; j < i; j++)
        if (gridNames[j] == gridNames[i])
          throw Error(
              concat("two grids are both named ", Quoted(gridNames[i])));
    }
    saveNanoVDB(fileName, voxelGrids, gridNames);
  })};
  if (error)
    error->message =
        concat("cannot save ", QuotedPath(fileName), ": ", error->message);
  return error;
}

float VoxelGrid::getBrickMinValue(int bx, int by, int bz) const noexcept {
  if (bx < 0 || bx >= mBrickCount.x || //
      by < 0 || by >= mBrickCount.y || //
      bz < 0 || bz >= mBrickCount.z)
    return mBackground;
  return mBrickMinValues[size_t(bx + mBrickCount.x *
                                         (by + int64_t(mBrickCount.y) * bz))];
}

float VoxelGrid::getBrickMaxValue(int bx, int by, int bz) const noexcept {
  if (bx < 0 || bx >= mBrickCount.x || //
      by < 0 || by >= mBrickCount.y || //
      bz < 0 || bz >= mBrickCount.z)
    return mBackground;
  return mBrickMaxValues[size_t(bx + mBrickCount.x *
                                         (by + int64_t(mBrickCount.y) * bz))];
}

float VoxelGrid::fetch(int x, int y, int z) const noexcept {
  if (x < 0 || x >= mExtent.x || //
      y < 0 || y >= mExtent.y || //
      z < 0 || z >= mExtent.z)
    return mBackground;
  const auto brickIndex{mBrickTable[size_t(
      (x / B) + mBrickCount.x * ((y / B) + int64_t(mBrickCount.y) * (z / B)))]};
  if (brickIndex < 0) return mBackground;
  return mBrickData[size_t(brickIndex) * BRICK_VOLUME + (x % B) +
                    B * ((y % B) + int64_t(B) * (z % B))];
}

float VoxelGrid::sample(float3 coord) const noexcept {
  if (!isValid()) return mBackground;
  // Texture space [0,1]^3 spans the extent with values at voxel
  // centers, so the continuous voxel-space position is offset by half:
  // this matches the 2D convention in 'tex.smdl', and the clamped
  // corner fetches below match 'wrap_clamp'. Clamping the coordinate
  // here is redundant with the corner clamping except that it keeps the
  // 'int' casts below in range for arbitrarily wild inputs.
  const float px{std::clamp(coord.x, 0.0f, 1.0f) * float(mExtent.x) - 0.5f};
  const float py{std::clamp(coord.y, 0.0f, 1.0f) * float(mExtent.y) - 0.5f};
  const float pz{std::clamp(coord.z, 0.0f, 1.0f) * float(mExtent.z) - 0.5f};
  const int ix{int(std::floor(px))};
  const int iy{int(std::floor(py))};
  const int iz{int(std::floor(pz))};
  const float fx{px - float(ix)};
  const float fy{py - float(iy)};
  const float fz{pz - float(iz)};
  const auto clampedFetch{[&](int x, int y, int z) {
    return fetch(std::clamp(x, 0, mExtent.x - 1),
                 std::clamp(y, 0, mExtent.y - 1),
                 std::clamp(z, 0, mExtent.z - 1));
  }};
  const auto lerp{[](float a, float b, float t) { return a + t * (b - a); }};
  return lerp(
      lerp(lerp(clampedFetch(ix, iy, iz), clampedFetch(ix + 1, iy, iz), fx),
           lerp(clampedFetch(ix, iy + 1, iz), clampedFetch(ix + 1, iy + 1, iz),
                fx),
           fy),
      lerp(lerp(clampedFetch(ix, iy, iz + 1), clampedFetch(ix + 1, iy, iz + 1),
                fx),
           lerp(clampedFetch(ix, iy + 1, iz + 1),
                clampedFetch(ix + 1, iy + 1, iz + 1), fx),
           fy),
      fz);
}

} // namespace smdl
