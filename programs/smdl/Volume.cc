#include <filesystem>
#include <memory>
#include <string>
#include <vector>

#include "llvm/ADT/StringRef.h"
#include "llvm/Support/raw_ostream.h"

#include "smdl/Resource/VoxelGrid.h"
#include "smdl/Support/Error.h"
#include "smdl/Support/Filesystem.h"
#include "smdl/Support/Strings.h"

#include "Options.h"
#include "Volume.h"

namespace {

[[nodiscard]] bool isNanoVDBFileName(llvm::StringRef fileName) {
  return fileName.ends_with_insensitive(".nvdb");
}

// Describe what one voxel grid file holds. This mode earns its place
// beside the conversion: the maximum is what `tex::max_value()` returns
// for the grid and the world bounds are what `density_bound_min` and
// `density_bound_max` want, so it is where a hand-written volume
// material gets its numbers.
void printVolumeInfo(const std::string &fileName, const std::string &gridName,
                     const smdl::VoxelGrid &grid) {
  const smdl::int3 extent{grid.getExtent()};
  const smdl::int3 brickCount{grid.getBrickCount()};
  const smdl::float3 boundMin{grid.getWorldBoundMin()};
  const smdl::float3 boundMax{grid.getWorldBoundMax()};
  llvm::outs() << smdl::concat(
      smdl::bestPathForPrinting(fileName),
      gridName.empty() ? std::string()
                       : smdl::concat(": grid ", smdl::Quoted(gridName)),
      "\n  extent ", extent.x, " x ", extent.y, " x ", extent.z, " (",
      brickCount.x, " x ", brickCount.y, " x ", brickCount.z, " bricks)",
      "\n  background ", grid.getBackground(), //
      "\n  values ", grid.getMinValue(), " to ", grid.getMaxValue(),
      "\n  bounds [", boundMin.x, ", ", boundMin.y, ", ", boundMin.z, "] to [",
      boundMax.x, ", ", boundMax.y, ", ", boundMax.z, "]\n");
  llvm::outs().flush();
}

} // namespace

// '-grid' names the grid to READ from a NanoVDB input and the grid to
// WRITE into a NanoVDB output, which is what lets one flag carry every
// combination of the two formats. A Mitsuba volume holds one anonymous
// grid, so the name reaches it in neither direction.
void runVolume(const Options &opts) {
  const auto explicitName{[&](size_t i) {
    return i < opts.volume.gridNames.size() ? opts.volume.gridNames[i]
                                            : std::string();
  }};
  std::vector<std::unique_ptr<smdl::VoxelGrid>> grids{};
  std::vector<std::string> writeNames{};
  for (size_t i = 0; i < opts.inputs.size(); i++) {
    const std::string &fileName{opts.inputs[i]};
    std::unique_ptr<smdl::VoxelGrid> grid{std::make_unique<smdl::VoxelGrid>()};
    // An unnamed NanoVDB input reads its first grid, so the name only
    // travels when it was actually asked for.
    const std::string readName{isNanoVDBFileName(fileName) ? explicitName(i)
                                                           : std::string()};
    if (std::optional<smdl::Error> error{
            grid->loadFromFile(fileName, readName)})
      error->printAndExit();
    std::string writeName{explicitName(i)};
    if (writeName.empty())
      writeName = std::filesystem::path(fileName).stem().string();
    grids.push_back(std::move(grid));
    writeNames.push_back(std::move(writeName));
  }
  const std::string &output{opts.volume.fileName};
  if (output.empty()) {
    for (size_t i = 0; i < grids.size(); i++)
      printVolumeInfo(opts.inputs[i], explicitName(i), *grids[i]);
    return;
  }
  if (grids.size() == 1) {
    if (std::optional<smdl::Error> error{grids[0]->saveToFile(
            output, isNanoVDBFileName(output) ? writeNames[0] : std::string())})
      error->printAndExit();
    return;
  }
  std::vector<const smdl::VoxelGrid *> pointers{};
  for (const auto &grid : grids) pointers.push_back(grid.get());
  if (std::optional<smdl::Error> error{
          smdl::VoxelGrid::saveToFile(output, pointers, writeNames)})
    error->printAndExit();
}
