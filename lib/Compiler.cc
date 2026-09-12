#include "smdl/Compiler.h"
#include "smdl/Support/Logger.h"
#include "smdl/Support/Parallel.h"
#include "smdl/Support/Profiler.h"
#include "smdl/Support/QualifiedName.h"

#include <algorithm>
#include <bitset>
#include <chrono>
#include <cstddef>
#include <filesystem>

#include "llvm/ADT/DenseSet.h"
#include "llvm/ExecutionEngine/Orc/AbsoluteSymbols.h"
#include "llvm/ExecutionEngine/Orc/Core.h"
#include "llvm/ExecutionEngine/Orc/LLJIT.h"
#include "llvm/ExecutionEngine/Orc/Mangling.h"
#include "llvm/ExecutionEngine/Orc/ThreadSafeModule.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/Support/WithColor.h"
#include "llvm/Support/raw_ostream.h"

#include "Archive.h"
#include "Compiler/BuiltinAccess.h"
#include "Compiler/Context.h"

#if SMDL_HAS_PTEX
#include "Ptexture.h"
#endif // #if SMDL_HAS_PTEX

namespace smdl {

Compiler::Compiler(uint32_t wavelengthBaseMax)
    : wavelengthBaseMax(wavelengthBaseMax) {}

namespace {
// Sort JIT handle records by module display name, then line number.
// The display name rather than the file name, because a module that
// has no file has no file name to be distinguished by.
template <typename T> void sortByModuleAndLine(std::vector<T> &elems) {
  std::sort(elems.begin(), elems.end(), [](const auto &lhs, const auto &rhs) {
    return std::pair(std::string_view(lhs.moduleDisplayName), lhs.lineNo) <
           std::pair(std::string_view(rhs.moduleDisplayName), rhs.lineNo);
  });
}

// Visit `[itrFirst, itrLast)` runs of records that share a module,
// assuming the records are sorted by `sortByModuleAndLine`.
template <typename Iterator, typename Visitor>
void forEachModuleGroup(Iterator itr, Iterator itrEnd, Visitor &&visitor) {
  while (itr != itrEnd) {
    auto itrLast{itr};
    while (itrLast != itrEnd &&
           itrLast->moduleDisplayName == itr->moduleDisplayName)
      ++itrLast;
    visitor(itr, itrLast);
    itr = itrLast;
  }
}

// What the debug line for a loaded resource says about it. An image is
// not here: `loadImage()` only probes the file, so an image is described
// once it is decoded, at the end of `compile()`.
[[nodiscard]] std::string describeResource(const Ptexture &ptexture) {
  std::string result{concat(Counted(ptexture.channelCount, "channel"))};
#if SMDL_HAS_PTEX
  result = concat(static_cast<PtexTexture *>(ptexture.texture)->numFaces(),
                  " faces, ", result);
#endif // #if SMDL_HAS_PTEX
  return result;
}

[[nodiscard]] std::string describeResource(const BSDFMeasurement &measurement) {
  const size_t numValues{measurement.numTheta * measurement.numTheta *
                         measurement.numPhi};
  return concat(
      measurement.kind == BSDFMeasurement::KIND_REFLECTION ? "reflection"
                                                           : "transmission",
      ", ", measurement.numTheta, " x ", measurement.numTheta, " x ",
      measurement.numPhi,
      measurement.type == BSDFMeasurement::TYPE_FLOAT ? " float" : " float3",
      ", ", Bytes(numValues * BSDFMeasurement::sizeOf(measurement.type)));
}

[[nodiscard]] std::string describeResource(const LightProfile &lightProfile) {
  return concat(lightProfile.version, ", ", lightProfile.vertAngles.size(),
                " vertical x ", lightProfile.horzAngles.size(),
                " horizontal angles");
}

[[nodiscard]] std::string describeSamples(Span<const float> wavelengths) {
  if (wavelengths.empty()) return "no samples";
  return concat(Counted(wavelengths.size(), "sample"), " from ",
                Brief(wavelengths[0]), " to ",
                Brief(wavelengths[wavelengths.size() - 1]), " nm");
}

[[nodiscard]] std::string describeResource(const Spectrum &spectrum) {
  return describeSamples(SpectrumView(spectrum).wavelengths);
}

[[nodiscard]] std::string
describeResource(const SpectrumLibrary &spectrumLibrary) {
  const size_t numCurves{spectrumLibrary.getNumCurves()};
  return concat(
      Counted(numCurves, "curve"), " of ",
      describeSamples(spectrumLibrary.getCurveByIndex(0).wavelengths));
}

[[nodiscard]] std::string describeResource(const VoxelGrid &voxelGrid) {
  const int3 extent{voxelGrid.getExtent()};
  return concat(extent.x, " x ", extent.y, " x ", extent.z, " voxels, values ",
                Brief(voxelGrid.getMinValue()), " to ",
                Brief(voxelGrid.getMaxValue()), ", ",
                Bytes(voxelGrid.getSizeInBytes()));
}

[[nodiscard]] std::string describeImage(const Image &image) {
  const int numLevels{image.getNumLevels()};
  return concat(image.getNumTexelsX(), " x ", image.getNumTexelsY(), ", ",
                image.getNumChannels(), "-channel ",
                Image::getFormatName(image.getFormat()),
                numLevels > 1 ? concat(", ", numLevels, " mip levels")
                              : std::string(),
                ", ", Bytes(image.getSizeInBytes()));
}

// A duration for the log, in milliseconds below a second. The thresholds
// sit where three significant digits would round up into exponent form.
[[nodiscard]] std::string describeDuration(double seconds) {
  if (seconds < 0.9995) return concat(Brief(seconds * 1e3, 3), " ms");
  return concat(Brief(seconds, seconds < 999.5 ? 3 : 6), " s");
}

// The entries of a resource cache in file name order. The caches are keyed
// by pointer, so their own order changes from run to run, and so would any
// log that followed it.
template <typename Map>
[[nodiscard]] auto sortedByFileName(const Map &resources) {
  using Resource = typename Map::mapped_type::element_type;
  std::vector<std::pair<const MD5FileHash *, Resource *>> entries{};
  entries.reserve(resources.size());
  for (const auto &[key, resource] : resources)
    entries.emplace_back(key, resource.get());
  std::sort(entries.begin(), entries.end(),
            [](const auto &lhs, const auto &rhs) {
              return lhs.first->canonicalFileNames[0] <
                     rhs.first->canonicalFileNames[0];
            });
  return entries;
}

// The material name nearest `materialName`, or empty if none is close.
// Each material is spelled with as many components as `materialName` has,
// so that a typo in a bare name suggests a bare name. The materials the
// desired-material filter skipped count too, since a misspelled desired
// name is exactly what leaves the intended material skipped.
[[nodiscard]] std::string suggestMaterialName(const Compiler &compiler,
                                              std::string_view materialName) {
  const bool isAbsolute{materialName.substr(0, 2) == "::"};
  const size_t numComponents{splitQualifiedName(materialName).size()};
  std::vector<std::string> spellings{};
  auto addSpelling{[&](std::string_view qualifiedName) {
    if (isAbsolute) {
      spellings.emplace_back(qualifiedName);
      return;
    }
    std::vector<std::string_view> components{splitQualifiedName(qualifiedName)};
    components.erase(components.begin(),
                     components.end() -
                         ptrdiff_t(std::min(components.size(), numComponents)));
    spellings.emplace_back(joinQualifiedName(components)).erase(0, 2);
  }};
  for (const auto &jitMaterial : compiler.getMaterials())
    if (!jitMaterial.moduleIsShadowed) addSpelling(jitMaterial.qualifiedName);
  for (const auto &skippedName : compiler.getSkippedMaterialNames())
    addSpelling(skippedName);
  const std::vector<std::string_view> candidates{
      std::vector<std::string_view>(spellings.begin(), spellings.end())};
  return std::string(suggestNearestName(materialName, candidates));
}
} // namespace

Compiler::~Compiler() = default;

void Ptexture::release() noexcept {
#if SMDL_HAS_PTEX
  if (texture) static_cast<PtexTexture *>(texture)->release();
#endif // #if SMDL_HAS_PTEX
  texture = nullptr;
  channelCount = 0;
  alphaIndex = -1;
}

namespace {
// Parse the dot-separated package prefix encoded by an MDL archive
// file name per the MDL specification, e.g., `vendor.metals.mdr`
// encodes `{"vendor", "metals"}`. Throws on empty components.
[[nodiscard]]
std::vector<std::string>
parseArchivePackagePrefix(const std::string &fileName) {
  std::string stem{std::filesystem::path(fileName).stem().string()};
  llvm::SmallVector<llvm::StringRef> components{};
  llvm::StringRef(stem).split(components, '.');
  std::vector<std::string> prefix{};
  for (auto component : components) {
    if (component.empty())
      throw Error(concat("invalid archive name ", QuotedPath(fileName),
                         ": empty package prefix component"));
    prefix.push_back(component.str());
  }
  return prefix;
}

// Does the archive entry conform to the package prefix encoded by the
// archive file name? A conforming `.mdl` entry is either the enclosed
// module itself (`vendor/metals.mdl` for the prefix
// `{"vendor", "metals"}`) or anywhere under the enclosed package
// directory (`vendor/metals/...`).
[[nodiscard]]
bool isConformingArchiveEntry(const std::vector<std::string> &prefix,
                              const std::string &entryName) {
  llvm::SmallVector<llvm::StringRef> components{};
  llvm::StringRef(entryName).split(components, '/');
  if (components.size() == prefix.size())
    return std::equal(prefix.begin(), prefix.end() - 1, components.begin()) &&
           components.back() == prefix.back() + ".mdl";
  return components.size() > prefix.size() &&
         std::equal(prefix.begin(), prefix.end(), components.begin());
}

// Is `parent` a lexical ancestor directory of `child`? Assumes both
// paths are already canonical. Equal paths do not count.
[[nodiscard]]
bool isLexicalSubPath(const std::string &parent, const std::string &child) {
  std::filesystem::path parentPath{parent};
  std::filesystem::path childPath{child};
  auto [parentItr, childItr] =
      std::mismatch(parentPath.begin(), parentPath.end(), //
                    childPath.begin(), childPath.end());
  return parentItr == parentPath.end() && childItr != childPath.end();
}
} // namespace

void Compiler::registerModule(std::unique_ptr<Module> loadedModule,
                              std::vector<std::string> *addedModuleNames) {
  Module &module_{*mModules.emplace_back(std::move(loadedModule))};
  if (!module_.getFileName().empty()) {
    mModuleFileNames.emplace(std::string(module_.getFileName()), &module_);
  }
  std::string qualifiedName{module_.getQualifiedName()};
  if (auto [itr, inserted] =
          mModulesByQualifiedName.try_emplace(qualifiedName, &module_);
      !inserted) {
    // The earliest search root wins for qualified-name lookup. The
    // module still loads and compiles, and relative imports within
    // its own tree still resolve to it.
    module_.mIsShadowed = true;
    SMDL_LOG_WARN("module ", Quoted(qualifiedName), " in ",
                  QuotedPath(module_.getDisplayName()), " is shadowed by ",
                  QuotedPath(itr->second->getDisplayName()));
  }
  if (addedModuleNames) {
    addedModuleNames->push_back(std::move(qualifiedName));
  }
}

namespace {
// Normalize a host-supplied module name to an absolute qualified name,
// or throw an `Error` explaining why it is not a legal module name. It
// must be spellable in an `import`, so every component is an ordinary
// identifier.
[[nodiscard]]
std::string normalizeModuleName(const std::string &moduleName) {
  const auto isIdentifier{[](std::string_view component) {
    const auto isLetter{[](char ch) {
      return ch == '_' || (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z');
    }};
    if (component.empty() || !isLetter(component[0])) return false;
    return std::all_of(component.begin(), component.end(), [&](char ch) {
      return isLetter(ch) || (ch >= '0' && ch <= '9');
    });
  }};
  std::vector<std::string_view> components{splitQualifiedName(moduleName)};
  if (components.empty()) {
    throw Error(concat("module name ", Quoted(moduleName), " is empty"));
  }
  for (const auto &component : components) {
    if (!isIdentifier(component)) {
      throw Error(concat("module name ", Quoted(moduleName), " has component ",
                         Quoted(component), " that is not an identifier"));
    }
  }
  return joinQualifiedName(components);
}
} // namespace

std::optional<Error> Compiler::addCode(std::string moduleName,
                                       std::string sourceCode,
                                       std::string anchorDirectory) noexcept {
  SMDL_PROFILER_ENTRY("Compiler::addCode()", moduleName.c_str());
  return catchAndReturnError([&] {
    std::string qualifiedName{normalizeModuleName(moduleName)};
    if (auto itr{mModulesByQualifiedName.find(qualifiedName)};
        itr != mModulesByQualifiedName.end()) {
      // Adding the same source code under the same name again is a
      // no-op, so a host may register its defaults defensively. Any
      // other clash is a name the host does not actually own, which is
      // a host bug rather than the search-root accident that 'add()'
      // resolves by shadowing.
      if (itr->second->isFromSourceCode() &&
          itr->second->getSourceCode() == sourceCode) {
        return;
      }
      throw Error(concat("cannot add module ", Quoted(qualifiedName),
                         ": the name is already taken by ",
                         QuotedPath(itr->second->getDisplayName())));
    }
    // An absolute import resolves builtins first, so a module named
    // after one compiles but is unreachable by qualified name.
    Span<const std::string_view> builtinNames{builtin::getAllNames()};
    if (std::any_of(builtinNames.begin(), builtinNames.end(),
                    [&](std::string_view builtinName) {
                      return joinQualifiedName(splitQualifiedName(
                                 builtinName)) == qualifiedName;
                    }))
      SMDL_LOG_WARN("module ", Quoted(qualifiedName),
                    " has the same name as a builtin module, so imports "
                    "of that name resolve to the builtin");
    if (!anchorDirectory.empty()) {
      if (!isDirectory(anchorDirectory)) {
        throw Error(concat("cannot add module ", Quoted(qualifiedName),
                           ": the anchor ", QuotedPath(anchorDirectory),
                           " is not an existing directory"));
      }
      anchorDirectory = makePathCanonical(std::move(anchorDirectory));
    }
    SMDL_LOG_DEBUG("Adding MDL source code as ", Quoted(qualifiedName));
    registerModule(Module::loadFromSourceCode(
                       qualifiedName, std::move(sourceCode), anchorDirectory),
                   nullptr);
  });
}

std::optional<Error>
Compiler::add(std::string fileOrDirName,
              std::vector<std::string> *addedModuleNames) noexcept {
  SMDL_PROFILER_ENTRY("Compiler::add()", fileOrDirName.c_str());
  // The filesystem iterators, 'Archive', and 'Module::loadFromFile' all
  // throw; catch everything so the 'optional<Error>' contract holds.
  return catchAndReturnError([&] {
    auto addMDLE{[&](const std::string &fileName) {
      SMDL_LOG_DEBUG("Adding MDLE ", QuotedPath(fileName));
      // An MDLE is a self-contained encapsulated material. Identity
      // is content-based: the qualified name is '::mdle::<md5>' of
      // the container bytes, so identical containers at different
      // paths dedupe to one module and distinct containers can never
      // collide.
      std::string contentHash{std::string(MD5Hash::hashFile(fileName))};
      std::string qualifiedName{"::mdle::" + contentHash};
      if (auto itr{mModulesByQualifiedName.find(qualifiedName)};
          itr != mModulesByQualifiedName.end()) {
        if (addedModuleNames) {
          addedModuleNames->push_back(std::move(qualifiedName));
        }
        return;
      }
      // Load 'main.mdl' and extract every other entry into a
      // content-addressed cache directory that serves as the anchor
      // for the module's resource lookups.
      std::string extractDir{(std::filesystem::temp_directory_path() /
                              ("smdl-mdle-" + contentHash))
                                 .string()};
      Archive archive{fileName};
      std::optional<std::string> mainSource{};
      int numExtracted{0};
      for (int i = 0; i < archive.get_file_count(); i++) {
        std::string entryName{archive.get_file_name(i)};
        if (entryName == "main.mdl") {
          mainSource = archive.extract_file(i);
        } else if (!entryName.empty() && entryName.back() != '/') {
          std::filesystem::path outPath{std::filesystem::path(extractDir) /
                                        entryName};
          std::filesystem::create_directories(outPath.parent_path());
          openOrThrow(outPath.string(), std::ios::out | std::ios::binary)
              << archive.extract_file(i);
          numExtracted++;
        }
      }
      if (!mainSource) {
        throw Error(concat("MDLE ", QuotedPath(fileName),
                           " does not contain 'main.mdl'"));
      }
      SMDL_LOG_DEBUG("Extracted ", Counted(numExtracted, "resource"),
                     " of MDLE ", QuotedPath(fileName), " into ",
                     QuotedPath(extractDir));
      registerModule(Module::loadFromMDLE(fileName, *mainSource, qualifiedName,
                                          extractDir),
                     addedModuleNames);
    }};
    auto addArchive{[&](const std::string &fileName,
                        const std::string &searchRoot) {
      SMDL_LOG_DEBUG("Adding MDL archive ", QuotedPath(fileName));
      // Per the MDL specification, the archive file name encodes the
      // enclosed package prefix: 'vendor.metals.mdr' provides
      // '::vendor::metals', and every '.mdl' entry must be the
      // enclosed module ('vendor/metals.mdl') or live under the
      // enclosed package directory ('vendor/metals/...').
      std::vector<std::string> prefix{parseArchivePackagePrefix(fileName)};
      {
        // Duplicating the enclosed contents as loose files in the
        // same search root is an error.
        std::string loosePath{searchRoot};
        for (const auto &component : prefix) {
          loosePath = joinPaths(loosePath, component);
        }
        if (isDirectory(loosePath) || isFile(loosePath + ".mdl") ||
            isFile(loosePath + ".smdl")) {
          throw Error(concat("archive ", QuotedPath(fileName),
                             " conflicts with loose contents at ",
                             QuotedPath(loosePath),
                             " in the same search root"));
        }
      }
      Archive archive{fileName};
      for (int i = 0; i < archive.get_file_count(); i++) {
        if (std::string entryName{archive.get_file_name(i)};
            hasExtension(entryName, ".mdl")) {
          if (!isConformingArchiveEntry(prefix, entryName)) {
            throw Error(concat(
                "archive ", QuotedPath(fileName), " entry ", Quoted(entryName),
                " does not conform to the package prefix encoded "
                "by the archive file name"));
          }
          if (std::string entryPath{joinPaths(fileName, entryName)};
              mModuleFileNames.count(entryPath) == 0) {
            SMDL_LOG_DEBUG("Adding MDL file from archive ",
                           QuotedPath(entryPath));
            registerModule(
                Module::loadFromFileExtractedFromArchive(
                    fileName, entryName, archive.extract_file(i), searchRoot),
                addedModuleNames);
          }
        }
      }
    }};
    auto addLooseFile{
        [&](const std::string &fileName, const std::string &searchRoot) {
          if (auto itr{mModuleFileNames.find(fileName)};
              itr == mModuleFileNames.end()) {
            SMDL_LOG_DEBUG("Adding MDL file ", QuotedPath(fileName));
            registerModule(Module::loadFromFile(fileName, searchRoot),
                           addedModuleNames);
          } else if (itr->second->getSearchRoot() != searchRoot) {
            // Already added under a different search root: the first
            // identity wins.
            SMDL_LOG_WARN("module file ", QuotedPath(fileName),
                          " was already added as ",
                          Quoted(itr->second->getQualifiedName()),
                          "; keeping the existing identity");
          }
        }};
    auto addFile{
        [&](const std::string &fileName, const std::string &searchRoot) {
          if (llvm::StringRef(fileName).ends_with_insensitive(".mdle")) {
            addMDLE(fileName);
          } else if (llvm::StringRef(fileName).ends_with_insensitive(".mdr")) {
            addArchive(fileName, searchRoot);
          } else {
            addLooseFile(fileName, searchRoot);
          }
        }};
    if (std::optional<std::string> maybePath{fileLocator.locate(
            fileOrDirName, {},
            FileLocator::REGULAR_FILES | FileLocator::DIRS)}) {
      std::string &path{*maybePath};
      if (isFile(path)) {
        addFile(path, parentPathOf(path));
        return;
      } else if (isDirectory(path)) {
        if (mModuleDirNames.count(path) != 0) {
          // Re-adding the same search root is a no-op.
          return;
        }
        for (const auto &dir : mModuleDirSearchPaths) {
          if (isLexicalSubPath(dir, path) || isLexicalSubPath(path, dir)) {
            throw Error(
                concat("cannot add search root ", QuotedPath(path),
                       ": nested inside or encloses another search root ",
                       QuotedPath(dir),
                       " (would give modules ambiguous qualified names)"));
          }
        }
        // Collect the top-level archives, sorted so registration order
        // is deterministic, and reject overlapping package prefixes:
        // per the MDL specification, 'a.b.mdr' and 'a.b.c.mdr' must not
        // coexist in the same search root (siblings like 'a.c.mdr' are
        // fine).
        std::vector<std::string> archivePaths{};
        for (const auto &entry : std::filesystem::directory_iterator(path)) {
          if (std::string entryPath{makePathCanonical(entry.path().string())};
              isFile(entryPath) && hasExtension(entryPath, ".mdr")) {
            archivePaths.push_back(std::move(entryPath));
          }
        }
        std::sort(archivePaths.begin(), archivePaths.end());
        for (size_t i = 0; i < archivePaths.size(); i++) {
          std::vector<std::string> prefixI{
              parseArchivePackagePrefix(archivePaths[i])};
          for (size_t j = i + 1; j < archivePaths.size(); j++) {
            std::vector<std::string> prefixJ{
                parseArchivePackagePrefix(archivePaths[j])};
            if (size_t n{std::min(prefixI.size(), prefixJ.size())};
                std::equal(prefixI.begin(), prefixI.begin() + long(n),
                           prefixJ.begin())) {
              throw Error(concat(
                  "archives ", //
                  QuotedPath(archivePaths[i]), " and ",
                  QuotedPath(archivePaths[j]),
                  " have overlapping package prefixes in the same search "
                  "root"));
            }
          }
        }
        SMDL_LOG_DEBUG("Adding MDL directory ", QuotedPath(path));
        mModuleDirNames.insert(path);
        mModuleDirSearchPaths.emplace_back(path);
        for (const auto &archivePath : archivePaths) {
          addFile(archivePath, path);
        }
        for (const auto &entry :
             std::filesystem::recursive_directory_iterator(path)) {
          if (std::string entryPath{makePathCanonical(entry.path().string())};
              isFile(entryPath)) {
            if (hasExtension(entryPath, ".mdl") ||
                hasExtension(entryPath, ".smdl")) {
              addFile(entryPath, path);
            } else if (hasExtension(entryPath, ".mdr") &&
                       !isPathEquivalent(parentPathOf(entryPath), path)) {
              // Per the MDL specification, archives are only recognized
              // at the top level of a search root.
              SMDL_LOG_WARN("ignoring archive ", QuotedPath(entryPath),
                            " because it is not at the top level of search "
                            "root ",
                            QuotedPath(path));
            }
          }
        }
        return;
      }
    }
    throw Error(concat("cannot locate ", Quoted(fileOrDirName)));
  });
}

namespace {
// The bytes of a 'State' that are the same at every point of one path
// inside one medium instance: the wavelength grid and its weights, the
// units, the animation time, the object transform, the transport mode,
// and the allocator that '#bump' reads. Every other byte is point-varying,
// padding included, so a new 'State' field is point-varying until it is
// listed here. The animation time is constant because every instance a
// path scatters in is evaluated at that path's time; 'tangentToObject' is
// not, because 'State::finalize()' moves the surface point into its last
// column.
const std::bitset<sizeof(State)> &pathConstantStateBytes() {
  static const std::bitset<sizeof(State)> bytes{[] {
    std::bitset<sizeof(State)> bits{};
    auto allow{[&](size_t offset, size_t size) {
      for (size_t i = offset; i < offset + size; i++) bits.set(i);
    }};
#define ALLOW(name) allow(offsetof(State, name), sizeof(State::name))
    ALLOW(allocator);
    ALLOW(wavelengthBase);
    ALLOW(wavelengthMin);
    ALLOW(wavelengthMax);
    ALLOW(wavelengthWeight);
    ALLOW(metersPerSceneUnit);
    ALLOW(animationTime);
    ALLOW(objectId);
    ALLOW(objectToWorld);
    ALLOW(transport);
#undef ALLOW
    return bits;
  }()};
  return bytes;
}

// Does every byte of the 'State' that a function reads through its pointer
// argument 'arg' lie in 'pathConstantStateBytes()'? This walks the uses of
// the argument after optimization: a constant GEP is followed at its
// offset, a load or a constant-length memory read through the pointer is
// checked against the allowed bytes, and a call to a function defined in
// the module is followed into that parameter, so the RGB and spectral
// conversions, which are 'noinline' and read only 'wavelengthBase', are
// seen through. Anything else, a write, an escape, a variable index, a
// call to a declaration (a scene-data getter or a '@(foreign)' function,
// both of which see the whole state), makes the answer false, which hosts
// read as heterogeneous: the walk is conservative by construction, and an
// argument with no uses at all is the empty read set and true. Loaded
// values are never followed: a load of 'wavelengthBase' is a read of that
// field, and the reads through the pointer it holds are not state reads.
[[nodiscard]] bool readsOnlyPathConstantState(const llvm::DataLayout &layout,
                                              const llvm::Argument *arg) {
  const std::bitset<sizeof(State)> &allowed{pathConstantStateBytes()};
  auto isAllowed{[&](uint64_t offset, uint64_t size) {
    if (offset + size > sizeof(State)) return false;
    for (uint64_t i = offset; i < offset + size; i++)
      if (!allowed.test(size_t(i))) return false;
    return true;
  }};
  llvm::SmallVector<std::pair<const llvm::Value *, uint64_t>> worklist{
      {arg, 0}};
  llvm::DenseSet<std::pair<const llvm::Value *, uint64_t>> visited{};
  while (!worklist.empty()) {
    const auto [ptr, offset] = worklist.pop_back_val();
    if (!visited.insert({ptr, offset}).second) continue;
    for (const llvm::Use &use : ptr->uses()) {
      const llvm::User *user{use.getUser()};
      if (const auto *gep{llvm::dyn_cast<llvm::GetElementPtrInst>(user)}) {
        llvm::APInt gepOffset(64, 0);
        if (gep->getPointerOperand() != ptr ||
            !gep->accumulateConstantOffset(layout, gepOffset) ||
            gepOffset.isNegative())
          return false;
        worklist.push_back({gep, offset + gepOffset.getZExtValue()});
      } else if (const auto *load{llvm::dyn_cast<llvm::LoadInst>(user)}) {
        if (!isAllowed(offset, layout.getTypeStoreSize(load->getType())))
          return false;
      } else if (const auto *transfer{
                     llvm::dyn_cast<llvm::MemTransferInst>(user)}) {
        const auto *length{
            llvm::dyn_cast<llvm::ConstantInt>(transfer->getLength())};
        if (transfer->getRawSource() != ptr || transfer->getRawDest() == ptr ||
            !length || !isAllowed(offset, length->getZExtValue()))
          return false;
      } else if (const auto *intrinsic{
                     llvm::dyn_cast<llvm::IntrinsicInst>(user)}) {
        switch (intrinsic->getIntrinsicID()) {
        case llvm::Intrinsic::lifetime_start:
        case llvm::Intrinsic::lifetime_end:
        case llvm::Intrinsic::invariant_start:
        case llvm::Intrinsic::invariant_end:
        case llvm::Intrinsic::assume:
          break;
        default:
          return false;
        }
      } else if (llvm::isa<llvm::ICmpInst>(user)) {
        // A comparison of the pointer itself reads nothing through it.
      } else if (const auto *call{llvm::dyn_cast<llvm::CallBase>(user)}) {
        const llvm::Function *callee{call->getCalledFunction()};
        if (!callee || callee->isDeclaration() || !call->isArgOperand(&use) ||
            call->getArgOperandNo(&use) >= callee->arg_size())
          return false;
        worklist.push_back(
            {callee->getArg(call->getArgOperandNo(&use)), offset});
      } else {
        return false;
      }
    }
  }
  return true;
}

// Derive the value-dependent static material flags after optimization.
//
// `FunctionType::initializeMaterialFunctions` fills the type-level
// (`#isDefault`-derived) bits of `staticFlags`/`staticFlagsKnown` at
// emit time and also emits, per material, the `.opacityEvaluate` entry
// point and a `.thinWalledProbe` scaffolding function. After the
// optimizer runs, a body that reduces to returning a constant proves the
// corresponding flag bit for every possible instance, so it is marked
// known here; a body that stays runtime (or an unoptimized module) just
// leaves the bit unknown, which hosts must treat conservatively.
void deriveStaticMaterialFlags(llvm::Module &llvmModule,
                               std::vector<JIT::MaterialDef> &materials) {
  // If every 'ret' in the named function returns one identical constant,
  // return it, else null.
  auto foldedReturnValue{[&](std::string_view name) -> const llvm::Constant * {
    llvm::Function *func{llvmModule.getFunction(name)};
    if (!func || func->isDeclaration()) return nullptr;
    const llvm::Constant *uniqueConst{};
    for (auto &block : *func) {
      if (llvm::ReturnInst *
          ret{llvm::dyn_cast<llvm::ReturnInst>(block.getTerminator())}) {
        llvm::Constant *retConst{
            llvm::dyn_cast_if_present<llvm::Constant>(ret->getReturnValue())};
        if (!retConst || (uniqueConst && uniqueConst != retConst))
          return nullptr;
        uniqueConst = retConst;
      }
    }
    return uniqueConst;
  }};
  for (auto &jitMaterial : materials) {
    // Recover the symbol base from the evaluate-opacity entry point name,
    // '<symbolBase>.opacityEvaluate'.
    std::string_view symbolBase{
        std::string_view(jitMaterial.opacityEvaluate.name)};
    SMDL_SANITY_CHECK(
        llvm::StringRef(symbolBase).ends_with(".opacityEvaluate"));
    symbolBase.remove_suffix(std::string_view(".opacityEvaluate").size());
    if (const llvm::ConstantFP *opacity{
            llvm::dyn_cast_if_present<llvm::ConstantFP>(
                foldedReturnValue(jitMaterial.opacityEvaluate.name))}) {
      jitMaterial.staticFlagsKnown |= MATERIAL_HAS_CUTOUT;
      if (opacity->getValueAPF().convertToFloat() < 1.0f)
        jitMaterial.staticFlags |= MATERIAL_HAS_CUTOUT;
    }
    std::string thinWalledProbeName{concat(symbolBase, ".thinWalledProbe")};
    if (const llvm::ConstantInt *isThinWalled{
            llvm::dyn_cast_if_present<llvm::ConstantInt>(
                foldedReturnValue(thinWalledProbeName))}) {
      jitMaterial.staticFlagsKnown |= MATERIAL_THIN_WALLED;
      if (!isThinWalled->isZero())
        jitMaterial.staticFlags |= MATERIAL_THIN_WALLED;
    }
    // The displacement probe returns 'geometry.displacement' itself, so
    // a body folded to a constant vector settles
    // 'MATERIAL_HAS_DISPLACEMENT': known, and set iff the constant is
    // not the zero vector (-0.0 counts as zero).
    // A body that did not fold leaves the bit unknown, which hosts
    // treat as possibly displacing. See
    // 'JIT::MaterialDef::hasZeroDisplacement()'.
    std::string displacementProbeName{concat(symbolBase, ".displacementProbe")};
    if (const llvm::Constant *displacement{
            foldedReturnValue(displacementProbeName)}) {
      jitMaterial.staticFlagsKnown |= MATERIAL_HAS_DISPLACEMENT;
      if (!llvmIsZeroValue(displacement))
        jitMaterial.staticFlags |= MATERIAL_HAS_DISPLACEMENT;
    }
    // The normal probe returns 'geometry.normal - $state.normal', which
    // folds to the constant zero vector exactly when the material
    // leaves the shading normal alone, settling
    // 'MATERIAL_REMAPS_NORMAL' the way the displacement probe settles
    // its flag. See 'JIT::MaterialDef::canRemapNormal()'.
    std::string normalProbeName{concat(symbolBase, ".normalProbe")};
    if (const llvm::Constant *normalDelta{foldedReturnValue(normalProbeName)}) {
      jitMaterial.staticFlagsKnown |= MATERIAL_REMAPS_NORMAL;
      if (!llvmIsZeroValue(normalDelta))
        jitMaterial.staticFlags |= MATERIAL_REMAPS_NORMAL;
    }
    // A material with no volume is trivially point-independent, and so
    // is a '.volumeEvaluate' body that reads nothing of its '%state'
    // argument but the fields constant along a path (an RGB constant
    // reads the wavelength grid, a texel fetch nothing at all). Either
    // way mark 'MATERIAL_HAS_HETEROGENEOUS_COEFFICIENTS' known and
    // unset. Otherwise the bit stays unknown rather than set, because
    // the state use may be incidental (every load at 'OPT_LEVEL_NONE',
    // or an un-removable side-effecting call such as a scene-data
    // lookup anywhere in the material body); hosts treat unknown as
    // heterogeneous, which is the conservative direction. See
    // 'JIT::MaterialDef::hasHomogeneousCoefficients()'.
    llvm::Function *volumeEvaluateFunc{
        llvmModule.getFunction(jitMaterial.volumeEvaluate.name)};
    if (!(jitMaterial.staticFlags & MATERIAL_HAS_VOLUME) ||
        (volumeEvaluateFunc && !volumeEvaluateFunc->isDeclaration() &&
         volumeEvaluateFunc->arg_size() >= 1 &&
         readsOnlyPathConstantState(llvmModule.getDataLayout(),
                                    volumeEvaluateFunc->getArg(0)))) {
      jitMaterial.staticFlagsKnown |= MATERIAL_HAS_HETEROGENEOUS_COEFFICIENTS;
    }
    // The probes are compile-time scaffolding, not host entry points;
    // erase them so they are never JIT-compiled.
    if (llvm::Function * probeFunc{llvmModule.getFunction(thinWalledProbeName)})
      probeFunc->eraseFromParent();
    if (llvm::Function *
        probeFunc{llvmModule.getFunction(displacementProbeName)})
      probeFunc->eraseFromParent();
    if (llvm::Function * probeFunc{llvmModule.getFunction(normalProbeName)})
      probeFunc->eraseFromParent();
  }
}
} // namespace

// An image enters the IR as exactly one external symbol (see
// `Context::getImageTexelBase()`), so liveness is exactly whether that
// symbol still has uses: the optimizer erases the references along with
// the code that made them, and nothing else in the module can name the
// texels. This finds images at `OPT_LEVEL_NONE` too, where nothing is
// dead-code eliminated but constant-field elimination keeps a comptime
// `texture_2d` out of the IR, so a texture read only for its extent
// never references its symbol in the first place.
size_t Compiler::dropUnusedImages() {
  size_t numDropped{0};
  for (auto [fileHash, image] : sortedByFileName(mImages)) {
    auto itr{mImageSymbolNames.find(image)};
    if (itr == mImageSymbolNames.end()) continue;
    // Absent as well as unused: an image loaded by a texture that failed
    // to construct never reached `getImageTexelBase()` at all.
    llvm::GlobalVariable *llvmGlobal{mLLVMModule->getNamedGlobal(itr->second)};
    if (llvmGlobal) {
      // The comptime `texture_2d` aggregate holding the symbol is a
      // constant, and a constant that nothing in the module reaches is
      // still a use until it is collected. Without this, an image that
      // only ever contributed its extent looks read.
      llvmGlobal->removeDeadConstantUsers();
      if (!llvmGlobal->use_empty()) continue;
    }
    SMDL_LOG_DEBUG("Dropping image ",
                   QuotedPath(fileHash->canonicalFileNames[0]),
                   ": never read by the compiled code");
    if (llvmGlobal) llvmGlobal->eraseFromParent();
    image->abandonLoad();
    mImageSymbolNames.erase(itr);
    numDropped++;
  }
  return numDropped;
}

namespace {
// Describe an error the JIT execution session reports. A symbol missing
// because the host process does not define a '@(foreign)' function is
// described at the function's declaration.
[[nodiscard]] std::vector<Error>
describeJITSessionError(llvm::Error error, char globalPrefix,
                        const std::unordered_map<std::string, SourceLocation>
                            &foreignFunctionSourceLocations) {
  std::vector<Error> errors{};
  llvm::handleAllErrors(
      std::move(error),
      [&](const llvm::orc::SymbolsNotFound &notFound) {
        std::string otherNames{};
        for (const auto &symbol : notFound.getSymbols()) {
          llvm::StringRef name{*symbol};
          if (globalPrefix != '\0')
            name.consume_front(llvm::StringRef(&globalPrefix, 1));
          if (auto itr{foreignFunctionSourceLocations.find(name.str())};
              itr != foreignFunctionSourceLocations.end()) {
            const SourceLocation &srcLoc{itr->second};
            errors.emplace_back(srcLoc.formatMessage(concat(
                                    "'@(foreign)' function ", Quoted(name),
                                    " is not defined in the host process")),
                                srcLoc.getSourceSnippet());
          } else {
            if (!otherNames.empty()) otherNames += ", ";
            otherNames += concat(Quoted(name));
          }
        }
        if (!otherNames.empty())
          errors.emplace_back(
              concat("JIT session error: symbols not found: ", otherNames));
      },
      [&](const llvm::ErrorInfoBase &info) {
        errors.emplace_back(concat("JIT session error: ", info.message()));
      });
  return errors;
}
} // namespace

void Compiler::resetForRecompile() {
  // Free the previous JIT first: this invalidates every function pointer
  // previously handed out, per the lifetime contract on the class.
  mLLVMJit.reset();
  mForeignFunctionSourceLocations.clear();
  mJITSessionErrors.clear();
  mWarnedResourceKeys.clear();
  mImages.clear();
  mImageMipRequesters.clear();
  mImageSymbolNames.clear();
  mPtextures.clear();
  mBSDFMeasurements.clear();
  mLightProfiles.clear();
  mVoxelGrids.clear();
  mSpectrums.clear();
  mSpectrumLibraries.clear();
  mBuiltinCalleeAddresses.clear();
  mRGBToColor.func = nullptr;
  mColorToRGB.func = nullptr;
  mSkippedMaterialNames.clear();
  mMaterialDefs.clear();
  mUnitTests.clear();
  mExecs.clear();
  mLLVMContext = std::make_unique<llvm::LLVMContext>();
  mLLVMModule = std::make_unique<llvm::Module>("MDL", *mLLVMContext);
  mLLVMModule->setTargetTriple(
      llvm::Triple(llvm::StringRef(NativeTarget::get().triple)));
  mLLVMModule->setDataLayout(NativeTarget::get().machine->createDataLayout());
  // Be explicit that the JIT links against the host process's own symbols:
  // '@(foreign)' declarations and emitted libcalls (e.g. 'strncmp')
  // resolve via 'dlsym' on the current process.
  mLLVMJit = llvmThrowIfError(
      llvm::orc::LLJITBuilder().setLinkProcessSymbolsByDefault(true).create());
  mLLVMJit->getExecutionSession().setErrorReporter([this](llvm::Error error) {
    for (auto &sessionError : describeJITSessionError(
             std::move(error), mLLVMJit->getDataLayout().getGlobalPrefix(),
             mForeignFunctionSourceLocations)) {
      if (mIsJITCompiling) {
        mJITSessionErrors.push_back(std::move(sessionError));
      } else {
        sessionError.print();
      }
    }
  });
}

std::optional<Error> Compiler::compile(OptLevel optLevel) noexcept {
  SMDL_PROFILER_ENTRY("Compiler::compile()");
  // The 'Context' constructor and the emit phase can throw; catch
  // everything so the 'optional<Error>' contract holds instead of exiting
  // or terminating the host process.
  return catchAndReturnError([&] {
    resetForRecompile();
    ProfilerEntry *initializeEntry{profilerEntryBegin("Initialize")};
    Context context{*this};
    for (auto &module_ : mModules) module_->reset();
    profilerEntryEnd(initializeEntry);
    {
      SMDL_PROFILER_ENTRY("Parse AST");
      for (auto &module_ : mModules)
        if (std::optional<Error> error{module_->parse(mAllocator)})
          throw std::move(*error);
    }
    {
      SMDL_PROFILER_ENTRY("Emit LLVM-IR");
      for (auto &module_ : mModules)
        if (std::optional<Error> error{module_->compile(context)})
          throw std::move(*error);
    }
    // Sort JIT materials and unit tests by module and line number in
    // case we want to print them later.
    sortByModuleAndLine(mMaterialDefs);
    sortByModuleAndLine(mUnitTests);
    // Warn about desired material names that matched nothing at all, so
    // a typo does not silently skip the material it meant to keep.
    for (const auto &desiredName : mDesiredMaterialNames) {
      if (std::none_of(mMaterialDefs.begin(), mMaterialDefs.end(),
                       [&](const auto &jitMaterial) {
                         return matchesMaterialName(desiredName,
                                                    jitMaterial.qualifiedName);
                       })) {
        std::string suggestion{suggestMaterialName(*this, desiredName)};
        SMDL_LOG_WARN("desired material ", Quoted(desiredName),
                      " does not match any material in the added modules",
                      suggestion.empty()
                          ? std::string()
                          : concat("; did you mean ", Quoted(suggestion), "?"));
      }
    }
    if (optLevel != OPT_LEVEL_NONE) {
      SMDL_PROFILER_ENTRY("Optimize LLVM-IR");
      LLVMOptimizer llvmOptimizer{};
      llvmOptimizer.run(*mLLVMModule, optLevel == OPT_LEVEL_O1
                                          ? llvm::OptimizationLevel::O1
                                      : optLevel == OPT_LEVEL_O2
                                          ? llvm::OptimizationLevel::O2
                                          : llvm::OptimizationLevel::O3);
    }
    deriveStaticMaterialFlags(*mLLVMModule, mMaterialDefs);
    // Drop the images the optimizer proved unread before decoding the
    // rest, which is why decoding waits until here: the drop decision
    // needs the optimized module, after 'deriveStaticMaterialFlags' has
    // erased the probe scaffolding whose references must not keep an
    // image alive.
    const size_t numDropped{dropUnusedImages()};
    // Finish loading the images that still have a decode pending, i.e.,
    // neither failed 'startLoad()' nor were dropped above.
    std::vector<std::pair<const MD5FileHash *, Image *>> imageEntries{
        sortedByFileName(mImages)};
    imageEntries.erase(std::remove_if(imageEntries.begin(), imageEntries.end(),
                                      [](const auto &entry) {
                                        return !entry.second->hasPendingLoad();
                                      }),
                       imageEntries.end());
    if (!imageEntries.empty()) {
      SMDL_PROFILER_ENTRY("Load images in parallel");
      const std::chrono::time_point<
          std::chrono::steady_clock,
          std::chrono::duration<long, std::ratio<1, 1000000000>>>
          startTime{std::chrono::steady_clock::now()};
      // A decode failure must not unwind out of 'parallelFor'; the image
      // keeps its pre-allocated (zeroed) texels, matching the 'loadImage'
      // policy. The workers only record what happened, and the log is
      // written afterward in file name order, so that it reads the same
      // from run to run.
      std::vector<std::optional<Error>> errors{
          std::vector<std::optional<Error>>(imageEntries.size())};
      parallelFor(0, imageEntries.size(), [&](size_t i) {
        SMDL_PROFILER_ENTRY(
            "Load image", imageEntries[i].first->canonicalFileNames[0].c_str());
        errors[i] =
            catchAndReturnError([&] { imageEntries[i].second->finishLoad(); });
      });
      const double seconds{std::chrono::duration<double>(
                               std::chrono::steady_clock::now() - startTime)
                               .count()};
      size_t numLoaded{0};
      size_t numBytes{0};
      for (size_t i = 0; i < imageEntries.size(); i++) {
        const std::string &fileName{
            imageEntries[i].first->canonicalFileNames[0]};
        const Image &image{*imageEntries[i].second};
        if (errors[i]) {
          SMDL_LOG_WARN("cannot load ", QuotedPath(fileName), ": ",
                        errors[i]->message);
          continue;
        }
        SMDL_LOG_DEBUG("Loaded image ", QuotedPath(fileName), ": ",
                       describeImage(image));
        numLoaded++;
        numBytes += image.getSizeInBytes();
      }
      if (numLoaded > 0) {
        SMDL_LOG_INFO("Loaded ", Counted(numLoaded, "image"), " (",
                      Bytes(numBytes), ") in ", describeDuration(seconds),
                      numDropped > 0
                          ? concat(", skipping ", numDropped,
                                   " that the compiled code never reads")
                          : std::string());
      }
    }
  });
}

std::optional<Error>
Compiler::formatSourceFiles(const FormatOptions &formatOptions) noexcept {
  SMDL_PROFILER_ENTRY("Compiler::formatSourceFiles()");
  for (auto &module_ : mModules) {
    if (module_->isFileBacked()) {
      if (std::optional<Error> error{module_->formatSourceFiles(formatOptions)})
        return error;
    }
  }
  return std::nullopt;
}

std::optional<Error> Compiler::extractDocs(DocDatabase &docs) noexcept {
  SMDL_PROFILER_ENTRY("Compiler::extractDocs()");
  return catchAndReturnError([&] {
    for (auto &module_ : mModules) {
      if (module_->isShadowed()) continue;
      if (std::optional<Error> error{module_->parse(mAllocator)})
        throw std::move(*error);
      docs.modules.push_back(extractDocModule(*module_));
    }
  });
}

llvm::LLVMContext &Compiler::getLLVMContext() {
  if (!mLLVMContext)
    throw Error("no LLVM context: 'compile()' must be called first (and "
                "'jitCompile()' consumes it)");
  return *mLLVMContext;
}

llvm::Module &Compiler::getLLVMModule() {
  if (!mLLVMModule)
    throw Error("no LLVM module: 'compile()' must be called first (and "
                "'jitCompile()' consumes it)");
  return *mLLVMModule;
}

namespace {
// Look up the resource for the given key in `resources`, running `loader`
// exactly once per distinct key. The key is the content hash of the file,
// possibly extended with load parameters (see `mImages`). A load failure
// is a warning, not an error: the resource stays default-constructed and
// rendering continues. A load that succeeds says what it loaded at debug
// level, naming `part` too if the file holds more than one thing to load.
template <typename K, typename T, typename Hash, typename Eq, typename Loader>
T &loadResource(std::unordered_map<K, std::unique_ptr<T>, Hash, Eq> &resources,
                const K &key, const SourceLocation &srcLoc,
                const std::string &fileName, Loader &&loader,
                std::string_view part = {}) {
  auto [itr, inserted] = resources.try_emplace(key);
  if (inserted) {
    itr->second = std::make_unique<T>();
    if (std::optional<Error> error{
            std::invoke(std::forward<Loader>(loader), *itr->second)}) {
      srcLoc.logWarn(error->message);
    } else if constexpr (!std::is_same_v<T, Image>) {
      if (Logger::get().isEnabled(LOG_LEVEL_DEBUG))
        srcLoc.logDebug(concat("Loaded ", QuotedPath(fileName),
                               part.empty() ? "" : " ", part, ": ",
                               describeResource(*itr->second)));
    }
  }
  return *itr->second;
}
} // namespace

bool Compiler::logResourceWarningOnce(const SourceLocation &srcLoc,
                                      const std::string &key,
                                      std::string_view message) {
  if (!mWarnedResourceKeys.insert(key).second) return false;
  srcLoc.logWarn(message);
  return true;
}

const Image &Compiler::loadImage(const std::string &fileName,
                                 const SourceLocation &srcLoc,
                                 bool useMipLevels, Image::MipFilter filter) {
  const MD5FileHash *fileHash{mFileHasher[fileName]};
  Image &image{
      loadResource(mImages, fileHash, srcLoc, fileName, [&](Image &image) {
        SMDL_PROFILER_ENTRY("Compiler::loadImage()", fileName.c_str());
        // Probes the file and nothing more: the texels are allocated and
        // decoded at the end of the compile, by which point every reference
        // has been seen and the level count is settled.
        return image.startLoad(fileName);
      })};
  // Named by content hash, so every reference to the same file resolves
  // to one symbol and one set of texels.
  mImageSymbolNames.try_emplace(
      &image, concat("smdl.image.", std::string(fileHash->hash)));
  // The request is applied on every reference, not just the one that
  // decoded the image, so that it does not matter which reference comes
  // first: the mip levels are generated at the end of the compile, by
  // which point every reference has been seen.
  if (useMipLevels) {
    auto [itr, inserted] = mImageMipRequesters.try_emplace(&image, srcLoc);
    if (!image.requestMipLevels(filter)) {
      auto filterName{[](Image::MipFilter f) {
        return f == Image::MIP_MAX ? "maximum" : "mean";
      }};
      srcLoc.throwError(
          "cannot request a ", filterName(filter), " mip chain for ",
          QuotedPath(fileName), ": a ", filterName(image.getMipFilter()),
          " mip chain was requested at ", std::string(itr->second),
          ", and an image holds one chain");
    }
  }
  return image;
}

const Ptexture &Compiler::loadPtexture(const std::string &fileName,
                                       const SourceLocation &srcLoc) {
  return loadResource(
      mPtextures, mFileHasher[fileName], srcLoc, fileName,
      [&](Ptexture &ptexture) -> std::optional<Error> {
#if SMDL_HAS_PTEX
        SMDL_PROFILER_ENTRY("Compiler::loadPtexture()", fileName.c_str());
        Ptex::String message{};
        PtexTexture *texture{PtexTexture::open(fileName.c_str(), message,
                                               /*premultiply=*/false)};
        if (!texture)
          return Error(concat("cannot load ", QuotedPath(fileName), ": ",
                              message.c_str()));
        ptexture.texture = texture;
        ptexture.channelCount = texture->numChannels();
        ptexture.alphaIndex = texture->alphaChannel();
        return std::nullopt;
#else
        return Error(concat("cannot load ", QuotedPath(fileName),
                            ": built without ptex!"));
#endif // #if SMDL_HAS_PTEX
      });
}

const BSDFMeasurement &
Compiler::loadBSDFMeasurement(const std::string &fileName,
                              const SourceLocation &srcLoc) {
  return loadResource(mBSDFMeasurements, mFileHasher[fileName], srcLoc,
                      fileName, [&](BSDFMeasurement &bsdfMeasurement) {
                        SMDL_PROFILER_ENTRY("Compiler::loadBSDFMeasurement()",
                                            fileName.c_str());
                        return bsdfMeasurement.loadFromFile(fileName);
                      });
}

const LightProfile &Compiler::loadLightProfile(const std::string &fileName,
                                               const SourceLocation &srcLoc) {
  return loadResource(mLightProfiles, mFileHasher[fileName], srcLoc, fileName,
                      [&](LightProfile &lightProfile) {
                        SMDL_PROFILER_ENTRY("Compiler::loadLightProfile()",
                                            fileName.c_str());
                        return lightProfile.loadFromFile(fileName);
                      });
}

const VoxelGrid &Compiler::loadVoxelGrid(const std::string &fileName,
                                         const std::string &gridName,
                                         const SourceLocation &srcLoc) {
  return loadResource(
      mVoxelGrids, std::pair(mFileHasher[fileName], gridName), srcLoc, fileName,
      [&](VoxelGrid &voxelGrid) {
        SMDL_PROFILER_ENTRY("Compiler::loadVoxelGrid()", fileName.c_str());
        return voxelGrid.loadFromFile(fileName, gridName);
      },
      gridName.empty() ? std::string() : concat("grid ", Quoted(gridName)));
}

SpectrumView Compiler::loadSpectrum(const std::string &fileName,
                                    const SourceLocation &srcLoc) {
  return SpectrumView(loadResource(
      mSpectrums, mFileHasher[fileName], srcLoc, fileName,
      [&](Spectrum &spectrum) {
        SMDL_PROFILER_ENTRY("Compiler::loadSpectrum()", fileName.c_str());
        return spectrum.loadFromFile(fileName);
      }));
}

// A library that failed to load has no curves, and has said so already, so
// the two lookups below only speak up about a library with curves to find.
SpectrumView Compiler::loadSpectrum(const std::string &fileName, int curveIndex,
                                    const SourceLocation &srcLoc) {
  const SpectrumLibrary &spectrumLibrary{loadSpectrumLibrary(fileName, srcLoc)};
  SpectrumView spectrumView{spectrumLibrary.getCurveByIndex(curveIndex)};
  if (const size_t numCurves{spectrumLibrary.getNumCurves()};
      spectrumView.curveValues.empty() && numCurves > 0) {
    logResourceWarningOnce(srcLoc, concat(fileName, "\n", curveIndex),
                           concat("spectrum library ", QuotedPath(fileName),
                                  " has no curve at index ", curveIndex,
                                  " (it has ", Counted(numCurves, "curve"),
                                  ")"));
  }
  return spectrumView;
}

SpectrumView Compiler::loadSpectrum(const std::string &fileName,
                                    const std::string &curveName,
                                    const SourceLocation &srcLoc) {
  const SpectrumLibrary &spectrumLibrary{loadSpectrumLibrary(fileName, srcLoc)};
  SpectrumView spectrumView{spectrumLibrary.getCurveByName(curveName)};
  if (spectrumView.curveValues.empty() && spectrumLibrary.getNumCurves() > 0) {
    std::string message{concat("spectrum library ", QuotedPath(fileName),
                               " has no curve named ", Quoted(curveName))};
    const Span<const std::string> curveNames{spectrumLibrary.getCurveNames()};
    if (curveNames.empty()) {
      message += " (its curves are unnamed)";
    } else {
      const std::vector<std::string_view> candidates{
          std::vector<std::string_view>(curveNames.begin(), curveNames.end())};
      if (std::string_view similar{suggestNearestName(curveName, candidates)};
          !similar.empty())
        message += concat("; did you mean ", Quoted(similar), "?");
    }
    logResourceWarningOnce(srcLoc, concat(fileName, "\n", curveName), message);
  }
  return spectrumView;
}

const SpectrumLibrary &
Compiler::loadSpectrumLibrary(const std::string &fileName,
                              const SourceLocation &srcLoc) {
  return loadResource(mSpectrumLibraries, mFileHasher[fileName], srcLoc,
                      fileName, [&](SpectrumLibrary &spectrumLibrary) {
                        SMDL_PROFILER_ENTRY("Compiler::loadSpectrum()",
                                            fileName.c_str());
                        return spectrumLibrary.loadFromFile(fileName);
                      });
}

std::optional<Error> Compiler::dump(DumpFormat dumpFormat,
                                    std::string &out) noexcept {
  return catchAndReturnError([&] {
    if (dumpFormat == DUMP_FORMAT_IR) {
      llvm::raw_string_ostream os{out};
      os << getLLVMModule();
    } else {
      llvm::SmallVector<char> str{};
      llvm::raw_svector_ostream os{str};
      llvm::legacy::PassManager passManager{};
      if (NativeTarget::get().machine->addPassesToEmitFile(
              passManager, os, nullptr,
              dumpFormat == DUMP_FORMAT_ASM
                  ? llvm::CodeGenFileType::AssemblyFile
                  : llvm::CodeGenFileType::ObjectFile))
        throw Error("cannot emit assembly or object code for the native "
                    "target");
      // The codegen passes mutate the IR, so run them on a clone to keep
      // the module later handed to the JIT pristine.
      std::unique_ptr<llvm::Module> clonedModule{
          llvm::CloneModule(getLLVMModule())};
      passManager.run(*clonedModule);
      out = std::string(os.str());
    }
  });
}

std::optional<Error> Compiler::jitCompile() noexcept {
  SMDL_PROFILER_ENTRY("Compiler::jit_compile()");
  mIsJITCompiling = true;
  std::optional<Error> error{catchAndReturnError([&] {
    if (!mLLVMJit || !mLLVMModule || !mLLVMContext)
      throw Error("nothing to JIT-compile: 'compile()' must be called first");
    // Define the builtin runtime callees ('smdlPanic', 'smdlBumpAllocate',
    // ...) as absolute symbols so they resolve even when the host process
    // does not export its own symbols (e.g. static link without
    // '--export-dynamic'), and the image texel bases likewise: emitted
    // code names those by symbol rather than by address, so this is where
    // their addresses are finally committed.
    if (!mBuiltinCalleeAddresses.empty() || !mImageSymbolNames.empty()) {
      llvm::orc::MangleAndInterner mangle{mLLVMJit->getExecutionSession(),
                                          mLLVMJit->getDataLayout()};
      llvm::orc::SymbolMap symbolMap{};
      for (const auto &[calleeName, calleeAddr] : mBuiltinCalleeAddresses)
        symbolMap[mangle(calleeName)] = llvm::orc::ExecutorSymbolDef(
            llvm::orc::ExecutorAddr::fromPtr(calleeAddr),
            llvm::JITSymbolFlags::Exported | llvm::JITSymbolFlags::Callable);
      for (const auto &[image, symbolName] : mImageSymbolNames)
        symbolMap[mangle(symbolName)] = llvm::orc::ExecutorSymbolDef(
            llvm::orc::ExecutorAddr::fromPtr(image->getTexels()),
            llvm::JITSymbolFlags::Exported);
      llvmThrowIfError(mLLVMJit->getMainJITDylib().define(
          llvm::orc::absoluteSymbols(std::move(symbolMap))));
    }
    // Hand the module to the JIT, dropping our handles up front: a failed
    // call must not leave moved-from state behind for 'dump()' or
    // 'getLLVMModule()' to trip over.
    llvm::orc::ThreadSafeModule llvmJitModule{std::move(mLLVMModule),
                                              std::move(mLLVMContext)};
    llvmThrowIfError(mLLVMJit->addIRModule(std::move(llvmJitModule)));
    jitLookup(mColorToRGB);
    jitLookup(mRGBToColor);
    for (auto &jitMaterial : mMaterialDefs) {
      jitLookup(jitMaterial.evaluate);
      jitLookup(jitMaterial.opacityEvaluate);
      jitLookup(jitMaterial.displacementEvaluate);
      jitLookup(jitMaterial.volumeEvaluate);
      jitLookup(jitMaterial.scatterEvaluate);
      jitLookup(jitMaterial.scatterSample);
      // Emitted only when the host asked for them; see
      // 'Compiler::shouldEmitScatterNormal'.
      if (!jitMaterial.scatterNormalSample.name.empty()) {
        jitLookup(jitMaterial.scatterNormalEvaluate);
        jitLookup(jitMaterial.scatterNormalSample);
        jitLookup(jitMaterial.geometryNormalEvaluate);
      }
      jitLookup(jitMaterial.emissionEvaluate);
      jitLookup(jitMaterial.emissionSample);
      jitLookup(jitMaterial.volumeScatterEvaluate);
      jitLookup(jitMaterial.volumeScatterSample);
      jitLookup(jitMaterial.hairScatterEvaluate);
      jitLookup(jitMaterial.hairScatterSample);
    }
    for (auto &jitUnitTest : mUnitTests) jitLookup(jitUnitTest.test);
    for (auto &jitExec : mExecs) jitLookup(jitExec);
    // Deallocate everything we no longer need!
    for (auto &mod : mModules) mod->reset();
    mAllocator.reset();
  })};
  mIsJITCompiling = false;
  mForeignFunctionSourceLocations.clear();
  std::vector<Error> sessionErrors{std::move(mJITSessionErrors)};
  mJITSessionErrors.clear();
  if (!error) {
    for (const auto &sessionError : sessionErrors) sessionError.print();
  } else if (!sessionErrors.empty()) {
    // A session error is the cause, and the lookup that failed is only
    // how it surfaced, so the session errors lead.
    Error cause{std::move(sessionErrors.front())};
    for (size_t i = 1; i < sessionErrors.size(); i++)
      cause.message += concat("\n  ", sessionErrors[i].message);
    cause.message += concat("\n  ", error->message);
    error = std::move(cause);
  }
  return error;
}

void *Compiler::jitLookup(std::string_view name) {
  llvm::Expected<llvm::orc::ExecutorAddr> symbol{mLLVMJit->lookup(name)};
  if (!symbol)
    throw Error(concat("cannot resolve JIT symbol ", Quoted(name), ": ",
                       llvm::toString(symbol.takeError())));
  return symbol->toPtr<void *>();
}

const JIT::MaterialDef *
Compiler::findMaterial(std::string_view materialName) const noexcept try {
  std::vector<const JIT::MaterialDef *> results{findMaterials(materialName)};
  return results.size() == 1 ? results.front() : nullptr;
} catch (...) {
  return nullptr;
}

std::string
Compiler::explainMaterialLookup(std::string_view materialName) const {
  std::vector<const JIT::MaterialDef *> results{findMaterials(materialName)};
  if (results.size() == 1) return {};
  if (results.size() > 1) {
    std::string message{concat("material name ", Quoted(materialName),
                               " is ambiguous, matching ", results.size(),
                               " materials:")};
    for (const auto *jitMaterial : results)
      message += concat(
          "\n  ", Quoted(jitMaterial->qualifiedName), " declared at ",
          LocationMarkup(jitMaterial->moduleDisplayName, jitMaterial->lineNo,
                         /*charNo=*/0, !jitMaterial->moduleFileName.empty()));
    return message;
  }
  // Distinguish "never existed" from "excluded by the desired-material
  // filter", so a host that forgot a name gets an actionable error.
  for (const auto &skippedName : mSkippedMaterialNames)
    if (matchesMaterialName(materialName, skippedName))
      return concat("material name ", Quoted(materialName), " matches ",
                    Quoted(skippedName),
                    ", which was not compiled because it is not a desired "
                    "material (see 'Compiler::setDesiredMaterials()')");
  std::string message{concat("no material matches ", Quoted(materialName))};
  if (std::string suggestion{suggestMaterialName(*this, materialName)};
      !suggestion.empty())
    message += concat("; did you mean ", Quoted(suggestion), "?");
  return message;
}

std::vector<const JIT::MaterialDef *>
Compiler::findMaterials(std::string_view materialName) const {
  std::vector<const JIT::MaterialDef *> results{};
  for (const auto &jitMaterial : mMaterialDefs) {
    if (!jitMaterial.moduleIsShadowed &&
        matchesMaterialName(materialName, jitMaterial.qualifiedName))
      results.push_back(&jitMaterial);
  }
  return results;
}

bool Compiler::matchesMaterialName(std::string_view materialName,
                                   std::string_view qualifiedName) noexcept {
  return materialName.substr(0, 2) == "::"
             ? qualifiedName == materialName
             : isQualifiedNameSuffix(materialName, qualifiedName);
}

float3 Compiler::convertColorToRGB(const State &state,
                                   const float *color) const noexcept {
  SMDL_SANITY_CHECK(mColorToRGB && color);
  SMDL_SANITY_CHECK(state.wavelengthBase != nullptr);
  float3 rgb{};
  mColorToRGB(state, color, rgb);
  return rgb;
}

void Compiler::convertRGBToColor(const State &state, const float3 &rgb,
                                 float *color) const noexcept {
  SMDL_SANITY_CHECK(mRGBToColor && color);
  SMDL_SANITY_CHECK(state.wavelengthBase != nullptr);
  mRGBToColor(state, rgb, color);
}

namespace {
// The color scheme of the unit test results, sharing the vocabulary of
// the `doc` subcommand's text printer: identity in blue, the name being
// reported in cyan, and metadata in grey, plus green and red for the
// results themselves.
constexpr llvm::HighlightColor testColorFile{llvm::HighlightColor::Tag};
constexpr llvm::HighlightColor testColorName{llvm::HighlightColor::Attribute};
constexpr llvm::HighlightColor testColorMetadata{llvm::HighlightColor::Note};
constexpr llvm::HighlightColor testColorSuccess{llvm::HighlightColor::String};
constexpr llvm::HighlightColor testColorFailure{llvm::HighlightColor::Error};
} // namespace

std::optional<Error> Compiler::runUnitTests(const State &state) noexcept {
  return catchAndReturnError([&] {
    // NOTE: Print through `llvm::errs()` rather than `std::cerr` so that
    // `WithColor` can colorize. The mode is resolved here rather than
    // left to its own detection, so that the report and the log sink
    // agree on whether standard error is colored. Both streams write to
    // file descriptor 2 unbuffered, so this stays correctly interleaved
    // with the logger, which still prints through `std::cerr`.
    //
    // NOTE: Each colored span opens and closes before the test runs, so
    // that a test that crashes cannot leave the terminal colored.
    llvm::raw_fd_ostream &os{llvm::errs()};
    const llvm::ColorMode llvmColorMode{
        shouldUseColors(ansiColorMode, cerrSupportsANSIColors())
            ? llvm::ColorMode::Enable
            : llvm::ColorMode::Disable};
    forEachModuleGroup(
        mUnitTests.begin(), mUnitTests.end(), [&](auto itr0, auto itr1) {
          os << "Running tests in ";
          llvm::WithColor(os, testColorFile, llvmColorMode)
              << concat(QuotedPath(itr0->moduleDisplayName));
          os << ":\n";
          for (; itr0 != itr1; ++itr0) {
            os << "  ";
            llvm::WithColor(os, testColorName, llvmColorMode)
                << concat(Quoted(itr0->testName));
            llvm::WithColor(os, testColorMetadata, llvmColorMode)
                << concat(" (line ", itr0->lineNo, ")");
            os << " ... ";
            try {
              if (!itr0->test)
                throw Error(concat("unit test ", Quoted(itr0->testName),
                                   " has no JIT-compiled function"));
              itr0->test(state);
              llvm::WithColor(os, testColorSuccess, llvmColorMode) << "success";
              os << '\n';
            } catch (const Error &) {
              llvm::WithColor(os, testColorFailure, llvmColorMode) << "failure";
              os << '\n';
              throw;
            }
          }
          os << '\n';
        });
  });
}

std::optional<Error> Compiler::runExecs() noexcept {
  return catchAndReturnError([&] {
    for (auto &jitExec : mExecs) {
      if (!jitExec.func)
        throw Error(concat("exec ", Quoted(jitExec.name),
                           " has no JIT-compiled function: 'jitCompile()' "
                           "must be called first"));
      jitExec();
    }
  });
}

std::string Compiler::printMaterialSummary() const {
  // Summarize the statically known, shadow-relevant flags: the cutout
  // opacity status ('opaque' proven, 'cutout' proven, 'cutout?' only
  // knowable at runtime), plus 'volume' and 'emissive' when present.
  auto printStaticFlags{[](const JIT::MaterialDef &jitMaterial) {
    std::string flags{};
    if ((jitMaterial.staticFlagsKnown & MATERIAL_HAS_CUTOUT) == 0)
      flags += " [cutout?";
    else if ((jitMaterial.staticFlags & MATERIAL_HAS_CUTOUT) != 0)
      flags += " [cutout";
    else
      flags += " [opaque";
    if (jitMaterial.hasVolume()) flags += ", volume";
    if ((jitMaterial.staticFlags &
         (MATERIAL_HAS_SURFACE_EMISSION | MATERIAL_HAS_BACKFACE_EMISSION)) != 0)
      flags += ", emissive";
    flags += ']';
    return flags;
  }};
  std::string message{};
  forEachModuleGroup(
      mMaterialDefs.begin(), mMaterialDefs.end(), [&](auto itr0, auto itr1) {
        message += concat(QuotedPath(itr0->moduleDisplayName), " contains ",
                          Counted(size_t(itr1 - itr0), "material"), ":\n");
        for (; itr0 != itr1; ++itr0) {
          message += "  ";
          message += concat(Quoted(itr0->materialName), " (line ", itr0->lineNo,
                            ")", printStaticFlags(*itr0), "\n");
        }
      });
  return message;
}

} // namespace smdl

#if SMDL_HAS_PTEX
namespace {

// Per-thread Ptex filters: 'PtexFilter::eval' mutates filter members, so
// render threads must not share one filter instance. Each filter holds a
// reference on its 'PtexTexture', so a cached filter stays memory-safe
// even after the compiler releases the texture on recompile.
class ThreadLocalPtexFilters final {
public:
  ~ThreadLocalPtexFilters() {
    for (auto &[texture, filter] : mFilters) filter->release();
  }
  [[nodiscard]] SMDL_NO_INLINE PtexFilter *get(const smdl::Ptexture &ptex) {
    PtexTexture *texture{static_cast<PtexTexture *>(ptex.texture)};
    PtexFilter *&filter{mFilters[texture]};
    if (SMDL_UNLIKELY(!filter))
      filter = PtexFilter::getFilter(
          texture, PtexFilter::Options(PtexFilter::f_bilinear));
    return filter;
  }

private:
  std::unordered_map<const void *, PtexFilter *> mFilters{};
};

} // namespace
#endif // #if SMDL_HAS_PTEX

extern "C" {

SMDL_EXPORT void smdlPtexEvaluate(const void *state,
                                  const ::smdl::Ptexture *ptex, int gamma,
                                  int first, int num, float *out) {
  SMDL_SANITY_CHECK(state != nullptr);
  SMDL_SANITY_CHECK(out != nullptr);
  std::fill_n(out, num, 0.0f);
#if SMDL_HAS_PTEX
  thread_local ThreadLocalPtexFilters filters{};
  const smdl::State &smdlState{*static_cast<const smdl::State *>(state)};
  if (ptex && ptex->texture && first < ptex->channelCount) {
    num = std::min(num, int(ptex->channelCount - first));
    filters.get(*ptex)->eval(out, first, num, smdlState.ptexFaceId,
                             smdlState.ptexFaceUV.x, smdlState.ptexFaceUV.y,
                             /*uw1=*/0.0f, /*vw1=*/0.0f,
                             /*uw2=*/0.0f, /*vw2=*/0.0f,
                             /*width=*/1.0f, /*blur=*/0.0f);
    if (gamma == 1) { // sRGB?
      for (int i = 0; i < num; i++) {
        int channel{first + i};
        if (channel != ptex->alphaIndex) {
          // The piecewise sRGB decoding per IEC 61966-2-1, matching
          // 'decodeSRGB' in 'Builtin/tex.smdl'.
          float value{out[i]};
          out[i] = value <= 0.04045f
                       ? value * (1.0f / 12.92f)
                       : std::pow((value + 0.055f) * (1.0f / 1.055f), 2.4f);
        }
      }
    }
  }
#endif // #if SMDL_HAS_PTEX
}

} // extern "C"
