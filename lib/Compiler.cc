#include "smdl/Compiler.h"
#include "smdl/Support/Logger.h"
#include "smdl/Support/Parallel.h"
#include "smdl/Support/Profiler.h"
#include "smdl/Support/QualifiedName.h"

#include <algorithm>
#include <chrono>
#include <filesystem>

#include "llvm/ExecutionEngine/Orc/AbsoluteSymbols.h"
#include "llvm/ExecutionEngine/Orc/LLJIT.h"
#include "llvm/ExecutionEngine/Orc/Mangling.h"
#include "llvm/ExecutionEngine/Orc/ThreadSafeModule.h"
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

// Sort JIT handle records by module display name, then line number.
// The display name rather than the file name, because a module that
// has no file has no file name to be distinguished by.
template <typename T> static void sortByModuleAndLine(std::vector<T> &elems) {
  std::sort(elems.begin(), elems.end(), [](const auto &lhs, const auto &rhs) {
    return std::pair(std::string_view(lhs.moduleDisplayName), lhs.lineNo) <
           std::pair(std::string_view(rhs.moduleDisplayName), rhs.lineNo);
  });
}

// Visit `[itrFirst, itrLast)` runs of records that share a module,
// assuming the records are sorted by `sortByModuleAndLine`.
template <typename Iterator, typename Visitor>
static void forEachModuleGroup(Iterator itr, Iterator itrEnd,
                               Visitor &&visitor) {
  while (itr != itrEnd) {
    auto itrLast{itr};
    while (itrLast != itrEnd &&
           itrLast->moduleDisplayName == itr->moduleDisplayName)
      ++itrLast;
    visitor(itr, itrLast);
    itr = itrLast;
  }
}

Compiler::~Compiler() = default;

void Ptexture::release() noexcept {
#if SMDL_HAS_PTEX
  if (textureFilter) static_cast<PtexFilter *>(textureFilter)->release();
  if (texture) static_cast<PtexTexture *>(texture)->release();
#endif // #if SMDL_HAS_PTEX
  texture = nullptr;
  textureFilter = nullptr;
  channelCount = 0;
  alphaIndex = -1;
}

// Parse the dot-separated package prefix encoded by an MDL archive
// file name per the MDL specification, e.g., `vendor.metals.mdr`
// encodes `{"vendor", "metals"}`. Throws on empty components.
[[nodiscard]]
static std::vector<std::string>
parseArchivePackagePrefix(const std::string &fileName) {
  auto stem{std::filesystem::path(fileName).stem().string()};
  auto components{llvm::SmallVector<llvm::StringRef>{}};
  llvm::StringRef(stem).split(components, '.');
  auto prefix{std::vector<std::string>()};
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
static bool isConformingArchiveEntry(const std::vector<std::string> &prefix,
                                     const std::string &entryName) {
  auto components{llvm::SmallVector<llvm::StringRef>{}};
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
static bool isLexicalSubPath(const std::string &parent,
                             const std::string &child) {
  auto parentPath{std::filesystem::path(parent)};
  auto childPath{std::filesystem::path(child)};
  auto [parentItr, childItr] =
      std::mismatch(parentPath.begin(), parentPath.end(), //
                    childPath.begin(), childPath.end());
  return parentItr == parentPath.end() && childItr != childPath.end();
}

void Compiler::registerModule(std::unique_ptr<Module> loadedModule,
                              std::vector<std::string> *addedModuleNames) {
  auto &module_{*mModules.emplace_back(std::move(loadedModule))};
  if (!module_.getFileName().empty()) {
    mModuleFileNames.emplace(std::string(module_.getFileName()), &module_);
  }
  auto qualifiedName{std::string(module_.getQualifiedName())};
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

// Normalize a host-supplied module name to an absolute qualified name,
// or throw an `Error` explaining why it is not a legal module name. It
// must be spellable in an `import`, so every component is an ordinary
// identifier.
[[nodiscard]]
static std::string normalizeModuleName(const std::string &moduleName) {
  const auto isIdentifier{[](std::string_view component) {
    const auto isLetter{[](char ch) {
      return ch == '_' || (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z');
    }};
    if (component.empty() || !isLetter(component[0])) return false;
    return std::all_of(component.begin(), component.end(), [&](char ch) {
      return isLetter(ch) || (ch >= '0' && ch <= '9');
    });
  }};
  auto components{splitQualifiedName(moduleName)};
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

std::optional<Error> Compiler::addCode(std::string moduleName,
                                       std::string sourceCode,
                                       std::string anchorDirectory) noexcept {
  SMDL_PROFILER_ENTRY("Compiler::addCode()", moduleName.c_str());
  return catchAndReturnError([&] {
    auto qualifiedName{normalizeModuleName(moduleName)};
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
    auto builtinNames{builtin::getAllNames()};
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
      auto contentHash{std::string(MD5Hash::hashFile(fileName))};
      auto qualifiedName{"::mdle::" + contentHash};
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
      auto extractDir{(std::filesystem::temp_directory_path() /
                       ("smdl-mdle-" + contentHash))
                          .string()};
      auto archive{Archive{fileName}};
      auto mainSource{std::optional<std::string>()};
      for (int i = 0; i < archive.get_file_count(); i++) {
        auto entryName{archive.get_file_name(i)};
        if (entryName == "main.mdl") {
          mainSource = archive.extract_file(i);
        } else if (!entryName.empty() && entryName.back() != '/') {
          auto outPath{std::filesystem::path(extractDir) / entryName};
          std::filesystem::create_directories(outPath.parent_path());
          openOrThrow(outPath.string(), std::ios::out | std::ios::binary)
              << archive.extract_file(i);
        }
      }
      if (!mainSource) {
        throw Error(concat("MDLE ", QuotedPath(fileName),
                           " does not contain 'main.mdl'"));
      }
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
      auto prefix{parseArchivePackagePrefix(fileName)};
      {
        // Duplicating the enclosed contents as loose files in the
        // same search root is an error.
        auto loosePath{searchRoot};
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
      auto archive{Archive{fileName}};
      for (int i = 0; i < archive.get_file_count(); i++) {
        if (auto entryName{archive.get_file_name(i)};
            hasExtension(entryName, ".mdl")) {
          if (!isConformingArchiveEntry(prefix, entryName)) {
            throw Error(concat(
                "archive ", QuotedPath(fileName), " entry ", Quoted(entryName),
                " does not conform to the package prefix encoded "
                "by the archive file name"));
          }
          if (auto entryPath{joinPaths(fileName, entryName)};
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
    if (auto maybePath{fileLocator.locate(fileOrDirName, {},
                                          FileLocator::REGULAR_FILES |
                                              FileLocator::DIRS)}) {
      auto &path{*maybePath};
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
        auto archivePaths{std::vector<std::string>()};
        for (const auto &entry : std::filesystem::directory_iterator(path)) {
          if (auto entryPath{makePathCanonical(entry.path().string())};
              isFile(entryPath) && hasExtension(entryPath, ".mdr")) {
            archivePaths.push_back(std::move(entryPath));
          }
        }
        std::sort(archivePaths.begin(), archivePaths.end());
        for (size_t i = 0; i < archivePaths.size(); i++) {
          auto prefixI{parseArchivePackagePrefix(archivePaths[i])};
          for (size_t j = i + 1; j < archivePaths.size(); j++) {
            auto prefixJ{parseArchivePackagePrefix(archivePaths[j])};
            if (auto n{std::min(prefixI.size(), prefixJ.size())};
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
          if (auto entryPath{makePathCanonical(entry.path().string())};
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

// Derive the value-dependent static material flags after optimization.
//
// `FunctionType::initializeMaterialFunctions` fills the type-level
// (`#isDefault`-derived) bits of `staticFlags`/`staticFlagsKnown` at
// emit time and also emits, per material, the `.evaluateOpacity` entry
// point and a `.thinWalledProbe` scaffolding function. After the
// optimizer runs, a body that reduces to returning a constant proves the
// corresponding flag bit for every possible instance, so it is marked
// known here; a body that stays runtime (or an unoptimized module) just
// leaves the bit unknown, which hosts must treat conservatively.
static void deriveStaticMaterialFlags(llvm::Module &llvmModule,
                                      std::vector<JIT::Material> &materials) {
  // If every 'ret' in the named function returns one identical constant,
  // return it, else null.
  auto foldedReturnValue{[&](std::string_view name) -> const llvm::Constant * {
    auto func{llvmModule.getFunction(name)};
    if (!func || func->isDeclaration()) return nullptr;
    const llvm::Constant *uniqueConst{};
    for (auto &block : *func) {
      if (auto ret{llvm::dyn_cast<llvm::ReturnInst>(block.getTerminator())}) {
        auto retConst{
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
    // '<symbolBase>.evaluateOpacity'.
    auto symbolBase{std::string_view(jitMaterial.evaluateOpacity.name)};
    SMDL_SANITY_CHECK(
        llvm::StringRef(symbolBase).ends_with(".evaluateOpacity"));
    symbolBase.remove_suffix(std::string_view(".evaluateOpacity").size());
    if (auto opacity{llvm::dyn_cast_if_present<llvm::ConstantFP>(
            foldedReturnValue(jitMaterial.evaluateOpacity.name))}) {
      jitMaterial.staticFlagsKnown |= MATERIAL_HAS_CUTOUT;
      if (opacity->getValueAPF().convertToFloat() < 1.0f)
        jitMaterial.staticFlags |= MATERIAL_HAS_CUTOUT;
    }
    auto thinWalledProbeName{concat(symbolBase, ".thinWalledProbe")};
    if (auto thinWalled{llvm::dyn_cast_if_present<llvm::ConstantInt>(
            foldedReturnValue(thinWalledProbeName))}) {
      jitMaterial.staticFlagsKnown |= MATERIAL_THIN_WALLED;
      if (!thinWalled->isZero())
        jitMaterial.staticFlags |= MATERIAL_THIN_WALLED;
    }
    // The displacement probe returns 'geometry.displacement' itself, so
    // a body folded to a constant vector settles
    // 'MATERIAL_HAS_DISPLACEMENT': known, and set iff the constant is
    // not the zero vector (-0.0 counts as zero).
    // A body that did not fold leaves the bit unknown, which hosts
    // treat as possibly displacing. See
    // 'JIT::Material::hasZeroDisplacement()'.
    auto displacementProbeName{concat(symbolBase, ".displacementProbe")};
    if (auto displacement{foldedReturnValue(displacementProbeName)}) {
      jitMaterial.staticFlagsKnown |= MATERIAL_HAS_DISPLACEMENT;
      if (!llvmIsZeroValue(displacement))
        jitMaterial.staticFlags |= MATERIAL_HAS_DISPLACEMENT;
    }
    // The normal probe returns 'geometry.normal - $state.normal', which
    // folds to the constant zero vector exactly when the material
    // leaves the shading normal alone, settling
    // 'MATERIAL_REMAPS_NORMAL' the way the displacement probe settles
    // its flag. See 'JIT::Material::remapsNormal()'.
    auto normalProbeName{concat(symbolBase, ".normalProbe")};
    if (auto normalDelta{foldedReturnValue(normalProbeName)}) {
      jitMaterial.staticFlagsKnown |= MATERIAL_REMAPS_NORMAL;
      if (!llvmIsZeroValue(normalDelta))
        jitMaterial.staticFlags |= MATERIAL_REMAPS_NORMAL;
    }
    // A material with no volume is trivially position-independent, and
    // a '.volumeEvaluate' body that no longer touches its '%state'
    // argument proves the volume coefficients independent of the
    // evaluation point (they may still be baked resource reads, which
    // is equally position-independent). Either way the material is
    // provably homogeneous: mark 'MATERIAL_HAS_HETEROGENEOUS_VOLUME'
    // known and unset. Otherwise the bit stays unknown rather than
    // set, because the state use may be incidental (the allocator at
    // 'OPT_LEVEL_NONE', or an un-removable side-effecting call such as
    // a scene-data lookup anywhere in the material body); hosts treat
    // unknown as heterogeneous, which is the conservative direction.
    // See 'JIT::Material::hasHomogeneousVolume()'.
    auto volumeEvaluateFunc{
        llvmModule.getFunction(jitMaterial.volumeEvaluate.name)};
    if (!(jitMaterial.staticFlags & MATERIAL_HAS_VOLUME) ||
        (volumeEvaluateFunc && !volumeEvaluateFunc->isDeclaration() &&
         volumeEvaluateFunc->arg_size() >= 1 &&
         volumeEvaluateFunc->getArg(0)->use_empty())) {
      jitMaterial.staticFlagsKnown |= MATERIAL_HAS_HETEROGENEOUS_VOLUME;
    }
    // The probes are compile-time scaffolding, not host entry points;
    // erase them so they are never JIT-compiled.
    if (auto probeFunc{llvmModule.getFunction(thinWalledProbeName)})
      probeFunc->eraseFromParent();
    if (auto probeFunc{llvmModule.getFunction(displacementProbeName)})
      probeFunc->eraseFromParent();
    if (auto probeFunc{llvmModule.getFunction(normalProbeName)})
      probeFunc->eraseFromParent();
  }
}

// Texel addresses only ever enter the IR as `inttoptr` constants of an
// image's reservation (see `IntrinsicID::LoadTexture2D`), and any
// derived address the optimizer folds to a constant stays inside the
// reservation, so the test is interval membership: scan every integer
// constant reachable from an instruction operand or global initializer
// and mark the reservation it lands in. The failure direction is
// conservative by construction: a coincidental in-range constant keeps
// an image alive, but a referenced image cannot be missed, because
// every access derives its address from one of the baked constants. At
// `OPT_LEVEL_NONE` nothing is provably unused because nothing was
// dead-code eliminated, and an image whose `startLoad()` failed has no
// reservation, so neither is ever dropped here.
size_t Compiler::dropUnusedImages() {
  const auto &llvmModule{*mLLVMModule};
  struct Interval final {
    uint64_t addressBegin{};
    // Inclusive, so a folded one-past-end address still counts.
    uint64_t addressEnd{};
    const MD5FileHash *fileHash{};
    Image *image{};
    bool used{};
  };
  auto intervals{std::vector<Interval>()};
  for (auto &[fileHash, image] : mImages) {
    if (const auto *texels{image->getTexels()}) {
      auto addressBegin{uint64_t(reinterpret_cast<uintptr_t>(texels))};
      intervals.push_back(Interval{addressBegin,
                                   addressBegin + image->getSizeInBytes(),
                                   fileHash, image.get(), false});
    }
  }
  if (intervals.empty()) return 0;
  std::sort(intervals.begin(), intervals.end(),
            [](const Interval &lhs, const Interval &rhs) {
              return lhs.addressBegin < rhs.addressBegin;
            });
  // Mark the interval containing the address, if any. Reservations are
  // disjoint, but the inclusive upper bound can touch the next
  // reservation's begin, so check the predecessor too.
  auto markAddress{[&](uint64_t address) {
    auto itr{std::upper_bound(intervals.begin(), intervals.end(), address,
                              [](uint64_t addr, const Interval &interval) {
                                return addr < interval.addressBegin;
                              })};
    for (int i = 0; i < 2 && itr != intervals.begin(); i++) {
      --itr;
      if (itr->addressBegin <= address && address <= itr->addressEnd) {
        itr->used = true;
      }
    }
  }};
  // Walk every integer constant in the module, including those nested
  // in constant expressions and aggregate initializers. Constants form
  // a DAG, so remember what has already been visited.
  llvm::SmallPtrSet<const llvm::Constant *, 32> visited{};
  auto scanConstant{[&](const llvm::Constant *constant, auto &self) -> void {
    if (!visited.insert(constant).second) return;
    if (auto constantInt{llvm::dyn_cast<llvm::ConstantInt>(constant)}) {
      if (constantInt->getBitWidth() == 64)
        markAddress(constantInt->getZExtValue());
      return;
    }
    if (auto constantData{
            llvm::dyn_cast<llvm::ConstantDataSequential>(constant)}) {
      if (constantData->getElementType()->isIntegerTy(64))
        for (unsigned i = 0; i < constantData->getNumElements(); i++)
          markAddress(constantData->getElementAsInteger(i));
      return;
    }
    for (const auto &operand : constant->operands())
      if (auto operandConstant{llvm::dyn_cast<llvm::Constant>(operand)})
        self(operandConstant, self);
  }};
  for (const auto &global : llvmModule.globals())
    if (global.hasInitializer())
      scanConstant(global.getInitializer(), scanConstant);
  for (const auto &func : llvmModule.functions())
    for (const auto &block : func)
      for (const auto &inst : block)
        for (const auto &operand : inst.operands())
          if (auto constant{llvm::dyn_cast<llvm::Constant>(operand)})
            scanConstant(constant, scanConstant);
  auto numDropped{size_t(0)};
  for (auto &interval : intervals) {
    if (!interval.used) {
      SMDL_LOG_DEBUG("Dropping image ",
                     QuotedPath(interval.fileHash->canonicalFileNames[0]),
                     ": never read by the compiled code");
      interval.image->abandonLoad();
      numDropped++;
    }
  }
  return numDropped;
}

void Compiler::resetForRecompile() {
  // Free the previous JIT first: this invalidates every function pointer
  // previously handed out, per the lifetime contract on the class.
  mLLVMJit.reset();
  mJITSessionErrors.clear();
  mWarnedResourceFileNames.clear();
  mImages.clear();
  mImageMipRequesters.clear();
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
  mMaterials.clear();
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
    auto message{llvm::toString(std::move(error))};
    SMDL_LOG_ERROR("JIT session error: ", message);
    if (!mJITSessionErrors.empty()) mJITSessionErrors += '\n';
    mJITSessionErrors += message;
  });
}

std::optional<Error> Compiler::compile(OptLevel optLevel) noexcept {
  SMDL_PROFILER_ENTRY("Compiler::compile()");
  // The 'Context' constructor and the emit phase can throw; catch
  // everything so the 'optional<Error>' contract holds instead of exiting
  // or terminating the host process.
  return catchAndReturnError([&] {
    resetForRecompile();
    auto initializeEntry{profilerEntryBegin("Initialize")};
    Context context{*this};
    for (auto &module_ : mModules) module_->reset();
    profilerEntryEnd(initializeEntry);
    {
      SMDL_PROFILER_ENTRY("Parse AST");
      for (auto &module_ : mModules)
        if (auto error{module_->parse(mAllocator)}) throw std::move(*error);
    }
    {
      SMDL_PROFILER_ENTRY("Emit LLVM-IR");
      for (auto &module_ : mModules)
        if (auto error{module_->compile(context)}) throw std::move(*error);
    }
    // Sort JIT materials and unit tests by module and line number in
    // case we want to print them later.
    sortByModuleAndLine(mMaterials);
    sortByModuleAndLine(mUnitTests);
    // Warn about desired material names that matched nothing at all, so
    // a typo does not silently skip the material it meant to keep.
    for (const auto &desiredName : mDesiredMaterialNames) {
      if (std::none_of(mMaterials.begin(), mMaterials.end(),
                       [&](const auto &jitMaterial) {
                         return matchesMaterialName(desiredName,
                                                    jitMaterial.qualifiedName);
                       })) {
        SMDL_LOG_WARN("Desired material ", Quoted(desiredName),
                      " does not match any material in the added modules");
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
    deriveStaticMaterialFlags(*mLLVMModule, mMaterials);
    // Drop the images the optimizer proved unread before decoding the
    // rest, which is why decoding waits until here: the drop decision
    // needs the optimized module, after 'deriveStaticMaterialFlags' has
    // erased the probe scaffolding whose references must not keep an
    // image alive.
    if (auto numDropped{dropUnusedImages()}) {
      SMDL_LOG_INFO("Dropped ", numDropped,
                    " image(s) never read by the compiled code");
    }
    // Finish loading the images that still hold a texel reservation,
    // i.e., neither failed 'startLoad()' nor were dropped above.
    auto imageEntries{std::vector<std::pair<const MD5FileHash *, Image *>>()};
    imageEntries.reserve(mImages.size());
    for (auto &[key, image] : mImages)
      if (image->getTexels()) imageEntries.emplace_back(key, image.get());
    if (!imageEntries.empty()) {
      SMDL_PROFILER_ENTRY("Load images in parallel");
      SMDL_LOG_INFO("Loading images ...");
      auto now{std::chrono::steady_clock::now()};
      parallelFor(0, imageEntries.size(), [&](size_t i) {
        auto fileHash{imageEntries[i].first};
        auto image{imageEntries[i].second};
        SMDL_PROFILER_ENTRY("Load image",
                            fileHash->canonicalFileNames[0].c_str());
        SMDL_LOG_DEBUG("Loading image ",
                       QuotedPath(fileHash->canonicalFileNames[0]), " ...");
        // A decode failure must not unwind out of 'parallelFor'; warn
        // and continue with the image's pre-allocated (zeroed) texels,
        // matching the 'loadImage' policy.
        if (auto error{catchAndReturnError([&] { image->finishLoad(); })}) {
          SMDL_LOG_WARN("cannot load ",
                        QuotedPath(fileHash->canonicalFileNames[0]), ": ",
                        error->message);
        }
      });
      auto duration{std::chrono::duration_cast<std::chrono::microseconds>(
                        std::chrono::steady_clock::now() - now)
                        .count()};
      SMDL_LOG_INFO("Loading images done. [", std::to_string(duration * 1e-6),
                    " seconds]");
    }
  });
}

std::optional<Error>
Compiler::formatSourceFiles(const FormatOptions &formatOptions) noexcept {
  SMDL_PROFILER_ENTRY("Compiler::formatSourceFiles()");
  for (auto &module_ : mModules) {
    if (module_->isFileBacked()) {
      if (auto error{module_->formatSourceFiles(formatOptions)}) return error;
    }
  }
  return std::nullopt;
}

std::optional<Error> Compiler::extractDocs(DocDatabase &docs) noexcept {
  SMDL_PROFILER_ENTRY("Compiler::extractDocs()");
  return catchAndReturnError([&] {
    for (auto &module_ : mModules) {
      if (module_->isShadowed()) continue;
      if (auto error{module_->parse(mAllocator)}) throw std::move(*error);
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

// Look up the resource for the given key in `resources`, running `loader`
// exactly once per distinct key. The key is the content hash of the file,
// possibly extended with load parameters (see `mImages`). A load failure
// is a warning, not an error: the resource stays default-constructed and
// rendering continues.
template <typename K, typename T, typename Hash, typename Eq, typename Loader>
static T &
loadResource(std::unordered_map<K, std::unique_ptr<T>, Hash, Eq> &resources,
             const K &key, const SourceLocation &srcLoc, Loader &&loader) {
  auto [itr, inserted] = resources.try_emplace(key);
  if (inserted) {
    itr->second = std::make_unique<T>();
    if (auto error{std::invoke(std::forward<Loader>(loader), *itr->second)}) {
      srcLoc.logWarn(error->message);
    }
  }
  return *itr->second;
}

void Compiler::logResourceWarningOnce(const SourceLocation &srcLoc,
                                      const std::string &fileName,
                                      std::string_view message) {
  if (mWarnedResourceFileNames.insert(fileName).second) srcLoc.logWarn(message);
}

const Image &Compiler::loadImage(const std::string &fileName,
                                 const SourceLocation &srcLoc,
                                 bool withMipLevels, Image::MipFilter filter) {
  auto &image{
      loadResource(mImages, mFileHasher[fileName], srcLoc, [&](Image &image) {
        SMDL_PROFILER_ENTRY("Compiler::loadImage()", fileName.c_str());
        // What decides the layout is the compiler-wide switch and not
        // this reference's request: the request is sticky and shared, so
        // a later reference may ask for a chain that this one did not
        // want, and the space for it has to be reserved by now.
        return image.startLoad(fileName, enableMipMaps);
      })};
  // The request is applied on every reference, not just the one that
  // decoded the image, so that it does not matter which reference comes
  // first: the mip levels are generated at the end of the compile, by
  // which point every reference has been seen.
  if (withMipLevels && enableMipMaps) {
    auto [itr, inserted] = mImageMipRequesters.try_emplace(&image, srcLoc);
    if (!image.requestMipLevels(filter)) {
      auto filterName{[](Image::MipFilter f) {
        return f == Image::MIP_MAX ? "maximum" : "mean";
      }};
      srcLoc.throwError(
          "cannot request a ", filterName(filter), " mip chain for ",
          QuotedPath(fileName), ": a ", filterName(image.getMipFilter()),
          " mip chain was requested at ", itr->second.getModuleDisplayName(),
          ":", itr->second.lineNo, ", and an image holds one chain");
    }
  } else if (withMipLevels) {
    // Refusing is the whole point of the switch, so this is not a
    // warning. It is still the reason a render is aliased, so say so
    // where a debug log will show it.
    SMDL_LOG_DEBUG("Ignoring the mip levels requested for ",
                   QuotedPath(fileName), ": mip maps are disabled");
  }
  return image;
}

const Ptexture &Compiler::loadPtexture(const std::string &fileName,
                                       const SourceLocation &srcLoc) {
  return loadResource(
      mPtextures, mFileHasher[fileName], srcLoc,
      [&](Ptexture &ptexture) -> std::optional<Error> {
#if SMDL_HAS_PTEX
        SMDL_PROFILER_ENTRY("Compiler::loadPtexture()", fileName.c_str());
        Ptex::String message{};
        auto texture{PtexTexture::open(fileName.c_str(), message,
                                       /*premultiply=*/false)};
        if (!texture)
          return Error(concat("cannot load ", QuotedPath(fileName), ": ",
                              message.c_str()));
        ptexture.texture = texture;
        // NOTE: No shared 'PtexFilter': 'PtexFilter::eval' mutates filter
        // members, so 'smdlPtexEvaluate' maintains per-thread filters.
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
                      [&](BSDFMeasurement &bsdfMeasurement) {
                        SMDL_PROFILER_ENTRY("Compiler::loadBSDFMeasurement()",
                                            fileName.c_str());
                        return bsdfMeasurement.loadFromFile(fileName);
                      });
}

const LightProfile &Compiler::loadLightProfile(const std::string &fileName,
                                               const SourceLocation &srcLoc) {
  return loadResource(mLightProfiles, mFileHasher[fileName], srcLoc,
                      [&](LightProfile &lightProfile) {
                        SMDL_PROFILER_ENTRY("Compiler::loadLightProfile()",
                                            fileName.c_str());
                        return lightProfile.loadFromFile(fileName);
                      });
}

const VoxelGrid &Compiler::loadVoxelGrid(const std::string &fileName,
                                         const std::string &gridName,
                                         const SourceLocation &srcLoc) {
  return loadResource(mVoxelGrids, std::pair(mFileHasher[fileName], gridName),
                      srcLoc, [&](VoxelGrid &voxelGrid) {
                        SMDL_PROFILER_ENTRY("Compiler::loadVoxelGrid()",
                                            fileName.c_str());
                        return voxelGrid.loadFromFile(fileName, gridName);
                      });
}

SpectrumView Compiler::loadSpectrum(const std::string &fileName,
                                    const SourceLocation &srcLoc) {
  return SpectrumView(loadResource(
      mSpectrums, mFileHasher[fileName], srcLoc, [&](Spectrum &spectrum) {
        SMDL_PROFILER_ENTRY("Compiler::loadSpectrum()", fileName.c_str());
        return spectrum.loadFromFile(fileName);
      }));
}

SpectrumView Compiler::loadSpectrum(const std::string &fileName, int curveIndex,
                                    const SourceLocation &srcLoc) {
  return loadSpectrumLibrary(fileName, srcLoc).getCurveByIndex(curveIndex);
}

SpectrumView Compiler::loadSpectrum(const std::string &fileName,
                                    const std::string &curveName,
                                    const SourceLocation &srcLoc) {
  return loadSpectrumLibrary(fileName, srcLoc).getCurveByName(curveName);
}

const SpectrumLibrary &
Compiler::loadSpectrumLibrary(const std::string &fileName,
                              const SourceLocation &srcLoc) {
  return loadResource(mSpectrumLibraries, mFileHasher[fileName], srcLoc,
                      [&](SpectrumLibrary &spectrumLibrary) {
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
      auto clonedModule{llvm::CloneModule(getLLVMModule())};
      passManager.run(*clonedModule);
      out = std::string(os.str());
    }
  });
}

std::optional<Error> Compiler::jitCompile() noexcept {
  SMDL_PROFILER_ENTRY("Compiler::jit_compile()");
  auto error{catchAndReturnError([&] {
    if (!mLLVMJit || !mLLVMModule || !mLLVMContext)
      throw Error("nothing to JIT-compile: 'compile()' must be called first");
    // Define the builtin runtime callees ('smdlPanic', 'smdlBumpAllocate',
    // ...) as absolute symbols so they resolve even when the host process
    // does not export its own symbols (e.g. static link without
    // '--export-dynamic').
    if (!mBuiltinCalleeAddresses.empty()) {
      auto mangle{llvm::orc::MangleAndInterner(mLLVMJit->getExecutionSession(),
                                               mLLVMJit->getDataLayout())};
      auto symbolMap{llvm::orc::SymbolMap{}};
      for (const auto &[calleeName, calleeAddr] : mBuiltinCalleeAddresses)
        symbolMap[mangle(calleeName)] = llvm::orc::ExecutorSymbolDef(
            llvm::orc::ExecutorAddr::fromPtr(calleeAddr),
            llvm::JITSymbolFlags::Exported | llvm::JITSymbolFlags::Callable);
      llvmThrowIfError(mLLVMJit->getMainJITDylib().define(
          llvm::orc::absoluteSymbols(std::move(symbolMap))));
    }
    // Hand the module to the JIT, dropping our handles up front: a failed
    // call must not leave moved-from state behind for 'dump()' or
    // 'getLLVMModule()' to trip over.
    auto llvmJitModule{llvm::orc::ThreadSafeModule(std::move(mLLVMModule),
                                                   std::move(mLLVMContext))};
    llvmThrowIfError(mLLVMJit->addIRModule(std::move(llvmJitModule)));
    jitLookup(mColorToRGB);
    jitLookup(mRGBToColor);
    for (auto &jitMaterial : mMaterials) {
      jitLookup(jitMaterial.evaluate);
      jitLookup(jitMaterial.evaluateOpacity);
      jitLookup(jitMaterial.displacementEvaluate);
      jitLookup(jitMaterial.volumeEvaluate);
      jitLookup(jitMaterial.scatterEvaluate);
      jitLookup(jitMaterial.scatterSample);
      // Emitted only when the host asked for them; see
      // 'Compiler::enableScatterNormal'.
      if (!jitMaterial.scatterNormalSample.name.empty()) {
        jitLookup(jitMaterial.scatterNormalSample);
        jitLookup(jitMaterial.scatterNormalEvaluate);
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
  if (error && !mJITSessionErrors.empty()) {
    error->message += "\nJIT session errors:\n";
    error->message += mJITSessionErrors;
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

const JIT::Material *
Compiler::findMaterial(std::string_view materialName) const noexcept try {
  auto results{findMaterials(materialName)};
  if (results.empty()) {
    // Distinguish "never existed" from "excluded by the desired-material
    // filter", so a host that forgot a name gets an actionable error.
    for (const auto &skippedName : mSkippedMaterialNames) {
      if (matchesMaterialName(materialName, skippedName)) {
        SMDL_LOG_ERROR("Material ", Quoted(materialName), " matches ",
                       Quoted(skippedName),
                       ", which was skipped because it is not a desired "
                       "material, see 'Compiler::setDesiredMaterials()'");
        break;
      }
    }
    return nullptr;
  }
  if (results.size() > 1) {
    auto message{concat("Material ", Quoted(materialName),
                        " is ambiguous with ", results.size(), " matches:")};
    for (const auto *jitMaterial : results) {
      message += "\n  ";
      message +=
          concat(jitMaterial->qualifiedName, " (",
                 jitMaterial->moduleDisplayName, ":", jitMaterial->lineNo, ")");
    }
    SMDL_LOG_ERROR(message);
    return nullptr;
  }
  return results.front();
} catch (...) {
  return nullptr;
}

std::vector<const JIT::Material *>
Compiler::findMaterials(std::string_view materialName) const {
  auto results{std::vector<const JIT::Material *>()};
  for (const auto &jitMaterial : mMaterials) {
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
  SMDL_SANITY_CHECK(state.wavelength_base != nullptr);
  float3 rgb{};
  mColorToRGB(state, color, rgb);
  return rgb;
}

void Compiler::convertRGBToColor(const State &state, const float3 &rgb,
                                 float *color) const noexcept {
  SMDL_SANITY_CHECK(mRGBToColor && color);
  SMDL_SANITY_CHECK(state.wavelength_base != nullptr);
  mRGBToColor(state, rgb, color);
}

// The color scheme of the unit test results, sharing the vocabulary of
// the `doc` subcommand's text printer: identity in blue, the name being
// reported in cyan, and metadata in grey, plus green and red for the
// results themselves.
static constexpr auto testColorFile{llvm::HighlightColor::Tag};
static constexpr auto testColorName{llvm::HighlightColor::Attribute};
static constexpr auto testColorMetadata{llvm::HighlightColor::Note};
static constexpr auto testColorSuccess{llvm::HighlightColor::String};
static constexpr auto testColorFailure{llvm::HighlightColor::Error};

std::optional<Error> Compiler::runUnitTests(const State &state) noexcept {
  return catchAndReturnError([&] {
    // NOTE: Print through `llvm::errs()` rather than `std::cerr` so that
    // `WithColor` can colorize. It detects the terminal itself, so piped
    // and redirected output stays plain text. Both streams write to file
    // descriptor 2 unbuffered, so this stays correctly interleaved with
    // the logger, which still prints through `std::cerr`.
    //
    // NOTE: Each colored span opens and closes before the test runs, so
    // that a test that crashes cannot leave the terminal colored.
    auto &os{llvm::errs()};
    const auto llvmColorMode{
        colorMode == COLOR_MODE_ALWAYS  ? llvm::ColorMode::Enable
        : colorMode == COLOR_MODE_NEVER ? llvm::ColorMode::Disable
                                        : llvm::ColorMode::Auto};
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
  auto printStaticFlags{[](const JIT::Material &jitMaterial) {
    auto flags{std::string()};
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
      mMaterials.begin(), mMaterials.end(), [&](auto itr0, auto itr1) {
        message += concat(QuotedPath(itr0->moduleDisplayName), " contains ",
                          itr1 - itr0, " materials:\n");
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

  [[nodiscard]] PtexFilter *get(const smdl::Ptexture &ptex) {
    auto &filter{mFilters[ptex.texture]};
    if (!filter)
      filter =
          PtexFilter::getFilter(static_cast<PtexTexture *>(ptex.texture),
                                PtexFilter::Options(PtexFilter::f_bilinear));
    return filter;
  }

private:
  std::map<const void *, PtexFilter *> mFilters{};
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
  if (ptex && ptex->texture && first < ptex->channelCount) {
    num = std::min(num, int(ptex->channelCount - first));
    thread_local ThreadLocalPtexFilters filters{};
    const auto &smdlState{*static_cast<const smdl::State *>(state)};
    filters.get(*ptex)->eval(out, first, num, smdlState.ptex_face_id,
                             smdlState.ptex_face_uv.x, smdlState.ptex_face_uv.y,
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
