#include "Layout.h"

#include "Asset.h"
#include "CurvesFile.h"
#include "PlacesFile.h"

#include "smdl/Support/Error.h"
#include "smdl/Support/Logger.h"
#include "smdl/Support/Strings.h"

#include <filesystem>
#include <fstream>
#include <optional>

// The lowering half of the layout toolchain: resolve every path, follow
// every import, and flatten the document tree into the item list the
// renderer consumes. Everything scoped (names, aliases, import-site
// overrides) is folded into the items here, which is what lets the rest
// of the pipeline stay a consumer of plain data.

namespace {

using RenameMap = std::map<std::string, std::string, std::less<>>;

// The composition of two rename maps as functions: apply `inner`, then
// `outer`. Renames compose associatively, which is what lets a whole
// import chain fold into the single `MaterialAssignment::renames` map an
// item carries.
[[nodiscard]] RenameMap composeRename(const RenameMap &inner,
                                      const RenameMap &outer) {
  if (inner.empty()) return outer;
  if (outer.empty()) return inner;
  auto result{RenameMap()};
  for (const auto &[from, to] : inner) {
    auto itr{outer.find(to)};
    result.emplace(from, itr == outer.end() ? to : itr->second);
  }
  for (const auto &[from, to] : outer) result.try_emplace(from, to);
  return result;
}

// Does the file begin with the `#smdl layout` magic? Reads just enough
// bytes to decide, so calling it on a large binary mesh costs nothing.
[[nodiscard]] bool sniffLayoutMagic(const std::filesystem::path &path) {
  auto stream{std::ifstream(path, std::ios::binary)};
  if (!stream) return false;
  std::array<char, 16> buffer{};
  stream.read(buffer.data(), sizeof(buffer));
  const auto text{std::string_view(buffer.data(), size_t(stream.gcount()))};
  const auto magic{LAYOUT_MAGIC};
  return smdl::startsWith(text, magic) &&
         (text.size() == magic.size() || text[magic.size()] == '\n' ||
          text[magic.size()] == '\r' || text[magic.size()] == ' ');
}

// Does the file begin with the `.curves` magic? The binary sibling of
// `sniffLayoutMagic()`, so that the extension stays advisory here too.
[[nodiscard]] bool sniffCurvesMagic(const std::filesystem::path &path) {
  auto stream{std::ifstream(path, std::ios::binary)};
  if (!stream) return false;
  std::array<char, 8> buffer{};
  stream.read(buffer.data(), sizeof(buffer));
  return size_t(stream.gcount()) == CURVES_MAGIC.size() &&
         std::string_view(buffer.data(), CURVES_MAGIC.size()) == CURVES_MAGIC;
}

// Thrown to abandon the placement being lowered after its diagnostic is
// emitted; caught by the placement loop, so one bad path does not hide
// the diagnostics of the placements after it.
class SkipPlacement final {};

// What one written path turns out to name.
class Target final {
public:
  enum class Kind {
    MESH,   // A mesh file, `correction` applied if it came via an asset.
    CURVES, // A `.curves` fiber file; `correction` applies the same way.
    LAYOUT, // Another layout, to be recursed into.
  };
  Kind kind{Kind::MESH};
  std::string path{};
  float4x4 correction{float4x4(1.0f)};

  // The kind as a diagnostic spells it.
  [[nodiscard]] std::string_view kindName() const noexcept {
    return kind == Kind::LAYOUT   ? "layout"
           : kind == Kind::CURVES ? "curves file"
                                  : "mesh file";
  }
};

class Lowerer final {
public:
  Lowerer(LayoutDiagnostics &diags, const AssetSearchPath &search,
          Layout &result)
      : mDiags(diags), mSearch(search), mResult(result) {}

  void lowerFile(const std::string &fileName, const float4x4 &xf,
                 const RenameMap &outerRenames,
                 const std::optional<bool> &outerCaster, bool isEntry,
                 const LayoutLocation &importSite) {
    std::error_code ignored{};
    auto canonical{std::filesystem::weakly_canonical(fileName, ignored)};
    if (canonical.empty()) canonical = fileName;
    for (const auto &frame : mOpenFiles)
      if (frame.canonical == canonical) {
        auto message{std::string("import cycle:")};
        auto inCycle{false};
        for (const auto &open : mOpenFiles) {
          if (open.canonical == canonical) inCycle = true;
          if (inCycle) message += smdl::concat(" ", open.fileName, " ->");
        }
        message += smdl::concat(" ", fileName);
        auto &error{mDiags.error(importSite, message)};
        for (const auto &open : mOpenFiles)
          if (open.importSite)
            error.note(open.importSite, "imported from here");
        return;
      }
    const auto firstNestedDiag{mDiags.all().size()};
    const LayoutSource *source{};
    try {
      source = &mDiags.loadSource(fileName);
    } catch (const smdl::Error &error) {
      if (!importSite) throw;
      mDiags.error(importSite, error.message);
      return;
    }
    const auto document{
        parseLayout(mDiags, *source,
                    std::filesystem::path(fileName).parent_path().string())};
    mOpenFiles.push_back({canonical, fileName, importSite});
    lowerDocument(document, xf, outerRenames, outerCaster, isEntry);
    mOpenFiles.pop_back();
    // Everything the nested file reported, parse and lowering alike,
    // points back at the import that pulled it in.
    if (importSite)
      mDiags.noteAllFrom(firstNestedDiag, importSite, "imported from here");
  }

private:
  void lowerDocument(const LayoutDocument &document, const float4x4 &xf,
                     const RenameMap &outerRenames,
                     const std::optional<bool> &outerCaster, bool isEntry) {
    // Only the entry file's camera, sky, and medium take effect. An
    // imported layout carrying its own is a layout that can also render
    // standalone, so say what is being ignored rather than erroring, and
    // never adopt it silently.
    if (isEntry) {
      mResult.camera = document.camera;
      mResult.sky = document.sky;
      mResult.entryMaterialAliases = document.materialAliases;
      if (!document.mediumName.empty())
        mResult.exteriorMediumName = document.mediumName;
      if (!document.iblPath.empty())
        if (auto resolved{resolvePath(document, document.iblPath,
                                      document.iblPathLoc, false)};
            !resolved.empty())
          mResult.sky.iblFileName = resolved.string();
    } else {
      if (document.cameraLoc)
        mDiags.warn(document.cameraLoc,
                    "the 'camera' of an imported layout is ignored (only "
                    "the entry layout's camera takes effect)");
      if (document.skyLoc)
        mDiags.warn(document.skyLoc,
                    "the 'sky' of an imported layout is ignored (only the "
                    "entry layout's sky takes effect)");
      if (document.mediumLoc)
        mDiags.warn(document.mediumLoc,
                    "the 'medium' of an imported layout is ignored (only "
                    "the entry layout's medium takes effect)");
    }
    auto usedAssets{std::vector<bool>(document.assets.size(), false)};
    auto usedGroups{std::vector<bool>(document.groups.size(), false)};
    auto usedLights{std::vector<bool>(document.lights.size(), false)};
    auto groupStack{std::vector<GroupFrame>()};
    lowerPlacements(document, document.placements, xf, outerRenames,
                    outerCaster, usedAssets, usedGroups, usedLights, groupStack,
                    std::string());
    for (size_t i = 0; i < document.assets.size(); i++)
      if (!usedAssets[i])
        mDiags.warn(document.assets[i].nameLoc,
                    smdl::concat("unused asset ",
                                 smdl::Quoted(document.assets[i].name)));
    for (size_t i = 0; i < document.groups.size(); i++)
      if (!usedGroups[i])
        mDiags.warn(document.groups[i].nameLoc,
                    smdl::concat("unused group ",
                                 smdl::Quoted(document.groups[i].name)));
    for (size_t i = 0; i < document.lights.size(); i++)
      if (!usedLights[i])
        mDiags.warn(document.lights[i].nameLoc,
                    smdl::concat("unused light ",
                                 smdl::Quoted(document.lights[i].name)));
  }

  // One `place` of a group on the lowering stack, for cycle detection
  // and for the chain the cycle error reports.
  class GroupFrame final {
  public:
    const LayoutGroupDecl *group{};
    LayoutLocation site{};
  };

  void lowerPlacements(const LayoutDocument &document,
                       const std::vector<LayoutPlacement> &placements,
                       const float4x4 &xf, const RenameMap &outerRenames,
                       const std::optional<bool> &outerCaster,
                       std::vector<bool> &usedAssets,
                       std::vector<bool> &usedGroups,
                       std::vector<bool> &usedLights,
                       std::vector<GroupFrame> &groupStack,
                       const std::string &namePrefix) {
    for (const auto &placement : placements) {
      try {
        if (placement.kind == LayoutPlacement::Kind::PLACE) {
          lowerPlace(document, placement, xf, outerRenames, outerCaster,
                     usedAssets, usedGroups, usedLights, groupStack,
                     namePrefix);
        } else {
          lowerImport(document, placement, xf, outerRenames, outerCaster);
        }
        // NOLINTNEXTLINE
      } catch (const SkipPlacement &) {
        // The diagnostic is already emitted; move on to the next.
      }
    }
  }

  void lowerPlace(const LayoutDocument &document,
                  const LayoutPlacement &placement, const float4x4 &xf,
                  const RenameMap &outerRenames,
                  const std::optional<bool> &outerCaster,
                  std::vector<bool> &usedAssets, std::vector<bool> &usedGroups,
                  std::vector<bool> &usedLights,
                  std::vector<GroupFrame> &groupStack,
                  const std::string &namePrefix) {
    const auto placeName{placement.asName.empty() ? namePrefix
                         : namePrefix.empty()
                             ? placement.asName
                             : namePrefix + "/" + placement.asName};
    const LayoutAssetDecl *decl{};
    for (size_t i = 0; i < document.assets.size(); i++)
      if (document.assets[i].name == placement.assetName) {
        decl = &document.assets[i];
        usedAssets[i] = true;
        break;
      }
    const LayoutGroupDecl *group{};
    if (!decl)
      for (size_t i = 0; i < document.groups.size(); i++)
        if (document.groups[i].name == placement.assetName) {
          group = &document.groups[i];
          usedGroups[i] = true;
          break;
        }
    const LayoutLightDecl *light{};
    if (!decl && !group)
      for (size_t i = 0; i < document.lights.size(); i++)
        if (document.lights[i].name == placement.assetName) {
          light = &document.lights[i];
          usedLights[i] = true;
          break;
        }
    if (!decl && !group && !light) {
      auto &error{
          mDiags.error(placement.assetNameLoc,
                       smdl::concat("no asset, group, or light named ",
                                    smdl::Quoted(placement.assetName)))};
      auto candidates{std::vector<std::string_view>()};
      for (const auto &asset : document.assets)
        candidates.push_back(asset.name);
      for (const auto &declared : document.groups)
        candidates.push_back(declared.name);
      for (const auto &declared : document.lights)
        candidates.push_back(declared.name);
      if (const auto nearest{
              smdl::suggestNearest(placement.assetName, candidates)};
          !nearest.empty())
        error.note({},
                   smdl::concat("did you mean ", smdl::Quoted(nearest), "?"));
      throw SkipPlacement();
    }
    // A light has no material slots, so a place-site override on one is a
    // statement that does nothing; say so rather than silently ignore it.
    if (light && (!placement.overrides.empty() || !placement.variants.empty()))
      mDiags.warn(placement.assetNameLoc,
                  smdl::concat("material overrides on the light ",
                               smdl::Quoted(placement.assetName),
                               " have no effect"));
    if (light && placement.casterOverride)
      mDiags.warn(placement.casterLoc,
                  smdl::concat("'caster' on the light ",
                               smdl::Quoted(placement.assetName),
                               " has no effect"));
    // The place's own overrides apply outside everything the target says
    // for itself, and inside everything above: each syntactic enclosure
    // adds its rename layer one step further out.
    const auto baseOuter{composeRename(placement.overrides, outerRenames)};
    // The caster mark composes the other way round: the innermost
    // explicit word wins, and the asset's own mark is the default.
    const auto effectiveCaster{
        placement.casterOverride ? placement.casterOverride : outerCaster};
    if (!placement.placesPath.empty()) {
      // The bulk form: one instance per record, each record's transform
      // standing where a one-line place's operations would, and each
      // record's variant composing where that place's own overrides
      // would (inside this statement's, outside the target's).
      const auto resolved{resolvePath(document, placement.placesPath,
                                      placement.placesPathLoc, true)};
      auto places{PlacesFile()};
      try {
        places = readPlacesFile(resolved.string());
      } catch (const smdl::Error &error) {
        mDiags.error(placement.placesPathLoc, error.message);
        throw SkipPlacement();
      }
      if (!placement.variants.empty() && !places.hasVariants())
        mDiags.warn(placement.placesPathLoc,
                    "the buffer has no variant column, so the 'variant' "
                    "blocks go unused");
      for (size_t i = 0; i < places.variants.size(); i++)
        if (places.variants[i] != PlacesFile::NO_VARIANT &&
            places.variants[i] >= placement.variants.size()) {
          mDiags.error(placement.placesPathLoc,
                       smdl::concat("record ", i, " picks variant ",
                                    places.variants[i], ", but only ",
                                    placement.variants.size(),
                                    " variant block(s) are declared"));
          throw SkipPlacement();
        }
      // The composed rename layer per variant, folded once rather than
      // once per record: a scatter reuses a handful of variants across
      // thousands of records.
      auto outerByVariant{std::vector<RenameMap>()};
      outerByVariant.reserve(placement.variants.size());
      for (const auto &variant : placement.variants)
        outerByVariant.push_back(composeRename(variant, baseOuter));
      SMDL_LOG_DEBUG("Scattering ", places.transforms.size(),
                     " record(s) from ", smdl::QuotedPath(resolved.string()));
      const auto recordXf{[&](size_t i) {
        return xf * placement.transform * places.transforms[i];
      }};
      const auto outerFor{[&](size_t i) -> const RenameMap & {
        const auto variantIndex{places.hasVariants() ? places.variants[i]
                                                     : PlacesFile::NO_VARIANT};
        return variantIndex == PlacesFile::NO_VARIANT
                   ? baseOuter
                   : outerByVariant[variantIndex];
      }};
      // A group or a layout target scatters record by record, because
      // their contents multiply through recursion. A direct mesh or
      // primitive target batches by variant class instead, one item per
      // class with every record's fully composed transform, so the
      // scene can build one Embree instance array per mesh rather than
      // one instance geometry per record.
      auto target{Target()};
      const auto batchable{
          decl != nullptr &&
          (decl->primitive.active() ||
           (target = resolveTarget(document, decl->path, decl->pathLoc)).kind !=
               Target::Kind::LAYOUT)};
      if (batchable && !decl->primitive.active())
        checkAssetTargetKind(*decl, target);
      if (!batchable) {
        for (size_t i = 0; i < places.transforms.size(); i++)
          lowerPlaceTarget(document, decl, group, light, placement.assetNameLoc,
                           recordXf(i), outerFor(i), effectiveCaster,
                           usedAssets, usedGroups, usedLights, groupStack,
                           placeName);
        return;
      }
      // Batch by variant class in first-appearance order. Instance
      // order within the scatter is therefore grouped by class rather
      // than by record, which only `state::object_id` could ever
      // observe.
      auto classSlots{std::map<uint32_t, size_t>()};
      auto batches{std::vector<std::pair<uint32_t, std::vector<float4x4>>>()};
      for (size_t i = 0; i < places.transforms.size(); i++) {
        const auto variantIndex{places.hasVariants() ? places.variants[i]
                                                     : PlacesFile::NO_VARIANT};
        const auto [slot, isNew]{
            classSlots.try_emplace(variantIndex, batches.size())};
        if (isNew) batches.emplace_back(variantIndex, std::vector<float4x4>());
        batches[slot->second].second.push_back(
            decl->primitive.active()
                ? recordXf(i) * decl->transform
                : recordXf(i) * decl->transform * target.correction);
      }
      for (auto &[variantIndex, xfs] : batches) {
        auto &item{mResult.items.emplace_back()};
        if (decl->primitive.active()) {
          item.primitive = decl->primitive;
        } else if (target.kind == Target::Kind::CURVES) {
          item.fileName = target.path;
          item.curves = decl->curves;
          item.curves.active = true;
        } else {
          item.fileName = target.path;
          item.selection = decl->selection;
          item.subdiv = decl->subdiv;
        }
        item.materials = decl->materials;
        item.materials.renames = composeRename(
            document.materialAliases, variantIndex == PlacesFile::NO_VARIANT
                                          ? baseOuter
                                          : outerByVariant[variantIndex]);
        item.caster = casterOf(*decl, effectiveCaster, target);
        item.causticLight = decl->caustic;
        item.placeName = placeName;
        if (xfs.size() == 1) {
          item.objectToWorld = xfs[0];
        } else {
          item.batchXfs = std::move(xfs);
        }
      }
      return;
    }
    lowerPlaceTarget(document, decl, group, light, placement.assetNameLoc,
                     xf * placement.transform, baseOuter, effectiveCaster,
                     usedAssets, usedGroups, usedLights, groupStack, placeName);
  }

  // One resolved placement of `decl`, `group`, or `light` (exactly one
  // is set) under the fully combined placement transform: the shared
  // tail of the ordinary and bulk `place` forms.
  void lowerPlaceTarget(
      const LayoutDocument &document, const LayoutAssetDecl *decl,
      const LayoutGroupDecl *group, const LayoutLightDecl *lightDecl,
      const LayoutLocation &nameLoc, const float4x4 &combinedXf,
      const RenameMap &effectiveOuter,
      const std::optional<bool> &effectiveCaster, std::vector<bool> &usedAssets,
      std::vector<bool> &usedGroups, std::vector<bool> &usedLights,
      std::vector<GroupFrame> &groupStack, const std::string &placeName) {
    if (lightDecl) {
      auto &light{mResult.lights.emplace_back()};
      light.decl = *lightDecl;
      light.lightToWorld = combinedXf * lightDecl->transform;
      light.placeName = placeName;
      if (lightDecl->kind == LayoutLightDecl::Kind::PROFILE)
        light.decl.profilePath = resolvePath(document, lightDecl->profilePath,
                                             lightDecl->profilePathLoc, true)
                                     .string();
      return;
    }
    if (group) {
      for (const auto &frame : groupStack)
        if (frame.group == group) {
          auto message{std::string("group cycle:")};
          auto inCycle{false};
          for (const auto &open : groupStack) {
            if (open.group == group) inCycle = true;
            if (inCycle) message += smdl::concat(" ", open.group->name, " ->");
          }
          message += smdl::concat(" ", group->name);
          auto &error{mDiags.error(nameLoc, message)};
          for (const auto &open : groupStack)
            error.note(open.site, "placed from here");
          throw SkipPlacement();
        }
      groupStack.push_back({group, nameLoc});
      lowerPlacements(document, group->placements, combinedXf, effectiveOuter,
                      effectiveCaster, usedAssets, usedGroups, usedLights,
                      groupStack, placeName);
      groupStack.pop_back();
      return;
    }
    // A primitive is pure geometry by construction: no path to resolve,
    // no file to read, and the parser already guaranteed the assignment.
    if (decl->primitive.active()) {
      auto &item{mResult.items.emplace_back()};
      item.primitive = decl->primitive;
      item.objectToWorld = combinedXf * decl->transform;
      item.materials = decl->materials;
      item.materials.renames =
          composeRename(document.materialAliases, effectiveOuter);
      item.caster = effectiveCaster.value_or(decl->caster);
      item.causticLight = decl->caustic;
      item.placeName = placeName;
      return;
    }
    const auto target{resolveTarget(document, decl->path, decl->pathLoc)};
    checkAssetTargetKind(*decl, target);
    if (target.kind == Target::Kind::LAYOUT) {
      // The asset's own mark passes down like an override, since there is
      // no one item for it to mark.
      lowerFile(target.path, combinedXf * decl->transform,
                composeRename(subtreeRenames(decl->materials, decl->pathLoc),
                              effectiveOuter),
                effectiveCaster ? effectiveCaster
                : decl->caster  ? std::optional<bool>(true)
                                : std::nullopt,
                false, decl->pathLoc);
      return;
    }
    auto &item{mResult.items.emplace_back()};
    item.fileName = target.path;
    item.objectToWorld = combinedXf * decl->transform * target.correction;
    if (target.kind == Target::Kind::CURVES) {
      item.curves = decl->curves;
      item.curves.active = true;
    } else {
      item.selection = decl->selection;
      item.subdiv = decl->subdiv;
    }
    item.materials = decl->materials;
    item.materials.renames =
        composeRename(document.materialAliases, effectiveOuter);
    item.caster = casterOf(*decl, effectiveCaster, target);
    item.causticLight = decl->caustic;
    item.placeName = placeName;
  }

  // The composed mark of an item lowered from `decl` through a target of
  // known kind: a groom cannot carry it, since the manifold walk has no
  // smooth surface to run on there.
  [[nodiscard]] bool casterOf(const LayoutAssetDecl &decl,
                              const std::optional<bool> &effectiveCaster,
                              const Target &target) {
    const bool caster{effectiveCaster.value_or(decl.caster)};
    if (caster && target.kind == Target::Kind::CURVES) {
      mDiags.error(decl.casterLoc ? decl.casterLoc : decl.pathLoc,
                   smdl::concat("'caster' applies to a mesh file or a shape, "
                                "but ",
                                smdl::QuotedPath(decl.path),
                                " is a curves "
                                "file"));
      throw SkipPlacement();
    }
    return caster;
  }

  // The kind-specific properties an asset block can write, cross-checked
  // once the target's kind is known: the parser cannot know what a path
  // names, so this is where 'subdivide' on a groom or 'ribbon' on a
  // mesh is caught. Selecting or subdividing a layout would mean
  // applying it to every file the layout names, which is never what the
  // author means; say so instead of silently ignoring it.
  void checkAssetTargetKind(const LayoutAssetDecl &decl, const Target &target) {
    if (target.kind != Target::Kind::MESH &&
        (!decl.selection.patterns.empty() || decl.selection.recenter ||
         decl.subdiv.active())) {
      mDiags.error(decl.pathLoc,
                   smdl::concat("'select', 'recenter', 'subdivide', and "
                                "'displace' apply to a mesh file, but ",
                                smdl::QuotedPath(decl.path), " is a ",
                                target.kindName()));
      throw SkipPlacement();
    }
    if (target.kind != Target::Kind::CURVES && decl.curves.anyOps()) {
      mDiags.error(decl.curvesOpsLoc,
                   smdl::concat("'tube', 'ribbon', and 'radius_scale' apply "
                                "to a curves file, but ",
                                smdl::QuotedPath(decl.path), " is a ",
                                target.kindName()));
      throw SkipPlacement();
    }
    if (target.kind == Target::Kind::CURVES) {
      // Fibers have no mesh slots, so like a primitive the one thing
      // the declaration cannot go without is the whole-asset binding.
      if (!decl.materials.bySlot.empty()) {
        mDiags.error(decl.nameLoc, "a curves file has no material slots; use "
                                   "'material <name>' alone");
        throw SkipPlacement();
      }
      if (decl.materials.all.empty()) {
        mDiags.error(decl.nameLoc,
                     smdl::concat("the curves asset ", smdl::Quoted(decl.name),
                                  " needs 'material <name>' in its block"));
        throw SkipPlacement();
      }
    }
  }

  void lowerImport(const LayoutDocument &document,
                   const LayoutPlacement &placement, const float4x4 &xf,
                   const RenameMap &outerRenames,
                   const std::optional<bool> &outerCaster) {
    const auto target{
        resolveTarget(document, placement.importPath, placement.importPathLoc)};
    const auto effectiveCaster{
        placement.casterOverride ? placement.casterOverride : outerCaster};
    if (target.kind == Target::Kind::LAYOUT) {
      lowerFile(target.path, xf * placement.transform,
                composeRename(subtreeRenames(placement.importMaterials,
                                             placement.importPathLoc),
                              outerRenames),
                effectiveCaster, false, placement.importPathLoc);
      return;
    }
    if (effectiveCaster.value_or(false) &&
        target.kind == Target::Kind::CURVES) {
      mDiags.error(placement.casterLoc ? placement.casterLoc
                                       : placement.importPathLoc,
                   smdl::concat("'caster' applies to a mesh file or a shape, "
                                "but ",
                                smdl::QuotedPath(placement.importPath),
                                " is a curves file"));
      throw SkipPlacement();
    }
    if (target.kind == Target::Kind::CURVES) {
      // Fibers have no slots to default from, so an import must bind
      // them, exactly as an asset declaration must.
      if (!placement.importMaterials.bySlot.empty()) {
        mDiags.error(placement.importPathLoc,
                     "a curves file has no material slots; use "
                     "'material <name>' alone");
        throw SkipPlacement();
      }
      if (placement.importMaterials.all.empty()) {
        mDiags
            .error(placement.importPathLoc,
                   smdl::concat("importing the curves file ",
                                smdl::QuotedPath(placement.importPath),
                                " needs a material"))
            .note({}, "write 'import \"<path>\" { material <name> }'");
        throw SkipPlacement();
      }
    }
    auto &item{mResult.items.emplace_back()};
    item.fileName = target.path;
    item.curves.active = target.kind == Target::Kind::CURVES;
    item.objectToWorld = xf * placement.transform * target.correction;
    item.materials = placement.importMaterials;
    item.materials.renames =
        composeRename(document.materialAliases, outerRenames);
    item.caster = effectiveCaster.value_or(false);
  }

  // The `material` assignments written against a layout target, turned
  // into the rename layer they mean there: what the subtree resolves as
  // `from` resolves as `to` instead. The whole-target form has nothing
  // to say about a subtree, since a rename maps names it can spell.
  [[nodiscard]] RenameMap subtreeRenames(const MaterialAssignment &materials,
                                         const LayoutLocation &location) {
    if (!materials.all.empty()) {
      mDiags.error(location,
                   "assigning every material of a layout at once is not "
                   "supported; assign per name with 'material \"<name>\" = "
                   "<material>'");
      throw SkipPlacement();
    }
    return materials.bySlot;
  }

  // Where a written path actually is, and what it names.
  [[nodiscard]] Target resolveTarget(const LayoutDocument &document,
                                     const std::string &path,
                                     const LayoutLocation &location) {
    auto target{Target()};
    auto resolved{resolvePath(document, path, location, true)};
    // An asset directory stands for the mesh its manifest names, placed
    // under the correction the manifest records. The correction applies
    // innermost, so that everything said about the asset still reads as
    // applying to the asset as the user sees it rather than as the file
    // happens to store it.
    try {
      if (std::filesystem::is_directory(resolved)) {
        auto manifest{findAssetManifest(resolved.string())};
        if (manifest.empty()) {
          mDiags.error(location,
                       smdl::concat("cannot import the directory ",
                                    smdl::QuotedPath(resolved.string()),
                                    ": it holds no '.asset' manifest, so it "
                                    "is not an asset"));
          throw SkipPlacement();
        }
        resolved = manifest;
      }
      if (resolved.extension() == ASSET_EXTENSION) {
        const auto asset{readAsset(resolved.string())};
        resolved = asset.renderFileName;
        target.correction = asset.correction;
      }
    } catch (const smdl::Error &error) {
      mDiags.error(location, error.message);
      throw SkipPlacement();
    }
    if (resolved.extension() == ".scene") {
      mDiags.error(location, smdl::concat("the '.scene' format was retired; ",
                                          smdl::QuotedPath(path),
                                          " must be ported to '.layout'"));
      throw SkipPlacement();
    }
    if (resolved.extension() == PLACES_EXTENSION) {
      mDiags.error(location,
                   smdl::concat(smdl::QuotedPath(path),
                                " is a '.places' buffer, which is scattered "
                                "with 'place <asset> * \"<file>\"' rather "
                                "than imported"));
      throw SkipPlacement();
    }
    if (resolved.extension() == CURVES_EXTENSION || sniffCurvesMagic(resolved))
      target.kind = Target::Kind::CURVES;
    else if (resolved.extension() == LAYOUT_EXTENSION ||
             sniffLayoutMagic(resolved))
      target.kind = Target::Kind::LAYOUT;
    target.path = resolved.string();
    return target;
  }

  // Where a written path actually is: beside the file that wrote it, or
  // failing that in one of the asset directories, in the order they were
  // given. An absolute path is taken as written. Returns empty after
  // reporting if `skip` is false; throws `SkipPlacement` otherwise.
  [[nodiscard]] std::filesystem::path
  resolvePath(const LayoutDocument &document, const std::string &path,
              const LayoutLocation &location, bool skip) {
    auto written{std::filesystem::path(path)};
    auto candidates{std::vector<std::filesystem::path>()};
    if (written.is_absolute()) {
      candidates.push_back(written);
    } else {
      candidates.push_back(std::filesystem::path(document.directory) / written);
      for (const auto &directory : mSearch)
        candidates.push_back(std::filesystem::path(directory) / written);
    }
    for (const auto &candidate : candidates)
      if (std::filesystem::exists(candidate)) return candidate;
    // Name every place it was looked for. A path that is subtly wrong is
    // the common case, and the list is what shows which part of it is.
    auto message{
        smdl::concat("cannot find ", smdl::QuotedPath(path), ", looked for:")};
    for (const auto &candidate : candidates)
      message += smdl::concat("\n  ", smdl::QuotedPath(candidate.string()));
    if (mSearch.empty() && !written.is_absolute())
      message += "\n  (pass -asset-dir to say where the asset library is)";
    mDiags.error(location, message);
    if (skip) throw SkipPlacement();
    return {};
  }

  class OpenFile final {
  public:
    std::filesystem::path canonical{};
    std::string fileName{};
    LayoutLocation importSite{};
  };

  LayoutDiagnostics &mDiags;
  const AssetSearchPath &mSearch;
  Layout &mResult;
  std::vector<OpenFile> mOpenFiles{};
};

} // namespace

Layout lowerLayout(LayoutDiagnostics &diags, const std::string &fileName,
                   const AssetSearchPath &search) {
  auto result{Layout()};
  Lowerer(diags, search, result)
      .lowerFile(fileName, float4x4(1.0f), {}, std::nullopt, true, {});
  return result;
}

Layout readLayout(const std::string &fileName, const AssetSearchPath &search) {
  auto diags{LayoutDiagnostics()};
  auto result{lowerLayout(diags, fileName, search)};
  if (!diags.empty()) diags.printAll(smdl::cerrSupportsANSIColors());
  if (diags.hasErrors())
    throw smdl::Error(smdl::concat("cannot read ", smdl::QuotedPath(fileName),
                                   ": ", diags.summary()));
  SMDL_LOG_DEBUG("Read ", smdl::QuotedPath(fileName), ": ", result.items.size(),
                 " item(s)");
  return result;
}

Layout resolveLayoutArgument(const std::string &fileName,
                             const AssetSearchPath &search) {
  auto path{std::filesystem::path(fileName)};
  if (path.extension() == ".scene")
    throw smdl::Error(smdl::concat("the '.scene' format was retired; ",
                                   smdl::QuotedPath(fileName),
                                   " must be ported to '.layout'"));
  // The argument names an asset the same way an import does, so that a
  // prepared asset can be rendered on its own without a layout file to
  // wrap it.
  auto result{Layout()};
  if (std::filesystem::is_directory(path)) {
    auto manifest{findAssetManifest(path.string())};
    if (manifest.empty())
      throw smdl::Error(smdl::concat("cannot render the directory ",
                                     smdl::QuotedPath(path.string()),
                                     ": it holds no '.asset' manifest, so it "
                                     "is not an asset"));
    path = manifest;
  }
  // A bare curves argument (or a manifest naming one) renders with the
  // renderer's default material, which is what a groom wants before its
  // materials are written; a layout binds one explicitly.
  auto classifyCurves{[](const std::filesystem::path &renderPath) {
    return renderPath.extension() == CURVES_EXTENSION ||
           sniffCurvesMagic(renderPath);
  }};
  if (path.extension() == ASSET_EXTENSION) {
    const auto asset{readAsset(path.string())};
    auto &item{result.items.emplace_back()};
    item.fileName = asset.renderFileName;
    item.curves.active = classifyCurves(asset.renderFileName);
    item.objectToWorld = asset.correction;
    result.frontAzimuth = asset.front;
    return result;
  }
  if (path.extension() == LAYOUT_EXTENSION || sniffLayoutMagic(path))
    return readLayout(fileName, search);
  auto &item{result.items.emplace_back()};
  item.fileName = path.string();
  item.curves.active = classifyCurves(path);
  return result;
}
