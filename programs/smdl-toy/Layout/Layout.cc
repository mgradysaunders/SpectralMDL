#include "Layout/Layout.h"

#include "Layout/CameraFile.h"

#include "IO/AssetFile.h"
#include "IO/CurvesFile.h"
#include "IO/PlacesFile.h"

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
  RenameMap result{};
  for (const auto &[from, to] : inner) {
    auto itr{outer.find(to)};
    result.emplace(from, itr == outer.end() ? to : itr->second);
  }
  for (const auto &[from, to] : outer) result.try_emplace(from, to);
  return result;
}

// Does the file begin with the `.curves` magic? Reads just enough bytes
// to decide, so calling it on a large binary mesh costs nothing, and it
// is what keeps the `.curves` extension advisory.
[[nodiscard]] bool sniffCurvesMagic(const std::filesystem::path &path) {
  std::ifstream stream{path, std::ios::binary};
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

// The words said for the two per-placement marks on the place path so
// far: each unset until some site says it, with where it was said, so
// that a word refused further down can point back at it.
class MarkOverrides final {
public:
  std::optional<bool> isCaster{};
  LayoutLocation casterLoc{};
  std::optional<bool> isLight{};
  LayoutLocation lightLoc{};

  // These words under `placement`'s own: the innermost explicit word
  // wins.
  [[nodiscard]] MarkOverrides over(const LayoutPlacement &placement) const {
    MarkOverrides result{*this};
    if (placement.casterOverride) {
      result.isCaster = placement.casterOverride;
      result.casterLoc = placement.casterLoc;
    }
    if (placement.lightOverride) {
      result.isLight = placement.lightOverride;
      result.lightLoc = placement.lightLoc;
    }
    return result;
  }
};

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

// A placement transform at both keys of the shutter, whether anything
// on the path actually carried a `motion` block, and the seconds the
// path adds to the clock of what it places. Composes pairwise, so a
// static factor (an asset's correction, a file's node transform, a bulk
// record) multiplies into both keys, and a moving factor above or below
// it moves everything it holds; the offsets add, since they travel the
// same path the keys do.
class MotionXf final {
public:
  MotionXf() = default;
  MotionXf(const float4x4 &xf) : open(xf), shut(xf) {}

  // A placement: its own operations composed outside its track,
  // sampled at the two instants shifted by the clock this placement
  // sits on, which is the offsets of every place above it.
  MotionXf(const float4x4 &xf, const MotionTrack &track,
           const MotionSampling &sampling, float offset)
      : open(xf), shut(xf), isMoving(!track.empty()), offset(offset) {
    if (isMoving) {
      open = xf * track.at(sampling.open);
      shut = xf * track.at(sampling.shut);
    }
  }

  [[nodiscard]] MotionXf operator*(const MotionXf &other) const {
    MotionXf result{};
    result.open = open * other.open;
    result.shut = shut * other.shut;
    result.isMoving = isMoving || other.isMoving;
    result.offset = offset + other.offset;
    return result;
  }

  // The shut key an item or light records: the composed shut
  // transform when something moved and it differs from the open one,
  // absent otherwise, so that a track whose two samples agree (a shut
  // shutter, or a key pair that restates one transform) lowers to a
  // static placement.
  [[nodiscard]] std::optional<float4x4> shutKey() const {
    if (!isMoving) return std::nullopt;
    for (size_t j = 0; j < 4; j++)
      for (size_t i = 0; i < 4; i++)
        if (open[j][i] != shut[j][i]) return shut;
    return std::nullopt;
  }

  float4x4 open{float4x4(1.0f)};
  float4x4 shut{float4x4(1.0f)};
  bool isMoving{};
  float offset{};
};

// The clip an item lowered from `decl` plays, with the path's offsets
// added to the asset's own.
[[nodiscard]] AnimationSpec animationOf(const LayoutAssetDecl &decl,
                                        const MotionXf &xf) {
  AnimationSpec spec{decl.animation};
  spec.offset += xf.offset;
  return spec;
}

void placeItem(LayoutItem &item, const MotionXf &xf) {
  item.objectToWorld = xf.open;
  item.objectToWorldShut = xf.shutKey();
}

// A batch moves as a whole or not at all, so the shut keys are written
// for every record or for none.
void placeBatch(LayoutItem &item, const std::vector<MotionXf> &xfs) {
  item.batchXfs.reserve(xfs.size());
  for (const auto &xf : xfs) item.batchXfs.push_back(xf.open);
  bool isMoving{};
  for (const auto &xf : xfs) isMoving |= xf.shutKey().has_value();
  if (!isMoving) return;
  item.batchXfsShut.reserve(xfs.size());
  for (const auto &xf : xfs) item.batchXfsShut.push_back(xf.shut);
}

class Lowerer final {
public:
  Lowerer(LayoutDiagnostics &diags, const AssetSearchPath &search,
          const MotionSampling &sampling, Layout &result)
      : mDiags(diags), mSearch(search), mSampling(sampling), mResult(result) {}

  void lowerFile(const std::string &fileName, const MotionXf &xf,
                 const RenameMap &outerRenames, const MarkOverrides &outerMarks,
                 bool isEntry, const LayoutLocation &importSite) {
    std::error_code ignored{};
    std::filesystem::path canonical{
        std::filesystem::weakly_canonical(fileName, ignored)};
    if (canonical.empty()) canonical = fileName;
    for (const auto &frame : mOpenFiles)
      if (frame.canonical == canonical) {
        std::string message{"import cycle:"};
        bool isInCycle{false};
        for (const auto &open : mOpenFiles) {
          if (open.canonical == canonical) isInCycle = true;
          if (isInCycle) message += smdl::concat(" ", open.fileName, " ->");
        }
        message += smdl::concat(" ", fileName);
        LayoutDiagnostic &error{mDiags.error(importSite, message)};
        for (const auto &open : mOpenFiles)
          if (open.importSite)
            error.note(open.importSite, "imported from here");
        return;
      }
    const size_t firstNestedDiag{mDiags.all().size()};
    const LayoutSource *source{};
    try {
      source = &mDiags.loadSource(fileName);
    } catch (const smdl::Error &error) {
      if (!importSite) throw;
      mDiags.error(importSite, error.message);
      return;
    }
    const LayoutDocument document{
        parseLayout(mDiags, *source,
                    std::filesystem::path(fileName).parent_path().string())};
    mOpenFiles.push_back({canonical, fileName, importSite});
    lowerDocument(document, xf, outerRenames, outerMarks, isEntry);
    mOpenFiles.pop_back();
    // Everything the nested file reported, parse and lowering alike,
    // points back at the import that pulled it in.
    if (importSite)
      mDiags.noteAllFrom(firstNestedDiag, importSite, "imported from here");
  }

private:
  void lowerDocument(const LayoutDocument &document, const MotionXf &xf,
                     const RenameMap &outerRenames,
                     const MarkOverrides &outerMarks, bool isEntry) {
    // Only the entry file's sky, haze, and medium take effect. An
    // imported layout carrying its own is a layout that can also render
    // standalone, so say what is being ignored rather than erroring, and
    // never adopt it silently.
    if (isEntry) {
      mResult.sky = document.sky;
      mResult.haze = document.haze;
      mResult.hasHaze = bool(document.hazeLoc);
      mResult.entryMaterialAliases = document.materialAliases;
      if (!document.mediumName.empty())
        mResult.exteriorMediumName = document.mediumName;
      if (!document.iblPath.empty())
        if (std::filesystem::path resolved{resolvePath(
                document, document.iblPath, document.iblPathLoc, false)};
            !resolved.empty())
          mResult.sky.iblFileName = resolved.string();
    } else {
      if (document.skyLoc)
        mDiags.warn(document.skyLoc,
                    "the 'sky' of an imported layout is ignored (only the "
                    "entry layout's sky takes effect)");
      if (document.hazeLoc)
        mDiags.warn(document.hazeLoc,
                    "the 'haze' of an imported layout is ignored (only the "
                    "entry layout's haze takes effect)");
      if (document.mediumLoc)
        mDiags.warn(document.mediumLoc,
                    "the 'medium' of an imported layout is ignored (only "
                    "the entry layout's medium takes effect)");
    }
    std::vector<bool> usedAssets(document.assets.size(), false);
    std::vector<bool> usedGroups(document.groups.size(), false);
    std::vector<bool> usedLights(document.lights.size(), false);
    std::vector<GroupFrame> groupStack{};
    lowerPlacements(document, document.placements, xf, outerRenames, outerMarks,
                    usedAssets, usedGroups, usedLights, groupStack,
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
                       const MotionXf &xf, const RenameMap &outerRenames,
                       const MarkOverrides &outerMarks,
                       std::vector<bool> &usedAssets,
                       std::vector<bool> &usedGroups,
                       std::vector<bool> &usedLights,
                       std::vector<GroupFrame> &groupStack,
                       const std::string &namePrefix) {
    for (const auto &placement : placements) {
      try {
        if (placement.kind == LayoutPlacement::Kind::PLACE) {
          lowerPlace(document, placement, xf, outerRenames, outerMarks,
                     usedAssets, usedGroups, usedLights, groupStack,
                     namePrefix);
        } else {
          lowerImport(document, placement, xf, outerRenames, outerMarks);
        }
        // NOLINTNEXTLINE
      } catch (const SkipPlacement &) {
        // The diagnostic is already emitted; move on to the next.
      }
    }
  }

  void lowerPlace(const LayoutDocument &document,
                  const LayoutPlacement &placement, const MotionXf &xf,
                  const RenameMap &outerRenames,
                  const MarkOverrides &outerMarks,
                  std::vector<bool> &usedAssets, std::vector<bool> &usedGroups,
                  std::vector<bool> &usedLights,
                  std::vector<GroupFrame> &groupStack,
                  const std::string &namePrefix) {
    const std::string placeName{placement.asName.empty() ? namePrefix
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
      LayoutDiagnostic &error{
          mDiags.error(placement.assetNameLoc,
                       smdl::concat("no asset, group, or light named ",
                                    smdl::Quoted(placement.assetName)))};
      std::vector<std::string_view> candidates{};
      for (const auto &asset : document.assets)
        candidates.push_back(asset.name);
      for (const auto &declared : document.groups)
        candidates.push_back(declared.name);
      for (const auto &declared : document.lights)
        candidates.push_back(declared.name);
      if (const std::string_view nearest{
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
    if (light && placement.lightOverride)
      mDiags.warn(placement.lightLoc,
                  smdl::concat("'light' on the light ",
                               smdl::Quoted(placement.assetName),
                               " has no effect"));
    // The place's own overrides apply outside everything the target says
    // for itself, and inside everything above: each syntactic enclosure
    // adds its rename layer one step further out.
    const RenameMap baseOuter{composeRename(placement.overrides, outerRenames)};
    // The marks compose the other way round: the innermost explicit
    // word wins, and the asset's own mark is the default.
    const MarkOverrides effectiveMarks{outerMarks.over(placement)};
    // The placement's own keys under everything above it. The block's
    // shut key, when there is one, is absolute: it stands where the
    // open operations stand, and everything below composes under both.
    // The clock this placement sits on: every enclosing `offset` plus
    // its own. Its track and the clips of what it places read the same
    // one, so a placement written one second behind is one second behind
    // in both.
    const float offset{xf.offset + placement.animationOffset.value_or(0.0f)};
    const MotionSampling sampling{mSampling.shiftedBy(offset)};
    const MotionXf placeXf{
        xf * MotionXf(placement.transform, placement.motion, sampling,
                      placement.animationOffset.value_or(0.0f))};
    if (!placement.motion.empty() &&
        placement.motion.hasKeyBetween(sampling.open, sampling.shut))
      mDiags.warn(placement.motionLoc,
                  "a key of this 'motion' sits inside the shutter, so the "
                  "placement moves along the chord of its two ends");
    if (!placement.placesPath.empty()) {
      // The bulk form: one instance per record, each record's transform
      // standing where a one-line place's operations would, and each
      // record's variant composing where that place's own overrides
      // would (inside this statement's, outside the target's).
      const std::filesystem::path resolved{resolvePath(
          document, placement.placesPath, placement.placesPathLoc, true)};
      PlacesFile places{};
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
                                    smdl::Counted(placement.variants.size(),
                                                  "variant block"),
                                    placement.variants.size() == 1
                                        ? " is declared"
                                        : " are declared"));
          throw SkipPlacement();
        }
      // The composed rename layer per variant, folded once rather than
      // once per record: a scatter reuses a handful of variants across
      // thousands of records.
      std::vector<RenameMap> outerByVariant{};
      outerByVariant.reserve(placement.variants.size());
      for (const auto &variant : placement.variants)
        outerByVariant.push_back(composeRename(variant, baseOuter));
      SMDL_LOG_DEBUG("Scattering ",
                     smdl::Counted(places.transforms.size(), "record"),
                     " from ", smdl::QuotedPath(resolved.string()));
      const auto recordXf{
          [&](size_t i) { return placeXf * MotionXf(places.transforms[i]); }};
      const auto outerFor{[&](size_t i) -> const RenameMap & {
        const uint32_t variantIndex{
            places.hasVariants() ? places.variants[i] : PlacesFile::NO_VARIANT};
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
      Target target{};
      const bool batchable{
          decl != nullptr &&
          (decl->primitive.isActive() ||
           (target = resolveTarget(document, decl->path, decl->pathLoc)).kind !=
               Target::Kind::LAYOUT)};
      if (batchable && !decl->primitive.isActive())
        checkAssetTargetKind(*decl, target);
      if (!batchable) {
        for (size_t i = 0; i < places.transforms.size(); i++)
          lowerPlaceTarget(document, decl, group, light, placement.assetNameLoc,
                           recordXf(i), outerFor(i), effectiveMarks, usedAssets,
                           usedGroups, usedLights, groupStack, placeName);
        return;
      }
      // Batch by variant class in first-appearance order. Instance
      // order within the scatter is therefore grouped by class rather
      // than by record, which only `state::object_id` could ever
      // observe.
      std::map<uint32_t, size_t> classSlots{};
      std::vector<std::pair<uint32_t, std::vector<MotionXf>>> batches{};
      for (size_t i = 0; i < places.transforms.size(); i++) {
        const uint32_t variantIndex{
            places.hasVariants() ? places.variants[i] : PlacesFile::NO_VARIANT};
        const auto [slot, isNew]{
            classSlots.try_emplace(variantIndex, batches.size())};
        if (isNew) batches.emplace_back(variantIndex, std::vector<MotionXf>());
        batches[slot->second].second.push_back(
            decl->primitive.isActive()
                ? recordXf(i) * MotionXf(decl->transform)
                : recordXf(i) * MotionXf(decl->transform) *
                      MotionXf(target.correction));
      }
      // The marks are resolved before any item exists, so a refused
      // mark leaves nothing half-built behind its diagnostic.
      const bool isCaster{isCasterOf(*decl, effectiveMarks, &target)};
      const bool hasLightMark{isLightOf(*decl, effectiveMarks, &target)};
      for (auto &[variantIndex, xfs] : batches) {
        LayoutItem &item{mResult.items.emplace_back()};
        if (decl->primitive.isActive()) {
          item.primitive = decl->primitive;
        } else if (target.kind == Target::Kind::CURVES) {
          item.fileName = target.path;
          item.curves = decl->curves;
          item.curves.isActive = true;
        } else {
          item.fileName = target.path;
          item.selection = decl->selection;
          item.subdiv = decl->subdiv;
          item.animation = animationOf(*decl, placeXf);
        }
        item.materials = decl->materials;
        item.materials.renames = composeRename(
            document.materialAliases, variantIndex == PlacesFile::NO_VARIANT
                                          ? baseOuter
                                          : outerByVariant[variantIndex]);
        item.isCaster = isCaster;
        item.isLight = hasLightMark;
        item.isCausticLight = decl->isCaustic;
        item.placeName = placeName;
        if (xfs.size() == 1) {
          placeItem(item, xfs[0]);
        } else {
          placeBatch(item, xfs);
        }
      }
      return;
    }
    lowerPlaceTarget(document, decl, group, light, placement.assetNameLoc,
                     placeXf, baseOuter, effectiveMarks, usedAssets, usedGroups,
                     usedLights, groupStack, placeName);
  }

  // One resolved placement of `decl`, `group`, or `light` (exactly one
  // is set) under the fully combined placement transform: the shared
  // tail of the ordinary and bulk `place` forms.
  void lowerPlaceTarget(
      const LayoutDocument &document, const LayoutAssetDecl *decl,
      const LayoutGroupDecl *group, const LayoutLightDecl *lightDecl,
      const LayoutLocation &nameLoc, const MotionXf &combinedXf,
      const RenameMap &effectiveOuter, const MarkOverrides &effectiveMarks,
      std::vector<bool> &usedAssets, std::vector<bool> &usedGroups,
      std::vector<bool> &usedLights, std::vector<GroupFrame> &groupStack,
      const std::string &placeName) {
    if (lightDecl) {
      LayoutLight &light{mResult.lights.emplace_back()};
      light.decl = *lightDecl;
      const MotionXf lightXf{combinedXf * MotionXf(lightDecl->transform)};
      light.lightToWorld = lightXf.open;
      light.lightToWorldShut = lightXf.shutKey();
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
          std::string message{"group cycle:"};
          bool isInCycle{false};
          for (const auto &open : groupStack) {
            if (open.group == group) isInCycle = true;
            if (isInCycle)
              message += smdl::concat(" ", open.group->name, " ->");
          }
          message += smdl::concat(" ", group->name);
          LayoutDiagnostic &error{mDiags.error(nameLoc, message)};
          for (const auto &open : groupStack)
            error.note(open.site, "placed from here");
          throw SkipPlacement();
        }
      groupStack.push_back({group, nameLoc});
      lowerPlacements(document, group->placements, combinedXf, effectiveOuter,
                      effectiveMarks, usedAssets, usedGroups, usedLights,
                      groupStack, placeName);
      groupStack.pop_back();
      return;
    }
    // A primitive is pure geometry by construction: no path to resolve,
    // no file to read, and the parser already guaranteed the assignment.
    if (decl->primitive.isActive()) {
      const bool isCaster{isCasterOf(*decl, effectiveMarks, nullptr)};
      const bool hasLightMark{isLightOf(*decl, effectiveMarks, nullptr)};
      LayoutItem &item{mResult.items.emplace_back()};
      item.primitive = decl->primitive;
      placeItem(item, combinedXf * MotionXf(decl->transform));
      item.materials = decl->materials;
      item.materials.renames =
          composeRename(document.materialAliases, effectiveOuter);
      item.isCaster = isCaster;
      item.isLight = hasLightMark;
      item.isCausticLight = decl->isCaustic;
      item.placeName = placeName;
      return;
    }
    const Target target{resolveTarget(document, decl->path, decl->pathLoc)};
    checkAssetTargetKind(*decl, target);
    if (target.kind == Target::Kind::LAYOUT) {
      // The asset's own marks pass down like overrides, since there is
      // no one item for them to mark; the checks still run here, so
      // that `light off` under `caustic` is refused wherever it is said.
      (void)isLightOf(*decl, effectiveMarks, nullptr);
      MarkOverrides passed{effectiveMarks};
      if (!passed.isCaster && decl->isCaster) {
        passed.isCaster = true;
        passed.casterLoc = decl->casterLoc;
      }
      if (!passed.isLight && (decl->isLight || decl->isCaustic)) {
        passed.isLight = true;
        passed.lightLoc = decl->lightLoc ? decl->lightLoc : decl->nameLoc;
      }
      lowerFile(target.path, combinedXf * MotionXf(decl->transform),
                composeRename(subtreeRenames(decl->materials, decl->pathLoc),
                              effectiveOuter),
                passed, false, decl->pathLoc);
      return;
    }
    const bool isCaster{isCasterOf(*decl, effectiveMarks, &target)};
    const bool hasLightMark{isLightOf(*decl, effectiveMarks, &target)};
    LayoutItem &item{mResult.items.emplace_back()};
    item.fileName = target.path;
    placeItem(item, combinedXf * MotionXf(decl->transform) *
                        MotionXf(target.correction));
    if (target.kind == Target::Kind::CURVES) {
      item.curves = decl->curves;
      item.curves.isActive = true;
    } else {
      item.selection = decl->selection;
      item.subdiv = decl->subdiv;
      item.animation = animationOf(*decl, combinedXf);
    }
    item.materials = decl->materials;
    item.materials.renames =
        composeRename(document.materialAliases, effectiveOuter);
    item.isCaster = isCaster;
    item.isLight = hasLightMark;
    item.isCausticLight = decl->isCaustic;
    item.placeName = placeName;
  }

  // The composed caster mark of an item lowered from `decl`, through a
  // target of known kind or through no target at all (a shape): a groom
  // cannot carry it, since the manifold walk has no smooth surface to
  // run on there.
  [[nodiscard]] bool isCasterOf(const LayoutAssetDecl &decl,
                                const MarkOverrides &marks,
                                const Target *target) {
    const bool isCaster{marks.isCaster.value_or(decl.isCaster)};
    if (isCaster && target && target->kind == Target::Kind::CURVES) {
      mDiags.error(decl.casterLoc ? decl.casterLoc : decl.pathLoc,
                   smdl::concat("'caster' applies to a mesh file or a shape, "
                                "but ",
                                smdl::QuotedPath(decl.path),
                                " is a curves file"));
      throw SkipPlacement();
    }
    return isCaster;
  }

  // The composed light mark, likewise: `caustic` implies it and cannot
  // be turned off underneath, and a groom cannot carry it, since light
  // selection has no way to sample a fiber.
  [[nodiscard]] bool isLightOf(const LayoutAssetDecl &decl,
                               const MarkOverrides &marks,
                               const Target *target) {
    if (decl.isCaustic && marks.isLight == std::optional<bool>(false)) {
      mDiags
          .error(marks.lightLoc,
                 smdl::concat("'light off' cannot apply to ",
                              smdl::Quoted(decl.name),
                              ": its 'caustic' mark makes it a light"))
          .note(decl.nameLoc, "declared 'caustic' here");
      throw SkipPlacement();
    }
    const bool isLight{marks.isLight.value_or(decl.isLight) || decl.isCaustic};
    if (isLight && target && target->kind == Target::Kind::CURVES) {
      mDiags.error(decl.lightLoc ? decl.lightLoc : decl.pathLoc,
                   smdl::concat("'light' applies to a mesh file or a shape, "
                                "but ",
                                smdl::QuotedPath(decl.path),
                                " is a curves file"));
      throw SkipPlacement();
    }
    return isLight;
  }

  // The kind-specific properties an asset block can write, cross-checked
  // once the target's kind is known: the parser cannot know what a path
  // names, so this is where 'subdivide' on a groom or 'ribbon' on a
  // mesh is caught. Selecting or subdividing a layout would mean
  // applying it to every file the layout names, which is never what the
  // author means; say so instead of silently ignoring it.
  void checkAssetTargetKind(const LayoutAssetDecl &decl, const Target &target) {
    if (target.kind != Target::Kind::MESH &&
        (!decl.selection.patterns.empty() || decl.selection.shouldRecenter ||
         decl.subdiv.isActive())) {
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
    if (target.kind != Target::Kind::MESH && decl.animationLoc) {
      mDiags.error(decl.animationLoc,
                   smdl::concat("'animation' applies to a mesh file, but ",
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
                   const LayoutPlacement &placement, const MotionXf &xf,
                   const RenameMap &outerRenames,
                   const MarkOverrides &outerMarks) {
    const Target target{
        resolveTarget(document, placement.importPath, placement.importPathLoc)};
    const MarkOverrides effectiveMarks{outerMarks.over(placement)};
    if (target.kind == Target::Kind::LAYOUT) {
      lowerFile(target.path, xf * MotionXf(placement.transform),
                composeRename(subtreeRenames(placement.importMaterials,
                                             placement.importPathLoc),
                              outerRenames),
                effectiveMarks, false, placement.importPathLoc);
      return;
    }
    if (target.kind == Target::Kind::CURVES) {
      const auto refuseMark{[&](const LayoutLocation &markLoc,
                                std::string_view word) {
        mDiags.error(markLoc ? markLoc : placement.importPathLoc,
                     smdl::concat("'", word,
                                  "' applies to a mesh file or a shape, but ",
                                  smdl::QuotedPath(placement.importPath),
                                  " is a curves file"));
        throw SkipPlacement();
      }};
      if (effectiveMarks.isCaster.value_or(false))
        refuseMark(effectiveMarks.casterLoc, "caster");
      if (effectiveMarks.isLight.value_or(false))
        refuseMark(effectiveMarks.lightLoc, "light");
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
    LayoutItem &item{mResult.items.emplace_back()};
    item.fileName = target.path;
    item.curves.isActive = target.kind == Target::Kind::CURVES;
    if (target.kind == Target::Kind::MESH) item.animation.offset = xf.offset;
    placeItem(item,
              xf * MotionXf(placement.transform) * MotionXf(target.correction));
    item.materials = placement.importMaterials;
    item.materials.renames =
        composeRename(document.materialAliases, outerRenames);
    item.isCaster = effectiveMarks.isCaster.value_or(false);
    item.isLight = effectiveMarks.isLight.value_or(false);
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
    Target target{};
    std::filesystem::path resolved{resolvePath(document, path, location, true)};
    // An asset directory stands for the mesh its manifest names, placed
    // under the correction the manifest records. The correction applies
    // innermost, so that everything said about the asset still reads as
    // applying to the asset as the user sees it rather than as the file
    // happens to store it.
    try {
      if (std::filesystem::is_directory(resolved)) {
        std::string manifest{findAssetManifest(resolved.string())};
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
        const AssetFile asset{readAssetFile(resolved.string())};
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
    else if (resolved.extension() == LAYOUT_EXTENSION)
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
              const LayoutLocation &location, bool shouldSkip) {
    std::filesystem::path written{path};
    std::vector<std::filesystem::path> candidates{};
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
    std::string message{
        smdl::concat("cannot find ", smdl::QuotedPath(path), ", looked for:")};
    for (const auto &candidate : candidates)
      message += smdl::concat("\n  ", smdl::QuotedPath(candidate.string()));
    if (mSearch.empty() && !written.is_absolute())
      message += "\n  (pass -asset-dir to say where the asset library is)";
    mDiags.error(location, message);
    if (shouldSkip) throw SkipPlacement();
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
  MotionSampling mSampling{};
  Layout &mResult;
  std::vector<OpenFile> mOpenFiles{};
};

} // namespace

Layout lowerLayout(LayoutDiagnostics &diags, const std::string &fileName,
                   const AssetSearchPath &search,
                   const MotionSampling &sampling) {
  Layout result{};
  Lowerer(diags, search, sampling, result)
      .lowerFile(fileName, MotionXf(), {}, {}, true, {});
  return result;
}

Layout readLayout(const std::string &fileName, const AssetSearchPath &search,
                  const MotionSampling &sampling) {
  LayoutDiagnostics diags{};
  Layout result{lowerLayout(diags, fileName, search, sampling)};
  diags.printAllAndRefuse(fileName);
  SMDL_LOG_DEBUG("Read ", smdl::QuotedPath(fileName), ": ",
                 smdl::Counted(result.items.size(), "item"));
  return result;
}

Layout resolveLayoutArgument(const std::string &fileName,
                             const AssetSearchPath &search,
                             const MotionSampling &sampling) {
  std::filesystem::path path{fileName};
  if (path.extension() == ".scene")
    throw smdl::Error(smdl::concat("the '.scene' format was retired; ",
                                   smdl::QuotedPath(fileName),
                                   " must be ported to '.layout'"));
  if (path.extension() == CAMERA_EXTENSION)
    throw smdl::Error(
        smdl::concat(smdl::QuotedPath(fileName),
                     " is a camera, not a scene; give it with '-camera', or "
                     "name it after the layout it belongs to and it is found "
                     "beside it"));
  // The argument names an asset the same way an import does, so that a
  // prepared asset can be rendered on its own without a layout file to
  // wrap it.
  Layout result{};
  if (std::filesystem::is_directory(path)) {
    std::string manifest{findAssetManifest(path.string())};
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
  // With no layout to carry marks, every emitter is a light: nothing
  // could say otherwise, and a bare model with a lamp in it expects to be
  // lit by it.
  if (path.extension() == ASSET_EXTENSION) {
    const AssetFile asset{readAssetFile(path.string())};
    LayoutItem &item{result.items.emplace_back()};
    item.fileName = asset.renderFileName;
    item.curves.isActive = classifyCurves(asset.renderFileName);
    item.objectToWorld = asset.correction;
    item.isLight = !item.curves.isActive;
    result.frontAzimuth = asset.front;
    return result;
  }
  if (path.extension() == LAYOUT_EXTENSION)
    return readLayout(fileName, search, sampling);
  LayoutItem &item{result.items.emplace_back()};
  item.fileName = path.string();
  item.curves.isActive = classifyCurves(path);
  item.isLight = !item.curves.isActive;
  return result;
}
