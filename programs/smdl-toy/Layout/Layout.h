/// \file
/// The layout format: the document model, the parser entry points, and
/// the lowering that turns a document into the flat item list the
/// renderer consumes.
#pragma once

#include <cstdint>
#include <map>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

#include "Common.h"

// For `ObjectSelection`, which an `import` block spells and the
// shared import resolves.
#include "IO/MeshImport.h"

#include "Layout/LayoutDiagnostics.h"

/// Where to look for a relative path that is not found beside the file
/// that wrote it: the `-asset-dir` directories, in the order they were
/// given.
///
/// A layout names assets, never where a machine keeps them, so the same
/// file renders wherever the library sits and a layout directory stays
/// self-contained and zippable. Nothing may ever make an absolute path
/// the natural spelling.
///
using AssetSearchPath = std::vector<std::string>;

/// The extension that conventionally marks a layout file. Advisory: the
/// `#smdl layout` first line is what actually decides.
constexpr std::string_view LAYOUT_EXTENSION = ".layout";

/// The magic that must begin the first line of a layout file. It starts
/// with `#`, so it reads as a comment to the grammar and as an identity
/// to everything else, exactly as `#smdl` marks an SMDL file.
constexpr std::string_view LAYOUT_MAGIC = "#smdl layout";

/// How an asset wants its meshes refined at load time. See the
/// `subdivide` and `displace` operations in the layout grammar.
class SubdivSpec final {
public:
  /// Which topological split refinement uses, decided by the faces the
  /// mesh is authored with rather than by the look wanted. This is
  /// independent of `smooth`: see the 2x2 of schemes in
  /// `subdivideMesh()`.
  enum class Scheme {
    CATMARK, ///< Split polygons of any size into quads.
    LOOP,    ///< Split triangles into triangles; triangles only.
  };

  /// The uniform refinement levels, 0 for none. Each level multiplies
  /// the face count by 4.
  uint32_t levels{};

  /// The topological split.
  ///
  /// Catmull-Clark accepts polygons of any size; Loop accepts only triangles,
  /// and the import triangulates for it.
  Scheme scheme{Scheme::CATMARK};

  /// Smooth refinement? True snaps the result to the limit surface of
  /// `scheme`, which is what a modeling package shows. False leaves
  /// every vertex and UV where linear interpolation puts it, raising
  /// the sampling density without moving the surface: the refinement a
  /// displacement map authored against the mesh as modeled wants.
  bool smooth{true};

  /// Apply the material `geometry.displacement` to the final vertices?
  bool displace{};

  /// Anything to do at all?
  [[nodiscard]] bool active() const noexcept { return levels > 0 || displace; }

  /// A key that distinguishes two specs, for callers that cache by
  /// (file, spec). Empty for the inactive default, so a file placed
  /// without the feature keeps its historical cache key.
  [[nodiscard]] std::string key() const {
    if (!active()) return {};
    auto result{std::string("subdivide ") + std::to_string(levels)};
    if (scheme == Scheme::LOOP) result += " loop";
    if (!smooth) result += " linear";
    if (displace) result += " displace";
    return result;
  }
};

/// A built-in analytic shape an `asset` can be declared from instead of
/// a file: `asset ball = sphere { radius 0.05 material lamp }`.
///
/// The shapes are authored in a fixed object space, and any other
/// orientation or proportion is spelled with the asset's own transform
/// operations: the sphere and the box are centered at the origin, the
/// box spanning `[-size/2, +size/2]`; the disk lies in the XY plane
/// facing +Z; the cylinder and cone stand on the XY plane and rise
/// along +Z to `height` (the cone's apex at the top), both closed by
/// caps so that a transmissive material gets a watertight interior. The
/// disk is the one open shape.
///
/// A centered box is what makes it the container a heterogeneous volume
/// wants: the asset's own `translate` composes into the placement
/// rather than into object space, so the box the medium fills is
/// exactly `[-size/2, +size/2]` wherever the declaration puts it, which
/// is what a material's `density_bound_min` and `density_bound_max`
/// state.
///
/// A primitive has no mesh slots, no objects to `select`, and nothing
/// to `subdivide` or `displace`, so it requires exactly one thing an
/// asset block can say: `material <name>`. The geometry itself is
/// analytic (see `Primitive.h`): exact hits and exact normals, with no
/// tessellation anywhere.
class PrimitiveSpec final {
public:
  enum class Shape {
    NONE, ///< Not a primitive: the asset names a file.
    SPHERE,
    BOX,
    DISK,
    CYLINDER,
    CONE,
  };
  Shape shape{Shape::NONE};
  float radius{1.0f};
  float height{1.0f};
  float3 size{1.0f};

  /// Is a primitive at all?
  [[nodiscard]] bool active() const noexcept { return shape != Shape::NONE; }

  /// Does the shape have a `radius` to speak of?
  [[nodiscard]] bool hasRadius() const noexcept {
    return active() && shape != Shape::BOX;
  }

  /// Does the shape have a `height` to speak of?
  [[nodiscard]] bool hasHeight() const noexcept {
    return shape == Shape::CYLINDER || shape == Shape::CONE;
  }

  /// Does the shape have a `size` to speak of?
  [[nodiscard]] bool hasSize() const noexcept { return shape == Shape::BOX; }

  /// The shape keyword, as the grammar spells it.
  [[nodiscard]] std::string_view name() const noexcept {
    switch (shape) {
    case Shape::SPHERE:
      return "sphere";
    case Shape::BOX:
      return "box";
    case Shape::DISK:
      return "disk";
    case Shape::CYLINDER:
      return "cylinder";
    case Shape::CONE:
      return "cone";
    default:
      return "none";
    }
  }

  /// A key that distinguishes two specs, for callers that cache by it.
  [[nodiscard]] std::string key() const {
    if (!active()) return {};
    auto result{std::string(name())};
    if (hasRadius()) result += " r=" + std::to_string(radius);
    if (hasHeight()) result += " h=" + std::to_string(height);
    if (hasSize())
      result += " s=" + std::to_string(size.x) + "," + std::to_string(size.y) +
                "," + std::to_string(size.z);
    return result;
  }
};

/// How an asset whose source is a `.curves` file wants its fibers
/// rendered. See `CurvesFile.h` for the file format and `Curves.h` for
/// the state conventions.
///
/// The split of responsibilities is deliberate: the **basis** (what the
/// stored points mean) is a fact about the data and lives in the binary
/// header, while the **cross-section mode** here is a rendering
/// fidelity choice about the same data, exactly as `subdivide` is for a
/// mesh, so it lives in the grammar where one groom can be placed as
/// cheap ribbons in one layout and true tubes in another.
class CurvesSpec final {
public:
  enum class Mode {
    TUBE,   ///< A swept surface: real geometry, allows closeups.
    RIBBON, ///< Camera-facing flat curves: the fast mode for dense,
            ///< distant fibers.
  };

  /// Is a curves item at all? The parser never sets this (it cannot
  /// know what a path names); the lowering sets it when the target
  /// classifies as a `.curves` file.
  bool active{};

  /// The cross-section rendered around the stored points.
  Mode mode{Mode::TUBE};

  /// Did the declaration say `tube` or `ribbon` explicitly? Kept so the
  /// lowering can reject the ops on a target that turns out not to be
  /// curves.
  bool modeSet{};

  /// A uniform multiplier on every stored radius, applied at load. This
  /// earns its place because no placement transform can express it: a
  /// uniform scale thickens and lengthens together, and width alone is
  /// exactly what a groom most often needs adjusted.
  float radiusScale{1.0f};

  /// Did the declaration write any curves operation at all?
  [[nodiscard]] bool anyOps() const noexcept {
    return modeSet || radiusScale != 1.0f;
  }

  /// A key that distinguishes two specs, for callers that cache by
  /// (file, spec).
  [[nodiscard]] std::string key() const {
    if (!active) return {};
    auto result{
        std::string(mode == Mode::RIBBON ? "curves ribbon" : "curves tube")};
    if (radiusScale != 1.0f) result += " x" + std::to_string(radiusScale);
    return result;
  }
};

/// What one lowered item's meshes are shaded by, whatever the mesh file
/// calls its own materials.
///
/// Exported material names cannot carry identity (a library exports
/// eleven unrelated rocks all calling their one material `MatID_1`), so
/// shading is said at the `asset` declaration or the import site, and
/// this class is how what was said there travels with the item.
///
/// Resolution is two maps applied in sequence. The **assignment** (`all`
/// and `bySlot`) speaks about the mesh file's own slot names. The
/// **renames** map then speaks about the result: the lowering folds the
/// declaring file's `material` aliases and every import-site assignment
/// on the chain above it into this one map, which is what makes alias
/// scoping and subtree overrides per-item facts instead of scene-global
/// state.
///
class MaterialAssignment final {
public:
  /// The material every slot resolves to, or empty.
  std::string all{};

  /// Per-slot assignments, which win over `all` where they apply. Keyed
  /// by the name the mesh file uses.
  std::map<std::string, std::string, std::less<>> bySlot{};

  /// Renames applied to the result of the slot resolution: the
  /// declaring file's aliases and the import chain's overrides, folded
  /// into one map by the lowering. Applied once, never chained.
  std::map<std::string, std::string, std::less<>> renames{};

  /// Anything to say at all?
  [[nodiscard]] bool empty() const noexcept {
    return all.empty() && bySlot.empty() && renames.empty();
  }

  /// What the item shades `name` with: the per-slot assignment, else
  /// the whole assignment, else `name` itself, then through `renames`.
  [[nodiscard]] std::string_view resolve(std::string_view name) const {
    if (auto slot{bySlot.find(name)}; slot != bySlot.end())
      name = slot->second;
    else if (!all.empty())
      name = all;
    if (auto rename{renames.find(name)}; rename != renames.end())
      name = rename->second;
    return name;
  }

  /// A key that distinguishes two assignments, for callers that cache
  /// by (file, assignment). Empty for the default, so an item that says
  /// nothing about materials keeps its historical cache key.
  [[nodiscard]] std::string key() const {
    if (empty()) return {};
    auto result{std::string("material")};
    if (!all.empty()) result += " " + all;
    for (const auto &[slot, target] : bySlot)
      result += " \"" + slot + "\"=" + target;
    for (const auto &[from, to] : renames) result += " ~\"" + from + "\"=" + to;
    return result;
  }
};

/// The framing at shutter shut, the `motion` block inside `camera`.
/// A key not restated holds its open value, so a block that names only
/// `look_from` is a dolly and one that names only `look_to` is a pan.
/// The same word on a `place` restates that placement at shutter shut;
/// see `LayoutPlacement::motion`.
class LayoutCameraMotion final {
public:
  std::optional<float3> lookFrom{};
  std::optional<float3> lookTo{};
  std::optional<float3> lookUp{};
};

/// The camera a layout's `camera` directive describes.
///
/// Everything is optional and unset by default. The built-in defaults
/// are the base, the file overrides those, and explicit command-line
/// flags override the file.
///
class LayoutCamera final {
public:
  std::optional<int2> resolution{};
  std::optional<float3> lookFrom{};
  std::optional<float3> lookTo{};
  std::optional<float3> lookUp{};

  /// The shut keys, present iff a `motion` block appeared. The keys
  /// are absolute, written against this block's own framing: a flag
  /// that replaces the framing drops them (see `main()`).
  std::optional<LayoutCameraMotion> motion{};
  std::optional<float> fovYDeg{};
  std::optional<float> fStop{};
  std::optional<float> aperture{};
  std::optional<float> focus{};
  std::optional<int> blades{};
  std::optional<float> bladeAngleDeg{};
  std::optional<float> distortionK1{};
  std::optional<float> distortionK2{};
  std::optional<bool> distortionFit{};
  std::optional<float> vignetting{};
  std::optional<float> catEye{};
  std::optional<float> catEyeRadius{};
};

/// The environment a layout's `sky` directive describes.
///
/// Everything is optional and merged with the command line the same
/// way `LayoutCamera` works.
///
class LayoutSky final {
public:
  /// `none`: no environment at all, mirroring `-no-sky`.
  std::optional<bool> none{};

  std::optional<float> sunZenith{};
  std::optional<float> sunAzimuth{};
  std::optional<float> visibility{};
  std::optional<float> waterVapor{};
  std::optional<float> scale{};
  std::optional<float> moonPhase{};
  std::optional<float> moonDistance{};

  /// An image-based environment, path resolved by the lowering, which
  /// takes the place of the procedural sun and sky exactly as `-ibl`
  /// does.
  std::optional<std::string> iblFileName{};
  std::optional<float> iblScale{};
};

/// The exterior atmosphere a layout's `haze` directive describes: the
/// distance haze that produces aerial perspective, whose extinction
/// falls off exponentially with height.
///
/// Writing the block at all turns the haze on. Everything in it is
/// optional and merged with the command line the same way `LayoutSky`
/// works, and an unwritten `visibility` follows the sky's. The haze is
/// the medium of everything outside all geometry, so it and the
/// `medium` directive cannot both name the exterior.
///
/// The haze and the sun-sky model overlap: the model is fitted for an
/// observer under the whole atmospheric column, aerosol included, so a
/// ray that ends on the sky is attenuated by a layer whose effect the
/// radiance it carries already accounts for. Over the finite paths
/// aerial perspective is about, which the model has no notion of, the
/// haze is exactly what is missing; toward the sky it is counted twice.
/// Matching the two visibilities keeps them reading consistently, and
/// keeping the haze thin keeps the overlap small. The two carry the
/// same aerosol, but only the sun-sky model carries gaseous absorption,
/// so a distant surface is not dimmed in the water bands the way the
/// sky behind it is.
///
class LayoutHaze final {
public:
  /// `none`: no haze at all, mirroring `-no-haze`.
  std::optional<bool> none{};

  std::optional<float> visibility{};
  std::optional<float> scaleHeight{};
  std::optional<float> baseHeight{};

  /// The water droplet diameter in micrometers, which drives the
  /// approximate Mie phase function; see `HazeOptions::dropletSize`.
  std::optional<float> droplet{};
};

/// The clock a layout's `time` directive sets: where the frame sits in
/// the host's animation, in seconds, and how long the shutter stays
/// open. Everything is optional and merged with the command line the
/// same way `LayoutCamera` works, over the defaults of zero and zero.
///
/// The shutter is open iff `shutter` is positive. Shut, every path
/// renders at `base` exactly, whatever motion the layout carries.
///
class LayoutTime final {
public:
  /// `base`: `State::animation_time` at shutter open, in seconds.
  std::optional<float> base{};

  /// `shutter`: the seconds from open to shut, nonnegative.
  std::optional<float> shutter{};
};

/// One `asset` declaration: a named source with the properties of what
/// is loaded, as written. Nothing in it puts geometry in the world.
class LayoutAssetDecl final {
public:
  std::string name{};
  LayoutLocation nameLoc{};

  /// The source path, as written; the lowering resolves it. Empty when
  /// the asset is a primitive, in which case `pathLoc` marks the
  /// shape keyword instead.
  std::string path{};
  LayoutLocation pathLoc{};

  /// The built-in shape, when the asset is `= sphere|disk|cylinder|cone`
  /// rather than `= "<path>"`.
  PrimitiveSpec primitive{};

  /// The correction transform the block's operations accumulated,
  /// applied innermost, underneath every placement of the asset.
  float4x4 transform{float4x4(1.0f)};

  ObjectSelection selection{};
  SubdivSpec subdiv{};
  MaterialAssignment materials{};

  /// The curves operations the block wrote (`tube`, `ribbon`,
  /// `radius_scale`), with `active` still false: whether the path
  /// actually names a `.curves` file is the lowering's to discover, and
  /// `curvesOpsLoc` is where its complaint points when it does not.
  CurvesSpec curves{};
  LayoutLocation curvesOpsLoc{};

  /// The bare operation `caster`: every placement of this asset is a
  /// caustic caster, a surface the renderer's manifold estimators search
  /// for specular and glossy connections to the lights and claim that
  /// transport from the path tracer. The mark is scene judgment, which
  /// is why it lives here and not on the material: the same chrome is a
  /// caster on a mirror wall and noise on a thousand screws. A
  /// placement overrides it with `caster` or `caster off`, a bulk place
  /// inherits it for every record, and a mark on an asset whose material
  /// has no Dirac or glossy lobe is reported at scene load and ignored.
  /// It applies to mesh files and shapes; a curves groom cannot carry
  /// it, and on a layout target it passes down to the whole subtree
  /// like a placement override.
  bool caster{};
  LayoutLocation casterLoc{};

  /// The bare operation `light`: every placement of this asset is a
  /// light, an emitter that light selection (next-event estimation, and
  /// the MIS and manifold gathers that hang off it) aims at. An emissive
  /// surface without the mark still emits, through the path hits the
  /// walk finds on its own, at MIS weight 1. The mark is scene judgment
  /// like `caster`, and for the same reason: emission is a fact about a
  /// material, and whether a surface is worth aiming at is a fact about
  /// the scene; a lamp is a light on the ceiling and clutter in a pile,
  /// and a material whose emission covers a sliver of its surface is
  /// rarely worth a share of every gather. It composes exactly as
  /// `caster` does: a placement overrides it with `light` or `light
  /// off`, a bulk place inherits it for every record, on a layout target
  /// it passes down to the whole subtree, a curves groom cannot carry
  /// it, and a mark on an asset whose material has no emission is
  /// reported at scene load and ignored. `caustic` implies it.
  bool light{};
  LayoutLocation lightLoc{};

  /// The bare operation `caustic`: every placement of this asset is a
  /// caustic target, an emitter the renderer's manifold reflective
  /// gather searches the marked casters for. Meaningful only on an
  /// emissive asset, and only once any light in the scene carries the
  /// mark: with no marks anywhere every light is a target, and with
  /// any, only the marked ones are, which is how a scene with many
  /// lights restricts the search to the few worth it. Asset-level only,
  /// no placement override: mark the asset, or declare two. A caustic
  /// target is necessarily a light, so the mark implies `light`, and
  /// `light off` on a placement of a caustic asset is an error.
  bool caustic{};
};

/// One `light` declaration: a named emitter with no surface in the
/// scene, placeable exactly like an asset, so `place` gives it its
/// position, orientation, and instancing for free:
///
///     light lamp = point { power 60 temperature 3000 }
///     light beam = spot { power 100 angle 40 blend 0.2 }
///     light street = profile "street.ies" { scale 2 }
///     light panel = rect { size 2 1 power 400 }
///     light ring = disk { radius 0.5 power 100 }
///     place lamp translate 0 0 3
///
/// A point light emits uniformly; a spot and a profile emit along the
/// local **-Z** axis (a spot with an identity placement shines straight
/// down, and an IES profile's photometric nadir points the same way),
/// aimed by the placement's rotations. A rect and a disk are flat,
/// one-sided Lambertian emitters lying in the local XY plane, centered
/// at the origin and emitting into the local -Z half space: the same
/// convention, so an identity placement lights what is below it. The
/// primitive `disk` shape faces +Z; the light does not. The placement's
/// `scale` acts on the extent, so `place panel scale 2 1 1` doubles the
/// width, while `power` stays fixed and the radiance falls as the area
/// grows.
///
/// No light declared here has a surface: the camera cannot see it, a
/// BSDF-sampled continuation cannot hit it, and a shadow ray passes
/// through where it is. It reaches the scene through light selection
/// alone, at MIS weight 1. A lamp that should be seen, or reflected, is
/// a primitive asset with an emissive material and the `light` mark;
/// see `LayoutAssetDecl::light`.
///
/// `power` is the total radiant power in watts. The spectral shape is
/// flat across the render band by default; `temperature` reshapes it to
/// a blackbody, and `color` multiplies by an RGB tint (uplifted at
/// render time), which scales power the way dimming a lamp does. A
/// profile's intensities are already watts per steradian, so it takes
/// `scale` instead, and giving it `power` renormalizes its total.
///
/// `caustic` marks the light as a caustic target; see
/// `LayoutAssetDecl::caustic` for the semantics the mark shares with an
/// emissive asset's.
class LayoutLightDecl final {
public:
  enum class Kind {
    POINT,
    SPOT,
    PROFILE,
    RECT,
    DISK,
  };
  std::string name{};
  LayoutLocation nameLoc{};
  Kind kind{Kind::POINT};

  /// The IES path, as written (PROFILE only); the lowering resolves it.
  std::string profilePath{};
  LayoutLocation profilePathLoc{};

  /// The total radiant power in watts. For PROFILE, applied only when
  /// written (`powerSet`), renormalizing the profile's own total.
  float power{1.0f};
  bool powerSet{};

  /// The blackbody temperature in kelvin shaping the spectrum, or 0 for
  /// a flat spectrum across the render band.
  float temperature{};

  /// The RGB tint, uplifted to a spectrum at render time.
  float3 color{1.0f};

  /// SPOT: the full cone apex angle in degrees.
  float spotAngle{60.0f};

  /// SPOT: the fraction of the cone smoothed from full intensity down
  /// to zero at the edge, in [0, 1].
  float spotBlend{0.15f};

  /// PROFILE: a multiplier on the profile's intensities.
  float scale{1.0f};

  /// RECT: the width and height along the local X and Y axes.
  float2 size{1.0f, 1.0f};

  /// DISK: the radius in the local XY plane.
  float radius{0.5f};

  /// Is a caustic target; see `LayoutAssetDecl::caustic`.
  bool caustic{};

  /// The correction transform the block's operations accumulated,
  /// applied innermost, underneath every placement of the light.
  float4x4 transform{float4x4(1.0f)};

  /// The kind keyword, as the grammar spells it.
  [[nodiscard]] std::string_view kindName() const noexcept {
    switch (kind) {
    case Kind::SPOT:
      return "spot";
    case Kind::PROFILE:
      return "profile";
    case Kind::RECT:
      return "rect";
    case Kind::DISK:
      return "disk";
    default:
      return "point";
    }
  }
};

/// One world-contributing statement, in document order: a `place` of a
/// declared asset or group, or an anonymous `import`.
///
/// A `place` takes three spellings: bare (`place rock`), a block
/// (`place rock { ... }`), or everything on the `place` keyword's own
/// line (`place rock scale 0.5 translate 1 0 0`), which is the form a
/// machine writes one instance per line in.
class LayoutPlacement final {
public:
  enum class Kind {
    PLACE,
    IMPORT,
  };
  Kind kind{Kind::PLACE};

  /// PLACE: the declared asset or group the placement names.
  std::string assetName{};
  LayoutLocation assetNameLoc{};

  /// PLACE: the stable identity `place <name> as <id>` records, or
  /// empty. Nothing consumes it yet: it is the seam a future animation
  /// or override layer refers to.
  std::string asName{};
  LayoutLocation asNameLoc{};

  /// PLACE: the site's `material <from> = <to>` overrides: what this
  /// placement's subtree resolves as `from` resolves as `to` instead,
  /// applied after the target's own assignment and its file's aliases.
  ///
  /// The override joins the item's `MaterialAssignment::renames`, and
  /// `Scene::add()` resolves renames per INSTANCE when they do not feed
  /// displacement, so N shading-only overrides of one asset share one
  /// mesh and one BVH. Overriding a displacing asset still duplicates
  /// its meshes, because the renamed material's displacement bakes into
  /// the vertices at commit.
  std::map<std::string, std::string, std::less<>> overrides{};

  /// PLACE: the `.places` buffer of a bulk placement, as written
  /// (`place <name> * "<file>"`), or empty for an ordinary place. Each
  /// record of the buffer stands where a one-line place's operations
  /// would: composed under this placement's own operations and over the
  /// target's correction transform, one instance per record.
  std::string placesPath{};
  LayoutLocation placesPathLoc{};

  /// PLACE: the `variant { material <from> = <to> ... }` blocks of a
  /// bulk placement, in order of appearance: the override table the
  /// buffer's per-record variant indices pick from. A variant composes
  /// exactly where a per-record place's own overrides would, inside
  /// this placement's `overrides`.
  std::vector<std::map<std::string, std::string, std::less<>>> variants{};

  /// IMPORT: the path, as written; the lowering resolves it.
  std::string importPath{};
  LayoutLocation importPathLoc{};

  /// IMPORT: the site's `material` assignments. On a mesh or asset
  /// import these assign the mesh's slots, exactly as an `asset` block
  /// would; on a layout import they rename what the subtree resolves.
  MaterialAssignment importMaterials{};

  /// The site's `caster` or `caster off`, or unset to take what the
  /// target declares: see `LayoutAssetDecl::caster`. On a place of a
  /// group or a layout import it passes down to every placement inside
  /// that does not say otherwise itself, the innermost explicit word
  /// winning.
  std::optional<bool> casterOverride{};
  LayoutLocation casterLoc{};

  /// The site's `light` or `light off`, or unset to take what the target
  /// declares: see `LayoutAssetDecl::light`. Passes down exactly as
  /// `casterOverride` does.
  std::optional<bool> lightOverride{};
  LayoutLocation lightLoc{};

  /// The transform the block's operations accumulated.
  float4x4 transform{float4x4(1.0f)};

  /// PLACE: the transform at shutter shut, the `motion { ... }` block's
  /// operations accumulated from identity exactly as `transform` is, or
  /// unset for a static placement. Absolute, never a delta: it restates
  /// the placement the way the open key states it, which is what a
  /// machine writes. A bulk place's block moves the whole scatter; the
  /// records carry no keys of their own.
  std::optional<float4x4> motion{};
  LayoutLocation motionLoc{};
};

/// One `group` declaration: a named, reusable arrangement of `place`
/// statements, placeable exactly like an asset. The group's contents
/// compose underneath the placing transform, and groups nest. Nothing
/// but `place` appears inside one: a whole-file placement is spelled as
/// an `asset` plus a `place`, so that a group stays an arrangement and
/// never a scope.
class LayoutGroupDecl final {
public:
  std::string name{};
  LayoutLocation nameLoc{};
  std::vector<LayoutPlacement> placements{};
};

/// A parsed layout file, as written: what the parser hands the
/// lowering.
class LayoutDocument final {
public:
  /// The source the document was parsed from, owned by the
  /// `LayoutDiagnostics` that loaded it.
  const LayoutSource *source{};

  /// The directory of the file, which relative paths resolve against.
  std::string directory{};

  std::vector<LayoutAssetDecl> assets{};
  std::vector<LayoutGroupDecl> groups{};
  std::vector<LayoutLightDecl> lights{};
  std::vector<LayoutPlacement> placements{};

  /// This file's `material` aliases, last-wins within the file.
  std::map<std::string, std::string, std::less<>> materialAliases{};

  std::string mediumName{};
  LayoutLocation mediumLoc{};

  /// The camera, sky, haze, and time, with the location of the first
  /// directive so that lowering a non-entry file can say what it is
  /// ignoring. An invalid location means the directive never appeared.
  LayoutCamera camera{};
  LayoutLocation cameraLoc{};
  LayoutSky sky{};
  LayoutLocation skyLoc{};
  LayoutHaze haze{};
  LayoutLocation hazeLoc{};
  LayoutTime time{};
  LayoutLocation timeLoc{};

  /// The written `ibl` path and where it was written, resolved by the
  /// lowering rather than the parser, so the parser stays free of the
  /// filesystem.
  std::string iblPath{};
  LayoutLocation iblPathLoc{};

  /// The declared asset named `name`, or null. Assets, groups, and
  /// lights share one namespace, since a `place` names any of them.
  [[nodiscard]] const LayoutAssetDecl *findAsset(std::string_view name) const {
    for (const auto &asset : assets)
      if (asset.name == name) return &asset;
    return nullptr;
  }

  /// The declared group named `name`, or null.
  [[nodiscard]] const LayoutGroupDecl *findGroup(std::string_view name) const {
    for (const auto &group : groups)
      if (group.name == name) return &group;
    return nullptr;
  }

  /// The declared light named `name`, or null.
  [[nodiscard]] const LayoutLightDecl *findLight(std::string_view name) const {
    for (const auto &light : lights)
      if (light.name == name) return &light;
    return nullptr;
  }
};

/// One mesh file, or a selected part of one, placed in the world: what
/// the lowering emits and `Scene::add()` consumes.
class LayoutItem final {
public:
  std::string fileName{};    ///< The mesh or curves file, path resolved;
                             ///< empty for a primitive item.
  PrimitiveSpec primitive{}; ///< The shape, when a primitive.
  CurvesSpec curves{};       ///< The fiber spec, when a curves file.
  float4x4 objectToWorld{float4x4(1.0f)}; ///< Where to put it.

  /// Where to put it at shutter shut, present when something on the
  /// place path carried a `motion` block and the composed shut key
  /// differs from `objectToWorld`; absent for a static item, which
  /// includes one whose blocks restate their open keys. Composition is
  /// pairwise: a group's shut key over a member's, the asset's
  /// correction and the file's node transforms under both.
  std::optional<float4x4> objectToWorldShut{};
  ObjectSelection selection{};    ///< Which of its objects to place.
  SubdivSpec subdiv{};            ///< How to refine it at load time.
  MaterialAssignment materials{}; ///< What shades it, fully composed.

  /// Is a caustic caster, with the asset's mark and every override on
  /// the place path already composed; see `LayoutAssetDecl::caster`.
  bool caster{};

  /// Is a light, composed the same way, with `caustic` folded in; see
  /// `LayoutAssetDecl::light`.
  bool light{};

  /// Is a caustic target, from the asset's mark alone; see
  /// `LayoutAssetDecl::caustic`.
  bool causticLight{};

  /// The `/`-joined chain of `as` names along the place path that
  /// produced this item, or empty. The stable identity a future stage
  /// animates or overrides by; nothing consumes it yet.
  std::string placeName{};

  /// When non-empty, this one item stands for a whole batch: one
  /// placement per entry, each a fully composed world transform, and
  /// `objectToWorld` is unused. This is how a `.places` scatter reaches
  /// the scene as a batch rather than as N items, so the scene can
  /// build one Embree instance array per mesh instead of N instance
  /// geometries. The lowering batches the records of a bulk place that
  /// share a variant class (their materials must agree for the batch to
  /// share one binding), so instance order within a scatter is grouped
  /// by variant rather than by record.
  std::vector<float4x4> batchXfs{};

  /// The shut keys of a batch, parallel to `batchXfs`, or empty when
  /// the scatter is static. A bulk place's `motion` moves the scatter
  /// as a whole, so every record's shut key is its record transform
  /// under the moved placement.
  std::vector<float4x4> batchXfsShut{};
};

/// One punctual light placed in the world: what the lowering emits and
/// the renderer's `LightSampler` consumes. The declaration travels by
/// value with the placement transform composed over its correction, and
/// `decl.profilePath` already resolved.
class LayoutLight final {
public:
  LayoutLightDecl decl{};

  /// The fully composed placement, positioning the light at its origin
  /// and aiming its local -Z axis.
  float4x4 lightToWorld{float4x4(1.0f)};

  /// The placement at shutter shut, present when the light moves; see
  /// `LayoutItem::objectToWorldShut` for when that is.
  std::optional<float4x4> lightToWorldShut{};

  /// The `/`-joined chain of `as` names along the place path, or empty.
  std::string placeName{};
};

/// A lowered layout: the flat item list, plus the entry file's camera,
/// sky, haze, time, medium, and aliases. Everything scoped is already folded
/// into the items; what remains here is exactly what `main()` consumes.
class Layout final {
public:
  std::vector<LayoutItem> items{};

  /// The punctual lights, from every file the lowering visited.
  std::vector<LayoutLight> lights{};

  /// The entry file's `material` aliases. The items already carry them
  /// folded into their `MaterialAssignment::renames`; this copy exists
  /// only so that command-line-facing names (the `-ground-material`
  /// name) can resolve through them too.
  std::map<std::string, std::string, std::less<>> entryMaterialAliases{};

  /// The MDL material whose volume is the exterior medium, or empty for
  /// vacuum. See the `medium` directive.
  std::string exteriorMediumName{};

  /// Whatever the entry file's `camera` directives named, merged.
  LayoutCamera camera{};

  /// Whatever the entry file's `sky` directives named, merged.
  LayoutSky sky{};

  /// Whatever the entry file's `haze` directives named, merged, and
  /// whether one appeared at all, which is what turns the haze on.
  LayoutHaze haze{};
  bool hasHaze{};

  /// Whatever the entry file's `time` directives named, merged.
  LayoutTime time{};

  /// The `front:` azimuth of the asset manifest the command line named
  /// directly, or unset. Only `resolveLayoutArgument()` fills this in:
  /// it belongs to rendering one bare asset, where `-autolook` locks to
  /// it, rather than to a layout of many.
  std::optional<float> frontAzimuth{};
};

/// Parse one layout source into a document.
///
/// Pure: no filesystem access, no path resolution, no imports followed.
/// Errors and warnings accumulate in `diags`, and the returned document
/// is the best effort regardless, so one bad statement does not hide
/// the diagnostics of the statements after it.
[[nodiscard]] LayoutDocument parseLayout(LayoutDiagnostics &diags,
                                         const LayoutSource &source,
                                         std::string directory);

/// Parse `fileName` and recursively lower it, imports and all.
///
/// Accumulates into `diags` and returns the best effort; the caller
/// decides when errors are fatal. This is the seam the tests drive.
[[nodiscard]] Layout lowerLayout(LayoutDiagnostics &diags,
                                 const std::string &fileName,
                                 const AssetSearchPath &search = {});

/// Read a layout file: parse, lower, print every diagnostic to standard
/// error (colored when stderr is a terminal), and throw if any were
/// errors.
///
/// \throws smdl::Error  On any error, after printing the diagnostics.
///
[[nodiscard]] Layout readLayout(const std::string &fileName,
                                const AssetSearchPath &search = {});

/// Resolve one command-line scene argument into the items it stands
/// for: a layout file (by extension or by its `#smdl layout` first
/// line) yields everything it names, an asset resolves through its
/// manifest, and anything else is a mesh file placed at the origin. A
/// `.scene` file is an error naming the retirement.
[[nodiscard]] Layout resolveLayoutArgument(const std::string &fileName,
                                           const AssetSearchPath &search = {});
