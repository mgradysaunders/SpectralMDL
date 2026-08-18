"""Scattering a prepared asset over a surface with geometry nodes.

Nothing about scattering is special to this workflow: Blender's own
instancing is what does it, and the exporter already sees geometry-nodes
instances the same way it sees any other, through
`depsgraph.object_instances`. Twenty scattered rocks export as one `asset`
declaration and twenty `place` operations, which is one mesh and one BVH in
the renderer.

Blender ships the scatter itself, as the essentials `Scatter on Surface`
node group, and it instances through Object Info with `As Instance` and
through Collection Info, which is exactly what the exporter can name. So
this operator only adds that modifier and points it at the asset. Density,
seed, Poisson spacing, alignment and randomization are all the modifier's
own interface afterward.

What Blender does not ship is a slope, which every terrain scatter wants:
grass on the flats, bare rock on the cliffs. So the other operator here
writes the angle between the surface normal and world up into a face
attribute, which the scatter reads through its `Distribution Mask`. That
mask multiplies density rather than thresholding it, so the band feathers
instead of ending on a line.
"""

import math
import os

import bpy

from .exporter import (ASSET_KEY, SELECT_KEY, SOURCES_COLLECTION,
                       SURFACE_GROUP, asset_tag, is_asset_collection,
                       seed_slots)

# The essentials node group this adds, and the file it is appended from.
# Appending keeps one local copy that every scatter in the file shares,
# which also works in a background Blender, where the asset-browser route
# (`object.modifier_add_node_group`) reports "Asset loading is unfinished".
SCATTER_GROUP = "Scatter on Surface"
SCATTER_LIBRARY = ("assets", "nodes", "geometry_nodes_essentials.blend")

# `Instance Type` is a menu socket, and the Python API stores those as
# opaque ints with no name to look one up by. Verified against Blender
# 5.1.1: 1 is Object, the default, and 0 is Collection. Nothing pins this
# but the essentials file itself, so the operator checks that the scatter
# resolved to the collection rather than trusting the constant.
INSTANCE_TYPE_COLLECTION = 0

# The slope mask this builds, and the attribute it writes by default.
SLOPE_GROUP = "SMDL Slope Mask"
SLOPE_ATTRIBUTE = "smdl_slope"

# The clump profile a clustered scatter distributes within. Binding the
# second scatter to this attribute is also what keeps it on the patches:
# the surface underneath carries no such attribute, so it reads zero there.
PATCH_ATTRIBUTE = "smdl_patch"

# Standing clustered instances back up, which the scatter's own alignment
# cannot do: see `upright_node_group()`. The mark is what keeps the
# uprighting off instances that were already there, since it is written
# before the scatter runs and only what the scatter adds goes without it.
UPRIGHT_GROUP = "SMDL Upright Instances"
PIN_GROUP = "SMDL Pin Rotations"
PIN_ATTRIBUTE = "smdl_pinned"

# A band of zero width is a hard edge, which the smoothstep cannot express
# without dividing by zero, so the blend never goes below this.
SLOPE_BLEND_MIN = 1e-4


def slope_mask_node_group():
    """Build (or reuse) the group that writes the slope band attribute.

    The band is the angle between the surface normal and world +Z,
    smoothstepped over `Blend` at each end, stored on faces: a face normal
    is the ground's actual slope, where a vertex normal is an average that
    interpolates across the triangle and admits instances past the angle
    that was asked for.
    """
    existing = bpy.data.node_groups.get(SLOPE_GROUP)
    if existing is not None:
        return existing
    tree = bpy.data.node_groups.new(SLOPE_GROUP, "GeometryNodeTree")
    tree.interface.new_socket("Geometry", in_out="INPUT",
                              socket_type="NodeSocketGeometry")
    tree.interface.new_socket("Geometry", in_out="OUTPUT",
                              socket_type="NodeSocketGeometry")
    for name, default in (("Min Slope", 0.0),
                          ("Max Slope", math.radians(30.0)),
                          ("Blend", math.radians(5.0))):
        socket = tree.interface.new_socket(name, in_out="INPUT",
                                           socket_type="NodeSocketFloat")
        socket.subtype = "ANGLE"
        socket.default_value = default
        socket.min_value = 0.0
        socket.max_value = math.pi
    name_socket = tree.interface.new_socket("Name", in_out="INPUT",
                                            socket_type="NodeSocketString")
    name_socket.default_value = SLOPE_ATTRIBUTE

    nodes = tree.nodes
    group_in = nodes.new("NodeGroupInput")
    group_out = nodes.new("NodeGroupOutput")
    normal = nodes.new("GeometryNodeInputNormal")
    split = nodes.new("ShaderNodeSeparateXYZ")
    angle = nodes.new("ShaderNodeMath")
    angle.operation = "ARCCOSINE"
    blend = nodes.new("ShaderNodeMath")
    blend.operation = "MAXIMUM"
    blend.inputs[1].default_value = SLOPE_BLEND_MIN
    start = nodes.new("ShaderNodeMath")
    start.operation = "SUBTRACT"
    end = nodes.new("ShaderNodeMath")
    end.operation = "ADD"
    rising = nodes.new("ShaderNodeMapRange")
    falling = nodes.new("ShaderNodeMapRange")
    for ramp in (rising, falling):
        ramp.interpolation_type = "SMOOTHSTEP"
        ramp.clamp = True
    rising.inputs["To Min"].default_value = 0.0
    rising.inputs["To Max"].default_value = 1.0
    falling.inputs["To Min"].default_value = 1.0
    falling.inputs["To Max"].default_value = 0.0
    band = nodes.new("ShaderNodeMath")
    band.operation = "MULTIPLY"
    store = nodes.new("GeometryNodeStoreNamedAttribute")
    store.domain = "FACE"
    store.data_type = "FLOAT"

    group_in.location = (-800, 0)
    normal.location = (-800, -260)
    split.location = (-620, -260)
    angle.location = (-460, -260)
    blend.location = (-620, 200)
    start.location = (-460, 260)
    end.location = (-460, 100)
    rising.location = (-280, 200)
    falling.location = (-280, -60)
    band.location = (-80, 60)
    store.location = (100, 0)
    group_out.location = (300, 0)

    link = tree.links.new
    link(normal.outputs["Normal"], split.inputs[0])
    link(split.outputs["Z"], angle.inputs[0])
    link(group_in.outputs["Blend"], blend.inputs[0])
    link(group_in.outputs["Min Slope"], start.inputs[0])
    link(blend.outputs[0], start.inputs[1])
    link(group_in.outputs["Max Slope"], end.inputs[0])
    link(blend.outputs[0], end.inputs[1])
    link(angle.outputs[0], rising.inputs["Value"])
    link(start.outputs[0], rising.inputs["From Min"])
    link(group_in.outputs["Min Slope"], rising.inputs["From Max"])
    link(angle.outputs[0], falling.inputs["Value"])
    link(group_in.outputs["Max Slope"], falling.inputs["From Min"])
    link(end.outputs[0], falling.inputs["From Max"])
    link(rising.outputs["Result"], band.inputs[0])
    link(falling.outputs["Result"], band.inputs[1])
    link(group_in.outputs[0], store.inputs["Geometry"])
    link(group_in.outputs["Name"], store.inputs["Name"])
    link(band.outputs[0], store.inputs["Value"])
    link(store.outputs["Geometry"], group_out.inputs[0])
    return tree


def asset_surface_node_group():
    """Build (or reuse) the group that gives an imported asset geometry of
    its own.

    An asset arrives as an empty instancing a proxy collection, so there is
    nothing for a modifier to distribute over, which is why a terrain or a
    cliff cannot be scattered on as it stands. This pulls the proxy in and
    realizes it: a slope mask then has faces to write to and a scatter has
    a surface to sample. What the layout writes is unchanged, since the
    object still carries the asset's tag and the renderer still loads the
    asset by name; the proxy stands in here exactly as it does in the
    viewport.
    """
    existing = bpy.data.node_groups.get(SURFACE_GROUP)
    if existing is not None:
        return existing
    tree = bpy.data.node_groups.new(SURFACE_GROUP, "GeometryNodeTree")
    tree.interface.new_socket("Geometry", in_out="INPUT",
                              socket_type="NodeSocketGeometry")
    tree.interface.new_socket("Geometry", in_out="OUTPUT",
                              socket_type="NodeSocketGeometry")
    tree.interface.new_socket("Collection", in_out="INPUT",
                              socket_type="NodeSocketCollection")
    group_in = tree.nodes.new("NodeGroupInput")
    group_out = tree.nodes.new("NodeGroupOutput")
    info = tree.nodes.new("GeometryNodeCollectionInfo")
    # Original, not relative: relative rebases the proxy into the object's
    # space, which lands it where the collection was authored rather than
    # where the asset was placed. Original leaves the object's own
    # transform to do the placing, exactly as the instancing empty did.
    info.transform_space = "ORIGINAL"
    info.inputs["Separate Children"].default_value = True
    realize = tree.nodes.new("GeometryNodeRealizeInstances")
    group_in.location = (-400, 0)
    info.location = (-200, 0)
    realize.location = (0, 0)
    group_out.location = (200, 0)
    link = tree.links.new
    link(group_in.outputs["Collection"], info.inputs["Collection"])
    link(info.outputs["Instances"], realize.inputs[0])
    link(realize.outputs[0], group_out.inputs[0])
    return tree


def is_asset_instance(ob):
    """Is this object an imported asset, rather than a mesh modeled here?"""
    return (ob is not None and ob.instance_type == "COLLECTION"
            and ob.instance_collection is not None
            and (ASSET_KEY in ob.keys()
                 or is_asset_collection(ob.instance_collection)))


def as_surface(context, ob):
    """The mesh whose modifier stack a scatter goes on, and what to say
    about it.

    Ground, cliffs and terrain arrive as assets as often as they are
    modeled in Blender, so an asset instance is turned into an object that
    carries the same tag, the same transform and the same export options,
    and grows the proxy geometry through `asset_surface_node_group()`. The
    layout is unmoved by the swap: a tagged object is placed exactly like
    the instance it replaces.
    """
    if ob.type == "MESH":
        return ob, ""
    if not is_asset_instance(ob):
        return None, (f"{ob.name} is neither a mesh nor an imported asset, "
                      f"so there is no surface to scatter over")
    tag = asset_tag(ob)
    if tag is None:
        return None, (f"{ob.name} carries no {ASSET_KEY} tag to place it by, "
                      f"so a surface built from it would bake its proxy "
                      f"instead of referencing the asset")
    name = ob.name
    collection = ob.instance_collection
    matrix = ob.matrix_world.copy()
    # Where the asset was filed, so that replacing it does not move it to
    # the top of the outliner.
    homes = list(ob.users_collection)
    properties = {key: ob[key] for key in ob.keys()}
    options = getattr(ob, "smdl_asset_options", None)
    saved = {} if options is None else {
        field: getattr(options, field) for field in
        ("subdivide", "scheme", "linear", "displace", "material", "variants")}
    saved_slots = [] if options is None else [
        (row.name, row.material, row.variants, row.stale)
        for row in options.slots]
    # Out of the way first, so the surface can take the name that names it.
    bpy.data.objects.remove(ob)

    surface = bpy.data.objects.new(name, bpy.data.meshes.new(name))
    for key, value in properties.items():
        surface[key] = value
    # The tag is what makes this a placement rather than baked geometry,
    # and an asset dragged from the Asset Browser keeps it on the
    # collection, so the instance may have had nothing to copy.
    surface[ASSET_KEY], surface[SELECT_KEY] = tag
    if saved:
        for field, value in saved.items():
            setattr(surface.smdl_asset_options, field, value)
    for slot, material_name, variants, stale in saved_slots:
        row = surface.smdl_asset_options.slots.add()
        row.name, row.material, row.variants, row.stale = (
            slot, material_name, variants, stale)
    for home in homes or [context.scene.collection]:
        home.objects.link(surface)
    surface.matrix_world = matrix
    modifier = surface.modifiers.new("Surface From Asset", "NODES")
    modifier.node_group = asset_surface_node_group()
    set_inputs(modifier, {"Collection": collection})
    surface.select_set(True)
    context.view_layer.objects.active = surface
    return surface, f"{name} is a scatter surface now"


def pin_node_group():
    """Build (or reuse) the group that marks the instances already present,
    so that a later uprighting leaves them alone."""
    existing = bpy.data.node_groups.get(PIN_GROUP)
    if existing is not None:
        return existing
    tree = bpy.data.node_groups.new(PIN_GROUP, "GeometryNodeTree")
    tree.interface.new_socket("Geometry", in_out="INPUT",
                              socket_type="NodeSocketGeometry")
    tree.interface.new_socket("Geometry", in_out="OUTPUT",
                              socket_type="NodeSocketGeometry")
    group_in = tree.nodes.new("NodeGroupInput")
    group_out = tree.nodes.new("NodeGroupOutput")
    store = tree.nodes.new("GeometryNodeStoreNamedAttribute")
    store.domain = "INSTANCE"
    store.data_type = "BOOLEAN"
    store.inputs["Name"].default_value = PIN_ATTRIBUTE
    store.inputs["Value"].default_value = True
    group_in.location = (-300, 0)
    store.location = (-100, 0)
    group_out.location = (120, 0)
    tree.links.new(group_in.outputs[0], store.inputs["Geometry"])
    tree.links.new(store.outputs["Geometry"], group_out.inputs[0])
    return tree


def upright_node_group():
    """Build (or reuse) the group that stands instances back up.

    A clustered scatter works inside its patch, and the patch lies on the
    ground, so turning the scatter's own alignment off only makes an
    instance upright with respect to a tilted clump. Undoing each
    instance's rotation in world space is what actually makes a trunk
    vertical on a hillside, and it leaves the position alone.

    Only the unmarked are stood up, so a scatter that asked to lean with
    the ground goes on leaning: see `pin_node_group()`.
    """
    existing = bpy.data.node_groups.get(UPRIGHT_GROUP)
    if existing is not None:
        return existing
    tree = bpy.data.node_groups.new(UPRIGHT_GROUP, "GeometryNodeTree")
    tree.interface.new_socket("Geometry", in_out="INPUT",
                              socket_type="NodeSocketGeometry")
    tree.interface.new_socket("Geometry", in_out="OUTPUT",
                              socket_type="NodeSocketGeometry")
    group_in = tree.nodes.new("NodeGroupInput")
    group_out = tree.nodes.new("NodeGroupOutput")
    rotation = tree.nodes.new("GeometryNodeInputInstanceRotation")
    invert = tree.nodes.new("FunctionNodeInvertRotation")
    rotate = tree.nodes.new("GeometryNodeRotateInstances")
    rotate.inputs["Local Space"].default_value = False
    pinned = tree.nodes.new("GeometryNodeInputNamedAttribute")
    pinned.data_type = "BOOLEAN"
    pinned.inputs["Name"].default_value = PIN_ATTRIBUTE
    loose = tree.nodes.new("FunctionNodeBooleanMath")
    loose.operation = "NOT"
    group_in.location = (-400, 0)
    pinned.location = (-400, 180)
    loose.location = (-220, 180)
    rotation.location = (-400, -200)
    invert.location = (-220, -200)
    rotate.location = (-40, 0)
    group_out.location = (160, 0)
    link = tree.links.new
    link(group_in.outputs[0], rotate.inputs["Instances"])
    link(pinned.outputs["Attribute"], loose.inputs[0])
    link(loose.outputs[0], rotate.inputs["Selection"])
    link(rotation.outputs["Rotation"], invert.inputs["Rotation"])
    link(invert.outputs["Rotation"], rotate.inputs["Rotation"])
    link(rotate.outputs[0], group_out.inputs[0])
    return tree


def patch_profile(radius, spread):
    """A clump's density profile: a Gaussian of the distance from its
    centre, shifted so that the rim is exactly zero and the clump ends
    where the patch does."""
    sigma = max(spread, 1e-3) * radius
    rim = math.exp(-0.5 * (radius / sigma) ** 2)
    def value(distance):
        gaussian = math.exp(-0.5 * (distance / sigma) ** 2)
        return max(0.0, (gaussian - rim) / (1.0 - rim))
    return value


def patch_mean(mesh):
    """The mask's area-weighted average over the patch, which is what turns
    a wanted count per clump into a density.

    Measured on the mesh rather than integrated from the profile, because
    what a scatter samples is the mask interpolated across each face, and
    that averages a few percent away from the curve it was built from.
    """
    values = [0.0] * len(mesh.vertices)
    mesh.attributes[PATCH_ATTRIBUTE].data.foreach_get("value", values)
    total = 0.0
    area = 0.0
    for polygon in mesh.polygons:
        corners = [values[index] for index in polygon.vertices]
        total += sum(corners) / len(corners) * polygon.area
        area += polygon.area
    return total / area if area else 1.0


def cluster_patch_mesh(name, radius, spread, rings=6, segments=24):
    """The disk a clustered scatter distributes within, carrying its
    profile as a vertex attribute.

    Concentric rings rather than one fan, because the mask is interpolated
    across each face from its corners: a single ring would flatten the
    Gaussian into a cone.
    """
    profile = patch_profile(radius, spread)
    vertices = [(0.0, 0.0, 0.0)]
    values = [profile(0.0)]
    for ring in range(1, rings + 1):
        distance = radius * ring / rings
        for segment in range(segments):
            angle = 2.0 * math.pi * segment / segments
            vertices.append((distance * math.cos(angle),
                             distance * math.sin(angle), 0.0))
            values.append(profile(distance))
    faces = []
    for segment in range(segments):
        faces.append((0, 1 + segment, 1 + (segment + 1) % segments))
    for ring in range(rings - 1):
        inner = 1 + ring * segments
        outer = inner + segments
        for segment in range(segments):
            following = (segment + 1) % segments
            faces.append((inner + segment, outer + segment,
                          outer + following, inner + following))
    mesh = bpy.data.meshes.new(name)
    mesh.from_pydata(vertices, [], faces)
    mesh.update()
    attribute = mesh.attributes.new(PATCH_ATTRIBUTE, "FLOAT", "POINT")
    attribute.data.foreach_set("value", values)
    return mesh


# The per-scatter material-names input an earlier version added to the
# scatter group. Retired: the retained source's Material Variants is the
# one channel for randomized materials, so a file that gained the socket
# has it removed the next time a scatter is added, rather than keeping a
# dead control that nothing reads.
LEGACY_MATERIALS_SOCKET = "SMDL Materials"


def remove_legacy_materials_socket(tree):
    """Drop the retired per-scatter names input from the group."""
    if tree is not None:
        for item in list(tree.interface.items_tree):
            if (item.item_type == "SOCKET" and item.in_out == "INPUT"
                    and item.name == LEGACY_MATERIALS_SOCKET):
                tree.interface.remove(item)
    return tree


def scatter_node_group():
    """The essentials `Scatter on Surface` group, appended once and shared,
    or None if this Blender does not ship it."""
    existing = bpy.data.node_groups.get(SCATTER_GROUP)
    if existing is not None:
        return remove_legacy_materials_socket(existing)
    path = os.path.join(bpy.utils.system_resource("DATAFILES"),
                        *SCATTER_LIBRARY)
    if not os.path.exists(path):
        return None
    with bpy.data.libraries.load(path, link=False) as (source, target):
        if SCATTER_GROUP not in source.node_groups:
            return None
        target.node_groups = [SCATTER_GROUP]
    return bpy.data.node_groups.get(SCATTER_GROUP)


def input_identifiers(tree):
    """A group's input sockets as (identifier, name) pairs.

    Sockets are matched by name rather than by identifier, so a user's own
    copy or edit of a built-in group is configured the same way. Names are
    not unique in every group, so this stays a list.
    """
    return [(item.identifier, item.name)
            for item in tree.interface.items_tree
            if item.item_type == "SOCKET" and item.in_out == "INPUT"]


def set_inputs(modifier, values):
    """Set a geometry-nodes modifier's inputs by socket name, ignoring any
    the group does not have.

    A group may spell one setting as two sockets of different types, as the
    essentials scatter does with its uniform and per-axis `Randomize
    Scale`; the one that cannot hold the value refuses it, which is the
    right answer for whichever of the pair is not in use.
    """
    for identifier, name in input_identifiers(modifier.node_group):
        if name in values:
            try:
                modifier[identifier] = values[name]
            except TypeError:
                continue


def get_input(modifier, socket, default=None):
    """One of a modifier's input values, by socket name."""
    for identifier, name in input_identifiers(modifier.node_group):
        if name == socket:
            try:
                return modifier[identifier]
            except KeyError:
                return default
    return default


def bind_attribute(modifier, socket, attribute):
    """Read one field input from a named attribute, which is what the
    spreadsheet toggle beside the input does in the modifier. False if the
    group has no such input."""
    for identifier, name in input_identifiers(modifier.node_group):
        if name == socket:
            modifier[identifier + "_use_attribute"] = True
            modifier[identifier + "_attribute_name"] = attribute
            return True
    return False


def bound_attribute(modifier, socket):
    """What a field input reads, or empty if it reads its own value."""
    for identifier, name in input_identifiers(modifier.node_group):
        if name == socket and modifier.get(identifier + "_use_attribute"):
            return str(modifier.get(identifier + "_attribute_name", ""))
    return ""


def is_scatter(modifier):
    """Is this modifier one of the built-in scatters, whatever the file
    renamed it to? The infrastructure this add-on adds around one, the
    surface and the mask and the uprighting, is not."""
    return (modifier.type == "NODES" and modifier.node_group is not None
            and modifier.node_group.name.split(".")[0] == SCATTER_GROUP)


def is_slope_mask(modifier):
    """Is this modifier one of ours, whatever the file renamed it to?"""
    return (modifier.type == "NODES" and modifier.node_group is not None
            and modifier.node_group.name.split(".")[0] == SLOPE_GROUP)


def slope_mask_attribute_of(modifier):
    """The attribute one slope mask writes."""
    for identifier, name in input_identifiers(modifier.node_group):
        if name == "Name":
            return str(modifier.get(identifier, "")) or SLOPE_ATTRIBUTE
    return ""


def slope_mask_attribute(ob):
    """The attribute an object's slope mask writes, or empty.

    The last mask in the stack wins, since that is the one a scatter added
    below it reads.
    """
    found = ""
    for modifier in ob.modifiers:
        if is_slope_mask(modifier):
            found = slope_mask_attribute_of(modifier) or found
    return found


def sources_collection(context):
    """The collection retained scatter sources are filed in, created and
    linked under the scene collection on demand."""
    root = context.scene.collection
    collection = bpy.data.collections.get(SOURCES_COLLECTION)
    if collection is None:
        collection = bpy.data.collections.new(SOURCES_COLLECTION)
    if collection not in root.children_recursive:
        root.children.link(collection)
    return collection


def retire_source(context, asset):
    """File a scattered asset's empty as a hidden source instead of
    deleting it.

    Render hidden keeps it and everything it emits out of the export by
    the scaffolding rule, so retaining it costs the layout nothing, and it
    keeps the one local object the asset's export options can ride on: the
    proxies are linked and read-only, and the surface scatters other
    assets too (see `exporter.scatter_source_options()`). The outliner
    still offers it: the material override can be set after the fact, and
    unhiding it for a moment lets it be selected to scatter the same asset
    again (a hidden object refuses selection, so the eye toggle comes
    first), after which retiring hides it back. One source per proxy
    collection: a second empty pointing at the same collection is deleted,
    so two panels never fight over one asset. Returns what to say about it.
    """
    sources = sources_collection(context)
    for other in sources.objects:
        if (other is not asset
                and other.instance_collection is asset.instance_collection):
            bpy.data.objects.remove(asset)
            return f"source options ride on {other.name}"
    asset.select_set(False)
    for home in list(asset.users_collection):
        if home is not sources:
            home.objects.unlink(asset)
    if asset.name not in sources.objects:
        sources.objects.link(asset)
    # An Asset-Browser drag never passes through Import Asset, so this is
    # where its per-slot rows are seeded.
    seed_slots(asset)
    asset.hide_render = True
    context.view_layer.update()
    try:
        asset.hide_set(True)
    except RuntimeError:
        # The sources collection can be excluded from the view layer,
        # which already hides more thoroughly than the eye toggle.
        pass
    return f"source filed in {sources.name}"


def selected_assets(context, surface):
    """Every imported asset the user pointed at, and why there are none.

    Every way of failing this looks the same from the button, so each one
    says what it found rather than repeating the recipe.
    """
    others = [ob for ob in context.selected_objects if ob is not surface]
    if not others:
        return [], (f"select one or more imported assets as well as "
                    f"{surface.name}, which is the surface because it is "
                    f"active")
    instancers = [ob for ob in others if ob.instance_collection is not None]
    if not instancers:
        return [], (f"{others[0].name} instances no collection, so it is "
                    f"not an imported asset: bring one in with Import "
                    f"Asset or from the Asset Browser")
    assets = [ob for ob in instancers
              if (ASSET_KEY in ob.keys()
                  or is_asset_collection(ob.instance_collection))]
    if not assets:
        return [], (f"nothing in {instancers[0].instance_collection.name} "
                    f"carries an {ASSET_KEY} tag, so it is not a prepared "
                    f"asset: build one with etc/prepare_asset.py")
    return assets, ""


def scatter_set(assets):
    """The collection a scatter of these assets instances.

    With one asset its proxy collection is the set, as it always was. With
    several, one new collection takes their proxy collections as child
    collections: Collection Info separates direct children, so Pick
    Instance chooses one child per point, that is one asset, whole
    (verified against Blender 5.1.1, member objects co-located per pick).
    An existing set with exactly these children is reused, so scattering
    the same selection twice does not accumulate collections. The set is
    deliberately not linked into the scene tree, or its contents would
    draw at the origin; the modifier's Collection field reaches it, and
    the retained sources are the outliner handle.
    """
    collections = []
    for asset in assets:
        if asset.instance_collection not in collections:
            collections.append(asset.instance_collection)
    if len(collections) == 1:
        return collections[0]
    wanted = set(collections)
    for existing in bpy.data.collections:
        if (existing.library is None and not existing.objects
                and set(existing.children) == wanted):
            return existing
    group = bpy.data.collections.new(
        f"Scatter Set ({collections[0].name} +{len(collections) - 1})")
    for collection in collections:
        group.children.link(collection)
    return group


def retire_sources(context, assets):
    """File every pointed-at empty, and one line about it."""
    notes = [retire_source(context, asset) for asset in assets]
    if len(notes) == 1:
        return notes[0]
    return f"{len(notes)} sources filed in {SOURCES_COLLECTION}"


def instance_counts(context, surface, collection):
    """How many instances the surface emits, and how many of them are the
    collection's objects.

    What the second count decides is whether `Instance Type` took, which
    the opaque menu int cannot say on its own.
    """
    members = set(collection.all_objects)
    emitted = 0
    matched = 0
    for instance in context.evaluated_depsgraph_get().object_instances:
        if not instance.is_instance or instance.parent is None:
            continue
        if instance.parent.original is not surface:
            continue
        emitted += 1
        if instance.object.original in members:
            matched += 1
    return emitted, matched


class SMDL_OT_add_slope_mask(bpy.types.Operator):
    """Write the active mesh's slope to an attribute, so that a scatter can
    keep its instances off the cliffs"""

    bl_idname = "smdl.add_slope_mask"
    bl_label = "Add Slope Mask"
    bl_options = {"REGISTER", "UNDO"}

    min_slope: bpy.props.FloatProperty(
        name="Min Slope", subtype="ANGLE",
        description="Leave ground flatter than this angle from horizontal "
                    "bare, which is what scattering onto cliffs alone asks "
                    "for",
        default=0.0, min=0.0, max=math.pi)
    max_slope: bpy.props.FloatProperty(
        name="Max Slope", subtype="ANGLE",
        description="Leave ground steeper than this angle from horizontal "
                    "bare",
        default=math.radians(30.0), min=0.0, max=math.pi)
    blend: bpy.props.FloatProperty(
        name="Blend", subtype="ANGLE",
        description="Fade the band out over this angle beyond each limit. "
                    "The mask multiplies density, so a fade thins the "
                    "scatter out rather than ending it on a line, at the "
                    "cost of reaching past the angles asked for. Zero keeps "
                    "them exact",
        default=math.radians(5.0), min=0.0, max=math.pi)
    attribute: bpy.props.StringProperty(
        name="Attribute",
        description="What to call the mask, so that one terrain can carry "
                    "several bands and each scatter read its own",
        default=SLOPE_ATTRIBUTE)

    @classmethod
    def poll(cls, context):
        ob = context.active_object
        # An imported asset has no geometry of its own, but `as_surface()`
        # gives it some, so a terrain that arrived as an asset is as good a
        # surface as one modeled here.
        return ob is not None and (ob.type == "MESH" or is_asset_instance(ob))

    def invoke(self, context, event):
        # These buttons live in a Properties tab, where Blender draws no
        # "Adjust Last Operation" panel, so the settings are asked for
        # before the operator runs rather than tuned after it.
        existing = next((modifier
                         for modifier in context.active_object.modifiers
                         if is_slope_mask(modifier)), None)
        if existing is not None:
            # Editing the band that is already there beats replacing it
            # with the defaults.
            self.min_slope = get_input(existing, "Min Slope", self.min_slope)
            self.max_slope = get_input(existing, "Max Slope", self.max_slope)
            self.blend = get_input(existing, "Blend", self.blend)
            self.attribute = slope_mask_attribute_of(existing)
        return context.window_manager.invoke_props_dialog(self)

    def execute(self, context):
        surface, note = as_surface(context, context.active_object)
        if surface is None:
            self.report({"ERROR"}, note)
            return {"CANCELLED"}
        name = self.attribute.strip() or SLOPE_ATTRIBUTE
        modifier = next((m for m in surface.modifiers if is_slope_mask(m)
                         and slope_mask_attribute_of(m) == name), None)
        placed = modifier is not None
        if modifier is None:
            modifier = surface.modifiers.new(f"Slope Mask ({name})", "NODES")
            modifier.node_group = slope_mask_node_group()
        set_inputs(modifier, {
            "Min Slope": self.min_slope,
            "Max Slope": self.max_slope,
            "Blend": self.blend,
            "Name": name,
        })
        if not placed:
            # Above the scatters, which is the whole point, but below
            # anything that shapes the surface: the slope of a terrain is
            # the slope of the terrain as displaced, not as modeled.
            scatters = [index for index, other in enumerate(surface.modifiers)
                        if is_scatter(other)]
            if scatters:
                bpy.ops.object.modifier_move_to_index(modifier=modifier.name,
                                                      index=scatters[0])

        # A scatter that already reads a mask is reading it deliberately;
        # one that reads none was scattered before this existed, and adding
        # a mask it does not see is the sort of silent nothing this add-on
        # is otherwise at pains to warn about.
        stack = list(surface.modifiers)
        adopted = 0
        for other in stack[stack.index(modifier) + 1:]:
            if not is_scatter(other):
                continue
            if bound_attribute(other, "Distribution Mask"):
                continue
            if bind_attribute(other, "Distribution Mask", name):
                adopted += 1

        surface.update_tag()
        self.report({"INFO"},
                    f"{name} covers "
                    f"{math.degrees(self.min_slope):.0f} to "
                    f"{math.degrees(self.max_slope):.0f} degrees of "
                    f"{surface.name}" +
                    (f", and {adopted} scatter(s) below it now read it"
                     if adopted else "") +
                    (f" ({note})" if note else ""))
        return {"FINISHED"}


class SMDL_OT_scatter_asset(bpy.types.Operator):
    """Scatter the selected assets over the active mesh with geometry
    nodes; with several selected, each instance picks one at random"""

    bl_idname = "smdl.scatter_asset"
    bl_label = "Scatter Assets On Surface"
    bl_options = {"REGISTER", "UNDO"}

    density: bpy.props.FloatProperty(
        name="Density", description="Instances per square unit of surface",
        default=0.5, min=0.0001, soft_max=50.0)
    align: bpy.props.BoolProperty(
        name="Align To Surface",
        description="Lean instances with the surface normal, which ground "
                    "cover wants and an upright trunk does not",
        default=True)
    mask: bpy.props.StringProperty(
        name="Slope Mask",
        description="Scatter by this attribute, which Add Slope Mask "
                    "writes. Empty reads whatever mask the surface already "
                    "carries",
        default="")

    @classmethod
    def poll(cls, context):
        ob = context.active_object
        # An imported asset has no geometry of its own, but `as_surface()`
        # gives it some, so a terrain that arrived as an asset is as good a
        # surface as one modeled here.
        return ob is not None and (ob.type == "MESH" or is_asset_instance(ob))

    def invoke(self, context, event):
        # Asked for up front, since a Properties tab has no "Adjust Last
        # Operation" panel to tune them in afterward.
        return context.window_manager.invoke_props_dialog(self)

    def execute(self, context):
        # The surface first: converting an asset instance replaces it, and
        # what is selected has to be read after that.
        surface, note = as_surface(context, context.active_object)
        if surface is None:
            self.report({"ERROR"}, note)
            return {"CANCELLED"}
        assets, problem = selected_assets(context, surface)
        if not assets:
            self.report({"ERROR"}, problem)
            return {"CANCELLED"}
        tree = scatter_node_group()
        if tree is None:
            self.report({"ERROR"}, f"this Blender ships no {SCATTER_GROUP!r} "
                                   f"node group to scatter with")
            return {"CANCELLED"}

        collection = scatter_set(assets)
        modifier = surface.modifiers.new(f"Scatter {collection.name}", "NODES")
        modifier.node_group = tree
        set_inputs(modifier, {
            "Instance Type": INSTANCE_TYPE_COLLECTION,
            "Collection": collection,
            "Density": self.density,
            # Picking is the point of scattering: between an asset's own
            # object variants, and between the assets of a set; with one
            # object it costs nothing.
            "Pick Instance": True,
            # Identical poses read as wallpaper, so the one thing worth
            # turning on up front is a random turn about the up axis.
            "Randomize": True,
            "Randomize Rotation": [0.0, 0.0, math.pi],
            "Align Rotation": self.align,
        })
        mask = self.mask.strip() or slope_mask_attribute(surface)
        if mask:
            bind_attribute(modifier, "Distribution Mask", mask)
        surface.update_tag()
        context.view_layer.update()

        emitted, matched = instance_counts(context, surface, collection)
        if emitted and not matched:
            self.report({"WARNING"},
                        f"{collection.name} did not take: set the modifier's "
                        f"'Instance Type' to Collection by hand")
            return {"FINISHED"}
        # The pointed-at empties have done their job as pointers; they stay
        # on as the scatter's hidden sources, carrying per-asset options.
        kept = retire_sources(context, assets)
        if not emitted:
            self.report({"WARNING"}, f"nothing landed on {surface.name}: "
                                     f"raise the density"
                                     + (f", or widen {mask}" if mask else ""))
        else:
            self.report({"INFO"}, f"scattering {matched} of "
                                  f"{collection.name} over {surface.name}"
                                  + (f" within {mask}" if mask else "")
                                  + f"; {kept}"
                                  + (f" ({note})" if note else ""))
        return {"FINISHED"}


class SMDL_OT_add_cluster_scatter(bpy.types.Operator):
    """Scatter the selected assets over the active mesh in clumps: patches
    first, then the assets within each patch, each instance picking one at
    random when several are selected"""

    bl_idname = "smdl.add_cluster_scatter"
    bl_label = "Scatter Assets In Clusters"
    bl_options = {"REGISTER", "UNDO"}

    clusters: bpy.props.FloatProperty(
        name="Clusters", description="Clumps per square unit of surface",
        default=0.05, min=1e-6, soft_max=10.0)
    radius: bpy.props.FloatProperty(
        name="Patch Radius", subtype="DISTANCE",
        description="How far a clump reaches. Keep it under about half a "
                    "metre on rough ground: the patch is flat, so its "
                    "instances follow the disk rather than the surface",
        default=1.0, min=0.001, soft_max=20.0)
    spread: bpy.props.FloatProperty(
        name="Spread",
        description="The Gaussian's standard deviation as a fraction of the "
                    "radius. Small gathers the clump at its centre; one "
                    "spreads it flat to the rim",
        default=0.45, min=0.05, max=1.0)
    count: bpy.props.IntProperty(
        name="Per Cluster",
        description="Instances in an average clump. Scattering is random, "
                    "so the count varies around it",
        default=12, min=1, soft_max=1000)
    size_variation: bpy.props.FloatProperty(
        name="Size Variation",
        description="Vary clump size by this fraction, so the patches do "
                    "not read as one stamp repeated",
        default=0.3, min=0.0, max=1.0)
    align: bpy.props.BoolProperty(
        name="Align To Surface",
        description="Lean instances with the surface normal, which ground "
                    "cover wants and an upright trunk does not",
        default=True)

    @classmethod
    def poll(cls, context):
        ob = context.active_object
        # An imported asset has no geometry of its own, but `as_surface()`
        # gives it some, so a terrain that arrived as an asset is as good a
        # surface as one modeled here.
        return ob is not None and (ob.type == "MESH" or is_asset_instance(ob))

    def invoke(self, context, event):
        # Asked for up front, since a Properties tab has no "Adjust Last
        # Operation" panel to tune them in afterward.
        return context.window_manager.invoke_props_dialog(self)

    def execute(self, context):
        # The surface first: converting an asset instance replaces it, and
        # what is selected has to be read after that.
        surface, note = as_surface(context, context.active_object)
        if surface is None:
            self.report({"ERROR"}, note)
            return {"CANCELLED"}
        assets, problem = selected_assets(context, surface)
        if not assets:
            self.report({"ERROR"}, problem)
            return {"CANCELLED"}
        tree = scatter_node_group()
        if tree is None:
            self.report({"ERROR"}, f"this Blender ships no {SCATTER_GROUP!r} "
                                   f"node group to scatter with")
            return {"CANCELLED"}
        collection = scatter_set(assets)

        name = f"{collection.name} Patch"
        mesh = cluster_patch_mesh(name, self.radius, self.spread)
        patch = bpy.data.objects.new(name, mesh)
        # Scaffolding, not content: the layout skips what a render skips,
        # so the disks stay out of the export while the second scatter goes
        # on seeing them.
        patch.hide_render = True
        context.scene.collection.objects.link(patch)

        if not self.align:
            # Before anything of ours exists, so that what is already
            # scattered here keeps the rotation it asked for.
            pin = surface.modifiers.new("Pin Rotations", "NODES")
            pin.node_group = pin_node_group()
        centres = surface.modifiers.new(f"Cluster {collection.name}", "NODES")
        centres.node_group = tree
        set_inputs(centres, {
            "Object": patch,
            "Density": self.clusters,
            # A clump lies on the ground it grows out of, whatever the
            # instances within it end up doing.
            "Align Rotation": True,
            "Randomize": self.size_variation > 0.0,
            "Randomize Scale": self.size_variation,
            "Randomize Rotation": [0.0, 0.0, math.pi],
        })
        mask = slope_mask_attribute(surface)
        if mask:
            bind_attribute(centres, "Distribution Mask", mask)

        area = math.pi * self.radius ** 2 * patch_mean(mesh)
        modifier = surface.modifiers.new(f"Scatter {collection.name}", "NODES")
        modifier.node_group = tree
        set_inputs(modifier, {
            "Instance Type": INSTANCE_TYPE_COLLECTION,
            "Collection": collection,
            "Density": self.count / area,
            "Scatter on Instances": True,
            "Pick Instance": True,
            "Align Rotation": True,
            "Randomize": True,
            "Randomize Rotation": [0.0, 0.0, math.pi],
        })
        bind_attribute(modifier, "Distribution Mask", PATCH_ATTRIBUTE)
        if not self.align:
            # Turning the scatter's alignment off would only stand an
            # instance up within its clump, and the clump lies on the
            # slope, so the world-space rotation is what has to go.
            upright = surface.modifiers.new(f"Upright {collection.name}",
                                            "NODES")
            upright.node_group = upright_node_group()
        surface.update_tag()
        context.view_layer.update()

        emitted, matched = instance_counts(context, surface, collection)
        if emitted and not matched:
            self.report({"WARNING"},
                        f"{collection.name} did not take: set the modifier's "
                        f"'Instance Type' to Collection by hand")
            return {"FINISHED"}
        kept = retire_sources(context, assets)
        patches = emitted - matched
        if not matched:
            self.report({"WARNING"}, f"nothing landed on {surface.name}: "
                                     f"raise the cluster density"
                                     + (f", or widen {mask}" if mask else ""))
        else:
            # The achieved average rather than the asked-for one: the
            # distribution is random and the mask is sampled per face, so
            # the two land within about a tenth of each other.
            self.report({"INFO"},
                        f"scattering {matched} of {collection.name} over "
                        f"{patches} clump(s) on {surface.name}, "
                        f"{matched / max(1, patches):.0f} per clump"
                        + (f" within {mask}" if mask else "")
                        + f"; {kept}"
                        + (f" ({note})" if note else ""))
        return {"FINISHED"}


CLASSES = (SMDL_OT_add_slope_mask, SMDL_OT_scatter_asset,
           SMDL_OT_add_cluster_scatter)
