"""Writing a `.layout` out of a Blender scene.

The whole exporter rests on one fact established when the proxy was built:
a proxy object's local space is exactly the space the renderer places, so
the matrix Blender shows for it is the matrix the layout writes, with
nothing to convert. See `etc/scripts/asset_proxy.py`.

A tagged proxy becomes an `asset` declaration and a `place` per instance,
with `depsgraph.object_instances` flattening geometry-nodes scatter and
particles into the same list. A collection instance of an untagged
collection holding tagged proxies is a user-authored arrangement, and
becomes a `group` placed once per instance so the arrangement survives the
round trip. Either list past a threshold is written as a binary `.places`
sidecar scattered with `place <name> * "<file>"`, which the renderer
instantiates as one Embree instance array. A Curves object is hair groomed
here rather than imported: it bakes to a `.curves` sidecar (see
`curves.py`) declared and placed as an asset of its own.
"""

import math
import os
import re
import struct

import bpy

# The custom properties a proxy carries to say what it stands for.
ASSET_KEY = "smdl_asset"
SELECT_KEY = "smdl_select"

# The sidecar holding whatever was modeled in Blender rather than imported.
BAKED_SUFFIX = ".blender.obj"

# The node group `scatter.py` gives an asset so that it has geometry to
# scatter over. Named here because the export is what it has to agree with:
# an object growing an asset's proxy has to carry the asset's tag, or the
# proxy is baked in place of the asset it stands for.
SURFACE_GROUP = "SMDL Asset Surface"

# Where `scatter.py` files a scattered asset's empty: retained render
# hidden rather than deleted, so the outliner still offers it and its
# export options stay editable. Named here because the export reads it
# back: a member of this collection outranks other hidden instancers when
# `scatter_source_options()` decides whose options a scattered instance
# gets.
SOURCES_COLLECTION = "SMDL Scatter Sources"

# Placement lists at or above this length become '.places' sidecars; below
# it, readable text wins.
PLACES_THRESHOLD = 64


def tag_of(ob):
    """The (manifest, select) an object stands for, or None.

    Read from the object, and failing that from any collection it belongs
    to, since dragging an asset out of the Asset Browser instances a
    collection rather than an object.
    """
    if ASSET_KEY in ob.keys():
        return str(ob[ASSET_KEY]), str(ob.get(SELECT_KEY, ""))
    for collection in ob.users_collection:
        if ASSET_KEY in collection.keys():
            return (str(collection[ASSET_KEY]),
                    str(collection.get(SELECT_KEY, "")))
    return None


def collection_tag(collection):
    """The (manifest, select) a proxy collection stands for, or None.

    The tag sits in a different place depending on how the asset arrived:
    on the instancing empty when Import Asset made it, and on the
    collection, a collection inside it, or the proxy objects when it was
    dragged out of the Asset Browser, since a dragged instance carries
    nothing of its own.
    """
    holders = ([collection] + list(collection.children_recursive)
               + list(collection.all_objects))
    for holder in holders:
        if ASSET_KEY in holder.keys():
            return str(holder[ASSET_KEY]), str(holder.get(SELECT_KEY, ""))
    return None


def is_asset_collection(collection):
    """Is this collection a prepared asset?"""
    return collection_tag(collection) is not None


def asset_tag(ob):
    """The (manifest, select) an asset instance stands for, or None.

    What the layout is written from: an object carrying this is placed by
    name, and one without it is geometry modeled here and is baked. So a
    surface built out of an asset has to carry it, or the asset's proxy
    lands in the sidecar in place of the asset itself.
    """
    if ASSET_KEY in ob.keys():
        return str(ob[ASSET_KEY]), str(ob.get(SELECT_KEY, ""))
    if ob.instance_collection is None:
        return None
    return collection_tag(ob.instance_collection)


def is_render_hidden(instance):
    """Is this instance's source, or whatever emits it, kept out of renders?

    Blender leaves such an object and everything it emits out of a render,
    so the layout leaves them out too. That is what makes an object
    authoring scaffolding rather than content: the patch disks a clustered
    scatter distributes over are geometry in the scene and belong in no
    layout.
    """
    if instance.object.original.hide_render:
        return True
    parent = instance.parent
    return parent is not None and parent.original.hide_render


def objects_under(collection):
    """Every object a collection holds, its child collections included."""
    found = set(collection.objects)
    for child in collection.children:
        found |= objects_under(child)
    return found


def is_group_instance(ob):
    """Is this a collection instance of a user-authored arrangement, as
    opposed to of an asset proxy?"""
    return (ob.type == "EMPTY" and ob.instance_type == "COLLECTION"
            and ob.instance_collection is not None
            and tag_of(ob) is None
            and ASSET_KEY not in ob.instance_collection.keys())


NO_OPTIONS = ((0, "CATMARK", False, False), (False, False), "", (), ())


def options_of(ob):
    """The per-object export options of a placed asset: the subdivision
    part (levels, scheme, linear, displace), the caustic marks (caster,
    caustic), the flat material override, the flat variant names, and the
    per-slot rows that carry content as (slot, override, variants) triples.

    Subdivision and the marks are properties of the asset declaration, so
    they join the key `gather()` groups placements by; everything else is
    a per-place fact.

    Any content-bearing slot row switches the asset to per-slot
    resolution and the flat fields stand down; `gather()` applies that
    precedence, and the panel grays the flat fields to match. Stale rows
    are export-inert, and `diagnose()` reports the ones with content."""
    options = getattr(ob, "smdl_asset_options", None)
    if options is None:
        return NO_OPTIONS
    levels = int(options.subdivide)
    subdiv = (levels,
              options.scheme if levels else "CATMARK",
              bool(options.linear) if levels else False,
              bool(options.displace))
    marks = (bool(options.caster), bool(options.caustic))
    variants = tuple(name.strip() for name in options.variants.split(",")
                     if name.strip())
    slots = []
    for row in options.slots:
        if row.stale:
            continue
        row_override = row.material.strip()
        row_variants = tuple(name.strip()
                             for name in row.variants.split(",")
                             if name.strip())
        if row_override or row_variants:
            slots.append((row.name, row_override, row_variants))
    return subdiv, marks, options.material.strip(), variants, tuple(slots)


def slot_pick(random_id, slot):
    """A stable per-instance seed for one slot's variant list.

    `random_id` folded with the slot name and finalized, so slots pick
    independently and editing one slot's list never reshuffles another
    slot's picks. The flat variants channel keeps its historical bare
    `random_id` modulo instead, so files from before slots existed
    re-export byte-identically.
    """
    mixed = random_id & 0xFFFFFFFF
    for byte in slot.encode("utf-8"):
        mixed = ((mixed ^ byte) * 0x01000193) & 0xFFFFFFFF
    mixed = ((mixed ^ (mixed >> 16)) * 0x045D9F3B) & 0xFFFFFFFF
    mixed = ((mixed ^ (mixed >> 16)) * 0x045D9F3B) & 0xFFFFFFFF
    return mixed ^ (mixed >> 16)


def slot_names(tag):
    """The material slot names the manifest records for `tag`, in manifest
    order, or [] when the manifest cannot say.

    An empty select means the whole file, so it collects over every
    object. These names are what the renderer resolves and what per-slot
    rows target, so nothing else may invent them.
    """
    from . import manifest as manifest_module
    manifest_path, select = tag
    try:
        asset = manifest_module.Asset(manifest_path)
    except (OSError, ValueError):
        return []
    if select:
        entry = asset.object_for(select)
        return list(entry["materials"]) if entry else []
    names = []
    for name in asset.materials:
        # The whole-file list, which is where a root-node mesh with no
        # selectable objects records its names.
        if name not in names:
            names.append(name)
    for entry in asset.objects:
        for name in entry["materials"]:
            if name not in names:
                names.append(name)
    return names


def seed_slots(ob):
    """Seed (or refresh) a tagged object's per-slot material rows from its
    manifest. Returns how many rows were added.

    Idempotent, and never destructive: existing rows keep whatever was
    typed into them, rows whose slot the manifest no longer names are
    flagged stale rather than deleted, and a single-material asset gets no
    rows at all, since the flat override and variants fields are the
    better interface for it. Called from operators, because a panel
    cannot create collection items while drawing.
    """
    tag = asset_tag(ob)
    options = getattr(ob, "smdl_asset_options", None)
    if tag is None or options is None:
        return 0
    names = slot_names(tag)
    for row in options.slots:
        row.stale = row.name not in names
    if len(names) < 2:
        return 0
    existing = {row.name for row in options.slots}
    added = 0
    for name in names:
        if name not in existing:
            row = options.slots.add()
            row.name = name
            added += 1
    return added


def groom_options_of(ob):
    """The per-groom export options: (mode, radius_scale, material)."""
    options = getattr(ob, "smdl_groom_options", None)
    if options is None:
        return "TUBE", 1.0, ""
    return options.mode, float(options.radius_scale), options.material.strip()


def scatter_source_options(scene):
    """Per-object export options claimed by retained scatter sources.

    A scattered instance's options ride on the hidden source empty
    `scatter.py` files away, not on the emitting surface, which scatters
    other assets too; they cannot ride on the proxies, which are linked and
    read-only. A source is a tagged, render-hidden collection instancer,
    mapped here from every member of its proxy collection to its options.
    Sources filed in the sources collection outrank other render-hidden
    instancers, so a placed asset the user merely hid cannot displace a
    filed source; the emitter remains the fallback for files from before
    sources were retained.
    """
    filed = {}
    loose = {}
    for ob in scene.objects:
        if (not ob.hide_render or ob.instance_type != "COLLECTION"
                or ob.instance_collection is None or asset_tag(ob) is None):
            continue
        in_sources = any(held.name.split(".")[0] == SOURCES_COLLECTION
                         for held in ob.users_collection)
        (filed if in_sources else loose).setdefault(ob.instance_collection,
                                                    ob)
    claimed = {}
    for tier in (filed, loose):
        for source_collection, ob in tier.items():
            options = options_of(ob)
            for member in source_collection.all_objects:
                claimed.setdefault(member, options)
    return claimed


def gather(context, collection=None):
    """Everything the layout asks for: the flat asset placements grouped by
    what they place, how they refine it, and how it is marked for caustics,
    each entry a (matrix, flat material override, per-slot combo) triple
    where the combo is the (slot, chosen name) picks of an asset resolved
    per slot; the group
    bundles (one per instanced user collection:
    its tagged members, its groom members, and the world matrix of every
    instance); the untagged objects, which were modeled here rather than
    imported and so have to be baked rather than referenced; and the flat
    grooms (one per Curves object, with its placement matrices), which
    bake to `.curves` sidecars.

    A group member's options come from the member object inside the
    collection that carries its tag; members that share a tag share
    options, first one wins. With a collection, only what that collection
    holds is gathered.
    """
    wanted = objects_under(collection) if collection is not None else None
    depsgraph = context.evaluated_depsgraph_get()

    # The candidate arrangements: untagged collection instances, grouped
    # by the collection they instance, in scene order.
    group_empties = {}
    group_order = []
    for ob in context.scene.objects:
        if wanted is not None and ob not in wanted:
            continue
        if not is_group_instance(ob):
            continue
        source = ob.instance_collection
        if source not in group_empties:
            group_empties[source] = []
            group_order.append(source)
        group_empties[source].append(ob)

    # First pass: an arrangement that holds geometry modeled in place has
    # nothing for a `group` member to reference, so it is demoted whole and
    # its instances flatten.
    emitters = {ob: source for source in group_order
                for ob in group_empties[source]}
    demoted = set()
    for instance in depsgraph.object_instances:
        ob = instance.object
        if ob is None or ob.type != "MESH" or not ob.data.vertices:
            continue
        if not instance.is_instance or instance.parent is None:
            continue
        if is_render_hidden(instance):
            continue
        emitter = instance.parent.original
        if emitter in emitters and tag_of(ob.original) is None:
            demoted.add(emitters[emitter])
    for source in demoted:
        del group_empties[source]
    group_order = [source for source in group_order if source not in demoted]
    emitters = {ob: source for source in group_order
                for ob in group_empties[source]}
    first_of = {source: group_empties[source][0] for source in group_order}

    # A group member's options come off the tagged member objects inside
    # its collection, matched to emissions by tag.
    member_options = {}
    for source in group_order:
        by_tag = {}
        for ob in objects_under(source):
            tag = tag_of(ob)
            if tag is not None and tag not in by_tag:
                by_tag[tag] = options_of(ob)
        member_options[source] = by_tag

    # Second pass: the placements themselves. A group's members are read
    # off the emissions of its FIRST instance, relative to that instance,
    # so the member matrices are exactly what Blender composes.
    source_options = scatter_source_options(context.scene)
    placements = {}
    order = []
    untagged = []
    baked = set()
    grooms = {}
    groom_order = []
    members = {source: [] for source in group_order}
    groom_members = {source: [] for source in group_order}
    for instance in depsgraph.object_instances:
        ob = instance.object
        if ob is None:
            continue
        if ob.type == "CURVES":
            # Hair groomed here rather than imported: baked to a `.curves`
            # sidecar and placed, one groom per original object, since the
            # grooming modifiers live on the object. See `curves.py`.
            if is_render_hidden(instance):
                continue
            original = ob.original
            emitter = (instance.parent.original
                       if instance.is_instance and instance.parent else None)
            if emitter is not None and emitter in emitters:
                # A groom inside a user arrangement: unlike an untagged
                # mesh it references an asset of its own once baked, so
                # the arrangement keeps its `group` and the groom rides
                # along as a member.
                source = emitters[emitter]
                if emitter is first_of[source]:
                    relative = (first_of[source].matrix_world.inverted() @
                                instance.matrix_world)
                    groom_members[source].append((original, relative))
                continue
            if wanted is not None and not instance_is_wanted(instance,
                                                             wanted):
                continue
            if original not in grooms:
                grooms[original] = []
                groom_order.append(original)
            grooms[original].append(instance.matrix_world.copy())
            continue
        if ob.type != "MESH" or not ob.data.vertices:
            continue
        if is_render_hidden(instance):
            continue
        original = ob.original
        emitter = (instance.parent.original
                   if instance.is_instance and instance.parent else None)
        if emitter is not None and emitter in emitters:
            source = emitters[emitter]
            if emitter is first_of[source]:
                relative = (first_of[source].matrix_world.inverted() @
                            instance.matrix_world)
                tag = tag_of(original)
                subdiv, marks, override, _, member_slots = \
                    member_options[source].get(tag, NO_OPTIONS)
                members[source].append((tag, subdiv, marks, override,
                                        member_slots, relative))
            continue
        if wanted is not None and not instance_is_wanted(instance, wanted):
            continue
        tag = tag_of(original)
        if tag is None:
            # Only the emitting object is baked: an instanced untagged mesh
            # would otherwise go into the sidecar once per instance,
            # geometry and all. Anonymous geometry is the exception, since
            # Blender reports it against its emitter and no name can reach
            # it; the emitter is baked even when its own surface is empty,
            # and `bake_untagged()` picks the instances up from there.
            if not instance.is_instance or original is emitter:
                if original not in baked:
                    baked.add(original)
                    untagged.append(original)
            continue
        # The export options ride on the object the user placed and
        # edits, which for a proxy collection instance is the emitting
        # empty rather than the mesh inside it. A scattered instance is
        # emitted by the surface instead, so its asset's retained source
        # wins when one exists.
        owner = (instance.parent.original
                 if instance.is_instance and instance.parent else original)
        subdiv, marks, override, variants, slots = options_of(owner)
        if (instance.is_instance
                and not (owner.instance_type == "COLLECTION"
                         and owner.instance_collection is not None)):
            # The emitter's own slot rows describe the emitter's asset,
            # not whatever it scatters, so only its flat fields stand in
            # for a scattered instance without a retained source.
            slots = ()
            claimed = source_options.get(original)
            if claimed is not None:
                subdiv, marks, override, variants, slots = claimed
        combo = ()
        if slots:
            # Per-slot resolution: each slot picks on its own and the flat
            # fields stand down (the panel grays them to match). Variants
            # spread only over instances, exactly as the flat list does.
            picks = []
            for slot, slot_override, slot_variants in slots:
                if slot_variants and instance.is_instance:
                    picks.append((slot, slot_variants[
                        slot_pick(instance.random_id, slot)
                        % len(slot_variants)]))
                elif slot_override:
                    picks.append((slot, slot_override))
            combo = tuple(picks)
            override = ""
        elif variants and instance.is_instance:
            # One name per instance, chosen by the id Blender gives the
            # instance rather than by the order they arrive in, so a scatter
            # keeps its variants across exports and across a re-evaluation.
            # This is the only per-instance channel Blender offers Python:
            # instance attributes are not readable from here.
            override = variants[instance.random_id % len(variants)]
        key = (tag, subdiv, marks)
        if key not in placements:
            placements[key] = []
            order.append(key)
        placements[key].append((instance.matrix_world.copy(), override,
                                combo))

    groups = [(source, members[source], groom_members[source],
               [ob.matrix_world.copy() for ob in group_empties[source]])
              for source in group_order
              if members[source] or groom_members[source]]
    return ([(key, placements[key]) for key in order], groups, untagged,
            [(ob, grooms[ob]) for ob in groom_order])


def instance_is_wanted(instance, wanted):
    """Is this instance one of `wanted`, or emitted by one?

    An instanced object lives in the collection being instanced rather than
    in the one holding the instance, so what decides is the emitter.
    """
    if instance.is_instance:
        parent = instance.parent
        return parent is not None and parent.original in wanted
    return instance.object.original in wanted


def modifier_inputs(modifier):
    """A geometry-nodes modifier's input values, keyed by socket name.

    Keyed by name rather than by node group, so that a user's own copy or
    edit of a built-in group is read the same way as the original.
    """
    tree = modifier.node_group
    if tree is None:
        return {}
    values = {}
    for item in tree.interface.items_tree:
        if item.item_type != "SOCKET" or item.in_out != "INPUT":
            continue
        try:
            values[item.name] = modifier[item.identifier]
        except KeyError:
            continue
    return values


def reads_camera(tree, seen):
    """Does this node tree read the camera, directly or through a group?"""
    if tree is None or tree in seen:
        return False
    seen.add(tree)
    for node in tree.nodes:
        if node.bl_idname in ("GeometryNodeCameraInfo",
                              "GeometryNodeInputActiveCamera"):
            return True
        if reads_camera(getattr(node, "node_tree", None), seen):
            return True
    return False


def diagnose(context, collection=None):
    """What quietly changed what the export can see.

    Each of these leaves a file that looks right and is not, so each is
    worth a word even though none of them is an error. Several follow
    from the exporter reading the viewport depsgraph, which is the only one
    Blender offers outside a render engine: viewport visibility therefore
    decides what a layout contains. Others follow from an instance
    reaching the layout as a placement only when it references an object or
    a collection, which is what `As Instance` on Object Info, and Collection
    Info, produce; anything else instances anonymous geometry, which Blender
    reports against the emitter and the exporter can only bake. Legacy
    particle hair is its own case: rendered as strands it is in the
    depsgraph's instance list not at all, so it shows on screen and
    reaches no layout.
    """
    problems = []
    wanted = objects_under(collection) if collection is not None else None
    depsgraph = context.evaluated_depsgraph_get()

    anonymous = {}
    for instance in depsgraph.object_instances:
        if not instance.is_instance or instance.parent is None:
            continue
        emitter = instance.parent.original
        if instance.object.original is not emitter:
            continue
        if is_render_hidden(instance):
            continue
        if wanted is not None and emitter not in wanted:
            continue
        anonymous[emitter.name] = anonymous.get(emitter.name, 0) + 1
    for name, count in sorted(anonymous.items()):
        problems.append(f"{count} instance(s) emitted by {name} reference "
                        f"anonymous geometry rather than an object, so they "
                        f"are baked into {BAKED_SUFFIX} one copy at a time "
                        f"rather than placed")

    objects = wanted if wanted is not None else set(context.scene.objects)
    for ob in sorted(objects, key=lambda ob: ob.name):
        for modifier in ob.modifiers:
            if modifier.type != "NODES":
                continue
            if (modifier.node_group is not None
                    and modifier.node_group.name.split(".")[0] == SURFACE_GROUP
                    and tag_of(ob) is None):
                problems.append(f"{ob.name} grows an asset's proxy but "
                                f"carries no {ASSET_KEY}, so the proxy is "
                                f"baked into {BAKED_SUFFIX} in place of the "
                                f"asset it stands for")
            where = f"{modifier.name} on {ob.name}"
            if not modifier.show_viewport and modifier.show_render:
                problems.append(f"{where} is off in the viewport and on in "
                                f"the render, and the export reads the "
                                f"viewport, so it contributed nothing")
            values = modifier_inputs(modifier)
            visibility = values.get("Viewport Visibility")
            if isinstance(visibility, float) and visibility < 1.0:
                problems.append(f"{where} shows {visibility:.0%} of its "
                                f"instances in the viewport, and the export "
                                f"reads the viewport, so the rest are missing")
            # The hair assets' name for the same dial: the fraction of
            # interpolated strands the viewport shows.
            amount = values.get("Viewport Amount")
            if isinstance(amount, float) and amount < 1.0:
                problems.append(f"{where} shows {amount:.0%} of its strands "
                                f"in the viewport, and the export reads the "
                                f"viewport, so the rest are missing")
            if values.get("Realize Instances"):
                problems.append(f"{where} realizes its instances, so they are "
                                f"baked into {BAKED_SUFFIX} rather than "
                                f"placed")
            if reads_camera(modifier.node_group, set()):
                problems.append(f"{where} reads the camera, so whatever it "
                                f"culls or swaps is fixed in the layout for "
                                f"the current camera alone")
        # Hair as instanced objects reaches the depsgraph and is placed;
        # hair as strand paths reaches nothing.
        if not ob.hide_render:
            for system in ob.particle_systems:
                if (system.settings.type == "HAIR"
                        and system.settings.render_type == "PATH"):
                    problems.append(
                        f"{ob.name} grows hair with the legacy particle "
                        f"system, which is missing from the instance list "
                        f"the export reads, so the hair reaches no layout; "
                        f"Convert Particle System to Curves makes it a "
                        f"groom the export bakes")
        options = getattr(ob, "smdl_asset_options", None)
        if options is None or not len(options.slots):
            continue
        live_variants = False
        for row in options.slots:
            content = row.material.strip() or row.variants.strip()
            if row.stale and content:
                problems.append(
                    f"the manifest for {ob.name} no longer names the "
                    f"material slot {row.name!r}, so its override or "
                    f"variants were skipped: Sync Slots refreshes the rows")
            if not row.stale and row.variants.strip():
                live_variants = True
        if options.displace and live_variants:
            problems.append(
                f"{ob.name} displaces and randomizes slot materials, so the "
                f"renderer bakes one displaced mesh per material "
                f"combination in use: correct, but costly")

    # The caustic emitter mark reads as a property of one light and is a
    # statement about the whole scene: it turns the search from every
    # light and the sky into the marked ones alone, so the first mark
    # silently drops the sky out of it. Lamps are read out of the scene
    # rather than out of `wanted`, exactly as `light_blocks()` reads them.
    marked = [ob.name for ob in objects
              if getattr(ob, "smdl_asset_options", None) is not None
              and ob.smdl_asset_options.caustic]
    for ob in sorted(context.scene.objects, key=lambda ob: ob.name):
        if ob.type != "LIGHT" or not ob.visible_get():
            continue
        options = getattr(ob.data, "smdl_light_options", None)
        if options is None or not options.caustic:
            continue
        if ob.data.type in ("POINT", "SPOT"):
            marked.append(ob.name)
        else:
            problems.append(f"{ob.name} is marked a caustic emitter, but a "
                            f"{ob.data.type.lower()} lamp exports no light "
                            f"declaration to carry the mark")
    if marked and context.scene.smdl_render.sky_scale > 0:
        problems.append(f"{len(marked)} caustic emitter mark(s) restrict the "
                        f"caustic search to what is marked, so the sky is "
                        f"left out of it; clearing them all searches every "
                        f"light and the sky instead")
    return problems


def relative_asset_path(manifest, asset_root, scene_directory):
    """How to name an asset in the layout.

    The directory where naming it is unambiguous, which is what a
    hand-written layout says, and the manifest itself where it is not: a
    directory of variants holds one manifest per variant, and the renderer
    resolves a directory by finding the one manifest in it. Relative to the
    asset root if it lives under one, since that is what `-asset-dir`
    resolves; relative to the layout otherwise, and absolute as a last
    resort.
    """
    from . import manifest as manifest_module

    manifest = os.path.abspath(manifest)
    directory = os.path.dirname(manifest)
    single = manifest_module.find_in_directory(directory)
    target = directory if single and os.path.abspath(single) == manifest \
        else manifest
    for base in (asset_root, scene_directory):
        if not base:
            continue
        base = os.path.abspath(bpy.path.abspath(base))
        try:
            relative = os.path.relpath(target, base)
        except ValueError:
            continue
        if not relative.startswith(".."):
            return relative.replace(os.sep, "/")
    return target.replace(os.sep, "/")


def format_matrix(matrix):
    """A 4x4 in the row-major order the `matrix` operation reads."""
    rows = []
    for i in range(4):
        rows.append(" ".join(f"{matrix[i][j]:.9g}" for j in range(4)))
    return rows


def place_lines(name, matrix, indent=""):
    """One `place` of `name` under `matrix`, wrapped one row per line: the
    parser reads a number wherever it left off, so only the operation itself
    has to sit on the place line."""
    lead = f"{indent}place {name} matrix "
    rows = format_matrix(matrix)
    return [lead + rows[0]] + [" " * len(lead) + row for row in rows[1:]]


def write_places(filepath, matrices, column=None):
    """Write matrices as a `.places` buffer: the 20-byte header, the top
    three rows of each matrix row-major, then the optional variant column,
    as `programs/smdl-toy/Places.h` documents. `column` entries are variant
    indices, None where a record has no variant."""
    with open(filepath, "wb") as stream:
        stream.write(struct.pack("<8sHHII", b"SMDLPLCS", 1,
                                 1 if column else 0, len(matrices), 0))
        for matrix in matrices:
            stream.write(struct.pack(
                "<12f", *[matrix[i][j] for i in range(3) for j in range(4)]))
        if column:
            stream.write(struct.pack(
                f"<{len(column)}I",
                *[0xFFFFFFFF if index is None else index
                  for index in column]))


def to_identifier(name):
    """The nearest layout identifier to `name`."""
    result = "".join(ch if ch.isalnum() or ch == "_" else "_" for ch in name)
    if not result or result[0].isdigit():
        result = "_" + result
    return result


def camera_block(scene):
    """The scene camera as a `camera` directive, or nothing.

    Blender's camera looks down its local -Z with +Y up and states its
    vertical field of view directly, so this is a naming exercise rather
    than a conversion. The lens comes from the add-on's Camera Options
    panel alone; the camera datablock's own Depth of Field is deliberately
    not read, so blur can never come from a panel this add-on does not
    draw. A setting is written only when it differs from the renderer's
    default, which is what the grammar demands: zero is not a value there,
    it is the absence of one.
    """
    camera = scene.camera
    if camera is None or camera.type != "CAMERA":
        return []
    matrix = camera.matrix_world
    origin = matrix.translation
    forward = -matrix.col[2].xyz.normalized()
    up = matrix.col[1].xyz.normalized()
    target = origin + forward
    data = camera.data
    render = scene.render
    width = int(render.resolution_x * render.resolution_percentage / 100)
    height = int(render.resolution_y * render.resolution_percentage / 100)
    lines = [
        "camera {",
        f"  resolution {max(width, 1)} {max(height, 1)}",
        f"  look_from {origin.x:.9g} {origin.y:.9g} {origin.z:.9g}",
        f"  look_to {target.x:.9g} {target.y:.9g} {target.z:.9g}",
        f"  look_up {up.x:.9g} {up.y:.9g} {up.z:.9g}",
        f"  fovy {data.angle_y * 180.0 / 3.14159265358979:.9g}",
    ]
    settings = scene.smdl_render
    if settings.vignette > 0:
        lines.append(f"  vignetting {settings.vignette:.9g}")
    if settings.dof:
        distance = settings.focus_distance
        if settings.focus_object is not None:
            distance = (settings.focus_object.matrix_world.translation -
                        origin).dot(forward)
        if distance > 0:
            lines.append(f"  focus {distance:.9g}")
        # Edited in millimeters, written in scene units (meters).
        radius = settings.aperture_radius / 1000.0
        if radius > 0:
            lines.append(f"  aperture {radius:.9g}")
            if settings.blades_enable:
                lines.append(f"  blades {settings.blades}")
                if settings.blade_angle != 0:
                    lines.append(f"  blade_angle "
                                 f"{settings.blade_angle:.9g}")
            if settings.cat_eye_enable and settings.cat_eye > 0:
                lines.append(f"  cat_eye {settings.cat_eye:.9g}")
                # A factor of the aperture radius here, and the renderer's
                # default is already the wide-open radius, so 1 is silence.
                if settings.cat_eye_radius != 1.0:
                    lines.append(f"  cat_eye_radius "
                                 f"{settings.cat_eye_radius * radius:.9g}")
    if settings.distortion_enable:
        if settings.distortion_k1 != 0:
            lines.append(f"  distortion_k1 {settings.distortion_k1:.9g}")
        if settings.distortion_k2 != 0:
            lines.append(f"  distortion_k2 {settings.distortion_k2:.9g}")
        if settings.distortion_fit:
            lines.append("  distortion_fit")
    lines.append("}")
    return lines


def world_environment_image(scene):
    """The image the world uses as an environment, if it uses one."""
    world = scene.world
    if world is None or not world.use_nodes:
        return ""
    for node in world.node_tree.nodes:
        if node.type == "TEX_ENVIRONMENT" and node.image:
            path = bpy.path.abspath(node.image.filepath)
            if path:
                return os.path.abspath(path)
    return ""


def sky_block(scene):
    """The lighting as a `sky` directive, or nothing.

    A sun lamp gives the sun's position, which is the whole of what the
    procedural sky needs; the Sun-Sky Options panel supplies the
    atmosphere. An environment image takes the place of the procedural sky
    as it does on the command line, so it wins where both are present.

    Nothing else about the lamp is exported. Its energy is in units the
    fitted sky does not share, and inventing a conversion would be a way to
    look right and be wrong; brightness belongs to the panel's factor.
    """
    settings = scene.smdl_render
    # Zero cannot be written as a scale (the grammar rejects it, rightly:
    # zero is not a brightness), but it means something: no sky at all.
    if not settings.sky_scale > 0:
        return ["sky {", "  none", "}"]
    image = world_environment_image(scene)
    if image:
        lines = ["sky {", f'  ibl "{image}"']
        if settings.sky_scale != 1.0:
            lines.append(f"  ibl_scale {settings.sky_scale:.9g}")
        lines.append("}")
        return lines
    lines = []
    for ob in scene.objects:
        if ob.type == "LIGHT" and ob.data.type == "SUN" and ob.visible_get():
            # A sun lamp shines along its own -Z, so +Z points back at the
            # sun, which is the direction the renderer states.
            toward = ob.matrix_world.col[2].xyz.normalized()
            zenith = math.degrees(math.acos(max(-1.0, min(1.0, toward.z))))
            azimuth = math.degrees(math.atan2(toward.y, toward.x)) % 360.0
            lines.append(f"  sun_zenith {zenith:.9g}")
            lines.append(f"  sun_azimuth {azimuth:.9g}")
            break
    if settings.sky_scale != 1.0:
        lines.append(f"  scale {settings.sky_scale:.9g}")
    if settings.visibility != 23.0:
        lines.append(f"  visibility {settings.visibility:.9g}")
    if settings.water_vapor != 1.0:
        lines.append(f"  water_vapor {settings.water_vapor:.9g}")
    if not lines:
        return []
    return ["sky {"] + lines + ["}"]


def light_ies_path(light):
    """The IES file an SMDL profile light can read, or "".

    Blender attaches IES data through an IES Texture node on the light,
    which is read here exactly as `world_environment_image` reads the
    world's environment node: a filepath lifted from the tree, never the
    shading behind it. IES text embedded in the .blend has no file to
    name, so it does not export.
    """
    if not light.use_nodes or light.node_tree is None:
        return ""
    for node in light.node_tree.nodes:
        if node.type == "TEX_IES" and node.mode == "EXTERNAL" and node.filepath:
            path = bpy.path.abspath(node.filepath)
            if path:
                return os.path.abspath(path)
    return ""


def light_blocks(scene, problems):
    """The scene's point and spot lamps as `light` declarations plus one
    `place` per lamp, or nothing.

    A sun lamp belongs to `sky_block` and an area lamp is really an
    emissive material on a shape, which is the user's to author, so both
    are left out; area lamps get a problem note rather than silence.

    A lamp marked a caustic emitter says so here; `diagnose()` reports
    the marks on the lamps that do not export.

    Blender treats a spot as a point lamp with a cone mask, so its
    `energy` is the power of the full sphere and the cone keeps only its
    share; the layout's `power` is what the cone actually emits, so the
    export scales by the cone's solid-angle fraction and the on-axis
    intensity comes out identical.
    """
    lines = []
    names = set()
    for ob in scene.objects:
        if ob.type != "LIGHT" or not ob.visible_get():
            continue
        light = ob.data
        if light.type == "AREA":
            problems.append(f"the area lamp {ob.name!r} does not export; "
                            f"model it as a disk or plane with an emissive "
                            f"material instead")
            continue
        if light.type not in ("POINT", "SPOT"):
            continue
        name = "light_" + to_identifier(ob.name)
        while name in names:
            name += "_"
        names.add(name)
        ies = light_ies_path(light)
        settings = []
        if light.type == "SPOT":
            angle = math.degrees(light.spot_size)
            blend = light.spot_blend
            cos_outer = math.cos(light.spot_size / 2)
            cos_inner = math.cos(light.spot_size / 2 * (1 - blend))
            fraction = ((1 - cos_inner) + (cos_inner - cos_outer) / 2) / 2
            power = light.energy * fraction
            head = f"light {name} = spot {{"
            settings.append(f"  power {power:.9g}")
            settings.append(f"  angle {angle:.9g}")
            if blend > 0:
                settings.append(f"  blend {blend:.9g}")
        elif ies:
            head = f'light {name} = profile "{ies}" {{'
            settings.append(f"  power {light.energy:.9g}")
        else:
            head = f"light {name} = point {{"
            settings.append(f"  power {light.energy:.9g}")
        if getattr(light, "use_temperature", False):
            settings.append(f"  temperature {light.temperature:.9g}")
        color = tuple(light.color)
        if color != (1.0, 1.0, 1.0):
            settings.append(f"  color {color[0]:.9g} {color[1]:.9g} "
                            f"{color[2]:.9g}")
        marks = getattr(light, "smdl_light_options", None)
        if marks is not None and marks.caustic:
            settings.append("  caustic")
        lines.append(head)
        lines.extend(settings)
        lines.append("}")
        lines.extend(place_lines(name, ob.matrix_world))
        lines.append("")
    return lines


def is_mdl_identifier(name):
    if not name or not (name[0].isalpha() or name[0] == "_"):
        return False
    return all(ch.isalnum() or ch == "_" for ch in name)


def existing_materials(filepath):
    """The material assignments already written in the file being replaced,
    keyed by the (reference, select) of the asset they belong to.

    A layout is exported over and over as things move, and its material
    assignments are hand-authored knowledge the exporter cannot re-derive,
    so whatever an asset block said last time is written again verbatim.
    """
    found = {}
    try:
        with open(filepath) as stream:
            text = stream.read()
    except OSError:
        return found
    # Asset bodies hold no nested braces, so a non-greedy block match is
    # exact rather than approximate.
    pattern = re.compile(r'asset\s+\w+\s*=\s*"([^"]+)"\s*\{([^}]*)\}')
    for match in pattern.finditer(text):
        reference, body = match.group(1), match.group(2)
        select_match = re.search(r'select\s+"([^"]+)"', body)
        select = select_match.group(1) if select_match else ""
        assigned = [line.strip() for line in body.splitlines()
                    if line.strip().startswith("material ")]
        if assigned:
            found[(reference, select)] = assigned
    return found


def write_material_source(scene, filepath):
    """Write the scene's material text beside the layout, and say what
    stopped it.

    The file is named after the layout, like the other sidecars, so a
    layout directory stays the self-contained thing the format asks for.
    Nothing is written over unless it is already this source, or is the
    very file the text was opened from: an export that silently replaced a
    hand-written module would cost more than it saves.
    """
    text = getattr(scene, "smdl_material_text", None)
    if text is None:
        return "", ""
    source = text.as_string()
    if os.path.exists(filepath):
        try:
            with open(filepath) as stream:
                existing = stream.read()
        except OSError as error:
            return "", f"cannot read {filepath}: {error}"
        opened_from = os.path.abspath(bpy.path.abspath(text.filepath)) \
            if text.filepath else ""
        if existing != source and opened_from != os.path.abspath(filepath):
            return "", (f"{os.path.basename(filepath)} was not written from "
                        f"{text.name}, so it was left alone and the material "
                        f"source was not exported")
    try:
        with open(filepath, "w") as stream:
            stream.write(source)
    except OSError as error:
        return "", f"cannot write {filepath}: {error}"
    return filepath, ""


def bake_untagged(context, objects, filepath):
    """Write the objects modeled here rather than imported to a sidecar.

    Object and material names survive an OBJ, which is all the renderer
    needs from them: geometry is placed by name and shaded by name.

    Each object is baked from a copy of its evaluated mesh rather than
    exported with `apply_modifiers`, because the OBJ exporter writes the
    instances of what it exports: a scatter of tagged proxies over untagged
    terrain would land in the sidecar as well, one baked copy per placement
    the layout already carries. An evaluated mesh is the object's own
    geometry with its instances left behind.

    The instances the layout could not place are then added back, since
    nothing else keeps them: anonymous geometry, which has no object to
    reference at all, and instances of untagged meshes, which stand for no
    asset. Both can only go in the sidecar, one baked copy apiece.

    The copies borrow their originals' names for the duration, since the
    name is what the renderer sees.
    """
    depsgraph = context.evaluated_depsgraph_get()
    baking = set(objects)
    renamed = {}
    meshes = []
    copies = []
    try:
        for ob in objects:
            mesh = bpy.data.meshes.new_from_object(
                ob.evaluated_get(depsgraph), preserve_all_data_layers=True,
                depsgraph=depsgraph)
            if mesh is None:
                continue
            meshes.append(mesh)
            name = ob.name
            # A linked object cannot be renamed, so its copy keeps the
            # suffixed name Blender gives it rather than nothing baking.
            if ob.library is None:
                renamed[ob] = name
                ob.name = "_smdl_baking_" + name
            if not mesh.vertices:
                # A scatter that does not keep its surface has nothing of
                # its own; only what it instances is worth baking.
                continue
            copy = bpy.data.objects.new(name, mesh)
            copy.matrix_world = ob.matrix_world
            context.scene.collection.objects.link(copy)
            copies.append(copy)

        # One mesh copy serves every instance of the same geometry, which
        # is what Blender hands out: the instances share one data-block and
        # differ only in their transforms.
        shared = {}
        for instance in depsgraph.object_instances:
            if not instance.is_instance or instance.parent is None:
                continue
            emitter = instance.parent.original
            if emitter not in baking:
                continue
            if tag_of(instance.object.original) is not None:
                continue  # a tagged proxy is already a placement
            if is_render_hidden(instance):
                continue
            key = instance.object.data.as_pointer()
            mesh = shared.get(key)
            if mesh is None:
                mesh = shared[key] = instance.object.data.copy()
                meshes.append(mesh)
            # Anonymous geometry names itself after the emitter, since
            # Blender reports it against the emitter; an instanced mesh
            # keeps its own name.
            source = instance.object.original
            copy = bpy.data.objects.new(renamed.get(source, source.name), mesh)
            copy.matrix_world = instance.matrix_world
            context.scene.collection.objects.link(copy)
            copies.append(copy)
        if not copies:
            return
        previous = list(context.selected_objects)
        active = context.view_layer.objects.active
        bpy.ops.object.select_all(action="DESELECT")
        for copy in copies:
            copy.select_set(True)
        context.view_layer.objects.active = copies[0]
        # Materials are exported for their names alone: `usemtl` carries the
        # only thing the renderer wants, and turning materials off would drop
        # the name with them. The `.mtl` that comes along is never read.
        bpy.ops.wm.obj_export(filepath=filepath, export_selected_objects=True,
                              forward_axis="Y", up_axis="Z",
                              export_materials=True, apply_modifiers=False)
        bpy.ops.object.select_all(action="DESELECT")
        for ob in previous:
            ob.select_set(True)
        context.view_layer.objects.active = active
    finally:
        for copy in copies:
            bpy.data.objects.remove(copy)
        for mesh in meshes:
            bpy.data.meshes.remove(mesh)
        for ob, name in renamed.items():
            ob.name = name


def declaration_hint(manifest_path, select):
    """What to name an asset declaration after: the manifest, unless the
    manifest offers several objects and the select is what tells them
    apart. A single-object manifest's select is the mesh file's own node
    name, which is whatever its exporter left there."""
    from . import manifest as manifest_module
    if select:
        try:
            asset = manifest_module.Asset(manifest_path)
        except (OSError, ValueError):
            return select
        if len(asset.objects) > 1:
            return select
    return os.path.basename(manifest_path)


class _Assets:
    """The asset declarations an export accumulates, one per distinct
    (manifest, select), named as a layout identifier. Groups share this
    identifier namespace, so they are deduplicated here too."""

    def __init__(self):
        self.by_tag = {}
        self.order = []
        self.used_names = set()

    def unique(self, name):
        name = to_identifier(name) or "_"
        candidate, counter = name, 2
        while candidate in self.used_names:
            candidate = f"{name}_{counter}"
            counter += 1
        self.used_names.add(candidate)
        return candidate

    def name_for(self, key, hint, suffix=""):
        if key in self.by_tag:
            return self.by_tag[key]
        # The suffix goes on after the manifest extension comes off, or
        # every suffixed name carries the '.asset' through into itself.
        if hint.endswith(".asset"):
            hint = hint[:-len(".asset")]
        name = self.unique(hint + suffix)
        self.by_tag[key] = name
        self.order.append(key)
        return name


def write_scene(context, filepath, asset_root="", bake=True, collection=None,
                places_threshold=PLACES_THRESHOLD):
    """Write the Blender scene as a `.layout`. Returns a report of what it
    did and of anything the renderer will not be able to resolve."""
    from . import manifest as manifest_module

    scene_directory = os.path.dirname(os.path.abspath(filepath))
    layout_stem = os.path.splitext(os.path.basename(filepath))[0]
    flat, groups, untagged, grooms = gather(context, collection)
    problems = diagnose(context, collection)
    materials = []
    alias_targets = {}
    sidecars = []
    previous_materials = existing_materials(filepath)
    material_file, material_problem = write_material_source(
        context.scene, os.path.join(scene_directory, layout_stem + ".smdl"))
    if material_problem:
        problems.append(material_problem)
    # Exposure is a tonemapping option rather than a scene fact, so the
    # layout cannot carry it; the suggested command does instead.
    named = os.path.basename(material_file) if material_file \
        else "<materials>.mdl"
    hint = f"smdl-toy <this file> {named} -asset-dir <asset root>"
    exposure = context.scene.smdl_render.exposure
    if exposure != 1.0:
        hint += f" -exposure {exposure:.9g}"
    lines = [
        "#smdl layout",
        "# Written by the SpectralMDL layout add-on from "
        f"{os.path.basename(bpy.data.filepath) or 'an unsaved file'}.",
        "#",
        "# Render with the asset library on the search path:",
        f"#   {hint}",
        "",
    ]

    # One declaration per distinct (manifest, select, refinement, marks)
    # that anything places, directly or through a group: instances that
    # subdivide differently are different mesh data in the renderer, and
    # the caustic marks have no per-place spelling to carry them. The
    # renderer caches an import by file, selection, and refinement, so two
    # declarations that differ only in their marks still load one mesh.
    assets = _Assets()
    every_key = [key for key, _ in flat]
    for _, group_members, _, _ in groups:
        for tag, subdiv, marks, _, _, _ in group_members:
            if (tag, subdiv, marks) not in every_key:
                every_key.append((tag, subdiv, marks))
    declared = []
    base_of = {}
    assignment_of = {}
    for key in every_key:
        (manifest_path, select), subdiv, marks = key
        levels, scheme, linear, displace = subdiv
        caster, caustic = marks
        if not os.path.exists(manifest_path):
            problems.append(f"the asset manifest {manifest_path} is gone, so "
                            f"placement(s) of it were skipped")
            continue
        suffix = f"_sub{levels}" if levels else ""
        if displace and not levels:
            suffix = "_displaced"
        if caster:
            suffix += "_caster"
        if caustic:
            suffix += "_caustic"
        name = assets.name_for(
            key, declaration_hint(manifest_path, select), suffix)
        reference = relative_asset_path(manifest_path, asset_root,
                                        scene_directory)
        # What shades this asset: whatever it was assigned last time. Only
        # when that says nothing do the mesh file's own material names go
        # into the file-wide alias list at the bottom.
        assigned = previous_materials.get((reference, select))
        if not assigned:
            asset = manifest_module.Asset(manifest_path)
            entry = asset.object_for(select) if select else None
            for material_name in (entry["materials"] if entry else []):
                if material_name not in materials:
                    materials.append(material_name)
                if not is_mdl_identifier(material_name):
                    # The alias section below will actively rename this
                    # name, and a place override composes OUTSIDE the
                    # file's aliases, so a rename targeting the slot has
                    # to speak about the alias target instead: see
                    # `resolve_renames()`.
                    alias_targets[material_name] = to_identifier(
                        material_name)
        # The base a flat per-place override renames: known exactly when
        # the asset assigns one material to everything.
        base = ""
        if len(assigned) == 1:
            words = assigned[0].split()
            if len(words) == 2 and words[0] == "material":
                base = words[1]
        base_of[key] = base
        # The per-slot view of the same lines, mirroring the renderer's
        # resolution: a slot renames through its own assignment, else the
        # whole-asset one, else its raw mesh name.
        all_name = ""
        by_slot = {}
        for line in assigned:
            slot_line = re.match(r'material\s+"([^"]+)"\s*=\s*(\S+)$', line)
            if slot_line:
                by_slot[slot_line.group(1)] = slot_line.group(2)
                continue
            words = line.split()
            if len(words) == 2 and words[0] == "material":
                all_name = words[1]
        assignment_of[key] = (all_name, by_slot)
        lines.append(f'asset {name} = "{reference}" {{')
        if select:
            # The proxy was built around this object's recentered pivot, so
            # the layout has to recenter to match it.
            lines.append(f'  select "{select}"')
            lines.append("  recenter")
        for assignment in assigned:
            lines.append(f"  {assignment}")
        if levels:
            op = f"  subdivide {levels}"
            if scheme == "LOOP":
                op += " loop"
            if linear:
                op += " linear"
            lines.append(op)
        if displace:
            lines.append("  displace")
        if caster:
            lines.append("  caster")
        if caustic:
            lines.append("  caustic")
        lines.append("}")
        declared.append(key)
    if declared:
        lines.append("")
    flat = [(key, entries) for key, entries in flat if key in declared]
    groups = [(source,
               [entry for entry in group_members
                if entry[:3] in declared],
               groom_members, instances)
              for source, group_members, groom_members, instances in groups]

    # The groom declarations: hair groomed here, baked to a `.curves`
    # sidecar apiece and placed like any other asset, directly or as a
    # group member. The material comes from the panel, else whatever was
    # hand-assigned last time, else the first material slot; the grammar
    # requires one on a curves asset, so a groom that says nothing is
    # shaded by its own name and reported.
    from . import curves as curves_module
    depsgraph = context.evaluated_depsgraph_get()
    every_groom = [ob for ob, _ in grooms]
    for _, _, groom_members, _ in groups:
        for ob, _ in groom_members:
            if ob not in every_groom:
                every_groom.append(ob)
    groom_names = {}
    for ob in every_groom:
        name = assets.unique(ob.name)
        sidecar = f"{layout_stem}.{name}.curves"
        strands, _, _, notes = curves_module.bake(
            depsgraph, ob, os.path.join(scene_directory, sidecar))
        problems.extend(notes)
        if not strands:
            continue
        sidecars.append(sidecar)
        mode, radius_scale, material_name = groom_options_of(ob)
        assigned = previous_materials.get((sidecar, ""))
        if not material_name and not assigned:
            material_name = next((slot.name for slot in ob.material_slots
                                  if slot.name), "")
            if not material_name:
                material_name = ob.name
                problems.append(f"the groom {ob.name} has no material, so "
                                f"its own name shades it")
        if material_name:
            if not is_mdl_identifier(material_name):
                fixed = to_identifier(material_name)
                problems.append(f"the material name {material_name!r} on "
                                f"the groom {ob.name} is not an MDL "
                                f"identifier, so {fixed} was written "
                                f"instead")
                material_name = fixed
            assigned = [f"material {material_name}"]
        lines.append(f'asset {name} = "{sidecar}" {{')
        for assignment in assigned:
            lines.append(f"  {assignment}")
        if mode == "RIBBON":
            lines.append("  ribbon")
        if radius_scale != 1.0:
            lines.append(f"  radius_scale {radius_scale:.9g}")
        lines.append("}")
        groom_names[ob] = name
    if groom_names:
        lines.append("")
    groom_placements = [(groom_names[ob], matrices)
                        for ob, matrices in grooms if ob in groom_names]
    groups = [(source, group_members,
               [(ob, matrix) for ob, matrix in groom_members
                if ob in groom_names],
               instances)
              for source, group_members, groom_members, instances in groups]
    groups = [entry for entry in groups if entry[1] or entry[2]]

    reported = set()

    def say_once(problem):
        # Per-instance resolution rediscovers the same conflict once per
        # entry, and a repeated problem line helps nobody.
        if problem not in reported:
            reported.add(problem)
            problems.append(problem)

    def resolve_renames(name, key, entries, where=""):
        """Each entry's concrete rename pairs, as (matrix, pairs): a slot
        combo resolves through the asset's assignment (a slot's own line,
        else the whole-asset one, else its raw mesh name, quoted when it
        is not an identifier), and the flat override resolves through the
        single declared base exactly as it always has. Renames are
        applied once and never chained, so two slots sharing one resolved
        base cannot be split: the first wins and the conflict is
        reported.
        """
        base = base_of.get(key, "")
        all_name, by_slot = assignment_of.get(key, ("", {}))
        resolved = []
        for matrix, override, combo in entries:
            pairs = []
            used = {}
            for slot, chosen in combo:
                slot_base = by_slot.get(slot) or all_name or slot
                # A place override composes OUTSIDE the file's aliases,
                # so where the alias section renames this leftover name,
                # the alias target is the name to rename.
                slot_base = alias_targets.get(slot_base, slot_base)
                if slot_base in used:
                    if used[slot_base][1] != chosen:
                        say_once(
                            f"the slots {used[slot_base][0]!r} and {slot!r} "
                            f"of {name} both resolve to {slot_base!r}, and "
                            f"one rename cannot split them, so "
                            f"{used[slot_base][0]!r} wins: assign the slots "
                            f"distinct materials on the asset declaration "
                            f"to separate them")
                    continue
                used[slot_base] = (slot, chosen)
                if chosen != slot_base:
                    pairs.append((slot_base if is_mdl_identifier(slot_base)
                                  else f'"{slot_base}"', chosen))
            if override and not combo:
                if base and override != base:
                    pairs.append((base, override))
                elif not base:
                    say_once(
                        f"cannot override the material of {name}{where}: "
                        f"the asset declares no single material to rename")
            resolved.append((matrix, tuple(pairs)))
        return resolved

    # The group declarations: user-authored arrangements, one `group` per
    # instanced collection, members placed relative to the instance.
    group_names = []
    for source, group_members, groom_members, _ in groups:
        name = assets.unique(source.name)
        group_names.append(name)
        lines.append(f"group {name} {{")
        for tag, subdiv, marks, override, member_slots, matrix \
                in group_members:
            key = (tag, subdiv, marks)
            member = assets.by_tag[key]
            # A group is one template placed whole, so a member's slot
            # overrides apply and its variants have nothing to vary over.
            if any(slot_variants for _, _, slot_variants in member_slots):
                say_once(f"material variants on {member} in {source.name} "
                         f"are ignored: a group is one template placed "
                         f"whole, so nothing varies per instance inside it")
            combo = tuple((slot, slot_override)
                          for slot, slot_override, _ in member_slots
                          if slot_override)
            _, pairs = resolve_renames(member, key,
                                       [(matrix, override, combo)],
                                       where=f" in {source.name}")[0]
            if pairs:
                lines.append(f"  place {member} {{")
                lines.extend(f"    material {frm} = {to}"
                             for frm, to in pairs)
                rows = format_matrix(matrix)
                lines.append("    matrix " + rows[0])
                lines.extend("           " + row for row in rows[1:])
                lines.append("  }")
            else:
                lines.extend(place_lines(member, matrix, "  "))
        for ob, matrix in groom_members:
            lines.extend(place_lines(groom_names[ob], matrix, "  "))
        lines.append("}")
        lines.append("")

    def emit_placements(name, entries):
        """The places of one name, each entry a (matrix, rename pairs):
        readable text below the threshold, a `.places` sidecar at or
        above it, where the distinct pair sets become the buffer's
        variant tables, one per material combination in use."""
        if places_threshold and len(entries) >= places_threshold:
            sidecar = f"{layout_stem}.{name}.places"
            tables = []
            table_index = {}
            column = []
            for _, pairs in entries:
                if not pairs:
                    column.append(None)
                    continue
                if pairs not in table_index:
                    table_index[pairs] = len(tables)
                    tables.append(pairs)
                column.append(table_index[pairs])
            write_places(os.path.join(scene_directory, sidecar),
                         [matrix for matrix, _ in entries],
                         column if tables else None)
            sidecars.append(sidecar)
            if tables:
                lines.append(f'place {name} * "{sidecar}" {{')
                for pairs in tables:
                    if len(pairs) == 1:
                        lines.append(f"  variant {{ material {pairs[0][0]} = "
                                     f"{pairs[0][1]} }}")
                    else:
                        lines.append("  variant {")
                        lines.extend(f"    material {frm} = {to}"
                                     for frm, to in pairs)
                        lines.append("  }")
                lines.append("}")
            else:
                lines.append(f'place {name} * "{sidecar}"')
        else:
            for matrix, pairs in entries:
                if pairs:
                    lines.append(f"place {name} {{")
                    lines.extend(f"  material {frm} = {to}"
                                 for frm, to in pairs)
                    rows = format_matrix(matrix)
                    lines.append("  matrix " + rows[0])
                    lines.extend("         " + row for row in rows[1:])
                    lines.append("}")
                else:
                    lines.extend(place_lines(name, matrix))

    for key, entries in flat:
        emit_placements(assets.by_tag[key],
                        resolve_renames(assets.by_tag[key], key, entries))
    for (_, _, _, instances), name in zip(groups, group_names):
        emit_placements(name, [(matrix, ()) for matrix in instances])
    for name, matrices in groom_placements:
        emit_placements(name, [(matrix, ()) for matrix in matrices])
    if flat or groups or groom_placements:
        lines.append("")

    if untagged and bake:
        baked = layout_stem + BAKED_SUFFIX
        bake_untagged(context, untagged, os.path.join(scene_directory, baked))
        lines.append("# Modeled here rather than imported, so baked out "
                     "whole.")
        lines.append(f'import "{baked}"')
        lines.append("")
        for ob in untagged:
            for slot in ob.material_slots:
                if slot.material and slot.material.name not in materials:
                    materials.append(slot.material.name)
            if not ob.material_slots:
                problems.append(f"{ob.name} has no material, so the renderer "
                                f"has no name to shade it by")

    if materials:
        lines.append("# The material names left over: the assets above say "
                     "what shades them,")
        lines.append("# and these are the mesh files' own names for what is "
                     "left. Give each one")
        lines.append("# the name of an MDL material, and uncomment any that "
                     "MDL cannot spell as")
        lines.append("# written. A name repeated across unrelated assets "
                     "cannot be told apart")
        lines.append("# here; assign those on the asset instead, with "
                     "'material <name>'.")
        for name in materials:
            if is_mdl_identifier(name):
                lines.append(f"# material \"{name}\" = {name}")
            else:
                lines.append(f"material \"{name}\" = {to_identifier(name)}")
                problems.append(f"the material name {name!r} is not an MDL "
                                f"identifier, so an alias was written for it")
        lines.append("")

    lines.extend(light_blocks(context.scene, problems))
    lines.extend(sky_block(context.scene))
    if lines and lines[-1] == "}":
        lines.append("")
    lines.extend(camera_block(context.scene))
    if context.scene.camera is None:
        problems.append("the scene has no camera, so the layout carries no "
                        "viewpoint")

    with open(filepath, "w") as stream:
        stream.write("\n".join(lines).rstrip() + "\n")

    return {
        "placements": (sum(len(entries) for _, entries in flat) +
                       sum(len(instances) *
                           (len(group_members) + len(groom_members))
                           for _, group_members, groom_members, instances
                           in groups) +
                       sum(len(matrices) for _, matrices in groom_placements)),
        "assets": len(declared) + len(groom_names),
        "grooms": len(groom_names),
        "groups": len(groups),
        "sidecars": sidecars,
        "baked": len(untagged) if bake else 0,
        "material_file": material_file,
        "materials": materials,
        "problems": problems,
    }
