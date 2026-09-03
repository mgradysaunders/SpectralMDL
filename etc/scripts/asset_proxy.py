"""Build the Blender proxy of a prepared asset. Runs inside Blender:

    blender -b --factory-startup --python asset_proxy.py -- \
        --manifest DIR/name.asset --listing listing.json [options]

`prepare_asset.py build` is what normally runs this. The render mesh is
imported raw, mapped into the file's own space (the space the renderer's
listing reports), checked against that listing, and then put through the
manifest's correction and each object's pivot, so that a proxy object's
local space is exactly the space the renderer places: the matrix Blender
shows for an instance is the matrix the layout writes. Each object becomes
one asset-marked collection holding one reduced, material-free mesh,
tagged with the manifest and the select, catalogued, and carrying its
thumbnail as the preview.
"""

import argparse
import importlib.util
import json
import math
import os
import random
import sys

import bmesh
import bpy
from mathutils import Matrix, Vector

HERE = os.path.dirname(os.path.abspath(__file__))


def load_module(name, path):
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


prepare = load_module("prepare_asset", os.path.join(HERE, "prepare_asset.py"))

ASSET_KEY = "smdl_asset"
SELECT_KEY = "smdl_select"


def parse_args():
    argv = sys.argv[sys.argv.index("--") + 1:] if "--" in sys.argv else []
    parser = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    parser.add_argument("--manifest", required=True)
    parser.add_argument("--listing", required=True,
                        help="the file's entry of 'smdl-toy -list-objects "
                             "-json', for the space check")
    parser.add_argument("--catalog-id")
    parser.add_argument("--tags", default="")
    parser.add_argument("--keep-leaf", type=float, default=0.15)
    parser.add_argument("--leaf-slots", default="leaf,snow,trail")
    parser.add_argument("--target-triangles", type=int, default=20000)
    parser.add_argument("--body-angle", type=float, default=30.0,
                        help="planar dissolve angle in degrees for a "
                             "tree's body")
    return parser.parse_args(argv)


def fail(message):
    print("Error: " + message)
    sys.exit(1)


#--{ Import

def mesh_objects():
    return [ob for ob in bpy.data.objects if ob.type == "MESH"]


def world_bounds(objects):
    lo = Vector((math.inf,) * 3)
    hi = Vector((-math.inf,) * 3)
    for ob in objects:
        for corner in ob.bound_box:
            point = ob.matrix_world @ Vector(corner)
            lo = Vector(map(min, lo, point))
            hi = Vector(map(max, hi, point))
    return lo, hi


def import_render(path, listing):
    """Import the render mesh and return the matrix from Blender's import
    space into the file's own space, verified against the listing."""
    ext = os.path.splitext(path)[1].lower()
    if ext == ".fbx":
        # Manual orientation with Blender's own axes is the identity, so
        # only the file's unit scale is applied, and the extents below
        # recover it rather than trusting either side's reading of it.
        bpy.ops.import_scene.fbx(
            filepath=path, use_manual_orientation=True, axis_forward="Y",
            axis_up="Z", global_scale=1.0, bake_space_transform=False,
            use_custom_normals=False, use_image_search=False, use_anim=False,
            ignore_leaf_bones=True)
        lo, hi = world_bounds(mesh_objects())
        ratios = []
        for axis in range(3):
            extent = hi[axis] - lo[axis]
            wanted = listing["bounds"][1][axis] - listing["bounds"][0][axis]
            if extent > 1e-9 and wanted > 1e-9:
                ratios.append(wanted / extent)
        if not ratios:
            fail(f"{path} has no extent to compare against the listing")
        to_file = Matrix.Scale(sorted(ratios)[len(ratios) // 2], 4)
    elif ext == ".obj":
        bpy.ops.wm.obj_import(filepath=path, forward_axis="Y", up_axis="Z",
                              global_scale=1.0, validate_meshes=True)
        to_file = Matrix.Identity(4)
    elif ext in (".gltf", ".glb"):
        # The glTF importer always turns the file's Y up into Blender's Z
        # up; undo it so the manifest's own correction is what applies.
        bpy.ops.import_scene.gltf(filepath=path, import_shading="NORMALS")
        to_file = Matrix.Rotation(-math.pi / 2.0, 4, "X")
    else:
        fail(f"no importer for {path}")
    check_space(to_file, listing, path)
    return to_file


def check_space(to_file, listing, path):
    lo, hi = world_bounds(mesh_objects())
    got_lo = Vector((math.inf,) * 3)
    got_hi = Vector((-math.inf,) * 3)
    for corner in ((x, y, z) for x in (lo.x, hi.x) for y in (lo.y, hi.y)
                   for z in (lo.z, hi.z)):
        point = to_file @ Vector(corner)
        got_lo = Vector(map(min, got_lo, point))
        got_hi = Vector(map(max, got_hi, point))
    want_lo = Vector(listing["bounds"][0])
    want_hi = Vector(listing["bounds"][1])
    tolerance = 0.01 * max((want_hi - want_lo)[:]) + 1e-5
    if max((got_lo - want_lo).length, (got_hi - want_hi).length) > tolerance:
        fail(f"Blender's import of {path} does not land in the renderer's "
             f"file space: Blender {[round(v, 3) for v in got_lo]} .. "
             f"{[round(v, 3) for v in got_hi]}, listing "
             f"{[round(v, 3) for v in want_lo]} .. "
             f"{[round(v, 3) for v in want_hi]}")


def node_path(ob):
    parts = []
    while ob is not None:
        parts.append(ob.name)
        ob = ob.parent
    return "/".join(reversed(parts))


def matching_objects(select):
    """The Blender objects a select pattern stands for, subtree included,
    with matches nested inside another match dropped, as the renderer's
    `ObjectSelection` does."""
    import fnmatch
    matched = []
    for ob in bpy.data.objects:
        candidate = node_path(ob) if "/" in select else ob.name
        if fnmatch.fnmatchcase(candidate, select):
            matched.append(ob)
    roots = [ob for ob in matched
             if not any(other is not ob and ob in other.children_recursive
                        for other in matched)]
    pieces = []
    for root in roots:
        for ob in [root] + list(root.children_recursive):
            if ob.type == "MESH" and ob not in pieces:
                pieces.append(ob)
    return pieces

#--}
#--{ Geometry

def joined_mesh(pieces, name, transform):
    """One mesh from many objects, every vertex taken through the object's
    world matrix and then `transform`, with the material slots of all the
    pieces merged by name so that faces can still be told apart by slot."""
    if len(pieces) == 1:
        # The usual case, and the one that can be millions of triangles:
        # a copy transformed in place costs no Python per vertex.
        ob = pieces[0]
        out = ob.data.copy()
        out.name = name
        out.transform(transform @ ob.matrix_world)
        for layer in list(out.uv_layers):
            out.uv_layers.remove(layer)
        for attribute in list(out.color_attributes):
            out.color_attributes.remove(attribute)
        materials = [slot.material.name if slot.material else ""
                     for slot in ob.material_slots] or [""]
        return out, materials
    materials = []
    vertices = []
    faces = []
    indices = []
    for ob in pieces:
        mesh = ob.data
        matrix = transform @ ob.matrix_world
        count = len(mesh.vertices)
        flat = [0.0] * (3 * count)
        mesh.vertices.foreach_get("co", flat)
        base = len(vertices)
        for i in range(count):
            vertices.append(matrix @ Vector(flat[3 * i:3 * i + 3]))
        remap = []
        for slot in ob.material_slots:
            slot_name = slot.material.name if slot.material else ""
            if slot_name not in materials:
                materials.append(slot_name)
            remap.append(materials.index(slot_name))
        if not remap:
            if "" not in materials:
                materials.append("")
            remap.append(materials.index(""))
        loops = [0] * len(mesh.loops)
        mesh.loops.foreach_get("vertex_index", loops)
        starts = [0] * len(mesh.polygons)
        totals = [0] * len(mesh.polygons)
        slots = [0] * len(mesh.polygons)
        mesh.polygons.foreach_get("loop_start", starts)
        mesh.polygons.foreach_get("loop_total", totals)
        mesh.polygons.foreach_get("material_index", slots)
        for start, total, slot in zip(starts, totals, slots):
            faces.append([base + loops[j]
                          for j in range(start, start + total)])
            indices.append(remap[min(slot, len(remap) - 1)])
    out = bpy.data.meshes.new(name)
    out.from_pydata([v[:] for v in vertices], [], faces)
    out.polygons.foreach_set("material_index", indices)
    out.validate()
    out.update()
    return out, materials


def triangle_count(mesh):
    totals = [0] * len(mesh.polygons)
    mesh.polygons.foreach_get("loop_total", totals)
    return sum(totals) - 2 * len(totals)


def split_faces(mesh, keep):
    """A copy of `mesh` holding only the faces `keep(material_index)`
    accepts."""
    bm = bmesh.new()
    bm.from_mesh(mesh)
    drop = [face for face in bm.faces if not keep(face.material_index)]
    bmesh.ops.delete(bm, geom=drop, context="FACES")
    out = bpy.data.meshes.new(mesh.name + ".part")
    bm.to_mesh(out)
    bm.free()
    out.update()
    return out


def thinned(mesh, fraction, seed=0):
    """The mesh with a deterministic `fraction` of its faces kept."""
    rng = random.Random(seed)
    bm = bmesh.new()
    bm.from_mesh(mesh)
    drop = [face for face in bm.faces if rng.random() >= fraction]
    bmesh.ops.delete(bm, geom=drop, context="FACES")
    out = bpy.data.meshes.new(mesh.name + ".thin")
    bm.to_mesh(out)
    bm.free()
    out.update()
    return out


def decimated(mesh, target=None, planar_angle=None):
    """The mesh collapsed to about `target` triangles, or with its near
    coplanar faces dissolved up to `planar_angle` radians.

    Collapse is right for a solid: a stone or a statue keeps its shape
    down to a few thousand triangles. It is wrong for a tree, where the
    branches are thousands of thin separate tubes: collapse stalls far
    above the target and leaves the tubes as spikes, while a planar
    dissolve keeps every branch as a branch.
    """
    count = triangle_count(mesh)
    if target is not None and count <= target:
        return mesh
    ob = bpy.data.objects.new("decimate", mesh)
    bpy.context.scene.collection.objects.link(ob)
    modifier = ob.modifiers.new("Decimate", "DECIMATE")
    if planar_angle is not None:
        modifier.decimate_type = "DISSOLVE"
        modifier.angle_limit = planar_angle
    else:
        modifier.decimate_type = "COLLAPSE"
        modifier.ratio = target / count
        modifier.use_collapse_triangulate = True
    depsgraph = bpy.context.evaluated_depsgraph_get()
    out = bpy.data.meshes.new_from_object(ob.evaluated_get(depsgraph))
    out.name = mesh.name + ".dec"
    bpy.data.objects.remove(ob)
    bpy.data.meshes.remove(mesh)
    out.update()
    return out


def concatenated(meshes, name):
    vertices = []
    faces = []
    for mesh in meshes:
        base = len(vertices)
        count = len(mesh.vertices)
        flat = [0.0] * (3 * count)
        mesh.vertices.foreach_get("co", flat)
        vertices += [tuple(flat[3 * i:3 * i + 3]) for i in range(count)]
        loops = [0] * len(mesh.loops)
        mesh.loops.foreach_get("vertex_index", loops)
        for poly in mesh.polygons:
            start, total = poly.loop_start, poly.loop_total
            faces.append([base + loops[j]
                          for j in range(start, start + total)])
    out = bpy.data.meshes.new(name)
    out.from_pydata(vertices, [], faces)
    out.validate()
    out.update()
    return out


def reduced(mesh, materials, leaf_slots, keep_leaf, target, body_angle):
    """The proxy geometry, at about `target` triangles for a solid.

    An asset with card slots (leaves, twigs, snow: faces that are many,
    small, and disconnected) is a tree: its cards are thinned by dropping
    faces at random, which keeps every surviving face exactly where it
    was, and its body is planar-dissolved rather than collapsed (see
    `decimated()`), so the body lands where the branching lets it and the
    cards get half the target, capped at `keep_leaf` of their faces. An
    asset without card slots is collapsed to the target.
    """
    leaf = {i for i, slot_name in enumerate(materials)
            if slot_name.split(".")[0].lower() in leaf_slots}
    if not leaf:
        return decimated(mesh, target=target)
    body = split_faces(mesh, lambda index: index not in leaf)
    cards = split_faces(mesh, lambda index: index in leaf)
    bpy.data.meshes.remove(mesh)
    body_count = triangle_count(body)
    card_count = triangle_count(cards)
    if card_count:
        fraction = min(keep_leaf, (target // 2) / card_count)
        if fraction < 1.0:
            thin = thinned(cards, fraction)
            bpy.data.meshes.remove(cards)
            cards = thin
        body = decimated(body, planar_angle=body_angle)
    else:
        body = decimated(body, target=target)
    print(f"reduce: body {body_count} -> {triangle_count(body)}, cards "
          f"{card_count} -> {triangle_count(cards)}")
    if not len(cards.polygons):
        bpy.data.meshes.remove(cards)
        return body
    out = concatenated([body, cards], "proxy")
    bpy.data.meshes.remove(body)
    bpy.data.meshes.remove(cards)
    return out

#--}
#--{ Assets

def make_asset(mesh, display_name, manifest_path, select, thumb_path,
               catalog_id, tags, description):
    # No Blender materials, by policy: shading is the layout's business,
    # and a proxy copied from the import would otherwise drag the file's
    # materials and image references into every layout that links it.
    mesh.materials.clear()
    ob = bpy.data.objects.new(display_name, mesh)
    mesh.name = display_name
    collection = bpy.data.collections.new(display_name)
    collection.objects.link(ob)
    bpy.context.scene.collection.children.link(collection)
    # On the collection for the Asset Browser's drag, and on the object
    # for the exporter's instance lookup; the add-on reads both.
    for holder in (collection, ob):
        holder[ASSET_KEY] = manifest_path
        holder[SELECT_KEY] = select
    collection.asset_mark()
    data = collection.asset_data
    if catalog_id:
        data.catalog_id = catalog_id
    data.author = "prepare_asset.py"
    data.description = description
    for tag in tags:
        data.tags.new(tag)
    if thumb_path and os.path.isfile(thumb_path):
        image = bpy.data.images.load(thumb_path)
        preview = collection.preview_ensure()
        preview.image_size = tuple(image.size)
        preview.image_pixels_float = image.pixels[:]
        bpy.data.images.remove(image)
    return collection


def main():
    args = parse_args()
    manifest_path = os.path.abspath(args.manifest)
    asset = prepare.load_manifest_module().Asset(manifest_path)
    with open(args.listing) as stream:
        listing = json.load(stream)
    directory = os.path.dirname(manifest_path)
    stem = os.path.splitext(os.path.basename(manifest_path))[0]
    leaf_slots = {name.strip().lower() for name in args.leaf_slots.split(",")
                  if name.strip()}
    tags = [tag for tag in args.tags.split(",") if tag]
    if not asset.proxy:
        fail(f"{manifest_path} names no proxy")

    bpy.ops.wm.read_homefile(use_empty=True)
    render_path = os.path.join(directory, asset.render)
    to_file = import_render(render_path, listing)
    imported = list(bpy.data.objects)

    correction = Matrix.Scale(asset.scale, 4)
    if asset.up == "y":
        correction = correction @ Matrix.Rotation(math.pi / 2.0, 4, "X")

    entries = asset.objects or [None]
    multi = len(asset.objects) > 1
    made = []
    counts = []
    for entry in entries:
        select = entry["select"] if entry else ""
        if entry:
            pieces = matching_objects(select)
            if not pieces:
                fail(f"no imported object matches select {select!r}; "
                     f"Blender named them "
                     f"{[node_path(ob) for ob in imported][:12]}")
            pivot = Vector(entry["pivot"])
        else:
            pieces = [ob for ob in imported if ob.type == "MESH"]
            pivot = Vector((0.0, 0.0, 0.0))
        transform = correction @ Matrix.Translation(-pivot) @ to_file
        mesh, materials = joined_mesh(pieces, "joined", transform)
        source_count = triangle_count(mesh)
        mesh = reduced(mesh, materials, leaf_slots, args.keep_leaf,
                       args.target_triangles, math.radians(args.body_angle))
        display = asset.name if not multi else f"{asset.name} {select}"
        thumb = os.path.join(directory, prepare.thumbnail_name(stem, select,
                                                               multi))
        description = (f"{source_count} triangles from {asset.render}"
                       + (f", select {select}" if select else ""))
        made.append(make_asset(mesh, display, manifest_path, select, thumb,
                               args.catalog_id, tags, description))
        counts.append(triangle_count(mesh))

    keep = {ob for collection in made for ob in collection.objects}
    for ob in list(bpy.data.objects):
        if ob not in keep:
            bpy.data.objects.remove(ob)
    while bpy.data.orphans_purge(do_recursive=True):
        pass

    proxy_path = asset.proxy_path
    os.makedirs(os.path.dirname(proxy_path), exist_ok=True)
    # No '.blend1' beside a file that is rebuilt from its source anyway.
    bpy.context.preferences.filepaths.save_version = 0
    bpy.ops.wm.save_as_mainfile(filepath=proxy_path, compress=True,
                                relative_remap=True)
    print(f"proxy: {os.path.relpath(proxy_path, directory)} "
          f"objects={len(made)} triangles={','.join(map(str, counts))} "
          f"size={os.path.getsize(proxy_path) // 1024}KB")

#--}


if __name__ == "__main__":
    main()
