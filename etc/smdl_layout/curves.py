"""Baking a Blender hair groom to a `.curves` sidecar.

The evaluated Curves datablock is the whole groom: the geometry-node
hair assets are applied, so sculpted guides arrive interpolated to
their full child density, radiused by their profile node, and deformed
onto the surface, all in the hair object's LOCAL space. The points
export verbatim under the Catmull-Rom basis, the window Blender and
Cycles evaluate hair with, and placement stays with the layout exactly
as it does for meshes.

Everything is read through `foreach_get` into numpy, because a groom
is bulk data (a hundred guides interpolate to a hundred thousand
points), and Blender's `curve_offset_data` is already the fence-post
offset table the format stores: when no strand is degenerate, the
export is a handful of bulk copies. See `CurvesFile.h` for the format.
"""

import struct

import numpy as np

CURVES_MAGIC = b"SMDLCRVS"
CATMULL_ROM = 2

# What a groom whose radius is missing or zero everywhere is written
# with, so it renders visibly instead of vanishing: the default root
# radius of Blender's own Set Hair Curve Profile node.
FALLBACK_RADIUS = 0.001


def bake(depsgraph, ob, filepath):
    """Bake the evaluated groom of the Curves object `ob` to `filepath`.

    Returns (strands, points, has_uvs, problems). Zero strands means
    nothing was written and the problems say why.
    """
    problems = []
    data = ob.evaluated_get(depsgraph).data
    num_curves = len(data.curves)
    num_points = len(data.points)
    if num_curves == 0 or num_points == 0:
        return 0, 0, False, [f"the groom {ob.name} is empty, so it was "
                             f"not exported"]

    offsets = np.empty(num_curves + 1, dtype=np.int32)
    data.curve_offset_data.foreach_get("value", offsets)
    positions = np.empty(num_points * 3, dtype=np.float32)
    data.attributes["position"].data.foreach_get("vector", positions)
    positions = positions.reshape(num_points, 3)

    radius = data.attributes.get("radius")
    if radius is not None and radius.domain == "POINT":
        radii = np.empty(num_points, dtype=np.float32)
        radius.data.foreach_get("value", radii)
    else:
        radii = None
    if radii is None or not radii.max() > 0.0:
        radii = np.full(num_points, FALLBACK_RADIUS, dtype=np.float32)
        problems.append(f"the groom {ob.name} has no radius, so every "
                        f"strand was written {FALLBACK_RADIUS * 1000:g} mm "
                        f"wide; a Set Hair Curve Profile node is how a "
                        f"groom states one")

    # Where each strand's root sits on the surface it grew from, which
    # is what lets one scalp texture drive per-strand color.
    uv = data.attributes.get("surface_uv_coordinate")
    if uv is not None and uv.domain == "CURVE":
        uvs = np.empty(num_curves * 2, dtype=np.float32)
        uv.data.foreach_get("vector", uvs)
        uvs = uvs.reshape(num_curves, 2)
    else:
        uvs = None
        problems.append(f"the groom {ob.name} carries no "
                        f"surface_uv_coordinate, so its strands have no "
                        f"root UVs for a surface texture to color")

    # Catmull-Rom needs 2 points per strand; shorter strands drop, and
    # with them the pass-through of Blender's own offset table.
    lengths = np.diff(offsets)
    keep = lengths >= 2
    if not keep.all():
        dropped = int(num_curves - int(keep.sum()))
        point_keep = np.repeat(keep, lengths)
        positions = positions[point_keep]
        radii = radii[point_keep]
        offsets = np.concatenate(([0], np.cumsum(lengths[keep])))
        if uvs is not None:
            uvs = uvs[keep]
        num_curves -= dropped
        num_points = len(radii)
        problems.append(f"{dropped} strand(s) of {ob.name} have fewer "
                        f"than 2 points and were dropped")
    if num_curves == 0:
        return 0, 0, False, [f"the groom {ob.name} holds no strands with "
                             f"2 or more points, so it was not exported"]

    block = np.empty((num_points, 4), dtype=np.float32)
    block[:, :3] = positions
    block[:, 3] = radii
    with open(filepath, "wb") as stream:
        stream.write(struct.pack("<8sHHHHIII", CURVES_MAGIC, 1, CATMULL_ROM,
                                 0 if uvs is None else 1, 0,
                                 num_curves, num_points, 0))
        stream.write(np.ascontiguousarray(offsets, dtype="<u4").tobytes())
        stream.write(np.ascontiguousarray(block, dtype="<f4").tobytes())
        if uvs is not None:
            stream.write(np.ascontiguousarray(uvs, dtype="<f4").tobytes())
    return num_curves, num_points, uvs is not None, problems
