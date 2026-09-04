"""Baking a Blender volume to a voxel file beside the layout.

Two sources, one output. A Volume object holds OpenVDB grids however they
arrived (imported from a `.vdb`, built by geometry nodes, a mesh converted
to one), and the evaluated volume spills to a temporary `.vdb` that the
OpenVDB module Blender bundles reads back losslessly, tiles expanded and
sparsity resolved. A Mantaflow gas domain holds no such grids at all: it
evaluates to a mesh and keeps its fields in `FluidDomainSettings`, which
hands them over as flat float arrays.

Both end in the same place: a dense `float32` array written as a Mitsuba
`.vol`, which `smdl volume` then converts to a sparse `.nvdb` when the
compiler is configured. Every grid of one object is written over one
shared box, so a material that reads two of them needs only one mapping
from `state::position()`.

That box is the voxel CORNER box, half a voxel outside the first and last
voxel centers, because the renderer spans texture space over the extent
with the values at voxel centers. It is returned in world units, since
object space in the renderer is world-scaled, and as a size and a center
rather than two corners, because the container it describes is a `box`
primitive centered on its own origin.
"""

import os
import shutil
import struct
import subprocess
import tempfile

import numpy as np

# The `FluidDomainSettings` field behind each grid name a gas domain can
# export. Only the single-channel ones: a material reads scalar fields,
# and velocity and color are not that.
FLUID_GRIDS = {
    "density": "density_grid",
    "flame": "flame_grid",
    "temperature": "temperature_grid",
    "heat": "heat_grid",
}


def import_openvdb():
    """Blender's bundled OpenVDB Python module, or None.

    It ships in Blender's own site-packages, named `openvdb` since 4.4 and
    `pyopenvdb` before that. A distribution that strips it leaves neither.
    """
    try:
        import openvdb
        return openvdb
    except ImportError:
        pass
    try:
        import pyopenvdb
        return pyopenvdb
    except ImportError:
        return None


def fluid_domain_of(ob):
    """The gas domain settings of `ob`, or None if it is not one."""
    if ob.type != "MESH":
        return None
    for modifier in ob.modifiers:
        if modifier.type != "FLUID":
            continue
        settings = getattr(modifier, "domain_settings", None)
        if (modifier.fluid_type == "DOMAIN" and settings is not None
                and settings.domain_type == "GAS"):
            return settings
    return None


def is_volume(ob):
    """Does this object export as a volume rather than as geometry?"""
    return ob.type == "VOLUME" or fluid_domain_of(ob) is not None


def grid_names(ob, depsgraph=None):
    """The grid names `ob` offers, for the panel to seed its rows with.

    Read from the EVALUATED object when a depsgraph is given, because a
    volume built by geometry nodes exists nowhere else: the original's
    own grids are whatever file it names, and empty when it names none.
    A gas domain's fields are fixed, and the ones it has not baked are
    empty.
    """
    if depsgraph is not None:
        ob = ob.evaluated_get(depsgraph)
    settings = fluid_domain_of(ob)
    if settings is not None:
        return [name for name, field in FLUID_GRIDS.items()
                if len(getattr(settings, field, ())) > 0]
    if ob.type != "VOLUME":
        return []
    try:
        ob.data.grids.load()
    except (AttributeError, RuntimeError):
        return []
    return [grid.name for grid in ob.data.grids]


def _axis_aligned(matrix):
    """Is this index-to-object matrix a scale and a translation alone?

    Anything else would put the grid's axes at an angle to the box that
    bounds it, which is not a thing the renderer's density hint can say.
    """
    scale = max(abs(matrix[i][i]) for i in range(3))
    for i in range(3):
        for j in range(3):
            if i != j and abs(matrix[i][j]) > 1e-6 * max(scale, 1e-9):
                return False
    return True


def _read_volume(depsgraph, ob, wanted):
    """The dense fields of a Volume object, over one shared index box.

    Returns (fields, box_lo, box_hi, problems), where `fields` maps a
    grid name to its values indexed (z, y, x) and the box is in the
    object's own local space.
    """
    problems = []
    openvdb = import_openvdb()
    if openvdb is None:
        return {}, None, None, [
            f"Blender's OpenVDB module is missing, so the volume {ob.name} "
            f"could not be read; a stock Blender build ships it"]
    data = ob.evaluated_get(depsgraph).data
    matrices = {grid.name: [list(row) for row in grid.matrix_object]
                for grid in data.grids}
    directory = tempfile.mkdtemp(prefix="smdl-volume-")
    path = os.path.join(directory, "spill.vdb")
    try:
        if not data.grids.save(path):
            return {}, None, None, [
                f"the volume {ob.name} could not be written out for reading"]
        # One index box for every grid, so that the object needs one
        # container and its material one mapping, whatever each grid's
        # own active region happens to be.
        grids = {}
        index_lo = None
        index_hi = None
        reference = None
        for name in wanted:
            if name not in matrices:
                problems.append(f"the volume {ob.name} has no grid named "
                                f"{name!r}, so it was skipped")
                continue
            matrix = matrices[name]
            if not _axis_aligned(matrix):
                problems.append(f"the grid {name!r} of {ob.name} is rotated "
                                f"or sheared in the object, which no bound "
                                f"box can describe, so it was skipped")
                continue
            if reference is None:
                reference = matrix
            elif any(abs(matrix[i][j] - reference[i][j]) > 1e-6
                     for i in range(3) for j in (0, 1, 2, 3)):
                problems.append(f"the grid {name!r} of {ob.name} does not "
                                f"share the voxel grid of the first one "
                                f"exported, so it was skipped")
                continue
            try:
                grid = openvdb.read(path, name)
            except Exception as error:
                problems.append(f"cannot read the grid {name!r} of "
                                f"{ob.name}: {error}")
                continue
            bound = grid.evalActiveVoxelBoundingBox()
            lo = np.array(bound[0], dtype=np.int64)
            hi = np.array(bound[1], dtype=np.int64)
            # An empty grid reports an inverted box, which is a shape
            # nothing downstream can be handed.
            if np.any(hi < lo):
                problems.append(f"the grid {name!r} of {ob.name} has no "
                                f"active voxels, so it was skipped")
                continue
            index_lo = lo if index_lo is None else np.minimum(index_lo, lo)
            index_hi = hi if index_hi is None else np.maximum(index_hi, hi)
            grids[name] = grid
        if not grids:
            return {}, None, None, problems
        dims = tuple(int(d) for d in (index_hi - index_lo + 1))
        fields = {}
        for name, grid in grids.items():
            # `copyToArray()` fills an array indexed [x, y, z]; the file
            # format is x-fastest, which is the transpose of that.
            values = np.zeros(dims, dtype=np.float32)
            values.fill(grid.background)
            grid.copyToArray(values, ijk=tuple(int(v) for v in index_lo))
            fields[name] = values.transpose(2, 1, 0)
        # The corner box, from the index-to-object matrix of the grids.
        scale = np.array([reference[i][i] for i in range(3)])
        offset = np.array([reference[i][3] for i in range(3)])
        corner0 = scale * (index_lo - 0.5) + offset
        corner1 = scale * (index_hi + 0.5) + offset
        return (fields, np.minimum(corner0, corner1),
                np.maximum(corner0, corner1), problems)
    finally:
        shutil.rmtree(directory, ignore_errors=True)


def _read_fluid(depsgraph, ob, settings, wanted):
    """The dense fields of a Mantaflow gas domain, and its local box.

    The fields live on the EVALUATED domain; the original reports an
    empty grid and a zero resolution.
    """
    problems = []
    if settings.use_adaptive_domain:
        return {}, None, None, [
            f"the fluid domain {ob.name} has Adaptive Domain on, which "
            f"moves the grid without saying where to, so it was skipped; "
            f"turn it off and re-bake"]
    evaluated = fluid_domain_of(ob.evaluated_get(depsgraph))
    if evaluated is None:
        return {}, None, None, [f"the fluid domain {ob.name} lost its "
                                f"modifier when evaluated"]
    resolution = tuple(evaluated.domain_resolution)
    if min(resolution) <= 0:
        return {}, None, None, [f"the fluid domain {ob.name} has not been "
                                f"baked, so it holds nothing to export"]
    base = resolution[0] * resolution[1] * resolution[2]
    fields = {}
    for name in wanted:
        field = FLUID_GRIDS.get(name)
        source = getattr(evaluated, field, None) if field else None
        if source is None or len(source) == 0:
            problems.append(f"the fluid domain {ob.name} has no {name!r} "
                            f"field baked, so it was skipped")
            continue
        # Noise raises the resolution of the density field alone, so the
        # length is what says which grid this is; the box is the same
        # either way.
        shape = resolution
        if len(source) != base:
            noise = evaluated.noise_scale
            scaled = tuple(r * noise for r in resolution)
            if len(source) == scaled[0] * scaled[1] * scaled[2]:
                shape = scaled
            else:
                problems.append(f"the {name!r} field of {ob.name} holds "
                                f"{len(source)} values, which is neither its "
                                f"resolution nor its noise multiple, so it "
                                f"was skipped")
                continue
        values = np.empty(len(source), dtype=np.float32)
        source.foreach_get(values)
        fields[name] = values.reshape(shape[2], shape[1], shape[0])
    if not fields:
        return {}, None, None, problems
    # The domain's own local box, which is what `start_point` and
    # `cell_size` describe: both are in the object's local space, and
    # `cell_size` is a vector, one per axis.
    box_lo = np.array(evaluated.start_point)
    box_hi = box_lo + np.array(evaluated.cell_size) * np.array(resolution)
    return fields, box_lo, box_hi, problems


def write_vol(path, values, box_lo, box_hi):
    """Write a Mitsuba `.vol`: the 48-byte header of magic, version 3,
    the float32 encoding, the extent, the channel count, and the box,
    then the values x-fastest. `values` is indexed (z, y, x)."""
    extent = (values.shape[2], values.shape[1], values.shape[0])
    with open(path, "wb") as stream:
        stream.write(struct.pack("<3sBiiiiiffffff", b"VOL", 3, 1, *extent, 1,
                                 *(float(v) for v in box_lo),
                                 *(float(v) for v in box_hi)))
        stream.write(np.ascontiguousarray(values, dtype="<f4").tobytes())


def convert_to_nvdb(compiler, sources, names, target):
    """Run `smdl volume` over the written `.vol` files, writing one
    NanoVDB file that carries them all as named grids.

    Returns the problem, or empty on success. NanoVDB is what is worth
    keeping on disk: it stores only the occupied leaves, and it is the
    one of the two formats that can hold more than one grid.
    """
    arguments = [compiler, "volume", *sources,
                 *(f"-grid={name}" for name in names), f"-output={target}"]
    try:
        run = subprocess.run(arguments, capture_output=True, text=True)
    except OSError as error:
        return f"cannot run {compiler}: {error}"
    if run.returncode != 0:
        message = (run.stderr or run.stdout).strip().splitlines()
        return (f"smdl volume failed: "
                f"{message[0] if message else 'no output'}")
    return ""


class Baked:
    """What one volume baked to: the files, what is in them, and the box
    they are addressed by.

    `size` and `center` are in world units, in the object's rigid frame,
    which is the space the renderer shades a placement in. The container
    is `box { size <size> translate <center> }`, so the material's bound
    box is exactly plus and minus half the size.
    """

    def __init__(self):
        self.files = {}
        self.selectors = {}
        self.extents = {}
        self.max_values = {}
        self.size = np.ones(3)
        self.center = np.zeros(3)

    @property
    def names(self):
        """The grids written, in the order they were written."""
        return list(self.files.keys())

    @property
    def sidecars(self):
        """The files written, each once however many grids it carries."""
        found = []
        for name in self.files.values():
            if name not in found:
                found.append(name)
        return found


def bake(depsgraph, ob, wanted, directory, stem, compiler=""):
    """Bake the wanted grids of `ob` into `directory`, named after `stem`.

    Returns (baked, problems). A `baked` with no sidecars means nothing
    was written and the problems say why.
    """
    baked = Baked()
    settings = fluid_domain_of(ob)
    if settings is not None:
        fields, box_lo, box_hi, problems = _read_fluid(depsgraph, ob,
                                                       settings, wanted)
    elif ob.type == "VOLUME":
        fields, box_lo, box_hi, problems = _read_volume(depsgraph, ob, wanted)
    else:
        return baked, [f"{ob.name} is not a volume"]
    if not fields:
        return baked, problems
    if not all(box_hi[i] > box_lo[i] for i in range(3)):
        return baked, problems + [f"the volume {ob.name} has an empty bound "
                                  f"box, so it was not exported"]

    # Object space in the renderer carries world units, so the box the
    # material declares is the local one scaled by the placement's own
    # scale; the placement itself is then written rigid. A mirrored
    # object would scale it negative, which is neither a size a `box`
    # accepts nor an orientation one can hold, so the box is taken
    # unmirrored and the flip is reported.
    scale = np.array(ob.matrix_world.to_scale())
    if np.any(scale < 0.0):
        problems.append(f"the volume {ob.name} is mirrored, which a bound "
                        f"box cannot follow, so its grid was written "
                        f"unmirrored")
    baked.size = (box_hi - box_lo) * np.abs(scale)
    baked.center = 0.5 * (box_lo + box_hi) * scale

    written = []
    for name, values in fields.items():
        path = os.path.join(directory, f"{stem}.{name}.vol")
        try:
            write_vol(path, values, -0.5 * baked.size, 0.5 * baked.size)
        except OSError as error:
            problems.append(f"cannot write {os.path.basename(path)}: {error}")
            continue
        written.append((name, path))
        baked.extents[name] = (values.shape[2], values.shape[1],
                               values.shape[0])
        baked.max_values[name] = float(values.max())

    if not written:
        return baked, problems
    # One NanoVDB file for the object when the compiler can make it, and
    # the dense intermediates go away with it.
    if compiler:
        target = os.path.join(directory, f"{stem}.nvdb")
        problem = convert_to_nvdb(compiler, [path for _, path in written],
                                  [name for name, _ in written], target)
        if problem:
            problems.append(f"{problem}; the volume {ob.name} kept its "
                            f"'.vol' file(s) instead")
        else:
            for _, path in written:
                try:
                    os.remove(path)
                except OSError:
                    pass
            for name, _ in written:
                baked.files[name] = os.path.basename(target)
                # A NanoVDB file names its grids, so the material picks
                # the one it wants; a Mitsuba volume holds one anonymous
                # grid per file and takes no selector.
                baked.selectors[name] = name
            return baked, problems
    for name, path in written:
        baked.files[name] = os.path.basename(path)
        baked.selectors[name] = ""
    return baked, problems
