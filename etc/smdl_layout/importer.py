"""Bringing a prepared asset into a layout.

The proxy is *linked* rather than appended, so the layout file holds a
reference to the asset's own `.blend` instead of a copy of it: re-running
the prepare tool then updates every layout that uses the asset, and the
provenance the proxy carries cannot drift from the asset it names.
"""

import math
import os

import bpy
from bpy_extras.io_utils import ImportHelper
from mathutils import Vector

from . import manifest as manifest_module
from .exporter import ASSET_KEY, SELECT_KEY, seed_slots


def link_proxy_collections(proxy_path, wanted=None):
    """Link the asset collections of a proxy `.blend`, in manifest order.

    Everything the proxy holds is linked, because everything it holds is an
    asset collection: the proxy was built from this manifest and holds one
    collection per object of it, so `wanted` decides only the order.
    Matching is by the `smdl_select` each collection carries, never by its
    name: the property is the provenance, and the name is a label chosen
    for whatever reads best in the Asset Browser.

    Note that `libraries.load` replaces the contents of the list assigned to
    `target.collections` with the datablocks it linked, in place. So the
    list of names becomes the list of collections on the way out of the
    `with`, and looking them up again by name afterward would find nothing.
    """
    linked = []
    with bpy.data.libraries.load(proxy_path, link=True) as (source, target):
        linked = list(source.collections)
        target.collections = linked
    collections = [item for item in linked if item is not None]
    if wanted:
        position = {select: index for index, select in enumerate(wanted)}
        collections.sort(key=lambda c: position.get(
            str(c.get(SELECT_KEY, c.name)), len(position)))
    return collections


def instance(collection, location, asset, select):
    """An empty instancing `collection`, tagged with what it stands for.

    The tag is duplicated onto the instance because that is what the
    exporter reads: a linked collection's own properties come along with
    the link, but an instance the user has moved is the thing being placed.
    """
    empty = bpy.data.objects.new(collection.name, None)
    empty.instance_type = "COLLECTION"
    empty.instance_collection = collection
    empty.location = location
    empty.empty_display_size = 0.25
    empty[ASSET_KEY] = asset.path
    empty[SELECT_KEY] = select
    bpy.context.scene.collection.objects.link(empty)
    seed_slots(empty)
    return empty


def import_asset(manifest_path, location=(0.0, 0.0, 0.0), spread=True):
    """Place every object of an asset, or the asset whole if it offers no
    objects to choose between.

    Returns the instances created, and a message if there is nothing to
    place.
    """
    asset = manifest_module.Asset(manifest_path)
    if not asset.proxy:
        return [], (f"{asset.name} has no proxy; run etc/prepare_asset.py on "
                    f"{asset.directory}")
    proxy_path = asset.proxy_path
    if not os.path.exists(proxy_path):
        return [], f"the proxy {proxy_path} does not exist"

    wanted = [entry["select"] for entry in asset.objects] or None
    collections = link_proxy_collections(proxy_path, wanted)
    if not collections:
        return [], f"the proxy {proxy_path} holds no asset collections"

    # Several objects of one asset arrive side by side rather than stacked,
    # since the point of importing them together is to see what is on offer.
    instances = []
    offset = 0.0
    for collection in collections:
        select = collection.get(SELECT_KEY, collection.name)
        position = (location[0] + offset, location[1], location[2])
        instances.append(instance(collection, position, asset, str(select)))
        if spread and len(collections) > 1:
            offset += collection_width(collection) * 1.2
    return instances, ""


def collection_width(collection):
    """How much room a collection takes along X, for laying several out in
    a row."""
    lo, hi = math.inf, -math.inf
    for ob in collection.objects:
        for corner in ob.bound_box:
            x = (ob.matrix_world @ Vector(corner)).x
            lo, hi = min(lo, x), max(hi, x)
    return max(hi - lo, 0.1) if lo <= hi else 1.0


class SMDL_OT_import_asset(bpy.types.Operator, ImportHelper):
    """Link a prepared asset's proxy and place it"""

    bl_idname = "smdl.import_asset"
    bl_label = "Import Asset"
    bl_options = {"REGISTER", "UNDO"}

    filename_ext = ".asset"
    filter_glob: bpy.props.StringProperty(default="*.asset",
                                          options={"HIDDEN"})
    filepath: bpy.props.StringProperty(subtype="FILE_PATH",
                                       options={"SKIP_SAVE"})
    spread: bpy.props.BoolProperty(
        name="Spread Objects",
        description="Place an asset's objects side by side rather than on "
                    "top of one another",
        default=True)

    def execute(self, context):
        path = self.filepath
        if os.path.isdir(path):
            path = manifest_module.find_in_directory(path)
            if not path:
                self.report({"ERROR"}, "that directory holds no single "
                                       "'.asset' manifest")
                return {"CANCELLED"}
        instances, message = import_asset(
            path, tuple(context.scene.cursor.location), self.spread)
        if message:
            self.report({"ERROR"}, message)
            return {"CANCELLED"}
        self.report({"INFO"}, f"placed {len(instances)} object(s)")
        return {"FINISHED"}


class SMDL_FH_asset(bpy.types.FileHandler):
    """Dropping a '.asset' into the viewport imports it."""

    bl_idname = "SMDL_FH_asset"
    bl_label = "SpectralMDL Asset"
    bl_import_operator = "smdl.import_asset"
    bl_file_extensions = ".asset"

    @classmethod
    def poll_drop(cls, context):
        return context.area and context.area.type == "VIEW_3D"


CLASSES = (SMDL_OT_import_asset, SMDL_FH_asset)
