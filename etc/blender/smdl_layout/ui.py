"""The panels, and the export, check, and asset-library operators."""

import os

import bpy
from bpy_extras.io_utils import ExportHelper

from .exporter import (asset_tag, seed_slots, slot_names, tag_of,
                       write_scene)
from .preview import SMDL_OT_render_preview


class SMDL_OT_sync_slots(bpy.types.Operator):
    """Seed or refresh this asset's per-slot material rows from its
    manifest"""

    bl_idname = "smdl.sync_slots"
    bl_label = "Sync Material Slots"
    bl_options = {"REGISTER", "UNDO"}

    @classmethod
    def poll(cls, context):
        ob = context.object
        return ob is not None and asset_tag(ob) is not None

    def execute(self, context):
        from . import manifest as manifest_module
        ob = context.object
        added = seed_slots(ob)
        rows = ob.smdl_asset_options.slots
        stale = sum(1 for row in rows if row.stale)
        if rows:
            self.report({"INFO"}, f"{len(rows)} slot(s) on {ob.name}"
                                  + (f", {added} added" if added else "")
                                  + (f", {stale} stale" if stale else ""))
            return {"FINISHED"}
        # No rows seeded: each way that happens deserves its own words,
        # since "cannot be read" once stood in for all of them and sent a
        # perfectly readable manifest to the wrong suspect.
        manifest_path, select = asset_tag(ob)
        try:
            asset = manifest_module.Asset(manifest_path)
        except (OSError, ValueError) as error:
            self.report({"WARNING"},
                        f"cannot read the manifest for {ob.name}: {error}")
            return {"FINISHED"}
        names = slot_names((manifest_path, select))
        if len(names) == 1:
            self.report({"INFO"},
                        f"{ob.name} has one material slot, which the "
                        f"Material Override and Variants fields cover")
        elif select and asset.object_for(select) is None:
            self.report({"WARNING"},
                        f"the manifest for {ob.name} does not list select "
                        f"{select!r}: the asset was re-prepared with "
                        f"different names, so re-import it")
        else:
            self.report({"WARNING"},
                        f"the manifest for {ob.name} records no material "
                        f"names: re-run etc/scripts/prepare_asset.py init "
                        f"--force on {os.path.dirname(manifest_path)}")
        return {"FINISHED"}


class SMDL_OT_sync_grids(bpy.types.Operator):
    """Seed or refresh this volume's grid rows from the volume itself"""

    bl_idname = "smdl.sync_grids"
    bl_label = "Sync Grids"
    bl_options = {"REGISTER", "UNDO"}

    @classmethod
    def poll(cls, context):
        from . import volume as volume_module
        ob = context.object
        return ob is not None and volume_module.is_volume(ob)

    def execute(self, context):
        from . import volume as volume_module
        ob = context.object
        # From the evaluated object: a volume built by geometry nodes has
        # its grids nowhere else, and a fluid domain reports an empty
        # field until it is evaluated.
        names = volume_module.grid_names(
            ob, context.evaluated_depsgraph_get())
        rows = ob.smdl_volume_options.grids
        known = {row.name for row in rows}
        # One grid is on by default, the density where there is one:
        # the others are fields a material asks for by name, and writing
        # them unasked fills the layout directory with files nobody
        # reads. Never leave every row off, or the panel would say
        # nothing is exported while an empty selection exports all of
        # them.
        default = "density" if "density" in names else (names[0] if names
                                                        else "")
        for name in names:
            if name not in known:
                row = rows.add()
                row.name = name
                row.export = name == default
        if not names:
            self.report({"WARNING"},
                        f"{ob.name} offers no grid; a fluid domain has to be "
                        f"baked first, and a volume has to hold one")
            return {"FINISHED"}
        self.report({"INFO"}, f"{len(rows)} grid(s) on {ob.name}")
        return {"FINISHED"}


class SMDL_OT_export_scene(bpy.types.Operator, ExportHelper):
    """Write this scene as a '.layout'"""

    bl_idname = "smdl.export_scene"
    bl_label = "Export Layout"

    filename_ext = ".layout"
    filter_glob: bpy.props.StringProperty(default="*.layout",
                                          options={"HIDDEN"})
    bake_untagged: bpy.props.BoolProperty(
        name="Bake Untagged Geometry",
        description="Write meshes that are not prepared assets to a sidecar "
                    "beside the layout, so that a ground plane modeled here "
                    "still renders",
        default=True)
    places_threshold: bpy.props.IntProperty(
        name="Binary Places Threshold",
        description="Placement lists at or above this length are written as "
                    "binary '.places' sidecars beside the layout instead of "
                    "text, which is what heavy scatter wants. Zero keeps "
                    "everything as text",
        default=64, min=0, soft_max=4096)

    collection: bpy.props.StringProperty(
        name="Collection",
        description="Export only this collection, which is what Blender's "
                    "per-collection exporters ask for. Empty exports the "
                    "whole scene",
        default="",
        options={"HIDDEN"})

    def execute(self, context):
        # Blender invokes a collection exporter with the collection in
        # context, so the same operator serves both the File menu and the
        # per-collection list without either knowing about the other.
        collection = bpy.data.collections.get(self.collection)
        if collection is None and getattr(self, "is_collection_export", False):
            collection = context.collection
        report = write_scene(context, self.filepath,
                             context.scene.smdl_asset_root,
                             self.bake_untagged, collection,
                             self.places_threshold)
        for problem in report["problems"]:
            self.report({"WARNING"}, problem)
        summary = (f"{report['placements']} placement(s) of "
                   f"{report['assets']} asset(s)")
        if report["groups"]:
            summary += f", {report['groups']} group(s)"
        if report["grooms"]:
            summary += f", {report['grooms']} groom(s)"
        if report["volumes"]:
            summary += f", {report['volumes']} volume(s)"
        if report["sidecars"]:
            summary += f", {len(report['sidecars'])} sidecar(s)"
        summary += (f", {report['baked']} baked, "
                    f"{len(report['materials'])} material name(s)")
        self.report({"INFO"}, summary)
        return {"FINISHED"}


class SMDL_FH_scene(bpy.types.FileHandler):
    """Lets a collection carry its own export settings, so re-exporting is
    one click rather than a trip through the File menu."""

    bl_idname = "SMDL_FH_scene"
    bl_label = "SpectralMDL Layout"
    bl_export_operator = "smdl.export_scene"
    bl_file_extensions = ".layout"

    @classmethod
    def poll_drop(cls, context):
        return context.area and context.area.type == "VIEW_3D"


class SMDL_OT_register_library(bpy.types.Operator):
    """Add the asset root to Blender's asset libraries, so that prepared
    assets appear in the Asset Browser"""

    bl_idname = "smdl.register_library"
    bl_label = "Register Asset Library"

    def execute(self, context):
        root = bpy.path.abspath(context.scene.smdl_asset_root)
        if not root or not os.path.isdir(root):
            self.report({"ERROR"}, "set the asset root to a directory first")
            return {"CANCELLED"}
        root = os.path.abspath(root)
        libraries = context.preferences.filepaths.asset_libraries
        for library in libraries:
            if os.path.abspath(bpy.path.abspath(library.path)) == root:
                self.report({"INFO"}, f"{library.name} already points there")
                return {"FINISHED"}
        bpy.ops.preferences.asset_library_add(directory=root)
        libraries[-1].name = os.path.basename(root) or "SpectralMDL Assets"
        self.report({"INFO"}, f"added {libraries[-1].name}; assets appear in "
                              f"the Asset Browser")
        return {"FINISHED"}


class SMDL_OT_check_scene(bpy.types.Operator):
    """Report what the renderer will not be able to resolve, without
    writing anything"""

    bl_idname = "smdl.check_scene"
    bl_label = "Check Layout"

    def execute(self, context):
        # Written to a scratch path and thrown away: the checks are the
        # export's own, so running them any other way would let the two
        # disagree. A reachable renderer then reparses the result, which
        # covers the reader as well as the writer.
        import subprocess
        import tempfile
        from .preview import resolve_renderer
        problems = []
        with tempfile.TemporaryDirectory() as directory:
            check_path = os.path.join(directory, "check.layout")
            report = write_scene(context, check_path,
                                 context.scene.smdl_asset_root, bake=False)
            problems = list(report["problems"])
            renderer = resolve_renderer(context.scene.smdl_toy_path)
            if renderer:
                command = [renderer, check_path, "-list-objects"]
                root = bpy.path.abspath(context.scene.smdl_asset_root).strip()
                if root:
                    command += ["-asset-dir", root]
                run = subprocess.run(command, capture_output=True, text=True)
                if run.returncode != 0:
                    tail = [line for line in run.stderr.splitlines()
                            if line.strip()]
                    problems.append("the renderer rejects the export: " +
                                    (tail[-1] if tail else "no message"))
        for problem in problems:
            self.report({"WARNING"}, problem)
        if not problems:
            self.report({"INFO"},
                        f"{report['placements']} placement(s) of "
                        f"{report['assets']} asset(s), nothing unresolved")
        return {"FINISHED"}


class SMDL_PT_layout(bpy.types.Panel):
    """The primary panel. Everything it offers lives in its child panels,
    so it draws nothing of its own."""

    bl_label = "SpectralMDL Layout"
    bl_space_type = "PROPERTIES"
    bl_region_type = "WINDOW"
    bl_context = "scene"

    def draw(self, context):
        pass


class SMDL_PT_import(bpy.types.Panel):
    bl_label = "Import"
    bl_space_type = "PROPERTIES"
    bl_region_type = "WINDOW"
    bl_context = "scene"
    bl_parent_id = "SMDL_PT_layout"

    def draw(self, context):
        layout = self.layout
        layout.prop(context.scene, "smdl_asset_root")
        layout.operator("smdl.register_library", icon="ASSET_MANAGER")
        layout.operator("smdl.import_asset", icon="IMPORT")
        layout.operator("smdl.add_slope_mask", icon="MOD_DISPLACE")
        layout.operator("smdl.scatter_asset", icon="OUTLINER_OB_POINTCLOUD")
        layout.operator("smdl.add_cluster_scatter",
                        icon="OUTLINER_OB_FORCE_FIELD")


class SMDL_PT_export(bpy.types.Panel):
    bl_label = "Export"
    bl_space_type = "PROPERTIES"
    bl_region_type = "WINDOW"
    bl_context = "scene"
    bl_parent_id = "SMDL_PT_layout"

    def draw(self, context):
        layout = self.layout
        layout.operator("smdl.export_scene", icon="EXPORT")
        layout.operator("smdl.check_scene", icon="CHECKMARK")


class SMDL_PT_camera(bpy.types.Panel):
    """The camera settings the exporter writes into the layout's `camera {}`
    block. Dependent rows gray out rather than vanish, so the panel never
    jumps around as checkboxes toggle."""

    bl_label = "Camera Options"
    bl_space_type = "PROPERTIES"
    bl_region_type = "WINDOW"
    bl_context = "scene"
    bl_parent_id = "SMDL_PT_layout"
    bl_options = {"DEFAULT_CLOSED"}

    def draw(self, context):
        layout = self.layout
        layout.use_property_split = True
        layout.use_property_decorate = False
        settings = context.scene.smdl_render
        layout.prop(settings, "vignette")

        layout.prop(settings, "dof")
        column = layout.column()
        column.active = settings.dof
        column.prop(settings, "focus_object")
        row = column.row()
        row.active = settings.dof and settings.focus_object is None
        row.prop(settings, "focus_distance")
        column.prop(settings, "aperture_radius")
        column.prop(settings, "blades_enable")
        sub = column.column()
        sub.active = settings.dof and settings.blades_enable
        sub.prop(settings, "blades")
        sub.prop(settings, "blade_angle")
        column.prop(settings, "cat_eye_enable")
        sub = column.column()
        sub.active = settings.dof and settings.cat_eye_enable
        sub.prop(settings, "cat_eye")
        sub.prop(settings, "cat_eye_radius")

        layout.prop(settings, "distortion_enable")
        column = layout.column()
        column.active = settings.distortion_enable
        column.prop(settings, "distortion_k1")
        column.prop(settings, "distortion_k2")
        column.prop(settings, "distortion_fit")


class SMDL_PT_sky(bpy.types.Panel):
    """The atmosphere settings the exporter writes into the layout's `sky {}`
    block. The sun's position is not here on purpose: a sun lamp states it
    by pointing, which is the whole premise of laying out in Blender."""

    bl_label = "Sun-Sky Options"
    bl_space_type = "PROPERTIES"
    bl_region_type = "WINDOW"
    bl_context = "scene"
    bl_parent_id = "SMDL_PT_layout"
    bl_options = {"DEFAULT_CLOSED"}

    def draw(self, context):
        layout = self.layout
        layout.use_property_split = True
        layout.use_property_decorate = False
        settings = context.scene.smdl_render
        layout.prop(settings, "sky_scale")
        column = layout.column()
        column.active = settings.sky_scale > 0
        column.prop(settings, "visibility")
        column.prop(settings, "water_vapor")


class SMDL_PT_haze(bpy.types.Panel):
    """The exterior haze the exporter writes into the layout's `haze {}`
    block, which is what produces aerial perspective. Off writes nothing,
    the renderer's default being no haze; on writes the block, and inside
    it only what differs from the renderer's defaults."""

    bl_label = "Haze Options"
    bl_space_type = "PROPERTIES"
    bl_region_type = "WINDOW"
    bl_context = "scene"
    bl_parent_id = "SMDL_PT_layout"
    bl_options = {"DEFAULT_CLOSED"}

    def draw(self, context):
        layout = self.layout
        layout.use_property_split = True
        layout.use_property_decorate = False
        settings = context.scene.smdl_render
        layout.prop(settings, "haze")
        column = layout.column()
        column.active = settings.haze
        column.prop(settings, "haze_match_sky")
        row = column.row()
        row.active = settings.haze and not settings.haze_match_sky
        row.prop(settings, "haze_visibility")
        column.prop(settings, "haze_scale_height")
        column.prop(settings, "haze_base_height")
        column.prop(settings, "haze_droplet")


class SMDL_PT_asset_object(bpy.types.Panel):
    """Per-object export options, shown on placed assets and on whatever
    scatters them. See `SMDLAssetOptions`."""

    bl_label = "SpectralMDL Asset"
    bl_space_type = "PROPERTIES"
    bl_region_type = "WINDOW"
    bl_context = "object"

    @classmethod
    def poll(cls, context):
        ob = context.object
        if ob is None or ob.type == "CURVES":
            # A groom has its own panel, and its hair-asset node groups
            # would drag this one in for options nothing reads.
            return False
        # A placed asset carries the tag, except when it was dragged from
        # the Asset Browser, which leaves the tag inside the linked proxy
        # collection: `asset_tag()` reads it wherever it lives, so a
        # dragged asset and its retained scatter source get the panel too.
        # A surface that scatters carries no tag at all, but the options
        # of everything it emits can be read from it, so anything holding
        # geometry nodes also qualifies.
        return (tag_of(ob) is not None or asset_tag(ob) is not None
                or any(modifier.type == "NODES"
                       for modifier in ob.modifiers))

    def draw(self, context):
        layout = self.layout
        layout.use_property_split = True
        layout.use_property_decorate = False
        ob = context.object
        options = ob.smdl_asset_options
        layout.prop(options, "subdivide")
        column = layout.column()
        column.active = options.subdivide > 0
        column.prop(options, "scheme")
        column.prop(options, "linear")
        layout.prop(options, "displace")
        layout.separator()
        # What the manifold estimators search, and what they search for.
        marks = layout.column(align=True)
        marks.prop(options, "caster")
        marks.prop(options, "light")
        marks.prop(options, "caustic")
        layout.separator()
        # Any slot row with content switches the asset to per-slot
        # resolution, so the flat fields gray out rather than lie.
        per_slot = any(row.material.strip() or row.variants.strip()
                       for row in options.slots)
        legacy = layout.column()
        legacy.active = not per_slot
        legacy.prop(options, "material")
        legacy.prop(options, "variants")
        if options.slots:
            layout.separator()
        for row in options.slots:
            block = layout.column(align=True)
            block.label(text=row.name + (" (not in the manifest)"
                                         if row.stale else ""),
                        icon="ERROR" if row.stale else "MATERIAL")
            block.prop(row, "material")
            block.prop(row, "variants")
        # Always offered on a tagged asset: deciding whether the rows
        # disagree with the manifest would mean reading the manifest,
        # which a redraw must not do.
        if asset_tag(ob) is not None:
            layout.operator("smdl.sync_slots", icon="FILE_REFRESH")


class SMDL_PT_groom_object(bpy.types.Panel):
    """Per-groom export options, shown on hair Curves objects. See
    `SMDLGroomOptions`."""

    bl_label = "SpectralMDL Groom"
    bl_space_type = "PROPERTIES"
    bl_region_type = "WINDOW"
    bl_context = "object"

    @classmethod
    def poll(cls, context):
        return context.object is not None and context.object.type == "CURVES"

    def draw(self, context):
        layout = self.layout
        layout.use_property_split = True
        layout.use_property_decorate = False
        options = context.object.smdl_groom_options
        layout.prop(options, "mode")
        layout.prop(options, "radius_scale")
        layout.prop(options, "material")


class SMDL_PT_volume_object(bpy.types.Panel):
    """Per-volume export options, shown on Volume objects and on fluid
    domains. See `SMDLVolumeOptions`."""

    bl_label = "SpectralMDL Volume"
    bl_space_type = "PROPERTIES"
    bl_region_type = "WINDOW"
    bl_context = "object"

    @classmethod
    def poll(cls, context):
        from . import volume as volume_module
        ob = context.object
        return ob is not None and volume_module.is_volume(ob)

    def draw(self, context):
        from . import volume as volume_module
        layout = self.layout
        layout.use_property_split = True
        layout.use_property_decorate = False
        ob = context.object
        options = ob.smdl_volume_options
        layout.prop(options, "material")
        layout.separator()
        column = layout.column(align=True)
        column.use_property_split = False
        for row in options.grids:
            column.prop(row, "export", text=row.name)
        if not options.grids:
            column.label(text="Every grid is exported", icon="INFO")
        layout.operator("smdl.sync_grids", icon="FILE_REFRESH")
        settings = volume_module.fluid_domain_of(ob)
        if settings is not None and settings.use_adaptive_domain:
            box = layout.box()
            box.label(text="Adaptive Domain moves the grid without",
                      icon="ERROR")
            box.label(text="saying where to, so this domain cannot")
            box.label(text="export. Turn it off and re-bake.")


class SMDL_PT_light(bpy.types.Panel):
    """Per-lamp export options, shown on lamps. See `SMDLLightOptions`."""

    bl_label = "SpectralMDL Light"
    bl_space_type = "PROPERTIES"
    bl_region_type = "WINDOW"
    bl_context = "data"

    @classmethod
    def poll(cls, context):
        return context.object is not None and context.object.type == "LIGHT"

    def draw(self, context):
        layout = self.layout
        layout.use_property_split = True
        layout.use_property_decorate = False
        light = context.object.data
        # A sun states the sky's direction rather than declaring a
        # light, so it has nowhere for the mark to sit.
        exports = light.type in ("POINT", "SPOT", "AREA")
        column = layout.column()
        column.active = exports
        column.prop(light.smdl_light_options, "caustic")
        if not exports:
            layout.label(text=f"A {light.type.lower()} lamp exports no "
                              f"light declaration",
                         icon="INFO")


class SMDL_PT_materials(bpy.types.Panel):
    """The scene's SMDL source, written beside the layout on export."""

    bl_label = "Materials"
    bl_space_type = "PROPERTIES"
    bl_region_type = "WINDOW"
    bl_context = "scene"
    bl_parent_id = "SMDL_PT_layout"
    bl_options = {"DEFAULT_CLOSED"}

    def draw(self, context):
        layout = self.layout
        scene = context.scene
        layout.prop(scene, "smdl_material_text")
        row = layout.row(align=True)
        row.operator("smdl.new_material", icon="TEXT")
        row.operator("smdl.check_material", icon="CHECKMARK")
        row.operator("smdl.format_material", icon="ALIGN_LEFT")
        layout.prop(scene, "smdl_compiler_path")
        # The path is what a scene whose materials live outside Blender
        # renders with, and what the text block above overrides.
        layout.prop(scene, "smdl_material_file")


class SMDL_PT_preview(bpy.types.Panel):
    bl_label = "Preview"
    bl_space_type = "PROPERTIES"
    bl_region_type = "WINDOW"
    bl_context = "scene"
    bl_parent_id = "SMDL_PT_layout"
    bl_options = {"DEFAULT_CLOSED"}

    def draw(self, context):
        layout = self.layout
        layout.prop(context.scene, "smdl_toy_path")
        row = layout.row(align=True)
        row.prop(context.scene, "smdl_preview_spp")
        row.prop(context.scene, "smdl_preview_scale")
        row = layout.row(align=True)
        row.prop(context.scene, "smdl_preview_every")
        row.prop(context.scene, "smdl_preview_threads")
        # Exposure is a tone mapping option the layout cannot carry, so it
        # rides the preview's command line and belongs with the preview.
        layout.prop(context.scene.smdl_render, "exposure")
        if SMDL_OT_render_preview.running():
            layout.operator("smdl.cancel_preview", icon="CANCEL")
            status = context.window_manager.smdl_preview_status
            if status:
                layout.label(text=status, icon="TIME")
        else:
            layout.operator("smdl.render_preview", icon="RENDER_STILL")


def menu_import(self, context):
    self.layout.operator("smdl.import_asset",
                         text="SpectralMDL Asset (.asset)")


def menu_export(self, context):
    self.layout.operator("smdl.export_scene",
                         text="SpectralMDL Layout (.layout)")


# The child panels must register after their parent, and register in the
# order they should appear.
CLASSES = (SMDL_OT_export_scene, SMDL_FH_scene, SMDL_OT_check_scene,
           SMDL_OT_register_library, SMDL_OT_sync_slots, SMDL_OT_sync_grids,
           SMDL_PT_layout,
           SMDL_PT_import,
           SMDL_PT_export, SMDL_PT_camera, SMDL_PT_sky, SMDL_PT_haze,
           SMDL_PT_materials,
           SMDL_PT_preview, SMDL_PT_asset_object, SMDL_PT_groom_object,
           SMDL_PT_volume_object, SMDL_PT_light)
