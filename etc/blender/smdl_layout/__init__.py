"""SpectralMDL layout: arrange prepared assets in Blender, render them
with smdl-toy.

Blender is used for 3D layout and nothing else. No material is translated,
no renderer is driven, and nothing is exported but transforms and names: a
material is associated by its name alone, exactly as a hand-written layout
associates it.
"""

import shutil

import bpy

# Reload Scripts re-imports this module but leaves Python's cache of the
# submodules alone, so an edit to any of them takes effect only if it is
# reloaded here. Guarded on a name that exists only on a re-import.
if "importer" in locals():
    import importlib

    manifest = importlib.reload(manifest)  # noqa: F821
    curves = importlib.reload(curves)  # noqa: F821
    exporter = importlib.reload(exporter)  # noqa: F821
    material = importlib.reload(material)  # noqa: F821
    importer = importlib.reload(importer)  # noqa: F821
    preview = importlib.reload(preview)  # noqa: F821
    scatter = importlib.reload(scatter)  # noqa: F821
    ui = importlib.reload(ui)  # noqa: F821

from . import (curves, exporter, importer, manifest, material, preview,
               scatter, ui)


class SMDLSlotOptions(bpy.types.PropertyGroup):
    """One material slot of a multi-material asset: the mesh file's name
    for it, and what this placement does to it.

    Rows are seeded from the asset's manifest by the import, retire, and
    Sync Slots operators (a panel cannot create collection items while
    drawing), and any row carrying content switches the asset to per-slot
    resolution: see `SMDLAssetOptions`.
    """

    name: bpy.props.StringProperty(
        name="Slot",
        description="The mesh file's material name, as the asset manifest "
                    "records it")
    material: bpy.props.StringProperty(
        name="Override",
        description="Shade this slot with this MDL material instead of the "
                    "asset's own. Empty leaves the slot alone",
        default="")
    variants: bpy.props.StringProperty(
        name="Variants",
        description="MDL material names to spread over this slot across "
                    "instances, comma separated. Each slot randomizes "
                    "independently, and every combination in use becomes "
                    "its own instance batch, so lists multiply per slot: "
                    "keep them modest",
        default="")
    stale: bpy.props.BoolProperty(
        name="Stale",
        description="The manifest no longer names this slot; kept so typed "
                    "names are not lost, and skipped by the export",
        default=False)


class SMDLAssetOptions(bpy.types.PropertyGroup):
    """Per-object export options, written into the exported layout.

    Subdivision, displacement, and the caustic marks belong to the asset
    declaration in the layout grammar, so instances that disagree export
    as separate declarations. A material override is a per-place fact and
    exports as one, so heavy scatter still shares one mesh in the
    renderer, and a list of them spreads over the instances an object
    emits. A scattered
    instance reads all of these from its asset's retained source empty
    when one exists (see `scatter.retire_source()`), and from the emitting
    surface otherwise.

    A multi-material asset carries one `slots` row per mesh material, and
    any row with content switches the asset to per-slot resolution: each
    slot overrides or randomizes independently, and `material` and
    `variants` here are ignored. With no row content, behavior is exactly
    the single-material behavior described above.
    """

    subdivide: bpy.props.IntProperty(
        name="Subdivide",
        description="Uniform subdivision levels applied by the renderer at "
                    "load time (each level multiplies the face count by 4). "
                    "Zero is off",
        default=0, min=0, max=8)
    scheme: bpy.props.EnumProperty(
        name="Scheme",
        description="Which topological split refinement uses, decided by "
                    "how the mesh is authored rather than by the look "
                    "wanted",
        items=(("CATMARK", "Catmull-Clark",
                "Split polygons of any size into quads"),
               ("LOOP", "Loop",
                "Split triangles into triangles; the renderer triangulates "
                "the mesh for it")),
        default="CATMARK")
    linear: bpy.props.BoolProperty(
        name="Linear",
        description="Refine without smoothing: raise the vertex density "
                    "without moving the surface, which is what a "
                    "displacement map authored against the mesh as modeled "
                    "wants",
        default=False)
    displace: bpy.props.BoolProperty(
        name="Displace",
        description="Apply the material's geometry.displacement to the "
                    "refined vertices at load time",
        default=False)
    caster: bpy.props.BoolProperty(
        name="Caustic Caster",
        description="Let the renderer's manifold estimators search this "
                    "surface for specular and glossy connections to the "
                    "lights, rather than leaving that transport to the "
                    "path tracer. Scene judgment rather than a material "
                    "fact: the same chrome is worth it on a mirror wall "
                    "and is noise on a thousand screws",
        default=False)
    caustic: bpy.props.BoolProperty(
        name="Caustic Emitter",
        description="Search the caustic casters for connections to this "
                    "asset's emission. Meaningful only on an emissive "
                    "asset, and only once something in the scene carries "
                    "the mark: with none anywhere every light and the sky "
                    "is searched, and with any, only the marked ones are",
        default=False)
    material: bpy.props.StringProperty(
        name="Material Override",
        description="Shade this placement with this MDL material instead "
                    "of the asset's own. Empty keeps the asset's material",
        default="")
    variants: bpy.props.StringProperty(
        name="Material Variants",
        description="MDL material names to spread over what this object "
                    "instances, comma separated, so that a scatter is not "
                    "one material repeated. Each instance keeps its name "
                    "across exports. Empty leaves the override alone",
        default="")
    slots: bpy.props.CollectionProperty(
        type=SMDLSlotOptions,
        name="Material Slots",
        description="Per-slot overrides and variants for a multi-material "
                    "asset, seeded from its manifest")


class SMDLGroomOptions(bpy.types.PropertyGroup):
    """Per-groom export options, written into the groom's asset declaration.

    Only non-defaults are written, so an untouched panel exports nothing.
    The geometry itself (points, radii, root UVs) comes from the evaluated
    hair and has no options: what the groom looks like is authored in
    Blender, and these are the renderer-side facts a layout states about
    it.
    """

    mode: bpy.props.EnumProperty(
        name="Cross Section",
        description="The surface the renderer sweeps around the stored "
                    "strand centers",
        items=(("TUBE", "Tube",
                "A swept surface: real geometry that holds up in closeups"),
               ("RIBBON", "Ribbon",
                "Camera-facing flat curves: the fast mode for dense, "
                "distant fibers")),
        default="TUBE")
    radius_scale: bpy.props.FloatProperty(
        name="Radius Scale",
        description="Uniform multiplier on every stored radius, applied by "
                    "the renderer at load time. Width alone is what a groom "
                    "most often needs adjusted, and no placement transform "
                    "can express it",
        default=1.0, min=0.001, soft_max=10.0)
    material: bpy.props.StringProperty(
        name="Material",
        description="The MDL material shading this groom. Empty reads the "
                    "first material slot's name, and failing that the "
                    "object's own name",
        default="")


class SMDLLightOptions(bpy.types.PropertyGroup):
    """Per-lamp export options, written into the exported layout's `light`
    declaration.

    On the lamp data rather than on the object, since every other fact the
    export takes from a lamp (power, color, temperature, cone angle, IES
    path) is read from there too, so lamps sharing a data-block agree about
    this the way they already agree about the rest.
    """

    caustic: bpy.props.BoolProperty(
        name="Caustic Emitter",
        description="Search the caustic casters for connections to this "
                    "lamp. It restricts anything only once something in "
                    "the scene carries the mark: with none anywhere every "
                    "light and the sky is searched, and with any, only the "
                    "marked ones are",
        default=False)


class SMDLRenderSettings(bpy.types.PropertyGroup):
    """Per-scene render settings.

    Everything here except Exposure is written into the exported layout's
    `camera {}`, `sky {}`, and `haze {}` blocks, and only when it differs
    from the renderer's default, so an untouched panel exports nothing.
    Exposure is a tonemapping option the layout does not carry, so it alone
    rides the preview command line as `-exposure`.
    """

    exposure: bpy.props.FloatProperty(
        name="Exposure",
        description="Linear brightness multiplier applied before tone "
                    "mapping. Passed to smdl-toy as -exposure rather than "
                    "written into the layout",
        default=1.0, min=0.0, soft_max=16.0)
    vignette: bpy.props.FloatProperty(
        name="Vignette",
        description="Strength of the cos^4 illumination falloff toward "
                    "the frame corners (0 is off, 1 is the physical law)",
        default=0.0, min=0.0, max=1.0)
    dof: bpy.props.BoolProperty(
        name="Depth of Field",
        description="Render with a finite aperture, so that only the "
                    "focus distance is sharp. This panel is the whole of "
                    "DOF; the camera's own Depth of Field settings are "
                    "deliberately not read",
        default=False)
    focus_object: bpy.props.PointerProperty(
        name="Focus Object", type=bpy.types.Object,
        description="Keep this object in focus. The focus distance is "
                    "measured from the camera to its origin along the "
                    "view axis at export, overriding Focus Distance")
    focus_distance: bpy.props.FloatProperty(
        name="Focus Distance",
        description="Distance along the view axis that is in focus. Zero "
                    "focuses at the point the camera looks at",
        default=0.0, min=0.0, subtype="DISTANCE")
    aperture_radius: bpy.props.FloatProperty(
        name="Aperture Radius [mm]",
        description="Radius of the lens opening in millimeters (the "
                    "layout receives meters). Larger is shallower "
                    "depth of field; 5 mm is a 50 mm lens at f/5",
        default=5.0, min=0.0, soft_max=25.0)
    blades_enable: bpy.props.BoolProperty(
        name="Aperture Blades",
        description="Give the aperture a polygonal shape, which shapes "
                    "out-of-focus highlights. Off is a round lens",
        default=False)
    blades: bpy.props.IntProperty(
        name="Blades",
        description="Number of aperture blades. Real lenses have 5 to 11",
        default=5, min=3, soft_max=16)
    blade_angle: bpy.props.FloatProperty(
        name="Offset Angle [degrees]",
        description="Rotation of the aperture polygon (0 puts a vertex "
                    "at screen right)",
        default=30.0)
    cat_eye_enable: bpy.props.BoolProperty(
        name="Cat Eye",
        description="Mechanical vignette from the lens barrel, which "
                    "clips out-of-focus highlights into cat's-eye shapes "
                    "toward the corners. Needs a finite aperture",
        default=False)
    cat_eye: bpy.props.FloatProperty(
        name="Strength",
        description="Relative rim displacement at the frame corner in "
                    "units of rim radius (0.5 costs about 1.35 stops in "
                    "the corners, 1 is fully dark)",
        default=0.5, min=0.0, max=1.0)
    cat_eye_radius: bpy.props.FloatProperty(
        name="Radius Factor",
        description="Barrel rim radius as a multiple of the aperture "
                    "radius, so 1 means wide open (the layout "
                    "receives meters). Fixing the rim and stopping the "
                    "aperture down weakens the cat's eye, as a real lens "
                    "does",
        default=1.0, min=0.01, soft_max=4.0)
    distortion_enable: bpy.props.BoolProperty(
        name="Lens Distortion",
        description="Apply radial lens distortion to the projection",
        default=False)
    distortion_k1: bpy.props.FloatProperty(
        name="Distortion k1",
        description="Quadratic radial distortion in units of relative "
                    "corner displacement: 0.1 pushes the corners out by "
                    "10 percent (barrel), negative pulls them in "
                    "(pincushion)",
        default=0.0, soft_min=-1.0, soft_max=1.0)
    distortion_k2: bpy.props.FloatProperty(
        name="Distortion k2",
        description="Quartic radial distortion in the same corner-"
                    "fraction units. Real lenses need this to pull the "
                    "corners back without over-bending the middle of the "
                    "frame",
        default=0.0, soft_min=-1.0, soft_max=1.0)
    distortion_fit: bpy.props.BoolProperty(
        name="Refit",
        description="Refit the projection so the frame corner directions "
                    "hold constant under distortion; a convenience for "
                    "comparing renders, not a physical effect",
        default=False)
    sky_scale: bpy.props.FloatProperty(
        name="Brightness Factor",
        description="Scale factor on the sky radiance (or on the "
                    "environment image when one is used). Zero disables "
                    "the sky entirely",
        default=1.0, min=0.0, soft_max=10.0)
    visibility: bpy.props.FloatProperty(
        name="Visibility [km]",
        description="Aerosol visibility in kilometers; lower is hazier",
        default=23.0, min=5.0, max=100.0)
    water_vapor: bpy.props.FloatProperty(
        name="Water Vapor",
        description="Water-vapor column scale factor; higher deepens the "
                    "near-infrared absorption bands",
        default=1.0, min=0.3, max=3.0)
    haze: bpy.props.BoolProperty(
        name="Haze",
        description="Fill the space outside all geometry with the exterior "
                    "haze that produces aerial perspective: one aerosol "
                    "whose extinction falls off exponentially with height. "
                    "Exported as the layout's haze block",
        default=False)
    haze_match_sky: bpy.props.BoolProperty(
        name="Match Sky Visibility",
        description="Take the haze's visibility from the sun-sky's, so "
                    "distant ground reads neither hazier nor clearer than "
                    "the horizon sky behind it",
        default=True)
    haze_visibility: bpy.props.FloatProperty(
        name="Visibility [km]",
        description="The haze's meteorological range at 550 nm, measured "
                    "at the base height; lower is hazier",
        default=23.0, min=0.5, soft_max=100.0)
    haze_scale_height: bpy.props.FloatProperty(
        name="Scale Height [m]",
        description="The height over which the extinction falls by a "
                    "factor of e. 2100 m is MODTRAN's rural boundary "
                    "layer in clear air; hazier air is mixed to a "
                    "shallower slab that no scale height fits well",
        default=2100.0, min=1.0, soft_max=10000.0)
    haze_base_height: bpy.props.FloatProperty(
        name="Base Height",
        description="The height at which the extinction is the one the "
                    "visibility names",
        default=0.0, subtype="DISTANCE")
    haze_droplet: bpy.props.FloatProperty(
        name="Droplet Size [um]",
        description="The water droplet diameter in micrometers that "
                    "shapes the phase function: 0.29 is the fit to "
                    "MODTRAN's rural aerosol, fog and cloud droplets run "
                    "from 5 to 50, and larger scatters more sharply "
                    "forward",
        default=0.29, min=0.01, max=50.0)


CLASSES = ((SMDLSlotOptions, SMDLAssetOptions, SMDLGroomOptions,
            SMDLLightOptions, SMDLRenderSettings) +
           importer.CLASSES +
           material.CLASSES + preview.CLASSES + scatter.CLASSES + ui.CLASSES)


def register():
    bpy.types.Scene.smdl_asset_root = bpy.props.StringProperty(
        name="Asset Root",
        description="The directory prepared assets live under. Paths in "
                    "the exported layout are written relative to it, which "
                    "is what makes the layout portable: the renderer "
                    "resolves them with -asset-dir",
        subtype="DIR_PATH")
    # Defaulted from PATH so the field shows what will actually run. A
    # default is display rather than data, so nothing machine-specific is
    # saved into the .blend.
    bpy.types.Scene.smdl_toy_path = bpy.props.StringProperty(
        name="smdl-toy",
        description="The renderer to preview with. Pre-filled when "
                    "smdl-toy is found on PATH; empty also falls back to "
                    "PATH at render time",
        default=shutil.which("smdl-toy") or "",
        subtype="FILE_PATH")
    bpy.types.Scene.smdl_material_file = bpy.props.StringProperty(
        name="Materials",
        description="The MDL or SMDL file naming this scene's materials. "
                    "Empty renders everything with the renderer's built-in "
                    "20 percent Lambertian, which is what a layout wants "
                    "before its materials are written",
        subtype="FILE_PATH")
    bpy.types.Scene.smdl_compiler_path = bpy.props.StringProperty(
        name="smdl",
        description="The compiler that checks and formats the material "
                    "text. Empty looks beside smdl-toy and then on PATH",
        default=shutil.which("smdl") or "",
        subtype="FILE_PATH")
    bpy.types.Scene.smdl_preview_spp = bpy.props.IntProperty(
        name="Preview Samples", description="Samples per pixel for the "
        "preview render", default=16, min=1, soft_max=256)
    bpy.types.Scene.smdl_preview_every = bpy.props.FloatProperty(
        name="Update Every",
        description="Seconds between the renderer rewriting the preview "
                    "image, so the viewport shows it converging. Zero waits "
                    "for the finished render",
        default=1.0, min=0.0, soft_max=30.0, subtype="TIME_ABSOLUTE",
        unit="TIME")
    bpy.types.Scene.smdl_preview_scale = bpy.props.IntProperty(
        name="Preview Scale", description="Percentage of the render "
        "resolution to preview at", default=50, min=1, max=100,
        subtype="PERCENTAGE")
    bpy.types.Scene.smdl_preview_threads = bpy.props.IntProperty(
        name="Threads",
        description="Threads the preview render may use. Zero, the "
                    "default, uses every hardware thread, which is fastest "
                    "but leaves Blender competing with the renderer for the "
                    "machine; a smaller number keeps cores free to keep "
                    "working in",
        default=0, min=0, soft_max=64)
    # Runtime only: what the render is doing belongs to the session, not
    # to the .blend.
    bpy.types.WindowManager.smdl_preview_status = bpy.props.StringProperty(
        name="Preview Status")
    for cls in CLASSES:
        bpy.utils.register_class(cls)
    bpy.types.Scene.smdl_render = bpy.props.PointerProperty(
        type=SMDLRenderSettings)
    # A text datablock rather than a path: the source is edited in Blender
    # and written out by the export, so the .blend is where it lives.
    bpy.types.Scene.smdl_material_text = bpy.props.PointerProperty(
        type=bpy.types.Text, name="Material Text",
        description="The text block holding this scene's SMDL materials, "
                    "written beside the layout when it is exported")
    bpy.types.Object.smdl_asset_options = bpy.props.PointerProperty(
        type=SMDLAssetOptions)
    bpy.types.Object.smdl_groom_options = bpy.props.PointerProperty(
        type=SMDLGroomOptions)
    bpy.types.Light.smdl_light_options = bpy.props.PointerProperty(
        type=SMDLLightOptions)
    bpy.types.TOPBAR_MT_file_import.append(ui.menu_import)
    bpy.types.TOPBAR_MT_file_export.append(ui.menu_export)


def unregister():
    bpy.types.TOPBAR_MT_file_export.remove(ui.menu_export)
    bpy.types.TOPBAR_MT_file_import.remove(ui.menu_import)
    del bpy.types.Light.smdl_light_options
    del bpy.types.Object.smdl_groom_options
    del bpy.types.Object.smdl_asset_options
    del bpy.types.Scene.smdl_material_text
    del bpy.types.Scene.smdl_render
    for cls in reversed(CLASSES):
        bpy.utils.unregister_class(cls)
    del bpy.types.WindowManager.smdl_preview_status
    del bpy.types.Scene.smdl_preview_threads
    del bpy.types.Scene.smdl_preview_scale
    del bpy.types.Scene.smdl_preview_every
    del bpy.types.Scene.smdl_preview_spp
    del bpy.types.Scene.smdl_material_file
    del bpy.types.Scene.smdl_compiler_path
    del bpy.types.Scene.smdl_toy_path
    del bpy.types.Scene.smdl_asset_root
