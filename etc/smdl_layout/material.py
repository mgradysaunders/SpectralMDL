"""Authoring the layout's materials in Blender's Text editor.

A layout names materials and never describes them, so a scene is half a
simulation until the `.smdl` those names resolve in exists. Holding that
source in a text datablock keeps the whole simulation in one file without
conceding anything: the buffer is SMDL source, the export writes it beside
the layout, and the renderer compiles it. Nothing here reads a shader
graph or translates a Blender material.

Checking and formatting are `smdl`'s, not this module's: `smdl list`
compiles and says where it failed, and `smdl format` is the same
formatter the builtin modules go through.
"""

import os
import shutil
import subprocess
import tempfile

import bpy

# What a new material file starts as: the renderer's own default material,
# which is a complete module and shades anything the layout has not named
# yet.
TEMPLATE = """#smdl
import ::df::*;

// The layout resolves the material names it was written with here. Assign
// one to an asset in the SpectralMDL Asset panel.

export material default_material() = material(
  surface: material_surface(
    scattering: df::diffuse_reflection_bsdf(tint: color(0.2))));
"""

def resolve_compiler(context):
    """The smdl to run: what was configured, whatever sits beside the
    configured renderer, or whatever is on PATH."""
    scene = context.scene
    configured = bpy.path.abspath(scene.smdl_compiler_path).strip()
    if configured:
        return configured if os.path.exists(configured) else ""
    renderer = bpy.path.abspath(scene.smdl_toy_path).strip()
    if renderer:
        beside = os.path.join(os.path.dirname(renderer), "smdl")
        if os.path.exists(beside):
            return beside
    return shutil.which("smdl") or ""


def material_text(context):
    """The text datablock holding this scene's materials, or None."""
    return context.scene.smdl_material_text


def run_compiler(context, subcommand, source, arguments=()):
    """Run one smdl subcommand over `source`, in a temporary directory.

    Returns (output, path, problem). A module's qualified name comes from
    its file stem, so the temporary file keeps the buffer's stem: what the
    diagnostics name is what the Text editor shows.
    """
    compiler = resolve_compiler(context)
    if not compiler:
        return "", "", ("cannot find smdl; set its path in the SpectralMDL "
                        "Layout panel")
    directory = tempfile.mkdtemp(prefix="smdl-material-")
    path = os.path.join(directory, "material.smdl")
    with open(path, "w") as stream:
        stream.write(source)
    try:
        run = subprocess.run([compiler, subcommand, *arguments, path],
                             capture_output=True, text=True)
    except OSError as error:
        shutil.rmtree(directory, ignore_errors=True)
        return "", "", f"cannot run {compiler}: {error}"
    return run.stderr + run.stdout, path, ""


def diagnostics(output, path):
    """The `[error]` and `[warn]` lines of an smdl run, as (level, line,
    message), with the temporary path taken back out of them."""
    found = []
    stem = os.path.basename(path)
    for line in output.splitlines():
        line = line.strip()
        for level in ("[error]", "[warn]"):
            if not line.startswith(level):
                continue
            body = line[len(level):].strip()
            number = 0
            if body.startswith("["):
                location, _, rest = body[1:].partition("]")
                where, _, digits = location.rpartition(":")
                if os.path.basename(where) == stem:
                    number = int(digits) if digits.isdigit() else 0
                    body = rest.strip()
            found.append((level.strip("[]"), number, body))
            break
    return found


class SMDL_OT_new_material(bpy.types.Operator):
    """Start a material file for this layout in the Text editor"""

    bl_idname = "smdl.new_material"
    bl_label = "New Material File"
    bl_options = {"REGISTER", "UNDO"}

    name: bpy.props.StringProperty(
        name="Name", description="What to call the text block. The exported "
                                 "file is named after the layout, so this is "
                                 "a label",
        default="materials.smdl")

    def invoke(self, context, event):
        return context.window_manager.invoke_props_dialog(self)

    def execute(self, context):
        name = self.name.strip() or "materials.smdl"
        text = bpy.data.texts.new(name)
        text.from_string(TEMPLATE)
        context.scene.smdl_material_text = text
        for window in context.window_manager.windows:
            for area in window.screen.areas:
                if area.type == "TEXT_EDITOR":
                    area.spaces.active.text = text
                    area.tag_redraw()
        self.report({"INFO"}, f"{text.name} is this scene's material file")
        return {"FINISHED"}


class SMDL_OT_check_material(bpy.types.Operator):
    """Compile this scene's material file and report what it says"""

    bl_idname = "smdl.check_material"
    bl_label = "Check Materials"

    @classmethod
    def poll(cls, context):
        return material_text(context) is not None

    def execute(self, context):
        text = material_text(context)
        output, path, problem = run_compiler(context, "list",
                                             text.as_string())
        if problem:
            self.report({"ERROR"}, problem)
            return {"CANCELLED"}
        found = diagnostics(output, path)
        # Reported as warnings even when they are errors, the way Check
        # Layout reports its own: the check ran, and what it found is the
        # source's problem rather than the operator's failure.
        for level, number, message in found:
            where = f"line {number}: " if number else ""
            self.report({"WARNING"}, where + message)
        errors = [entry for entry in found if entry[0] == "error"]
        if errors and errors[0][1]:
            # The first error is where the reading stops, so put the cursor
            # on it: a diagnostic that names a line and leaves you to find
            # it is half a diagnostic.
            text.cursor_set(errors[0][1] - 1)
            text.select_set(errors[0][1] - 1, 0, errors[0][1] - 1, -1)
        if errors:
            self.report({"WARNING"}, f"{text.name} does not compile: "
                                     f"{len(errors)} error(s)")
        else:
            materials = [line.split("'")[1] for line in output.splitlines()
                         if "New material '" in line and "'" in line]
            self.report({"INFO"},
                        f"{text.name} compiles: "
                        + (", ".join(materials) if materials
                           else "no materials in it yet"))
        return {"FINISHED"}


class SMDL_OT_format_material(bpy.types.Operator):
    """Format this scene's material file with smdl"""

    bl_idname = "smdl.format_material"
    bl_label = "Format Materials"
    bl_options = {"REGISTER", "UNDO"}

    @classmethod
    def poll(cls, context):
        return material_text(context) is not None

    def execute(self, context):
        text = material_text(context)
        source = text.as_string()
        output, path, problem = run_compiler(context, "format", source,
                                             arguments=("-i",))
        if problem:
            self.report({"ERROR"}, problem)
            return {"CANCELLED"}
        errors = [entry for entry in diagnostics(output, path)
                  if entry[0] == "error"]
        if errors:
            # Formatting parses, so a file that does not parse comes back
            # unchanged rather than mangled.
            self.report({"ERROR"}, f"line {errors[0][1]}: {errors[0][2]}"
                        if errors[0][1] else errors[0][2])
            return {"CANCELLED"}
        try:
            with open(path) as stream:
                formatted = stream.read()
        except OSError as error:
            self.report({"ERROR"}, f"cannot read the formatted source: "
                                   f"{error}")
            return {"CANCELLED"}
        if formatted == source:
            self.report({"INFO"}, f"{text.name} was already formatted")
            return {"FINISHED"}
        text.clear()
        text.write(formatted)
        self.report({"INFO"}, f"formatted {text.name}")
        return {"FINISHED"}


CLASSES = (SMDL_OT_new_material, SMDL_OT_check_material,
           SMDL_OT_format_material)
