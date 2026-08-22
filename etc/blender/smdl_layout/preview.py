"""Rendering the layout with smdl-toy, without leaving Blender.

Nothing here interprets the scene: it exports the layout the Export
operator would write and hands it to the renderer, so what the preview
shows is what the layout says. The render runs as a separate process
watched by a modal timer rather than blocking, so Blender stays usable
while it works and Escape can stop it.
"""

import os
import shutil
import subprocess
import tempfile

import bpy

from .exporter import write_scene

# What the finished preview is loaded into, reused across runs so that an
# Image Editor showing it keeps showing it.
IMAGE_NAME = "SpectralMDL Preview"

# One scratch directory per Blender session, so every preview renders to
# the SAME file path and the image datablock only ever reloads in place. A
# path that varied per run would force the datablock to be removed and
# recreated, which can leave an Image Editor pointing at a dead one.
_session_directory = None


def session_directory():
    global _session_directory
    if _session_directory is None or not os.path.isdir(_session_directory):
        _session_directory = tempfile.mkdtemp(prefix="smdl-preview-")
    return _session_directory


def resolve_renderer(path):
    """The smdl-toy to run: what was configured, or whatever is on PATH."""
    path = bpy.path.abspath(path).strip() if path else ""
    if path:
        return path if os.path.exists(path) else ""
    return shutil.which("smdl-toy") or ""


def build_command(context, renderer, scene_path, output_path, material="",
                  progress_path=""):
    scene = context.scene
    command = [renderer, scene_path]
    # Whatever the export just wrote from the Text editor, and failing that
    # whatever file the scene was pointed at.
    material = material or bpy.path.abspath(scene.smdl_material_file).strip()
    if material:
        command.append(material)
    root = bpy.path.abspath(scene.smdl_asset_root).strip()
    if root:
        command += ["-asset-dir", root]
    render = scene.render
    fraction = max(scene.smdl_preview_scale, 1) / 100.0
    width = max(int(render.resolution_x * fraction), 1)
    height = max(int(render.resolution_y * fraction), 1)
    # The layout carries a camera, resolution included, so this overrides
    # only the size and the framing stays exactly what the export wrote.
    command += ["-dims", f"{width},{height}",
                "-spp", str(max(scene.smdl_preview_spp, 1)),
                "-output", output_path]
    # The renderer rewrites the image as it converges and writes its
    # progress where this can read it: the bar on its stderr is drawn for
    # a person at a terminal and deliberately draws nothing into a pipe.
    if scene.smdl_preview_every > 0.0:
        command += ["-preview-every", f"{scene.smdl_preview_every:.9g}"]
    if progress_path:
        command += ["-progress-file", progress_path]
    # Zero is the renderer's own default, so it is left off the command
    # line entirely rather than spelled out as '-threads 0'.
    if scene.smdl_preview_threads > 0:
        command += ["-threads", str(scene.smdl_preview_threads)]
    # The one render setting the layout cannot carry: exposure is a
    # tonemapping option, applied to the image rather than the scene.
    exposure = scene.smdl_render.exposure
    if exposure != 1.0:
        command += ["-exposure", f"{exposure:.9g}"]
    return command


def read_progress(path):
    """The renderer's progress line as something to show a person, or
    empty. A missing or half-written file just reads as nothing."""
    try:
        with open(path) as stream:
            line = stream.read().strip()
    except OSError:
        return ""
    fields = {}
    note = ""
    if " note=" in line:
        line, _, note = line.partition(" note=")
    for entry in line.split():
        key, _, value = entry.partition("=")
        fields[key] = value
    try:
        done, total = float(fields["done"]), float(fields["total"])
        eta = float(fields.get("eta", -1.0))
    except (KeyError, ValueError):
        return ""
    if total <= 0:
        return ""
    status = f"{100.0 * done / total:.0f}%"
    if eta >= 0.0:
        # Shown in the same coarse steps the renderer's own bar uses: the
        # estimate is smoothed, and a number that still moves by a second
        # at a time reads as noise whatever it is doing.
        if eta < 20.0:
            status += f", {eta:.0f}s left"
        elif eta < 120.0:
            status += f", {5 * round(eta / 5):.0f}s left"
        else:
            status += f", {eta / 60.0:.0f} min left"
    if note:
        status += f" ({note})"
    return status


def show_image(path):
    """Load the finished render, and put it in an Image Editor if one is
    open."""
    image = bpy.data.images.get(IMAGE_NAME)
    if image is not None and image.filepath != path:
        bpy.data.images.remove(image)
        image = None
    if image is None:
        image = bpy.data.images.load(path)
        image.name = IMAGE_NAME
    else:
        image.reload()
    for window in bpy.context.window_manager.windows:
        for area in window.screen.areas:
            if area.type == "IMAGE_EDITOR":
                area.spaces.active.image = image
                area.tag_redraw()
    return image


class SMDL_OT_render_preview(bpy.types.Operator):
    """Render this layout with smdl-toy and show the result"""

    bl_idname = "smdl.render_preview"
    bl_label = "Render Preview"

    _process = None
    _timer = None
    _log_path = None
    _output = None
    _progress_path = None
    # The checkpoint already shown, so that the image reloads when the
    # renderer replaces it and not on every tick.
    _shown = None

    # One preview at a time: a second modal operator over the same output
    # file would race the first, and a stuck renderer would silently
    # stack them.
    _running = False

    # Set by the Cancel operator, which cannot reach the modal instance
    # itself; the next timer tick reads it and stops.
    _cancelled = False

    @classmethod
    def running(cls):
        """Is a preview rendering? What the panel draws its button from."""
        return cls._running

    @classmethod
    def cancel_requested(cls):
        cls._cancelled = True

    def execute(self, context):
        if SMDL_OT_render_preview._running:
            self.report({"WARNING"}, "a preview is already rendering "
                                     "(Escape cancels it)")
            return {"CANCELLED"}
        renderer = resolve_renderer(context.scene.smdl_toy_path)
        if not renderer:
            self.report({"ERROR"}, "cannot find smdl-toy; set its path in the "
                                   "SpectralMDL Layout panel")
            return {"CANCELLED"}
        if context.scene.camera is None:
            self.report({"ERROR"}, "the scene has no camera to render from")
            return {"CANCELLED"}

        directory = session_directory()
        scene_path = os.path.join(directory, "preview.layout")
        self._output = os.path.join(directory, "preview.png")
        self._log_path = os.path.join(directory, "preview.log")
        self._progress_path = os.path.join(directory, "preview.progress")
        self._shown = None
        # A stale image from the previous run must not read as success if
        # this run dies before writing its own.
        for path in (self._output, self._progress_path):
            if os.path.exists(path):
                os.remove(path)
        report = write_scene(context, scene_path,
                             context.scene.smdl_asset_root)
        for problem in report["problems"]:
            self.report({"WARNING"}, problem)

        command = build_command(context, renderer, scene_path, self._output,
                                report["material_file"],
                                self._progress_path)
        try:
            # The renderer's stderr goes to a FILE, never a pipe: a pipe
            # nobody drains fills its OS buffer at around 64 KB, and a
            # renderer with a lot to say then blocks on a stderr write
            # forever.
            with open(self._log_path, "w") as log:
                self._process = subprocess.Popen(
                    command, stdout=subprocess.DEVNULL, stderr=log)
        except OSError as error:
            self.cleanup(context)
            self.report({"ERROR"}, f"cannot run {renderer}: {error}")
            return {"CANCELLED"}
        SMDL_OT_render_preview._running = True
        SMDL_OT_render_preview._cancelled = False
        self._timer = context.window_manager.event_timer_add(
            0.25, window=context.window)
        context.window_manager.modal_handler_add(self)
        self.report({"INFO"}, f"rendering {report['placements']} placement(s) "
                              f"at {context.scene.smdl_preview_spp} spp")
        return {"RUNNING_MODAL"}

    def modal(self, context, event):
        if event.type == "ESC" or SMDL_OT_render_preview._cancelled:
            return self.stop(context)
        if event.type != "TIMER":
            return {"PASS_THROUGH"}
        if self._process.poll() is None:
            self.refresh(context)
            return {"RUNNING_MODAL"}
        if self._process.returncode != 0 or not os.path.exists(self._output):
            # The renderer's own last words say more than anything this
            # could say about them.
            try:
                with open(self._log_path) as log:
                    errors = log.read()
            except OSError:
                errors = ""
            last = [line for line in errors.splitlines() if line.strip()]
            self.cleanup(context)
            self.report({"ERROR"}, last[-1] if last else
                        "smdl-toy failed with no message")
            return {"CANCELLED"}
        show_image(self._output)
        self.finish(context)
        self.report({"INFO"}, f"preview rendered to {self._output}")
        return {"FINISHED"}

    def stop(self, context):
        """Kill the renderer and keep whatever it had drawn by then.

        A cancelled preview is worth as much as the checkpoint it reached,
        which is often the whole point of cancelling: enough of the image
        arrived to decide something.
        """
        status = read_progress(self._progress_path)
        self._process.terminate()
        try:
            self._process.wait(timeout=5.0)
        except subprocess.TimeoutExpired:
            self._process.kill()
        shown = os.path.exists(self._output)
        if shown:
            show_image(self._output)
        self.cleanup(context)
        self.report({"INFO"}, "preview cancelled"
                    + (f" at {status}" if status else "")
                    + (", showing the last update" if shown else ""))
        return {"CANCELLED"}

    def refresh(self, context):
        """Show the checkpoint the renderer has written, and what it says
        about how far along it is."""
        status = read_progress(self._progress_path)
        if status != context.window_manager.smdl_preview_status:
            context.window_manager.smdl_preview_status = status
            for window in context.window_manager.windows:
                for area in window.screen.areas:
                    if area.type == "PROPERTIES":
                        area.tag_redraw()
        try:
            stamp = os.path.getmtime(self._output)
        except OSError:
            return
        if stamp != self._shown:
            self._shown = stamp
            show_image(self._output)

    def finish(self, context):
        if self._timer is not None:
            context.window_manager.event_timer_remove(self._timer)
            self._timer = None
        self._process = None
        context.window_manager.smdl_preview_status = ""
        SMDL_OT_render_preview._running = False
        SMDL_OT_render_preview._cancelled = False

    def cleanup(self, context):
        self.finish(context)
        # The renderer writes its checkpoints through a sibling file; a
        # killed one can leave the last of them behind.
        part = os.path.splitext(self._output)
        for path in (part[0] + ".part" + part[1], self._progress_path):
            if path and os.path.exists(path):
                try:
                    os.remove(path)
                except OSError:
                    pass


class SMDL_OT_cancel_preview(bpy.types.Operator):
    """Stop the preview that is rendering, keeping its last update"""

    bl_idname = "smdl.cancel_preview"
    bl_label = "Cancel Preview"

    @classmethod
    def poll(cls, context):
        return SMDL_OT_render_preview.running()

    def execute(self, context):
        # The render is a modal operator this cannot reach, so this asks
        # and its next timer tick answers. Escape over the viewport does
        # the same thing; this is the one that can be clicked.
        SMDL_OT_render_preview.cancel_requested()
        return {"FINISHED"}


CLASSES = (SMDL_OT_render_preview, SMDL_OT_cancel_preview)
