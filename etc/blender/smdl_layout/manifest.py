"""Reading `.asset` manifests from inside Blender.

Reads the same strict subset the renderer's C++ reader does and nothing
more, so a manifest that this accepts is one the renderer accepts.
"""

import os


class Asset:
    """One asset manifest: what to render, how to correct it, and what of it
    can be selected."""

    def __init__(self, path):
        self.path = os.path.abspath(path)
        self.directory = os.path.dirname(self.path)
        self.name = os.path.basename(self.directory)
        self.render = ""
        self.proxy = ""
        self.up = "z"
        self.scale = 1.0
        self.materials = []
        self.objects = []
        self._read()

    def object_for(self, select):
        for entry in self.objects:
            if entry["select"] == select:
                return entry
        return None

    @property
    def proxy_path(self):
        return os.path.join(self.directory, self.proxy) if self.proxy else ""

    def _read(self):
        current = None
        in_objects = False
        with open(self.path) as stream:
            for line in stream:
                stripped = strip_comment(line).rstrip()
                if not stripped.strip():
                    continue
                indent = len(stripped) - len(stripped.lstrip(" "))
                content = stripped.strip()
                is_item = content.startswith("- ")
                if is_item:
                    content = content[2:].strip()
                key, _, value = content.partition(":")
                key, value = key.strip(), value.strip()
                if indent == 0 and not is_item:
                    in_objects = key == "objects"
                    if in_objects:
                        continue
                    self._scalar(key, unquote(value))
                elif in_objects:
                    if is_item:
                        current = {"select": "", "materials": [],
                                   "pivot": [0.0, 0.0, 0.0], "triangles": 0}
                        self.objects.append(current)
                    if current is not None:
                        self._object_key(current, key, value)

    def _scalar(self, key, value):
        if key == "name":
            self.name = value
        elif key == "render":
            self.render = value
        elif key == "proxy":
            self.proxy = value
        elif key == "up":
            self.up = value
        elif key == "scale":
            self.scale = float(value)
        elif key == "materials":
            # The whole file's material names, for a file whose geometry
            # sits on its unnamed root node and so lists no objects.
            self.materials = [unquote(item) for item in inline_list(value)]

    @staticmethod
    def _object_key(entry, key, value):
        if key == "select":
            entry["select"] = unquote(value)
        elif key == "materials":
            entry["materials"] = [unquote(item) for item in inline_list(value)]
        elif key == "pivot":
            entry["pivot"] = [float(item) for item in inline_list(value)]
        elif key == "triangles":
            entry["triangles"] = int(float(value))


def strip_comment(text):
    quoted = False
    for i, ch in enumerate(text):
        if ch == '"':
            quoted = not quoted
        elif ch == "#" and not quoted and (i == 0 or text[i - 1] in " \t"):
            return text[:i]
    return text


def unquote(text):
    text = text.strip()
    if len(text) >= 2 and text[0] == '"' and text[-1] == '"':
        return text[1:-1]
    return text


def inline_list(text):
    text = text.strip()
    if text.startswith("[") and text.endswith("]"):
        text = text[1:-1]
    return [item.strip() for item in text.split(",") if item.strip()]


def find_in_directory(directory):
    """The single `.asset` manifest in a directory, or empty.

    The same rule the renderer resolves an asset directory by, so what a
    layout can import is exactly what a layout file can name.
    """
    found = [entry for entry in sorted(os.listdir(directory))
             if entry.lower().endswith(".asset")]
    return os.path.join(directory, found[0]) if len(found) == 1 else ""
