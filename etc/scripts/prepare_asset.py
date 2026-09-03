#!/usr/bin/env python3
"""Prepare mesh files as smdl-toy assets.

    prepare_asset.py init DIR --render MESH [options]  write a manifest
    prepare_asset.py build MANIFEST|DIR ... [options]  thumbnails and proxies
    prepare_asset.py catalog ROOT                      write the catalog file
    prepare_asset.py all ROOT [options]                catalog, then build all

`init` asks `smdl-toy -list-objects -json` what the mesh file holds and
records it: the up axis and unit scale the file declares, the selectable
objects with their pivots and material slots, and where the proxy will go.
The manifest is then the source of truth; `build` never rewrites one except
to record the azimuth `-autolook` solved, and `init` refuses to overwrite
without `--force`.

`build` renders one thumbnail per object with smdl-toy, then runs Blender
in the background with `asset_proxy.py` beside this script to produce the
proxy `.blend` the layout add-on links: one asset-marked collection per
object, tagged with the manifest and the select, catalogued, carrying the
thumbnail as its preview.
"""

import argparse
import concurrent.futures
import fnmatch
import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
import uuid

HERE = os.path.dirname(os.path.abspath(__file__))
ADDON = os.path.join(HERE, "..", "blender", "smdl_layout")
CATALOG_FILE = "blender_assets.cats.txt"
ASSIMP_TRIVIA = "$AssimpFbx$"


def load_manifest_module():
    """The add-on's manifest reader, imported by path so that what this
    tool writes is read back by exactly the code the add-on runs."""
    import importlib.util
    path = os.path.join(ADDON, "manifest.py")
    spec = importlib.util.spec_from_file_location("smdl_manifest", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def find_tool(explicit, name, fallback=None):
    if explicit:
        return explicit
    found = shutil.which(name)
    if found:
        return found
    if fallback and os.path.exists(fallback):
        return fallback
    sys.exit(f"cannot find {name}; pass --{name}")


def default_smdl_toy():
    return os.path.normpath(os.path.join(HERE, "..", "..", "build", "bin",
                                         "smdl-toy"))


def run_listing(smdl_toy, mesh_path):
    """The file's entry of `smdl-toy -list-objects -json`."""
    run = subprocess.run([smdl_toy, mesh_path, "-list-objects", "-json"],
                         capture_output=True, text=True)
    if run.returncode != 0:
        tail = [line for line in run.stderr.splitlines() if line.strip()]
        raise RuntimeError(f"smdl-toy cannot list {mesh_path}: "
                           + (tail[-1] if tail else "no message"))
    return json.loads(run.stdout)["files"][0]


def snake(text):
    text = re.sub(r"[^A-Za-z0-9]+", "_", text)
    text = re.sub(r"([a-z0-9])([A-Z])", r"\1_\2", text)
    return text.strip("_").lower()


def slug(text):
    return re.sub(r"[^A-Za-z0-9]+", "_", text).strip("_").lower()


def title(stem):
    return " ".join(word.capitalize() for word in stem.split("_") if word)


def thumbnail_name(stem, select, multi):
    """Where a manifest's thumbnail goes, relative to the asset directory.
    Shared with `asset_proxy.py`, which loads it as the preview."""
    if multi and select:
        return os.path.join("thumbs", f"{stem}_{slug(select)}.png")
    return os.path.join("thumbs", f"{stem}.png")


def yaml_scalar(value):
    text = str(value)
    if any(ch in text for ch in "#:,[]\"") or text != text.strip():
        return '"' + text.replace('"', '\\"') + '"'
    return text


def yaml_list(items):
    return "[" + ", ".join(yaml_scalar(item) for item in items) + "]"


def number(value):
    return f"{float(value):.7g}"


#--{ init

def pick_objects(nodes, patterns):
    """The nodes that stand for authored objects: the topmost nodes that
    carry geometry, looking through assimp's synthetic pivot nodes, with a
    match claiming its whole subtree."""
    picked = []
    skip = None
    for node in nodes:
        path = node["path"]
        if skip is not None and path.startswith(skip + "/"):
            continue
        leaf = path.rsplit("/", 1)[-1]
        if ASSIMP_TRIVIA in leaf or node["triangles"] == 0:
            continue
        picked.append(node)
        skip = path
    if patterns:
        def wanted(node):
            leaf = node["path"].rsplit("/", 1)[-1]
            return any(fnmatch.fnmatchcase(node["path"], pattern)
                       or fnmatch.fnmatchcase(leaf, pattern)
                       for pattern in patterns)
        picked = [node for node in picked if wanted(node)]
    return picked


def select_for(node, nodes):
    """A bare name when it names one node in the file, else the full path."""
    leaf = node["path"].rsplit("/", 1)[-1]
    if sum(1 for other in nodes
           if other["path"].rsplit("/", 1)[-1] == leaf) == 1:
        return leaf
    return node["path"]


def command_init(args):
    smdl_toy = find_tool(args.smdl_toy, "smdl-toy", default_smdl_toy())
    directory = os.path.abspath(args.directory)
    mesh_path = os.path.join(directory, args.render)
    if not os.path.isfile(mesh_path):
        sys.exit(f"no such mesh: {mesh_path}")
    stem = args.stem or snake(os.path.splitext(args.render)[0])
    manifest_path = os.path.join(directory, stem + ".asset")
    if os.path.exists(manifest_path) and not args.force:
        sys.exit(f"{manifest_path} exists; pass --force to overwrite it")
    listing = run_listing(smdl_toy, mesh_path)

    if args.up:
        up = args.up
    elif listing["up_axis"] == 1:
        up = "y"
    elif listing["up_axis"] == 0:
        sys.exit(f"{mesh_path} declares an X up axis, which no manifest "
                 f"can say")
    else:
        up = "z"
    if args.scale is not None:
        scale = args.scale
    elif listing["meters_per_unit"] > 0:
        scale = listing["meters_per_unit"]
    else:
        scale = 1.0

    objects = [] if args.whole else pick_objects(listing["objects"],
                                                 args.objects)
    if args.objects and not objects:
        sys.exit("no object matches " + ", ".join(args.objects))

    lines = ["asset: 1",
             f"name: {yaml_scalar(args.name or title(stem))}",
             f"render: {yaml_scalar(args.render)}",
             f"proxy: {yaml_scalar('proxy/' + stem + '.blend')}"]
    if up == "y":
        lines.append("up: y")
    if scale != 1.0:
        lines.append(f"scale: {number(scale)}")
    if args.front is not None:
        lines.append(f"front: {number(args.front)}")
    if objects:
        lines.append("objects:")
        for node in objects:
            select = select_for(node, listing["objects"])
            lines.append(f"  - select: {yaml_scalar(select)}")
            lines.append(f"    materials: {yaml_list(node['materials'])}")
            lines.append("    pivot: [" + ", ".join(number(v)
                                                    for v in node["pivot"])
                         + "]")
            lines.append(f"    triangles: {node['triangles']}")
    else:
        lines.append(f"materials: {yaml_list(listing['materials'])}")
    with open(manifest_path, "w") as stream:
        stream.write("\n".join(lines) + "\n")
    print(f"wrote {manifest_path}: {len(objects) or 'whole file'} object(s), "
          f"{listing['triangles']} triangles, up {up}, scale {number(scale)}")

#--}
#--{ catalog

def catalog_root(manifest_path, explicit=None):
    """The library root a manifest belongs to: the given one, else the
    nearest ancestor holding a catalog file, else none."""
    if explicit:
        return os.path.abspath(explicit)
    directory = os.path.dirname(os.path.abspath(manifest_path))
    while True:
        if os.path.isfile(os.path.join(directory, CATALOG_FILE)):
            return directory
        parent = os.path.dirname(directory)
        if parent == directory:
            return None
        directory = parent


def catalog_path_for(directory, root):
    relative = os.path.relpath(os.path.abspath(directory), root)
    if relative == ".":
        return ""
    return "/".join(title(part) for part in relative.split(os.sep))


def catalog_uuid(path):
    return str(uuid.uuid5(uuid.NAMESPACE_URL, "smdl-asset-catalog/" + path))


def manifests_under(root):
    found = []
    for dirpath, dirnames, files in os.walk(root):
        dirnames[:] = sorted(d for d in dirnames
                             if d not in ("proxy", "thumbs"))
        found += sorted(os.path.join(dirpath, f) for f in files
                        if f.endswith(".asset"))
    return found


def command_catalog(args):
    root = os.path.abspath(args.root)
    paths = set()
    for manifest in manifests_under(root):
        path = catalog_path_for(os.path.dirname(manifest), root)
        parts = path.split("/") if path else []
        for i in range(1, len(parts) + 1):
            paths.add("/".join(parts[:i]))
    lines = ["# This is an Asset Catalog Definition file for Blender.",
             "#",
             "# Empty lines and lines starting with `#` will be ignored.",
             "# The first non-ignored line should be the version indicator.",
             "# Other lines are of the format "
             "\"UUID:catalog/path/for/assets:simple catalog name\"",
             "",
             "VERSION 1",
             ""]
    for path in sorted(paths):
        lines.append(f"{catalog_uuid(path)}:{path}:{path.replace('/', '-')}")
    target = os.path.join(root, CATALOG_FILE)
    with open(target, "w") as stream:
        stream.write("\n".join(lines) + "\n")
    print(f"wrote {target}: {len(paths)} catalog(s)")

#--}
#--{ build

AZIMUTH = re.compile(r"Autolook: azimuth (\S+) deg")


def record_front(manifest_path, azimuth):
    """Add `front:` to a manifest that lacks it, before `objects:` or
    `materials:` so the scalars stay together."""
    with open(manifest_path) as stream:
        lines = stream.read().splitlines()
    if any(line.split(":")[0].strip() == "front" for line in lines
           if not line.startswith(" ")):
        return
    insert = len(lines)
    for i, line in enumerate(lines):
        key = line.split(":")[0].strip()
        if not line.startswith(" ") and key in ("objects", "materials"):
            insert = i
            break
    lines.insert(insert, f"front: {number(azimuth)}")
    with open(manifest_path, "w") as stream:
        stream.write("\n".join(lines) + "\n")


def render_thumbnails(args, smdl_toy, manifest_path, asset):
    directory = os.path.dirname(manifest_path)
    stem = os.path.splitext(os.path.basename(manifest_path))[0]
    os.makedirs(os.path.join(directory, "thumbs"), exist_ok=True)
    entries = asset.objects or [None]
    multi = len(asset.objects) > 1
    front = None
    with open(manifest_path) as stream:
        for line in stream:
            if line.startswith("front:"):
                front = float(line.split(":", 1)[1].split("#")[0])
    for entry in entries:
        select = entry["select"] if entry else ""
        output = os.path.join(directory, thumbnail_name(stem, select, multi))
        with tempfile.TemporaryDirectory(prefix="smdl-thumb-") as scratch:
            layout = os.path.join(scratch, "thumb.layout")
            lines = ["#smdl layout",
                     f'asset thing = "{os.path.basename(manifest_path)}" {{']
            if select:
                lines.append(f'  select "{select}"')
                lines.append("  recenter")
            lines.append("}")
            lines.append("place thing")
            with open(layout, "w") as stream:
                stream.write("\n".join(lines) + "\n")
            command = [smdl_toy, layout]
            if args.materials:
                command += [os.path.abspath(args.materials),
                            "-fallback-material", "default_object"]
            command += ["-asset-dir", directory, "-autolook", "-ground",
                        "-resolution", f"{args.size},{args.size}",
                        "-spp", str(args.spp), "-progress", "none",
                        "-output-rgb", output]
            if args.exposure != 1.0:
                command += ["-exposure", number(args.exposure)]
            if front is not None:
                command += ["-autolook-azimuth", number(front)]
            if args.threads:
                command += ["-threads", str(args.threads)]
            run = subprocess.run(command, capture_output=True, text=True)
        if run.returncode != 0:
            tail = [line for line in run.stderr.splitlines() if line.strip()]
            raise RuntimeError(f"thumbnail of {select or stem} failed: "
                               + (tail[-1] if tail else "no message"))
        match = AZIMUTH.search(run.stderr)
        if match and front is None and not multi and not args.no_front:
            record_front(manifest_path, float(match.group(1)))
        print(f"  thumbnail {os.path.relpath(output, directory)}"
              + (f" (azimuth {match.group(1)})" if match else ""))


def build_proxy(args, blender, smdl_toy, manifest_path, asset, root):
    directory = os.path.dirname(manifest_path)
    listing = run_listing(smdl_toy, os.path.join(directory, asset.render))
    command = [blender, "-b", "--factory-startup", "--python",
               os.path.join(HERE, "asset_proxy.py"), "--",
               "--manifest", manifest_path,
               "--keep-leaf", number(args.keep_leaf),
               "--leaf-slots", args.leaf_slots,
               "--target-triangles", str(args.target_triangles),
               "--body-angle", number(args.body_angle)]
    if root:
        path = catalog_path_for(directory, root)
        tags = [part.lower() for part in path.split("/") if part]
        command += ["--catalog-id", catalog_uuid(path), "--tags",
                    ",".join(tags)]
    with tempfile.TemporaryDirectory(prefix="smdl-proxy-") as scratch:
        listing_path = os.path.join(scratch, "listing.json")
        with open(listing_path, "w") as stream:
            json.dump(listing, stream)
        command += ["--listing", listing_path]
        run = subprocess.run(command, capture_output=True, text=True)
    lines = [line for line in (run.stdout + run.stderr).splitlines()
             if line.startswith(("proxy:", "reduce:")) or "Error" in line
             or "Traceback" in line]
    if run.returncode != 0 or not any(line.startswith("proxy:")
                                      for line in lines):
        tail = [line for line in (run.stdout + run.stderr).splitlines()
                if line.strip()]
        raise RuntimeError(f"proxy of {manifest_path} failed:\n  "
                           + "\n  ".join(tail[-12:]))
    for line in lines:
        print("  " + line)


def build_one(args, tools, manifest_path):
    manifest_module, smdl_toy, blender = tools
    manifest_path = os.path.abspath(manifest_path)
    asset = manifest_module.Asset(manifest_path)
    print(manifest_path)
    if not args.proxy_only:
        render_thumbnails(args, smdl_toy, manifest_path, asset)
    if not args.thumbs_only:
        root = catalog_root(manifest_path, args.root)
        build_proxy(args, blender, smdl_toy, manifest_path, asset, root)


def expand_targets(targets):
    manifests = []
    for target in targets:
        if os.path.isdir(target):
            found = sorted(os.path.join(target, f) for f in os.listdir(target)
                           if f.endswith(".asset"))
            if not found:
                sys.exit(f"{target} holds no '.asset' manifest")
            manifests += found
        elif target.endswith(".asset") and os.path.isfile(target):
            manifests.append(target)
        else:
            sys.exit(f"{target} is neither a manifest nor a directory")
    return manifests


def build_all(args, manifests):
    manifest_module = load_manifest_module()
    smdl_toy = find_tool(args.smdl_toy, "smdl-toy", default_smdl_toy())
    blender = None if args.thumbs_only else find_tool(args.blender, "blender")
    tools = (manifest_module, smdl_toy, blender)
    failures = []
    # Thumbnails use every core of the renderer, so they run one at a
    # time; the Blender proxies are single-threaded and run in a pool.
    if not args.proxy_only:
        for manifest in manifests:
            try:
                asset = manifest_module.Asset(manifest)
                print(manifest)
                render_thumbnails(args, smdl_toy, manifest, asset)
            except Exception as error:
                failures.append((manifest, str(error)))
                print(f"  FAILED: {error}")
    if not args.thumbs_only:
        def job(manifest):
            asset = manifest_module.Asset(manifest)
            root = catalog_root(manifest, args.root)
            build_proxy(args, blender, smdl_toy, manifest, asset, root)
            return manifest
        with concurrent.futures.ThreadPoolExecutor(args.jobs) as pool:
            futures = {pool.submit(job, m): m for m in manifests}
            for future in concurrent.futures.as_completed(futures):
                manifest = futures[future]
                try:
                    future.result()
                except Exception as error:
                    failures.append((manifest, str(error)))
                    print(f"{manifest}\n  FAILED: {error}")
    if failures:
        print(f"{len(failures)} of {len(manifests)} failed:")
        for manifest, error in failures:
            print(f"  {manifest}: {error.splitlines()[0]}")
        sys.exit(1)
    print(f"built {len(manifests)} manifest(s)")


def command_build(args):
    build_all(args, [os.path.abspath(m) for m in expand_targets(args.targets)])


def command_all(args):
    args.root = os.path.abspath(args.root)
    command_catalog(args)
    build_all(args, manifests_under(args.root))

#--}


def add_build_options(parser):
    parser.add_argument("--thumbs-only", action="store_true")
    parser.add_argument("--proxy-only", action="store_true")
    parser.add_argument("--materials", help="an MDL/SMDL file to shade the "
                        "thumbnails with (unresolved names fall back to the "
                        "renderer's gray)")
    parser.add_argument("--size", type=int, default=256)
    parser.add_argument("--spp", type=int, default=64)
    parser.add_argument("--exposure", type=float, default=2.0,
                        help="thumbnail exposure; the renderer's own default "
                             "of 1 reads dark under the gray fallback")
    parser.add_argument("--threads", type=int, default=0)
    parser.add_argument("--no-front", action="store_true",
                        help="do not record the solved azimuth as 'front:'")
    parser.add_argument("--keep-leaf", type=float, default=0.15,
                        help="fraction of leaf-slot faces the proxy keeps")
    parser.add_argument("--leaf-slots", default="leaf,stem,snow,trail",
                        help="material slot names thinned as cards rather "
                             "than collapsed")
    parser.add_argument("--target-triangles", type=int, default=20000,
                        help="proxy triangles for a solid; a tree's body "
                             "is planar-dissolved instead")
    parser.add_argument("--body-angle", type=float, default=30.0,
                        help="planar dissolve angle in degrees for a tree's "
                             "body")
    parser.add_argument("--root", help="the library root, for the catalog "
                        "(default: the nearest ancestor holding "
                        + CATALOG_FILE + ")")
    parser.add_argument("--jobs", type=int, default=1,
                        help="Blender proxy builds to run at once")
    parser.add_argument("--smdl-toy")
    parser.add_argument("--blender")


def main():
    parser = argparse.ArgumentParser(
        description=__doc__.split("\n")[0],
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__)
    commands = parser.add_subparsers(dest="command", required=True)

    init = commands.add_parser("init", help="write a manifest")
    init.add_argument("directory")
    init.add_argument("--render", required=True,
                      help="the mesh file, relative to the directory")
    init.add_argument("--stem", help="the manifest name (default: from the "
                      "mesh file name)")
    init.add_argument("--name", help="the display name (default: from the "
                      "stem)")
    init.add_argument("--up", choices=("y", "z"))
    init.add_argument("--scale", type=float)
    init.add_argument("--objects", nargs="*", default=[],
                      help="glob patterns selecting which objects to list")
    init.add_argument("--whole", action="store_true",
                      help="list no objects: the file is placed whole")
    init.add_argument("--front", type=float)
    init.add_argument("--force", action="store_true")
    init.add_argument("--smdl-toy")
    init.set_defaults(run=command_init)

    build = commands.add_parser("build", help="thumbnails and proxies")
    build.add_argument("targets", nargs="+", help="manifests or directories")
    add_build_options(build)
    build.set_defaults(run=command_build)

    catalog = commands.add_parser("catalog", help="write the catalog file")
    catalog.add_argument("root")
    catalog.set_defaults(run=command_catalog)

    everything = commands.add_parser("all", help="catalog and build all")
    everything.add_argument("root")
    add_build_options(everything)
    everything.set_defaults(run=command_all)

    args = parser.parse_args()
    args.run(args)


if __name__ == "__main__":
    main()
