#include "CurvesFile.h"
#include "Layout.h"
#include "PlacesFile.h"
#include "Scene.h"

#include "llvm/Support/JSON.h"
#include "llvm/Support/WithColor.h"

#include "smdl/Support/Logger.h"

#include <cctype>
#include <filesystem>
#include <set>

// The human- and machine-readable tables over a lowered layout: what
// each file offers to `select`, and how each material name resolves.
// This lives apart from `Layout.cc` because it needs LLVM output
// streams and the scene's assimp-backed introspection, neither of which
// the parser and lowering may depend on.

namespace {

// The material name is the entire binding surface between a scene file
// and a material, so a name that no MDL material can be called is a
// binding that can never resolve. Exporters routinely emit names like
// `Material.001` or `Wood Floor`, which `findMaterial()` cannot match.
[[nodiscard]] bool isMDLIdentifier(std::string_view name) {
  if (name.empty() || !(std::isalpha(uint8_t(name[0])) || name[0] == '_'))
    return false;
  for (char ch : name)
    if (!(std::isalnum(uint8_t(ch)) || ch == '_')) return false;
  return true;
}

// The nearest MDL identifier to `name`, to suggest as a rename.
[[nodiscard]] std::string toMDLIdentifier(std::string_view name) {
  std::string result{};
  for (char ch : name)
    result += std::isalnum(uint8_t(ch)) || ch == '_' ? ch : '_';
  if (result.empty() || std::isdigit(uint8_t(result[0])))
    result.insert(result.begin(), '_');
  return result;
}

// Print the object rows of one file's usage, preceded by a blank line
// and followed by another, or the note that the file has nothing to
// name. The caller prints the header line that says which file this is.
void printObjectUsageRows(llvm::raw_ostream &os,
                          const std::vector<ObjectUsage> &usage) {
  if (usage.empty()) {
    os << "\n  No named object holds geometry. Everything sits on the "
          "file's root node, which 'select' cannot name.\n\n";
    return;
  }
  // Pad the name field so the counts line up in a column, which is the
  // thing being scanned. Nesting is shown by indenting the path, but the
  // path printed is still the whole thing, because that is what a pattern
  // containing '/' has to match.
  auto names{std::vector<std::string>()};
  auto counts{std::vector<std::string>()};
  size_t nameWidth{};
  size_t countWidth{};
  for (const auto &entry : usage) {
    names.push_back(smdl::concat(std::string(2 * entry.depth, ' '),
                                 smdl::Quoted(entry.path)));
    counts.push_back(smdl::concat(
        entry.triangleCount, entry.triangleCount == 1 ? " tri" : " tris",
        entry.instanceCount == 1
            ? std::string()
            : smdl::concat(" in ", entry.instanceCount, " meshes")));
    nameWidth = std::max(nameWidth, names.back().size());
    countWidth = std::max(countWidth, counts.back().size());
  }
  os << '\n';
  for (size_t i = 0; i < usage.size(); i++) {
    const auto &entry{usage[i]};
    os << "  ";
    llvm::WithColor(os, llvm::HighlightColor::Tag) << names[i];
    os.indent(nameWidth - names[i].size() + 2);
    llvm::WithColor(os, llvm::HighlightColor::Note) << counts[i];
    os.indent(countWidth - counts[i].size() + 2);
    if (entry.materialNames.size() == 1) {
      llvm::WithColor(os, llvm::HighlightColor::Attribute)
          << smdl::concat(smdl::Quoted(entry.materialNames[0]));
    } else {
      llvm::WithColor(os, llvm::HighlightColor::Note)
          << smdl::concat(entry.materialNames.size(), " materials");
    }
    os << '\n';
  }
  os << '\n';
}

} // namespace

void printObjectTableJSON(const Layout &layout) {
  auto seenFiles{std::set<std::string, std::less<>>()};
  llvm::json::OStream json{llvm::outs(), 2};
  json.object([&] {
    json.attributeArray("files", [&] {
      for (const auto &item : layout.items) {
        // Primitives and curves offer nothing to 'select', and this JSON
        // is what the mesh preparation tooling reads, so neither appears
        // here.
        if (item.primitive.active() || item.curves.active) continue;
        if (!seenFiles.insert(item.fileName).second) continue;
        auto info{ObjectFileInfo()};
        const auto usage{importObjectUsage(item.fileName, &info)};
        objectListingJSON(json, item.fileName, info, usage);
      }
    });
  });
  llvm::outs() << '\n';
}

void printObjectTable(const Layout &layout) {
  auto &os{llvm::outs()};
  auto seenFiles{std::set<std::string, std::less<>>()};
  for (const auto &item : layout.items) {
    if (item.primitive.active()) continue;
    if (!seenFiles.insert(item.fileName).second) continue;
    // A groom has nothing to 'select', but silence would read as a
    // hole, so it gets its one-line summary.
    if (item.curves.active) {
      const auto file{readCurvesFile(item.fileName)};
      os << smdl::concat(item.fileName, ": curves, ", file.strandCount(),
                         " strand(s), ", file.points.size(), " point(s), ",
                         CurvesFile::basisName(file.basis), " basis",
                         file.hasRootUVs() ? ", root UVs" : "", "\n\n");
      continue;
    }
    const auto usage{importObjectUsage(item.fileName)};
    uint64_t numTriangles{};
    for (const auto &entry : usage)
      if (entry.depth == 0) numTriangles += entry.triangleCount;
    os << smdl::concat(item.fileName, ": ", usage.size(), " object(s), ",
                       numTriangles, " triangles\n");
    printObjectUsageRows(os, usage);
  }
  os << "Select one in a '.layout' file, for instance:\n"
        "  asset thing = \"file\" { select \"name\" recenter }\n"
        "  place thing { translate 1 0 0 }\n";
}

// Merge the material usage by name across the layout, since that is how
// the scene resolves them: one MDL material serves every file that names
// it. A file is read once per (selection, assignment) key: its material
// names depend on both, but not on where it was placed. The names
// reported are the ones the scene resolves, after every fold the
// lowering composed into the item. Mesh and triangle counts describe
// distinct geometry, counted once per unique item; the instance count
// describes what is actually in the scene, so it scales by how many
// times the layout asks for the item.
//
[[nodiscard]]
static std::vector<MaterialUsage> collectMaterialUsage(const Layout &layout) {
  auto importKey{[](const LayoutItem &item) {
    return std::pair(item.fileName + "|" + item.primitive.key() + "|" +
                         item.curves.key(),
                     item.selection.key() + "|" + item.materials.key());
  }};
  auto multiplicity{std::map<std::pair<std::string, std::string>, uint32_t>()};
  for (const auto &item : layout.items)
    multiplicity[importKey(item)] +=
        item.batchXfs.empty() ? 1 : uint32_t(item.batchXfs.size());
  auto usage{std::vector<MaterialUsage>()};
  auto indexByName{std::map<std::string, size_t, std::less<>>()};
  auto seenImports{std::set<std::pair<std::string, std::string>>()};
  for (const auto &item : layout.items) {
    const auto key{importKey(item)};
    if (!seenImports.insert(key).second) continue;
    // A primitive or a groom is one implicit mesh with the one name its
    // asset assigned; there is no file to ask.
    auto itemUsage{std::vector<MaterialUsage>()};
    if (item.primitive.active() || item.curves.active) {
      auto &entry{itemUsage.emplace_back()};
      entry.name = "";
      entry.meshCount = 1;
      entry.instanceCount = 1;
    } else {
      itemUsage = importMaterialUsage(item.fileName, item.selection);
    }
    for (auto &entry : itemUsage) {
      entry.name = std::string(item.materials.resolve(entry.name));
      entry.instanceCount *= multiplicity[key];
      auto [slot, isNew]{indexByName.try_emplace(entry.name, usage.size())};
      if (isNew) {
        usage.push_back(std::move(entry));
      } else {
        auto &merged{usage[slot->second]};
        merged.meshCount += entry.meshCount;
        merged.instanceCount += entry.instanceCount;
        merged.triangleCount += entry.triangleCount;
      }
    }
  }
  return usage;
}

void printMaterialTableJSON(const smdl::Compiler *compiler,
                            const Layout &layout) {
  const auto usage{collectMaterialUsage(layout)};
  llvm::json::OStream json{llvm::outs(), 2};
  json.object([&] {
    json.attributeArray("materials", [&] {
      for (const auto &entry : usage)
        json.object([&] {
          // The aliases and overrides are already folded into the item
          // assignments, so the name IS the lookup; the key stays for
          // the tools that read it.
          json.attribute("name", entry.name);
          json.attribute("lookup", entry.name);
          json.attribute("identifier", isMDLIdentifier(entry.name));
          json.attribute("suggestion", toMDLIdentifier(entry.name));
          json.attribute("meshes", entry.meshCount);
          json.attribute("instances", entry.instanceCount);
          json.attribute("triangles", entry.triangleCount);
          json.attributeArray("resolved", [&] {
            if (compiler)
              for (const auto *match : compiler->findMaterials(entry.name))
                json.value(match->qualifiedName);
          });
        });
    });
  });
  llvm::outs() << '\n';
}

void printMaterialTable(const smdl::Compiler *compiler, const Layout &layout) {
  auto seenFiles{std::set<std::string, std::less<>>()};
  for (const auto &item : layout.items) seenFiles.insert(item.fileName);
  const auto usage{collectMaterialUsage(layout)};
  auto &os{llvm::outs()};
  uint32_t numMeshes{};
  uint32_t numInstances{};
  for (const auto &entry : usage)
    numMeshes += entry.meshCount, numInstances += entry.instanceCount;
  {
    size_t i{};
    for (const auto &fileName : seenFiles)
      os << (i++ == 0 ? "" : ", ") << fileName;
  }
  os << smdl::concat(": ", usage.size(), " material(s) on ", numMeshes,
                     " mesh(es), ", numInstances, " instance(s)\n");
  if (usage.empty()) return;
  // Pad the name and count fields to a common width so the statuses line up
  // in a column, which is the thing being scanned for.
  auto names{std::vector<std::string>()};
  auto counts{std::vector<std::string>()};
  size_t nameWidth{};
  size_t countWidth{};
  for (const auto &entry : usage) {
    names.push_back(smdl::concat(smdl::Quoted(entry.name)));
    counts.push_back(smdl::concat(
        entry.meshCount, entry.meshCount == 1 ? " mesh, " : " meshes,", " ",
        entry.triangleCount, entry.triangleCount == 1 ? " tri" : " tris"));
    nameWidth = std::max(nameWidth, names.back().size());
    countWidth = std::max(countWidth, counts.back().size());
  }
  auto pad{[&](const std::string &field, size_t width) {
    os.indent(width - field.size() + 2);
  }};
  os << '\n';
  for (size_t i = 0; i < usage.size(); i++) {
    const auto &entry{usage[i]};
    os << "  ";
    llvm::WithColor(os, llvm::HighlightColor::Tag) << names[i];
    pad(names[i], nameWidth);
    llvm::WithColor(os, llvm::HighlightColor::Note) << counts[i];
    // The identifier check comes first: a name that cannot be spelled as
    // an MDL identifier can never match anything, so reporting it as
    // merely missing would send the user looking for the wrong problem.
    // Every alias and override is already folded into the name, so the
    // name shown is exactly the name that will be looked up.
    const bool isInvalid{!isMDLIdentifier(entry.name)};
    if (!isInvalid && !compiler) {
      os << '\n'; // Nothing to resolve against.
      continue;
    }
    pad(counts[i], countWidth);
    if (isInvalid) {
      llvm::WithColor(os, llvm::HighlightColor::Error) << "invalid";
      os << (entry.name.empty()
                 ? std::string("    unnamed; give it a name in the layout")
                 : smdl::concat("    not an MDL identifier; try ",
                                smdl::Quoted(toMDLIdentifier(entry.name))));
      os << '\n';
      continue;
    }
    const auto matches{compiler->findMaterials(entry.name)};
    if (matches.empty()) {
      llvm::WithColor(os, llvm::HighlightColor::Error) << "missing";
      os << "    no MDL material matches\n";
    } else if (matches.size() > 1) {
      llvm::WithColor(os, llvm::HighlightColor::Warning) << "ambiguous";
      for (size_t j = 0; j < matches.size(); j++)
        os << (j == 0 ? "  " : ", ") << matches[j]->qualifiedName;
      os << '\n';
    } else {
      llvm::WithColor(os, llvm::HighlightColor::String) << "ok";
      os << "         ";
      llvm::WithColor(os, llvm::HighlightColor::Attribute)
          << matches[0]->qualifiedName;
      os << '\n';
    }
  }
  os << '\n';
  if (!compiler)
    os << "Pass the MDL modules too to see how each name resolves.\n";
}

// A float with enough digits to read back exactly through the layout
// parser, for the pack/dump round trip.
[[nodiscard]]
static std::string placeNumber(float value) {
  char buffer[32]{};
  std::snprintf(buffer, sizeof(buffer), "%.9g", double(value));
  return buffer;
}

void dumpPlaces(const std::string &fileName) {
  const auto places{readPlacesFile(fileName)};
  auto &os{llvm::outs()};
  os << smdl::concat("# ", fileName, ": version ", places.version, ", ",
                     places.transforms.size(), " record(s)",
                     places.hasVariants() ? ", with a variant column" : "",
                     "\n# 'thing' stands for whatever asset or group the "
                     "buffer scatters.\n");
  for (size_t i = 0; i < places.transforms.size(); i++) {
    const auto &transform{places.transforms[i]};
    os << "place thing matrix";
    for (int row = 0; row < 4; row++)
      for (int column = 0; column < 4; column++)
        os << ' ' << placeNumber(transform[column][row]);
    if (places.hasVariants() && places.variants[i] != PlacesFile::NO_VARIANT)
      os << smdl::concat("  # variant ", places.variants[i]);
    os << '\n';
  }
}

void dumpCurves(const std::string &fileName) {
  const auto file{readCurvesFile(fileName)};
  BoundBox3 bound{};
  auto minRadius{+INF};
  auto maxRadius{-INF};
  for (const auto &point : file.points) {
    bound.extend(float3(point.x, point.y, point.z));
    minRadius = std::min(minRadius, point.w);
    maxRadius = std::max(maxRadius, point.w);
  }
  auto &os{llvm::outs()};
  os << smdl::concat(
      fileName, ": version ", file.version, ", ",
      CurvesFile::basisName(file.basis), " basis\n  ", file.strandCount(),
      " strand(s), ", file.points.size(), " point(s)",
      file.hasRootUVs() ? ", with a root UV column" : "", "\n  bounds [",
      bound.lower.x, ", ", bound.lower.y, ", ", bound.lower.z, "] to [",
      bound.upper.x, ", ", bound.upper.y, ", ", bound.upper.z, "]\n  radius ",
      minRadius, " to ", maxRadius, "\n");
}

void packPlaces(const std::string &layoutFileName, std::string outputFileName) {
  if (outputFileName.empty())
    outputFileName = std::filesystem::path(layoutFileName)
                         .replace_extension(PLACES_EXTENSION)
                         .string();
  auto diags{LayoutDiagnostics()};
  const auto &source{diags.loadSource(layoutFileName)};
  const auto document{parseLayout(
      diags, source,
      std::filesystem::path(layoutFileName).parent_path().string())};
  if (!diags.empty()) diags.printAll(smdl::cerrSupportsANSIColors());
  if (diags.hasErrors())
    throw smdl::Error(smdl::concat("cannot pack ",
                                   smdl::QuotedPath(layoutFileName), ": ",
                                   diags.summary()));
  auto places{PlacesFile()};
  auto assetName{std::string()};
  using Overrides = std::map<std::string, std::string, std::less<>>;
  auto variants{std::vector<Overrides>()};
  auto variantIndexByOverrides{std::map<Overrides, uint32_t>()};
  auto anyVariant{false};
  for (const auto &placement : document.placements) {
    // Only ordinary places pack: each is one record, and everything
    // else (imports, bulk places) has no record to become.
    if (placement.kind != LayoutPlacement::Kind::PLACE ||
        !placement.placesPath.empty())
      throw smdl::Error(smdl::concat(
          "cannot pack ", smdl::QuotedPath(layoutFileName),
          ": every top-level placement must be an ordinary 'place'"));
    if (assetName.empty()) {
      assetName = placement.assetName;
    } else if (assetName != placement.assetName) {
      throw smdl::Error(smdl::concat(
          "cannot pack ", smdl::QuotedPath(layoutFileName), ": it places ",
          smdl::Quoted(assetName), " and ", smdl::Quoted(placement.assetName),
          ", and a '.places' buffer scatters one asset or group"));
    }
    places.transforms.push_back(placement.transform);
    auto variantIndex{PlacesFile::NO_VARIANT};
    if (!placement.overrides.empty()) {
      const auto [entry, isNew]{variantIndexByOverrides.try_emplace(
          placement.overrides, uint32_t(variants.size()))};
      if (isNew) variants.push_back(placement.overrides);
      variantIndex = entry->second;
      anyVariant = true;
    }
    places.variants.push_back(variantIndex);
  }
  if (places.transforms.empty())
    throw smdl::Error(smdl::concat("cannot pack ",
                                   smdl::QuotedPath(layoutFileName),
                                   ": it has no 'place' statements"));
  if (!anyVariant) places.variants.clear();
  writePlacesFile(outputFileName, places);
  // The wrapper the buffer wants to live under, ready to paste.
  auto &os{llvm::outs()};
  os << smdl::concat(
      "Packed ", places.transforms.size(), " record(s)",
      anyVariant ? smdl::concat(" over ", variants.size(), " variant(s)")
                 : std::string(),
      " into ", smdl::QuotedPath(outputFileName), ". Scatter it with:\n\n");
  const auto relative{
      std::filesystem::path(outputFileName).filename().string()};
  os << smdl::concat("  place ", assetName, " * \"", relative, "\"");
  if (anyVariant) {
    os << " {\n";
    for (const auto &variant : variants) {
      os << "    variant {";
      for (const auto &[from, to] : variant)
        os << smdl::concat(" material \"", from, "\" = ", to);
      os << " }\n";
    }
    os << "  }";
  }
  os << "\n";
}
