#include "CurvesFile.h"
#include "Layout.h"
#include "PlacesFile.h"
#include "Scene.h"

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

// Is spelled as an MDL identifier, and therefore can be matched by
// `smdl::Compiler::findMaterial()` at all? Exporters routinely emit names
// like `Material.001` or `Wood Floor` that no MDL material can be called.
[[nodiscard]]
static bool isMDLIdentifier(std::string_view name) {
  if (name.empty() || !(std::isalpha(uint8_t(name[0])) || name[0] == '_'))
    return false;
  for (char ch : name)
    if (!(std::isalnum(uint8_t(ch)) || ch == '_')) return false;
  return true;
}

// The nearest MDL identifier to `name`, to suggest as a rename.
[[nodiscard]]
static std::string toMDLIdentifier(std::string_view name) {
  std::string result{};
  for (char ch : name)
    result += std::isalnum(uint8_t(ch)) || ch == '_' ? ch : '_';
  if (result.empty() || std::isdigit(uint8_t(result[0])))
    result.insert(result.begin(), '_');
  return result;
}

// A JSON string, escaped. Paths and authored names are arbitrary text, so
// this cannot be skipped even though it almost never does anything.
[[nodiscard]]
static std::string jsonString(std::string_view text) {
  auto result{std::string("\"")};
  for (char ch : text) {
    switch (ch) {
    case '"':
      result += "\\\"";
      break;
    case '\\':
      result += "\\\\";
      break;
    case '\n':
      result += "\\n";
      break;
    case '\r':
      result += "\\r";
      break;
    case '\t':
      result += "\\t";
      break;
    default:
      if (uint8_t(ch) < 0x20) {
        static constexpr char DIGITS[]{"0123456789abcdef"};
        result += "\\u00";
        result += DIGITS[(uint8_t(ch) >> 4) & 0xF];
        result += DIGITS[uint8_t(ch) & 0xF];
      } else {
        result += ch;
      }
      break;
    }
  }
  return result += '"';
}

// A float with enough digits to read back exactly.
[[nodiscard]]
static std::string jsonNumber(float value) {
  // JSON has no infinity, and empty bounds are a real answer rather than
  // an error, so they are reported as the absence of a number.
  if (!std::isfinite(value)) return "null";
  char buffer[32]{};
  std::snprintf(buffer, sizeof(buffer), "%.9g", double(value));
  return buffer;
}

[[nodiscard]]
static std::string jsonNumbers(const float3 &values) {
  return smdl::concat("[", jsonNumber(values.x), ", ", jsonNumber(values.y),
                      ", ", jsonNumber(values.z), "]");
}

[[nodiscard]]
static std::string jsonStrings(const std::vector<std::string> &values) {
  auto result{std::string("[")};
  for (size_t i = 0; i < values.size(); i++)
    result += smdl::concat(i == 0 ? "" : ", ", jsonString(values[i]));
  return result += ']';
}

void printObjectTableJSON(const Layout &layout) {
  auto &os{llvm::outs()};
  auto seenFiles{std::set<std::string, std::less<>>()};
  os << "{\n  \"files\": [\n";
  size_t fileIndex{};
  for (const auto &item : layout.items) {
    // Primitives and curves offer nothing to 'select', and this JSON is
    // what the mesh preparation tooling reads, so neither appears here.
    if (item.primitive.active() || item.curves.active) continue;
    if (!seenFiles.insert(item.fileName).second) continue;
    auto info{ObjectFileInfo()};
    const auto usage{importObjectUsage(item.fileName, &info)};
    os << (fileIndex++ == 0 ? "" : ",\n");
    os << smdl::concat(
        "    {\n      \"file\": ", jsonString(item.fileName),
        ",\n      \"triangles\": ", info.triangleCount,
        ",\n      \"up_axis\": ", info.upAxis,
        ",\n      \"up_axis_sign\": ", info.upAxisSign,
        ",\n      \"units_per_meter\": ", jsonNumber(info.unitsPerMeter),
        ",\n      \"bounds\": [", jsonNumbers(info.boundMin), ", ",
        jsonNumbers(info.boundMax), "]",
        ",\n      \"materials\": ", jsonStrings(info.materialNames),
        ",\n      \"objects\": [\n");
    for (size_t i = 0; i < usage.size(); i++) {
      const auto &entry{usage[i]};
      os << smdl::concat("        {\"path\": ", jsonString(entry.path),
                         ", \"depth\": ", entry.depth,
                         ", \"triangles\": ", entry.triangleCount,
                         ", \"instances\": ", entry.instanceCount,
                         ", \"materials\": ", jsonStrings(entry.materialNames),
                         ", \"pivot\": ", jsonNumbers(entry.pivot),
                         ", \"bounds\": [", jsonNumbers(entry.boundMin), ", ",
                         jsonNumbers(entry.boundMax), "]}",
                         i + 1 < usage.size() ? ",\n" : "\n");
    }
    os << "      ]\n    }";
  }
  os << "\n  ]\n}\n";
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
    if (usage.empty()) {
      os << "\n  No named object holds geometry. Everything sits on the "
            "file's root node, which 'select' cannot name.\n\n";
      continue;
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
        item.batchTransforms.empty() ? 1
                                     : uint32_t(item.batchTransforms.size());
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
  auto &os{llvm::outs()};
  const auto usage{collectMaterialUsage(layout)};
  os << "{\n  \"materials\": [\n";
  for (size_t i = 0; i < usage.size(); i++) {
    const auto &entry{usage[i]};
    // The aliases and overrides are already folded into the item
    // assignments, so the name IS the lookup; the key stays for the
    // tools that read it.
    auto qualified{std::vector<std::string>()};
    if (compiler)
      for (const auto *match : compiler->findMaterials(entry.name))
        qualified.push_back(match->qualifiedName);
    os << smdl::concat(
        "    {\"name\": ", jsonString(entry.name),
        ", \"lookup\": ", jsonString(entry.name),
        ", \"identifier\": ", isMDLIdentifier(entry.name) ? "true" : "false",
        ", \"suggestion\": ", jsonString(toMDLIdentifier(entry.name)),
        ", \"meshes\": ", entry.meshCount,
        ", \"instances\": ", entry.instanceCount,
        ", \"triangles\": ", entry.triangleCount,
        ", \"resolved\": ", jsonStrings(qualified), "}",
        i + 1 < usage.size() ? ",\n" : "\n");
  }
  os << "  ]\n}\n";
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
  auto lower{float3(+INF, +INF, +INF)};
  auto upper{float3(-INF, -INF, -INF)};
  auto minRadius{+INF};
  auto maxRadius{-INF};
  for (const auto &point : file.points) {
    for (int axis = 0; axis < 3; axis++) {
      lower[axis] = std::min(lower[axis], point[axis]);
      upper[axis] = std::max(upper[axis], point[axis]);
    }
    minRadius = std::min(minRadius, point.w);
    maxRadius = std::max(maxRadius, point.w);
  }
  auto &os{llvm::outs()};
  os << smdl::concat(
      fileName, ": version ", file.version, ", ",
      CurvesFile::basisName(file.basis), " basis\n  ", file.strandCount(),
      " strand(s), ", file.points.size(), " point(s)",
      file.hasRootUVs() ? ", with a root UV column" : "", "\n  bounds [",
      lower.x, ", ", lower.y, ", ", lower.z, "] to [", upper.x, ", ", upper.y,
      ", ", upper.z, "]\n  radius ", minRadius, " to ", maxRadius, "\n");
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
