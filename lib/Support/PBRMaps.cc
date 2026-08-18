#include "smdl/Support/PBRMaps.h"
#include "smdl/Support/Filesystem.h"
#include "smdl/Support/StringHelpers.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringSwitch.h"
#include <algorithm>
#include <cctype>
#include <cmath>
#include <cstdlib>
#include <filesystem>
#include <map>

namespace smdl {

using Role = PBRMaps::Role;
using Map = PBRMaps::Map;

[[nodiscard]] static bool is_scalar_role(Role role) {
  return role == PBRMaps::ROLE_ROUGHNESS || role == PBRMaps::ROLE_METALLIC ||
         role == PBRMaps::ROLE_HEIGHT;
}

namespace {

// The manifest parser. The grammar is the strict flat subset of YAML
// documented on `PBRMaps`; everything outside the subset
// is a specific, line-numbered error.
class ManifestParser final {
public:
  ManifestParser(PBRMaps &manifest, llvm::StringRef sourceCode,
                 const std::string &sourceName)
      : manifest(manifest), sourceCode(sourceCode), sourceName(sourceName) {}

  void parse() {
    lexLines();
    size_t i{0};
    while (i < lines.size()) {
      const auto &line{lines[i]};
      if (line.indent != 0) fail(line.lineNo, "unexpected indentation");
      checkDuplicate(topLevelSeen, line.key, line.lineNo);
      if (auto role{roleForKey(line.key)}) {
        i = parseMap(*role, i);
      } else {
        parseTopLevel(line);
        i++;
      }
    }
    // Two spellings of the relief scale is not an error: a scanned pack
    // whose relief was measured and then dialed back by eye is a real
    // authoring state. The manifest is only ambiguous, so say which one
    // wins and carry on.
    auto reliefScaleLine{topLevelSeen.find("relief_scale")};
    auto physicalReliefLine{topLevelSeen.find("physical_relief")};
    if (reliefScaleLine != topLevelSeen.end() &&
        physicalReliefLine != topLevelSeen.end())
      warn(reliefScaleLine->second,
           concat("'relief_scale' overrides 'physical_relief' (line ",
                  physicalReliefLine->second, ")"));
    // The horizon map stores tangents at the maps' relief scale, which
    // are meaningless without that scale to decode them against.
    const auto &horizon{manifest.maps[int(PBRMaps::ROLE_HORIZON)]};
    if (horizon && !manifest.effectiveReliefScale())
      fail(horizon.lineNo, "the horizon map requires 'relief_scale', or "
                           "'physical_extent' and 'physical_relief'");
  }

private:
  // One content line, comments stripped, split at the first `:`.
  struct Line final {
    int lineNo{};
    int indent{};
    llvm::StringRef key{};
    llvm::StringRef value{};
  };

  [[noreturn]] void fail(int lineNo, std::string_view message) {
    throw Error(concat("cannot load ", QuotedPath(sourceName), ": line ",
                       lineNo, ": ", message));
  }

  void warn(int lineNo, std::string_view message) {
    manifest.warnings.push_back(
        concat(QuotedPath(sourceName), ": line ", lineNo, ": ", message));
  }

  void lexLines() {
    auto remainder{sourceCode};
    remainder.consume_front("\xEF\xBB\xBF"); // UTF-8 BOM
    int lineNo{0};
    while (!remainder.empty()) {
      auto [text, rest] = remainder.split('\n');
      remainder = rest;
      lineNo++;
      text.consume_back("\r");
      size_t indent{0};
      while (indent < text.size() && text[indent] == ' ') indent++;
      if (indent < text.size() && text[indent] == '\t')
        fail(lineNo, "tab in indentation (use spaces)");
      auto content{stripComment(text.drop_front(indent)).rtrim()};
      if (content.empty()) continue;
      auto colon{content.find(':')};
      if (colon == llvm::StringRef::npos) fail(lineNo, "expected 'key: value'");
      auto key{content.take_front(colon).rtrim()};
      auto after{content.drop_front(colon + 1)};
      if (key.empty()) fail(lineNo, "expected 'key: value'");
      if (!after.empty() && after[0] != ' ')
        fail(lineNo, "expected a space after ':'");
      lines.push_back(Line{lineNo, int(indent), key, after.trim()});
    }
  }

  // Strip a full-line or trailing comment, respecting double quotes.
  [[nodiscard]] llvm::StringRef stripComment(llvm::StringRef text) {
    bool inQuotes{false};
    bool inEscape{false};
    for (size_t i = 0; i < text.size(); i++) {
      char ch{text[i]};
      if (inQuotes) {
        if (inEscape)
          inEscape = false;
        else if (ch == '\\')
          inEscape = true;
        else if (ch == '"')
          inQuotes = false;
      } else if (ch == '"') {
        inQuotes = true;
      } else if (ch == '#' &&
                 (i == 0 || text[i - 1] == ' ' || text[i - 1] == '\t')) {
        return text.take_front(i);
      }
    }
    return text;
  }

  void checkDuplicate(std::map<std::string, int> &seen, llvm::StringRef key,
                      int lineNo) {
    auto [itr, inserted] = seen.try_emplace(key.str(), lineNo);
    if (!inserted)
      fail(lineNo, concat("duplicate key ", Quoted(key.str()),
                          " (already on line ", itr->second, ")"));
  }

  [[nodiscard]] std::optional<Role> roleForKey(llvm::StringRef key) {
    return llvm::StringSwitch<std::optional<Role>>(key)
        .Case("basecolor", PBRMaps::ROLE_BASECOLOR)
        .Case("normal", PBRMaps::ROLE_NORMAL)
        .Case("roughness", PBRMaps::ROLE_ROUGHNESS)
        .Case("metallic", PBRMaps::ROLE_METALLIC)
        .Case("height", PBRMaps::ROLE_HEIGHT)
        .Case("emission", PBRMaps::ROLE_EMISSION)
        .Case("horizon", PBRMaps::ROLE_HORIZON)
        .Case("class", PBRMaps::ROLE_CLASS)
        .Default(std::nullopt);
  }

  void parseTopLevel(const Line &line) {
    if (line.value.empty())
      fail(line.lineNo,
           concat("expected a value for ", Quoted(line.key.str())));
    if (line.key == "pbr") {
      auto version{parseInt(line)};
      if (version != 1)
        fail(line.lineNo, concat("unsupported manifest version ", version,
                                 " (this build supports version 1)"));
      manifest.version = int(version);
    } else if (line.key == "name") {
      manifest.name = parseString(line);
    } else if (line.key == "description") {
      manifest.description = parseString(line);
    } else if (line.key == "physical_extent") {
      auto items{parseList(line)};
      if (items.size() != 2)
        fail(line.lineNo, "expected two positive reals for 'physical_extent'");
      auto extentU{parseFloat(line, items[0])};
      auto extentV{parseFloat(line, items[1])};
      if (!(extentU > 0 && extentV > 0))
        fail(line.lineNo, "expected two positive reals for 'physical_extent'");
      manifest.physicalExtent = {extentU, extentV};
    } else if (line.key == "physical_relief") {
      auto relief{parseFloat(line, line.value)};
      if (!(relief > 0))
        fail(line.lineNo, "expected a positive real for 'physical_relief' "
                          "(omit the key for no relief)");
      manifest.physicalRelief = relief;
    } else if (line.key == "relief_scale") {
      auto scale{parseFloat(line, line.value)};
      if (!(scale > 0))
        fail(line.lineNo, "expected a positive real for 'relief_scale' "
                          "(omit the key for no relief)");
      manifest.reliefScale = scale;
    } else if (line.key == "hex_rotation") {
      auto rotation{parseFloat(line, line.value)};
      if (!(rotation >= 0 && rotation <= 1))
        fail(line.lineNo, "expected a real in [0, 1] for 'hex_rotation'");
      manifest.hexRotation = rotation;
    } else {
      fail(line.lineNo, concat("unknown key ", Quoted(line.key.str())));
    }
  }

  // Parse a map role, in scalar form `role: filename` or block form
  // `role:` followed by an indented table. Returns the index of the
  // next top-level line.
  [[nodiscard]] size_t parseMap(Role role, size_t i) {
    const auto &line{lines[i]};
    auto &map{manifest.maps[int(role)]};
    map.role = role;
    map.lineNo = line.lineNo;
    if (!line.value.empty()) {
      map.file = parseFileName(line, parseString(line));
      return i + 1;
    }
    int blockIndent{0};
    auto blockSeen{std::map<std::string, int>{}};
    size_t j{i + 1};
    for (; j < lines.size() && lines[j].indent > 0; j++) {
      const auto &entry{lines[j]};
      if (blockIndent == 0)
        blockIndent = entry.indent;
      else if (entry.indent != blockIndent)
        fail(entry.lineNo, "inconsistent indentation");
      checkDuplicate(blockSeen, entry.key, entry.lineNo);
      parseMapEntry(role, map, entry);
    }
    if (blockIndent == 0)
      fail(line.lineNo,
           concat("expected a filename or an indented block after ",
                  Quoted(line.key.str()), ":"));
    if (map.file.empty())
      fail(line.lineNo, concat("missing 'file' for ", Quoted(line.key.str())));
    return j;
  }

  void parseMapEntry(Role role, Map &map, const Line &entry) {
    const char *roleStr{PBRMaps::roleName(role)};
    if (entry.value.empty())
      fail(entry.lineNo,
           concat("expected a value for ", Quoted(entry.key.str())));
    if (entry.key == "file") {
      map.file = parseFileName(entry, parseString(entry));
    } else if (entry.key == "channel") {
      if (!is_scalar_role(role))
        fail(entry.lineNo,
             concat("'channel' is not allowed for '", roleStr, "'"));
      map.channel = llvm::StringSwitch<PBRMaps::Channel>(entry.value)
                        .Case("r", PBRMaps::CHANNEL_R)
                        .Case("g", PBRMaps::CHANNEL_G)
                        .Case("b", PBRMaps::CHANNEL_B)
                        .Case("a", PBRMaps::CHANNEL_A)
                        .Default(PBRMaps::CHANNEL_NATURAL);
      if (map.channel == PBRMaps::CHANNEL_NATURAL)
        fail(entry.lineNo, "expected 'r', 'g', 'b', or 'a' for 'channel'");
    } else if (entry.key == "color_space") {
      if (entry.value == "srgb")
        map.colorSpace = PBRMaps::COLOR_SPACE_SRGB;
      else if (entry.value == "linear")
        map.colorSpace = PBRMaps::COLOR_SPACE_LINEAR;
      else
        fail(entry.lineNo, "expected 'srgb' or 'linear' for 'color_space'");
    } else if (entry.key == "convention") {
      if (role != PBRMaps::ROLE_NORMAL)
        fail(entry.lineNo,
             concat("'convention' is not allowed for '", roleStr, "'"));
      if (entry.value == "opengl")
        map.normalConvention = PBRMaps::NORMAL_CONVENTION_OPENGL;
      else if (entry.value == "directx")
        map.normalConvention = PBRMaps::NORMAL_CONVENTION_DIRECTX;
      else if (entry.value == "opengl_xy")
        map.normalConvention = PBRMaps::NORMAL_CONVENTION_OPENGL_XY;
      else if (entry.value == "directx_xy")
        map.normalConvention = PBRMaps::NORMAL_CONVENTION_DIRECTX_XY;
      else if (entry.value == "xy")
        fail(entry.lineNo, "'xy' is ambiguous: use 'opengl_xy' (Y up) or "
                           "'directx_xy' (Y flipped)");
      else
        fail(entry.lineNo, "expected 'opengl', 'directx', 'opengl_xy', or "
                           "'directx_xy' for 'convention'");
    } else if (entry.key == "max_slope") {
      if (role != PBRMaps::ROLE_HORIZON)
        fail(entry.lineNo,
             concat("'max_slope' is not allowed for '", roleStr, "'"));
      auto maxSlope{parseFloat(entry, entry.value)};
      if (!(maxSlope > 0))
        fail(entry.lineNo, "expected a positive real for 'max_slope'");
      map.maxSlope = maxSlope;
    } else if (entry.key == "classes") {
      // The schema used to make the class map name its classes, and the
      // names decided how many weights came out of a lookup. Say what
      // replaced it rather than reporting an unknown key, since a
      // manifest carrying the old spelling is a manifest whose author
      // is owed the new rule.
      fail(entry.lineNo,
           "'classes' is retired: a class map carries up to four classes "
           "in R, G, B, and the remainder, and what they mean belongs "
           "beside the map (see the generator's '.json' sidecar) rather "
           "than in this manifest");
    } else {
      fail(entry.lineNo, concat("unknown key ", Quoted(entry.key.str())));
    }
  }

  [[nodiscard]] std::string parseString(const Line &line) {
    auto value{line.value};
    if (!value.consume_front("\"")) return value.str();
    auto result{std::string{}};
    bool inEscape{false};
    for (size_t i = 0; i < value.size(); i++) {
      char ch{value[i]};
      if (inEscape) {
        if (ch != '"' && ch != '\\')
          fail(line.lineNo, "invalid escape (only '\\\"' and '\\\\')");
        result += ch;
        inEscape = false;
      } else if (ch == '\\') {
        inEscape = true;
      } else if (ch == '"') {
        if (i + 1 != value.size())
          fail(line.lineNo, "unexpected text after string");
        return result;
      } else {
        result += ch;
      }
    }
    fail(line.lineNo, "unterminated string");
  }

  [[nodiscard]] std::string parseFileName(const Line &line, std::string file) {
    if (file.empty()) fail(line.lineNo, "expected a filename");
    if (file.find('\\') != std::string::npos)
      fail(line.lineNo,
           concat("invalid filename ", Quoted(file), " (use forward slashes)"));
    if (file[0] == '/' || file[0] == '~' ||
        (file.size() > 1 && std::isalpha(uint8_t(file[0])) && file[1] == ':'))
      fail(line.lineNo,
           concat("invalid filename ", Quoted(file),
                  " (must be relative to the manifest directory)"));
    auto remainder{llvm::StringRef(file)};
    while (!remainder.empty()) {
      auto [component, rest] = remainder.split('/');
      remainder = rest;
      if (component == "..")
        fail(line.lineNo, concat("invalid filename ", Quoted(file),
                                 " (must not contain '..')"));
    }
    return file;
  }

  [[nodiscard]] float parseFloat(const Line &line, llvm::StringRef value) {
    auto str{value.str()};
    char *strEnd{};
    auto result{std::strtod(str.c_str(), &strEnd)};
    if (str.empty() || strEnd != str.c_str() + str.size() ||
        !std::isfinite(result))
      fail(line.lineNo,
           concat("expected a real number for ", Quoted(line.key.str())));
    return float(result);
  }

  [[nodiscard]] long parseInt(const Line &line) {
    auto str{line.value.str()};
    char *strEnd{};
    auto result{std::strtol(str.c_str(), &strEnd, 10)};
    if (str.empty() || strEnd != str.c_str() + str.size())
      fail(line.lineNo,
           concat("expected an integer for ", Quoted(line.key.str())));
    return result;
  }

  [[nodiscard]] llvm::SmallVector<llvm::StringRef, 4>
  parseList(const Line &line) {
    auto value{line.value};
    if (!value.consume_front("[") || !value.consume_back("]"))
      fail(line.lineNo, concat("expected an inline list '[...]' for ",
                               Quoted(line.key.str())));
    if (value.contains('[') || value.contains(']'))
      fail(line.lineNo, "nested lists are not supported");
    auto items{llvm::SmallVector<llvm::StringRef, 4>{}};
    if (value.trim().empty()) return items;
    auto parts{llvm::SmallVector<llvm::StringRef, 4>{}};
    value.split(parts, ',', /*MaxSplit=*/-1, /*KeepEmpty=*/true);
    for (auto part : parts) {
      part = part.trim();
      if (part.empty()) fail(line.lineNo, "empty list item");
      items.push_back(part);
    }
    return items;
  }

private:
  PBRMaps &manifest;

  llvm::StringRef sourceCode{};

  const std::string &sourceName;

  std::vector<Line> lines{};

  std::map<std::string, int> topLevelSeen{};
};

} // namespace

void PBRMaps::clear() noexcept {
  version = 1;
  name.clear();
  description.clear();
  physicalExtent = std::nullopt;
  physicalRelief = std::nullopt;
  reliefScale = std::nullopt;
  hexRotation = 0.0f;
  for (auto &map : maps) map = Map{};
  warnings.clear();
}

std::optional<Error> PBRMaps::parse(std::string_view sourceCode,
                                    const std::string &sourceName) noexcept {
  clear();
  return catchAndReturnError([&] {
    auto sourceNameOrDefault{sourceName.empty() ? std::string("<string>")
                                                : sourceName};
    ManifestParser(*this, llvm::StringRef(sourceCode.data(), sourceCode.size()),
                   sourceNameOrDefault)
        .parse();
  });
}

std::optional<Error>
PBRMaps::loadFromFile(const std::string &fileName) noexcept {
  clear();
  return catchAndReturnError([&] {
    auto sourceCode{readOrThrow(fileName)};
    ManifestParser(*this, sourceCode, fileName).parse();
  });
}

std::string PBRMaps::resolveFileName(const std::string &path) {
  if (!isDirectory(path)) return path;
  auto candidates{std::vector<std::string>{}};
  auto errorCode{std::error_code{}};
  auto dirItr{std::filesystem::directory_iterator(path, errorCode)};
  if (errorCode)
    throw Error(concat("cannot resolve PBR maps ", QuotedPath(path), ": ",
                       errorCode.message()));
  for (const auto &entry : dirItr) {
    if (entry.is_regular_file(errorCode) && entry.path().extension() == ".pbr")
      candidates.push_back(entry.path().string());
  }
  if (candidates.empty())
    throw Error(concat("cannot resolve PBR maps ", QuotedPath(path),
                       ": no '.pbr' manifest"));
  if (candidates.size() > 1) {
    std::sort(candidates.begin(), candidates.end());
    auto message{concat("cannot resolve PBR maps ", QuotedPath(path),
                        ": more than one '.pbr' manifest:")};
    for (const auto &candidate : candidates)
      message += concat("\n  ", QuotedPath(candidate));
    throw Error(std::move(message));
  }
  return candidates[0];
}

const char *PBRMaps::roleName(Role role) noexcept {
  switch (role) {
  case ROLE_BASECOLOR:
    return "basecolor";
  case ROLE_NORMAL:
    return "normal";
  case ROLE_ROUGHNESS:
    return "roughness";
  case ROLE_METALLIC:
    return "metallic";
  case ROLE_HEIGHT:
    return "height";
  case ROLE_EMISSION:
    return "emission";
  case ROLE_HORIZON:
    return "horizon";
  case ROLE_CLASS:
    return "class";
  default:
    break;
  }
  return "unknown";
}

} // namespace smdl
