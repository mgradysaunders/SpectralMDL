#include "Asset.h"

#include <algorithm>
#include <filesystem>
#include <fstream>
#include <set>

#include "smdl/Support/Error.h"
#include "smdl/Support/Logger.h"

namespace {

// The manifest parser. The grammar is the `key: value` scalar subset of the
// one `smdl::PBRMaps` documents; everything outside it is a line-numbered
// error.
class AssetParser final {
public:
  AssetParser(Asset &asset, std::string_view sourceCode,
              const std::string &fileName)
      : mAsset(asset), mSourceCode(sourceCode), mFileName(fileName) {}

  void parse() {
    lexLines();
    size_t i{};
    while (i < mLines.size()) {
      const auto &line{mLines[i]};
      if (line.indent != 0 || line.isItem)
        fail(line.lineNo, "unexpected indentation");
      if (!mSeen.insert(std::string(line.key)).second)
        fail(line.lineNo,
             smdl::concat("duplicate key ", smdl::Quoted(line.key)));
      if (line.key == "objects") {
        if (!line.value.empty())
          fail(line.lineNo, "expected 'objects:' alone, with the objects on "
                            "the indented lines below it");
        i = parseObjects(i + 1);
      } else {
        parseTopLevel(line);
        i++;
      }
    }
    if (mAsset.renderFileName.empty())
      fail(mLines.empty() ? 1 : mLines.back().lineNo,
           "expected a 'render' key naming the mesh");
    // A Y-up file gets a quarter turn about X to stand up in the Z-up
    // world; the uniform unit scale commutes with it, so both fold into
    // one matrix.
    mAsset.correction = float4x4(1.0f);
    if (mUpIsY) {
      mAsset.correction[1] = float4(0, 0, 1, 0);
      mAsset.correction[2] = float4(0, -1, 0, 0);
    }
    for (size_t j = 0; j < 3; j++)
      mAsset.correction[j] = mScale * mAsset.correction[j];
  }

private:
  // One content line, comments stripped and split at the first `:`. A `- `
  // prefix begins a sequence item; deeper-indented keys after it belong to
  // the same item.
  struct Line final {
    size_t lineNo{};
    size_t indent{};
    bool isItem{};
    std::string_view key{};
    std::string_view value{};
  };

  void lexLines() {
    auto remainder{mSourceCode};
    if (remainder.substr(0, 3) == "\xEF\xBB\xBF") // UTF-8 BOM
      remainder.remove_prefix(3);
    size_t lineNo{};
    while (!remainder.empty()) {
      auto text{remainder.substr(0, remainder.find('\n'))};
      remainder.remove_prefix(std::min(text.size() + 1, remainder.size()));
      lineNo++;
      if (!text.empty() && text.back() == '\r') text.remove_suffix(1);
      size_t indent{};
      while (indent < text.size() && text[indent] == ' ') indent++;
      if (indent < text.size() && text[indent] == '\t')
        fail(lineNo, "tab in indentation (use spaces)");
      auto content{trim(stripComment(text.substr(indent)))};
      if (content.empty()) continue;
      bool isItem{};
      if (content.substr(0, 2) == "- ") {
        isItem = true;
        indent += 2;
        content = trim(content.substr(2));
      } else if (content == "-") {
        fail(lineNo, "expected a key after '-'");
      }
      auto colon{content.find(':')};
      if (colon == std::string_view::npos)
        fail(lineNo, "expected 'key: value'");
      auto key{trim(content.substr(0, colon))};
      auto value{trim(content.substr(colon + 1))};
      if (key.empty()) fail(lineNo, "expected 'key: value'");
      mLines.push_back(Line{lineNo, indent, isItem, key, value});
    }
  }

  void parseTopLevel(const Line &line) {
    if (line.value.empty())
      fail(line.lineNo,
           smdl::concat("expected a value for ", smdl::Quoted(line.key)));
    if (line.key == "asset") {
      if (line.value != "1")
        fail(line.lineNo, smdl::concat("unsupported manifest version ",
                                       smdl::Quoted(line.value),
                                       " (this build supports version 1)"));
    } else if (line.key == "name") {
      mAsset.name = unquote(line.value);
    } else if (line.key == "render") {
      mAsset.renderFileName = checkRelative(line.lineNo, unquote(line.value));
    } else if (line.key == "proxy") {
      mAsset.proxyFileName = checkRelative(line.lineNo, unquote(line.value));
    } else if (line.key == "up") {
      if (line.value != "y" && line.value != "z")
        fail(line.lineNo, smdl::concat("expected 'y' or 'z' for 'up', got ",
                                       smdl::Quoted(line.value)));
      mUpIsY = line.value == "y";
    } else if (line.key == "scale") {
      mScale = number(line.lineNo, line.value);
      if (!(mScale > 0))
        fail(line.lineNo, "expected a positive number for 'scale'");
    } else if (line.key == "front") {
      mAsset.front = number(line.lineNo, line.value);
    } else if (line.key == "materials") {
      for (const auto &item : list(line))
        mAsset.materials.push_back(unquote(item));
    } else {
      fail(line.lineNo, smdl::concat("unknown key ", smdl::Quoted(line.key),
                                     " (expected asset, name, render, proxy, "
                                     "up, scale, front, materials, or "
                                     "objects)"));
    }
  }

  // The block sequence under `objects:`. Returns the index of the next
  // top-level line.
  [[nodiscard]] size_t parseObjects(size_t i) {
    auto seen{std::set<std::string, std::less<>>()};
    size_t itemIndent{};
    for (; i < mLines.size() && mLines[i].indent > 0; i++) {
      const auto &line{mLines[i]};
      if (line.isItem) {
        mAsset.objects.emplace_back();
        itemIndent = line.indent;
        seen.clear();
      } else if (mAsset.objects.empty()) {
        fail(line.lineNo, "expected '- ' to begin the first object");
      } else if (line.indent < itemIndent) {
        fail(line.lineNo, "expected the keys of one object to line up under "
                          "the '- ' that begins it");
      }
      if (!seen.insert(std::string(line.key)).second)
        fail(line.lineNo, smdl::concat("duplicate key ", smdl::Quoted(line.key),
                                       " in the same object"));
      parseObjectKey(line, mAsset.objects.back());
    }
    if (mAsset.objects.empty()) fail(mLines[i - 1].lineNo, "expected objects");
    for (const auto &object : mAsset.objects)
      if (object.select.empty())
        fail(mLines[i - 1].lineNo,
             "expected every object to have a 'select' key");
    return i;
  }

  void parseObjectKey(const Line &line, Asset::Object &object) {
    if (line.value.empty())
      fail(line.lineNo,
           smdl::concat("expected a value for ", smdl::Quoted(line.key)));
    if (line.key == "select") {
      object.select = unquote(line.value);
    } else if (line.key == "materials") {
      for (const auto &item : list(line))
        object.materials.push_back(unquote(item));
    } else if (line.key == "pivot") {
      auto items{list(line)};
      if (items.size() != 3)
        fail(line.lineNo, "expected three numbers for 'pivot'");
      for (size_t axis = 0; axis < 3; axis++)
        object.pivot[axis] = number(line.lineNo, items[axis]);
    } else if (line.key == "triangles") {
      object.triangleCount = uint64_t(number(line.lineNo, line.value));
    } else {
      fail(line.lineNo,
           smdl::concat("unknown object key ", smdl::Quoted(line.key),
                        " (expected select, materials, pivot, or triangles)"));
    }
  }

  // An inline list `[a, b, c]`, the only bracketed form in the subset.
  [[nodiscard]] std::vector<std::string_view> list(const Line &line) const {
    auto text{line.value};
    if (text.size() < 2 || text.front() != '[' || text.back() != ']')
      fail(line.lineNo,
           smdl::concat("expected a list in brackets for ",
                        smdl::Quoted(line.key), ", got ", smdl::Quoted(text)));
    text = trim(text.substr(1, text.size() - 2));
    auto items{std::vector<std::string_view>()};
    if (text.empty()) return items;
    for (size_t pos{}; pos <= text.size();) {
      auto comma{text.find(',', pos)};
      if (comma == std::string_view::npos) comma = text.size();
      items.push_back(trim(text.substr(pos, comma - pos)));
      pos = comma + 1;
    }
    return items;
  }

  [[noreturn]] void fail(size_t lineNo, std::string_view message) const {
    throw smdl::Error(smdl::concat(mFileName, ":", lineNo, ": ", message));
  }

  // Strip a full-line or trailing comment, respecting double quotes.
  [[nodiscard]] static std::string_view stripComment(std::string_view text) {
    bool inQuotes{};
    for (size_t i = 0; i < text.size(); i++) {
      if (text[i] == '"') {
        inQuotes = !inQuotes;
      } else if (!inQuotes && text[i] == '#' &&
                 (i == 0 || text[i - 1] == ' ' || text[i - 1] == '\t')) {
        return text.substr(0, i);
      }
    }
    return text;
  }

  [[nodiscard]] static std::string_view trim(std::string_view text) {
    while (!text.empty() && (text.front() == ' ' || text.front() == '\t'))
      text.remove_prefix(1);
    while (!text.empty() && (text.back() == ' ' || text.back() == '\t'))
      text.remove_suffix(1);
    return text;
  }

  [[nodiscard]] static std::string unquote(std::string_view text) {
    if (text.size() >= 2 && text.front() == '"' && text.back() == '"')
      text = text.substr(1, text.size() - 2);
    return std::string(text);
  }

  [[nodiscard]] float number(size_t lineNo, std::string_view text) const {
    size_t used{};
    float value{};
    try {
      value = std::stof(std::string(text), &used);
    } catch (const std::exception &) {
      used = 0;
    }
    if (used != text.size())
      fail(lineNo, smdl::concat("expected a number, got ", smdl::Quoted(text)));
    return value;
  }

  // A manifest may only name files inside its own package, keeping asset
  // directories relocatable; the same rule the `.pbr` manifests follow.
  [[nodiscard]] std::string checkRelative(size_t lineNo,
                                          std::string file) const {
    if (file.empty()) fail(lineNo, "expected a file name");
    if (file[0] == '/' || file[0] == '~' || file.find('\\') != file.npos ||
        (file.size() > 1 && file[1] == ':'))
      fail(lineNo, smdl::concat("expected a relative file name, got ",
                                smdl::QuotedPath(file)));
    for (const auto &part : std::filesystem::path(file))
      if (part == "..")
        fail(lineNo, smdl::concat("expected a file name inside the asset "
                                  "directory, got ",
                                  smdl::QuotedPath(file)));
    return file;
  }

  Asset &mAsset;
  std::string_view mSourceCode{};
  const std::string &mFileName;
  std::vector<Line> mLines{};
  std::set<std::string, std::less<>> mSeen{};
  bool mUpIsY{};
  float mScale{1.0f};
};

} // namespace

Asset readAsset(const std::string &fileName) {
  auto stream{std::ifstream(fileName, std::ios::binary)};
  if (!stream)
    throw smdl::Error(smdl::concat("cannot open asset manifest ",
                                   smdl::QuotedPath(fileName)));
  auto sourceCode{std::string(std::istreambuf_iterator<char>(stream),
                              std::istreambuf_iterator<char>())};
  auto asset{Asset()};
  AssetParser(asset, sourceCode, fileName).parse();
  auto directory{std::filesystem::path(fileName).parent_path()};
  auto renderPath{directory / asset.renderFileName};
  if (!std::filesystem::exists(renderPath))
    throw smdl::Error(smdl::concat(fileName, ": the 'render' mesh ",
                                   smdl::QuotedPath(asset.renderFileName),
                                   " does not exist in ",
                                   smdl::QuotedPath(directory.string())));
  asset.renderFileName = renderPath.string();
  if (!asset.proxyFileName.empty())
    asset.proxyFileName = (directory / asset.proxyFileName).string();
  SMDL_LOG_DEBUG("Read ", smdl::QuotedPath(fileName), ": render ",
                 smdl::QuotedPath(asset.renderFileName), ", ",
                 asset.objects.size(), " object(s)");
  return asset;
}

std::string findAssetManifest(const std::string &directory) {
  auto errorCode{std::error_code{}};
  auto dirItr{std::filesystem::directory_iterator(directory, errorCode)};
  if (errorCode)
    throw smdl::Error(smdl::concat("cannot read directory ",
                                   smdl::QuotedPath(directory), ": ",
                                   errorCode.message()));
  auto candidates{std::vector<std::string>()};
  for (const auto &entry : dirItr)
    if (entry.is_regular_file(errorCode) &&
        entry.path().extension() == ASSET_EXTENSION)
      candidates.push_back(entry.path().string());
  if (candidates.empty()) return {};
  if (candidates.size() > 1) {
    std::sort(candidates.begin(), candidates.end());
    auto message{smdl::concat("cannot resolve asset ",
                              smdl::QuotedPath(directory),
                              ": more than one '.asset' manifest:")};
    for (const auto &candidate : candidates)
      message += smdl::concat("\n  ", smdl::QuotedPath(candidate));
    throw smdl::Error(std::move(message));
  }
  return candidates[0];
}
