#include "IO/AssetFile.h"

#include <algorithm>
#include <filesystem>

#include "smdl/Support/Error.h"
#include "smdl/Support/Filesystem.h"
#include "smdl/Support/FlatYAML.h"
#include "smdl/Support/Logger.h"
#include "smdl/Support/Strings.h"

namespace {

using smdl::FlatYAML;

// The manifest reader over the shared flat YAML subset; everything the
// grammar accepts but the schema does not is a line-numbered error.
class AssetReader final {
public:
  AssetReader(AssetFile &asset, const FlatYAML &doc)
      : mAsset(asset), mDoc(doc) {}

  void read() {
    for (const auto &entry : mDoc.root) {
      const auto &key{entry.key};
      if (key == "asset") {
        auto version{mDoc.toInt(entry)};
        if (version != 1)
          mDoc.fail(entry,
                    smdl::concat("unsupported manifest version ", version,
                                 " (this build supports version 1)"));
      } else if (key == "name") {
        mAsset.name = mDoc.toString(entry);
      } else if (key == "render") {
        mAsset.renderFileName = checkRelative(entry, mDoc.toString(entry));
      } else if (key == "proxy") {
        mAsset.proxyFileName = checkRelative(entry, mDoc.toString(entry));
      } else if (key == "up") {
        const auto &up{mDoc.toString(entry)};
        if (up != "y" && up != "z")
          mDoc.fail(entry, smdl::concat("expected 'y' or 'z' for 'up', got ",
                                        smdl::Quoted(up)));
        mUpIsY = up == "y";
      } else if (key == "scale") {
        mScale = mDoc.toFloat(entry);
        if (!(mScale > 0))
          mDoc.fail(entry, "expected a positive number for 'scale'");
      } else if (key == "front") {
        mAsset.front = mDoc.toFloat(entry);
      } else if (key == "materials") {
        mAsset.materials = names(entry);
      } else if (key == "objects") {
        readObjects(entry);
      } else {
        mDoc.fail(entry, smdl::concat("unknown key ", smdl::Quoted(key),
                                      " (expected asset, name, render, "
                                      "proxy, up, scale, front, materials, "
                                      "or objects)"));
      }
    }
    if (mAsset.renderFileName.empty())
      mDoc.fail(mDoc.root.empty() ? 1 : mDoc.root.back().lineNo,
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
  void readObjects(const FlatYAML::Entry &entry) {
    for (const auto &item : mDoc.toSequence(entry)) {
      auto &object{mAsset.objects.emplace_back()};
      for (const auto &sub : item) {
        const auto &key{sub.key};
        if (key == "select") {
          object.select = mDoc.toString(sub);
        } else if (key == "materials") {
          object.materials = names(sub);
        } else if (key == "pivot") {
          auto pivot{mDoc.toFloats(sub, 3)};
          object.pivot = float3(pivot[0], pivot[1], pivot[2]);
        } else if (key == "triangles") {
          object.triangleCount = uint64_t(std::max(0L, mDoc.toInt(sub)));
        } else {
          mDoc.fail(sub, smdl::concat("unknown object key ", smdl::Quoted(key),
                                      " (expected select, materials, pivot, "
                                      "or triangles)"));
        }
      }
      if (object.select.empty())
        mDoc.fail(item.empty() ? entry.lineNo : item.front().lineNo,
                  "expected every object to have a 'select' key");
    }
  }

  // An inline list of names.
  [[nodiscard]] std::vector<std::string>
  names(const FlatYAML::Entry &entry) const {
    auto result{std::vector<std::string>()};
    for (const auto &item : mDoc.toList(entry)) {
      if (item.kind != FlatYAML::Node::SCALAR)
        mDoc.fail(entry, smdl::concat("expected a list of names for ",
                                      smdl::Quoted(entry.key)));
      result.push_back(item.text);
    }
    return result;
  }

  // A manifest may only name files inside its own package, keeping asset
  // directories relocatable.
  [[nodiscard]] std::string checkRelative(const FlatYAML::Entry &entry,
                                          std::string file) const {
    if (file.empty()) mDoc.fail(entry, "expected a file name");
    if (file[0] == '/' || file[0] == '~' || file.find('\\') != file.npos ||
        (file.size() > 1 && file[1] == ':'))
      mDoc.fail(entry, smdl::concat("expected a relative file name, got ",
                                    smdl::QuotedPath(file)));
    for (const auto &part : std::filesystem::path(file))
      if (part == "..")
        mDoc.fail(entry, smdl::concat("expected a file name inside the asset "
                                      "directory, got ",
                                      smdl::QuotedPath(file)));
    return file;
  }

  AssetFile &mAsset;
  const FlatYAML &mDoc;
  bool mUpIsY{};
  float mScale{1.0f};
};

} // namespace

AssetFile readAssetFile(const std::string &fileName) {
  auto sourceCode{smdl::readOrThrow(fileName)};
  auto doc{FlatYAML::parse(sourceCode, fileName)};
  auto asset{AssetFile()};
  AssetReader(asset, doc).read();
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
