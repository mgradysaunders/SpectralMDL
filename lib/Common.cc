#include "smdl/Common.h"
#include "smdl/Module.h"
#include "smdl/Support/Logger.h"

#include "llvm/Config/llvm-config.h"

#include "thirdparty/Versions.h"
#include "thirdparty/miniz/miniz.h"

namespace smdl {

BuildInfo BuildInfo::get() noexcept {
  BuildInfo info{};
  info.major = SMDL_VERSION_MAJOR;
  info.minor = SMDL_VERSION_MINOR;
  info.patch = SMDL_VERSION_PATCH;
  info.gitBranch = SMDL_GIT_BRANCH;
  info.gitCommit = SMDL_GIT_COMMIT;
  info.llvmVersion = LLVM_VERSION_STRING;
  info.buildDate = __DATE__ " " __TIME__;
#if defined(__cpp_rtti) || defined(__GXX_RTTI) || defined(_CPPRTTI)
  info.hasRTTI = true;
#endif
#ifdef SMDL_DYNAMIC_SCHEDULING
  info.hasDynamicScheduling = SMDL_DYNAMIC_SCHEDULING;
#endif
  info.withMiniz = MZ_VERSION;
  info.withSTBImage = SMDL_STB_IMAGE_VERSION;
  info.withSTBImageWrite = SMDL_STB_IMAGE_WRITE_VERSION;
  info.withSTBImageResize = SMDL_STB_IMAGE_RESIZE_VERSION;
  info.withSTBSprintf = SMDL_STB_SPRINTF_VERSION;
  info.withTinyEXR = SMDL_TINYEXR_VERSION;
#ifdef SMDL_PTEX_VERSION
  info.withPtex = SMDL_PTEX_VERSION;
#endif
#ifdef SMDL_NANOVDB_VERSION
  info.withNanoVDB = SMDL_NANOVDB_VERSION;
#endif
  info.thirdparty = {
      {"LLVM", info.llvmVersion},
      {"miniz", info.withMiniz},
      {"stb_image", info.withSTBImage},
      {"stb_image_write", info.withSTBImageWrite},
      {"stb_image_resize2", info.withSTBImageResize},
      {"stb_sprintf", info.withSTBSprintf},
      {"tinyexr", info.withTinyEXR},
      {"Ptex", info.withPtex ? info.withPtex : "off"},
      {"NanoVDB", info.withNanoVDB ? info.withNanoVDB : "off"},
  };
  return info;
}

std::string BuildInfo::toString() const {
  std::string result{concat("SpectralMDL ", major, ".", minor, ".", patch,  //
                            " (", gitBranch, ", commit ", gitCommit, ")\n", //
                            "  built:      ", buildDate, "\n",              //
                            "  options:    rtti ", hasRTTI ? "on" : "off",  //
                            ", dynamic scheduling ",                        //
                            hasDynamicScheduling ? "on" : "off", "\n")};
  // The dependencies as one comma-separated list, greedily wrapped to
  // 80 columns under a hanging indent the width of the label.
  constexpr size_t COLUMNS{80};
  constexpr std::string_view LABEL{"  thirdparty: "};
  result += LABEL;
  size_t column{LABEL.size()};
  for (size_t i{}; i < thirdparty.size(); i++) {
    std::string item{thirdparty[i].name + ' ' + thirdparty[i].version};
    if (i + 1 < thirdparty.size()) item += ',';
    if (i > 0) {
      if (column + 1 + item.size() > COLUMNS) {
        result += '\n';
        result.append(LABEL.size(), ' ');
        column = LABEL.size();
      } else {
        result += ' ';
        column++;
      }
    }
    result += item;
    column += item.size();
  }
  result += '\n';
  return result;
}

std::string_view SourceLocation::getModuleName() const {
  return module_ ? module_->getName() : std::string_view();
}

std::string_view SourceLocation::getModuleFileName() const {
  return module_ ? module_->getFileName() : std::string_view();
}

std::string_view SourceLocation::getModuleDisplayName() const {
  return module_ ? module_->getDisplayName() : std::string_view();
}

std::string SourceLocation::getSourceSnippet() const {
  std::string_view sourceCode{module_ ? module_->getSourceCode()
                                      : std::string_view()};
  if (sourceCode.empty()) return {};
  // An error raised at EOF has no character to point at, so clamp and let
  // the caret land one past the end of the last line.
  size_t pos{i < sourceCode.size() ? size_t(i) : sourceCode.size()};
  size_t lineBegin{sourceCode.rfind('\n', pos)};
  lineBegin = lineBegin == std::string_view::npos ? 0 : lineBegin + 1;
  size_t lineEnd{sourceCode.find('\n', pos)};
  lineEnd = lineEnd == std::string_view::npos ? sourceCode.size() : lineEnd;
  std::string_view line{sourceCode.substr(lineBegin, lineEnd - lineBegin)};
  if (!line.empty() && line.back() == '\r') line.remove_suffix(1);
  size_t column{pos - lineBegin};
  if (line.empty() || column > line.size()) return {};
  std::string gutter{std::to_string(lineNo)};
  std::string str{};
  str += "\n  ";
  str += gutter;
  str += " | ";
  str += line;
  str += "\n  ";
  str.append(gutter.size(), ' ');
  str += " | ";
  // Copy the indentation verbatim so that a tab-indented line keeps the
  // caret under the right character.
  for (size_t j = 0; j < column; j++) str += line[j] == '\t' ? '\t' : ' ';
  str += '^';
  return str;
}

std::string SourceLocation::formatMessage(std::string_view message) const {
  std::string str{std::string(*this)};
  if (!str.empty()) str += ' ';
  str += message;
  return str;
}

void SourceLocation::logDebug(std::string_view message) const {
  SMDL_LOG_DEBUG(formatMessage(message));
}

void SourceLocation::logInfo(std::string_view message) const {
  SMDL_LOG_INFO(formatMessage(message));
}

void SourceLocation::logWarn(std::string_view message) const {
  // No source snippet: warnings come in bulk and mostly name what they are
  // about, so the caret costs more in noise than it returns in clarity.
  SMDL_LOG_WARN(formatMessage(message));
}

void SourceLocation::logError(std::string_view message) const {
  SMDL_LOG_ERROR(formatMessage(message), getSourceSnippet());
}

void SourceLocation::throwError(std::string message) const {
  throw Error(formatMessage(message), getSourceSnippet());
}

SourceLocation::operator std::string() const {
  if (!module_) return {};
  return concat(SpellLocation(module_->getDisplayName(), lineNo, charNo,
                              module_->isFileBacked()));
}

} // namespace smdl
