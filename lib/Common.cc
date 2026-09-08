#include "smdl/Common.h"
#include "smdl/Module.h"
#include "smdl/Support/Logger.h"

#include "llvm/ADT/StringRef.h"
#include "llvm/Config/llvm-config.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/WithColor.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/TargetParser/Host.h"

#include "thirdparty/Versions.h"
#include "thirdparty/miniz.h"

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
      {"tinyexr", info.withTinyEXR},
      {"Ptex", info.withPtex ? info.withPtex : "off"},
      {"NanoVDB", info.withNanoVDB ? info.withNanoVDB : "off"},
  };
  return info;
}

std::string BuildInfo::toString() const {
  auto result{concat("SpectralMDL ", major, ".", minor, ".", patch,  //
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
    auto item{thirdparty[i].name + ' ' + thirdparty[i].version};
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

const NativeTarget &NativeTarget::get() noexcept {
  // Lazy magic static: initializing LLVM at static-initialization time
  // would run before 'main' in every process linking the library and be
  // exposed to static-init-order hazards.
  static const NativeTarget nativeTarget{[]() {
    // Both of these return true on failure, which happens if the LLVM we
    // linked has no code generator for this machine. CMake is supposed to
    // have guaranteed otherwise, so say so plainly here instead of letting it
    // resurface as a baffling 'lookupTarget' failure below.
    if (llvm::InitializeNativeTarget() ||
        llvm::InitializeNativeTargetAsmPrinter())
      llvm::report_fatal_error("LLVM has no code generator for this machine");
    std::string name{llvm::sys::getHostCPUName()};
    std::string triple{llvm::sys::getDefaultTargetTriple()};
    auto targetError{std::string{}};
    auto target{
        llvm::TargetRegistry::lookupTarget(llvm::Triple(triple), targetError)};
    if (!target) llvm::report_fatal_error(targetError.c_str());
    llvm::TargetOptions opts{};
    return NativeTarget{name, triple,
                        target->createTargetMachine(llvm::Triple(triple), name,
                                                    "", opts,
                                                    llvm::Reloc::PIC_)};
  }()};
  return nativeTarget;
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
  auto sourceCode{module_ ? module_->getSourceCode() : std::string_view()};
  if (sourceCode.empty()) return {};
  // An error raised at EOF has no character to point at, so clamp and let
  // the caret land one past the end of the last line.
  auto pos{i < sourceCode.size() ? size_t(i) : sourceCode.size()};
  auto lineBegin{sourceCode.rfind('\n', pos)};
  lineBegin = lineBegin == std::string_view::npos ? 0 : lineBegin + 1;
  auto lineEnd{sourceCode.find('\n', pos)};
  lineEnd = lineEnd == std::string_view::npos ? sourceCode.size() : lineEnd;
  auto line{sourceCode.substr(lineBegin, lineEnd - lineBegin)};
  if (!line.empty() && line.back() == '\r') line.remove_suffix(1);
  auto column{pos - lineBegin};
  if (line.empty() || column > line.size()) return {};
  auto gutter{std::to_string(lineNo)};
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

void SourceLocation::logWarn(std::string_view message) const {
  // No source snippet: warnings come in bulk and mostly name what they are
  // about, so the caret costs more in noise than it returns in clarity.
  auto str{std::string(*this)};
  if (!str.empty()) str += ' ';
  str += message;
  SMDL_LOG_WARN(str);
}

void SourceLocation::logError(std::string_view message) const {
  auto str{std::string(*this)};
  if (!str.empty()) str += ' ';
  str += message;
  str += getSourceSnippet();
  SMDL_LOG_ERROR(str);
}

void SourceLocation::throwError(std::string message) const {
  auto str{std::string(*this)};
  if (!str.empty()) str += ' ';
  str += message;
  throw Error(std::move(str), getSourceSnippet());
}

SourceLocation::operator std::string() const {
  std::string str{};
  if (module_) {
    str += '[';
    // A module with no file prints its origin markup verbatim, e.g.,
    // '<builtin ::df>'; only a real path is worth shortening.
    if (module_->isFileBacked()) {
      str += bestPathForPrinting(std::string(module_->getDisplayName()));
    } else {
      str += module_->getDisplayName();
    }
    str += ':';
    str += std::to_string(lineNo);
    str += ':';
    str += std::to_string(charNo);
    str += ']';
  }
  return str;
}

void State::finalize() noexcept {
  // 1. Every loop below indexes the tangent arrays by it, and so does the
  // generated code that reads them, so a host asking for more spaces than
  // are there is clamped once, here, rather than running off the end.
  textureSpaceCount = std::clamp(textureSpaceCount, 0, int(TEXTURE_SPACE_MAX));
  vertexColorCount = std::clamp(vertexColorCount, 0, int(VERTEX_COLOR_MAX));

  // 2. Orthonormalize normal and tangent vectors.
  if (!tryNormalize(normal)) normal = {0, 0, 1};
  for (int i = 0; i < textureSpaceCount; i++)
    gramSchmidtOrthonormalize(normal, textureTangentU[i], textureTangentV[i]);

  // 3. Orthonormalize geometry normal and tangent vectors.
  if (!tryNormalize(geometryNormal)) geometryNormal = normal;
  for (int i = 0; i < textureSpaceCount; i++)
    gramSchmidtOrthonormalize(geometryNormal, geometryTangentU[i],
                              geometryTangentV[i]);

  // 4. Orthonormalize object-to-world matrix. An already orthonormal one
  // is left exactly as the host set it; otherwise this is `orthonormalize()`
  // of it, which a host can call to predict the answer bit for bit.
  //
  // The matrix is a per-instance constant that arrives again at every
  // shading point, and a renderer that hands over the rigid frame it
  // already derived takes the first branch every time, so the six dot
  // products that recognize the case are worth their cost against the
  // three square roots and six divides they skip.
  const auto axisX{float3(objectToWorld[0])};
  const auto axisY{float3(objectToWorld[1])};
  const auto axisZ{float3(objectToWorld[2])};
  constexpr float ORTHONORMAL_EPS = 1e-6f;
  const auto isOrthonormal{[&] {
    return std::abs(lengthSquared(axisX) - 1) < ORTHONORMAL_EPS &&
           std::abs(lengthSquared(axisY) - 1) < ORTHONORMAL_EPS &&
           std::abs(lengthSquared(axisZ) - 1) < ORTHONORMAL_EPS &&
           std::abs(dot(axisX, axisY)) < ORTHONORMAL_EPS &&
           std::abs(dot(axisX, axisZ)) < ORTHONORMAL_EPS &&
           std::abs(dot(axisY, axisZ)) < ORTHONORMAL_EPS;
  }};
  if (!isOrthonormal()) {
    auto axes{orthonormalize(float3x3(axisX, axisY, axisZ))};
    objectToWorld[0] = float4(axes[0], 0.0f);
    objectToWorld[1] = float4(axes[1], 0.0f);
    objectToWorld[2] = float4(axes[2], 0.0f);
  }

  // 5 and 6.
  finalizeUnchecked();
}

} // namespace smdl
