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
  return info;
}

std::string BuildInfo::toString() const {
  return concat("SpectralMDL ", major, ".", minor, ".", patch,           //
                " (", gitBranch, ", commit ", gitCommit, ")\n",          //
                "  built:      ", buildDate,                             //
                hasRTTI ? " (RTTI on)\n" : " (RTTI off)\n",              //
                "  LLVM:       ", llvmVersion, "\n",                     //
                "  Ptex:       ", withPtex ? withPtex : "off", "\n",     //
                "  NanoVDB:    ",                                        //
                withNanoVDB ? concat("OpenVDB ", withNanoVDB)            //
                            : std::string("off"),                        //
                "\n",                                                    //
                "  vendored:   miniz ", withMiniz,                       //
                ", stb_image ", withSTBImage,                            //
                ", stb_image_write ", withSTBImageWrite, ",\n",          //
                "              stb_image_resize2 ", withSTBImageResize,  //
                ", tinyexr ", withTinyEXR, "\n");
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

void SourceLocation::logWarn(std::string_view message) const {
  auto str{std::string(*this)};
  if (!str.empty()) str += ' ';
  str += message;
  SMDL_LOG_WARN(str);
}

void SourceLocation::logError(std::string_view message) const {
  auto str{std::string(*this)};
  if (!str.empty()) str += ' ';
  str += message;
  SMDL_LOG_ERROR(str);
}

void SourceLocation::throwError(std::string message) const {
  auto str{std::string(*this)};
  if (!str.empty()) str += ' ';
  str += message;
  throw Error(std::move(str));
}

SourceLocation::operator std::string() const {
  std::string str{};
  if (module_) {
    str += '[';
    if (module_->isBuiltin()) {
      str += module_->getName();
    } else {
      str += bestPathForPrinting(std::string(module_->getFileName()));
    }
    str += ':';
    str += std::to_string(lineNo);
    str += ']';
  }
  return str;
}

void State::finalizeAndApplyInternalSpaceConventions() noexcept {
  // 1. Orthonormalize normal and tangent vectors.
  if (!tryNormalize(normal)) normal = {0, 0, 1};
  for (int i = 0; i < texture_space_max; i++)
    gramSchmidtOrthonormalize(normal, texture_tangent_u[i],
                              texture_tangent_v[i]);

  // 2. Orthonormalize geometry normal and tangent vectors.
  if (!tryNormalize(geometry_normal)) geometry_normal = normal;
  for (int i = 0; i < texture_space_max; i++)
    gramSchmidtOrthonormalize(geometry_normal, geometry_tangent_u[i],
                              geometry_tangent_v[i]);

  // 3. Construct the tangent-to-object matrix.
  tangent_to_object_matrix[0] = float4(geometry_tangent_u[0], 0.0f);
  tangent_to_object_matrix[1] = float4(geometry_tangent_v[0], 0.0f);
  tangent_to_object_matrix[2] = float4(geometry_normal, 0.0f);
  tangent_to_object_matrix[3] = float4(position, 1.0f);

  // 4. Transform everything from object space to tangent space.
  auto object_to_tangent_matrix{affineInverse(tangent_to_object_matrix)};
  position = {};
  direction = object_to_tangent_matrix * float4(direction, 0.0f);
  motion = object_to_tangent_matrix * float4(motion, 0.0f);
  normal = object_to_tangent_matrix * float4(normal, 0.0f);
  geometry_normal = {0, 0, 1};
  for (int i = 0; i < texture_space_max; i++) {
    texture_tangent_u[i] =
        object_to_tangent_matrix * float4(texture_tangent_u[i], 0.0f);
    texture_tangent_v[i] =
        object_to_tangent_matrix * float4(texture_tangent_v[i], 0.0f);
    geometry_tangent_u[i] =
        object_to_tangent_matrix * float4(geometry_tangent_u[i], 0.0f);
    geometry_tangent_v[i] =
        object_to_tangent_matrix * float4(geometry_tangent_v[i], 0.0f);
  }

  // 5. Orthonormalize object-to-world matrix. A host that needs to know
  // what this leaves behind can call `orthonormalize()` on the same matrix
  // and get the same answer, bit for bit.
  auto axes{orthonormalize(float3x3(float3(object_to_world_matrix[0]),
                                    float3(object_to_world_matrix[1]),
                                    float3(object_to_world_matrix[2])))};
  object_to_world_matrix[0] = float4(axes[0], 0.0f);
  object_to_world_matrix[1] = float4(axes[1], 0.0f);
  object_to_world_matrix[2] = float4(axes[2], 0.0f);
}

} // namespace smdl
