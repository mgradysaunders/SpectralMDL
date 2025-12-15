#include "smdl/Common.h"
#include "smdl/Module.h"
#include "smdl/Support/Logger.h"

#include "llvm/ADT/StringRef.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/WithColor.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/TargetParser/Host.h"

namespace smdl {

BuildInfo BuildInfo::get() noexcept {
  return {SMDL_VERSION_MAJOR, //
          SMDL_VERSION_MINOR, //
          SMDL_VERSION_PATCH, //
          SMDL_GIT_BRANCH,    //
          SMDL_GIT_COMMIT};
}

static const NativeTarget nativeTarget{[]() {
  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmPrinter();
  std::string name{llvm::sys::getHostCPUName()};
  std::string triple{llvm::sys::getDefaultTargetTriple()};
  auto targetError{std::string{}};
  auto target{llvm::TargetRegistry::lookupTarget(triple, targetError)};
  if (!target)
    llvm::report_fatal_error(targetError.c_str());
  llvm::TargetOptions opts{};
  return NativeTarget{name, triple,
                      target->createTargetMachine(llvm::Triple(triple), name,
                                                  "", opts, llvm::Reloc::PIC_)};
}()};

const NativeTarget &NativeTarget::get() noexcept { return nativeTarget; }

std::string_view SourceLocation::getModuleName() const {
  return module_ ? module_->getName() : std::string_view();
}

std::string_view SourceLocation::getModuleFileName() const {
  return module_ ? module_->getFileName() : std::string_view();
}

void SourceLocation::logWarn(std::string_view message) const {
  auto str{std::string(*this)};
  if (!str.empty())
    str += ' ';
  str += message;
  SMDL_LOG_WARN(str);
}

void SourceLocation::logError(std::string_view message) const {
  auto str{std::string(*this)};
  if (!str.empty())
    str += ' ';
  str += message;
  SMDL_LOG_ERROR(str);
}

void SourceLocation::throwError(std::string message) const {
  auto str{std::string(*this)};
  if (!str.empty())
    str += ' ';
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
      str += makePathRelative(std::string(module_->getFileName()));
    }
    str += ':';
    str += std::to_string(lineNo);
    str += ']';
  }
  return str;
}

static void gramSchmidtOrthonormalize(const float3 &normal, float3 &tangentU,
                                      float3 &tangentV) {
  tangentU = tangentU - dot(tangentU, normal) * normal;
  if (!tryNormalize(tangentU)) {
    tangentU = normalize(perpendicularTo(normal));
  }
  tangentV = tangentV - dot(tangentV, normal) * normal -
             dot(tangentV, tangentU) * tangentU;
  if (!tryNormalize(tangentV)) {
    tangentV = normalize(cross(normal, tangentU));
  }
}

void State::finalizeAndApplyInternalSpaceConventions() noexcept {
  // 1. Orthonormalize normal and tangent vectors.
  if (!tryNormalize(normal))
    normal = {0, 0, 1};
  for (int i = 0; i < texture_space_max; i++)
    gramSchmidtOrthonormalize(normal, texture_tangent_u[i],
                              texture_tangent_v[i]);

  // 2. Orthonormalize geometry normal and tangent vectors.
  if (!tryNormalize(geometry_normal))
    geometry_normal = normal;
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
  /* direction = object_to_tangent_matrix * float4(direction, 0.0f); */
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

  // 5. Orthonormalize object-to-world matrix.
  float3 axisX{object_to_world_matrix[0]};
  float3 axisY{object_to_world_matrix[1]};
  float3 axisZ{object_to_world_matrix[2]};
  if (!tryNormalize(axisZ))
    axisZ = {0, 0, 1};
  gramSchmidtOrthonormalize(axisZ, axisX, axisY);
  object_to_world_matrix[0] = float4(axisX, 0.0f);
  object_to_world_matrix[1] = float4(axisY, 0.0f);
  object_to_world_matrix[2] = float4(axisZ, 0.0f);
}

} // namespace smdl
