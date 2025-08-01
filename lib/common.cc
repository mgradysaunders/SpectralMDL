#include "smdl/common.h"
#include "smdl/Module.h"
#include "smdl/Support/Logger.h"

#include "llvm/ADT/StringRef.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/Error.h"
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
  return NativeTarget{
      name, triple,
      target->createTargetMachine(triple, name, "", opts, llvm::Reloc::PIC_)};
}()};

const NativeTarget &NativeTarget::get() noexcept { return nativeTarget; }

std::string_view SourceLocation::get_module_name() const {
  return module_ ? module_->get_name() : std::string_view();
}

std::string_view SourceLocation::get_module_file_name() const {
  return module_ ? module_->get_file_name() : std::string_view();
}

void SourceLocation::log_warn(std::string_view message) const {
  auto str{std::string(*this)};
  if (!str.empty())
    str += ' ';
  str += message;
  SMDL_LOG_WARN(str);
}

void SourceLocation::log_error(std::string_view message) const {
  auto str{std::string(*this)};
  if (!str.empty())
    str += ' ';
  str += message;
  SMDL_LOG_ERROR(str);
}

void SourceLocation::throw_error(std::string message) const {
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
    if (module_->is_builtin()) {
      str += module_->get_name();
    } else {
      str += relative(std::string(module_->get_file_name()));
    }
    str += ':';
    str += std::to_string(lineNo);
    str += ']';
  }
  return str;
}

void State::finalize_for_runtime_conventions() {
  // 1. Orthonormalize normal and tangent vectors.
  normal = normalize(normal);
  if (!is_all_finite(normal)) {
    normal = {0, 0, 1};
  }
  for (int i = 0; i < texture_space_max; i++) {
    auto &tu{texture_tangent_u[i]};
    auto &tv{texture_tangent_v[i]};
    auto &tw{normal};
    tu = normalize(tu - dot(tu, tw) * tw);
    if (!is_all_finite(tv)) {
      tu = normalize(perpendicular_to(tw));
    }
    tv = normalize(tv - dot(tv, tw) * tw - dot(tv, tu) * tu);
    if (!is_all_finite(tv)) {
      tv = normalize(cross(tw, tu));
    }
  }

  // 2. Orthonormalize geometry normal and tangent vectors.
  geometry_normal = normalize(geometry_normal);
  if (!is_all_finite(geometry_normal)) {
    geometry_normal = normal;
  }
  for (int i = 0; i < texture_space_max; i++) {
    auto &tu{geometry_tangent_u[i]};
    auto &tv{geometry_tangent_v[i]};
    auto &tw{geometry_normal};
    tu = normalize(tu - dot(tu, tw) * tw);
    if (!is_all_finite(tu)) {
      tu = normalize(perpendicular_to(tw));
    }
    tv = normalize(tv - dot(tv, tw) * tw - dot(tv, tu) * tu);
    if (!is_all_finite(tv)) {
      tv = normalize(cross(tw, tu));
    }
  }

  // 3. Construct the tangent-to-object matrix.
  tangent_to_object_matrix[0] = float4(geometry_tangent_u[0], 0.0f);
  tangent_to_object_matrix[1] = float4(geometry_tangent_v[0], 0.0f);
  tangent_to_object_matrix[2] = float4(geometry_normal, 0.0f);
  tangent_to_object_matrix[3] = float4(position, 1.0f);

  // 4. Transform everything from object space to tangent space.
  auto object_to_tangent_matrix{affine_inverse(tangent_to_object_matrix)};
  position = {};
  geometry_normal = {0, 0, 1};
  normal = object_to_tangent_matrix * float4(normal, 0.0f);
  motion = object_to_tangent_matrix * float4(motion, 0.0f);
  direction = object_to_tangent_matrix * float4(direction, 0.0f);
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
}

} // namespace smdl
