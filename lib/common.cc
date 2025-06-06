#include "smdl/common.h"
#include "smdl/Logger.h"
#include "smdl/Module.h"

#if __GNUC__ || __clang__
#if __has_include(<cxxabi.h>)
#define HAS_CXXABI 1
#include <cxxabi.h>
#endif // #if __has_include(<cxxabi.h>)
#endif // #if __GNUC__ || __clang__

#include "filesystem.h"

#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/Allocator.h"
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

static NativeTarget nativeTarget{};

void init_or_exit() {
  static const int onlyOnce = []() {
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    static std::string name{llvm::sys::getHostCPUName()};
    static std::string triple{llvm::sys::getDefaultTargetTriple()};
    auto targetError{std::string{}};
    auto target{llvm::TargetRegistry::lookupTarget(triple, targetError)};
    if (!target)
      llvm::report_fatal_error(targetError.c_str());
    llvm::TargetOptions opts{};
    nativeTarget.name = name;
    nativeTarget.triple = triple;
    nativeTarget.machine =
        target->createTargetMachine(triple, name, "", opts, llvm::Reloc::PIC_);
    return 0;
  }();
  SMDL_SANITY_CHECK(onlyOnce == 0); // Silence unused variable warning
}

const NativeTarget &get_native_target() { return nativeTarget; }

BumpPtrAllocator::BumpPtrAllocator() { ptr = new llvm::BumpPtrAllocator(); }

BumpPtrAllocator::~BumpPtrAllocator() {
  delete static_cast<llvm::BumpPtrAllocator *>(ptr);
  ptr = nullptr;
}

void *BumpPtrAllocator::allocate(size_t size, size_t align) {
  if (size == 0)
    return nullptr;
  return static_cast<llvm::BumpPtrAllocator *>(ptr)->Allocate(size, align);
}

void BumpPtrAllocator::reset() {
  static_cast<llvm::BumpPtrAllocator *>(ptr)->Reset();
}

size_t BumpPtrAllocator::bytes_allocated() const {
  return static_cast<const llvm::BumpPtrAllocator *>(ptr)->getBytesAllocated();
}

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
      str += fs_abbreviate(fs_make_path(module_->get_file_name()));
    }
    str += ':';
    str += std::to_string(lineNo);
    str += ']';
  }
  return str;
}

void Error::print() const { SMDL_LOG_ERROR(message); }

void Error::print_and_exit() const {
  print();
  std::exit(EXIT_FAILURE);
}

std::string abi_demangle(const char *name) {
  if (!name)
    return {};
#if HAS_CXXABI
  int status{};
  char *cstr{::abi::__cxa_demangle(name, 0, 0, &status)};
  if (!cstr)
    return name;
  std::string str{cstr};
  std::free(cstr);
  return str;
#else
  return name;
#endif // #if HAS_CXXABI
}

std::string abi_demangle_exception_name() {
#if HAS_CXXABI
  return abi_demangle(::abi::__cxa_current_exception_type()->name());
#else
  return "unknown exception";
#endif // #if HAS_CXXABI
}

void State::finalize_for_runtime_conventions() {
  // 1. Orthonormalize normal and tangent vectors.
  normal = normalize(normal);
  for (int i = 0; i < texture_space_max; i++) {
    auto &tu{texture_tangent_u[i]};
    auto &tv{texture_tangent_v[i]};
    auto &tw{normal};
    tu = normalize(tu - dot(tu, tw) * tw);
    tv = normalize(tv - dot(tv, tw) * tw - dot(tv, tu) * tu);
  }

  // 2. Orthonormalize geometry normal and tangent vectors.
  geometry_normal = normalize(geometry_normal);
  for (int i = 0; i < texture_space_max; i++) {
    auto &tu{geometry_tangent_u[i]};
    auto &tv{geometry_tangent_v[i]};
    auto &tw{geometry_normal};
    tu = normalize(tu - dot(tu, tw) * tw);
    tv = normalize(tv - dot(tv, tw) * tw - dot(tv, tu) * tu);
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

void sanity_check_failed(const char *condition, const char *file, int line,
                         const char *more) {
  std::string message{"Sanity check failed! "};
  message += condition, message += '\n';
  message += "  File = ", message += file, message += '\n';
  message += "  Line = ", message += std::to_string(line), message += '\n';
  if (more) {
    message += "\n...\n\n";
    message += more;
    message += "\n";
  }
  llvm::report_fatal_error(message.c_str());
}

} // namespace smdl
