#include "smdl/common.h"
#include "smdl/Logger.h"
#include "smdl/Module.h"

#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/Allocator.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/WithColor.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/TargetParser/Host.h"

namespace smdl {

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

std::string_view SourceLocation::get_source_code() const {
  if (module_) {
    auto sourceCode{module_->get_source_code()};
    const char *ptrBegin{sourceCode.data()};
    const char *ptrEnd{sourceCode.data() + sourceCode.size()};
    const char *ptr0{ptrBegin + i};
    const char *ptr1{ptrBegin + i};
    while (ptr0 > ptrBegin) {
      if (*(ptr0 - 1) == '\n')
        break;
      --ptr0;
    }
    while (ptr1 < ptrEnd) {
      if (*ptr1 == '\n')
        break;
      ++ptr1;
    }
    return std::string_view(ptr0, size_t(ptr1 - ptr0));
  } else {
    return std::string_view();
  }
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
  throw Error(std::move(message), *this);
}

SourceLocation::operator std::string() const {
  if (module_)
    return concat("[",
                  module_->is_builtin() ? module_->get_name()
                                        : module_->get_file_name(),
                  ":", lineNo, "]");
  else
    return {};
}

void Error::print() const {
  auto str{std::string()};
  auto os{llvm::raw_string_ostream(str)};
  if (srcLoc.module_)
    llvm::WithColor(os, llvm::HighlightColor::Address)
        << '['
        << (srcLoc.module_->is_builtin() ? srcLoc.module_->get_name()
                                         : srcLoc.module_->get_file_name())
        << ':' << srcLoc.lineNo << "] ";
  os << message;
  SMDL_LOG_ERROR(str);
}

void Error::print_and_exit() const {
  print();
  std::exit(EXIT_FAILURE);
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
