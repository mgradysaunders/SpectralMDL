#include "smdl/common.h"
#include "smdl/Logger.h"
#include "smdl/Module.h"

#if __GNUC__ || __clang__
#if __has_include(<cxxabi.h>)
#define HAS_CXXABI 1
#include <cxxabi.h>
#endif // #if __has_include(<cxxabi.h>)
#endif // #if __GNUC__ || __clang__

#include <cerrno>
#include <streambuf>

#include "filesystem.h"

#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/Allocator.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/MD5.h"
#include "llvm/Support/Parallel.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/WithColor.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/TargetParser/Host.h"

namespace smdl {

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

bool has_extension(std::string_view path, std::string_view extension) noexcept {
  return llvm::StringRef(path).ends_with_insensitive(extension);
}

bool exists(const std::string &path) noexcept try {
  return fs::exists(path);
} catch (...) {
  return false;
}

bool is_file(const std::string &path) noexcept try {
  return fs::is_regular_file(path);
} catch (...) {
  return false;
}

bool is_directory(const std::string &path) noexcept try {
  return fs::is_directory(path);
} catch (...) {
  return false;
}

bool is_path_equivalent(const std::string &path0,
                        const std::string &path1) noexcept try {
  return canonical(path0) == canonical(path1);
} catch (...) {
  return false;
}

bool is_parent_path_of(const std::string &path0,
                       const std::string &path1) noexcept try {
  return fs::relative(canonical(path1), canonical(path0)) != fs::path();
} catch (...) {
  return false;
}

std::string join_paths(std::string_view path0, std::string_view path1) {
  if (path1.empty())
    return std::string(path0);
  if (path0.empty())
    return std::string(path1);
  return (fs_make_path(path0) / fs_make_path(path1)).string();
}

std::string canonical(std::string path) noexcept try {
  if (starts_with(path, "~")) {
    llvm::SmallString<128> pathTmp{};
    llvm::sys::fs::expand_tilde(path, pathTmp);
    path = pathTmp.str();
  }
  return fs::weakly_canonical(path).string();
} catch (...) {
  return path;
}

std::string relative(std::string path) noexcept try {
  return fs::relative(path).string();
} catch (...) {
  return path;
}

std::string parent_path(std::string path) noexcept try {
  return fs::path(path).parent_path().string();
} catch (...) {
  return path;
}

std::fstream open_or_throw(const std::string &path, std::ios::openmode mode) {
  auto stream{std::fstream(path, mode)};
  if (!stream.is_open())
    throw Error(
        concat("cannot open ", quoted(path), ": ", std::strerror(errno)));
  return stream;
}

std::string read_or_throw(const std::string &path) {
  auto stream{open_or_throw(path, std::ios::in | std::ios::binary)};
  return std::string((std::istreambuf_iterator<char>(stream)),
                     std::istreambuf_iterator<char>());
}

void quoted::append_to(std::string &result) {
  result += '\'';
  result += str;
  result += '\'';
}

void quoted_path::append_to(std::string &result) {
  result += '\'';
  auto relPath{relative(std::string(str))};
  if (!relPath.empty() && relPath.size() < str.size()) {
    result += relPath;
  } else {
    result += str;
  }
  result += '\'';
}

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
      str += relative(std::string(module_->get_file_name()));
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

SemanticVersion SemanticVersion::parse(const std::string &versionStr) {
  auto version{SemanticVersion{}};
  auto success{[&]() {
    auto src{llvm::StringRef(versionStr).trim()};
    if (src.consumeInteger(10, version.major) || !src.consume_front(".") ||
        src.consumeInteger(10, version.minor) || !src.consume_front(".") ||
        src.consumeInteger(10, version.patch))
      return false;
    if (src.consume_front("-")) {
      auto [src0, src1] = src.split('+');
      version.preRelease = std::string(src0);
      version.buildMetadata = std::string(src1);
    } else if (src.consume_front("+")) {
      version.buildMetadata = std::string(src);
    }
    return true;
  }()};
  if (!success)
    throw Error(concat("invalid version string ", quoted(versionStr)));
  return version;
}

SemanticVersion::CompareResult
SemanticVersion::compare(const SemanticVersion &other) const noexcept {
  const SemanticVersion &lhs{*this};
  const SemanticVersion &rhs{other};
  // 1. Precedence MUST be calculated by separating the version into major,
  //    minor, patch, and pre-release identifiers in that order (Build
  //    metadata does not figure into precedence).
  // 2. Precedence is determined by the first difference when comparing each
  //    of these identifiers from left to right as follows: Major, minor, and
  //    patch version are always compared numerically
  if (lhs.major != rhs.major)
    return lhs.major > rhs.major ? NEWER_BY_MAJOR_VERSION
                                 : OLDER_BY_MAJOR_VERSION;
  if (lhs.minor != rhs.minor)
    return lhs.minor > rhs.minor ? NEWER_BY_MINOR_VERSION
                                 : OLDER_BY_MINOR_VERSION;
  if (lhs.patch != rhs.patch)
    return lhs.patch > rhs.patch ? NEWER_BY_PATCH_VERSION
                                 : OLDER_BY_PATCH_VERSION;
  if (lhs.has_pre_release() || rhs.has_pre_release()) {
    // 3. When major, minor, and patch are equal, a pre-release version
    //    has lower precedence than a normal version.
    if (!lhs.has_pre_release())
      return NEWER_BY_PRE_RELEASE;
    if (!rhs.has_pre_release())
      return OLDER_BY_PRE_RELEASE;
    // 4. Precedence for two pre-release versions with the same major,
    //    minor, and patch version MUST be determined by comparing each
    //    dot separated identifier from left to right until a difference is
    //    found as follows:
    //    1. Identifiers consisting of only digits are compared
    //       numerically
    //    2. Identifiers with letters or hyphens are compared lexically in
    //       ASCII sort order
    //    3. Numeric identifiers always have lower precedence than non-numeric
    //       identifiers
    //    4. A larger set of pre-release fields has a higher precedence than
    //       a smaller set, if all of the preceding identifiers are equal.
    auto lhsIdents{llvm::SmallVector<llvm::StringRef>{}};
    auto rhsIdents{llvm::SmallVector<llvm::StringRef>{}};
    llvm::StringRef(lhs.preRelease).split(lhsIdents, '.');
    llvm::StringRef(rhs.preRelease).split(rhsIdents, '.');
    for (size_t i = 0; i < size_t(std::min(lhsIdents.size(), rhsIdents.size()));
         i++) {
      llvm::errs() << lhsIdents[i] << " <=> " << rhsIdents[i] << '\n';
      uint32_t lhsNum{};
      uint32_t rhsNum{};
      bool lhsIsNumeric{!lhsIdents[i].getAsInteger(10, lhsNum)};
      bool rhsIsNumeric{!rhsIdents[i].getAsInteger(10, rhsNum)};
      if (lhsIsNumeric && rhsIsNumeric) {
        if (lhsNum != rhsNum)
          return lhsNum > rhsNum ? NEWER_BY_PRE_RELEASE : OLDER_BY_PRE_RELEASE;
      } else if (!lhsIsNumeric && rhsIsNumeric) {
        return NEWER_BY_PRE_RELEASE;
      } else if (lhsIsNumeric && !rhsIsNumeric) {
        return OLDER_BY_PRE_RELEASE;
      } else {
        int result{lhsIdents[i].compare(rhsIdents[i])};
        if (result != 0)
          return result > 0 ? NEWER_BY_PRE_RELEASE : OLDER_BY_PRE_RELEASE;
      }
    }
    if (lhsIdents.size() != rhsIdents.size()) {
      return lhsIdents.size() > rhsIdents.size() ? NEWER_BY_PRE_RELEASE
                                                 : OLDER_BY_PRE_RELEASE;
    }
  }
  return EXACTLY_EQUAL;
}

SemanticVersion::operator std::string() const {
  auto str{std::string()};
  str += std::to_string(major), str += '.';
  str += std::to_string(minor), str += '.';
  str += std::to_string(patch);
  if (!preRelease.empty()) {
    str += '-';
    str += preRelease;
  }
  if (!buildMetadata.empty()) {
    str += '+';
    str += buildMetadata;
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

MD5Hash MD5Hash::hash_file(const std::string &fileName) noexcept try {
  auto hasher{llvm::MD5()};
  auto buffer{std::array<char, 128>{}};
  auto stream{open_or_throw(fileName, std::ios::in | std::ios::binary)};
  while (!stream.eof()) {
    stream.read(buffer.data(), buffer.size());
    hasher.update(llvm::StringRef(buffer.data(), stream.gcount()));
  }
  return MD5Hash{hasher.result().words()};
} catch (...) {
  return MD5Hash{}; // Zero
}

MD5Hash MD5Hash::hash_memory(const void *mem, size_t memSize) noexcept {
  auto result{llvm::MD5::hash(
      llvm::ArrayRef<uint8_t>{static_cast<const uint8_t *>(mem), memSize})};
  return MD5Hash{result.words()};
}

MD5Hash::operator std::string() const {
  auto bytes{std::array<uint8_t, 16>{}};
  llvm::support::endian::write64le(&bytes[0], lower_bits());
  llvm::support::endian::write64le(&bytes[8], upper_bits());
  return llvm::toHex(llvm::ArrayRef<uint8_t>{bytes.data(), 16},
                     /*LowerCase=*/true);
}

const MD5FileHash *MD5FileHasher::operator[](const std::string &fileName) {
  auto canonicalFileName{canonical(fileName)};
  auto [itr, inserted] = fileHashes.try_emplace(canonicalFileName);
  auto &fileHash{itr->second};
  if (inserted) {
    fileHash.hash = MD5Hash::hash_file(canonicalFileName);
    fileHash.canonicalFileNames.push_back(canonicalFileName);
  }
  return &fileHash;
}

void parallel_for(size_t count, const std::function<void(size_t)> &func) {
  SMDL_SANITY_CHECK(func);
  llvm::parallelFor(0, count, func);
}

} // namespace smdl
