/// \file
#pragma once

#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <functional>
#include <map>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "smdl/Export.h"
#include "smdl/Support/BumpPtrAllocator.h"
#include "smdl/Support/Error.h"
#include "smdl/Support/Filesystem.h"
#include "smdl/Support/Macros.h"
#include "smdl/Support/Span.h"
#include "smdl/Support/Strings.h"
#include "smdl/Support/VectorMath.h"

namespace llvm {

class Constant;
class ConstantInt;
class LLVMContext;
class Module;
class Type;
class Value;

namespace orc {

class ThreadSafeModule;
class LLJIT;

} // namespace orc

} // namespace llvm

/// The top-level SMDL namespace.
namespace smdl {

/// \addtogroup compiler
/// \{

/// The SMDL build information.
class SMDL_EXPORT BuildInfo final {
public:
  /// A third-party dependency: its name and the version linked, or "off"
  /// for one the build configured out.
  struct ThirdParty final {
    std::string name{};
    std::string version{};
  };

  /// Get.
  [[nodiscard]] static BuildInfo get() noexcept;

  /// Summarize as a human-readable multi-line string, with `thirdparty`
  /// as one comma-separated list wrapped to 80 columns.
  [[nodiscard]] std::string toString() const;

public:
  /// The major version number.
  uint32_t major{};

  /// The minor version number.
  uint32_t minor{};

  /// The patch version number.
  uint32_t patch{};

  /// The git branch name, or "unknown" if it was unavailable at build time.
  const char *gitBranch{};

  /// The git commit hash, or "unknown" if it was unavailable at build time.
  const char *gitCommit{};

  /// The LLVM version linked into the library. Never null.
  const char *llvmVersion{};

  /// The compile date and time, from `__DATE__` and `__TIME__`. Never null.
  /// This tracks the translation unit that defines `get()`, so an
  /// incremental rebuild of other code does not refresh it.
  const char *buildDate{};

  /// Was the library built with RTTI?
  bool hasRTTI{};

  /// Does `parallelFor()` schedule dynamically? False is the fixed split
  /// of the range into contiguous tasks. This changes only how fast a
  /// parallel loop runs, never what it computes.
  bool hasDynamicScheduling{};

  /// The version of the vendored miniz. Never null.
  const char *withMiniz{};

  /// The version of the vendored stb_image. Never null.
  const char *withSTBImage{};

  /// The version of the vendored stb_image_write. Never null.
  const char *withSTBImageWrite{};

  /// The version of the vendored stb_image_resize2. Never null.
  const char *withSTBImageResize{};

  /// The version of the vendored stb_sprintf. Never null.
  const char *withSTBSprintf{};

  /// The version of the vendored tinyexr. Never null.
  const char *withTinyEXR{};

  /// The pinned Ptex release tag, or null if built without Ptex.
  const char *withPtex{};

  /// The pinned OpenVDB release tag providing NanoVDB, or null if built
  /// without NanoVDB.
  const char *withNanoVDB{};

  /// The dependencies above in the order `toString()` lists them. A
  /// program linking dependencies of its own appends them before
  /// printing, so that the list reads as one.
  std::vector<ThirdParty> thirdparty{};
};

/// \}

/// \addtogroup compiler
/// \{

class Compiler;
class Module;
class Type;

/// A source location somewhere in an MDL module.
class SMDL_EXPORT SourceLocation final {
public:
  /// Get the module name.
  [[nodiscard]] std::string_view getModuleName() const;

  /// Get the file name. This is empty unless the module is file backed.
  [[nodiscard]] std::string_view getModuleFileName() const;

  /// Get the name to print in diagnostics, which is the file name for
  /// ordinary modules and origin markup for the others. See
  /// `Module::getDisplayName()`.
  [[nodiscard]] std::string_view getModuleDisplayName() const;

  /// Get the source line containing this location with a caret under the
  /// relevant column, as a block that begins with a newline so that it may
  /// be appended after a diagnostic message and whatever notes follow it.
  /// Returns the empty string if there is no source code to show.
  [[nodiscard]] std::string getSourceSnippet() const;

  /// Get `message` as a diagnostic at this location reads: the location, a
  /// space, and the message, or the message alone if there is no location.
  [[nodiscard]] std::string formatMessage(std::string_view message) const;

  /// Log a debug message.
  void logDebug(std::string_view message) const;

  /// Log an informational message.
  void logInfo(std::string_view message) const;

  /// Log a warning.
  void logWarn(std::string_view message) const;

  /// Log an error, with the source snippet beneath it.
  void logError(std::string_view message) const;

  /// Throw an `Error`.
  [[noreturn]] void throwError(std::string message) const;

  /// Throw an `Error` using `concat` to concatenate the arguments.
  template <typename T0, typename T1, typename... Ts>
  [[noreturn]] void throwError(T0 &&value0, T1 &&value1, Ts &&...values) const {
    throwError(concat(std::forward<T0>(value0), std::forward<T1>(value1),
                      std::forward<Ts>(values)...));
  }

  /// Is not-valid?
  [[nodiscard]] bool operator!() const { return !module_; }

  /// Is valid?
  [[nodiscard]] operator bool() const { return module_; }

  /// Convert to the markup `SpellLocation` writes, or to the empty string
  /// if there is no module.
  [[nodiscard]] operator std::string() const;

public:
  /// The associated MDL module, which contains the filename and source code.
  Module *module_{};

  /// The line number.
  uint32_t lineNo{1};

  /// The character number in the line.
  uint32_t charNo{1};

  /// The raw index in the source code string.
  uint64_t i{};
};

/// The format options.
class SMDL_EXPORT FormatOptions final {
public:
  /// Format files in-place. If false, prints formatted source code to `stdout`.
  bool isInPlace{};

  /// Remove comments from formatted source code.
  bool shouldDropComments{};

  /// Keep `///` and `///<` documentation comments even when
  /// `shouldDropComments` is true.
  bool shouldKeepDocComments{};

  /// Remove annotations from formatted source code.
  bool shouldDropAnnotations{};

  /// Want compact?
  bool isCompact{};

  /// The column past which the formatter prefers to break a line.
  ///
  /// This is a preference and not a guarantee. The formatter breaks a
  /// comma-separated list or continues after a `=` only when doing so
  /// actually improves the layout, and never rearranges an expression
  /// just to respect the limit, so a long expression that cannot be
  /// helped is left alone. Zero disables column awareness entirely,
  /// as does `compact`.
  int softColumnLimit{80};
};

/// \}

} // namespace smdl
