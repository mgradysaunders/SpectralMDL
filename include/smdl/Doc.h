/// \file
#pragma once

#include <optional>
#include <string>
#include <vector>

#include "smdl/Common.h"

namespace smdl {

class Module;

/// \addtogroup Main
/// \{

/// A documented parameter in a `DocEntry`.
class SMDL_EXPORT DocParam final {
public:
  /// The name.
  std::string name{};

  /// The signature, e.g., `const float weight = 1.0`, with whitespace
  /// normalized and comments and annotations stripped.
  std::string signature{};

  /// The documentation text. This may be empty!
  std::string docText{};
};

/// A documented declaration in a `DocModule`.
class SMDL_EXPORT DocEntry final {
public:
  /// The declaration kind: `"annotation"`, `"enum"`, `"enumerator"`,
  /// `"field"`, `"function"`, `"namespace"`, `"struct"`, `"tag"`,
  /// `"typedef"`, or `"variable"`.
  std::string kind{};

  /// The name.
  std::string name{};

  /// The qualified name, e.g., `::df::diffuse_reflection_bsdf`.
  std::string qualifiedName{};

  /// Is marked with the keyword `export`? (Members inherit this from
  /// the containing declaration.)
  bool isExported{};

  /// The line number in the module source.
  uint32_t lineNo{};

  /// The signature, with whitespace normalized and comments and
  /// annotations stripped.
  std::string signature{};

  /// The documentation text, from the `///` block above the
  /// declaration, else the trailing `///<` comment, else the
  /// `anno::description(...)` annotation. This may be empty!
  std::string docText{};

  /// The parameters if this is a function or annotation. This may be
  /// empty!
  std::vector<DocParam> params{};

  /// The members if this is a struct (fields), enum (enumerators), or
  /// namespace (nested declarations). This may be empty!
  std::vector<DocEntry> members{};
};

/// The documentation extracted from one module.
class SMDL_EXPORT DocModule final {
public:
  /// The module name, e.g., `df`.
  std::string name{};

  /// The qualified module name, e.g., `::df`.
  std::string qualifiedName{};

  /// The file name. This is empty for builtin modules!
  std::string fileName{};

  /// The module documentation text, from the `///` block above the
  /// `#smdl` marker or `mdl X.Y` version, else the
  /// `anno::description(...)` in the `module [[ ... ]]` annotations.
  /// This may be empty!
  std::string docText{};

  /// The documented declarations.
  std::vector<DocEntry> entries{};
};

/// The documentation database, see `Compiler::extractDocs()`.
class SMDL_EXPORT DocDatabase final {
public:
  /// The modules.
  std::vector<DocModule> modules{};

  /// Find entries whose qualified name ends with the given (possibly
  /// partly qualified) symbol name on component boundaries, e.g.,
  /// `relativeIOR` or `::df::relativeIOR`.
  [[nodiscard]] std::vector<const DocEntry *>
  findSymbol(std::string_view symbolName) const;

  /// Remove declarations that are not marked with the keyword `export`,
  /// keeping the members of whatever remains. Namespaces are filtered
  /// recursively and removed when they end up empty.
  void removeNonExported();

  /// Print as JSON.
  [[nodiscard]] std::string printJSON() const;

  /// Print as Markdown.
  [[nodiscard]] std::string printMarkdown() const;
};

/// Extract documentation from the given module, which must already be
/// parsed.
[[nodiscard]] SMDL_EXPORT DocModule extractDocModule(const Module &module_);

/// Get the names of the builtin modules, e.g., `df` and `models::prospect`.
[[nodiscard]] SMDL_EXPORT std::vector<std::string_view> getBuiltinModuleNames();

/// Extract documentation from the builtin module with the given name,
/// e.g., `df`. Returns nothing if there is no such builtin module.
[[nodiscard]] SMDL_EXPORT std::optional<DocModule>
extractBuiltinDocModule(std::string_view name);

/// \}

} // namespace smdl
