/// \file
#pragma once

#include "smdl/AST/Stmt.h"

namespace smdl::AST {

/// A fully parsed file.
class SMDL_EXPORT File final : public NodeSubclass<NodeKind::File> {
public:
  /// The MDL version at the top of the file, e.g., `mdl 1.7;`
  class Version final {
  public:
    /// The keyword `mdl`.
    std::string_view srcKwMdl{};

    /// The version string, e.g., `1.7`.
    std::string_view srcVersion{};

    /// The major version number.
    uint32_t major{};

    /// The minor version number.
    uint32_t minor{};

    /// The semicolon `;`.
    std::string_view srcSemicolon{};
  };

  explicit File(std::string_view srcKwSmdlSyntax,
                std::optional<Version> version,
                std::vector<BumpPtr<Decl>> importDecls,
                std::string_view srcKwModule,
                BumpPtr<AnnotationBlock> moduleAnnotations,
                std::string_view srcSemicolonAfterModule,
                std::vector<BumpPtr<Decl>> globalDecls)
      : srcKwSmdlSyntax(srcKwSmdlSyntax), version(version),
        importDecls(std::move(importDecls)), srcKwModule(srcKwModule),
        moduleAnnotations(std::move(moduleAnnotations)),
        srcSemicolonAfterModule(srcSemicolonAfterModule),
        globalDecls(std::move(globalDecls)) {}

  [[nodiscard]] bool is_smdl_syntax() const { return !srcKwSmdlSyntax.empty(); }

  /// The source keyword `#smdl_syntax`. This may be empty!
  std::string_view srcKwSmdlSyntax{};

  /// The version. This may be nullopt!
  std::optional<Version> version{};

  /// The import declarations.
  std::vector<BumpPtr<Decl>> importDecls{};

  /// The keyword `module`. This may be empty!
  std::string_view srcKwModule{};

  /// The module annotations. This may be null!
  BumpPtr<AnnotationBlock> moduleAnnotations{};

  /// The semicolon `;` after the `module` keyword. This may be empty!
  std::string_view srcSemicolonAfterModule{};

  /// The global declarations.
  std::vector<BumpPtr<Decl>> globalDecls{};
};

} // namespace smdl::AST
