/// \file
#pragma once

#include "smdl/AST/Expr.h"

namespace smdl {

class Crumb;

} // namespace smdl

namespace smdl::AST {

/// An annotation.
class SMDL_EXPORT Annotation final {
public:
  /// The identifier.
  BumpPtr<Identifier> identifier;

  // The arguments.
  ArgumentList args;

  /// The next comma `,`. This may be empty!
  std::string_view srcComma{};

  /// Has identifier with the given name sequence?
  [[nodiscard]] bool hasIdentifier(Span<const std::string_view> names) const {
    return identifier && Span<const std::string_view>(*identifier) == names;
  }
};

/// An annotation block (between double brackets `[[ ... ]]`).
class SMDL_EXPORT AnnotationBlock final {
public:
  explicit AnnotationBlock(std::string_view srcDoubleBrackL,
                           std::vector<Annotation> annos,
                           std::string_view srcDoubleBrackR)
      : srcDoubleBrackL(srcDoubleBrackL), annos(std::move(annos)),
        srcDoubleBrackR(srcDoubleBrackR) {}

public:
  /// The size.
  [[nodiscard]] size_t size() const { return annos.size(); }

  /// The begin iterator.
  [[nodiscard]] auto begin() { return annos.begin(); }

  /// The begin iterator, const variant.
  [[nodiscard]] auto begin() const { return annos.begin(); }

  /// The end iterator.
  [[nodiscard]] auto end() { return annos.end(); }

  /// The end iterator, const variant.
  [[nodiscard]] auto end() const { return annos.end(); }

  /// The access operator.
  [[nodiscard]] auto &operator[](size_t i) const { return annos[i]; }

public:
  /// Has comma `,` after the last annotation?
  [[nodiscard]] bool hasTrailingComma() const {
    return !annos.empty() && !annos.back().srcComma.empty();
  }

  /// Is marked as `unused()` or `anno::unused()`?
  [[nodiscard]] bool isMarkedUnused() const {
    for (const auto &anno : annos)
      if (anno.hasIdentifier({"unused"}) ||
          anno.hasIdentifier({"anno", "unused"}))
        return true;
    return false;
  }

  /// Is marked as `deprecated()` or `anno::deprecated()`?
  [[nodiscard]] bool isMarkedDeprecated() const {
    for (const auto &anno : annos)
      if (anno.hasIdentifier({"deprecated"}) ||
          anno.hasIdentifier({"anno", "deprecated"}))
        return true;
    return false;
  }

public:
  /// The source location.
  SourceLocation srcLoc;

  /// The double bracket `[[`.
  std::string_view srcDoubleBrackL;

  /// The annotations.
  std::vector<Annotation> annos;

  /// The double bracket `]]`.
  std::string_view srcDoubleBrackR;
};

/// A parameter.
class SMDL_EXPORT Parameter final : public NodeSubclass<NodeKind::Parameter> {
public:
  /// The type.
  BumpPtr<Type> type;

  /// The name.
  Name name{};

  /// The equal `=`. This may be empty!
  std::string_view srcEqual;

  /// The initializer expression. This may be null!
  BumpPtr<Expr> exprInit;

  /// The annotations.
  BumpPtr<AnnotationBlock> annotations;

  /// The next comma `,`. This may be empty!
  std::string_view srcComma;

  /// The complete source region.
  std::string_view src;

  /// Has warning been issued about this parameter yet? Used to prevent
  /// the same warning being logged over and over again.
  bool warningIssued{};
};

/// A parameter list.
class SMDL_EXPORT ParameterList final {
public:
  /// The size.
  [[nodiscard]] size_t size() const { return params.size(); }

  /// The begin iterator.
  [[nodiscard]] auto begin() { return params.begin(); }

  /// The begin iterator, const variant.
  [[nodiscard]] auto begin() const { return params.begin(); }

  /// The end iterator.
  [[nodiscard]] auto end() { return params.end(); }

  /// The end iterator, const variant.
  [[nodiscard]] auto end() const { return params.end(); }

  /// The access operator.
  [[nodiscard]] auto &operator[](size_t i) const { return params[i]; }

public:
  /// Is parameter list for function variant?
  [[nodiscard]] bool isVariant() const { return !srcStar.empty(); }

  /// Has comma `,` after the last parameter?
  [[nodiscard]] bool hasTrailingComma() const {
    return !params.empty() && !params.back().srcComma.empty() && !hasTrailingEllipsis();
  }

  /// Has ellipsis `...` after the last parameter?
  [[nodiscard]] bool hasTrailingEllipsis() const {
    return !srcEllipsis.empty();
  }

public:
  /// The parenthesis `(`.
  std::string_view srcParenL;

  /// The star `*` if parameter list for function variant. This may be empty!
  std::string_view srcStar;

  /// The parameters.
  std::vector<Parameter> params;

  /// The ellipsis `...` if variadic. This may be empty!
  std::string_view srcEllipsis;

  /// The parenthesis `)`.
  std::string_view srcParenR;
};

/// An import path.
///
/// \note
/// This is almost the same as an `Identifier` in that it is a sequence of
/// elements separated by double colons. However, elements in an import path may
/// also be literal strings to account for spaces in directory names.
///
class SMDL_EXPORT ImportPath final {
public:
  class Element final {
  public:
    /// The double colon `::` before the name. This may be empty!
    std::string_view srcDoubleColon;

    /// The source name. This may be empty! (In which case, `literalString` must
    /// not be empty!)
    std::string_view srcName;

    /// The literal string.
    BumpPtr<LiteralString> literalString;

    /// Is literal string and not a simple name?
    [[nodiscard]] bool is_literal_string() const { return literalString; }
  };

  ImportPath() = default;

  explicit ImportPath(std::vector<Element> elems) : elements(std::move(elems)) {
    elementViews.resize(elements.size());
    for (size_t i = 0; i < elements.size(); i++)
      elementViews[i] = elements[i].is_literal_string()
                            ? std::string_view(elements[i].literalString->value)
                            : elements[i].srcName;
  }

  /// The number of names.
  [[nodiscard]] size_t size() const { return elements.size(); }

  /// Is absolute import path? (e.g., `::df::*`)
  [[nodiscard]] bool isAbsolute() const {
    return !elements.empty() && !elements[0].srcDoubleColon.empty();
  }

  /// Is relative import path? (e.g., `df::*`)
  [[nodiscard]] bool isRelative() const {
    return !elements.empty() && elements[0].srcDoubleColon.empty();
  }

  /// Is import all? (i.e., ends with `::*`?)
  [[nodiscard]] bool isImportAll() const {
    return !elements.empty() && elements.back().srcName == "*";
  }

  [[nodiscard]] operator Span<const std::string_view>() const {
    return elementViews;
  }

  /// The elements.
  std::vector<Element> elements;

  /// The source views extracted from the elements for convenience.
  std::vector<std::string_view> elementViews;
};

/// An `annotation` declaration.
///
/// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// annotation my_annotation(string message = "", int flags = 0);
/// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
///
class SMDL_EXPORT AnnotationDecl final
    : public DeclSubclass<DeclKind::AnnotationDecl> {
public:
  explicit AnnotationDecl(std::string_view srcKwAnnotation, Name name,
                          ParameterList params,
                          BumpPtr<AnnotationBlock> annotations,
                          std::string_view srcSemicolon)
      : srcKwAnnotation(srcKwAnnotation), name(name), params(std::move(params)),
        annotations(std::move(annotations)), srcSemicolon(srcSemicolon) {}

  /// The keyword `annotation`.
  std::string_view srcKwAnnotation;

  /// The name.
  Name name;

  /// The parameters.
  ParameterList params;

  /// The annotations on this annotation.
  BumpPtr<AnnotationBlock> annotations;

  /// The semicolon `;`.
  std::string_view srcSemicolon;
};

/// An `enum` declaration.
///
/// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// enum Weather {
///   Sunny = 1,
///   PartlyCloudy = 7,
///   ChanceOfRain, // Implicitly 8
///   Rainy         // Implicitly 9
/// };
/// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
class SMDL_EXPORT Enum final : public DeclSubclass<DeclKind::Enum> {
public:
  /// An `enum` declarator name and optional initializer.
  class Declarator final : public NodeSubclass<NodeKind::EnumDeclarator> {
  public:
    /// The enum declaration that contains this declarator.
    Enum *const decl{};

    /// The name.
    Name name;

    /// The equal `=`. This may be empty!
    std::string_view srcEqual;

    /// The initializer expression. This may be null!
    BumpPtr<Expr> exprInit;

    /// The annotations. This may be null!
    BumpPtr<AnnotationBlock> annotations;

    /// The next comma `,`. This may be empty!
    std::string_view srcComma;

    /// The LLVM constant value (This is computed later during compilation)
    llvm::ConstantInt *llvmConst{};

    /// Has warning been issued about this parameter yet? Used to prevent
    /// the same warning being logged over and over again.
    bool warningIssued{};
  };

  explicit Enum(std::string_view srcKwEnum, Name name,
                BumpPtr<AnnotationBlock> annotations,
                std::string_view srcBraceL, std::vector<Declarator> declarators,
                std::string_view srcBraceR, std::string_view srcSemicolon)
      : srcKwEnum(srcKwEnum), name(std::move(name)),
        annotations(std::move(annotations)), srcBraceL(srcBraceL),
        declarators(std::move(declarators)), srcBraceR(srcBraceR),
        srcSemicolon(srcSemicolon) {
    for (auto &declarator : this->declarators) {
      const_cast<Enum *&>(declarator.decl) = this;
    }
  }

  /// The keyword `enum`.
  std::string_view srcKwEnum;

  /// The name.
  Name name;

  /// The annotations. This may be null!
  BumpPtr<AnnotationBlock> annotations;

  /// The brace `{`.
  std::string_view srcBraceL;

  /// The declarators.
  std::vector<Declarator> declarators;

  /// The brace `}`.
  std::string_view srcBraceR;

  /// The semicolon `;`.
  std::string_view srcSemicolon;

  /// Has comma `,` after the last declarator?
  [[nodiscard]] bool hasTrailingComma() const {
    return !declarators.empty() && !declarators.back().srcComma.empty();
  }
};

/// An `exec` declaration. (This is an extension!)
///
/// ~~~~~~~~~~~~~~~~~~~~~~~~~~
/// exec {
///   #print("Hello, world!");
/// }
/// ~~~~~~~~~~~~~~~~~~~~~~~~~~
class SMDL_EXPORT Exec final : public DeclSubclass<DeclKind::Exec> {
public:
  explicit Exec(std::string_view srcKwExec, BumpPtr<Stmt> stmt)
      : srcKwExec(srcKwExec), stmt(std::move(stmt)) {}

  /// The keyword `exec`.
  std::string_view srcKwExec;

  /// The statement, must be a compound statement.
  BumpPtr<Stmt> stmt;
};

/// A function declaration.
class SMDL_EXPORT Function final : public DeclSubclass<DeclKind::Function> {
public:
  class LetAndCall final {
  public:
    /// The let expression. This may be null.
    Let *let{};

    /// The call expression. This must be non-null.
    Call *call{};

    [[nodiscard]] operator bool() const { return call; }
  };

  explicit Function(BumpPtr<Type> returnType,
                    BumpPtr<AnnotationBlock> earlyAnnotations, Name name,
                    ParameterList params, std::string_view srcFrequency,
                    BumpPtr<AnnotationBlock> lateAnnotations,
                    std::string_view srcEqual, BumpPtr<Node> definition,
                    std::string_view srcSemicolon)
      : returnType(std::move(returnType)),
        earlyAnnotations(std::move(earlyAnnotations)), name(name),
        params(std::move(params)), srcFrequency(srcFrequency),
        lateAnnotations(std::move(lateAnnotations)), srcEqual(srcEqual),
        definition(std::move(definition)), srcSemicolon(srcSemicolon) {}

  /// Is a function declaration without a definition?
  [[nodiscard]] bool isDeclarationWithoutDefinition() const {
    return !definition;
  }

  /// Is a function variant?
  [[nodiscard]] bool isVariant() const { return params.isVariant(); }

  /// If this is a function variant, get the variant let and call expressions.
  /// Else throw an error.
  [[nodiscard]] LetAndCall getVariantLetAndCallExpressions() const;

public:
  /// The return type.
  BumpPtr<Type> returnType;

  /// The early annotations between return type and name. This may be null!
  BumpPtr<AnnotationBlock> earlyAnnotations;

  /// The name.
  Name name;

  /// The parameters.
  ParameterList params;

  /// The frequency qualifier `uniform` or `varying`. This may be empty!
  std::string_view srcFrequency;

  /// The late annotations between signature and definition. This may be null!
  BumpPtr<AnnotationBlock> lateAnnotations;

  /// The equal `=`. This may be empty!
  std::string_view srcEqual;

  /// The definition. This may be null!
  BumpPtr<Node> definition;

  /// The semicolon `;`. This may be empty!
  std::string_view srcSemicolon;
};

/// An `import` declaration.
class Import final : public DeclSubclass<DeclKind::Import> {
public:
  class ImportPathWrapper final {
  public:
    /// The import path.
    ImportPath importPath;

    /// The next comma `,`. This may be empty!
    std::string_view srcComma;
  };

  explicit Import(std::string_view srcKwImport,
                  std::vector<ImportPathWrapper> importPathWrappers,
                  std::string_view srcSemicolon)
      : srcKwImport(srcKwImport),
        importPathWrappers(std::move(importPathWrappers)),
        srcSemicolon(srcSemicolon) {}

  /// The keyword `import`.
  std::string_view srcKwImport;

  /// The import paths.
  std::vector<ImportPathWrapper> importPathWrappers;

  /// The semicolon `;`.
  std::string_view srcSemicolon;

  /// Has comma `,` after the last import path?
  [[nodiscard]] bool hasTrailingComma() const {
    return !importPathWrappers.empty() &&
           !importPathWrappers.back().srcComma.empty();
  }
};

/// A `namespace` declaration. (This is an extension!)
class SMDL_EXPORT Namespace final : public DeclSubclass<DeclKind::Namespace> {
public:
  explicit Namespace(std::string_view srcKwNamespace,
                     BumpPtr<Identifier> identifier, std::string_view srcBraceL,
                     std::vector<BumpPtr<Decl>> decls,
                     std::string_view srcBraceR)
      : srcKwNamespace(srcKwNamespace), identifier(std::move(identifier)),
        srcBraceL(srcBraceL), decls(std::move(decls)), srcBraceR(srcBraceR) {}

  /// The keyword `namespace`.
  std::string_view srcKwNamespace;

  /// The identifier.
  BumpPtr<Identifier> identifier;

  /// The brace `{`.
  std::string_view srcBraceL;

  /// The declarations.
  std::vector<BumpPtr<Decl>> decls;

  /// The brace `}`.
  std::string_view srcBraceR;

  /// The first crumb for inside-namespace lookup. This is populated later.
  Crumb *firstCrumb{};

  /// The last crumb for inside-namespace lookup. This is populated later.
  Crumb *lastCrumb{};
};

/// A `struct` declaration.
class SMDL_EXPORT Struct final : public DeclSubclass<DeclKind::Struct> {
public:
  /// A `struct` tag. This is an extension!
  class Tag final {
  public:
    /// Is marked with the keyword `default`?
    [[nodiscard]] bool isDefault() const { return !srcKwDefault.empty(); }

    /// The keyword `default`. This may be empty!
    std::string_view srcKwDefault;

    /// The tag type. This should resolve to `Identifier`, but it is represented
    /// by a `Type` in order for the compiler to easily resolve the tag type it
    /// represents and store it in the AST using existing mechanisms.
    BumpPtr<Type> type;

    /// The next comma `,`. This may be empty!
    std::string_view srcComma;
  };

  /// A `struct` constructor. This is an extension!
  class Constructor final {
  public:
    /// The name, which must be equivalent to the struct name.
    Name name;

    /// The parameters.
    ParameterList params;

    /// The source equal `=`.
    std::string_view srcEqual;

    /// The expression.
    BumpPtr<Expr> expr;

    /// The source semicolon `;`.
    std::string_view srcSemicolon;
  };

  /// A `struct` field declarator.
  class Field final : public NodeSubclass<NodeKind::Field> {
  public:
    /// The type.
    BumpPtr<Type> type;

    /// The name.
    Name name;

    /// The equal `=`. This may be empty!
    std::string_view srcEqual;

    /// The initializer expression. This may be null!
    BumpPtr<Expr> exprInit;

    /// The annotation block. This may be null!
    BumpPtr<AnnotationBlock> annotations;

    /// The semicolon `;`.
    std::string_view srcSemicolon;
  };

  explicit Struct(std::string_view srcKwStruct, Name name,
                  std::string_view srcColonBeforeTags, std::vector<Tag> tags,
                  BumpPtr<AnnotationBlock> annotations,
                  std::string_view srcBraceL,
                  std::vector<Constructor> constructors,
                  std::vector<Field> fields, std::string_view srcKwFinalize,
                  BumpPtr<Stmt> stmtFinalize, std::string_view srcBraceR,
                  std::string_view srcSemicolon)
      : srcKwStruct(srcKwStruct), name(std::move(name)),
        srcColonBeforeTags(srcColonBeforeTags), tags(std::move(tags)),
        annotations(std::move(annotations)), srcBraceL(srcBraceL),
        constructors(std::move(constructors)), fields(std::move(fields)),
        srcKwFinalize(srcKwFinalize), stmtFinalize(std::move(stmtFinalize)),
        srcBraceR(srcBraceR), srcSemicolon(srcSemicolon) {}

  /// The keyword `struct`.
  std::string_view srcKwStruct;

  /// The name.
  Name name;

  /// The colon `:` before the tags. This may be empty!
  std::string_view srcColonBeforeTags;

  /// The tags. This may be empty!
  std::vector<Tag> tags;

  /// The annotations. This may be null!
  BumpPtr<AnnotationBlock> annotations;

  /// The brace `{`.
  std::string_view srcBraceL;

  /// The constructors. This is an extension and may be empty!
  std::vector<Constructor> constructors;

  /// The fields.
  std::vector<Field> fields;

  /// The keyword `finalize`. This is an extension and may be empty!
  std::string_view srcKwFinalize;

  /// The statement after `finalize`. This is an extension and may be nulL!
  BumpPtr<Stmt> stmtFinalize;

  /// The brace `}`.
  std::string_view srcBraceR;

  /// The semicolon `;`.
  std::string_view srcSemicolon;

  /// Has comma `,` after the last tag?
  [[nodiscard]] bool hasTrailingCommaOnTags() const {
    return !tags.empty() && !tags.back().srcComma.empty();
  }
};

/// A `tag` declaration. (This is an extension!)
class SMDL_EXPORT Tag final : public DeclSubclass<DeclKind::Tag> {
public:
  explicit Tag(std::string_view srcKwTag, Name name,
               std::string_view srcSemicolon)
      : srcKwTag(srcKwTag), name(std::move(name)), srcSemicolon(srcSemicolon) {}

  /// The keyword `tag`.
  std::string_view srcKwTag;

  /// The name.
  Name name;

  /// The semicolon `;`.
  std::string_view srcSemicolon;
};

/// A `typedef` declaration.
class SMDL_EXPORT Typedef final : public DeclSubclass<DeclKind::Typedef> {
public:
  explicit Typedef(std::string_view srcKwTypedef, BumpPtr<Type> type, Name name,
                   std::string_view srcSemicolon)
      : srcKwTypedef(srcKwTypedef), type(std::move(type)),
        name(std::move(name)), srcSemicolon(srcSemicolon) {}

  /// The keyword `typedef`.
  std::string_view srcKwTypedef;

  /// The type.
  BumpPtr<Type> type;

  /// The name.
  Name name;

  /// The semicolon `;`.
  std::string_view srcSemicolon;
};

/// A `unit_test` declaration. (This is an extension!)
class SMDL_EXPORT UnitTest final : public DeclSubclass<DeclKind::UnitTest> {
public:
  explicit UnitTest(std::string_view srcKwUnitTest, BumpPtr<LiteralString> name,
                    BumpPtr<Stmt> stmt)
      : srcKwUnitTest(srcKwUnitTest), name(std::move(name)),
        stmt(std::move(stmt)) {}

  /// The keyword `unit_test`.
  std::string_view srcKwUnitTest;

  /// The literal string name.
  BumpPtr<LiteralString> name;

  /// The body statement.
  BumpPtr<Stmt> stmt;
};

/// A `using ... = ...;` declaration.
class SMDL_EXPORT UsingAlias final : public DeclSubclass<DeclKind::UsingAlias> {
public:
  explicit UsingAlias(std::string_view srcKwUsing, Name name,
                      std::string_view srcEqual, ImportPath importPath,
                      std::string_view srcSemicolon)
      : srcKwUsing(srcKwUsing), name(std::move(name)), srcEqual(srcEqual),
        importPath(std::move(importPath)), srcSemicolon(srcSemicolon) {}

  /// The keyword `using`.
  std::string_view srcKwUsing;

  /// The name.
  Name name;

  /// The equal `=`.
  std::string_view srcEqual;

  /// The import path.
  ImportPath importPath;

  /// The semicolon `;`.
  std::string_view srcSemicolon;
};

/// A `using ... import ...;` declaration.
class SMDL_EXPORT UsingImport final
    : public DeclSubclass<DeclKind::UsingImport> {
public:
  class Name final {
  public:
    /// The name.
    std::string_view srcName;

    /// The next comma `,`. This may be empty!
    std::string_view srcComma;
  };

  explicit UsingImport(std::string_view srcKwUsing, ImportPath importPath,
                       std::string_view srcKwImport, std::vector<Name> names,
                       std::string_view srcSemicolon)
      : srcKwUsing(srcKwUsing), importPath(std::move(importPath)),
        srcKwImport(srcKwImport), names(std::move(names)),
        srcSemicolon(srcSemicolon) {}

  [[nodiscard]] bool isImportAll() const {
    return names.size() == 1 && names[0].srcName == "*";
  }

  /// The keyword `using`.
  std::string_view srcKwUsing;

  /// The import path.
  ImportPath importPath;

  /// The keyword `import`.
  std::string_view srcKwImport;

  /// The names.
  std::vector<Name> names;

  /// The semicolon `;`.
  std::string_view srcSemicolon;

  /// Has comma `,` after last import name?
  [[nodiscard]] bool hasTrailingComma() const {
    return !names.empty() && !names.back().srcComma.empty();
  }
};

/// A variable declaration.
class SMDL_EXPORT Variable final : public DeclSubclass<DeclKind::Variable> {
public:
  class Declarator final : public NodeSubclass<NodeKind::VariableDeclarator> {
  public:
    /// Is destructure declarator? E.g., `{foo, bar, baz}`
    [[nodiscard]] bool isDestructure() const noexcept {
      return !srcBraceL.empty() && !srcBraceR.empty();
    }

  public:
    /// The variable declaration that contains this declarator.
    Variable *const decl{};

    class DeclaratorName final {
    public:
      /// The name.
      Name name;

      /// The next comma `,`. This may be empty!
      std::string_view srcComma{};
    };

    /// The destructure brace `{`. This may be empty!
    std::string_view srcBraceL;

    /// The name(s).
    std::vector<DeclaratorName> names;

    /// The destructure brace `}`. This may be empty!
    std::string_view srcBraceR;

    /// The equal `=`. This may be empty!
    std::string_view srcEqual;

    /// The initializer expression. This may be null!
    BumpPtr<Expr> exprInit;

    /// The initializer arguments. This may be null!
    std::optional<ArgumentList> argsInit;

    /// The annotations.
    BumpPtr<AnnotationBlock> annotations;

    /// The next comma `,`. This may be empty!
    std::string_view srcComma;

    /// Has warning been issued about this variable yet? Used to prevent
    /// the same warning being logged over and over again.
    bool warningIssued{};
  };

  explicit Variable(BumpPtr<Type> type, std::vector<Declarator> declarators,
                    std::string_view srcSemicolon)
      : type(std::move(type)), declarators(std::move(declarators)),
        srcSemicolon(srcSemicolon) {
    for (auto &declarator : this->declarators) {
      const_cast<Variable *&>(declarator.decl) = this;
    }
  }

  /// The type.
  BumpPtr<Type> type;

  /// The declarators.
  std::vector<Declarator> declarators;

  /// The semicolon `;`.
  std::string_view srcSemicolon;

  /// Has comma `,` after the last declarator?
  [[nodiscard]] bool hasTrailingComma() const {
    return !declarators.empty() && !declarators.back().srcComma.empty();
  }
};

} // namespace smdl::AST
