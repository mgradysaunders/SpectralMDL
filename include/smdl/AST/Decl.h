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
  BumpPtr<Identifier> identifier{};

  // The arguments.
  ArgumentList args{};

  /// The next comma `,`. This may be empty!
  std::string_view srcComma{};
};

/// An annotation block (between double brackets `[[ ... ]]`).
class SMDL_EXPORT AnnotationBlock final {
public:
  explicit AnnotationBlock(std::string_view srcDoubleBrackL,
                           std::vector<Annotation> annotations,
                           std::string_view srcDoubleBrackR)
      : srcDoubleBrackL(srcDoubleBrackL), annotations(std::move(annotations)),
        srcDoubleBrackR(srcDoubleBrackR) {}

  /// The source location.
  SourceLocation srcLoc{};

  /// The double bracket `[[`.
  std::string_view srcDoubleBrackL{};

  /// The annotations.
  std::vector<Annotation> annotations{};

  /// The double bracket `]]`.
  std::string_view srcDoubleBrackR{};

  /// Has comma `,` after the last annotation?
  [[nodiscard]] bool has_trailing_comma() const {
    return !annotations.empty() && !annotations.back().srcComma.empty();
  }
};

/// A parameter.
class SMDL_EXPORT Parameter final : public NodeSubclass<NodeKind::Parameter> {
public:
  /// The type.
  BumpPtr<Type> type{};

  /// The name.
  Name name{};

  /// The equal `=`. This may be empty!
  std::string_view srcEqual{};

  /// The initializer expression. This may be null!
  BumpPtr<Expr> exprInit{};

  /// The annotations.
  BumpPtr<AnnotationBlock> annotations{};

  /// The next comma `,`. This may be empty!
  std::string_view srcComma{};

  /// The complete source region.
  std::string_view src{};

  /// Has warning been issued about this parameter yet? Used to prevent
  /// the same warning being logged over and over again.
  bool warningIssued{};
};

/// A parameter list.
class SMDL_EXPORT ParameterList final
    : public NodeSubclass<NodeKind::ParameterList> {
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

  /// Is parameter list for function variant?
  [[nodiscard]] bool is_variant() const { return !srcStar.empty(); }

  /// Has comma `,` after the last parameter?
  [[nodiscard]] bool has_trailing_comma() const {
    return !params.empty() && !params.back().srcComma.empty();
  }

  /// The parenthesis `(`.
  std::string_view srcParenL{};

  /// The source `*` if variant params. This maybe empty!
  std::string_view srcStar{};

  /// The parameters.
  std::vector<Parameter> params{};

  /// The parenthesis `)`.
  std::string_view srcParenR{};
};

class SMDL_EXPORT ImportPath final {
public:
  class Element final {
  public:
    /// The double colon `::` before the name. This may be empty!
    std::string_view srcDoubleColon{};

    /// The source name. This may be empty! (In which case, `literalString` must
    /// not be empty!)
    std::string_view srcName{};

    /// The literal string.
    BumpPtr<LiteralString> literalString{};

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
  [[nodiscard]] bool is_absolute() const {
    return !elements.empty() && !elements[0].srcDoubleColon.empty();
  }

  /// Is relative import path? (e.g., `df::*`)
  [[nodiscard]] bool is_relative() const {
    return !elements.empty() && elements[0].srcDoubleColon.empty();
  }

  /// Is import all? (i.e., ends with `::*`?)
  [[nodiscard]] bool is_import_all() const {
    return !elements.empty() && elements.back().srcName == "*";
  }

  [[nodiscard]] operator Span<std::string_view>() const { return elementViews; }

  /// The elements.
  std::vector<Element> elements{};

  /// The source views extracted from the elements for convenience.
  std::vector<std::string_view> elementViews{};
};

/// An `enum` declaration.
class SMDL_EXPORT Enum final : public DeclSubclass<DeclKind::Enum> {
public:
  /// An `enum` declarator name and optional initializer.
  class Declarator final {
  public:
    /// The name.
    Name name{};

    /// The equal `=`. This may be empty!
    std::string_view srcEqual{};

    /// The initializer expression. This may be null!
    BumpPtr<Expr> exprInit{};

    /// The annotations. This may be null!
    BumpPtr<AnnotationBlock> annotations{};

    /// The next comma `,`. This may be empty!
    std::string_view srcComma{};

    /// The LLVM constant value (This is computed later during compilation)
    llvm::ConstantInt *llvmConst{};
  };

  explicit Enum(std::string_view srcKwEnum, Name name,
                BumpPtr<AnnotationBlock> annotations,
                std::string_view srcBraceL, std::vector<Declarator> declarators,
                std::string_view srcBraceR, std::string_view srcSemicolon)
      : srcKwEnum(srcKwEnum), name(std::move(name)),
        annotations(std::move(annotations)), srcBraceL(srcBraceL),
        declarators(std::move(declarators)), srcBraceR(srcBraceR),
        srcSemicolon(srcSemicolon) {}

  /// The keyword `enum`.
  std::string_view srcKwEnum{};

  /// The name.
  Name name{};

  /// The annotations. This may be null!
  BumpPtr<AnnotationBlock> annotations{};

  /// The brace `{`.
  std::string_view srcBraceL{};

  /// The declarators.
  std::vector<Declarator> declarators{};

  /// The brace `}`.
  std::string_view srcBraceR{};

  /// The semicolon `;`.
  std::string_view srcSemicolon{};

  /// Has comma `,` after the last declarator?
  [[nodiscard]] bool has_trailing_comma() const {
    return !declarators.empty() && !declarators.back().srcComma.empty();
  }
};

/// A function declaration.
class SMDL_EXPORT Function final : public DeclSubclass<DeclKind::Function> {
public:
  /// The function attributes, e.g., `@(pure macro)`.
  class SMDL_EXPORT Attributes final {
  public:
    [[nodiscard]] bool has(std::string_view attr) const {
      return std::find(attrs.begin(), attrs.end(), attr) != attrs.end();
    }

    /// The at `@`.
    std::string_view srcAt{};

    /// The parenthesis `(`.
    std::string_view srcParenL{};

    /// The attributes.
    std::vector<std::string_view> attrs{};

    /// The parenthesis `)`.
    std::string_view srcParenR{};
  };

  class LetAndCall final {
  public:
    /// The let expression. This may be null.
    Let *let{};

    /// The call expression. This must be non-null.
    Call *call{};

    [[nodiscard]] operator bool() const { return call; }
  };

  explicit Function(std::optional<Attributes> attributes,
                    BumpPtr<Type> returnType,
                    BumpPtr<AnnotationBlock> earlyAnnotations, Name name,
                    ParameterList params, std::string_view srcFrequency,
                    BumpPtr<AnnotationBlock> lateAnnotations,
                    std::string_view srcEqual, BumpPtr<Node> definition,
                    std::string_view srcSemicolon)
      : attributes(std::move(attributes)), returnType(std::move(returnType)),
        earlyAnnotations(std::move(earlyAnnotations)), name(name),
        params(std::move(params)), srcFrequency(srcFrequency),
        lateAnnotations(std::move(lateAnnotations)), srcEqual(srcEqual),
        definition(std::move(definition)), srcSemicolon(srcSemicolon) {}

  /// Has the given attribute?
  [[nodiscard]] bool has_attribute(std::string_view attr) const {
    return attributes && attributes->has(attr);
  }

  /// Is a function declaration without a definition?
  [[nodiscard]] bool is_declaration_without_definition() const {
    return !definition;
  }

  /// Is a function variant?
  [[nodiscard]] bool is_variant() const { return params.is_variant(); }

  /// If this is a function variant, get the variant let and call expressions.
  /// Else throw an error.
  [[nodiscard]] LetAndCall get_variant_let_and_call_expressions() const;

public:
  /// The attributes. This may be null!
  std::optional<Attributes> attributes{};

  /// The return type.
  BumpPtr<Type> returnType{};

  /// The early annotations between return type and name. This may be null!
  BumpPtr<AnnotationBlock> earlyAnnotations{};

  /// The name.
  Name name{};

  /// The parameters.
  ParameterList params{};

  /// The frequency qualifier `uniform` or `varying`. This may be empty!
  std::string_view srcFrequency{};

  /// The late annotations between signature and definition. This may be null!
  BumpPtr<AnnotationBlock> lateAnnotations{};

  /// The equal `=`. This may be empty!
  std::string_view srcEqual{};

  /// The definition. This may be null!
  BumpPtr<Node> definition{};

  /// The semicolon `;`. This may be empty!
  std::string_view srcSemicolon{};
};

/// An `import` declaration.
class Import final : public DeclSubclass<DeclKind::Import> {
public:
  class ImportPathWrapper final {
  public:
    /// The import path.
    ImportPath importPath{};

    /// The next comma `,`. This may be empty!
    std::string_view srcComma{};
  };

  explicit Import(std::string_view srcKwImport,
                  std::vector<ImportPathWrapper> importPathWrappers,
                  std::string_view srcSemicolon)
      : srcKwImport(srcKwImport),
        importPathWrappers(std::move(importPathWrappers)),
        srcSemicolon(srcSemicolon) {}

  /// The keyword `import`.
  std::string_view srcKwImport{};

  /// The import paths.
  std::vector<ImportPathWrapper> importPathWrappers{};

  /// The semicolon `;`.
  std::string_view srcSemicolon{};

  /// Has comma `,` after the last import path?
  [[nodiscard]] bool has_trailing_comma() const {
    return !importPathWrappers.empty() &&
           !importPathWrappers.back().srcComma.empty();
  }
};

/// A `namespace` declaration.
class SMDL_EXPORT Namespace final : public DeclSubclass<DeclKind::Namespace> {
public:
  explicit Namespace(std::string_view srcKwNamespace,
                     BumpPtr<Identifier> identifier, std::string_view srcBraceL,
                     std::vector<BumpPtr<Decl>> decls,
                     std::string_view srcBraceR)
      : srcKwNamespace(srcKwNamespace), identifier(std::move(identifier)),
        srcBraceL(srcBraceL), decls(std::move(decls)), srcBraceR(srcBraceR) {}

  /// The keyword `namespace`.
  std::string_view srcKwNamespace{};

  /// The identifier.
  BumpPtr<Identifier> identifier{};

  /// The brace `{`.
  std::string_view srcBraceL{};

  /// The declarations.
  std::vector<BumpPtr<Decl>> decls{};

  /// The brace `}`.
  std::string_view srcBraceR{};

  /// The last crumb for inside-namespace lookup. This is populated later.
  Crumb *lastCrumb{};
};

/// A `struct` declaration.
class SMDL_EXPORT Struct final : public DeclSubclass<DeclKind::Struct> {
public:
  /// A `struct` tag declarator.
  class Tag final {
  public:
    /// Is marked with the keyword `default`?
    [[nodiscard]] bool is_default() const { return !srcKwDefault.empty(); }

    /// The keyword `default`. This may be empty!
    std::string_view srcKwDefault{};

    /// The tag type. This should resolve to `Identifier`, but it is represented
    /// by a `Type` in order for the compiler to easily resolve the tag type it
    /// represents and store it in the AST using existing mechanisms.
    BumpPtr<Type> type{};

    /// The next comma `,`. This may be empty!
    std::string_view srcComma{};
  };

  /// A `struct` field declarator.
  class Field final : public NodeSubclass<NodeKind::StructField> {
  public:
    /// The type.
    BumpPtr<Type> type{};

    /// The name.
    Name name{};

    /// The equal `=`. This may be empty!
    std::string_view srcEqual{};

    /// The initializer expression. This may be null!
    BumpPtr<Expr> exprInit{};

    /// The annotation block. This may be null!
    BumpPtr<AnnotationBlock> annotations{};

    /// The semicolon `;`.
    std::string_view srcSemicolon{};
  };

  explicit Struct(std::string_view srcKwStruct, Name name,
                  std::string_view srcColonBeforeTags, std::vector<Tag> tags,
                  BumpPtr<AnnotationBlock> annotations,
                  std::string_view srcBraceL, std::vector<Field> fields,
                  std::string_view srcKwFinalize, BumpPtr<Stmt> stmtFinalize,
                  std::string_view srcBraceR, std::string_view srcSemicolon)
      : srcKwStruct(srcKwStruct), name(std::move(name)),
        srcColonBeforeTags(srcColonBeforeTags), tags(std::move(tags)),
        annotations(std::move(annotations)), srcBraceL(srcBraceL),
        fields(std::move(fields)), srcKwFinalize(srcKwFinalize),
        stmtFinalize(std::move(stmtFinalize)), srcBraceR(srcBraceR),
        srcSemicolon(srcSemicolon) {}

  /// The keyword `struct`.
  std::string_view srcKwStruct{};

  /// The name.
  Name name{};

  /// The colon `:` before the tags. This may be empty!
  std::string_view srcColonBeforeTags{};

  /// The tags. This may be empty!
  std::vector<Tag> tags{};

  /// The annotations. This may be null!
  BumpPtr<AnnotationBlock> annotations{};

  /// The brace `{`.
  std::string_view srcBraceL{};

  /// The fields.
  std::vector<Field> fields{};

  /// The keyword `finalize`. This may be empty!
  std::string_view srcKwFinalize{};

  /// The statement after `finalize`. This may be nulL!
  BumpPtr<Stmt> stmtFinalize{};

  /// The brace `}`.
  std::string_view srcBraceR{};

  /// The semicolon `;`.
  std::string_view srcSemicolon{};

  /// Has comma `,` after the last tag?
  [[nodiscard]] bool has_trailing_comma_on_tags() const {
    return !tags.empty() && !tags.back().srcComma.empty();
  }
};

/// A `tag` declaration.
class SMDL_EXPORT Tag final : public DeclSubclass<DeclKind::Tag> {
public:
  explicit Tag(std::string_view srcKwTag, Name name,
               std::string_view srcSemicolon)
      : srcKwTag(srcKwTag), name(std::move(name)), srcSemicolon(srcSemicolon) {}

  /// The keyword `tag`.
  std::string_view srcKwTag{};

  /// The name.
  Name name{};

  /// The semicolon `;`.
  std::string_view srcSemicolon{};
};

/// A `typedef` declaration.
class SMDL_EXPORT Typedef final : public DeclSubclass<DeclKind::Typedef> {
public:
  explicit Typedef(std::string_view srcKwTypedef, BumpPtr<Type> type, Name name,
                   std::string_view srcSemicolon)
      : srcKwTypedef(srcKwTypedef), type(std::move(type)),
        name(std::move(name)), srcSemicolon(srcSemicolon) {}

  /// The keyword `typedef`.
  std::string_view srcKwTypedef{};

  /// The type.
  BumpPtr<Type> type{};

  /// The name.
  Name name{};

  /// The semicolon `;`.
  std::string_view srcSemicolon{};
};

/// A `unit_test` declaration.
class SMDL_EXPORT UnitTest final : public DeclSubclass<DeclKind::UnitTest> {
public:
  explicit UnitTest(std::string_view srcKwUnitTest, BumpPtr<LiteralString> name,
                    BumpPtr<Stmt> stmt)
      : srcKwUnitTest(srcKwUnitTest), name(std::move(name)),
        stmt(std::move(stmt)) {}

  /// The keyword `unit_test`.
  std::string_view srcKwUnitTest{};

  /// The literal string name.
  BumpPtr<LiteralString> name{};

  /// The body statement.
  BumpPtr<Stmt> stmt{};
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
  std::string_view srcKwUsing{};

  /// The name.
  Name name{};

  /// The equal `=`.
  std::string_view srcEqual{};

  /// The import path.
  ImportPath importPath{};

  /// The semicolon `;`.
  std::string_view srcSemicolon{};
};

/// A `using ... import ...;` declaration.
class SMDL_EXPORT UsingImport final
    : public DeclSubclass<DeclKind::UsingImport> {
public:
  class Name final {
  public:
    /// The name.
    std::string_view srcName{};

    /// The next comma `,`. This may be empty!
    std::string_view srcComma{};
  };

  explicit UsingImport(std::string_view srcKwUsing, ImportPath importPath,
                       std::string_view srcKwImport, std::vector<Name> names,
                       std::string_view srcSemicolon)
      : srcKwUsing(srcKwUsing), importPath(std::move(importPath)),
        srcKwImport(srcKwImport), names(std::move(names)),
        srcSemicolon(srcSemicolon) {}

  [[nodiscard]] bool is_import_all() const {
    return names.size() == 1 && names[0].srcName == "*";
  }

  /// The keyword `using`.
  std::string_view srcKwUsing{};

  /// The import path.
  ImportPath importPath{};

  /// The keyword `import`.
  std::string_view srcKwImport{};

  /// The names.
  std::vector<Name> names{};

  /// The semicolon `;`.
  std::string_view srcSemicolon{};

  /// Has comma `,` after last import name?
  [[nodiscard]] bool has_trailing_comma() const {
    return !names.empty() && !names.back().srcComma.empty();
  }
};

/// A variable declaration.
class SMDL_EXPORT Variable final : public DeclSubclass<DeclKind::Variable> {
public:
  class Declarator final : public NodeSubclass<NodeKind::VariableDeclarator> {
  public:
    /// The name.
    Name name{};

    /// The equal `=`. This may be empty!
    std::string_view srcEqual{};

    /// The initializer expression. This may be null!
    BumpPtr<Expr> exprInit{};

    /// The initializer arguments. This may be null!
    std::optional<ArgumentList> argsInit{};

    /// The annotations.
    BumpPtr<AnnotationBlock> annotations{};

    /// The next comma `,`. This may be empty!
    std::string_view srcComma{};

    /// Has warning been issued about this variable yet? Used to prevent
    /// the same warning being logged over and over again.
    bool warningIssued{};
  };

  explicit Variable(BumpPtr<Type> type, std::vector<Declarator> declarators,
                    std::string_view srcSemicolon)
      : type(std::move(type)), declarators(std::move(declarators)),
        srcSemicolon(srcSemicolon) {}

  /// The type.
  BumpPtr<Type> type{};

  /// The declarators.
  std::vector<Declarator> declarators{};

  /// The semicolon `;`.
  std::string_view srcSemicolon{};

  /// Has comma `,` after the last declarator?
  [[nodiscard]] bool has_trailing_comma() const {
    return !declarators.empty() && !declarators.back().srcComma.empty();
  }
};

} // namespace smdl::AST
