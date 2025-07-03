/// \file
#pragma once

#include "smdl/common.h"

/// Abstract-syntax-tree interfaces.
namespace smdl::AST {

/// The representation of a simple name in the AST.
class SMDL_EXPORT Name final {
public:
  Name() = default;

  explicit Name(SourceLocation srcLoc, std::string_view srcName)
      : srcLoc(srcLoc), srcName(srcName) {}

  [[nodiscard]] operator bool() const { return !srcName.empty(); }

  [[nodiscard]] operator std::string_view() const { return srcName; }

  [[nodiscard]] operator Span<std::string_view>() const { return srcName; }

  /// The source location.
  SourceLocation srcLoc{};

  /// The source name.
  std::string_view srcName{};
};

enum class NodeKind : uint8_t {
  File,               ///< The kind of the top-level file.
  Decl,               ///< The base kind of all declarations in `AST/Decl.h`.
  Expr,               ///< The base kind of all expressions in `AST/Expr.h`.
  Stmt,               ///< The base kind of all statements in `AST/Stmt.h`.
  Parameter,          ///< The kind of `Parameter`.
  Field,              ///< The kind of `Struct::Field`.
  EnumDeclarator,     ///< The kind of `Enum::Declarator`.
  VariableDeclarator, ///< The kind of `Variable::Declarator`.
};

/// The base type for all nodes.
class SMDL_EXPORT Node {
public:
  explicit Node(NodeKind nodeKind) : nodeKind(nodeKind) {}

  virtual ~Node() = default;

  /// The source location.
  SourceLocation srcLoc{};

  /// The node kind.
  const NodeKind nodeKind;
};

/// The helper to define node subclasses that sets the `NodeKind` and
/// implements `classof`.
template <NodeKind K> class SMDL_EXPORT NodeSubclass : public Node {
public:
  NodeSubclass() : Node(K) {}

  /// The `classof` implementation for `llvm::isa` and `llvm::dyn_cast`.
  static bool classof(const Node *node) { return node->nodeKind == K; }
};

/// Convert `NodeKind` to string.
[[nodiscard]] SMDL_EXPORT std::string_view to_string(NodeKind nodeKind);

enum class DeclKind : uint8_t {
  AnnotationDecl,
  Enum,
  Exec,
  Function,
  Import,
  Namespace,
  Struct,
  Tag,
  Typedef,
  UnitTest,
  UsingAlias,
  UsingImport,
  Variable,
};

/// The base type for declarations.
class SMDL_EXPORT Decl : public NodeSubclass<NodeKind::Decl> {
public:
  /// The attributes, e.g., `@(pure noinline)`.
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

  explicit Decl(DeclKind declKind) : declKind(declKind) {}

  /// The declaration kind.
  const DeclKind declKind;

  /// Is a global declaration?
  bool isGlobal{};

  /// The attributes. This may be null!
  std::optional<Attributes> attributes{};

  /// The keyword `export`. This may be empty!
  std::string_view srcKwExport{};

  /// Has the given attribute?
  [[nodiscard]] bool has_attribute(std::string_view attr) const {
    return attributes && attributes->has(attr);
  }

  /// Is marked with the keyword `export`?
  [[nodiscard]] bool is_exported() const { return !srcKwExport.empty(); }
};

/// The helper to define declaration subclasses that sets the `DeclKind` and
/// implements `classof`.
template <DeclKind K> class SMDL_EXPORT DeclSubclass : public Decl {
public:
  DeclSubclass() : Decl(K) {}

  /// The `classof` implementation for `llvm::isa` and `llvm::dyn_cast`.
  static bool classof(const Decl *decl) { return decl->declKind == K; }

  /// The `classof` implementation for `llvm::isa` and `llvm::dyn_cast`.
  static bool classof(const Node *node) {
    return node->nodeKind == NodeKind::Decl &&
           classof(static_cast<const Decl *>(node));
  }
};

/// Convert `DeclKind` to string.
[[nodiscard]] SMDL_EXPORT std::string_view to_string(DeclKind declKind);

enum class ExprKind : uint8_t {
  AccessField,
  AccessIndex,
  Binary,
  Call,
  Identifier,
  Intrinsic,
  Let,
  LiteralBool,
  LiteralFloat,
  LiteralInt,
  LiteralString,
  Parens,
  ReturnFrom,
  Select,
  SizeName,
  Type,
  TypeCast,
  Unary,
};

/// The base type for expressions.
class SMDL_EXPORT Expr : public NodeSubclass<NodeKind::Expr> {
public:
  explicit Expr(ExprKind exprKind) : exprKind(exprKind) {}

  /// The expression kind.
  const ExprKind exprKind;
};

/// The helper to define expression subclasses that sets the `ExprKind` and
/// implements `classof`.
template <ExprKind K> class SMDL_EXPORT ExprSubclass : public Expr {
public:
  ExprSubclass() : Expr(K) {}

  /// The `classof` implementation for `llvm::isa` and `llvm::dyn_cast`.
  static bool classof(const Expr *expr) { return expr->exprKind == K; }

  /// The `classof` implementation for `llvm::isa` and `llvm::dyn_cast`.
  static bool classof(const Node *node) {
    return node->nodeKind == NodeKind::Expr &&
           classof(static_cast<const Expr *>(node));
  }
};

/// Convert `ExprKind` to string.
[[nodiscard]] SMDL_EXPORT std::string_view to_string(ExprKind exprKind);

enum class StmtKind : uint8_t {
  Break,
  Compound,
  Continue,
  DeclStmt,
  Defer,
  DoWhile,
  ExprStmt,
  For,
  If,
  Preserve,
  Return,
  Switch,
  Unreachable,
  Visit,
  While,
};

/// The base type for statements.
class SMDL_EXPORT Stmt : public NodeSubclass<NodeKind::Stmt> {
public:
  explicit Stmt(StmtKind stmtKind) : stmtKind(stmtKind) {}

  /// The statement kind.
  const StmtKind stmtKind;
};

/// The helper to define statement subclasses that sets the `StmtKind` and
/// implements `classof`.
template <StmtKind K> class SMDL_EXPORT StmtSubclass : public Stmt {
public:
  StmtSubclass() : Stmt(K) {}

  /// The `classof` implementation for `llvm::isa` and `llvm::dyn_cast`.
  static bool classof(const Stmt *stmt) { return stmt->stmtKind == K; }

  /// The `classof` implementation for `llvm::isa` and `llvm::dyn_cast`.
  static bool classof(const Node *node) {
    return node->nodeKind == NodeKind::Stmt &&
           classof(static_cast<const Stmt *>(node));
  }
};

/// Convert `StmtKind` to string.
[[nodiscard]] SMDL_EXPORT std::string_view to_string(StmtKind stmtKind);

} // namespace smdl::AST
