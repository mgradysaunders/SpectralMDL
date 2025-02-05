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

enum class NodeKind : uint8_t { Decl, Expr, File, Stmt };

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

enum class DeclKind : uint8_t {
  Enum,
  Function,
  Import,
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
  explicit Decl(DeclKind declKind) : declKind(declKind) {}

  /// The declaration kind.
  const DeclKind declKind;

  /// Is a global declaration?
  bool isGlobal{};

  /// The keyword `export`. This may be empty!
  std::string_view srcKwExport{};

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
  LateIf,
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

} // namespace smdl::AST
