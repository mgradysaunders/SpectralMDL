// vim:foldmethod=marker:foldlevel=0:fmr=--{,--}
#pragma once

#include "llvm.h"

namespace smdl::Compiler {

class Crumb;
class Function;
class Module;
class Type;

} // namespace smdl::Compiler

namespace smdl::AST {

class SourceLocation final {
public:
  [[nodiscard]] constexpr operator bool() const { return line != 0; }

  llvm::StringRef file{};

  uint32_t line{};

  void report_error(std::string message) const { throw Error(std::move(message), file.str(), line); }

  void report_warning(std::string message) const {
    llvm::WithColor(llvm::errs(), llvm::HighlightColor::Warning) << "[warning] ";
    if (!file.empty() && line > 0)
      llvm::WithColor(llvm::errs(), llvm::HighlightColor::Address) << '[' << file << ':' << line << "] ";
    llvm::errs() << message << '\n';
  }
};

/// This is the data structure to hold the MDL version which must appear at the
/// top of each MDL file. E.g., `mdl 1.7;`
class Version final {
public:
  [[nodiscard]] constexpr bool operator==(const Version &) const = default;
  [[nodiscard]] constexpr bool operator!=(const Version &) const = default;
  [[nodiscard]] constexpr auto operator<=>(const Version &) const = default;
  uint8_t major{};
  uint8_t minor{};

  [[nodiscard]] static constexpr Version builtin_version() { return {1, 9}; }
};

/// An enum to represent frequency qualifiers. We don't really need these for
/// anything in CPU-only compilation, but we still have to parse them so we may
/// as well keep them around.
enum class FrequencyQualifier : uint8_t { Uniform, Varying };

enum class Precision : uint8_t { Default, Single, Double };

class Node;
class Expr;
class Decl;
class Stmt;

enum class NodeKind : uint8_t { Expr, Decl, Stmt, File };

class Node {
public:
  explicit Node(NodeKind nodeKind) : nodeKind(nodeKind) {}

  virtual ~Node() = default;

  const NodeKind nodeKind;

  SourceLocation srcLoc{};

  llvm::StringRef src{};
};

template <NodeKind K> class NodeSubclass : public Node {
public:
  NodeSubclass() : Node(K) {}

  static bool classof(const Node *node) { return node->nodeKind == K; }
};

//--{ Expr
enum class ExprKind : uint8_t {
  Binary,
  Call,
  Cast,
  Conditional,
  GetField,
  GetIndex,
  Identifier,
  Intrinsic,
  Let,
  LiteralBool,
  LiteralFloat,
  LiteralInt,
  LiteralString,
  Name,
  Parens,
  ReturnFrom,
  SizeName,
  Type,
  Unary,
};

class Expr : public NodeSubclass<NodeKind::Expr> {
public:
  explicit Expr(ExprKind exprKind) : exprKind(exprKind) {}

  const ExprKind exprKind;
};

template <ExprKind K> class ExprSubclass : public Expr {
public:
  ExprSubclass() : Expr(K) {}

  static bool classof(const Expr *expr) { return expr->exprKind == K; }

  static bool classof(const Node *node) { return node->nodeKind == NodeKind::Expr && classof(static_cast<const Expr *>(node)); }
};

class Name final : public ExprSubclass<ExprKind::Name> {
public:
  explicit Name(llvm::StringRef name) : name(name) {}

  llvm::StringRef name{};
};

class Identifier final : public ExprSubclass<ExprKind::Identifier> {
public:
  Identifier(llvm::SmallVector<unique_bump_ptr<Name>> names, bool isAbs) : names(std::move(names)), isAbs(isAbs) {}

  [[nodiscard]] llvm::SmallVector<llvm::StringRef> get_string_refs() const {
    auto nameRefs{llvm::SmallVector<llvm::StringRef>{}};
    for (auto &name : names)
      nameRefs.push_back(name->name);
    return nameRefs;
  }

  [[nodiscard]] bool is_simple_name() const { return names.size() == 1 && !isAbs; }

  [[nodiscard]] operator std::string() const;

  llvm::SmallVector<unique_bump_ptr<Name>> names{};

  bool isAbs{};
};

class Arg final {
public:
  Arg() = default;

  Arg(unique_bump_ptr<Name> name, unique_bump_ptr<Expr> expr) : name(std::move(name)), expr(std::move(expr)) {}

  /// The name. This may be null.
  unique_bump_ptr<Name> name{};

  /// The expression. This may NOT be null.
  unique_bump_ptr<Expr> expr{};

  /// The source location.
  SourceLocation srcLoc{};

  /// The entire source-code range containing the argument name and expression. Useful
  /// for outputting in '#assert(...)'.
  llvm::StringRef src{};

  /// Is marked with the 'visit' keyword?
  bool isVisit{};
};

class ArgList final {
public:
  /// The arguments.
  llvm::SmallVector<Arg> args{};

  /// The source location of the opening '('.
  SourceLocation srcLoc{};

  /// The entire source-code range between the opening '(' and closing ')'.
  llvm::StringRef src{};
};

class Annotation final {
public:
  Annotation(unique_bump_ptr<Identifier> identifier, ArgList args) : identifier(std::move(identifier)), args(std::move(args)) {}

  unique_bump_ptr<Identifier> identifier{};

  ArgList args{};
};

using AnnotationBlock = llvm::SmallVector<Annotation>;

[[nodiscard]] inline bool has_annotation(const AnnotationBlock &annotations, auto &&pred) {
  return std::find_if(annotations.begin(), annotations.end(), std::forward<decltype(pred)>(pred)) != annotations.end();
}

class Type final : public ExprSubclass<ExprKind::Type> {
public:
  struct Attrs final {
    uint8_t isConst : 1 {};  ///< `const`
    uint8_t isStatic : 1 {}; ///< `static`
    uint8_t isInline : 1 {}; ///< `inline`
  };

  Type(
      std::optional<FrequencyQualifier> frequency, Attrs attrs, unique_bump_ptr<Expr> expr,
      std::optional<AnnotationBlock> annotations = {})
      : frequency(frequency), attrs(attrs), expr(std::move(expr)), annotations(std::move(annotations)) {}

  std::optional<FrequencyQualifier> frequency{};

  Attrs attrs{};

  unique_bump_ptr<Expr> expr{};

  std::optional<AnnotationBlock> annotations{};

  Compiler::Type *type{};
};

class Param final {
public:
  unique_bump_ptr<Type> type{};

  unique_bump_ptr<Name> name{};

  unique_bump_ptr<Expr> init{};

  std::optional<AnnotationBlock> annotations{};
};

class ParamList final {
public:
  [[nodiscard]] bool empty() const { return params.empty(); }

  [[nodiscard]] size_t size() const { return params.size(); }

  [[nodiscard]] auto begin() const { return params.begin(); }

  [[nodiscard]] auto end() const { return params.end(); }

  llvm::SmallVector<Param> params{};

  bool isVarArg{};

  llvm::StringRef src{};
};

//--{ Unary
enum class UnaryOp : uint32_t {
  Incr = 1,   ///< "++"
  Decr,       ///< "--"
  Pos,        ///< "+"
  Neg,        ///< "-"
  Not,        ///< "~"
  LogicalNot, ///< "!"
  Address,    ///< "&" NOTE: This is non-standard.
  Deref,      ///< "*" NOTE: This is non-standard.
  Maybe,      ///< "?" NOTE: This is non-standard.
  Postfix = 1UL << 31,
  PostfixIncr = Postfix | Incr,
  PostfixDecr = Postfix | Decr,
};

[[nodiscard]] constexpr UnaryOp operator&(UnaryOp opA, UnaryOp opB) { return UnaryOp(uint32_t(opA) & uint32_t(opB)); }

[[nodiscard]] constexpr UnaryOp operator|(UnaryOp opA, UnaryOp opB) { return UnaryOp(uint32_t(opA) | uint32_t(opB)); }

[[nodiscard]] constexpr UnaryOp operator~(UnaryOp op) { return UnaryOp(~uint32_t(op)); }

[[nodiscard]] const char *to_string(UnaryOp op);

class Unary final : public ExprSubclass<ExprKind::Unary> {
public:
  Unary(UnaryOp op, unique_bump_ptr<Expr> expr) : op(op), expr(std::move(expr)) {}

  UnaryOp op{};

  unique_bump_ptr<Expr> expr{};
};
//--}

//--{ Binary
enum class BinaryOp : uint32_t {
  Add = 1,                    ///< "+"
  Sub = 2,                    ///< "-"
  Mul = 3,                    ///< "*"
  Div = 4,                    ///< "/"
  Rem = 5,                    ///< "%"
  And = 6,                    ///< "&"
  Or = 7,                     ///< "|"
  Xor = 8,                    ///< "^"
  Shl = 9,                    ///< "<<"
  AShr = 10,                  ///< ">>"
  LShr = 11,                  ///< ">>>"
  Eq = 1UL << 31,             ///< "="
  EqAdd = Eq | Add,           ///< "+="
  EqSub = Eq | Sub,           ///< "-="
  EqMul = Eq | Mul,           ///< "*="
  EqDiv = Eq | Div,           ///< "/="
  EqRem = Eq | Rem,           ///< "%="
  EqAnd = Eq | And,           ///< "&="
  EqOr = Eq | Or,             ///< "|="
  EqXor = Eq | Xor,           ///< "^="
  EqShl = Eq | Shl,           ///< "<<="
  EqAShr = Eq | AShr,         ///< ">>="
  EqLShr = Eq | LShr,         ///< ">>>="
  CmpEq = 12,                 ///< "=="
  CmpNe = 13,                 ///< "!="
  CmpLt = 14,                 ///< "<"
  CmpGt = 15,                 ///< ">"
  CmpLe = 16,                 ///< "<="
  CmpGe = 17,                 ///< ">="
  Logical = 1UL << 30,        ///< Is logical?
  LogicalAnd = Logical | And, ///< "&&"
  LogicalOr = Logical | Or,   ///< "||"
  Comma = 18,                 ///< ","
  Def = 19,                   ///< ":=" NOTE: This is non-standard.
  Subset = 20,                ///< "<:" NOTE: This is non-standard.
};

[[nodiscard]] constexpr bool is_compare_op(BinaryOp op) { return BinaryOp::CmpEq <= op && op <= BinaryOp::CmpGe; }

[[nodiscard]] constexpr BinaryOp operator&(BinaryOp opA, BinaryOp opB) { return BinaryOp(uint32_t(opA) & uint32_t(opB)); }

[[nodiscard]] constexpr BinaryOp operator|(BinaryOp opA, BinaryOp opB) { return BinaryOp(uint32_t(opA) | uint32_t(opB)); }

[[nodiscard]] constexpr BinaryOp operator~(BinaryOp op) { return BinaryOp(~uint32_t(op)); }

[[nodiscard]] const char *to_string(BinaryOp op);

class Binary final : public ExprSubclass<ExprKind::Binary> {
public:
  Binary(BinaryOp op, unique_bump_ptr<Expr> lhs, unique_bump_ptr<Expr> rhs)
      : op(op), lhs(std::move(lhs)), rhs(std::move(rhs)) {}

  BinaryOp op{};

  unique_bump_ptr<Expr> lhs{};

  unique_bump_ptr<Expr> rhs{};
};
//--}

//--{ Conditional
class Conditional final : public ExprSubclass<ExprKind::Conditional> {
public:
  Conditional(unique_bump_ptr<Expr> cond, unique_bump_ptr<Expr> ifPass, unique_bump_ptr<Expr> ifFail)
      : cond(std::move(cond)), ifPass(std::move(ifPass)), ifFail(std::move(ifFail)) {}

  unique_bump_ptr<Expr> cond{};

  unique_bump_ptr<Expr> ifPass{};

  unique_bump_ptr<Expr> ifFail{};
};
//--}

class Variable;

//--{ Let
class Let final : public ExprSubclass<ExprKind::Let> {
public:
  Let(llvm::SmallVector<unique_bump_ptr<Variable>> vars, unique_bump_ptr<Expr> expr);

  ~Let();

  llvm::SmallVector<unique_bump_ptr<Variable>> vars{};

  unique_bump_ptr<Expr> expr{};
};
//--}

//--{ GetField
class GetField final : public ExprSubclass<ExprKind::GetField> {
public:
  GetField(unique_bump_ptr<Expr> what, unique_bump_ptr<Name> name) : what(std::move(what)), name(std::move(name)) {}

  unique_bump_ptr<Expr> what{};

  unique_bump_ptr<Name> name{};
};
//--}

//--{ GetIndex
class GetIndex final : public ExprSubclass<ExprKind::GetIndex> {
public:
  GetIndex(unique_bump_ptr<Expr> expr, llvm::SmallVector<unique_bump_ptr<Expr>> indices)
      : expr(std::move(expr)), indices(std::move(indices)) {}

  unique_bump_ptr<Expr> expr{};

  llvm::SmallVector<unique_bump_ptr<Expr>> indices{};
};
//--}

//--{ Cast
class Cast final : public ExprSubclass<ExprKind::Cast> {
public:
  Cast(unique_bump_ptr<Type> type, unique_bump_ptr<Expr> expr) : type(std::move(type)), expr(std::move(expr)) {}

  unique_bump_ptr<Type> type{};

  unique_bump_ptr<Expr> expr{};
};
//--}

//--{ Call
class Call final : public ExprSubclass<ExprKind::Call> {
public:
  explicit Call(unique_bump_ptr<Expr> expr, ArgList args) : expr(std::move(expr)), args(std::move(args)) {}

  unique_bump_ptr<Expr> expr{};

  ArgList args{};
};
//--}

//--{ SizeName
class SizeName final : public ExprSubclass<ExprKind::SizeName> {
public:
  explicit SizeName(unique_bump_ptr<Name> name) : name(std::move(name)) {}

  unique_bump_ptr<Name> name{};
};
//--}

//--{ Literal[...]
class LiteralBool final : public ExprSubclass<ExprKind::LiteralBool> {
public:
  explicit LiteralBool(bool value) : value(value) {}

  bool value{};
};

class LiteralInt final : public ExprSubclass<ExprKind::LiteralInt> {
public:
  explicit LiteralInt(uint64_t value) : value(value) {}

  uint64_t value{};
};

class LiteralFloat final : public ExprSubclass<ExprKind::LiteralFloat> {
public:
  explicit LiteralFloat(double value, Precision precision) : value(value), precision(precision) {}

  double value{};

  Precision precision{};
};

class LiteralString final : public ExprSubclass<ExprKind::LiteralString> {
public:
  explicit LiteralString(llvm::SmallString<64> value) : value(std::move(value)) {}

  llvm::SmallString<64> value{};
};
//--}

class Intrinsic final : public ExprSubclass<ExprKind::Intrinsic> {
public:
  explicit Intrinsic(llvm::StringRef name) : name(name) {}

  llvm::StringRef name{};
};

class ReturnFrom final : public ExprSubclass<ExprKind::ReturnFrom> {
public:
  explicit ReturnFrom(unique_bump_ptr<Stmt> stmt);

  ~ReturnFrom();

  unique_bump_ptr<Stmt> stmt;
};

class Parens final : public ExprSubclass<ExprKind::Parens> {
public:
  explicit Parens(unique_bump_ptr<Expr> expr, bool isCompileTime) : expr(std::move(expr)), isCompileTime(isCompileTime) {}

  unique_bump_ptr<Expr> expr{};

  bool isCompileTime{};
};
//--}

//--{ Decl
enum class DeclKind : uint8_t { Enum, Function, Import, Struct, Tag, Typedef, UnitTest, UsingAlias, UsingImport, Variable };

/// The base type for declarations.
class Decl : public NodeSubclass<NodeKind::Decl> {
public:
  explicit Decl(DeclKind declKind) : declKind(declKind) {}

  const DeclKind declKind;

  /// Is a global declaration?
  bool isGlobal{};

  /// Is exported with the `export` keyword?
  bool isExport{};

  /// The associated compiler module.
  Compiler::Module *module{};

  /// The associated compiler crumb.
  Compiler::Crumb *crumb{};
};

template <DeclKind K> class DeclSubclass : public Decl {
public:
  DeclSubclass() : Decl(K) {}

  static bool classof(const Decl *decl) { return decl->declKind == K; }

  static bool classof(const Node *node) { return node->nodeKind == NodeKind::Decl && classof(static_cast<const Decl *>(node)); }
};

class Enum final : public DeclSubclass<DeclKind::Enum> {
public:
  struct Declarator final {
    unique_bump_ptr<Name> name{};

    unique_bump_ptr<Expr> init{};

    std::optional<AnnotationBlock> annotations{};

    llvm::ConstantInt *llvmConst{};
  };

  Enum(unique_bump_ptr<Name> name, std::optional<AnnotationBlock> annotations, llvm::SmallVector<Declarator> declarators)
      : name(std::move(name)), annotations(std::move(annotations)), declarators(std::move(declarators)) {}

  unique_bump_ptr<Name> name{};

  std::optional<AnnotationBlock> annotations{};

  llvm::SmallVector<Declarator> declarators{};
};

class Function final : public DeclSubclass<DeclKind::Function> {
public:
  struct Attrs final {
    /// Is marked `@(alwaysinline)`?
    uint8_t isAlwaysInline : 1 {};

    /// Is marked `@(cold)`?
    uint8_t isCold : 1 {};

    /// Is marked `@(foreign)`?
    uint8_t isForeign : 1 {};

    /// Is marked `@(hot)`?
    uint8_t isHot : 1 {};

    /// Is marked `@(macro)`?
    uint8_t isMacro : 1 {};

    /// Is marked `@(noinline)`?
    uint8_t isNoInline : 1 {};

    /// Is marked `@(optnone)`?
    uint8_t isOptNone : 1 {};

    /// Is marked `@(optsize)`?
    uint8_t isOptSize : 1 {};

    /// Is marked `@(pure)`?
    uint8_t isPure : 1 {};

    /// Is marked `@(visible)`?
    uint8_t isVisible : 1 {};
  };

  struct LetAndCall final {
    /// The let expression. This may be null.
    Let *let{};

    /// The call expression. This must be non-null.
    Call *call{};

    [[nodiscard]] operator bool() const { return call; }
  };

  Function(
      Attrs attrs, bool isVariant, unique_bump_ptr<Type> returnType, std::optional<AnnotationBlock> earlyAnnotations,
      unique_bump_ptr<Name> name, ParamList params, std::optional<FrequencyQualifier> frequency,
      std::optional<AnnotationBlock> lateAnnotations, unique_bump_ptr<Node> definition)
      : attrs(attrs), isVariant(isVariant), returnType(std::move(returnType)), earlyAnnotations(std::move(earlyAnnotations)),
        name(std::move(name)), params(std::move(params)), frequency(frequency), lateAnnotations(std::move(lateAnnotations)),
        definition(std::move(definition)) {}

  /// If this is a function variant, get the variant let and call expressions. Else throw an error.
  [[nodiscard]] LetAndCall get_variant_let_and_call_expressions() const;

public:
  const Attrs attrs{};

  const bool isVariant{};

  unique_bump_ptr<Type> returnType{};

  std::optional<AnnotationBlock> earlyAnnotations{};

  unique_bump_ptr<Name> name{};

  ParamList params{};

  std::optional<FrequencyQualifier> frequency{};

  std::optional<AnnotationBlock> lateAnnotations{};

  unique_bump_ptr<Node> definition{};
};

class Import final : public DeclSubclass<DeclKind::Import> {
public:
  Import(llvm::SmallVector<unique_bump_ptr<Identifier>> paths) : paths(std::move(paths)) {}

  llvm::SmallVector<unique_bump_ptr<Identifier>> paths{};
};

class Struct final : public DeclSubclass<DeclKind::Struct> {
public:
  struct Field final {
    /// Is marked with the `void` keyword?
    ///
    /// This is used to ignore struct members that appear only for API compatibility with the MDL specification.
    /// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    /// export struct diffuse_transmission_bsdf: bsdf {
    ///   color tint = color(1.0);
    ///   void string handle = ""; // Don't care
    /// };
    /// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    bool isVoid{};

    /// The type.
    unique_bump_ptr<Type> type{};

    /// The name.
    unique_bump_ptr<Name> name{};

    /// Optional! The initializer expression.
    unique_bump_ptr<Expr> init{};

    /// Optional! The annotation block.
    std::optional<AnnotationBlock> annotations{};
  };

  struct Tag final {
    /// Is marked with the `default` keyword?
    bool isDefault{};

    /// The tag type. This should resolve to `Identifier`, but it is represented by a `Type` in order for
    /// the compiler to easily resolve the tag type it represents and store it in the AST using existing
    /// mechanisms.
    unique_bump_ptr<Type> type{};
  };

  Struct(
      unique_bump_ptr<Name> name, std::optional<AnnotationBlock> annotations, llvm::SmallVector<Field> fields,
      llvm::SmallVector<Tag> tags = {})
      : name(std::move(name)), annotations(std::move(annotations)), fields(std::move(fields)), tags(std::move(tags)) {}

  unique_bump_ptr<Name> name{};

  std::optional<AnnotationBlock> annotations{};

  llvm::SmallVector<Field> fields{};

  llvm::SmallVector<Tag> tags{};
};

class Tag final : public DeclSubclass<DeclKind::Tag> {
public:
  Tag(unique_bump_ptr<Name> name) : name(std::move(name)) {}

  unique_bump_ptr<Name> name{};
};

class Typedef final : public DeclSubclass<DeclKind::Typedef> {
public:
  Typedef(unique_bump_ptr<Type> type, unique_bump_ptr<Name> name) : type(std::move(type)), name(std::move(name)) {}

  unique_bump_ptr<Type> type{};

  unique_bump_ptr<Name> name{};
};

class UnitTest final : public DeclSubclass<DeclKind::UnitTest> {
public:
  UnitTest(llvm::SmallString<64> name, unique_bump_ptr<Node> body) : name(std::move(name)), body(std::move(body)) {}

  llvm::SmallString<64> name{};

  unique_bump_ptr<Node> body{};
};

class UsingAlias final : public DeclSubclass<DeclKind::UsingAlias> {
public:
  UsingAlias(unique_bump_ptr<Name> name, unique_bump_ptr<Identifier> path) : name(std::move(name)), path(std::move(path)) {}

  unique_bump_ptr<Name> name{};

  unique_bump_ptr<Identifier> path{};
};

class UsingImport final : public DeclSubclass<DeclKind::UsingImport> {
public:
  UsingImport(unique_bump_ptr<Identifier> path, llvm::SmallVector<unique_bump_ptr<Name>> names)
      : path(std::move(path)), names(std::move(names)) {}

  [[nodiscard]] bool is_import_all() const { return names.empty(); }

  unique_bump_ptr<Identifier> path{};

  llvm::SmallVector<unique_bump_ptr<Name>> names{};
};

class Variable final : public DeclSubclass<DeclKind::Variable> {
public:
  class Declarator final {
  public:
    unique_bump_ptr<Name> name{};

    unique_bump_ptr<Expr> init{};

    std::optional<ArgList> args{};

    std::optional<AnnotationBlock> annotations{};
  };

  Variable(unique_bump_ptr<Type> type, llvm::SmallVector<Declarator, 1> declarators)
      : type(std::move(type)), declarators(std::move(declarators)) {}

  unique_bump_ptr<Type> type{};

  llvm::SmallVector<Declarator, 1> declarators{};
};
//--}

//--{ Stmt
enum class StmtKind : uint8_t {
  Break,
  Compound,
  Continue,
  DeclStmt,
  Defer, ///< NOTE: This is non-standard.
  DoWhile,
  ExprStmt,
  For,
  If,
  Preserve, ///< NOTE: This is non-standard.
  Return,
  Switch,
  Unreachable, ///< NOTE: This is non-standard.
  Visit,       ///< NOTE: This is non-standard.
  While,
};

[[nodiscard]] const char *to_string(StmtKind kind);

class Stmt : public NodeSubclass<NodeKind::Stmt> {
public:
  explicit Stmt(StmtKind stmtKind) : stmtKind(stmtKind) {}

  const StmtKind stmtKind;
};

template <StmtKind K> class StmtSubclass : public Stmt {
public:
  StmtSubclass() : Stmt(K) {}

  static bool classof(const Stmt *stmt) { return stmt->stmtKind == K; }

  static bool classof(const Node *node) { return node->nodeKind == NodeKind::Stmt && classof(static_cast<const Stmt *>(node)); }
};

class Break final : public StmtSubclass<StmtKind::Break> {
public:
  Break(unique_bump_ptr<Expr> cond) : cond(std::move(cond)) {};

  // The late 'if' condition. NOTE: This is non-standard.
  unique_bump_ptr<Expr> cond{};
};

class Compound final : public StmtSubclass<StmtKind::Compound> {
public:
  Compound(llvm::SmallVector<unique_bump_ptr<Stmt>> stmts) : stmts(std::move(stmts)) {}

  llvm::SmallVector<unique_bump_ptr<Stmt>> stmts{};
};

class Continue final : public StmtSubclass<StmtKind::Continue> {
public:
  Continue(unique_bump_ptr<Expr> cond) : cond(std::move(cond)) {}

  // The late 'if' condition. NOTE: This is non-standard.
  unique_bump_ptr<Expr> cond{};
};

class DeclStmt final : public StmtSubclass<StmtKind::DeclStmt> {
public:
  DeclStmt(unique_bump_ptr<Decl> decl) : decl(std::move(decl)) {}

  unique_bump_ptr<Decl> decl{};
};

class Defer final : public StmtSubclass<StmtKind::Defer> {
public:
  Defer(unique_bump_ptr<Stmt> stmt) : stmt(std::move(stmt)) {}

  unique_bump_ptr<Stmt> stmt{};

  Compiler::Crumb *crumb{};
};

class DoWhile final : public StmtSubclass<StmtKind::DoWhile> {
public:
  DoWhile(unique_bump_ptr<Stmt> body, unique_bump_ptr<Expr> cond) : body(std::move(body)), cond(std::move(cond)) {}

  unique_bump_ptr<Stmt> body{};

  unique_bump_ptr<Expr> cond{};
};

class ExprStmt final : public StmtSubclass<StmtKind::ExprStmt> {
public:
  ExprStmt() = default; // Empty statement

  ExprStmt(unique_bump_ptr<Expr> expr, unique_bump_ptr<Expr> cond = {}) : expr(std::move(expr)), cond(std::move(cond)) {}

  unique_bump_ptr<Expr> expr{};

  unique_bump_ptr<Expr> cond{};
};

class For final : public StmtSubclass<StmtKind::For> {
public:
  For(unique_bump_ptr<Stmt> init, unique_bump_ptr<Expr> cond, unique_bump_ptr<Expr> incr, unique_bump_ptr<Stmt> body)
      : init(std::move(init)), cond(std::move(cond)), incr(std::move(incr)), body(std::move(body)) {}

  unique_bump_ptr<Stmt> init{};

  unique_bump_ptr<Expr> cond{};

  unique_bump_ptr<Expr> incr{};

  unique_bump_ptr<Stmt> body{};
};

class If final : public StmtSubclass<StmtKind::If> {
public:
  If(unique_bump_ptr<Expr> cond, unique_bump_ptr<Stmt> ifPass, unique_bump_ptr<Stmt> ifFail)
      : cond(std::move(cond)), ifPass(std::move(ifPass)), ifFail(std::move(ifFail)) {}

  unique_bump_ptr<Expr> cond{};

  unique_bump_ptr<Stmt> ifPass{};

  unique_bump_ptr<Stmt> ifFail{};
};

class Preserve final : public StmtSubclass<StmtKind::Preserve> {
public:
  Preserve(llvm::SmallVector<unique_bump_ptr<Expr>> exprs) : exprs(std::move(exprs)) {}

  llvm::SmallVector<unique_bump_ptr<Expr>> exprs{};
};

class Return final : public StmtSubclass<StmtKind::Return> {
public:
  Return(unique_bump_ptr<Expr> expr, unique_bump_ptr<Expr> cond = {}) : expr(std::move(expr)), cond(std::move(cond)) {}

  unique_bump_ptr<Expr> expr{};

  // The late 'if' condition. NOTE: This is non-standard.
  unique_bump_ptr<Expr> cond{};
};

class Switch final : public StmtSubclass<StmtKind::Switch> {
public:
  class Case final {
  public:
    [[nodiscard]] bool is_default() const { return !cond.get(); }

    unique_bump_ptr<Expr> cond{};

    llvm::SmallVector<unique_bump_ptr<Stmt>> stmts{};
  };

  Switch(unique_bump_ptr<Expr> what, llvm::SmallVector<Case> cases) : what(std::move(what)), cases(std::move(cases)) {}

  unique_bump_ptr<Expr> what{};

  llvm::SmallVector<Case> cases{};
};

class Unreachable final : public StmtSubclass<StmtKind::Unreachable> {};

class Visit final : public StmtSubclass<StmtKind::Visit> {
public:
  Visit(unique_bump_ptr<Name> name, unique_bump_ptr<Expr> what, unique_bump_ptr<Stmt> body)
      : name(std::move(name)), what(std::move(what)), body(std::move(body)) {}

  unique_bump_ptr<Name> name{};

  unique_bump_ptr<Expr> what{};

  unique_bump_ptr<Stmt> body{};
};

class While final : public StmtSubclass<StmtKind::While> {
public:
  While(unique_bump_ptr<Expr> cond, unique_bump_ptr<Stmt> body) : cond(std::move(cond)), body(std::move(body)) {}

  unique_bump_ptr<Expr> cond{};

  unique_bump_ptr<Stmt> body{};
};
//--}

class File final : public NodeSubclass<NodeKind::File> {
public:
  File(
      bool isSmdlSyntax, Version version, llvm::SmallVector<unique_bump_ptr<Decl>> imports,
      std::optional<AnnotationBlock> annotations, llvm::SmallVector<unique_bump_ptr<Decl>> globals)
      : isSmdlSyntax(isSmdlSyntax), version(version), imports(std::move(imports)), annotations(std::move(annotations)),
        globals(std::move(globals)) {}

  bool isSmdlSyntax{};

  Version version{};

  llvm::SmallVector<unique_bump_ptr<Decl>> imports{};

  std::optional<AnnotationBlock> annotations{};

  llvm::SmallVector<unique_bump_ptr<Decl>> globals{};
};

} // namespace smdl::AST
