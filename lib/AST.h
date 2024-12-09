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

using SourceRef = llvm::StringRef;

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

  // TODO Probably get rid of this in the future? (in favor of SourceLocation on held SourceRefs)
  SourceLocation srcLoc{};

  // TODO Probably get rid of this in the future?
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
  explicit Name(SourceRef srcName) : srcName(srcName) {}

  SourceRef srcName{};
};

class Identifier final : public ExprSubclass<ExprKind::Identifier> {
public:
  Identifier(vector_or_SmallVector<unique_bump_ptr<Name>> names, bool isAbs) : names(std::move(names)), isAbs(isAbs) {}

  [[nodiscard]] vector_or_SmallVector<llvm::StringRef> get_string_refs() const {
    auto nameRefs{vector_or_SmallVector<llvm::StringRef>{}};
    for (auto &name : names)
      nameRefs.push_back(name->srcName);
    return nameRefs;
  }

  [[nodiscard]] bool is_simple_name() const { return names.size() == 1 && !isAbs; }

  [[nodiscard]] operator std::string() const;

  vector_or_SmallVector<unique_bump_ptr<Name>> names{};

  bool isAbs{};
};

class Arg final {
public:
  /// Is marked with the 'visit' keyword?
  bool isVisit{};

  /// The keyword `visit`. This may be empty!
  SourceRef srcKwVisit{};

  /// The name. This may be null!
  unique_bump_ptr<Name> name{};

  /// The colon `:` after the name. This may be empty!
  SourceRef srcColonAfterName{};

  /// The expression.
  unique_bump_ptr<Expr> expr{};

  /// The next comma `,`. This may be empty!
  SourceRef srcComma{};

  /// The source location.
  SourceLocation srcLoc{};

  /// The entire source-code range containing the argument name and expression. Useful
  /// for outputting in '#assert(...)'.
  llvm::StringRef src{};
};

class ArgList final {
public:
  /// The parenthesis `(`.
  SourceRef srcParenL{};

  /// The arguments.
  vector_or_SmallVector<Arg> args{};

  /// The parenthesis `)`.
  SourceRef srcParenR{};

  /// The source location of the opening `(`.
  SourceLocation srcLoc{};

  /// The source region between the opening `(` and closing `)`.
  llvm::StringRef src{};
};

class Annotation final {
public:
  /// The identifier.
  unique_bump_ptr<Identifier> identifier{};

  // The arguments.
  ArgList args{};

  /// The next comma `,`. This may be empty!
  SourceRef srcComma{};
};

class AnnotationBlock final {
public:
  explicit AnnotationBlock(SourceRef srcDoubleBrackL, vector_or_SmallVector<Annotation> annotations, SourceRef srcDoubleBrackR)
      : srcDoubleBrackL(srcDoubleBrackL), annotations(std::move(annotations)), srcDoubleBrackR(srcDoubleBrackR) {}

  SourceRef srcDoubleBrackL{};

  vector_or_SmallVector<Annotation> annotations{};

  SourceRef srcDoubleBrackR{};
};

class Type final : public ExprSubclass<ExprKind::Type> {
public:
  struct Attrs final {
    uint8_t isConst : 1 {};  ///< `const`
    uint8_t isStatic : 1 {}; ///< `static`
    uint8_t isInline : 1 {}; ///< `inline`
  };

  Type(
      std::optional<FrequencyQualifier> frequency, Attrs attrs, unique_bump_ptr<Expr> expr,
      unique_bump_ptr<AnnotationBlock> annotations = {})
      : frequency(frequency), attrs(attrs), expr(std::move(expr)), annotations(std::move(annotations)) {}

  std::optional<FrequencyQualifier> frequency{};

  Attrs attrs{};

  unique_bump_ptr<Expr> expr{};

  unique_bump_ptr<AnnotationBlock> annotations{};

  Compiler::Type *type{};
};

class Param final {
public:
  /// The type.
  unique_bump_ptr<Type> type{};

  /// The name.
  unique_bump_ptr<Name> name{};

  /// The equal `=`. This may be empty!
  SourceRef srcEq{};

  /// The initializer expression. This may be null!
  unique_bump_ptr<Expr> init{};

  /// The annotations.
  unique_bump_ptr<AnnotationBlock> annotations{};

  /// The next comma `,`. This may be empty!
  SourceRef srcComma{};
};

class ParamList final {
public:
  [[nodiscard]] bool empty() const { return params.empty(); }

  [[nodiscard]] size_t size() const { return params.size(); }

  [[nodiscard]] auto begin() const { return params.begin(); }

  [[nodiscard]] auto end() const { return params.end(); }

  /// The parenthesis `(`.
  SourceRef srcParenL{};

  /// The parameters.
  vector_or_SmallVector<Param> params{};

  /// The parenthesis `)`.
  SourceRef srcParenR{};

  /// The source region between the opening `(` and closing `)`.
  llvm::StringRef src{};
};

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
  explicit Unary(SourceRef srcOp, UnaryOp op, unique_bump_ptr<Expr> expr) : srcOp(srcOp), op(op), expr(std::move(expr)) {}

  [[nodiscard]] bool is_postfix() const { return (op & UnaryOp::Postfix) == UnaryOp::Postfix; }

  [[nodiscard]] bool is_prefix() const { return !is_postfix(); }

  /// The source of the operator.
  SourceRef srcOp{};

  /// The operator.
  UnaryOp op{};

  /// The expression.
  unique_bump_ptr<Expr> expr{};
};

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
  explicit Binary(unique_bump_ptr<Expr> lhs, SourceRef srcOp, BinaryOp op, unique_bump_ptr<Expr> rhs)
      : lhs(std::move(lhs)), srcOp(srcOp), op(op), rhs(std::move(rhs)) {}

  /// The left-hand side expression.
  unique_bump_ptr<Expr> lhs{};

  /// The source of the operator.
  SourceRef srcOp{};

  /// The operator.
  BinaryOp op{};

  /// The right-hand side expression.
  unique_bump_ptr<Expr> rhs{};
};

class Conditional final : public ExprSubclass<ExprKind::Conditional> {
public:
  explicit Conditional(
      unique_bump_ptr<Expr> cond, SourceRef srcQuestion, unique_bump_ptr<Expr> ifPass, SourceRef srcColon,
      unique_bump_ptr<Expr> ifFail)
      : cond(std::move(cond)), srcQuestion(srcQuestion), ifPass(std::move(ifPass)), srcColon(srcColon),
        ifFail(std::move(ifFail)) {}

  /// The condition expression.
  unique_bump_ptr<Expr> cond{};

  /// The question mark `?`.
  SourceRef srcQuestion{};

  /// The if-pass expression.
  unique_bump_ptr<Expr> ifPass{};

  /// The colon `:`.
  SourceRef srcColon{};

  /// The if-fail expression.
  unique_bump_ptr<Expr> ifFail{};
};

class Variable;

class Let final : public ExprSubclass<ExprKind::Let> {
public:
  explicit Let(
      SourceRef srcKwLet, SourceRef srcBraceL, vector_or_SmallVector<unique_bump_ptr<Variable>> vars, SourceRef srcBraceR,
      SourceRef srcKwIn, unique_bump_ptr<Expr> expr);

  ~Let();

  /// The keyword `let`.
  SourceRef srcKwLet{};

  /// The brace `{`. This may be empty!
  SourceRef srcBraceL{};

  /// The variables.
  vector_or_SmallVector<unique_bump_ptr<Variable>> vars{};

  /// The brace `}`. This may be empty!
  SourceRef srcBraceR{};

  /// The keyword `in`.
  SourceRef srcKwIn{};

  /// The expression.
  unique_bump_ptr<Expr> expr{};
};

class GetField final : public ExprSubclass<ExprKind::GetField> {
public:
  explicit GetField(unique_bump_ptr<Expr> expr, SourceRef srcDot, unique_bump_ptr<Name> name)
      : expr(std::move(expr)), srcDot(srcDot), name(std::move(name)) {}

  /// The expression.
  unique_bump_ptr<Expr> expr{};

  /// The dot `.`.
  SourceRef srcDot{};

  /// The field name.
  unique_bump_ptr<Name> name{};
};

class GetIndex final : public ExprSubclass<ExprKind::GetIndex> {
public:
  struct Index final {
    /// The square bracket `[`.
    SourceRef srcBrackL{};

    /// The expression. This may be null! (`[]`)
    unique_bump_ptr<Expr> expr{};

    /// The square bracket `]`.
    SourceRef srcBrackR{};
  };

  explicit GetIndex(unique_bump_ptr<Expr> expr, vector_or_SmallVector<Index> indexes)
      : expr(std::move(expr)), indexes(std::move(indexes)) {}

  unique_bump_ptr<Expr> expr{};

  vector_or_SmallVector<Index> indexes{};
};

class Cast final : public ExprSubclass<ExprKind::Cast> {
public:
  explicit Cast(
      SourceRef srcKwCast, SourceRef srcAngleL, unique_bump_ptr<Type> type, SourceRef srcAngleR, unique_bump_ptr<Expr> expr)
      : srcKwCast(srcKwCast), srcAngleL(srcAngleL), type(std::move(type)), srcAngleR(srcAngleR), expr(std::move(expr)) {}

  /// The keyword `cast`.
  SourceRef srcKwCast{};

  /// The angle bracket `<`.
  SourceRef srcAngleL{};

  /// The type.
  unique_bump_ptr<Type> type{};

  /// The angle bracket `>`.
  SourceRef srcAngleR{};

  /// The expression.
  unique_bump_ptr<Expr> expr{};
};

class Call final : public ExprSubclass<ExprKind::Call> {
public:
  explicit Call(unique_bump_ptr<Expr> expr, ArgList args) : expr(std::move(expr)), args(std::move(args)) {}

  /// The expression.
  unique_bump_ptr<Expr> expr{};

  /// The arguments.
  ArgList args{};
};

class SizeName final : public ExprSubclass<ExprKind::SizeName> {
public:
  explicit SizeName(SourceRef srcAngleL, unique_bump_ptr<Name> name, SourceRef srcAngleR)
      : srcAngleL(srcAngleL), name(std::move(name)), srcAngleR(srcAngleR) {}

  /// The angle bracket `<`.
  SourceRef srcAngleL{};

  /// The name.
  unique_bump_ptr<Name> name{};

  /// The angle bracket `>`.
  SourceRef srcAngleR{};
};

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

class Intrinsic final : public ExprSubclass<ExprKind::Intrinsic> {
public:
  explicit Intrinsic(llvm::StringRef name) : name(name) {}

  llvm::StringRef name{};
};

class ReturnFrom final : public ExprSubclass<ExprKind::ReturnFrom> {
public:
  explicit ReturnFrom(SourceRef srcKwReturnFrom, unique_bump_ptr<Stmt> stmt);

  ~ReturnFrom();

  /// The keyword `return_from`.
  SourceRef srcKwReturnFrom{};

  /// The statement.
  unique_bump_ptr<Stmt> stmt;
};

class Parens final : public ExprSubclass<ExprKind::Parens> {
public:
  explicit Parens(bool isCompileTime, SourceRef srcDollar, SourceRef srcParenL, unique_bump_ptr<Expr> expr, SourceRef srcParenR)
      : isCompileTime(isCompileTime), srcDollar(srcDollar), srcParenL(srcParenL), expr(std::move(expr)), srcParenR(srcParenR) {}

  /// Is compile time?
  bool isCompileTime{};

  /// The dollar sign `$`. This may be empty!
  SourceRef srcDollar{};

  /// The parenthesis `(`.
  SourceRef srcParenL{};

  /// The expression.
  unique_bump_ptr<Expr> expr{};

  /// The parenthesis `)`.
  SourceRef srcParenR{};
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

  /// Is exported with the keyword `export`?
  bool isExport{};

  /// The keyword `export`. This may be empty!
  SourceRef srcKwExport{};

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
    /// The name.
    unique_bump_ptr<Name> name{};

    /// The equal `=`. This may be empty!
    SourceRef srcEq{};

    /// The initializer expression. This may be null!
    unique_bump_ptr<Expr> init{};

    /// The annotations.
    unique_bump_ptr<AnnotationBlock> annotations{};

    /// The next comma `,`. This may be empty!
    SourceRef srcComma{};

    /// The LLVM constant value (This is computed later during compilation)
    llvm::ConstantInt *llvmConst{};
  };

  explicit Enum(
      SourceRef srcKwEnum, unique_bump_ptr<Name> name, unique_bump_ptr<AnnotationBlock> annotations, SourceRef srcBraceL,
      vector_or_SmallVector<Declarator> declarators, SourceRef srcBraceR, SourceRef srcSemicolon)
      : srcKwEnum(srcKwEnum), name(std::move(name)), annotations(std::move(annotations)), srcBraceL(srcBraceL),
        declarators(std::move(declarators)), srcBraceR(srcBraceR), srcSemicolon(srcSemicolon) {}

  /// The keyword `enum`.
  SourceRef srcKwEnum{};

  /// The name.
  unique_bump_ptr<Name> name{};

  /// The annotations.
  unique_bump_ptr<AnnotationBlock> annotations{};

  /// The brace `{`.
  SourceRef srcBraceL{};

  /// The declarators.
  vector_or_SmallVector<Declarator> declarators{};

  /// The brace `}`.
  SourceRef srcBraceR{};

  /// The semicolon `;`.
  SourceRef srcSemicolon{};
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
      Attrs attrs, bool isVariant, unique_bump_ptr<Type> returnType, unique_bump_ptr<AnnotationBlock> earlyAnnotations,
      unique_bump_ptr<Name> name, ParamList params, std::optional<FrequencyQualifier> frequency,
      unique_bump_ptr<AnnotationBlock> lateAnnotations, unique_bump_ptr<Node> definition)
      : attrs(attrs), isVariant(isVariant), returnType(std::move(returnType)), earlyAnnotations(std::move(earlyAnnotations)),
        name(std::move(name)), params(std::move(params)), frequency(frequency), lateAnnotations(std::move(lateAnnotations)),
        definition(std::move(definition)) {}

  /// If this is a function variant, get the variant let and call expressions. Else throw an error.
  [[nodiscard]] LetAndCall get_variant_let_and_call_expressions() const;

public:
  const Attrs attrs{};

  const bool isVariant{};

  unique_bump_ptr<Type> returnType{};

  unique_bump_ptr<AnnotationBlock> earlyAnnotations{};

  unique_bump_ptr<Name> name{};

  ParamList params{};

  std::optional<FrequencyQualifier> frequency{};

  unique_bump_ptr<AnnotationBlock> lateAnnotations{};

  unique_bump_ptr<Node> definition{};
};

class Import final : public DeclSubclass<DeclKind::Import> {
public:
  struct ImportPath final {
    /// The identifier.
    unique_bump_ptr<Identifier> identifier{};

    /// The next comma `,`. This may be empty!
    SourceRef srcComma{};
  };

  explicit Import(SourceRef srcKwImport, vector_or_SmallVector<ImportPath> paths, SourceRef srcSemicolon)
      : srcKwImport(srcKwImport), paths(std::move(paths)), srcSemicolon(srcSemicolon) {}

  /// The keyword `import`.
  SourceRef srcKwImport{};

  /// The paths.
  vector_or_SmallVector<ImportPath> paths{};

  /// The semicolon `;`.
  SourceRef srcSemicolon{};
};

class Struct final : public DeclSubclass<DeclKind::Struct> {
public:
  struct Field final {
    /// Is marked with the keyword `void`?
    ///
    /// This omits members that appear only for API compatibility with the MDL specification.
    /// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    /// export struct diffuse_transmission_bsdf: bsdf {
    ///   color tint = color(1.0);
    ///   void string handle = ""; // Don't care
    /// };
    /// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    bool isVoid{};

    /// The keyword `void`. This may be empty!
    SourceRef srcKwVoid{};

    /// The type.
    unique_bump_ptr<Type> type{};

    /// The name.
    unique_bump_ptr<Name> name{};

    /// The equal `=`. This may be empty!
    SourceRef srcEq{};

    /// The initializer expression. This may be null!
    unique_bump_ptr<Expr> init{};

    /// The annotation block. This may be null!
    unique_bump_ptr<AnnotationBlock> annotations{};

    /// The semicolon `;`.
    SourceRef srcSemicolon{};
  };

  struct Tag final {
    /// Is marked with the `default` keyword?
    bool isDefault{};

    /// The keyword `default`.
    SourceRef srcKwDefault{};

    /// The tag type. This should resolve to `Identifier`, but it is represented by a `Type` in order for
    /// the compiler to easily resolve the tag type it represents and store it in the AST using existing
    /// mechanisms.
    unique_bump_ptr<Type> type{};

    /// The next comma `,`. This may be empty!
    SourceRef srcComma{};
  };

  explicit Struct(
      SourceRef srcKwStruct, unique_bump_ptr<Name> name, SourceRef srcColonBeforeTags, vector_or_SmallVector<Tag> tags,
      unique_bump_ptr<AnnotationBlock> annotations, SourceRef srcBraceL, vector_or_SmallVector<Field> fields,
      SourceRef srcBraceR, SourceRef srcSemicolon)
      : srcKwStruct(srcKwStruct), name(std::move(name)), srcColonBeforeTags(srcColonBeforeTags), tags(std::move(tags)),
        annotations(std::move(annotations)), srcBraceL(srcBraceL), fields(std::move(fields)), srcBraceR(srcBraceR),
        srcSemicolon(srcSemicolon) {}

  /// The keyword `struct`.
  SourceRef srcKwStruct{};

  /// The name.
  unique_bump_ptr<Name> name{};

  /// The colon `:` before the tags. This may be empty!
  SourceRef srcColonBeforeTags{};

  /// The tags. This may be empty!
  vector_or_SmallVector<Tag> tags{};

  /// The annotations. This may be null!
  unique_bump_ptr<AnnotationBlock> annotations{};

  /// The brace `{`.
  SourceRef srcBraceL{};

  /// The fields.
  vector_or_SmallVector<Field> fields{};

  /// The brace `}`.
  SourceRef srcBraceR{};

  /// The semicolon `;`.
  SourceRef srcSemicolon{};
};

class Tag final : public DeclSubclass<DeclKind::Tag> {
public:
  explicit Tag(SourceRef srcKwTag, unique_bump_ptr<Name> name, SourceRef srcSemicolon)
      : srcKwTag(srcKwTag), name(std::move(name)), srcSemicolon(srcSemicolon) {}

  /// The keyword `tag`.
  SourceRef srcKwTag{};

  /// The name.
  unique_bump_ptr<Name> name{};

  /// The semicolon `;`.
  SourceRef srcSemicolon{};
};

class Typedef final : public DeclSubclass<DeclKind::Typedef> {
public:
  explicit Typedef(SourceRef srcKwTypedef, unique_bump_ptr<Type> type, unique_bump_ptr<Name> name, SourceRef srcSemicolon)
      : srcKwTypedef(srcKwTypedef), type(std::move(type)), name(std::move(name)), srcSemicolon(srcSemicolon) {}

  /// The keyword `typedef`.
  SourceRef srcKwTypedef{};

  /// The type.
  unique_bump_ptr<Type> type{};

  /// The name.
  unique_bump_ptr<Name> name{};

  /// The semicolon `;`.
  SourceRef srcSemicolon{};
};

class UnitTest final : public DeclSubclass<DeclKind::UnitTest> {
public:
  explicit UnitTest(SourceRef srcKwUnitTest, unique_bump_ptr<LiteralString> name, unique_bump_ptr<Node> body)
      : srcKwUnitTest(srcKwUnitTest), name(std::move(name)), body(std::move(body)) {}

  /// The keyword `unit_test`.
  SourceRef srcKwUnitTest{};

  unique_bump_ptr<LiteralString> name{};

  unique_bump_ptr<Node> body{};
};

class UsingAlias final : public DeclSubclass<DeclKind::UsingAlias> {
public:
  explicit UsingAlias(
      SourceRef srcKwUsing, unique_bump_ptr<Name> name, SourceRef srcEq, unique_bump_ptr<Identifier> path,
      SourceRef srcSemicolon)
      : srcKwUsing(srcKwUsing), name(std::move(name)), srcEq(srcEq), path(std::move(path)), srcSemicolon(srcSemicolon) {}

  /// The keyword `using`.
  SourceRef srcKwUsing{};

  /// The name.
  unique_bump_ptr<Name> name{};

  /// The equal `=`.
  SourceRef srcEq{};

  /// The path.
  unique_bump_ptr<Identifier> path{};

  /// The semicolon `;`.
  SourceRef srcSemicolon{};
};

class UsingImport final : public DeclSubclass<DeclKind::UsingImport> {
public:
  struct ImportName final {
    /// The name.
    SourceRef srcName{};

    /// The next comma `,`. This may be empty!
    SourceRef srcComma{};
  };

  explicit UsingImport(
      SourceRef srcKwUsing, unique_bump_ptr<Identifier> path, SourceRef srcKwImport, vector_or_SmallVector<ImportName> names,
      SourceRef srcSemicolon)
      : srcKwUsing(srcKwUsing), path(std::move(path)), srcKwImport(srcKwImport), names(std::move(names)),
        srcSemicolon(srcSemicolon) {}

  [[nodiscard]] bool is_import_all() const { return names.size() == 1 && names[0].srcName == "*"; }

  /// The keyword `using`.
  SourceRef srcKwUsing{};

  /// The path.
  unique_bump_ptr<Identifier> path{};

  /// The keyword `import`.
  SourceRef srcKwImport{};

  /// The names, or empty if import all.
  vector_or_SmallVector<ImportName> names{};

  /// The semicolon `;`.
  SourceRef srcSemicolon{};
};

class Variable final : public DeclSubclass<DeclKind::Variable> {
public:
  struct Declarator final {
    /// The name.
    unique_bump_ptr<Name> name{};

    /// The equal `=`. This may be empty!
    SourceRef srcEq{};

    /// The initializer. This may be null!
    unique_bump_ptr<Expr> init{};

    /// The constructor arguments. This may be null!
    std::optional<ArgList> args{};

    /// The annotations.
    unique_bump_ptr<AnnotationBlock> annotations{};

    /// The next comma `,`. This may be empty!
    SourceRef srcComma{};
  };

  explicit Variable(unique_bump_ptr<Type> type, vector_or_SmallVector<Declarator> declarators, SourceRef srcSemicolon)
      : type(std::move(type)), declarators(std::move(declarators)), srcSemicolon(srcSemicolon) {}

  /// The type.
  unique_bump_ptr<Type> type{};

  /// The declarators.
  vector_or_SmallVector<Declarator> declarators{};

  /// The semicolon `;`.
  SourceRef srcSemicolon{};
};
//--}

//--{ Stmt
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
  While
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

class LateIf final {
public:
  explicit LateIf(SourceRef srcKwIf, unique_bump_ptr<Expr> cond) : srcKwIf(srcKwIf), cond(std::move(cond)) {}

  /// The keyword `if`.
  SourceRef srcKwIf{};

  /// The condition expression.
  unique_bump_ptr<Expr> cond{};
};

class Break final : public StmtSubclass<StmtKind::Break> {
public:
  explicit Break(SourceRef srcKwBreak, std::optional<LateIf> lateIf, SourceRef srcSemicolon)
      : srcKwBreak(srcKwBreak), lateIf(std::move(lateIf)), srcSemicolon(srcSemicolon) {}

  /// The keyword `break`.
  SourceRef srcKwBreak{};

  /// The late if.
  std::optional<LateIf> lateIf{};

  /// The semicolon `;`.
  SourceRef srcSemicolon{};
};

class Compound final : public StmtSubclass<StmtKind::Compound> {
public:
  explicit Compound(SourceRef srcBraceL, vector_or_SmallVector<unique_bump_ptr<Stmt>> stmts, SourceRef srcBraceR)
      : srcBraceL(srcBraceL), stmts(std::move(stmts)), srcBraceR(srcBraceR) {}

  /// The brace `{`.
  SourceRef srcBraceL{};

  /// The statements.
  vector_or_SmallVector<unique_bump_ptr<Stmt>> stmts{};

  /// The brace `}`.
  SourceRef srcBraceR{};
};

class Continue final : public StmtSubclass<StmtKind::Continue> {
public:
  explicit Continue(SourceRef srcKwContinue, std::optional<LateIf> lateIf, SourceRef srcSemicolon)
      : srcKwContinue(srcKwContinue), lateIf(std::move(lateIf)), srcSemicolon(srcSemicolon) {}

  /// The keyword `continue`.
  SourceRef srcKwContinue{};

  /// The late if.
  std::optional<LateIf> lateIf{};

  /// The semicolon `;`.
  SourceRef srcSemicolon{};
};

class DeclStmt final : public StmtSubclass<StmtKind::DeclStmt> {
public:
  explicit DeclStmt(unique_bump_ptr<Decl> decl) : decl(std::move(decl)) {}

  unique_bump_ptr<Decl> decl{};
};

class Defer final : public StmtSubclass<StmtKind::Defer> {
public:
  explicit Defer(SourceRef srcKwDefer, unique_bump_ptr<Stmt> stmt) : srcKwDefer(srcKwDefer), stmt(std::move(stmt)) {}

  /// The keyword `defer`.
  SourceRef srcKwDefer{};

  /// The statement.
  unique_bump_ptr<Stmt> stmt{};

  Compiler::Crumb *crumb{};
};

class DoWhile final : public StmtSubclass<StmtKind::DoWhile> {
public:
  explicit DoWhile(
      SourceRef srcKwDo, unique_bump_ptr<Stmt> body, SourceRef srcKwWhile, unique_bump_ptr<Expr> cond, SourceRef srcSemicolon)
      : srcKwDo(srcKwDo), body(std::move(body)), srcKwWhile(srcKwWhile), cond(std::move(cond)), srcSemicolon(srcSemicolon) {}

  /// The keyword `do`.
  SourceRef srcKwDo{};

  /// The loop body statement.
  unique_bump_ptr<Stmt> body{};

  /// The keyword `while`.
  SourceRef srcKwWhile{};

  /// The loop condition expression.
  unique_bump_ptr<Expr> cond{};

  /// The semicolon `;`.
  SourceRef srcSemicolon{};
};

class ExprStmt final : public StmtSubclass<StmtKind::ExprStmt> {
public:
  ExprStmt() = default; // Empty statement

  explicit ExprStmt(unique_bump_ptr<Expr> expr, std::optional<LateIf> lateIf, SourceRef srcSemicolon)
      : expr(std::move(expr)), lateIf(std::move(lateIf)), srcSemicolon(srcSemicolon) {}

  /// The expression.
  unique_bump_ptr<Expr> expr{};

  /// The late if.
  std::optional<LateIf> lateIf{};

  /// The semicolon `;`.
  SourceRef srcSemicolon{};
};

class For final : public StmtSubclass<StmtKind::For> {
public:
  explicit For(
      SourceRef srcKwFor, SourceRef srcParenL, unique_bump_ptr<Stmt> init, unique_bump_ptr<Expr> cond,
      SourceRef srcSemicolonAfterCond, unique_bump_ptr<Expr> incr, SourceRef srcParenR, unique_bump_ptr<Stmt> body)
      : srcKwFor(srcKwFor), srcParenL(srcParenL), init(std::move(init)), cond(std::move(cond)),
        srcSemicolonAfterCond(srcSemicolonAfterCond), incr(std::move(incr)), srcParenR(srcParenR), body(std::move(body)) {}

  /// The keyword `for`.
  SourceRef srcKwFor{};

  /// The parenthesis `(`.
  SourceRef srcParenL{};

  /// The initializer.
  unique_bump_ptr<Stmt> init{};

  /// The condition expression.
  unique_bump_ptr<Expr> cond{};

  /// The semicolon `;` after the condition expression.
  SourceRef srcSemicolonAfterCond{};

  /// The increment expression.
  unique_bump_ptr<Expr> incr{};

  /// The parenthesis `)`.
  SourceRef srcParenR{};

  /// The body statement.
  unique_bump_ptr<Stmt> body{};
};

class If final : public StmtSubclass<StmtKind::If> {
public:
  explicit If(
      SourceRef srcKwIf, unique_bump_ptr<Expr> cond, unique_bump_ptr<Stmt> ifPass, SourceRef srcKwElse,
      unique_bump_ptr<Stmt> ifFail)
      : srcKwIf(srcKwIf), cond(std::move(cond)), ifPass(std::move(ifPass)), srcKwElse(srcKwElse), ifFail(std::move(ifFail)) {}

  /// The keyword `if`.
  SourceRef srcKwIf{};

  /// The condition.
  unique_bump_ptr<Expr> cond{};

  /// The if-pass statement.
  unique_bump_ptr<Stmt> ifPass{};

  /// The keyword `else` (may be empty).
  SourceRef srcKwElse{};

  /// The if-fail statement (may be null).
  unique_bump_ptr<Stmt> ifFail{};
};

class Preserve final : public StmtSubclass<StmtKind::Preserve> {
public:
  explicit Preserve(SourceRef srcKwPreserve, vector_or_SmallVector<unique_bump_ptr<Expr>> exprs, SourceRef srcSemicolon)
      : srcKwPreserve(srcKwPreserve), exprs(std::move(exprs)), srcSemicolon(srcSemicolon) {}

  /// The keyword `preserve`.
  SourceRef srcKwPreserve{};

  /// The comma-separated expressions to preserve.
  vector_or_SmallVector<unique_bump_ptr<Expr>> exprs{};

  /// The semicolon `;`.
  SourceRef srcSemicolon{};
};

class Return final : public StmtSubclass<StmtKind::Return> {
public:
  explicit Return(SourceRef srcKwReturn, unique_bump_ptr<Expr> expr, std::optional<LateIf> lateIf, SourceRef srcSemicolon)
      : srcKwReturn(srcKwReturn), expr(std::move(expr)), lateIf(std::move(lateIf)), srcSemicolon(srcSemicolon) {}

  /// The keyword `return`.
  SourceRef srcKwReturn{};

  /// The expression.
  unique_bump_ptr<Expr> expr{};

  /// The late if.
  std::optional<LateIf> lateIf{};

  /// The semicolon `;`.
  SourceRef srcSemicolon{};
};

class Switch final : public StmtSubclass<StmtKind::Switch> {
public:
  struct Case final {
    /// Is default?
    [[nodiscard]] bool is_default() const { return !cond.get(); }

    /// The keyword `case` or `default`.
    SourceRef srcKwCaseOrDefault{};

    /// The condition. This may be null! (if `default`)
    unique_bump_ptr<Expr> cond{};

    /// The colon `:`.
    SourceRef srcColon{};

    /// The statements.
    vector_or_SmallVector<unique_bump_ptr<Stmt>> stmts{};
  };

  explicit Switch(
      SourceRef srcKwSwitch, unique_bump_ptr<Expr> expr, SourceRef srcBraceL, vector_or_SmallVector<Case> cases,
      SourceRef srcBraceR)
      : srcKwSwitch(srcKwSwitch), expr(std::move(expr)), srcBraceL(srcBraceL), cases(std::move(cases)), srcBraceR(srcBraceR) {}

  /// The keyword `switch`.
  SourceRef srcKwSwitch{};

  /// The expression.
  unique_bump_ptr<Expr> expr{};

  /// The brace `{`.
  SourceRef srcBraceL{};

  /// The cases.
  vector_or_SmallVector<Case> cases{};

  /// The brace `}`.
  SourceRef srcBraceR{};
};

class Unreachable final : public StmtSubclass<StmtKind::Unreachable> {
public:
  explicit Unreachable(SourceRef srcKwUnreachable, SourceRef srcSemicolon)
      : srcKwUnreachable(srcKwUnreachable), srcSemicolon(srcSemicolon) {}

  /// The keyword `unreachable`.
  SourceRef srcKwUnreachable{};

  /// The semicolon `;`.
  SourceRef srcSemicolon{};
};

class Visit final : public StmtSubclass<StmtKind::Visit> {
public:
  explicit Visit(
      SourceRef srcKwVisit, unique_bump_ptr<Name> name, SourceRef srcKwIn, unique_bump_ptr<Expr> expr,
      unique_bump_ptr<Stmt> body)
      : srcKwVisit(srcKwVisit), name(std::move(name)), srcKwIn(srcKwIn), expr(std::move(expr)), body(std::move(body)) {}

  /// The keyword `visit`.
  SourceRef srcKwVisit{};

  /// The name.
  unique_bump_ptr<Name> name{};

  /// The keyword `in`.
  SourceRef srcKwIn{};

  /// The expression.
  unique_bump_ptr<Expr> expr{};

  /// The body statement.
  unique_bump_ptr<Stmt> body{};
};

class While final : public StmtSubclass<StmtKind::While> {
public:
  explicit While(SourceRef srcKwWhile, unique_bump_ptr<Expr> cond, unique_bump_ptr<Stmt> body)
      : srcKwWhile(srcKwWhile), cond(std::move(cond)), body(std::move(body)) {}

  /// The keyword `while`.
  SourceRef srcKwWhile{};

  /// The condition expression.
  unique_bump_ptr<Expr> cond{};

  /// The body statement.
  unique_bump_ptr<Stmt> body{};
};
//--}

inline Let::Let(
    SourceRef srcKwLet, SourceRef srcBraceL, vector_or_SmallVector<unique_bump_ptr<Variable>> vars, SourceRef srcBraceR,
    SourceRef srcKwIn, unique_bump_ptr<Expr> expr)
    : srcKwLet(srcKwLet), srcBraceL(srcBraceL), vars(std::move(vars)), srcBraceR(srcBraceR), srcKwIn(srcKwIn),
      expr(std::move(expr)) {}

inline Let::~Let() {}

inline ReturnFrom::ReturnFrom(llvm::StringRef srcKwReturnFrom, unique_bump_ptr<Stmt> stmt)
    : srcKwReturnFrom(srcKwReturnFrom), stmt(std::move(stmt)) {}

inline ReturnFrom::~ReturnFrom() {}

class File final : public NodeSubclass<NodeKind::File> {
public:
  File(
      bool isSmdlSyntax, Version version, vector_or_SmallVector<unique_bump_ptr<Decl>> imports,
      unique_bump_ptr<AnnotationBlock> annotations, vector_or_SmallVector<unique_bump_ptr<Decl>> globals)
      : isSmdlSyntax(isSmdlSyntax), version(version), imports(std::move(imports)), annotations(std::move(annotations)),
        globals(std::move(globals)) {}

  bool isSmdlSyntax{};

  Version version{};

  vector_or_SmallVector<unique_bump_ptr<Decl>> imports{};

  unique_bump_ptr<AnnotationBlock> annotations{};

  vector_or_SmallVector<unique_bump_ptr<Decl>> globals{};
};

} // namespace smdl::AST
