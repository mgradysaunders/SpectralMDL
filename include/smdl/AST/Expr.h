#pragma once

#include "smdl/AST/Node.h"

namespace smdl::AST {

/// An argument.
class SMDL_EXPORT Argument final {
public:
  /// Is positional or unnamed?
  [[nodiscard]] bool is_positional() const { return name.srcName.empty(); }

  /// Is named?
  [[nodiscard]] bool is_named() const { return !name.srcName.empty(); }

  /// Is marked with the keyword `visit`?
  [[nodiscard]] bool is_visited() const { return !srcKwVisit.empty(); }

public:
  /// The source location.
  SourceLocation srcLoc{};

  /// The keyword `visit`. This may be empty!
  std::string_view srcKwVisit{};

  /// The name. This may be empty!
  Name name{};

  /// The colon `:` after the name. This may be empty!
  std::string_view srcColonAfterName{};

  /// The expression.
  BumpPtr<Expr> expr{};

  /// The next comma `,`. This may be empty!
  std::string_view srcComma{};

  /// The complete source region.
  std::string_view src{};
};

/// An argument list.
class SMDL_EXPORT ArgumentList final {
public:
  /// The size.
  [[nodiscard]] size_t size() const { return args.size(); }

  /// The begin iterator.
  [[nodiscard]] auto begin() { return args.begin(); }

  /// The begin iterator, const variant.
  [[nodiscard]] auto begin() const { return args.begin(); }

  /// The end iterator.
  [[nodiscard]] auto end() { return args.end(); }

  /// The end iterator, const variant.
  [[nodiscard]] auto end() const { return args.end(); }

  /// The access operator.
  [[nodiscard]] auto &operator[](size_t i) { return args[i]; }

  /// The access operator, const variant.
  [[nodiscard]] auto &operator[](size_t i) const { return args[i]; }

  /// Has comma `,` after the last argument?
  [[nodiscard]] bool has_trailing_comma() const {
    return !args.empty() && !args.back().srcComma.empty();
  }

public:
  /// The source location.
  SourceLocation srcLoc{};

  /// The parenthesis `(`.
  std::string_view srcParenL{};

  /// The args.
  std::vector<Argument> args{};

  /// The parenthesis `)`.
  std::string_view srcParenR{};
};

class SMDL_EXPORT AccessField final
    : public ExprSubclass<ExprKind::AccessField> {
public:
  explicit AccessField(BumpPtr<Expr> expr, std::string_view srcDot, Name name)
      : expr(std::move(expr)), srcDot(srcDot), name(std::move(name)) {}

  /// The expression.
  BumpPtr<Expr> expr{};

  /// The dot `.`.
  std::string_view srcDot{};

  /// The field name.
  Name name{};
};

class SMDL_EXPORT AccessIndex final
    : public ExprSubclass<ExprKind::AccessIndex> {
public:
  class Index final {
  public:
    /// The square bracket `[`.
    std::string_view srcBrackL{};

    /// The expression. This may be null! (`[]`)
    BumpPtr<Expr> expr{};

    /// The square bracket `]`.
    std::string_view srcBrackR{};
  };

  explicit AccessIndex(BumpPtr<Expr> expr, std::vector<Index> indexes)
      : expr(std::move(expr)), indexes(std::move(indexes)) {}

  /// The expression.
  BumpPtr<Expr> expr{};

  /// The indexes. (These are kept together instead of nested recursively for
  /// convenience.)
  std::vector<Index> indexes{};
};

inline namespace binary_ops {

enum BinaryOp : uint32_t {
  BINOP_ADD = 1,                             ///< `+`
  BINOP_SUB = 2,                             ///< `-`
  BINOP_MUL = 3,                             ///< `*`
  BINOP_DIV = 4,                             ///< `/`
  BINOP_REM = 5,                             ///< `%`
  BINOP_AND = 6,                             ///< `&`
  BINOP_OR = 7,                              ///< `|`
  BINOP_XOR = 8,                             ///< `^`
  BINOP_SHL = 9,                             ///< `<<`
  BINOP_ASHR = 10,                           ///< `>>`
  BINOP_LSHR = 11,                           ///< `>>>`
  BINOP_EQ = 1UL << 31,                      ///< `=`
  BINOP_EQ_ADD = BINOP_EQ | BINOP_ADD,       ///< `+=`
  BINOP_EQ_SUB = BINOP_EQ | BINOP_SUB,       ///< `-=`
  BINOP_EQ_MUL = BINOP_EQ | BINOP_MUL,       ///< `*=`
  BINOP_EQ_DIV = BINOP_EQ | BINOP_DIV,       ///< `/=`
  BINOP_EQ_REM = BINOP_EQ | BINOP_REM,       ///< `%=`
  BINOP_EQ_AND = BINOP_EQ | BINOP_AND,       ///< `&=`
  BINOP_EQ_OR = BINOP_EQ | BINOP_OR,         ///< `|=`
  BINOP_EQ_XOR = BINOP_EQ | BINOP_XOR,       ///< `^=`
  BINOP_EQ_SHL = BINOP_EQ | BINOP_SHL,       ///< `<<=`
  BINOP_EQ_ASHR = BINOP_EQ | BINOP_ASHR,     ///< `>>=`
  BINOP_EQ_LSHR = BINOP_EQ | BINOP_LSHR,     ///< `>>>=`
  BINOP_CMP_EQ = 12,                         ///< `==`
  BINOP_CMP_NE = 13,                         ///< `!=`
  BINOP_CMP_LT = 14,                         ///< `<`
  BINOP_CMP_GT = 15,                         ///< `>`
  BINOP_CMP_LE = 16,                         ///< `<=`
  BINOP_CMP_GE = 17,                         ///< `>=`
  BINOP_LOGIC = 1UL << 30,                   ///< Is logical?
  BINOP_LOGIC_AND = BINOP_LOGIC | BINOP_AND, ///< `&&`
  BINOP_LOGIC_OR = BINOP_LOGIC | BINOP_OR,   ///< `||`
  BINOP_COMMA = 18,                          ///< `,`
  BINOP_LET = 19,                            ///< `:=`
  BINOP_SUBSET = 20,                         ///< `<:`
};

} // namespace binary_ops

[[nodiscard]] constexpr bool is_compare_op(BinaryOp op) {
  return BINOP_CMP_EQ <= op && op <= BINOP_CMP_GE;
}

[[nodiscard]] constexpr BinaryOp operator&(BinaryOp opA, BinaryOp opB) {
  return BinaryOp(uint32_t(opA) & uint32_t(opB));
}

[[nodiscard]] constexpr BinaryOp operator|(BinaryOp opA, BinaryOp opB) {
  return BinaryOp(uint32_t(opA) | uint32_t(opB));
}

[[nodiscard]] constexpr BinaryOp operator~(BinaryOp op) {
  return BinaryOp(~uint32_t(op));
}

[[nodiscard]] constexpr const char *to_string(BinaryOp op) {
  switch (op) {
  case BINOP_ADD:
    return "+";
  case BINOP_SUB:
    return "-";
  case BINOP_MUL:
    return "*";
  case BINOP_DIV:
    return "/";
  case BINOP_REM:
    return "%";
  case BINOP_AND:
    return "&";
  case BINOP_OR:
    return "|";
  case BINOP_XOR:
    return "^";
  case BINOP_SHL:
    return "<<";
  case BINOP_ASHR:
    return ">>";
  case BINOP_LSHR:
    return ">>>";
  case BINOP_EQ:
    return "=";
  case BINOP_EQ_ADD:
    return "+=";
  case BINOP_EQ_SUB:
    return "-=";
  case BINOP_EQ_MUL:
    return "*=";
  case BINOP_EQ_DIV:
    return "/=";
  case BINOP_EQ_REM:
    return "%=";
  case BINOP_EQ_AND:
    return "&=";
  case BINOP_EQ_OR:
    return "|=";
  case BINOP_EQ_XOR:
    return "^=";
  case BINOP_EQ_SHL:
    return "<<=";
  case BINOP_EQ_ASHR:
    return ">>=";
  case BINOP_EQ_LSHR:
    return ">>>=";
  case BINOP_CMP_EQ:
    return "==";
  case BINOP_CMP_NE:
    return "!=";
  case BINOP_CMP_LT:
    return "<";
  case BINOP_CMP_LE:
    return "<=";
  case BINOP_CMP_GT:
    return ">";
  case BINOP_CMP_GE:
    return ">=";
  case BINOP_LOGIC_AND:
    return "&&";
  case BINOP_LOGIC_OR:
    return "||";
  case BINOP_COMMA:
    return ",";
  case BINOP_LET:
    return ":=";
  case BINOP_SUBSET:
    return "<:";
  default:
    break;
  }
  return {};
}

/// A binary expression.
class SMDL_EXPORT Binary final : public ExprSubclass<ExprKind::Binary> {
public:
  explicit Binary(BumpPtr<Expr> exprLhs, std::string_view srcOp, BinaryOp op,
                  BumpPtr<Expr> exprRhs)
      : exprLhs(std::move(exprLhs)), srcOp(srcOp), op(op),
        exprRhs(std::move(exprRhs)) {}

  /// The left-hand side expression.
  BumpPtr<Expr> exprLhs{};

  /// The source of the operator.
  std::string_view srcOp{};

  /// The operator.
  BinaryOp op{};

  /// The right-hand side expression.
  BumpPtr<Expr> exprRhs{};
};

/// A call expression.
class SMDL_EXPORT Call final : public ExprSubclass<ExprKind::Call> {
public:
  explicit Call(BumpPtr<Expr> expr, ArgumentList args)
      : expr(std::move(expr)), args(std::move(args)) {}

  /// The expression.
  BumpPtr<Expr> expr{};

  /// The arguments.
  ArgumentList args{};
};

/// An identifier expression.
class SMDL_EXPORT Identifier final : public ExprSubclass<ExprKind::Identifier> {
public:
  class Element final {
  public:
    /// The double colon `::` before the name. This may be empty on
    /// the first name!
    std::string_view srcDoubleColon{};

    /// The name.
    Name name{};
  };

  explicit Identifier(std::vector<Element> elems) : elements(std::move(elems)) {
    elementViews.resize(elements.size());
    for (size_t i = 0; i < elements.size(); i++)
      elementViews[i] = elements[i].name.srcName;
  }

  /// The number of names.
  [[nodiscard]] size_t size() const { return elements.size(); }

  /// Is absolute identifier? e.g., `::absolute::id`.
  [[nodiscard]] bool is_absolute() const {
    return !elements.empty() && !elements[0].srcDoubleColon.empty();
  }

  /// Is relative identifier? e.g., `relative::id`.
  [[nodiscard]] bool is_relative() const {
    return !elements.empty() && elements[0].srcDoubleColon.empty();
  }

  /// Is simple name?
  [[nodiscard]] bool is_simple_name() const {
    return elements.size() == 1 && is_relative();
  }

  /// Implicit conversion to span of string views.
  [[nodiscard]] operator Span<std::string_view>() const { return elementViews; }

  /// The elements.
  std::vector<Element> elements{};

  /// The element views extracted from the elements for convenience.
  std::vector<std::string_view> elementViews{};
};

/// An intrinsic expression.
class SMDL_EXPORT Intrinsic final : public ExprSubclass<ExprKind::Intrinsic> {
public:
  explicit Intrinsic(std::string_view srcName) : srcName(srcName) {}

  /// The intrinsic name (including the leading `#`).
  std::string_view srcName{};
};

/// A `let` expression.
///
/// ~~~~~~~~~~~~~~~~
/// let {
///   int varA = 2;
///   int varB = 3;
/// } in varA + varB
/// ~~~~~~~~~~~~~~~~
class SMDL_EXPORT Let final : public ExprSubclass<ExprKind::Let> {
public:
  explicit Let(std::string_view srcKwLet, std::string_view srcBraceL,
               std::vector<BumpPtr<Decl>> decls, std::string_view srcBraceR,
               std::string_view srcKwIn, BumpPtr<Expr> expr)
      : srcKwLet(srcKwLet), srcBraceL(srcBraceL), decls(std::move(decls)),
        srcBraceR(srcBraceR), srcKwIn(srcKwIn), expr(std::move(expr)) {}

  /// The keyword `let`.
  std::string_view srcKwLet{};

  /// The brace `{`. This may be empty!
  std::string_view srcBraceL{};

  /// The declarations.
  std::vector<BumpPtr<Decl>> decls{};

  /// The brace `}`. This may be empty!
  std::string_view srcBraceR{};

  /// The keyword `in`.
  std::string_view srcKwIn{};

  /// The expression.
  BumpPtr<Expr> expr{};
};

/// A literal bool expression.
class SMDL_EXPORT LiteralBool final
    : public ExprSubclass<ExprKind::LiteralBool> {
public:
  explicit LiteralBool(std::string_view srcValue, bool value)
      : srcValue(srcValue), value(value) {}

  /// The source of the literal value. (Either `true` or `false`)
  std::string_view srcValue{};

  /// The literal value.
  bool value{};
};

/// A literal int expression.
class SMDL_EXPORT LiteralInt final : public ExprSubclass<ExprKind::LiteralInt> {
public:
  explicit LiteralInt(std::string_view srcValue, uint64_t value)
      : srcValue(srcValue), value(value) {}

  /// The source of the literal value.
  std::string_view srcValue{};

  /// The literal value.
  uint64_t value{};
};

/// A literal float expression.
class SMDL_EXPORT LiteralFloat final
    : public ExprSubclass<ExprKind::LiteralFloat> {
public:
  explicit LiteralFloat(std::string_view srcValue, double value)
      : srcValue(srcValue), value(value) {}

  /// The source of the literal value.
  std::string_view srcValue{};

  /// The literal value.
  double value{};
};

/// A literal string expression.
class SMDL_EXPORT LiteralString final
    : public ExprSubclass<ExprKind::LiteralString> {
public:
  explicit LiteralString(std::string_view srcValue, std::string value)
      : srcValue(srcValue), value(std::move(value)) {}

  /// The source of the literal value.
  std::string_view srcValue{};

  /// The literal value.
  std::string value{};
};

/// An expression in parentheses.
class SMDL_EXPORT Parens final : public ExprSubclass<ExprKind::Parens> {
public:
  explicit Parens(std::string_view srcDollar, std::string_view srcParenL,
                  BumpPtr<Expr> expr, std::string_view srcParenR)
      : srcDollar(srcDollar), srcParenL(srcParenL), expr(std::move(expr)),
        srcParenR(srcParenR) {}

  /// The dollar sign `$`. This may be empty!
  std::string_view srcDollar{};

  /// The parenthesis `(`.
  std::string_view srcParenL{};

  /// The expression.
  BumpPtr<Expr> expr{};

  /// The parenthesis `)`.
  std::string_view srcParenR{};

  /// Is intended to force compile-time evaluation?
  [[nodiscard]] bool is_comptime() const { return !srcDollar.empty(); }
};

/// A `return_from` expression.
///
/// ~~~~~~~~~~~~~~~~~~~
/// return_from {
///   return something;
/// }
/// ~~~~~~~~~~~~~~~~~~~
class SMDL_EXPORT ReturnFrom final : public ExprSubclass<ExprKind::ReturnFrom> {
public:
  explicit ReturnFrom(std::string_view srcKwReturnFrom, BumpPtr<Stmt> stmt)
      : srcKwReturnFrom(srcKwReturnFrom), stmt(std::move(stmt)) {}

  /// The keyword `return_from`.
  std::string_view srcKwReturnFrom{};

  /// The statement. (This should always be a `Compound` statement)
  BumpPtr<Stmt> stmt;
};

/// A select expression.
///
/// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/// exprCond ? exprThen : exprElse
/// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
class SMDL_EXPORT Select final : public ExprSubclass<ExprKind::Select> {
public:
  explicit Select(BumpPtr<Expr> exprCond, std::string_view srcQuestion,
                  BumpPtr<Expr> exprThen, std::string_view srcColon,
                  BumpPtr<Expr> exprElse)
      : exprCond(std::move(exprCond)), srcQuestion(srcQuestion),
        exprThen(std::move(exprThen)), srcColon(srcColon),
        exprElse(std::move(exprElse)) {}

  /// The condition expression.
  BumpPtr<Expr> exprCond{};

  /// The question mark `?`.
  std::string_view srcQuestion{};

  /// The then expression.
  BumpPtr<Expr> exprThen{};

  /// The colon `:`.
  std::string_view srcColon{};

  /// The else expression.
  BumpPtr<Expr> exprElse{};
};

/// A size name expression (parsed as `<name>` between `[...]`).
class SMDL_EXPORT SizeName final : public ExprSubclass<ExprKind::SizeName> {
public:
  explicit SizeName(std::string_view srcAngleL, Name name,
                    std::string_view srcAngleR)
      : srcAngleL(srcAngleL), name(std::move(name)), srcAngleR(srcAngleR) {}

  /// The angle bracket `<`.
  std::string_view srcAngleL{};

  /// The name.
  Name name{};

  /// The angle bracket `>`.
  std::string_view srcAngleR{};
};

/// A type expression.
class SMDL_EXPORT Type final : public ExprSubclass<ExprKind::Type> {
public:
  explicit Type(std::vector<std::string_view> srcQuals, BumpPtr<Expr> expr)
      : srcQuals(std::move(srcQuals)), expr(std::move(expr)) {}

  [[nodiscard]] bool has_qualifier(std::string_view qual) const {
    return std::find(srcQuals.begin(), srcQuals.end(), qual) != srcQuals.end();
  }

  /// The source qualifiers.
  std::vector<std::string_view> srcQuals{};

  /// The expression.
  BumpPtr<Expr> expr{};

  /// The MDL type. This is resolved later in the compile.
  ::smdl::Type *type{};
};

/// A type `cast<...>(...)` expression.
class SMDL_EXPORT TypeCast final : public ExprSubclass<ExprKind::TypeCast> {
public:
  explicit TypeCast(std::string_view srcKwCast, std::string_view srcAngleL,
                    BumpPtr<Type> type, std::string_view srcAngleR,
                    BumpPtr<Expr> expr)
      : srcKwCast(srcKwCast), srcAngleL(srcAngleL), type(std::move(type)),
        srcAngleR(srcAngleR), expr(std::move(expr)) {}

  /// The keyword `cast`.
  std::string_view srcKwCast{};

  /// The angle bracket `<`.
  std::string_view srcAngleL{};

  /// The type.
  BumpPtr<Type> type{};

  /// The angle bracket `>`.
  std::string_view srcAngleR{};

  /// The expression. (This should always be a `Parens` expression)
  BumpPtr<Expr> expr{};
};

inline namespace unary_ops {

enum UnaryOp : uint32_t {
  UNOP_INC = 1,   ///< `++`
  UNOP_DEC,       ///< `--`
  UNOP_POS,       ///< `+`
  UNOP_NEG,       ///< `-`
  UNOP_NOT,       ///< `~`
  UNOP_LOGIC_NOT, ///< `!`
  UNOP_ADDR,      ///< `&`
  UNOP_DEREF,     ///< `*`
  UNOP_MAYBE,     ///< `?`
  UNOP_POSTFIX = 1UL << 31,
  UNOP_POSTFIX_INC = UNOP_POSTFIX | UNOP_INC,
  UNOP_POSTFIX_DEC = UNOP_POSTFIX | UNOP_DEC,
};

} // namespace unary_ops

[[nodiscard]] constexpr UnaryOp operator&(UnaryOp opA, UnaryOp opB) {
  return UnaryOp(uint32_t(opA) & uint32_t(opB));
}

[[nodiscard]] constexpr UnaryOp operator|(UnaryOp opA, UnaryOp opB) {
  return UnaryOp(uint32_t(opA) | uint32_t(opB));
}

[[nodiscard]] constexpr UnaryOp operator~(UnaryOp op) {
  return UnaryOp(~uint32_t(op));
}

[[nodiscard]] constexpr const char *to_string(UnaryOp op) {
  switch (op & ~UNOP_POSTFIX) {
  case UNOP_INC:
    return "++";
  case UNOP_DEC:
    return "--";
  case UNOP_POS:
    return "+";
  case UNOP_NEG:
    return "-";
  case UNOP_NOT:
    return "~";
  case UNOP_LOGIC_NOT:
    return "!";
  case UNOP_ADDR:
    return "&";
  case UNOP_DEREF:
    return "*";
  case UNOP_MAYBE:
    return "?";
  default:
    break;
  }
  return {};
}

/// A unary expression.
class SMDL_EXPORT Unary final : public ExprSubclass<ExprKind::Unary> {
public:
  explicit Unary(std::string_view srcOp, UnaryOp op, BumpPtr<Expr> expr)
      : srcOp(srcOp), op(op), expr(std::move(expr)) {}

  /// Is a postfix expression?
  [[nodiscard]] bool is_postfix() const {
    return (op & UNOP_POSTFIX) == UNOP_POSTFIX;
  }

  /// Get the operator enum without the postfix flag.
  [[nodiscard]] UnaryOp op_without_postfix() const {
    return (op & ~UNOP_POSTFIX);
  }

  /// The source of the operator.
  std::string_view srcOp{};

  /// The operator.
  UnaryOp op{};

  /// The expression.
  BumpPtr<Expr> expr{};
};

} // namespace smdl::AST
