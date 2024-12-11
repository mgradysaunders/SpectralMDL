#include "AST.h"

namespace smdl::AST {

void SourceLocation::report_warning(std::string message) const {
  llvm::WithColor(llvm::errs(), llvm::HighlightColor::Warning) << "[warning] ";
  if (!file.empty() && line > 0)
    llvm::WithColor(llvm::errs(), llvm::HighlightColor::Address) << '[' << file << ':' << line << "] ";
  llvm::errs() << message << '\n';
}

Identifier::IdentifierName::operator llvm::StringRef() const {
  if (name) {
    return name;
  } else {
    sanity_check(literalString);
    sanity_check(literalString->exprKind == AST::ExprKind::LiteralString);
    return static_cast<AST::LiteralString *>(literalString.get())->value.str();
  }
}

vector_or_SmallVector<llvm::StringRef> Identifier::get_string_refs() const {
  auto refs{vector_or_SmallVector<llvm::StringRef>{}};
  for (auto &name : names)
    refs.push_back(llvm::StringRef(name));
  return refs;
}

Identifier::operator std::string() const {
  std::string str{};
  for (auto &[srcDoubleColon, name, literalString] : names) {
    for (auto ch : srcDoubleColon)
      str += ch;
    if (name) {
      for (auto ch : name.srcName)
        str += ch;
    } else {
      for (auto ch : static_cast<AST::LiteralString *>(literalString.get())->srcValue)
        str += ch;
    }
  }
  return str;
}

const char *to_string(UnaryOp op) {
  switch (op & ~UnaryOp::Postfix) {
  case UnaryOp::Incr: return "++";
  case UnaryOp::Decr: return "--";
  case UnaryOp::Pos: return "+";
  case UnaryOp::Neg: return "-";
  case UnaryOp::Not: return "~";
  case UnaryOp::LogicalNot: return "!";
  case UnaryOp::Address: return "&";
  case UnaryOp::Deref: return "*";
  case UnaryOp::Maybe: return "?";
  default: break;
  }
  return {};
}

const char *to_string(BinaryOp op) {
  switch (op) {
  case BinaryOp::Add: return "+";
  case BinaryOp::Sub: return "-";
  case BinaryOp::Mul: return "*";
  case BinaryOp::Div: return "/";
  case BinaryOp::Rem: return "%";
  case BinaryOp::And: return "&";
  case BinaryOp::Or: return "|";
  case BinaryOp::Xor: return "^";
  case BinaryOp::Shl: return "<<";
  case BinaryOp::AShr: return ">>";
  case BinaryOp::LShr: return ">>>";
  case BinaryOp::Eq: return "=";
  case BinaryOp::EqAdd: return "+=";
  case BinaryOp::EqSub: return "-=";
  case BinaryOp::EqMul: return "*=";
  case BinaryOp::EqDiv: return "/=";
  case BinaryOp::EqRem: return "%=";
  case BinaryOp::EqAnd: return "&=";
  case BinaryOp::EqOr: return "|=";
  case BinaryOp::EqXor: return "^=";
  case BinaryOp::EqShl: return "<<=";
  case BinaryOp::EqAShr: return ">>=";
  case BinaryOp::EqLShr: return ">>>=";
  case BinaryOp::CmpEq: return "==";
  case BinaryOp::CmpNe: return "!=";
  case BinaryOp::CmpLt: return "<";
  case BinaryOp::CmpLe: return "<=";
  case BinaryOp::CmpGt: return ">";
  case BinaryOp::CmpGe: return ">=";
  case BinaryOp::LogicalAnd: return "&&";
  case BinaryOp::LogicalOr: return "||";
  case BinaryOp::Comma: return ",";
  case BinaryOp::Def: return ":=";
  case BinaryOp::Subset: return "<:";
  default: sanity_check(false); break;
  }
  return {};
}

Let::Let(
    SourceRef srcKwLet, SourceRef srcBraceL, vector_or_SmallVector<unique_bump_ptr<Variable>> vars, SourceRef srcBraceR,
    SourceRef srcKwIn, unique_bump_ptr<Expr> expr)
    : srcKwLet(srcKwLet), srcBraceL(srcBraceL), vars(std::move(vars)), srcBraceR(srcBraceR), srcKwIn(srcKwIn),
      expr(std::move(expr)) {}

Let::~Let() {}

ReturnFrom::ReturnFrom(llvm::StringRef srcKwReturnFrom, unique_bump_ptr<Stmt> stmt)
    : srcKwReturnFrom(srcKwReturnFrom), stmt(std::move(stmt)) {}

ReturnFrom::~ReturnFrom() {}

Function::LetAndCall Function::get_variant_let_and_call_expressions() const {
  if (!(isVariant && params.empty() && definition && llvm::isa<Return>(definition.get())))
    srcLoc.report_error(std::format("function variant '{}' has invalid declaration", name.srcName));
  auto letAndCall{LetAndCall{}};
  auto expr{static_cast<Return *>(definition.get())->expr.get()};
  if (llvm::isa<Call>(expr)) {
    letAndCall.call = static_cast<Call *>(expr);
  } else if (llvm::isa<Let>(expr)) {
    letAndCall.let = static_cast<Let *>(expr);
    letAndCall.call = llvm::dyn_cast<Call>(letAndCall.let->expr.get());
    if (!letAndCall.call)
      srcLoc.report_error(
          std::format("function variant '{}' definition with 'let' must be followed by call expression", name.srcName));
  }
  if (!letAndCall.call)
    srcLoc.report_error(std::format("function variant '{}' definition must be 'let' or call expression", name.srcName));
  for (auto &arg : letAndCall.call->args.args)
    if (!arg.name)
      srcLoc.report_error(
          std::format("call in definition of function variant '{}' must only use named arguments", name.srcName));
  return letAndCall;
}

} // namespace smdl::AST
