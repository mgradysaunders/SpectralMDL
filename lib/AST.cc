#include "AST.h"

namespace smdl::AST {

Identifier::operator std::string() const {
  std::string str{};
  if (isAbsolute)
    str += "::";
  for (size_t i{}; i < names.size(); i++) {
    str += names[i]->name;
    if (i + 1 < names.size())
      str += "::";
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

Let::Let(llvm::SmallVector<unique_bump_ptr<Variable>> vars, unique_bump_ptr<Expr> expr)
    : vars(std::move(vars)), expr(std::move(expr)) {}

Let::~Let() {}

ReturnFrom::ReturnFrom(unique_bump_ptr<Stmt> stmt) : stmt(std::move(stmt)) {}

ReturnFrom::~ReturnFrom() {}

} // namespace smdl::AST
