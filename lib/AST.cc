#include "AST.h"

namespace smdl::AST {

Identifier::operator std::string() const {
  std::string str{};
  if (isAbs)
    str += "::";
  for (size_t i{}; i < names.size(); i++) {
    str += names[i].name.srcName;
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
      srcLoc.report_error(std::format("call in definition of function variant '{}' must only use named arguments", name.srcName));
  return letAndCall;
}

} // namespace smdl::AST
