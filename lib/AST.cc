#include "smdl/AST/Decl.h"
#include "smdl/AST/Stmt.h"

#include "llvm/Support/Casting.h"

namespace smdl::AST {

std::string_view to_string(NodeKind nodeKind) {
  switch (nodeKind) {
  case NodeKind::File:
    return "File";
  case NodeKind::Decl:
    return "Decl";
  case NodeKind::Expr:
    return "Expr";
  case NodeKind::Stmt:
    return "Stmt";
  case NodeKind::Parameter:
    return "Parameter";
  case NodeKind::Field:
    return "Field";
  case NodeKind::EnumDeclarator:
    return "EnumDeclarator";
  case NodeKind::VariableDeclarator:
    return "VariableDeclarator";
  default:
    break;
  }
  return {};
}

std::string_view to_string(DeclKind declKind) {
  switch (declKind) {
  case DeclKind::Enum:
    return "Enum";
  case DeclKind::Function:
    return "Function";
  case DeclKind::Import:
    return "Import";
  case DeclKind::Namespace:
    return "Namespace";
  case DeclKind::Struct:
    return "Struct";
  case DeclKind::Tag:
    return "Tag";
  case DeclKind::Typedef:
    return "Typedef";
  case DeclKind::UnitTest:
    return "UnitTest";
  case DeclKind::UsingAlias:
    return "UsingAlias";
  case DeclKind::UsingImport:
    return "UsingImport";
  case DeclKind::Variable:
    return "Variable";
  default:
    break;
  }
  return {};
}

std::string_view to_string(ExprKind exprKind) {
  switch (exprKind) {
  case ExprKind::AccessField:
    return "AccessField";
  case ExprKind::AccessIndex:
    return "AccessIndex";
  case ExprKind::Binary:
    return "Binary";
  case ExprKind::Call:
    return "Call";
  case ExprKind::Identifier:
    return "Identifier";
  case ExprKind::Intrinsic:
    return "Intrinsic";
  case ExprKind::Let:
    return "Let";
  case ExprKind::LiteralBool:
    return "LiteralBool";
  case ExprKind::LiteralFloat:
    return "LiteralFloat";
  case ExprKind::LiteralInt:
    return "LiteralInt";
  case ExprKind::LiteralString:
    return "LiteralString";
  case ExprKind::Parens:
    return "Parens";
  case ExprKind::ReturnFrom:
    return "ReturnFrom";
  case ExprKind::Select:
    return "Select";
  case ExprKind::SizeName:
    return "SizeName";
  case ExprKind::Type:
    return "Type";
  case ExprKind::TypeCast:
    return "TypeCast";
  case ExprKind::Unary:
    return "Unary";
  default:
    break;
  }
  return {};
}

std::string_view to_string(StmtKind stmtKind) {
  switch (stmtKind) {
  case StmtKind::Break:
    return "Break";
  case StmtKind::Compound:
    return "Compound";
  case StmtKind::Continue:
    return "Continue";
  case StmtKind::DeclStmt:
    return "DeclStmt";
  case StmtKind::Defer:
    return "Defer";
  case StmtKind::DoWhile:
    return "DoWhile";
  case StmtKind::ExprStmt:
    return "ExprStmt";
  case StmtKind::For:
    return "For";
  case StmtKind::If:
    return "If";
  case StmtKind::Preserve:
    return "Preserve";
  case StmtKind::Return:
    return "Return";
  case StmtKind::Switch:
    return "Switch";
  case StmtKind::Unreachable:
    return "Unreachable";
  case StmtKind::Visit:
    return "Visit";
  case StmtKind::While:
    return "While";
  default:
    break;
  }
  return {};
}

Function::LetAndCall Function::get_variant_let_and_call_expressions() const {
  if (!(is_variant() && definition && llvm::isa<Return>(definition.get())))
    srcLoc.throwError(concat("function variant ", Quoted(name.srcName),
                             " has invalid declaration"));
  auto letAndCall{LetAndCall{}};
  auto expr{static_cast<Return *>(definition.get())->expr.get()};
  if (llvm::isa<Call>(expr)) {
    letAndCall.call = static_cast<Call *>(expr);
  } else if (llvm::isa<Let>(expr)) {
    letAndCall.let = static_cast<Let *>(expr);
    letAndCall.call = llvm::dyn_cast<Call>(letAndCall.let->expr.get());
    if (!letAndCall.call) {
      srcLoc.throwError(
          concat("function variant ", Quoted(name.srcName),
                 " definition with 'let' must be followed by call expression"));
    }
  }
  if (!letAndCall.call) {
    srcLoc.throwError(concat("function variant ", Quoted(name.srcName),
                             " definition must be 'let' or call expression"));
  }
  for (auto &arg : letAndCall.call->args) {
    if (!arg.is_named()) {
      srcLoc.throwError(concat("call in definition of function variant ",
                               Quoted(name.srcName),
                               " must only use named arguments"));
    }
  }
  return letAndCall;
}

} // namespace smdl::AST
