#include "smdl/AST/Decl.h"
#include "smdl/AST/Stmt.h"

#include "llvm/Support/Casting.h"

namespace smdl::AST {

Function::LetAndCall Function::get_variant_let_and_call_expressions() const {
  if (!(is_variant() && definition && llvm::isa<Return>(definition.get())))
    srcLoc.throw_error(concat("function variant ", quoted(name.srcName),
                              " has invalid declaration"));
  auto letAndCall{LetAndCall{}};
  auto expr{static_cast<Return *>(definition.get())->expr.get()};
  if (llvm::isa<Call>(expr)) {
    letAndCall.call = static_cast<Call *>(expr);
  } else if (llvm::isa<Let>(expr)) {
    letAndCall.let = static_cast<Let *>(expr);
    letAndCall.call = llvm::dyn_cast<Call>(letAndCall.let->expr.get());
    if (!letAndCall.call) {
      srcLoc.throw_error(
          concat("function variant ", quoted(name.srcName),
                 " definition with 'let' must be followed by call expression"));
    }
  }
  if (!letAndCall.call) {
    srcLoc.throw_error(concat("function variant ", quoted(name.srcName),
                              " definition must be 'let' or call expression"));
  }
  for (auto &arg : letAndCall.call->args) {
    if (!arg.is_named()) {
      srcLoc.throw_error(concat("call in definition of function variant ",
                                quoted(name.srcName),
                                " must only use named arguments"));
    }
  }
  return letAndCall;
}

} // namespace smdl::AST
