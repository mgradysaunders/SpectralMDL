// vim:foldmethod=marker:foldlevel=0:fmr=--{,--}
#include "Formatter.h"

namespace smdl {

void Formatter::write(char ch) {
  if (!str.empty() && str.back() == '\n' && ch != '\n') {
    for (int i = 0; i < indentLevel; i++)
      str += "  ";
  }
  str += ch;
}

void Formatter::write(llvm::StringRef str) {
  for (auto ch : str)
    write(ch);
}

void Formatter::write_literal_string(llvm::StringRef str) {
  for (auto ch : str) {
    // TODO Handle all cases
    switch (ch) {
    case '\n': write("\\n"); break;
    case '\t': write("\\t"); break;
    case '\r': write("\\r"); break;
    case '\v': write("\\v"); break;
    default: write(ch); break;
    }
  }
}

void Formatter::write_as_compound(AST::Node &node) {
  if (!llvm::isa<AST::Compound>(&node)) {
    indentLevel++;
    write("{\n", node);
    indentLevel--;
    write("\n}");
  } else {
    write(node);
  }
}

void Formatter::guarantee_newlines(size_t n) {
  if (!str.empty()) {
    size_t num = 0;
    for (auto itr = str.rbegin(); itr != str.rend(); ++itr) {
      if (*itr != '\n')
        break;
      num++;
    }
    for (size_t i = num; i < n; i++)
      write('\n');
  }
}

void Formatter::write(AST::Node &node) { write_type_switch<AST::Decl, AST::Expr, AST::File, AST::Stmt>(node); }

void Formatter::write(AST::File &file) {
  for (auto &decl : file.imports)
    write(decl);
  for (auto &decl : file.globals)
    write(decl);
}

//--{ Dump: Decl
void Formatter::write(AST::Decl &decl) {
  write_type_switch<
      AST::Enum, AST::Function, AST::Import, AST::Struct, AST::Tag, AST::Typedef, AST::UnitTest, AST::UsingAlias,
      AST::UsingImport, AST::Variable>(decl);
}

void Formatter::write(AST::Enum &decl) {
  if (decl.isGlobal)
    guarantee_newlines(2);
  if (decl.isExport)
    write("export ");
  write("enum ", decl.name, " {");
  ++indentLevel;
  for (size_t i = 0; i < decl.declarators.size(); i++) {
    guarantee_newlines(1);
    write(decl.declarators[i]);
    if (i + 1 < decl.declarators.size())
      write(',');
  }
  --indentLevel;
  write("\n};");
}

void Formatter::write(AST::Function &decl) {
  if (decl.isGlobal)
    guarantee_newlines(2);
  if (decl.isExport)
    write("export ");
  if (write(decl.attrs))
    write(' ');
  write(decl.returnType);
  if (decl.earlyAnnotations)
    write(' ', *decl.earlyAnnotations);
  write(' ', decl.name);
  if (decl.isVariant) {
    write("(*)");
  } else {
    write('(', decl.params, ')');
  }
  if (decl.frequency)
    write(' ', *decl.frequency);
  if (decl.lateAnnotations)
    write(' ', *decl.lateAnnotations);
  if (decl.definition == nullptr) {
    write(';');
  } else {
    if (auto stmt{llvm::dyn_cast<AST::Return>(decl.definition.get())}) {
      write(" = ", stmt->expr, ';');
    } else {
      write(' ', decl.definition);
    }
  }
}

void Formatter::write(AST::Import &decl) {
  guarantee_newlines(1);
  write("import ");
  write_separated(decl.paths, ", ");
  write(';');
}

void Formatter::write(AST::Struct &decl) {
  if (decl.isGlobal)
    guarantee_newlines(2);
  if (decl.isExport)
    write("export ");
  write("struct ", decl.name);
  if (!decl.tags.empty()) {
    write(": ");
    write_separated(decl.tags, ", ");
  }
  if (decl.fields.empty()) {
    write(" { /* Empty! */ };");
  } else {
    write(" {");
    ++indentLevel;
    for (auto &field : decl.fields) {
      guarantee_newlines(1);
      write(field);
    }
    --indentLevel;
    write("\n};");
  }
}

void Formatter::write(AST::Tag &decl) {
  if (decl.isGlobal)
    guarantee_newlines(2);
  if (decl.isExport)
    write("export ");
  write("tag ", decl.name, ';');
}

void Formatter::write(AST::Typedef &decl) {
  if (decl.isGlobal)
    guarantee_newlines(2);
  if (decl.isExport)
    write("export ");
  write("typedef ", decl.type, ' ', decl.name, ';');
}

void Formatter::write(AST::UnitTest &decl) {
  guarantee_newlines(2);
  write("unit_test \"");
  write_literal_string(decl.name);
  write("\" ");
  write(decl.body);
}

void Formatter::write(AST::UsingAlias &decl) {
  guarantee_newlines(1);
  write("using ", decl.name, " = ");
  write(';');
}

void Formatter::write(AST::UsingImport &decl) {
  guarantee_newlines(1);
  if (decl.isExport)
    write("export ");
  write("using ", decl.path, " import ");
  if (decl.is_import_all())
    write('*');
  else
    write_separated(decl.names, ", ");
  write(';');
}

void Formatter::write(AST::Variable &decl) {
  if (decl.isGlobal)
    guarantee_newlines(2);
  if (decl.isExport)
    write("export ");
  write(decl.type, ' ');
  write_separated(decl.declarators, ", ");
  write(';');
}
//--}

//--{ Dump: Exprs
void Formatter::write(AST::Expr &expr) {
  write_type_switch<
      AST::Binary, AST::Call, AST::Cast, AST::Conditional, AST::GetField, AST::GetIndex, AST::Identifier, AST::Intrinsic,
      AST::Name, AST::Let, AST::LiteralBool, AST::LiteralFloat, AST::LiteralInt, AST::LiteralString, AST::Parens,
      AST::ReturnFrom, AST::SizeName, AST::Type, AST::Unary>(expr);
}

void Formatter::write(AST::Binary &expr) {
  if (expr.op == AST::BinaryOp::Comma)
    write(expr.lhs, ", ", expr.rhs);
  else
    write(expr.lhs, ' ', AST::to_string(expr.op), ' ', expr.rhs);
}

void Formatter::write(AST::Call &expr) {
  write(expr.expr, '(');
  auto isAlwaysIndented{[&]() {
    if (expr.args.args.empty())
      return false;
    if (auto ident{llvm::dyn_cast<AST::Identifier>(expr.expr.get())}) {
      if (ident->is_simple_name()) {
        auto &name{ident->names[0]->name};
        if (name == "material" || name == "material_surface" || name == "material_geometry" || name == "material_emission" ||
            name == "material_volume")
          return true;
      }
    }
    return false;
  }()};
  if (isAlwaysIndented) {
    ++indentLevel;
    write('\n');
    write_separated(expr.args.args, ",\n");
    --indentLevel;
    write("\n)");
  } else {
    write(expr.args, ')');
  }
}

void Formatter::write(AST::Cast &expr) { write("cast<", expr.type, ">(", expr.expr, ')'); }

void Formatter::write(AST::Conditional &expr) { write(expr.cond, " ? ", expr.ifPass, " : ", expr.ifFail); }

void Formatter::write(AST::GetField &expr) { write(expr.what, '.', expr.name); }

void Formatter::write(AST::GetIndex &expr) {
  write(expr.expr);
  for (auto &index : expr.indices)
    if (index)
      write('[', index, ']');
    else
      write("[]");
}

void Formatter::write(AST::Identifier &expr) {
  if (expr.isAbs)
    write("::");
  write(expr.names[0]);
  for (size_t i = 1; i < expr.names.size(); i++) {
    write("::");
    write(expr.names[i]);
  }
}

void Formatter::write(AST::Intrinsic &expr) { write('#', expr.name); }

void Formatter::write(AST::Name &expr) { write(expr.name); }

void Formatter::write(AST::Let &expr) {
  if (expr.vars.empty()) {
    write(expr.expr);
  } else {
    write("let {");
    ++indentLevel;
    for (auto &var : expr.vars) {
      guarantee_newlines(1);
      write(var);
    }
    --indentLevel;
    write("\n} in ", expr.expr);
  }
}

void Formatter::write(AST::LiteralBool &expr) { write(expr.value ? "true" : "false"); }

void Formatter::write(AST::LiteralFloat &expr) { write(expr.src); }

void Formatter::write(AST::LiteralInt &expr) { write(expr.src); }

void Formatter::write(AST::LiteralString &expr) { write(expr.src); }

void Formatter::write(AST::Parens &expr) {
  if (expr.isCompileTime) {
    write('$');
    write('(', expr.expr, ')');
  } else {
    if (expr.expr->exprKind == AST::ExprKind::Parens)
      write(expr.expr); // Remove redundant
    else
      write('(', expr.expr, ')');
  }
}

void Formatter::write(AST::ReturnFrom &expr) {
  write("return_from ");
  write_as_compound(*expr.stmt);
}

void Formatter::write(AST::SizeName &expr) {
  if (expr.name)
    write('<', expr.name, '>');
}

void Formatter::write(AST::Type &expr) {
  if (expr.frequency)
    write(*expr.frequency, ' ');
  if (expr.attrs.isConst)
    write("const ");
  if (expr.attrs.isStatic)
    write("static ");
  if (expr.attrs.isInline)
    write("inline ");
  write(expr.expr);
  if (expr.annotations)
    write(' ', *expr.annotations);
}

void Formatter::write(AST::Unary &expr) { write(AST::to_string(expr.op), expr.expr); }
//--}

//--{ Dump: Stmts
void Formatter::write(AST::Stmt &stmt) {
  write_type_switch<
      AST::Break, AST::Compound, AST::Continue, AST::DeclStmt, AST::Defer, AST::DoWhile, AST::ExprStmt, AST::For, AST::If,
      AST::Preserve, AST::Return, AST::Switch, AST::Unreachable, AST::Visit, AST::While>(stmt);
}

void Formatter::write(AST::Break &stmt) {
  write("break");
  if (stmt.cond != nullptr)
    write(" if ", stmt.cond);
  write(';');
}

void Formatter::write(AST::Compound &stmt) {
  write('{');
  ++indentLevel;
  for (auto &subStmt : stmt.stmts)
    write('\n', subStmt);
  --indentLevel;
  write("\n}");
}

void Formatter::write(AST::Continue &stmt) {
  write("continue");
  if (stmt.cond != nullptr)
    write(" if ", stmt.cond);
  write(';');
}

void Formatter::write(AST::DeclStmt &stmt) { write(*stmt.decl); }

void Formatter::write(AST::Defer &stmt) {
  write("defer ");
  write_as_compound(*stmt.stmt);
}

void Formatter::write(AST::DoWhile &stmt) {
  write("do ");
  write_as_compound(*stmt.body);
  write(" while ", stmt.cond, ';');
}

void Formatter::write(AST::ExprStmt &stmt) {
  if (stmt.expr) {
    write(*stmt.expr);
    if (stmt.cond != nullptr)
      write(" if ", stmt.cond);
    write(';');
  }
}

void Formatter::write(AST::For &stmt) {
  write("for (", stmt.init, ' ', stmt.cond, "; ", stmt.incr, ") ");
  write_as_compound(*stmt.body);
}

void Formatter::write(AST::If &stmt) {
  write("if ", stmt.cond, ' ');
  write_as_compound(*stmt.ifPass);
  if (stmt.ifFail) {
    write(" else ");
    if (llvm::isa<AST::If>(stmt.ifFail.get()))
      write(stmt.ifFail);
    else
      write_as_compound(*stmt.ifFail);
  }
}

void Formatter::write(AST::Preserve &stmt) {
  write("preserve ");
  for (size_t i = 0; i < stmt.exprs.size(); i++) {
    write(stmt.exprs[i]);
    if (i + 1 < stmt.exprs.size())
      write(", ");
  }
  write(';');
}

void Formatter::write(AST::Return &stmt) {
  write("return");
  if (stmt.expr != nullptr)
    write(' ', stmt.expr);
  if (stmt.cond != nullptr)
    write(" if ", stmt.cond);
  write(';');
}

void Formatter::write(AST::Switch &stmt) {
  write("switch ", stmt.what, " {");
  for (size_t i = 0; i < stmt.cases.size(); i++) {
    guarantee_newlines(1);
    if (stmt.cases[i].is_default()) {
      write("default:");
    } else {
      write("case ", stmt.cases[i].cond, ':');
    }
    ++indentLevel;
    for (auto &subStmt : stmt.cases[i].stmts) {
      guarantee_newlines(1);
      write(subStmt);
    }
    --indentLevel;
  }
  write("\n}");
}

void Formatter::write(AST::Unreachable &) { write("unreachable"); }

void Formatter::write(AST::Visit &stmt) {
  write("visit ", stmt.name);
  if (stmt.what != nullptr)
    write(" in ", stmt.what);
  write(' ', stmt.body);
}

void Formatter::write(AST::While &stmt) {
  write("while ", stmt.cond, ' ');
  write_as_compound(*stmt.body);
}
//--}

void Formatter::write(AST::FrequencyQualifier freq) { write(freq == AST::FrequencyQualifier::Uniform ? "uniform" : "varying"); }

void Formatter::write(const AST::Annotation &annotation) { write(annotation.identifier, '(', annotation.args, ')'); }

void Formatter::write(const AST::AnnotationBlock &annotations) {
  write("[[");
  write_separated(annotations, ", ");
  write("]]");
}

void Formatter::write(const AST::Arg &arg) {
  if (arg.isVisited)
    write("visit ");
  if (arg.name != nullptr)
    write(arg.name, ": ");
  write(arg.expr);
}

void Formatter::write(const AST::ArgList &args) {
  if (args.args.size() > 1 && args.src.size() > 80) {
    ++indentLevel;
    write('\n');
    write_separated(args.args, ",\n");
    --indentLevel;
    write('\n');
  } else {
    write_separated(args.args, ", ");
  }
}

void Formatter::write(const AST::Param &param) {
  write(param.type, ' ', param.name);
  if (param.init)
    write(" = ", param.init);
  if (param.annotations)
    write(' ', *param.annotations);
}

void Formatter::write(const AST::ParamList &params) {
  if (params.params.size() > 1 && params.src.size() > 80) {
    indentLevel += 2;
    write('\n');
    write_separated(params.params, ",\n");
    indentLevel -= 2;
  } else {
    write_separated(params.params, ", ");
  }
}

void Formatter::write(const AST::Enum::Declarator &declarator) {
  write(declarator.name);
  if (declarator.init)
    write(" = ", declarator.init);
  if (declarator.annotations)
    write(' ', declarator.annotations);
}

bool Formatter::write(const AST::Function::Attrs &attrs) {
  llvm::SmallVector<llvm::StringRef> names{};
  if (attrs.isAlwaysInline)
    names.push_back("alwaysinline");
  if (attrs.isCold)
    names.push_back("cold");
  if (attrs.isForeign)
    names.push_back("foreign");
  if (attrs.isHot)
    names.push_back("hot");
  if (attrs.isMacro)
    names.push_back("macro");
  if (attrs.isNoInline)
    names.push_back("noinline");
  if (attrs.isOptNone)
    names.push_back("optnone");
  if (attrs.isOptSize)
    names.push_back("optsize");
  if (attrs.isPure)
    names.push_back("pure");
  if (attrs.isVisible)
    names.push_back("visible");
  if (!names.empty()) {
    write("@(");
    write_separated(names, " ");
    write(')');
    return true;
  }
  return false;
}

void Formatter::write(const AST::Struct::Field &field) {
  if (field.isVoid)
    write("void ");
  write(field.type, ' ', field.name);
  if (field.init)
    write(" = ", field.init);
  if (field.annotations)
    write(' ', *field.annotations);
  write(';');
}

void Formatter::write(const AST::Struct::Tag &tag) {
  if (tag.isDefault)
    write("default ");
  write(tag.type);
}

void Formatter::write(const AST::Variable::Declarator &declarator) {
  write(declarator.name);
  if (declarator.init != nullptr) {
    write(" = ", declarator.init);
  } else if (declarator.args) {
    write('(', declarator.args, ')');
  }
  if (declarator.annotations) {
    write(' ', *declarator.annotations);
  }
}

} // namespace smdl
