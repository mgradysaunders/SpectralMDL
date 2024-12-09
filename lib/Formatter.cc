#include "Formatter.h"

namespace smdl {

[[nodiscard]] static size_t count_newlines(llvm::StringRef str) {
  size_t num{};
  for (char c : str)
    num += (c == '\n') ? 1 : 0;
  return num;
}

[[nodiscard]] static size_t length_with_reduced_whitespace(llvm::StringRef str) {
  size_t num{};
  for (auto itr = str.begin(); itr < str.end();) {
    ++num;
    ++itr;
    while (itr < str.end() && std::isspace(static_cast<unsigned char>(*itr)))
      ++itr;
  }
  return num;
}

void Formatter::write(char ch) {
  if (!str.empty() && str.back() == '\n' && ch != '\n') {
    for (int i = 0; i < indent; i++)
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
    write('{');
    increment_indent(1, [&]() { write('\n', node); });
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
void Formatter::try_to_preserve_comments(llvm::StringRef src0, llvm::StringRef src1) {
  if (!src0.empty() && !src1.empty()) {
    auto ptr0 = src0.data() + src0.size();
    auto ptr1 = src1.data();
    if (ptr0 < ptr1) {
      try_to_preserve_comments(llvm::StringRef(ptr0, ptr1 - ptr0));
    }
  }
}

void Formatter::try_to_preserve_comments(llvm::StringRef s) {
  int iter = 0;
  while (!s.empty() && iter++ < 1000) {
    size_t numNewLines = count_newlines(s.take_while([](char c) { return std::isspace(static_cast<unsigned char>(c)); }));
    s = s.ltrim();
    if (s.empty())
      break;
    if (s.starts_with("//")) {
      s = s.drop_front(2);
      write("//");
      while (!s.empty()) {
        if (s.front() == '\n') {
          s = s.drop_front();
          write('\n');
          break;
        }
        str += s.front();
        s = s.drop_front(1);
      }
    } else if (s.starts_with("/*")) {
      s = s.drop_front(2);
      write("/*");
      while (!s.empty()) {
        if (s.starts_with("*/")) {
          s = s.drop_front(2);
          str += "*/ ";
          break;
        }
        str += s.front();
        s = s.drop_front(1);
      }
    }
  }
}

void Formatter::write(AST::Node &node) { write_type_switch<AST::Decl, AST::Expr, AST::File, AST::Stmt>(node); }

void Formatter::write(AST::File &file) {
  if (file.isSmdlSyntax)
    write("#smdl_syntax\n\n");
  else
    write(std::format("mdl {}.{};\n\n", int(file.version.major), int(file.version.minor)));

  // TODO sort imports
  for (auto &decl : file.imports)
    write(decl);
  // TODO annotations
  for (size_t i = 0; i < file.globals.size(); i++) {
    guarantee_newlines(2);
    if (i > 0)
      try_to_preserve_comments(file.globals[i - 1]->src, file.globals[i]->src);
    if (file.globals[i]->isExport)
      write("export ");
    write(file.globals[i]);
  }
}

//--{ Write: Decl
void Formatter::write(AST::Decl &decl) {
  write_type_switch<
      AST::Enum, AST::Function, AST::Import, AST::Struct, AST::Tag, AST::Typedef, AST::UnitTest, AST::UsingAlias,
      AST::UsingImport, AST::Variable>(decl);
}

void Formatter::write(AST::Enum &decl) {
  write("enum ", decl.name, " {");
  increment_indent(1, [&]() {
    for (size_t i = 0; i < decl.declarators.size(); i++) {
      guarantee_newlines(1);
      write(decl.declarators[i]);
      if (i + 1 < decl.declarators.size())
        write(',');
    }
  });
  write("\n};");
}

void Formatter::write(AST::Function &decl) {
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
  write("struct ", decl.name);
  if (!decl.tags.empty()) {
    write(": ");
    write_separated(decl.tags, ", ");
  }
  if (decl.fields.empty()) {
    write(" {};");
  } else {
    write(" {");
    increment_indent(1, [&]() {
      for (size_t i = 0; i < decl.fields.size(); i++) {
        // TODO preserve comments
        guarantee_newlines(1);
        write(decl.fields[i]);
      }
    });
    write("\n};");
  }
}

void Formatter::write(AST::Tag &decl) { write("tag ", decl.name, ';'); }

void Formatter::write(AST::Typedef &decl) { write("typedef ", decl.type, ' ', decl.name, ';'); }

void Formatter::write(AST::UnitTest &decl) {
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
  write(decl.type, ' ');
  write_separated(decl.declarators, ", ");
  write(';');
}
//--}

//--{ Write: Exprs
void Formatter::write(AST::Expr &expr) {
  write_type_switch<
      AST::Binary, AST::Call, AST::Cast, AST::Conditional, AST::GetField, AST::GetIndex, AST::Identifier, AST::Intrinsic,
      AST::Name, AST::Let, AST::LiteralBool, AST::LiteralFloat, AST::LiteralInt, AST::LiteralString, AST::Parens,
      AST::ReturnFrom, AST::SizeName, AST::Type, AST::Unary>(expr);
}

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

void Formatter::write(AST::Let &expr) {
  if (expr.vars.empty()) {
    write(expr.expr);
  } else {
    write("let {");
    increment_indent(1, [&]() {
      for (auto &var : expr.vars) {
        guarantee_newlines(1);
        write(var);
      }
    });
    write("\n} in ", expr.expr);
  }
}

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
//--}

//--{ Write: Stmts
void Formatter::write(AST::Stmt &stmt) {
  write_type_switch<
      AST::Break, AST::Compound, AST::Continue, AST::DeclStmt, AST::Defer, AST::DoWhile, AST::ExprStmt, AST::For, AST::If,
      AST::Preserve, AST::Return, AST::Switch, AST::Unreachable, AST::Visit, AST::While>(stmt);
}

void Formatter::write(AST::Compound &stmt) {
  write('{');
  increment_indent(1, [&]() {
    for (size_t i = 0; i < stmt.stmts.size(); i++) {
      write('\n');
      if (i == 0) {
        auto ptr0{stmt.src.data() + 1};
        auto ptr1{stmt.stmts[0]->src.data()};
        if (ptr0 < ptr1)
          try_to_preserve_comments(llvm::StringRef(ptr0, ptr1 - ptr0));
      }
      if (i > 0)
        try_to_preserve_comments(stmt.stmts[i - 1]->src, stmt.stmts[i]->src);
      write(stmt.stmts[i]);
    }
    // for (auto &subStmt : stmt.stmts)
    //   write('\n', subStmt);
  });
  write("\n}");
}

void Formatter::write(AST::Defer &stmt) {
  write(stmt.srcKwDefer, ' ', stmt.stmt);
}

void Formatter::write(AST::DoWhile &stmt) {
  write(stmt.srcKwDo, ' ', stmt.body, ' ', stmt.srcKwWhile, ' ', stmt.cond, stmt.srcSemicolon);
}

void Formatter::write(AST::ExprStmt &stmt) {
  if (stmt.expr) {
    write(stmt.expr, stmt.lateIf, stmt.srcSemicolon);
  }
}

void Formatter::write(AST::For &stmt) {
  write("for (", stmt.init, ' ', stmt.cond, "; ", stmt.incr, ") ");
  write_as_compound(*stmt.body);
}

void Formatter::write(AST::If &stmt) {
  write(stmt.srcKwIf, ' ', stmt.cond, ' ');
  write_as_compound(*stmt.ifPass);
  if (stmt.ifFail) {
    write(' ', stmt.srcKwElse, ' ');
    if (llvm::isa<AST::If>(stmt.ifFail.get()))
      write(stmt.ifFail);
    else
      write_as_compound(*stmt.ifFail);
  }
}

void Formatter::write(AST::Preserve &stmt) {
  write(stmt.srcKwPreserve, ' ');
  for (size_t i = 0; i < stmt.exprs.size(); i++) {
    write(stmt.exprs[i]);
    if (i + 1 < stmt.exprs.size())
      write(", ");
  }
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
    increment_indent(1, [&]() {
      for (auto &subStmt : stmt.cases[i].stmts) {
        guarantee_newlines(1);
        write(subStmt);
      }
    });
  }
  write("\n}");
}

void Formatter::write(AST::While &stmt) {
  write(stmt.srcKwWhile, ' ', stmt.cond, ' ');
  write_as_compound(*stmt.body);
}
//--}

void Formatter::write(const AST::Annotation &annotation) { write(annotation.identifier, '(', annotation.args, ')'); }

void Formatter::write(const AST::AnnotationBlock &annotations) {
  write("[[");
  write_separated(annotations, ", ");
  write("]]");
}

void Formatter::write(const AST::Arg &arg) {
  if (arg.isVisit)
    write("visit ");
  if (arg.name != nullptr)
    write(arg.name, ": ");
  write(arg.expr);
}

void Formatter::write(const AST::ArgList &args) {
  if (args.args.size() > 1 && length_with_reduced_whitespace(args.src) > 80) {
    increment_indent(1, [&]() {
      write('\n');
      write_separated(args.args, ",\n");
    });
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
  if (params.params.size() > 1 && length_with_reduced_whitespace(params.src) > 80) {
    increment_indent(2, [&]() {
      write('\n');
      write_separated(params.params, ",\n");
    });
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

void Formatter::write(const AST::LateIf &lateIf) { write(' ', lateIf.srcKwIf, ' ', lateIf.cond); }

} // namespace smdl
