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

void Formatter::write(SepSpace) {
  if (!str.empty() && str.back() != ' ' && str.back() != '\n')
    str += ' ';
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

#if 0
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
#endif

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
    write(file.globals[i]->srcKwExport, SEP_SPACE, file.globals[i]);
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
    write(SEP_SPACE, *decl.earlyAnnotations);
  write(SEP_SPACE, decl.name);
  if (decl.isVariant) {
    write("(*)");
  } else {
    write('(', decl.params, ')');
  }
  if (decl.frequency)
    write(SEP_SPACE, *decl.frequency);
  if (decl.lateAnnotations)
    write(SEP_SPACE, *decl.lateAnnotations);
  if (decl.definition == nullptr) {
    write(';');
  } else {
    if (auto stmt{llvm::dyn_cast<AST::Return>(decl.definition.get())}) {
      write(" = ", stmt->expr, ';');
    } else {
      write(SEP_SPACE, decl.definition);
    }
  }
}

void Formatter::write(AST::Import &decl) {
  write(decl.srcKwImport, SEP_SPACE);
  write_separated(decl.paths, ", ");
  write(decl.srcSemicolon);
}

void Formatter::write(AST::Struct &decl) {
  write(decl.srcKwStruct, SEP_SPACE, decl.name);
  if (!decl.tags.empty()) {
    write(decl.srcColonBeforeTags);
    for (auto &tag : decl.tags)
      write(SEP_SPACE, tag.srcKwDefault, SEP_SPACE, tag.type, tag.srcComma);
  }
  if (decl.fields.empty()) {
    write(SEP_SPACE, decl.srcBraceL, decl.srcBraceR, decl.srcSemicolon);
  } else {
    write(SEP_SPACE, decl.srcBraceL);
    increment_indent(1, [&]() {
      for (auto &field : decl.fields) {
        guarantee_newlines(1);
        write(field.srcKwVoid, SEP_SPACE, field.type, SEP_SPACE, field.name);
        if (field.init)
          write(SEP_SPACE, field.srcEq, SEP_SPACE, field.init);
        if (field.annotations)
          write(SEP_SPACE, *field.annotations);
        write(field.srcSemicolon);
      }
    });
    write('\n', decl.srcBraceR, decl.srcSemicolon);
  }
}

void Formatter::write(AST::UsingImport &decl) {
  write(decl.srcKwExport, SEP_SPACE, decl.srcKwUsing, SEP_SPACE, decl.path, SEP_SPACE, decl.srcKwImport, SEP_SPACE);
  if (decl.is_import_all())
    write(decl.srcStar);
  else
    write_separated(decl.names, ", ");
  write(decl.srcSemicolon);
}

void Formatter::write(AST::Variable &decl) {
  write(decl.type);
  for (auto &declarator : decl.declarators) {
    write(SEP_SPACE, declarator.name);
    if (declarator.init != nullptr)
      write(SEP_SPACE, declarator.srcEq, SEP_SPACE, declarator.init);
    else if (declarator.args)
      write(declarator.args);
    if (declarator.annotations)
      write(SEP_SPACE, *declarator.annotations);
    write(declarator.srcComma);
  }
  write(decl.srcSemicolon);
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
  write(expr.srcKwLet, SEP_SPACE);
  if (!expr.srcBraceL.empty()) {
    write(expr.srcBraceL);
    increment_indent(1, [&]() {
      for (auto &var : expr.vars) {
        guarantee_newlines(1);
        write(var);
      }
    });
    write('\n', expr.srcBraceR, SEP_SPACE, expr.srcKwIn, SEP_SPACE, expr.expr);
  } else {
    sanity_check(expr.vars.size() == 1);
    write(expr.vars[0], SEP_SPACE, expr.srcKwIn, SEP_SPACE, expr.expr);
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
    write(SEP_SPACE, *expr.annotations);
}
//--}

//--{ Write: Stmts
void Formatter::write(AST::Stmt &stmt) {
  write_type_switch<
      AST::Break, AST::Compound, AST::Continue, AST::DeclStmt, AST::Defer, AST::DoWhile, AST::ExprStmt, AST::For, AST::If,
      AST::Preserve, AST::Return, AST::Switch, AST::Unreachable, AST::Visit, AST::While>(stmt);
}

void Formatter::write(AST::Compound &stmt) {
  write(stmt.srcBraceL);
  increment_indent(1, [&]() {
    for (size_t i = 0; i < stmt.stmts.size(); i++) {
      write('\n', stmt.stmts[i]);
    }
  });
  write('\n', stmt.srcBraceR);
}

void Formatter::write(AST::Preserve &stmt) {
  write(stmt.srcKwPreserve, SEP_SPACE);
  for (size_t i = 0; i < stmt.exprs.size(); i++) {
    write(stmt.exprs[i]);
    if (i + 1 < stmt.exprs.size())
      write(", ");
  }
  write(stmt.srcSemicolon);
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
//--}

void Formatter::write(const AST::Annotation &annotation) { write(annotation.identifier, annotation.args); }

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
  write('(');
  if (args.args.size() > 1 && length_with_reduced_whitespace(args.src) > 80) {
    increment_indent(1, [&]() {
      write('\n');
      write_separated(args.args, ",\n");
    });
    write('\n');
  } else {
    write_separated(args.args, ", ");
  }
  write(')');
}

void Formatter::write(const AST::Param &param) {
  write(param.type, SEP_SPACE, param.name);
  if (param.init)
    write(" = ", param.init);
  if (param.annotations)
    write(SEP_SPACE, *param.annotations);
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
    write(SEP_SPACE, declarator.annotations);
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

} // namespace smdl
