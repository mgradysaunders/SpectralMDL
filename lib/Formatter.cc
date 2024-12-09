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

void Formatter::write(guarantee_newline) {
  if (!str.empty() && str.back() != '\n')
    str += '\n';
}

void Formatter::write(guarantee_space) {
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

  // TODO sort imports?
  for (auto &decl : file.imports)
    write(guarantee_newline(), decl);
  // TODO annotations
  for (size_t i = 0; i < file.globals.size(); i++) {
    write(guarantee_newline(), file.globals[i]->srcKwExport, guarantee_space(), file.globals[i]);
  }
}

//--{ Write: Decl
void Formatter::write(AST::Decl &decl) {
  write_type_switch<
      AST::Enum, AST::Function, AST::Import, AST::Struct, AST::Tag, AST::Typedef, AST::UnitTest, AST::UsingAlias,
      AST::UsingImport, AST::Variable>(decl);
}

void Formatter::write(AST::Function &decl) {
  if (write(decl.attrs))
    write(' ');
  write(decl.returnType, decl.earlyAnnotations, guarantee_space(), decl.name);
  if (decl.isVariant) {
    write("(*)");
  } else {
    write(decl.params);
  }
  if (decl.frequency)
    write(guarantee_space(), *decl.frequency);
  write(decl.lateAnnotations);
  if (decl.definition == nullptr) {
    write(';');
  } else {
    if (auto stmt{llvm::dyn_cast<AST::Return>(decl.definition.get())}) {
      write(" = ", stmt->expr, ';');
    } else {
      write(guarantee_space(), decl.definition);
    }
  }
}

void Formatter::write(AST::Expr &expr) {
  write_type_switch<
      AST::Binary, AST::Call, AST::Cast, AST::Conditional, AST::GetField, AST::GetIndex, AST::Identifier, AST::Intrinsic,
      AST::Name, AST::Let, AST::LiteralBool, AST::LiteralFloat, AST::LiteralInt, AST::LiteralString, AST::Parens,
      AST::ReturnFrom, AST::SizeName, AST::Type, AST::Unary>(expr);
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
    write(guarantee_space(), *expr.annotations);
}

void Formatter::write(AST::Stmt &stmt) {
  write_type_switch<
      AST::Break, AST::Compound, AST::Continue, AST::DeclStmt, AST::Defer, AST::DoWhile, AST::ExprStmt, AST::For, AST::If,
      AST::Preserve, AST::Return, AST::Switch, AST::Unreachable, AST::Visit, AST::While>(stmt);
}

void Formatter::write(AST::Preserve &stmt) {
  write(stmt.srcKwPreserve, guarantee_space());
  for (size_t i = 0; i < stmt.exprs.size(); i++) {
    write(stmt.exprs[i]);
    if (i + 1 < stmt.exprs.size())
      write(", ");
  }
  write(stmt.srcSemicolon);
}

void Formatter::write(const AST::AnnotationBlock &annotations) {
  write(guarantee_space(), "[[");
  for (auto &annotation : annotations) {
    write(annotation.identifier, annotation.args);
    if (!annotation.srcComma.empty())
      write(annotation.srcComma, guarantee_space());
  }
  write("]]");
}

void Formatter::write(const AST::Arg &arg) {
  if (arg.isVisit)
    write(arg.srcKwVisit, guarantee_space());
  if (arg.name != nullptr)
    write(arg.name, arg.srcColonAfterName, guarantee_space());
  write(arg.expr);
}

void Formatter::write(const AST::ArgList &args) {
  write(args.srcParenL);
  if (args.args.size() > 1 && length_with_reduced_whitespace(args.src) > 80) {
    increment_indent(1, [&]() {
      write(guarantee_newline());
      for (auto &arg : args.args)
        write(guarantee_newline(), arg, arg.srcComma);
    });
    write(guarantee_newline());
  } else {
    for (auto &arg : args.args) {
      write(arg, arg.srcComma);
      if (!arg.srcComma.empty())
        write(guarantee_space());
    }
  }
  write(args.srcParenR);
}

void Formatter::write(const AST::Param &param) {
  write(param.type, guarantee_space(), param.name);
  if (param.init)
    write(guarantee_space(), param.srcEq, guarantee_space(), param.init);
  write(param.annotations);
}

void Formatter::write(const AST::ParamList &params) {
  if (params.params.size() > 1 && length_with_reduced_whitespace(params.src) > 80) {
    increment_indent(2, [&]() {
      for (auto &param : params.params) {
        write(guarantee_newline(), param, param.srcComma);
      }
    });
  } else {
    for (auto &param : params.params) {
      write(param);
      if (!param.srcComma.empty())
        write(param.srcComma, guarantee_space());
    }
  }
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
