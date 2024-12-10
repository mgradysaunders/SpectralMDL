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

void Formatter::write(AST::SourceRef srcRef) {
  if (!srcRef.empty()) {
    if (last == nullptr)
      last = src.data();
    const char *itr0 = srcRef.data();
    const char *itr1 = itr0 + srcRef.size();
    if (last < itr0) {
      auto inBetween = llvm::StringRef(last, itr0 - last);
      auto s = inBetween;
      int iter = 0;
      while (!s.empty() && iter++ < 1000) {
        s = s.ltrim();
        if (s.empty())
          break;
        if (s.starts_with("//")) {
          s = s.drop_front(2);
          str += "//";
          while (!s.empty()) {
            if (s.front() == '\n') {
              s = s.drop_front();
              write('\n');
              break;
            }
            str += s.front();
            s = s.drop_front();
          }
        }
      }
    }
    for (const char *itr = itr0; itr < itr1; itr++)
      write(*itr);
    last = itr1;
  }
}

#if 0
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
    write(file.srcKwSmdlSyntax, guarantee_newline());
  if (file.version)
    write(file.version->srcKwMdl, guarantee_space(), file.version->srcVersion, file.version->srcSemicolon, guarantee_newline());

  // TODO sort imports?
  for (auto &decl : file.imports)
    write(guarantee_newline(), decl);
  // TODO annotations
  for (auto &decl : file.globals)
    write(guarantee_newline(), decl->srcKwExport, guarantee_space(), decl);
}

//--{ Write: Decl
void Formatter::write(AST::Decl &decl) {
  write_type_switch<
      AST::Enum, AST::Function, AST::Import, AST::Struct, AST::Tag, AST::Typedef, AST::UnitTest, AST::UsingAlias,
      AST::UsingImport, AST::Variable>(decl);
}

void Formatter::write(AST::Function &decl) {
  if (!decl.srcAttrsAt.empty()) {
    write(decl.srcAttrsAt, decl.srcAttrsParenL);
    for (size_t i = 0; i < decl.srcAttrs.size(); i++) {
      write(decl.srcAttrs[i]);
      if (i + 1 < decl.srcAttrs.size())
        write(guarantee_space());
    }
    write(decl.srcAttrsParenR, guarantee_space());
  }
  write(decl.returnType, decl.earlyAnnotations, guarantee_space(), decl.name);
  if (decl.isVariant) {
    write(decl.srcVariantParenL, decl.srcVariantStar, decl.srcVariantParenR);
  } else {
    write(decl.params);
  }
  if (!decl.srcFrequency.empty())
    write(guarantee_space(), decl.srcFrequency);
  write(decl.lateAnnotations);
  if (decl.definition == nullptr) {
    write(decl.srcSemicolon);
  } else {
    if (auto stmt{llvm::dyn_cast<AST::Return>(decl.definition.get())}) {
      write(guarantee_space(), decl.srcEq, guarantee_space(), stmt->expr, decl.srcSemicolon);
    } else {
      write(guarantee_space(), decl.definition);
    }
  }
}

void Formatter::write(AST::Expr &expr) {
  write_type_switch<
      AST::Binary, AST::Call, AST::Cast, AST::Conditional, AST::GetField, AST::GetIndex, AST::Identifier, AST::Intrinsic,
      AST::Let, AST::LiteralBool, AST::LiteralFloat, AST::LiteralInt, AST::LiteralString, AST::Parens, AST::ReturnFrom,
      AST::SizeName, AST::Type, AST::Unary>(expr);
}

void Formatter::write(AST::Identifier &expr) {
  if (expr.isAbs)
    write(':', ':'); // TODO
  write(expr.names[0]);
  for (size_t i = 1; i < expr.names.size(); i++) {
    write(':', ':'); // TODO
    write(expr.names[i]);
  }
}

void Formatter::write(AST::Type &expr) {
  for (auto &srcAttr : expr.srcAttrs)
    write(srcAttr, guarantee_space());
  write(expr.expr);
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
      write(',', ' '); // TODO
  }
  write(stmt.srcSemicolon);
}

void Formatter::write(const AST::Arg &arg) {
  if (arg.isVisit)
    write(arg.srcKwVisit, guarantee_space());
  if (arg.name)
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
  write(params.srcParenL);
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
  write(params.srcParenR);
}

} // namespace smdl
