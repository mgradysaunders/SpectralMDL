// vim:foldmethod=marker:foldlevel=0:fmr=--{,--}
#include "Parser.h"

namespace smdl {

//--{ Basics
Parser::Char Parser::peek(size_t *sz) const {
  auto result{utf8::decode_next(get_remaining_text(), sz)};
  if (!result)
    report_error("utf-8 decoding failed");
  return *result;
}

void Parser::skip() {
  uint32_t i = state.i;
  auto skipSome = [&] {
    if (get_remaining_text().starts_with("//")) {
      next();
      next();
      while (!is_eof() && peek() != '\n')
        next();
      return true;
    }
    if (get_remaining_text().starts_with("/*")) {
      next();
      next();
      while (!is_eof() && !get_remaining_text().starts_with("*/"))
        next();
      if (is_eof())
        report_error("unexpected EOF in multiline comment");
      next();
      next();
      return true;
    }
    if (utf8::is_space(peek()))
      return next(), true;
    return false;
  };
  while (!is_eof() && skipSome())
    continue;
}

Parser::Char Parser::next() {
  if (!is_eof()) {
    auto sz{size_t(0)};
    auto ch{peek(&sz)};
    if (ch == '\n') {
      state.lineNo++;
      state.charNo = 1;
    } else
      state.charNo++;
    state.i += sz;
    return ch;
  } else {
    return 0;
  }
}

bool Parser::next(llvm::StringRef str, AST::SourceRef *src) {
  checkpoint();
  auto cursor{save_cursor()};
  while (!str.empty()) {
    auto sz{size_t(0)};
    auto ch{utf8::decode_next(str, &sz)};
    if (!ch)
      report_error("utf-8 decoding failed");
    if (!next(*ch))
      break;
    else
      str = str.substr(sz);
  }
  if (str.empty()) {
    if (src)
      *src = source_since(cursor);
    return accept();
  } else {
    return reject();
  }
}

bool Parser::next_word(llvm::StringRef str, AST::SourceRef *src) {
  checkpoint();
  if (!next(str, src))
    return reject();
  if (utf8::is_word(peek())) {
    return reject(); // Make sure the keyword doesn't continue to form a name!
  } else {
    return accept();
  }
}

AST::SourceRef Parser::next_word() {
  checkpoint();
  bool success{false};
  auto i{get_position()};
  if (isSmdlSyntax && peek() == '$')
    next();
  if (utf8::is_alpha(peek()))
    success = true, next();
  while (utf8::is_word(peek()))
    success = true, next();
  if (!success) {
    reject();
    return {};
  } else {
    accept();
    return text.substr(i, get_position() - i);
  }
}

AST::SourceRef Parser::next_int() {
  auto i{get_position()};
  while (utf8::is_digit(peek()))
    next();
  return text.substr(i, get_position() - i);
}

//--}

//--{ Parse: Decls
auto Parser::parse_mdl() -> unique_bump_ptr_wrapper<AST::File> {
  skip();
  auto srcKwSmdlSyntax{AST::SourceRef()};
  isSmdlSyntax = next("#smdl_syntax", &srcKwSmdlSyntax);
  skip();
  auto version{parse_mdl_version()};
  if (!version && !isSmdlSyntax)
    report_error("expected MDL version");
  auto imports{vector_or_SmallVector<unique_bump_ptr<AST::Decl>>{}};
  while (true) {
    auto parse_any_import{[&]() -> unique_bump_ptr_wrapper<AST::Decl> {
      if (auto decl{parse_using_alias()})
        return decl;
      if (auto decl{parse_using_import()})
        return decl;
      if (auto decl{parse_import()})
        return decl;
      return nullptr;
    }};
    auto decl{parse_any_import()};
    if (!decl)
      break;
    imports.push_back(std::move(decl));
  }
  auto srcKwModule{AST::SourceRef()};
  auto annotations{unique_bump_ptr<AST::AnnotationBlock>{}};
  auto srcSemicolonAfterModule{AST::SourceRef()};
  skip();
  if (next_word("module", &srcKwModule)) {
    annotations = parse_annotation_block();
    if (!annotations)
      report_error("expected annotation block after 'module'");
    if (!delimiter(';', &srcSemicolonAfterModule))
      report_error("expected ';' after 'module [[ ... ]]'");
  }
  auto globals{vector_or_SmallVector<unique_bump_ptr<AST::Decl>>{}};
  while (true) {
    auto global{parse_global_declaration()};
    if (!global)
      break;
    globals.push_back(std::move(global));
    skip();
    if (is_eof())
      break;
  }
  if (!is_eof())
    report_error("expected EOF (apparently failed to parse everything!)");
  return bump_allocate<AST::File>(
      isSmdlSyntax, srcKwSmdlSyntax, version, std::move(imports), srcKwModule, std::move(annotations), srcSemicolonAfterModule,
      std::move(globals));
}

auto Parser::parse_mdl_version() -> std::optional<AST::Version> {
  AST::Version version{};
  auto cursor0{save_cursor()};
  if (!next_word("mdl", &version.srcKwMdl))
    return std::nullopt;
  skip();
  auto cursor1{save_cursor()};
  auto major{next_int()};
  if (major.empty() || !next('.'))
    report_error("expected 'MAJOR.MINOR' version after 'mdl'", cursor0);
  version.major = llvm::APInt(32, major, 10).getLimitedValue();
  auto minor{next_int()};
  if (minor.empty())
    report_error("expected 'MAJOR.MINOR' version after 'mdl'", cursor0);
  version.minor = llvm::APInt(32, minor, 10).getLimitedValue();
  version.srcVersion = source_since(cursor1);
  if (!delimiter(';', &version.srcSemicolon))
    report_error("expected ';' after 'mdl MAJOR.MINOR'", cursor0);
  return version;
}

auto Parser::parse_import_path(bool isUnicode) -> unique_bump_ptr_wrapper<AST::Identifier> {
  checkpoint();
  auto cursor{save_cursor()};
  auto names{vector_or_SmallVector<AST::Identifier::IdentifierName>{}};
  auto srcDoubleColon{AST::SourceRef()};
  bool isAbs{next("::", &srcDoubleColon)};
  while (true) {
    auto identifierName{AST::Identifier::IdentifierName{srcDoubleColon, {}, {}}};
    auto parse_any_name{[&]() -> bool {
      auto cursor{save_cursor()};
      if (auto name{parse_simple_name()}) {
        identifierName.name = *name;
        return true;
      }
      if (auto srcName{AST::SourceRef()}; next("..", &srcName) || next(".", &srcName) || next("*", &srcName)) {
        identifierName.name = AST::Name{source_location(cursor), srcName};
        return true;
      }
      if (isUnicode) {
        if (auto expr{parse_literal_expression()}; expr && llvm::isa<AST::LiteralString>(expr.get())) {
          identifierName.literalString = std::move(expr);
          return true;
        }
      }
      return false;
    }};
    if (!parse_any_name())
      break;
    names.push_back(std::move(identifierName));
    skip();
    srcDoubleColon = {};
    if (names.back().name.srcName == "*" || !next("::", &srcDoubleColon))
      break;
  }
  if (!isAbs && names.empty()) {
    reject();
    return nullptr;
  }
  accept();
  return attach(bump_allocate<AST::Identifier>(std::move(names)), cursor);
}

auto Parser::parse_using_alias() -> unique_bump_ptr_wrapper<AST::UsingAlias> {
  checkpoint();
  auto cursor{save_cursor()};
  auto srcKwUsing{AST::SourceRef()};
  if (!next_word("using", &srcKwUsing)) {
    reject();
    return nullptr;
  }
  skip();
  auto name{parse_simple_name()};
  auto srcEq{AST::SourceRef()};
  if (!name || !delimiter('=', &srcEq)) {
    reject();
    return nullptr;
  }
  auto path{parse_import_path(/*isUnicode=*/true)};
  if (!path)
    report_error("expected import path after 'using ... ='", cursor);
  auto srcSemicolon{AST::SourceRef()};
  if (!delimiter(';', &srcSemicolon))
    report_error("expected ';' after 'using ... = ...'", cursor);
  accept();
  return attach(bump_allocate<AST::UsingAlias>(srcKwUsing, *name, srcEq, std::move(path), srcSemicolon), cursor);
}

auto Parser::parse_using_import() -> unique_bump_ptr_wrapper<AST::UsingImport> {
  checkpoint();
  auto cursor{save_cursor()};
  auto srcKwExport{AST::SourceRef()};
  bool isExport{next_word("export", &srcKwExport)};
  auto srcKwUsing{AST::SourceRef()};
  skip();
  if (!next_word("using", &srcKwUsing)) {
    reject();
    return nullptr;
  }
  auto path{parse_import_path(/*isUnicode=*/false)};
  if (!path) {
    reject();
    return nullptr;
  }
  if (path->names.back().name.srcName == "*")
    report_error("import path after '[export] using' must not end with '::*'");
  skip();
  auto srcKwImport{AST::SourceRef()};
  if (!next_word("import", &srcKwImport))
    report_error("expected 'import' after '[export] using ...'", cursor);
  skip();
  auto names{vector_or_SmallVector<AST::UsingImport::ImportName>{}};
  auto srcStar{AST::SourceRef()};
  if (next('*', &srcStar)) {
    names.push_back(AST::UsingImport::ImportName{srcStar, {}});
  } else {
    while (true) {
      skip();
      auto name{parse_simple_name()};
      if (!name)
        report_error("expected import name", cursor);
      names.push_back(AST::UsingImport::ImportName{name->srcName, {}});
      if (!delimiter(',', &names.back().srcComma))
        break;
    }
  }
  auto srcSemicolon{AST::SourceRef()};
  if (!delimiter(';', &srcSemicolon))
    report_error("expected ';' after '[export] using ... import ...'", cursor);
  accept();
  auto decl{bump_allocate<AST::UsingImport>(srcKwUsing, std::move(path), srcKwImport, std::move(names), srcSemicolon)};
  decl->isExport = isExport;
  decl->srcKwExport = srcKwExport;
  return attach(decl, cursor);
}

auto Parser::parse_import() -> unique_bump_ptr_wrapper<AST::Import> {
  auto cursor{save_cursor()};
  auto srcKwImport{AST::SourceRef()};
  if (!next_word("import", &srcKwImport))
    return nullptr;
  auto paths{vector_or_SmallVector<AST::Import::ImportPath>{}};
  while (true) {
    auto path{parse_import_path(/*isUnicode=*/false)};
    if (!path)
      report_error("expected import path", cursor);
    paths.push_back(AST::Import::ImportPath{std::move(path), {}});
    if (!delimiter(',', &paths.back().srcComma))
      break;
  }
  auto srcSemicolon{AST::SourceRef()};
  if (!delimiter(';', &srcSemicolon))
    report_error("expected ';' after 'import ...'", cursor);
  return attach(bump_allocate<AST::Import>(srcKwImport, std::move(paths), srcSemicolon), cursor);
}

auto Parser::parse_global_declaration() -> unique_bump_ptr_wrapper<AST::Decl> {
  checkpoint();
  auto srcKwExport{AST::SourceRef()};
  bool isExport{next_word("export", &srcKwExport)};
  auto decl{[&]() -> unique_bump_ptr_wrapper<AST::Decl> {
    if (auto decl{parse_function_declaration()})
      return decl;
    if (auto decl{parse_type_declaration()})
      return decl;
    if (auto decl{parse_variable_declaration()})
      return decl;
    if (isSmdlSyntax) {
      if (auto decl{parse_unit_test_declaration()})
        return decl;
    }
    return nullptr;
  }()};
  if (!decl) {
    reject();
    if (next_word("using") || next_word("import"))
      report_error("'using' and 'import' declarations must appear at the top of the file");
    return nullptr;
  }
  decl.get()->isGlobal = true;
  decl.get()->isExport = isExport;
  decl.get()->srcKwExport = srcKwExport;
  accept();
  return decl;
}

auto Parser::parse_type_declaration() -> unique_bump_ptr_wrapper<AST::Decl> {
  if (auto decl{parse_alias_type_declaration()})
    return decl;
  if (auto decl{parse_struct_type_declaration()})
    return decl;
  if (auto decl{parse_enum_type_declaration()})
    return decl;
  if (isSmdlSyntax) {
    if (auto decl{parse_tag_declaration()})
      return decl;
  }
  return nullptr;
}

auto Parser::parse_alias_type_declaration() -> unique_bump_ptr_wrapper<AST::Typedef> {
  auto cursor{save_cursor()};
  auto srcKwTypedef{AST::SourceRef()};
  if (!next_word("typedef", &srcKwTypedef))
    return nullptr;
  auto type{parse_type()};
  if (!type)
    report_error("expected type after 'typedef'", cursor);
  auto name{parse_simple_name()};
  if (!name)
    report_error("expected name after 'typedef ...'", cursor);
  auto srcSemicolon{AST::SourceRef()};
  if (!delimiter(';', &srcSemicolon))
    report_error("expected ';' after 'typedef ... NAME'", cursor);
  return attach(bump_allocate<AST::Typedef>(srcKwTypedef, std::move(type), *name, srcSemicolon), cursor);
}

auto Parser::parse_struct_type_declaration() -> unique_bump_ptr_wrapper<AST::Struct> {
  checkpoint();
  auto cursor{save_cursor()};
  auto srcKwStruct{AST::SourceRef()};
  if (!next_word("struct", &srcKwStruct)) {
    reject();
    return nullptr;
  }
  auto name{parse_simple_name()};
  if (!name)
    report_error("expected name after 'struct'", cursor);
  auto srcColonBeforeTags{AST::SourceRef()};
  auto tags{vector_or_SmallVector<AST::Struct::Tag>{}};
  if (delimiter(':', &srcColonBeforeTags)) {
    while (true) {
      auto srcKwDefault{AST::SourceRef()};
      bool isDefault{next_word("default", &srcKwDefault)};
      auto tagName{parse_identifier()};
      if (!tagName)
        break;
      auto &tag{tags.emplace_back()};
      tag.isDefault = isDefault;
      tag.srcKwDefault = srcKwDefault;
      tag.type.reset(bump_allocate<AST::Type>(vector_or_SmallVector<AST::SourceRef>(), std::move(tagName)));
      if (!delimiter(',', &tag.srcComma))
        break;
    }
  }
  auto annotations{parse_annotation_block()};
  auto srcBraceL{AST::SourceRef()};
  if (!delimiter('{', &srcBraceL))
    report_error("expected '{' after 'struct NAME'", cursor);
  auto fields{vector_or_SmallVector<AST::Struct::Field>{}};
  while (true) {
    auto field{parse_struct_field_declarator()};
    if (!field)
      break;
    fields.push_back(std::move(*field));
    skip();
    if (peek() == '}')
      break;
  }
  auto srcBraceR{AST::SourceRef()};
  if (!delimiter('}', &srcBraceR))
    report_error("expected '}' after 'struct NAME { ...'", cursor);
  auto srcSemicolon{AST::SourceRef()};
  if (!delimiter(';', &srcSemicolon))
    report_error("expected ';' after 'struct NAME { ... }'", cursor);
  accept();
  return attach(
      bump_allocate<AST::Struct>(
          srcKwStruct, *name, srcColonBeforeTags, std::move(tags), std::move(annotations), srcBraceL, std::move(fields),
          srcBraceR, srcSemicolon),
      cursor);
}

auto Parser::parse_struct_field_declarator() -> std::optional<AST::Struct::Field> {
  checkpoint();
  auto cursor{save_cursor()};
  auto srcKwVoid{AST::SourceRef()};
  bool isVoid{isSmdlSyntax && next_word("void", &srcKwVoid)};
  auto type{parse_type()};
  if (!type) {
    reject();
    return std::nullopt;
  }
  auto name{parse_simple_name()};
  if (!name) {
    reject();
    return std::nullopt;
  }
  auto field{AST::Struct::Field{}};
  field.isVoid = isVoid;
  field.srcKwVoid = srcKwVoid;
  field.type = std::move(type);
  field.name = *name;
  if (delimiter('=', &field.srcEq)) {
    auto init{parse_expression()};
    if (!init)
      report_error("expected initializer after '='", cursor);
    field.init = std::move(init);
  }
  field.annotations = parse_annotation_block();
  if (!delimiter(';', &field.srcSemicolon))
    report_error("expected ';' after field", cursor);
  accept();
  return std::move(field);
}

auto Parser::parse_enum_type_declaration() -> unique_bump_ptr_wrapper<AST::Enum> {
  auto cursor{save_cursor()};
  auto srcKwEnum{AST::SourceRef()};
  if (!next_word("enum", &srcKwEnum))
    return nullptr;
  auto name{parse_simple_name()};
  if (!name)
    report_error("expected name after 'enum'", cursor);
  auto annotations{parse_annotation_block()};
  auto srcBraceL{AST::SourceRef()};
  if (!delimiter('{', &srcBraceL))
    report_error("expected '{' after 'enum NAME'", cursor);
  auto declarators{vector_or_SmallVector<AST::Enum::Declarator>{}};
  while (true) {
    auto declarator{parse_enum_value_declarator()};
    if (!declarator)
      break;
    declarators.push_back(std::move(*declarator));
    if (!delimiter(',', &declarators.back().srcComma))
      break;
  }
  auto srcBraceR{AST::SourceRef()};
  if (!delimiter('}', &srcBraceR))
    report_error("expected '}' after 'enum NAME { ...'", cursor);
  auto srcSemicolon{AST::SourceRef()};
  if (!delimiter(';', &srcSemicolon))
    report_error("expected ';' after 'enum NAME { ... }'", cursor);
  return attach(
      bump_allocate<AST::Enum>(
          srcKwEnum, *name, std::move(annotations), srcBraceL, std::move(declarators), srcBraceR, srcSemicolon),
      cursor);
}

auto Parser::parse_enum_value_declarator() -> std::optional<AST::Enum::Declarator> {
  checkpoint();
  auto cursor{save_cursor()};
  auto name{parse_simple_name()};
  if (!name) {
    reject();
    return std::nullopt;
  }
  auto declarator{AST::Enum::Declarator{}};
  declarator.name = *name;
  if (delimiter('=', &declarator.srcEq)) {
    auto init{parse_assignment_expression()};
    if (!init)
      report_error("expected initializer after '='", cursor);
    declarator.init = std::move(init);
  }
  declarator.annotations = parse_annotation_block();
  accept();
  return std::move(declarator);
}

auto Parser::parse_variable_declaration() -> unique_bump_ptr_wrapper<AST::Variable> {
  checkpoint();
  auto cursor{save_cursor()};
  auto type{parse_type()};
  if (!type) {
    reject();
    return nullptr;
  }
  auto declarators{vector_or_SmallVector<AST::Variable::Declarator>{}};
  while (true) {
    auto declarator{parse_variable_declarator()};
    if (!declarator)
      break;
    declarators.push_back(std::move(*declarator));
    if (!delimiter(',', &declarators.back().srcComma))
      break;
  }
  if (declarators.empty()) {
    reject();
    return nullptr;
  }
  auto srcSemicolon{AST::SourceRef()};
  if (!delimiter(';', &srcSemicolon))
    report_error("expected ';' after variable declaration", cursor);
  accept();
  return attach(bump_allocate<AST::Variable>(std::move(type), std::move(declarators), srcSemicolon), cursor);
}

auto Parser::parse_variable_declarator() -> std::optional<AST::Variable::Declarator> {
  checkpoint();
  auto cursor{save_cursor()};
  auto name{parse_simple_name()};
  if (!name) {
    reject();
    return std::nullopt;
  }
  auto declarator{AST::Variable::Declarator{}};
  declarator.name = *name;
  if (delimiter('=', &declarator.srcEq)) {
    auto init{parse_assignment_expression()};
    if (!init)
      report_error("expected initializer after '='", cursor);
    declarator.init = std::move(init);
  } else if (auto args{parse_argument_list()}) {
    declarator.args = std::move(args);
  }
  declarator.annotations = parse_annotation_block();
  accept();
  return declarator;
}

auto Parser::parse_function_declaration() -> unique_bump_ptr_wrapper<AST::Function> {
  checkpoint();
  auto cursor{save_cursor()};
  auto srcAttrsAt{AST::SourceRef()};
  auto srcAttrsParenL{AST::SourceRef()};
  auto srcAttrs{vector_or_SmallVector<AST::SourceRef>()};
  auto srcAttrsParenR{AST::SourceRef()};
  if (isSmdlSyntax && delimiter('@', &srcAttrsAt)) {
    if (!delimiter('(', &srcAttrsParenL))
      report_error("expected '@(...)' syntax for function attributes", cursor);
    while (true) {
      checkpoint();
      auto srcAttr{AST::SourceRef()};
      if (next_word(
              {"alwaysinline", "cold", "foreign", "hot", "macro", "noinline", "optnone", "optsize", "pure", "visible"},
              &srcAttr)) {
        accept();
        srcAttrs.push_back(srcAttr);
      } else {
        skip();
        if (peek() != ')')
          report_error("unrecognized function attribute", cursor);
        reject();
        break;
      }
    }
    if (!delimiter(')', &srcAttrsParenR))
      report_error("expected '@(...)' syntax for function attributes", cursor);
  }
  auto type{parse_type()};
  if (!type) {
    reject();
    return nullptr;
  }
  auto earlyAnnotations{parse_annotation_block()};
  auto name{parse_simple_name()};
  if (!name) {
    reject();
    return nullptr;
  }
  bool isVariant{};
  auto srcVariantParenL{AST::SourceRef()};
  auto srcVariantStar{AST::SourceRef()};
  auto srcVariantParenR{AST::SourceRef()};
  auto params{parse_parameter_list()};
  if (!params) {
    if (delimiter('(', &srcVariantParenL) && delimiter('*', &srcVariantStar) && delimiter(')', &srcVariantParenR)) {
      isVariant = true;
      params = AST::ParamList();
    } else {
      reject();
      return nullptr;
    }
  }
  auto srcFrequency{AST::SourceRef()};
  if (!next_word({"uniform", "varying"}, &srcFrequency))
    srcFrequency = {};
  auto lateAnnotations{parse_annotation_block()};
  auto srcEq{AST::SourceRef()};
  auto definition{unique_bump_ptr<AST::Node>{}};
  auto srcSemicolon{AST::SourceRef()};
  skip();
  if (isVariant && peek() != '=')
    report_error("function variant must be defined by 'let' or call expression", cursor);
  if (delimiter(';', &srcSemicolon)) {
    // Nothing
  } else if (delimiter('=', &srcEq)) {
    auto def{parse_expression()};
    if (!def)
      report_error("expected function expression after '='", cursor);
    if (!delimiter(';', &srcSemicolon))
      report_error("expected ';' after function expression", cursor);
    if (isVariant && !llvm::isa<AST::Let>(def.get()) && !llvm::isa<AST::Call>(def.get()))
      report_error("function variant must be defined by 'let' or call expression", cursor);
    definition.reset(bump_allocate<AST::Return>(llvm::StringRef(), std::move(def), std::nullopt, llvm::StringRef()));
  } else {
    auto def{parse_compound_statement()};
    if (!def)
      report_error("expected ';' or function definition", cursor);
    definition = std::move(def);
  }
  accept();
  return attach(
      bump_allocate<AST::Function>(
          srcAttrsAt, srcAttrsParenL, std::move(srcAttrs), srcAttrsParenR, std::move(type), std::move(earlyAnnotations), *name,
          std::move(*params), isVariant, srcVariantParenL, srcVariantStar, srcVariantParenR, srcFrequency,
          std::move(lateAnnotations), srcEq, std::move(definition), srcSemicolon),
      cursor);
}

auto Parser::parse_tag_declaration() -> unique_bump_ptr_wrapper<AST::Tag> {
  auto cursor{save_cursor()};
  auto srcKwTag{AST::SourceRef()};
  if (!next_word("tag", &srcKwTag))
    return nullptr;
  auto name{parse_simple_name()};
  if (!name)
    report_error("expected name after 'tag'", cursor);
  auto srcSemicolon{AST::SourceRef()};
  if (!delimiter(';', &srcSemicolon))
    report_error("expected ';' after 'tag ...'", cursor);
  return attach(bump_allocate<AST::Tag>(srcKwTag, *name, srcSemicolon), cursor);
}

auto Parser::parse_unit_test_declaration() -> unique_bump_ptr_wrapper<AST::UnitTest> {
  auto cursor{save_cursor()};
  auto srcKwUnitTest{AST::SourceRef()};
  if (!next_word("unit_test", &srcKwUnitTest))
    return nullptr;
  auto desc{parse_literal_expression()};
  if (!desc || !llvm::isa<AST::LiteralString>(desc.get()))
    report_error("expected literal string after 'unit_test'", cursor);
  auto body{parse_compound_statement()};
  if (!body)
    report_error("expected compound statement after 'unit_test ...'", cursor);
  return attach(
      bump_allocate<AST::UnitTest>(
          srcKwUnitTest, unique_bump_ptr<AST::LiteralString>(static_cast<AST::LiteralString *>(desc.release())),
          std::move(body)),
      cursor);
}
//--}

//--{ Parse: Exprs
auto Parser::parse_simple_name() -> std::optional<AST::Name> {
  checkpoint();
  auto cursor{save_cursor()};
  if (auto word{next_word()}; !word.empty()) {
    static const llvm::StringRef reservedKeywords[]{
        "break",  "case",   "cast", "const",   "continue", "default", "do",      "else",    "enum",
        "export", "false",  "for",  "if",      "import",   "let",     "module",  "package", "return",
        "struct", "switch", "true", "typedef", "uniform",  "using",   "varying", "while",
    };
    for (const auto &reservedKeyword : reservedKeywords) {
      if (word == reservedKeyword) {
        reject();
        return std::nullopt;
      }
    }
    if (isSmdlSyntax) {
      static const llvm::StringRef reservedKeywordsExtendedSyntax[]{
          "defer", "inline", "return_from", "static", "tag", "unreachable", "visit",
      };
      for (const auto &reservedKeyword : reservedKeywordsExtendedSyntax) {
        if (word == reservedKeyword) {
          reject();
          return std::nullopt;
        }
      }
    }
    accept();
    return AST::Name{source_location(cursor), word};
  } else {
    reject();
    return std::nullopt;
  }
}

auto Parser::parse_identifier() -> unique_bump_ptr_wrapper<AST::Identifier> {
  checkpoint();
  auto cursor{state};
  auto pos{get_position()};
  auto names{vector_or_SmallVector<AST::Identifier::IdentifierName>{}};
  auto srcDoubleColon{AST::SourceRef()};
  auto isAbs{next("::", &srcDoubleColon)};
  if (auto name{parse_simple_name()}) {
    names.push_back(AST::Identifier::IdentifierName{srcDoubleColon, *name, {}});
  } else {
    if (isAbs)
      report_error("expected name after '::'", cursor);
    else {
      reject();
      return nullptr;
    }
  }
  while (true) {
    checkpoint();
    srcDoubleColon = {};
    if (next("::", &srcDoubleColon)) {
      if (auto name{parse_simple_name()}) {
        names.push_back(AST::Identifier::IdentifierName{srcDoubleColon, *name, {}});
        accept();
        continue;
      }
    }
    reject();
    break;
  }
  if (pos != get_position()) {
    accept();
    return attach(bump_allocate<AST::Identifier>(std::move(names)), cursor);
  } else {
    reject();
    return nullptr;
  }
}

auto Parser::parse_type() -> unique_bump_ptr_wrapper<AST::Type> {
  checkpoint();
  auto cursor{save_cursor()};
  auto srcAttrs{vector_or_SmallVector<AST::SourceRef>()};
  while (true) {
    checkpoint();
    auto srcAttr{AST::SourceRef()};
    if (next_word({"const", "inline", "static", "uniform", "varying"}, &srcAttr)) {
      accept();
      srcAttrs.push_back(srcAttr);
    } else {
      reject();
      break;
    }
  }
  auto expr{parse_unary_expression()};
  if (!expr) {
    reject();
    return nullptr;
  }
  return attach(bump_allocate<AST::Type>(std::move(srcAttrs), std::move(expr)), cursor);
}

auto Parser::parse_parameter() -> std::optional<AST::Param> {
  checkpoint();
  auto cursor{save_cursor()};
  auto type{parse_type()};
  if (!type) {
    reject();
    return std::nullopt;
  }
  auto name{parse_simple_name()};
  if (!name) {
    reject();
    return std::nullopt;
  }
  auto param{AST::Param{}};
  param.type = std::move(type);
  param.name = *name;
  if (delimiter('=', &param.srcEq)) {
    auto init{parse_assignment_expression()};
    if (!init)
      report_error("expected initializer after '='");
    param.init = std::move(init);
  }
  param.annotations = parse_annotation_block();
  accept();
  return std::move(param);
}

auto Parser::parse_parameter_list() -> std::optional<AST::ParamList> {
  checkpoint();
  auto cursor{save_cursor()};
  auto params{AST::ParamList{}};
  if (!delimiter('(', &params.srcParenL)) {
    reject();
    return std::nullopt;
  }
  while (true) {
    auto param{parse_parameter()};
    if (!param)
      break;
    params.params.push_back(std::move(*param));
    if (!delimiter(',', &params.params.back().srcComma))
      break;
    skip();
  }
  if (!delimiter(')', &params.srcParenR)) {
    reject();
    return std::nullopt;
  }
  accept();
  params.src = source_since(cursor);
  return std::move(params);
}

auto Parser::parse_argument() -> std::optional<AST::Arg> {
  checkpoint();
  auto cursor{save_cursor()};
  auto arg{AST::Arg{}};
  arg.isVisit = isSmdlSyntax && next_word("visit", &arg.srcKwVisit);
  arg.name = [&]() -> AST::Name {
    checkpoint();
    if (auto name{parse_simple_name()}; name && delimiter(':', &arg.srcColonAfterName) && peek() != ':' && peek() != '=') {
      accept();
      return *name;
    } else {
      arg.srcColonAfterName = {};
      reject();
      return {};
    }
  }();
  arg.expr = parse_assignment_expression();
  if (!arg.expr) {
    reject();
    return std::nullopt;
  }
  arg.srcLoc.file = file;
  arg.srcLoc.line = cursor.lineNo;
  arg.src = source_since(cursor);
  accept();
  return std::move(arg);
}

auto Parser::parse_argument_list() -> std::optional<AST::ArgList> {
  checkpoint();
  auto cursor{save_cursor()};
  auto args{AST::ArgList{}};
  if (!delimiter('(', &args.srcParenL)) {
    reject();
    return std::nullopt;
  }
  while (true) {
    auto arg{parse_argument()};
    if (!arg)
      break;
    args.args.push_back(std::move(*arg));
    if (!delimiter(',', &args.args.back().srcComma))
      break;
  }
  if (!delimiter(')', &args.srcParenR)) {
    reject();
    return std::nullopt;
  }
  accept();
  args.srcLoc.file = file;
  args.srcLoc.line = cursor.lineNo;
  args.src = source_since(cursor);
  return std::move(args);
}

auto Parser::parse_annotation() -> std::optional<AST::Annotation> {
  checkpoint();
  auto identifier{parse_identifier()};
  if (!identifier) {
    reject();
    return std::nullopt;
  }
  auto args{parse_argument_list()};
  if (!args) {
    reject();
    return std::nullopt;
  }
  accept();
  return AST::Annotation{std::move(identifier), std::move(*args)};
}

auto Parser::parse_annotation_block() -> unique_bump_ptr_wrapper<AST::AnnotationBlock> {
  skip();
  auto srcDoubleBrackL{AST::SourceRef()};
  if (!next("[[", &srcDoubleBrackL))
    return nullptr;
  auto annotations{vector_or_SmallVector<AST::Annotation>{}};
  while (true) {
    auto annotation{parse_annotation()};
    if (!annotation)
      break;
    annotations.push_back(std::move(*annotation));
    if (!delimiter(',', &annotations.back().srcComma))
      break;
    skip();
    if (get_remaining_text().starts_with("]]"))
      break;
  }
  auto srcDoubleBrackR{AST::SourceRef()};
  skip();
  if (!next("]]", &srcDoubleBrackR))
    report_error("expected ']]' to close annotation block");
  return bump_allocate<AST::AnnotationBlock>(srcDoubleBrackL, std::move(annotations), srcDoubleBrackR);
}

auto Parser::parse_expression() -> unique_bump_ptr_wrapper<AST::Expr> {
  return parse_binary_left_associative({AST::BinaryOp::Comma}, [&] { return parse_assignment_expression(); });
}

auto Parser::parse_expression_in_parentheses() -> unique_bump_ptr_wrapper<AST::Expr> {
  checkpoint();
  auto cursor{save_cursor()};
  auto srcDollar{AST::SourceRef()};
  auto srcParenL{AST::SourceRef()};
  auto isCompileTime{isSmdlSyntax && next('$', &srcDollar)};
  if (!delimiter('(', &srcParenL)) {
    reject();
    return nullptr;
  }
  auto expr{parse_expression()};
  if (!expr) {
    reject();
    return nullptr;
  }
  auto srcParenR{AST::SourceRef()};
  if (!delimiter(')', &srcParenR)) {
    reject();
    return nullptr;
  }
  accept();
  return attach(bump_allocate<AST::Parens>(isCompileTime, srcDollar, srcParenL, std::move(expr), srcParenR), cursor);
}

auto Parser::parse_assignment_expression() -> unique_bump_ptr_wrapper<AST::Expr> {
  return parse_binary_right_associative(
      {AST::BinaryOp::Def, //
       AST::BinaryOp::EqLShr, AST::BinaryOp::EqAdd, AST::BinaryOp::EqSub, AST::BinaryOp::EqMul, AST::BinaryOp::EqDiv,
       AST::BinaryOp::EqRem, AST::BinaryOp::EqShl, AST::BinaryOp::EqAShr, AST::BinaryOp::EqAnd, AST::BinaryOp::EqOr,
       AST::BinaryOp::EqXor, AST::BinaryOp::Eq},
      [&] { return parse_conditional_expression(); });
}

auto Parser::parse_conditional_expression() -> unique_bump_ptr_wrapper<AST::Expr> {
  auto expr{parse_logical_or_expression()};
  if (!expr)
    return nullptr;
  skip();
  auto cursor{state};
  auto srcQuestion{AST::SourceRef()};
  if (delimiter('?', &srcQuestion)) {
    auto exprThen{parse_expression()};
    if (!exprThen)
      report_error("expected then clause in conditional expression");
    auto srcColon{AST::SourceRef()};
    auto exprElse{delimiter(':', &srcColon) ? parse_assignment_expression() : nullptr};
    if (!exprElse)
      report_error("expected else clause in conditional expression");
    expr = attach(
        bump_allocate<AST::Conditional>(std::move(expr), srcQuestion, std::move(exprThen), srcColon, std::move(exprElse)),
        cursor);
  }
  return expr;
}

auto Parser::parse_logical_or_expression() -> unique_bump_ptr_wrapper<AST::Expr> {
  return parse_binary_left_associative({AST::BinaryOp::LogicalOr}, [&] { return parse_logical_and_expression(); });
}

auto Parser::parse_logical_and_expression() -> unique_bump_ptr_wrapper<AST::Expr> {
  return parse_binary_left_associative({AST::BinaryOp::LogicalAnd}, [&] { return parse_inclusive_or_expression(); });
}

auto Parser::parse_inclusive_or_expression() -> unique_bump_ptr_wrapper<AST::Expr> {
  return parse_binary_left_associative({AST::BinaryOp::Or}, [&] { return parse_exclusive_or_expression(); });
}

auto Parser::parse_exclusive_or_expression() -> unique_bump_ptr_wrapper<AST::Expr> {
  return parse_binary_left_associative({AST::BinaryOp::Xor}, [&] { return parse_and_expression(); });
}

auto Parser::parse_and_expression() -> unique_bump_ptr_wrapper<AST::Expr> {
  return parse_binary_left_associative({AST::BinaryOp::And}, [&] { return parse_equality_expression(); });
}

auto Parser::parse_equality_expression() -> unique_bump_ptr_wrapper<AST::Expr> {
  return parse_binary_left_associative(
      {AST::BinaryOp::CmpEq, AST::BinaryOp::CmpNe}, [&] { return parse_relational_expression(); });
}

auto Parser::parse_relational_expression() -> unique_bump_ptr_wrapper<AST::Expr> {
  return parse_binary_left_associative(
      {AST::BinaryOp::Subset, //
       AST::BinaryOp::CmpLe, AST::BinaryOp::CmpGe, AST::BinaryOp::CmpLt, AST::BinaryOp::CmpGt},
      [&] { return parse_shift_expression(); });
}

auto Parser::parse_shift_expression() -> unique_bump_ptr_wrapper<AST::Expr> {
  return parse_binary_left_associative(
      {AST::BinaryOp::LShr, AST::BinaryOp::Shl, AST::BinaryOp::AShr}, [&] { return parse_additive_expression(); });
}

auto Parser::parse_additive_expression() -> unique_bump_ptr_wrapper<AST::Expr> {
  return parse_binary_left_associative(
      {AST::BinaryOp::Add, AST::BinaryOp::Sub}, [&] { return parse_multiplicative_expression(); });
}

auto Parser::parse_multiplicative_expression() -> unique_bump_ptr_wrapper<AST::Expr> {
  return parse_binary_left_associative(
      {AST::BinaryOp::Mul, AST::BinaryOp::Div, AST::BinaryOp::Rem}, [&] { return parse_unary_expression(); });
}

auto Parser::parse_unary_expression() -> unique_bump_ptr_wrapper<AST::Expr> {
  if (auto expr{parse_postfix_expression()})
    return expr;
  auto parse_prefix_expression{[&]() -> unique_bump_ptr_wrapper<AST::Expr> {
    checkpoint();
    auto cursor{state};
    auto srcOp{AST::SourceRef()};
    auto op{parse_unary_op(&srcOp)};
    if (!op) {
      reject();
      return nullptr;
    }
    auto expr{parse_unary_expression()};
    if (!expr) {
      reject();
      return nullptr;
    }
    accept();
    expr = attach(bump_allocate<AST::Unary>(srcOp, *op, std::move(expr)), cursor);
    return expr;
  }};
  if (auto expr{parse_prefix_expression()})
    return expr;
  if (auto expr{parse_let_expression()})
    return expr;
  if (isSmdlSyntax) {
    if (auto expr{parse_return_from_expression()})
      return expr;
  }
  return nullptr;
}

auto Parser::parse_postfix_expression() -> unique_bump_ptr_wrapper<AST::Expr> {
  auto expr{parse_primary_expression()};
  if (!expr)
    return nullptr;
  auto withPostfix{[&]() -> unique_bump_ptr_wrapper<AST::Expr> {
    auto cursor{save_cursor()};
    auto srcDot{AST::SourceRef()};
    if (delimiter('.', &srcDot)) {
      auto name{parse_simple_name()};
      if (!name)
        report_error("expected name after '.'", cursor);
      return attach(bump_allocate<AST::GetField>(std::move(expr), srcDot, *name), cursor);
    }
    auto srcOp{AST::SourceRef()};
    if (next("++", &srcOp))
      return attach(bump_allocate<AST::Unary>(srcOp, AST::UnaryOp::PostfixIncr, std::move(expr)), cursor);
    if (next("--", &srcOp))
      return attach(bump_allocate<AST::Unary>(srcOp, AST::UnaryOp::PostfixDecr, std::move(expr)), cursor);
    if (auto args{parse_argument_list()})
      return attach(bump_allocate<AST::Call>(std::move(expr), std::move(*args)), cursor);
    auto indexes{vector_or_SmallVector<AST::GetIndex::Index>{}};
    auto srcBrackL{AST::SourceRef()};
    auto srcBrackR{AST::SourceRef()};
    while (!get_remaining_text().starts_with("[[") && delimiter('[', &srcBrackL)) {
      auto index{AST::GetIndex::Index{}};
      auto srcAngleL{AST::SourceRef()};
      if (delimiter('<', &srcAngleL)) {
        auto name{parse_simple_name()};
        if (!name)
          report_error("expected name after '[<'", cursor);
        auto srcAngleR{AST::SourceRef()};
        if (!delimiter('>', &srcAngleR))
          report_error("expected '>]'", cursor);
        index.expr.reset(attach(bump_allocate<AST::SizeName>(srcAngleL, *name, srcAngleR), cursor));
      } else {
        index.expr = parse_expression(); // This may be null for `[]`
      }
      if (!delimiter(']', &srcBrackR))
        report_error("expected ']'", cursor);
      skip();
      index.srcBrackL = srcBrackL;
      index.srcBrackR = srcBrackR;
      indexes.push_back(std::move(index));
    }
    if (!indexes.empty())
      return bump_allocate<AST::GetIndex>(std::move(expr), std::move(indexes));
    return nullptr;
  }};
  while (true) {
    auto nextExpr{withPostfix()};
    if (!nextExpr)
      break;
    expr = std::move(nextExpr);
  }
  return expr;
}

auto Parser::parse_let_expression() -> unique_bump_ptr_wrapper<AST::Expr> {
  auto cursor{save_cursor()};
  auto srcKwLet{AST::SourceRef()};
  if (!next_word("let", &srcKwLet))
    return nullptr;
  auto vars{vector_or_SmallVector<unique_bump_ptr<AST::Variable>>{}};
  auto srcBraceL{AST::SourceRef{}};
  auto srcBraceR{AST::SourceRef{}};
  if (delimiter('{', &srcBraceL)) {
    while (true) {
      auto var{parse_variable_declaration()};
      if (!var)
        break;
      vars.push_back(std::move(var));
      skip();
      if (peek() == '}')
        break;
    }
    if (!delimiter('}', &srcBraceR))
      report_error("expected closing '}' after 'let'");
  } else {
    auto var{parse_variable_declaration()};
    if (!var)
      report_error("expected variable declaration after 'let'");
    vars.push_back(std::move(var));
  }
  auto srcKwIn{AST::SourceRef()};
  if (!next_word("in", &srcKwIn))
    report_error("expected 'in' after 'let ...'");
  auto expr{parse_conditional_expression()};
  if (!expr)
    report_error("expected expression after 'let ... in'");
  return attach(bump_allocate<AST::Let>(srcKwLet, srcBraceL, std::move(vars), srcBraceR, srcKwIn, std::move(expr)), cursor);
}

auto Parser::parse_return_from_expression() -> unique_bump_ptr_wrapper<AST::ReturnFrom> {
  auto cursor{save_cursor()};
  auto srcKwReturnFrom{AST::SourceRef()};
  if (!next_word("return_from", &srcKwReturnFrom))
    return nullptr;
  auto stmt{parse_compound_statement()};
  if (!stmt)
    report_error("expected compound statement after 'return_from'", cursor);
  return attach(bump_allocate<AST::ReturnFrom>(srcKwReturnFrom, std::move(stmt)), cursor);
}

auto Parser::parse_primary_expression() -> unique_bump_ptr_wrapper<AST::Expr> {
  auto cursor{save_cursor()};
  if (auto expr{parse_expression_in_parentheses()})
    return expr;
  if (auto expr{parse_literal_expression()})
    return expr;
  if (auto expr{parse_identifier()})
    return expr;
  auto srcKwCast{AST::SourceRef()};
  if (next_word("cast", &srcKwCast)) {
    auto srcAngleL{AST::SourceRef()};
    if (!delimiter('<', &srcAngleL))
      report_error("expected opening '<' after 'cast'");
    auto type{parse_type()};
    if (!type)
      report_error("expected type after 'cast'");
    auto srcAngleR{AST::SourceRef()};
    if (!delimiter('>', &srcAngleR))
      report_error("expected closing '>' after 'cast'");
    auto expr{parse_expression_in_parentheses()};
    if (!expr)
      report_error("expected parenthesized expression after 'cast<...>'");
    return attach(bump_allocate<AST::Cast>(srcKwCast, srcAngleL, std::move(type), srcAngleR, std::move(expr)), cursor);
  }
  return nullptr;
}

auto Parser::parse_literal_expression() -> unique_bump_ptr_wrapper<AST::Expr> {
  checkpoint();
  auto cursor{state};
  if (next_word("true")) {
    accept();
    return attach(bump_allocate<AST::LiteralBool>(source_since(cursor), true), cursor);
  }
  if (next_word("false")) {
    accept();
    return attach(bump_allocate<AST::LiteralBool>(source_since(cursor), false), cursor);
  }
  if (utf8::is_digit(peek())) { // LiteralInt or LiteralFloat
    auto parseDigitString{[&](auto &&is_digit) {
      llvm::SmallString<64> digits{};
      while (is_digit(peek())) {
        digits.push_back(peek());
        next();
        if (next('\'')) { // Maybe consume single-quote separator.
          if (peek() == '\'')
            report_error("numeric literal must not contain adjacent single-quote separators");
          if (!is_digit(peek()))
            report_error("numeric literal must not be terminated by single-quote separator");
        }
      }
      return digits;
    }};
    auto parseIntWithPrefix{[&](auto &&is_digit, int radix, const char *prefix, const char *info, std::string &digitsStr) {
      if (!is_digit(peek()))
        report_error(std::format("expected numeric literal prefix '{}' to be followed by {}", prefix, info));
      auto digits{parseDigitString(is_digit)};
      auto bits{llvm::APInt::getBitsNeeded(digits, radix)};
      if (bits > 64) // TODO Better warnings
        llvm::errs() << "Warning: integer literal exceeds 64-bits\n";
      digitsStr = prefix;
      digitsStr += std::string(digits);
      return llvm::APInt(bits, digits, radix);
    }};
    if (!get_remaining_text().starts_with("0.") && !get_remaining_text().starts_with("0e") &&
        !get_remaining_text().starts_with("0E") && next('0')) {
      llvm::APInt value{64, 0};
      std::string digits{};
      if (utf8::is_digit_8(peek())) {
        value = parseIntWithPrefix(utf8::is_digit_8, 8, "0", "[0-7]", digits);
      } else if (next('b') || next('B')) {
        value = parseIntWithPrefix(utf8::is_digit_2, 2, "0b", "[0-1]", digits);
      } else if (next('x') || next('X')) {
        value = parseIntWithPrefix(utf8::is_digit_16, 16, "0x", "[0-9a-fA-F]", digits);
      } else {
        digits = "0";
      }
      accept();
      return attach(bump_allocate<AST::LiteralInt>(source_since(cursor), value.getLimitedValue()), cursor);
    } else {
      bool isInt = true;
      std::string digits = std::string(parseDigitString(utf8::is_digit));
      if (next('.')) {
        digits += '.';
        digits += std::string(parseDigitString(utf8::is_digit));
        isInt = false;
      }
      if (next('e') || next('E')) {
        digits += 'e';
        if (next('+'))
          digits += '+';
        else if (next('-'))
          digits += '-';
        if (!utf8::is_digit(peek()))
          report_error("expected exponent after 'e' in floating point literal.");
        digits += std::string(parseDigitString(utf8::is_digit));
        isInt = false;
      }
      AST::Precision precision{AST::Precision::Default};
      if (next('d') || next('D')) {
        isInt = false;
        precision = AST::Precision::Double;
      } else if (next('f') || next('F')) {
        isInt = false;
        precision = AST::Precision::Single;
      }
      accept();
      if (isInt) {
        return attach(
            bump_allocate<AST::LiteralInt>(
                source_since(cursor), llvm::APInt(llvm::APInt::getBitsNeeded(digits, 10), digits, 10).getLimitedValue()),
            cursor);
      } else {
        llvm::APFloat value(llvm::APFloat::IEEEdouble());
        auto opStatus{value.convertFromString(digits, llvm::APFloat::rmNearestTiesToEven)};
        if (!opStatus)
          report_error(std::format("failed to parse floating point literal '{}'", digits.c_str()));
        return attach(bump_allocate<AST::LiteralFloat>(source_since(cursor), value.convertToDouble(), precision), cursor);
      }
    }
  }
  if (peek() == '"') { // LiteralString
    auto parseChar{[&] {
      Char c{next()};
      // Escaped character?
      if (c == '\\') {
        c = next();
        switch (c) {
        case '\'': c = 0x27UL; break; // single quote
        case '"': c = 0x22UL; break;  // double quote
        case 'a': c = 0x07UL; break;  // alert
        case 'b': c = 0x08UL; break;  // backspace
        case 'f': c = 0x0CUL; break;  // form feed
        case 'n': c = 0x0AUL; break;  // new line
        case 'r': c = 0x0DUL; break;  // carriage return
        case 't': c = 0x09UL; break;  // horizontal tab
        case 'v': c = 0x0BUL; break;  // vertical tab
        case '0':                     // octal
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
          c = utf8::oct_to_int(c);
          for (uint32_t i{}; i < 2; i++) {
            Char x{next()};
            if (!utf8::is_digit_8(x))
              report_error("expected 3 octal digits after '\\'");
            c = (c << 3) | utf8::oct_to_int(x);
          }
          break;
        case 'x': // hexadecimal
          c = 0;
          for (uint32_t i{}; i < 2; i++) {
            Char x{next()};
            if (!utf8::is_digit_16(x))
              report_error("expected 2 hexadecimal digits after '\\x'");
            c = (c << 4) | utf8::hex_to_int(x);
          }
          break;
        case 'u': // 16-bit codepoint
          c = 0;
          for (uint32_t i{}; i < 4; i++) {
            Char x{next()};
            if (!utf8::is_digit_16(x))
              report_error("expected 4 hexadecimal digits after '\\u'");
            c = (c << 4) | utf8::hex_to_int(x);
          }
          break;
        case 'U': // 32-bit codepoint
          c = 0;
          for (uint32_t i{}; i < 8; i++) {
            Char x{next()};
            if (!utf8::is_digit_16(x))
              report_error("expected 8 hexadecimal digits after '\\U'");
            c = (c << 4) | utf8::hex_to_int(x);
          }
          break;
        default: break;
        }
      }
      return c;
    }};
    vector_or_SmallVector<Char> str32{};
    while (delimiter('"')) {
      while (true) {
        if (is_eof())
          report_error("unexpected end-of-file in literal string");
        Char c = peek();
        if (c == '\n')
          report_error("unexpected end-of-line in literal string");
        if (c == '"')
          break;
        str32.push_back(parseChar());
      }
      if (!delimiter('"'))
        report_error("expected '\"' to close literal string");
    }
    std::string str{};
    if (!llvm::convertUTF32ToUTF8String(str32, str))
      report_error("utf-8 encoding failed!");
    accept();
    return attach(bump_allocate<AST::LiteralString>(source_since(cursor), llvm::SmallString<64>(str.c_str())), cursor);
  }
  if (isSmdlSyntax) {
    if (next('#')) {
      if (next_word().empty()) {
        reject();
        return nullptr;
      } else {
        accept();
        return attach(bump_allocate<AST::Intrinsic>(source_since(cursor)), cursor);
      }
    }
  }
  reject();
  return nullptr;
}

auto Parser::parse_binary_left_associative(
    llvm::ArrayRef<AST::BinaryOp> ops,
    const std::function<unique_bump_ptr_wrapper<AST::Expr>()> &parseTerm) -> unique_bump_ptr_wrapper<AST::Expr> {
  checkpoint();
  auto lhs{parseTerm()};
  if (!lhs) {
    reject();
    return nullptr;
  }
  while (true) {
    checkpoint();
    auto cursor{state};
    auto srcOp{AST::SourceRef()};
    auto op{parse_binary_op(ops, &srcOp)};
    if (!op) {
      reject();
      break;
    }
    skip();
    auto rhs{parseTerm()};
    if (!rhs) {
      reject();
      break;
    } else {
      accept();
      lhs = attach(bump_allocate<AST::Binary>(std::move(lhs), srcOp, *op, std::move(rhs)), cursor);
    }
  }
  accept();
  return lhs;
}

auto Parser::parse_binary_right_associative(
    llvm::ArrayRef<AST::BinaryOp> ops,
    const std::function<unique_bump_ptr_wrapper<AST::Expr>()> &parseTerm) -> unique_bump_ptr_wrapper<AST::Expr> {
  auto lhs{parseTerm()};
  if (!lhs)
    return nullptr;
  checkpoint();
  auto cursor{state};
  auto srcOp{AST::SourceRef()};
  auto op{parse_binary_op(ops, &srcOp)};
  if (!op) {
    reject();
    return lhs;
  }
  skip();
  auto rhs{parse_binary_right_associative(ops, parseTerm)};
  if (!rhs) {
    reject();
    return lhs;
  } else {
    accept();
    return attach(bump_allocate<AST::Binary>(std::move(lhs), srcOp, *op, std::move(rhs)), cursor);
  }
}

auto Parser::parse_unary_op(AST::SourceRef *src) -> std::optional<AST::UnaryOp> {
  using UnOp = AST::UnaryOp;
  for (auto &op : std::array{UnOp::Incr, UnOp::Decr, UnOp::Pos, UnOp::Neg, UnOp::Not, UnOp::LogicalNot})
    if (next(AST::to_string(op), src))
      return op;
  if (isSmdlSyntax) {
    if (next(AST::to_string(UnOp::Address), src))
      return UnOp::Address;
    if (next(AST::to_string(UnOp::Deref), src))
      return UnOp::Deref;
    if (next(AST::to_string(UnOp::Maybe), src))
      return UnOp::Maybe;
  }
  return std::nullopt;
}

auto Parser::parse_binary_op(llvm::ArrayRef<AST::BinaryOp> ops, AST::SourceRef *src) -> std::optional<AST::BinaryOp> {
  for (auto &op : ops) {
    // Don't process ":=" or "<:" unless in extended syntax mode.
    if ((op == AST::BinaryOp::Def || op == AST::BinaryOp::Subset) && !isSmdlSyntax)
      continue;
    // Don't mistake bit and for logical and.
    if (op == AST::BinaryOp::And && get_remaining_text().starts_with("&&"))
      continue;
    if (next(AST::to_string(op), src))
      return op;
  }
  return std::nullopt;
}
//--}

//--{ Parse: Stmts
auto Parser::parse_statement() -> unique_bump_ptr_wrapper<AST::Stmt> {
  auto cursor{save_cursor()};
  if (auto stmt{parse_compound_statement()})
    return stmt;
  if (auto stmt{parse_if_statement()})
    return stmt;
  if (auto stmt{parse_switch_statement()})
    return stmt;
  if (auto stmt{parse_while_statement()})
    return stmt;
  if (auto stmt{parse_do_statement()})
    return stmt;
  if (auto stmt{parse_for_statement()})
    return stmt;
  if (auto stmt{parse_break_statement()})
    return stmt;
  if (auto stmt{parse_continue_statement()})
    return stmt;
  if (auto stmt{parse_return_statement()})
    return stmt;
  if (isSmdlSyntax) {
    if (auto stmt{parse_unreachable_statement()})
      return stmt;
    if (auto stmt{parse_preserve_statement()})
      return stmt;
    if (auto stmt{parse_defer_statement()})
      return stmt;
    if (auto stmt{parse_visit_statement()})
      return stmt;
  }
  if (auto decl{parse_type_declaration()})
    return bump_allocate<AST::DeclStmt>(std::move(decl));
  if (auto decl{parse_variable_declaration()})
    return attach(bump_allocate<AST::DeclStmt>(std::move(decl)), cursor);
  if (delimiter(';'))
    return attach(bump_allocate<AST::ExprStmt>(), cursor);
  if (auto expr{parse_expression()}) {
    auto lateIf{parse_late_if()};
    auto srcSemicolon{AST::SourceRef()};
    if (!delimiter(';', &srcSemicolon))
      report_error("expected ';' after expression", cursor);
    return attach(bump_allocate<AST::ExprStmt>(std::move(expr), std::move(lateIf), srcSemicolon), cursor);
  }
  return nullptr;
}

auto Parser::parse_compound_statement() -> unique_bump_ptr_wrapper<AST::Compound> {
  auto cursor{save_cursor()};
  auto srcBraceL{AST::SourceRef()};
  if (!delimiter('{', &srcBraceL))
    return nullptr;
  auto stmts{vector_or_SmallVector<unique_bump_ptr<AST::Stmt>>{}};
  while (true) {
    auto stmt{parse_statement()};
    if (!stmt)
      break;
    stmts.push_back(std::move(stmt));
    skip();
    if (peek() == '}')
      break;
  }
  auto srcBraceR{AST::SourceRef()};
  if (!delimiter('}', &srcBraceR))
    report_error("expected '}' to close compound statement");
  return attach(bump_allocate<AST::Compound>(srcBraceL, std::move(stmts), srcBraceR), cursor);
}

auto Parser::parse_if_statement() -> unique_bump_ptr_wrapper<AST::If> {
  auto cursor{save_cursor()};
  auto srcKwIf{AST::SourceRef()};
  if (!next_word("if", &srcKwIf))
    return nullptr;
  auto cond{parse_expression_in_parentheses()};
  if (!cond)
    report_error("expected parenthesized condition after 'if'", cursor);
  auto ifPass{parse_statement()};
  if (!ifPass)
    report_error("expected statement after 'if (...)'", cursor);
  auto srcKwElse{AST::SourceRef()};
  if (auto cursorElse{save_cursor()}; next_word("else", &srcKwElse)) {
    auto ifFail{parse_statement()};
    if (!ifFail)
      report_error("expected statement after 'else'", cursorElse);
    return attach(bump_allocate<AST::If>(srcKwIf, std::move(cond), std::move(ifPass), srcKwElse, std::move(ifFail)), cursor);
  } else {
    return attach(bump_allocate<AST::If>(srcKwIf, std::move(cond), std::move(ifPass), srcKwElse, nullptr), cursor);
  }
}

auto Parser::parse_switch_statement() -> unique_bump_ptr_wrapper<AST::Switch> {
  auto cursor{save_cursor()};
  auto srcKwSwitch{AST::SourceRef()};
  if (!next_word("switch", &srcKwSwitch))
    return nullptr;
  auto expr{parse_expression_in_parentheses()};
  if (!expr)
    report_error("expected parenthesized expression after 'switch'", cursor);
  auto srcBraceL{AST::SourceRef()};
  if (!delimiter('{', &srcBraceL))
    report_error("expected opening '{' after 'switch'", cursor);
  auto switchCases{vector_or_SmallVector<AST::Switch::Case>{}};
  while (true) {
    auto switchCase{parse_switch_case()};
    if (!switchCase)
      break;
    switchCases.push_back(std::move(*switchCase));
    skip();
    if (peek() == '}')
      break;
  }
  auto srcBraceR{AST::SourceRef()};
  if (!delimiter('}', &srcBraceR))
    report_error("expected closing '}' after 'switch'", cursor);
  return attach(bump_allocate<AST::Switch>(srcKwSwitch, std::move(expr), srcBraceL, std::move(switchCases), srcBraceR), cursor);
}

auto Parser::parse_switch_case() -> std::optional<AST::Switch::Case> {
  auto cursor{save_cursor()};
  auto switchCase{AST::Switch::Case{}};
  if (next_word("case", &switchCase.srcKwCaseOrDefault)) {
    auto cond{parse_expression()};
    if (!cond)
      report_error("expected expression after 'case'", cursor);
    if (!delimiter(':', &switchCase.srcColon))
      report_error("expected ':' after 'case ...'", cursor);
    switchCase.cond = std::move(cond);
  } else if (next_word("default", &switchCase.srcKwCaseOrDefault)) {
    if (!delimiter(':', &switchCase.srcColon))
      report_error("expected ':' after 'default'", cursor);
  } else {
    return std::nullopt;
  }
  while (true) {
    auto stmt{parse_statement()};
    if (!stmt)
      break;
    switchCase.stmts.push_back(std::move(stmt));
    skip();
  }
  return std::move(switchCase);
}

auto Parser::parse_while_statement() -> unique_bump_ptr_wrapper<AST::While> {
  auto cursor{save_cursor()};
  auto srcKwWhile{AST::SourceRef()};
  if (!next_word("while", &srcKwWhile))
    return nullptr;
  auto expr{parse_expression_in_parentheses()};
  if (!expr)
    report_error("expected parenthesized expression after 'while'");
  auto stmt{parse_statement()};
  if (!stmt)
    report_error("expected statement after 'while (...)'");
  return attach(bump_allocate<AST::While>(srcKwWhile, std::move(expr), std::move(stmt)), cursor);
}

auto Parser::parse_do_statement() -> unique_bump_ptr_wrapper<AST::DoWhile> {
  auto cursor{save_cursor()};
  auto srcKwDo{AST::SourceRef()};
  if (!next_word("do", &srcKwDo))
    return nullptr;
  auto stmt{parse_statement()};
  if (!stmt)
    report_error("expected statement after 'do'", cursor);
  auto srcKwWhile{AST::SourceRef()};
  if (!next_word("while", &srcKwWhile))
    report_error("expected 'while' after 'do ...'", cursor);
  auto expr{parse_expression_in_parentheses()};
  if (!expr)
    report_error("expected parenthesized expression after 'do ... while'", cursor);
  auto srcSemicolon{AST::SourceRef()};
  if (!delimiter(';', &srcSemicolon))
    report_error("expected ';' after 'do ... while (...)'", cursor);
  return attach(bump_allocate<AST::DoWhile>(srcKwDo, std::move(stmt), srcKwWhile, std::move(expr), srcSemicolon), cursor);
}

auto Parser::parse_for_statement() -> unique_bump_ptr_wrapper<AST::For> {
  auto cursor{save_cursor()};
  auto srcKwFor{AST::SourceRef()};
  if (!next_word("for", &srcKwFor))
    return nullptr;
  auto srcParenL{AST::SourceRef()};
  if (!delimiter('(', &srcParenL))
    report_error("expected '(' after 'for'", cursor);
  auto init{unique_bump_ptr_wrapper<AST::Stmt>{}};
  if (auto decl{parse_variable_declaration()}) {
    auto srcLoc{decl->srcLoc};
    init = bump_allocate<AST::DeclStmt>(std::move(decl));
    init->srcLoc = srcLoc;
  } else if (auto expr{parse_expression()}) {
    auto srcSemicolon{AST::SourceRef()};
    if (!delimiter(';', &srcSemicolon))
      report_error("expected ';' after expression", cursor);
    auto srcLoc{expr->srcLoc};
    init = bump_allocate<AST::ExprStmt>(std::move(expr), std::nullopt, srcSemicolon);
    init->srcLoc = srcLoc;
  } else {
    report_error("expected variable declaration or expression after 'for'", cursor);
  }
  auto cond{parse_expression()};
  auto srcSemicolonAfterCond{AST::SourceRef()};
  if (!delimiter(';', &srcSemicolonAfterCond))
    report_error("expected ';' after expression", cursor);
  auto incr{parse_expression()};
  auto srcParenR{AST::SourceRef()};
  if (!delimiter(')', &srcParenR))
    report_error("expected closing ')' after 'for'", cursor);
  auto body{parse_statement()};
  if (!body)
    report_error("expected statement after 'for (...)'", cursor);
  return attach(
      bump_allocate<AST::For>(
          srcKwFor, srcParenL, std::move(init), std::move(cond), srcSemicolonAfterCond, std::move(incr), srcParenR,
          std::move(body)),
      cursor);
}

auto Parser::parse_break_statement() -> unique_bump_ptr_wrapper<AST::Break> {
  auto cursor{save_cursor()};
  auto srcKwBreak{AST::SourceRef()};
  if (!next_word("break", &srcKwBreak))
    return nullptr;
  auto lateIf{parse_late_if()};
  auto srcSemicolon{AST::SourceRef()};
  if (!delimiter(';', &srcSemicolon))
    report_error("expected ';' after 'break'", cursor);
  return attach(bump_allocate<AST::Break>(srcKwBreak, std::move(lateIf), srcSemicolon), cursor);
}

auto Parser::parse_continue_statement() -> unique_bump_ptr_wrapper<AST::Continue> {
  auto cursor{save_cursor()};
  auto srcKwContinue{AST::SourceRef()};
  if (!next_word("continue", &srcKwContinue))
    return nullptr;
  auto lateIf{parse_late_if()};
  auto srcSemicolon{AST::SourceRef()};
  if (!delimiter(';', &srcSemicolon))
    report_error("expected ';' after 'continue'", cursor);
  return attach(bump_allocate<AST::Continue>(srcKwContinue, std::move(lateIf), srcSemicolon), cursor);
}

auto Parser::parse_return_statement() -> unique_bump_ptr_wrapper<AST::Return> {
  auto cursor{save_cursor()};
  auto srcKwReturn{AST::SourceRef()};
  if (!next_word("return", &srcKwReturn))
    return nullptr;
  auto expr{parse_expression()};
  if (!expr && !isSmdlSyntax)
    report_error("expected expression after 'return'", cursor);
  auto lateIf{parse_late_if()};
  auto srcSemicolon{AST::SourceRef()};
  if (!delimiter(';', &srcSemicolon))
    report_error("expected ';' after 'return ...'", cursor);
  return attach(bump_allocate<AST::Return>(srcKwReturn, std::move(expr), std::move(lateIf), srcSemicolon), cursor);
}

auto Parser::parse_unreachable_statement() -> unique_bump_ptr_wrapper<AST::Unreachable> {
  auto cursor{save_cursor()};
  auto srcKwUnreachable{AST::SourceRef()};
  if (!next_word("unreachable", &srcKwUnreachable))
    return nullptr;
  auto srcSemicolon{AST::SourceRef()};
  if (!delimiter(';', &srcSemicolon))
    report_error("expected ';' after 'unreachable'", cursor);
  return attach(bump_allocate<AST::Unreachable>(srcKwUnreachable, srcSemicolon), cursor);
}

auto Parser::parse_preserve_statement() -> unique_bump_ptr_wrapper<AST::Preserve> {
  auto cursor{save_cursor()};
  auto srcKwPreserve{AST::SourceRef()};
  if (!next_word("preserve", &srcKwPreserve))
    return nullptr;
  auto preservees{vector_or_SmallVector<AST::Preserve::Preservee>{}};
  while (true) {
    auto expr{parse_unary_expression()};
    if (!expr)
      break;
    preservees.push_back(AST::Preserve::Preservee{std::move(expr), {}});
    if (!delimiter(',', &preservees.back().srcComma))
      break;
  }
  auto srcSemicolon{AST::SourceRef()};
  if (!delimiter(';', &srcSemicolon))
    report_error("expected ';' after 'preserve ...'", cursor);
  return attach(bump_allocate<AST::Preserve>(srcKwPreserve, std::move(preservees), srcSemicolon), cursor);
}

auto Parser::parse_defer_statement() -> unique_bump_ptr_wrapper<AST::Defer> {
  auto cursor{save_cursor()};
  auto srcKwDefer{AST::SourceRef()};
  if (!next_word("defer", &srcKwDefer))
    return nullptr;
  auto stmt{parse_statement()};
  if (!stmt)
    report_error("expected statement after 'defer'", cursor);
  return attach(bump_allocate<AST::Defer>(srcKwDefer, std::move(stmt)), cursor);
}

auto Parser::parse_visit_statement() -> unique_bump_ptr_wrapper<AST::Visit> {
  auto cursor{save_cursor()};
  auto srcKwVisit{AST::SourceRef()};
  if (!next_word("visit", &srcKwVisit))
    return nullptr;
  auto name{parse_simple_name()};
  if (!name)
    report_error("expected simple name after 'visit'", cursor);
  auto srcKwIn{AST::SourceRef()};
  if (!next_word("in", &srcKwIn))
    report_error("expected 'in' after 'visit ...'", cursor);
  auto expr{parse_expression()};
  if (!expr)
    report_error("expected expression after 'visit ... in'", cursor);
  auto body{parse_compound_statement()};
  if (!body)
    report_error("expected compound statement after 'visit ... in ...'", cursor);
  return attach(bump_allocate<AST::Visit>(srcKwVisit, *name, srcKwIn, std::move(expr), std::move(body)), cursor);
}

auto Parser::parse_late_if() -> std::optional<AST::LateIf> {
  auto srcKwIf{AST::SourceRef()};
  if (!isSmdlSyntax || !next_word("if", &srcKwIf))
    return std::nullopt;
  auto cond{parse_expression_in_parentheses()};
  if (!cond)
    report_error("expected expression in parentheses after '... if'");
  return AST::LateIf(srcKwIf, std::move(cond));
}
//--}

} // namespace smdl
