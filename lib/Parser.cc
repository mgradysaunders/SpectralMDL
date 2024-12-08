// vim:foldmethod=marker:foldlevel=0:fmr=--{,--}
#include "Parser.h"

namespace smdl {

//--{ Basics
Parser::Char Parser::peek(size_t *numBytes) const {
  auto result{utf8::decode_next(get_remaining_text(), numBytes)};
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
    auto numBytes{size_t(0)};
    auto ch{peek(&numBytes)};
    if (ch == '\n') {
      state.lineNo++;
      state.charNo = 1;
    } else
      state.charNo++;
    state.i += numBytes;
    return ch;
  } else {
    return 0;
  }
}

bool Parser::next(llvm::StringRef str) {
  checkpoint();
  while (!str.empty()) {
    auto numBytes{size_t(0)};
    auto ch{utf8::decode_next(str, &numBytes)};
    if (!ch)
      report_error("utf-8 decoding failed");
    if (!next(*ch))
      break;
    else
      str = str.substr(numBytes);
  }
  return str.empty() ? accept() : reject();
}

bool Parser::next_word(llvm::StringRef str) {
  checkpoint();
  if (!next(str))
    return reject();
  if (utf8::is_word(peek())) {
    return reject(); // Make sure the keyword doesn't continue to form a name!
  } else {
    return accept();
  }
}

llvm::StringRef Parser::next_word() {
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

llvm::StringRef Parser::next_int() {
  auto i{get_position()};
  while (utf8::is_digit(peek()))
    next();
  return text.substr(i, get_position() - i);
}

//--}

//--{ Parse: Decls
auto Parser::parse_mdl() -> unique_bump_ptr_wrapper<AST::File> {
  skip();
  isSmdlSyntax = next("#smdl_syntax");
  skip();
  auto version{parse_mdl_version()};
  if (!version) {
    if (!isSmdlSyntax)
      report_error("expected MDL version");
    else
      version = AST::Version::builtin_version();
  }
  auto imports{llvm::SmallVector<unique_bump_ptr<AST::Decl>>{}};
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
  auto annotations{std::optional<AST::AnnotationBlock>{}};
  skip();
  if (next_word("module")) {
    annotations = parse_annotation_block();
    if (!annotations)
      report_error("expected annotation block after 'module'");
    if (!delimiter(';'))
      report_error("expected ';' after 'module [[ ... ]]'");
  }
  auto globals{llvm::SmallVector<unique_bump_ptr<AST::Decl>>{}};
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
  return bump_allocate<AST::File>(isSmdlSyntax, *version, std::move(imports), std::move(annotations), std::move(globals));
}

auto Parser::parse_mdl_version() -> std::optional<AST::Version> {
  auto cursor{save_cursor()};
  if (!next_word("mdl"))
    return std::nullopt;
  skip();
  auto major{next_int()};
  if (major.empty() || !next('.'))
    report_error("expected 'MAJOR.MINOR' version after 'mdl'", cursor);
  auto minor{next_int()};
  if (minor.empty())
    report_error("expected 'MAJOR.MINOR' version after 'mdl'", cursor);
  AST::Version version{};
  version.major = llvm::APInt(32, major, 10).getLimitedValue();
  version.minor = llvm::APInt(32, minor, 10).getLimitedValue();
  if (!delimiter(';'))
    report_error("expected ';' after 'mdl MAJOR.MINOR'", cursor);
  return version;
}

auto Parser::parse_unicode_name() -> unique_bump_ptr_wrapper<AST::Name> {
  checkpoint();
  auto cursor{save_cursor()};
  if (auto name{parse_simple_name()}) {
    accept();
    return std::move(name);
  } else if (auto expr{parse_literal_expression()}; expr && llvm::isa<AST::LiteralString>(expr.get())) {
    accept();
    auto &value{static_cast<AST::LiteralString *>(expr.get())->value};
    auto valuePtr{static_cast<char *>(bumpAllocator.Allocate(value.size(), 1))};
    std::copy(value.begin(), value.end(), valuePtr);
    return attach(bump_allocate<AST::Name>(llvm::StringRef(valuePtr, value.size())), cursor);
  } else {
    reject();
    return nullptr;
  }
}

auto Parser::parse_import_path(bool isUnicode) -> unique_bump_ptr_wrapper<AST::Identifier> {
  checkpoint();
  auto cursor{save_cursor()};
  auto names{llvm::SmallVector<unique_bump_ptr<AST::Name>>{}};
  bool isAbs{next("::")};
  while (true) {
    auto parse_any_name{[&]() -> unique_bump_ptr_wrapper<AST::Name> {
      auto cursor{save_cursor()};
      if (isUnicode) {
        if (auto name{parse_unicode_name()})
          return std::move(name);
      } else {
        if (auto name{parse_simple_name()})
          return std::move(name);
        if (next("*"))
          return attach(bump_allocate<AST::Name>("*"), cursor);
      }
      if (next(".."))
        return attach(bump_allocate<AST::Name>(".."), cursor);
      if (next('.'))
        return attach(bump_allocate<AST::Name>("."), cursor);
      return nullptr;
    }};
    auto name{parse_any_name()};
    if (!name)
      break;
    names.push_back(std::move(name));
    skip();
    if (names.back()->name == "*" || !next("::"))
      break;
  }
  if (!isAbs && names.empty()) {
    reject();
    return nullptr;
  }
  accept();
  return attach(bump_allocate<AST::Identifier>(std::move(names), isAbs), cursor);
}

auto Parser::parse_using_alias() -> unique_bump_ptr_wrapper<AST::UsingAlias> {
  checkpoint();
  auto cursor{save_cursor()};
  if (!next_word("using")) {
    reject();
    return nullptr;
  }
  skip();
  auto name{parse_simple_name()};
  if (!name || !delimiter('=')) {
    reject();
    return nullptr;
  }
  auto path{parse_import_path(/*isUnicode=*/true)};
  if (!path)
    report_error("expected import path after 'using ... ='", cursor);
  if (!delimiter(';'))
    report_error("expected ';' after 'using ... = ...'", cursor);
  accept();
  return attach(bump_allocate<AST::UsingAlias>(std::move(name), std::move(path)), cursor);
}

auto Parser::parse_using_import() -> unique_bump_ptr_wrapper<AST::UsingImport> {
  checkpoint();
  auto cursor{save_cursor()};
  bool isExport{next_word("export")};
  skip();
  if (!next_word("using")) {
    reject();
    return nullptr;
  }
  auto path{parse_import_path(/*isUnicode=*/false)};
  if (!path) {
    reject();
    return nullptr;
  }
  if (path->names.back()->name == "*")
    report_error("import path after '[export] using' must not end with '::*'");
  skip();
  if (!next_word("import"))
    report_error("expected 'import' after '[export] using ...'", cursor);
  skip();
  auto names{llvm::SmallVector<unique_bump_ptr<AST::Name>>{}};
  if (!next('*')) {
    while (true) {
      skip();
      auto name{parse_simple_name()};
      if (!name)
        report_error("expected import name", cursor);
      names.push_back(std::move(name));
      if (!delimiter(','))
        break;
    }
  }
  if (!delimiter(';'))
    report_error("expected ';' after '[export] using ... import ...'", cursor);
  accept();
  auto decl{bump_allocate<AST::UsingImport>(std::move(path), std::move(names))};
  decl->isExport = isExport;
  return attach(decl, cursor);
}

auto Parser::parse_import() -> unique_bump_ptr_wrapper<AST::Import> {
  auto cursor{save_cursor()};
  if (!next_word("import"))
    return nullptr;
  auto paths{llvm::SmallVector<unique_bump_ptr<AST::Identifier>>{}};
  while (true) {
    auto path{parse_import_path(/*isUnicode=*/false)};
    if (!path)
      report_error("expected import path", cursor);
    paths.push_back(std::move(path));
    if (!delimiter(','))
      break;
  }
  if (!delimiter(';'))
    report_error("expected ';' after 'import ...'", cursor);
  return attach(bump_allocate<AST::Import>(std::move(paths)), cursor);
}

auto Parser::parse_global_declaration() -> unique_bump_ptr_wrapper<AST::Decl> {
  checkpoint();
  bool isExport{next_word("export")};
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
  if (!next_word("typedef"))
    return nullptr;
  auto type{parse_type()};
  if (!type)
    report_error("expected type after 'typedef'", cursor);
  auto name{parse_simple_name()};
  if (!name)
    report_error("expected name after 'typedef ...'", cursor);
  if (!delimiter(';'))
    report_error("expected ';' after 'typedef ... NAME'", cursor);
  return attach(bump_allocate<AST::Typedef>(std::move(type), std::move(name)), cursor);
}

auto Parser::parse_struct_type_declaration() -> unique_bump_ptr_wrapper<AST::Struct> {
  checkpoint();
  auto cursor{save_cursor()};
  if (!next_word("struct")) {
    reject();
    return nullptr;
  }
  auto name{parse_simple_name()};
  if (!name)
    report_error("expected name after 'struct'", cursor);
  auto annotations{parse_annotation_block()};
  auto tags{llvm::SmallVector<AST::Struct::Tag>{}};
  if (delimiter(':')) {
    while (true) {
      bool isDefault{next_word("default")};
      auto tagName{parse_identifier()};
      if (!tagName)
        break;
      auto &tag{tags.emplace_back()};
      tag.isDefault = isDefault;
      tag.type.reset(bump_allocate<AST::Type>(std::nullopt, AST::Type::Attrs{}, std::move(tagName)));
      skip();
      if (!delimiter(','))
        break;
    }
  }
  if (!delimiter('{'))
    report_error("expected '{' after 'struct NAME'", cursor);
  auto fields{llvm::SmallVector<AST::Struct::Field>{}};
  while (true) {
    auto field{parse_struct_field_declarator()};
    if (!field)
      break;
    fields.push_back(std::move(*field));
    skip();
    if (peek() == '}')
      break;
  }
  if (!delimiter('}'))
    report_error("expected '}' after 'struct NAME { ...'", cursor);
  if (!delimiter(';'))
    report_error("expected ';' after 'struct NAME { ... }'", cursor);
  accept();
  return attach(
      bump_allocate<AST::Struct>(std::move(name), std::move(annotations), std::move(fields), std::move(tags)), cursor);
}

auto Parser::parse_struct_field_declarator() -> std::optional<AST::Struct::Field> {
  checkpoint();
  auto cursor{save_cursor()};
  bool isVoid{isSmdlSyntax && next_word("void")};
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
  field.type = std::move(type);
  field.name = std::move(name);
  if (delimiter('=')) {
    auto init{parse_expression()};
    if (!init)
      report_error("expected initializer after '='", cursor);
    field.init = std::move(init);
  }
  field.annotations = parse_annotation_block();
  if (!delimiter(';'))
    report_error("expected ';' after field", cursor);
  accept();
  return std::move(field);
}

auto Parser::parse_enum_type_declaration() -> unique_bump_ptr_wrapper<AST::Enum> {
  auto cursor{save_cursor()};
  if (!next_word("enum"))
    return nullptr;
  auto name{parse_simple_name()};
  if (!name)
    report_error("expected name after 'enum'", cursor);
  auto annotations{parse_annotation_block()};
  if (!delimiter('{'))
    report_error("expected '{' after 'enum NAME'", cursor);
  auto declarators{llvm::SmallVector<AST::Enum::Declarator>{}};
  while (true) {
    auto declarator{parse_enum_value_declarator()};
    if (!declarator)
      break;
    declarators.push_back(std::move(*declarator));
    if (!delimiter(','))
      break;
  }
  if (!delimiter('}'))
    report_error("expected '}' after 'enum NAME { ...'", cursor);
  if (!delimiter(';'))
    report_error("expected ';' after 'enum NAME { ... }'", cursor);
  return attach(bump_allocate<AST::Enum>(std::move(name), std::move(annotations), std::move(declarators)), cursor);
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
  declarator.name = std::move(name);
  if (delimiter('=')) {
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
  auto declarators{llvm::SmallVector<AST::Variable::Declarator, 1>{}};
  while (true) {
    auto declarator{parse_variable_declarator()};
    if (!declarator)
      break;
    declarators.push_back(std::move(*declarator));
    if (!delimiter(','))
      break;
  }
  if (declarators.empty()) {
    reject();
    return nullptr;
  }
  if (!delimiter(';'))
    report_error("expected ';' after variable declaration", cursor);
  accept();
  return attach(bump_allocate<AST::Variable>(std::move(type), std::move(declarators)), cursor);
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
  declarator.name = std::move(name);
  if (delimiter('=')) {
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
  auto attrs{AST::Function::Attrs{}};
  if (isSmdlSyntax && delimiter('@')) {
    if (!delimiter('('))
      report_error("expected '@(...)' syntax for function attributes", cursor);
    while (true) {
      checkpoint();
      if (next_word("visible")) {
        accept(), attrs.isVisible = true;
      } else if (next_word("foreign")) {
        accept(), attrs.isForeign = true;
      } else if (next_word("pure")) {
        accept(), attrs.isPure = true;
      } else if (next_word("macro")) {
        accept(), attrs.isMacro = true;
      } else if (next_word("noinline")) {
        accept(), attrs.isNoInline = true;
      } else if (next_word("alwaysinline")) {
        accept(), attrs.isAlwaysInline = true;
      } else if (next_word("hot")) {
        accept(), attrs.isHot = true;
      } else if (next_word("cold")) {
        accept(), attrs.isCold = true;
      } else if (next_word("optsize")) {
        accept(), attrs.isOptSize = true;
      } else if (next_word("optnone")) {
        accept(), attrs.isOptNone = true;
      } else {
        skip();
        if (peek() != ')')
          report_error("unrecognized function attribute", cursor);
        reject();
        break;
      }
    }
    if (!delimiter(')'))
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
  bool is_variant{};
  auto params{parse_parameter_list()};
  if (!params) {
    if (delimiter('(') && delimiter('*') && delimiter(')')) {
      is_variant = true;
      params = AST::ParamList();
    } else {
      reject();
      return nullptr;
    }
  }
  auto frequency{std::optional<AST::FrequencyQualifier>{}};
  if (next_word("uniform"))
    frequency = AST::FrequencyQualifier::Uniform;
  else if (next_word("varying"))
    frequency = AST::FrequencyQualifier::Varying;
  auto lateAnnotations{parse_annotation_block()};
  auto definition{unique_bump_ptr<AST::Node>{}};
  skip();
  if (is_variant && peek() != '=')
    report_error("function variant must be defined by 'let' or call expression", cursor);
  if (delimiter(';')) {
    // Nothing
  } else if (delimiter('=')) {
    auto def{parse_expression()};
    if (!def)
      report_error("expected function expression after '='", cursor);
    if (!delimiter(';'))
      report_error("expected ';' after function expression", cursor);
    if (is_variant && !llvm::isa<AST::Let>(def.get()) && !llvm::isa<AST::Call>(def.get()))
      report_error("function variant must be defined by 'let' or call expression", cursor);
    definition.reset(bump_allocate<AST::Return>(std::move(def)));
  } else {
    auto def{parse_compound_statement()};
    if (!def)
      report_error("expected ';' or function definition", cursor);
    definition = std::move(def);
  }
  accept();
  return attach(
      bump_allocate<AST::Function>(
          attrs, is_variant, std::move(type), std::move(earlyAnnotations), std::move(name), std::move(*params), frequency,
          std::move(lateAnnotations), std::move(definition)),
      cursor);
}

auto Parser::parse_tag_declaration() -> unique_bump_ptr_wrapper<AST::Tag> {
  auto cursor{save_cursor()};
  if (!next_word("tag"))
    return nullptr;
  auto name{parse_simple_name()};
  if (!name)
    report_error("expected name after 'tag'", cursor);
  if (!delimiter(';'))
    report_error("expected ';' after 'tag ...'", cursor);
  return attach(bump_allocate<AST::Tag>(std::move(name)), cursor);
}

auto Parser::parse_unit_test_declaration() -> unique_bump_ptr_wrapper<AST::UnitTest> {
  auto cursor{save_cursor()};
  if (!next_word("unit_test"))
    return nullptr;
  auto desc{parse_literal_expression()};
  if (!desc || !llvm::isa<AST::LiteralString>(desc.get()))
    report_error("expected literal string after 'unit_test'", cursor);
  auto body{parse_compound_statement()};
  if (!body)
    report_error("expected compound statement after 'unit_test ...'", cursor);
  return attach(
      bump_allocate<AST::UnitTest>(std::move(static_cast<AST::LiteralString *>(desc.get())->value), std::move(body)), cursor);
}
//--}

//--{ Parse: Exprs
auto Parser::parse_simple_name() -> unique_bump_ptr_wrapper<AST::Name> {
  checkpoint();
  auto cursor{state};
  if (auto name{next_word()}; !name.empty()) {
    static const llvm::StringRef reservedKeywords[]{
        "break",  "case",   "cast", "const",   "continue", "default", "do",      "else",    "enum",
        "export", "false",  "for",  "if",      "import",   "let",     "module",  "package", "return",
        "struct", "switch", "true", "typedef", "uniform",  "using",   "varying", "while",
    };
    for (const auto &reservedKeyword : reservedKeywords) {
      if (name == reservedKeyword) {
        reject();
        return nullptr;
      }
    }
    if (isSmdlSyntax) {
      static const llvm::StringRef reservedKeywordsExtendedSyntax[]{
          "defer", "inline", "return_from", "static", "tag", "unreachable", "visit",
      };
      for (const auto &reservedKeyword : reservedKeywordsExtendedSyntax) {
        if (name == reservedKeyword) {
          reject();
          return nullptr;
        }
      }
    }
    accept();
    return attach(bump_allocate<AST::Name>(name), cursor);
  } else {
    reject();
    return nullptr;
  }
}

auto Parser::parse_identifier() -> unique_bump_ptr_wrapper<AST::Identifier> {
  checkpoint();
  auto cursor{state};
  auto pos{get_position()};
  auto names{llvm::SmallVector<unique_bump_ptr<AST::Name>>{}};
  auto isAbs{next("::")};
  if (auto name{parse_simple_name()}) {
    names.push_back(std::move(name));
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
    if (next("::")) {
      if (auto name{parse_simple_name()}) {
        names.push_back(std::move(name));
        accept();
        continue;
      }
    }
    reject();
    break;
  }
  if (pos != get_position()) {
    accept();
    return attach(bump_allocate<AST::Identifier>(std::move(names), isAbs), cursor);
  } else {
    reject();
    return nullptr;
  }
}

auto Parser::parse_type() -> unique_bump_ptr_wrapper<AST::Type> {
  checkpoint();
  auto cursor{save_cursor()};
  auto frequency{std::optional<AST::FrequencyQualifier>{}};
  if (next_word("uniform")) {
    frequency = AST::FrequencyQualifier::Uniform;
  } else if (next_word("varying")) {
    frequency = AST::FrequencyQualifier::Varying;
  }
  auto attrs{AST::Type::Attrs{}};
  while (true) {
    checkpoint();
    if (next_word("const")) {
      accept(), attrs.isConst = true;
    } else if (isSmdlSyntax && next_word("static")) {
      accept(), attrs.isStatic = true;
    } else if (isSmdlSyntax && next_word("inline")) {
      accept(), attrs.isInline = true;
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
  auto annotations{parse_annotation_block()}; // NOTE: Non-standard!
  return attach(bump_allocate<AST::Type>(frequency, attrs, std::move(expr), std::move(annotations)), cursor);
}

auto Parser::parse_parameter() -> std::optional<AST::Param> {
  checkpoint();
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
  param.name = std::move(name);
  if (delimiter('=')) {
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
  if (!delimiter('(')) {
    reject();
    return std::nullopt;
  }
  auto params{AST::ParamList{}};
  while (true) {
    auto param{parse_parameter()};
    if (!param)
      break;
    params.params.push_back(std::move(*param));
    if (!delimiter(','))
      break;
    skip();
    if (next("...")) {
      params.isVarArg = true;
      break;
    }
  }
  if (!delimiter(')')) {
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
  bool isVisit{isSmdlSyntax && next_word("visit")};
  auto parse_argument_name{[&]() -> unique_bump_ptr_wrapper<AST::Name> {
    checkpoint();
    if (auto name{parse_simple_name()}; name && delimiter(':') && peek() != ':' && peek() != '=') {
      accept();
      return std::move(name);
    } else {
      reject();
      return nullptr;
    }
  }};
  auto name{parse_argument_name()};
  auto expr{parse_assignment_expression()};
  if (!expr) {
    reject();
    return std::nullopt;
  }
  auto arg{AST::Arg{}};
  arg.name = std::move(name);
  arg.expr = std::move(expr);
  arg.srcLoc.file = file;
  arg.srcLoc.line = cursor.lineNo;
  arg.src = source_since(cursor);
  arg.isVisit = isVisit;
  accept();
  return std::move(arg);
}

auto Parser::parse_argument_list() -> std::optional<AST::ArgList> {
  checkpoint();
  auto cursor{save_cursor()};
  if (!delimiter('(')) {
    reject();
    return std::nullopt;
  }
  auto args{AST::ArgList{}};
  while (true) {
    auto arg{parse_argument()};
    if (!arg)
      break;
    args.args.push_back(std::move(*arg));
    if (!delimiter(','))
      break;
  }
  if (!delimiter(')')) {
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
  return AST::Annotation(std::move(identifier), std::move(*args));
}

auto Parser::parse_annotation_block() -> std::optional<AST::AnnotationBlock> {
  skip();
  if (!next("[["))
    return std::nullopt;
  AST::AnnotationBlock annotations{};
  while (true) {
    auto annotation{parse_annotation()};
    if (!annotation)
      break;
    annotations.push_back(std::move(*annotation));
    if (!delimiter(','))
      break;
    skip();
    if (get_remaining_text().starts_with("]]"))
      break;
  }
  skip();
  if (!next("]]"))
    report_error("expected ']]' to close annotation block");
  return std::move(annotations);
}

auto Parser::parse_expression() -> unique_bump_ptr_wrapper<AST::Expr> {
  return parse_binary_left_associative({AST::BinaryOp::Comma}, [&] { return parse_assignment_expression(); });
}

auto Parser::parse_expression_in_parentheses() -> unique_bump_ptr_wrapper<AST::Expr> {
  checkpoint();
  auto cursor{save_cursor()};
  auto forceCompileTime{isSmdlSyntax && next('$')};
  if (!delimiter('(')) {
    reject();
    return nullptr;
  }
  auto expr{parse_expression()};
  if (!expr) {
    reject();
    return nullptr;
  }
  if (!delimiter(')')) {
    reject();
    return nullptr;
  }
  accept();
  return attach(bump_allocate<AST::Parens>(std::move(expr), forceCompileTime), cursor);
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
  if (delimiter('?')) {
    auto exprThen{parse_expression()};
    if (!exprThen)
      report_error("expected then clause in conditional expression");
    auto exprElse{delimiter(':') ? parse_assignment_expression() : nullptr};
    if (!exprElse)
      report_error("expected else clause in conditional expression");
    expr = attach(bump_allocate<AST::Conditional>(std::move(expr), std::move(exprThen), std::move(exprElse)), cursor);
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
    auto op{parse_unary_op()};
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
    expr = attach(bump_allocate<AST::Unary>(*op, std::move(expr)), cursor);
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
    if (delimiter('.')) {
      auto name{parse_simple_name()};
      if (!name)
        report_error("expected name after '.'");
      return attach(bump_allocate<AST::GetField>(std::move(expr), std::move(name)), cursor);
    }
    if (next("++"))
      return attach(bump_allocate<AST::Unary>(AST::UnaryOp::PostfixIncr, std::move(expr)), cursor);
    if (next("--"))
      return attach(bump_allocate<AST::Unary>(AST::UnaryOp::PostfixDecr, std::move(expr)), cursor);
    if (auto args{parse_argument_list()})
      return attach(bump_allocate<AST::Call>(std::move(expr), std::move(*args)), cursor);
    auto indices{llvm::SmallVector<unique_bump_ptr<AST::Expr>>{}};
    while (!get_remaining_text().starts_with("[[") && delimiter('[')) {
      if (delimiter('<')) {
        auto name{parse_simple_name()};
        if (!name)
          report_error("expected name after '[<'");
        if (!delimiter('>'))
          report_error("expected '>]'");
        indices.emplace_back(attach(bump_allocate<AST::SizeName>(std::move(name)), cursor));
      } else {
        indices.emplace_back(parse_expression()); // This may be null for '[]'
      }
      if (!delimiter(']'))
        report_error("expected ']'");
      skip();
    }
    if (!indices.empty())
      return bump_allocate<AST::GetIndex>(std::move(expr), std::move(indices));
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
  if (!next_word("let"))
    return nullptr;
  auto vars{llvm::SmallVector<unique_bump_ptr<AST::Variable>>{}};
  if (delimiter('{')) {
    while (true) {
      auto var{parse_variable_declaration()};
      if (!var)
        break;
      vars.push_back(std::move(var));
      skip();
      if (peek() == '}')
        break;
    }
    if (!delimiter('}'))
      report_error("expected closing '}' after 'let'");
  } else {
    auto var{parse_variable_declaration()};
    if (!var)
      report_error("expected variable declaration after 'let'");
    vars.push_back(std::move(var));
  }
  if (!next_word("in"))
    report_error("expected 'in' after 'let'");
  auto expr{parse_conditional_expression()};
  if (!expr)
    report_error("expected expression after 'in'");
  return attach(bump_allocate<AST::Let>(std::move(vars), std::move(expr)), cursor);
}

auto Parser::parse_return_from_expression() -> unique_bump_ptr_wrapper<AST::ReturnFrom> {
  auto cursor{save_cursor()};
  if (!next_word("return_from"))
    return nullptr;
  auto stmt{parse_compound_statement()};
  if (!stmt)
    report_error("expected compound statement after 'return_from'", cursor);
  return attach(bump_allocate<AST::ReturnFrom>(std::move(stmt)), cursor);
}

auto Parser::parse_primary_expression() -> unique_bump_ptr_wrapper<AST::Expr> {
  auto cursor{save_cursor()};
  if (auto expr{parse_expression_in_parentheses()})
    return expr;
  if (auto expr{parse_literal_expression()})
    return expr;
  if (auto expr{parse_identifier()})
    return expr;
  if (next_word("cast")) {
    if (!delimiter('<'))
      report_error("expected opening '<' after 'cast'");
    auto type{parse_type()};
    if (!type)
      report_error("expected type after 'cast'");
    if (!delimiter('>'))
      report_error("expected closing '>' after 'cast'");
    auto expr{parse_expression_in_parentheses()};
    if (!expr)
      report_error("expected parenthesized expression after 'cast<...>'");
    return attach(bump_allocate<AST::Cast>(std::move(type), std::move(expr)), cursor);
  }
  return nullptr;
}

auto Parser::parse_literal_expression() -> unique_bump_ptr_wrapper<AST::Expr> {
  checkpoint();
  auto cursor{state};
  if (next_word("true")) {
    accept();
    return attach(bump_allocate<AST::LiteralBool>(true), cursor);
  }
  if (next_word("false")) {
    accept();
    return attach(bump_allocate<AST::LiteralBool>(false), cursor);
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
            report_error("numeric literal must not be terminated by a single-quote separator");
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
      return attach(bump_allocate<AST::LiteralInt>(value.getLimitedValue()), cursor);
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
            bump_allocate<AST::LiteralInt>(llvm::APInt(llvm::APInt::getBitsNeeded(digits, 10), digits, 10).getLimitedValue()),
            cursor);
      } else {
        llvm::APFloat value(llvm::APFloat::IEEEdouble());
        auto opStatus{value.convertFromString(digits, llvm::APFloat::rmNearestTiesToEven)};
        if (!opStatus)
          report_error(std::format("failed to parse floating point literal '{}'", digits.c_str()));
        return attach(bump_allocate<AST::LiteralFloat>(value.convertToDouble(), precision), cursor);
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
    llvm::SmallVector<Char> str32{};
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
    return attach(bump_allocate<AST::LiteralString>(llvm::SmallString<64>(str.c_str())), cursor);
  }
  if (isSmdlSyntax) {
    if (next('#')) {
      auto name{next_word()};
      if (name.empty()) {
        reject();
        return nullptr;
      }
      accept();
      return attach(bump_allocate<AST::Intrinsic>(name), cursor);
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
    auto op{parse_binary_op(ops)};
    if (!op) {
      reject();
      break;
    }
    auto opSrc{source_since(cursor)}; // Source region of binary operator is the operator itself
    skip();
    auto rhs{parseTerm()};
    if (!rhs) {
      reject();
      break;
    } else {
      accept();
      lhs = attach(bump_allocate<AST::Binary>(*op, std::move(lhs), std::move(rhs)), cursor, opSrc);
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
  auto op{parse_binary_op(ops)};
  if (!op) {
    reject();
    return lhs;
  }
  auto opSrc{source_since(cursor)}; // Source region of binary operator is the operator itself
  skip();
  auto rhs{parse_binary_right_associative(ops, parseTerm)};
  if (!rhs) {
    reject();
    return lhs;
  } else {
    accept();
    return attach(bump_allocate<AST::Binary>(*op, std::move(lhs), std::move(rhs)), cursor, opSrc);
  }
}

auto Parser::parse_unary_op() -> std::optional<AST::UnaryOp> {
  using UnOp = AST::UnaryOp;
  for (auto &op : std::array{UnOp::Incr, UnOp::Decr, UnOp::Pos, UnOp::Neg, UnOp::Not, UnOp::LogicalNot})
    if (next(AST::to_string(op)))
      return op;
  if (isSmdlSyntax) {
    if (next(AST::to_string(UnOp::Address)))
      return UnOp::Address;
    if (next(AST::to_string(UnOp::Deref)))
      return UnOp::Deref;
    if (next(AST::to_string(UnOp::Maybe)))
      return UnOp::Maybe;
  }
  return std::nullopt;
}

auto Parser::parse_binary_op(llvm::ArrayRef<AST::BinaryOp> ops) -> std::optional<AST::BinaryOp> {
  for (auto &op : ops) {
    // Don't process ":=" or "<:" unless in extended syntax mode.
    if ((op == AST::BinaryOp::Def || op == AST::BinaryOp::Subset) && !isSmdlSyntax)
      continue;
    // Don't mistake bit and for logical and.
    if (op == AST::BinaryOp::And && get_remaining_text().starts_with("&&"))
      continue;
    if (next(AST::to_string(op)))
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
    auto cond{parse_late_if_condition()};
    if (!delimiter(';'))
      report_error("expected ';' after expression", cursor);
    return attach(bump_allocate<AST::ExprStmt>(std::move(expr), std::move(cond)), cursor);
  }
  return nullptr;
}

auto Parser::parse_compound_statement() -> unique_bump_ptr_wrapper<AST::Compound> {
  auto cursor{save_cursor()};
  if (!delimiter('{'))
    return nullptr;
  auto stmts{llvm::SmallVector<unique_bump_ptr<AST::Stmt>>{}};
  while (true) {
    auto stmt{parse_statement()};
    if (!stmt)
      break;
    stmts.push_back(std::move(stmt));
    skip();
    if (peek() == '}')
      break;
  }
  if (!delimiter('}'))
    report_error("expected '}' to close compound statement");
  return attach(bump_allocate<AST::Compound>(std::move(stmts)), cursor);
}

auto Parser::parse_if_statement() -> unique_bump_ptr_wrapper<AST::If> {
  auto cursor{save_cursor()};
  if (!next_word("if"))
    return nullptr;
  auto cond{parse_expression_in_parentheses()};
  if (!cond)
    report_error("expected parenthesized condition after 'if'", cursor);
  auto ifPass{parse_statement()};
  if (!ifPass)
    report_error("expected statement after 'if (...)'", cursor);
  if (auto cursorElse{save_cursor()}; next_word("else")) {
    auto ifFail{parse_statement()};
    if (!ifFail)
      report_error("expected statement after 'else'", cursorElse);
    return attach(bump_allocate<AST::If>(std::move(cond), std::move(ifPass), std::move(ifFail)), cursor);
  } else {
    return attach(bump_allocate<AST::If>(std::move(cond), std::move(ifPass), nullptr), cursor);
  }
}

auto Parser::parse_switch_statement() -> unique_bump_ptr_wrapper<AST::Switch> {
  auto cursor{save_cursor()};
  if (!next_word("switch"))
    return nullptr;
  auto what{parse_expression_in_parentheses()};
  if (!what)
    report_error("expected parenthesized expression after 'switch'", cursor);
  if (!delimiter('{'))
    report_error("expected opening '{' after 'switch'", cursor);
  auto switchCases{llvm::SmallVector<AST::Switch::Case>{}};
  while (true) {
    auto switchCase{parse_switch_case()};
    if (!switchCase)
      break;
    switchCases.push_back(std::move(*switchCase));
    skip();
    if (peek() == '}')
      break;
  }
  if (!delimiter('}'))
    report_error("expected closing '}' after 'switch'", cursor);
  return attach(bump_allocate<AST::Switch>(std::move(what), std::move(switchCases)), cursor);
}

auto Parser::parse_switch_case() -> std::optional<AST::Switch::Case> {
  auto cursor{save_cursor()};
  auto switchCase{AST::Switch::Case{}};
  if (next_word("case")) {
    auto cond{parse_expression()};
    if (!cond)
      report_error("expected expression after 'case'", cursor);
    if (!delimiter(':'))
      report_error("expected ':' after 'case ...'", cursor);
    switchCase.cond = std::move(cond);
  } else if (next_word("default")) {
    if (!delimiter(':'))
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
  if (!next_word("while"))
    return nullptr;
  auto expr{parse_expression_in_parentheses()};
  if (!expr)
    report_error("expected parenthesized expression after 'while'");
  auto stmt{parse_statement()};
  if (!stmt)
    report_error("expected statement after 'while (...)'");
  return attach(bump_allocate<AST::While>(std::move(expr), std::move(stmt)), cursor);
}

auto Parser::parse_do_statement() -> unique_bump_ptr_wrapper<AST::DoWhile> {
  auto cursor{save_cursor()};
  if (!next_word("do"))
    return nullptr;
  auto stmt{parse_statement()};
  if (!stmt)
    report_error("expected statement after 'do'", cursor);
  if (!next_word("while"))
    report_error("expected 'while' after 'do { ... }'", cursor);
  auto expr{parse_expression_in_parentheses()};
  if (!expr)
    report_error("expected parenthesized expression after 'do { ... } while'", cursor);
  if (!delimiter(';'))
    report_error("expected ';' after 'do { ... } while (...)'", cursor);
  return attach(bump_allocate<AST::DoWhile>(std::move(stmt), std::move(expr)), cursor);
}

auto Parser::parse_for_statement() -> unique_bump_ptr_wrapper<AST::For> {
  auto cursor{save_cursor()};
  if (!next_word("for"))
    return nullptr;
  if (!delimiter('('))
    report_error("expected '(' after 'for'", cursor);
  auto init{unique_bump_ptr_wrapper<AST::Stmt>{}};
  if (auto decl{parse_variable_declaration()}) {
    auto srcLoc{decl->srcLoc};
    init = bump_allocate<AST::DeclStmt>(std::move(decl));
    init->srcLoc = srcLoc;
  } else if (auto expr{parse_expression()}) {
    if (!delimiter(';'))
      report_error("expected ';' after expression", cursor);
    auto srcLoc{expr->srcLoc};
    init = bump_allocate<AST::ExprStmt>(std::move(expr));
    init->srcLoc = srcLoc;
  } else {
    report_error("expected variable declaration or expression after 'for'", cursor);
  }
  auto cond{parse_expression()};
  if (!delimiter(';'))
    report_error("expected ';' after expression", cursor);
  auto incr{parse_expression()};
  if (!delimiter(')'))
    report_error("expected closing ')' after 'for'", cursor);
  auto body{parse_statement()};
  if (!body)
    report_error("expected statement after 'for (...)'", cursor);
  return attach(bump_allocate<AST::For>(std::move(init), std::move(cond), std::move(incr), std::move(body)), cursor);
}

auto Parser::parse_break_statement() -> unique_bump_ptr_wrapper<AST::Break> {
  auto cursor{save_cursor()};
  if (!next_word("break"))
    return nullptr;
  auto cond{parse_late_if_condition()};
  if (!delimiter(';'))
    report_error("expected ';' after 'break'", cursor);
  return attach(bump_allocate<AST::Break>(std::move(cond)), cursor);
}

auto Parser::parse_continue_statement() -> unique_bump_ptr_wrapper<AST::Continue> {
  auto cursor{save_cursor()};
  if (!next_word("continue"))
    return nullptr;
  auto cond{parse_late_if_condition()};
  if (!delimiter(';'))
    report_error("expected ';' after 'continue'", cursor);
  return attach(bump_allocate<AST::Continue>(std::move(cond)), cursor);
}

auto Parser::parse_return_statement() -> unique_bump_ptr_wrapper<AST::Return> {
  auto cursor{save_cursor()};
  if (!next_word("return"))
    return nullptr;
  auto expr{parse_expression()};
  if (!expr && !isSmdlSyntax)
    report_error("expected expression after 'return'", cursor);
  auto cond{parse_late_if_condition()};
  if (!delimiter(';'))
    report_error("expected ';' after expression", cursor);
  return attach(bump_allocate<AST::Return>(std::move(expr), std::move(cond)), cursor);
}

auto Parser::parse_unreachable_statement() -> unique_bump_ptr_wrapper<AST::Unreachable> {
  auto cursor{save_cursor()};
  if (!next_word("unreachable"))
    return nullptr;
  if (!delimiter(';'))
    report_error("expected ';' after 'unreachable'", cursor);
  return attach(bump_allocate<AST::Unreachable>(), cursor);
}

auto Parser::parse_preserve_statement() -> unique_bump_ptr_wrapper<AST::Preserve> {
  auto cursor{save_cursor()};
  if (!next_word("preserve"))
    return nullptr;
  auto exprs{llvm::SmallVector<unique_bump_ptr<AST::Expr>>{}};
  while (true) {
    auto expr{parse_unary_expression()};
    if (!expr)
      break;
    exprs.push_back(std::move(expr));
    if (!delimiter(','))
      break;
  }
  if (!delimiter(';'))
    report_error("expected ';' after 'preserve ...'", cursor);
  return attach(bump_allocate<AST::Preserve>(std::move(exprs)), cursor);
}

auto Parser::parse_defer_statement() -> unique_bump_ptr_wrapper<AST::Defer> {
  auto cursor{save_cursor()};
  if (!next_word("defer"))
    return nullptr;
  auto stmt{parse_statement()};
  if (!stmt)
    report_error("expected statement after 'defer'", cursor);
  return attach(bump_allocate<AST::Defer>(std::move(stmt)), cursor);
}

auto Parser::parse_visit_statement() -> unique_bump_ptr_wrapper<AST::Visit> {
  auto cursor{save_cursor()};
  if (!next_word("visit"))
    return nullptr;
  auto name{parse_simple_name()};
  if (!name)
    report_error("expected simple name after 'visit'", cursor);
  auto what{unique_bump_ptr<AST::Expr>{}};
  if (!next_word("in")) {
    // 'visit name' same as 'visit name in name'
    auto names{llvm::SmallVector<unique_bump_ptr<AST::Name>>{}};
    names.emplace_back(bump_allocate<AST::Name>(name->name));
    what.reset(bump_allocate<AST::Identifier>(std::move(names), false));
  } else {
    what = parse_expression();
    if (!what)
      report_error("expected expression after 'visit ... in'", cursor);
  }
  auto body{parse_compound_statement()};
  if (!body)
    report_error("expected compound statement after 'visit ... in ...'", cursor);
  return attach(bump_allocate<AST::Visit>(std::move(name), std::move(what), std::move(body)), cursor);
}

auto Parser::parse_late_if_condition() -> unique_bump_ptr_wrapper<AST::Expr> {
  if (!(isSmdlSyntax && next_word("if")))
    return nullptr;
  auto cond{parse_expression_in_parentheses()};
  if (!cond)
    report_error("expected expression in parentheses after '... if'");
  return cond;
}
//--}

} // namespace smdl
