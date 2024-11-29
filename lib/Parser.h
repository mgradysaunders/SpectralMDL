// vim:foldmethod=marker:foldlevel=0:fmr=--{,--}
#pragma once

#include "AST.h"

namespace smdl {

//--{ 'unique_bump_ptr_wrapper'
template <typename T> class unique_bump_ptr_wrapper final {
public:
  unique_bump_ptr_wrapper() = default;

  unique_bump_ptr_wrapper(std::nullptr_t) {}

  template <std::derived_from<T> U> unique_bump_ptr_wrapper(U *ptr) : ptr(static_cast<T *>(ptr)) {}

  template <std::derived_from<T> U>
  unique_bump_ptr_wrapper(unique_bump_ptr_wrapper<U> ptr) : ptr(static_cast<T *>(ptr.release())) {}

  [[nodiscard]] operator bool() const { return ptr != nullptr; }

  template <typename S> [[nodiscard]] operator std::unique_ptr<S, unique_bump_ptr_deleter>() && {
    return unique_bump_ptr<S>(static_cast<S *>(ptr.release()));
  }

  [[nodiscard]] T *operator->() { return ptr.get(); }

  [[nodiscard]] T *get() { return ptr.get(); }

  [[nodiscard]] T *release() { return ptr.release(); }

  unique_bump_ptr<T> ptr{};
};
//--}

//--{ 'utf8'
namespace utf8 {

[[nodiscard]] inline const std::locale &get_locale() {
  static const std::locale loc("en_US.UTF8");
  return loc;
}

[[nodiscard]] inline bool is_blank(llvm::UTF32 ch) { return std::isblank(wchar_t(ch), get_locale()); }

[[nodiscard]] inline bool is_space(llvm::UTF32 ch) { return std::isspace(wchar_t(ch), get_locale()); }

[[nodiscard]] inline bool is_alpha(llvm::UTF32 ch) { return std::isalpha(wchar_t(ch), get_locale()); }

[[nodiscard]] inline bool is_digit(llvm::UTF32 ch) { return std::isdigit(wchar_t(ch), get_locale()); }

[[nodiscard]] inline bool is_xdigit(llvm::UTF32 ch) { return std::isxdigit(wchar_t(ch), get_locale()); }

[[nodiscard]] inline bool is_digit_2(llvm::UTF32 ch) { return '0' <= ch && ch <= '1'; }

[[nodiscard]] inline bool is_digit_8(llvm::UTF32 ch) { return '0' <= ch && ch <= '7'; }

[[nodiscard]] inline bool is_digit_16(llvm::UTF32 ch) {
  return ('0' <= ch && ch <= '9') || ('a' <= ch && ch <= 'f') || ('A' <= ch && ch <= 'F');
}

[[nodiscard]] inline bool is_word(llvm::UTF32 ch) { return std::isalnum(wchar_t(ch), get_locale()) or ch == '_'; }

[[nodiscard]] inline llvm::UTF32 oct_to_int(llvm::UTF32 ch) {
  if ('0' <= ch && ch <= '7')
    return ch - '0';
  return 0;
}

[[nodiscard]] inline llvm::UTF32 hex_to_int(llvm::UTF32 ch) {
  if ('0' <= ch && ch <= '9')
    return ch - '0';
  if ('a' <= ch && ch <= 'f')
    return ch - 'a' + 10;
  if ('A' <= ch && ch <= 'F')
    return ch - 'A' + 10;
  return 0;
}

[[nodiscard]] inline llvm::UTF32 to_lower(llvm::UTF32 ch) { return std::tolower(wchar_t(ch), get_locale()); }

[[nodiscard]] inline llvm::UTF32 to_upper(llvm::UTF32 ch) { return std::toupper(wchar_t(ch), get_locale()); }

[[nodiscard]] inline size_t get_size(llvm::UTF8 first) { return llvm::getNumBytesForUTF8(first); }

[[nodiscard]] inline size_t get_size(llvm::StringRef str) {
  size_t i{};
  size_t n{};
  while (i < str.size()) {
    i += get_size(str[i]);
    n++;
  }
  return n;
}

[[nodiscard]] inline std::optional<llvm::UTF32> decode_next(llvm::StringRef str, size_t *numBytes = nullptr) {
  if (str.empty()) {
    if (numBytes)
      *numBytes = 0;
    return llvm::UTF32(0);
  }
  auto inputBegin{reinterpret_cast<const llvm::UTF8 *>(str.bytes_begin())};
  auto inputEnd{reinterpret_cast<const llvm::UTF8 *>(str.bytes_end())};
  auto inputItr{inputBegin};
  auto output{llvm::UTF32{}};
  auto outputBegin{&output};
  if (auto result{llvm::ConvertUTF8toUTF32(&inputItr, inputEnd, &outputBegin, &output + 1, llvm::strictConversion)};
      !(result == llvm::conversionOK || result == llvm::targetExhausted))
    return std::nullopt;
  if (numBytes)
    *numBytes = inputItr - inputBegin;
  return output;
}

struct Encoding final {
  Encoding(llvm::UTF32 codepoint) {
    char *itr0 = &buffer[0];
    char *itr1 = &buffer[0];
    if (!llvm::ConvertCodePointToUTF8(codepoint, itr1))
      assert(!"Error performing UTF8 encoding!");
    strv = llvm::StringRef(itr0, itr1 - itr0);
  }
  char buffer[8] = {};
  llvm::StringRef strv;
};

template <typename... Args> inline std::basic_string<Args...> &operator+=(std::basic_string<Args...> &str, Encoding encoding) {
  for (char ch : encoding.strv)
    str.push_back(ch);
  return str;
}

} // namespace utf8
//--}

class Parser final {
public:
  using Char = llvm::UTF32;

  struct Cursor final {
    uint64_t lineNo{1};

    uint64_t charNo{1};

    uint64_t i{};
  };

  Parser(llvm::BumpPtrAllocator &bumpAllocator, llvm::StringRef file, llvm::StringRef text, bool isExtendedSyntax = false)
      : bumpAllocator(bumpAllocator), file(file), text(text), isExtendedSyntax(isExtendedSyntax) {}

  Parser(const Parser &) = delete;

  Parser(Parser &&) = delete;

  [[nodiscard]] unique_bump_ptr<AST::File> parse() { return parse_mdl(); }

  [[nodiscard]] bool is_extended_syntax() const { return isExtendedSyntax; }

private:
  //--{ Basics
  [[nodiscard]] bool is_eof() const { return state.i >= text.size(); }

  [[nodiscard]] auto get_position() const { return state.i; }

  [[nodiscard]] auto get_text() const { return text; }

  [[nodiscard]] auto get_remaining_text() const { return text.substr(state.i); }

  [[nodiscard]] Char peek(size_t *numBytes = nullptr) const;

  void skip();

  void checkpoint() { skip(), states.push_back(state); }

  bool accept() {
    assert(states.size() >= 1);
    states.pop_back();
    return true;
  }

  bool reject() {
    assert(states.size() >= 1);
    state = states.back(), states.pop_back();
    return false;
  }

  Char next();

  bool next(Char c) {
    if (peek() == c) {
      next();
      return true;
    } else {
      return false;
    }
  }

  bool delimiter(Char c) {
    checkpoint();
    skip();
    if (next(c)) {
      accept();
      return true;
    } else {
      reject();
      return false;
    }
  }

  bool next(llvm::StringRef);

  [[nodiscard]] bool next_word(llvm::StringRef);

  [[nodiscard]] llvm::StringRef next_word();

  [[nodiscard]] llvm::StringRef next_int();

  [[nodiscard]] auto attach(auto node, Cursor cursor) {
    node->srcLoc.file = std::string_view(file);
    node->srcLoc.line = cursor.lineNo;
    return node;
  }

  [[nodiscard]] auto save_cursor() {
    skip();
    return state;
  }

  void report_error(std::string message) const { report_error(std::move(message), state); }

  void report_error(std::string message, Cursor cursor) const { throw Error(std::move(message), file.str(), cursor.lineNo); }
  //--}

public:
  //--{ Parse: Decls
  [[nodiscard]] auto parse_mdl() -> unique_bump_ptr_wrapper<AST::File>;

  [[nodiscard]] auto parse_mdl_version() -> std::optional<AST::Version>;

  [[nodiscard]] auto parse_import_path(bool isUnicode) -> unique_bump_ptr_wrapper<AST::Identifier>;

  [[nodiscard]] auto parse_unicode_name() -> unique_bump_ptr_wrapper<AST::Name>;

  [[nodiscard]] auto parse_using_alias() -> unique_bump_ptr_wrapper<AST::UsingAlias>;

  [[nodiscard]] auto parse_using_import() -> unique_bump_ptr_wrapper<AST::UsingImport>;

  [[nodiscard]] auto parse_import() -> unique_bump_ptr_wrapper<AST::Import>;

  [[nodiscard]] auto parse_global_declaration() -> unique_bump_ptr_wrapper<AST::Decl>;

  [[nodiscard]] auto parse_type_declaration() -> unique_bump_ptr_wrapper<AST::Decl>;

  [[nodiscard]] auto parse_alias_type_declaration() -> unique_bump_ptr_wrapper<AST::Typedef>;

  [[nodiscard]] auto parse_struct_type_declaration() -> unique_bump_ptr_wrapper<AST::Struct>;

  [[nodiscard]] auto parse_struct_field_declarator() -> std::optional<AST::Struct::Field>;

  [[nodiscard]] auto parse_enum_type_declaration() -> unique_bump_ptr_wrapper<AST::Enum>;

  [[nodiscard]] auto parse_enum_value_declarator() -> std::optional<AST::Enum::Constant>;

  [[nodiscard]] auto parse_variable_declaration() -> unique_bump_ptr_wrapper<AST::Variable>;

  [[nodiscard]] auto parse_variable_declarator() -> std::optional<AST::Variable::Value>;

  [[nodiscard]] auto parse_function_declaration() -> unique_bump_ptr_wrapper<AST::Function>;

  [[nodiscard]] auto parse_tag_declaration() -> unique_bump_ptr_wrapper<AST::Tag>;

  [[nodiscard]] auto parse_unit_test_declaration() -> unique_bump_ptr_wrapper<AST::UnitTest>;
  //--}

  //--{ Parse: Stmts
  [[nodiscard]] auto parse_statement() -> unique_bump_ptr_wrapper<AST::Stmt>;

  [[nodiscard]] auto parse_compound_statement() -> unique_bump_ptr_wrapper<AST::Compound>;

  [[nodiscard]] auto parse_if_statement() -> unique_bump_ptr_wrapper<AST::If>;

  [[nodiscard]] auto parse_switch_statement() -> unique_bump_ptr_wrapper<AST::Switch>;

  [[nodiscard]] auto parse_switch_case() -> std::optional<AST::Switch::Case>;

  [[nodiscard]] auto parse_while_statement() -> unique_bump_ptr_wrapper<AST::While>;

  [[nodiscard]] auto parse_do_statement() -> unique_bump_ptr_wrapper<AST::DoWhile>;

  [[nodiscard]] auto parse_for_statement() -> unique_bump_ptr_wrapper<AST::For>;

  [[nodiscard]] auto parse_break_statement() -> unique_bump_ptr_wrapper<AST::Break>;

  [[nodiscard]] auto parse_continue_statement() -> unique_bump_ptr_wrapper<AST::Continue>;

  [[nodiscard]] auto parse_return_statement() -> unique_bump_ptr_wrapper<AST::Return>;

  [[nodiscard]] auto parse_unreachable_statement() -> unique_bump_ptr_wrapper<AST::Unreachable>;

  [[nodiscard]] auto parse_preserve_statement() -> unique_bump_ptr_wrapper<AST::Preserve>;

  [[nodiscard]] auto parse_defer_statement() -> unique_bump_ptr_wrapper<AST::Defer>;

  [[nodiscard]] auto parse_visit_statement() -> unique_bump_ptr_wrapper<AST::Visit>;

  [[nodiscard]] auto parse_late_if_condition() -> unique_bump_ptr_wrapper<AST::Expr>;
  //--}

  //--{ Parse: Exprs
  [[nodiscard]] auto parse_simple_name() -> unique_bump_ptr_wrapper<AST::Name>;

  [[nodiscard]] auto parse_identifier() -> unique_bump_ptr_wrapper<AST::Identifier>;

  [[nodiscard]] auto parse_type() -> unique_bump_ptr_wrapper<AST::Type>;

  [[nodiscard]] auto parse_parameter() -> std::optional<AST::Param>;

  [[nodiscard]] auto parse_parameter_list() -> std::optional<AST::ParamList>;

  [[nodiscard]] auto parse_argument() -> std::optional<AST::Arg>;

  [[nodiscard]] auto parse_argument_list() -> std::optional<AST::ArgList>;

  [[nodiscard]] auto parse_annotation() -> std::optional<AST::Annotation>;

  [[nodiscard]] auto parse_annotation_block() -> std::optional<AST::AnnotationBlock>;

  [[nodiscard]] auto parse_expression() -> unique_bump_ptr_wrapper<AST::Expr>;

  [[nodiscard]] auto parse_expression_in_parentheses() -> unique_bump_ptr_wrapper<AST::Expr>;

  [[nodiscard]] auto parse_assignment_expression() -> unique_bump_ptr_wrapper<AST::Expr>;

  [[nodiscard]] auto parse_conditional_expression() -> unique_bump_ptr_wrapper<AST::Expr>;

  [[nodiscard]] auto parse_logical_or_expression() -> unique_bump_ptr_wrapper<AST::Expr>;

  [[nodiscard]] auto parse_logical_and_expression() -> unique_bump_ptr_wrapper<AST::Expr>;

  [[nodiscard]] auto parse_inclusive_or_expression() -> unique_bump_ptr_wrapper<AST::Expr>;

  [[nodiscard]] auto parse_exclusive_or_expression() -> unique_bump_ptr_wrapper<AST::Expr>;

  [[nodiscard]] auto parse_and_expression() -> unique_bump_ptr_wrapper<AST::Expr>;

  [[nodiscard]] auto parse_equality_expression() -> unique_bump_ptr_wrapper<AST::Expr>;

  [[nodiscard]] auto parse_relational_expression() -> unique_bump_ptr_wrapper<AST::Expr>;

  [[nodiscard]] auto parse_shift_expression() -> unique_bump_ptr_wrapper<AST::Expr>;

  [[nodiscard]] auto parse_additive_expression() -> unique_bump_ptr_wrapper<AST::Expr>;

  [[nodiscard]] auto parse_multiplicative_expression() -> unique_bump_ptr_wrapper<AST::Expr>;

  [[nodiscard]] auto parse_unary_expression() -> unique_bump_ptr_wrapper<AST::Expr>;

  [[nodiscard]] auto parse_postfix_expression() -> unique_bump_ptr_wrapper<AST::Expr>;

  [[nodiscard]] auto parse_let_expression() -> unique_bump_ptr_wrapper<AST::Expr>;

  [[nodiscard]] auto parse_return_from_expression() -> unique_bump_ptr_wrapper<AST::ReturnFrom>;

  [[nodiscard]] auto parse_primary_expression() -> unique_bump_ptr_wrapper<AST::Expr>;

  [[nodiscard]] auto parse_literal_expression() -> unique_bump_ptr_wrapper<AST::Expr>;

  [[nodiscard]] auto parse_unary_op() -> std::optional<AST::UnaryOp>;

  [[nodiscard]] auto parse_binary_op(llvm::ArrayRef<AST::BinaryOp> ops) -> std::optional<AST::BinaryOp>;

  [[nodiscard]] auto parse_binary_left_associative(
      llvm::ArrayRef<AST::BinaryOp> ops,
      const std::function<unique_bump_ptr_wrapper<AST::Expr>()> &parseTerm) -> unique_bump_ptr_wrapper<AST::Expr>;

  [[nodiscard]] auto parse_binary_right_associative(
      llvm::ArrayRef<AST::BinaryOp> ops,
      const std::function<unique_bump_ptr_wrapper<AST::Expr>()> &parseTerm) -> unique_bump_ptr_wrapper<AST::Expr>;
  //--}

private:
  llvm::BumpPtrAllocator &bumpAllocator;

  llvm::StringRef file{"(unnamed)"};

  llvm::StringRef text{};

  Cursor state{};

  llvm::SmallVector<Cursor> states{};

  bool isExtendedSyntax{false};

  template <typename T> [[nodiscard]] inline T *bump_allocate(auto &&...args) {
    return new (bumpAllocator.Allocate(sizeof(T), alignof(T))) T(std::forward<decltype(args)>(args)...);
  }
};

} // namespace smdl
