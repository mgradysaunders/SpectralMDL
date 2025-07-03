/// \file
// vim:foldmethod=marker:foldlevel=0:fmr=--{,--}
#pragma once

#include <array>
#include <functional>
#include <initializer_list>

#include "smdl/AST.h"
#include "smdl/Module.h"

namespace smdl {

/// \addtogroup Main
/// \{

/// The parser.
class SMDL_EXPORT Parser final {
public:
  explicit Parser(BumpPtrAllocator &allocator, Module &module_,
                  bool isSmdl = false)
      : allocator(allocator), module_(module_), isSmdl(isSmdl) {
    srcLoc.module_ = &module_;
  }

  /// Non-copyable and non-movable.
  Parser(const Parser &) = delete;

  /// Parse everything.
  [[nodiscard]] BumpPtr<AST::File> parse() { return parse_file(); }

private:
  //--{ Basics
  [[nodiscard]] bool is_eof() const {
    return srcLoc.i >= get_source_code().size();
  }

  [[nodiscard]] std::string_view get_source_code() const {
    return module_.get_source_code();
  }

  [[nodiscard]] std::string_view
  get_source_code_between(const SourceLocation &srcLoc0,
                          const SourceLocation &srcLoc1) const {
    return get_source_code().substr(srcLoc0.i, srcLoc1.i - srcLoc0.i);
  }

  [[nodiscard]] std::string_view get_remaining_source_code() const {
    return get_source_code().substr(srcLoc.i);
  }

  [[nodiscard]] char peek() const;

  char next();

  std::string_view next(size_t n);

  [[nodiscard]] std::optional<std::string_view> next(std::string_view str);

  [[nodiscard]] std::optional<std::string_view>
  next_delimiter(std::string_view str) {
    skip();
    return next(str);
  }

  [[nodiscard]] std::optional<std::string_view>
  next_keyword(std::string_view str);

  [[nodiscard]] std::optional<std::string_view>
  next_keyword(std::initializer_list<std::string_view> strs) {
    for (auto str : strs)
      if (auto result{next_keyword(str)})
        return result;
    return std::nullopt;
  }

  [[nodiscard]] std::optional<std::string_view> next_word();

  [[nodiscard]] std::optional<std::string_view> next_integer();

  void skip();

  SourceLocation checkpoint() {
    skip();
    srcLocStack.push_back(srcLoc);
    return srcLoc;
  }

  void accept() {
    SMDL_SANITY_CHECK(srcLocStack.size() >= 1);
    srcLocStack.pop_back();
  }

  void reject() {
    SMDL_SANITY_CHECK(srcLocStack.size() >= 1);
    srcLoc = srcLocStack.back(), srcLocStack.pop_back();
  }
  //--}

private:
  //--{ Parse: Expr
  [[nodiscard]] auto parse_simple_name() -> std::optional<AST::Name>;

  [[nodiscard]] auto parse_identifier() -> BumpPtr<AST::Identifier>;

  [[nodiscard]] auto parse_type() -> BumpPtr<AST::Type>;

  [[nodiscard]] auto parse_parameter() -> std::optional<AST::Parameter>;

  [[nodiscard]] auto parse_parameter_list()
      -> std::optional<AST::ParameterList>;

  [[nodiscard]] auto parse_argument() -> std::optional<AST::Argument>;

  [[nodiscard]] auto parse_argument_list() -> std::optional<AST::ArgumentList>;

  [[nodiscard]] auto parse_annotation() -> std::optional<AST::Annotation>;

  [[nodiscard]] auto parse_annotation_block() -> BumpPtr<AST::AnnotationBlock>;

  [[nodiscard]] auto parse_expression_in_parentheses() -> BumpPtr<AST::Expr>;

  [[nodiscard]] auto parse_expression() -> BumpPtr<AST::Expr>;

  [[nodiscard]] auto parse_assignment_expression() -> BumpPtr<AST::Expr>;

  [[nodiscard]] auto parse_else_expression() -> BumpPtr<AST::Expr>;

  [[nodiscard]] auto parse_conditional_expression() -> BumpPtr<AST::Expr>;

  [[nodiscard]] auto parse_logical_or_expression() -> BumpPtr<AST::Expr>;

  [[nodiscard]] auto parse_logical_and_expression() -> BumpPtr<AST::Expr>;

  [[nodiscard]] auto parse_inclusive_or_expression() -> BumpPtr<AST::Expr>;

  [[nodiscard]] auto parse_exclusive_or_expression() -> BumpPtr<AST::Expr>;

  [[nodiscard]] auto parse_and_expression() -> BumpPtr<AST::Expr>;

  [[nodiscard]] auto parse_equality_expression() -> BumpPtr<AST::Expr>;

  [[nodiscard]] auto parse_relational_expression() -> BumpPtr<AST::Expr>;

  [[nodiscard]] auto parse_shift_expression() -> BumpPtr<AST::Expr>;

  [[nodiscard]] auto parse_additive_expression() -> BumpPtr<AST::Expr>;

  [[nodiscard]] auto parse_multiplicative_expression() -> BumpPtr<AST::Expr>;

  [[nodiscard]] auto parse_unary_expression() -> BumpPtr<AST::Expr>;

  [[nodiscard]] auto parse_postfix_expression() -> BumpPtr<AST::Expr>;

  [[nodiscard]] auto parse_let_expression() -> BumpPtr<AST::Expr>;

  [[nodiscard]] auto parse_return_from_expression() -> BumpPtr<AST::Expr>;

  [[nodiscard]] auto parse_primary_expression() -> BumpPtr<AST::Expr>;

  [[nodiscard]] auto parse_literal_expression() -> BumpPtr<AST::Expr>;

  [[nodiscard]] auto parse_literal_bool_expression()
      -> BumpPtr<AST::LiteralBool>;

  [[nodiscard]] auto parse_literal_string_expression()
      -> BumpPtr<AST::LiteralString>;

  [[nodiscard]] auto parse_literal_number_expression() -> BumpPtr<AST::Expr>;

  struct ParsedUnaryOp final {
    std::string_view srcOp{};
    AST::UnaryOp op{};
  };

  [[nodiscard]] auto parse_unary_op() -> std::optional<ParsedUnaryOp>;

  struct ParsedBinaryOp final {
    std::string_view srcOp{};
    AST::BinaryOp op{};
  };

  [[nodiscard]] auto parse_binary_op(Span<AST::BinaryOp> ops)
      -> std::optional<ParsedBinaryOp>;

  template <typename Func>
  [[nodiscard]] auto parse_binary_left_associative(Span<AST::BinaryOp> ops,
                                                   const Func &parseInner)
      -> BumpPtr<AST::Expr> {
    auto exprLhs{parseInner()};
    if (!exprLhs) {
      return nullptr;
    }
    while (true) {
      auto srcLoc0{checkpoint()};
      auto op{parse_binary_op(ops)};
      if (!op) {
        reject();
        break;
      }
      skip();

      // If parsing an approximate comparison operator `==~` or `!=~`, then
      // also parse the epsilon in `[ ... ]` after the operator and before
      // the right-hand side expression. This is extended syntax!
      if (op->op == BINOP_APPROX_CMP_EQ || //
          op->op == BINOP_APPROX_CMP_NE) {
        auto srcBrackL{next_delimiter("[")};
        auto exprEps{parse_unary_expression()};
        auto srcBrackR{next_delimiter("]")};
        if (!srcBrackL || !exprEps || !srcBrackR)
          srcLoc0.throw_error("expected '[EPSILON]' after ", quoted(op->srcOp));
        auto exprRhs{parseInner()};
        if (!exprRhs)
          srcLoc0.throw_error("expected '[EPSILON] EXPRESSION' after ",
                              quoted(op->srcOp));
        accept();
        exprLhs = allocate<AST::Binary>(
            srcLoc0, std::in_place, std::move(exprLhs), op->srcOp, op->op,
            *srcBrackL, std::move(exprEps), *srcBrackR, std::move(exprRhs));
        continue;
      }

      auto exprRhs{parseInner()};
      if (!exprRhs) {
        reject();
        break;
      } else {
        accept();
        exprLhs =
            allocate<AST::Binary>(srcLoc0, std::in_place, std::move(exprLhs),
                                  op->srcOp, op->op, std::move(exprRhs));
      }
    }
    return exprLhs;
  }

  template <typename Func>
  [[nodiscard]] auto parse_binary_right_associative(Span<AST::BinaryOp> ops,
                                                    const Func &parseInner)
      -> BumpPtr<AST::Expr> {
    auto exprLhs{parseInner()};
    if (!exprLhs)
      return nullptr;
    auto srcLoc0{checkpoint()};
    auto op{parse_binary_op(ops)};
    if (!op) {
      reject();
      return exprLhs;
    }
    skip();
    auto exprRhs{parse_binary_right_associative(ops, parseInner)};
    if (!exprRhs) {
      reject();
      return exprLhs;
    } else {
      accept();
      return allocate<AST::Binary>(srcLoc0, std::in_place, std::move(exprLhs),
                                   op->srcOp, op->op, std::move(exprRhs));
    }
  }
  //--}

private:
  //--{ Parse: Decls
  [[nodiscard]] auto parse_file() -> BumpPtr<AST::File>;

  [[nodiscard]] auto parse_file_version() -> std::optional<AST::File::Version>;

  [[nodiscard]] auto parse_import_path() -> std::optional<AST::ImportPath>;

  [[nodiscard]] auto parse_using_alias() -> BumpPtr<AST::UsingAlias>;

  [[nodiscard]] auto parse_using_import() -> BumpPtr<AST::UsingImport>;

  [[nodiscard]] auto parse_import() -> BumpPtr<AST::Import>;

  [[nodiscard]] auto parse_attributes() -> std::optional<AST::Decl::Attributes>;

  [[nodiscard]] auto parse_global_declaration() -> BumpPtr<AST::Decl>;

  [[nodiscard]] auto parse_annotation_declaration() -> BumpPtr<AST::Decl>;

  [[nodiscard]] auto parse_type_declaration() -> BumpPtr<AST::Decl>;

  [[nodiscard]] auto parse_alias_type_declaration() -> BumpPtr<AST::Typedef>;

  [[nodiscard]] auto parse_struct_type_declaration() -> BumpPtr<AST::Struct>;

  [[nodiscard]] auto parse_struct_constructor()
      -> std::optional<AST::Struct::Constructor>;

  [[nodiscard]] auto parse_struct_field_declarator()
      -> std::optional<AST::Struct::Field>;

  [[nodiscard]] auto parse_enum_type_declaration() -> BumpPtr<AST::Enum>;

  [[nodiscard]] auto parse_enum_value_declarator()
      -> std::optional<AST::Enum::Declarator>;

  [[nodiscard]] auto parse_variable_declaration() -> BumpPtr<AST::Variable>;

  [[nodiscard]] auto parse_variable_declarator()
      -> std::optional<AST::Variable::Declarator>;

  [[nodiscard]] auto parse_function_declaration() -> BumpPtr<AST::Function>;

  [[nodiscard]] auto parse_tag_declaration() -> BumpPtr<AST::Tag>;

  [[nodiscard]] auto parse_exec_declaration() -> BumpPtr<AST::Exec>;

  [[nodiscard]] auto parse_unit_test_declaration() -> BumpPtr<AST::UnitTest>;

  [[nodiscard]] auto parse_namespace_declaration() -> BumpPtr<AST::Namespace>;
  //--}

private:
  //--{ Parse: Stmt
  [[nodiscard]] auto parse_statement() -> BumpPtr<AST::Stmt>;

  [[nodiscard]] auto parse_compound_statement() -> BumpPtr<AST::Compound>;

  [[nodiscard]] auto parse_if_statement() -> BumpPtr<AST::If>;

  [[nodiscard]] auto parse_switch_statement() -> BumpPtr<AST::Switch>;

  [[nodiscard]] auto parse_switch_case() -> std::optional<AST::Switch::Case>;

  [[nodiscard]] auto parse_while_statement() -> BumpPtr<AST::While>;

  [[nodiscard]] auto parse_do_statement() -> BumpPtr<AST::DoWhile>;

  [[nodiscard]] auto parse_for_statement() -> BumpPtr<AST::For>;

  [[nodiscard]] auto parse_break_statement() -> BumpPtr<AST::Break>;

  [[nodiscard]] auto parse_continue_statement() -> BumpPtr<AST::Continue>;

  [[nodiscard]] auto parse_return_statement() -> BumpPtr<AST::Return>;

  [[nodiscard]] auto parse_unreachable_statement() -> BumpPtr<AST::Unreachable>;

  [[nodiscard]] auto parse_preserve_statement() -> BumpPtr<AST::Preserve>;

  [[nodiscard]] auto parse_defer_statement() -> BumpPtr<AST::Defer>;

  [[nodiscard]] auto parse_visit_statement() -> BumpPtr<AST::Visit>;

  [[nodiscard]] auto parse_late_if() -> std::optional<AST::LateIf>;
  //--}

private:
  BumpPtrAllocator &allocator;

  Module &module_;

  SourceLocation srcLoc{};

  std::vector<SourceLocation> srcLocStack{};

  bool isSmdl{};

  template <typename T, typename... Args>
  [[nodiscard]] BumpPtr<T> allocate(SourceLocation srcLoc, std::in_place_t,
                                    Args &&...args) {
    auto result{allocator.allocate<T>(std::forward<Args>(args)...)};
    result->srcLoc = srcLoc;
    return result;
  }
};

/// \}

} // namespace smdl
