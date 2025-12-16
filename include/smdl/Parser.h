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
  [[nodiscard]] BumpPtr<AST::File> parse() { return parseFile(); }

private:
  //--{ Basics
  [[nodiscard]] bool isEOF() const {
    return srcLoc.i >= getSourceCode().size();
  }

  [[nodiscard]] std::string_view getSourceCode() const {
    return module_.getSourceCode();
  }

  [[nodiscard]] std::string_view
  getSourceCodeBetween(const SourceLocation &srcLoc0,
                       const SourceLocation &srcLoc1) const {
    return getSourceCode().substr(srcLoc0.i, srcLoc1.i - srcLoc0.i);
  }

  [[nodiscard]] std::string_view getRemainingSourceCode() const {
    return getSourceCode().substr(srcLoc.i);
  }

  [[nodiscard]] char peek() const;

  char next();

  std::string_view next(size_t n);

  [[nodiscard]] std::optional<std::string_view> next(std::string_view str);

  [[nodiscard]] std::optional<std::string_view>
  nextDelimiter(std::string_view str) {
    skip();
    return next(str);
  }

  [[nodiscard]] std::optional<std::string_view>
  nextKeyword(std::string_view str);

  [[nodiscard]] std::optional<std::string_view>
  nextKeyword(std::initializer_list<std::string_view> strs) {
    for (auto str : strs)
      if (auto result{nextKeyword(str)})
        return result;
    return std::nullopt;
  }

  [[nodiscard]] std::optional<std::string_view> nextWord();

  [[nodiscard]] std::optional<std::string_view> nextInteger();

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
  [[nodiscard]] auto parseSimpleName() -> std::optional<AST::Name>;

  [[nodiscard]] auto parseIdentifier() -> BumpPtr<AST::Identifier>;

  [[nodiscard]] auto parseType() -> BumpPtr<AST::Type>;

  [[nodiscard]] auto parseParameter() -> std::optional<AST::Parameter>;

  [[nodiscard]] auto parseParameterList() -> std::optional<AST::ParameterList>;

  [[nodiscard]] auto parseArgument() -> std::optional<AST::Argument>;

  [[nodiscard]] auto parseArgumentList() -> std::optional<AST::ArgumentList>;

  [[nodiscard]] auto parseAnnotation() -> std::optional<AST::Annotation>;

  [[nodiscard]] auto parseAnnotationBlock() -> BumpPtr<AST::AnnotationBlock>;

  [[nodiscard]] auto parseExpressionInParentheses() -> BumpPtr<AST::Expr>;

  [[nodiscard]] auto parseExpression() -> BumpPtr<AST::Expr>;

  [[nodiscard]] auto parseAssignmentExpression() -> BumpPtr<AST::Expr>;

  [[nodiscard]] auto parseElseExpression() -> BumpPtr<AST::Expr>;

  [[nodiscard]] auto parseConditionalExpression() -> BumpPtr<AST::Expr>;

  [[nodiscard]] auto parseLogicalOrExpression() -> BumpPtr<AST::Expr>;

  [[nodiscard]] auto parseLogicalAndExpression() -> BumpPtr<AST::Expr>;

  [[nodiscard]] auto parseInclusiveOrExpression() -> BumpPtr<AST::Expr>;

  [[nodiscard]] auto parseExclusiveOrExpression() -> BumpPtr<AST::Expr>;

  [[nodiscard]] auto parseAndExpression() -> BumpPtr<AST::Expr>;

  [[nodiscard]] auto parseEqualityExpression() -> BumpPtr<AST::Expr>;

  [[nodiscard]] auto parseRelationalExpression() -> BumpPtr<AST::Expr>;

  [[nodiscard]] auto parseShiftExpression() -> BumpPtr<AST::Expr>;

  [[nodiscard]] auto parseAdditiveExpression() -> BumpPtr<AST::Expr>;

  [[nodiscard]] auto parseMultiplicativeExpression() -> BumpPtr<AST::Expr>;

  [[nodiscard]] auto parseUnaryExpression() -> BumpPtr<AST::Expr>;

  [[nodiscard]] auto parsePostfixExpression() -> BumpPtr<AST::Expr>;

  [[nodiscard]] auto parseLetExpression() -> BumpPtr<AST::Expr>;

  [[nodiscard]] auto parseReturnFromExpression() -> BumpPtr<AST::Expr>;

  [[nodiscard]] auto parsePrimaryExpression() -> BumpPtr<AST::Expr>;

  [[nodiscard]] auto parseLiteralExpression() -> BumpPtr<AST::Expr>;

  [[nodiscard]] auto parseLiteralBoolExpression() -> BumpPtr<AST::LiteralBool>;

  [[nodiscard]] auto parseLiteralStringExpression()
      -> BumpPtr<AST::LiteralString>;

  [[nodiscard]] auto parseLiteralNumberExpression() -> BumpPtr<AST::Expr>;

  struct ParsedUnaryOp final {
    std::string_view srcOp{};
    AST::UnaryOp op{};
  };

  [[nodiscard]] auto parseUnaryOp() -> std::optional<ParsedUnaryOp>;

  struct ParsedBinaryOp final {
    std::string_view srcOp{};
    AST::BinaryOp op{};
  };

  [[nodiscard]] auto parseBinaryOp(Span<const AST::BinaryOp> ops)
      -> std::optional<ParsedBinaryOp>;

  template <typename Func>
  [[nodiscard]] auto parseBinaryLeftAssociative(Span<const AST::BinaryOp> ops,
                                                const Func &parseInner)
      -> BumpPtr<AST::Expr> {
    auto exprLhs{parseInner()};
    if (!exprLhs) {
      return nullptr;
    }
    while (true) {
      auto srcLoc0{checkpoint()};
      auto op{parseBinaryOp(ops)};
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
        auto srcBrackL{nextDelimiter("[")};
        auto exprEps{parseUnaryExpression()};
        auto srcBrackR{nextDelimiter("]")};
        if (!srcBrackL || !exprEps || !srcBrackR)
          srcLoc0.throwError("expected '[EPSILON]' after ", Quoted(op->srcOp));
        auto exprRhs{parseInner()};
        if (!exprRhs)
          srcLoc0.throwError("expected '[EPSILON] EXPRESSION' after ",
                             Quoted(op->srcOp));
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
  [[nodiscard]] auto parseBinaryRightAssociative(Span<const AST::BinaryOp> ops,
                                                 const Func &parseInner)
      -> BumpPtr<AST::Expr> {
    auto exprLhs{parseInner()};
    if (!exprLhs)
      return nullptr;
    auto srcLoc0{checkpoint()};
    auto op{parseBinaryOp(ops)};
    if (!op) {
      reject();
      return exprLhs;
    }
    skip();
    auto exprRhs{parseBinaryRightAssociative(ops, parseInner)};
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
  [[nodiscard]] auto parseFile() -> BumpPtr<AST::File>;

  [[nodiscard]] auto parseFileVersion() -> std::optional<AST::File::Version>;

  [[nodiscard]] auto parseImportPath() -> std::optional<AST::ImportPath>;

  [[nodiscard]] auto parseUsingAlias() -> BumpPtr<AST::UsingAlias>;

  [[nodiscard]] auto parseUsingImport() -> BumpPtr<AST::UsingImport>;

  [[nodiscard]] auto parseImport() -> BumpPtr<AST::Import>;

  [[nodiscard]] auto parseAttributes() -> std::optional<AST::Decl::Attributes>;

  [[nodiscard]] auto parseGlobalDeclaration() -> BumpPtr<AST::Decl>;

  [[nodiscard]] auto parseAnnotationDeclaration() -> BumpPtr<AST::Decl>;

  [[nodiscard]] auto parseTypeDeclaration() -> BumpPtr<AST::Decl>;

  [[nodiscard]] auto parseAliasTypeDeclaration() -> BumpPtr<AST::Typedef>;

  [[nodiscard]] auto parseStructTypeDeclaration() -> BumpPtr<AST::Struct>;

  [[nodiscard]] auto parseStructConstructor()
      -> std::optional<AST::Struct::Constructor>;

  [[nodiscard]] auto parseStructFieldDeclarator()
      -> std::optional<AST::Struct::Field>;

  [[nodiscard]] auto parseEnumTypeDeclaration() -> BumpPtr<AST::Enum>;

  [[nodiscard]] auto parseEnumValueDeclarator()
      -> std::optional<AST::Enum::Declarator>;

  [[nodiscard]] auto parseVariableDeclaration() -> BumpPtr<AST::Variable>;

  [[nodiscard]] auto parseVariableDeclarator()
      -> std::optional<AST::Variable::Declarator>;

  [[nodiscard]] auto parseFunctionDeclaration() -> BumpPtr<AST::Function>;

  [[nodiscard]] auto parseTagDeclaration() -> BumpPtr<AST::Tag>;

  [[nodiscard]] auto parseExecDeclaration() -> BumpPtr<AST::Exec>;

  [[nodiscard]] auto parseUnitTestDeclaration() -> BumpPtr<AST::UnitTest>;

  [[nodiscard]] auto parseNamespaceDeclaration() -> BumpPtr<AST::Namespace>;
  //--}

private:
  //--{ Parse: Stmt
  [[nodiscard]] auto parseStatement() -> BumpPtr<AST::Stmt>;

  [[nodiscard]] auto parseCompoundStatement() -> BumpPtr<AST::Compound>;

  [[nodiscard]] auto parseIfStatement() -> BumpPtr<AST::If>;

  [[nodiscard]] auto parseSwitchStatement() -> BumpPtr<AST::Switch>;

  [[nodiscard]] auto parseSwitchCase() -> std::optional<AST::Switch::Case>;

  [[nodiscard]] auto parseWhileStatement() -> BumpPtr<AST::While>;

  [[nodiscard]] auto parseDoStatement() -> BumpPtr<AST::DoWhile>;

  [[nodiscard]] auto parseForStatement() -> BumpPtr<AST::For>;

  [[nodiscard]] auto parseBreakStatement() -> BumpPtr<AST::Break>;

  [[nodiscard]] auto parseContinueStatement() -> BumpPtr<AST::Continue>;

  [[nodiscard]] auto parseReturnStatement() -> BumpPtr<AST::Return>;

  [[nodiscard]] auto parseUnreachableStatement() -> BumpPtr<AST::Unreachable>;

  [[nodiscard]] auto parsePreserveStatement() -> BumpPtr<AST::Preserve>;

  [[nodiscard]] auto parseDeferStatement() -> BumpPtr<AST::Defer>;

  [[nodiscard]] auto parseVisitStatement() -> BumpPtr<AST::Visit>;

  [[nodiscard]] auto parseLateIf() -> std::optional<AST::LateIf>;
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
