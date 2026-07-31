/// \file
// vim:foldmethod=marker:foldlevel=0:fmr=--{,--}
#pragma once

#include <array>
#include <functional>
#include <initializer_list>
#include <type_traits>
#include <utility>

#include "smdl/AST.h"
#include "smdl/Module.h"

namespace smdl {

/// \addtogroup compiler
/// \{

/// The parser.
class SMDL_EXPORT Parser final {
public:
  explicit Parser(BumpPtrAllocator &allocator, Module &module_,
                  bool isSMDL = false)
      : mAllocator(allocator), mModule(module_), mIsSMDL(isSMDL) {
    mSrcLoc.module_ = &module_;
  }

  /// Non-copyable and non-movable.
  Parser(const Parser &) = delete;

  /// Parse everything.
  [[nodiscard]] BumpPtr<AST::File> parse() { return parseFile(); }

private:
  //--{ Basics
  [[nodiscard]] bool isEOF() const {
    return mSrcLoc.i >= getSourceCode().size();
  }

  [[nodiscard]] std::string_view getSourceCode() const {
    return mModule.getSourceCode();
  }

  [[nodiscard]] std::string_view
  getSourceCodeBetween(const SourceLocation &srcLoc0,
                       const SourceLocation &srcLoc1) const {
    return getSourceCode().substr(srcLoc0.i, srcLoc1.i - srcLoc0.i);
  }

  [[nodiscard]] std::string_view getRemainingSourceCode() const {
    return getSourceCode().substr(mSrcLoc.i);
  }

  [[nodiscard]] char peek() const {
    if (isEOF()) return '\0';
    return getSourceCode()[mSrcLoc.i];
  }

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
      if (auto result{nextKeyword(str)}) return result;
    return std::nullopt;
  }

  /// The result of `nextKeywordAndLocation()` or `nextDelimiterAndLocation()`:
  /// the parsed source code and the source location where it begins.
  struct ParsedToken final {
    /// The source location.
    SourceLocation srcLoc{};

    /// The source code.
    std::string_view src{};
  };

  /// Skip whitespace and comments, then parse the given keyword, also
  /// capturing the source location at the start of the keyword. This is the
  /// common prologue of most declaration and statement parse functions.
  [[nodiscard]] std::optional<ParsedToken>
  nextKeywordAndLocation(std::string_view str) {
    skip();
    auto srcLoc{mSrcLoc};
    if (auto srcKw{nextKeyword(str)}) return ParsedToken{srcLoc, *srcKw};
    return std::nullopt;
  }

  /// Same as `nextKeywordAndLocation()`, but for delimiters.
  [[nodiscard]] std::optional<ParsedToken>
  nextDelimiterAndLocation(std::string_view str) {
    skip();
    auto srcLoc{mSrcLoc};
    if (auto src{next(str)}) return ParsedToken{srcLoc, *src};
    return std::nullopt;
  }

  /// Unwrap an optional source string, defaulting to the empty string.
  [[nodiscard]] static std::string_view
  orEmpty(const std::optional<std::string_view> &src) {
    return src ? *src : std::string_view();
  }

  [[nodiscard]] std::optional<std::string_view> nextWord();

  [[nodiscard]] std::optional<std::string_view> nextInteger();

  void skip();

  /// Get the pending documentation comment block most recently scanned
  /// by `skip()`. This may be empty!
  [[nodiscard]] std::string_view getPendingDocComment() const {
    return getSourceCode().substr(mPendingDocCommentBegin,
                                  mPendingDocCommentEnd -
                                      mPendingDocCommentBegin);
  }

  /// Get the pending documentation comment block if it attaches to the
  /// given source index, meaning it is separated from it by whitespace
  /// containing at most one newline. This may be empty!
  [[nodiscard]] std::string_view getDocCommentBefore(size_t srcIndex) const;

  /// Does the pending trailing documentation comment (`///<`) trail
  /// source code on its line? If not, it is a stray `///<` on a line of
  /// its own, which attaches to nothing.
  [[nodiscard]] bool pendingTrailingDocCommentTrailsCode() const;

  /// The trait to detect item types that can hold a trailing
  /// documentation comment.
  template <typename Item, typename = void>
  struct HasTrailingDocComment : std::false_type {};
  template <typename Item>
  struct HasTrailingDocComment<
      Item, std::void_t<decltype(std::declval<Item &>().srcDocCommentTrailing)>>
      : std::true_type {};

  /// Attach the pending trailing documentation comment (`///<`) to the
  /// last item in `items` if it belongs to it: the comment must lie
  /// strictly between the start of the item and the current scan
  /// position (so a stale comment left behind by a rejected parse
  /// further ahead can never attach), and must trail source code on its
  /// line. Attaching consumes the pending comment; this is safe under
  /// backtracking because a rewind that re-scans the comment also
  /// re-records it.
  template <typename Item>
  void attachPendingTrailingDocComment(std::vector<Item> &items) {
    if (mPendingTrailingDocCommentBegin == mPendingTrailingDocCommentEnd ||
        items.empty())
      return;
    if (!(items.back().srcLoc.i < mPendingTrailingDocCommentBegin &&
          mPendingTrailingDocCommentEnd <= mSrcLoc.i) ||
        !pendingTrailingDocCommentTrailsCode())
      return;
    items.back().srcDocCommentTrailing = getSourceCode().substr(
        mPendingTrailingDocCommentBegin,
        mPendingTrailingDocCommentEnd - mPendingTrailingDocCommentBegin);
    mPendingTrailingDocCommentBegin = mPendingTrailingDocCommentEnd = 0;
  }

  SourceLocation checkpoint() {
    skip();
    mSrcLocStack.push_back(mSrcLoc);
    return mSrcLoc;
  }

  void accept() {
    SMDL_SANITY_CHECK(mSrcLocStack.size() >= 1);
    mSrcLocStack.pop_back();
  }

  void reject() {
    SMDL_SANITY_CHECK(mSrcLocStack.size() >= 1);
    mSrcLoc = mSrcLocStack.back(), mSrcLocStack.pop_back();
  }

  /// The maximum recursion depth of `parseUnaryExpression()` and
  /// `parseStatement()`, so pathologically nested inputs fail with a parse
  /// error instead of overflowing the native stack.
  static constexpr int MAX_PARSE_DEPTH = 256;

  /// The RAII guard that enforces `MAX_PARSE_DEPTH`.
  struct ParseDepthGuard final {
    explicit ParseDepthGuard(Parser &parser) : mParser(parser) {
      if (mParser.mParseDepth >= MAX_PARSE_DEPTH)
        mParser.mSrcLoc.throwError("nesting exceeds maximum parse depth");
      ++mParser.mParseDepth;
    }

    ~ParseDepthGuard() { --mParser.mParseDepth; }

    Parser &mParser;
  };

  /// Parse a comma-separated list of items into `items`.
  ///
  /// The `parseItem` callback must return an optional-like value of the item
  /// type, which must have a `srcComma` member. The list is lenient in that
  /// it terminates without an error at the first item that fails to parse,
  /// so the caller is responsible for verifying that the relevant closing
  /// delimiter follows. If `srcCloser` is non-empty, the list also
  /// terminates if it appears after a comma, e.g., to allow a trailing
  /// comma before the `]]` that closes an annotation block.
  template <typename Item, typename ParseItem>
  void parseCommaSeparated(std::vector<Item> &items, const ParseItem &parseItem,
                           std::string_view srcCloser = {}) {
    while (true) {
      skip();
      if constexpr (HasTrailingDocComment<Item>::value)
        attachPendingTrailingDocComment(items);
      auto item{parseItem()};
      if (!item) break;
      items.push_back(std::move(*item));
      auto srcComma{nextDelimiter(",")};
      if (!srcComma) break;
      items.back().srcComma = *srcComma;
      if (!srcCloser.empty()) {
        skip();
        if (startsWith(getRemainingSourceCode(), srcCloser)) break;
      }
    }
    // Every loop exit is preceded by a `skip()`, so a trailing comment
    // after the last item has already been scanned.
    if constexpr (HasTrailingDocComment<Item>::value)
      attachPendingTrailingDocComment(items);
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

      // If parsing an approximate comparison operator `~==` or `~!=`, then
      // also parse the epsilon after the operator and before the right-hand
      // side expression: `|EPSILON|` is an absolute tolerance, `(EPSILON)`
      // is a relative tolerance. This is extended syntax!
      if (op->op == BINOP_APPROX_CMP_EQ || //
          op->op == BINOP_APPROX_CMP_NE) {
        auto srcDelimL{nextDelimiter("|")};
        auto exprEps{BumpPtr<AST::Expr>{}};
        auto srcDelimR{std::optional<std::string_view>{}};
        if (srcDelimL) {
          exprEps = parseUnaryExpression();
          srcDelimR = nextDelimiter("|");
        } else {
          srcDelimL = nextDelimiter("(");
          if (srcDelimL) {
            exprEps = parseExpression();
            srcDelimR = nextDelimiter(")");
          } else if (nextDelimiter("[")) {
            srcLoc0.throwError(
                "'[EPSILON]' syntax was replaced: use '|EPSILON|' for "
                "absolute or '(EPSILON)' for relative tolerance after ",
                Quoted(op->srcOp));
          }
        }
        if (!srcDelimL || !exprEps || !srcDelimR)
          srcLoc0.throwError("expected '|EPSILON|' or '(EPSILON)' after ",
                             Quoted(op->srcOp));
        auto exprRhs{parseInner()};
        if (!exprRhs)
          srcLoc0.throwError("expected 'EPSILON EXPRESSION' after ",
                             Quoted(op->srcOp));
        accept();
        exprLhs = allocate<AST::Binary>(
            srcLoc0, std::in_place, std::move(exprLhs), op->srcOp, op->op,
            *srcDelimL, std::move(exprEps), *srcDelimR, std::move(exprRhs));
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
    if (!exprLhs) return nullptr;
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

  /// Parse a `break` or `continue` statement, which are identical except
  /// for the keyword and the AST node type.
  template <typename Node>
  [[nodiscard]] BumpPtr<Node> parseJumpStatement(std::string_view keyword) {
    auto kw{nextKeywordAndLocation(keyword)};
    if (!kw) return nullptr;
    auto lateIf{parseLateIf()};
    auto srcSemicolon{nextDelimiter(";")};
    if (!srcSemicolon)
      kw->srcLoc.throwError("expected ';' after ", Quoted(keyword));
    return allocate<Node>(kw->srcLoc, std::in_place, kw->src, std::move(lateIf),
                          *srcSemicolon);
  }

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
  BumpPtrAllocator &mAllocator;

  Module &mModule;

  SourceLocation mSrcLoc{};

  /// The checkpoint stack for backtracking. NOTE: A thrown parse error may
  /// leave entries behind. This is fine because each `Parser` is discarded
  /// after a single call to `parse()`, but do not reuse a `Parser` after
  /// catching a parse error!
  std::vector<SourceLocation> mSrcLocStack;

  /// The begin index of the pending documentation comment block
  /// (consecutive `///` lines) most recently scanned by `skip()`. Equal
  /// to `mPendingDocCommentEnd` if there is none.
  ///
  /// NOTE: This is a pure function of how far `skip()` has scanned, so
  /// the backtracking in `checkpoint()`/`reject()` needs no save and
  /// restore logic: after a rewind, the next `skip()` over the same
  /// region recomputes identical state, and a stale block from an
  /// earlier region fails the whitespace adjacency test in
  /// `getDocCommentBefore()`.
  size_t mPendingDocCommentBegin{};

  /// The end index of the pending documentation comment block, see
  /// `mPendingDocCommentBegin`.
  size_t mPendingDocCommentEnd{};

  /// The begin index of the pending trailing documentation comment
  /// (`///<`) most recently scanned by `skip()`. Equal to
  /// `mPendingTrailingDocCommentEnd` if there is none. The same
  /// backtracking reasoning as `mPendingDocCommentBegin` applies, plus
  /// the position guards in `attachPendingTrailingDocComment()`.
  size_t mPendingTrailingDocCommentBegin{};

  /// The end index of the pending trailing documentation comment, see
  /// `mPendingTrailingDocCommentBegin`.
  size_t mPendingTrailingDocCommentEnd{};

  /// The current recursion depth, see `ParseDepthGuard`.
  int mParseDepth{};

  bool mIsSMDL{};

  template <typename T, typename... Args>
  [[nodiscard]] BumpPtr<T> allocate(SourceLocation srcLoc, std::in_place_t,
                                    Args &&...args) {
    auto result{mAllocator.allocate<T>(std::forward<Args>(args)...)};
    result->srcLoc = srcLoc;
    return result;
  }
};

/// \}

} // namespace smdl
