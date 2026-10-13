/// \file
// vim:foldmethod=marker:foldlevel=0:fmr=--{,--}
#pragma once

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
  explicit Parser(BumpPtrAllocator &allocator, Module &module,
                  bool isSMDL = false)
      : allocator_(allocator), module_(module), isSMDL_(isSMDL) {
    srcLoc_.module = &module;
  }

  /// Non-copyable and non-movable.
  Parser(const Parser &) = delete;

  /// Parse everything.
  [[nodiscard]] BumpPtr<ast::File> parse() { return parseFile_(); }

private:
  //--{ Basics
  [[nodiscard]] bool isEOF_() const {
    return srcLoc_.i >= getSourceCode_().size();
  }

  [[nodiscard]] std::string_view getSourceCode_() const {
    return module_.getSourceCode();
  }

  [[nodiscard]] std::string_view
  getSourceCodeBetween_(const SourceLocation &srcLoc0,
                        const SourceLocation &srcLoc1) const {
    return getSourceCode_().substr(srcLoc0.i, srcLoc1.i - srcLoc0.i);
  }

  [[nodiscard]] std::string_view getRemainingSourceCode_() const {
    return getSourceCode_().substr(srcLoc_.i);
  }

  [[nodiscard]] char peek_() const {
    if (isEOF_()) return '\0';
    return getSourceCode_()[srcLoc_.i];
  }

  char next_();

  std::string_view next_(size_t n);

  [[nodiscard]] std::optional<std::string_view> next_(std::string_view str);

  [[nodiscard]] std::optional<std::string_view>
  nextDelimiter_(std::string_view str) {
    skip_();
    return next_(str);
  }

  [[nodiscard]] std::optional<std::string_view>
  nextKeyword_(std::string_view str);

  [[nodiscard]] std::optional<std::string_view>
  nextKeyword_(std::initializer_list<std::string_view> strs) {
    for (auto str : strs)
      if (std::optional<std::string_view> result{nextKeyword_(str)})
        return result;
    return std::nullopt;
  }

  /// The result of `nextKeywordAndLocation_()` or
  /// `nextDelimiterAndLocation_()`: the parsed source code and the source
  /// location where it begins.
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
  nextKeywordAndLocation_(std::string_view str) {
    skip_();
    SourceLocation srcLoc{srcLoc_};
    if (std::optional<std::string_view> srcKw{nextKeyword_(str)})
      return ParsedToken{srcLoc, *srcKw};
    return std::nullopt;
  }

  /// Same as `nextKeywordAndLocation_()`, but for delimiters.
  [[nodiscard]] std::optional<ParsedToken>
  nextDelimiterAndLocation_(std::string_view str) {
    skip_();
    SourceLocation srcLoc{srcLoc_};
    if (std::optional<std::string_view> src{next_(str)})
      return ParsedToken{srcLoc, *src};
    return std::nullopt;
  }

  /// Unwrap an optional source string, defaulting to the empty string.
  [[nodiscard]] static std::string_view
  orEmpty_(const std::optional<std::string_view> &src) {
    return src ? *src : std::string_view();
  }

  [[nodiscard]] std::optional<std::string_view> nextWord_();

  [[nodiscard]] std::optional<std::string_view> nextInteger_();

  void skip_();

  /// The token beginning at the given index in the source code, without
  /// moving the parser. Returns the empty view past the end.
  [[nodiscard]] std::string_view tokenAt_(uint64_t i) const;

  /// The next token as source text, for naming what the parser choked on.
  /// Skips whitespace and comments first, so this must only be used where
  /// the parser is about to throw. Returns the empty view at end of file.
  [[nodiscard]] std::string_view peekToken_();

  /// The explanation for a token the parser cannot use here, either
  /// because it belongs to another language or because it is a
  /// SpectralMDL extension in a file that did not opt in. Returns a clause
  /// ready to append to a message, or the empty string when there is
  /// nothing useful to say.
  [[nodiscard]] std::string explainToken_(std::string_view token) const;

  /// Throw `message`, naming the token that stopped the parser and
  /// explaining it when there is anything to explain. `srcLocStart`, when
  /// given, is where the construct that failed began: a borrowed keyword
  /// is usually consumed as a name long before the parse gives up, so
  /// `class Foo {` stops at the brace while `class` is the real mistake.
  [[noreturn]] void
  throwUnexpectedToken_(const SourceLocation &srcLoc, std::string_view message,
                        const SourceLocation *srcLocStart = nullptr);

  /// Get the pending documentation comment block most recently scanned
  /// by `skip_()`. This may be empty!
  [[nodiscard]] std::string_view getPendingDocComment_() const {
    return getSourceCode_().substr(pendingDocCommentBegin_,
                                   pendingDocCommentEnd_ -
                                       pendingDocCommentBegin_);
  }

  /// Get the pending documentation comment block if it attaches to the
  /// given source index, meaning it is separated from it by whitespace
  /// containing at most one newline. This may be empty!
  [[nodiscard]] std::string_view getDocCommentBefore_(size_t srcIndex) const;

  /// Does the pending trailing documentation comment (`///<`) trail
  /// source code on its line? If not, it is a stray `///<` on a line of
  /// its own, which attaches to nothing.
  [[nodiscard]] bool pendingTrailingDocCommentTrailsCode_() const;

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
  void attachPendingTrailingDocComment_(std::vector<Item> &items) {
    if (pendingTrailingDocCommentBegin_ == pendingTrailingDocCommentEnd_ ||
        items.empty())
      return;
    if (!(items.back().srcLoc.i < pendingTrailingDocCommentBegin_ &&
          pendingTrailingDocCommentEnd_ <= srcLoc_.i) ||
        !pendingTrailingDocCommentTrailsCode_())
      return;
    items.back().srcDocCommentTrailing = getSourceCode_().substr(
        pendingTrailingDocCommentBegin_,
        pendingTrailingDocCommentEnd_ - pendingTrailingDocCommentBegin_);
    pendingTrailingDocCommentBegin_ = pendingTrailingDocCommentEnd_ = 0;
  }

  SourceLocation checkpoint_() {
    skip_();
    srcLocStack_.push_back(srcLoc_);
    return srcLoc_;
  }

  void accept_() {
    SMDL_SANITY_CHECK(srcLocStack_.size() >= 1);
    srcLocStack_.pop_back();
  }

  void reject_() {
    SMDL_SANITY_CHECK(srcLocStack_.size() >= 1);
    srcLoc_ = srcLocStack_.back(), srcLocStack_.pop_back();
  }

  /// The maximum recursion depth of `parseUnaryExpression_()` and
  /// `parseStatement_()`, so pathologically nested inputs fail with a parse
  /// error instead of overflowing the native stack.
  static constexpr int MAX_PARSE_DEPTH = 256;

  /// The RAII guard that enforces `MAX_PARSE_DEPTH`.
  class ParseDepthGuard final {
  public:
    explicit ParseDepthGuard(Parser &parser) : parser_(parser) {
      if (parser_.parseDepth_ >= MAX_PARSE_DEPTH)
        parser_.srcLoc_.throwError("nesting exceeds maximum parse depth");
      ++parser_.parseDepth_;
    }

    ~ParseDepthGuard() { --parser_.parseDepth_; }

  private:
    Parser &parser_;
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
  void parseCommaSeparated_(std::vector<Item> &items,
                            const ParseItem &parseItem,
                            std::string_view srcCloser = {}) {
    while (true) {
      skip_();
      if constexpr (HasTrailingDocComment<Item>::value)
        attachPendingTrailingDocComment_(items);
      auto item{parseItem()};
      if (!item) break;
      items.push_back(std::move(*item));
      std::optional<std::string_view> srcComma{nextDelimiter_(",")};
      if (!srcComma) break;
      items.back().srcComma = *srcComma;
      if (!srcCloser.empty()) {
        skip_();
        if (startsWith(getRemainingSourceCode_(), srcCloser)) break;
      }
    }
    // Every loop exit is preceded by a `skip_()`, so a trailing comment
    // after the last item has already been scanned.
    if constexpr (HasTrailingDocComment<Item>::value)
      attachPendingTrailingDocComment_(items);
  }
  //--}

private:
  //--{ Parse: Expr
  [[nodiscard]] auto parseSimpleName_() -> std::optional<ast::Name>;

  [[nodiscard]] auto parseIdentifier_() -> BumpPtr<ast::Identifier>;

  [[nodiscard]] auto parseType_() -> BumpPtr<ast::Type>;

  [[nodiscard]] auto parseParameter_() -> std::optional<ast::Parameter>;

  [[nodiscard]] auto parseParameterList_() -> std::optional<ast::ParameterList>;

  [[nodiscard]] auto parseArgument_() -> std::optional<ast::Argument>;

  [[nodiscard]] auto parseArgumentList_() -> std::optional<ast::ArgumentList>;

  [[nodiscard]] auto parseAnnotation_() -> std::optional<ast::Annotation>;

  [[nodiscard]] auto parseAnnotationBlock_() -> BumpPtr<ast::AnnotationBlock>;

  [[nodiscard]] auto parseExpressionInParentheses_() -> BumpPtr<ast::Expr>;

  [[nodiscard]] auto parseExpression_() -> BumpPtr<ast::Expr>;

  [[nodiscard]] auto parseAssignmentExpression_() -> BumpPtr<ast::Expr>;

  [[nodiscard]] auto parseElseExpression_() -> BumpPtr<ast::Expr>;

  [[nodiscard]] auto parseConditionalExpression_() -> BumpPtr<ast::Expr>;

  [[nodiscard]] auto parseLogicalOrExpression_() -> BumpPtr<ast::Expr>;

  [[nodiscard]] auto parseLogicalAndExpression_() -> BumpPtr<ast::Expr>;

  [[nodiscard]] auto parseInclusiveOrExpression_() -> BumpPtr<ast::Expr>;

  [[nodiscard]] auto parseExclusiveOrExpression_() -> BumpPtr<ast::Expr>;

  [[nodiscard]] auto parseAndExpression_() -> BumpPtr<ast::Expr>;

  [[nodiscard]] auto parseEqualityExpression_() -> BumpPtr<ast::Expr>;

  [[nodiscard]] auto parseRelationalExpression_() -> BumpPtr<ast::Expr>;

  [[nodiscard]] auto parseShiftExpression_() -> BumpPtr<ast::Expr>;

  [[nodiscard]] auto parseAdditiveExpression_() -> BumpPtr<ast::Expr>;

  [[nodiscard]] auto parseMultiplicativeExpression_() -> BumpPtr<ast::Expr>;

  [[nodiscard]] auto parseUnaryExpression_() -> BumpPtr<ast::Expr>;

  [[nodiscard]] auto parsePostfixExpression_() -> BumpPtr<ast::Expr>;

  [[nodiscard]] auto parseLambdaExpression_() -> BumpPtr<ast::Expr>;

  [[nodiscard]] auto parseLetExpression_() -> BumpPtr<ast::Expr>;

  [[nodiscard]] auto parseReturnFromExpression_() -> BumpPtr<ast::Expr>;

  [[nodiscard]] auto parsePrimaryExpression_() -> BumpPtr<ast::Expr>;

  [[nodiscard]] auto parseLiteralExpression_() -> BumpPtr<ast::Expr>;

  [[nodiscard]] auto parseLiteralBoolExpression_() -> BumpPtr<ast::LiteralBool>;

  [[nodiscard]] auto parseLiteralStringExpression_()
      -> BumpPtr<ast::LiteralString>;

  /// Parse a unicode identifier, as `'ünï'`, which names a module or a
  /// package that is not a valid identifier. It takes the escapes of a
  /// literal string, and is not joined to one after it.
  [[nodiscard]] auto parseUnicodeIdentifier_() -> BumpPtr<ast::LiteralString>;

  [[nodiscard]] auto parseQuoted_(char quote) -> BumpPtr<ast::LiteralString>;

  [[nodiscard]] auto parseLiteralNumberExpression_() -> BumpPtr<ast::Expr>;

  struct ParsedUnaryOp final {
    std::string_view srcOp{};
    ast::UnaryOp op{};
  };

  [[nodiscard]] auto parseUnaryOp_() -> std::optional<ParsedUnaryOp>;

  struct ParsedBinaryOp final {
    std::string_view srcOp{};
    ast::BinaryOp op{};
  };

  [[nodiscard]] auto parseBinaryOp_(Span<const ast::BinaryOp> ops)
      -> std::optional<ParsedBinaryOp>;

  template <typename Func>
  [[nodiscard]] auto parseBinaryLeftAssociative_(Span<const ast::BinaryOp> ops,
                                                 const Func &parseInner)
      -> BumpPtr<ast::Expr> {
    BumpPtr<ast::Expr> exprLHS{parseInner()};
    if (!exprLHS) {
      return nullptr;
    }
    while (true) {
      SourceLocation srcLoc0{checkpoint_()};
      std::optional<Parser::ParsedBinaryOp> op{parseBinaryOp_(ops)};
      if (!op) {
        reject_();
        break;
      }
      skip_();

      // If parsing an approximate comparison operator `~==` or `~!=`, then
      // also parse the epsilon after the operator and before the right-hand
      // side expression: `|EPSILON|` is an absolute tolerance, `(EPSILON)`
      // is a relative tolerance. This is extended syntax!
      if (op->op == BINOP_APPROX_CMP_EQ || //
          op->op == BINOP_APPROX_CMP_NE) {
        std::optional<std::string_view> srcDelimL{nextDelimiter_("|")};
        BumpPtr<ast::Expr> exprEps{};
        std::optional<std::string_view> srcDelimR{};
        if (srcDelimL) {
          exprEps = parseUnaryExpression_();
          srcDelimR = nextDelimiter_("|");
        } else {
          srcDelimL = nextDelimiter_("(");
          if (srcDelimL) {
            exprEps = parseExpression_();
            srcDelimR = nextDelimiter_(")");
          } else if (nextDelimiter_("[")) {
            srcLoc0.throwError(
                "'[EPSILON]' syntax was replaced: use '|EPSILON|' for "
                "absolute or '(EPSILON)' for relative tolerance after ",
                SpellQuoted(op->srcOp));
          }
        }
        if (!srcDelimL || !exprEps || !srcDelimR)
          srcLoc0.throwError("expected '|EPSILON|' or '(EPSILON)' after ",
                             SpellQuoted(op->srcOp));
        BumpPtr<ast::Expr> exprRHS{parseInner()};
        if (!exprRHS)
          srcLoc0.throwError("expected 'EPSILON EXPRESSION' after ",
                             SpellQuoted(op->srcOp));
        accept_();
        exprLHS = allocate_<ast::Binary>(
            srcLoc0, std::in_place, std::move(exprLHS), op->srcOp, op->op,
            *srcDelimL, std::move(exprEps), *srcDelimR, std::move(exprRHS));
        continue;
      }

      BumpPtr<ast::Expr> exprRHS{parseInner()};
      if (!exprRHS) {
        reject_();
        break;
      } else {
        accept_();
        exprLHS =
            allocate_<ast::Binary>(srcLoc0, std::in_place, std::move(exprLHS),
                                   op->srcOp, op->op, std::move(exprRHS));
      }
    }
    return exprLHS;
  }

  template <typename Func>
  [[nodiscard]] auto parseBinaryRightAssociative_(Span<const ast::BinaryOp> ops,
                                                  const Func &parseInner)
      -> BumpPtr<ast::Expr> {
    BumpPtr<ast::Expr> exprLHS{parseInner()};
    if (!exprLHS) return nullptr;
    SourceLocation srcLoc0{checkpoint_()};
    std::optional<Parser::ParsedBinaryOp> op{parseBinaryOp_(ops)};
    if (!op) {
      reject_();
      return exprLHS;
    }
    skip_();
    BumpPtr<ast::Expr> exprRHS{parseBinaryRightAssociative_(ops, parseInner)};
    if (!exprRHS) {
      reject_();
      return exprLHS;
    } else {
      accept_();
      return allocate_<ast::Binary>(srcLoc0, std::in_place, std::move(exprLHS),
                                    op->srcOp, op->op, std::move(exprRHS));
    }
  }
  //--}

private:
  //--{ Parse: Decls
  [[nodiscard]] auto parseFile_() -> BumpPtr<ast::File>;

  [[nodiscard]] auto parseFileSearchDirs_()
      -> std::vector<ast::File::SearchDir>;

  [[nodiscard]] auto parseFileVersion_() -> std::optional<ast::File::Version>;

  [[nodiscard]] auto parseImportPath_() -> std::optional<ast::ImportPath>;

  [[nodiscard]] auto parseUsingAlias_() -> BumpPtr<ast::UsingAlias>;

  [[nodiscard]] auto parseUsingImport_() -> BumpPtr<ast::UsingImport>;

  [[nodiscard]] auto parseImport_() -> BumpPtr<ast::Import>;

  [[nodiscard]] auto parseAttributes_() -> std::optional<ast::Decl::Attributes>;

  [[nodiscard]] auto parseGlobalDeclaration_() -> BumpPtr<ast::Decl>;

  [[nodiscard]] auto parseAnnotationDeclaration_() -> BumpPtr<ast::Decl>;

  [[nodiscard]] auto parseTypeDeclaration_() -> BumpPtr<ast::Decl>;

  [[nodiscard]] auto parseAliasTypeDeclaration_() -> BumpPtr<ast::Typedef>;

  [[nodiscard]] auto parseStructTypeDeclaration_() -> BumpPtr<ast::Struct>;

  [[nodiscard]] auto parseStructCategoryDeclaration_()
      -> BumpPtr<ast::StructCategory>;

  [[nodiscard]] auto parseStructConstructor_()
      -> std::optional<ast::Struct::Constructor>;

  [[nodiscard]] auto parseStructFieldDeclarator_()
      -> std::optional<ast::Struct::Field>;

  [[nodiscard]] auto parseEnumTypeDeclaration_() -> BumpPtr<ast::Enum>;

  [[nodiscard]] auto parseEnumValueDeclarator_()
      -> std::optional<ast::Enum::Declarator>;

  [[nodiscard]] auto parseVariableDeclaration_() -> BumpPtr<ast::Variable>;

  [[nodiscard]] auto parseVariableDeclarator_()
      -> std::optional<ast::Variable::Declarator>;

  [[nodiscard]] auto parseFunctionDeclaration_() -> BumpPtr<ast::Function>;

  [[nodiscard]] auto parseTagDeclaration_() -> BumpPtr<ast::Tag>;

  [[nodiscard]] auto parseExecDeclaration_() -> BumpPtr<ast::Exec>;

  [[nodiscard]] auto parseUnitTestDeclaration_() -> BumpPtr<ast::UnitTest>;

  [[nodiscard]] auto parseNamespaceDeclaration_() -> BumpPtr<ast::Namespace>;
  //--}

private:
  //--{ Parse: Stmt
  [[nodiscard]] auto parseStatement_() -> BumpPtr<ast::Stmt>;

  [[nodiscard]] auto parseCompoundStatement_() -> BumpPtr<ast::Compound>;

  [[nodiscard]] auto parseIfStatement_() -> BumpPtr<ast::If>;

  [[nodiscard]] auto parseSwitchStatement_() -> BumpPtr<ast::Switch>;

  [[nodiscard]] auto parseSwitchCase_() -> std::optional<ast::Switch::Case>;

  [[nodiscard]] auto parseWhileStatement_() -> BumpPtr<ast::While>;

  [[nodiscard]] auto parseDoStatement_() -> BumpPtr<ast::DoWhile>;

  [[nodiscard]] auto parseForStatement_() -> BumpPtr<ast::For>;

  /// Parse a `break` or `continue` statement, which are identical except
  /// for the keyword and the AST node type.
  template <typename Node>
  [[nodiscard]] BumpPtr<Node> parseJumpStatement_(std::string_view keyword) {
    std::optional<Parser::ParsedToken> kw{nextKeywordAndLocation_(keyword)};
    if (!kw) return nullptr;
    std::optional<ast::LateIf> lateIf{parseLateIf_()};
    std::optional<std::string_view> srcSemicolon{nextDelimiter_(";")};
    if (!srcSemicolon)
      kw->srcLoc.throwError("expected ';' after ", SpellQuoted(keyword));
    return allocate_<Node>(kw->srcLoc, std::in_place, kw->src,
                           std::move(lateIf), *srcSemicolon);
  }

  [[nodiscard]] auto parseBreakStatement_() -> BumpPtr<ast::Break>;

  [[nodiscard]] auto parseContinueStatement_() -> BumpPtr<ast::Continue>;

  [[nodiscard]] auto parseReturnStatement_() -> BumpPtr<ast::Return>;

  [[nodiscard]] auto parseUnreachableStatement_() -> BumpPtr<ast::Unreachable>;

  [[nodiscard]] auto parsePreserveStatement_() -> BumpPtr<ast::Preserve>;

  [[nodiscard]] auto parseDeferStatement_() -> BumpPtr<ast::Defer>;

  [[nodiscard]] auto parseVisitStatement_() -> BumpPtr<ast::Visit>;

  [[nodiscard]] auto parseLateIf_() -> std::optional<ast::LateIf>;
  //--}

private:
  BumpPtrAllocator &allocator_;

  Module &module_;

  SourceLocation srcLoc_{};

  /// The checkpoint stack for backtracking. NOTE: A thrown parse error may
  /// leave entries behind. This is fine because each `Parser` is discarded
  /// after a single call to `parse()`, but do not reuse a `Parser` after
  /// catching a parse error!
  std::vector<SourceLocation> srcLocStack_;

  /// The begin index of the pending documentation comment block
  /// (consecutive `///` lines) most recently scanned by `skip_()`. Equal
  /// to `pendingDocCommentEnd_` if there is none.
  ///
  /// NOTE: This is a pure function of how far `skip_()` has scanned, so
  /// the backtracking in `checkpoint_()`/`reject_()` needs no save and
  /// restore logic: after a rewind, the next `skip_()` over the same
  /// region recomputes identical state, and a stale block from an
  /// earlier region fails the whitespace adjacency test in
  /// `getDocCommentBefore_()`.
  size_t pendingDocCommentBegin_{};

  /// The end index of the pending documentation comment block, see
  /// `pendingDocCommentBegin_`.
  size_t pendingDocCommentEnd_{};

  /// The begin index of the pending trailing documentation comment
  /// (`///<`) most recently scanned by `skip_()`. Equal to
  /// `pendingTrailingDocCommentEnd_` if there is none. The same
  /// backtracking reasoning as `pendingDocCommentBegin_` applies, plus
  /// the position guards in `attachPendingTrailingDocComment_()`.
  size_t pendingTrailingDocCommentBegin_{};

  /// The end index of the pending trailing documentation comment, see
  /// `pendingTrailingDocCommentBegin_`.
  size_t pendingTrailingDocCommentEnd_{};

  /// The current recursion depth, see `ParseDepthGuard`.
  int parseDepth_{};

  bool isSMDL_{};

  template <typename T, typename... Args>
  [[nodiscard]] BumpPtr<T> allocate_(SourceLocation srcLoc, std::in_place_t,
                                     Args &&...args) {
    auto result{allocator_.allocate<T>(std::forward<Args>(args)...)};
    result->srcLoc = srcLoc;
    return result;
  }
};

/// \}

} // namespace smdl
