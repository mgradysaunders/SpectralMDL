#include "doctest.h"

#include "smdl/AST.h"
#include "smdl/Module.h"

using namespace smdl;

namespace {

// The helper that owns the allocator and module backing a parsed AST.
class ParsedModule final {
public:
  explicit ParsedModule(std::string sourceCode)
      : mModule("test", std::move(sourceCode)) {
    auto error{mModule.parse(mAllocator)};
    REQUIRE(!error);
  }

  [[nodiscard]] const AST::File &root() const { return *mModule.getRoot(); }

  [[nodiscard]] const AST::Decl &decl(size_t i) const {
    REQUIRE(i < root().globalDecls.size());
    return *root().globalDecls[i];
  }

  template <typename T> [[nodiscard]] const T &declAs(size_t i) const {
    const auto *declT{static_cast<const T *>(&decl(i))};
    REQUIRE(T::classof(&decl(i)));
    return *declT;
  }

private:
  BumpPtrAllocator mAllocator{};

  Module mModule;
};

[[nodiscard]] static std::string docText(std::string_view srcDocComment) {
  return AST::getDocCommentText(srcDocComment);
}

// Parse source code that is expected to fail and return the error message,
// which is prefixed by the source location '[<string ::test>:LINE:COLUMN]'.
[[nodiscard]] static std::string parseError(std::string sourceCode) {
  auto allocator{BumpPtrAllocator{}};
  auto module{Module("test", std::move(sourceCode))};
  auto error{module.parse(allocator)};
  REQUIRE(error);
  return error->message;
}

[[nodiscard]] static bool contains(std::string_view str, std::string_view sub) {
  return str.find(sub) != str.npos;
}

} // namespace

TEST_CASE("Parser doc comments") {
  SUBCASE("getDocCommentText") {
    CHECK(docText("") == "");
    CHECK(docText("/// Hello") == "Hello");
    CHECK(docText("///No space") == "No space");
    CHECK(docText("///< Trailing") == "Trailing");
    CHECK(docText("/// Line one\n  /// Line two  ") == "Line one\nLine two");
    CHECK(docText("/// A\n///\n/// B") == "A\n\nB");
    CHECK(docText("///\n/// After blank") == "After blank");
  }
  SUBCASE("global declarations") {
    auto parsed{ParsedModule(R"(#smdl
/// The documented constant.
const int X = 1;

/// The documented function,
/// on two lines.
@(pure macro)
export auto foo(int a) = a;

const int UNDOCUMENTED = 0;
)")};
    REQUIRE(parsed.root().globalDecls.size() == 3);
    CHECK(docText(parsed.decl(0).srcDocComment) == "The documented constant.");
    CHECK(docText(parsed.decl(1).srcDocComment) ==
          "The documented function,\non two lines.");
    CHECK(parsed.decl(2).srcDocComment.empty());
  }
  SUBCASE("blank lines and ordinary comments break attachment") {
    auto parsed{ParsedModule(R"(#smdl
/// Not attached, blank line follows.

const int A = 0;
/// Not attached, ordinary comment follows.
// An ordinary comment.
const int B = 0;
/// Not attached, multiline comment follows.
/* A multiline comment. */
const int C = 0;
)")};
    REQUIRE(parsed.root().globalDecls.size() == 3);
    CHECK(parsed.decl(0).srcDocComment.empty());
    CHECK(parsed.decl(1).srcDocComment.empty());
    CHECK(parsed.decl(2).srcDocComment.empty());
  }
  SUBCASE("blank line between doc lines starts a new block") {
    auto parsed{ParsedModule(R"(#smdl
/// Dropped.

/// Kept.
const int A = 0;
)")};
    CHECK(docText(parsed.decl(0).srcDocComment) == "Kept.");
  }
  SUBCASE("struct fields") {
    auto parsed{ParsedModule(R"(#smdl
struct S {
  /// The first field,
  /// on two lines.
  int field1 = 0;
  int field2 = 0;
};
)")};
    const auto &decl{parsed.declAs<AST::Struct>(0)};
    REQUIRE(decl.fields.size() == 2);
    CHECK(docText(decl.fields[0].srcDocComment) ==
          "The first field,\non two lines.");
    CHECK(decl.fields[1].srcDocComment.empty());
  }
  SUBCASE("enum declarators") {
    auto parsed{ParsedModule(R"(#smdl
enum E {
  /// The first.
  E_FIRST,
  E_SECOND,
};
)")};
    const auto &decl{parsed.declAs<AST::Enum>(0)};
    REQUIRE(decl.declarators.size() == 2);
    CHECK(docText(decl.declarators[0].srcDocComment) == "The first.");
    CHECK(decl.declarators[1].srcDocComment.empty());
  }
  SUBCASE("trailing '///<' on enum declarators") {
    auto parsed{ParsedModule(R"(#smdl
enum E {
  E_FIRST = 0x0,  ///< The first.
  E_SECOND = 0x1, ///< The second.
};
)")};
    const auto &decl{parsed.declAs<AST::Enum>(0)};
    REQUIRE(decl.declarators.size() == 2);
    CHECK(docText(decl.declarators[0].srcDocCommentTrailing) == "The first.");
    CHECK(docText(decl.declarators[1].srcDocCommentTrailing) == "The second.");
    CHECK(decl.declarators[0].srcDocComment.empty());
    CHECK(decl.declarators[1].srcDocComment.empty());
  }
  SUBCASE("trailing '///<' on last declarator without comma") {
    auto parsed{ParsedModule(R"(#smdl
enum E {
  E_FIRST,
  E_SECOND ///< The second.
};
)")};
    const auto &decl{parsed.declAs<AST::Enum>(0)};
    REQUIRE(decl.declarators.size() == 2);
    CHECK(decl.declarators[0].srcDocCommentTrailing.empty());
    CHECK(docText(decl.declarators[1].srcDocCommentTrailing) == "The second.");
  }
  SUBCASE("stray '///<' on its own line attaches to nothing") {
    auto parsed{ParsedModule(R"(#smdl
enum E {
  E_FIRST,
  ///< Stray, attaches to nothing.
  E_SECOND,
};
)")};
    const auto &decl{parsed.declAs<AST::Enum>(0)};
    REQUIRE(decl.declarators.size() == 2);
    CHECK(decl.declarators[0].srcDocComment.empty());
    CHECK(decl.declarators[0].srcDocCommentTrailing.empty());
    CHECK(decl.declarators[1].srcDocComment.empty());
    CHECK(decl.declarators[1].srcDocCommentTrailing.empty());
  }
  SUBCASE("stray '///<' before any item attaches to nothing") {
    auto parsed{ParsedModule(R"(#smdl
enum E { ///< Stray, trails the brace, not a declarator.
  E_FIRST,
};
)")};
    const auto &decl{parsed.declAs<AST::Enum>(0)};
    REQUIRE(decl.declarators.size() == 1);
    CHECK(decl.declarators[0].srcDocComment.empty());
    CHECK(decl.declarators[0].srcDocCommentTrailing.empty());
  }
  SUBCASE("leading and trailing docs together") {
    auto parsed{ParsedModule(R"(#smdl
enum E {
  /// The leading doc.
  E_FIRST, ///< The trailing doc.
  E_SECOND,
};
)")};
    const auto &decl{parsed.declAs<AST::Enum>(0)};
    REQUIRE(decl.declarators.size() == 2);
    CHECK(docText(decl.declarators[0].srcDocComment) == "The leading doc.");
    CHECK(docText(decl.declarators[0].srcDocCommentTrailing) ==
          "The trailing doc.");
    CHECK(decl.declarators[1].srcDocComment.empty());
    CHECK(decl.declarators[1].srcDocCommentTrailing.empty());
  }
  SUBCASE("trailing '///<' on struct fields") {
    auto parsed{ParsedModule(R"(#smdl
struct S {
  int field1 = 0; ///< The first.
  int field2 = 0; ///< The second.
};
)")};
    const auto &decl{parsed.declAs<AST::Struct>(0)};
    REQUIRE(decl.fields.size() == 2);
    CHECK(docText(decl.fields[0].srcDocCommentTrailing) == "The first.");
    CHECK(docText(decl.fields[1].srcDocCommentTrailing) == "The second.");
  }
  SUBCASE("trailing '///<' on parameters") {
    auto parsed{ParsedModule(R"(#smdl
int f(
  int a, ///< The a.
  int b) = a + b;
)")};
    const auto &decl{parsed.declAs<AST::Function>(0)};
    REQUIRE(decl.params.size() == 2);
    CHECK(docText(decl.params[0].srcDocCommentTrailing) == "The a.");
    CHECK(decl.params[1].srcDocCommentTrailing.empty());
  }
  SUBCASE("trailing '///<' on variable declarators") {
    auto parsed{ParsedModule(R"(#smdl
const int c0 = 0, ///< The c0.
  c1 = 1; ///< The c1.
)")};
    const auto &decl{parsed.declAs<AST::Variable>(0)};
    REQUIRE(decl.declarators.size() == 2);
    CHECK(docText(decl.declarators[0].srcDocCommentTrailing) == "The c0.");
    CHECK(docText(decl.declarators[1].srcDocCommentTrailing) == "The c1.");
  }
  SUBCASE("parameters") {
    auto parsed{ParsedModule(R"(#smdl
int f(
  /// The parameter.
  int a,
  int b) = a + b;
)")};
    const auto &decl{parsed.declAs<AST::Function>(0)};
    REQUIRE(decl.params.size() == 2);
    CHECK(docText(decl.params[0].srcDocComment) == "The parameter.");
    CHECK(decl.params[1].srcDocComment.empty());
  }
  SUBCASE("variable declarators") {
    auto parsed{ParsedModule(R"(#smdl
const int c0 = 0,
  /// The second declarator.
  c1 = 1;
)")};
    const auto &decl{parsed.declAs<AST::Variable>(0)};
    REQUIRE(decl.declarators.size() == 2);
    CHECK(decl.declarators[0].srcDocComment.empty());
    CHECK(docText(decl.declarators[1].srcDocComment) ==
          "The second declarator.");
  }
  SUBCASE("namespaced declarations") {
    auto parsed{ParsedModule(R"(#smdl
namespace ns {
/// The nested function.
int g() = 0;
}
)")};
    const auto &decl{parsed.declAs<AST::Namespace>(0)};
    REQUIRE(decl.decls.size() == 1);
    CHECK(docText(decl.decls[0]->srcDocComment) == "The nested function.");
  }
  SUBCASE("module doc, SMDL syntax") {
    auto parsed{ParsedModule(R"(// A line comment.
/// The module documentation,
/// on two lines.
#smdl

const int X = 0;
)")};
    CHECK(docText(parsed.root().srcDocComment) ==
          "The module documentation,\non two lines.");
    CHECK(parsed.decl(0).srcDocComment.empty());
  }
  SUBCASE("module doc, MDL syntax") {
    auto parsed{ParsedModule(R"(/// The module documentation.
mdl 1.7;

/// The documented constant.
export const int X = 0;
)")};
    CHECK(docText(parsed.root().srcDocComment) == "The module documentation.");
    CHECK(docText(parsed.decl(0).srcDocComment) == "The documented constant.");
  }
}

TEST_CASE("Parser let expression") {
  SUBCASE("non-declaration is reported at the offending token") {
    auto message{parseError(R"(#smdl
exec {
  int y = let {
    float x = 2;
    x = 3;
  } in int(x);
}
)")};
    CHECK(contains(message, "::test>:5:"));
    CHECK(contains(message, "must contain only declarations"));
  }
  SUBCASE("unterminated block is reported at the 'let'") {
    auto message{parseError(R"(#smdl
exec {
  int y = let {
    float x = 2;
)")};
    CHECK(contains(message, "::test>:3:"));
    CHECK(contains(message, "expected closing '}' after 'let'"));
  }
}

TEST_CASE("Parser unexpected token") {
  SUBCASE("The token that stopped the parse is named") {
    auto message{parseError("#smdl\nint i = 1\nint j = 2;\n")};
    CHECK(contains(message, "but found 'int'"));
  }
  SUBCASE("End of file is said plainly") {
    auto message{parseError("#smdl\nexec {\n  int i = 1;\n")};
    CHECK(contains(message, "reached the end of the file"));
  }
  SUBCASE("A keyword borrowed from another language is explained") {
    CHECK(contains(parseError("#smdl\nclass Foo { int a; };\n"),
                   "there is no 'class'; use 'struct'"));
    CHECK(contains(parseError("#smdl\nunion U { int a; };\n"),
                   "union types are written '(A | B)'"));
    CHECK(contains(parseError("#smdl\n#define N 3\nexec { #print(N); }\n"),
                   "there is no preprocessor"));
  }
  SUBCASE("A borrowed keyword inside the construct is found") {
    // 'new' is neither where the parse started nor where it stopped.
    CHECK(contains(parseError("#smdl\nexec { auto p = new int(3); }\n"),
                   "there is no 'new'"));
    // 'and' sits past the token that stopped the parse.
    CHECK(contains(parseError("#smdl\nexec { #assert(1 == 1 and 2 == 2); }\n"),
                   "there is no 'and'; use '&&'"));
    CHECK(contains(parseError("#smdl\nexec { #assert(1 is int); }\n"),
                   "the type test operator is '<:'"));
  }
  SUBCASE("A borrowed keyword in a comment or string is not advice") {
    auto message{parseError("#smdl\nexec { int i = 1 /* class */ 2; }\n")};
    CHECK(contains(message, "but found"));
    CHECK(!contains(message, "use 'struct'"));
    message = parseError("#smdl\nexec { string s = \"class\" 2; }\n");
    CHECK(!contains(message, "use 'struct'"));
  }
  SUBCASE("An extension in a conformant file says to use '#smdl'") {
    CHECK(contains(parseError("mdl 1.8;\nunit_test \"x\" {}\n"),
                   "'unit_test' is a SpectralMDL extension"));
    CHECK(contains(
        parseError("mdl 1.8;\nexport int f() { return #sizeOf(int); }\n"),
        "'#sizeOf' is a SpectralMDL extension"));
  }
  SUBCASE("An extension in an SMDL file is not blamed on the dialect") {
    CHECK(!contains(parseError("#smdl\nexec { int i = 1 unit_test; }\n"),
                    "SpectralMDL extension"));
  }
  SUBCASE("A character literal is called out") {
    CHECK(contains(parseError("#smdl\nexec { int c = 'a'; }\n"),
                   "there are no character literals"));
  }
  SUBCASE("An unclosed delimiter says where it opened") {
    CHECK(contains(parseError("#smdl\nexec {\n  int i = (1 +\n    2;\n}\n"),
                   "to close the '(' opened at line 3"));
  }
  SUBCASE("An empty destructure is not a declarator") {
    // Accepting '{}' here swallowed the body of 'exec {}'.
    auto parsed{ParsedModule("#smdl\nexec {}\n")};
    CHECK(parsed.root().globalDecls.size() == 1);
  }
}
