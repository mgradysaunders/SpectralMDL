#include "doctest.h"

#include <string>

#include "smdl/Support/FlatYAML.h"

using smdl::FlatYAML;

// Parse and require success.
static FlatYAML parseOK(const std::string &source) {
  FlatYAML doc{};
  try {
    doc = FlatYAML::parse(source, "test.yaml");
  } catch (const smdl::Error &error) {
    MESSAGE(error.message);
    REQUIRE(false);
  }
  return doc;
}

// Parse, require failure, and require the message to name the line and
// contain the expected fragment.
static void parseFail(const std::string &source, int lineNo,
                      const std::string &fragment) {
  try {
    (void)FlatYAML::parse(source, "test.yaml");
    FAIL("expected a parse error containing '" << fragment << "'");
  } catch (const smdl::Error &error) {
    CAPTURE(error.message);
    CHECK(error.message.find(fragment) != std::string::npos);
    CHECK(error.message.find("line " + std::to_string(lineNo) + ":") !=
          std::string::npos);
  }
}

static const FlatYAML::Entry &entryOf(const FlatYAML &doc,
                                      const std::string &key) {
  auto entry{FlatYAML::find(doc.root, key)};
  REQUIRE(entry);
  return *entry;
}

TEST_CASE("FlatYAML") {
  SUBCASE("Scalars, comments, quotes, blank lines, CRLF, BOM") {
    auto doc{parseOK("\xEF\xBB\xBF# A comment\r\n"
                     "\r\n"
                     "name: Forest Ground  # trailing comment\r\n"
                     "hash: \"a # b\"\r\n"
                     "quoted: \"say \\\"hi\\\" \\\\ done\"\r\n"
                     "number: 1.5\r\n"
                     "text: \"1.5\"\r\n"
                     "count: 42\r\n")};
    CHECK(doc.sourceName == "test.yaml");
    REQUIRE(doc.root.size() == 6);
    CHECK(doc.toString(entryOf(doc, "name")) == "Forest Ground");
    CHECK(doc.toString(entryOf(doc, "hash")) == "a # b");
    CHECK(doc.toString(entryOf(doc, "quoted")) == "say \"hi\" \\ done");
    CHECK(entryOf(doc, "quoted").value.quoted);
    CHECK(!entryOf(doc, "number").value.quoted);
    CHECK(doc.toFloat(entryOf(doc, "number")) == doctest::Approx(1.5f));
    CHECK(doc.toInt(entryOf(doc, "count")) == 42);
    CHECK(entryOf(doc, "name").lineNo == 3);
    CHECK(FlatYAML::find(doc.root, "missing") == nullptr);
    // A quoted number is a string, and a scalar is not a list or a block.
    CHECK(doc.toString(entryOf(doc, "text")) == "1.5");
    CHECK_THROWS_WITH_AS((void)doc.toFloat(entryOf(doc, "text")),
                         doctest::Contains("expected a real number for 'text'"),
                         smdl::Error);
    CHECK_THROWS_WITH_AS((void)doc.toInt(entryOf(doc, "number")),
                         doctest::Contains("expected an integer for 'number'"),
                         smdl::Error);
    CHECK_THROWS_WITH_AS(
        (void)doc.toList(entryOf(doc, "number")),
        doctest::Contains("expected an inline list '[...]' for 'number'"),
        smdl::Error);
    CHECK_THROWS_WITH_AS(
        (void)doc.toMap(entryOf(doc, "number")),
        doctest::Contains("expected an indented block after 'number':"),
        smdl::Error);
    CHECK_THROWS_WITH_AS((void)doc.toSequence(entryOf(doc, "number")),
                         doctest::Contains("expected an indented sequence"),
                         smdl::Error);
  }

  SUBCASE("Inline lists") {
    auto doc{parseOK("empty: []\n"
                     "reals: [1, 2.5, -3e2]\n"
                     "words: [ a , \"b, c\" , d ]\n"
                     "nested: [0, [0.9, 0.8, 0.7]]\n")};
    CHECK(doc.toList(entryOf(doc, "empty")).empty());
    auto reals{doc.toFloats(entryOf(doc, "reals"), 3)};
    CHECK(reals[2] == doctest::Approx(-300.0f));
    const auto &words{doc.toList(entryOf(doc, "words"))};
    REQUIRE(words.size() == 3);
    CHECK(words[0].text == "a");
    CHECK(words[1].text == "b, c");
    CHECK(words[1].quoted);
    CHECK(words[2].text == "d");
    const auto &nested{doc.toList(entryOf(doc, "nested"))};
    REQUIRE(nested.size() == 2);
    CHECK(nested[0].kind == FlatYAML::Node::SCALAR);
    CHECK(nested[1].kind == FlatYAML::Node::LIST);
    REQUIRE(nested[1].items.size() == 3);
    CHECK(doc.toFloat(entryOf(doc, "nested"), nested[1].items[2]) ==
          doctest::Approx(0.7f));
    CHECK_THROWS_WITH_AS((void)doc.toFloats(entryOf(doc, "reals"), 2),
                         doctest::Contains("expected a list of 2 reals"),
                         smdl::Error);
    CHECK_THROWS_WITH_AS((void)doc.toFloats(entryOf(doc, "words"), 3),
                         doctest::Contains("expected a list of 3 reals"),
                         smdl::Error);
    CHECK_THROWS_WITH_AS((void)doc.toString(entryOf(doc, "reals")),
                         doctest::Contains("expected a string for 'reals'"),
                         smdl::Error);
    parseFail("a: [1, 2\n", 1, "expected ']' to close the inline list");
    parseFail("a: [1, , 2]\n", 1, "empty list item");
    parseFail("a: [1, [2, [3]]]\n", 1, "lists nest only one level deep");
    parseFail("a: [1]]\n", 1, "unexpected ']'");
  }

  SUBCASE("Block maps") {
    auto doc{parseOK("normal:\n"
                     "  file: n.png\n"
                     "  range: [0, 1]\n"
                     "after: yes\n")};
    const auto &block{doc.toMap(entryOf(doc, "normal"))};
    REQUIRE(block.size() == 2);
    CHECK(block[0].key == "file");
    CHECK(block[0].lineNo == 2);
    CHECK(block[1].value.kind == FlatYAML::Node::LIST);
    CHECK(entryOf(doc, "normal").value.kind == FlatYAML::Node::MAP);
    CHECK(doc.toString(entryOf(doc, "after")) == "yes");
    parseFail("normal:\n  file: n.png\n   over: x\n", 3,
              "inconsistent indentation");
    parseFail("name: x\n  stray: y\n", 2, "unexpected indentation");
    parseFail("normal:\n", 1, "expected a value or an indented block after");
    parseFail("normal:\nafter: x\n", 1,
              "expected a value or an indented block after");
    parseFail("a:\n  b:\n    c: 1\n", 3,
              "nested blocks are only supported one level deep");
    parseFail("\tname: x\n", 1, "tab in indentation");
  }

  SUBCASE("Block sequences of maps") {
    auto doc{parseOK("objects:\n"
                     "  - select: rock_03\n"
                     "    materials: [rock, moss]\n"
                     "    triangles: 5000\n"
                     "  -   select: rock_04\n"
                     "      triangles: 12\n"
                     "name: after\n")};
    const auto &sequence{doc.toSequence(entryOf(doc, "objects"))};
    REQUIRE(sequence.size() == 2);
    REQUIRE(sequence[0].size() == 3);
    CHECK(sequence[0][0].key == "select");
    CHECK(sequence[0][0].value.text == "rock_03");
    CHECK(sequence[0][1].value.kind == FlatYAML::Node::LIST);
    CHECK(doc.toInt(sequence[0][2]) == 5000);
    REQUIRE(sequence[1].size() == 2);
    CHECK(sequence[1][0].value.text == "rock_04");
    CHECK(sequence[1][1].lineNo == 6);
    CHECK(doc.toString(entryOf(doc, "name")) == "after");
    parseFail("- select: x\n", 1, "unexpected '-'");
    parseFail("objects:\n  - select: x\n   triangles: 1\n", 3,
              "inconsistent indentation");
    parseFail("objects:\n  - select:\n      nested: 1\n", 2,
              "nested blocks are not supported inside sequence items");
    parseFail("objects:\n  - select: x\n  triangles: 1\n", 3,
              "expected '- ' to start a sequence item");
    parseFail("objects:\n  - select: x\n    select: y\n", 3, "duplicate key");
    parseFail("objects:\n  -\n", 2, "expected 'key: value' after '-'");
  }

  SUBCASE("Keys and structure errors") {
    parseFail("just some text\n", 1, "expected 'key: value'");
    parseFail("basecolor:a.png\n", 1, "expected a space after ':'");
    parseFail(": x\n", 1, "expected 'key: value'");
    parseFail("a: 1\na: 2\n", 2, "duplicate key 'a' (already on line 1)");
    parseFail("n:\n  f: 1\n  f: 2\n", 3, "duplicate key");
    parseFail("name: \"unterminated\n", 1, "unterminated string");
    parseFail("name: \"x\" y\n", 1, "unexpected text after string");
    parseFail("name: \"bad \\n escape\"\n", 1, "invalid escape");
  }

  SUBCASE("Numbers") {
    CHECK(FlatYAML::parseNumber("1.5") == doctest::Approx(1.5));
    CHECK(FlatYAML::parseNumber("-2") == doctest::Approx(-2.0));
    CHECK(FlatYAML::parseNumber("1e3") == doctest::Approx(1000.0));
    CHECK(FlatYAML::parseNumber(".5") == doctest::Approx(0.5));
    CHECK(!FlatYAML::parseNumber(""));
    CHECK(!FlatYAML::parseNumber("1.png"));
    CHECK(!FlatYAML::parseNumber("0x10"));
    CHECK(!FlatYAML::parseNumber("inf"));
    CHECK(!FlatYAML::parseNumber("nan"));
    CHECK(!FlatYAML::parseNumber(" 1"));
    CHECK(!FlatYAML::parseNumber("1 "));
    CHECK(!FlatYAML::parseNumber("1e999"));
    CHECK(FlatYAML::isNumber("1200"));
    CHECK(!FlatYAML::isNumber("Cu"));
  }
}
