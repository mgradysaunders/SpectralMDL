#include "smdl/Support/FlatYAML.h"

#include <cmath>
#include <cstdlib>
#include <map>

#include "smdl/Support/Strings.h"

namespace smdl {

namespace {

[[nodiscard]] std::string_view trimLeft(std::string_view text) {
  while (!text.empty() && (text.front() == ' ' || text.front() == '\t'))
    text.remove_prefix(1);
  return text;
}

[[nodiscard]] std::string_view trimRight(std::string_view text) {
  while (!text.empty() && (text.back() == ' ' || text.back() == '\t'))
    text.remove_suffix(1);
  return text;
}

[[nodiscard]] std::string_view trim(std::string_view text) {
  return trimLeft(trimRight(text));
}

// The parser: a line lexer followed by a structure pass over the lines.
class Parser final {
public:
  Parser(FlatYAML &doc, std::string_view source) : mDoc(doc), mSource(source) {}

  void parse() {
    lexLines();
    size_t i{0};
    mDoc.root = parseMap(i, 0);
    if (i < mLines.size())
      mDoc.fail(mLines[i].lineNo, "unexpected indentation");
  }

private:
  // One content line, comments stripped, split at the first ':'. For a
  // sequence item, 'hasDash' is set, 'indent' is the column of the dash, and
  // 'keyColumn' is the column of the key after it.
  struct Line final {
    int lineNo{};
    int indent{};
    int keyColumn{};
    bool hasDash{};
    std::string_view key{};
    std::string_view value{};
  };

  void lexLines() {
    auto remainder{mSource};
    if (startsWith(remainder, "\xEF\xBB\xBF")) remainder.remove_prefix(3);
    int lineNo{0};
    while (!remainder.empty()) {
      auto newline{remainder.find('\n')};
      auto text{remainder.substr(0, newline)};
      remainder = newline == std::string_view::npos
                      ? std::string_view{}
                      : remainder.substr(newline + 1);
      lineNo++;
      if (!text.empty() && text.back() == '\r') text.remove_suffix(1);
      size_t indent{0};
      while (indent < text.size() && text[indent] == ' ') indent++;
      if (indent < text.size() && text[indent] == '\t')
        mDoc.fail(lineNo, "tab in indentation (use spaces)");
      auto content{trimRight(stripComment(text.substr(indent)))};
      if (content.empty()) continue;
      Line line{};
      line.lineNo = lineNo;
      line.indent = int(indent);
      line.keyColumn = int(indent);
      if (content == "-" || startsWith(content, "- ")) {
        line.hasDash = true;
        auto afterDash{content.substr(1)};
        auto stripped{trimLeft(afterDash)};
        line.keyColumn = int(indent + 1 + (afterDash.size() - stripped.size()));
        content = stripped;
        if (content.empty())
          mDoc.fail(lineNo, "expected 'key: value' after '-'");
      }
      auto colon{content.find(':')};
      if (colon == std::string_view::npos)
        mDoc.fail(lineNo, "expected 'key: value'");
      auto key{trimRight(content.substr(0, colon))};
      auto after{content.substr(colon + 1)};
      if (key.empty()) mDoc.fail(lineNo, "expected 'key: value'");
      if (!after.empty() && after[0] != ' ')
        mDoc.fail(lineNo, "expected a space after ':'");
      line.key = key;
      line.value = trim(after);
      mLines.push_back(line);
    }
  }

  // Strip a full-line or trailing comment, respecting double quotes.
  [[nodiscard]] std::string_view stripComment(std::string_view text) {
    bool isInQuotes{false};
    bool isInEscape{false};
    for (size_t i = 0; i < text.size(); i++) {
      char ch{text[i]};
      if (isInQuotes) {
        if (isInEscape)
          isInEscape = false;
        else if (ch == '\\')
          isInEscape = true;
        else if (ch == '"')
          isInQuotes = false;
      } else if (ch == '"') {
        isInQuotes = true;
      } else if (ch == '#' &&
                 (i == 0 || text[i - 1] == ' ' || text[i - 1] == '\t')) {
        return text.substr(0, i);
      }
    }
    return text;
  }

  void checkDuplicate(std::map<std::string, int, std::less<>> &seen,
                      std::string_view key, int lineNo) {
    auto [itr, inserted] = seen.try_emplace(std::string(key), lineNo);
    if (!inserted)
      mDoc.fail(lineNo, concat("duplicate key ", Quoted(key),
                               " (already on line ", itr->second, ")"));
  }

  // Parse the block map whose entries sit at exactly 'indent', starting at
  // line 'i' and leaving 'i' at the first line that does not belong to it.
  [[nodiscard]] FlatYAML::Map parseMap(size_t &i, int indent) {
    auto map{FlatYAML::Map{}};
    auto seen{std::map<std::string, int, std::less<>>{}};
    while (i < mLines.size()) {
      const auto &line{mLines[i]};
      if (line.indent < indent) break;
      if (line.indent > indent)
        mDoc.fail(line.lineNo, indent > 0 ? "inconsistent indentation"
                                          : "unexpected indentation");
      if (line.hasDash)
        mDoc.fail(line.lineNo, "unexpected '-' (not inside a sequence)");
      checkDuplicate(seen, line.key, line.lineNo);
      auto &entry{map.emplace_back()};
      entry.key = std::string(line.key);
      entry.lineNo = line.lineNo;
      i++;
      if (!line.value.empty()) {
        entry.value = parseInline(line.lineNo, line.value);
      } else {
        if (i >= mLines.size() || mLines[i].indent <= indent)
          mDoc.fail(line.lineNo,
                    concat("expected a value or an indented block after ",
                           Quoted(line.key), ":"));
        if (indent > 0)
          mDoc.fail(mLines[i].lineNo,
                    "nested blocks are only supported one level deep");
        entry.value.lineNo = line.lineNo;
        if (mLines[i].hasDash) {
          entry.value.kind = FlatYAML::Node::SEQUENCE;
          entry.value.sequence = parseSequence(i, mLines[i].indent);
        } else {
          entry.value.kind = FlatYAML::Node::MAP;
          entry.value.map = parseMap(i, mLines[i].indent);
        }
      }
    }
    return map;
  }

  // Parse the block sequence whose dashes sit at exactly 'dashIndent'.
  [[nodiscard]] std::vector<FlatYAML::Map> parseSequence(size_t &i,
                                                         int dashIndent) {
    auto sequence{std::vector<FlatYAML::Map>{}};
    while (i < mLines.size() && mLines[i].indent >= dashIndent) {
      const auto &first{mLines[i]};
      if (!first.hasDash || first.indent != dashIndent)
        mDoc.fail(first.lineNo, "expected '- ' to start a sequence item");
      auto seen{std::map<std::string, int, std::less<>>{}};
      auto &item{sequence.emplace_back()};
      auto addEntry{[&](const Line &line) {
        if (line.value.empty())
          mDoc.fail(line.lineNo,
                    "nested blocks are not supported inside sequence items");
        checkDuplicate(seen, line.key, line.lineNo);
        auto &entry{item.emplace_back()};
        entry.key = std::string(line.key);
        entry.lineNo = line.lineNo;
        entry.value = parseInline(line.lineNo, line.value);
      }};
      addEntry(first);
      i++;
      while (i < mLines.size() && !mLines[i].hasDash &&
             mLines[i].indent > dashIndent) {
        if (mLines[i].indent != first.keyColumn)
          mDoc.fail(mLines[i].lineNo, "inconsistent indentation");
        addEntry(mLines[i]);
        i++;
      }
    }
    return sequence;
  }

  // Parse a scalar or an inline list.
  [[nodiscard]] FlatYAML::Node parseInline(int lineNo, std::string_view value) {
    FlatYAML::Node node{};
    node.lineNo = lineNo;
    if (startsWith(value, "[")) {
      if (value.back() != ']')
        mDoc.fail(lineNo, "expected ']' to close the inline list");
      node.kind = FlatYAML::Node::LIST;
      node.items = parseListItems(lineNo, value.substr(1, value.size() - 2),
                                  /*canNest=*/true);
    } else {
      node.kind = FlatYAML::Node::SCALAR;
      node.text = parseScalar(lineNo, value, node.isQuoted);
    }
    return node;
  }

  // Split the inside of an inline list at the commas outside quotes and
  // outside one level of nested brackets.
  [[nodiscard]] std::vector<FlatYAML::Node>
  parseListItems(int lineNo, std::string_view inside, bool canNest) {
    auto items{std::vector<FlatYAML::Node>{}};
    if (trim(inside).empty()) return items;
    size_t start{0};
    int depth{0};
    bool isInQuotes{false};
    bool isInEscape{false};
    auto flush{[&](size_t end) {
      auto text{trim(inside.substr(start, end - start))};
      if (text.empty()) mDoc.fail(lineNo, "empty list item");
      FlatYAML::Node item{};
      item.lineNo = lineNo;
      if (startsWith(text, "[")) {
        if (!canNest) mDoc.fail(lineNo, "lists nest only one level deep");
        if (text.back() != ']')
          mDoc.fail(lineNo, "expected ']' to close the inline list");
        item.kind = FlatYAML::Node::LIST;
        item.items = parseListItems(lineNo, text.substr(1, text.size() - 2),
                                    /*canNest=*/false);
      } else {
        item.kind = FlatYAML::Node::SCALAR;
        item.text = parseScalar(lineNo, text, item.isQuoted);
      }
      items.push_back(std::move(item));
      start = end + 1;
    }};
    for (size_t i = 0; i < inside.size(); i++) {
      char ch{inside[i]};
      if (isInQuotes) {
        if (isInEscape)
          isInEscape = false;
        else if (ch == '\\')
          isInEscape = true;
        else if (ch == '"')
          isInQuotes = false;
      } else if (ch == '"') {
        isInQuotes = true;
      } else if (ch == '[') {
        depth++;
      } else if (ch == ']') {
        if (depth == 0) mDoc.fail(lineNo, "unexpected ']' in the inline list");
        depth--;
      } else if (ch == ',' && depth == 0) {
        flush(i);
      }
    }
    if (depth != 0) mDoc.fail(lineNo, "expected ']' to close the inline list");
    flush(inside.size());
    return items;
  }

  // Unescape a double-quoted scalar, or pass a bare one through.
  [[nodiscard]] std::string parseScalar(int lineNo, std::string_view value,
                                        bool &isQuoted) {
    isQuoted = false;
    if (value.empty() || value.front() != '"') return std::string(value);
    isQuoted = true;
    auto result{std::string{}};
    bool isInEscape{false};
    for (size_t i = 1; i < value.size(); i++) {
      char ch{value[i]};
      if (isInEscape) {
        if (ch != '"' && ch != '\\')
          mDoc.fail(lineNo, "invalid escape (only '\\\"' and '\\\\')");
        result += ch;
        isInEscape = false;
      } else if (ch == '\\') {
        isInEscape = true;
      } else if (ch == '"') {
        if (i + 1 != value.size())
          mDoc.fail(lineNo, "unexpected text after string");
        return result;
      } else {
        result += ch;
      }
    }
    mDoc.fail(lineNo, "unterminated string");
  }

private:
  FlatYAML &mDoc;

  std::string_view mSource{};

  std::vector<Line> mLines{};
};

} // namespace

FlatYAML FlatYAML::parse(std::string_view source, std::string sourceName) {
  FlatYAML doc{};
  doc.sourceName = std::move(sourceName);
  Parser(doc, source).parse();
  return doc;
}

void FlatYAML::fail(int lineNo, std::string_view message) const {
  throw Error(concat(QuotedPath(sourceName), ": line ", lineNo, ": ", message));
}

const std::string &FlatYAML::toString(const Entry &entry) const {
  if (entry.value.kind != Node::SCALAR)
    fail(entry, concat("expected a string for ", Quoted(entry.key)));
  return entry.value.text;
}

float FlatYAML::toFloat(const Entry &entry) const {
  return toFloat(entry, entry.value);
}

float FlatYAML::toFloat(const Entry &entry, const Node &item) const {
  if (item.kind == Node::SCALAR && !item.isQuoted)
    if (auto number{parseNumber(item.text)}) return float(*number);
  fail(entry, concat("expected a real number for ", Quoted(entry.key)));
}

long FlatYAML::toInt(const Entry &entry) const {
  if (entry.value.kind == Node::SCALAR && !entry.value.isQuoted) {
    const auto &text{entry.value.text};
    char *end{};
    auto result{std::strtol(text.c_str(), &end, 10)};
    if (!text.empty() && end == text.c_str() + text.size()) return result;
  }
  fail(entry, concat("expected an integer for ", Quoted(entry.key)));
}

const std::vector<FlatYAML::Node> &FlatYAML::toList(const Entry &entry) const {
  if (entry.value.kind != Node::LIST)
    fail(entry,
         concat("expected an inline list '[...]' for ", Quoted(entry.key)));
  return entry.value.items;
}

std::vector<float> FlatYAML::toFloats(const Entry &entry, size_t count) const {
  auto result{std::vector<float>{}};
  if (entry.value.kind == Node::LIST && entry.value.items.size() == count) {
    for (const auto &item : entry.value.items) {
      if (item.kind != Node::SCALAR || item.isQuoted) break;
      auto number{parseNumber(item.text)};
      if (!number) break;
      result.push_back(float(*number));
    }
  }
  if (result.size() != count)
    fail(entry, concat("expected a list of ", count, " reals for ",
                       Quoted(entry.key)));
  return result;
}

const FlatYAML::Map &FlatYAML::toMap(const Entry &entry) const {
  if (entry.value.kind != Node::MAP)
    fail(entry,
         concat("expected an indented block after ", Quoted(entry.key), ":"));
  return entry.value.map;
}

const std::vector<FlatYAML::Map> &
FlatYAML::toSequence(const Entry &entry) const {
  if (entry.value.kind != Node::SEQUENCE)
    fail(entry, concat("expected an indented sequence of '- key: value' "
                       "items after ",
                       Quoted(entry.key), ":"));
  return entry.value.sequence;
}

const FlatYAML::Entry *FlatYAML::find(const Map &map,
                                      std::string_view key) noexcept {
  for (const auto &entry : map)
    if (entry.key == key) return &entry;
  return nullptr;
}

std::optional<double> FlatYAML::parseNumber(std::string_view text) noexcept {
  if (text.empty() || text.size() > 64) return std::nullopt;
  // No leading whitespace and no hexadecimal, infinity, or NaN spellings:
  // a number is digits, a sign, a point, and an exponent, nothing else.
  for (char ch : text)
    if (!(isDigit(ch) || ch == '+' || ch == '-' || ch == '.' || ch == 'e' ||
          ch == 'E'))
      return std::nullopt;
  char buffer[65]{};
  text.copy(buffer, text.size());
  char *end{};
  auto result{std::strtod(buffer, &end)};
  if (end != buffer + text.size() || !std::isfinite(result))
    return std::nullopt;
  return result;
}

} // namespace smdl
