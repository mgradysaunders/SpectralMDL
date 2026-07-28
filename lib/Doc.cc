#include "smdl/Doc.h"

#include <algorithm>
#include <cstdio>

#include "smdl/AST.h"
#include "smdl/Module.h"
#include "smdl/Support/QualifiedName.h"

#include "Compiler/BuiltinAccess.h"

namespace smdl {

//--{ Extraction
namespace {

/// If a comment or a `[[ ... ]]` annotation block begins at index `i`,
/// advance past it. Otherwise return `i` unchanged. These are the spans
/// the normalizer drops outright, as opposed to whitespace, which it
/// collapses.
[[nodiscard]] size_t skipDropped(std::string_view src, size_t i) {
  auto remaining{src.substr(i)};
  auto skipPast{[&](std::string_view close) {
    auto j{src.find(close, i + 2)};
    return j == std::string_view::npos ? src.size() : j + close.size();
  }};
  if (startsWith(remaining, "//")) {
    // Stop at the newline rather than past it: it is whitespace, which
    // the caller still has to account for.
    auto j{src.find('\n', i + 2)};
    return j == std::string_view::npos ? src.size() : j;
  }
  if (startsWith(remaining, "/*")) return skipPast("*/");
  if (startsWith(remaining, "[[")) return skipPast("]]");
  return i;
}

/// Advance past everything the normalizer drops: whitespace, comments,
/// and `[[ ... ]]` annotation blocks.
[[nodiscard]] size_t skipIgnorable(std::string_view src, size_t i) {
  while (i < src.size()) {
    if (isSpace(src[i])) {
      i++;
      continue;
    }
    auto j{skipDropped(src, i)};
    if (j == i) break;
    i = j;
  }
  return i;
}

/// Is the `=` at index `i` part of a multi-character operator, e.g.,
/// `==`, `!=`, `<=`, or `:=`? Such an `=` belongs to an expression inside
/// an initializer and must not be spaced apart, unlike the `=` that
/// introduces the initializer itself.
[[nodiscard]] bool isCompoundOperatorEquals(std::string_view src, size_t i) {
  static constexpr std::string_view OperatorChars{"!<>=~:+-*/%&|^"};
  return (i > 0 && OperatorChars.find(src[i - 1]) != std::string_view::npos) ||
         (i + 1 < src.size() && src[i + 1] == '=');
}

/// Normalize a raw signature slice: strip comments and `[[ ... ]]`
/// annotation blocks, collapse whitespace runs to single spaces, and tidy
/// spacing around punctuation.
///
/// NOTE: This also re-expands the spacing that minification removes, so
/// that the embedded builtins read like the sources they came from:
/// initializers are spaced as `x = 1` and separators as `a, b`, and a
/// trailing comma before a closing bracket is dropped.
///
/// If `srcName` points into `src`, `nameOffset` receives the offset at
/// which it lands in the result, which survives normalization because
/// identifiers are copied verbatim.
[[nodiscard]] std::string normalizeSignature(std::string_view src,
                                             std::string_view srcName = {},
                                             uint32_t *nameOffset = nullptr) {
  auto result{std::string{}};
  result.reserve(src.size());
  auto nameBegin{std::string_view::npos};
  if (!srcName.empty() && !src.empty() && srcName.data() >= src.data() &&
      srcName.data() + srcName.size() <= src.data() + src.size()) {
    nameBegin = size_t(srcName.data() - src.data());
  }
  auto lastWasSpace{true}; // Also trims leading whitespace
  auto addSpace{[&] {
    if (!lastWasSpace) {
      if (!result.empty() && result.back() != '(') result += ' ';
      lastWasSpace = true;
    }
  }};
  size_t i{};
  while (i < src.size()) {
    if (auto j{skipDropped(src, i)}; j != i) {
      i = j;
      addSpace();
    } else if (isSpace(src[i])) {
      i++;
      addSpace();
    } else {
      const auto ch{src[i]};
      // Drop a trailing comma before a closing bracket, which minified
      // sources keep but which reads as a missing argument. NOTE: The
      // lookahead must skip comments too: with `--keep-doc-comments`, a
      // trailing `///<` sits between the last comma and the `)`.
      if (ch == ',') {
        auto j{skipIgnorable(src, i + 1)};
        if (j < src.size() &&
            (src[j] == ')' || src[j] == ']' || src[j] == '}')) {
          i = j;
          continue;
        }
      }
      if ((ch == ',' || ch == ')') && !result.empty() && result.back() == ' ')
        result.pop_back();
      const auto spaceAround{ch == '=' && !isCompoundOperatorEquals(src, i)};
      if (spaceAround) addSpace();
      if (i == nameBegin && nameOffset) *nameOffset = uint32_t(result.size());
      result += ch;
      lastWasSpace = false;
      if (spaceAround || ch == ',') {
        result += ' ';
        lastWasSpace = true;
      }
      i++;
    }
  }
  while (!result.empty() && result.back() == ' ') result.pop_back();
  return result;
}

/// Get the string in the `description(...)` or `anno::description(...)`
/// annotation if present. This may be empty!
[[nodiscard]] std::string
descriptionOf(const AST::AnnotationBlock *annotations) {
  if (!annotations) return {};
  for (const auto &anno : *annotations) {
    if (anno.hasIdentifier({"description"}) ||
        anno.hasIdentifier({"anno", "description"})) {
      for (const auto &arg : anno.args) {
        if (arg.expr && arg.expr->exprKind == AST::ExprKind::LiteralString)
          return static_cast<const AST::LiteralString *>(arg.expr.get())->value;
      }
    }
  }
  return {};
}

/// Get the documentation text from the first non-empty source: the
/// leading `///` block, the trailing `///<` comment, or the
/// `description(...)` annotation. This may be empty!
[[nodiscard]] std::string docTextOf(std::string_view srcDocComment,
                                    std::string_view srcTrailing,
                                    const AST::AnnotationBlock *annotations) {
  auto text{AST::getDocCommentText(srcDocComment)};
  if (text.empty()) text = AST::getDocCommentText(srcTrailing);
  if (text.empty()) text = descriptionOf(annotations);
  return text;
}

/// The AST-to-`DocModule` extractor.
class Extractor final {
public:
  explicit Extractor(const Module &module_)
      : mModule(module_), mSource(module_.getSourceCode()) {}

  [[nodiscard]] DocModule extract() {
    auto mod{DocModule{}};
    mod.name = std::string(mModule.getName());
    mod.qualifiedName = mModule.getQualifiedName().empty()
                            ? "::" + mod.name
                            : std::string(mModule.getQualifiedName());
    mod.fileName = std::string(mModule.getFileName());
    mQualifiedNamePrefix = mod.qualifiedName;
    const auto *root{mModule.getRoot()};
    if (!root) return mod;
    mod.docText = AST::getDocCommentText(root->srcDocComment);
    if (mod.docText.empty())
      mod.docText = descriptionOf(root->moduleAnnotations.get());
    for (const auto &decl : root->globalDecls) extractDecl(*decl, mod.entries);
    return mod;
  }

private:
  /// Get the begin index of a non-empty span into the module source.
  [[nodiscard]] size_t beginOf(std::string_view span) const {
    return size_t(span.data() - mSource.data());
  }

  /// Get the end index of a non-empty span into the module source.
  [[nodiscard]] size_t endOf(std::string_view span) const {
    return beginOf(span) + span.size();
  }

  [[nodiscard]] std::string_view slice(size_t i0, size_t i1) const {
    return mSource.substr(i0, i1 - i0);
  }

  /// Slice from `i0` up to the separating comma if there is one, else up
  /// to the token that terminates the enclosing declaration. This is the
  /// shape of every comma-separated declarator and parameter.
  [[nodiscard]] std::string_view sliceUntil(size_t i0,
                                            std::string_view srcComma,
                                            std::string_view srcEnd) const {
    return slice(i0, beginOf(!srcComma.empty() ? srcComma : srcEnd));
  }

  /// The signature prefix from the attributes and the `export` keyword,
  /// which precede the declaration's own source location.
  [[nodiscard]] static std::string declPrefix(const AST::Decl &decl) {
    auto prefix{std::string{}};
    if (decl.attributes) {
      prefix += "@(";
      for (size_t i = 0; i < decl.attributes->attrs.size(); i++) {
        if (i > 0) prefix += ' ';
        prefix += decl.attributes->attrs[i];
      }
      prefix += ") ";
    }
    if (decl.isExported()) prefix += "export ";
    return prefix;
  }

  [[nodiscard]] std::string qualify(std::string_view name) const {
    return mQualifiedNamePrefix + "::" + std::string(name);
  }

  /// Set `entry.signature` from the given prefix and source slice, and
  /// record where the declared name landed in it.
  static void setSignature(DocEntry &entry, std::string_view prefix,
                           std::string_view src, std::string_view srcName) {
    auto offset{DocEntry::NO_NAME_OFFSET};
    entry.signature =
        std::string(prefix) + normalizeSignature(src, srcName, &offset);
    if (offset != DocEntry::NO_NAME_OFFSET)
      entry.nameOffset = uint32_t(prefix.size()) + offset;
  }

  [[nodiscard]] DocEntry makeEntry(const AST::Decl &decl, const char *kind,
                                   std::string_view name) const {
    auto entry{DocEntry{}};
    entry.kind = kind;
    entry.name = std::string(name);
    entry.qualifiedName = qualify(name);
    entry.isExported = decl.isExported();
    entry.lineNo = decl.srcLoc.lineNo;
    return entry;
  }

  /// Make an entry for a member of an enum or a struct. Members are not
  /// `AST::Decl`s, so they carry no `export` of their own and their
  /// qualified names are formed differently, but everything else about
  /// them is the same: `node` is an `AST::Enum::Declarator` or an
  /// `AST::Struct::Field`, and `srcEnd` is where its signature stops.
  template <typename Node>
  [[nodiscard]] DocEntry makeMember(const DocEntry &parent, const char *kind,
                                    std::string qualifiedName, const Node &node,
                                    std::string_view srcEnd) const {
    auto member{DocEntry{}};
    member.kind = kind;
    member.name = std::string(node.name.srcName);
    member.qualifiedName = std::move(qualifiedName);
    member.isExported = parent.isExported;
    member.lineNo = node.srcLoc.lineNo;
    setSignature(member, {}, slice(node.srcLoc.i, beginOf(srcEnd)),
                 node.name.srcName);
    member.docText = docTextOf(node.srcDocComment, node.srcDocCommentTrailing,
                               node.annotations.get());
    return member;
  }

  /// Extract a declaration whose entire signature runs from its own
  /// source location to its semicolon, i.e., `AST::Tag` and
  /// `AST::Typedef`.
  template <typename Decl>
  void extractSimpleDecl(const AST::Decl &decl, const char *kind,
                         std::vector<DocEntry> &out) {
    const auto &d{static_cast<const Decl &>(decl)};
    auto entry{makeEntry(decl, kind, d.name.srcName)};
    setSignature(entry, declPrefix(decl),
                 slice(decl.srcLoc.i, beginOf(d.srcSemicolon)), d.name.srcName);
    entry.docText = docTextOf(decl.srcDocComment, {}, nullptr);
    out.push_back(std::move(entry));
  }

  void extractDecl(const AST::Decl &decl, std::vector<DocEntry> &out) {
    switch (decl.declKind) {
    case AST::DeclKind::AnnotationDecl:
      extractAnnotationDecl(static_cast<const AST::AnnotationDecl &>(decl),
                            out);
      break;
    case AST::DeclKind::Enum:
      extractEnum(static_cast<const AST::Enum &>(decl), out);
      break;
    case AST::DeclKind::Function:
      extractFunction(static_cast<const AST::Function &>(decl), out);
      break;
    case AST::DeclKind::Namespace:
      extractNamespace(static_cast<const AST::Namespace &>(decl), out);
      break;
    case AST::DeclKind::Struct:
      extractStruct(static_cast<const AST::Struct &>(decl), out);
      break;
    case AST::DeclKind::Tag:
      extractSimpleDecl<AST::Tag>(decl, "tag", out);
      break;
    case AST::DeclKind::Typedef:
      extractSimpleDecl<AST::Typedef>(decl, "typedef", out);
      break;
    case AST::DeclKind::Variable:
      extractVariable(static_cast<const AST::Variable &>(decl), out);
      break;
    default:
      // Skip declarations that are not documentable API surface:
      // `exec`, `unit_test`, imports, and using aliases.
      break;
    }
  }

  void extractAnnotationDecl(const AST::AnnotationDecl &decl,
                             std::vector<DocEntry> &out) {
    auto entry{makeEntry(decl, "annotation", decl.name.srcName)};
    setSignature(entry, declPrefix(decl),
                 slice(decl.srcLoc.i, endOf(decl.params.srcParenR)),
                 decl.name.srcName);
    entry.docText = docTextOf(decl.srcDocComment, {}, decl.annotations.get());
    extractParams(decl.params, entry.params);
    out.push_back(std::move(entry));
  }

  void extractEnum(const AST::Enum &decl, std::vector<DocEntry> &out) {
    auto entry{makeEntry(decl, "enum", decl.name.srcName)};
    setSignature(entry, declPrefix(decl),
                 slice(decl.srcLoc.i, endOf(decl.name.srcName)),
                 decl.name.srcName);
    entry.docText = docTextOf(decl.srcDocComment, {}, decl.annotations.get());
    for (const auto &declarator : decl.declarators) {
      // NOTE: Enum values are injected into the enclosing scope, so the
      // qualified name does not include the enum name.
      entry.members.push_back(makeMember(
          entry, "enumerator", qualify(declarator.name.srcName), declarator,
          !declarator.srcComma.empty() ? declarator.srcComma : decl.srcBraceR));
    }
    out.push_back(std::move(entry));
  }

  void extractFunction(const AST::Function &decl, std::vector<DocEntry> &out) {
    auto entry{makeEntry(decl, "function", decl.name.srcName)};
    setSignature(entry, declPrefix(decl),
                 slice(decl.srcLoc.i, !decl.srcFrequency.empty()
                                          ? endOf(decl.srcFrequency)
                                          : endOf(decl.params.srcParenR)),
                 decl.name.srcName);
    entry.docText =
        docTextOf(decl.srcDocComment, {},
                  decl.lateAnnotations ? decl.lateAnnotations.get()
                                       : decl.earlyAnnotations.get());
    extractParams(decl.params, entry.params);
    out.push_back(std::move(entry));
  }

  void extractNamespace(const AST::Namespace &decl,
                        std::vector<DocEntry> &out) {
    auto name{std::string{}};
    for (auto elemName : Span<const std::string_view>(*decl.identifier)) {
      if (!name.empty()) name += "::";
      name += elemName;
    }
    auto entry{makeEntry(decl, "namespace", name)};
    entry.signature = "namespace " + name;
    entry.nameOffset = uint32_t(entry.signature.size() - name.size());
    entry.docText = docTextOf(decl.srcDocComment, {}, nullptr);
    auto prevPrefix{mQualifiedNamePrefix};
    mQualifiedNamePrefix = entry.qualifiedName;
    for (const auto &inner : decl.decls) extractDecl(*inner, entry.members);
    mQualifiedNamePrefix = std::move(prevPrefix);
    out.push_back(std::move(entry));
  }

  void extractStruct(const AST::Struct &decl, std::vector<DocEntry> &out) {
    auto entry{makeEntry(decl, "struct", decl.name.srcName)};
    setSignature(entry, declPrefix(decl),
                 slice(decl.srcLoc.i, endOf(decl.name.srcName)),
                 decl.name.srcName);
    entry.docText = docTextOf(decl.srcDocComment, {}, decl.annotations.get());
    for (const auto &field : decl.fields) {
      entry.members.push_back(makeMember(
          entry, "field",
          entry.qualifiedName + "::" + std::string(field.name.srcName), field,
          field.srcSemicolon));
    }
    out.push_back(std::move(entry));
  }

  void extractVariable(const AST::Variable &decl, std::vector<DocEntry> &out) {
    if (decl.declarators.empty()) return;
    auto typeSrc{slice(decl.srcLoc.i, decl.declarators[0].srcLoc.i)};
    for (const auto &declarator : decl.declarators) {
      auto name{std::string{}};
      if (declarator.isDestructure()) {
        name += '{';
        for (size_t i = 0; i < declarator.names.size(); i++) {
          if (i > 0) name += ", ";
          name += declarator.names[i].name.srcName;
        }
        name += '}';
      } else if (!declarator.names.empty()) {
        name = std::string(declarator.names[0].name.srcName);
      }
      auto entry{makeEntry(decl, "variable", name)};
      entry.lineNo = declarator.srcLoc.lineNo;
      auto declaratorSrc{sliceUntil(declarator.srcLoc.i, declarator.srcComma,
                                    decl.srcSemicolon)};
      // NOTE: The type and the declarator are not contiguous in the
      // source, so this is the one signature built by concatenation.
      // The name span must be rebased onto the concatenated string.
      auto signatureSrc{std::string(typeSrc) + " " +
                        std::string(declaratorSrc)};
      auto srcName{std::string_view{}};
      if (!declarator.isDestructure() && !declarator.names.empty()) {
        srcName = std::string_view(
            signatureSrc.data() + typeSrc.size() + 1 +
                size_t(declarator.names[0].name.srcName.data() -
                       declaratorSrc.data()),
            declarator.names[0].name.srcName.size());
      }
      setSignature(entry, declPrefix(decl), signatureSrc, srcName);
      entry.docText =
          docTextOf(declarator.srcDocComment, declarator.srcDocCommentTrailing,
                    declarator.annotations.get());
      if (entry.docText.empty())
        entry.docText = AST::getDocCommentText(decl.srcDocComment);
      out.push_back(std::move(entry));
    }
  }

  void extractParams(const AST::ParameterList &params,
                     std::vector<DocParam> &out) {
    for (const auto &param : params) {
      auto docParam{DocParam{}};
      docParam.name = std::string(param.name.srcName);
      // NOTE: `Parameter::src` is never populated, so slice from the
      // parameter start to its comma or the closing parenthesis.
      docParam.signature = normalizeSignature(
          sliceUntil(param.srcLoc.i, param.srcComma, params.srcParenR));
      docParam.docText =
          docTextOf(param.srcDocComment, param.srcDocCommentTrailing,
                    param.annotations.get());
      out.push_back(std::move(docParam));
    }
  }

private:
  const Module &mModule;

  std::string_view mSource;

  /// The qualified name prefix, i.e., the qualified module name plus
  /// the namespaces currently being descended into.
  std::string mQualifiedNamePrefix{};
};

} // namespace

DocModule extractDocModule(const Module &module_) {
  return Extractor(module_).extract();
}

std::vector<std::string_view> getBuiltinModuleNames() {
  auto names{builtin::getAllNames()};
  return std::vector<std::string_view>(names.begin(), names.end());
}

std::optional<DocModule> extractBuiltinDocModule(std::string_view name) {
  const auto *sourceCode{builtin::getSourceCode(name)};
  if (!sourceCode) return std::nullopt;
  auto allocator{BumpPtrAllocator{}};
  auto module_{Module(std::string(name), std::string(sourceCode))};
  if (auto error{module_.parse(allocator)}) return std::nullopt;
  return extractDocModule(module_);
}
//--}

//--{ Database queries
std::vector<const DocEntry *>
DocDatabase::findSymbol(std::string_view symbolName) const {
  auto found{std::vector<const DocEntry *>{}};
  auto walk{[&](auto &&self, const std::vector<DocEntry> &entries) -> void {
    for (const auto &entry : entries) {
      if (isQualifiedNameSuffix(symbolName, entry.qualifiedName))
        found.push_back(&entry);
      self(self, entry.members);
    }
  }};
  for (const auto &mod : modules) walk(walk, mod.entries);
  return found;
}

void DocDatabase::removeHidden() {
  auto filter{[](auto &&self, std::vector<DocEntry> &entries) -> void {
    for (auto &entry : entries) self(self, entry.members);
    entries.erase(std::remove_if(entries.begin(), entries.end(),
                                 [](const DocEntry &entry) {
                                   if (startsWith(entry.name, "_")) return true;
                                   // Namespaces cannot be exported, so
                                   // keep them while they still have
                                   // visible members.
                                   return !entry.isExported &&
                                          (entry.kind != "namespace" ||
                                           entry.members.empty());
                                 }),
                  entries.end());
  }};
  for (auto &mod : modules) filter(filter, mod.entries);
}
//--}

//--{ Print: JSON
namespace {

/// The JSON printer. Just enough of a writer to emit the database, with
/// the escaping, the indentation, and the key punctuation in one place
/// instead of spelled out at every field.
class JSONWriter final {
public:
  explicit JSONWriter(std::string &out) : mOut(out) {}

  void writeEntry(const DocEntry &entry, int depth) {
    mOut += "{\n";
    writeField(depth + 1, "kind", entry.kind);
    writeField(depth + 1, "name", entry.name);
    writeField(depth + 1, "qualifiedName", entry.qualifiedName);
    writeRawField(depth + 1, "isExported", entry.isExported ? "true" : "false");
    writeRawField(depth + 1, "lineNo", std::to_string(entry.lineNo));
    writeField(depth + 1, "signature", entry.signature);
    writeRawField(depth + 1, "nameOffset",
                  entry.nameOffset == DocEntry::NO_NAME_OFFSET
                      ? std::string("null")
                      : std::to_string(entry.nameOffset));
    writeField(depth + 1, "docText", entry.docText);
    // Parameters are small enough to stay on one line each.
    writeKey(depth + 1, "params");
    mOut += '[';
    for (size_t i = 0; i < entry.params.size(); i++) {
      const auto &param{entry.params[i]};
      mOut += i > 0 ? ", {" : "{";
      writeInlineField("name", param.name), mOut += ", ";
      writeInlineField("signature", param.signature), mOut += ", ";
      writeInlineField("docText", param.docText);
      mOut += '}';
    }
    mOut += "],\n";
    writeKey(depth + 1, "members");
    mOut += '[';
    for (size_t i = 0; i < entry.members.size(); i++) {
      if (i > 0) mOut += ", ";
      writeEntry(entry.members[i], depth + 1);
    }
    mOut += "]\n";
    writeIndent(depth);
    mOut += '}';
  }

  void writeModule(const DocModule &mod) {
    writeIndent(2);
    mOut += "{\n";
    writeField(3, "name", mod.name);
    writeField(3, "qualifiedName", mod.qualifiedName);
    writeField(3, "fileName", mod.fileName);
    writeField(3, "docText", mod.docText);
    writeKey(3, "entries");
    mOut += "[\n";
    for (size_t i = 0; i < mod.entries.size(); i++) {
      writeIndent(4);
      writeEntry(mod.entries[i], 4);
      if (i + 1 < mod.entries.size()) mOut += ',';
      mOut += '\n';
    }
    writeIndent(3);
    mOut += "]\n";
    writeIndent(2);
    mOut += '}';
  }

private:
  void writeIndent(int depth) { mOut.append(size_t(2 * depth), ' '); }

  /// Write the indented `"key": ` that opens a field, leaving the value
  /// to the caller.
  void writeKey(int depth, const char *key) {
    writeIndent(depth);
    mOut += '"', mOut += key, mOut += "\": ";
  }

  /// Write `"key": "value",` on a line of its own.
  void writeField(int depth, const char *key, std::string_view value) {
    writeKey(depth, key);
    writeString(value);
    mOut += ",\n";
  }

  /// Write `"key": value,` on a line of its own, where the value is
  /// already JSON: a number, a boolean, or `null`.
  void writeRawField(int depth, const char *key, std::string_view value) {
    writeKey(depth, key);
    mOut += value;
    mOut += ",\n";
  }

  /// Write `"key": "value"` with no indentation and no newline.
  void writeInlineField(const char *key, std::string_view value) {
    mOut += '"', mOut += key, mOut += "\": ";
    writeString(value);
  }

  void writeString(std::string_view str) {
    mOut += '"';
    for (char ch : str) {
      switch (ch) {
      case '"':
        mOut += "\\\"";
        break;
      case '\\':
        mOut += "\\\\";
        break;
      case '\n':
        mOut += "\\n";
        break;
      case '\t':
        mOut += "\\t";
        break;
      case '\r':
        mOut += "\\r";
        break;
      default:
        if (uint8_t(ch) < 0x20) {
          char buf[8]{};
          std::snprintf(buf, sizeof(buf), "\\u%04X", unsigned(uint8_t(ch)));
          mOut += buf;
        } else {
          mOut += ch;
        }
        break;
      }
    }
    mOut += '"';
  }

  std::string &mOut;
};

} // namespace

std::string DocDatabase::printJSON() const {
  auto out{std::string{}};
  auto json{JSONWriter(out)};
  out += "{\n  \"modules\": [\n";
  for (size_t i = 0; i < modules.size(); i++) {
    json.writeModule(modules[i]);
    if (i + 1 < modules.size()) out += ',';
    out += '\n';
  }
  out += "  ]\n}\n";
  return out;
}
//--}

//--{ Print: Markdown

/// Is this the kind of member that documents inline as a bullet rather
/// than as a section of its own, i.e., a struct field or an enumerator?
[[nodiscard]] static bool isMarkdownBullet(const DocEntry &member) {
  return member.kind == "field" || member.kind == "enumerator";
}

/// Write a `- \`signature\` — documentation` bullet, for a `DocParam` or
/// an inline `DocEntry` member, both of which document this way.
template <typename Item>
static void printMarkdownBullet(std::string &out, const Item &item) {
  out += "- `" + item.signature + "`";
  if (!item.docText.empty()) out += " — " + item.docText;
  out += '\n';
}

static void printMarkdownEntry(std::string &out, const DocEntry &entry,
                               int level) {
  out.append(size_t(std::min(level, 6)), '#');
  out += " `" + entry.qualifiedName + "`\n\n";
  out += "*" + entry.kind + "* (line " + std::to_string(entry.lineNo) + ")\n\n";
  out += "```smdl\n" + entry.signature + "\n```\n\n";
  if (!entry.docText.empty()) out += entry.docText + "\n\n";
  if (!entry.params.empty()) {
    for (const auto &param : entry.params) printMarkdownBullet(out, param);
    out += '\n';
  }
  for (const auto &member : entry.members) {
    if (isMarkdownBullet(member)) {
      printMarkdownBullet(out, member);
    } else {
      printMarkdownEntry(out, member, level + 1);
    }
  }
  if (!entry.members.empty() && isMarkdownBullet(entry.members.front()))
    out += '\n';
}

std::string DocDatabase::printMarkdown() const {
  auto out{std::string{}};
  for (const auto &mod : modules) {
    out += "# Module `" + mod.qualifiedName + "`\n\n";
    if (!mod.fileName.empty()) out += "Defined in `" + mod.fileName + "`.\n\n";
    if (!mod.docText.empty()) out += mod.docText + "\n\n";
    for (const auto &entry : mod.entries) printMarkdownEntry(out, entry, 2);
  }
  return out;
}
//--}

} // namespace smdl
