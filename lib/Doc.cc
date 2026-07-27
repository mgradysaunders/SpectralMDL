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

/// Normalize a raw signature slice: strip comments and `[[ ... ]]`
/// annotation blocks, collapse whitespace runs to single spaces, and
/// tidy spacing around punctuation.
[[nodiscard]] std::string normalizeSignature(std::string_view src) {
  auto result{std::string{}};
  result.reserve(src.size());
  auto lastWasSpace{true}; // Also trims leading whitespace
  auto addSpace{[&] {
    if (!lastWasSpace) {
      if (!result.empty() && result.back() != '(') result += ' ';
      lastWasSpace = true;
    }
  }};
  size_t i{};
  while (i < src.size()) {
    auto remaining{src.substr(i)};
    if (startsWith(remaining, "//")) {
      while (i < src.size() && src[i] != '\n') i++;
      addSpace();
    } else if (startsWith(remaining, "/*")) {
      auto close{src.find("*/", i + 2)};
      i = close == std::string_view::npos ? src.size() : close + 2;
      addSpace();
    } else if (startsWith(remaining, "[[")) {
      auto close{src.find("]]", i + 2)};
      i = close == std::string_view::npos ? src.size() : close + 2;
      addSpace();
    } else if (isSpace(src[i])) {
      i++;
      addSpace();
    } else {
      if ((src[i] == ',' || src[i] == ')') && !result.empty() &&
          result.back() == ' ') {
        result.pop_back();
      }
      result += src[i++];
      lastWasSpace = false;
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
    case AST::DeclKind::Tag: {
      const auto &d{static_cast<const AST::Tag &>(decl)};
      auto entry{makeEntry(decl, "tag", d.name.srcName)};
      entry.signature =
          declPrefix(decl) +
          normalizeSignature(slice(decl.srcLoc.i, beginOf(d.srcSemicolon)));
      entry.docText = docTextOf(decl.srcDocComment, {}, nullptr);
      out.push_back(std::move(entry));
      break;
    }
    case AST::DeclKind::Typedef: {
      const auto &d{static_cast<const AST::Typedef &>(decl)};
      auto entry{makeEntry(decl, "typedef", d.name.srcName)};
      entry.signature =
          declPrefix(decl) +
          normalizeSignature(slice(decl.srcLoc.i, beginOf(d.srcSemicolon)));
      entry.docText = docTextOf(decl.srcDocComment, {}, nullptr);
      out.push_back(std::move(entry));
      break;
    }
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
    entry.signature =
        declPrefix(decl) +
        normalizeSignature(slice(decl.srcLoc.i, endOf(decl.params.srcParenR)));
    entry.docText = docTextOf(decl.srcDocComment, {}, decl.annotations.get());
    extractParams(decl.params, entry.params);
    out.push_back(std::move(entry));
  }

  void extractEnum(const AST::Enum &decl, std::vector<DocEntry> &out) {
    auto entry{makeEntry(decl, "enum", decl.name.srcName)};
    entry.signature =
        declPrefix(decl) +
        normalizeSignature(slice(decl.srcLoc.i, endOf(decl.name.srcName)));
    entry.docText = docTextOf(decl.srcDocComment, {}, decl.annotations.get());
    for (const auto &declarator : decl.declarators) {
      auto member{DocEntry{}};
      member.kind = "enumerator";
      member.name = std::string(declarator.name.srcName);
      // NOTE: Enum values are injected into the enclosing scope, so the
      // qualified name does not include the enum name.
      member.qualifiedName = qualify(member.name);
      member.isExported = entry.isExported;
      member.lineNo = declarator.srcLoc.lineNo;
      member.signature = normalizeSignature(
          slice(declarator.srcLoc.i, !declarator.srcComma.empty()
                                         ? beginOf(declarator.srcComma)
                                         : beginOf(decl.srcBraceR)));
      member.docText =
          docTextOf(declarator.srcDocComment, declarator.srcDocCommentTrailing,
                    declarator.annotations.get());
      entry.members.push_back(std::move(member));
    }
    out.push_back(std::move(entry));
  }

  void extractFunction(const AST::Function &decl, std::vector<DocEntry> &out) {
    auto entry{makeEntry(decl, "function", decl.name.srcName)};
    entry.signature = declPrefix(decl) +
                      normalizeSignature(slice(
                          decl.srcLoc.i, !decl.srcFrequency.empty()
                                             ? endOf(decl.srcFrequency)
                                             : endOf(decl.params.srcParenR)));
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
    entry.docText = docTextOf(decl.srcDocComment, {}, nullptr);
    auto prevPrefix{mQualifiedNamePrefix};
    mQualifiedNamePrefix = entry.qualifiedName;
    for (const auto &inner : decl.decls) extractDecl(*inner, entry.members);
    mQualifiedNamePrefix = std::move(prevPrefix);
    out.push_back(std::move(entry));
  }

  void extractStruct(const AST::Struct &decl, std::vector<DocEntry> &out) {
    auto entry{makeEntry(decl, "struct", decl.name.srcName)};
    entry.signature =
        declPrefix(decl) +
        normalizeSignature(slice(decl.srcLoc.i, endOf(decl.name.srcName)));
    entry.docText = docTextOf(decl.srcDocComment, {}, decl.annotations.get());
    for (const auto &field : decl.fields) {
      auto member{DocEntry{}};
      member.kind = "field";
      member.name = std::string(field.name.srcName);
      member.qualifiedName = entry.qualifiedName + "::" + member.name;
      member.isExported = entry.isExported;
      member.lineNo = field.srcLoc.lineNo;
      member.signature = normalizeSignature(
          slice(field.srcLoc.i, beginOf(field.srcSemicolon)));
      member.docText =
          docTextOf(field.srcDocComment, field.srcDocCommentTrailing,
                    field.annotations.get());
      entry.members.push_back(std::move(member));
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
      auto declaratorSrc{
          slice(declarator.srcLoc.i, !declarator.srcComma.empty()
                                         ? beginOf(declarator.srcComma)
                                         : beginOf(decl.srcSemicolon))};
      entry.signature =
          declPrefix(decl) + normalizeSignature(std::string(typeSrc) + " " +
                                                std::string(declaratorSrc));
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
      docParam.signature = normalizeSignature(slice(
          param.srcLoc.i, !param.srcComma.empty() ? beginOf(param.srcComma)
                                                  : beginOf(params.srcParenR)));
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

void DocDatabase::removeNonExported() {
  auto filter{[](auto &&self, std::vector<DocEntry> &entries) -> void {
    for (auto &entry : entries)
      if (entry.kind == "namespace") self(self, entry.members);
    entries.erase(std::remove_if(entries.begin(), entries.end(),
                                 [](const DocEntry &entry) {
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
static void printJSONString(std::string &out, std::string_view str) {
  out += '"';
  for (char ch : str) {
    switch (ch) {
    case '"':
      out += "\\\"";
      break;
    case '\\':
      out += "\\\\";
      break;
    case '\n':
      out += "\\n";
      break;
    case '\t':
      out += "\\t";
      break;
    case '\r':
      out += "\\r";
      break;
    default:
      if (uint8_t(ch) < 0x20) {
        char buf[8]{};
        std::snprintf(buf, sizeof(buf), "\\u%04X", unsigned(uint8_t(ch)));
        out += buf;
      } else {
        out += ch;
      }
      break;
    }
  }
  out += '"';
}

static void printJSONIndent(std::string &out, int depth) {
  out.append(size_t(2 * depth), ' ');
}

static void printJSONEntry(std::string &out, const DocEntry &entry, int depth) {
  auto field{[&](const char *key, std::string_view value, bool comma = true) {
    printJSONIndent(out, depth + 1);
    out += '"', out += key, out += "\": ";
    printJSONString(out, value);
    if (comma) out += ',';
    out += '\n';
  }};
  out += "{\n";
  field("kind", entry.kind);
  field("name", entry.name);
  field("qualifiedName", entry.qualifiedName);
  printJSONIndent(out, depth + 1);
  out += "\"isExported\": ", out += entry.isExported ? "true" : "false",
      out += ",\n";
  printJSONIndent(out, depth + 1);
  out += "\"lineNo\": " + std::to_string(entry.lineNo) + ",\n";
  field("signature", entry.signature);
  field("docText", entry.docText);
  printJSONIndent(out, depth + 1);
  out += "\"params\": [";
  for (size_t i = 0; i < entry.params.size(); i++) {
    out += i > 0 ? ", {" : "{";
    out += "\"name\": ", printJSONString(out, entry.params[i].name);
    out += ", \"signature\": ", printJSONString(out, entry.params[i].signature);
    out += ", \"docText\": ", printJSONString(out, entry.params[i].docText);
    out += '}';
  }
  out += "],\n";
  printJSONIndent(out, depth + 1);
  out += "\"members\": [";
  for (size_t i = 0; i < entry.members.size(); i++) {
    if (i > 0) out += ", ";
    printJSONEntry(out, entry.members[i], depth + 1);
  }
  out += "]\n";
  printJSONIndent(out, depth);
  out += '}';
}

std::string DocDatabase::printJSON() const {
  auto out{std::string{}};
  out += "{\n  \"modules\": [\n";
  for (size_t i = 0; i < modules.size(); i++) {
    const auto &mod{modules[i]};
    printJSONIndent(out, 2);
    out += "{\n";
    auto field{[&](const char *key, std::string_view value) {
      printJSONIndent(out, 3);
      out += '"', out += key, out += "\": ";
      printJSONString(out, value);
      out += ",\n";
    }};
    field("name", mod.name);
    field("qualifiedName", mod.qualifiedName);
    field("fileName", mod.fileName);
    field("docText", mod.docText);
    printJSONIndent(out, 3);
    out += "\"entries\": [\n";
    for (size_t j = 0; j < mod.entries.size(); j++) {
      printJSONIndent(out, 4);
      printJSONEntry(out, mod.entries[j], 4);
      if (j + 1 < mod.entries.size()) out += ',';
      out += '\n';
    }
    printJSONIndent(out, 3);
    out += "]\n";
    printJSONIndent(out, 2);
    out += '}';
    if (i + 1 < modules.size()) out += ',';
    out += '\n';
  }
  out += "  ]\n}\n";
  return out;
}
//--}

//--{ Print: Markdown
static void printMarkdownEntry(std::string &out, const DocEntry &entry,
                               int level) {
  out.append(size_t(std::min(level, 6)), '#');
  out += " `" + entry.qualifiedName + "`\n\n";
  out += "*" + entry.kind + "* (line " + std::to_string(entry.lineNo) + ")\n\n";
  out += "```smdl\n" + entry.signature + "\n```\n\n";
  if (!entry.docText.empty()) out += entry.docText + "\n\n";
  if (!entry.params.empty()) {
    for (const auto &param : entry.params) {
      out += "- `" + param.signature + "`";
      if (!param.docText.empty()) out += " — " + param.docText;
      out += '\n';
    }
    out += '\n';
  }
  for (const auto &member : entry.members) {
    if (member.kind == "field" || member.kind == "enumerator") {
      out += "- `" + member.signature + "`";
      if (!member.docText.empty()) out += " — " + member.docText;
      out += '\n';
    } else {
      printMarkdownEntry(out, member, level + 1);
    }
  }
  if (!entry.members.empty() && (entry.members.front().kind == "field" ||
                                 entry.members.front().kind == "enumerator"))
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
