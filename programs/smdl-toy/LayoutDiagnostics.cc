#include "LayoutDiagnostics.h"

#include "smdl/Support/Error.h"
#include "smdl/Support/StringHelpers.h"

#include <algorithm>
#include <fstream>
#include <iostream>
#include <iterator>

LayoutSource::LayoutSource(std::string fileName, std::string text)
    : fileName(std::move(fileName)), text(std::move(text)) {
  mLineStarts.push_back(0);
  for (uint32_t i = 0; i < uint32_t(this->text.size()); i++)
    if (this->text[i] == '\n') mLineStarts.push_back(i + 1);
}

LayoutSource::LineAndColumn
LayoutSource::lineAndColumn(uint32_t offset) const noexcept {
  offset = std::min(offset, uint32_t(text.size()));
  const auto itr{
      std::upper_bound(mLineStarts.begin(), mLineStarts.end(), offset)};
  const auto lineIndex{uint32_t(itr - mLineStarts.begin()) - 1};
  return {lineIndex + 1, offset - mLineStarts[lineIndex] + 1};
}

std::string_view LayoutSource::lineText(uint32_t lineNo) const noexcept {
  if (lineNo < 1 || lineNo > lineCount()) return {};
  const auto start{mLineStarts[lineNo - 1]};
  auto end{lineNo < lineCount() ? mLineStarts[lineNo] : uint32_t(text.size())};
  if (end > start && text[end - 1] == '\n') end--;
  if (end > start && text[end - 1] == '\r') end--;
  return std::string_view(text).substr(start, end - start);
}

LayoutDiagnostic &LayoutDiagnostic::note(LayoutLocation noteLoc,
                                         std::string noteMessage) {
  auto &added{notes.emplace_back()};
  added.kind = Kind::NOTE;
  added.location = noteLoc;
  added.message = std::move(noteMessage);
  return *this;
}

const LayoutSource &LayoutDiagnostics::loadSource(const std::string &fileName) {
  auto stream{std::ifstream(fileName, std::ios::binary)};
  if (!stream)
    throw smdl::Error(
        smdl::concat("cannot open layout file ", smdl::QuotedPath(fileName)));
  return mSources.emplace_back(
      fileName, std::string(std::istreambuf_iterator<char>(stream),
                            std::istreambuf_iterator<char>()));
}

const LayoutSource &LayoutDiagnostics::addSource(std::string fileName,
                                                 std::string text) {
  return mSources.emplace_back(std::move(fileName), std::move(text));
}

LayoutDiagnostic &LayoutDiagnostics::error(LayoutLocation location,
                                           std::string message) {
  auto &added{mDiagnostics.emplace_back()};
  added.kind = LayoutDiagnostic::Kind::ERROR;
  added.location = location;
  added.message = std::move(message);
  mErrorCount++;
  return added;
}

LayoutDiagnostic &LayoutDiagnostics::warn(LayoutLocation location,
                                          std::string message) {
  auto &added{mDiagnostics.emplace_back()};
  added.kind = LayoutDiagnostic::Kind::WARNING;
  added.location = location;
  added.message = std::move(message);
  mWarningCount++;
  return added;
}

void LayoutDiagnostics::noteAllFrom(size_t firstIndex, LayoutLocation noteLoc,
                                    std::string noteMessage) {
  for (size_t i = firstIndex; i < mDiagnostics.size(); i++)
    mDiagnostics[i].note(noteLoc, noteMessage);
}

namespace {

// The ANSI palette, clang-flavored: the location and message render bold,
// the kind renders bold in its own color, and the caret marker renders
// green. Everything funnels through these two helpers so that the
// uncolored rendering is byte-for-byte free of escapes, which is what the
// tests pin down and what tools parsing stderr require.
constexpr std::string_view ANSI_BOLD = "\033[1m";
constexpr std::string_view ANSI_RESET = "\033[0m";
constexpr std::string_view ANSI_GREEN = "\033[1;32m";

[[nodiscard]] std::string_view kindLabel(LayoutDiagnostic::Kind kind) noexcept {
  switch (kind) {
  case LayoutDiagnostic::Kind::ERROR:
    return "error";
  case LayoutDiagnostic::Kind::WARNING:
    return "warning";
  case LayoutDiagnostic::Kind::NOTE:
    return "note";
  }
  return "error";
}

[[nodiscard]] std::string_view kindColor(LayoutDiagnostic::Kind kind) noexcept {
  switch (kind) {
  case LayoutDiagnostic::Kind::ERROR:
    return "\033[1;31m"; // bold red
  case LayoutDiagnostic::Kind::WARNING:
    return "\033[1;33m"; // bold yellow
  case LayoutDiagnostic::Kind::NOTE:
    return "\033[1;36m"; // bold cyan
  }
  return ANSI_BOLD;
}

// One diagnostic or note, without recursing into its notes.
void renderOne(std::string &result, const LayoutDiagnostic &diagnostic,
               bool colored) {
  const auto &location{diagnostic.location};
  if (colored) result += ANSI_BOLD;
  if (location) {
    const auto [lineNo,
                charNo]{location.source->lineAndColumn(location.offset)};
    result +=
        smdl::concat(location.source->fileName, ":", lineNo, ":", charNo, ": ");
  }
  if (colored) result += kindColor(diagnostic.kind);
  result += kindLabel(diagnostic.kind);
  result += ':';
  if (colored) result += ANSI_RESET, result += ANSI_BOLD;
  result += ' ';
  result += diagnostic.message;
  if (colored) result += ANSI_RESET;
  result += '\n';
  if (!location) return;
  // The excerpt: the marked line verbatim, then the marker under it. The
  // marker prefix copies the line's own tabs so the caret column agrees
  // with the text however wide a tab displays; the marked range clamps to
  // the end of the line it starts on.
  const auto [lineNo, charNo]{location.source->lineAndColumn(location.offset)};
  const auto line{location.source->lineText(lineNo)};
  result += line;
  result += '\n';
  const auto column{size_t(charNo) - 1};
  for (size_t i = 0; i < column; i++)
    result += i < line.size() && line[i] == '\t' ? '\t' : ' ';
  if (colored) result += ANSI_GREEN;
  result += '^';
  if (location.length > 1 && column < line.size()) {
    const auto available{line.size() - column - 1};
    const auto tildes{std::min(size_t(location.length) - 1, available)};
    result.append(tildes, '~');
  }
  if (colored) result += ANSI_RESET;
  result += '\n';
}

} // namespace

std::string LayoutDiagnostics::render(const LayoutDiagnostic &diagnostic,
                                      bool colored) {
  auto result{std::string()};
  renderOne(result, diagnostic, colored);
  for (const auto &note : diagnostic.notes) renderOne(result, note, colored);
  return result;
}

std::string LayoutDiagnostics::renderAll(bool colored) const {
  auto result{std::string()};
  for (const auto &diagnostic : mDiagnostics)
    result += render(diagnostic, colored);
  if (!mDiagnostics.empty()) {
    if (colored) result += ANSI_BOLD;
    result += summary();
    if (colored) result += ANSI_RESET;
    result += '\n';
  }
  return result;
}

std::string LayoutDiagnostics::summary() const {
  if (mDiagnostics.empty()) return {};
  auto result{std::string()};
  if (mErrorCount > 0)
    result +=
        smdl::concat(mErrorCount, mErrorCount == 1 ? " error" : " errors");
  if (mWarningCount > 0) {
    if (!result.empty()) result += ", ";
    result += smdl::concat(mWarningCount,
                           mWarningCount == 1 ? " warning" : " warnings");
  }
  return result;
}

void LayoutDiagnostics::printAll(bool colored) const {
  std::cerr << renderAll(colored);
}
