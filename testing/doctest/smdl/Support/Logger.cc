#include "CompileFixtures.h"

#include "smdl/Support/Logger.h"

#include <algorithm>
#include <cstddef>
#include <iostream>
#include <iterator>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>

namespace {

constexpr smdl::LogLevel LEVELS[]{smdl::LOG_LEVEL_DEBUG, smdl::LOG_LEVEL_INFO,
                                  smdl::LOG_LEVEL_WARN, smdl::LOG_LEVEL_ERROR};

// `str` with its escape sequences taken out, which for anything
// `formatLogMessage()` colors must be the uncolored rendering.
[[nodiscard]] std::string stripEscapes(std::string_view str) {
  std::string result{};
  for (size_t i = 0; i < str.size(); i++) {
    if (str[i] == '\033') {
      i = str.find('m', i);
      if (i == std::string_view::npos) break;
      continue;
    }
    result += str[i];
  }
  return result;
}

// What `formatLogMessage()` makes of `message` as a colored warning,
// with the label taken off the front.
[[nodiscard]] std::string highlighted(std::string_view message) {
  const std::string_view label{
      smdl::logLevelLabel(smdl::LOG_LEVEL_WARN, true, false)};
  std::string str{
      smdl::formatLogMessage(smdl::LOG_LEVEL_WARN, message, true, false)};
  REQUIRE(smdl::startsWith(str, label));
  return str.substr(label.size());
}

using Runs = std::vector<std::string>;

// The runs of that rendering that carry a style of their own, in order
// and with the escape codes taken off: what the highlighter picked out
// of the message, whatever palette it picked it out with. The palette
// is a matter of taste and no test pins it; which text is picked out is
// the behavior.
[[nodiscard]] Runs styledRuns(std::string_view message) {
  const std::string str{highlighted(message)};
  Runs runs{};
  bool isStyled{false};
  for (size_t i = 0; i < str.size(); i++) {
    if (str[i] == '\033') {
      const size_t end{str.find('m', i)};
      if (end == std::string::npos) break;
      const bool isReset{str.substr(i, end - i + 1) == "\033[0m"};
      if (!isReset && !isStyled) runs.emplace_back();
      isStyled = !isReset;
      i = end;
      continue;
    }
    if (isStyled) runs.back() += str[i];
  }
  return runs;
}

// Point a standard stream at a string for a scope.
class CapturedStream final {
public:
  explicit CapturedStream(std::ostream &stream)
      : mStream(stream), mPrevious(stream.rdbuf(mBuffer.rdbuf())) {}

  CapturedStream(const CapturedStream &) = delete;

  CapturedStream &operator=(const CapturedStream &) = delete;

  ~CapturedStream() { mStream.rdbuf(mPrevious); }

  [[nodiscard]] std::string str() const { return mBuffer.str(); }

private:
  std::ostream &mStream;

  std::ostringstream mBuffer;

  std::streambuf *mPrevious{};
};

// The two variables `shouldUseColors()` reads, pinned for a scope. An
// empty value counts as unset, the same as for the locale variables.
class ScopedColorEnv final {
public:
  ScopedColorEnv(const std::string &term, const std::string &noColor)
      : mTerm("TERM", term), mNoColor("NO_COLOR", noColor) {}

private:
  ScopedEnv mTerm;

  ScopedEnv mNoColor;
};

// The length of the UTF-8 sequence that `lead` begins, or 0 if it does
// not begin one.
[[nodiscard]] size_t utf8SequenceLength(unsigned char lead) {
  if (lead < 0x80) return 1;
  if ((lead & 0xE0) == 0xC0) return 2;
  if ((lead & 0xF0) == 0xE0) return 3;
  if ((lead & 0xF8) == 0xF0) return 4;
  return 0;
}

// The three variables `localeIsUTF8()` reads, pinned for a scope so that
// the developer's own locale cannot decide a test. An empty value is
// skipped, the same as an unset one.
class ScopedLocale final {
public:
  ScopedLocale(const std::string &lcAll, const std::string &lcCType,
               const std::string &lang)
      : mLCAll("LC_ALL", lcAll), mLCCType("LC_CTYPE", lcCType),
        mLang("LANG", lang) {}

private:
  ScopedEnv mLCAll;

  ScopedEnv mLCCType;

  ScopedEnv mLang;
};

} // namespace

TEST_CASE("Logger: level labels") {
  SUBCASE("ASCII labels are bracketed words, info included") {
    CHECK(smdl::logLevelLabel(smdl::LOG_LEVEL_DEBUG, false, false) ==
          "[debug] ");
    CHECK(smdl::logLevelLabel(smdl::LOG_LEVEL_INFO, false, false) == "[info] ");
    CHECK(smdl::logLevelLabel(smdl::LOG_LEVEL_WARN, false, false) == "[warn] ");
    CHECK(smdl::logLevelLabel(smdl::LOG_LEVEL_ERROR, false, false) ==
          "[error] ");
  }
  SUBCASE("Colored labels wrap the same text and reset before the space") {
    for (const bool useUnicode : {false, true}) {
      for (const auto level : LEVELS) {
        const std::string_view plain{
            smdl::logLevelLabel(level, false, useUnicode)};
        const std::string colored{
            std::string(smdl::logLevelLabel(level, true, useUnicode))};
        const std::string tail{std::string(plain.substr(0, plain.size() - 1)) +
                               "\033[0m "};
        CHECK(smdl::startsWith(colored, "\033["));
        REQUIRE(colored.size() > tail.size());
        CHECK(colored.substr(colored.size() - tail.size()) == tail);
      }
    }
  }
  SUBCASE("Each Unicode label is one non-ASCII code point and a space") {
    for (const auto level : LEVELS) {
      const std::string_view label{smdl::logLevelLabel(level, false, true)};
      REQUIRE(!label.empty());
      const size_t length{
          utf8SequenceLength(static_cast<unsigned char>(label[0]))};
      CHECK(length > 1);
      CHECK(label.size() == length + 1);
      CHECK(label.back() == ' ');
    }
  }
  SUBCASE("No two levels share a label") {
    for (const bool useUnicode : {false, true})
      for (size_t i = 0; i < std::size(LEVELS); i++)
        for (size_t j = i + 1; j < std::size(LEVELS); j++)
          CHECK(smdl::logLevelLabel(LEVELS[i], false, useUnicode) !=
                smdl::logLevelLabel(LEVELS[j], false, useUnicode));
  }
}

TEST_CASE("Logger: a message below the minimum level is never built") {
  const CollectedLog logged{"built"};
  REQUIRE_FALSE(smdl::Logger::get().isEnabled(smdl::LOG_LEVEL_DEBUG));
  REQUIRE(smdl::Logger::get().isEnabled(smdl::LOG_LEVEL_INFO));
  int numBuilt{0};
  const auto build{[&] {
    numBuilt++;
    return std::string("built");
  }};
  SMDL_LOG_DEBUG(build());
  CHECK(numBuilt == 0);
  CHECK(logged.messages().empty());
  SMDL_LOG_INFO(build());
  CHECK(numBuilt == 1);
  CHECK(logged.messages().size() == 1);
}

TEST_CASE("Logger: Unicode mode") {
  SUBCASE("Always and never ignore the terminal and the locale") {
    for (const char *lang : {"C", "en_US.UTF-8"}) {
      const ScopedLocale locale{"", "", lang};
      for (const bool isTerminal : {false, true}) {
        CHECK(smdl::shouldUseUnicode(smdl::UNICODE_MODE_ALWAYS, isTerminal));
        CHECK_FALSE(
            smdl::shouldUseUnicode(smdl::UNICODE_MODE_NEVER, isTerminal));
      }
    }
  }
  SUBCASE("Auto needs a terminal") {
    const ScopedLocale locale{"", "", "en_US.UTF-8"};
    CHECK(smdl::shouldUseUnicode(smdl::UNICODE_MODE_AUTO, true));
    CHECK_FALSE(smdl::shouldUseUnicode(smdl::UNICODE_MODE_AUTO, false));
  }
  SUBCASE("Auto needs a UTF-8 locale") {
    const ScopedLocale locale{"", "", "C"};
    CHECK_FALSE(smdl::shouldUseUnicode(smdl::UNICODE_MODE_AUTO, true));
  }
  SUBCASE("The first non-empty locale variable decides") {
    {
      const ScopedLocale locale{"C", "en_US.UTF-8", "en_US.UTF-8"};
      CHECK_FALSE(smdl::localeIsUTF8());
    }
    {
      const ScopedLocale locale{"", "C.UTF-8", "C"};
      CHECK(smdl::localeIsUTF8());
    }
    {
      const ScopedLocale locale{"", "", ""};
      CHECK_FALSE(smdl::localeIsUTF8());
    }
  }
  SUBCASE("Either spelling of UTF-8 matches in any case") {
    for (const char *lang :
         {"en_US.UTF-8", "en_US.utf8", "C.Utf-8", "de_DE.UTF8"}) {
      const ScopedLocale locale{"", "", lang};
      CHECK_MESSAGE(smdl::localeIsUTF8(), lang);
    }
    const ScopedLocale locale{"", "", "en_US.ISO-8859-1"};
    CHECK_FALSE(smdl::localeIsUTF8());
  }
}

TEST_CASE("Logger: color mode") {
  SUBCASE("Always and never ignore the terminal and the environment") {
    for (const char *term : {"xterm-256color", "dumb", ""}) {
      for (const char *noColor : {"", "1"}) {
        const ScopedColorEnv env{term, noColor};
        for (const bool isTerminal : {false, true}) {
          CHECK(
              smdl::shouldUseColors(smdl::ANSI_COLOR_MODE_ALWAYS, isTerminal));
          CHECK_FALSE(
              smdl::shouldUseColors(smdl::ANSI_COLOR_MODE_NEVER, isTerminal));
        }
      }
    }
  }
  SUBCASE("Auto needs a terminal") {
    const ScopedColorEnv env{"xterm-256color", ""};
    CHECK(smdl::shouldUseColors(smdl::ANSI_COLOR_MODE_AUTO, true));
    CHECK_FALSE(smdl::shouldUseColors(smdl::ANSI_COLOR_MODE_AUTO, false));
  }
  SUBCASE("Auto honors NO_COLOR whatever its value, unless it is empty") {
    for (const char *noColor : {"1", "0", "false"}) {
      const ScopedColorEnv env{"xterm-256color", noColor};
      CHECK_FALSE_MESSAGE(
          smdl::shouldUseColors(smdl::ANSI_COLOR_MODE_AUTO, true), noColor);
    }
  }
  SUBCASE("Auto wants a TERM that is set and is not dumb") {
    for (const char *term : {"dumb", ""}) {
      const ScopedColorEnv env{term, ""};
      CHECK_FALSE_MESSAGE(
          smdl::shouldUseColors(smdl::ANSI_COLOR_MODE_AUTO, true), term);
    }
  }
}

TEST_CASE("Logger: message formatting") {
  const std::string_view samples[]{
      "plain words",
      "[a.mdl:3:1] cannot load 'x.png': file not found",
      "::a::m declared at [a.mdl:12]\n  ::b::m declared at [b.mdl:4]",
      "don't split 'bob's.png' or an unclosed 'quote",
      "done [0.5 seconds] at [12:30]",
      "[a.mdl:2:5] error\n  2 | int i = 'x';\n    |         ^",
      "",
      "\n"};
  SUBCASE("Without colors the message is exactly as given") {
    for (const bool useUnicode : {false, true})
      for (const auto level : LEVELS)
        for (const auto message : samples)
          CHECK(smdl::formatLogMessage(level, message, false, useUnicode) ==
                std::string(smdl::logLevelLabel(level, false, useUnicode)) +
                    std::string(message));
  }
  SUBCASE("Taking the colors out gives back the plain rendering") {
    for (const bool useUnicode : {false, true})
      for (const auto level : LEVELS)
        for (const auto message : samples)
          CHECK(stripEscapes(
                    smdl::formatLogMessage(level, message, true, useUnicode)) ==
                smdl::formatLogMessage(level, message, false, useUnicode));
  }
  SUBCASE("A location is picked out wherever it is") {
    CHECK(styledRuns("[a.mdl:3:1] x") == Runs{"[a.mdl:3:1]"});
    CHECK(styledRuns("::m declared at [a.mdl:12]") == Runs{"[a.mdl:12]"});
    CHECK(styledRuns("[<builtin ::df>:1036:12] y") ==
          Runs{"[<builtin ::df>:1036:12]"});
  }
  SUBCASE("A bracket that is not a location is left alone") {
    for (const char *message :
         {"done [0.5 seconds]", "at [12:30]", "[12:30:45] x", "[opaque]",
          "x[a.mdl:3]", "[a.mdl:3", "[a.mdl:x]", "[:3]"})
      CHECK(highlighted(message) == message);
  }
  SUBCASE("The location is the only thing a message line picks out") {
    // The quoted strings, the numbers and the dimensions a message is
    // built from are left exactly as they were written.
    CHECK(styledRuns("[a.mdl:3:1] cannot load 'x2.png' after 4 tries") ==
          Runs{"[a.mdl:3:1]"});
    for (const char *message :
         {"cannot load 'x.png': y", "11 bands over 380.0-780.0 nm",
          "a 1280x720 frame at 35.82x23.88 mm", "weighs 1.234e-4 of it"})
      CHECK(highlighted(message) == message);
  }
  SUBCASE("A source snippet keeps its source line as written") {
    const smdl::Error error{
        compileError("#smdl\nexec { int i = nope; } // not 'this' [a:1]\n")};
    const std::string message{error.message + error.snippet};
    const Runs runs{styledRuns(message)};
    const auto picksOut{[&](const std::string &run) {
      return std::find(runs.begin(), runs.end(), run) != runs.end();
    }};
    CHECK(stripEscapes(highlighted(message)) == message);
    REQUIRE(!runs.empty());
    CHECK(runs.front() == "[<string ::diag>:2:16]");
    CHECK(picksOut("  2 |"));
    CHECK(picksOut("    |"));
    CHECK(runs.back() == "^");
    // The source line is left as written: its quotes and brackets are
    // code rather than a message's.
    CHECK_FALSE(picksOut("'this'"));
    CHECK_FALSE(picksOut("[a:1]"));
  }
  SUBCASE("A caret line without a gutter marks its source line too") {
    CHECK(styledRuns("refused\n  fovy 'x'\n  ^~~~") == Runs{"^~~~"});
  }
  SUBCASE("A debug message is dimmed whole and nothing in it highlighted") {
    const std::string_view message{
        std::string_view("New material '::m' at [a.mdl:3]")};
    const std::string str{
        smdl::formatLogMessage(smdl::LOG_LEVEL_DEBUG, message, true, false)};
    CHECK(smdl::startsWith(str, "\033["));
    // Whole, which is to say the message survives in it unbroken.
    CHECK_CONTAINS(str, std::string(message));
  }
  SUBCASE("A message with escape codes of its own is left as it is") {
    const std::string_view message{
        std::string_view("already \033[1mbold\033[0m 'x'")};
    for (const auto level : LEVELS)
      CHECK(smdl::formatLogMessage(level, message, true, false) ==
            std::string(smdl::logLevelLabel(level, true, false)) +
                std::string(message));
  }
}

TEST_CASE("Logger: the default sinks print what formatLogMessage renders") {
  const std::string_view message{"cannot load 'x.png'"};
  for (const auto colorMode :
       {smdl::ANSI_COLOR_MODE_ALWAYS, smdl::ANSI_COLOR_MODE_NEVER}) {
    const bool useColors{colorMode == smdl::ANSI_COLOR_MODE_ALWAYS};
    const std::string expected{smdl::formatLogMessage(smdl::LOG_LEVEL_WARN,
                                                      message, useColors,
                                                      false) +
                               "\n"};
    {
      smdl::LogSinks::PrintToCerr sink{smdl::UNICODE_MODE_NEVER};
      sink.setColorMode(colorMode);
      const CapturedStream captured{std::cerr};
      sink.logMessage(smdl::LOG_LEVEL_WARN, message);
      CHECK(captured.str() == expected);
    }
    {
      smdl::LogSinks::PrintToCout sink{smdl::UNICODE_MODE_NEVER};
      sink.setColorMode(colorMode);
      const CapturedStream captured{std::cout};
      sink.logMessage(smdl::LOG_LEVEL_WARN, message);
      CHECK(captured.str() == expected);
    }
  }
}
