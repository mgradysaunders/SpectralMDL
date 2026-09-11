#include "CompileFixtures.h"

#include "smdl/Support/Logger.h"

#include <cstddef>
#include <iterator>
#include <string>

namespace {

constexpr smdl::LogLevel LEVELS[]{smdl::LOG_LEVEL_DEBUG, smdl::LOG_LEVEL_INFO,
                                  smdl::LOG_LEVEL_WARN, smdl::LOG_LEVEL_ERROR};

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
        const auto plain{smdl::logLevelLabel(level, false, useUnicode)};
        const auto colored{
            std::string(smdl::logLevelLabel(level, true, useUnicode))};
        const auto tail{std::string(plain.substr(0, plain.size() - 1)) +
                        "\033[0m "};
        CHECK(smdl::startsWith(colored, "\033["));
        REQUIRE(colored.size() > tail.size());
        CHECK(colored.substr(colored.size() - tail.size()) == tail);
      }
    }
  }
  SUBCASE("Each Unicode label is one non-ASCII code point and a space") {
    for (const auto level : LEVELS) {
      const auto label{smdl::logLevelLabel(level, false, true)};
      REQUIRE(!label.empty());
      const auto length{
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
  auto numBuilt{0};
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
