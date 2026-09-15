#include "CompileFixtures.h"

#include "smdl/Support/Logger.h"

#include <cstddef>
#include <iostream>
#include <iterator>
#include <sstream>
#include <string>
#include <string_view>

namespace {

constexpr smdl::LogLevel LEVELS[]{smdl::LOG_LEVEL_DEBUG, smdl::LOG_LEVEL_INFO,
                                  smdl::LOG_LEVEL_WARN, smdl::LOG_LEVEL_ERROR};

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

} // namespace

TEST_CASE("Logger: level labels") {
  SUBCASE("Every label is a bracketed word and a space, info included") {
    CHECK(smdl::logLevelLabel(smdl::LOG_LEVEL_DEBUG) == "[debug] ");
    CHECK(smdl::logLevelLabel(smdl::LOG_LEVEL_INFO) == "[info] ");
    CHECK(smdl::logLevelLabel(smdl::LOG_LEVEL_WARN) == "[warn] ");
    CHECK(smdl::logLevelLabel(smdl::LOG_LEVEL_ERROR) == "[error] ");
  }
  SUBCASE("No label carries an escape code, so a redirected log is plain") {
    for (const auto level : LEVELS) {
      const std::string_view label{smdl::logLevelLabel(level)};
      CHECK(label.find('\033') == std::string_view::npos);
      CHECK(label.back() == ' ');
    }
  }
  SUBCASE("No two levels share a label") {
    for (size_t i = 0; i < std::size(LEVELS); i++)
      for (size_t j = i + 1; j < std::size(LEVELS); j++)
        CHECK(smdl::logLevelLabel(LEVELS[i]) != smdl::logLevelLabel(LEVELS[j]));
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

TEST_CASE("Logger: the default sinks print the label and the message") {
  const std::string_view message{"cannot load 'x.png'"};
  // A message with an escape code of its own, to pin that a sink hands
  // the text through rather than rendering it: what a host's own sink
  // does with a message is the host's business.
  const std::string_view styled{"already \033[1mbold\033[0m"};
  for (const auto level : LEVELS) {
    for (const auto text : {message, styled}) {
      const std::string expected{std::string(smdl::logLevelLabel(level)) +
                                 std::string(text) + "\n"};
      {
        smdl::LogSinks::PrintToCerr sink{};
        const CapturedStream captured{std::cerr};
        sink.logMessage(level, text);
        CHECK(captured.str() == expected);
      }
      {
        smdl::LogSinks::PrintToCout sink{};
        const CapturedStream captured{std::cout};
        sink.logMessage(level, text);
        CHECK(captured.str() == expected);
      }
    }
  }
}
