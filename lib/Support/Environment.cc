#include "Support/Environment.h"

#include <cstdlib>
#include <string_view>

namespace smdl {

namespace {
// The text the variable 'name' is set to, or empty if it is unset.
[[nodiscard]] std::string readEnv(const char *name) {
  const char *value{std::getenv(name)};
  return value ? std::string(value) : std::string();
}

// Is a flag set to anything but '0'? Unset and empty mean the flag is
// not given at all, so a caller asks only of nonempty text.
[[nodiscard]] bool parseFlag(std::string_view text) { return text != "0"; }

// How an override parses and spells a value of each type. 'accepted'
// lists the texts that parse, for the warning about one that does not.
template <typename T> struct ValueSyntax;

template <> struct ValueSyntax<bool> final {
  // Every nonempty text is a flag, so none is malformed.
  static constexpr const char *accepted{""};
  [[nodiscard]] static std::optional<bool> parse(std::string_view text) {
    return parseFlag(text);
  }
  [[nodiscard]] static std::string spell(bool value) {
    return value ? "true" : "false";
  }
};

template <> struct ValueSyntax<OptLevel> final {
  static constexpr const char *accepted{"0, 1, 2, or 3"};
  [[nodiscard]] static std::optional<OptLevel> parse(std::string_view text) {
    if (text.size() == 1 && '0' <= text[0] && text[0] <= '3')
      return OptLevel(text[0] - '0');
    return std::nullopt;
  }
  [[nodiscard]] static std::string spell(OptLevel value) {
    return std::to_string(int(value));
  }
};

template <> struct ValueSyntax<LogLevel> final {
  static constexpr const char *accepted{
      R"("debug", "info", "warn", or "error")"};
  [[nodiscard]] static std::optional<LogLevel> parse(std::string_view text) {
    return parseLogLevel(text);
  }
  [[nodiscard]] static std::string spell(LogLevel value) {
    return std::string(logLevelName(value));
  }
};
} // namespace

template <typename T>
EnvOverride<T>::EnvOverride(const char *name, const char *what)
    : mName(name), mWhat(what), mText(readEnv(name)) {
  if (!mText.empty()) mValue = ValueSyntax<T>::parse(mText);
}

template <typename T> void EnvOverride<T>::report(T hostValue) const {
  const bool isMalformed{!mText.empty() && !mValue};
  const bool isOverriding{mValue && *mValue != hostValue};
  if (!(isMalformed || isOverriding) || mIsReported.exchange(true)) return;
  if (isMalformed) {
    SMDL_LOG_WARN("Ignoring ", mName, ", which is ", SpellQuoted(mText),
                  " rather than ", ValueSyntax<T>::accepted);
  } else {
    SMDL_LOG_WARN("Setting ", mWhat, " to ", ValueSyntax<T>::spell(*mValue),
                  ", not the host's ", ValueSyntax<T>::spell(hostValue),
                  ", because ", mName, " is ", SpellQuoted(mText));
  }
}

template class EnvOverride<bool>;
template class EnvOverride<OptLevel>;
template class EnvOverride<LogLevel>;

const Environment &Environment::get() {
  static const Environment environment{};
  return environment;
}

Environment::Environment() : dumpIRDir(readEnv("SMDL_DUMP_IR")) {
  if (std::string text{readEnv("SMDL_PERF_MAP")}; !text.empty())
    shouldWritePerfMap = parseFlag(text);
}

} // namespace smdl
