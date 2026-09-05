#include <cmath>
#include <cstdlib>

#include "smdl/Support/Strings.h"

#include "IO/RenderHeader.h"

namespace {

// What makes these the renderer's fields rather than the format's.
constexpr const char *PREFIX{"render "};

// How each field type crosses the header, written and read side by side
// because the two have to agree.
void spell(std::string &line, uint64_t value) { line += std::to_string(value); }
void spell(std::string &line, double value) { line += std::to_string(value); }
void spell(std::string &line, bool value) { line += value ? '1' : '0'; }
void spell(std::string &line, const std::string &value) { line += value; }

void parse(const std::string &text, uint64_t &value) {
  value = std::strtoull(text.c_str(), nullptr, 10);
}

void parse(const std::string &text, double &value) {
  // A tally is a duration, so anything a corrupt or hand-edited header
  // offers that is not one starts the count over rather than poisoning
  // every later session's total.
  const double seconds{std::strtod(text.c_str(), nullptr)};
  value = std::isfinite(seconds) && seconds > 0.0 ? seconds : 0.0;
}

void parse(const std::string &text, bool &value) { value = text != "0"; }

void parse(const std::string &text, std::string &value) { value = text; }

// The field table: one row per field, walked by both directions, which
// is what keeps a name from being spelled twice. The order is the order
// the lines appear in the file, and is the one thing here worth leaving
// alone: it is what a sequence written by an older build looks like.
template <typename Self, typename Visitor>
void visitFields(Self &self, Visitor &&visit) {
  visit("sessions", self.sessions);
  visit("seconds", self.seconds);
  visit("cpu seconds", self.cpuSeconds);
  visit("sampler", self.sampler);
  visit("sample offset", self.sampleOffset);
  visit("wavelength jitter", self.wavelengthJitter);
  visit("args", self.args);
}

} // namespace

std::vector<std::string> RenderHeader::headerLines() const {
  auto lines{std::vector<std::string>()};
  visitFields(*this, [&](const char *name, const auto &value) {
    auto line{smdl::concat(PREFIX, name, " = ")};
    spell(line, value);
    lines.push_back(std::move(line));
  });
  return lines;
}

void RenderHeader::readFrom(const std::map<std::string, std::string> &fields) {
  visitFields(*this, [&](const char *name, auto &value) {
    if (auto itr{fields.find(smdl::concat(PREFIX, name))}; itr != fields.end())
      parse(itr->second, value);
  });
}
