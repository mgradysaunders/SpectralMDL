#include <cmath>
#include <cstdlib>
#include <utility>

#include "smdl/Support/Strings.h"

#include "IO/RenderHeader.h"

namespace {

// What makes these the renderer's fields rather than the format's.
constexpr const char *PREFIX{"render "};

// How each field type crosses the header, written and read side by side
// because the two have to agree.
void spell(std::string &line, uint64_t value) { line += std::to_string(value); }
// Nine significant digits, every one a double carries that a reader
// recovering electrons from a gain could want, and no trailing zeros.
void spell(std::string &line, double value) {
  smdl::Brief(value, 9).appendTo(line);
}
void spell(std::string &line, bool value) { line += value ? '1' : '0'; }
void spell(std::string &line, const std::string &value) { line += value; }

// The format's `{a, b, c}` array form, for a list of names.
void spell(std::string &line, const std::vector<std::string> &values) {
  line += '{';
  for (size_t i = 0; i < values.size(); i++)
    line += (i > 0 ? ", " : "") + values[i];
  line += '}';
}

void parse(const std::string &text, uint64_t &value) {
  value = std::strtoull(text.c_str(), nullptr, 10);
}

void parse(const std::string &text, double &value) {
  // A tally is a duration and the rest are physical quantities, none of
  // them negative, so anything a corrupt or hand-edited header offers
  // that is not one reads as zero rather than poisoning every later
  // session's total.
  const double parsed{std::strtod(text.c_str(), nullptr)};
  value = std::isfinite(parsed) && parsed > 0.0 ? parsed : 0.0;
}

void parse(const std::string &text, bool &value) { value = text != "0"; }

void parse(const std::string &text, std::string &value) { value = text; }

void parse(const std::string &text, std::vector<std::string> &values) {
  values.clear();
  auto list{text};
  for (auto &c : list)
    if (c == '{' || c == '}') c = ' ';
  for (size_t pos{}; pos <= list.size();) {
    auto end{list.find(',', pos)};
    if (end == std::string::npos) end = list.size();
    auto name{list.substr(pos, end - pos)};
    const char *WS{" \t\r\n"};
    name.erase(0, name.find_first_not_of(WS));
    name.erase(name.find_last_not_of(WS) + 1);
    if (!name.empty()) values.push_back(std::move(name));
    pos = end + 1;
  }
}

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
  visit("wavelength jitter", self.hasWavelengthJitter);
  visit("args", self.args);
}

// The band film's table, under the same prefix.
template <typename Self, typename Visitor>
void visitResponseFields(Self &self, Visitor &&visit) {
  visit("response kind", self.kind);
  visit("response hash", self.hash);
  visit("cfa columns", self.cfaColumns);
  visit("cfa", self.cfa);
}

// The readout's table, under the same prefix.
template <typename Self, typename Visitor>
void visitDetectorFields(Self &self, Visitor &&visit) {
  visit("detector seed", self.seed);
  visit("detector noise", self.noise);
  visit("detector exposure", self.exposure);
  visit("detector pixel width", self.pixelWidth);
  visit("detector pixel height", self.pixelHeight);
  visit("detector f number", self.fNumber);
  visit("detector full well", self.fullWell);
  visit("detector read noise", self.readNoise);
  visit("detector dark electrons", self.darkElectrons);
  visit("detector gain", self.gain);
  visit("detector black level", self.blackLevel);
  visit("detector bits", self.bits);
  visit("detector electrons per film unit", self.electronsPerFilmUnit);
  visit("detector noise limited share", self.noiseLimitedShare);
}

// The two directions over either table.
template <typename Self, typename Walk>
[[nodiscard]] std::vector<std::string> linesOf(const Self &self, Walk &&walk) {
  auto lines{std::vector<std::string>()};
  walk(self, [&](const char *name, const auto &value) {
    auto line{smdl::concat(PREFIX, name, " = ")};
    spell(line, value);
    lines.push_back(std::move(line));
  });
  return lines;
}

template <typename Self, typename Walk>
void readInto(Self &self, const std::map<std::string, std::string> &fields,
              Walk &&walk) {
  walk(self, [&](const char *name, auto &value) {
    if (auto itr{fields.find(smdl::concat(PREFIX, name))}; itr != fields.end())
      parse(itr->second, value);
  });
}

} // namespace

std::vector<std::string> RenderHeader::headerLines() const {
  return linesOf(*this,
                 [](auto &self, auto &&visit) { visitFields(self, visit); });
}

void RenderHeader::readFrom(const std::map<std::string, std::string> &fields) {
  readInto(*this, fields,
           [](auto &self, auto &&visit) { visitFields(self, visit); });
}

std::vector<std::string> ResponseHeader::headerLines() const {
  return linesOf(*this, [](auto &self, auto &&visit) {
    visitResponseFields(self, visit);
  });
}

void ResponseHeader::readFrom(
    const std::map<std::string, std::string> &fields) {
  readInto(*this, fields,
           [](auto &self, auto &&visit) { visitResponseFields(self, visit); });
}

std::vector<std::string> DetectorHeader::headerLines() const {
  return linesOf(*this, [](auto &self, auto &&visit) {
    visitDetectorFields(self, visit);
  });
}

void DetectorHeader::readFrom(
    const std::map<std::string, std::string> &fields) {
  readInto(*this, fields,
           [](auto &self, auto &&visit) { visitDetectorFields(self, visit); });
}
