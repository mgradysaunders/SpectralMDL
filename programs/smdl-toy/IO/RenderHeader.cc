#include <cmath>
#include <cstdlib>
#include <utility>

#include "smdl/Support/Macros.h"
#include "smdl/Support/Strings.h"

#include "IO/RenderHeader.h"

namespace {

// What makes these the renderer's fields rather than the format's.
constexpr const char *PREFIX{"render "};

// How each field type crosses the header, written and read side by side
// because the two have to agree.
SMDL_ALWAYS_INLINE void spell(std::string &line, uint64_t value) {
  line += std::to_string(value);
}
SMDL_ALWAYS_INLINE void spell(std::string &line, double value) {
  smdl::Brief(value, 9).appendTo(line);
}
SMDL_ALWAYS_INLINE void spell(std::string &line, bool value) {
  line += value ? '1' : '0';
}
SMDL_ALWAYS_INLINE void spell(std::string &line, const std::string &value) {
  line += value;
}
SMDL_ALWAYS_INLINE void spell(std::string &line,
                              const std::vector<std::string> &values) {
  line += '{';
  for (size_t i = 0; i < values.size(); i++)
    line += (i > 0 ? ", " : "") + values[i];
  line += '}';
}
void spell(std::string &line, const std::vector<double> &values) {
  line += '{';
  for (size_t i = 0; i < values.size(); i++) {
    if (i > 0) line += ", ";
    spell(line, values[i]);
  }
  line += '}';
}
void spell(std::string &line, const std::vector<float> &values) {
  spell(line, std::vector<double>(values.begin(), values.end()));
}

SMDL_ALWAYS_INLINE void parse(const std::string &text, uint64_t &value) {
  value = std::strtoull(text.c_str(), nullptr, 10);
}
SMDL_ALWAYS_INLINE void parse(const std::string &text, double &value) {
  // A tally is a duration and the rest are physical quantities, none of
  // them negative, so anything a corrupt or hand-edited header offers
  // that is not one reads as zero rather than poisoning every later
  // session's total.
  const double parsed{std::strtod(text.c_str(), nullptr)};
  value = std::isfinite(parsed) && parsed > 0.0 ? parsed : 0.0;
}
SMDL_ALWAYS_INLINE void parse(const std::string &text, bool &value) {
  value = text != "0";
}
SMDL_ALWAYS_INLINE void parse(const std::string &text, std::string &value) {
  value = text;
}
void parse(const std::string &text, std::vector<std::string> &values) {
  values.clear();
  std::string list{text};
  for (auto &c : list)
    if (c == '{' || c == '}') c = ' ';
  for (size_t pos{}; pos <= list.size();) {
    size_t end{list.find(',', pos)};
    if (end == std::string::npos) end = list.size();
    std::string name{list.substr(pos, end - pos)};
    const char *WS{" \t\r\n"};
    name.erase(0, name.find_first_not_of(WS));
    name.erase(name.find_last_not_of(WS) + 1);
    if (!name.empty()) values.push_back(std::move(name));
    pos = end + 1;
  }
}
void parse(const std::string &text, std::vector<double> &values) {
  // A list with an entry that is not a number is no list at all, so the
  // field reads as absent rather than as a grid with a hole in it.
  std::vector<std::string> items{};
  parse(text, items);
  values.clear();
  for (const auto &item : items) {
    char *end{};
    const double value{std::strtod(item.c_str(), &end)};
    if (end == item.c_str() || *end != '\0' || !std::isfinite(value)) {
      values.clear();
      return;
    }
    values.push_back(value);
  }
}
void parse(const std::string &text, std::vector<float> &values) {
  std::vector<double> parsed{};
  parse(text, parsed);
  values.assign(parsed.begin(), parsed.end());
}

// The grid fields, each name spelled here alone: the one grid's edges
// and, when the format's list does not carry them, its wavelengths; the
// names of a tile's grids, and each grid's two lists by its index in
// that list. By index rather than by name because a reader folds a
// field's key to lower case, and a band's name is the sensor's.
[[nodiscard]] std::string fieldBandEdges() {
  return smdl::concat(PREFIX, "band edges");
}
[[nodiscard]] std::string fieldGridWavelengths() {
  return smdl::concat(PREFIX, "grid wavelengths");
}
[[nodiscard]] std::string fieldGrids() { return smdl::concat(PREFIX, "grids"); }
[[nodiscard]] std::string fieldGridWavelengths(size_t index) {
  return smdl::concat(PREFIX, "grid ", index, " wavelengths");
}
[[nodiscard]] std::string fieldGridBandEdges(size_t index) {
  return smdl::concat(PREFIX, "grid ", index, " band edges");
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
  visit("quantity", self.quantity);
  visit("metered exposure", self.meteredLuxSeconds);
  visit("metered iso", self.meteredISO);
}

// The band film's table, under the same prefix.
template <typename Self, typename Visitor>
void visitResponseFields(Self &self, Visitor &&visit) {
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
  visit("detector iso", self.iso);
  visit("detector base iso", self.baseISO);
  visit("detector iso metered", self.wasISOMetered);
  visit("detector white level", self.whiteLevel);
}

// The two directions over either table.
template <typename Self, typename Walk>
[[nodiscard]] std::vector<std::string> linesOf(const Self &self, Walk &&walk) {
  std::vector<std::string> lines{};
  walk(self, [&](const char *name, const auto &value) {
    std::string line{smdl::concat(PREFIX, name, " = ")};
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

std::vector<std::string> GridHeader::headerLines() const {
  std::vector<std::string> lines{};
  const auto push{[&](const std::string &name, const auto &value) {
    std::string line{smdl::concat(name, " = ")};
    spell(line, value);
    lines.push_back(std::move(line));
  }};
  if (grids.size() == 1 && grids.front().name.empty()) {
    push(fieldBandEdges(), grids.front().bandEdges);
    if (!grids.front().wavelengths.empty())
      push(fieldGridWavelengths(), grids.front().wavelengths);
    return lines;
  }
  std::vector<std::string> names{};
  for (const auto &grid : grids) names.push_back(grid.name);
  push(fieldGrids(), names);
  for (size_t k = 0; k < grids.size(); k++) {
    push(fieldGridWavelengths(k), grids[k].wavelengths);
    push(fieldGridBandEdges(k), grids[k].bandEdges);
  }
  return lines;
}

void GridHeader::readFrom(const std::map<std::string, std::string> &fields) {
  grids.clear();
  const auto read{[&](const std::string &name, auto &value) {
    if (auto itr{fields.find(name)}; itr != fields.end())
      parse(itr->second, value);
  }};
  std::vector<std::string> names{};
  read(fieldGrids(), names);
  if (!names.empty()) {
    for (size_t k = 0; k < names.size(); k++) {
      Grid &grid{grids.emplace_back()};
      grid.name = names[k];
      read(fieldGridWavelengths(k), grid.wavelengths);
      read(fieldGridBandEdges(k), grid.bandEdges);
    }
    return;
  }
  if (fields.count(fieldBandEdges()) > 0) {
    Grid &grid{grids.emplace_back()};
    read(fieldBandEdges(), grid.bandEdges);
    read(fieldGridWavelengths(), grid.wavelengths);
  }
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
