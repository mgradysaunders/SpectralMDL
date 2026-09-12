#include "Fixtures.h"

#include <map>
#include <string>

#include "IO/RenderHeader.h"

namespace {

// What `smdl::SpectralFilm::readENVIFile()` hands back: the header's
// `key = value` lines as a map, keyed by the lower-cased key. Writing
// the same shape here is what makes this a round trip rather than two
// halves tested apart.
[[nodiscard]] std::map<std::string, std::string>
asFields(const std::vector<std::string> &lines) {
  std::map<std::string, std::string> fields{};
  for (const auto &line : lines) {
    const size_t eq{line.find(" = ")};
    REQUIRE(eq != std::string::npos);
    fields[line.substr(0, eq)] = line.substr(eq + 3);
  }
  return fields;
}

[[nodiscard]] RenderHeader makeHeader() {
  RenderHeader header{};
  header.sessions = 7;
  header.seconds = 1234.5;
  header.cpuSeconds = 9876.25;
  header.sampler = "owen-sobol-1";
  header.sampleOffset = 4096;
  header.hasWavelengthJitter = true;
  header.args = "scene.layout -spp 64 -resume out.envi";
  header.quantity = "irradiance";
  return header;
}

} // namespace

TEST_CASE("RenderHeader: round trip") {
  const RenderHeader written{makeHeader()};
  const std::map<std::string, std::string> fields{
      asFields(written.headerLines())};
  SUBCASE("Every field survives the header") {
    RenderHeader read{};
    read.readFrom(fields);
    CHECK(read.sessions == written.sessions);
    CHECK(read.seconds == doctest::Approx(written.seconds));
    CHECK(read.cpuSeconds == doctest::Approx(written.cpuSeconds));
    CHECK(read.sampler == written.sampler);
    CHECK(read.sampleOffset == written.sampleOffset);
    CHECK(read.hasWavelengthJitter == written.hasWavelengthJitter);
    CHECK(read.args == written.args);
    CHECK(read.quantity == written.quantity);
  }
  SUBCASE("Every field is written, under the 'render' prefix") {
    // The count is the guard against a field being added to the struct
    // and left out of the table, which is the drift this type exists to
    // prevent; bump it when a field is genuinely added.
    CHECK(fields.size() == 8);
    for (const auto &field : fields) {
      CAPTURE(field.first);
      CHECK(field.first.rfind("render ", 0) == 0);
    }
  }
  SUBCASE("A field the file does not carry leaves the value alone") {
    // This is what makes a sequence written by an older build resumable
    // rather than an error.
    RenderHeader read{makeHeader()};
    read.readFrom({});
    CHECK(read.sessions == 7);
    CHECK(read.sampler == "owen-sobol-1");
    CHECK(read.args == written.args);
  }
  SUBCASE("A tally that is not a duration starts over") {
    // A corrupt or hand-edited header must not poison every later
    // session's total.
    for (const char *text : {"-1", "not a number", "nan", "inf"}) {
      CAPTURE(text);
      RenderHeader read{makeHeader()};
      read.readFrom({{"render seconds", text}});
      CHECK(read.seconds == 0.0);
    }
  }
  SUBCASE("The jitter flag is the file's '0' or '1'") {
    RenderHeader read{};
    read.readFrom({{"render wavelength jitter", "0"}});
    CHECK(!read.hasWavelengthJitter);
    read.readFrom({{"render wavelength jitter", "1"}});
    CHECK(read.hasWavelengthJitter);
  }
  SUBCASE("An empty header writes lines a reader takes as defaults") {
    RenderHeader read{makeHeader()};
    read.readFrom(asFields(RenderHeader().headerLines()));
    CHECK(read.sessions == 0);
    CHECK(read.seconds == 0.0);
    CHECK(read.sampler.empty());
    CHECK(read.sampleOffset == 0);
    CHECK(!read.hasWavelengthJitter);
    CHECK(read.args.empty());
    // A file written before the film could hold anything but radiance
    // says nothing, which a resume reads as radiance.
    CHECK(read.quantity.empty());
  }
}

TEST_CASE("DetectorHeader: round trip") {
  DetectorHeader written{};
  written.seed = 7;
  written.noise = "shot";
  written.exposure = 0.001;
  written.pixelWidth = 4;
  written.pixelHeight = 4.5;
  written.fNumber = 8;
  written.fullWell = 16000;
  written.readNoise = 1.5;
  written.darkElectrons = 1e-7;
  written.gain = 0.00341;
  written.blackLevel = 64;
  written.bits = 14;
  written.electronsPerFilmUnit = 1.96349541e-16;
  written.iso = 640;
  written.baseISO = 100;
  written.wasISOMetered = true;
  written.whiteLevel = 16383;
  const std::map<std::string, std::string> fields{
      asFields(written.headerLines())};
  SUBCASE("Every field survives the header at its digits") {
    CHECK(fields.at("render detector gain") == "0.00341");
    CHECK(fields.at("render detector dark electrons") == "1e-07");
    CHECK(fields.at("render detector noise") == "shot");
    DetectorHeader read{};
    read.readFrom(fields);
    CHECK(read.seed == 7);
    CHECK(read.noise == "shot");
    CHECK(read.exposure == doctest::Approx(0.001));
    CHECK(read.pixelWidth == 4);
    CHECK(read.pixelHeight == 4.5);
    CHECK(read.fNumber == 8);
    CHECK(read.fullWell == 16000);
    CHECK(read.readNoise == 1.5);
    CHECK(read.darkElectrons == doctest::Approx(1e-7));
    CHECK(read.gain == doctest::Approx(0.00341));
    CHECK(read.blackLevel == 64);
    CHECK(read.bits == 14);
    CHECK(read.electronsPerFilmUnit == doctest::Approx(1.96349541e-16));
    CHECK(read.iso == 640);
    CHECK(read.baseISO == 100);
    CHECK(read.wasISOMetered);
    CHECK(read.whiteLevel == 16383);
  }
  SUBCASE("Every field is written, under the 'render detector' prefix") {
    CHECK(fields.size() == 17);
    for (const auto &field : fields) {
      CAPTURE(field.first);
      CHECK(field.first.rfind("render detector ", 0) == 0);
    }
  }
}

TEST_CASE("ResponseHeader: round trip") {
  ResponseHeader written{};
  written.hash = "0123456789abcdef0123456789abcdef";
  written.cfaColumns = 2;
  written.cfa = {"R", "G", "G", "B"};
  const std::map<std::string, std::string> fields{
      asFields(written.headerLines())};
  SUBCASE("Every field survives the header, the tile as a list") {
    CHECK(fields.at("render cfa") == "{R, G, G, B}");
    ResponseHeader read{};
    read.readFrom(fields);
    CHECK(read.hash == written.hash);
    CHECK(read.cfaColumns == 2);
    CHECK(read.cfa == written.cfa);
  }
  SUBCASE("Every field is written, under the 'render' prefix") {
    CHECK(fields.size() == 3);
    for (const auto &field : fields) {
      CAPTURE(field.first);
      CHECK(field.first.rfind("render ", 0) == 0);
    }
  }
  SUBCASE("No tile spells an empty list and reads back as none") {
    ResponseHeader plain{};
    const std::map<std::string, std::string> plainFields{
        asFields(plain.headerLines())};
    CHECK(plainFields.at("render cfa") == "{}");
    CHECK(plainFields.at("render cfa columns") == "0");
    ResponseHeader read{written};
    read.readFrom(plainFields);
    CHECK(read.cfa.empty());
    CHECK(read.cfaColumns == 0);
  }
  SUBCASE("A field the file does not carry leaves the value alone") {
    ResponseHeader read{written};
    read.readFrom({});
    CHECK(read.hash == written.hash);
    CHECK(read.cfa.size() == 4);
  }
}
