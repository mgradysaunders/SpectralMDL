#include "doctest.h"

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
  auto fields{std::map<std::string, std::string>()};
  for (const auto &line : lines) {
    const auto eq{line.find(" = ")};
    REQUIRE(eq != std::string::npos);
    fields[line.substr(0, eq)] = line.substr(eq + 3);
  }
  return fields;
}

[[nodiscard]] RenderHeader makeHeader() {
  auto header{RenderHeader()};
  header.sessions = 7;
  header.seconds = 1234.5;
  header.cpuSeconds = 9876.25;
  header.sampler = "owen-sobol-1";
  header.sampleOffset = 4096;
  header.wavelengthJitter = true;
  header.args = "scene.layout -spp 64 -resume out.envi";
  return header;
}

} // namespace

TEST_CASE("RenderHeader: round trip") {
  const auto written{makeHeader()};
  const auto fields{asFields(written.headerLines())};
  SUBCASE("Every field survives the header") {
    auto read{RenderHeader()};
    read.readFrom(fields);
    CHECK(read.sessions == written.sessions);
    CHECK(read.seconds == doctest::Approx(written.seconds));
    CHECK(read.cpuSeconds == doctest::Approx(written.cpuSeconds));
    CHECK(read.sampler == written.sampler);
    CHECK(read.sampleOffset == written.sampleOffset);
    CHECK(read.wavelengthJitter == written.wavelengthJitter);
    CHECK(read.args == written.args);
  }
  SUBCASE("Every field is written, under the 'render' prefix") {
    // The count is the guard against a field being added to the struct
    // and left out of the table, which is the drift this type exists to
    // prevent; bump it when a field is genuinely added.
    CHECK(fields.size() == 7);
    for (const auto &[name, value] : fields) {
      CAPTURE(name);
      CHECK(name.rfind("render ", 0) == 0);
    }
  }
  SUBCASE("A field the file does not carry leaves the value alone") {
    // This is what makes a sequence written by an older build resumable
    // rather than an error.
    auto read{makeHeader()};
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
      auto read{makeHeader()};
      read.readFrom({{"render seconds", text}});
      CHECK(read.seconds == 0.0);
    }
  }
  SUBCASE("The jitter flag is the file's '0' or '1'") {
    auto read{RenderHeader()};
    read.readFrom({{"render wavelength jitter", "0"}});
    CHECK(!read.wavelengthJitter);
    read.readFrom({{"render wavelength jitter", "1"}});
    CHECK(read.wavelengthJitter);
  }
  SUBCASE("An empty header writes lines a reader takes as defaults") {
    auto read{makeHeader()};
    read.readFrom(asFields(RenderHeader().headerLines()));
    CHECK(read.sessions == 0);
    CHECK(read.seconds == 0.0);
    CHECK(read.sampler.empty());
    CHECK(read.sampleOffset == 0);
    CHECK(!read.wavelengthJitter);
    CHECK(read.args.empty());
  }
}
