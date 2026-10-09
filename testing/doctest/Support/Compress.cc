#include "Fixtures.h"

#include <string>
#include <vector>

#include "smdl/Support/Compress.h"
#include "smdl/Support/Error.h"

namespace {

// Text with real redundancy, so that the round trip runs through the
// match finder rather than through the stored-block fallback that any
// short random string would take.
[[nodiscard]] std::vector<std::byte> makeBytes(size_t count) {
  std::vector<std::byte> bytes{};
  bytes.reserve(count);
  for (size_t i = 0; i < count; i++)
    bytes.push_back(std::byte("the quick brown fox "[i % 20]));
  return bytes;
}

[[nodiscard]] std::vector<std::byte> roundTrip(const std::vector<std::byte> &in,
                                               int level = 6) {
  const std::vector<std::byte> deflated{smdl::compressBytes(in, level)};
  std::vector<std::byte> out(in.size());
  smdl::decompressBytesInto(out, deflated);
  return out;
}

} // namespace

TEST_CASE("Compress: round trip") {
  SUBCASE("Bytes survive at every level") {
    const std::vector<std::byte> bytes{makeBytes(4096)};
    for (int level = 0; level <= 9; level++) {
      CAPTURE(level);
      CHECK(roundTrip(bytes, level) == bytes);
    }
  }
  SUBCASE("Redundant input gets smaller") {
    const std::vector<std::byte> bytes{makeBytes(4096)};
    CHECK(smdl::compressBytes(bytes).size() < bytes.size());
  }
  SUBCASE("An empty input survives") {
    const std::vector<std::byte> bytes{};
    CHECK(roundTrip(bytes).empty());
  }
  SUBCASE("A level out of range is refused") {
    CHECK_THROWS_AS((void)smdl::compressBytes(makeBytes(16), 10), smdl::Error);
    CHECK_THROWS_AS((void)smdl::compressBytes(makeBytes(16), -1), smdl::Error);
  }
  SUBCASE("A destination of the wrong size is refused") {
    const std::vector<std::byte> deflated{smdl::compressBytes(makeBytes(256))};
    std::vector<std::byte> tooSmall(128);
    std::vector<std::byte> tooLarge(512);
    CHECK_THROWS_AS(smdl::decompressBytesInto(tooSmall, deflated), smdl::Error);
    CHECK_THROWS_AS(smdl::decompressBytesInto(tooLarge, deflated), smdl::Error);
  }
  SUBCASE("A corrupt stream is refused") {
    std::vector<std::byte> deflated{smdl::compressBytes(makeBytes(256))};
    REQUIRE(deflated.size() > 8);
    for (size_t i = 4; i < deflated.size(); i++)
      deflated[i] = std::byte(0xA5);
    std::vector<std::byte> out(256);
    CHECK_THROWS_AS(smdl::decompressBytesInto(out, deflated), smdl::Error);
  }
  SUBCASE("An empty stream is refused") {
    std::vector<std::byte> out(16);
    CHECK_THROWS_AS(smdl::decompressBytesInto(out, {}), smdl::Error);
  }
}
