#include "doctest.h"

#include "smdl/Support/MD5Hash.h"

TEST_CASE("MD5Hash") {
  CHECK(std::string(smdl::MD5Hash::hash_memory("Hello, world!")) ==
        "6cd3556deb0da54bca060b4c39479839");
}
