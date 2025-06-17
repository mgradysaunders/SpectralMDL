#include "smdl/BSDFMeasurement.h"

#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Endian.h"
#include "llvm/Support/MemAlloc.h"

namespace smdl {

std::optional<Error>
BSDFMeasurement::load_from_file_memory(const std::string &file) noexcept {
  clear();
  auto error{catch_and_return_error([&] {
    auto mem{llvm::StringRef(file)};
    if (!mem.consume_front("NVIDIA ARC MBSDF V1\n")) {
      throw Error("not an MBSDF file");
    }
    auto dataBlockOffset{[&]() -> size_t {
      kind = KIND_REFLECTION;
      if (auto i{mem.find("MBSDF_DATA=\n")}; i < mem.size())
        return i;
      if (auto i{mem.find("MBSDF_DATA_REFLECTION=\n")}; i < mem.size())
        return i;
      kind = KIND_TRANSMISSION;
      if (auto i{mem.find("MBSDF_DATA_TRANSMISSION=\n")}; i < mem.size())
        return i;
      throw Error("missing data block");
      return 0;
    }()};
    {
      auto unquote{[](llvm::StringRef quoted) -> std::string {
        if (!quoted.consume_front("\"")) {
          return std::string(quoted); // Error?
        } else {
          auto unquoted{std::string()};
          while (!quoted.empty()) {
            if (quoted.consume_front(R"(\n)")) {
              unquoted += '\n';
            } else if (quoted.consume_front(R"(\\)")) {
              unquoted += '\\';
            } else if (quoted.consume_front(R"(\")")) {
              unquoted += '"';
            } else {
              unquoted += quoted.front(), quoted = quoted.drop_front(1);
            }
          }
          return unquoted;
        }
      }};
      auto mdLines{llvm::SmallVector<llvm::StringRef>{}};
      mem.substr(0, dataBlockOffset)
          .split(mdLines, '\n', /*MaxSplit=*/-1, /*KeepEmpty=*/false);
      for (auto mdLine : mdLines) {
        auto kv{mdLine.split('=')};
        metaData[std::string(kv.first)] = unquote(kv.second);
      }
    }
    mem = mem.substr(dataBlockOffset);
    mem = mem.drop_until([](char ch) { return ch == '\n'; });
    mem = mem.drop_front(1);
    if (mem.size() < 12) {
      throw Error("invalid data block");
    }
    type = Type(llvm::support::endian::read32le(mem.data()));
    numTheta = llvm::support::endian::read32le(mem.data() + 4);
    numPhi = llvm::support::endian::read32le(mem.data() + 8);
    buffer =
        llvm::allocate_buffer(numTheta * numTheta * numPhi * size_of(type), 16);
    mem = mem.substr(12);
    auto num{numTheta * numTheta * numPhi};
    auto srcPtr{reinterpret_cast<const uint32_t *>(mem.data())};
    auto dstPtr{static_cast<uint32_t *>(buffer)};
    switch (type) {
    case TYPE_FLOAT:
      for (size_t i = 0; i < num; i++, srcPtr += 1, dstPtr += 1) {
        dstPtr[0] = llvm::support::endian::read32le(srcPtr);
      }
      break;
    case TYPE_FLOAT3:
      for (size_t i = 0; i < num; i++, srcPtr += 3, dstPtr += 4) {
        dstPtr[0] = llvm::support::endian::read32le(&srcPtr[0]);
        dstPtr[1] = llvm::support::endian::read32le(&srcPtr[1]);
        dstPtr[2] = llvm::support::endian::read32le(&srcPtr[2]);
        dstPtr[3] = 0;
        // TODO Decode sRGB?
      }
      break;
    default:
      throw Error("unknown type");
      break;
    }
  })};
  if (error) {
    clear();
  }
  return error;
}

std::optional<Error>
BSDFMeasurement::load_from_file(const std::string &fileName) noexcept {
  clear();
  auto file{std::string()};
  if (auto error{
          catch_and_return_error([&] { file = read_or_throw(fileName); })})
    return error;
  if (auto error{load_from_file_memory(file)})
    return Error(concat("cannot load ", quoted(relative(fileName)), ": ",
                        error->message));
  return std::nullopt;
}

void BSDFMeasurement::clear() noexcept {
  if (buffer)
    llvm::deallocate_buffer(buffer,
                            numTheta * numTheta * numPhi * size_of(type), 16);
  numTheta = 0;
  numPhi = 0;
  buffer = nullptr;
  metaData.clear();
}

} // namespace smdl
