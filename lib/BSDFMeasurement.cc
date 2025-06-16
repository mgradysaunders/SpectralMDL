#include "smdl/BSDFMeasurement.h"

#include "filesystem.h"

#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Endian.h"
#include "llvm/Support/MemAlloc.h"

namespace smdl {

std::unique_ptr<BSDFMeasurement>
BSDFMeasurement::load_from_memory(const std::string &file) {
  auto result{std::make_unique<BSDFMeasurement>()};
  auto mem{llvm::StringRef(file)};
  if (!mem.consume_front("NVIDIA ARC MBSDF V1\n")) {
    throw Error("not an MBSDF file");
  }
  auto dataBlockOffset{[&]() -> size_t {
    result->kind = KIND_REFLECTION;
    if (auto i{mem.find("MBSDF_DATA=\n")}; i < mem.size())
      return i;
    if (auto i{mem.find("MBSDF_DATA_REFLECTION=\n")}; i < mem.size())
      return i;
    result->kind = KIND_TRANSMISSION;
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
      result->metaData[std::string(kv.first)] = unquote(kv.second);
    }
  }
  mem = mem.substr(dataBlockOffset);
  mem = mem.drop_until([](char ch) { return ch == '\n'; });
  mem = mem.drop_front(1);
  if (mem.size() < 12) {
    throw Error("invalid data block");
  }
  result->type = Type(llvm::support::endian::read32le(mem.data()));
  result->numTheta = llvm::support::endian::read32le(mem.data() + 4);
  result->numPhi = llvm::support::endian::read32le(mem.data() + 8);
  result->buffer =
      llvm::allocate_buffer(result->numTheta * result->numTheta *
                                result->numPhi * size_of(result->type),
                            16);
  mem = mem.substr(12);
  auto num{result->numTheta * result->numTheta * result->numPhi};
  auto srcPtr{reinterpret_cast<const uint32_t *>(mem.data())};
  auto dstPtr{static_cast<uint32_t *>(result->buffer)};
  switch (result->type) {
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
  return result;
}

std::unique_ptr<BSDFMeasurement>
BSDFMeasurement::load_from_file(const std::string &fileName) {
  auto result{std::unique_ptr<BSDFMeasurement>{}};
  auto file{fs_read(fileName)};
  if (auto error{
          catch_and_return_error([&] { result = load_from_memory(file); })}) {
    throw Error(concat("cannot load ", quoted(fileName), ": ", error->message));
  }
  return result;
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
