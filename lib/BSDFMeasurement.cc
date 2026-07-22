#include "smdl/BSDFMeasurement.h"

#include <algorithm>
#include <cmath>

#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Endian.h"
#include "llvm/Support/MemAlloc.h"

namespace smdl {

std::optional<Error>
BSDFMeasurement::loadFromFileMemory(const std::string &file) noexcept {
  clear();
  auto error{catchAndReturnError([&] {
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
    return error;
  }
  // Build one sampling distribution per outgoing zenith angle cell, over the
  // incoming zenith angle rows and azimuth difference columns of the
  // corresponding table slice, weighted by the projected solid angle of the
  // incoming direction.
  distributions.reserve(numTheta);
  auto values{std::vector<float>(numTheta * numPhi)};
  for (size_t iO = 0; iO < numTheta; iO++) {
    for (size_t iI = 0; iI < numTheta; iI++) {
      const float thetai{0.5f * PI * (float(iI) + 0.5f) / float(numTheta)};
      const float weight{std::cos(thetai) * std::sin(thetai)};
      for (size_t iP = 0; iP < numPhi; iP++) {
        const auto value{fetch(int(iO), int(iI), int(iP))};
        values[numPhi * iI + iP] =
            weight * std::fmax((value.x + value.y + value.z) / 3.0f, 0.0f);
      }
    }
    distributions.emplace_back(int(numPhi), int(numTheta), values);
  }
  return std::nullopt;
}

float3 BSDFMeasurement::fetch(int iThetao, int iThetai,
                              int iPhi) const noexcept {
  if (!buffer || numTheta == 0 || numPhi == 0)
    return {};
  iThetao = std::clamp(iThetao, 0, int(numTheta) - 1);
  iThetai = std::clamp(iThetai, 0, int(numTheta) - 1);
  iPhi = std::clamp(iPhi, 0, int(numPhi) - 1);
  const size_t i{(size_t(iThetao) * numTheta + size_t(iThetai)) * numPhi +
                 size_t(iPhi)};
  if (type == TYPE_FLOAT) {
    const float value{static_cast<const float *>(buffer)[i]};
    return float3(value, value, value);
  } else {
    return static_cast<const float3 *>(buffer)[i];
  }
}

/// The lookup of a cell-centered linear interpolation, where `t` is the
/// continuous cell coordinate and `n` is the number of cells.
struct CellLookup final {
  int index0{};
  int index1{};
  float fraction{};
};

[[nodiscard]] static CellLookup cellLookup(float t, int n) noexcept {
  t = std::clamp(t - 0.5f, 0.0f, float(n - 1));
  const int i0{std::min(int(t), n - 1)};
  return {i0, std::min(i0 + 1, n - 1), t - float(i0)};
}

float3 BSDFMeasurement::interpolate(float thetao, float thetai,
                                    float phi) const noexcept {
  if (!buffer || numTheta == 0 || numPhi == 0)
    return {};
  const auto o{
      cellLookup(thetao * (2.0f / PI) * float(numTheta), int(numTheta))};
  const auto i{
      cellLookup(thetai * (2.0f / PI) * float(numTheta), int(numTheta))};
  const auto p{cellLookup(phi * (1.0f / PI) * float(numPhi), int(numPhi))};
  const auto lerp{[](const float3 &a, const float3 &b, float t) {
    return a + (b - a) * t;
  }};
  return lerp(lerp(lerp(fetch(o.index0, i.index0, p.index0),
                        fetch(o.index0, i.index0, p.index1), p.fraction),
                   lerp(fetch(o.index0, i.index1, p.index0),
                        fetch(o.index0, i.index1, p.index1), p.fraction),
                   i.fraction),
              lerp(lerp(fetch(o.index1, i.index0, p.index0),
                        fetch(o.index1, i.index0, p.index1), p.fraction),
                   lerp(fetch(o.index1, i.index1, p.index0),
                        fetch(o.index1, i.index1, p.index1), p.fraction),
                   i.fraction),
              o.fraction);
}

/// Calculate the azimuth difference angle in `[0, pi]`.
[[nodiscard]] static float azimuthDifference(const float3 &wo,
                                             const float3 &wi) noexcept {
  float phi{std::atan2(wi.y, wi.x) - std::atan2(wo.y, wo.x)};
  if (phi < -PI)
    phi += 2.0f * PI;
  if (phi > +PI)
    phi -= 2.0f * PI;
  return std::abs(phi);
}

float3 BSDFMeasurement::interpolate(float3 wo, float3 wi) const noexcept {
  wo = normalize(wo);
  wi = normalize(wi);
  if (!std::isfinite(wo.x + wo.y + wo.z + wi.x + wi.y + wi.z))
    return {};
  return interpolate(std::acos(std::clamp(std::abs(wo.z), 0.0f, 1.0f)),
                     std::acos(std::clamp(std::abs(wi.z), 0.0f, 1.0f)),
                     azimuthDifference(wo, wi));
}

float BSDFMeasurement::directionPDF(float3 wo, float3 wi) const noexcept {
  if (distributions.empty())
    return 0;
  wo = normalize(wo);
  wi = normalize(wi);
  if (!std::isfinite(wo.x + wo.y + wo.z + wi.x + wi.y + wi.z))
    return 0;
  const float cosThetai{std::clamp(std::abs(wi.z), 0.0f, 1.0f)};
  const float sinThetai{std::sqrt(std::max(1.0f - cosThetai * cosThetai, //
                                           0.0f))};
  if (!(sinThetai > 0))
    return 0;
  const int nTheta{int(numTheta)};
  const int nPhi{int(numPhi)};
  const float thetao{std::acos(std::clamp(std::abs(wo.z), 0.0f, 1.0f))};
  const float thetai{std::acos(cosThetai)};
  const int iThetao{
      std::clamp(int(thetao * (2.0f / PI) * float(nTheta)), 0, nTheta - 1)};
  const int iThetai{
      std::clamp(int(thetai * (2.0f / PI) * float(nTheta)), 0, nTheta - 1)};
  const int iPhi{std::clamp(
      int(azimuthDifference(wo, wi) * (1.0f / PI) * float(nPhi)), 0, nPhi - 1)};
  const float pmf{distributions[iThetao].pixelPMF(int2(iPhi, iThetai))};
  const float dTheta{0.5f * PI / float(nTheta)};
  const float dPhi{PI / float(nPhi)};
  // The extra factor of one half accounts for the mirror sign of the
  // azimuth difference.
  return pmf / (2.0f * dTheta * dPhi * sinThetai);
}

float3 BSDFMeasurement::directionSample(float2 xi, float3 wo,
                                        float *pdf) const noexcept {
  if (pdf)
    *pdf = 0;
  if (distributions.empty())
    return {};
  wo = normalize(wo);
  if (!std::isfinite(wo.x + wo.y + wo.z))
    return {};
  const int nTheta{int(numTheta)};
  const int nPhi{int(numPhi)};
  const float thetao{std::acos(std::clamp(std::abs(wo.z), 0.0f, 1.0f))};
  const int iThetao{
      std::clamp(int(thetao * (2.0f / PI) * float(nTheta)), 0, nTheta - 1)};
  float pmf{};
  const auto i{distributions[iThetao].pixelSample(xi, &xi, &pmf)};
  if (!(pmf > 0))
    return {};
  // Split the remapped azimuth sample into a mirror sign and a within-cell
  // offset, because the table only spans azimuth differences in `[0, pi]`.
  float phiSign{+1.0f};
  if (xi.x < 0.5f) {
    xi.x = 2.0f * xi.x;
  } else {
    xi.x = 2.0f * xi.x - 1.0f;
    phiSign = -1.0f;
  }
  const float dTheta{0.5f * PI / float(nTheta)};
  const float dPhi{PI / float(nPhi)};
  const float thetai{(float(i.y) + xi.y) * dTheta};
  const float sinThetai{std::sin(thetai)};
  const float cosThetai{std::cos(thetai)};
  if (!(sinThetai > 0))
    return {};
  const float phi{std::atan2(wo.y, wo.x) +
                  phiSign * (float(i.x) + xi.x) * dPhi};
  if (pdf)
    *pdf = pmf / (2.0f * dTheta * dPhi * sinThetai);
  return float3(sinThetai * std::cos(phi), sinThetai * std::sin(phi),
                cosThetai);
}

std::optional<Error>
BSDFMeasurement::loadFromFile(const std::string &fileName) noexcept {
  clear();
  auto file{std::string()};
  if (auto error{catchAndReturnError([&] { file = readOrThrow(fileName); })})
    return error;
  if (auto error{loadFromFileMemory(file)})
    return Error(
        concat("cannot load ", QuotedPath(fileName), ": ", error->message));
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
  distributions.clear();
}

} // namespace smdl

extern "C" {

SMDL_EXPORT void smdBSDFMeasurementInterpolate(const void *measurement,
                                               const smdl::float3 &wo,
                                               const smdl::float3 &wi,
                                               smdl::float3 *result) {
  if (result)
    *result = measurement
                  ? static_cast<const smdl::BSDFMeasurement *>(measurement)
                        ->interpolate(wo, wi)
                  : smdl::float3();
}

SMDL_EXPORT float smdBSDFMeasurementDirectionPDF(const void *measurement,
                                                 const smdl::float3 &wo,
                                                 const smdl::float3 &wi) {
  return measurement ? static_cast<const smdl::BSDFMeasurement *>(measurement)
                           ->directionPDF(wo, wi)
                     : 0.0f;
}

SMDL_EXPORT void smdBSDFMeasurementDirectionSample(const void *measurement,
                                                   const smdl::float2 &xi,
                                                   const smdl::float3 &wo,
                                                   smdl::float3 *wi,
                                                   float *pdf) {
  if (!measurement) {
    if (wi)
      *wi = {};
    if (pdf)
      *pdf = 0.0f;
    return;
  }
  auto w{static_cast<const smdl::BSDFMeasurement *>(measurement)
             ->directionSample(xi, wo, pdf)};
  if (wi)
    *wi = w;
}

} // extern "C"
