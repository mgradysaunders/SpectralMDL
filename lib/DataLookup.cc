#include "DataLookup.h"

namespace smdl {

extern "C" {

SMDL_EXPORT int_t smdl_data_isvalid(const DataLookup &data, const string_t &name) { return data.isvalid(name); }

SMDL_EXPORT int_t smdl_data_lookup_int(const DataLookup &data, const string_t &name, int_t &value) {
  if (const auto ptr{data.lookup<int_t>(name)}) {
    value = *ptr;
    return true;
  }
  return false;
}

SMDL_EXPORT int_t smdl_data_lookup_int2(const DataLookup &data, const string_t &name, int2_t &value) {
  if (const auto ptr{data.lookup<int2_t>(name)}) {
    value = *ptr;
    return true;
  }
  return false;
}

SMDL_EXPORT int_t smdl_data_lookup_int3(const DataLookup &data, const string_t &name, int3_t &value) {
  if (const auto ptr{data.lookup<int3_t>(name)}) {
    value = *ptr;
    return true;
  }
  return false;
}

SMDL_EXPORT int_t smdl_data_lookup_int4(const DataLookup &data, const string_t &name, int4_t &value) {
  if (const auto ptr{data.lookup<int4_t>(name)}) {
    value = *ptr;
    return true;
  }
  return false;
}

SMDL_EXPORT int_t smdl_data_lookup_float(const DataLookup &data, const string_t &name, float_t &value) {
  if (const auto ptr{data.lookup<float_t>(name)}) {
    value = *ptr;
    return true;
  }
  return false;
}

SMDL_EXPORT int_t smdl_data_lookup_float2(const DataLookup &data, const string_t &name, float2_t &value) {
  if (const auto ptr{data.lookup<float2_t>(name)}) {
    value = *ptr;
    return true;
  }
  return false;
}

SMDL_EXPORT int_t smdl_data_lookup_float3(const DataLookup &data, const string_t &name, float3_t &value) {
  if (const auto ptr{data.lookup<float3_t>(name)}) {
    value = *ptr;
    return true;
  }
  return false;
}

SMDL_EXPORT int_t smdl_data_lookup_float4(const DataLookup &data, const string_t &name, float4_t &value) {
  if (const auto ptr{data.lookup<float4_t>(name)}) {
    value = *ptr;
    return true;
  }
  return false;
}

SMDL_EXPORT int_t smdl_data_lookup_color(const DataLookup &data, const string_t &name, float_t *value) {
  if (const auto ptr{data.lookup<DataLookup::color_t>(name)}) {
    std::copy(ptr->begin(), ptr->end(), value);
    return true;
  }
  return false;
}

} // extern "C"

} // namespace smdl
