#include "DataLookup.h"

extern "C" {

SMDL_EXPORT smdl::int_t smdl_data_isvalid(const smdl::DataLookup &data, const smdl::string_t &name) {
  return data.isvalid(name);
}

SMDL_EXPORT smdl::int_t smdl_data_lookup_int(const smdl::DataLookup &data, const smdl::string_t &name, smdl::int_t &value) {
  if (const auto ptr{data.lookup<smdl::int_t>(name)}) {
    value = *ptr;
    return true;
  }
  return false;
}

SMDL_EXPORT smdl::int_t smdl_data_lookup_int2(const smdl::DataLookup &data, const smdl::string_t &name, smdl::int2_t &value) {
  if (const auto ptr{data.lookup<smdl::int2_t>(name)}) {
    value = *ptr;
    return true;
  }
  return false;
}

SMDL_EXPORT smdl::int_t smdl_data_lookup_int3(const smdl::DataLookup &data, const smdl::string_t &name, smdl::int3_t &value) {
  if (const auto ptr{data.lookup<smdl::int3_t>(name)}) {
    value = *ptr;
    return true;
  }
  return false;
}

SMDL_EXPORT smdl::int_t smdl_data_lookup_int4(const smdl::DataLookup &data, const smdl::string_t &name, smdl::int4_t &value) {
  if (const auto ptr{data.lookup<smdl::int4_t>(name)}) {
    value = *ptr;
    return true;
  }
  return false;
}

SMDL_EXPORT smdl::int_t smdl_data_lookup_float(const smdl::DataLookup &data, const smdl::string_t &name, smdl::float_t &value) {
  if (const auto ptr{data.lookup<smdl::float_t>(name)}) {
    value = *ptr;
    return true;
  }
  return false;
}

SMDL_EXPORT smdl::int_t smdl_data_lookup_float2(
    const smdl::DataLookup &data, const smdl::string_t &name, smdl::float2_t &value) {
  if (const auto ptr{data.lookup<smdl::float2_t>(name)}) {
    value = *ptr;
    return true;
  }
  return false;
}

SMDL_EXPORT smdl::int_t smdl_data_lookup_float3(
    const smdl::DataLookup &data, const smdl::string_t &name, smdl::float3_t &value) {
  if (const auto ptr{data.lookup<smdl::float3_t>(name)}) {
    value = *ptr;
    return true;
  }
  return false;
}

SMDL_EXPORT smdl::int_t smdl_data_lookup_float4(
    const smdl::DataLookup &data, const smdl::string_t &name, smdl::float4_t &value) {
  if (const auto ptr{data.lookup<smdl::float4_t>(name)}) {
    value = *ptr;
    return true;
  }
  return false;
}

SMDL_EXPORT smdl::int_t smdl_data_lookup_color(const smdl::DataLookup &data, const smdl::string_t &name, smdl::float_t *value) {
  if (const auto ptr{data.lookup<smdl::DataLookup::color_t>(name)}) {
    std::copy(ptr->begin(), ptr->end(), value);
    return true;
  }
  return false;
}

} // extern "C"
