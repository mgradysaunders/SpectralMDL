#include "smdl/Support/Error.h"

#include "smdl/Support/Logger.h"

#if __GNUC__ || __clang__
#if __has_include(<cxxabi.h>)
#define SMDL_HAS_CXXABI 1
#include <cxxabi.h>
#endif // #if __has_include(<cxxabi.h>)
#endif // #if __GNUC__ || __clang__

namespace smdl {

void Error::print() const { SMDL_LOG_ERROR(message); }

void Error::print_and_exit() const {
  print();
  std::exit(EXIT_FAILURE);
}

std::string abi_demangle(const char *name) {
  if (!name)
    return {};
#if SMDL_HAS_CXXABI
  int status{};
  char *cstr{::abi::__cxa_demangle(name, 0, 0, &status)};
  if (!cstr)
    return name;
  std::string str{cstr};
  std::free(cstr);
  return str;
#else
  return name;
#endif // #if SMDL_HAS_CXXABI
}

std::string abi_demangle_exception_name() {
#if SMDL_HAS_CXXABI
  return abi_demangle(::abi::__cxa_current_exception_type()->name());
#else
  return "unknown exception";
#endif // #if SMDL_HAS_CXXABI
}

} // namespace smdl
