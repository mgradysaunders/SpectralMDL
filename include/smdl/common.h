#pragma once

#include "smdl/Export.h"

#include <cstddef>
#include <cstdint>
#include <filesystem>
#include <format>
#include <functional>
#include <map>
#include <memory>
#include <optional>
#include <source_location>
#include <string>
#include <string_view>
#include <vector>

namespace llvm {

class TargetMachine;

} // namespace llvm

namespace smdl {

SMDL_EXPORT void init_or_exit();

class SMDL_EXPORT Error final {
public:
  Error(std::string message, std::string file = {}, uint32_t line = {})
      : message(std::move(message)), file(std::move(file)), line(line) {}

  void print() const;

public:
  std::string message{};

  std::string file{};

  uint32_t line{};
};

} // namespace smdl
