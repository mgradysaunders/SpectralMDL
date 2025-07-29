#include "smdl/Support/StringHelpers.h"
#include "smdl/Support/Filesystem.h"

namespace smdl {

void quoted::append_to(std::string &result) {
  result += '\'';
  result += str;
  result += '\'';
}

void quoted_path::append_to(std::string &result) {
  result += '\'';
  auto relPath{relative(std::string(str))};
  if (!relPath.empty() && relPath.size() < str.size()) {
    result += relPath;
  } else {
    result += str;
  }
  result += '\'';
}

} // namespace smdl
