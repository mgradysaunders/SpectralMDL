#include "smdl/Support/StringHelpers.h"
#include "smdl/Support/Filesystem.h"

namespace smdl {

void Quoted::appendTo(std::string &result) {
  result += '\'';
  result += str;
  result += '\'';
}

void QuotedPath::appendTo(std::string &result) {
  result += '\'';
  auto relPath{makePathRelative(std::string(str))};
  if (!relPath.empty() && relPath.size() < str.size()) {
    result += relPath;
  } else {
    result += str;
  }
  result += '\'';
}

} // namespace smdl
