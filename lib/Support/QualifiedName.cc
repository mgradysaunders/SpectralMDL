#include "smdl/Support/QualifiedName.h"

#include <algorithm>

namespace smdl {

std::vector<std::string_view>
splitQualifiedName(std::string_view qualifiedName) {
  std::vector<std::string_view> components{};
  if (qualifiedName.substr(0, 2) == "::") {
    qualifiedName.remove_prefix(2);
  }
  if (qualifiedName.empty()) {
    return components;
  }
  while (true) {
    size_t pos{qualifiedName.find("::")};
    if (pos == qualifiedName.npos) {
      components.push_back(qualifiedName);
      return components;
    }
    components.push_back(qualifiedName.substr(0, pos));
    qualifiedName.remove_prefix(pos + 2);
  }
}

std::string joinQualifiedName(Span<const std::string_view> components) {
  std::string name{};
  for (const auto &component : components) {
    name += "::";
    name += component;
  }
  return name;
}

bool isQualifiedNameSuffix(std::string_view name,
                           std::string_view qualifiedName) {
  std::vector<std::string_view> nameComponents{splitQualifiedName(name)};
  std::vector<std::string_view> components{splitQualifiedName(qualifiedName)};
  if (nameComponents.empty() || nameComponents.size() > components.size()) {
    return false;
  }
  return std::equal(nameComponents.rbegin(), nameComponents.rend(),
                    components.rbegin());
}

} // namespace smdl
