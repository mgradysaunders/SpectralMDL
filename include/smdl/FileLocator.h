#pragma once

#include "smdl/common.h"

namespace smdl {

class SMDL_EXPORT FileLocator final {
public:
  class SMDL_EXPORT ImagePath final {
  public:
    [[nodiscard]] bool operator==(const ImagePath &other) const = default;

    [[nodiscard]] bool operator!=(const ImagePath &other) const = default;

    [[nodiscard]] auto operator<=>(const ImagePath &other) const = default;

    std::filesystem::path path{};

    uint32_t iU{};

    uint32_t iV{};
  };

  void set_search_pwd(bool yes) { searchPwd = yes; }

  void add_search_dir(std::filesystem::path dir, bool isRecursive = false);

  [[nodiscard]] std::optional<std::filesystem::path> locate(
      const std::filesystem::path &fname, std::filesystem::path fname0 = {}) const;

  [[nodiscard]] std::vector<ImagePath> locate_images(std::filesystem::path fname, std::filesystem::path fname0 = {}) const;

private:
  bool searchPwd{true};

  std::vector<std::pair<std::filesystem::path, bool>> searchDirs{};
};

} // namespace smdl
