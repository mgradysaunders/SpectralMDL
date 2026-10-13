/// \file
#pragma once

#include <functional>
#include <memory>
#include <optional>
#include <string>
#include <string_view>

#include "smdl/Support/Error.h"

namespace smdl {

/// \addtogroup resource
/// \{

/// What a `HostResourceLoader` is asked to load.
struct HostResourceRequest final {
  /// The kind, which is the key the loader is registered under in
  /// `Compiler::hostResourceLoaders`.
  std::string_view kind{};

  /// The file, located the way every resource is and made canonical.
  std::string_view fileName{};

  /// What to load from inside the file, or empty if the source named
  /// nothing.
  std::string_view selector{};
};

/// A resource the host loaded for an MDL `host_resource`.
struct HostResource final {
  /// The resource, which JIT-compiled code receives as the raw pointer
  /// `host_resource.ptr`. The `Compiler` holds this reference until the
  /// next `compile()` or until it is destroyed, so a host that keeps one
  /// of its own can share the resource across recompiles.
  std::shared_ptr<void> ptr{};

  /// The flags, baked into `host_resource.flags` as a constant.
  int flags{};

  /// What was loaded, for the line logged at debug level. Optional.
  std::string description{};
};

/// A loader for one kind of host resource.
///
/// It fills `resource`, or returns an error, which `Compiler::compile()`
/// reports as a warning against the MDL line that named the file. An
/// exception escaping it is reported the same way, and so is a success
/// that leaves `resource.ptr` null, since a null pointer is how MDL code
/// tells that nothing loaded. After any failure the resource reads as
/// null with no flags.
///
/// It runs on the thread calling `compile()`, while materials are
/// emitted, at most once per distinct kind, file content, and selector
/// per compile, and never from JIT-compiled code.
using HostResourceLoader = std::function<std::optional<Error>(
    const HostResourceRequest &request, HostResource &resource)>;

/// \}

} // namespace smdl
