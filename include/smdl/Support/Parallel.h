/// \file
#pragma once

#include <cstddef>
#include <functional>

#include "smdl/Export.h"
#include "smdl/Support/MacroHelpers.h"

namespace smdl {

/// \addtogroup Support
/// \{

/// \name Functions (parallel)
/// \{

#if 0
/// Invoke function once on each worker thread.
SMDL_EXPORT
void invoke_once_on_each_worker_thread(const std::function<void()> &func);

/// Is executing on main thread?
[[nodiscard]] SMDL_EXPORT bool is_main_thread() noexcept;

/// Is executing on worker thread?
[[nodiscard]] SMDL_EXPORT bool is_worker_thread() noexcept;
#endif

/// Get worker thread index.
[[nodiscard]] SMDL_EXPORT int get_worker_thread_index() noexcept;

/// Get worker thread count.
[[nodiscard]] SMDL_EXPORT int get_worker_thread_count() noexcept;

/// Parallel for loop.
SMDL_EXPORT
void parallel_for(size_t indexBegin, size_t indexEnd,
                  const std::function<void(size_t)> &func);

/// \}

/// \}

} // namespace smdl
