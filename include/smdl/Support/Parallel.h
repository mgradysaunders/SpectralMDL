/// \file
#pragma once

#include <cstddef>
#include <functional>

#include "smdl/Export.h"

namespace smdl {

/// \addtogroup support
/// \{

/// \name Functions (parallel)
///
/// The thread pool behind `parallelFor()` is process-wide and shared by
/// everything in the library, so `setThreadCount()` is a free function
/// rather than a setting on any one `Compiler`. It is also created once,
/// lazily, by whichever parallel operation runs first, which is what
/// makes the ordering requirement below real rather than advisory.
///
/// \{

/// Limit parallel work to at most `numThreads` threads.
///
/// Zero, the default, means every hardware thread. One means no thread
/// pool at all: `parallelFor()` runs the loop inline on the calling
/// thread, which is what makes a single-threaded run debuggable.
///
/// \note
/// The thread pool is created by the first parallel operation and
/// cannot be resized afterward, so this must be called before
/// `Compiler::compile()` and before any `parallelFor()`. A later call
/// logs a warning and does nothing.
///
SMDL_EXPORT void setThreadCount(unsigned numThreads);

/// The number of threads `parallelFor()` resolves to, which is the
/// hardware thread count unless `setThreadCount()` asked for fewer.
///
/// Does not create the thread pool, so this is safe to call before
/// `setThreadCount()`.
[[nodiscard]] SMDL_EXPORT unsigned getThreadCount();

/// Invoke `func(i)` for every `i` in `[begin, end)`, in parallel.
///
/// The split into tasks depends only on the range, so the work each
/// index sees is the same at any thread count.
///
/// \note
/// `func` must not throw. It runs on a thread pool built without
/// exception support, so an exception that escapes it terminates the
/// process; a caller whose body can throw has to catch at the body and
/// rethrow on the calling thread.
///
SMDL_EXPORT void parallelFor(size_t begin, size_t end,
                             const std::function<void(size_t)> &func);

/// \}

/// \}

} // namespace smdl
