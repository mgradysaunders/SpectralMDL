/// \file
/// The CPU denormal policy the render threads run under.
#pragma once

/// Does this target have an MXCSR to set? SSE2 is part of the x86-64
/// baseline, so every 64-bit x86 build qualifies; the 32-bit MSVC spelling
/// is the `/arch:SSE2` level. Anything else (AArch64 in particular) has no
/// MXCSR and `ScopedFlushDenormals` compiles away to nothing.
#if defined(__SSE2__) || defined(_M_X64) || \
    (defined(_M_IX86_FP) && _M_IX86_FP >= 2)
#define SMDL_TOY_HAS_MXCSR 1
#include <xmmintrin.h>
#else
#define SMDL_TOY_HAS_MXCSR 0
#endif

/// Flush denormals to zero on the calling thread for the duration of the
/// scope, restoring the thread's previous mode on the way out.
///
/// A denormal result costs a microcode assist of order a hundred cycles,
/// and the JIT'd material code produces enough of them to be a
/// measurable fraction of a render. Nothing is lost by flushing: a
/// denormal float is below 1e-38, which no radiance estimate, throughput
/// or density this renderer forms can distinguish from zero.
///
/// Scoped rather than set once per thread because `parallelFor()` hands
/// its workers back to a shared pool, and the mode is a property of the
/// thread: a task that leaves it set would impose it on whatever
/// unrelated work the pool schedules there next.
class ScopedFlushDenormals final {
public:
  ScopedFlushDenormals() noexcept {
#if SMDL_TOY_HAS_MXCSR
    // Flush-to-zero for denormal results, denormals-are-zero for
    // denormal operands. Both bits are architectural on x86-64.
    constexpr unsigned FLUSH_TO_ZERO{1u << 15};
    constexpr unsigned DENORMALS_ARE_ZERO{1u << 6};
    mSaved = _mm_getcsr();
    _mm_setcsr(mSaved | FLUSH_TO_ZERO | DENORMALS_ARE_ZERO);
#endif
  }

  ~ScopedFlushDenormals() {
#if SMDL_TOY_HAS_MXCSR
    _mm_setcsr(mSaved);
#endif
  }

  ScopedFlushDenormals(const ScopedFlushDenormals &) = delete;

  ScopedFlushDenormals &operator=(const ScopedFlushDenormals &) = delete;

#if SMDL_TOY_HAS_MXCSR
private:
  /// The mode to restore.
  unsigned mSaved{};
#endif
};
