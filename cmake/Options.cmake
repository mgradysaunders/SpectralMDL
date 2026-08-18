option(SMDL_INSTALL "Generate install rules?" ${PROJECT_IS_TOP_LEVEL})
option(SMDL_BUILD_LLVM "Build LLVM?" OFF)
set(SMDL_LLVM_ARCH "" CACHE STRING
  "LLVM code generation backend to link, or empty to detect it")
option(SMDL_ENABLE_NANOVDB "Build with NanoVDB?" ON)
option(SMDL_ENABLE_PTEX "Build with Ptex?" ON)
option(SMDL_ENABLE_RTTI "Build with C++ Run-Time Type Information (RTTI)?" ON)
set(SMDL_SANITIZE "" CACHE STRING
  "Sanitizers to build with, passed to -fsanitize= (e.g. 'address', 'undefined', 'address,undefined')")
option(SMDL_TOY "Build toy renderer?" OFF)
# Embree compiles ray tracing kernels for every instruction set up to 
# a maximum level and dispatches between them at run time: each level
# costs build time and binary size whether or not the machine can 
# execute it. AVX2 is the default ceiling because the AVX-512
# kernels are the largest and the least likely to run: no consumer
# Intel CPU since Alder Lake, and no AMD CPU before Zen 4. Raise to
# AVX512 when the machine actually has it (Xeon, or Zen 4 and later).
# 
# The Embree ISA ladder is DEFAULT, SSE2, SSE42, AVX, AVX2, AVX-512. And 
# footgun alert: DEFAULT is not the setting it sounds like. It detects the
# compiler's baseline target, which on x86-64 is SSE2, not the maximum the
# host can execute. Embree handles the ARM ladder differently and
# chooses correctly on its own.
set(SMDL_TOY_EMBREE_X86_ISA "AVX2" CACHE STRING
  "Highest instruction set Embree builds ray tracing kernels for (x86 only)")
