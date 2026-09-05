option(SMDL_INSTALL "Generate install rules?" ${PROJECT_IS_TOP_LEVEL})
option(SMDL_BUILD_LLVM "Build LLVM?" OFF)
set(SMDL_LLVM_ARCH "" CACHE STRING "LLVM code generation backend to link, or empty to detect it")
set(SMDL_SANITIZE "" CACHE STRING "Sanitizers to build with, passed to -fsanitize= (e.g. 'address', 'undefined', 'address,undefined')")
option(SMDL_RTTI "Build with C++ Run-Time Type Information (RTTI)?" ON)
# The instruction set C++ may assume on x86. Empty is the x86-64 baseline
# which runs anywhere. Raising it makes binaries require that instruction 
# set and a machine without it faults on the first instruction it does not
# recognize, with no diagnostic. AVX2 is the useful setting and is the
# x86-64-v3 microarchitecture level, meaning Intel from Haswell (2013) and
# AMD from Excavator (2015) but not the low-power Atom, Celeron and Pentium
# Silver line, which shipped without AVX2 as late as Jasper Lake in 2021.
set(SMDL_CXX_X86_ISA "" CACHE STRING
  "Instruction set the C++ may assume on x86: SSE42, AVX, AVX2, or AVX512 (empty for the x86-64 baseline)")
# Off by default because the resulting binaries are only guaranteed to run
# on machines with the build host's instruction sets. This only affects the
# C++ in the library and the toy renderer: JIT'd material code always
# targets the host machine regardless, and Embree builds and dispatches its
# own kernels (see SMDL_TOY_EMBREE_X86_ISA).
option(SMDL_CXX_NATIVE "Compile for the build machine's CPU (-march=native) at the cost of portability?" OFF)
# Off by default because it is not free and not yet needed: it trades the
# task spawns for one atomic per chunk, and on a 12 thread machine the
# renderer already runs at 98 to 99 percent of perfect scaling, so there
# is nothing to win. Turn it on to measure on hardware with enough cores
# to show a difference.
option(SMDL_DYNAMIC_SCHEDULING "Schedule parallel loops dynamically instead of in fixed chunks?" OFF)
option(SMDL_ENABLE_PTEX "Build with Ptex integration?" ON)
option(SMDL_ENABLE_NANOVDB "Build with NanoVDB integration?" ON)
option(SMDL_TOY "Build the toy renderer?" OFF)
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
