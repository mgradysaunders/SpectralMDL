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
# C++ in the library: JIT'd material code always targets the host machine
# regardless.
option(SMDL_CXX_NATIVE "Compile for the build machine's CPU (-march=native) at the cost of portability?" OFF)
# Off by default because it is not free and not yet needed: it trades the
# task spawns for one atomic per chunk, and on a 12 thread machine a host
# program already runs at 98 to 99 percent of perfect scaling, so there
# is nothing to win. Turn it on to measure on hardware with enough cores
# to show a difference.
option(SMDL_DYNAMIC_SCHEDULING "Schedule parallel loops dynamically instead of in fixed chunks?" OFF)
option(SMDL_ENABLE_PTEX "Build with Ptex integration?" ON)
option(SMDL_ENABLE_NANOVDB "Build with NanoVDB integration?" ON)
