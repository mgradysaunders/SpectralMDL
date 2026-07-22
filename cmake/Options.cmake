option(SMDL_BUILD_LLVM "Build LLVM?" OFF)
option(SMDL_ENABLE_PTEX "Build with Ptex?" ON)
option(SMDL_ENABLE_RTTI "Build with C++ Run-Time Type Information (RTTI)?" ON)
option(SMDL_TOY "Build toy renderer?" OFF)
set(SMDL_SANITIZE "" CACHE STRING
  "Sanitizers to build with, passed to -fsanitize= (e.g. 'address', 'undefined', 'address,undefined')")
