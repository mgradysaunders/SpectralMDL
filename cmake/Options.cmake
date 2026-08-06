option(SMDL_BUILD_LLVM "Build LLVM?" OFF)
option(SMDL_ENABLE_PTEX "Build with Ptex?" ON)
# Defaults OFF when SMDL is pulled in as a subproject (FetchContent or
# add_subdirectory), so that a downstream consumer does not silently inherit
# SMDL's headers, package config, and export set into its own 'install'.
option(SMDL_INSTALL "Generate install rules?" ${PROJECT_IS_TOP_LEVEL})
option(SMDL_ENABLE_RTTI "Build with C++ Run-Time Type Information (RTTI)?" ON)
option(SMDL_TOY "Build toy renderer?" OFF)
set(SMDL_SANITIZE "" CACHE STRING
  "Sanitizers to build with, passed to -fsanitize= (e.g. 'address', 'undefined', 'address,undefined')")
