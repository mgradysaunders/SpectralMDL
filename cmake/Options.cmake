# If ON, the compiler is configured to use all available CPU 
# instructions on the build machine. This maximizes performance at the 
# cost of limiting how distributable the binaries are. If you build on 
# an x86-64 machine with an optional CPU feature, say AVX512, then the 
# resulting binaries won't work on an otherwise compatible x86-64 
# machine without the optional feature.
option(SMDL_NATIVE "Build for native hardware?" OFF)

option(SMDL_BUILD_LLVM "Build LLVM?" OFF)

option(SMDL_USE_BOOST_FILESYSTEM "Build with Boost filesystem instead of STL?" OFF)

option(SMDL_ENABLE_PTEX "Build with Ptex?" ON)

option(SMDL_ENABLE_RTTI "Build with C++ Run-Time Type Information (RTTI)?" ON)

option(SMDL_TOY "Build toy renderer?" OFF)

