include_guard(GLOBAL)

set(CMAKE_POSITION_INDEPENDENT_CODE ON CACHE BOOL "" FORCE)

# The optional dependencies are fetched with two 'FetchContent_Declare'
# arguments that older CMake does not have: OVERRIDE_FIND_PACKAGE (3.24), which
# is how Ptex's own 'find_package(libdeflate)' resolves to the copy built here,
# and EXCLUDE_FROM_ALL (3.28), which keeps the subprojects' tools, test
# binaries, and install rules out of this build. Nothing else in SpectralMDL
# needs either, so the higher floor is a condition of asking for a fetch rather
# than of configuring at all.
if(CMAKE_VERSION VERSION_LESS "3.28")
  set(FetchingOptions)
  foreach(Opt SMDL_ENABLE_NANOVDB SMDL_ENABLE_PTEX SMDL_TOY)
    if(${Opt})
      list(APPEND FetchingOptions "-D${Opt}=OFF")
    endif()
  endforeach()
  if(FetchingOptions)
    list(JOIN FetchingOptions " " FetchingOptions)
    message(FATAL_ERROR
      "This is CMake ${CMAKE_VERSION}, and fetching the optional dependencies "
      "needs CMake 3.28 or newer. Configure with ${FetchingOptions} to build "
      "SpectralMDL and the 'smdl' program alone, which need only CMake "
      "${CMAKE_MINIMUM_REQUIRED_VERSION}.")
  endif()
endif()

# Work out the one code generation backend to link. This has to happen before
# the SMDL_BUILD_LLVM branch below, which needs the answer in order to tell
# LLVM what to build.
if(SMDL_LLVM_ARCH)
  set(SMDL_ARCH "${SMDL_LLVM_ARCH}")
else()
  # See NativeArch.cmake, included before this script in the 
  # top level CMakeLists.txt
  smdl_detect_native_arch(SMDL_ARCH)
endif()

# If SMDL_BUILD_LLVM=ON, then we'll build all the LLVM libraries
# we need to compile the rest of SMDL. NOTE: This will take a 
# little while!
# 
# CMake provides two mechanisms that sound useful---that is, 
# `FetchContent_Declare` and `ExternalProject_Add`. Neither of 
# these is actually suitable! This is because LLVM is huge and 
# it is critical to shallow clone (`--depth=1`) just the tagged 
# release. FetchContent can be shallow, but it always pulls ALL 
# branches, which takes forever. For this reason, we use `execute_process`
# to the clone manually into our build directory, then `add_subdirectory`
# to incorporate it into our CMake build.

# The oldest LLVM whose API this compiles against, and the tag that
# SMDL_BUILD_LLVM clones when there is no suitable installation to find.
set(SMDL_LLVM_VERSION_MIN "22.1.0")
set(SMDL_LLVM_TAG "llvmorg-22.1.8")
if(SMDL_BUILD_LLVM)
  # Fail now rather than after spending several minutes cloning LLVM.
  if(NOT SMDL_ARCH)
    message(FATAL_ERROR
      "SpectralMDL cannot tell which LLVM codgen backend to build for "
      "'${CMAKE_SYSTEM_PROCESSOR}'. SpectralMDL is a JIT compiler, so "
      "it only builds on hardware that LLVM can generate code for. If "
      "LLVM supports this processor, add it to 'smdl_llvm_arch_for_processor' "
      "in 'cmake/NativeArch.cmake' or set SMDL_LLVM_ARCH to the backend name "
      "(for example 'X86' or 'AArch64').")
  endif()
  message(STATUS "LLVM: building ${SMDL_LLVM_TAG} in tree")
  execute_process(
    COMMAND
      "git"
      "clone"
      "--depth=1"
      "--branch=${SMDL_LLVM_TAG}"
      "https://github.com/llvm/llvm-project"
      "llvm-project"
    WORKING_DIRECTORY "${CMAKE_BINARY_DIR}"
  )
  set(LLVM_ENABLE_PROJECTS "llvm" CACHE INTERNAL "" FORCE)
  set(LLVM_ENABLE_PIC ON CACHE INTERNAL "" FORCE)
  set(LLVM_ENABLE_RTTI ${SMDL_RTTI} CACHE INTERNAL "" FORCE)
  set(LLVM_ENABLE_BINDINGS OFF CACHE INTERNAL "" FORCE)
  set(LLVM_BUILD_EXAMPLES OFF CACHE INTERNAL "" FORCE)
  set(LLVM_BUILD_TESTS OFF CACHE INTERNAL "" FORCE)
  set(LLVM_BUILD_TOOLS OFF CACHE INTERNAL "" FORCE)
  set(LLVM_ENABLE_ZLIB OFF CACHE INTERNAL "" FORCE)
  set(LLVM_ENABLE_ZSTD OFF CACHE INTERNAL "" FORCE)
  set(LLVM_INCLUDE_BENCHMARKS OFF CACHE INTERNAL "" FORCE)
  set(LLVM_INSTALL_TOOLCHAIN_ONLY ON CACHE INTERNAL "" FORCE)
  set(LLVM_TARGETS_TO_BUILD "${SMDL_ARCH}" CACHE INTERNAL "" FORCE)
  add_subdirectory(
    "${CMAKE_BINARY_DIR}/llvm-project/llvm" 
    "${CMAKE_BINARY_DIR}/llvm-project-build"
  )
  set(
    LLVM_INCLUDE_DIR 
    "${CMAKE_BINARY_DIR}/llvm-project/llvm/include"
    "${CMAKE_BINARY_DIR}/llvm-project-build/include"
  )
else()
  # Do not pass the version to 'find_package'. LLVM ships a hand written
  # 'LLVMConfigVersion.cmake' that treats the request as an exact major.minor
  # match with a patch floor, so asking for 22.1.0 would reject 22.2 and every
  # later release, and fail with a generic message instead of this one.
  find_package(LLVM REQUIRED)
  if(LLVM_PACKAGE_VERSION VERSION_LESS SMDL_LLVM_VERSION_MIN)
    message(FATAL_ERROR
      "SpectralMDL needs LLVM ${SMDL_LLVM_VERSION_MIN} or newer, but found "
      "${LLVM_PACKAGE_VERSION} at '${LLVM_DIR}'. Point CMAKE_PREFIX_PATH at a "
      "newer LLVM, or configure -DSMDL_BUILD_LLVM=ON to build one.")
  endif()
  message(STATUS "LLVM: ${LLVM_PACKAGE_VERSION} (${LLVM_DIR})")
  # LLVM computed this from its own host triple, and it is the last word:
  # 'llvm-config.h' defines LLVM_NATIVE_TARGET as
  # 'LLVMInitialize${LLVM_NATIVE_ARCH}Target', so this is literally the
  # backend that 'InitializeNativeTarget()' is going to ask for.
  if(LLVM_NATIVE_ARCH)
    if(SMDL_ARCH AND NOT SMDL_ARCH STREQUAL LLVM_NATIVE_ARCH)
      if(SMDL_LLVM_ARCH)
        set(ArchBlame "SMDL_LLVM_ARCH asks for '${SMDL_LLVM_ARCH}'")
      else()
        set(ArchBlame "processor '${CMAKE_SYSTEM_PROCESSOR}' needs '${SMDL_ARCH}'")
      endif()
      message(FATAL_ERROR
        "${ArchBlame}, but this LLVM was built to run on '${LLVM_NATIVE_ARCH}'. "
        "Direct CMAKE_PREFIX_PATH at an LLVM for the right machine, or configure "
        "-DSMDL_BUILD_LLVM=ON to build one.")
    endif()
    set(SMDL_ARCH "${LLVM_NATIVE_ARCH}")
  elseif(NOT SMDL_ARCH)
    message(FATAL_ERROR
      "SpectralMDL cannot tell which LLVM code generation backend to link "
      "for processor '${CMAKE_SYSTEM_PROCESSOR}', and this LLVM does not "
      "report an LLVM_NATIVE_ARCH. Set SMDL_LLVM_ARCH to the backend name "
      "('X86', 'AArch64', etc).")
  endif()
  if(NOT SMDL_ARCH IN_LIST LLVM_TARGETS_TO_BUILD)
    message(FATAL_ERROR
      "This LLVM was built without the '${SMDL_ARCH}' code generator "
      "(it has: ${LLVM_TARGETS_TO_BUILD}), so it cannot JIT compile for this "
      "machine. Rebuild LLVM with LLVM_TARGETS_TO_BUILD=host, or configure "
      "SpectralMDL with -DSMDL_BUILD_LLVM=ON to build one.")
  endif()
endif()
message(STATUS "LLVM_INCLUDE_DIR: ${LLVM_INCLUDE_DIR}")
message(STATUS "SMDL_ARCH: ${SMDL_ARCH}")
set(
  SMDL_LLVM_TARGETS
  "LLVMTarget"
  "LLVMOrcJIT"
  "LLVM${SMDL_ARCH}CodeGen"
  )
foreach(SMDL_LLVM_TARGET ${SMDL_LLVM_TARGETS})
  if(NOT TARGET ${SMDL_LLVM_TARGET})
    message(FATAL_ERROR
      "LLVM library target '${SMDL_LLVM_TARGET}' does not exist. This LLVM "
      "cannot generate code for '${SMDL_ARCH}'.")
  endif()
endforeach()

include(FetchContent)
set(FETCHCONTENT_QUIET FALSE)

# Link every fetched dependency into 'libSpectralMDL.so' and the programs
# statically, so that nothing has to be found on the library path at run time.
# A normal variable rather than a cache entry is deliberate: it shadows the
# cache for this directory and below without stomping the preference of a
# parent project that pulls SpectralMDL in by 'add_subdirectory'. SpectralMDL
# itself declares SHARED explicitly and is unaffected.
set(BUILD_SHARED_LIBS OFF)

# Some of these projects register CTest entries whether or not the enclosing
# project asked for any, and they show up as noise in our own 'ctest' output.
# Ptex is the stubborn case: it calls 'enable_testing()' itself and then adds
# four tests unconditionally, so BUILD_TESTING is not the lever, and our own
# 'testing/' subdirectory honors BUILD_TESTING and would go dark with it.
# Shadow the two test commands instead. This hides the tests without stopping
# their binaries from compiling, which is what EXCLUDE_FROM_ALL below is for,
# so the per-project switches (EMBREE_TESTING_INTENSITY and friends) are still
# the first line of defense.
function(add_test)
  if(NOT SMDL_SUPPRESS_FOREIGN_TESTS)
    _add_test(${ARGV})
  endif()
endfunction()
function(set_tests_properties)
  if(NOT SMDL_SUPPRESS_FOREIGN_TESTS)
    _set_tests_properties(${ARGV})
  endif()
endfunction()

# Fetch one dependency the way SpectralMDL always wants it: shallow cloned at
# a fixed tag, resolving any 'find_package' of the same name to this copy
# instead of the system, and built only as far as something links it. That
# last part is EXCLUDE_FROM_ALL, which keeps the tools and test binaries
# nobody asked for out of the build and keeps the subproject's own install
# rules out of our install tree. OPTIONS takes name/value pairs and seeds them
# as internal cache entries, which is how all of these projects expect to be
# configured from the outside.
#
# This is a macro rather than a function because 'FetchContent_MakeAvailable'
# publishes '<name>_SOURCE_DIR' into the calling scope, and 'lib' and
# 'programs' read 'ptex_SOURCE_DIR' and 'opensubdiv_SOURCE_DIR' from the top
# level. A function would swallow both and leave empty include paths behind.
macro(smdl_fetch_dependency DEP_NAME)
  set(Options)
  set(OneValArgs REPOSITORY TAG SOURCE_SUBDIR)
  set(MultiValArgs OPTIONS)
  cmake_parse_arguments(
    Args "${Options}" "${OneValArgs}" "${MultiValArgs}" ${ARGN}
    )
  list(LENGTH Args_OPTIONS NumOptionWords)
  math(EXPR OddOptionWord "${NumOptionWords} % 2")
  if(OddOptionWord)
    message(FATAL_ERROR
      "smdl_fetch_dependency(${DEP_NAME}) wants OPTIONS in name/value pairs, "
      "but got ${NumOptionWords} words. A dropped value would otherwise seed "
      "an empty cache entry and silently turn the option off.")
  endif()
  # Record the pinned tag at top level (this is a macro, so the variable
  # lands in the including scope), letting 'lib' and 'programs' bake
  # dependency versions into BuildInfo and the '--version' printers.
  set(SMDL_${DEP_NAME}_TAG "${Args_TAG}")
  # SOURCE_SUBDIR names the directory to add. If the directory has no
  # CMakeLists.txt, that means to ask for the sources alone, i.e., a 
  # header-only dependency like NanoVDB
  set(DeclareArgs)
  if(Args_SOURCE_SUBDIR)
    list(APPEND DeclareArgs SOURCE_SUBDIR "${Args_SOURCE_SUBDIR}")
  endif()
  FetchContent_Declare(
    "${DEP_NAME}"
    GIT_REPOSITORY "${Args_REPOSITORY}"
    GIT_TAG "${Args_TAG}"
    GIT_SHALLOW TRUE
    GIT_PROGRESS TRUE
    OVERRIDE_FIND_PACKAGE
    EXCLUDE_FROM_ALL
    ${DeclareArgs}
    )
  while(Args_OPTIONS)
    list(POP_FRONT Args_OPTIONS OptionName OptionValue)
    set(${OptionName} ${OptionValue} CACHE INTERNAL "")
  endwhile()
  set(SMDL_SUPPRESS_FOREIGN_TESTS ON)
  FetchContent_MakeAvailable("${DEP_NAME}")
  set(SMDL_SUPPRESS_FOREIGN_TESTS OFF)
endmacro()

if(SMDL_ENABLE_PTEX)
  # Ptex 2.5 compresses with libdeflate instead of zlib, and its top-level
  # 'find_package(libdeflate REQUIRED)' would otherwise demand a system
  # install. OVERRIDE_FIND_PACKAGE writes a redirect config into
  # CMAKE_FIND_PACKAGE_REDIRECTS_DIR, which 'find_package' consults before
  # anything else, so Ptex resolves to the copy built here.
  smdl_fetch_dependency(
    "libdeflate"
    REPOSITORY "https://github.com/ebiggers/libdeflate"
    TAG "v1.25"
    OPTIONS
      LIBDEFLATE_BUILD_SHARED_LIB OFF
      LIBDEFLATE_BUILD_GZIP OFF
    )
  smdl_fetch_dependency(
    "Ptex"
    REPOSITORY "https://github.com/wdas/ptex"
    TAG "v2.5.2"
    OPTIONS
      PTEX_BUILD_STATIC_LIBS ON
      PTEX_BUILD_SHARED_LIBS OFF
      PTEX_BUILD_DOCS OFF
    )
  # Assimp
  #
  # Narrowed to the widely adopted formats. Assimp reads roughly fifty, most
  # of them for games and modelers that no asset pipeline is going to hand us,
  # and every one costs build time and binary size. Exporters are on because
  # the asset processing tooling reads and writes through Assimp both ways,
  # so each format is enabled in both directions where a writer exists (there
  # is no OFF exporter). 'assbin' is Assimp's own container and the only
  # lossless one of the set, which is what makes it a round-trip test
  # fixture rather than a delivery format.
  smdl_fetch_dependency(
    "Assimp"
    REPOSITORY "https://github.com/assimp/assimp"
    TAG "v6.0.5"
    OPTIONS
      ASSIMP_WARNINGS_AS_ERRORS OFF
      ASSIMP_BUILD_SAMPLES OFF
      ASSIMP_BUILD_TESTS OFF
      ASSIMP_BUILD_ZLIB ON
      ASSIMP_INSTALL OFF
      ASSIMP_NO_EXPORT OFF
      ASSIMP_BUILD_ALL_IMPORTERS_BY_DEFAULT OFF
      ASSIMP_BUILD_ASSBIN_IMPORTER ON
      ASSIMP_BUILD_COLLADA_IMPORTER ON
      ASSIMP_BUILD_FBX_IMPORTER ON
      ASSIMP_BUILD_GLTF_IMPORTER ON
      ASSIMP_BUILD_OBJ_IMPORTER ON
      ASSIMP_BUILD_OFF_IMPORTER ON
      ASSIMP_BUILD_OPENGEX_IMPORTER ON
      ASSIMP_BUILD_PLY_IMPORTER ON
      ASSIMP_BUILD_STL_IMPORTER ON
      ASSIMP_BUILD_ALL_EXPORTERS_BY_DEFAULT OFF
      ASSIMP_BUILD_ASSBIN_EXPORTER ON
      ASSIMP_BUILD_COLLADA_EXPORTER ON
      ASSIMP_BUILD_FBX_EXPORTER ON
      ASSIMP_BUILD_GLTF_EXPORTER ON
      ASSIMP_BUILD_OBJ_EXPORTER ON
      ASSIMP_BUILD_OPENGEX_EXPORTER ON
      ASSIMP_BUILD_PLY_EXPORTER ON
      ASSIMP_BUILD_STL_EXPORTER ON
    )
endif()

if(SMDL_ENABLE_NANOVDB)
  # NanoVDB lives inside the OpenVDB repository and has no release of its own,
  # so the tag here is an OpenVDB one. 'nanovdb' holds no CMakeLists.txt, which
  # is what makes it the right SOURCE_SUBDIR: the headers get populated and
  # nothing is ever added as a subdirectory, so OpenVDB proper, along with the
  # TBB and Blosc it wants, stays out of the build entirely.
  smdl_fetch_dependency(
    "NanoVDB"
    REPOSITORY "https://github.com/AcademySoftwareFoundation/openvdb"
    TAG "v13.0.0"
    SOURCE_SUBDIR "nanovdb"
    )
endif()

if(SMDL_TOY)
  # Embree
  #
  # Only the geometry types smdl-toy actually creates are compiled in:
  # triangles, curves, user geometry for the analytic primitives, and the two
  # instance kinds. Turning off everything irrelevant alongside the default
  # SMDL_TOY_EMBREE_X86_ISA=AVX2 reduces the smdl-toy binary size by ~20MB.
  set(EmbreeISAOption)
  if(SMDL_ARCH STREQUAL "X86")
    list(APPEND EmbreeISAOption EMBREE_MAX_ISA "${SMDL_TOY_EMBREE_X86_ISA}")
  endif()
  smdl_fetch_dependency(
    "Embree"
    REPOSITORY "https://github.com/RenderKit/embree"
    TAG "v4.4.1"
    OPTIONS
      EMBREE_STATIC_LIB ON
      EMBREE_TUTORIALS OFF
      EMBREE_ISPC_SUPPORT OFF
      EMBREE_TASKING_SYSTEM OFF
      EMBREE_TESTING_INTENSITY 0  # The documented "no testing" setting
      EMBREE_GEOMETRY_QUAD OFF
      EMBREE_GEOMETRY_SUBDIVISION OFF
      EMBREE_GEOMETRY_GRID OFF
      EMBREE_GEOMETRY_POINT OFF
      EMBREE_RAY_PACKETS OFF
      EMBREE_FILTER_FUNCTION OFF
      EMBREE_RAY_MASK OFF
      ${EmbreeISAOption}
    )

  # OpenSubdiv
  #
  # CPU only: smdl-toy uses Far/Sdc/Vtr for load-time uniform refinement,
  # which the 'osd_static_cpu' library carries. Everything GPU, every
  # tasking backend, and all of the examples and regression suites are
  # switched off. The library does not export its include directory, so
  # 'smdl_add_program' passes '${opensubdiv_SOURCE_DIR}' by hand.
  smdl_fetch_dependency(
    "OpenSubdiv"
    REPOSITORY "https://github.com/PixarAnimationStudios/OpenSubdiv"
    TAG "v3_7_0"
    OPTIONS
      NO_EXAMPLES ON
      NO_TUTORIALS ON
      NO_REGRESSION ON
      NO_TESTS ON
      NO_GLTESTS ON
      NO_DOC ON
      NO_OMP ON
      NO_TBB ON
      NO_CUDA ON
      NO_OPENCL ON
      NO_CLEW ON
      NO_DX ON
      NO_METAL ON
      NO_OPENGL ON
      NO_GLEW ON
      NO_GLFW ON
      NO_PTEX ON
    )
endif()
