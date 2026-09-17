# The C++ suite: one executable over the vendored framework, with the
# test for 'lib/X/Y.cc' at 'X/Y.cc' here. 'doctest.h' and 'doctest.cc'
# are the framework and its runner, compiled into the suite alongside
# the tests.
#
# The RTTI flag matches the library's, as 'lib' and 'programs' do; the
# 'BuildInfo' test relies on this to check 'hasRTTI' against the test
# binary's own compile. The floating-point flags match too, because the
# suite tests inline header code (the fast-math approximations) that
# only means what it means under the flags the library compiles it with.
add_executable("smdl-doctest")
target_sources(
  "smdl-doctest"
  PRIVATE
    "doctest.cc"
    "Common.cc"
    "Compiler.cc"
    "Compiler/Diagnostics.cc"
    "Compiler/Emitter.cc"
    "Compiler/Resources.cc"
    "Doc.cc"
    "JIT.cc"
    "Manifold.cc"
    "Module.cc"
    "Parser.cc"
    "RenderUtil/Colorimetry.cc"
    "RenderUtil/FastMath.cc"
    "RenderUtil/Haze.cc"
    "RenderUtil/Illuminant.cc"
    "RenderUtil/MajorantWalk.cc"
    "RenderUtil/MetalIOR.cc"
    "RenderUtil/MonteCarlo.cc"
    "RenderUtil/OpticalGlass.cc"
    "RenderUtil/SunSky.cc"
    "Resource/BSDFMeasurement.cc"
    "Resource/Image.cc"
    "Resource/LightProfile.cc"
    "Resource/Spectrum.cc"
    "Resource/SpectrumLibrary.cc"
    "Resource/VoxelGrid.cc"
    "SceneData.cc"
    "Support/BumpPtrAllocator.cc"
    "Support/Compress.cc"
    "Support/FileLocator.cc"
    "Support/Filesystem.cc"
    "Support/Logger.cc"
    "Support/MD5Hash.cc"
    "Support/Profiler.cc"
    "Support/QualifiedName.cc"
    "Support/RNG.cc"
    "Support/SIMD.cc"
    "Support/Strings.cc"
    "Support/VectorMath.cc"
  )
target_link_libraries("smdl-doctest" PRIVATE "SpectralMDL::SpectralMDL")
# So test sources at any depth can include "Fixtures.h" and
# "CompileFixtures.h" unqualified, which is how they reach the shared
# vocabulary; 'Fixtures.h' includes 'doctest.h' and is what a test file
# includes instead of it.
target_include_directories("smdl-doctest" PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
if(RTTI_FLAG)
  target_compile_options("smdl-doctest" PRIVATE "${RTTI_FLAG}")
endif()
if(CXX_FP_FLAGS)
  target_compile_options("smdl-doctest" PRIVATE ${CXX_FP_FLAGS})
endif()
smdl_shorten_file_macro("smdl-doctest" "${PROJECT_SOURCE_DIR}/testing")
set_target_properties(
  "smdl-doctest"
  PROPERTIES
    CXX_STANDARD 17
    CXX_STANDARD_REQUIRED YES
    CXX_VISIBILITY_PRESET hidden
    VISIBILITY_INLINES_HIDDEN ON
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin"
  )

# One seed per configure. The suite runs in a random order ('-ob=rand'
# below, which is what makes '-rs' mean anything), so that a case which
# only passes after another case ran shows up here instead of somewhere
# else; the CTest log prints the seed to reproduce it with. Every test
# that installs process-wide state puts it back, which is what makes a
# random order safe.
#
# CTest runs the suite with 'SMDL_DEFAULT_SEARCH_DIRS' cleared: those
# directories belong to the developer's shell, and what a test resolves
# must not depend on them.
string(RANDOM LENGTH 8 ALPHABET "0123456789" SEED)
add_test(
  NAME "smdl-doctest"
  COMMAND $<TARGET_FILE:smdl-doctest> -ob=rand -rs=${SEED}
  )
set_tests_properties(
  "smdl-doctest"
  PROPERTIES
    ENVIRONMENT "SMDL_DEFAULT_SEARCH_DIRS="
  )
