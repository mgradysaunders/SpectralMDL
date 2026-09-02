string(RANDOM LENGTH 8 ALPHABET "012345679" SEED)
add_executable("smdl-doctest")
# Test sources mirror the layout of 'lib/': the test for 'lib/X/Y.cc'
# lives at 'X/Y.cc' here. The 'doctest.*' files at the root are the
# vendored framework and runner, not tests of a lib file.
target_sources(
  "smdl-doctest"
  PRIVATE
    "doctest.cc"
    "Common.cc"
    "Compiler.cc"
    "Compiler/Emitter.cc"
    "Doc.cc"
    "Manifold.cc"
    "Module.cc"
    "Parser.cc"
    "RenderUtil/SpectralColor.cc"
    "RenderUtil/FastMath.cc"
    "RenderUtil/Illuminant.cc"
    "RenderUtil/Metal.cc"
    "RenderUtil/MonteCarlo.cc"
    "RenderUtil/PBRMaps.cc"
    "RenderUtil/SpectralFilm.cc"
    "RenderUtil/SunSky.cc"
    "Resource/BSDFMeasurement.cc"
    "Resource/Image.cc"
    "Resource/LightProfile.cc"
    "Resource/SceneData.cc"
    "Resource/VoxelGrid.cc"
    "Support/FileLocator.cc"
    "Support/MD5Hash.cc"
    "Support/QualifiedName.cc"
    "Support/RNG.cc"
    "Support/Strings.cc"
  )
# So nested test sources can '#include "doctest.h"' unqualified.
target_include_directories(
  "smdl-doctest"
  PRIVATE
    "${CMAKE_CURRENT_SOURCE_DIR}"
  )
target_link_libraries(
  "smdl-doctest"
  PRIVATE
    "SpectralMDL::SpectralMDL"
  )
# Match the library's RTTI setting, as 'lib' and 'programs' do; the
# 'BuildInfo' test relies on this to check 'hasRTTI' against this
# binary's own compile.
target_compile_options(
  "smdl-doctest"
  PRIVATE
    "${RTTI_FLAG}"
  )
set_target_properties(
  "smdl-doctest"
  PROPERTIES
    CXX_VISIBILITY_PRESET hidden
    VISIBILITY_INLINES_HIDDEN ON
    POSITION_INDEPENDENT_CODE ON
  )
set_target_properties(
  "smdl-doctest"
  PROPERTIES
    CXX_STANDARD 17
    CXX_STANDARD_REQUIRED YES
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin"
  )
add_test(NAME "smdl-doctest" COMMAND $<TARGET_FILE:smdl-doctest> -rs=${SEED})
