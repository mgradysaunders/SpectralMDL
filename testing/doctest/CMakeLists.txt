string(RANDOM LENGTH 8 ALPHABET "012345679" SEED)
add_executable("smdl-doctest")
# Test sources mirror the layout of 'lib/': the test for 'lib/X/Y.cc'
# lives at 'X/Y.cc' here. The 'doctest.*' files at the root are the
# vendored framework and runner, not tests of a lib file.
target_sources(
  "smdl-doctest"
  PRIVATE
    "doctest.cc"
    "BSDFMeasurement.cc"
    "Compiler.cc"
    "Doc.cc"
    "FileLocator.cc"
    "Image.cc"
    "LightProfile.cc"
    "Module.cc"
    "Parser.cc"
    "Support/Illuminant.cc"
    "Support/MD5Hash.cc"
    "Support/MetalIOR.cc"
    "Support/MonteCarlo.cc"
    "Support/QualifiedName.cc"
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
