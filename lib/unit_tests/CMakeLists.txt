if(CMAKE_PROJECT_NAME STREQUAL PROJECT_NAME AND BUILD_TESTING)
  string(RANDOM LENGTH 8 ALPHABET "012345679" SEED)
  add_executable("smdl-doctest")
  target_sources(
    "smdl-doctest"
    PRIVATE 
      "doctest.cc" 
      "BSDFMeasurement.cc"
      "LightProfile.cc"
      "MD5Hash.cc"
      "Sampling.cc"
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
endif()
