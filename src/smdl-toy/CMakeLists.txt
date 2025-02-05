add_executable(smdl-toy "main.cc" "Scene.cc")
set_target_properties(
  smdl-toy
  PROPERTIES
    CXX_STANDARD 17
    CXX_STANDARD_REQUIRED YES
    LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib"
    ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib"
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin"
  )
target_link_libraries(
  smdl-toy
  PUBLIC
    "SpectralMDL"
    "tbb"
    "embree"
    "assimp"
  )
target_include_directories(
  smdl-toy
  PRIVATE 
    "${LLVM_INCLUDE_DIR}"
  )
