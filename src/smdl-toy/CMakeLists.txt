add_executable(
  smdl-toy 
  "BDPTIntegrator.cc"
  "Integrator.cc"
  "MLTIntegrator.cc"
  "PathIntegrator.cc"
  "Scene.cc"
  "Vertex.cc"
  "main.cc"
  )
set_target_properties(
  smdl-toy
  PROPERTIES
    CXX_STANDARD 17
    CXX_STANDARD_REQUIRED YES
    LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib"
    ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib"
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin"
  )
target_compile_options(
  smdl-toy
  PRIVATE 
    "${RTTI_FLAG}"
  )
target_link_libraries(
  smdl-toy
  PRIVATE
    "SpectralMDL"
    "embree"
    "assimp"
    ${SMDL_LLVM_TARGETS}
  )
target_include_directories(
  smdl-toy
  PRIVATE 
    "${CMAKE_CURRENT_SOURCE_DIR}/pcg-cpp"
    "${LLVM_INCLUDE_DIR}"
  )
