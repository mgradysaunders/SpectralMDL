add_executable(smdl "main.cc")
set_target_properties(
  smdl
  PROPERTIES
    CXX_STANDARD 17
    CXX_STANDARD_REQUIRED YES
    LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib"
    ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib"
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin"
  )
target_link_libraries(
  smdl
  PRIVATE
    "SpectralMDL"
    ${SMDL_LLVM_TARGETS}
  )
target_include_directories(
  smdl
  PRIVATE 
    "${LLVM_INCLUDE_DIR}"
  )
