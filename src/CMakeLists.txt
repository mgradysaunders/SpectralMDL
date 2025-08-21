macro(smdl_add_program BIN_NAME)
  set(Options)
  set(OneValArgs)
  set(MultiValArgs SOURCES DEPENDS MORE_INCLUDES)
  cmake_parse_arguments(Args "${Options}" "${OneValArgs}" "${MultiValArgs}" ${ARGN})
  set(MissingTargets)
  if(Args_DEPENDS)
    foreach(DEPEND ${Args_DEPENDS})
      if(NOT TARGET ${DEPEND})
        list(APPEND MissingTargets "${DEPEND}")
      endif()
    endforeach(DEPEND)
  endif()
  if(NOT MissingTargets)
    add_executable(${BIN_NAME})
    set_target_properties(
      ${BIN_NAME}
      PROPERTIES
        CXX_STANDARD 17
        CXX_STANDARD_REQUIRED YES
        LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib"
        ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib"
        RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin"
      )
    target_sources(${BIN_NAME} PRIVATE "${Args_SOURCES}")
    target_compile_options(${BIN_NAME} PRIVATE "${RTTI_FLAG}")
    target_link_libraries(${BIN_NAME} PRIVATE "SpectralMDL" ${SMDL_LLVM_TARGETS})
    target_include_directories(${BIN_NAME} PRIVATE "${LLVM_INCLUDE_DIR}")
    if(Args_DEPENDS)
      foreach(DEPEND ${Args_DEPENDS})
        target_link_libraries(${BIN_NAME} PRIVATE ${DEPEND})
      endforeach(DEPEND)
    endif()
    if(Args_MORE_INCLUDES)
      foreach(INCLUDE_DIR ${Args_MORE_INCLUDES})
        target_include_directories(${BIN_NAME} PRIVATE ${INCLUDE_DIR})
      endforeach(DEPEND)
    endif()
    target_include_directories(
      ${BIN_NAME} 
      PRIVATE 
        "${CMAKE_SOURCE_DIR}/src/common"
        "${CMAKE_SOURCE_DIR}/src/common/pcg-cpp"
      )
    install(
      TARGETS "${BIN_NAME}"
      EXPORT "${PROJECT_NAME}_Targets"
      ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}"
      LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}"
      RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}"
      )
  else()
    message(WARNING "Skipping program \"${BIN_NAME}\" due to missing targets: ${MissingTargets}")
  endif()
endmacro()

#add_subdirectory(assimp-material-extractor)
add_subdirectory(smdl)
if(SMDL_TOY)
  add_subdirectory(smdl-toy)
endif()
