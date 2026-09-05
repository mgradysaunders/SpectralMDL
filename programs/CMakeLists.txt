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
    # The installed programs sit in '<prefix>/bin' and load 'libSpectralMDL'
    # out of '<prefix>/lib', so they need an RPATH relative to their own
    # location. Without it an installed binary only resolves the library by
    # accident, through whatever the build tree happened to bake in.
    if(APPLE)
      set(RPATH_ORIGIN "@loader_path")
    else()
      set(RPATH_ORIGIN "$ORIGIN")
    endif()
    set_target_properties(
      ${BIN_NAME}
      PROPERTIES
        CXX_STANDARD 17
        CXX_STANDARD_REQUIRED YES
        LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib"
        ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib"
        RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin"
        INSTALL_RPATH "${RPATH_ORIGIN}/../${CMAKE_INSTALL_LIBDIR}"
      )
    target_sources(${BIN_NAME} PRIVATE "${Args_SOURCES}")
    target_compile_options(${BIN_NAME} PRIVATE "${RTTI_FLAG}")
    if(UNIX)
      target_compile_options(${BIN_NAME} PRIVATE "-fmacro-prefix-map=${PROJECT_SOURCE_DIR}=.")
    endif()
    # The programs use LLVM only for command line parsing and diagnostics
    # ('llvm/Support/...'), so they want LLVMSupport and nothing else. All of
    # the code generation lives inside SpectralMDL itself, which links the
    # native backend privately.
    target_link_libraries(${BIN_NAME} PRIVATE "SpectralMDL" "LLVMSupport")
    target_include_directories(${BIN_NAME} PRIVATE "${LLVM_INCLUDE_DIR}")
    if(Args_DEPENDS)
      foreach(DEPEND ${Args_DEPENDS})
        target_link_libraries(${BIN_NAME} PRIVATE ${DEPEND})
      endforeach(DEPEND)
    endif()
    if(Args_MORE_INCLUDES)
      foreach(INCLUDE_DIR ${Args_MORE_INCLUDES})
        target_include_directories(${BIN_NAME} PRIVATE ${INCLUDE_DIR})
      endforeach()
    endif()
    if(SMDL_INSTALL)
      install(
        TARGETS "${BIN_NAME}"
        EXPORT "${PROJECT_NAME}_Targets"
        ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}"
        LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}"
        RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}"
        )
    endif()
  else()
    message(WARNING "Skipping program \"${BIN_NAME}\" due to missing targets: ${MissingTargets}")
  endif()
endmacro()

add_subdirectory(smdl)
if(SMDL_TOY)
add_subdirectory(smdl-toy)
endif()
