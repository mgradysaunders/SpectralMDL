if(CMAKE_PROJECT_NAME STREQUAL PROJECT_NAME AND BUILD_TESTING)
  add_subdirectory(doctest)
  # The SMDL-language suite: 'smdlc test' treats the directory as a
  # search root, which is required for the relative package imports in
  # the suite (per-file invocation would break them), and exits nonzero
  # on any failing 'unit_test'.
  add_test(
    NAME "smdl-language"
    COMMAND $<TARGET_FILE:smdlc> test "${CMAKE_CURRENT_SOURCE_DIR}/smdl"
    )
endif()
