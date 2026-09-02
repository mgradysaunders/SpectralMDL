if(CMAKE_PROJECT_NAME STREQUAL PROJECT_NAME AND BUILD_TESTING)
  add_subdirectory(doctest)
  # The SMDL-language suite: 'smdl test' treats the directory as a
  # search root, which is required for the relative package imports in
  # the suite (per-file invocation would break them), and exits nonzero
  # on any failing 'unit_test'.
  add_test(
    NAME "smdl-language"
    COMMAND $<TARGET_FILE:smdl> test "${CMAKE_CURRENT_SOURCE_DIR}/language"
    )
  # The smdl-toy manifold walk test: the connection measure against the
  # flat-mirror analytic value and against finite differences of the
  # solved receiver direction, on scenes the test builds itself; exits
  # nonzero on any failing check.
  if(SMDL_TOY)
    add_test(
      NAME "smdl-toy-manifold"
      COMMAND $<TARGET_FILE:smdl-toy> -mnee-test-manifoldwalk
      )
  endif()
endif()
