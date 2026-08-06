include_guard(GLOBAL)

# Stamp the build with the branch and commit that produced it. This must never
# be fatal: the source tree may legitimately have no git history at all (release
# tarballs, vendored copies, container builds that copy sources without '.git'),
# and git itself may refuse to answer even inside a real repository when the
# checkout is owned by another user ('detected dubious ownership'). In any of
# those cases we fall back to "unknown" and carry on.
#
# WORKING_DIRECTORY matters. Without it, 'git' runs in the current binary
# directory and walks up from there, so a downstream project that pulls SMDL in
# with FetchContent would stamp SMDL with *its own* branch and commit.
set(SMDL_GIT_BRANCH "unknown")
set(SMDL_GIT_COMMIT "unknown")
find_package(Git QUIET)
if(Git_FOUND)
  execute_process(
    COMMAND "${GIT_EXECUTABLE}" rev-parse --abbrev-ref HEAD
    WORKING_DIRECTORY "${CMAKE_CURRENT_LIST_DIR}"
    OUTPUT_VARIABLE _SMDL_GIT_BRANCH
    OUTPUT_STRIP_TRAILING_WHITESPACE
    ERROR_QUIET
    RESULT_VARIABLE _SMDL_GIT_RESULT
    )
  if(_SMDL_GIT_RESULT EQUAL 0 AND _SMDL_GIT_BRANCH)
    set(SMDL_GIT_BRANCH "${_SMDL_GIT_BRANCH}")
  endif()
  execute_process(
    COMMAND "${GIT_EXECUTABLE}" rev-parse --short=7 HEAD
    WORKING_DIRECTORY "${CMAKE_CURRENT_LIST_DIR}"
    OUTPUT_VARIABLE _SMDL_GIT_COMMIT
    OUTPUT_STRIP_TRAILING_WHITESPACE
    ERROR_QUIET
    RESULT_VARIABLE _SMDL_GIT_RESULT
    )
  if(_SMDL_GIT_RESULT EQUAL 0 AND _SMDL_GIT_COMMIT)
    set(SMDL_GIT_COMMIT "${_SMDL_GIT_COMMIT}")
  endif()
  unset(_SMDL_GIT_BRANCH)
  unset(_SMDL_GIT_COMMIT)
  unset(_SMDL_GIT_RESULT)
endif()
message(STATUS "SMDL_GIT_BRANCH=${SMDL_GIT_BRANCH}")
message(STATUS "SMDL_GIT_COMMIT=${SMDL_GIT_COMMIT}")
