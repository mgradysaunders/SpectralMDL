# Two suites, one framework. 'doctest.h' and 'doctest.cc' at this level
# are the vendored framework and its runner, built once as an object
# library that every suite links, so the suites share one 'main()' and
# one copy of the framework's implementation. Each suite is a
# subdirectory named for the executable it tests, laid out to mirror
# that executable's sources: 'smdl/' mirrors 'lib/', and 'smdl-toy/'
# mirrors 'programs/smdl-toy/'.
add_library("doctest-runner" OBJECT "doctest.cc")
# So test sources at any depth can '#include "Fixtures.h"' unqualified,
# which is how both suites reach the vocabulary they share; 'Fixtures.h'
# includes 'doctest.h' and is what a test file includes instead of it.
target_include_directories(
  "doctest-runner"
  PUBLIC
    "${CMAKE_CURRENT_SOURCE_DIR}"
  )
set_target_properties(
  "doctest-runner"
  PROPERTIES
    CXX_STANDARD 17
    CXX_STANDARD_REQUIRED YES
    CXX_VISIBILITY_PRESET hidden
    VISIBILITY_INLINES_HIDDEN ON
  )
if(RTTI_FLAG)
  target_compile_options("doctest-runner" PRIVATE "${RTTI_FLAG}")
endif()

# One seed per configure, shared by the suites. The suites run in a random
# order ('-ob=rand' below, which is what makes '-rs' mean anything), so
# that a case which only passes after another case ran shows up here
# instead of somewhere else; the CTest log prints the seed to reproduce it
# with. Every test that installs process-wide state puts it back, which is
# what makes a random order safe: see 'smdl-toy/RenderFixtures.h'.
string(RANDOM LENGTH 8 ALPHABET "0123456789" SEED)

# Add one suite: an executable of the given test sources over the
# runner and whatever LINK names (the library, or the renderer's object
# library), registered with CTest under its own name.
#
# The RTTI flag matches the library's, as 'lib' and 'programs' do; the
# 'BuildInfo' test relies on this to check 'hasRTTI' against the test
# binary's own compile. The floating-point flags match too, because the
# suites test inline header code (the spectral color predicates, the
# fast-math approximations) that only means what it means under the
# flags the library and the renderer compile it with.
#
# CTest runs the suite with 'SMDL_DEFAULT_SEARCH_DIRS' cleared: those
# directories belong to the developer's shell, and what a test resolves
# must not depend on them.
function(smdl_add_doctest BIN_NAME)
  set(Options)
  set(OneValArgs)
  set(MultiValArgs SOURCES LINK)
  cmake_parse_arguments(Args "${Options}" "${OneValArgs}" "${MultiValArgs}" ${ARGN})
  add_executable(${BIN_NAME})
  target_sources(${BIN_NAME} PRIVATE ${Args_SOURCES})
  target_link_libraries(${BIN_NAME} PRIVATE "doctest-runner" ${Args_LINK})
  if(RTTI_FLAG)
    target_compile_options(${BIN_NAME} PRIVATE "${RTTI_FLAG}")
  endif()
  if(CXX_FP_FLAGS)
    target_compile_options(${BIN_NAME} PRIVATE ${CXX_FP_FLAGS})
  endif()
  set_target_properties(
    ${BIN_NAME}
    PROPERTIES
      CXX_STANDARD 17
      CXX_STANDARD_REQUIRED YES
      CXX_VISIBILITY_PRESET hidden
      VISIBILITY_INLINES_HIDDEN ON
      RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin"
    )
  add_test(NAME ${BIN_NAME} COMMAND $<TARGET_FILE:${BIN_NAME}> -ob=rand -rs=${SEED})
  set_tests_properties(
    ${BIN_NAME}
    PROPERTIES
      ENVIRONMENT "SMDL_DEFAULT_SEARCH_DIRS="
    )
endfunction()

add_subdirectory(smdl)
# The renderer suite links the renderer's object library, which exists
# only when 'programs' built 'smdl-toy' (see 'SMDL_TOY' and the
# dependency checks in 'programs/smdl-toy/CMakeLists.txt').
if(TARGET smdl-toy-objects)
  add_subdirectory(smdl-toy)
endif()
