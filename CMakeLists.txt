# The floor is 3.21, for 'PROJECT_IS_TOP_LEVEL'. Fetching the optional
# dependencies needs 3.28, enforced in 'Dependencies.cmake'.
cmake_minimum_required(VERSION 3.21...3.28)
set(SMDL_VERSION_MAJOR 1 CACHE INTERNAL "")
set(SMDL_VERSION_MINOR 0 CACHE INTERNAL "")
set(SMDL_VERSION_PATCH 0 CACHE INTERNAL "")
set(SMDL_VERSION ${SMDL_VERSION_MAJOR}.${SMDL_VERSION_MINOR}.${SMDL_VERSION_PATCH} CACHE INTERNAL "")
project(
  SpectralMDL
  VERSION ${SMDL_VERSION}
  DESCRIPTION "Spectral MDL"
  LANGUAGES CXX C
  )

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

include(CTest)
include(cmake/Git.cmake)
include(cmake/Options.cmake)
include(cmake/NativeArch.cmake)
include(cmake/Dependencies.cmake)

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release) 
endif()

# The RTTI setting must agree with how LLVM itself was built. MSVC spells
# this '/GR-' and does not understand '-fno-rtti'.
if(NOT SMDL_ENABLE_RTTI)
  if(MSVC)
    set(RTTI_FLAG "/GR-")
  else()
    set(RTTI_FLAG "-fno-rtti")
  endif()
else()
  set(RTTI_FLAG "")
endif()

# MSVC needs to be told two things it cannot work out on its own: '/utf-8',
# because the sources are UTF-8 and a few doc comments are not ASCII (MSVC
# otherwise reads them in the active code page and warns C4819), and
# '/bigobj', because the LLVM-heavy translation units under 'lib' blow past
# the default object-file section limit. Set here rather than per-target so
# 'programs' and 'testing' inherit them. NOTE: This is deliberately after
# 'Dependencies.cmake', which is where in-tree LLVM, Ptex, Embree, and
# Assimp are added, so their builds are not affected.
if(MSVC)
  add_compile_options("/utf-8" "/bigobj")
endif()

if(SMDL_SANITIZE)
  add_compile_options(-fsanitize=${SMDL_SANITIZE} -fno-omit-frame-pointer)
  add_link_options(-fsanitize=${SMDL_SANITIZE})
  if(SMDL_SANITIZE MATCHES "undefined")
    # JIT-compiled functions have no UBSAN signature prologue, so indirect-call
    # checks on calls into JIT'd code could fault or false-positive.
    add_compile_options(-fno-sanitize=function)
  endif()
endif()

# Include GNUInstallDirs unconditionally: the directory variables it defines are
# baked into the targets' INSTALL_INTERFACE regardless of whether SMDL itself
# generates any install rules.
include(GNUInstallDirs)

# Install. Everything below is conditional on SMDL_INSTALL, which defaults OFF
# when SMDL is a subproject. The install rules in 'lib' and 'programs' are
# guarded by the same option, and must stay in agreement with the
# install(EXPORT) here: a target added to the export set by install(TARGETS)
# without a matching install(EXPORT) is an error at generate time.
if(SMDL_INSTALL)
  message(STATUS "INSTALL_PREFIX: ${CMAKE_INSTALL_PREFIX}")
  message(STATUS "INSTALL_INCLUDEDIR: ${CMAKE_INSTALL_INCLUDEDIR}")
  message(STATUS "INSTALL_LIBDIR: ${CMAKE_INSTALL_LIBDIR}")
  message(STATUS "INSTALL_BINDIR: ${CMAKE_INSTALL_BINDIR}")

  include(CMakePackageConfigHelpers)
  write_basic_package_version_file(
    "${PROJECT_NAME}ConfigVersion.cmake"
    VERSION "${PROJECT_VERSION}"
    COMPATIBILITY "ExactVersion"
  )
  set(INCLUDE_DIR include)
  configure_package_config_file(
    "${PROJECT_SOURCE_DIR}/cmake/${PROJECT_NAME}Config.cmake.in"
    "${PROJECT_BINARY_DIR}/${PROJECT_NAME}Config.cmake"
    PATH_VARS INCLUDE_DIR
    INSTALL_DESTINATION "${CMAKE_INSTALL_DATAROOTDIR}/${PROJECT_NAME}/cmake"
  )
  install(
    FILES
      "${PROJECT_BINARY_DIR}/${PROJECT_NAME}Config.cmake"
      "${PROJECT_BINARY_DIR}/${PROJECT_NAME}ConfigVersion.cmake"
    DESTINATION
      "${CMAKE_INSTALL_DATAROOTDIR}/${PROJECT_NAME}/cmake"
  )

  # Install the include directory, containing all of the header files for the project.
  install(
    DIRECTORY "${PROJECT_SOURCE_DIR}/include/"
    DESTINATION "include"
  )

  # Install the targets. Note: At this point in the CMake execution, no targets have actually been
  # defined yet. By the time CMake has descended into all of the lib directories the targets will
  # all be populated.
  install(
    EXPORT      "${PROJECT_NAME}_Targets"
    FILE        "${PROJECT_NAME}Targets.cmake"
    NAMESPACE   "${PROJECT_NAME}::"
    DESTINATION "${CMAKE_INSTALL_DATAROOTDIR}/${PROJECT_NAME}/cmake"
  )
endif()

add_subdirectory(lib)
add_subdirectory(programs)
add_subdirectory(testing)
