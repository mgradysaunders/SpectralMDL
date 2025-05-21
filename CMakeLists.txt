cmake_minimum_required(VERSION 3.22)
project(
  SpectralMDL
  VERSION 1.0.0
  DESCRIPTION "Spectral MDL"
  LANGUAGES CXX C
  )

#include(cmake/Git.cmake)
include(cmake/Options.cmake)
include(cmake/Dependencies.cmake)

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release) 
endif()

if(NOT SMDL_ENABLE_RTTI)
  set(RTTI_FLAG "-fno-rtti")
else()
  set(RTTI_FLAG "")
endif()

# Include GNUInstallDirs and print them out.
include(GNUInstallDirs)
message(STATUS "INSTALL_PREFIX: ${CMAKE_INSTALL_PREFIX}")
message(STATUS "INSTALL_INCLUDEDIR: ${CMAKE_INSTALL_INCLUDEDIR}")
message(STATUS "INSTALL_LIBDIR: ${CMAKE_INSTALL_LIBDIR}")
message(STATUS "INSTALL_BINDIR: ${CMAKE_INSTALL_BINDIR}")

# Install.
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

add_subdirectory(lib)
add_subdirectory(src)
