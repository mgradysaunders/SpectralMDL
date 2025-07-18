add_custom_target(
  builtin_header
  COMMAND "./builtin.h.rb"
  WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/Compiler"
  )
add_custom_target(
  builtin_albedo
  COMMAND "${CMAKE_BINARY_DIR}/bin/smdl test tabulate.smdl"
  WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/Compiler/builtin/albedo"
  )
add_library(
  SpectralMDL 
  SHARED
    "AST.cc"
    "BSDFMeasurement.cc"
    "common.cc"
    "Compiler.cc"
    "Compiler/Context.cc"
    "Compiler/Emitter.cc"
    "Compiler/llvm.cc"
    "Compiler/Type.cc"
    "Compiler/Value.cc"
    "DiscreteDistribution.cc"
    "FileLocator.cc"
    "Formatter.cc"
    "Image.cc"
    "LightProfile.cc"
    "Logger.cc"
    "Module.cc"
    "Parser.cc"
    "SceneData.cc"
    "thirdparty/miniz.c"
  )
add_library(
  ${PROJECT_NAME}::SpectralMDL ALIAS SpectralMDL
  )
set_target_properties(
  SpectralMDL
  PROPERTIES
    CXX_VISIBILITY_PRESET hidden
    VISIBILITY_INLINES_HIDDEN ON
    POSITION_INDEPENDENT_CODE ON
  )
set_target_properties(
  SpectralMDL
  PROPERTIES
    CXX_STANDARD 17
    CXX_STANDARD_REQUIRED YES
    OUTPUT_NAME "SpectralMDL"
    VERSION "${PROJECT_VERSION}"
    SOVERSION "${PROJECT_VERSION_MAJOR}.${PROJECT_VERSION_MINOR}"
    LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib"
    ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib"
  )
if(UNIX)
  target_compile_options(
    SpectralMDL
    PRIVATE 
      "$<$<CONFIG:RELEASE>:-O3;-DNDEBUG>"
      "-Wall"
      "-Wextra"
      "-Wno-unused-parameter"
    )
endif()
target_compile_options(
  SpectralMDL
  PRIVATE 
    "${RTTI_FLAG}"
  )
target_include_directories(
  SpectralMDL
  PUBLIC
    $<BUILD_INTERFACE:${${PROJECT_NAME}_SOURCE_DIR}/include>
    $<BUILD_INTERFACE:${${PROJECT_NAME}_BINARY_DIR}/include>
    $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>
  )
target_include_directories(
  SpectralMDL
  PRIVATE 
    "${LLVM_INCLUDE_DIR}"
    "${CMAKE_CURRENT_LIST_DIR}"
    "${CMAKE_CURRENT_LIST_DIR}/Compiler"
  )
target_compile_definitions(
  SpectralMDL
  PRIVATE
    "-DSMDL_VERSION_MAJOR=${SMDL_VERSION_MAJOR}"
    "-DSMDL_VERSION_MINOR=${SMDL_VERSION_MINOR}"
    "-DSMDL_VERSION_PATCH=${SMDL_VERSION_PATCH}"
    "-DSMDL_GIT_BRANCH=\"${SMDL_GIT_BRANCH}\""
    "-DSMDL_GIT_COMMIT=\"${SMDL_GIT_COMMIT}\""
  )
find_package(Threads REQUIRED)
target_link_libraries(SpectralMDL PRIVATE Threads::Threads ${SMDL_LLVM_TARGETS})

if(SMDL_USE_BOOST_FILESYSTEM)
  find_package(Boost CONFIG COMPONENTS filesystem REQUIRED)
  target_link_libraries(SpectralMDL PRIVATE "Boost::filesystem")
  target_compile_definitions(SpectralMDL PRIVATE "-DSMDL_USE_BOOST_FILESYSTEM=1")
endif()

if(TARGET Ptex_static)
  target_compile_options(Ptex_static PRIVATE "${RTTI_FLAG}")
  target_link_libraries(SpectralMDL PRIVATE Ptex_static)
  target_include_directories(SpectralMDL PRIVATE "${ptex_SOURCE_DIR}/src/ptex")
  target_compile_definitions(SpectralMDL PRIVATE "-DSMDL_HAS_PTEX=1")
endif()

include(GenerateExportHeader)
generate_export_header(
  SpectralMDL
  EXPORT_MACRO_NAME "SMDL_EXPORT"
  EXPORT_FILE_NAME "${PROJECT_BINARY_DIR}/include/smdl/Export.h"
  )
install(
  FILES "${PROJECT_BINARY_DIR}/include/smdl/Export.h"
  DESTINATION "include/smdl"
  )
install(
  TARGETS "SpectralMDL"
  EXPORT "${PROJECT_NAME}_Targets"
  ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}"
  LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}"
  RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}"
  COMPONENT "SpectralMDL"
  ) 

add_subdirectory(unit_tests)
