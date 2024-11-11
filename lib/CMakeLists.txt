#add_custom_target(
#  SpectralMDL_builtins
#  DEPENDS "${CMAKE_CURRENT_LIST_DIR}/dummy_that_does_not_exist.h"
#  )
#add_custom_command(
#  OUTPUT
#    "${CMAKE_CURRENT_LIST_DIR}/dummy_that_does_not_exist.h"
#    "${CMAKE_CURRENT_LIST_DIR}/builtins.h"
#  COMMAND "./generate_builtins"
#  WORKING_DIRECTORY ${CMAKE_CURRENT_LIST_DIR}
#  )
add_library(
  SpectralMDL 
  SHARED
    "AST.cc"
    "Compiler/Context.cc"
    "Compiler/Emitter.cc"
    "Compiler/Function.cc"
    "Compiler/Module.cc"
    "Compiler/Type.cc"
    "Compiler/Value.cc"
    "FileLocator.cc"
    "MDLInstance.cc"
    "Parser.cc"
    "foreign.cc"
    "llvm.cc"
    "type.cc"
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
    CXX_STANDARD 20
    CXX_STANDARD_REQUIRED YES
    OUTPUT_NAME "SpectralMDL"
    VERSION "${PROJECT_VERSION}"
    SOVERSION "${PROJECT_VERSION_MAJOR}.${PROJECT_VERSION_MINOR}"
    LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib"
    ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib"
  )
target_compile_options(
  SpectralMDL
  PRIVATE 
    "$<$<CONFIG:RELEASE>:-O3;-DNDEBUG>"
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
target_link_libraries(
  SpectralMDL
  PUBLIC
    "LLVMTarget"
    "LLVMXCoreCodeGen" 
    "LLVMX86CodeGen"
    "LLVMOrcJIT"
  )
if(TARGET Ptex_static)
  target_link_libraries(SpectralMDL PRIVATE Ptex_static)
  target_include_directories(SpectralMDL PRIVATE "${ptex_SOURCE_DIR}/src/ptex")
  target_compile_definitions(SpectralMDL PRIVATE "-DWITH_PTEX=1")
endif()
add_library(${PROJECT_NAME}::SpectralMDL ALIAS SpectralMDL)
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

add_library(
  SpectralMDL_ImageLoader
  SHARED
    "ImageLoader.cc"
  )
set_target_properties(
  SpectralMDL_ImageLoader
  PROPERTIES
    CXX_STANDARD 20
    CXX_STANDARD_REQUIRED YES
    OUTPUT_NAME "SpectralMDL_ImageLoader"
    VERSION "${PROJECT_VERSION}"
    SOVERSION "${PROJECT_VERSION_MAJOR}.${PROJECT_VERSION_MINOR}"
    LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib"
    ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib"
  )
target_link_libraries(SpectralMDL_ImageLoader PRIVATE SpectralMDL)
install(
  TARGETS "SpectralMDL_ImageLoader"
  EXPORT "${PROJECT_NAME}_Targets"
  ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}"
  LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}"
  RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}"
  COMPONENT "SpectralMDL_ImageLoader"
  )
add_library(${PROJECT_NAME}::SpectralMDL_ImageLoader ALIAS SpectralMDL_ImageLoader)
