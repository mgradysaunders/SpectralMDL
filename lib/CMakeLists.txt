# Manual, on-demand regeneration targets. Neither is part of 'all'.
#
# NOTE: Invoke the interpreter explicitly instead of relying on the script's
# shebang and executable bit, which do not exist on Windows.
add_custom_target(
  builtin_header
  COMMAND ruby "Builtin.h.rb"
  WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/Compiler"
  )
# NOTE: Each of these must be a separate COMMAND argument, and the path has
# to come from '$<TARGET_FILE:smdl>' rather than being spelled out: that
# picks up the '.exe' suffix on Windows and the per-configuration output
# subdirectory of multi-config generators, and it makes the target depend on
# 'smdl' automatically.
add_custom_target(
  builtin_albedo
  COMMAND "$<TARGET_FILE:smdl>" test "tabulate.mdl"
  WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/Compiler/Builtin/Albedo"
  )
# The vendored tinyexr v3, a C11 library. zstd and libdeflate are left
# out: tinyexr's own inflate serves ZIP, ZIPS, and PXR24, and ZSTD files
# fail to load as unsupported. The CUDA and Vulkan backends and the
# freestanding support are not vendored at all.
set(
  SMDL_TINYEXR_SOURCES
    "thirdparty/tinyexr/exr_attr.c"
    "thirdparty/tinyexr/exr_b44.c"
    "thirdparty/tinyexr/exr_codec.c"
    "thirdparty/tinyexr/exr_color.c"
    "thirdparty/tinyexr/exr_convert.c"
    "thirdparty/tinyexr/exr_core.c"
    "thirdparty/tinyexr/exr_cpu.c"
    "thirdparty/tinyexr/exr_deep.c"
    "thirdparty/tinyexr/exr_deflate.c"
    "thirdparty/tinyexr/exr_fpnge.c"
    "thirdparty/tinyexr/exr_half.c"
    "thirdparty/tinyexr/exr_jph.c"
    "thirdparty/tinyexr/exr_jph_simd.c"
    "thirdparty/tinyexr/exr_jph_simd_neon.c"
    "thirdparty/tinyexr/exr_mip.c"
    "thirdparty/tinyexr/exr_piz.c"
    "thirdparty/tinyexr/exr_pxr24.c"
    "thirdparty/tinyexr/exr_reader.c"
    "thirdparty/tinyexr/exr_resize.c"
    "thirdparty/tinyexr/exr_rle.c"
    "thirdparty/tinyexr/exr_simd_neon.c"
    "thirdparty/tinyexr/exr_simd_x86.c"
    "thirdparty/tinyexr/exr_spectral.c"
    "thirdparty/tinyexr/exr_stdio.c"
    "thirdparty/tinyexr/exr_thread.c"
    "thirdparty/tinyexr/exr_tonemap.c"
    "thirdparty/tinyexr/exr_util_simd_neon.c"
    "thirdparty/tinyexr/exr_util_simd_x86.c"
    "thirdparty/tinyexr/exr_writer.c"
    "thirdparty/tinyexr/exr_zip.c"
  )
set_source_files_properties(
  ${SMDL_TINYEXR_SOURCES}
  PROPERTIES
    COMPILE_DEFINITIONS "EXR_NO_ZSTD"
  )
# Vendored code, so its warnings are not ours to fix.
if(UNIX)
  set_source_files_properties(
    ${SMDL_TINYEXR_SOURCES} PROPERTIES COMPILE_OPTIONS "-w"
    )
endif()
add_library(
  SpectralMDL 
  SHARED
    "AST.cc"
    "Common.cc"
    "Compiler.cc"
    "Compiler/Context.cc"
    "Compiler/Emitter.cc"
    "Compiler/Intrinsics.cc"
    "Compiler/llvm.cc"
    "Compiler/Type.cc"
    "Compiler/Value.cc"
    "Doc.cc"
    "Formatter.cc"
    "Manifold.cc"
    "Module.cc"
    "Parser.cc"
    "RenderUtil/Colorimetry.cc"
    "RenderUtil/Haze.cc"
    "RenderUtil/Illuminant.cc"
    "RenderUtil/MetalIOR.cc"
    "RenderUtil/MonteCarlo.cc"
    "RenderUtil/OpticalGlass.cc"
    "RenderUtil/SunSky.cc"
    "Resource/BSDFMeasurement.cc"
    "Resource/Image.cc"
    "Resource/LightProfile.cc"
    "Resource/Spectrum.cc"
    "Resource/SpectrumLibrary.cc"
    "Resource/VoxelGrid.cc"
    "SceneData.cc"
    "State.cc"
    "Support/BumpPtrAllocator.cc"
    "Support/Compress.cc"
    "Support/Environment.cc"
    "Support/Error.cc"
    "Support/FileLocator.cc"
    "Support/Filesystem.cc"
    "Support/Logger.cc"
    "Support/MD5Hash.cc"
    "Support/Macros.cc"
    "Support/Parallel.cc"
    "Support/Profiler.cc"
    "Support/QualifiedName.cc"
    "Support/Strings.cc"
    "Support/Terminal.cc"
    "thirdparty/miniz/miniz.c"
    ${SMDL_TINYEXR_SOURCES}
  )
add_library(
  ${PROJECT_NAME}::SpectralMDL ALIAS SpectralMDL
  )
set_target_properties(
  SpectralMDL
  PROPERTIES
    CXX_VISIBILITY_PRESET hidden
    C_VISIBILITY_PRESET hidden
    VISIBILITY_INLINES_HIDDEN ON
    POSITION_INDEPENDENT_CODE ON
  )
set_target_properties(
  SpectralMDL
  PROPERTIES
    CXX_STANDARD 17
    CXX_STANDARD_REQUIRED YES
    C_STANDARD 11
    C_STANDARD_REQUIRED YES
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
smdl_shorten_file_macro(SpectralMDL "${PROJECT_SOURCE_DIR}")
target_compile_options(
  SpectralMDL
  PRIVATE
    "${RTTI_FLAG}"
  )
if(SMDL_CXX_NATIVE)
  target_compile_options(
    SpectralMDL
    PRIVATE
      "-march=native"
      "-mtune=native"
    )
endif()
if(CXX_ARCH_FLAGS)
  target_compile_options(SpectralMDL PRIVATE ${CXX_ARCH_FLAGS})
endif()
if(CXX_FP_FLAGS)
  target_compile_options(SpectralMDL PRIVATE ${CXX_FP_FLAGS})
endif()
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
if(SMDL_DYNAMIC_SCHEDULING)
  target_compile_definitions(
    SpectralMDL
    PRIVATE
      "-DSMDL_DYNAMIC_SCHEDULING=1"
    )
endif()
find_package(Threads REQUIRED)
target_link_libraries(SpectralMDL PRIVATE Threads::Threads ${SMDL_LLVM_TARGETS})

if(TARGET Ptex_static)
  target_compile_options(Ptex_static PRIVATE "${RTTI_FLAG}")
  # Ptex 2.5 calls 'unlink' in 'PtexWriter.cpp' but only includes <unistd.h>
  # on FreeBSD and macOS.
  if(NOT WIN32)
    target_compile_options(Ptex_static PRIVATE "SHELL:-include unistd.h")
  endif()
  target_link_libraries(SpectralMDL PRIVATE Ptex_static)
  target_include_directories(SpectralMDL PRIVATE "${ptex_SOURCE_DIR}/src/ptex")
  if(NOT SMDL_Ptex_TAG)
    # 'Ptex_static' can come from a parent project instead of our own fetch,
    # in which case the pinned tag is unknown.
    set(SMDL_Ptex_TAG "unknown")
  endif()
  target_compile_definitions(
    SpectralMDL
    PRIVATE
      "-DSMDL_HAS_PTEX=1"
      "-DSMDL_PTEX_VERSION=\"${SMDL_Ptex_TAG}\""
    )
endif()

if(nanovdb_SOURCE_DIR)
  # The headers sit in '<openvdb>/nanovdb/nanovdb', so the directory above
  # them is what belongs on the search path.
  target_include_directories(
    SpectralMDL PRIVATE "${nanovdb_SOURCE_DIR}/nanovdb"
    )
  if(NOT SMDL_NanoVDB_TAG)
    set(SMDL_NanoVDB_TAG "unknown")
  endif()
  target_compile_definitions(
    SpectralMDL
    PRIVATE
      "-DSMDL_HAS_NANOVDB=1"
      "-DSMDL_NANOVDB_VERSION=\"${SMDL_NanoVDB_TAG}\""
    )
endif()

include(GenerateExportHeader)
generate_export_header(
  SpectralMDL
  EXPORT_MACRO_NAME "SMDL_EXPORT"
  EXPORT_FILE_NAME "${PROJECT_BINARY_DIR}/include/smdl/Export.h"
  )
if(SMDL_INSTALL)
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
endif()
