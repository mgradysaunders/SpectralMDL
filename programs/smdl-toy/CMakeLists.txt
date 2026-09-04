# Everything but 'main.cc' is an object library, so that the program and
# the 'smdl-toy-doctest' suite link the very same objects: the tests
# exercise the renderer's own translation units rather than a second
# compile of them. The dependencies, the include directories, and the
# ISA flags are PUBLIC so that whatever links the objects compiles and
# links compatibly with them.
#
# The TARGET checks mirror the ones 'smdl_add_program' makes: a missing
# dependency skips the program rather than failing the build.
set(SMDL_TOY_DEPENDS "embree" "assimp" "osd_static_cpu")
set(MissingTargets)
foreach(DEPEND ${SMDL_TOY_DEPENDS})
  if(NOT TARGET ${DEPEND})
    list(APPEND MissingTargets "${DEPEND}")
  endif()
endforeach()
if(MissingTargets)
  message(WARNING "Skipping program \"smdl-toy\" due to missing targets: ${MissingTargets}")
  return()
endif()

add_library("smdl-toy-objects" OBJECT)
target_sources(
  "smdl-toy-objects"
  PRIVATE
    "Progress.cc"
    "Tonemap.cc"
    "IO/AssetFile.cc"
    "IO/CurvesFile.cc"
    "IO/MeshImport.cc"
    "IO/PlacesFile.cc"
    "Layout/Layout.cc"
    "Layout/LayoutDiagnostics.cc"
    "Layout/LayoutParser.cc"
    "Layout/LayoutTables.cc"
    "Scene/Curves.cc"
    "Scene/Primitive.cc"
    "Scene/Scene.cc"
    "Scene/Subdivide.cc"
    "Render/Autolook.cc"
    "Render/Camera.cc"
    "Render/Guiding.cc"
    "Render/Light.cc"
    "Render/Manifold.cc"
    "Render/Medium.cc"
    "Render/PathTracing.cc"
  )
set_target_properties(
  "smdl-toy-objects"
  PROPERTIES
    CXX_STANDARD 17
    CXX_STANDARD_REQUIRED YES
  )
if(RTTI_FLAG)
  target_compile_options("smdl-toy-objects" PRIVATE "${RTTI_FLAG}")
endif()
# LLVMSupport is for command line parsing and JSON output only; all of
# the code generation lives inside SpectralMDL, which links the native
# backend privately.
target_link_libraries(
  "smdl-toy-objects"
  PUBLIC
    "SpectralMDL"
    "LLVMSupport"
    ${SMDL_TOY_DEPENDS}
  )
target_include_directories(
  "smdl-toy-objects"
  PUBLIC
    # Includes are spelled from this directory ('Scene/Scene.h').
    "${CMAKE_CURRENT_SOURCE_DIR}"
    # OpenSubdiv's static library does not export its include directory.
    "${opensubdiv_SOURCE_DIR}"
    "${LLVM_INCLUDE_DIR}"
  )
if(SMDL_CXX_NATIVE)
  target_compile_options(
    "smdl-toy-objects"
    PUBLIC
      "-march=native"
      "-mtune=native"
    )
endif()
if(CXX_ARCH_FLAGS)
  target_compile_options("smdl-toy-objects" PUBLIC ${CXX_ARCH_FLAGS})
endif()

smdl_add_program(
  "smdl-toy"
  SOURCES
    "main.cc"
  DEPENDS
    "smdl-toy-objects"
  )
