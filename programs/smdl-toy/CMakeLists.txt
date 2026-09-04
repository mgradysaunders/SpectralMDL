smdl_add_program(
  "smdl-toy"
  SOURCES
    "main.cc"
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
  DEPENDS
    "embree"
    "assimp"
    "osd_static_cpu"
  MORE_INCLUDES
    # Includes are spelled from this directory ('Scene/Scene.h').
    "${CMAKE_CURRENT_SOURCE_DIR}"
    # OpenSubdiv's static library does not export its include directory.
    "${opensubdiv_SOURCE_DIR}"
  )
# The TARGET check matters: 'smdl_add_program' skips the executable
# entirely when a dependency is missing.
if(SMDL_CXX_NATIVE AND TARGET smdl-toy)
  target_compile_options(
    "smdl-toy"
    PRIVATE
      "-march=native"
      "-mtune=native"
    )
endif()
if(CXX_ARCH_FLAGS AND TARGET smdl-toy)
  target_compile_options(smdl-toy PRIVATE ${CXX_ARCH_FLAGS})
endif()
