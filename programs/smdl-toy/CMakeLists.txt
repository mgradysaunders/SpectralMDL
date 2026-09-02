smdl_add_program(
  "smdl-toy"
  SOURCES
    "Asset.cc"
    "Autolook.cc"
    "Camera.cc"
    "Curves.cc"
    "CurvesFile.cc"
    "Guiding.cc"
    "Layout.cc"
    "LayoutDiagnostics.cc"
    "LayoutParser.cc"
    "LayoutTables.cc"
    "Light.cc"
    "main.cc"
    "Manifold.cc"
    "Medium.cc"
    "MeshImport.cc"
    "PathTracing.cc"
    "PlacesFile.cc"
    "Primitive.cc"
    "Progress.cc"
    "Scene.cc"
    "Subdivide.cc"
    "Tonemap.cc"
  DEPENDS
    "embree"
    "assimp"
    "osd_static_cpu"
  MORE_INCLUDES
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
