# The renderer suite: the test for 'programs/smdl-toy/X/Y.cc' lives at
# 'X/Y.cc' here, over the renderer's own objects.
smdl_add_doctest(
  "smdl-toy-doctest"
  SOURCES
    "Color.cc"
    "IO/MeshDeform.cc"
    "IO/PlacesFile.cc"
    "Layout/Layout.cc"
    "Layout/LayoutParser.cc"
    "Render/Camera.cc"
    "Render/Light.cc"
    "Render/LightTree.cc"
    "Render/Manifold.cc"
    "Scene/Primitive.cc"
    "Scene/Scene.cc"
  LINK
    "smdl-toy-objects"
  )
# The fixtures the suite's tests share ('RigFixtures.h') live beside this
# file, so it is on the include path.
target_include_directories("smdl-toy-doctest" PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
