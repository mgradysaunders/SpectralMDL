# The renderer suite: the test for 'programs/smdl-toy/X/Y.cc' lives at
# 'X/Y.cc' here, over the renderer's own objects.
smdl_add_doctest(
  "smdl-toy-doctest"
  SOURCES
    "IO/PlacesFile.cc"
    "Layout/Layout.cc"
    "Layout/LayoutParser.cc"
    "Render/Light.cc"
    "Render/Manifold.cc"
    "Scene/Primitive.cc"
  LINK
    "smdl-toy-objects"
  )
