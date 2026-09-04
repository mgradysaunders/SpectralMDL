# The renderer suite: the test for 'programs/smdl-toy/X/Y.cc' lives at
# 'X/Y.cc' here, over the renderer's own objects.
smdl_add_doctest(
  "smdl-toy-doctest"
  SOURCES
    "IO/PlacesFile.cc"
    "Layout/LayoutParser.cc"
    "Render/Manifold.cc"
  LINK
    "smdl-toy-objects"
  )
