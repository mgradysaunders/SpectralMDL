# The renderer suite: the test for 'programs/smdl-toy/X/Y.cc' lives at
# 'X/Y.cc' here, over the renderer's own objects.
smdl_add_doctest(
  "smdl-toy-doctest"
  SOURCES
    "CameraModel.cc"
    "Color.cc"
    "MedianFilter.cc"
    "IO/CurvesFile.cc"
    "IO/MeshDeform.cc"
    "IO/PlacesFile.cc"
    "IO/RenderHeader.cc"
    "Layout/CameraFile.cc"
    "Layout/Layout.cc"
    "Layout/LayoutParser.cc"
    "Layout/LensFile.cc"
    "Layout/Motion.cc"
    "Layout/SensorFile.cc"
    "Render/Autolook.cc"
    "Render/Camera.cc"
    "Render/Lens.cc"
    "Render/Light.cc"
    "Render/LightTree.cc"
    "Render/Manifold.cc"
    "Render/Medium.cc"
    "Render/PathStats.cc"
    "Scene/Primitive.cc"
    "Scene/Scene.cc"
    "Sensor/Colorimetry.cc"
    "Sensor/Detector.cc"
    "Sensor/Develop.cc"
    "Sensor/Response.cc"
    "Sensor/Sensor.cc"
  LINK
    "smdl-toy-objects"
  )
# The fixtures the suite's tests share ('RenderFixtures.h' and
# 'RigFixtures.h') live beside this file, so it is on the include path.
target_include_directories("smdl-toy-doctest" PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
