find_package(LLVM REQUIRED)
message(STATUS "LLVM_INCLUDE_DIR: ${LLVM_INCLUDE_DIR}")

include(FetchContent)
set(FETCHCONTENT_QUIET FALSE)

if(SMDL_ENABLE_PTEX)
  FetchContent_Declare(
    "Ptex"
    GIT_REPOSITORY https://github.com/wdas/ptex
    GIT_TAG v2.4.3
    GIT_PROGRESS TRUE
  )
  FetchContent_MakeAvailable("Ptex")
endif()

# if(SMDL_ENABLE_MATERIALX)
#   FetchContent_Declare(
#     "MaterialX"
#     GIT_REPOSITORY https://github.com/AcademySoftwareFoundation/MaterialX
#     GIT_TAG v1.39.1
#     GIT_PROGRESS TRUE
#   )
#   FetchContent_MakeAvailable("MaterialX")
# endif()

if(SMDL_BUILD_TOY)
  # Assimp
  FetchContent_Declare(
    "Assimp"
    GIT_REPOSITORY https://github.com/assimp/assimp
    GIT_TAG v5.4.3
    GIT_PROGRESS TRUE
  )
  set(ASSIMP_NO_EXPORT ON CACHE INTERNAL "")
  set(ASSIMP_BUILD_SAMPLES OFF CACHE INTERNAL "")
  set(ASSIMP_BUILD_TESTS OFF CACHE INTERNAL "")
  set(ASSIMP_BUILD_ZLIB ON CACHE INTERNAL "")
  set(ASSIMP_INSTALL OFF CACHE INTERNAL "")
  set(ASSIMP_WARNINGS_AS_ERRORS OFF CACHE INTERNAL "")
  FetchContent_MakeAvailable("Assimp")

  # TBB
  FetchContent_Declare(
    "TBB"
    GIT_REPOSITORY https://github.com/uxlfoundation/oneTBB
    GIT_TAG v2022.1.0
    GIT_PROGRESS TRUE
  )
  set(TBB_STRICT OFF CACHE INTERNAL "")
  set(TBB_TEST OFF CACHE INTERNAL "")
  FetchContent_MakeAvailable("TBB")

  # Embree
  FetchContent_Declare(
    "Embree"
    GIT_REPOSITORY https://github.com/RenderKit/embree
    GIT_TAG v4.4.0
    GIT_PROGRESS TRUE
  )
  set(EMBREE_TUTORIALS OFF CACHE INTERNAL "")
  set(EMBREE_ISPC_SUPPORT OFF CACHE INTERNAL "")
  set(EMBREE_TASKING_SYSTEM OFF CACHE INTERNAL "")
  FetchContent_MakeAvailable("Embree")
endif()
