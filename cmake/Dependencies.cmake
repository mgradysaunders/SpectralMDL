include_guard(GLOBAL)

set(CMAKE_POSITION_INDEPENDENT_CODE ON CACHE BOOL "" FORCE)

if(SMDL_BUILD_LLVM)
  execute_process(
    COMMAND
      "git"
      "clone"
      "--depth=1"
      "--branch=llvmorg-20.1.3"
      "https://github.com/llvm/llvm-project"
      "llvm-project"
    WORKING_DIRECTORY "${CMAKE_BINARY_DIR}"
    )
  set(LLVM_ENABLE_PROJECTS "llvm" CACHE STRING "" FORCE)
  set(LLVM_ENABLE_PIC ON CACHE BOOL "" FORCE)
  set(LLVM_ENABLE_RTTI ${SMDL_ENABLE_RTTI} CACHE BOOL "" FORCE)
  set(LLVM_ENABLE_BINDINGS OFF CACHE BOOL "" FORCE)
  set(LLVM_BUILD_EXAMPLES OFF CACHE BOOL "" FORCE)
  set(LLVM_BUILD_TESTS OFF CACHE BOOL "" FORCE)
  set(LLVM_BUILD_TOOLS OFF CACHE BOOL "" FORCE)
  set(LLVM_ENABLE_ZLIB OFF CACHE BOOL "" FORCE)
  set(LLVM_ENABLE_ZSTD OFF CACHE BOOL "" FORCE)
  set(LLVM_INCLUDE_BENCHMARKS OFF CACHE BOOL "" FORCE)
  set(LLVM_INSTALL_TOOLCHAIN_ONLY ON CACHE BOOL "" FORCE)
  set(LLVM_TARGETS_TO_BUILD "AArch64;ARM;X86;XCore" CACHE STRING "" FORCE)
  add_subdirectory(
    "${CMAKE_BINARY_DIR}/llvm-project/llvm" 
    "${CMAKE_BINARY_DIR}/llvm-project-build"
    )
  set(
    LLVM_INCLUDE_DIR 
    "${CMAKE_BINARY_DIR}/llvm-project/llvm/include"
    "${CMAKE_BINARY_DIR}/llvm-project-build/include"
    )
else()
  find_package(LLVM REQUIRED)
endif()
message(STATUS "LLVM_INCLUDE_DIR: ${LLVM_INCLUDE_DIR}")
set(
  SMDL_LLVM_TARGETS 
  "LLVMTarget"
  "LLVMAArch64CodeGen"
  "LLVMARMCodeGen"
  "LLVMXCoreCodeGen" 
  "LLVMX86CodeGen"
  "LLVMOrcJIT"
  )

include(FetchContent)
set(FETCHCONTENT_QUIET FALSE)

if(SMDL_ENABLE_PTEX)
  FetchContent_Declare(
    "Ptex"
    GIT_REPOSITORY "https://github.com/wdas/ptex"
    GIT_TAG "v2.4.3"
    GIT_SHALLOW TRUE
    GIT_PROGRESS TRUE
    )
  FetchContent_MakeAvailable("Ptex")
endif()

if(SMDL_TOY)
  # Assimp
  FetchContent_Declare(
    "Assimp"
    GIT_REPOSITORY "https://github.com/assimp/assimp"
    GIT_TAG "v5.4.3"
    GIT_SHALLOW TRUE
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
    GIT_REPOSITORY "https://github.com/uxlfoundation/oneTBB"
    GIT_TAG "v2022.1.0"
    GIT_SHALLOW TRUE
    GIT_PROGRESS TRUE
    )
  set(TBB_STRICT OFF CACHE INTERNAL "")
  set(TBB_TEST OFF CACHE INTERNAL "")
  FetchContent_MakeAvailable("TBB")

  # Embree
  FetchContent_Declare(
    "Embree"
    GIT_REPOSITORY "https://github.com/RenderKit/embree"
    GIT_TAG "v4.4.0"
    GIT_SHALLOW TRUE
    GIT_PROGRESS TRUE
    )
  set(EMBREE_TUTORIALS OFF CACHE INTERNAL "")
  set(EMBREE_ISPC_SUPPORT OFF CACHE INTERNAL "")
  set(EMBREE_TASKING_SYSTEM OFF CACHE INTERNAL "")
  FetchContent_MakeAvailable("Embree")
endif()
