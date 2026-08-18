include_guard(GLOBAL)

# Map a processor name onto the name of the LLVM backend that generates code
# for it. This mirrors the table in LLVM's own 'cmake/config-ix.cmake' and
# covers the spellings that CMake actually produces: 'AMD64' on Windows,
# 'amd64' on FreeBSD, 'i86pc' on Solaris, 'arm64' on macOS, and 'ARM64' on
# Windows on ARM.
#
# Backends that cannot host a JIT (WebAssembly, NVPTX, SPIRV, AMDGPU) are
# deliberately absent: mapping them would produce a build that configures
# happily and then fails at run time.
#
# Sets OUT_VAR to the empty string if the processor is not a viable JIT host.
function(smdl_llvm_arch_for_processor OUT_VAR PROCESSOR)
  string(TOLOWER "${PROCESSOR}" Proc)
  # NOTE: 'aarch64|arm64' must be tested before the bare 'arm' prefix!
  if(Proc MATCHES "^(x86_64|amd64|x64|x86|i[3-6]86|i86pc)$")
    set(Arch "X86")
  elseif(Proc MATCHES "^(aarch64|arm64)")
    set(Arch "AArch64")
  elseif(Proc MATCHES "^arm")
    set(Arch "ARM")
  elseif(Proc MATCHES "^(powerpc|ppc)")
    set(Arch "PowerPC")
  elseif(Proc MATCHES "^riscv")
    set(Arch "RISCV")
  elseif(Proc MATCHES "^s390")
    set(Arch "SystemZ")
  elseif(Proc MATCHES "^loongarch")
    set(Arch "LoongArch")
  elseif(Proc MATCHES "^mips")
    set(Arch "Mips")
  elseif(Proc MATCHES "^sparc")
    set(Arch "Sparc")
  elseif(Proc MATCHES "^m68k")
    set(Arch "M68k")
  else()
    set(Arch "")
  endif()
  set(${OUT_VAR} "${Arch}" PARENT_SCOPE)
endfunction()

# Determine which LLVM codegen backend this build needs. SpectralMDL is a JIT
# compiler, so the only code it ever generates is code for the machine it is
# running on: there is nothing to configure and no reason to link any other
# backend.
#
# OUT_VAR may come back empty, because the caller is in a better position to
# decide what that means. With 'find_package(LLVM)' there is a second, more
# authoritative source (LLVM_NATIVE_ARCH) to fall back on, but with
# SMDL_BUILD_LLVM there is not.
function(smdl_detect_native_arch OUT_VAR)
  # SpectralMDL cannot be built as a macOS universal binary. LLVM bakes exactly
  # one native target and one default triple into 'llvm-config.h', and
  # 'getDefaultTargetTriple()' returns that baked string rather than asking the
  # host. Every slice of a universal binary compiles against the same header,
  # so the arm64 slice would initialize the x86 backend and claim an x86_64
  # triple. Build each architecture separately instead.
  list(LENGTH CMAKE_OSX_ARCHITECTURES NumArchs)
  if(NumArchs GREATER 1)
    message(FATAL_ERROR
      "CMAKE_OSX_ARCHITECTURES names more than one architecture "
      "(${CMAKE_OSX_ARCHITECTURES}), but SpectralMDL cannot be built as a "
      "universal binary: it JIT compiles through LLVM, which bakes a single "
      "native target into 'llvm-config.h' that every slice would share. "
      "Configure one build per architecture instead.")
  endif()
  if(CMAKE_OSX_ARCHITECTURES)
    set(Proc "${CMAKE_OSX_ARCHITECTURES}")
  else()
    set(Proc "${CMAKE_SYSTEM_PROCESSOR}")
  endif()
  smdl_llvm_arch_for_processor(Arch "${Proc}")
  set(${OUT_VAR} "${Arch}" PARENT_SCOPE)
endfunction()
