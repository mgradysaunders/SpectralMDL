include_guard(GLOBAL)

# Shorten what '__FILE__' expands to, so that a sanity check, a log line, and
# a doctest failure name a path relative to the tree instead of wherever the
# build machine happens to keep it. Nothing else depends on it: the flag
# rewrites the preprocessor macro alone and leaves the paths in the debug
# info absolute, which is what a debugger wants.
#
# GCC and Clang spell this '-fmacro-prefix-map=<old>=<new>', a plain prefix
# substitution over the path the compiler was handed. MSVC has no equivalent.
# The empty replacement is what makes the two agree: given a trailing
# separator on <old>, both leave 'lib/Compiler/Emitter.cc', whereas a '.'
# replacement leaves GCC's './lib/Compiler/Emitter.cc' against the same name
# normalized back to 'lib/Compiler/Emitter.cc' by Clang.
#
# The probe asserts that the rewrite actually happened rather than only that
# the flag was accepted, because a compiler that takes an unrecognized '-f'
# option with nothing worse than a warning (which is exactly what MSVC does)
# passes the weaker check and then shortens nothing. The '#line' stands in
# for the probe's own path so that the answer cannot depend on whether the
# generator spells source files absolutely on the command line.
include(CheckCXXSourceCompiles)
include(CMakePushCheckState)
cmake_push_check_state(RESET)
set(CMAKE_REQUIRED_FLAGS "-fmacro-prefix-map=/smdl-probe/=")
check_cxx_source_compiles(
  "#line 1 \"/smdl-probe/probe.cc\"
   static_assert(__FILE__[0] == 'p', \"__FILE__ was not remapped\");
   int main() { return 0; }"
  SMDL_HAVE_MACRO_PREFIX_MAP
  )
cmake_pop_check_state()

# Make '__FILE__' in TARGET's sources relative to ROOT, an absolute directory
# that contains them. Does nothing at all where the compiler cannot do it,
# since the only thing at stake is how long the names are.
function(smdl_shorten_file_macro TARGET ROOT)
  if(SMDL_HAVE_MACRO_PREFIX_MAP)
    target_compile_options(${TARGET} PRIVATE "-fmacro-prefix-map=${ROOT}/=")
  endif()
endfunction()
