# SpectralMDL

SpectralMDL is a
[spectral](https://en.wikipedia.org/wiki/Electromagnetic_spectrum)
[LLVM-based](https://llvm.org)
[JIT compiler](https://en.wikipedia.org/wiki/Just-in-time_compilation) for the
[Material Definition Language](https://www.nvidia.com/en-us/design-visualization/technologies/material-definition-language)
which is intended to be useful as a middleware C++ library in offline 
physically-based rendering programs.

SpectralMDL is developed at the 
[Digital Imaging and Remote Sensing (DIRS)](https://www.rit.edu/dirs) laboratory
at the Rochester Institute of Technology in support of the [DIRSIG](http://dirsig.cis.rit.edu)
simulation tool.

See [the documentation](https://mgradysaunders.github.io/SpectralMDL/docs/index) for more information.

## Environment variables

These let you debug a session without needing the host program to expose
a setting for each one. Every variable is read once per process, the
first time one is needed, and an unset or empty variable does nothing.

A variable that overrides a setting wins over whatever the host chose, but
leaves the host's own field alone. The first time it changes anything, it
logs a warning that names both values. A value it cannot parse is ignored
with a warning.

| Variable | Value | Effect |
|----------|-------|--------|
| `SMDL_DEBUG` | `0`, or anything else | Overrides `Compiler::isDebugEnabled`, the `$DEBUG` that turns on `debug::assert()`, `debug::breakpoint()`, and `debug::print()`. `0` forces it off. A breakpoint traps, which kills a process with no debugger attached. |
| `SMDL_OPT_LEVEL` | `0`, `1`, `2`, or `3` | Overrides the level `Compiler::compile()` optimizes LLVM-IR at. Machine code generation keeps its own default level. At `0`, some static material flags are unknown, so the host takes its conservative paths, and fewer unread images are skipped. |
| `SMDL_LOG_LEVEL` | `debug`, `info`, `warn`, or `error` | Overrides `Logger::setMinLevel()`. Messages reach only the sinks the host added. |
| `SMDL_DUMP_IR` | A directory | Each `Compiler::compile()` writes `smdl-<pid>-<n>-emitted.ll`, before the optimizer runs, and `smdl-<pid>-<n>-optimized.ll`, as `Compiler::jitCompile()` gets it. `<n>` counts the compiles in the process. The directory is created if needed. |
| `SMDL_PERF_MAP` | `0`, or anything else | `Compiler::jitCompile()` keeps frame pointers in the code it links, and names its functions in `/tmp/perf-<pid>.map` for `perf`. POSIX only. |
| `SMDL_DEFAULT_SEARCH_DIRS` | Directories, separated as in `PATH` | Configuration rather than a debugging aid, and read on every lookup: the directories every `FileLocator` searches last. |
