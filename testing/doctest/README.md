# The C++ suite

One executable over a vendored framework, run by

```sh
ctest --test-dir build          # this and smdl-language
./build/bin/smdl-doctest        # the library
```

99 test cases and 447 subcases, about thirty seconds.
Useful flags, all of which work because `doctest.cc` forwards the command
line untouched: `-ltc` lists the case names, `-tc=` and `-sc=` and `-sf=`
filter by case, subcase, and source file, `-s` reports the passing
assertions too, and `-d` gives the per-case durations.

The suite runs in a **random order** (`-ob=rand` with the seed CTest
prints), so a case that only passes after another case ran shows up here
rather than somewhere else. That is safe because nothing leaves
process-wide state behind; see the fixtures below.

CTest runs this suite, and `smdl-language`, with `SMDL_DEFAULT_SEARCH_DIRS`
cleared, since the default search directories belong to the developer's
shell and no test may depend on them. Running a binary directly inherits
them.

## Where a test lives

**The test for `lib/X/Y.cc` lives at `X/Y.cc` here**, beside the framework
it is built with. A test file is named for the thing it tests, at the path
that thing lives at, whether that is a `lib/` source or a public header
with no source of its own (`RenderUtil/FastMath.cc`,
`Support/VectorMath.cc`, `JIT.cc`). Where a contract is spread across
several sources and observable only from outside, the file is named for the
contract and sits under the directory of the component that surfaces it:
`Compiler/Diagnostics.cc` for the words a failing compile fails with,
`Compiler/Resources.cc` for the resources a compile loads and caches.

**Against `testing/language/`, the line is what the test can see.** The
SMDL-language suite pins what the language *means*. This suite pins what
the *host* sees: a compile that must fail and the words it fails with, the
IR that must be emitted, the entry points the host calls, the files on
disk, and anything at a `wavelengthBaseMax` other than the default. None of
those is expressible as a `unit_test` block, because a failing compile
aborts that suite's run and a running one cannot see its own IR. So the
error paths of lambdas and of call-site `inline` live here while their
positive behavior lives there, and neither is a duplicate of the other.

## Naming

**A test name is `Subject: what it is about`.** The subject is the type,
function, or facility the case is about, spelled as the source spells it
(`Image`, `MaterialDef`, `smdlEvalMetalIOR`, `findMaterial`), then a colon
and a lowercase phrase. That phrase is a claim when the case proves one
thing and a topic when it groups subcases that each prove one. The reporter
prints the file above the name, so the three lines read together:

    doctest/Resource/Image.cc:13:
    TEST CASE:  Image: reading, writing, and the mip chains
      A chain is built down to the 1x1 level

**A subcase states a claim**, sentence-cased, no trailing period, leaving
out the subject the case already supplies. Two carve-outs: a claim whose
subject is an identifier keeps the identifier's own spelling
(`reset rewinds into the first slab`), and where subcases parameterize a
claim the case has already stated in full, they name the parameter instead
(`With a pinhole`, `With variants`, `On a struct field`). A bare label is
not a name anywhere else. No two names inside one file are the same;
across files they often are, on purpose.

Reading `-ltc` end to end is the test of this: it should read as a
specification of what the library promises.

## Assertions

An assertion has to say what went wrong, which means three habits.

- A substring check is `CHECK_CONTAINS`, never
  `CHECK(text.find(needle) != std::string::npos)`. The latter reports
  `false` and not one character of the message the assertion is about.
- An `std::optional<Error>` goes through `CHECK_OK` or `REQUIRE_OK`, which
  report the message, and a compile that must fail goes through
  `CHECK_ERROR`. Never `REQUIRE(!expr)`, which reports nothing.
- Two vectors or matrices compare with `CHECK_SAME` or `CHECK_NEAR`, which
  print both sides. The library's `==` on a vector is componentwise and
  yields a vector of bools, so it is not something an assertion can take.
- `CAPTURE` of a raw `const char *` prints the pointer; wrap it in
  `std::string`.

`REQUIRE` is for a precondition whose failure would make the rest of the
body meaningless. `CHECK` is for the claim itself.

## Fixtures

Headers in two layers, the way `lib/Support/` -> `lib/` is.

    Fixtures.h                    needs only the public library
    CompileFixtures.h             adds what only this suite needs

Both sit at the top level, which is the suite's include path, so a test
source at any depth includes either unqualified. `Fixtures.h` includes
`doctest.h` and is what a test file includes instead of it.

**A test owns nothing global.** Anything process-wide that a test installs
is put back by a destructor, never by a statement at the end of a body: a
throwing `REQUIRE` would skip the statement. That covers the scratch
directory (`TempDir`), the logger's sinks (`CollectedLog`), and the
environment (`ScopedEnv`). It is what makes the random order safe, and the
one thing to get right when adding a test that touches any of them.

Two constraints worth knowing:

- A `TempDir` stem must be unique among the test cases in one binary, since
  the path is derived from it alone. Concurrent runs of the same binary
  still race on it, exactly as they did before; nothing depends on that.
- The logger has no targeted sink removal, so only one `CollectedLog` may
  be alive at a time. Give it a needle broad enough to cover everything the
  case wants, and tell the messages apart with `count()`.

## Adding a file

The suite lists its sources explicitly; there is no glob, so a file added
and not listed compiles nowhere and passes silently. Put its path in
`CMakeLists.txt` and nothing else. The C++ standard, the visibility, the
RTTI flag and the floating-point flags come from the target set up there;
the last two matter, because `BuildInfo`'s RTTI check and `FastMath`'s
error bounds only mean what they mean under the flags the library itself
compiles with.

The two golden tables (`RenderUtil/HazeGolden.inl`,
`RenderUtil/SunSkyGolden.inl`) are included rather than compiled, so they
are deliberately not listed. Each must be included inside an anonymous
namespace: they name some of the same constants, and internal linkage is
what keeps them apart. Both are generated in another repository, named in
their own header comments; regenerate rather than editing.

## What is not covered

The mirror rule turns the gaps into a list, and it is a long one. This is a
record, not a plan.

On the library side `lib/Compiler/Type.cc` (2990 lines) has no test at all,
and nor do `lib/Compiler/Context.cc`, `lib/Compiler/Value.cc`,
`lib/Compiler/llvm.cc`, `lib/Compiler/Intrinsics.cc` and
`lib/Support/Parallel.cc`. `lib/Formatter.cc`
(641 lines) has exactly one subcase, and it lives in `Module.cc`.
`lib/AST.cc` is reached only through `Parser.cc`'s `getDocCommentText`.

Three `Resource/VoxelGrid.cc` subcases become silent no-ops with zero
assertions when the build lacks NanoVDB, and doctest reports them as
passing.
