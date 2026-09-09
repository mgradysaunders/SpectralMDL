# The SMDL language suite

Every `.smdl` file here is compiled and every `unit_test` in it is run by

```sh
./build/bin/smdl test testing/language        # or: ctest -R smdl-language
```

The directory is the search root, which is why the suite has to be run as a
directory and not a file at a time: the relative imports would not resolve.
The whole run takes about nine seconds, the first failing test aborts the
rest, and no flag selects one test by name, so the suite is the unit of
iteration. Wavelength-dependent tests that need a grid other than the
default say so beside themselves, as
`smdl test -wavelength-range 900,2400 testing/language`.

## What is where

    lang/        the language itself, and the `#smdl` dialect
    builtin/     the builtin library, mirroring lib/Compiler/Builtin/
    support/     importable helpers the tests share, no tests of their own
    fixtures/    images, IES profiles, measured BSDFs, voxel grids

`builtin/` mirrors `lib/Compiler/Builtin/` file for file and directory for
directory, so `builtin/extras/hex.smdl` tests `::extras::hex` and an empty
slot in the tree is a module with no tests. A module needing more than one
file gets a directory of its own name, never a directory and a sibling file
with the same stem, since a package and a module cannot share a name.

`lang/` reads in this order, which is not the order the filenames sort in:

    types  operators  control_flow  functions  lambdas  structs  unions
    comptime  modules  search_dir  color  intrinsics  aggregate_abi

Companion modules that exist only to be imported live under
`lang/packages/`, and two of them carry the one test that can only be
written from inside them. `lang/sibling.smdl` and `lang/search_dir/` sit
where they do because where a file sits is what those tests are about.

## Conventions

**A test name is a sentence.** It states what the block proves, in sentence
case, with no trailing period, and leaves out the subject the file already
supplies. The runner prints the file above the names, so the two read
together:

    Running tests in 'testing/language/builtin/df/hair.smdl':
      'Energy is conserved in a white furnace' (line 155) ... success

A bare noun phrase is not a name. Recurring claims get one spelling, so
`Energy is conserved in a white furnace`, `The density integrates to one`,
`Sampling agrees with the reported density`, `Evaluation is reciprocal` and
`Values match the reference implementation` mean the same thing wherever
they appear. Two names inside one file are never the same; across files they
often are, on purpose.

**Every file opens with a `///` comment** saying what it covers and naming
anything external it pins: a paper, a reference implementation, a fixture
and where it came from. `smdl doc` renders it.

**`lang/` teaches and `builtin/` pins.** A `lang/` file explains the
construct before the tests that hold it down, with a C++ analogy where the
analogy earns its keep, because this suite is the only description of the
language there is. A `builtin/` file is terse: the header, and a comment
only where something is genuinely not obvious -- a citation, a tolerance's
justification, an invariant an estimator leans on. Do not read the `lang/`
density as licence elsewhere.

**Comments state the contract, never the bug.** Where a test exists because
a particular mistake was possible, say what has to hold and why it is easy
to get wrong. No "used to", no "previously", no bug narration.

**Imports are spelled at the call site.** `import ::math::*;` then
`math::normalize(...)`, so every call says which module it reached into.
`import X::*` makes a module reachable through its qualifier; `using X
import *` opens it for unqualified lookup as well, and the suite uses the
first form except where the second is what is being tested. Support modules
are imported absolutely (`using scatter = ::support::scatter;`), because a
relative `.::` resolves against the importing module's own package.

**A test that writes `$state` restores it with `preserve`.** Tests run in
lexicographic order by path, so a leak is a change in another file's
results. `preserve $state.coneWidth, $state.textureDensity;` cannot get the
pairing wrong the way a saved value and a `defer` can.

**Helpers.** One that returns a measurement is named for the measurement
(`furnaceIntegral`, `whiteSkyAlbedo`); one that asserts is named `check*`
and asserts only the property its name states. `camelCase` for helpers and
their locals; `snake_case` only where an identifier stands in for a name the
specification gives. A helper taking a lambda must be `@(macro)`: a lambda
expands at its call site, so a non-macro callee would try to expand a
caller-capturing body inside its own frame.

## Two things that are not free to change

`builtin/models/regolith.smdl` and `builtin/models/woody.smdl` each hold a
region between `// BEGIN GENERATED PRESET TESTS` and `// END GENERATED
PRESET TESTS`. It is written by a generator in another repository and must
not be hand-edited, the name of its `unit_test` included. The generated code
also depends on the module alias (`regolith`, `woody`) and the `::math`
import in the file header, so those are frozen too.

Images are cached by content hash, and one image holds one mip chain, so two
byte-identical files in different directories are one image and cannot be
asked for different chains. `fixtures/4x4.png` is requested with a mean
chain only; `fixtures/ridge_16.png` and `fixtures/bumps_32.png` with a
maximum chain only. `lang/search_dir/4x4.png` is a second copy of the same
bytes, which is fine because it is asked for the same way.
