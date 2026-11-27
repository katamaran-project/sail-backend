# Tests

## Unit Tests

Unit tests can be found under

* `tests/monad-tests`, which test the functionality provided by the monads library
* `tests/nanosail-tests`

To run all these tests

```bash
$ make test
```

* When running the tests, the verbosity level is set to quiet, thereby suppressing all logging.
  It is, however, possible to override this by setting the verbosity level explicitly using `VERBOSE`.

## CHERIoT Loop

`tests/cheriot` translates the CHERIoT core with this checkout's plugin, compiles it with `coqc` and reports progress; see [the loop's manual](../case-studies/cheriot/docs/loop.md).

```bash
$ gmake cheriot CHERIOT_SAIL_DIR=/path/to/cheriot-sail
$ make cheriot-lib-tests        # unit tests of tests/cheriot/lib and run.py, on a small fixture run
```

## End to End Tests

These tests translate a Sail model into Coq code and have Coq check its validity.
They can be found in `tests/coq-tests`: each subdirectory containing a `model.sail` is one test.
Each test also has a `configuration.lisp`, which includes `../shared-configuration.lisp`.

Tests that are known to fail, usually because the translation does not support a feature yet,
live in `tests/coq-tests/expected-failing` instead (see [Expected Failures](#expected-failures)).

A test proceeds in two phases:

1. Sail translates `model.sail` using the plugin (`sail --katamaran --katamaran-config configuration.lisp model.sail`).
   A model can consist of several Sail files: `model.sail` can `$include` the other files of the test's directory and its subdirectories.
2. `coqc` compiles the generated Rocq files, each after the files it `Require`s.

The generated files are the outputs of the templates that the test's configuration registers,
and with [`preserve-file-structure`](usage.md#preserve-file-structure) also the declarations (named after the output of `(program-translation)`,
e.g. `machine_decl.v`) and one file per Sail file in a subdirectory (`Machine/` by default).
The runner reads the outputs of the templates from the configuration, so that a missing one (if Sail failed) fails the test,
and takes every other `.v` file in the sandbox as generated: a test's inputs contain no `.v` files apart from the templates.
When there are files in subdirectories, it passes `-Q . ""` to `coqc`, since the other files refer to them as `Machine.foo`.
`preserve-file-structure-scattered` uses other names (`types.v`, `program.v`, `program_decl.v` and `Model/`).

A test passes if all files compile.
When the Coq phase is disabled (see `--no-coq` below), a test passes if Sail exits successfully.

The tests use the `sail` on `PATH`, but load the katamaran plugin built by `dune build` in this checkout
(`_build/default/sail_plugin/sail_plugin_katamaran.cmxs`) instead of the one installed in the opam switch.
This way, several checkouts or worktrees can be tested at the same time without overwriting each other's installed plugin.
Run `dune build` after every change; `make e2e` does this for you.

### Running All Tests

Running all tests is done using

```bash
$ make end-to-end-tests

# or, shorter
$ make e2e
```

This builds the plugin, then calls `tests/coq-tests/run-tests.py`.

* Running all these tests from scratch takes a few minutes.
  Later runs are faster thanks to the Coq cache (see below).
* The tests in `tests/coq-tests/expected-failing` are run too, and are expected to fail (see below).
  The run succeeds (exit status 0) if every other test passes and every expected failure fails.
* By default, one test runs per CPU. Use `-j N` to change this.
  More jobs than CPUs does not help: each `coqc` peaks at about 650MB, and on an 8-CPU, 16GB machine
  20 jobs were almost twice as slow as 8.
  If too much memory is consumed, Linux's way of dealing with this is to kill processes,
  so lower the number of jobs on machines with little memory.

#### The Coq Cache

Compiling the generated Coq dominates the run time of a test, and its result depends only on the generated
Rocq files and on the Coq installation.
The runner therefore stores each test's `coqc` exit codes and output in a cache,
keyed by the names and contents of those files and by a fingerprint of `coqc` (its version, and the size and modification
time of every `.vo` file under `coqc -where`, `ROCQPATH` and `COQPATH`).
When a test generates Coq that an earlier run already compiled, `coqc` is not run again and the earlier output
is reused; `output.txt` says so, `result.json` has `"coq_cached": true`, and the run reports the number of hits.

The cache does not depend on the checkout, so all checkouts and worktrees share it, and a new worktree starts warm.
It lives in `~/.cache/sail-backend/coq-cache` (`$XDG_CACHE_HOME/sail-backend/coq-cache` if `XDG_CACHE_HOME` is set).
`SAIL_BACKEND_COQ_CACHE` or `--coq-cache-dir DIR` puts it elsewhere.

* `--no-coq-cache` always runs `coqc`.
* `--keep-vo` also always runs `coqc`, since a cache hit produces no `.vo` files.
* The cache can be deleted at any time.

### Expected Failures

Each test has one of four outcomes:

| Outcome | Test is in                         | Result | Fails the run |
|---------|------------------------------------|--------|---------------|
| `PASS`  | `tests/coq-tests`                  | passed | no            |
| `FAIL`  | `tests/coq-tests`                  | failed | yes           |
| `XFAIL` | `tests/coq-tests/expected-failing` | failed | no            |
| `XPASS` | `tests/coq-tests/expected-failing` | passed | yes           |

An `XPASS` fails the run so that a test that starts to pass (because a feature was implemented, say) is noticed
and moved out of `expected-failing`, after which a regression makes it `FAIL`.
The runner prints the `git mv` commands that do this:

```bash
$ git mv tests/coq-tests/expected-failing/NAME tests/coq-tests/NAME
```

Conversely, a new test for a feature that is not implemented yet goes into `expected-failing`.
`tests/coq-tests/expected-failing/README.md` lists why each of its tests fails; keep it up to date when moving tests.

* Test names must be unique across both directories: `--test NAME` works the same for either.
* `--no-expected-failing` skips the expected failures, except those named with `--test` or `--include-file`.
* With `--no-coq`, a test passes if Sail translates it, which most expected failures do: their generated Coq is what fails.
  An `XPASS` therefore does not fail a run without the Coq phase.
* `expected-failing/shared-configuration.lisp` is a symbolic link to `../shared-configuration.lisp`,
  so the tests' `$include ../shared-configuration.lisp` also resolves when running Sail in a test's directory.

### Selecting Tests

```bash
# Run a single test (repeatable)
$ tests/coq-tests/run-tests.py --test bit-returning-one

# Run all tests whose name matches a regular expression (repeatable)
$ tests/coq-tests/run-tests.py --include 'let-shadow-'

# Run the tests listed in a file, one per line (# comments allowed)
$ tests/coq-tests/run-tests.py --include-file failures.txt

# Skip tests whose name matches a regular expression (repeatable)
$ tests/coq-tests/run-tests.py --exclude 'bitvectors-'

# Only print the selected tests, without running them
$ tests/coq-tests/run-tests.py --include 'atoms-' --list
```

* `--test`, `--include` and `--include-file` are combined; `--exclude` is applied afterwards.
  Without any of the first three, all tests are selected, including those in `expected-failing`.
* Naming a nonexistent test with `--test` or `--include-file` is an error.
* `--list-nontests` lists the directories that look like tests but lack a `model.sail`; these are ignored.
* The `failures.txt` of a previous run can be passed to `--include-file` to rerun only the failed tests.

### Options

* `--no-coq` skips the (very time consuming) Coq phase, so that only the translation is checked.
  Setting the `NO_COQ` environment variable to any value has the same effect.
* `--no-dump` disables dumping Sail's intermediate rewrite results (dumping is on by default).
* `--monomorphise` passes `--auto-mono --mono-rewrites` to Sail.
  Setting the `MONOMO` environment variable to any value has the same effect.
* `--no-coq-cache` always runs `coqc`, instead of reusing the result for identical generated Coq (see above).
* `--keep-vo` keeps the Coq build products (`.vo`, `.glob`, ...), which are otherwise deleted after each test (they take about 8MB per test).
* `--output-dir DIR` changes where results are stored (default: `tests/coq-tests/test-output`).
* `--sail` and `--coqc` select the binaries to use (default: `sail` and `coqc` from `PATH`).
* `--plugin CMXS` selects the katamaran plugin to load (default: the one built in this checkout).
  `--installed-plugin` uses the plugin installed in the opam switch instead.
* Verbosity can be set using `VERBOSE=n`. See the `Configuration` module.
* Command line flags take precedence over environment variables.

Run `tests/coq-tests/run-tests.py --help` for the full list.

### Test Output

The source tree is never modified: each test runs in its own sandbox.
Each run gets its own directory

```text
tests/coq-tests/test-output/<timestamp>-<commit>[-dirty]/
```

where `-dirty` indicates that there were uncommitted changes at the time of the run.
`test-output/latest` is a symbolic link to the most recent run.
Only the 5 newest runs are kept; `--keep-runs N` changes that (0 keeps all).

A run directory contains

* `summary.txt`: the outcome (`PASS`, `FAIL`, `XFAIL` or `XPASS`) and duration of each test, followed by the totals.
* `failures.txt`: the names of the failed tests (`FAIL` and `XFAIL`), one per line, usable with `--include-file`.
* `unexpected.txt`: the names of the tests that made the run fail (`FAIL`, and `XPASS` when the Coq phase ran).
* `run.json`: the run's settings and results.
* `shared-configuration.lisp`: a copy of the shared configuration, included by each test's configuration.
* one directory per test.

Each test directory contains

* copies of the test's inputs (`model.sail`, `configuration.lisp`, templates, ...);
* the generated Coq files (`base.v`, `machine.v`, ...);
* `output.txt`: the commands that were run, their combined stdout/stderr and exit codes;
* `output.ast.txt`: `output.txt` reindented by `tests/reindent-ast.py`, to make dumped ASTs readable;
* `result.json`: the test's result, whether it is expected to fail, its outcome, exit codes, whether the Coq result came from the cache, duration and the Sail command used;
* unless `--no-dump` is given, `rewrite-NN-<pass>.sail`, the Sail AST after each rewrite pass,
  and `delta-NN-<pass>.diff`, the diff between consecutive passes.

The run directories are not tracked by Git.

## Template Tests

`tests/template-tests` holds one test per subdirectory: Sail translates its `model.sail`, and every file in its `expected/`
(usually just `microsail.v`) must be generated identically.
A directory in `expected/` (such as `Machine/` for [`preserve-file-structure`](usage.md#preserve-file-structure), in test `006`)
must not get other `.v` files.

```bash
$ make template-tests
```

## Continuous Integration

The GitHub workflow `.github/workflows/build.yaml` runs on every push and pull request, in two jobs:

* `build` builds the plugin in an opam switch, following [the installation instructions](installation.md), and runs the unit tests (`dune test`).
* `test` runs, in the Nix development shell of `flake.nix` (which provides Rocq and Katamaran),
  `make template-tests` and `make end-to-end-tests`.

The end-to-end step fails if a test outside `expected-failing` fails, or if a test in `expected-failing` passes;
in the latter case, move that test out of `expected-failing` in the same pull request.
When the step fails, the job uploads the summary and each test's `output.txt`, `result.json` and generated Coq as the
`end-to-end-test-results` artifact.

The `test` job caches the Nix store (keyed by `flake.nix` and `flake.lock`) and the Coq cache.
A run without a Nix store cache builds Sail, which takes a few minutes, and fetches Katamaran from its cachix.
That cache evicts old builds within days, so a run without a Nix store cache may also build Katamaran (about 30 minutes).
Nix is installed single-user (`nix-quick-install-action`), because the cache cannot be restored into the root-owned
store of a multi-user installation.

The CHERIoT library tests, the MSP430 tests and the logging tests are not run by the workflow.
