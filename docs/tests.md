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

## CHERIoT Analysis Library

`tests/cheriot/lib` analyses one translation of the CHERIoT model.
It is the basis of the progress loop described in `docs/cheriot-loop.md`, and uses only Python's standard library.

* `extract.py` collects the *items* (untranslated definitions, `NYI` holes, warnings, hard failures, coqc errors)
  from the generated `.v` files, the log and the coqc logs.
* `callgraph.py` builds a call graph from a Sail AST dump after the rewrites and marks which items are reachable.
* `dangling.py` classifies the calls in `machine.v` to functions it does not define.
* `piles.py` sorts the items into the piles of the CHERIoT inventory.

`tests/cheriot/inventory.py RUN-DIR` runs all of them on a directory in which the plugin translated the model,
and writes `items.csv`, `dangling-calls.csv` and `piles.txt`.
Reachability needs a dump: run Sail with `--ddump-rewrite-ast RUN-DIR/dump/rw`, or pass `--dump FILE`.

Item constructs are read from the nanosail source at the line numbers the log gives,
so the analysis must use the source of the plugin that made the run (`--source-root`, by default this checkout).

To run the library's unit tests, which use the small fixture run in `tests/cheriot/tests/fixtures`:

```bash
$ make cheriot-lib-tests
```

## End to End Tests

These tests translate a Sail model into Coq code and have Coq check its validity.
They can be found in `tests/coq-tests`: each subdirectory containing a `model.sail` is one test.
Each test also has a `configuration.lisp`, which includes `../shared-configuration.lisp`.

A test proceeds in two phases:

1. Sail translates `model.sail` using the plugin (`sail --katamaran --katamaran-config configuration.lisp model.sail`).
2. `coqc` compiles the generated `base.v` and `machine.v`.

A test passes if both files compile.
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
* Not all tests pass due to missing features in the Sail-Coq translation step.
* By default, one test runs per CPU. Use `-j N` to change this.
  More jobs than CPUs does not help: each `coqc` peaks at about 650MB, and on an 8-CPU, 16GB machine
  20 jobs were almost twice as slow as 8.
  If too much memory is consumed, Linux's way of dealing with this is to kill processes,
  so lower the number of jobs on machines with little memory.

#### The Coq Cache

Compiling the generated Coq dominates the run time of a test, and its result depends only on the generated
`base.v` and `machine.v` and on the Coq installation.
The runner therefore stores each test's `coqc` exit codes and output in a cache,
keyed by the contents of those two files and by a fingerprint of `coqc` (its version, and the size and modification
time of every `.vo` file under `coqc -where`, `ROCQPATH` and `COQPATH`).
When a test generates Coq that an earlier run already compiled, `coqc` is not run again and the earlier output
is reused; `output.txt` says so, `result.json` has `"coq_cached": true`, and the run reports the number of hits.

The cache does not depend on the checkout, so all checkouts and worktrees share it, and a new worktree starts warm.
It lives in `~/.cache/sail-backend/coq-cache` (`$XDG_CACHE_HOME/sail-backend/coq-cache` if `XDG_CACHE_HOME` is set).
`SAIL_BACKEND_COQ_CACHE` or `--coq-cache-dir DIR` puts it elsewhere.

* `--no-coq-cache` always runs `coqc`.
* `--keep-vo` also always runs `coqc`, since a cache hit produces no `.vo` files.
* The cache can be deleted at any time.

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
  Without any of the first three, all tests are selected.
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

A run directory contains

* `summary.txt`: PASS/FAIL and duration for each test, followed by the totals.
* `failures.txt`: the names of the failed tests, one per line, usable with `--include-file`.
* `run.json`: the run's settings and results.
* `shared-configuration.lisp`: a copy of the shared configuration, included by each test's configuration.
* one directory per test.

Each test directory contains

* copies of the test's inputs (`model.sail`, `configuration.lisp`, templates, ...);
* the generated Coq files (`base.v`, `machine.v`, ...);
* `output.txt`: the commands that were run, their combined stdout/stderr and exit codes;
* `output.ast.txt`: `output.txt` reindented by `tests/reindent-ast.py`, to make dumped ASTs readable;
* `result.json`: the test's result, exit codes, whether the Coq result came from the cache, duration and the Sail command used;
* unless `--no-dump` is given, `rewrite-NN-<pass>.sail`, the Sail AST after each rewrite pass,
  and `delta-NN-<pass>.diff`, the diff between consecutive passes.

After each run, a line is appended to `test-output/test-history.csv`
containing the timestamp, the commit, whether there were uncommitted changes, the number of passed and failed tests, and the run identifier.
This file is tracked by Git; the run directories are not.
