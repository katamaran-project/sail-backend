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

## End to End Tests

These tests translate a Sail model into Coq code and have Coq check its validity.
They can be found in `tests/coq-tests`: each subdirectory containing a `model.sail` is one test.
Each test also has a `configuration.lisp`, which includes `../shared-configuration.lisp`.

A test proceeds in two phases:

1. Sail translates `model.sail` using the plugin (`sail --katamaran --katamaran-config configuration.lisp model.sail`).
2. `coqc` compiles the generated `base.v` and `machine.v`.

A test passes if both files compile.
When the Coq phase is disabled (see `--no-coq` below), a test passes if Sail exits successfully.

The tests use whatever `sail` is on `PATH`, meaning they test the last *installed* version of the plugin.
`make e2e` builds and installs the plugin first, but when running `run-tests.py` directly,
make sure to run `make install` after every change.

### Running All Tests

Running all tests is done using

```bash
$ make end-to-end-tests

# or, shorter
$ make e2e
```

This builds and installs the plugin, then calls `tests/coq-tests/run-tests.py`.

* Running all these tests takes several minutes.
* Not all tests pass due to missing features in the Sail-Coq translation step.
* By default, 20 tests run in parallel. Use `-j N` to change this.
  Note that if too much memory is consumed (which can happen when running too many tests in parallel),
  Linux's way of dealing with this is to kill processes.
  We worked on machines equipped with 32GB of RAM, so people with less memory might
  need to lower the number of parallel jobs.

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
* `--keep-vo` keeps the Coq build products (`.vo`, `.glob`, ...), which are otherwise deleted after each test (they take about 8MB per test).
* `--output-dir DIR` changes where results are stored (default: `tests/coq-tests/test-output`).
* `--sail` and `--coqc` select the binaries to use (default: `sail` and `coqc` from `PATH`).
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
* `result.json`: the test's result, exit codes, duration and the Sail command used;
* unless `--no-dump` is given, `rewrite-NN-<pass>.sail`, the Sail AST after each rewrite pass,
  and `delta-NN-<pass>.diff`, the diff between consecutive passes.

After each run, a line is appended to `test-output/test-history.csv`
containing the timestamp, the commit, whether there were uncommitted changes, the number of passed and failed tests, and the run identifier.
This file is tracked by Git; the run directories are not.
