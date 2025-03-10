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
These tests can be found in `tests/coq-tests`.

### Running All Tests

Running all tests is done using

```bash
$ make end-to-end-tests

# or, shorter
$ make e2e
```

* Running all these tests takes a few minutes.
* Not all tests pass due to missing features in the Sail-Coq translation step.

Three reports are generated:

* `tests-output.txt`: contains all `STDOUT` output generated while running the tests.
  This file is ignored by Git.
* `tests-history.txt`: each run, the number of failed and passed tests is appended to this file.
* `tests-overview.txt`: overwritten each run with a PASS/FAIL for each tests.

The tests are run by a Python script named `run-tests.py`, the main reason being that we wanted
to run the tests in parallel in a controlled way without having to deal with shell scripting madness.
Note that if too much memory is consumed (which can happen when running too many tests in parallel),
the Linux way of dealing this is to randomly close another process.
We worked on machines equipped with 32GB of RAM, so people with less memory might
need to lower the number of tests that are allowed to run in parallel.

### Running a Single Test

```bash
$ make install && (cd ./test/coq-test/TEST ; ../test.sh)
```

### Options

* Verbosity can be set using `VERBOSE=n`. See the `Configuration` module.
* Sail performs a number of rewrites.
  All intermediate (and final) results are written to file if the `DUMP` environment variable is set to any value, e.g., `DUMP= make e2e`.
  Note that defining `DUMP` also causes the `process-rewrites.rb` Ruby script to run, so make sure Ruby is installed (or remove the call to the script).
* Setting `MONOMO` to any value causes extra command line options to be passed along to Sail.
* Setting `NO_COQ` disables the (very time consuming) second test phase, i.e., Coq checking the generated code.
