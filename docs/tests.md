# Tests

## Unit Tests

Unit tests can be found under

* `tests/monad-tests`, which test the functionality provided by the monads library
* `tests/nanosail-tests`

To run all these tests

```bash
$ make test
```

## End to End Tests

These tests translate a Sail model into Coq code and have Coq check its validity.
These tests can be found in `tests/coq-tests`.

Running them is done using

```bash
$ make end-to-end-tests

# or, shorter
$ make e2e
```

* Note that running all these tests takes a few minutes.
* Note that not all tests pass.

Three reports are generated:

* `tests-output.txt`: contains all `STDOUT` output generated while running the tests.
  This file is ignored by Git.
* `tests-history.txt`: each run, the number of failed and passed tests is appended to this file.
* `tests-overview.txt`: overwritten each run with a PASS/FAIL for each tests.

