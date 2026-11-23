# Expected Failures

The end-to-end tests in this directory are known to fail, usually because the translation does not support a feature yet.
`run-tests.py` runs them together with the tests in `tests/coq-tests` (unless `--no-expected-failing` is given) and reports them as

* `XFAIL` when they fail, as expected: this does not fail the run;
* `XPASS` when they pass: this fails the run, just like an unexpected failure (`FAIL`) of a test outside this directory.

When a test here starts to pass, move it to `tests/coq-tests`, so that it is expected to pass from then on:

```bash
git mv tests/coq-tests/expected-failing/NAME tests/coq-tests/NAME
```

Likewise, a test that is known to fail (a new one for a missing feature, or an existing one broken on purpose) is moved here.
Test names must be unique across `tests/coq-tests` and this directory.
`shared-configuration.lisp` is a symbolic link to `../shared-configuration.lisp`, so that the tests' `$include ../shared-configuration.lisp` also works when running Sail in a test's directory.

| Test | Why it fails |
|---|---|
| `assert-flow-typing-runtime-width` | Bitvectors whose width is only known at run time are not supported: `machine.v` contains unresolved implicit arguments. |
| `match-list-nested` | Matching against several fixed-length list patterns (`[\| x, y \|]`, `[\| x, y, z \|]`) is not translated, which leaves `base.v` with an open goal (`UNTRANSLATED_DEFINITIONS`). |
| `vectors` | Generic `vector` types are not supported: they are translated to `NYI`. |
