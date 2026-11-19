# Contributing

## Environment

- Nix: the flake (`.envrc` runs `use flake`) pins OCaml 5.5.1, Sail 0.20.3, Rocq 9.2 and Katamaran main.
- opam: see [installation](docs/installation.md).
- Keep the versions in `flake.nix` and the CI workflow (`.github/workflows/build.yaml`) in sync.
- Use GNU make 4 or later (`gmake` on macOS); macOS's `make` 3.81 cannot parse some of the Makefiles.

## Build

- `make build` (`dune build`): a cold build takes a few seconds.
- `make install` (`dune install`) overwrites the plugin in the opam switch, which other checkouts may share.
  To test this checkout's plugin without installing it, use `bin/sail-local`, a drop-in `sail` that loads `_build/default/sail_plugin/sail_plugin_katamaran.cmxs`.
  The test targets do this by default.
- The plugin embeds its dependencies statically (`embed_in_plugin_libraries` in `sail_plugin/dune`), because Sail does not load them.
  A new library dependency of `nanosail` goes there too; the order of the list matters.

## Tests

Details in [tests](docs/tests.md).

| command | what |
|---|---|
| `make test` | unit tests (OUnit2, `tests/nanosail-tests`, `tests/monad-tests`); `VERBOSE=N` shows logging |
| `make e2e` | end-to-end tests: translate each `tests/coq-tests/*/model.sail`, compile with `coqc` |
| `make template-tests` | template tests |
| `make msp430-tests` | MSP430 tests |

- A new unit test suite is registered in the top-level list in `NanosailTests.ml`.
- E2E runner (`tests/coq-tests/run-tests.py`):
  - one test: `--test NAME`; also `--include REGEX`, `--exclude REGEX`, `--include-file FILE`, `--list`;
  - `--no-coq`: translation only; `--no-dump`: skip the rewrite dumps; `-j N`: jobs (default: one per CPU, each `coqc` needs about 650 MB);
  - `coqc` verdicts are cached in `~/.cache/sail-backend/coq-cache`, shared by all checkouts (`--no-coq-cache` disables it);
  - output: `tests/coq-tests/test-output/<timestamp>-<commit>[-dirty]/<test>/`, with `latest` pointing at the newest run;
  - some tests fail on purpose: they track features that are not implemented yet.
- Each test's `configuration.lisp` does `$include ../shared-configuration.lisp`.

## Coding conventions

Intentional, even where not idiomatic OCaml (rationale in [code overview](docs/code-overview.md#coding-style)):

- No wildcard `_` cases in matches: a new constructor then gives a compiler error at every match to update.
- One module per type (`Ast.Expression.Variable`, not `E_Variable`), with its `equal`, `to_fexpr` and `pp`.
- Define and use `equal` for each type; no polymorphic `=`.
- Annotate functions and parameters with their types. Functions often take all available context on purpose.
- Pass source locations with `[%here]` (ppx_here) to logging and `NYI` calls.
- Use Jane Street `Base` (and `ExtBase`), not the OCaml standard library.
- `make ws` strips trailing whitespace.

## Commits

- One title line (≤ 72 characters) that states the change.
- Keep documentation and code changes in separate commits.
