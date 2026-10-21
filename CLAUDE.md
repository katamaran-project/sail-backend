# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

A Sail plugin (`sail --katamaran`) that translates Sail ISA models into µSail (muSail, usail), the Coq/Rocq
embedding used by Katamaran. Longer documentation is in `docs/` (`code-overview.md`, `usage.md`,
`tests.md`, `sail-notes.md`, `monomorphization.md`, `slang.md`). Parts of `docs/tests.md` are outdated:
it still describes the old `test.sh` workflow. Use the `run-tests.py` instructions below instead.

## Environment and build

- The dev environment comes from the Nix flake (`.envrc` runs `use flake`). It pins Sail, Rocq 8.20 and Katamaran.
  If you use opam instead, see `docs/installation.md`.
- `make build` runs `dune build`. `make install` runs `dune install`. **The plugin must be installed before Sail can load it.**
  The end-to-end tests call the `sail` on `PATH`, so they test the last *installed* plugin. Run `make install` after every change before you run them.
- The project currently targets Sail 0.20.3. The CI workflow in `.github/workflows` still pins 0.18.
- The plugin statically embeds its dependencies (`embed_in_plugin_libraries` in `sail_plugin/dune`), because Sail does not load them itself.
  The order of that list matters. If you add a library dependency to `nanosail`, add it there as well.

## Tests

- **Unit tests** (OUnit2): run `make test` (`dune test`). They live in `tests/nanosail-tests` and `tests/monad-tests`.
  Each suite is registered in the top-level list in `NanosailTests.ml`, so a new suite must be added there.
  Logging is silenced unless `VERBOSE` is set.
- **End-to-end tests** (`tests/coq-tests`): each directory that contains a `model.sail` is one test.
  The runner translates the model with Sail, then compiles the generated `.v` with `coqc`.
  - Run all of them with `make e2e`, or directly with `tests/coq-tests/run-tests.py`. A full run takes several minutes and uses 20 parallel jobs by default (`-j N`).
  - Run a single test with `tests/coq-tests/run-tests.py --test <name>`. Other selection flags: `--include REGEX`, `--exclude REGEX`, `--include-file FILE`, `--list`.
  - Other useful flags: `--no-coq` (translation only, much faster; env `NO_COQ`), `--no-dump` (skip the rewrite dumps), `--monomorphise` (env `MONOMO`).
  - Each run happens in a sandbox: `tests/coq-tests/test-output/<timestamp>-<commit>[-dirty]/<test>/`, with `test-output/latest` pointing to it.
    The sandbox holds the generated Rocq, the `rewrite-NN-*.sail` dumps of each Sail rewrite pass, the `delta-NN-*.diff` diffs between consecutive passes, and the combined output.
    Pass/fail counts are appended to `test-output/test-history.csv`.
  - Some tests are expected to fail because some features are not implemented yet.
  - Test configurations `$include ../shared-configuration.lisp`.
- **Template tests**: `make template-tests`.

## Architecture

There are three dune projects, layered as follows:

- `monads/`: a small, general monad library (State, Result, StateResult, Writer, and others).
- `nanosail/`: all of the translation logic. This is a wrapped library, so modules are reached as `Nanosail.X`.
- `sail_plugin/`: a thin layer that registers the `--katamaran` options with libsail, selects which Sail rewrites run (`Rewrites.ml`), and calls into `nanosail`.

The translation pipeline is Sail AST (after Sail's rewrites) → **nanosail** IR → µSail Rocq text:

1. `nanosail/SailToNanosail/` translates each Sail definition inside the `TranslationContext` monad (a State+Result monad).
   This monad generates unique identifiers, stores and looks up definitions that are already translated, and records calls to polymorphic functions.
   - A `NotYetImplemented` failure gives up on the current definition only: it is recorded as `Ast.Definition.UntranslatedDefinition` and translation continues with the next definition.
   - A `Failure` halts the whole translation.
   - The per-construct translators are in `Translate/`. The largest is `Function.ml`, which turns Sail expressions into µSail statements and carries the expected type through the translation.
2. `nanosail/Ast/` defines the IR. Where Sail and µSail differ, the IR follows µSail: expressions and statements are separate.
   The IR also keeps numeric expressions and constraints, which monomorphization needs.
3. `nanosail/NanosailToMicrosail/` turns the IR into pretty-printed Rocq inside the `GenerationContext` monad.
   This monad supports comment and annotation *blocks* and `not_yet_implemented [%here]` holes.
4. Output is driven by **templates**. `configuration.lisp` is written in **Slang**, the project's own small Lisp in `nanosail/Slang/`.
   It registers template files, and the `(*< ... >*)` blocks inside each template are evaluated as Slang.
   The Slang functions that templates can call, such as `base-translation` and `program-translation`, are defined in `nanosail/Templates/Prelude.ml`.
   The configuration options themselves are defined in `nanosail/Configuration.ml`.

Supporting modules:
- `PP`: a custom pretty printer, not PPrint. It tags each output fragment with the OCaml source locations that produced it, which is how `.template.html` output gets its tooltips.
- `FExpr`: a structured debug representation. Most types have a `to_fexpr` function, and `FExpr.pp_diff` shows differences between two values.
- `Logging`/`LogLib`: logging whose level is set with the `VERBOSE` environment variable (0 to 4, default 2).
- `ExtBase`: extensions to Jane Street `Base`, which the code uses throughout instead of the OCaml standard library.

## Coding conventions

These are intentional, even though they are not idiomatic OCaml:

- **Do not use wildcard `_` cases in matches.** Handle each constructor explicitly, so that adding a constructor produces compiler errors at every match that needs updating.
- **Give each type its own module** (`Ast.Expression.Variable`, not `E_Variable`), with related functions such as `equal`, `to_fexpr` and `pp` inside that module.
- **Define and use `equal` for each type.** Do not use polymorphic `=`.
- **Annotate types explicitly** on functions and parameters. Functions often take every piece of available context on purpose, even when they do not use all of it yet.
- Use `[%here]` (ppx_here) to pass source locations to logging and NYI calls.
- `make ws` strips trailing whitespace.
