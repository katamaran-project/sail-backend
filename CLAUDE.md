# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

A Sail plugin (`sail --katamaran`) that translates Sail ISA models into Microsail (µSail, muSail, usail), the Coq/Rocq embedding used by Katamaran.
Longer documentation is in `docs/` (`code-overview.md`, `usage.md`, `tests.md`, `sail-notes.md`, `monomorphization.md`, `slang.md`).

## Environment and build

- The dev environment comes from the Nix flake (`.envrc` runs `use flake`). It pins OCaml 5.2.1, Sail 0.20.3, Rocq 9.2 and Katamaran (main, via its `rocq920` package and its nixpkgs).
  If you use opam instead, see `docs/installation.md`.
- `make build` runs `dune build`. `make install` runs `dune install`, which overwrites the plugin in the shared opam switch.
  **Do not run `make install` unless the user asks for it**: other checkouts and worktrees, possibly worked on in parallel, use the same switch.
  Instead, test this checkout's plugin as built by `dune build`:
  - `run-tests.py` and the Makefile's test targets do this by default (`run-tests.py --installed-plugin` uses the installed plugin instead).
  - Elsewhere, use `bin/sail-local` as a drop-in `sail`. It loads this checkout's built plugin in place of the installed one.
    Example: `gmake -C <katamaran>/case_study/Cheriot sail SAIL=$PWD/bin/sail-local CHERIOT_SAIL_DIR=<cheriot-sail>`,
    where `<katamaran>` and `<cheriot-sail>` are the paths of those checkouts.
    Always use GNU make 4 or later (`gmake` on macOS): the `make` 3.81 that ships with macOS cannot parse the Cheriot Makefile.
  The Cheriot Makefile passes `SAIL` on to cheriot-sail's Makefile. Its `CHERIOT_SAIL_DIR` variable selects which cheriot-sail checkout is used; it has no default and must always be passed.
  Its `OUT_DIR` variable sets where the output goes (the default is the case study directory). Give each parallel run its own `OUT_DIR`, and always pass it as an absolute path (see the warning in that Makefile).
  That Makefile still uses katamaran's own copy of the configuration; the maintained configuration and templates are now in `case-studies/cheriot/` (moved on 14 Nov 2026), and the loop below uses those.
- The project currently targets Sail 0.20.3 on OCaml 5.2.1, with Rocq 9.2 and Katamaran main. The CI workflow (`.github/workflows/build.yaml`) and `flake.nix` pin the same OCaml and Sail versions; keep them in sync.
- The plugin statically embeds its dependencies (`embed_in_plugin_libraries` in `sail_plugin/dune`), because Sail does not load them itself.
  The order of that list matters. If you add a library dependency to `nanosail`, add it there as well.

## Worktrees and feature workspaces

- **Create worktrees only with `bin/new-feature <branch>`. Never run `git worktree add` directly.**
  It creates `<root>/<branch>/` with a worktree of sail-backend and of each other repository configured for this user (for example katamaran), all on branch `<branch>`, plus a `CLAUDE.md` describing the workspace.
  `bin/new-feature --config` shows the root and the repositories; `--dry-run` shows what it would do. The configuration is in sail-backend's local git config (`newFeature.root`, `newFeature.repo`); the header of the script explains it.
  Do not change that configuration unless the user asks.
- Inside a workspace, use the sibling worktrees, not the main checkouts. Makefile defaults may point at main checkouts, so pass the paths explicitly,
  e.g. `CHERIOT_SAIL_DIR=<workspace>/cheriot-sail` and `SAIL=<workspace>/sail-backend/bin/sail-local`.

## Tests

- **Unit tests** (OUnit2): run `make test` (`dune test`). They live in `tests/nanosail-tests` and `tests/monad-tests`.
  Each suite is registered in the top-level list in `NanosailTests.ml`, so a new suite must be added there.
  Logging is silenced unless `VERBOSE` is set.
- **End-to-end tests** (`tests/coq-tests`): each directory that contains a `model.sail` is one test.
  The runner translates the model with Sail, then compiles the generated `.v` with `coqc`.
  - Run all of them with `make e2e`, or directly with `tests/coq-tests/run-tests.py`. A full run uses one job per CPU by default (`-j N`); more jobs than CPUs is slower, because each `coqc` needs about 650 MB.
    `coqc` verdicts are cached in `~/.cache/sail-backend/coq-cache`, keyed by the generated `.v` files and the Coq installation, so only tests whose generated Rocq changed are recompiled (`--no-coq-cache` disables this).
    The cache is shared by all checkouts and worktrees, so a new worktree needs no cold e2e run; a cold `dune build` takes only a few seconds.
  - Run a single test with `tests/coq-tests/run-tests.py --test <name>`. Other selection flags: `--include REGEX`, `--exclude REGEX`, `--include-file FILE`, `--list`.
  - Other useful flags: `--no-coq` (translation only, much faster; env `NO_COQ`), `--no-dump` (skip the rewrite dumps), `--monomorphise` (env `MONOMO`).
  - Each run happens in a sandbox: `tests/coq-tests/test-output/<timestamp>-<commit>[-dirty]/<test>/`, with `test-output/latest` pointing to it.
    The sandbox holds the generated Rocq, the `rewrite-NN-*.sail` dumps of each Sail rewrite pass, the `delta-NN-*.diff` diffs between consecutive passes, and the combined output.
    Pass/fail counts are appended to `test-output/test-history.csv`.
  - Some tests are expected to fail because some features are not implemented yet.
  - Test configurations `$include ../shared-configuration.lisp`.
- **Template tests**: `make template-tests`.
- **CHERIoT analysis library** (`tests/cheriot/lib`, standard-library Python): analyses a translation of the CHERIoT model, for the progress loop of `docs/cheriot-loop.md`.
  Run its unit tests with `make cheriot-lib-tests`; they use the small fixture run in `tests/cheriot/tests/fixtures`.
  `tests/cheriot/inventory.py RUN-DIR` runs the whole analysis on a run directory.
- **CHERIoT loop**: `gmake cheriot CHERIOT_SAIL_DIR=<cheriot-sail>` (`tests/cheriot/run.py`; the configuration is `case-studies/cheriot/configuration.lisp`, and `CHERIOT_CASE_STUDY` selects another case study directory) translates the CHERIoT core (the scope in `tests/cheriot/scope.toml`, computed from the call graph) with this checkout's plugin, compiles the result with coqc (plain, and with `NYI` as an axiom), and reports metrics, in about 1.5 minutes. `CHERIOT_ARGS=--no-coq` skips coqc; `--no-scope` translates the whole model.
  A run fails if any definition, clean instruction or Rocq position got worse than in `tests/cheriot/baseline.json`; `CHERIOT_ARGS=--update-baseline` accepts the current state, in the commit that makes the progress.
  Each run appends a line to `tests/cheriot/history.csv` (tracked).
  coqc runs under a watchdog that stops it when one command runs longer than `--coq-stall` seconds (default 45, kept tight on purpose: a stall points to a real problem, such as a quadratic derivation); the report names the stuck command.
  To compile a generated file by hand the same way, use `tests/cheriot/coq-watch.py --stall 45 FILE.v`, which reports the running command and its line as it goes.
  Rocq verdicts and `base.vo` are cached, keyed by the file contents and the Rocq installation.
  What is in and out of the CHERIoT scope is the user's decision, recorded in `cheriot-inventory/PROJECT-STATUS.md` §1a; read it before picking work.
  For example, virtual memory (Sv32, page tables, TLB) is out of scope: do not work on it or ask about it.
  Output goes to `tests/cheriot/output/<timestamp>-<commit>[-dirty]/` (about 160 MB per run, mostly the log; `latest` points to the newest).

## Architecture

There are three dune projects, layered as follows:

- `monads/`: a small, general monad library (State, Result, StateResult, Writer, and others).
- `nanosail/`: all of the translation logic. This is a wrapped library, so modules are reached as `Nanosail.X`.
- `sail_plugin/`: a thin layer that registers the `--katamaran` options with libsail, selects which Sail rewrites run (`Rewrites.ml`), and calls into `nanosail`.

Other top-level directories (not dune projects):
- `case-studies/`: whole Sail models translated with the plugin (`msp430/`, `minimal-caps/`); see `case-studies/README.md`. `case-studies/cheriot/` holds the CHERIoT configuration and templates (moved from katamaran's `case_study/Cheriot`) and `PROGRESS.md`, which tracks the translation (update it in any commit that changes the `gmake cheriot` numbers).
- `archived_experiments/`: old experiments kept for reference (`rewrites/`, `monomorphization-tests/`); not run by the build or the tests, and not maintained.
- `cheriot-inventory/`: the CHERIoT failure inventory and the project status and history documents.

The translation pipeline is Sail AST (after Sail's rewrites) → **nanosail** IR → MicroSail Rocq text:

1. `nanosail/SailToNanosail/` translates each Sail definition inside the `TranslationContext` monad (a State+Result monad).
   This monad generates unique identifiers, stores and looks up definitions that are already translated, and records calls to polymorphic functions.
   - A `NotYetImplemented` failure gives up on the current definition only: it is recorded as `Ast.Definition.UntranslatedDefinition` and translation continues with the next definition.
   - A `Failure` halts the whole translation.
   - The per-construct translators are in `Translate/`. The largest is `Function.ml`, which turns Sail expressions into Microsail statements and carries the expected type through the translation.
2. `nanosail/Ast/` defines the IR. Where Sail and Microsail differ, the IR follows Microsail: expressions and statements are separate.
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
