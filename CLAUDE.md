# CLAUDE.md

Guidance for Claude Code (claude.ai/code) in this repository.

A Sail plugin (`sail --katamaran`) that translates Sail ISA models to µSail, the Rocq embedding of Katamaran.

- Build, tests, coding conventions: [`CONTRIBUTING.md`](CONTRIBUTING.md). Follow its conventions (no wildcard matches, `equal` per type, explicit types).
- Architecture: [`docs/code-overview.md`](docs/code-overview.md). Configuration and templates: [`docs/usage.md`](docs/usage.md).
- Tests in detail: [`docs/tests.md`](docs/tests.md). Case studies: [`case-studies/README.md`](case-studies/README.md); CHERIoT: [`case-studies/cheriot/README.md`](case-studies/cheriot/README.md).

## Don'ts

- **No `make install`** unless the user asks: it overwrites the plugin in the shared opam switch, which parallel checkouts use.
  Test with `bin/sail-local` (the test targets already do).
- **In a multi-repository workspace**, use the sibling worktrees, not the main checkouts, and pass their paths explicitly.
- **No bare `git stash`**: the stash is shared by all worktrees. Use a WIP commit instead.
- **No macOS `make`**: always `gmake` (GNU make 4 or later).

## Habits

- After a change: `dune build`, `make test`, and `make e2e` (or `tests/coq-tests/run-tests.py --test NAME` for one test).
  `--no-coq` translates only, which is much faster.
- E2E `coqc` verdicts are cached across checkouts, so a new worktree needs no cold run.
- E2E tests that are expected to fail (unimplemented features) live in `tests/coq-tests/expected-failing`; `make e2e` succeeds only without `FAIL` or `XPASS`.
  An `XPASS` means: move the test out of `expected-failing` and update its `README.md`. CI runs all of this.
- New dependency of `nanosail`: add it to `embed_in_plugin_libraries` in `sail_plugin/dune` too (order matters).
- Logging level: `VERBOSE=0..4` (default 2).
