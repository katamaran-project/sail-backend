# Archived experiments

Experiments kept for reference. Nothing here is run by the build or the tests, and the scripts are not maintained.

| directory | what it is | status |
|---|---|---|
| `rewrites/` | small Sail models, each showing the effect of one Sail rewrite pass (see `rewrites/readme.md`); Frederic Vogels, Dec 2024 – Mar 2025 | `compile.sh` uses the `-katamaran_config` flag that Sail 0.20.3 rejects. To see what each rewrite does, use the end-to-end tests instead: `tests/coq-tests/run-tests.py` writes the AST after every rewrite (`rewrite-NN-*.sail`) and the diff between consecutive rewrites (`delta-NN-*.diff`) for every test. |
| `monomorphization-tests/` | translations of polymorphic Sail functions to Isabelle via Sail's Lem backend, to compare Sail's own monomorphisation (`--auto-mono`, `--lem-mwords`); Frederic Vogels, Dec 2024 | `build.sh` needs Lem and a Sail source checkout at `~/repos/sail` |

Both were moved here from the repository root and `tests/` on 13 Nov 2026.
