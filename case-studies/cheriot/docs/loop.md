# The CHERIoT loop

One command translates the CHERIoT core with this checkout's plugin, compiles the result with Rocq, measures it, and fails if anything got worse.
Code: `tests/cheriot/run.py` and `tests/cheriot/lib/`.

## Running

```sh
gmake cheriot CHERIOT_SAIL_DIR=/path/to/cheriot-sail                    # from the repository root
gmake cheriot CHERIOT_SAIL_DIR=... CHERIOT_ARGS='--no-coq --keep-runs 10'
tests/cheriot/run.py --cheriot-sail DIR [options]                        # the same, directly
```

| option | effect |
|---|---|
| `--cheriot-sail DIR` | cheriot-sail checkout with its `sail-riscv` submodule (default `$CHERIOT_SAIL_DIR`) |
| `--case-study DIR` | configuration and templates (default `$CHERIOT_CASE_STUDY`, else `case-studies/cheriot`) |
| `--no-scope` | translate and measure the whole model |
| `--no-coq` | skip Rocq |
| `--coq-stall S` | stop `coqc` when one command runs longer than `S` seconds (default 45) |
| `--no-coq-cache` | compile without the Rocq cache |
| `--update-baseline` | accept this run as the baseline, even if worse |
| `--keep-dumps` | keep Sail's AST dump after every rewrite (75 MB), not only the last |
| `--output-dir DIR` | default `tests/cheriot/output` |
| `--keep-runs N` | keep the `N` newest runs, delete older ones; 0 keeps all (default 5) |

- About 1.5 minutes. Uses `bin/sail-local` and needs Sail 0.20.3 on `PATH`.
- The model's sources come from cheriot-sail's Makefile (`SAIL_SRCS`, via `printvar.mak`), plus `main.sail`.
- The pinned cheriot-sail commit is in `tests/cheriot/inputs.toml`; another commit gives a warning.
- Exit status: 0 all stages passed, 1 a stage failed (the report says which), 2 bad arguments or inputs.

## Stages

1. **inputs**: check them, record versions (commits, Sail, Katamaran).
2. **typecheck**: plain Sail, so a model problem is not blamed on the plugin.
3. **scope**: compute the core from `scope.toml` and the call graph; write `scope-ignores.lisp`, included by the run's copy of the configuration.
4. **translate**: the plugin at `VERBOSE=4` (lower levels lose which definition each warning belongs to, and save little time).
5. **rocq**: plain and probe compilation (below).
6. **analyse**: items (untranslated definitions, holes, warnings, coqc errors), reachability, missing calls, definition statuses, clean instructions.
   Fails if `machine.v` calls an excluded definition other than from a dispatcher (the scope is wrong).
7. **ratchet**: compare with the baseline.

## Scope file

`tests/cheriot/scope.toml`:

```toml
instruction-files = ["sail-riscv/model/riscv_insts_base.sail", "src/cheri_insts.sail", ...]  # constructors of `ast` declared there
roots = ["step", "fetch", "ext_decode", "encdec_backwards", "init_model"]

[dispatchers]
execute = "execute_"          # from execute, follow only the calls to execute_<C> for in-scope C

[policy-excluded]
no-op = ["print_reg", ...]
failure = ["translate32", ...]
unconstrained = ["bits_str", ...]
registers = ["tlb32", ...]

[policy-excluded.value]
sys_pmp_count = "0"
```

- In scope: everything reachable from the roots, the registers and `execute_<C>` of each in-scope instruction.
- The call graph comes from Sail's AST dump after the rewrites, cached in `~/.cache/sail-backend/cheriot-dumps` (key: Sail version, `sail_plugin/Rewrites.ml`, the model's sources).
  On a miss, a Sail run makes it in about 20 s. The translation's own dump must be identical, or the cache key misses an input.
- Out of scope: ignored with `ignore-functions` and `ignore-values`. Types are never ignored.

### Policy exclusions

- Each `[policy-excluded]` entry is removed together with its uses (`exclude-functions-as-*`, `exclude-registers`; [usage](../../../docs/usage.md)). Why each is sound: [design](design.md#policy-exclusions).
- The scope does not follow edges into them; excluded registers are not roots.
- Counted on their own line of the report, not as failures; recorded in the baseline.
- Warnings: a name the model does not define, a register under a function key (or the reverse), an excluded root.

## Rocq

- **Plain**: `coqc base.v`, then `machine.v`, as the e2e tests do.
- **Probe**: copies in `probe/` where holes are an axiom, so the first error shows the first problem beyond the holes:
  - `Axiom NYI : forall {A}, list nat -> A.` after the imports; each `NYI[n]` becomes `(NYI (cons n%nat nil))`;
  - the body of a function with a hole or a call to a missing function is replaced by the axiom;
  - the proof of `fundef_bindfree` is admitted; the untranslated-definitions marker and comment are left out;
  - line numbers are mapped back to the original file.
- A file without `NYI` compiles once; plain and probe run in parallel.
- **Watchdog**: `coqc -time`; a command running longer than `--coq-stall` seconds stops it, and the compilation is `STUCK` on that command.
  The ratchet does not compare a stuck compilation; `--update-baseline` keeps the baseline's position for it.
  By hand: `tests/cheriot/coq-watch.py --stall 45 FILE.v`.
- **Cache**: verdicts and `.vo` files in `~/.cache/sail-backend/cheriot-rocq` (`$SAIL_BACKEND_CHERIOT_ROCQ_CACHE`), keyed by the file, what it loads and the Rocq installation.
- An error in the generated Rocq is a measurement, not a failed stage.

## Metrics

All restricted to the scope (`metrics.json`):

| metric | meaning |
|---|---|
| instructions clean | in-scope `execute_<C>` whose whole call cone is translated, without holes or calls to missing functions: the headline |
| instructions generated | in-scope `execute_<C>` that translate |
| functions generated / total | in-scope functions |
| untranslated definitions | definitions the translator gave up on |
| `NYI` holes | holes in the generated code |
| hard failures | errors that stop the translation |
| missing calls | calls in `machine.v` to undefined functions, by class: outside the scope, untranslated, extern |
| Rocq plain / probe | first error (file, line, definition) or `compiles` |

- Definition status: `ok` (translated, no holes) > `holes` > `missing`.
- `progress.json` lists each unclean instruction's blockers; the report ranks blockers by the number of instructions they block.
- `piles.txt` groups the items by cause (`tests/cheriot/lib/piles.py`).
- The report compares with the previous run of the same scope and with the baseline, and names the definitions that changed.

## Ratchet and baseline

- `tests/cheriot/baseline.json` (tracked) holds the numbers and the status of every in-scope definition.
- A run fails if anything got worse than there, even if the totals improved:
  - a definition's status went down, or it got more holes;
  - a clean instruction is no longer clean;
  - plain or probe Rocq stops earlier: fewer files, or a definition accepted in the baseline is no longer accepted.
- Notes, not failures: definitions entering or leaving the scope, newly excluded definitions, a changed `scope.toml` or cheriot-sail commit.
- `CHERIOT_ARGS=--update-baseline` accepts the current state, in the commit that makes the progress; update [progress.md](progress.md) in the same commit.

## Output

- `tests/cheriot/output/<timestamp>-<commit>[-dirty]/`, `latest` points at the newest; about 160 MB per run, mostly the log.
  - `base.v`, `machine.v`, `probe/`, `coqc-*.log`, `log.txt`, `dump/`;
  - `items.csv`, `dangling-calls.csv`, `piles.txt`, `metrics.json`, `progress.json`, `run.json`, `scope.json`, `scope-ignores.lisp`.
- Only the 5 newest runs are kept (`--keep-runs N`).
- Each run that gets through the analysis appends a line to [`history.csv`](history.csv) (tracked).
- `make cheriot-lib-tests`: unit tests of the library and of `run.py`, on the fixture run in `tests/cheriot/tests/fixtures`.
