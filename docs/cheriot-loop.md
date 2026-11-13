# Design: the CHERIoT progress loop

*Status: proposal, 13 Nov 2026. Branch `e2e-loop`.*

## 1. Goal

Make progress on translating the CHERIoT core **measurable after every change**, with two nested loops:

| loop | runs | time | answers |
|---|---|---|---|
| **inner** | unit tests + the small end-to-end tests in `tests/coq-tests` | seconds per test | "does this feature work?" Every gap has a small failing test that turns green when fixed. |
| **outer** (this document) | the CHERIoT model through the plugin and Rocq | about 1 minute | "how far is the CHERIoT core now, and did this change make anything worse?" |

The outer loop is one command. It reports a small set of numbers, and the per-definition changes since the last run. It **exits non-zero when something regressed**.

The scope decisions in `cheriot-inventory/PROJECT-STATUS.md` §1a apply:
- the core first (RV32I/E, M, Zicsr, CHERIoT capability instructions), without compressed instructions for now;
- dead code excluded by configuration;
- undefined values universally quantified;
- size-generic code deferred;
- `gmake` only.

## 2. Measured costs (13 Nov 2026, full model, M-series Mac)

| stage | time |
|---|---|
| Sail parse + typecheck, 67 files (`--just-check`) | 7.6 s |
| translation with the plugin, `VERBOSE=0`, no AST dumps | 39 s |
| same at `VERBOSE=4` (needed to attribute warnings to definitions; 148 MB log) | 85 s |
| `coqc base.v` (stops at the first `NYI`) | < 1 s |
| `machine.v` | 9 MB; never compiled yet |

The budget for one outer iteration is therefore **about one minute**. Restricting the translation to the core (§4) should shrink `machine.v` considerably, which will matter once coqc gets that far.

## 3. One command

```sh
gmake cheriot                    # from the sail-backend checkout or worktree
# or directly:
tests/cheriot/run.py [--cheriot-sail PATH] [--case-study PATH]
                     [--detail] [--no-coq] [--iterate-hard-failures]
                     [--update-baseline] [--compare RUN]
```

It always uses the plugin built in *this* checkout (`bin/sail-local`), as `run-tests.py` does.

### Stages

1. **Check inputs** (§6). Record the commit of each one.
2. **Typecheck** with plain Sail (8 s). On failure, stop: the problem is in the model or the inputs, not the plugin.
3. **Translate** at `VERBOSE=2` (`--detail`: `VERBOSE=4`, which attributes warnings to definitions), with the scope configuration from §4.
   - If Sail crashes, report the crash, the rewrite step and the source location; this is what P00 looks like.
   - If generation aborts with a hard failure, report the function and the message as the run's top blocker.
   - `--iterate-hard-failures` repeats the inventory's workaround: ignore the failing function, retry, and list every hard failure found.
4. **Extract** the items (`tests/cheriot/lib/extract.py`, from the inventory): untranslated definitions, `NYI` holes, hard failures, and warnings by kind.
5. **Calls to missing functions** (`lib/dangling.py`): calls in `machine.v` to functions it never defines, classified as extern, untranslated or excluded.
6. **Rocq**, unless `--no-coq`:
   - `coqc base.v`, then `machine.v`, as the e2e tests do;
   - a **probe compile** of copies in which `NYI` is an axiom (`Axiom NYI : forall {A}, list nat -> A.`). It shows the first *real* error beyond the holes, mapped back to the definition that contains it.

   The probe is the coqc progress signal until `base.v` compiles for real.
7. **Metrics, history and report** (§5).

All output goes to a sandbox, as in `run-tests.py`: `tests/cheriot/output/<timestamp>-<commit>[-dirty]/`, with a `latest` link. The sandbox holds the generated `.v` files, the log, `items.csv`, `metrics.json` and the coqc logs. Nothing in the source tree changes except the history file.

## 4. Scope: what counts as "the CHERIoT core"

The scope is **computed, not hand-listed**, from a small declarative file `tests/cheriot/scope.toml`:

```toml
# instruction constructors (union clause ast) whose execute clauses are in scope,
# selected by the source file they are defined in
instruction-files = [
  "sail-riscv/model/riscv_insts_base.sail",   # RV32I: 21 constructors
  "sail-riscv/model/riscv_insts_mext.sail",   # M: 6
  "sail-riscv/model/riscv_insts_zicsr.sail",  # Zicsr: 1
  "src/cheri_insts.sail",                     # CHERIoT capability instructions: 33
  # out of scope for now (decided 13 Nov 2026): compressed instructions,
  #   "sail-riscv/model/riscv_insts_cext.sail" (C: 34) and "src/cheri_insts_cext.sail" (CHERIoT C: 9)
]
# other entry points that are always in scope
roots = ["step", "fetch", "ext_decode", "encdec_backwards", "init_model"]   # all exist in the model
# functions that are reachable but deliberately not translated (policy, §4.2)
policy-excluded = ["print_*", "*_forwards" , "assembly_*", "BitStr", "..."]
```

### 4.1 From the scope file to a configuration

1. Build the call graph from the Sail AST after the last rewrite. Sail writes it with `--ddump-rewrite-ast`; `lib/callgraph.py` builds the graph from it.
   Since the rewrite pipeline runs anyway, the dump costs one extra file write. It is **cached**, keyed by the commits of cheriot-sail and sail-backend's `Rewrites.ml`.
2. **In scope** = everything reachable from `roots` plus `execute_<C>` for each in-scope constructor `C`, through calls *and* through mentions of types.
3. Everything else is **out of scope**. The runner writes it to a generated `scope-ignores.lisp`, which the case study's `configuration.lisp` includes with `$include`. The translator then skips it.

Because out-of-scope definitions are, by construction, called only from out-of-scope code, excluding them **never leaves calls to missing functions in scope**. This is the configuration-driven exclusion that `PROJECT-STATUS.md` asks for, without a new translator feature for the bulk of it.
Expected effect: the Zb\*, V, Sv32/TLB and PMP instruction paths and the assembly printer disappear from the output and from the numbers.

### 4.2 Policy exclusions

Some code *is* reachable from the core but is not wanted: tracing and printing (`print_reg` under `get_config_print_reg()`), and string formatting for those. Excluding these needs the one translator feature that `PROJECT-STATUS.md` §1a already calls for: **remove a definition and turn its calls into a no-op** (`unit`) or into an unconstrained value. That is its own piece of work, driven by an e2e test.
Until it exists, the runner reports policy-excluded definitions and their calls as a separate category, so that they do not hide real gaps.

## 5. What is measured, and the ratchet

### 5.1 Metrics (all restricted to the in-scope set)

| metric | meaning |
|---|---|
| `defs.in_scope` | in-scope definitions after rewrites (functions, `val`s, types, registers, top-level `let`s) |
| `defs.translated` / `defs.untranslated` | per kind; untranslated split by pile (the rules in `lib/piles.py`) |
| `nyi.holes`, `nyi.defs` | `NYI` holes and the definitions that contain them |
| `hard_failures` | 0, or the blocking function(s) |
| `calls.missing` | in-scope calls to undefined functions, by class: extern, untranslated, excluded |
| `instructions.translated` | in-scope `execute_<C>` clauses that translate |
| **`instructions.clean`** | in-scope `execute_<C>` clauses whose **whole call cone** is translated, without `NYI`s and without calls to missing functions. *This is the headline number*: an instruction that is clean is ready for Rocq, and later for a contract. |
| `rocq.base`, `rocq.machine` | `ok`, or the first error (file, line, definition) |
| `rocq.probe` | with `NYI` axiomatised: the first real error, and the fraction of definitions accepted before it |

### 5.2 Report

(The numbers in the example below are made up, to show the format.)

The report shows the change since the previous run and since the baseline, and **lists definitions, not just counts**: "now clean: `execute_ADDI`, `execute_CAndPerm`; newly broken: `execute_LOAD` (P03 nested sum pattern, `Match.ml:2081`)".

```
CHERIoT core @ 1069324f+dirty   (cheriot-sail 518813e, Katamaran 1688c950, Sail 0.20.3)
                         now    prev   baseline
instructions.clean        12      12       12
instructions.translated   58      56       56   +2: execute_FENCE, execute_FENCE_TSO
defs.untranslated         41      43       43   -2 (P04 vector literal of bits(1))
nyi.holes                 30      30       30
calls.missing            210     212      212
rocq.probe          base.v:598 (Access_kind: polymorphic union)   unchanged
result: PROGRESS (no regressions)
```

### 5.3 History and ratchet

- Every run appends one line to `tests/cheriot/history.csv`, which is tracked in git, like the e2e test history.
- `tests/cheriot/baseline.json`, also tracked, stores the **per-definition status** at the last accepted state.
- A run **fails (exit 1)** if any in-scope definition got worse than in the baseline (translated → untranslated, clean → not clean, new hard failure, Rocq first error earlier), even if the totals improved.
- `--update-baseline` accepts the current state. It is part of the commit that makes the progress, so `git log tests/cheriot/baseline.json` is the progress log.

## 6. Inputs and reproducibility

| input | where it comes from | recorded |
|---|---|---|
| plugin | this checkout, `dune build`, via `bin/sail-local` | commit + dirty flag |
| Sail | `sail` on `PATH` (must be 0.20.3) | version; a different version is an error |
| CHERIoT model | a cheriot-sail checkout: `--cheriot-sail`, else `$CHERIOT_SAIL_DIR`, else the path in the workspace `CLAUDE.md` | commit, submodule commit; a commit other than the pinned one (`tests/cheriot/inputs.toml`) is a warning |
| case study (configuration, templates, Makefile) | katamaran's `case_study/Cheriot` (`--case-study`), or `case-studies/cheriot` here, depending on the open question in §9 | commit |
| scope | `tests/cheriot/scope.toml` | hash |
| Katamaran library | installed in the switch | revision from `INSTALLED-FROM.txt`; compared with `flake.lock`'s pin |

Workarounds for blockers that are not fixed yet, such as the `-splice` for the Sail crash, live in `tests/cheriot/workarounds/`. Each names the e2e test that tracks its cause. The runner applies them, says so in the report, and **reports a workaround as removable** once its e2e test passes. Today that is one entry: `splice.sail` for P00, tracked by `assert-flow-typing-fixed-width`.

## 7. The inner loop, and how the two connect

- Every pile in `cheriot-inventory/20261113-1408/INVENTORY.md` that is in scope gets a small e2e test in `tests/coq-tests`, written before the fix: TDD.
- `run-tests.py` gets an **expected-failure list** (`tests/coq-tests/expected-failures.txt`, one test name and pile per line). It reports `XFAIL` (still failing, as expected), `XPASS` (fixed: remove it from the list) and `FAIL` (a regression). Unexpected failures make it exit non-zero.
- `gmake loop` runs, in order: `dune build`, `dune test`, the e2e tests without coqc, the e2e tests that changed or were named, then `gmake cheriot`. It stops at the first failure.
- The outer report links each untranslated definition to its pile, and so to the e2e test that drives the fix.

## 8. Implementation plan

Each step is small, has its own tests, and leaves the loop usable.

1. **Done** (14 Nov 2026). Move the inventory scripts (`extract.py`, `reachability.py`, `dangling-calls.py`, `piles.py`) into `tests/cheriot/lib/`, with unit tests on small fixture files (a few `.v` excerpts and log excerpts).
   They are now `extract.py`, `callgraph.py`, `dangling.py` and `piles.py`, with `tests/cheriot/inventory.py` running all four on a run directory, and `make cheriot-lib-tests`.
   On the full model they reproduce the inventory scripts' output exactly. Two changes: the nanosail source root is explicit
   (a wrong root used to leave every construct empty and so most items outside their pile, silently), and a hard failure is also read from a plain log.
2. `tests/cheriot/run.py` skeleton:
   - stages 1–5, the sandbox and `metrics.json`;
   - the `-splice` workaround wired in;
   - `gmake cheriot`.
3. Rocq stages: plain and probe compile, with the first error mapped to its definition.
4. Scope: `scope.toml`, cached call graph, generated `scope-ignores.lisp`, and the in-scope filter on all metrics. Check that no in-scope call goes missing because of the exclusion.
5. `instructions.clean` (call-cone check), history CSV, baseline and ratchet, and the report with per-definition changes.
6. `run-tests.py`: expected failures (`XFAIL`/`XPASS`), plus `gmake loop`.

Then the gap work starts, one pile at a time: first P00 (the `assert-flow-typing-fixed-width` test, which also removes the splice workaround), then the S items on the critical path.

## 9. Open questions

1. **Where does the CHERIoT case study live?** The runner can take the configuration and templates from katamaran's `case_study/Cheriot` (as today) or from `case-studies/cheriot` in this repository.
   The second keeps the whole loop in one repository and makes generated scope files easy to include. The first keeps one copy next to the future proofs.
2. **Pinning cheriot-sail:** a recorded commit with a warning (proposed), or a git submodule (exact, but submodules duplicate per worktree; see `HACKING.md`)?
3. **CI:** the outer loop needs cheriot-sail and Katamaran. Should it run only locally at first (proposed), or also in GitHub CI?
