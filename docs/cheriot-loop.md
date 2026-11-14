# Design: the CHERIoT progress loop

*Status: proposal, 13 Nov 2026; steps 1–5 of §8 implemented on 14 Nov 2026. Branch `e2e-loop`.*

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
| `run.py` translation: `VERBOSE=2` / `VERBOSE=4`, with `--ddump-rewrite-ast` (14 Nov) | 80 s / 88 s |
| `coqc base.v` (stops at the first `NYI`) | < 1 s |
| `machine.v` | 9 MB; never compiled yet |

The budget for one outer iteration is therefore **about one minute**. Restricting the translation to the core (§4) should shrink `machine.v` considerably, which will matter once coqc gets that far.

## 3. One command

```sh
gmake cheriot CHERIOT_SAIL_DIR=PATH    # from the sail-backend checkout or worktree
# or directly:
tests/cheriot/run.py [--cheriot-sail PATH] [--case-study PATH] [--no-scope] [--no-coq] [--coq-stall S] [--no-coq-cache]
                     [--keep-dumps] [--output-dir DIR]
                     [--update-baseline]
# planned: [--iterate-hard-failures] [--compare RUN]
```

It always uses the plugin built in *this* checkout (`bin/sail-local`), as `run-tests.py` does.
It takes the list of Sail files from cheriot-sail's Makefile (`SAIL_SRCS`, through the case study's `printvar.mak`, plus `main.sail`), as the case study's Makefile does.

*Implemented (steps 2 and 3):* stages 1–6 and the metrics in `metrics.json`; the Rocq stage runs before the extraction, so that `items.csv` also holds the coqc errors. Exit status 0 when every stage succeeded, 1 when one failed, 2 for invalid arguments or inputs.
The sandbox keeps only the last rewrite dump (all of them take 75 MB; `--keep-dumps` keeps them), but a run still takes about 160 MB, mostly the 140 MB log, so old runs in `tests/cheriot/output/` need deleting now and then.

### Stages

1. **Check inputs** (§6). Record the commit of each one.
2. **Typecheck** with plain Sail (8 s). On failure, stop: the problem is in the model or the inputs, not the plugin.
3. **Translate** at `VERBOSE=4`, with the scope configuration from §4.
   (A first version used `VERBOSE=2`, with `VERBOSE=4` as an option. Measured, `VERBOSE=2` saves only 8 of 88 s, but its log lacks the skipped definitions and the definition each warning and error belongs to, so the analysis loses whole categories. So `VERBOSE=4` is always used.)
   - If Sail crashes, report the crash, the rewrite step and the source location; this is what P00 looks like.
   - If generation aborts with a hard failure, report the function and the message as the run's top blocker.
   - `--iterate-hard-failures` repeats the inventory's workaround: ignore the failing function, retry, and list every hard failure found.
4. **Extract** the items (`tests/cheriot/lib/extract.py`, from the inventory): untranslated definitions, `NYI` holes, hard failures, and warnings by kind.
5. **Calls to missing functions** (`lib/dangling.py`): calls in `machine.v` to functions it never defines, classified as extern, untranslated or excluded.
6. **Rocq**, unless `--no-coq`:
   - `coqc base.v`, then `machine.v`, as the e2e tests do;
   - a **probe compile** of copies in which `NYI` is an axiom (`Axiom NYI : forall {A}, list nat -> A.`). It shows the first *real* error beyond the holes, mapped back to the definition that contains it.

   The probe is the coqc progress signal until `base.v` compiles for real.

   *Implemented (step 3)* in `lib/rocq.py`. The probe copies are in `probe/`; the prelude
   (`From Stdlib Require Import Lists.List. Import ListNotations.` and the axiom) goes right after the file's imports,
   the template's marker `Theorem UNTRANSLATED_DEFINITIONS : False. Qed.` and what follows it (a comment with the Sail source of the untranslated definitions, which Rocq's lexer need not accept) are left out (untranslated definitions are holes too),
   each hole `NYI[n]` is written `(NYI (cons n%nat nil))` (one argument, not read as a bit string by Katamaran's notations),
   the body of a function definition (`Definition fun_f … :=`) that contains a hole or a call to a function that the file does not define is replaced by the axiom,
   because a hole in a type leaves constraints that no axiom solves, and a missing callee is an unknown name (14 Nov 2026; both are counted by their own metrics);
   the proof of `fundef_bindfree` (which computes on every body, and is stuck on the axiom) is then admitted,
   and probe line numbers are mapped back to the original file. Each first error names the generated definition that contains it,
   and how many of the file's definitions come before it. An error in the generated Rocq is a measurement, not a failed stage;
   only a coqc that cannot run is.

   *Watchdog, cache and timings (14 Nov 2026).* coqc runs with `-time` under a watchdog (`rocq.watch`): `-time` reports each command as soon
   as it finishes, so the running command is the one after the last reported one. A command that runs longer than `--coq-stall` seconds
   (default 45) stops coqc, and the compilation is **STUCK** on that command (line, definition, how long). That is not an error of the
   generated Rocq but a command too slow to wait for, so the ratchet does not compare a stuck compilation (the run's outcome is `STUCK`),
   and `--update-baseline` keeps the baseline's position for it. `tests/cheriot/coq-watch.py FILE.v` runs the same watchdog by hand, with
   a status line every 30 s.
   A file without `NYI` compiles the same way in its probe copy, so it is compiled once and the probe reuses the plain result and `.vo`
   (`reused`); plain and probe compilations of the same file run in parallel. Verdicts and compiled `.vo` files are cached in
   `~/.cache/sail-backend/cheriot-rocq` (`$SAIL_BACKEND_CHERIOT_ROCQ_CACHE`), keyed by the file, the files it loads and the Rocq installation's
   fingerprint (as in `run-tests.py`); `--no-coq-cache` turns it off. The ten slowest commands of each compilation are in `metrics.json`
   (`rocq.<kind>[].slowest`), and the report lists the five slowest of each kind.
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

(This was the proposal. The file as implemented is `tests/cheriot/scope.toml`: it has `instruction-files`, `roots` and `dispatchers` (§4.1), but no `policy-excluded` yet.)

### 4.1 From the scope file to a configuration

*As implemented (step 4, `lib/scope.py`, `tests/cheriot/scope.toml`); this replaces the first version of this section, two of whose claims were wrong.*

1. **Call graph.** It is built from Sail's AST dump after the last rewrite (`--ddump-rewrite-ast`, `lib/callgraph.py`).
   Its nodes are the functions, the top-level values (`let`) and the registers; there is an edge when a definition mentions another node.
   The scope is needed *before* the translation, so the dump comes from a cache, keyed by the Sail version, `sail_plugin/Rewrites.ml` and the contents of the model's sources
   (`~/.cache/sail-backend/cheriot-dumps`). On a miss, a Sail run with a configuration that ignores every definition makes it in about 22 s.
   The translation writes the same dump again, and the analysis checks that the two are identical; if not, the cache key misses an input.
2. **In scope** = everything reachable from the scope file's `roots`, from every register, and from `execute_<C>` for each in-scope constructor `C`.
   - Registers are roots because the translator cannot ignore them, so whatever their definitions use is needed.
   - **Dispatchers.** `execute` is one `match` that calls `execute_<C>` for every constructor, and `step` calls `execute`, so plain reachability puts every instruction in scope.
     The scope file therefore names `execute` as a dispatcher: from it, only the calls for in-scope instructions are followed.
3. **Excluded** = the functions and values outside the scope. The runner writes them to `scope-ignores.lisp` in the run directory and appends `$include scope-ignores.lisp` to the run's copy of the case study's configuration.
   This needed a small configuration feature, `ignore-functions` and `ignore-values`: the existing `ignore-…-predicate` functions *replace* the predicate,
   so a generated predicate would have discarded the case study's own. The names given to the new functions are ignored in addition to what the predicates select (see `docs/usage.md`, and the e2e test `ignore-functions-by-name`).
   Types are not excluded: a type that only excluded code uses is still translated, which is harmless, while excluding one that in-scope code needs would break the translation.
4. **Check.** The first version of this section claimed that excluding out-of-scope definitions never leaves in-scope calls to missing functions.
   That is false for the dispatcher: `execute` still calls the `execute_<C>` of the 86 out-of-scope instructions, and those calls remain in `machine.v` as calls to missing functions (class *outside the scope*).
   Removing them needs the translator feature of §4.2 (turn calls into a failure or no-op). Any *other* call into excluded code means that the scope is wrong, and fails the analysis stage.
   On its first run this check found a real bug: functions with a doc comment (`/*! ... */`) before them were not nodes of the call graph, so `capToBits` was neither in scope nor excluded, and the `encCapToBits` it calls was excluded.
   (The inventory's `reachability.py` had the same gap: it missed 9 of the model's 1959 functions.)

All metrics are restricted to the scope: an item counts when its definition is in scope, or when it belongs to no definition (like a coqc error); the totals of functions and instructions are those in scope.
`--no-scope` translates and measures the whole model, as before.

What stays in scope, measured on 14 Nov 2026: 61 instructions and about 640 of 1959 functions.
The PMP and TLB code stays, because the memory path and the CSR instructions reach it, and so do the Zb\* mnemonic helpers, which the tracing and printing code reaches.
Removing those is the policy exclusion of §4.2.

### 4.2 Policy exclusions

Some code *is* reachable from the core but is not wanted: tracing and printing (`print_reg` under `get_config_print_reg()`), string formatting for those, and virtual memory (Sv32), which CHERIoT does not have.
These are removed by the translator feature that `PROJECT-STATUS.md` §1a calls for: **remove a definition together with its uses** (`docs/usage.md`, "Excluding definitions together with their uses";
e2e tests `tests/coq-tests/exclude-*`). Per definition, the configuration says what a use becomes: for a function, the unit value (no-op), a failure (`stm_fail`) or an unconstrained value
(a call to the foreign function `unconstrained σ`, which may return any value of σ); a register is left out of `Reg`, a read of it is an unconstrained value and a write a no-op.

*Implemented* (14 Nov 2026). The `[policy-excluded]` table of `scope.toml` lists them under `no-op`, `failure`, `unconstrained` and `registers`, each with the reason:

- the scope does not follow edges into them, so what only they reach leaves the scope too; excluded registers are not roots;
- `scope-ignores.lisp` gets the matching `exclude-functions-as-...` and `exclude-registers` calls;
- the report counts them on their own line, per policy, and not as failures (they are outside the scope); `metrics.json` has `scope.policy_excluded`;
- a call to a policy-excluded function that is still in `machine.v` is an analysis problem;
- `baseline.json` records them; the ratchet notes newly and no longer excluded definitions, and definitions that left the scope because of an exclusion. These are notes, not regressions;
- a name that the model does not define, a register under a function key (or the reverse), and an excluded root are warnings of the scope stage.

Excluded now: printing and tracing (print externs as no-ops; the strings they print and the `get_config_print_*` flags as unconstrained values) and Sv32
(`translate32` and `translationException` as failures, `flush_TLB` and `init_vmem` as no-ops, the register `tlb32`). PMP is not excluded: what `pmpCheck` and the
PMP registers become decides what proofs about the core may assume, so it is the user's decision.

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

*Implemented (steps 2–5)*, in `metrics.json` with somewhat different names (`lib/metrics.py`).
The clean instructions come from `lib/progress.py`, which gives each in-scope function and value a status:
`ok` (translated, no holes), `holes` (translated, with `NYI` holes) or `missing` (not translated, whatever the reason).
An instruction is clean when every definition in its call cone is `ok` and none calls a missing function, externs included (extern calls cannot compile until P16).
For each other instruction, `progress.json` in the run directory lists its *blockers*: the definitions in its cone that are not `ok`, and the missing functions it calls.
The report ranks blockers by how many instructions each blocks, which shows where a fix pays off most.

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

*Implemented (step 5):* the report has these columns (the previous run is the last line of `history.csv` with the same scope),
and the ratchet stage lists the definitions that got better or worse since the baseline.

### 5.3 History and ratchet

- Every run appends one line to `tests/cheriot/history.csv`, which is tracked in git, like the e2e test history.
- `tests/cheriot/baseline.json`, also tracked, stores the **per-definition status** at the last accepted state.
- A run **fails (exit 1)** if any in-scope definition got worse than in the baseline (translated → untranslated, clean → not clean, new hard failure, Rocq first error earlier), even if the totals improved.
- *Implemented (step 5)* as the `ratchet` stage (`lib/progress.py`), for runs with a scope. Worse means: a definition's status went down (`ok` > `holes` > `missing`), a definition with holes got more of them,
  a clean instruction is no longer clean, or the plain or probe Rocq compilation stops earlier.
  "Earlier" compares names, not counts: fewer files compiled, or a definition that Rocq accepted in the baseline still exists but is no longer accepted.
  (Comparing the number of definitions before the error made a pure clean-up, which ignores definitions before it, look like a regression.) Definitions that enter or leave the scope, and a changed `scope.toml` or cheriot-sail commit, are reported as notes.
- `case-studies/cheriot/PROGRESS.md` is the readable summary of the numbers, kept by hand: current numbers, blockers and a row per commit that changes them.
- `--update-baseline` accepts the current state. It is part of the commit that makes the progress, so `git log tests/cheriot/baseline.json` is the progress log.

## 6. Inputs and reproducibility

| input | where it comes from | recorded |
|---|---|---|
| plugin | this checkout, `dune build`, via `bin/sail-local` | commit + dirty flag |
| Sail | `sail` on `PATH` (must be 0.20.3) | version; a different version is an error |
| CHERIoT model | a cheriot-sail checkout: `--cheriot-sail`, else `$CHERIOT_SAIL_DIR`, else the path in the workspace `CLAUDE.md` | commit, submodule commit; a commit other than the pinned one (`tests/cheriot/inputs.toml`) is a warning |
| case study (configuration, templates) | `case-studies/cheriot` here (§9, question 1); `--case-study` or `$CHERIOT_CASE_STUDY` selects another | commit |
| scope | `tests/cheriot/scope.toml` | hash |
| Katamaran library | installed in the switch | revision from `INSTALLED-FROM.txt`; compared with `flake.lock`'s pin |

Workarounds for blockers that are not fixed yet live in `tests/cheriot/workarounds/`. Each names the e2e test that tracks its cause. The runner applies them, says so in the report, and **reports a workaround as removable** once its e2e test passes.
Today there are none: P00 and the hard failures were fixed in #65, so the full model translates without workarounds and step 2 did not build this mechanism. It gets built when the first workaround is needed.

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
2. **Done** (14 Nov 2026). `tests/cheriot/run.py` skeleton:
   - stages 1–5, the sandbox and `metrics.json` (`lib/metrics.py`);
   - ~~the `-splice` workaround wired in~~: no longer needed (§6);
   - `gmake cheriot`;
   - the cheriot-sail commit is pinned in `tests/cheriot/inputs.toml`, and a different commit gives a warning (the proposed answer to §9, question 2).
3. **Done** (14 Nov 2026). Rocq stages: plain and probe compile, with the first error mapped to its definition (`lib/rocq.py`).
4. **Done** (14 Nov 2026). Scope: `scope.toml`, cached call graph, generated `scope-ignores.lisp`, and the in-scope filter on all metrics. Check that no in-scope call goes missing because of the exclusion (§4.1; the dispatcher's calls are the expected exception).
5. **Done** (14 Nov 2026). `instructions.clean` (call-cone check), history CSV, baseline and ratchet, and the report with per-definition changes (`lib/progress.py`).
   Along the way, the call graph learnt to name Sail operators (`operator <_s`) as the translator does (`operator_barltbar_barsbar`); before, all of them were one node.
6. *Skipped* (14 Nov 2026): `run-tests.py`: expected failures (`XFAIL`/`XPASS`), plus `gmake loop`. Not needed while the loop uses no workarounds.

Then the gap work starts, one pile at a time, beginning with the S items on the critical path. (P00, planned as the first, was fixed in #65.)

## 9. Open questions

1. **Where does the CHERIoT case study live?** The runner can take the configuration and templates from katamaran's `case_study/Cheriot` (as today) or from `case-studies/cheriot` in this repository.
   The second keeps the whole loop in one repository and makes generated scope files easy to include. The first keeps one copy next to the future proofs.
   *Answered* (14 Nov 2026): here. The configuration, templates and `printvar.mak` moved to `case-studies/cheriot/`, which the loop uses by default.
2. **Pinning cheriot-sail:** a recorded commit with a warning (proposed), or a git submodule (exact, but submodules duplicate per worktree; see `HACKING.md`)?
3. **CI:** the outer loop needs cheriot-sail and Katamaran. Should it run only locally at first (proposed), or also in GitHub CI?
