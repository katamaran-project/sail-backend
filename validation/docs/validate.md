# gmake validate

Hand-picked programs run from reset in µSail (Rocq interpreter) and Sail's C emulator; the full state after reset and after every step compared (`state-format.md`).
Design: [`design.md`](design.md).

## Run

```sh
opam exec --switch=sail-backend -- gmake validate
opam exec --switch=sail-backend -- gmake validate VALIDATE_ARGS="addi mul -j 8"
validation/scripts/validate.py [CASE...] [-j N] [--no-cache]   # once built
validation/scripts/validate.py --list-divergences             # divergences.toml as Markdown
```

- `gmake validate` builds what is missing, then runs `validation/scripts/validate.py`:
  - emulator: `gmake -C validation/emulator` (~10 min cold; up to date: nothing).
  - model: `gmake -C validation/rocq model` only if `validation/build/model/base.v` is missing; refresh it by hand after a translator change.
  - Rocq: `gmake -C validation/rocq validate` (`Validate.vo` and what it needs).
- `VALIDATE_CHERIOT=DIR`: the cheriot-sail clone (default `$CHERIOT_SAIL_DIR`, else `../cheriot-sail` next to the checkout; the emulator was built from it).
- Exit status 1 if a case neither passes nor is an expected divergence.
- Output per case in `validation/build/validate/<case>/`: `prog.s`/`prog.elf` (assembled), `emu.run`/`emu.log`, `Run.v`/`rocq.run`/`coqc.log`, `diff.txt` (first differing step), `divergence.txt` (recorded vs actual deltas, if an expected divergence does not match). Table also in `validation/build/validate/results.md`.

## Table

```
| case | steps | stop | exit | result | Rocq |
```

- `steps`: steps run in Rocq (states − 1).
- `stop`: why the Rocq run ended: `Halted` (htif), `Failed` (`stm_fail`), `StepLimit`, `OutOfFuel`, `NoAnswer`.
- `exit`: the emulator's htif exit code (`-`: none, e.g. step limit).
- `result`:
  - `pass`: every state equal, same number of states, expected exit code.
  - `expected divergence at step n`: a case in `divergences.toml`, diverging exactly as recorded ([Known divergences](#known-divergences)).
  - `FAIL: ... (divergences.toml)`: a case in `divergences.toml` that does not diverge as recorded: none, another step, another stop, delta or exit code.
  - `FAIL at step n`: first differing state; `(Rocq stm_fail "msg")` or `(Rocq OutOfFuel)` when that is why.
  - `Rocq OutOfFuel` / `Rocq NoAnswer`: states equal up to there.
  - `BAD CASE: exit e, expected x`: both sides agree, but the program did not end as intended (e.g. an unintended trap).
  - `ERROR: ...`: assembler, emulator or coqc error.
- `Rocq`: coqc time, or `cached`.

## How it works

- `validation/scripts/mk-test-elf.py`: assembler (labels, RV32I/M/Zicsr, CHERIoT, `li`/`cla`); `prog.s` = `cases/lib/prologue.s` + case + `cases/lib/epilogue.s`.
- Emulator: `cheriot_sim_dump --disable-compressed -F -W --insns-per-tick 1000000000 --inst-limit L --dump-state emu.run prog.elf`.
- Rocq: generated `Run.v` per case (program words, limit), shared runner `validation/rocq/Validate.v`:
  ```coq
  Definition prog : list Z := [0x00000797; ...]%Z.
  Goal True. Redirect "rocq" ltac2:(print_lines constr:(case_lines fuel limit prog)). exact I. Qed.
  ```
  - `case_lines`: reset, up to `limit` steps (as `Harness.run_steps`), run-file lines, then `end <stop>`; keeps the states before an `OutOfFuel`/`NoAnswer`.
  - Fuel 1000 per call (depth; `harness.md`).
- Compare: `statefile.py compare` logic (`first_difference`, `state_diff`): first step whose states differ, `diff -u` of that step (`rocq` vs `emulator`).

### Stopping at the same step

- Step limit `L`: 200, or `# limit: N` in the case. Rocq: `L` calls of `step`; emulator: `--inst-limit L`, patched to count every `zstep` (upstream: only retired ones; `emulator.md`).
- Traps: the prologue points `MTCC` at a handler (epilogue) that ends the run via `tohost`; no fetch-fault loop.

### Clock ticks

- The emulator ticks `mcycle`/`mtime`/platform every `rv_insns_per_tick` retired steps, outside `step` (not in the model).
- Taken: `--insns-per-tick 1000000000` (option added by the patch): no tick in any run. Cases stay far below it anyway (step limit 200).

### Caching

- Rocq output cached by input hash, as e2e (`tests/coq-tests/run-tests.py`, whose `coq_fingerprint` is reused): `Run.v`, `base.v`, `machine.v`, `Interp.v`, `Harness.v`, `Names.v`, `Dump.v`, `Validate.v`, the Rocq installation.
- `$SAIL_BACKEND_VALIDATE_CACHE`, else `~/.cache/sail-backend/validate-cache/<hash>.out`; shared by all checkouts. `--no-cache` turns it off.
- The emulator always runs (milliseconds).

## Add a case

- `validation/cases/<name>.s`: the body only; `#` comments say what it covers.
- Registers: x0–x13 (RV32E); `c14`/`c15` (`a4`/`a5`) belong to prologue/epilogue.
- Integer and capability registers are the same (`a2` = `ca2`): keep integers (e.g. `t0`–`t2`) apart from capabilities (`ca0`–`ca3`, `cs0`, `cs1`).
- Memory: `cspecialr ca0, mtdc` (root memory cap, address 0), `li t0, 0x80002000`, `csetaddr ca0, ca0, t0`. Code is at `0x80000000`, `tohost` at `0x80001000`.
- Name `trap-*`: expected to trap (htif exit 1); else expected to fall through to `pass` (exit 0).
- Expected divergence: an entry in `validation/cases/divergences.toml` (below); the deltas from `statefile.py delta` of the two runs' states at steps n − 1 and n (`statefile.py split`).
- Check the program does what it says: `cheriot_sim_dump ... --trace validation/build/validate/<name>/prog.elf` (disassembly, register writes).
- New mnemonic: `mk-test-elf.py`; standard ones checked against llvm-mc by `validation/scripts/check-encodings.sh` (61 words equal); CHERIoT ones from the `encdec` clauses in cheriot-sail `src/cheri_insts.sail`, checked by the emulator's disassembly in `--trace`.

## Prologue and epilogue

```
cla c15, trap             # auipcc c15, 0; cincaddrimm c15, c15, trap - .
cspecialw mtcc, c15       # 3 steps
<case>
pass:  tohost := 1, tohost+4 := 0     # exit 0; 6 steps
trap:  tohost := 3, tohost+4 := 0     # exit 1 (MTCC)
```

## Cases

| case | covers |
|---|---|
| `addi` | ADDI: ±, extreme immediates, `x0` destination |
| `lui` | LUI: low, sign bit, all ones, 0; LUI + ADDI |
| `add` | ADD/SUB: signed overflow, carry out, negative |
| `mul` | MUL, MULH, MULHU, MULHSU, DIV(U)/REM(U): by zero, `INT_MIN / -1` |
| `beq` | BEQ not taken, taken forward; BNE backward loop; BLT not taken; BGEU taken |
| `jal` | CJAL into `cra` (sentry), `ct0` (unsealed), `cnull` (J); return by CJALR through the sentry |
| `lw-sw` | SW/SH/SB, LW/LH/LHU/LB/LBU, sign/zero extension, misaligned word |
| `csrrw` | CSRRW/S/C and immediate forms on `mscratch`, `mtval`, `mstatus`; read `minstret`, `misa` |
| `cincaddr` | CIncAddr(Imm), CSetAddr: root, bounded in/out of bounds, unrepresentable (tag cleared), null |
| `csetbounds` | CSetBounds(Imm/Exact): exact, rounded, inexact exact (tag cleared), beyond parent, null; CGetBase/Len/Top/Tag/Perm |
| `clc-csc` | CSC/CLC: tag kept; SW into a granule clears it; untagged load; store null |
| `cjalr` | CJALR: call into `cra` (sentry), sentry return, link into `ct1` (unsealed), jump with offset |
| `trap-null-store` | SW through null `ca0`: CHERI tag violation, `MTCC` handler |
| `trap-illegal` | encoding 0: illegal instruction, `MTCC` handler |
| `trap-rvc` | C absent: `C.ADDI` (`0x0585`) traps illegal on both sides |
| `trap-zb` | Zb\* absent: `ANDN` (`0x40c5f533`) traps illegal on both sides |
| `rv32e-rd` | RV32E: `x16` as destination (expected divergence) |
| `rv32e-rs` | RV32E: `x31` as source (expected divergence) |

## Known divergences

- One table: `validation/cases/divergences.toml`, one entry per case: step, µSail's stop and delta at that step, the emulator's delta and htif exit code, reason, doc.
- `validate.py` asserts it: `expected divergence at step n` only if the states are equal up to n − 1 and both deltas at n, Rocq's stop and the emulator's exit are exactly the recorded ones; anything else (passes, other step, other values) fails.
- Delta at n: lines of the state at n that differ from n − 1. µSail's failure state is its state when `stm_fail` ran (`nextPC` already set by `step`).
- Not divergences (excluded on both sides, ordinary `trap-*` cases): C (`sys_enable_rvc = false` / `--disable-compressed`), Zb\*/Zbk\* (`haveZb* = false` / emulator built without Zb\*).
- No cases: PMP, Sv32 (excluded).

Rendered by `validate.py --list-divergences`:

| case | what | step | µSail | emulator | reason | doc |
|---|---|---|---|---|---|---|
| `rv32e-rd` | `addi x16, zero, 1`: RV32E register >= 16 as destination | 5 | Failed: `instbits 0x00100813`, `nextPC 0x80000014`, `step_result "failure"` | exit 1: `MEPCC.address 0x80000010`, `PC 0x80000030`, `PCC.address 0x80000030`, `instbits 0x00100813`, `mcause 0x00000002`, `mepc 0x80000010`, `nextPC 0x80000030`, `nextPCC.address 0x80000030` | `Error_not_rv32e_register` is thrown; µSail has no exceptions (`throw` = `stm_fail`, `try` handler dropped), Sail's `try_execute` catches it and traps illegal | [design.md#exceptions-fail](../../case-studies/cheriot/docs/design.md#exceptions-fail) |
| `rv32e-rs` | `add a2, a1, x31`: RV32E register >= 16 as source | 5 | Failed: `instbits 0x01f58633`, `nextPC 0x80000014`, `step_result "failure"` | exit 1: `MEPCC.address 0x80000010`, `PC 0x80000030`, `PCC.address 0x80000030`, `instbits 0x01f58633`, `mcause 0x00000002`, `mepc 0x80000010`, `nextPC 0x80000030`, `nextPCC.address 0x80000030` | as `rv32e-rd` | [design.md#exceptions-fail](../../case-studies/cheriot/docs/design.md#exceptions-fail) |

## Results

| case            | steps | stop   | exit | result                        |
|-----------------|-------|--------|------|-------------------------------|
| add             | 19    | Halted | 0    | pass                          |
| addi            | 15    | Halted | 0    | pass                          |
| beq             | 23    | Halted | 0    | pass                          |
| cincaddr        | 25    | Halted | 0    | pass                          |
| cjalr           | 21    | Halted | 0    | pass                          |
| clc-csc         | 26    | Halted | 0    | pass                          |
| csetbounds      | 31    | Halted | 0    | pass                          |
| csrrw           | 22    | Halted | 0    | pass                          |
| jal             | 17    | Halted | 0    | pass                          |
| lui             | 15    | Halted | 0    | pass                          |
| lw-sw           | 27    | Halted | 0    | pass                          |
| mul             | 27    | Halted | 0    | pass                          |
| rv32e-rd        | 5     | Failed | 1    | expected divergence at step 5 |
| rv32e-rs        | 5     | Failed | 1    | expected divergence at step 5 |
| trap-illegal    | 11    | Halted | 1    | pass                          |
| trap-null-store | 11    | Halted | 1    | pass                          |
| trap-rvc        | 11    | Halted | 1    | pass                          |
| trap-zb         | 11    | Halted | 1    | pass                          |

- 16/18 pass, 2 expected divergence, 0 fail.
- `trap-rvc`, `trap-zb`: same delta at the trap on both sides (`mcause 2`, `mepc`, `MEPCC`, `PC`/`PCC` at the handler, `mtval 0`); C and Zb\* are absent alike, no divergence.
- No translator bug found by these cases.
- Checked that failures are reported (temporary changes, not kept):
  - `self: j self`, `# limit: 30`: both stop after 30 steps, states equal; `BAD CASE: exit -, expected 0`.
  - `divergences.toml` changed: step 4 or 6 → `FAIL: diverges at step 5, expected at step 4`; a delta line → `FAIL: rocq delta at step 5` (`divergence.txt` shows it); `emulator.exit` → `emulator exit 1, expected 0`; `rocq.stop` → `Rocq stops Failed after step 5, expected Halted`; entry for passing `addi` → `FAIL: no divergence, expected one at step 3`; entry without a case or with missing fields → error before any run.

## Timings

| what | time |
|---|---|
| Rocq, cold (`--no-cache`, `-j 4`, 8 cores) | 42 s |
| per case, coqc | 7–15 s (most of it printing ~10k lines with Ltac2) |
| cached rerun (`gmake validate`, everything built) | 1.8 s |
| emulator, all cases | < 1 s |
| emulator build, cold / relink after a patch change | ~10 min / 1 s |

## Open

- **Expected exit code by name** (`trap-*` → 1). Options: (a) name prefix; (b) a `# expect: trap` header. Recommendation: (a) while there are few cases. Taken: (a).
- **Printing cost.** coqc time is mostly Ltac2 printing of full states. Options: (a) keep; (b) Rocq prints deltas, Python rebuilds full states (`statefile.py delta` is exact: registers never disappear, memory accumulates). Recommendation: (a) until the suite grows (riscv-tests traces); then (b).
- **Not covered yet**: interrupts, `mret`/`ecall`, U-mode, sealing (`CSeal`/`CUnseal`), `AUICGP`, PCC bounds faults.
- **Divergence granularity.** Options: (a) exact deltas at the divergence step (taken); (b) only `step_result` and `mcause`. (a) also catches a change in µSail's partial state at the failure (e.g. `nextPC`), at the cost of updating the entry when the prologue changes addresses. Recommendation: (a). Taken: (a).
