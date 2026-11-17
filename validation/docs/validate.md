# gmake validate

Hand-picked programs run from reset in µSail (Rocq interpreter) and Sail's C emulator; the full state after reset and after every step compared (`state-format.md`).
Design: [`design.md`](design.md).

## Run

```sh
opam exec --switch=sail-backend -- gmake validate
opam exec --switch=sail-backend -- gmake validate VALIDATE_ARGS="addi mul -j 8"
validation/scripts/validate.py [CASE...] [-j N] [--no-cache]   # once built
```

- `gmake validate` builds what is missing, then runs `validation/scripts/validate.py`:
  - emulator: `gmake -C validation/emulator` (~10 min cold; up to date: nothing).
  - model: `gmake -C validation/rocq model` only if `validation/build/model/base.v` is missing; refresh it by hand after a translator change.
  - Rocq: `gmake -C validation/rocq validate` (`Validate.vo` and what it needs).
- `VALIDATE_CHERIOT=DIR`: the cheriot-sail clone (default `$CHERIOT_SAIL_DIR`, else `../cheriot-sail` next to the checkout; the emulator was built from it).
- Exit status 1 if a case does not pass.
- Output per case in `validation/build/validate/<case>/`: `prog.s`/`prog.elf` (assembled), `emu.run`/`emu.log`, `Run.v`/`rocq.run`/`coqc.log`, `diff.txt` (first differing step). Table also in `validation/build/validate/results.md`.

## Table

```
| case | steps | stop | exit | result | Rocq |
```

- `steps`: steps run in Rocq (states − 1).
- `stop`: why the Rocq run ended: `Halted` (htif), `Failed` (`stm_fail`), `StepLimit`, `OutOfFuel`, `NoAnswer`.
- `exit`: the emulator's htif exit code (`-`: none, e.g. step limit).
- `result`:
  - `pass`: every state equal, same number of states, expected exit code.
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

## Results

| case            | steps | stop   | exit | result |
|-----------------|-------|--------|------|--------|
| add             | 19    | Halted | 0    | pass   |
| addi            | 15    | Halted | 0    | pass   |
| beq             | 23    | Halted | 0    | pass   |
| cincaddr        | 25    | Halted | 0    | pass   |
| cjalr           | 21    | Halted | 0    | pass   |
| clc-csc         | 26    | Halted | 0    | pass   |
| csetbounds      | 31    | Halted | 0    | pass   |
| csrrw           | 22    | Halted | 0    | pass   |
| jal             | 17    | Halted | 0    | pass   |
| lui             | 15    | Halted | 0    | pass   |
| lw-sw           | 27    | Halted | 0    | pass   |
| mul             | 27    | Halted | 0    | pass   |
| trap-illegal    | 11    | Halted | 1    | pass   |
| trap-null-store | 11    | Halted | 1    | pass   |

- 14/14 pass: every state equal (304 states, 141839 lines; 465 register lines per state plus `mem`/`tag` lines).
- Failures: none. No translator bug found by these cases.
- Checked that failures are reported (temporary cases, not kept):
  - `addi x16, zero, 1` (RV32E): `FAIL at step 5 (Rocq stm_fail "failure")`, diff shows the emulator's illegal-instruction trap (`mcause 2`, `mepc`, `PC`). Expected divergence; its asserted case is the next task.
  - `self: j self`, `# limit: 30`: both stop after 30 steps, states equal; `BAD CASE: exit -, expected 0`.

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
- **Not covered yet**: interrupts, `mret`/`ecall`, U-mode, sealing (`CSeal`/`CUnseal`), `AUICGP`, PCC bounds faults, RV32E registers >= 16 (expected divergence, next task).
