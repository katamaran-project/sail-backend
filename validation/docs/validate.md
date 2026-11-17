# Translation validation

One command runs short programs in the generated µSail model (a Rocq interpreter) and in Sail's C emulator, and compares the full state after reset and after every step.
Design: [design.md](design.md). State format: [state-format.md](state-format.md).

## Running

```sh
gmake validate CHERIOT_SAIL_DIR=/path/to/cheriot-sail                 # from the repository root
gmake validate VALIDATE_ARGS='addi mul -j 8'                          # some cases
validation/scripts/validate.py [CASE...] [options]                     # the same, once built
```

| option | effect |
|---|---|
| `CASE...` | names in `validation/cases` (default all) |
| `-j N` | parallel Rocq runs |
| `--no-cache` | run Rocq even if cached |
| `--cache-dir DIR` | default `$SAIL_BACKEND_VALIDATE_CACHE`, else `~/.cache/sail-backend/validate-cache` |
| `--list-divergences` | print `divergences.toml` as a Markdown table |

- `gmake validate` first builds what is missing: the emulator (about 10 minutes), the model (`base.v`, `machine.v`, about 2 minutes), the Rocq runner.
  After a translator change, regenerate the model: `gmake -C validation/rocq model CHERIOT_SAIL_DIR=...`.
- `VALIDATE_CHERIOT=DIR`: the cheriot-sail checkout to build the emulator from (default `$CHERIOT_SAIL_DIR`, else `../cheriot-sail`).
- Cached: about 2 s. Cold: about 40 s at `-j 4`, mostly Rocq printing the states.
- Exit status 1 if a case neither passes nor diverges as recorded.

## Stages, per case

1. **assemble**: `validation/cases/lib/prologue.s` + the case + `epilogue.s` into an ELF (`validation/scripts/mk-test-elf.py`; no cross toolchain).
2. **emulator**: `cheriot_sim_dump --disable-compressed -F -W --insns-per-tick 1000000000 --inst-limit L --dump-state emu.run prog.elf`.
3. **Rocq**: a generated `Run.v` (the program's words, the limit) runs `Validate.v`: reset, then up to `L` steps, printing every state.
4. **compare**: the first step whose states differ, as a `diff -u`.

- Both sides stop at the same step: the run ends via `tohost`, or after `L` steps (200, or `# limit: N` in the case). The emulator patch counts every step, retired or not.
- A trap jumps to the epilogue's handler, which ends the run; without it, the reset `MTCC` (address 0) fetch-faults forever.

## Results

| column | meaning |
|---|---|
| `steps` | steps run in Rocq |
| `stop` | why the Rocq run ended: `Halted` (`tohost`), `Failed` (`stm_fail`), `StepLimit`, `OutOfFuel`, `NoAnswer` |
| `exit` | the emulator's exit code (0 pass, 1 trap, `-` none) |
| `result` | below |

- `pass`: every state equal, and the expected exit code (`trap-*` cases 1, others 0).
- `expected divergence at step n`: diverges exactly as recorded ([below](#known-divergences)).
- `FAIL at step n`: the first differing state; `diff.txt` holds the diff.
- `FAIL: ...`: a recorded divergence that did not happen as recorded; `divergence.txt` holds both.
- `BAD CASE`: both sides agree, but the program did not end as intended.
- `ERROR`: assembler, emulator or `coqc` error.

## Cases

| case | covers |
|---|---|
| `addi` | ADDI: signs, extreme immediates, `x0` destination |
| `lui` | LUI; LUI + ADDI |
| `add` | ADD/SUB: overflow, carry |
| `mul` | MUL, MULH(U/SU), DIV(U)/REM(U): by zero, `INT_MIN / -1` |
| `beq` | BEQ, BNE, BLT, BGEU: taken and not, forward and backward |
| `jal` | CJAL into `cra` (sentry), `ct0`, `cnull`; return by CJALR |
| `lw-sw` | SW/SH/SB, LW/LH(U)/LB(U), extension, misaligned word |
| `csrrw` | CSRRW/S/C and immediate forms; `minstret`, `misa` |
| `cincaddr` | CIncAddr(Imm), CSetAddr: in and out of bounds, unrepresentable, null |
| `csetbounds` | CSetBounds(Imm/Exact): exact, rounded, beyond parent, null; CGet\* |
| `clc-csc` | CSC/CLC: tag kept, cleared by a data store, untagged load |
| `cjalr` | CJALR: sentry call and return, link register, offset |
| `trap-null-store` | store through null: tag violation |
| `trap-illegal` | encoding 0 |
| `trap-rvc` | `C.ADDI`: compressed absent, traps illegal |
| `trap-zb` | `ANDN`: Zb\* absent, traps illegal |
| `rv32e-rd`, `rv32e-rs` | RV32E register >= 16 as destination, as source (expected divergences) |

### Adding a case

- `validation/cases/<name>.s`: the body only; a `#` comment says what it covers. Name it `trap-*` if it ends in a trap.
- Registers `x0`–`x13`; `c14`, `c15` belong to the prologue and epilogue.
- Integer and capability registers are the same (`a2` is `ca2`): keep integers and capabilities in different registers.
- Memory needs a tagged capability: `cspecialr ca0, mtdc` (the root, address 0), then `csetaddr`. Code at `0x80000000`, `tohost` at `0x80001000`.
- New mnemonics go in `mk-test-elf.py`; `validation/scripts/check-encodings.sh` checks the standard ones against `llvm-mc`.
- `--trace` on `validation/build/validate/<name>/prog.elf` shows what the program does.

## Known divergences

- `validation/cases/divergences.toml`, one entry per case: the step, each side's changes at that step (lines of the state format), Rocq's stop and the emulator's exit code, the reason.
- `expected divergence` only if the states are equal before that step and everything at it is exactly as recorded.

| case | step | µSail | emulator | reason |
|---|---|---|---|---|
| `rv32e-rd` | 5 | `stm_fail` | illegal-instruction trap | `Error_not_rv32e_register` is thrown; µSail has no exceptions ([design](../../case-studies/cheriot/docs/design.md#exceptions-fail)) |
| `rv32e-rs` | 5 | `stm_fail` | illegal-instruction trap | as `rv32e-rd` |

## Rocq side

- `validation/rocq/`: `Interp.v` (interpreter), `Harness.v` (foreign calls, start state, runs), `Dump.v`/`Names.v` (state format), `Validate.v` (runner); `InterpTests.v`, `StepADDI.v`, `DumpADDI.v` check them.
- `gmake -C validation/rocq [all|cheriot|dumps|validate|model]`; the model in `validation/build/model`.
- Fuel bounds nesting depth, not steps: `init_model` needs 65, one step about 50; the runner uses 1000.

```coq
exec : Oracle -> nat -> Stm Γ τ -> State -> CStore Γ -> Outcome (Result τ * State)
Inductive Outcome A := Done (a : A) | OutOfFuel | NoAnswer Δ σ (f : 𝑭𝑿 Δ σ).
```

| foreign call | answer |
|---|---|
| `__read_mem`, `__write_mem` | byte map at the physical address, little-endian; unwritten bytes 0 |
| `MEMr_tag`, `MEMw_tag` | tag map; unwritten tags `false` |
| externs with `semantics_*` (`bitvector_update`, `pow2`, ...) | that term |
| `unconstrained σ` | the type's generated inhabitant |
| `plat_*`, `sys_*` | the emulator's defaults (`riscv_platform_impl.c`), with `-F -W` |
| `get_16_random_bits` | 0 |
| barriers, `__write_mem_ea`, `plat_term_write` | no effect |

- The model's `Memory` is `Z -> Z`: bytes at their address, tags at negative keys, and a log of written keys above `2^40`, from which the dump lists the writes.

## Emulator

```sh
gmake -C validation/emulator CHERIOT=/path/to/cheriot-sail       # validation/build/emulator/cheriot_sim_dump
gmake -C validation/emulator check                               # compare with the Rocq demos
```

- Built at `-O1` (`C_OPT_FLAGS`): cheriot-sail's default `-O2 -flto` spends over 30 minutes in Apple's linker.
- Zb\* is left out at build time (`haveZb*()` is hard-coded `true` in the Sail source); nothing in the clone is edited, its Makefile is queried (`query.mk`).
- `dump_state.sail`: prints every register, capabilities by record field. Generated by `gen-dump.py` from the Rocq reset state's register names (`gmake dump-sail`).
- `riscv_sim.patch`, applied at build time: `--dump-state FILE`, `--insns-per-tick N`, and `--inst-limit` counting every step.
- `dump_state.c`: memory and tag writes, logged by wrapping the platform's write functions.

## Output

- `validation/build/validate/<case>/`: `prog.s`, `prog.elf`, `emu.run`, `Run.v`, `rocq.run`, `diff.txt`, `divergence.txt`; the table in `validation/build/validate/results.md`.
- Rocq results cached by the hash of `Run.v`, the model and the Rocq files; the emulator always runs (milliseconds).

## Limitations

- Not covered: interrupts, `mret`/`ecall`, U-mode, sealing, `AUICGP`, PCC bounds faults.
- The interpreter's soundness is not proved.
- `unconstrained` answers the type's inhabitant (`true` for `bool`); the emulator's undefined values are 0 or `false`. No case reads one.
