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

- `gmake validate` first builds what is missing: the emulator (about a minute), the model (`base.v`, `machine.v`, about 2 minutes), the Rocq runner.
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
- A Sail assertion or uncaught exception stops both sides inside a step: µSail `stm_fail` (Rocq `Failed`, the step's state printed), the emulator exits without a state for that step ([below](#both-sides-stop)).
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
- `both stop at step n`: both sides stop in step n exactly as recorded ([below](#both-sides-stop)).
- `FAIL at step n`: the first differing state; `diff.txt` holds the diff.
- `FAIL: ...`: a recorded divergence or stop that did not happen as recorded; `divergence.txt` holds both.
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
| `cap-misc` | CMove, CClearTag, CGetAddr, CGetType, CGetHigh, CSetHigh (incl. garbage high bits), CSEQX, CSub, CTestSubset |
| `cap-perm-bounds` | CAndPerm (re-encoded by CGetHigh), CRRL, CRAM, CSetBoundsRoundDown: exact, rounded, zero, beyond parent, null |
| `seal` | CSeal/CUnseal by the sealing root; refused: wrong otype, already sealed, no SE/US, untagged, sealed or out-of-bounds authority; sealed operands to other ops |
| `sentry` | CSeal into otypes 1-3; CJALR through them (call, tail call, `MIE` set and cleared), return through the backward sentry |
| `auicgp` | AUICGP: null `cgp`, positive and negative offset, unrepresentable, sealed |
| `trap-sentry-tail` | CJALR into `cnull` through an interrupt-disabling sentry: seal violation |
| `trap-sentry-offset` | CJALR through a sentry with a nonzero offset: seal violation |
| `trap-sealed-load` | LW through a sealed capability: seal violation |
| `csr-cheriot` | `mccsr`; `mshwm`, `mshwmb` rounding; stores in and outside `[mshwmb, mshwm)`; `mtvec` read |
| `csr-misc` | writes of `misa`, `mie`, `mip`, `mstatus` (legalized), `mcountinhibit`, `mcycle(h)`, `minstreth`; seed CSR; `mepc` read; machine IDs |
| `csrw-mtvec`, `csrw-mepc` | CSR write of `mtvec`, `mepc`: `internal_error` (both sides stop) |
| `mmio` | CLINT, UART, HTIF loads and stores; HTIF putchar |
| `trap-csr-asr` | CSR reads allowed and a write trapping with ASR cleared in PCC |
| `trap-medeleg` | `medeleg`: not defined without U, S, N; traps illegal |
| `trap-msip` | software interrupt: CLINT `msip`, `mie.MSIE`, `mstatus.MIE` |
| `trap-mtimer` | timer interrupt: CLINT `mtimecmp` <= `mtime`, `mie.MTIE` |
| `trap-clint-load` | load from an unmapped CLINT offset: load access fault |
| `ecall-mret` | ECALL, EBREAK to the case's own handler; `mcause`, `mepc`, `mtval`, `mtvec`; return by MRET; MIE/MPIE |
| `mret` | MRET to a written MEPCC, no trap: MIE := MPIE, MPIE := 1, MPP := M |
| `system` | FENCE (every pred/succ arm), FENCE.TSO, FENCE.I, reserved fences, SFENCE.VMA in M, WFI (mtime := mtimecmp) |
| `no-asr` | PCC without ASR: counter reads; CSR access, counter write, CSpecialR, MRET trap |
| `misaligned-jump` | BEQ, CJAL, CJALR to a target not 4-byte aligned: trap; CJALR clears bit 0 |
| `cap-faults` | CJALR, CLC, CSC: untagged, sealed, sentry kind, permissions, bounds; misaligned CLC/CSC |
| `mem-faults` | LW, SW, LB, CLC, CSC outside RAM, byte store to the CLINT: access faults |
| `trap-sret` | SRET in M without S-mode: illegal |
| `trap-mret-untagged` | MEPCC written with bit 0 set (tag cleared), MRET: fetch tag violation on PCC |
| `trap-pcc-bounds` | fetch past the end of a bounded PCC: bounds violation |
| `trap-branch-bounds` | taken branch outside PCC bounds: the fetch faults |
| `umode`, `untagged-mtcc` | MRET into U with `misa.U` = 0; trap to an untagged MTCC (both sides stop) |

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
- `validate.py --list-divergences` prints the whole table.

| case | step | µSail | emulator | reason |
|---|---|---|---|---|
| `rv32e-rd` | 5 | `stm_fail` | illegal-instruction trap | `Error_not_rv32e_register` is thrown; µSail has no exceptions ([design](../../case-studies/cheriot/docs/design.md#exceptions-fail)) |
| `rv32e-rs` | 5 | `stm_fail` | illegal-instruction trap | as `rv32e-rd` |

### Both sides stop

A Sail assertion or uncaught exception in a step: µSail `stm_fail`s (Rocq `Failed`, its state for the step has `step_result "<message>"`), the emulator prints a message and exits without a state for that step and without an htif exit code.
Recorded in `divergences.toml` too, with `emulator.stop` and `emulator.message` in place of `emulator.exit` and `emulator.delta`:

```toml
[umode]
step = 11
rocq.stop = "Failed"
rocq.delta = ['step_result "invalid current privilege"']
emulator.stop = "assertion"              # or "exception"
emulator.message = "invalid current privilege"
```

| `emulator.stop` | the emulator's output |
|---|---|
| `assertion` | `Assertion failed: <msg>` (Sail runtime, `sail_assert`) |
| `exception` | `Exiting due to uncaught exception: <msg>`, `<msg>` the `throw`'s location (`riscv_sim.c`) |

- `both stop at step n` only if the states are equal up to step n - 1; Rocq stops `Failed` after step n with the recorded delta; the emulator has states up to n - 1 only, no htif exit code, and an output line of the recorded kind whose `<msg>` contains `emulator.message`.
- Unrecorded, the same run is `FAIL at step n`, with both messages.

| case | step | µSail `step_result` | emulator | reason |
|---|---|---|---|---|
| `csrw-mtvec` | 6 | assertion failed (message dropped) | assertion `cheri_sys_exceptions.sail:144: unsupported` | CHERIoT's `set_mtvec` is `internal_error` |
| `csrw-mepc` | 6 | assertion failed (message dropped) | assertion `cheri_sys_exceptions.sail:117: unsupported` | CHERIoT's `set_xret_target` is `internal_error` (MEPCC is written by CSpecialW) |
| `umode` | 11 | `invalid current privilege` | assertion `invalid current privilege` | first step in U: `dispatchInterrupt` asserts Machine when U-mode is absent |
| `untagged-mtcc` | 7 | `failure` | exception at `riscv_types.sail:88` | PCC and MTCC untagged: `ext_fetch_check_pc` calls `not_implemented` |

- `internal_error`'s message is computed at run time; the translation drops it (`step_result "assertion failed (message computed at run time, dropped by the translation)"`).

## Rocq side

- `validation/rocq/`: `Interp.v` (interpreter), `Harness.v` (foreign calls, start state, runs), `Dump.v`/`Names.v` (state format), `Validate.v` (runner), `InterpCov.v`/`Coverage.v` ([coverage](#coverage)); `InterpTests.v`, `StepADDI.v`, `DumpADDI.v` check them.
- `gmake -C validation/rocq [all|cheriot|dumps|validate|coverage|model]`; the model in `validation/build/model`.
- Fuel bounds nesting depth, not steps: `init_model` needs 65, one step about 50; the runner uses 1000.

```coq
exec : Oracle -> nat -> Stm Γ τ -> State -> CStore Γ -> Outcome (Result τ * State)
Inductive Outcome A := Done (a : A) | OutOfFuel | NoAnswer Δ σ (f : 𝑭𝑿 Δ σ).
```

| foreign call | answer |
|---|---|
| `__read_mem`, `__write_mem` | byte map at the physical address, little-endian; unwritten bytes 0 |
| `MEMr_tag`, `MEMw_tag` | tag map; unwritten tags `false` |
| externs with `semantics_*` | that term |
| `bitvector_update` | computed by the harness |
| `unconstrained σ` | the type's generated inhabitant |
| `plat_*`, `sys_*` | the emulator's defaults (`riscv_platform_impl.c`), with `-F -W` |
| `get_16_random_bits` | 0 (the emulator reads `/dev/urandom`: cases read the seed CSR into `x0`) |
| barriers, `__write_mem_ea`, `plat_term_write` | no effect |

- The model's `Memory` is `Z -> Z`: bytes at their address, tags at negative keys, and a log of written keys above `2^40`, from which the dump lists the writes.

## Emulator

```sh
gmake -C validation/emulator CHERIOT=/path/to/cheriot-sail       # validation/build/emulator/cheriot_sim_dump
gmake -C validation/emulator check                               # compare with the Rocq demos
```

- Cold build: about a minute, mostly compiling the model's C. Sail to C takes seconds once Sail's cache of SMT results (`validation/build/sail_smt_cache`, kept by `gmake clean`) is filled, half a minute before.
  The model's C is built at `-O0` (`MODEL_C_FLAGS`): `-O1` takes about 10 minutes, cheriot-sail's default `-O2 -flto` over 30 in Apple's linker; a run still takes milliseconds.
  The other C files (`C_OPT_FLAGS`, default `-O1 -g`) are compiled once each, not at every link.
- Rebuilds: `dump_state.c` or `riscv_sim.patch` recompiles that file and relinks (under a second); `dump_state.sail` or a cheriot-sail source reruns Sail and recompiles the model's C only if Sail's output changed; `dump_state.h` recompiles the model's C.
- Targets depend on the commands that build them (`build/emulator/*.cmd`, rewritten when a command changes), not on the Makefiles: a changed flag rebuilds what it uses, a comment or a touched Makefile rebuilds nothing.
- Zb\* is left out at build time (`haveZb*()` is hard-coded `true` in the Sail source); nothing in the clone is edited, its Makefile is queried (`query.mk`).
- `dump_state.sail`: prints every register, capabilities by record field. Generated by `gen-dump.py` from the Rocq reset state's register names (`gmake dump-sail`).
- `riscv_sim.patch`, applied at build time: `--dump-state FILE`, `--insns-per-tick N`, and `--inst-limit` counting every step.
- `dump_state.c`: memory and tag writes, logged by wrapping the platform's write functions.

## Output

- `validation/build/validate/<case>/`: `prog.s`, `prog.elf`, `emu.run`, `Run.v`, `rocq.run`, `diff.txt`, `divergence.txt`; the table in `validation/build/validate/results.md`.
- Rocq results cached by the hash of `Run.v`, the model and the Rocq files; the emulator always runs (milliseconds).

## Coverage

Which functions of `machine.v` the cases call.

```sh
gmake validate-coverage                                   # gmake validate, then the coverage runs
gmake validate-coverage VALIDATE_ARGS='addi mul -j 8'
validation/scripts/coverage.py [CASE...] [-j N] [--no-cache] [--cache-dir DIR]   # once built
```

- Each case runs again as in `gmake validate` (reset, then up to its step limit), in `InterpCov.v`: `Interp.v`'s interpreter with call counts threaded through; `Interp.exec` is unchanged.
  `Coverage.v` gives every `Fun` and `FunX` constructor a key and its name, built from the inductive types as in `Names.v`.
- Column `check`: the run ends as the validation run of the same program (stop, steps); otherwise an error.
- `gmake validate` does not build or run any of it.

| counted | how |
|---|---|
| reachable | `Fun`s in the static call graph of `machine.v` (`stm_call` in the bodies) from `step` or `init_model` |
| dead | reachable, but not on this configuration: `DEAD` in `coverage.py`, a pattern and a reason each; an error if a case calls one |
| live | reachable, not dead: the denominator |
| foreign | `FunX`s called from reachable bodies |
| instructions | the live `execute_*` (one per `ast` constructor in scope) |

- Over-counted: functions dead on this configuration but not in `DEAD` (e.g. `haveZmmul`: only in `haveMulDiv() | haveZmmul()`, and `misa.M` stays 1).
- Under-counted: a function is counted once, whatever its widths (no monomorphisation); excluded definitions (`scope.toml`) are not in `machine.v`.
- Function coverage only, no branches: a called function may have arms no case takes. The `←` lists in the report point at such arms: the called functions that call an uncalled one.

Output: `validation/build/coverage/<case>/` (`prog.s`, `prog.elf`, `Cov.v`, `cov.out`: `call <name> <count>`, `foreign <name> <count>`, `end <stop> <steps>`), and `validation/build/coverage/report.md`: totals, per case (functions only that case calls), the live functions not called per area, the dead ones per reason.
Rocq results cached as `gmake validate`'s, by the hash of `Cov.v`, the model and the Rocq files.

## Limitations

- U-mode: reachable only for one step (MRET with MPP = 0), after which the model asserts; its CSR, ECALL and `check_Counteren` paths are unreachable on this configuration (`misa.U` = 0).
- The interpreter's soundness is not proved.
- `unconstrained` answers the type's inhabitant (`true` for `bool`); the emulator's undefined values are 0 or `false`. No case reads one.
