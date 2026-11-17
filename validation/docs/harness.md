# CHERIoT harness

Runs the generated CHERIoT µSail model in the interpreter (`interp.md`): oracle for its foreign functions, initial state, one `step`.

- `validation/rocq/Harness.v`: `oracle` (with the memory write log), memory/tag helpers, platform constants, `reg_default`, `reset`, `run_step`, `run_steps`.
- `validation/rocq/StepADDI.v`: `ADDI x1, x0, 5` at the entry, `init_model` then `step`; runs that trap-loop and that halt; checked by `vm_compute`.
- `validation/build/` (gitignored): `cheriot-run/` (run.py output), `model/` (`base.v`, `machine.v`, `.vo`).

## Build

```sh
cd validation/rocq
opam exec --switch=sail-backend -- gmake model CHERIOT_SAIL_DIR=~/phd/PROJECTS/cheriot-sail   # regenerate the model
opam exec --switch=sail-backend -- gmake cheriot                                            # model .vo, Harness, StepADDI
```

- `model`: `dune build`, then `tests/cheriot/run.py --no-coq` (same `scope.toml`, configuration, templates as `gmake cheriot`); copies `base.v`/`machine.v` into `build/model` only if changed.
- run.py appends to the tracked `case-studies/cheriot/docs/history.csv`: the rule restores it.
- Model compiled with plain `coqc -R . CheriotModel` (no stall limit); `validation/rocq` files use `-R ../build/model CheriotModel`:

```coq
From CheriotModel Require Import base machine.
Import UntitledBase ModelProgram.
Module Import I := MakeInterp UntitledBase ModelProgram.
```

## Memory

- The model's `Memory` is `Z -> Z` (base.v, `MemoryModel`, a `TODO` placeholder).
- Bytes: key = physical address, value in `[0, 256)`, little-endian.
- Tags: key = `-1 - tag address` (negative), value 0/1.
- Unwritten: byte 0, tag false (as the emulator).
- Program: `load_word mem_empty c_entry w`, `load_words mem_empty c_entry ws` (not logged).
- Write log: the oracle appends every written key to a log kept in the memory itself, above every physical address: key `2^40` = count, key `2^40 + 1 + i` = i-th key (byte address, or tag key). `log_keys m` lists them; the dump's `mem`/`tag` lines come from it (`state-format.md`).

## Foreign functions

| `FunX` | answer |
|---|---|
| `unconstrained σ` | the generated `Unconstrained` inhabitant (zeros, first constructor; `true` for `bool`) |
| `emod_int`, `pow2`, `min_int`, `count_leading_zeros`, `count_trailing_zeros`, `sail_sign_extend`, `sail_zero_extend`, `truncateLSB`, `bitvector_access`, `sail_zeros`, `get_slice_int`, `quot_round_zero`, `rem_round_zero` | generated `semantics_*` term |
| `bitvector_update` | harness: bit `i` of `x` set to `b` (no `semantics_*` generated) |
| `__read_mem(_, _, addr, w)` | `w` bytes at `addr` |
| `__write_mem(_, _, addr, w, data)` | writes `w` bytes, logs them; `true` |
| `__write_mem_ea`, `sail_barrier`, `cancel_reservation`, `plat_term_write` | `tt`, no effect |
| `MEMr_tag(a)` / `MEMw_tag(a, t)` | tag map; writes logged |
| `get_16_random_bits` | `0` (emulator: `/dev/urandom`) |
| platform settings | constants below |
| none | no `None` answers (never `NoAnswer`) |

Used by `init_model`: `unconstrained`, `pow2`, `sail_zero_extend`, `get_slice_int`, `sys_enable_fdext`, `sys_enable_zfinx`, `sys_enable_vext`.
Used by `step` on ADDI: `unconstrained`, `sail_sign_extend`, `sail_zero_extend`, `get_slice_int`, `__read_mem`, `elf_tohost`, `plat_{ram,rom,clint,uart}_{base,size}`.

## Constants

The validation emulator: `cheriot_sim_nozb --disable-compressed -F -W` (`emulator.md`), no `--ram-size`, no `-m`/`-i`/`--pmp-count`, built `NO_BOOT_ROM`.
Sources in `cheriot-sail/sail-riscv/c_emulator/` (`impl` = `riscv_platform_impl.c`, `sim` = `riscv_sim.c`); each foreign function returns the `rv_*` variable (`riscv_platform.c`).

| function | value | source |
|---|---|---|
| `plat_ram_base` / `plat_ram_size` | `0x80000000` / `0x4000000` | impl:21–22 |
| `plat_rom_base` / `plat_rom_size` | `0x1000` / `0x100` | impl:24–25 (boot ROM off: sim:89, so `init_sail_reset_vector` never changes them) |
| `plat_clint_base` / `plat_clint_size` | `0x2000000` / `0xc0000` | impl:42–43 |
| `plat_uart_base` / `plat_uart_size` | `0x10000000` / `0x100` | impl:45–46 |
| `elf_tohost` | `0x80001000` | ELF symbol `tohost` (sim:477; mk-test-elf.py); impl:48 default the same |
| `sys_enable_writable_misa` | `true` | impl:12 |
| `sys_enable_fdext` | **`false`** | impl:13 `true`; `-F`: sim:338–340 |
| `sys_enable_zfinx` | `false` | impl:9 |
| `sys_enable_writable_fiom` | `true` | impl:19 |
| `sys_enable_vext` | **`false`** | impl:14 `true`; `-W`: sim:342–344 |
| `plat_enable_misaligned_access` | `true` | impl:17 |
| `plat_mtval_has_illegal_inst_bits` | `false` | impl:18 |
| `get_16_random_bits` | `0` | impl:27 reads `/dev/urandom` (only the Zkr `seed` CSR; not reachable in the core) |

- Changed: `sys_enable_fdext`, `sys_enable_vext` (`true` → `false`). Effect: `misa` after reset `0x40a0103a` → `0x40801012` (as the emulator). StepADDI/DumpADDI expectations unchanged (no checked item depends on F/D/V).
- Value exclusions in `tests/cheriot/scope.toml`, not foreign: `sys_enable_rvc` = `false` (impl:10 `true`; `-C`: sim:326–328; sim:650 also clears `misa.C` after `init_model`, which the model's `init_model` already does), `sys_pmp_count` = 0 (impl:6), `haveZb*` = `false` (emulator: built without Zb\*).
- Not in the model (not foreign): `plat_enable_dirty_update`, `sys_enable_next`, `sys_pmp_grain`, `plat_insns_per_tick`.
## Initial state

- Registers: `reg_default`, as the emulator before `init_model`: bitvectors 0, bools `false`, enums the first constructor, records field by field (`zero_val` tactic).
  - Emulator (Sail C, `model_init`): `initialize_registers` sets every register without an initialiser to its `undefined_*` value, which Sail's C backend compiles to 0 / `false` / the first enum constructor (`undefined_Privilege` = `User`); register initialisers: `UART_DLAB = 0b0`, `tlb32 = None` (not in the model). Read in the generated C (`sail -c` with the nozb build's command, `riscv_model_RV32.c`: `model_init`, `zinitializze_registers`).
  - All model registers match; no int registers. Before: bools `true` (the `Unconstrained` inhabitant), no effect on any dumped item (all bools are set by `init_model`).
- `reset_from : nat -> State -> Outcome (Result unit * State)`: `init_model`, then `PC := 0x80000000` if it did not fail (emulator: `init_sail`, `zPC = elf_entry` after `zinit_model`). `reset fuel m` = `reset_from fuel (state0 m)`.
- `run_step_at fuel k st`: `step(k)`; `run_step` = `run_step_at _ 0`. The emulator passes the number of earlier steps that returned `true` (only printed).

## Runs

- `run_steps fuel n st : Outcome (list (Result bool * State) * Stop)`: up to `n` steps, as the emulator's loop (`while (!htif_done ...)`, sim:889), without `tick_clock`.
- `Stop`: `Halted` (`htif_done` after a step), `Failed` (a step returned `inl`), `StepLimit` (`n` steps).
- Every step counts, also `false` ones; so does `cheriot_sim_dump`'s `--inst-limit` (patched; upstream counts only `true` steps and never stops a fetch-fault loop). Same `n` on both sides: same last step.
- Termination: fuel bounds each step's depth, `n` the number of steps, so every run ends with `Done`, `OutOfFuel` or `NoAnswer`; no hang.
- Trap with `MTCC` = 0 (reset value, outside memory): the trapping instruction returns `true` (decoded), then every fetch faults (`mcause` 1, step `false`, PC stays 0) until `StepLimit`.
## StepADDI

- `0x00500093` at `0x80000000`; after `step`: result `true`, `PC = 0x80000004`, `x1.address = 5`, `x1.tag = false`, `minstret = 1`.
- Also checked: fuel is least (`reset` with 64, `step` with 49 give `OutOfFuel`).
- `trap_loop`: encoding `0` at the entry, 3 steps: `(true, PC 0, mcause 2)`, then `(false, 0, 1)` twice; `StepLimit`.
- `addi_add_sw_halts`: `validation/scripts/examples/addi-add-sw.s` (9 words), limit 20: 9 steps, `Halted`. Same as the emulator (`--trace`: 9 instructions, `mem[0x80001100] <- 0xA`, htif `SUCCESS`).
- Probes (not in the file): `ADDI x1, x0, -3` gives `0xfffffffd`; `ADDI x16, x0, 1` gives `inl "failure"` (RV32E, expected divergence).

## Fuel and timings

| what | time |
|---|---|
| `gmake model` (dune build + run.py `--no-coq`) | 65 s |
| `coqc base.v` | 7 s |
| `coqc machine.v` | 60–85 s (20 s: `fundef_bindfree`) |
| `coqc Harness.v` / `StepADDI.v` | 0.9 s / 1.1 s |
| `vm_compute` `trap_loop` / `addi_add_sw_halts` | 0.01 s / 0.05 s |
| `vm_compute` `init_model` + `step` | first in a file 0.15 s (VM compiles the model's constants); then 0.006 s |

- Fuel (nesting depth, `interp.md`): `init_model` 65, `step` (ADDI) 50.
- Other instructions may need more: use a generous fuel (e.g. 300; cost is depth, not steps), the least only in tests.
- `reg_default`: `exact bv.zero` before the `Unconstrained` hint; the hint's `cbn` on `ty.vregtype` (65536 bits, unary `nat`) ran for minutes.

## Limitations

- `MEMw_tag` changes `Memory`, but the generated `ForeignCall` says `μ' = μ` for it: the oracle is not `OracleSound` for tag writes (translator, Open).
- The write log adds keys `>= 2^40` on `__write_mem`/`MEMw_tag`: a `ForeignCall` that fixes `μ'` exactly would not allow them either (same Open; harmless for execution, no read reaches them).
- `unconstrained` has no name of the undefined value it stands for: always the inhabitant (`true` for `bool`); the emulator's `undefined_*` give 0/`false`. No case seen yet where `step` reads one.
- No `tick_clock`/`tick_platform` every 100 steps (`state-format.md`, Open).
## Open

- **Tag writes vs `ForeignCall`.** Options: (a) tags in `Memory` (negative keys), translator relaxes `MEMw_tag` to allow `μ'` to change, as `__write_mem`; (b) drop tag writes (sound, but a later `MEMr_tag` is wrong); (c) give the model a `Memory` record of bytes and tags.
  Recommendation: (a) now, (c) when the memory model is written. Taken: (a), harness side only.
- **history.csv.** run.py always appends; the `model` rule saves and restores it. Options: (a) keep; (b) a `--no-history` option in run.py. Recommendation: (b), small run.py change. Taken: (a).
- **`unconstrained` for `bool`.** Inhabitant `true`, emulator `false`. Options: (a) keep; (b) the oracle answers `false` for `unconstrained ty.bool` (and 0 etc., as Sail C). Recommendation: (b) if a divergence shows up; undefined values stay universally quantified in the translation (user ruling), the oracle only picks one. Taken: (a).
