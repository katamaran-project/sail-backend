# CHERIoT harness

Runs the generated CHERIoT µSail model in the interpreter (`interp.md`): oracle for its foreign functions, initial state, one `step`.

- `validation/rocq/Harness.v`: `oracle`, memory/tag helpers, platform constants, `reg_default`, `reset`, `run_step`.
- `validation/rocq/StepADDI.v`: `ADDI x1, x0, 5` at the entry, `init_model` then `step`, checked by `vm_compute`.
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
- Program: `load_word mem_empty c_entry w`.

## Foreign functions

| `FunX` | answer |
|---|---|
| `unconstrained σ` | the generated `Unconstrained` inhabitant (zeros, first constructor; `true` for `bool`) |
| `emod_int`, `pow2`, `min_int`, `count_leading_zeros`, `count_trailing_zeros`, `sail_sign_extend`, `sail_zero_extend`, `truncateLSB`, `bitvector_access`, `sail_zeros`, `get_slice_int`, `quot_round_zero`, `rem_round_zero` | generated `semantics_*` term |
| `bitvector_update` | harness: bit `i` of `x` set to `b` (no `semantics_*` generated) |
| `__read_mem(_, _, addr, w)` | `w` bytes at `addr` |
| `__write_mem(_, _, addr, w, data)` | writes `w` bytes; `true` |
| `__write_mem_ea`, `sail_barrier`, `cancel_reservation`, `plat_term_write` | `tt`, no effect |
| `MEMr_tag(a)` / `MEMw_tag(a, t)` | tag map |
| `get_16_random_bits` | `0` (emulator: `/dev/urandom`) |
| platform settings | constants below |
| none | no `None` answers (never `NoAnswer`) |

Used by `init_model`: `unconstrained`, `pow2`, `sail_zero_extend`, `get_slice_int`, `sys_enable_fdext`, `sys_enable_zfinx`, `sys_enable_vext`.
Used by `step` on ADDI: `unconstrained`, `sail_sign_extend`, `sail_zero_extend`, `get_slice_int`, `__read_mem`, `elf_tohost`, `plat_{ram,rom,clint,uart}_{base,size}`.

## Constants

Source: `sail-riscv/c_emulator/riscv_platform_impl.c` defaults, `riscv_sim.c` (`NO_BOOT_ROM`), `validation/scripts/mk-test-elf.py`.

| function | value | source |
|---|---|---|
| `plat_ram_base` / `plat_ram_size` | `0x80000000` / `0x4000000` | `rv_ram_*` |
| `plat_rom_base` / `plat_rom_size` | `0x1000` / `0x100` | `rv_rom_*` (unused without boot ROM) |
| `plat_clint_base` / `plat_clint_size` | `0x2000000` / `0xc0000` | `rv_clint_*` |
| `plat_uart_base` / `plat_uart_size` | `0x10000000` / `0x100` | `rv_uart_*` |
| `elf_tohost` | `0x80001000` | mk-test-elf.py `tohost` |
| `sys_enable_writable_misa` | `true` | `rv_enable_writable_misa` |
| `sys_enable_fdext` | `true` | `rv_enable_fdext` |
| `sys_enable_zfinx` | `false` | `rv_enable_zfinx` |
| `sys_enable_writable_fiom` | `true` | `rv_enable_writable_fiom` |
| `sys_enable_vext` | `true` | `rv_enable_vext` |
| `plat_enable_misaligned_access` | `true` | `rv_enable_misaligned` |
| `plat_mtval_has_illegal_inst_bits` | `false` | `rv_mtval_has_illegal_inst_bits` |

- `sys_enable_rvc` (false), `sys_pmp_count` (0), `haveZb*` (false): value exclusions in `tests/cheriot/scope.toml`, not foreign.

## Initial state

- Registers: `reg_default`, every register the `Unconstrained` inhabitant (bitvectors `bv.zero`).
- `reset : nat -> Memory -> Outcome (Result unit * State)`: `init_model`, then `PC := 0x80000000` if it did not fail (emulator: `init_sail`, `zPC = elf_entry` after `zinit_model`).
- `run_step : nat -> State -> Outcome (Result bool * State)`: `step(0)`.

## StepADDI

- `0x00500093` at `0x80000000`; after `step`: result `true`, `PC = 0x80000004`, `x1.address = 5`, `x1.tag = false`, `minstret = 1`.
- Also checked: fuel is least (`reset` with 64, `step` with 49 give `OutOfFuel`).
- Probes (not in the file): `ADDI x1, x0, -3` gives `0xfffffffd`; encoding `0` traps (PC = 0, `mcause = 2`); `ADDI x16, x0, 1` gives `inl "failure"` (RV32E, expected divergence).

## Fuel and timings

| what | time |
|---|---|
| `gmake model` (dune build + run.py `--no-coq`) | 65 s |
| `coqc base.v` | 7 s |
| `coqc machine.v` | 60–85 s (20 s: `fundef_bindfree`) |
| `coqc Harness.v` / `StepADDI.v` | 1.4 s / 1.4 s |
| `vm_compute` `init_model` + `step` | first in a file 0.15 s (VM compiles the model's constants); then 0.006 s |

- Fuel (nesting depth, `interp.md`): `init_model` 65, `step` (ADDI) 50.
- Other instructions may need more: use a generous fuel (e.g. 300; cost is depth, not steps), the least only in tests.
- `reg_default`: `exact bv.zero` before the `Unconstrained` hint; the hint's `cbn` on `ty.vregtype` (65536 bits, unary `nat`) ran for minutes.

## Limitations

- `MEMw_tag` changes `Memory`, but the generated `ForeignCall` says `μ' = μ` for it: the oracle is not `OracleSound` for tag writes (translator, Open).
- `unconstrained` has no name of the undefined value it stands for: always the inhabitant; the emulator's undefined values may differ (C: usually zero; `bool`: `false` vs inhabitant `true`).
- Registers before `init_model`: inhabitant, not the emulator's initial values (Sail register initialisers are not in the model).
- Constants are emulator defaults; if the emulator runs with other flags (`--disable-fdext`, `--disable-vext`, ...), change them here.

## Open

- **Tag writes vs `ForeignCall`.** Options: (a) tags in `Memory` (negative keys), translator relaxes `MEMw_tag` to allow `μ'` to change, as `__write_mem`; (b) drop tag writes (sound, but a later `MEMr_tag` is wrong); (c) give the model a `Memory` record of bytes and tags.
  Recommendation: (a) now, (c) when the memory model is written. Taken: (a), harness side only.
- **history.csv.** run.py always appends; the `model` rule saves and restores it. Options: (a) keep; (b) a `--no-history` option in run.py. Recommendation: (b), small run.py change. Taken: (a).
- **Emulator flags.** Constants assume the emulator's defaults plus `--disable-compressed`; align with `emulator.md` once written.
