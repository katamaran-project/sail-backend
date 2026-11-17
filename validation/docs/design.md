# Translation validation: design

The choices behind translation validation, and why each makes a passing run evidence that the generated µSail model (`base.v`, `machine.v`) behaves like Sail's CHERIoT model.
Running it: [validate.md](validate.md).

## What is compared

- **The full state after every step** of short programs, run from reset on both sides.
  - State: every register (capabilities field by field, CSRs, `PC`, `PCC`), the bytes and tags written; format in [state-format.md](state-format.md).
  - Per step, so a divergence points at one instruction.
- **Against Sail's C emulator**, built from the same cheriot-sail commit as the translation.
  Its `--trace` misses part of the state (`PCC`, the special capability registers, `mcause`, `minstret`, raw capability fields), so a small patch makes it print the full state.

## How µSail runs

- **A fuel-based interpreter in Rocq** (`validation/rocq/Interp.v`), run with `vm_compute`.
  - Generic over any Katamaran `Base`/`Program`, so it can move to Katamaran.
  - The outcome tells the three ways a run stops apart: `Done` (a value or an `stm_fail`), `OutOfFuel`, `NoAnswer f` (no value for foreign call `f`).
  - Not yet: soundness (`Done` implies Katamaran's `Steps`) is stated, not proved; extraction to OCaml for a fast simulator.

## Foreign calls

- **Answered by a harness** (`validation/rocq/Harness.v`), not left open:
  - memory and tags: a concrete byte and tag map;
  - platform settings that the translation leaves open: the emulator's configuration;
  - externs with a generated `semantics_*` term: that term, so it is tested too.
- Sound for testing: `ForeignCall` allows every value of an unconstrained call, so a passing run is a µSail execution.

## Same configuration on both sides

- **The translation's exclusions match the emulator's flags** ([CHERIoT design](../../case-studies/cheriot/docs/design.md#scope)):

  | feature | µSail | emulator |
  |---|---|---|
  | compressed (C) | on: `sys_enable_rvc = true` | on (default) |
  | misaligned accesses | on: `plat_enable_misaligned_access = true` | on (default) |
  | F/D, V | off: `sys_enable_fdext`, `sys_enable_zfinx`, `sys_enable_vext` = `false` | `-F -W` |
  | Zba, Zbb, Zbc, Zbs, Zbkb, Zbkx | on (`haveZb*()` are `true` in the model) | on (cheriot-sail's instruction list) |
  | PMP | `sys_pmp_count = 0` | `--pmp-count 0` (default) |

  The µSail column is value exclusions in `tests/cheriot/scope.toml` ([platform settings](../../case-studies/cheriot/docs/design.md#platform-settings)): fixed in the model, not answered by the harness.
  Only CHERIoT's configuration is validated: C on, no configuration with C off.

- **The same start state**: registers before `init_model` as Sail's C backend initialises them (zeros, `false`, first constructor), then `init_model`, then `PC` at the ELF entry.
- **No clock ticks**: the emulator's loop ticks `mcycle`/`mtime` outside `step`; validation runs set the interval beyond any run.

## Inputs

- Hand-picked programs, one per instruction class ([cases](validate.md#cases)).
- Next: `riscv-tests` traces; random encodings once the interpreter is extracted.

## Known divergences

- **Asserted, not skipped**: each is a case whose divergence (step, both sides' changes there) is recorded in `validation/cases/divergences.toml`; any other behaviour fails, so a change shows up.
- RV32E registers >= 16: µSail fails where Sail traps illegal ([exceptions fail](../../case-studies/cheriot/docs/design.md#exceptions-fail)).
- Excluded features (PMP, Sv32) have no cases. RV64-only Zb\* encodings must trap illegal on both sides: an ordinary case.
