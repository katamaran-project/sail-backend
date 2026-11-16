# CHERIoT translation: roadmap

What is still missing for a full translation of the CHERIoT core into Katamaran, and what each gap blocks.
The loop ranks blockers per instruction (`progress.json`, the report); [progress.md](progress.md) has the current numbers.
All 67 instructions are clean.

## Memory path

- **Writes**: done. Sail's constraints are hypotheses and flow-typed branches are decided in Rocq ([design](design.md#constraints-on-widths-are-implicit-hypotheses)); the whole write chain type-checks with its real bodies.
  - `STORE`, `StoreCapImm` still wait for `translationMode` (option patterns), `STORE` also for `ext_handle_data_check_error` (below).
- **Reads**: done. Width families for `MemoryOpResult(bits('n))`, copies of `MemoryOpResult_add_meta`/`drop_meta`, tuple matches split on a union column ([design](design.md#unions-at-a-width-families-indexed-by-nat)); `LOAD`, `LoadCapImm` are clean.
  - A width that Sail's solver proves equal to a size variable is that variable (`read_ram`'s `bits(8 * ('p8_times_n / 8))`).

## Patterns

- **A `match` on a bitvector with only a wildcard**: `match head_exp# {_ => false}` in Sail's `*_matches` mapping helpers. Not in scope today.

## Rocq output

- **Ignored casts**: 160 warnings in 27 in-scope definitions; check that dropping each cast is sound.

## Later

- **Exceptions**: `try` in `try_execute` (untranslated; called by `step`). Not needed for now.
- **Minimal faithful PMP**: 16 entries instead of `sys_pmp_count = 0`.
  - Register arrays (`pmpcfg_n`, `pmpaddr_n`) as one register per entry: a constant index is that register, a run-time index a `match`.
  - Loops with constant bounds (`foreach`) unrolled or as a recursive helper; `pmpCheck`'s `return` inside a `foreach`.
- **Compressed instructions** (`riscv_insts_cext.sail`, `src/cheri_insts_cext.sail`, 43 constructors): absent (`sys_enable_rvc = "false"`); add to `scope.toml` and remove that line.
- **Contracts**: Katamaran contracts for the generated functions; one per size-polymorphic function, proved for all widths.
