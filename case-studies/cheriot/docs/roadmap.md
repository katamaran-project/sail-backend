# CHERIoT translation: roadmap

What is still missing for a full translation of the CHERIoT core into Katamaran, and what each gap blocks.
The loop ranks blockers per instruction (`progress.json`, the report); [progress.md](progress.md) has the current numbers.
4 of 67 instructions are not clean, all on the memory read path: `LOAD`, `LoadCapImm`, `NOT_CAPMODE`, `NOT_C_CAPMODE`.

## Memory path

- **Writes**: done. Sail's constraints are hypotheses and flow-typed branches are decided in Rocq ([design](design.md#constraints-on-widths-are-implicit-hypotheses)); the whole write chain type-checks with its real bodies.
  - `STORE`, `StoreCapImm` still wait for `translationMode` (option patterns), `STORE` also for `ext_handle_data_check_error` (below).
- **Unions at an open type argument**: `MemoryOpResult(bits(8 * 'n))` needs a width-indexed union family (RiscvPmp's `ty_memory_op_result bytes`).
  - Untranslated: `mem_read`, `mem_read_priv(_meta)`, `mem_read_meta`, `phys_mem_read`, `pmp_mem_read`, `checked_mem_read`, `mmio_read`, `clint_load`, `htif_load`, `uart_load`, `rvfi_read`, `process_load`, `extend_value`.
  - Needs: a union family indexed by a `nat` (`Umemoryopresult_bits (n : nat)`), shared by the closed instances (`MemoryOpResult(bits(32))` is the family at 32).
  - Blocks: `LOAD`, `LoadCapImm`, and `fetch`.
- **Functions polymorphic in a type**: `MemoryOpResult_add_meta`, `MemoryOpResult_drop_meta` (over `'t`). Unions cannot be indexed by a `Ty` in Katamaran (`Ty` is built over the unions). Needs a decision (copies per type, or inlining). Block `LOAD`, `LoadCapImm`.
- **Widths that depend on a value**: `align_down`'s `bits('n)` for `n : itself('n)`. Holes. Block `LOAD`, `LoadCapImm`.
  - A width that Sail's solver proves equal to a size variable is that variable (`read_ram`'s `bits(8 * ('p8_times_n / 8))`).

## Patterns

- **A `match` on a bitvector with only a wildcard**: `match head_exp# {_ => false}` in Sail's `*_matches` mapping helpers. Not in scope today.

## Rocq output

- **`execute`'s compressed and Zb\* arms are failures, but reachable** (`haveZb*()` are `true`; `misa.C` is `sys_enable_rvc()`, unconstrained), so a proof says nothing about executions that decode one.
  Decision pending: make them absent like PMP (`haveZb*`, `sys_enable_rvc` excluded as the value `false`), or keep the failures as a documented assumption.
- **Slow decoder**: `encdec_backwards` type-checks but takes 608 s in `coqc`, and `End FunDefKit` 72 s, both over the 45 s stall limit (with the memory-write functions axiomatised, all of `machine.v` compiles in 753 s).
- **Ignored casts**: 160 warnings in 27 in-scope definitions; check that dropping each cast is sound.

## Later

- **Exceptions**: `try` in `try_execute` (untranslated; called by `step`). Not needed for now.
- **Minimal faithful PMP**: 16 entries instead of `sys_pmp_count = 0`.
  - Register arrays (`pmpcfg_n`, `pmpaddr_n`) as one register per entry: a constant index is that register, a run-time index a `match`.
  - Loops with constant bounds (`foreach`) unrolled or as a recursive helper; `pmpCheck`'s `return` inside a `foreach`.
- **Compressed instructions** (`riscv_insts_cext.sail`, `src/cheri_insts_cext.sail`, 43 constructors): add to `scope.toml`.
- **Contracts**: Katamaran contracts for the generated functions; one per size-polymorphic function, proved for all widths.
