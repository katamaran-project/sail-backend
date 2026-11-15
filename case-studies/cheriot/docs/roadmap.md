# CHERIoT translation: roadmap

What is still missing for a full translation of the CHERIoT core into Katamaran, and what each gap blocks.
The loop ranks blockers per instruction (`progress.json`, the report); [progress.md](progress.md) has the current numbers.
6 of 67 instructions are not clean, all on the memory path: `LOAD`, `STORE`, `LoadCapImm`, `StoreCapImm`, `NOT_CAPMODE`, `NOT_C_CAPMODE`.

## Memory path

- **Writes over `bits('p8_times_n)` need Sail's constraints.** The memory-write chain does not type-check in Rocq.
  ```sail
  val htif_store : forall 'n, 0 < 'n <= 8. (xlenbits, int('n), bits(8 * 'n)) -> MemoryOpResult(bool)
  // a slice of data : bits('p8_times_n) needs 16 <= 'p8_times_n;
  // after `if width == 4`, Sail's flow typing makes data a bits(32)
  ```
  - Needs: Sail's constraints (`'p8_times_n == 8 * 'n`, bounds) as Rocq preconditions, as RiscvPmp's `{H : restrict_bytes bytes}`, and flow-typed widths.
  - Blocks: `STORE`, `StoreCapImm`; the Rocq probe stops at `htif_store`. With the 13 functions over `bits('p8_times_n)` axiomatised, all of `machine.v` compiles.
- **Unions at an open type argument**: `MemoryOpResult(bits(8 * 'n))` needs a width-indexed union family (RiscvPmp's `ty_memory_op_result bytes`).
  - Untranslated: `mem_read`, `mem_read_priv(_meta)`, `mem_read_meta`, `phys_mem_read`, `pmp_mem_read`, `checked_mem_read`, `mmio_read`, `clint_load`, `htif_load`, `uart_load`, `rvfi_read`, `process_load`, `extend_value`, `phys_mem_write`.
  - Blocks: `LOAD`, `LoadCapImm`, `STORE`, `StoreCapImm`, and `fetch`.
- **Functions polymorphic in a type**: `MemoryOpResult_add_meta`, `MemoryOpResult_drop_meta` (over `'t`). Need unions indexed by a `Ty`. Block `LOAD`, `LoadCapImm`.
- **Widths that divide a size variable or depend on a value**: `read_ram`'s `bits(8 * ('p8_times_n / 8))`, `align_down`'s `bits('n)` for `n : itself('n)`. Holes. Block `LOAD`, `LoadCapImm`.

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
