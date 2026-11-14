# CHERIoT translation: roadmap

What is still missing for a full translation of the CHERIoT core into Katamaran, and what each gap blocks.
The loop ranks blockers per instruction (`progress.json`, the report); [progress.md](progress.md) has the current numbers.
6 of 61 instructions are not clean: `LOAD`, `STORE`, `LoadCapImm`, `StoreCapImm`, `CSR`, `CSetHigh`.

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

- **Option patterns**: a constructor or tuple pattern inside `Some` (option is µSail's `ty.sum`):
  ```sail
  match architecture(get_mstatus_SXL(mstatus)) { Some(RV32) => ..., _ => internal_error(...) }   // translationMode
  match dispatchInterrupt(cur_privilege) { Some(intr, priv) => ..., None() => ... }            // step
  ```
  - Blocks: `translationMode` (`LOAD`, `STORE`, `LoadCapImm`, `StoreCapImm`); `step` and `dispatchInterrupt` (`Some(ip, p)`), so interrupt handling.
- **Tuple pattern at a tuple type abbreviation**: `let (capEx, regnum) = err` with `err : ext_data_addr_error`.
  Blocks `ext_handle_data_check_error` (`LOAD`, `STORE`).
- **`match` on a `bool`**: `encdec_backwards` (the main decoder), from `bool_not_bits(s)` in the `DIV` encoding.
  Blocks decoding; not counted in clean instructions, as decoders are not in their call cones.

## Widths not known at compile time

- **Widths from type-level constants**: `tag_addr_to_addr(tag_addr) = tag_addr @ zeros(log2_cap_size)` with `type log2_cap_size : Int = 3`. Hole. Blocks `LOAD`, `STORE`, `LoadCapImm`, `StoreCapImm`.
- **Run-time widths and slice indices**: `writeCSR` (blocks `CSR`), `execute_CSetHigh`, `ext_data_get_addr` (`LOAD`, `STORE`).

## Undefined values and externs

- **`undefined_*` as unconstrained values.** The configuration skips the `undefined_*` functions; they should become `stm_foreign (unconstrained τ)` ([design](design.md#undefined-values-are-unconstrained)).
  No in-scope code calls them today, so nothing is blocked yet; needed as soon as one is reached.

## Rocq output

- **Calls to out-of-scope instructions.** `execute` still calls the 86 `execute_<C>` outside the scope, which `machine.v` does not define.
  Needs: those calls as failures (the dispatcher's out-of-scope arms excluded as `failure`). Blocks plain Rocq of `machine.v`.
- **Ignored casts**: 160 warnings in 27 in-scope definitions; check that dropping each cast is sound.

## Later

- **Exceptions**: `try` in `try_execute` (untranslated). Not needed for now.
- **Minimal faithful PMP**: 16 entries instead of `sys_pmp_count = 0`.
  - Register arrays (`pmpcfg_n`, `pmpaddr_n`) as one register per entry: a constant index is that register, a run-time index a `match`.
  - Loops with constant bounds (`foreach`) unrolled or as a recursive helper; `pmpCheck`'s `return` inside a `foreach`.
- **Compressed instructions** (`riscv_insts_cext.sail`, `src/cheri_insts_cext.sail`, 43 constructors): add to `scope.toml`.
- **Contracts**: Katamaran contracts for the generated functions; one per size-polymorphic function, proved for all widths.
