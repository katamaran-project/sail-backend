# CHERIoT translation: design

The choices behind the CHERIoT translation, and why each keeps the µSail model faithful enough to prove things about.
Configuration options are described in [`docs/usage.md`](../../../docs/usage.md); the loop in [loop.md](loop.md).

## Scope

- **The core only**: RV32I/E, M, Zicsr, the CHERIoT capability instructions, and the decoders' fallbacks for illegal or reserved encodings (67 `execute` clauses).
  - Selected by source file, or one by one, in [`tests/cheriot/scope.toml`](../../../tests/cheriot/scope.toml); everything they reach is in scope too.
  - The fallbacks (`ILLEGAL`, `C_ILLEGAL`, `FENCE_RESERVED`, `FENCEI_RESERVED`, `NOT_CAPMODE`, `NOT_C_CAPMODE`) are always reachable: they raise the illegal-instruction trap.
  - Out for now: compressed instructions, bit manipulation (Zb\*), vector.
- **The scope is computed, not listed**: from the model's call graph (Sail's AST after the rewrites).
  - In scope: the roots (`step`, `fetch`, `ext_decode`, `encdec_backwards`, `init_model`), all registers, and `execute_<C>` for each in-scope instruction `C`.
  - `execute` is a *dispatcher*: only its calls for in-scope instructions are followed. Its other 80 calls (compressed, Zb\*) are failures (`exclude-functions-as-failure`).
    Not unreachable yet: `haveZb*()` are `true` and `misa.C` is the platform setting `sys_enable_rvc()`, so these instructions decode; see [roadmap](roadmap.md#rocq-output).
  - Everything else is ignored by a generated configuration (`ignore-functions`, `ignore-values`). Types are never ignored: an unused type is harmless.
- **No virtual memory.** CHERIoT has no S-mode, so `satp` is never written and Sv32 translation is dead code.
  - `translate32`, `translationException`: failures (unreachable while `satp`'s reset mode is 0, which a proof must assume).
  - `flush_TLB`, `init_vmem`: no-ops (the TLB caches page-table entries; there are none).
  - The register `tlb32` and the types `TLB_Entry`, `TLB32_Entry` are left out.
- **PMP absent**: `sys_pmp_count` is `0`, a configuration the model allows (`init_pmp` asserts 0, 16 or 64).
  ```sail
  function pmp_mem_read(t, p, paddr, width, aq, rl, res, meta) =   // sail-riscv's riscv_mem.sail
    if   sys_pmp_count() == 0                                     // excluded as the value 0
    then checked_mem_read(t, paddr, width, aq, rl, res, meta)
    else match pmpCheck(paddr, width, t, p) { ... }               // unreachable: pmpCheck is a failure
  ```
  - `pmpCheck` and the PMP CSR accessors: failures. `pmp_mem_read`/`write` skip `pmpCheck` when the count is 0,
    and `is_CSR_defined` refuses the PMP CSRs (0x3A0–0x3EF) unless `sys_pmp_count() > idx`.
  - `init_pmp`: no-op. The registers `pmpcfg_n`, `pmpaddr_n`: left out of `Reg`.
  - Exact for a CHERIoT without PMP. Removing the PMP lines from `scope.toml` turns PMP back on.
- **Printing and tracing are excluded**, together with their uses (below).

## Undefined values are unconstrained

- A Sail `undefined` (an `undefined_<type>()` call after Sail's rewrites) means "any value". It must become an *unconstrained* value: a proof holds for every value.
- Never a fixed default: a default would let a proof rely on a value the hardware does not promise.
- The mechanism is µSail's foreign call to `unconstrained σ`, whose semantics allows every value of σ:
  ```coq
  | unconstrained (σ : Ty) {inhabitant : Unconstrained σ} : FunX ctx.nil σ
  stm_foreign (unconstrained ty.bool) env.nil
  ```
- The same mechanism serves unconstrained externs and excluded definitions (below).
- `undefined_*` is policy-excluded as `unconstrained` by a pattern (`"undefined_*"` in `scope.toml`): each one in-scope code reaches is excluded by name, so each call is `stm_foreign (unconstrained τ)`.
  No in-scope code reaches one yet. Test `undefined-as-unconstrained`.

## No monomorphisation

- A function polymorphic in a width (or a type) is **one** `Fun` constructor with the widths as implicit `nat` arguments, and one definition for all widths.
  ```sail
  val sign_extend : forall 'n 'm, 'm >= 'n. (implicit('m), bits('n)) -> bits('m)
  ```
  ```coq
  | sign_extend {kid__n : nat} {kid__m : nat} : Fun ["m" ∷ ty.int; "v" ∷ ty.bvec (kid__n)] (ty.bvec (kid__m))
  Definition fun_sign_extend {kid__n : nat} {kid__m : nat} : Stm [...] (ty.bvec (kid__m)) := ...
  ```
- Rocq infers the widths at each call.
- Why: one contract and one proof cover all widths.
  Katamaran's hand-written RiscvPmp model does the same (`mem_read (bytes : nat)`, proved once with `vm_reflexivity`).
  Monomorphisation (one copy per width) would need one contract and one proof per copy.
- Sail's constraints on the widths (`'m >= 'n`, `'p8_times_n == 8 * 'n`) are not translated yet:
  - an extension or truncation of a bitvector of variable width is a foreign call (`uop.zext` needs a proof of `n <= m`);
  - a width that depends on a value (`itself('n)`) or divides a size variable is a hole (`NYI`).
- A union with a type parameter becomes one monomorphic copy per *closed* type argument it is used at (`MemoryOpResult_bool`); unions at an open type wait for width-indexed unions ([roadmap](roadmap.md)).
- The `monomorphize` option still exists but is not used here.

## Externs: µSail operations or foreign functions

- A Sail extern (a `val` with a binding and no body) becomes:
  - a **µSail operation** when one matches and the widths are known (`shiftl` → `bop.shiftl`, `sub_vec` → `bop.bvsub`, `get_slice_int` → `uop.get_slice_int`, ...);
  - otherwise a **foreign function**: a `FunX` constructor with the extern's signature (implicit widths) and a `stm_foreign` call.
- Foreign semantics (`ForeignCall`) never fails and never changes registers; `pure` externs leave memory unchanged.
- The result is a Rocq term for Sail's arithmetic externs (`emod_int`, `pow2`, `count_leading_zeros`, ...), and unconstrained otherwise (platform settings, memory, tags):
  ```coq
  | plat_ram_base : FunX ["arg1" ∷ ty.unit] (ty.bvec 32)     (* any value: not a fixed constant *)
  | @emod_int => fun args res => γ' = γ /\ μ' = μ /\ res = inr (semantics_emod_int ...)
  ```
- Why sound: unconstrained is the weakest assumption; a term is given only where Sail's library fixes the meaning.

## Policy exclusions

Code that in-scope code reaches but the core does not want is removed **together with its uses** (`[policy-excluded]` in `scope.toml`).
The scope does not follow edges into an excluded definition, so what only it reaches leaves the scope.

| policy | a use becomes | used for | why sound |
|---|---|---|---|
| no-op | `stm_exp (exp_val ty.unit tt)` | printing (`print_reg`, ...), `flush_TLB`, `init_vmem`, `init_pmp` | only for `unit` functions whose effects the model never reads back |
| failure | `stm_fail τ "f is excluded by the configuration"` | `translate32`, `translationException`, `pmpCheck`, PMP CSR accessors | the call is unreachable; a contract must show it (or `fail_rule_pre` makes it vacuous) |
| unconstrained | `stm_foreign (unconstrained τ) env.nil` | strings for printing (`bits_str`, `RegStr`, ...), `get_config_print_*` | any value is allowed, so nothing is assumed; the values only feed no-op printing |
| value | the given Sail expression, type-checked in place | `sys_pmp_count = "0"` | a configuration the model allows; fixes the platform being verified |
| registers | left out of `Reg`; a read is unconstrained, a write a no-op | `tlb32`, `pmpcfg_n`, `pmpaddr_n` | only excluded code uses them meaningfully |

Example, a failure:
```coq
stm_fail (ty.sum (ty.union Uexceptiontype) ty.unit) "pmpCheck is excluded by the configuration"
```

## Configuration clean-up

- Overloads and pragmas are ignored: Sail's type checker has resolved them.
- Types no in-scope code uses are ignored: Sail's concurrency interface (`Access_kind`, ...), `regfp.sail`, the TLB entries.
- Generated helpers are skipped: `*_of_num`, `num_of_*` (except the model's own `num_of_ExceptionType`, `num_of_ext_exc_type`), `regval_of_*`, `undefined_*` (those that in-scope code calls are excluded as unconstrained, above).
- `option` and `result` translate to µSail's `ty.sum` (`translate-union-as-sum`).
