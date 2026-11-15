# CHERIoT translation: design

The choices behind the CHERIoT translation, and why each keeps the µSail model faithful enough to prove things about.
Configuration options are described in [`docs/usage.md`](../../../docs/usage.md); the loop in [loop.md](loop.md).

## Scope

- **The core only**: RV32I/E, M, Zicsr and the CHERIoT capability instructions (61 `execute` clauses).
  - Selected by source file in [`tests/cheriot/scope.toml`](../../../tests/cheriot/scope.toml); everything they reach is in scope too.
  - Out for now: compressed instructions, bit manipulation (Zb\*), vector.
- **The scope is computed, not listed**: from the model's call graph (Sail's AST after the rewrites).
  - In scope: the roots (`step`, `fetch`, `ext_decode`, `encdec_backwards`, `init_model`), all registers, and `execute_<C>` for each in-scope instruction `C`.
  - `execute` is a *dispatcher*: only its calls for in-scope instructions are followed. Its other 86 calls stay in `machine.v` as calls to missing functions.
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
- Not translated yet:
  - an extension or truncation of a bitvector of variable width is a foreign call (`uop.zext` would need a proof of `n <= m`);
  - a width that depends on a value (`itself('n)`) or divides a size variable is a hole (`NYI`).

## Constraints on widths are implicit hypotheses

```sail
val htif_store : forall 'n 'p8_times_n, 0 < 'n <= 8 & 'p8_times_n == 8 * 'n.   // after Sail's rewrites
  (xlenbits, int('n), bits('p8_times_n)) -> MemoryOpResult(bool)
```
```coq
| htif_store {kid__p8_times_n : nat} {kid__n : nat}
    {H : IsTrue ((0 <? Z.of_nat kid__n) && (Z.of_nat kid__n <=? 8) && (Z.of_nat kid__p8_times_n =? 8 * Z.of_nat kid__n))%Z}
    : Fun ["paddr" ∷ ty.bvec 32; "width" ∷ ty.int; "data" ∷ ty.bvec kid__p8_times_n] (ty.union Umemoryopresult_bool)
```
- **Which constraints**: the conjuncts of the function's quantifier that mention a width parameter.
  - Their other size variables become parameters too (`kid__n` above, which only `int('n)` mentions); repeated until nothing changes.
  - One hypothesis `H` per function: Katamaran's `IsTrue` of the conjuncts as a boolean over `Z` (`Z.of_nat` of each parameter), so that Sail's integer semantics (subtraction) is kept.
  - A conjunct Rocq cannot express (a boolean variable, an `App`) is left out: weaker, so sound; a body that needs it fails in Rocq.
  - The `Fun` constructor, the definition `fun_f` and the `FunDef` case (`@f kid__p8_times_n kid__n H => @fun_f kid__p8_times_n kid__n H`) take the same parameters.
- **Discharging them**: by Rocq's type class search, as for Katamaran's own `IsTrue` side conditions (`uop.vector_subrange`):
  - at concrete widths, Katamaran's hint computes the boolean;
  - otherwise, a hint in `machine.v` (`sail_constraint`) turns the `IsTrue` hypotheses in scope (the caller's own `H`, branch facts) into arithmetic and calls `lia`.
  - The same hint proves the side conditions in the body: a slice `data[15 .. 0]` needs `IsTrue (0 + 16 <=? kid__p8_times_n)`.
- **Parameters Rocq cannot infer** (those only the constraints relate to the types) are passed by name, from the argument of type `int('n)`:
  ```coq
  stm_call (htif_store (kid__n := 4)) [paddr; exp_int 4; data]
  ```
- **Branch facts** (Sail's flow typing): in a branch where Sail's constraints fix a size variable (`width == 4` gives `'n == 4`, so `'p8_times_n == 32`):
  - the branch is translated with the variable replaced by its value;
  - a Rocq match on `Nat.eq_dec` gives the equality, and each variable whose type mentions it is rebound with a cast;
  - the other arm is unreachable (Sail's typing guarantees `width = n`): `stm_fail`.
  ```coq
  stm_if (exp_var "width" = exp_int 4)
    (match Nat.eq_dec kid__p8_times_n 32 with
     | left H1 => stm_let "data" (ty.bvec 32) (stm_exp (sail_cast (fun k => ty.bvec k) H1 (exp_var "data"))) ...
     | right _ => stm_fail _ "unreachable: Sail's typing fixes kid__p8_times_n = 32 here"
     end) ...
  ```
  - `fundef_bindfree` case-splits on these decisions before `vm_compute`.
- **Not changed**: the `ForeignKit` (externs need no hypotheses; their semantics is total), and explicit `monomorphize` (monomorphs are closed).
- Why: one definition and one contract for all widths, as RiscvPmp's `{H : restrict_bytes bytes}`; the hypotheses are exactly what Sail's type checker assumed.
- Sail's constraints on the widths become implicit hypotheses (below).
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
