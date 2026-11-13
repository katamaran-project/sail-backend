# CHERIoT translation-failure inventory (13 Nov 2026)

A survey of every place where sail-backend fails on the CHERIoT Sail model, grouped by root cause.
Nothing in the translator was changed. All numbers come from the scripts in this directory and can be regenerated (see the end).

**Short version.**
- The full model now gets through Sail and both translation stages, but only after two workarounds:
  - a Sail crash caused by the plugin's rewrite pipeline, worked around with `-splice`;
  - five hard generation failures, worked around by ignoring five functions.
- **1135 of 1963 Sail functions** translate, and so do 118 of 147 `execute` clauses.
- What fails splits into a small set of real gaps on the `step`/`execute` path (176 definitions) and a large amount of noise: overloads, pragmas, assembly/printing mappings, and auto-generated bitfield setters.
- Rocq compilation stops at the first `NYI` hole in `base.v`. `machine.v` has never been compiled.
- On the main design question: Katamaran's own hand-written RiscvPmp model already handles size polymorphism (`mem_read (bytes : nat)`) and `MemoryOpResult` with **nat-indexed `Fun` constructors and union families**. Contracts and `ValidContract` lemmas are proven **once for all widths**.
  So we do not need Sail-level monomorphisation, or one lemma set per copy. This is the recommended direction for piles P13 and P02 (§5).

**Status update (13 Nov 2026, branch `e2e-loop`).** The rest of this document is the snapshot taken before; its numbers were not regenerated.
- **P00 is fixed.** `remove_blocks` is no longer in `Rewrites.ml` (option 1 below). The e2e suite is unchanged (303 pass, 32 fail).
  Turning it off exposed two translator bugs that were fixed at the same time: `make_sequence` ran the statements of a block in the wrong order, and statements of a block were translated without their type `unit`, which `exit` needs.
  Tests: `assert-flow-typing-fixed-width` (passes; guards the fix), `assert-flow-typing-constrained-width` (fails until the code generator uses Sail's constraints for slice indices) and `assert-flow-typing-runtime-width` (fails until bitvectors with run-time widths are supported, P13).
- The full model, **without the `-splice` workaround**, now gets through all Sail rewrites and the Sail → nanoSail translation.
  µSail generation then stops at the first hard failure, `add_bits_int` with a run-time integer in `getCapBoundsBits` (`FunctionCalls.ml:358`, §2b), so no `.v` file is written.
- **`add_bits_int` with a run-time integer is fixed** (P01, option 1): it is generated as `bv +ᵇ exp_unop uop.get_slice_int i`, whose semantics is `bv.of_Z`, i.e. `i` modulo 2ⁿ, as in Sail.
  Tests: `bitvectors-add-runtime-int`, `bitvectors-add-runtime-int-negative`.
  `getCapBoundsBits` and `__WriteRAM_Meta` are now generated. `__WriteRAM_Meta` has no holes; `getCapBoundsBits` keeps 4 `NYI`s, all from `… @ zeros(E)` with a run-time `E` ("expected constant in bitvector type", P01 site 5 below).
- **Next in line: the `sail_assert` message** (P01, option 2), now the first hard failure the full model hits: `internal_error` (called from 32 places) and `report_invalid_width`.
  With those two and `pmpAddrRange` ignored by configuration, µSail generation completes (`base.v` 1.7 MB, `machine.v` 9.2 MB).
  `pmpAddrRange` (`failed to determine bitvector size`) is PMP, which is out of scope, so it should be excluded by configuration rather than fixed.

---

## 1. Environment and commits

| | |
|---|---|
| opam switch | `sail-backend` (new, created for this survey; `katamaran9` does not exist on this machine) |
| OCaml | 5.2.1 (`ocamlfind ocamlopt -version`) |
| Sail | 0.20.3 (`sail --version`: `Sail 0.20.3 (sail @ opam-v2.4.1 0.20.3)`); z3 4.16.0 (Homebrew) |
| Rocq | 9.2 (`coqc --version`: `The Rocq Prover, version 9.2`), rocq-iris 4.5.0, rocq-stdpp 1.13.0, rocq-equations 1.3.2+9.2, dune 3.23.1 |
| Katamaran library | **not installed** in the switch. Katamaran `main` @ `b50d7e77`, built with `dune build -p coq-katamaran` from a `git archive` export in the session scratchpad, and put on `ROCQPATH`. 4 files of that build fail (`theories/Staging/{WorldIsomorphisms,BinaryExecutor/ShallowExecutorRel,NewShallow/IrisInstance}.v`, `case_study/deprecated/RiscvPmpUnboundedInts/Base.v`); the modules that the generated files import (`Base`, `Bitvector`, `Program`, `Semantics.Registers`) built fine. |
| sail-backend | `master` @ `e4434f39` (unchanged; plugin from `dune build`, used through `bin/sail-local`) |
| katamaran (case study) | branch `tex-cheriot` @ `28ab8ab9`, worktree `~/phd/PROJECTS/katamaran-tex-cheriot` of `~/phd/PROJECTS/katamaran` |
| cheriot-sail | `main` @ `518813e` in `~/phd/PROJECTS/cheriot-sail` (fast-forwarded from `906d662`); submodule `sail-riscv` @ `30317c5` |

The environment setup turned up some side findings, none of which were changed:
- **macOS `make` (GNU Make 3.81) cannot run the case-study Makefile.** Line 49 uses `#` as a sed delimiter inside `$(shell …)`, and make 3.81 treats it as a comment ("unterminated call to function `shell'"). Homebrew `gmake` 4.4.1 works. All runs here use `gmake`.
- **The six `sum-unions-*` e2e tests can never pass as committed.** Commit `998b14e5` added them without their `base/machine.template.{v,html}` files (sail exits with `machine.template.html: No such file or directory`).
- **NYI numbering mismatch.** In the generated code, `NYI[k]` uses a global counter (`GC.fresh_index`), but the comment above the definition numbers the annotations per frame (`[0]`, `[1]`, …). The numbers in code and comment therefore do not correspond. `extract.py` pairs them by order instead.
- **Installing Rocq into a switch downgrades dune**, which recompiles libsail. A plugin built before that (and a test run during it) breaks. The first translation-only e2e run here was invalid for that reason; it was discarded and its line removed from `test-history.csv`.
- `_opam/` (stale 2023 local switch) is still present and untouched. All commands ran with `OPAMSWITCH=sail-backend`.

**Baselines** (`tests/coq-tests/run-tests.py -j 4`):
- translation only (`--no-coq`): **326 pass / 8 fail**: the six `sum-unions-*` (missing templates), plus `functions-flip` and `monomorphize-type` (Sail type errors).
- with coqc: **303 pass / 31 fail**. This matches the last recorded run in `test-history.csv`; `docs/handover.md`'s 309/25 is not reproduced.
  Failing: `bitfield-assign bitvectors-indexing bitvectors-monomorphization bitvectors-variables feature-types-and-literals-3 feature-types-and-literals-5 functions-flip functions-implicit functions-ternary-scattered itself-001 itself-002 match-list-nested match-record match-tuple-triple-2 match-unions-nested match-unions-nested-2 model-008 monomorphize-constraint monomorphize-single-int monomorphize-two-ints monomorphize-type records-definition-with-triple sum-unions-*(6) tuples-quad tuples-triple vectors`.
- Unit tests (`dune test`): 2444 cases, OK (1 skipped).

## 2. What was run, what was left out, and why

| run | how | result |
|---|---|---|
| `prelude/` | `gmake -C <kat>/case_study/Cheriot test` (only `sail-riscv/model/prelude.sail`), `VERBOSE=4` | translates; `coqc base.v` fails at **line 227** (first `NYI`, `Access_kind`); handover said 277 |
| `full/` | `gmake … sail` (67 files: cheriot-sail's `SAIL_SRCS` + `main.sail`), `VERBOSE=4` | **Sail crashes** before translation (below) |
| `bisect/` | direct `sail-local` runs on subsets, `try.sh` | locates the crash; see below |
| **`full-workaround/`** | all 67 files + `-splice workarounds/splice.sail` + `workarounds/configuration.lisp`, `VERBOSE=4` | **translates completely**; this is the run the inventory is based on |
| `variant-more-sums/` | as above + 6 more `translate-union-as-sum` lines (`workarounds/configuration-sum-variant.lisp`) | experiment for pile P02/P03 |
| `coqc-probe/` | copies of `full-workaround/*.v` with `Axiom NYI : forall {A}, list nat -> A` added | to see the first coqc error beyond the `NYI` holes |

### 2a. The Sail crash and the workaround (`-splice`)

```
Internal error: Unreachable code (at "src/lib/type_error.ml" line 577):
sail-riscv/model/riscv_vext_regs.sail:175.52-68:
175 |  then print_reg("v" ^ dec_str(r) ^ " <- " ^ BitStr(v[VLEN - 1 .. 0]));
    | During rewriting: Could not resolve quantifiers for subrange_bits
    | * (0 <= 0 & (0 <= (('_#VLEN * 8) - 1) & (('_#VLEN * 8) - 1) < 65536))
```

- Plain `sail --just-check` on all 67 files succeeds, so this is **not a Sail bug in the model; it is caused by the rewrite pipeline that `sail_plugin/Rewrites.ml` selects.**
- Dumping the AST after each rewrite (`bisect/rewrite-dump/`) shows that the crash is in rewrite 50, `recheck_defs`. That pass runs right after `remove_blocks` → `attach_effects` → `toplevel_let_patterns`, all three marked "From Coq backend".
- `remove_blocks` turns
  ```sail
  let VLEN = unsigned(vlenb) * 8;
  assert(0 < VLEN & VLEN <= sizeof(vlenmax));
  ... v[VLEN - 1 .. 0] ...
  ```
  into `let _ : unit = assert(...) in ...`.
  Sail only uses an `assert` for flow typing when it is a statement in a block, so the re-check loses `VLEN <= 65536`.
  The handover's suspicion (`recheck_defs` + `let VLEN`) is right; `remove_blocks` is the trigger.
- The bisection (`bisect/try.sh`):
  - prefix 1–24 does not crash, but it stops at a hard generation failure (P01);
  - prefix 1–25 crashes (file 25 = `riscv_vext_regs.sail`);
  - dropping the two vector files fails Sail's typecheck, because `riscv_sys_control.sail` writes `vcsr`. So **no file can simply be left out.**
- **Workaround: Sail's `-splice` flag** (`workarounds/splice.sail`). No translator or cheriot-sail file was changed.
  - `wV` is replaced by a copy that guards the debug print with `if get_config_print_reg() & 0 < VLEN & VLEN <= sizeof(vlenmax)`. The semantics are unchanged.
  - The bodies of 10 vector-register helpers (`get_num_elem, read_/write_single_vreg, read_/write_vreg, read_/write_single_element, read_vmask, read_vmask_carry, write_vmask`) are replaced by `internal_error(...)`. All of them rely on `assert` for flow typing. **None is called from any other translated file**, because no V instructions are in `SAIL_SRCS`, so this is dead code for CHERIoT. Their own translation is lost from the inventory.

### 2b. Hard generation failures and the workaround (configuration)

A `GC.fail` during µSail generation aborts the whole output, so no `.v` file is written. `workarounds/hard-failure-loop.sh` repeatedly ran the translation, added each hard-failing function to the ignore predicate of a *copy* of the case-study configuration (`workarounds/configuration.lisp`, made by `make-config.py`), and stopped when generation completed. It took 5 rounds:

| function | Sail location | nanosail | message |
|---|---|---|---|
| `getCapBoundsBits` | `src/cheri_cap_common.sail:418` | `FunctionCalls.ml:358` | only calls to `add_bits_int` supported where second argument's value is known at compile time |
| `__WriteRAM_Meta` | `src/cheri_mem_metadata.sail:87` | `FunctionCalls.ml:358` | (same) |
| `internal_error` | `sail-riscv/model/riscv_types.sail:92` | `FunctionCalls.ml:533` | `sail_assert` should have its second argument known at compile time |
| `report_invalid_width` | `sail-riscv/model/riscv_types.sail:419` | `FunctionCalls.ml:533` | (same) |
| `pmpAddrRange` | `sail-riscv/model/riscv_pmp_control.sail:29` | `FunctionCalls.ml:350` | failed to determine bitvector size (in `add_bits_int`) |

Their callers still call them. That shows up in coqc and in §3's "calls to undefined functions" count.

## 3. Headline numbers (`full-workaround`)

| | count |
|---|---|
| Sail definitions processed by nanosail (after Sail's rewrites) | 5261 |
| — functions | 1963: **1135 translated**, 455 untranslated, 373 skipped by configuration |
| — `val`s (top-level type constraints) | 2140: 1541 translated, 214 untranslated, 385 skipped |
| — top-level `let` | 65: 43 translated, 22 untranslated |
| — types | unions 24, structs 8, enums 108, type abbreviations 106, registers 121: all accepted by Sail→nanosail, but 10 polymorphic union/record types carry `NYI`s in `base.v` |
| — overloads / pragmas | 484 / 229, all "untranslated" (noise, P12) |
| `execute` clauses | 147: **118 translated**, 29 untranslated |
| Untranslated definitions (`----` blocks) | 1405 blocks, 730 distinct names. Together with the NYI holes and hard failures: 768 distinct definitions, **176 of them reachable from `step`/`execute`** |
| `NYI` holes | **113** (32 in `base.v`, 81 in `machine.v`) in 33 definitions |
| Recovered `Failure`s (assertion errors → untranslated) | 57 |
| Hard generation failures | 5 (worked around, §2b) |
| Warnings | 4559 (3263 "call to polymorphic function", 370 "could not determine whether function is polymorphic", 198 + 87 polymorphic constraint / function, 157 ignored casts, 132 type mismatches, nearly all in `assembly_forwards`/`encdec_compressed_forwards`) |
| µSail functions with an unbound size variable (`kid__…`) in their signature | **54** (essentially the whole memory path) |
| Calls in `machine.v` to functions it does not define | **1955 calls to 152 functions** (71 Sail externs, 71 untranslated, 9 skipped by configuration, 1 other); `stm_foreign` is never generated (`full-workaround/dangling-calls.csv`) |
| `coqc base.v` | fails at line 593, the first `NYI` (`Access_kind`'s parameter). With `NYI` axiomatised, it fails at line 598: `Access_kind` takes a type parameter but its constructors are typed without it (P02). |
| `coqc machine.v` | never reached (needs `base.vo`) |

"Reachable" (`reachability.py`) means reachable in the call graph built from the Sail AST after the last rewrite. The roots are `step`, `loop`, `main`, the init functions, `execute`, the decoders and `fetch`. 728 functions are reachable from `step`.

## 4. Pile table, sorted by impact on a usable `step`/`execute`

"defs" = distinct definitions affected; "reach" = of those, reachable from `step`/`execute`. Generated by `piles.py`; P00 and P16 come from §2a and `dangling-calls.py`.

| # | pile | sites | defs | reach | size | blocks / blocked by |
|---|---|---:|---:|---:|---|---|
| P00 | ~~Sail crash in the plugin's rewrite pipeline (`remove_blocks` + `recheck_defs`)~~ **fixed** | 1 (file) | 11 stubbed | 0 | **S** | blocked *everything* for the full model |
| P13 | size-polymorphic functions (`bits('n)` in signatures): memory path | 87 warnings | 83 (54 with `kid__` left in µSail) | 52 | **L** (design) | blocks coqc of all loads/stores/fetch |
| P02 | polymorphic unions other than option/result | 86 | 39 | 31 | **M** | needs P03; blocks `coqc base.v` (first real error) |
| P03 | nested patterns inside sum constructors | 4 (11 after P02) | 4 → 11 | 3 → 10 | **M** | hidden behind P02 |
| P16 | Sail externs not mapped to µSail (no `stm_foreign` at all) | 1697 calls | 71 externs | — | **M** (+ decisions) | blocks coqc of most of `machine.v` |
| P05 | `foreach`, `loop`, `try`, early `return` | 25 | 25 | 23 | **M** | — |
| P04 | vector literal with `bits(1)` elements (`[bool_to_bit(x)]`) | 54 | 27 | 17 | **S** | **the decoders** (`encdec_backwards`, compressed) |
| P01 | integer arguments that must be known at compile time | 59 | 13 | 10 | **M** | hard failures; `getCapBoundsBits` (CHERI bounds); `add_bits_int` done, **the `sail_assert` message is next in line** |
| P08 | top-level `let` with a computed value | 22 | 22 | 16 | **S–M** | `null_cap`, root caps, MMIO bases |
| P06 | mutable local variables (`var x = …; x[i] = …`) | 4 | 4 | 4 | **M** | `setCapBounds`, `capToEncCap` (CHERI core) |
| P07 | register references (`register('a)`, `ref r`, `r->F() = v`) | 430 | 217 | 9 | **S** (+S) | `execute_CJALR`, `ext_init_regs` |
| P10 | vectors of non-bit elements (register arrays `pmpcfg_n`, `pmpaddr_n`) | 29 | 13 | 4 | **M** | PMP |
| P11 | numeric types: `range`, `if` in a numeric expression, structs with `Int` parameters | 13 | 12 | 6 | **S** each | `CGetLen/CGetTop`, TLB (not needed for CHERIoT?) |
| P09 | mappings over strings/bitvectors (assembly syntax, printing) | 140 | 140 | 16* | **S** to drop | only reachable through tracing |
| P12 | overloads and pragmas reported as untranslated | 714 | 252 | — | **S** | noise only |
| P14 | definitions skipped by configuration (hidden gaps) | 759 | 386 | 12 | **S–M** | `undefined_*`, `zeros_implicit`, `ones`, hard-failure functions |
| P15 | ignored casts | 157 warnings | 22 | 21 | **S** (check) | — |

\* reachable only via `print_*`/trace code (`hex_bits_*_forwards`, `cap_creg_name_forwards`, `fence_bits_forwards`).

Size: **S** ≈ a few days including tests, **M** ≈ 1–2 weeks, **L** = needs a design decision and several weeks.

## 5. Per-pile detail

Throughout: "usage in CHERIoT" counts come from the translation (`items.csv`) and from `feature-counts.txt`. The latter is a grep over the 67 sources, so it is approximate.
Examples give the Sail location and the nanosail location that raised the failure.

### P00. Sail crash caused by the rewrite pipeline — S, fixed

*Status.* Fixed on branch `e2e-loop` with option 1 below; see the status update at the top.

*What it is.* The plugin runs Sail's `remove_blocks` and then `recheck_defs`. `remove_blocks` destroys the flow typing that `assert` provides (§2a).
```sail
let VLEN = unsigned(vlenb) * 8;
assert(0 < VLEN & VLEN <= sizeof(vlenmax));   // gives the bound...
print_reg(BitStr(v[VLEN - 1 .. 0]))            // ...that this slice needs
```
*Usage.* 43 `assert(`s in 9 files. Here, 11 functions in `riscv_vext_regs.sail` (dead code for CHERIoT); any other model that relies on the same idiom will crash too.

*Options.*
1. Drop `remove_blocks` from `Rewrites.ml`, if nanosail does not need it. Nanosail translates `AE_block` itself (`Function.ml`, `statement_of_block`), so it may well not. Check with the e2e suite.
2. Keep `remove_blocks`, but drop the `recheck_defs` after it, or move the recheck before it. The recheck is only a sanity check.
3. Keep the splice workaround. Not a fix.

Recommendation: option 1, then option 2. Test: a new e2e test with the snippet above; none exists now.

### P13. Size-polymorphic functions (the memory path) — L, the main design decision

*What it is.* Sail functions that are polymorphic in an **integer** type variable that appears in a bitvector width:
```sail
val mem_read : forall 'n, 0 < 'n <= max_mem_access.
  (AccessType(ext_access_type), xlenbits, int('n), bool, bool, bool) -> MemoryOpResult(bits(8 * 'n))
```
µSail function types are first-order, so nanosail keeps the variable as `kid__…`. That gives µSail signatures like `align_down : Fun ["n" ∷ ty.int; "bv" ∷ ty.bvec (kid__m)] (ty.bvec (kid__m))`, which cannot compile.

*Usage in CHERIoT.* 83 functions are reported as "polymorphic function, no monomorphizations requested". 54 µSail signatures keep a `kid__` (list in `items.csv`, warning `Function.ml:1707`):
- `mem_read`, `mem_read_priv(_meta)`, `mem_write_value(_priv)(_meta)`
- `checked_mem_*`, `pmp_mem_*`, `phys_mem_write`, `read_ram`, `write_ram`, `mmio_write`, `htif_store`, `uart_store`
- `sign_extend`, `zero_extend`, `to_bits`, `align_down`, `rotate*`
- `is_some`, `unwrap_or`, … (type-polymorphic, see P02)

52 of the 83 are reachable: every load, store, capability load/store and fetch goes through them.
Not every polymorphic function is a problem. Where `'n` occurs only as `int('n)` it is erased to `ty.int`, which is harmless: `rX : Fun ["r" ∷ ty.int] (ty.bvec 32)`.

*Options.*
1. **Monomorphise in Sail** (Sail's `monomorphise` rewrite with the backend's monomorphisation requests).
   Each width (1, 2, 4, 8 bytes) gets its own copy of about 25 functions, and each copy needs its own contract and `ValidContract` lemma.
   This is the option you want to avoid, and the evidence below shows it is unnecessary.
   The handover also notes regressions in the `monomorphize-*` tests since Sail 0.20.3.
2. **Width-indexed µSail constructors, as in Katamaran's own RiscvPmp case study** (recommended).
   `case_study/RiscvPmp/Machine.v` declares
   ```coq
   | mem_read (bytes : nat) {H : restrict_bytes bytes} : Fun [typ ∷ ty_access_type; paddr ∷ ty_xlenbits] (ty_memory_op_result bytes)
   | read_ram (bytes : nat) : FunX [paddr ∷ ty_xlenbits] (ty_bytes bytes)
   Definition fun_mem_read (bytes : nat) {H : restrict_bytes bytes} : Stm [...] (ty_memory_op_result bytes) := ...
   ```
   and `Contracts.v` proves `Lemma valid_contract_mem_read (bytes : nat) {H : restrict_bytes bytes} : ValidContract (@mem_read bytes H). Proof. vm_reflexivity. Qed.`
   That is **one contract and one proof for all widths**. `checked_mem_read` needs a `destruct H` case split, but still one lemma.
   The translator would emit a `nat` parameter per integer type variable (`'n ↦ bytes`), turn the Sail constraint into a `restrict_*` predicate, and instantiate the parameter at each call site. The widths are always known there, after P01.
   Work:
   - a new kind of `Fun` constructor in the generator, plus its contracts template;
   - a family `ty_memory_op_result bytes` for the result type (see P02);
   - handling of constraints such as `'p8_times_n == 8 * 'n` (the second variable becomes an expression of the first).
3. **Maximum-width encoding** (`bits(64)` plus an explicit width argument, with truncation at call sites).
   A single copy and no µSail extension, but the translation no longer mirrors Sail. Contracts must reason about truncations, and the encoding itself must be trusted.
   This is worse than option 2, which Katamaran already supports.
4. **Treat the memory layer as hand-written foreign functions** (`FunX`), with contracts written by hand and the Sail code above them translated.
   This is the cheapest way to unblock the rest. It is reasonable for `read_ram`/`write_ram`/MMIO, which are externs anyway (P16), but it leaves the PMP/CHERI checks in between untranslated.

*Recommendation.* Option 2 for the layer that the Sail model defines, and option 4 only for the externs underneath. Before committing, check one thing with a Katamaran expert: that `vm_reflexivity` on a `nat`-parametric `ValidContract` keeps working once the bodies come from the translator rather than being hand-written. The RiscvPmp proof suggests it does.

*Tests.* None. `bitvectors-monomorphization`, `functions-implicit`, `itself-*` and `bitvectors-variables` touch related ground, and all currently fail. Easy to write: `val f : forall 'n, 'n in {1,2,4}. (int('n)) -> bits(8 * 'n)` called at two widths.

### P02. Polymorphic unions other than `option`/`result` — M

*What it is.*
```sail
union AccessType ('a : Type) = { Read : 'a, Write : 'a, ReadWrite : ('a, 'a), Execute : unit }
union MemoryOpResult ('a : Type) = { MemValue : 'a, MemException : ExceptionType }
union Ext_ControlAddr_Check ('a : Type) = { Ext_ControlAddr_OK : xlenbits, Ext_ControlAddr_Error : 'a }
union TR_Result('paddr : Type, 'failure : Type) = { TR_Address : ('paddr, ext_ptw), TR_Failure : ('failure, ext_ptw) }
```
- The type definition becomes `Inductive AccessType (kid__a : NYI) : Set := | Read : NYI -> AccessType …` (`Kind.ml:8`, `Type.ml:262`); this is the first real coqc error.
- Matches on such a type fail with `expected pattern AP_app (Read, …) while matching type Type:Application[…]` (`Match.ml:2450`).

*Usage in CHERIoT* (instantiations counted in the final Sail AST):

| union | constructors | instantiations | notes |
|---|---:|---|---|
| `AccessType` | 4 | **one**: `AccessType(ext_access_type)` (42 uses) | RiscvPmp has it as a monomorphic union |
| `Ext_FetchAddr_Check`, `Ext_ControlAddr_Check`, `Ext_DataAddr_Check` | 2 | **one each** (`CapEx`, `(CapEx, capreg_idx)`) | |
| `TR_Result`, `PTW_Result` | 2 | one or two (`bits(32)`/`bits(34)` paddr) | Sv32 translation, likely unused in CHERIoT |
| `MemoryOpResult` | 2 | **many**: `bool`, `bits(8n)`, `unit`, `Capability`, `('t, mem_meta)` | width-dependent, see P13 |
| `Access_kind`, `Mem_read_request`, `Mem_write_request` | — | concurrency interface from Sail's library | not used by CHERIoT's model; can be ignored by configuration |

It affects 39 definitions, 31 of them reachable:
- `execute_LOAD/STORE/StoreCapImm/BTYPE/RISCV_JAL/RISCV_JALR`
- `checked_mem_read`, `process_load`, `fetch`, `mmio_read`, `pmpCheckRWX`

*Options.*
1. **Specialise unions that have a single instantiation** (`AccessType`, `Ext_*_Check`): substitute the type argument and emit a monomorphic union.
   No function is duplicated, because no function is polymorphic in these types, so there is no lemma cost. It matches what RiscvPmp does by hand. **S–M.**
2. **`translate-union-as-sum`** (existing option) for two-constructor unions. Measured with `variant-more-sums`:
   - it fixes 7 functions (`execute_BTYPE`, `execute_RISCV_JAL`, `execute_RISCV_JALR`, `process_load`, `extend_value`, `walk32`, `MemoryOpResult_add_meta`);
   - type-level `NYI`s drop from 28 to 11;
   - another 7 then fail on P03 (`execute_LOAD`, `execute_STORE`, `execute_StoreCapImm`, `fetch`, `mem_read_cap`, `translateAddr_priv`, `MemoryOpResult_drop_meta`).

   It costs only configuration, but `ty.sum` loses constructor names in the µSail code and contracts, which makes contracts harder to read.
3. **Union families indexed by Rocq parameters** (RiscvPmp's `memory_op_result (bytes : nat)`). Needed for `MemoryOpResult` if P13 option 2 is chosen: the payload type depends on the width. **M**, shares machinery with P13.
4. Monomorphise every function that uses them. Rejected for the same reason as P13 option 1.

*Recommendation.* Option 1 for single-instantiation unions. For `MemoryOpResult`, use option 3 together with P13, or option 2 as a stopgap, which needs P03.
The polymorphic *helper functions* (`is_some`, `is_none`, `unwrap_or`, `ok_option`, `err_option`, `is_ok`, `is_err`) are already ignored in the tests. They could become type-indexed constructors (`| is_some (σ : Ty) : Fun [x ∷ ty.option σ] ty.bool`), as with P13, or simply be inlined: each is a single match.

*Tests.* `sum-unions-*` (broken templates); `match-unions-nested*` (failing); nothing for single-instantiation specialisation. Easy to write from the `AccessType` snippet.

### P03. Nested patterns inside sum constructors — M (mostly hidden behind P02)

*What it is.* `translate-union-as-sum` only accepts a plain binder under `Some`/`MemValue`/…:
```sail
match dispatchInterrupt(cur_privilege) {
  Some(intr, priv) => { handle_interrupt(intr, priv); ... },   // tuple pattern inside Some
  None() => ...
}
match arch { Some(RV32) => ..., _ => internal_error(...) }      // enum literal inside Some
```
(`Match.ml:2081/2082`, "only binder patterns supported in sum constructors".)

*Usage.* 4 definitions now (`step`, `dispatchInterrupt`, `translationMode`, `legalize_satp64`), 11 once P02 option 2 is applied. `step` itself is among them.

*Options.*
1. Bind a fresh variable and match on it again: the general nested-pattern compilation that `Match.ml` already does for variant unions.
   Tex's limitation note says this was simply not done yet. **M.**
2. Have Sail split such patterns before translation (there is no ready-made rewrite for this). Not recommended.

*Tests.* `sum-unions-option-tuple-match` (missing templates) is close. `match-unions-nested*` covers the variant-union case.

### P16. Sail externs that are not mapped to µSail — M plus decisions

*What it is.* Sail `val`s with an extern binding and no body, e.g. `val string_append = pure {c: "concat_str", …}`. They are translated as plain `stm_call`s to functions that do not exist. **No `stm_foreign` is generated anywhere.**
1697 calls to 71 externs (`full-workaround/dangling-calls.csv`), in four groups:

| group | fns | calls | examples | proposed treatment |
|---|---:|---:|---|---|
| strings / printing | 16 | 1399 | `string_append` 798, `eq_string` 402, `concat_str` 107, `bits_str`, `print_reg`, `print_platform` | **remove tracing**: treat `print*` as no-ops and string values as opaque (or drop the code under `get_config_print_*()`). Nearly all is assembly printing (P09) and trace output. |
| arithmetic / bitvector builtins | 28 | 144 | `bitvector_access` 26, `eq_anything` 22, `shift_bits_left/right`, `plain_vector_access`, `sub_vec`, `pow2`, `tdiv_int`, `count_leading_zeros` | map to µSail `bop`/`uop` where one exists; otherwise a small library of µSail functions (like the existing `sail_ones` handling) |
| `undefined_*` | 3 (+2 skipped by configuration) | 99 (+23) | `undefined_bitvector` 95, `undefined_Capability` 21 | decide the semantics: a foreign function with a trivial contract (nondeterministic, sound) is better than a fixed default value |
| platform / configuration / memory | 24 | 55 | `sys_enable_*`, `plat_ram_base`, `plat_rom_size`, `sys_pmp_count`, `__read_mem`, `__write_mem`, `cancel_reservation` | foreign functions (`FunX`) with hand-written contracts; configuration constants could become Rocq parameters |

*Recommendation.* Generate `stm_foreign` plus a `FunX` declaration for every extern, with a configuration hook to map an extern to a µSail primitive or to a no-op. With that in place, the rest is configuration and contracts. **M.**

### P05. `foreach`, `loop`, `try`, early `return` — M

*What it is.*
```sail
function clause execute (RISCV_CLZ(rs1, rd)) = {
  let rs1_val = X(rs1);
  result : nat = 0; done : bool = false;
  foreach (i from (sizeof(xlen) - 1) downto 0)
    if not(done) then if rs1_val[i] == bitzero then result = result + 1 else done = true;
  X(rd) = to_bits(sizeof(xlen), result);  RETIRE_SUCCESS
}
```
(`Function.ml:1265–1268`: `AE_return`, `AE_try`, `AE_for`, `AE_loop`.)

*Usage in CHERIoT.*
- `foreach`: 32 source lines in 8 files; 21 untranslated definitions, of which 16 are Zb* bit-manipulation `execute` clauses (CLZ, CTZ, CPOP, REV8, ORCB, CLMUL*, ZIP, XPERM, BREV8), plus `pmpCheck`, `pmpWriteCfgReg`, `init_pmp`, `to_bytes_le` and `from_bytes_le`.
- `try`: `try_execute`, `main`.
- `loop`: `loop`.
- early `return`: `mem_read_cap_revoked` (CHERIoT revocation check, reachable from `execute_LoadCapImm`).

*Options.*
1. **Unroll `foreach` with constant bounds** (all CHERIoT loops have `xlen`/`pmp_count` bounds).
   Simple and faithful. Contracts see straight-line code, which is longer but automatic. **S–M.**
2. Translate to a recursive µSail helper function, one per loop, which needs a contract or loop invariant.
   Cleaner for long loops, but µSail has no loop invariants, so every loop needs a hand-written contract. **M.**
3. `try`/`throw`: only `main` and `try_execute` catch, both at the top level. Translating `throw` as `stm_fail` and dropping `try` is sound for safety properties (exceptions become failures). Check this against what CHERIoT proofs need. **S.**
4. Early `return`: Sail's `early_return` rewrite eliminates `return`. It is in `Rewrites.ml`, but **commented out** (line 219), so first find out why. **S.**
5. `loop` (`main`'s outer loop): out of scope for function-level contracts. Ignore it by configuration.

*Recommendation.* Option 1 (unroll), plus options 4, 3 and 5. *Tests.* None.

### P04. Vector literal with `bits(1)` elements — S, high value

*What it is.* In Sail 0.20.3, `bit` is `bitvector(1)` (`lib/flow.sail:58`), so `[e1, …, en]` with single-bit elements has elements of type `Bitvector[1]`. `Function.ml:394` only accepts elements of type `Bit`.
```sail
function bool_to_bits x = [bool_to_bit(x)]
function getCapPerms(cap) : Capability -> CapPermsBits = [
  bool_to_bit(cap.perm_user0), bool_to_bit(cap.permit_seal), ..., bool_to_bit(cap.global) ]
```
*Usage.*
- 27 definitions, 17 reachable. Among them are **both decoders, `encdec_backwards` and `encdec_compressed_backwards`** (generated by Sail from bit-pattern mappings), plus `encdec_capmode_*`, `encCapabilityToCapability`, `getCapPerms`, `readCSR`, `writeCSR`, `execute_FENCE(_TSO)` and `bool_to_bits`.
- Callers: `bool_to_bits` 17 calls, `getCapPerms` 5 calls.

*Fix.* Accept `Bitvector[1]` elements and translate the literal as a concatenation (`bop.bvapp`), or as a `bv` literal when all elements are constants. **S.** *Tests.* None; trivial to add (`[bool_to_bit(x), bool_to_bit(y)]`).

### P01. Integer arguments that must be known at compile time — M

*What it is.* Several µSail operations take a width or amount as a Rocq-level `nat`, but in Sail it is a run-time `int` (or a type variable):
```sail
let c_b = 0 - a_hi;                                           // int
let base : CapAddrBits = truncate((a_top + c_b) @ c.B @ zeros(E), cap_addr_width);  // bits + int
assert(false, file ^ ":" ^ dec_str(line) ^ ": " ^ s);          // non-literal message
function tag_addr_to_addr(tag_addr : tagaddrbits) -> xlenbits = tag_addr @ zeros(log2_cap_size)
```
*Sites.*
- `FunctionCalls.ml:358` (`add_bits_int`) → `getCapBoundsBits`, `__WriteRAM_Meta`.
- `:350` (bitvector size) → `pmpAddrRange`.
- `:533` (`sail_assert` message) → `internal_error`, `report_invalid_width`.
- `:544` "expected constant in bitvector type" → 47 holes, 45 of them in `encdec_compressed_forwards`, plus `execute_CSetHigh` and `tag_addr_to_addr`.
- `:495/:680` (`sail_zero_extend`/`sign_extend`/index not constant) → `zero_extend`, `sign_extend`, `sail_mask`, `align_down`, `wV`.

13 definitions, 10 reachable, including **`getCapBoundsBits`, the core of CHERI bounds decoding**, and `internal_error`, which is called from 32 places.

*Options, per site.*
1. **Done.** `add_bits_int(v, i)` with a run-time `i`: translate as `v + to_bits(i)`. RiscvPmp has `to_bits (l : nat) : Fun [value ∷ ty.int] (ty.bvec l)`, and Katamaran may have a primitive int→bv conversion (to check). **S.**
2. **Next in line.** `sail_assert` with a non-literal message: drop the message (replace it with a fixed string). Messages have no verification meaning. **S.**
3. Widths that are constant after constant folding (`log2_cap_size`, `cap_addr_width`, `sizeof(xlen)`): extend `extract_compile_time_integer` to look up top-level `let`s that P08 currently fails to translate, and to evaluate `log2`/`MAX`, or run Sail's `constant_fold` with more aggressive settings. **S–M.**
4. Widths that really are polymorphic (`zero_extend`, `sign_extend`, `align_down` inside their own definitions): the same machinery as P13. Their *calls* are fine.

*Tests.* `let-shadow-*` and `bitvectors-ones-with-*` cover `sail_ones`/`sail_zeros`; nothing covers `add_bits_int`, `sail_assert` messages or `@ zeros(K)` with a top-level `K`.

### P08. Top-level `let` with a computed value — S–M

*What it is.*
```sail
let cap_max_addr = MAX(cap_addr_width)                     // E_app
let null_cap : Capability = struct { tag = false, ... }    // E_struct
let root_cap_exe : Capability = { max_bounds_cap with permit_execute = true, ... }   // E_struct_update
let zero_reg : regtype = null_cap                          // E_id
```
(`ValueDefinition.ml:21/23/31/32`.) 22 definitions, 16 reachable: `null_cap`, `root_cap_exe/mem/seal`, `max_bounds_cap`, `cap_max_addr`, `cap_max_otype`, `cap_len_width`, the `MTIME*`/`MSIP` MMIO bases, `default_write_acc` and `init_ext_ptw`. CHERIoT has 63 top-level `let`s in total.

*Options.*
1. Evaluate at translation time when the value is a closed expression over literals and other top-level values. That covers all 22, assuming `MAX`/`pow2` are available. The result is a Rocq `Definition` of the right `Val` type. **S–M.**
2. Translate as nullary µSail functions. This always works, but every use becomes a call, and contracts must mention them.

*Recommendation.* Option 1, falling back to option 2. *Tests.* `value-definition-*` cover literals only.

### P06. Mutable local variables — M

*What it is.*
```sail
var cperms : bits(cap_cperms_width) = zeros();
cperms[5] = bool_to_bit(cap.global);
if cap.permit_execute & ... then { cperms[4..3] = 0b01; cperms[2] = ...; }
```
(`Function.ml:1137`, "assignment to local variable (nonregister)".) After Sail's rewrites, most mutable locals have already been turned into `let`s. The 4 left over are all reachable and all CHERI core: `setCapBounds`, `capToEncCap`, `execute_CSpecialRW` and `execute_LoadCapImm`. 28 `var` lines in 4 files.

*Options.*
1. SSA-style renaming in nanosail: each assignment becomes a new `let`, and joins after `if` return the updated variables as a tuple. That is the standard technique, and Sail's own assignment rewrites do the same for other backends. **M.**
2. Use more of Sail's own rewrites. `exp_lift_assign` is enabled, but `letbind_effects` is commented out in `Rewrites.ml` (line 298); check why, and why these four functions survive. Possibly **S**; try this first.

*Tests.* None.

### P07. Register references — S (+S)

*What it is.* Sail generates a setter `_set_<Bitfield>_<field>(ref r, v)` for every bitfield type. Assignments to a register's bitfield field use it:
```sail
mstatus->MIE() = 0b0;      // in execute_CJALR
misa->X() = 0b1;           // in ext_init_regs
```
Its type uses `register('a)` (`Recursive.ml:70`, "unknown type register"), and `ref misa` hits `AV_ref` (`Function.ml:410`).

*Usage.* 209 setters (plus `__deref` and 4 `sail_mark_register*`-style library functions), 430 items in all. **They are all generated, and all unused except in 3 functions**: `execute_CJALR`, `ext_init` and `ext_init_regs`. The sources have 42 `x[F] = e` lines.

*Options.*
1. Ignore `_set_*` by configuration (removes about 420 items of noise). **S.**
2. Translate `f(ref r, …)` when `f` is known, by inlining: read `r`, update the field, write `r`. µSail has no first-class register references, but the target register is known statically at every call site. **S.**

*Tests.* `bitfield-assign` (fails) and `model-005..008`.

### P10. Vectors of non-bit elements (register arrays) — M

*What it is.* `register pmpcfg_n : vector(64, dec, Pmpcfg_ent)` and `register pmpaddr_n : vector(64, dec, xlenbits)`, indexed by run-time values:
```sail
pmpcfg_n[n*4 + 3].bits @ pmpcfg_n[n*4 + 2].bits @ ...
```
(`Type.ml:170`, `Vector` → NYI.) 29 holes in 13 definitions; 4 reachable: `pmpReadCfgReg`, `pmpReadAddrReg`, `pmpWriteAddrReg` and the `Reg` type.

*Options.*
1. Split each array into 64 registers, with a `match` on the index (like `rX`). This mirrors RiscvPmp (`pmp0cfg`, …). The code is large but simple. **M.**
2. µSail lists (`ty.list`). Not a good fit for registers.
3. If CHERIoT is configured with `sys_pmp_count() == 0` (to check), ignore the PMP functions by configuration. **S.**

### P11. Numeric types — S each

- `if` inside a numeric expression (`Recursive.ml:376`, `Nexp_if`): `X(rd) = to_bits(32, if len > cap_max_addr then cap_max_addr else len)` in `execute_CGetLen`/`CGetTop`. Erase it to `int`. **S.**
- `range(…)` as a type in some positions (`Type.ml:260`: `CapAddrInt`, `CapLen`; `Match.ml:3075`: `regidx_to_regno`). Erase to `int`, as is already done elsewhere. **S.**
- Structs with `Int` parameters: `struct TLB_Entry('asidlen: Int, 'valen: Int, …)` (`Function.ml:591` `Typ_app`, `:363` "Unexpected type"). Affects `match_TLB_Entry`, `flush_TLB_Entry`, `make_TLB_Entry` and `translate32`. Instantiate at the single use, or ignore Sv32/TLB if CHERIoT has no virtual memory (to check). **S.**

### P09. Mappings over strings and bitvectors — S (drop)

*What it is.* Sail turns each `mapping` into `X_forwards`, `X_backwards`, `X_forwards_matches` and `X_backwards_matches`. Those that `match` on a `string` (`Match.ml:3069`, 70 definitions) or on a bitvector scrutinee with literal patterns (`Match.ml:3071`, 70 definitions) are not translated.
36 mappings are affected (of 87 in the model):
- the whole `assembly` syntax (`*_mnemonic`, `reg_name`, `csr_name_map`, `hex_bits_N`);
- the string side of the small `encdec_*` helper mappings.

None is needed for execution. The decoder direction `encdec_backwards` fails only on P04.

*Recommendation.* Ignore `assembly`, `*_mnemonic`, `*_name*`, `hex_bits*`, `sep`, `spc`, `opt_spc` and the like by configuration, together with the print/string externs (P16). Supporting `match` on bitvector literals is still useful for hand-written decoders: **S–M**, and the test `bitvectors-matching` exists.

### P12. Overloads and pragmas reported as untranslated — S (noise)

`DEF_overload` (485) and `DEF_pragma` (229) hit `not_yet_implemented` in `Translation.ml:101/107`. Both are meaningless after Sail's type checker has run: overloads are already resolved, and pragmas like `$include_start`/`$file_start` are bookkeeping.
Return `IgnoredDefinition`, as `DEF_fixity` does. This removes 714 of the 1405 untranslated blocks. **S.**

### P14. Definitions skipped by configuration (hidden gaps) — S–M

What each predicate actually removed (full model): `*_of_num` 108 functions, `num_of_*` 109, `undefined_*` 145, `regval_of_*`/`*_of_regval` 0, `log2`, `ones`, `zeros_implicit`, `reverse_bits_in_byte`, `slice_mask`, `sail_ones`, `sail_zeros` (val only). The predicate for `sep` matches nothing: `sep` is a mapping, so its functions are `sep_forwards` etc. (P09). The inventory's own additions are the 5 hard failures from §2b.

What callers still need (from `dangling-calls.csv`):
- `undefined_Capability` 21 calls, `undefined_Privilege` 1;
- `zeros_implicit` 13 calls, `ones` 4;
- `num_of_ExceptionType` 1;
- `internal_error` 32 calls, `getCapBoundsBits` 4, `__WriteRAM_Meta` 1, `pmpAddrRange` 1.

Treatment:
- `undefined_*`: see P16.
- `zeros_implicit`/`ones`: translate calls natively, as is already done for `sail_zeros`/`sail_ones` (`docs/handover.md`).
- `*_of_num`/`num_of_*`: enum ↔ int conversions. Generate them, or keep them out if never used (1 call).

### P15. Ignored casts — S (check)

`Statements.ml:651` drops `E_typ` casts (157 warnings in 22 definitions): 126 in `encdec_compressed_forwards`, 6 in `assembly_forwards`, and the rest in `execute_ZBB_RTYPE`, `init_sys`, `htif_store`, …
A cast is harmless if source and target µSail types agree. Assert that, or emit an explicit conversion for `int`↔`range`/`bits`↔`bits('n)`. **S.**

## 6. Suggested order of attack

Aim: a `machine.v` that compiles for the RV32I + CHERIoT instruction subset, with the non-essential parts (assembly, printing, V, Zb*, Sv32) ignored by configuration rather than implemented.

1. ~~**P00** (rewrite pipeline)~~ and ~~**`add_bits_int`**~~ (done), the **`sail_assert` message** (P01 option 2, next: the first hard failure the full model now hits), excluding `pmpAddrRange` by configuration, and **P12** (overloads, pragmas): unblock the full model without workarounds and remove most of the noise. S + S + S.
2. **Configuration clean-up** (no translator change):
   - ignore P09 mappings and printing, the `_set_*` setters (P07 option 1), the concurrency-interface types (`Access_kind`, `Mem_*_request`), and V/Sv32/TLB if unused;
   - add the `translate-union-as-sum` lines from `workarounds/configuration-sum-variant.lisp` as a stopgap.
   This also makes the inventory much easier to read.
3. **P04** (bit literals: unblocks both decoders), **P08** (top-level lets: `null_cap`, root caps), **P11** (numeric types), and the S parts of **P01** (`sail_assert` message, `add_bits_int`, constant widths). All S, all on the critical path.
4. **P16**: emit `stm_foreign`/`FunX` for externs, with a mapping hook. This is the point where `machine.v` can start to type-check at all.
5. **P03**, then **P02** option 1 (specialise single-instantiation unions).
6. **P13** + P02 option 3: nat-indexed `Fun` constructors and union families, following RiscvPmp. This is the largest item.
   It unblocks the whole memory path: LOAD, STORE, capability load/store, fetch.
   Start with a design note and a 20-line e2e test.
7. **P06**, **P05** (unroll loops; drop `try`; early `return` rewrite), **P07** option 2, **P10** if PMP is in scope, **P15**.

At each step, rerun `hard-failure-loop.sh` and `extract.py` (see below) to measure progress. Item 4 is the first point at which `coqc machine.v` is meaningful, so coqc errors are not yet part of this inventory.

## 7. Open questions for you

1. **Scope.** Which ISA subset should the first verified CHERIoT model cover?
   For example RV32E/I + M + Zicsr + CHERIoT capability instructions, without Zb*, V, Sv32 or PMP. This decides whether P05 (Zb* loops), P10 (PMP) and P11's TLB part need implementing or just ignoring.
2. **P13/P02 direction.** Is nat-indexed constructors à la RiscvPmp acceptable as the target design? If so, check `vm_reflexivity` on parametric contracts with a Katamaran maintainer.
3. **`undefined` semantics.** Nondeterministic foreign function (sound, slightly more proof work), or a fixed default value (simpler, unsound in general)?
4. **Exceptions.** Is translating `throw` to `stm_fail` and dropping `try` acceptable for the properties you plan to prove?
5. **Printing and strings.** OK to drop all trace and printing code (the `get_config_print_*` branches) rather than model strings?
6. ~~**Rewrite pipeline.** May `remove_blocks` and the final `recheck_defs` be dropped from `Rewrites.ml`, or does something else depend on them? This needs a full e2e run to decide.~~ Answered: `remove_blocks` was dropped; the full e2e run is unchanged. The final `recheck_defs` stays.
7. **Housekeeping found along the way.** Fix the `sum-unions-*` templates, the NYI numbering, and the Makefile's `#` (GNU make ≥ 3.82 requirement)? Each is a few lines.
8. **Environment.** The new `sail-backend` switch has no Katamaran install, because Katamaran `main`'s full dune build fails in `theories/Staging`. Should Katamaran be installed properly (for example via its `rocq920` package or the Makefile, which skips `Staging`)?

## Files in this directory

| path | what |
|---|---|
| `prelude/`, `prelude.log` | step 2a (case-study `make test`) |
| `full/`, `full.log` | step 2b as specified (Sail crash) |
| `full-workaround/` | **the inventoried run**: `base.v`, `machine.v`, `log.txt` (148 MB, `VERBOSE=4`), `coqc-*.log`, `hard-failures.log`, `dangling-calls.csv` |
| `variant-more-sums/` | P02/P03 experiment |
| `coqc-probe/` | coqc with `NYI` axiomatised |
| `bisect/` | `try.sh`, source lists, prefix runs, `rewrite-dump/` (AST after each rewrite), `loop/` |
| `workarounds/` | `splice.sail`, `configuration.lisp` (+ `make-config.py`, `hard-failure-ignores.txt`, `hard-failures.log`, `hard-failure-loop.sh`), `configuration-sum-variant.lisp` |
| `extract.py` | items 3–7 → `items.csv` / `items.json` (fields: run, stage, kind, name, sail_loc, ocaml_loc, message, construct, where, snippet) |
| `reachability.py` | → `items-reachable.csv` (adds `reachable`) |
| `piles.py` | → `piles-<run>.csv` and the pile table |
| `dangling-calls.py` | → `<run>/dangling-calls.csv` |
| `summarize.py`, `feature-counts.sh` / `feature-counts.txt` | per-construct summary; source-level feature counts |

**To regenerate** (with `OPAMSWITCH=sail-backend`, `ROCQPATH` pointing at a Katamaran build, and `bin/sail-local` built):
```sh
workarounds/hard-failure-loop.sh                 # or reuse workarounds/configuration.lisp
VERBOSE=4 EXTRA="-splice $PWD/workarounds/splice.sail" bisect/try.sh ../full-workaround bisect/full-sources.txt workarounds/configuration.lisp
(cd full-workaround && coqc base.v > coqc-base.log 2>&1; coqc machine.v > coqc-machine.log 2>&1)
python3 extract.py full-workaround prelude --out items.csv --json items.json
python3 reachability.py && python3 piles.py full-workaround && python3 dangling-calls.py full-workaround
```
The directory takes about 790 MB, mostly the `VERBOSE=4` logs and the rewrite dumps.
