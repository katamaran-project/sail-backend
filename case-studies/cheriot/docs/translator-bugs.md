# Translator bugs found by the CHERIoT loop

Bugs (wrong or rejected output, wrong measurements), not missing features; see [roadmap.md](roadmap.md) for those.
Commit hashes are those on the branch at the time of writing.

## Fixed

### A model type named `S` broke `machine.v`'s `sail_constraint`
- Found: e2e `bitvectors-variables` (`struct S('m)`), once the struct translated: "Unknown constructor: S" in `machine.v`.
- Cause: the generated tactic refolds `Nat.add`/`Nat.mul` with `match n with | O => m | S p => ...`; the model's `Record S` shadows Peano's `S`.
- Fix: the tactic writes `Datatypes.O`/`Datatypes.S`.
```coq
(* before *) change (fix add ... := match n with | O => m | S p => S (add p m) end) with Nat.add in *;
(* after  *) change (fix add ... := match n with | Datatypes.O => m | Datatypes.S p => Datatypes.S (add p m) end) with Nat.add in *;
```

### Inlined scrutinee lost a width family's width
- Found: e2e `unions-width-family` failed after merging the read path.
- Cause: `let x = S in match x` printed as `match S` dropped the `let`'s type; a width family's union tag prints its width as `_` (`Ures_bits _`), so Rocq could not infer it.
- Fix: `cce22be3`: no inlining when the bound type is a width family.

### Tuple match binders were bound in reverse order
- Found: Rocq rejected a 3-tuple match of `(bits(8), bool, enum)` in a new test.
- Cause: `stm_match_tuple` binders `[a; b; c]` were printed `tuplepat_snoc (tuplepat_snoc (tuplepat_snoc tuplepat_nil c) b) a`; `tuplepat_snoc` adds at the end, so `a` was bound to the last component.
  With components of one type, Rocq accepted it and the meaning was wrong: CHERIoT's `(aq, rl, res)` flag triples (`read_kind_of_flags`, `mem_write_ea`, `mem_write_value_priv_meta`, `encdec_backwards`) had `aq` and the last flag swapped.
- Fix: the first binder is innermost. Test `match-tuple-union-option` (`pick`).
```coq
(* before *) stm_match_tuple e (tuplepat_snoc (tuplepat_snoc (tuplepat_snoc tuplepat_nil "c") "b") "a") ...
(* after  *) stm_match_tuple e (tuplepat_snoc (tuplepat_snoc (tuplepat_snoc tuplepat_nil "a") "b") "c") ...
```

### Unions at a flow-typed boolean had no instance
- Found: `phys_mem_write` untranslated ("union MemoryOpResult with type parameter used at non-closed type arguments `bool('_#result)`").
- Cause: `let result = MemValue(write_ram(...))` has type `MemoryOpResult(bool('p))`; the union instances were looked up by the type argument as written, and `bool('p)` is not closed.
- Fix: `bool('p)` and `int('n)` are `bool` and `int` in union type arguments (the translation erases the index anyway). Test `size-constraints-hypotheses` (`flag_result`).
```sail
let r = Ok(width > 4)    // r : Res(bool('p)), now the instance Res(bool)
```

### A dead branch with a false width hypothesis stalled `coqc`
- Found: after the decoder fix, the probe stalled at `execute_STORE` (45 s limit); with the `DOUBLE` arm replaced by a failure, all of `machine.v` compiled.
- Cause: Sail turns the guard of `DOUBLE if sizeof(xlen) >= 64 => mem_write_value(paddr, 8, rs2_val, ...)` into `if gteq_int(32, 64)` after `remove_impossible_int_cases`, so nothing folds it; its branch is checked assuming the false condition, and passes a `bits(32)` where `mem_write_value` at 8 bytes expects a `bits(64)`. Rocq searched for the false width hypothesis.
- Fix: an `if` whose condition is known at compile time (integer comparisons of constants, `not`, `and`, `or`, literals, value exclusions) becomes its live branch. Test `if-constant-condition`.
```sail
DOUBLE if sizeof(xlen) >= 64 => mem_write_value(paddr, 8, rs2_val, aq, rl, false),   // sail-riscv's execute STORE
```
```coq
(* before *) | DOUBLE => stm_if ((stm_exp ((exp_int 32%Z) >= (exp_int 64%Z)))) (stm_call (mem_write_value (kid__n := 8)) ...) (stm_call report_invalid_width ...)
(* after  *) | DOUBLE => stm_call report_invalid_width ...
```

### Slice indices known only to Sail's solver were holes
- Found: e2e `assert-flow-typing-constrained-width`.
- Cause: µSail's slices take their indices as Rocq numbers; the translator only accepted literals, not an index whose value Sail's flow typing knows (`assert(n == 32); v[n - 1 .. 0]`).
- Fix: such an index (an argument without an effect whose value `solve_unique` finds) becomes a literal before the translation (`rewrite_solved_indices`), and a width the solver determines becomes a constant (`normalise_widths`).
```coq
(* before *) stm_let "w" (ty.bvec (NYI[0])) (stm_exp (exp_unop (uop.vector_subrange NYI[1] NYI[2]) (exp_var "v")))
(* after  *) stm_let "w" (ty.bvec (32))      (stm_exp (exp_unop (uop.vector_subrange 0 32) (exp_var "v")))
```

### Struct patterns untranslated
- Found: e2e `match-record` failed ("expected pattern AP_struct (_, _) while matching type Record").
- Cause: the pattern translation had no case for struct patterns.
- Fix: a match with a single case whose pattern is a struct of variables and wildcards always matches; it destructures the record (`stm_match_record`) and binds the variables to its fields. Other struct patterns stay untranslated.
```sail
match s { struct { foo = x, bar = y, qux = z } => x }
```
```coq
(* after *) stm_match_record Rmystruct (stm_exp (exp_var "s")) (recordpat_snoc (... "foo" "жfoo0") ...) (stm_exp (exp_var "жfoo0"))
```

### Scattered functions took their arguments as one tuple
- Found: e2e `functions-ternary-scattered` failed: `scatfoo(XXX, 1)` passed two arguments to a `Fun` with one parameter.
- Cause: Sail's `merge_function_clauses` makes the clauses one function of a single variable, `merge#var`, bound to the tuple of the arguments.
- Fix: when the `val` declares several parameters and the pattern is one variable of a tuple type, the function gets one parameter per element and binds `merge#var` to their tuple first. Test `functions-ternary-scattered`.
```coq
(* before *) | scatfoo : Fun ["merge#var" ∷ ty.prod (ty.enum Emyenum) ty.int] ty.int
(* after  *) | scatfoo : Fun ["merge#var#1" ∷ ty.enum Emyenum; "merge#var#2" ∷ ty.int] ty.int
```

### Constructor patterns nested two levels deep
- Found: e2e `match-unions-nested-2` failed ("pattern is incompatible with type of value being matched").
- Cause: `merge_nested_constructor_patterns` built the inner match on a constructor's fields straight into a pattern tree, which takes no nested patterns; a pattern nested one level deeper (`C1(B1(A1(a, b)))`) reached it.
- Fix: the match on a single field is translated like any match (`translate_cases`), which merges at that level in turn.
```sail
match x { C1(B1(A1(a, b))) => "a", C2(A2(a, b)) => "b", C1(B2(_)) => "c", _ => "z" }
```

### Record field of a tuple type with three or more elements
- Found: e2e `records-definition-with-triple` failed in `base.v`.
- Cause: the Rocq type of a tuple of three or more was `T1 * T2 * T3`, but µSail's `ty.tuple` has Katamaran's `EnvRec` values, `((unit * T1) * T2) * T3`, so `record_fold` did not fit the record.
- Fix: such a tuple's Rocq type is `unit * T1 * T2 * T3` (a pair stays `T1 * T2`, µSail's `ty.prod`). The check in `type-alias-of-tuple` now builds `(tt, true, (e, i), b)`.
```sail
struct MyStruct = { foo : (int, int, int) }
```
```coq
(* before *) Record MyStruct := MkMyStruct { MyStruct_foo : (Z * Z * Z)%type }.
(* after  *) Record MyStruct := MkMyStruct { MyStruct_foo : (unit * Z * Z * Z)%type }.
```

### Function names clash with Katamaran's
- Found: e2e `model-008` failed: a Sail function named like a Katamaran definition clashes in `machine.v`.
- Cause: the translation keeps Sail's names; Katamaran's `DefaultRegStoreKit` declares `read_register` and `write_register`.
- Fix: the configuration function `rename` (no automatic scheme, by decision: a clash fails in Rocq until the configuration renames it). `model-008` renames both.
```sail
function write_register(r, w) = { ... }
```
```
(* before *) machine.v: Error: The label write_register is already declared.    (* DefaultRegStoreKit *)
(* after  *) (rename '(("read_register" "read_register_model") ("write_register" "write_register_model")))
```

### The decoder took 608 s in `coqc`
- Found: the probe stalled at `encdec_backwards` (45 s limit); a diagnostic run without the limit took 608 s, then 72 s at `End FunDefKit`.
- Cause: Sail's ANF names every matched value, `let x = S in match x { ... }`; the decoder is a chain of about 130 such matches (one per encoding), each nested in the previous one's last case, so every `let` lengthens the context of all later ones (a synthetic chain of 160: 49 s with the lets, 6.5 s without).
  Closing the `FunDefKit` section, which has no variables, made Rocq check every definition in it again.
- Fix: a `let` whose body is a match on it, with cases that do not use it, is printed as a match on the bound statement; `FunDefKit` is no longer a section. `encdec_backwards` 6 s, `End FunDefKit` gone. Test `match-scrutinee-without-let`.
```coq
(* before *) stm_let "ga#2252" (ty.sum (ty.union Uast) ty.unit) (...) (stm_match_sum (stm_exp (exp_var "ga#2252")) ...)
(* after  *) stm_match_sum (...) ...
```

### Discarded call to a type-polymorphic function behind argument lets: `stm_seq`'s type not inferable
- Found: by hand-compiling `machine.v` past the probe's stop, after the decoders' fallbacks entered the scope (`execute_NOT_C_CAPMODE`).
- Cause: `pp_sequence` fixes the discarded type of a call such as `internal_error : forall ('a : Type). ... -> 'a` to `ty.unit`, but only for a bare call; with a computed argument the call sits under a `let`, and Rocq cannot infer `σ`.
- Fix: the check looks through the `let`s around the call. Test `sequence-discarded-polymorphic-call`.
```sail
function clause execute(NOT_C_CAPMODE(s)) = {
  internal_error(__FILE__, __LINE__, "should be unreachable: execute(NOT_C_CAPMODE(" ^ BitStr(s) ^ "))");
  RETIRE_FAIL
}
```
```coq
(* before *) stm_seq (stm_let "ga#0" ty.string (...) (stm_call internal_error ...)) ...        (* ?σ *)
(* after  *) @stm_seq _ _ ty.unit (stm_let "ga#0" ty.string (...) (stm_call internal_error ...)) ...
```

### A binder in a `match` on a `bool` was dropped
- Found: while translating `match` on a `bool` (`encdec_backwards`); the pattern tree logged "ignoring binder while matching bool".
- Cause: the `Bool` node of the pattern tree keeps no binders, so `s => ... s ...` left `s` unbound.
- Fix: a binder case becomes a wildcard case whose body starts with `let s = <matched value>`. Test `match-bool-mapping`.
```sail
match bool_not_bits_backwards(mapping0#) { s => Some(DIV((rs2, rs1, s))) }    // Sail's rewrite of the DIV encoding
```
```coq
(* after *) stm_let "ga#15" (ty.bool) (stm_call bool_not_bits_backwards ...) (... exp_var "ga#15" ...)
```

### Tuple pattern at a tuple type abbreviation untranslated
- Found: `ext_handle_data_check_error` untranslated ("expected pattern AP_tuple _ while matching type Alias").
- Cause: patterns and matches were translated by the matched type as is; an abbreviation (`Alias`) was treated as an atomic type, which only a variable or wildcard pattern matches.
- Fix: both match the abbreviated type. Test `match-tuple-type-abbreviation`.
```sail
type ext_data_addr_error = (CapEx, capreg_idx)
function ext_handle_data_check_error(err : ext_data_addr_error) -> unit = {
  let (capEx, regnum) = err;
  handle_cheri_cap_exception(capEx, regnum)
}
```
```coq
(* after *) stm_match_prod (stm_exp (exp_var "err")) "ж10" "ж11" (stm_call handle ...)
```

### Concatenation widths behind type abbreviations were holes
- Found: the loop's top blocker, `tag_addr_to_addr` (4 instructions); also `ext_data_get_addr`, `execute_CSetHigh`, `writeCSR`.
- Cause: `@` read each operand's width from its type as is; `tagaddrbits` is `bits(xlen - log2_cap_size)`, `bits(32 - 3)` after Sail's rewrites, not a constant: `NYI` ("expected constant in bitvector type").
- Fix: aliases stripped and the width simplified; a width that stays symbolic is printed as in types. Test `bitvectors-concat-width-from-type-constant`.
```sail
type tagaddrbits = bits(xlen - log2_cap_size)
function tag_addr_to_addr(tag_addr : tagaddrbits) -> bits(xlen) = tag_addr @ zeros(log2_cap_size)
```
```coq
(* before *) exp_binop (@bop.bvapp _ 3 NYI[0]) (exp_var "ga#0") (exp_var "tag_addr")
(* after  *) exp_binop (@bop.bvapp _ 3 29) (exp_var "ga#0") (exp_var "tag_addr")
```

### Comment listing untranslated definitions is not valid Rocq
- Found: the loop's plain Rocq stopped in `base.v` at the template's `Theorem UNTRANSLATED_DEFINITIONS : False. Qed.` (fails by design); behind it, the comment with the Sail source of untranslated definitions.
- Cause: Rocq lexes string literals and nested comments inside comments; an odd number of `"` or a `*)` in the Sail source ends or breaks the comment ("Unterminated comment").
- Fix: `(untranslated-definitions)` escapes each listing (`( *`, `* )`; all `"` become `'` when their number is odd); the CHERIoT template marks the listing with a comment instead of the theorem. Test `untranslated-listing-comment`.
```sail
function guarded() = try { print_endline("an odd \" quote, a *) and a (*"); 1 } catch { Oops() => 0 }
```
```coq
(* before *) print_endline("an odd \" quote, a *) and a (*");    (* inside the listing comment *)
(* after  *) print_endline('an odd \' quote, a * ) and a ( *');
```

### `ones(n)` printed as the number 1
- Found: while working on `zero_extend(ones(n))`; Rocq accepted the output, so no test caught it.
- Cause: with `literal-zeros-and-ones` off (the default), `sail_ones(n)` with a known `n` was printed with Katamaran's `bv.one`, which is the number 1.
- Fix: `49397585` prints `bv.ones n` (all bits set).
```sail
let x : bits(8) = sail_ones(8)
```
```coq
(* before *) exp_val (ty.bvec 8) (@Bitvector.bv.one 8)    (* = 1 *)
(* after  *) exp_val (ty.bvec 8) (@Bitvector.bv.ones 8)
```

### Discarded calls lose the type that fixes implicit widths
- Found: Rocq could not infer the implicit result width of a size-polymorphic call whose value is discarded (`init_sys`; calls passed to excluded functions or registers).
- Cause: a discarded `let x : τ = f(...)` became `f(...); ()`, and later the unused `let` was dropped; `τ` was the only thing that fixed the callee's implicit width.
- Fix: `48ce7460` keeps the `let` with its type, and `simplify_unused_let_binder` keeps a `let` around a call. Test `exclude-discarded-width`.
```sail
val widen : forall 'n 'm, 'm >= 'n. (implicit('m), bits('n)) -> bits('m)
register scratch : bits(32)   // excluded by the configuration
function reset_scratch() = scratch = widen(0x00)
```
```coq
(* before *) stm_seq (stm_call widen ...) (stm_exp (exp_val ty.unit tt))    (* minimal; kid__m unknown *)
(* after  *) stm_let "ж1" (ty.bvec (32)) (stm_call widen ...) (stm_exp (exp_val (ty.unit) (tt)))
```

### `bit` typed as `ty.bool`
- Found: one-bit values clashed with their own type in Rocq.
- Cause: Sail 0.20's library defines `type bit = bitvector(1)`; the translator already made bit literals one-bit bitvectors (`[bv 1]`) but still typed `bit` as `ty.bool`, and as an NYI in Rocq-level types.
- Fix: `b09a4c80`: `bit` is `ty.bvec 1` (`bv 1` in Rocq-level types).
```sail
val foo : unit -> bit
function foo() = bitone
```
```coq
(* before *) ... (ty.bool) := stm_exp (exp_val (ty.bvec 1) ([bv 1]))    (* minimal *)
(* after  *) ... (ty.bvec (1)) := stm_exp (exp_val (ty.bvec 1) ([bv 1]))
```

### Numeric expressions simplified or printed wrongly
- Found: ill-typed widths in size-polymorphic functions (the memory path, `8 * ('n / 8)`).
- Cause:
  - `simplify` dropped `Neg` and `PowerOf2` of a non-constant: `-n` became `n`, `2 ^ n` became `n`;
  - `8 * (n / 8)` was printed `8 * n / 8`;
  - modulo was printed `%`, a scope delimiter in Rocq.
- Fix: `b09a4c80` keeps the operators, parenthesises the right operand of `*`, `/`, `mod`, prints `mod`; unit tests in `SimplificationTests.ml`, `NumericExpressionPrettyPrintingTests.ml`.

### Rocq record projections clash across structs
- Found: `base.v` stopped at `Record Capability`: "reserved already exists" (`Capability` shares `reserved`, `B`, `T`, `address` with `EncCapability`).
- Cause: Rocq projections are global names; Sail structs may share field names.
- Fix: `55b9b7b3`: the projection of field `f` of record `R` is `R_f`, always. µSail refers to fields by string, so `machine.v` is unchanged. Test `records-shared-field-names`.
```sail
struct Cap    = { address : bits(32), tag : bool }
struct EncCap = { address : bits(32), reserved : bool }
```
```coq
(* before *) Record EncCap : Set := MkEncCap { address : bv 32; ... }.        (* address already exists *)
(* after  *) Record EncCap : Set := MkEncCap { EncCap_address : bv 32; ... }.
```

### `*` in tuple abbreviations read as multiplication
- Found: `base.v` stopped at `ext_control_addr_error`.
- Cause: `Definition ext_control_addr_error := CapEx * capreg_idx` is parsed in `nat_scope`.
- Fix: `13f995df` prints `(A * B)%type`, which also keeps nested tuples apart. Test `type-alias-of-tuple`.
```sail
type err_and_index = (Err, bits(6))
```
```coq
(* before *) Definition err_and_index := Err * bv 6.
(* after  *) Definition err_and_index := (Err * bv 6)%type.
```

### Values named like types
- Found: 10 duplicate `Definition`s in `base.v` (`type X : Int = ...` next to `let X = sizeof(X)`).
- Cause: Sail has separate namespaces for types and values, Rocq has one.
- Fix: `13f995df`: the type keeps the name (Rocq types use it), the value is `<name>_value` (µSail code uses it). Test `names-type-and-value-shared`.
```sail
type word_width : Int = 32
let word_width = sizeof(word_width)
```
```coq
Definition word_width := 32.
Definition word_width_value : Z := (32)%Z.        (* before: a second Definition word_width *)
... stm_exp (exp_val (ty.int) (word_width_value))
```

### Compound bitvector widths not parenthesised
- Found: `base.v` failed at `CapBits` (`type CapBits = bits(8 * cap_size)`).
- Cause: the width was printed at top level: `bv 8 * 8` parses as `(bv 8) * 8`.
- Fix: `4e969f03` prints the width at argument level (also negative constants). Test `type-alias-of-bitvector-arithmetic-width`.
```coq
(* before *) Definition CapBits := bv 8 * 8.
(* after  *) Definition CapBits := bv (8 * 8).
```

### Tuples of three or more elements built as nested pairs
- Found: Rocq rejected every function that builds a triple (`read_kind_of_flags`, `mem_write_ea`, `encdec_mul_op_backwards`, ...); e2e `tuples-triple`, `tuples-quad`, `match-tuple-triple-2` failed.
- Cause: `expression_of_tuple` built `bop.pair (bop.pair x y) z` (`ty.prod (ty.prod ..) ..`) while the type was `ty.tuple [..]`.
- Fix: `b09a4c80`: a pair stays `bop.pair`, a longer tuple is `exp_tuple`.
```sail
val foo : unit -> (int, int, int)
function foo() = (1, 2, 3)
```
```coq
(* before *) stm_exp (exp_binop bop.pair (exp_binop bop.pair (exp_int 1%Z) (exp_int 2%Z)) (exp_int 3%Z))
(* after  *) stm_exp (exp_tuple [exp_int 1%Z; exp_int 2%Z; exp_int 3%Z])
```

### Constant shift amounts one bit too narrow
- Cause: the width of a constant shift amount was `Z.log2up`, one bit short for powers of two: 2 became `[bv 2]` of width 1, 4 `[bv 4]` of width 2.
- Fix: `33319e3c` uses `Z.numbits`.

### References to top-level values as free program variables
- Found: references to CHERIoT constants (`null_cap`, `cap_len_width`, ...).
- Cause: every reference to a top-level `let`, even a literal one, was `exp_var "x"`, a variable not in the function's context.
- Fix: `ba47e37d`: `exp_val <type> <name>`, with the value as a Rocq `Definition`. Test `toplevel-let-computed`.
```coq
(* before *) stm_exp (exp_var "max_addr")
(* after  *) stm_exp (exp_val (ty.bvec (32)) (max_addr))
```

### Field access through an alias of a record type
- Cause: field access on a value whose type is an alias of a record (`zero_reg : regtype`, `type regtype = Capability`) looked up the record definition under the alias name.
- Fix: `ba47e37d` resolves the alias first. Test `toplevel-let-computed`.

### Run-time shift amount stopped the whole translation
- Cause: a shift by an amount not known at compile time was a hard failure (`Failure`) instead of giving up on one definition.
- Fix: `115d7189`: the amount is converted with `uop.get_slice_int` to the width of the shifted bitvector.

### Size-polymorphic memory reads ill-typed, counted as generated
- Found: when unions with type parameters became monomorphic copies, 7 functions went from generated to untranslated.
- Cause: `mem_read`, `mem_read_meta`, `mem_read_priv`, `mem_read_priv_meta`, `phys_mem_write`, `pmp_mem_read`, `rvfi_read` use `MemoryOpResult` at a type with a size variable; their µSail applied `ty.union` to a type with a free variable, and the loop counted them as generated.
- Fix: `01eabff0` leaves such definitions untranslated (that run's "worse" accepted as the baseline).
```coq
ty.union Umemoryopresult (ty.bvec (8 * kid__p8_times_n / 8))    (* before: emitted, ill-typed *)
```

### `Derive EqDec`/`NoConfusion` too slow for CHERIoT
- Found: `coqc base.v` took 591 s (`Derive EqDec` for `astConstructor` 210 s, `ast` 149 s, `RegName` 98 s, `Enums` 55 s; `NoConfusion` for `ast` 31 s); the probe never finished the `depelim` proof of `𝑹𝑬𝑮_eq_dec` (about 14,000 obligations for 120 registers).
- Cause: Equations' derivations are quadratic in the number of constructors.
- Fix: `99f541c8`: linear instances for enums, unions and `sigT Reg` (`nanosail/NanosailToMicrosail/EqDec.ml`); records keep `Derive`. `base.v` 591 s -> 13 s. Test `eqdec-large-union-and-enum`.
```coq
(* before *) Derive NoConfusion for ast.  Derive EqDec for ast.
(* after  *) #[export] Instance Big_EqDec : EqDec Big :=
               nanosail_eqdec_via_N Big_eqdec_to_N Big_eqdec_of_N (...).
```

### E2e tests that could not run
- Found: 8 of 33 failing e2e tests failed before the translator ran.
- Cause:
  - the six `sum-unions-*` tests were committed (`998b14e5`) without `base`/`machine.template.{v,html}`;
  - `functions-flip`: `enum my_bit = { zero, one }` captures local variables of Sail's own `vector.sail`;
  - `monomorphize-type`: old `vector('a, 'n)` syntax, no prelude.
- Fix: `0278fb81` (templates added, constructors renamed, syntax updated); `monomorphize-type` then crashed in the translator (see Open) and was retired in `b2b44476`.

### Before the loop (`f9171dfb`, #65)
- Sail crashed in the plugin's rewrites: `remove_blocks` turned `assert(c); e` into `let _ = assert(c) in e`, and `recheck_defs` then lost the bound `c` gives; `remove_blocks` dropped.
```sail
assert(0 < VLEN & VLEN <= sizeof(vlenmax));   // gives the bound...
print_reg(BitStr(v[VLEN - 1 .. 0]))            // ...that this slice needs
```
- `make_sequence` ran block statements out of order: `[a; b; c]` became `Seq(b, Seq(c, a))` (hidden by the crash).
- Statements of a block had no `unit` type, so `exit` inside a block did not translate.
- `add_bits_int` failed with a run-time int and with a width behind an alias (`xlenbits`).

## Open

### Scrutinee inlining checks only the bound type for width families
- `cce22be3` keeps the `let` only when its own type is a width family. A tuple, sum or list containing one, or a union with other type parameters, is still inlined; built from constructors, its tag prints `(tag _)` and the width may be undetermined. No test hits it yet.

### `NYI` numbers do not match their explanations
- The code numbers holes with a global counter (`GC.fresh_index`); the comment above each definition numbers them per definition (frame) from 0.
- `tests/cheriot/lib` pairs them by order instead.
```coq
(*[0] : Not yet implemented; see nanosail/NanosailToMicrosail/FunctionCalls.ml line 773 (expected constant in bitvector type)*)
Definition fun_tag_addr_to_addr : Stm [...] ... :=
  ... (stm_exp (exp_binop (@bop.bvapp _ 3 NYI[3]) (exp_var "ga#283") (exp_var "tag_addr"))).
```

### `NYI` messages print a literal `%s`
- `FunctionCalls.ml` (slices, `update_vector_subrange`) passes `"%s's indices should be known at compile time"` as the message without formatting it.
```coq
[1] : Not yet implemented; see nanosail/NanosailToMicrosail/FunctionCalls.ml line 908 (%s's indices should be known at compile time)
```

### `argument-types-of-polymorphic-function-calls` crashes
- With the retired `monomorphize-type` model, the template function raises `GenerationContext.FrameException` ("should only be executed while within a frame"); still crashes on this branch. Cause not investigated.

### Monomorphisation requests never match
- Since Sail 0.20.3 the requests name size variables `'n`, the IR `kid__n`: nothing is substituted, calls are not redirected.
- The `monomorphize-*` tests were retired (`b2b44476`); size-polymorphic functions now use implicit arguments instead.

## Loop and measurement bugs

### Words in string literals counted as calls
- Found: the user noticed that `execute_NOT_CAPMODE` only calls `internal_error`, yet the loop listed the load path among its blockers.
- Cause: the call graph took every identifier in a body as a use, string literals included: `"should be unreachable: execute(NOT_CAPMODE("` made `execute_NOT_CAPMODE` call `execute`, whose cone has the whole load path.
- Fix: `76415d3d` skips string literals (`tests/cheriot/lib/callgraph.py`); `NOT_CAPMODE` and `NOT_C_CAPMODE` are clean (65 of 67).
```sail
function clause execute(NOT_CAPMODE(s)) = {
  internal_error(__FILE__, __LINE__, "should be unreachable: execute(NOT_CAPMODE(" ^ BitStr(s) ^ "))");
  RETIRE_FAIL
}
```

### The probe was skipped once the holes were gone
- Found: after `align_down` lost its last holes, the ratchet reported the probe stopping earlier, at plain Rocq's first error.
- Cause: the loop ran the probe only for a file with `NYI` (or the old marker), but the probe also axiomatises bodies that call a missing function.
- Fix: a file with such a body needs the probe too (`needs_probe`).

Bugs in `tests/cheriot` and the case study configuration, not the translator.

### Call graph merged all operators into one node
- `function operator <_s` and the other 4 operators were one node `operator`, so generated ignores named them wrongly.
- Fix: `c6e58096` names them as the translator does (`operator_barltbar_barsbar`); 4 functions joined the scope.

### Call graph missed functions with a doc comment
- A definition after `/*! ... */` was not a node (9 of 1959 functions, among them `capToBits`); found by the analysis's check that no in-scope code calls excluded code (`encCapToBits` was excluded).
- Fix: `c6e58096`.

### Call graph kept calls to inlined bitfield setters
- The translator inlines `_set_<B>_<F>(ref r, v)` as read, `_update_<B>_<F>`, write, and skips the setter; the call graph still had the setter, listed as a missing blocker of `CJALR`.
- Fix: `da9963ca` counts such a call as a call to `_update_*`; 5 setters left the scope.

### Clean check followed calls into policy-excluded definitions
- The scope stops at policy exclusions, the clean check did not: `translate32` led into the memory path, which counted against 23 instructions.
- Fix: `da9963ca`.

### Configuration skipped the model's own `num_of_*`
- `case-studies/cheriot/configuration.lisp` ignored every `num_of_*` (meant for Sail's generated enum conversions), including `num_of_ExceptionType` and `num_of_ext_exc_type`, which the model writes itself and trap handling calls.
- Fix: `da9963ca` (with the clean check: 15 more clean instructions).

### Rocq progress measured by position
- The ratchet counted the definitions before Rocq's first error; ignoring an earlier definition by configuration lowered it and read as a regression.
- Fix: `4e969f03` compares the sets of accepted definitions.

### Analysis silently empty with the wrong root
- `extract` read item constructs from the nanosail source next to the script; run from elsewhere, every construct was empty and most items fell out of their pile without an error. `last_dump` used a fixed file name that changes with the rewrite pipeline.
- Fix: `c6e58096`.

### Probe misread holes and implicit arguments
- `NYI[1]` as an argument was two arguments, and `[1]` a bit string; definitions with implicit binders (`Definition fun_f {kid__n : nat}`) were not recognised.
- Fix: `b09a4c80`: `(NYI (cons 1%nat nil))`; the binder pattern accepts implicit arguments.

### Probe stopped at the untranslated-definitions marker
- `Theorem UNTRANSLATED_DEFINITIONS : False. Qed.` always fails while definitions are untranslated, so the probe never reached `machine.v`.
- Fix: `73d51ac7` leaves out the marker and the comment after it (blank lines keep line numbers).

### Probe spent 47 s per axiomatised body in `fundef_bindfree`
- `vm_compute` is stuck on a body that is the `NYI` axiom and fails only after 47 s.
- Fix: `b09a4c80` admits the lemma in the probe copy when it replaced a body.

### Size-polymorphic memory reads counted as generated
- See Fixed: 7 ill-typed functions counted as generated until `01eabff0`.
