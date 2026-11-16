# µSail interpreter

Fuel-based big-step interpreter for Katamaran `Stm`, run with `vm_compute`.

- `validation/rocq/Interp.v`: the interpreter, generic over any `Base`/`Program`.
- `validation/rocq/InterpTests.v`: a tiny hand-written program and `Example`s.

## Build

```sh
opam exec --switch=sail-backend -- gmake -C validation/rocq
```

- Plain `coqc -Q . Validation`, Katamaran from the switch's `user-contrib` (as e2e tests).
- Clean build of both files: about 4 s.

## Interface

```coq
Module Type InterpOn (B : Base) (P : Program B).
Module MakeInterp (B : Base) (P : Program B) <: InterpOn B P.

Record State := mkState { st_regs : RegStore; st_mem : Memory }.
Definition Result τ := (string + Val τ)%type.     (* inl msg = stm_fail, inr v = stm_val *)
Definition Oracle :=
  forall Δ σ (f : 𝑭𝑿 Δ σ) (args : CStore Δ) (st : State), option (Result σ * State).
Inductive Outcome A :=
| Done (a : A)                    (* finished: value or stm_fail, in Result *)
| OutOfFuel
| NoAnswer Δ σ (f : 𝑭𝑿 Δ σ).      (* the oracle had no answer for f *)
obind : Outcome A -> (A -> Outcome B) -> Outcome B
omap  : (A -> B) -> Outcome A -> Outcome B

exec_aux  : Oracle -> nat -> Stm Γ τ -> State -> CStore Γ -> Outcome (Result τ * State * CStore Γ)
exec      : Oracle -> nat -> Stm Γ τ -> State -> CStore Γ -> Outcome (Result τ * State)
exec_call : Oracle -> nat -> 𝑭 Δ τ -> CStore Δ -> State -> Outcome (Result τ * State)
```

- Instantiate: `Module Import I := MakeInterp ModelBase ModelProgram.`
- Three cases, never mixed: `Done` (value or µSail failure), `OutOfFuel`, `NoAnswer f`.
  The first non-result reached wins (e.g. no fuel left at the foreign call: `OutOfFuel`).
- Fuel bounds recursion depth: each sub-statement, continuation and function body costs one.
  Sequenced statements each get the remaining fuel, so fuel ≈ max nesting/call depth, not step count.
- Oracle: gets the evaluated arguments and the whole state (registers + memory), as `ForeignCall` may change both.
  `None` stops the run with `NoAnswer f`. Same result type as `ForeignCall` (`string + Val σ`).
- Lemmas (`stm_lemmak`), `stm_debugk`: skipped.
- Failure: effects before `stm_fail` persist (register writes, memory); the rest is skipped.
- `exec_aux` also returns the final local store (needed by soundness).

## Soundness

- `Module Type InterpSoundOn B P SEM I`: `OracleSound oracle` (every answer allowed by `ForeignCall`) and
  `exec_aux_sound : exec_aux oracle n s st δ = Done (r, st', δ') -> ⟨γ, μ, δ, s⟩ --->* ⟨γ', μ', δ', result_stm r⟩`.
- **Stated only, not proved** (a module-type `Parameter`; nothing instantiates it).

## Tests (`InterpTests.v`)

- Registers `acc : int`, `flag : bool`, `pc : bits(32)`; memory: association list.
- Foreign `rd`, `wr` (memory), `rnd` (unconstrained); lemma `nonneg`.
- Covered: enum match (`cmp`, `main`), recursion (`sum_to`, `sum_list` with a list match), register read/write,
  bitvector add on `pc`, foreign calls through the oracle, `stm_assertk` pass/fail, `stm_fail` after a register write,
  `stm_bind`, `stm_debugk`, `let`/block/assignment scoping of the local store, `exec_call`.
- Non-results, each asserted exactly:
  - `t_fuel`: `sum_to 10` with fuel 20: `OutOfFuel`.
  - `t_rnd_none`: `rnd` without an answer: `NoAnswer rnd`; `t_rnd_none_deep`: same after a call and inside a `let`.
  - `t_rnd_fuel`: `rnd` with fuel 0: `OutOfFuel`, not `NoAnswer`.
  - `t_rnd_7`: another oracle answers `7`.
- Each: `Example ... = ... . Proof. vm_compute. reflexivity. Qed.`
- `DefaultRegStoreKit`'s `RegStore` is a function: compare states through `read_register` (`observe` in the tests).

## Limitations

- No soundness proof.
- `vm_compute` cost on the CHERIoT model (`step`) not measured yet (task 3).

## Open

- **Why `None`?** Options: (a) keep `option` (design doc signature); (b) an outcome type `Done r st | OutOfFuel | NoAnswer`.
  Taken: (b), user requirement (out of fuel told apart from every other non-result). The oracle keeps `option`.
- **Fuel measure.** Options: (a) depth (every recursive call); (b) only function calls; (c) step count (threaded fuel).
  Recommendation: (a): simplest, small `nat`, no state threading. Taken: (a).
