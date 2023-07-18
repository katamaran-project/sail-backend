From Coq Require Import
     Strings.String
     ZArith.BinInt.

From Katamaran Require Import
     Semantics.Registers
     Program.

Import ctx.notations
       ctx.resolution.

Local Open Scope string_scope.
Local Open Scope list_scope.



(*** TYPES ***)

Import DefaultBase.



(*** PROGRAM ***)

Module Import AnfProgram <: Program DefaultBase.

  Section FunDeclKit.

    Inductive Fun : PCtx -> Ty -> Set :=
    | not_in_anf : Fun ["()" ∷ ty.unit] (ty.list ty.int).

    Definition 𝑭  : PCtx -> Ty -> Set := Fun.
    Definition 𝑭𝑿 : PCtx -> Ty -> Set := fun _ _ => Empty_set.
    Definition 𝑳  : PCtx -> Set := fun _ => Empty_set.

  End FunDeclKit.

  Include FunDeclMixin DefaultBase.

  Section FunDefKit.

    Definition fun_not_in_anf : Stm ["()" ∷ ty.unit] (ty.list ty.int) :=
      let: "ga#0" :=
        stm_exp
          (exp_binop bop.cons (exp_int 2%Z) (exp_list (cons (exp_int 3%Z) nil)))
        in stm_exp (exp_binop bop.cons (exp_int 1%Z) (exp_var "ga#0")).

    Definition FunDef {Δ τ} (f : Fun Δ τ) : Stm Δ τ :=
      match f in Fun Δ τ return Stm Δ τ with
      | not_in_anf => fun_not_in_anf
      end.

  End FunDefKit.

  Include DefaultRegStoreKit DefaultBase.

  Section ForeignKit.
    Definition Memory : Set := unit.
    Definition ForeignCall {σs σ} (f : 𝑭𝑿 σs σ) (args : NamedEnv Val σs)
      (res : string + Val σ) (γ γ' : RegStore) (μ μ' : Memory) : Prop := False.
    Lemma ForeignProgress {σs σ} (f : 𝑭𝑿 σs σ) (args : NamedEnv Val σs) γ μ :
      exists γ' μ' res, ForeignCall f args res γ γ' μ μ'.
    Proof. destruct f. Qed.
  End ForeignKit.

  Include ProgramMixin DefaultBase.

End AnfProgram.

