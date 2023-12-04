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

Module Import PatternsProgram <: Program DefaultBase.

  Section FunDeclKit.

    Inductive Fun : PCtx -> Ty -> Set :=
    | is_empty : Fun ["l" ∷ (ty.list ty.int)] ty.bool
    | is_empty' : Fun ["l" ∷ (ty.list ty.int)] ty.bool
    | switch : Fun ["p" ∷ (ty.prod ty.int ty.bool)] (ty.prod ty.bool ty.int)
    | two_min : Fun ["l" ∷ (ty.list ty.int)] ty.bool.

    Definition 𝑭  : PCtx -> Ty -> Set := Fun.
    Definition 𝑭𝑿 : PCtx -> Ty -> Set := fun _ _ => Empty_set.
    Definition 𝑳  : PCtx -> Set := fun _ => Empty_set.

  End FunDeclKit.

  Include FunDeclMixin DefaultBase.

  Section FunDefKit.

    Definition fun_is_empty : Stm ["l" ∷ (ty.list ty.int)] ty.bool :=
      stm_match_list (stm_exp (exp_var "l")) (stm_exp (exp_true)) "h" "t"
        (stm_exp (exp_false)).

    Definition fun_is_empty' : Stm ["l" ∷ (ty.list ty.int)] ty.bool :=
      stm_match_list (stm_exp (exp_var "l")) (stm_exp (exp_true)) "h" "t"
        (stm_exp (exp_false)).

    Definition fun_switch : Stm ["p" ∷ (ty.prod ty.int ty.bool)]
        (ty.prod ty.bool ty.int) :=
      stm_match_prod (stm_exp (exp_var "p")) "l" "r"
        (stm_exp (exp_binop bop.pair (exp_var "r") (exp_var "l"))).

    Definition fun_two_min : Stm ["l" ∷ (ty.list ty.int)] ty.bool :=
      stm_match_list (stm_exp (exp_var "l")) (stm_exp (exp_false)) "a" "t"
        (stm_match_list (stm_exp (exp_var "t")) (stm_exp (exp_false)) "b" "t'"
          (stm_exp (exp_true))).

    Definition FunDef {Δ τ} (f : Fun Δ τ) : Stm Δ τ :=
      match f in Fun Δ τ return Stm Δ τ with
      | is_empty => fun_is_empty
      | is_empty' => fun_is_empty'
      | switch => fun_switch
      | two_min => fun_two_min
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

End PatternsProgram.

