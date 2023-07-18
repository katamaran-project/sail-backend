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

Module Import ArgumentsProgram <: Program DefaultBase.

  Section FunDeclKit.

    Inductive Fun : PCtx -> Ty -> Set :=
    | int_args : Fun ["n" ∷ ty.int; "m" ∷ ty.int] ty.int
    | string_args : Fun ["s" ∷ ty.string] ty.unit
    | bool_args : Fun ["b" ∷ ty.bool] ty.unit
    | unit_args : Fun ["()" ∷ ty.unit] ty.unit
    | list_args : Fun ["l" ∷ (ty.list ty.unit)] ty.unit.

    Definition 𝑭  : PCtx -> Ty -> Set := Fun.
    Definition 𝑭𝑿 : PCtx -> Ty -> Set := fun _ _ => Empty_set.
    Definition 𝑳  : PCtx -> Set := fun _ => Empty_set.

  End FunDeclKit.

  Include FunDeclMixin DefaultBase.

  Section FunDefKit.

    Definition fun_int_args : Stm ["n" ∷ ty.int; "m" ∷ ty.int] ty.int :=
      stm_exp (exp_int 1%Z).

    Definition fun_string_args : Stm ["s" ∷ ty.string] ty.unit :=
      stm_exp (exp_val ty.unit tt).

    Definition fun_bool_args : Stm ["b" ∷ ty.bool] ty.unit :=
      stm_exp (exp_val ty.unit tt).

    Definition fun_unit_args : Stm ["()" ∷ ty.unit] ty.unit :=
      stm_exp (exp_val ty.unit tt).

    Definition fun_list_args : Stm ["l" ∷ (ty.list ty.unit)] ty.unit :=
      stm_exp (exp_val ty.unit tt).

    Definition FunDef {Δ τ} (f : Fun Δ τ) : Stm Δ τ :=
      match f in Fun Δ τ return Stm Δ τ with
      | int_args => fun_int_args
      | string_args => fun_string_args
      | bool_args => fun_bool_args
      | unit_args => fun_unit_args
      | list_args => fun_list_args
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

End ArgumentsProgram.

