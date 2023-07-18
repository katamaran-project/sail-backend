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

Module Import Types_and_literalsProgram <: Program DefaultBase.

  Section FunDeclKit.

    Inductive Fun : PCtx -> Ty -> Set :=
    | unit_lit : Fun ["()" ∷ ty.unit] ty.unit
    | bool_pair : Fun ["()" ∷ ty.unit] (ty.prod ty.bool ty.bool)
    | int_tupple : Fun ["()" ∷ ty.unit] (ty.prod (ty.prod ty.int ty.int) ty.int)
    | string_list : Fun ["()" ∷ ty.unit] (ty.list ty.string)
    | mix_tuple : Fun ["()" ∷ ty.unit]
        (ty.prod (ty.prod (ty.prod ty.int ty.unit) (ty.prod ty.bool ty.int))
          ty.string).

    Definition 𝑭  : PCtx -> Ty -> Set := Fun.
    Definition 𝑭𝑿 : PCtx -> Ty -> Set := fun _ _ => Empty_set.
    Definition 𝑳  : PCtx -> Set := fun _ => Empty_set.

  End FunDeclKit.

  Include FunDeclMixin DefaultBase.

  Section FunDefKit.

    Definition fun_unit_lit : Stm ["()" ∷ ty.unit] ty.unit :=
      stm_exp (exp_val ty.unit tt).

    Definition fun_bool_pair : Stm ["()" ∷ ty.unit] (ty.prod ty.bool ty.bool) :=
      stm_exp (exp_binop bop.pair (exp_true) (exp_false)).

    Definition fun_int_tupple : Stm ["()" ∷ ty.unit]
        (ty.prod (ty.prod ty.int ty.int) ty.int) :=
      stm_exp
        (exp_binop bop.pair (exp_binop bop.pair (exp_int (-1%Z)) (exp_int 0%Z))
          (exp_int 1%Z)).

    Definition fun_string_list : Stm ["()" ∷ ty.unit] (ty.list ty.string) :=
      stm_exp
        (exp_list
          (cons (exp_string "This")
            (cons (exp_string "is") (cons (exp_string "fine.") nil)))).

    Definition fun_mix_tuple : Stm ["()" ∷ ty.unit]
        (ty.prod (ty.prod (ty.prod ty.int ty.unit) (ty.prod ty.bool ty.int))
          ty.string) :=
      stm_exp
        (exp_binop bop.pair
          (exp_binop bop.pair
            (exp_binop bop.pair (exp_int 100%Z) (exp_val ty.unit tt))
            (exp_binop bop.pair (exp_false) (exp_int (-2%Z))))
          (exp_string "true")).

    Definition FunDef {Δ τ} (f : Fun Δ τ) : Stm Δ τ :=
      match f in Fun Δ τ return Stm Δ τ with
      | unit_lit => fun_unit_lit
      | bool_pair => fun_bool_pair
      | int_tupple => fun_int_tupple
      | string_list => fun_string_list
      | mix_tuple => fun_mix_tuple
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

End Types_and_literalsProgram.

