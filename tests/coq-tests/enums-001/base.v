(*** PROGRAM ***)

Module Import UntitledProgram <: Program DefaultBase.
  Section FunDeclKit.
    Inductive Fun : PCtx -> Ty -> Set :=
    .
    Definition 𝑭  : PCtx -> Ty -> Set := Fun.
    Definition 𝑭𝑿 : PCtx -> Ty -> Set := fun _ _ => Empty_set.
    Definition 𝑳  : PCtx -> Set := fun _ => Empty_set.
  End FunDeclKit.

  Include FunDeclMixin DefaultBase.

  Section FunDefKit.
    Definition FunDef {Δ} {τ} (f : Fun Δ τ) : Stm Δ τ :=
      match f in Fun Δ τ return Stm Δ τ with
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
End UntitledProgram.

(*



*)
