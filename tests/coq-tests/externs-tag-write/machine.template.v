Require Export base.

(*<
  (generate (program-translation))
>*)

(* A tag write may change the memory in any way *)
Lemma MEMw_tag_changes_memory (a : Val (ty.bvec 64)) (t : bool) γ μ μ' :
  ModelProgram.ForeignCall MEMw_tag
    (env.snoc (env.snoc env.nil ("arg1" ∷ ty.bvec 64) a) ("arg2" ∷ ty.bool) t)
    (inr tt) γ γ μ μ'.
Proof. cbn. eauto. Qed.

(* A tag read leaves the memory unchanged *)
Lemma MEMr_tag_changes_nothing args (res : string + Val ty.bool) γ γ' μ μ' :
  ModelProgram.ForeignCall MEMr_tag args res γ γ' μ μ' ->
  γ' = γ /\ μ' = μ /\ exists v, res = inr v.
Proof. cbn. auto. Qed.
