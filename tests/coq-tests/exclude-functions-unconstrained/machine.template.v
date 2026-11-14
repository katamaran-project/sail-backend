Require Export base.

(*<
  (generate (program-translation))
>*)

(* The excluded functions are not in Fun *)
Fail Check pmp_allows.
Fail Check privilege_of.

(* The foreign function that produces an unconstrained value allows every value
   of its type, never fails, and leaves the registers and the memory unchanged. *)
Lemma unconstrained_allows_every_value (v : Val ty.bool) γ μ :
  ModelProgram.ForeignCall (unconstrained ty.bool) env.nil (inr v) γ γ μ μ.
Proof. cbn. eauto. Qed.

Lemma unconstrained_never_fails σ (inhabitant : ModelProgram.Unconstrained σ) (res : string + Val σ) γ γ' μ μ' :
  ModelProgram.ForeignCall (@unconstrained σ inhabitant) env.nil res γ γ' μ μ' ->
  γ' = γ /\ μ' = μ /\ exists v, res = inr v.
Proof. cbn. auto. Qed.

(* The call to pmp_allows is an unconstrained Boolean *)
Goal fun_checked_read =
  stm_let "ga#0" ty.bool (stm_foreign (unconstrained ty.bool) env.nil)
    (stm_if (stm_exp (exp_var "ga#0"))
       (stm_exp (exp_val (ty.bvec 8) [bv 0]))
       (stm_exp (exp_val (ty.bvec 8) [bv 255]))).
Proof. reflexivity. Qed.
