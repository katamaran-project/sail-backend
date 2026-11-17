Require Export base.

(*<
  (generate (program-translation))
>*)

(* Setting and clearing a bit *)
Goal @semantics_bitvector_update 8 (bv.of_Z 0x0f) 6%Z (bv.of_Z 1) = bv.of_Z 0x4f.
Proof. reflexivity. Qed.

Goal @semantics_bitvector_update 8 (bv.of_Z 0x0f) 2%Z (bv.of_Z 0) = bv.of_Z 0x0b.
Proof. reflexivity. Qed.

Goal @semantics_bitvector_update 8 (bv.of_Z 0x0f) 2%Z (bv.of_Z 1) = bv.of_Z 0x0f.
Proof. reflexivity. Qed.

(* The call's result is given by the semantics; the memory is unchanged *)
Lemma bitvector_update_semantics γ μ :
  ModelProgram.ForeignCall (@bitvector_update 8)
    (env.snoc (env.snoc (env.snoc env.nil ("arg1" ∷ ty.bvec 8) (bv.of_Z 0x0f))
       ("arg2" ∷ ty.int) 6%Z) ("arg3" ∷ ty.bvec 1) (bv.of_Z 1))
    (inr (bv.of_Z 0x4f)) γ γ μ μ.
Proof. cbn. auto. Qed.
