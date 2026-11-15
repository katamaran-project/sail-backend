(*<
  (generate (base-translation))
>*)

(*<
  (if (untranslated-definitions?)
    (generate "Theorem UNTRANSLATED_DEFINITIONS : False. Qed."))
>*)

(* The values are Rocq definitions, so proofs can compute with them *)
Lemma root_cap_tag : Cap_tag root_cap = true.
Proof. reflexivity. Qed.

Lemma len_width_value : len_width = 33%Z.
Proof. reflexivity. Qed.

Lemma zero_reg_is_null_cap : zero_reg = null_cap.
Proof. reflexivity. Qed.

(*

(*<
  (generate (untranslated-definitions))
>*)

*)
