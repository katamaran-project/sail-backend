(*<
  (generate (full-translation))

  (if (untranslated-definitions?)
    (generate "\n\nTheorem UNTRANSLATED_DEFINITIONS : False. Qed.")
>*)

Theorem imm_size_correct : imm_size = 12.
Proof. trivial. Qed.


Theorem imm_ex_size_correct : imm_ext_size = 20.
Proof. trivial. Qed.

(*

(*<
  (generate (untranslated-definitions))
>*)

*)
