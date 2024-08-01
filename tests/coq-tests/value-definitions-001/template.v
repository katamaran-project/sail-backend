(*<
  (generate (full-translation))

  (if (untranslated-definitions?)
    (generate "\n\nTheorem UNTRANSLATED_DEFINITIONS : False. Qed.")
>*)

Theorem imm_size_correct : imm_size = 12.
Proof. trivial. Qed.

(*

(*<
  (generate (untranslated-definitions))
>*)

*)
