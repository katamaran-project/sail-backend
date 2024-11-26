(*<
  (generate (base-translation))
>*)

Theorem imm_size_correct : imm_size = 12%Z.
Proof. trivial. Qed.

(*<
  (if (untranslated-definitions?)
    (generate "Theorem UNTRANSLATED_DEFINITIONS : False. Qed."))
>*)

(*

(*<
  (generate (untranslated-definitions))
>*)

*)
