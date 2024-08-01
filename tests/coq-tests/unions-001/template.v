(*<
  (generate (full-translation))

  (if (untranslated-definitions?)
    (generate "\n\nTheorem UNTRANSLATED_DEFINITIONS : False. Qed."))
>*)

Theorem test : forall x : instruction, x = Pop.
Proof.
  destruct x; trivial.
Qed.

(*

(*<
  (generate (untranslated-definitions))
>*)

*)
