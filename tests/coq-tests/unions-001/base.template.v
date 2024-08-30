(*<
  (generate (base-translation))
>*)

Theorem test : forall x : instruction, x = Pop.
Proof.
  destruct x; trivial.
Qed.

(*<
  (if (untranslated-definitions?)
    (generate "Theorem UNTRANSLATED_DEFINITIONS : False. Qed."))
>*)

(*

(*<
  (generate (untranslated-definitions))
>*)

*)
