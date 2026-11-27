(*<
  base-translation
>*)

Theorem test : forall x : instruction, x = Pop.
Proof.
  destruct x; trivial.
Qed.

(*<
  (when untranslated-definitions "Theorem UNTRANSLATED_DEFINITIONS : False. Qed.")
>*)

(*

(*<
  untranslated-definitions
>*)

*)
