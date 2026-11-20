(*<
  base-translation
>*)

Theorem test : forall x : instruction, exists n, x = Pop n.
Proof.
  intro x; destruct x as [n]; exists n; reflexivity.
Qed.

(*<
  (when untranslated-definitions "Theorem UNTRANSLATED_DEFINITIONS : False. Qed.")
>*)

(*

(*<
  untranslated-definitions
>*)

*)
