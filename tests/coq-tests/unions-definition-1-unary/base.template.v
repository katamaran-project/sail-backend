(*<
  (generate (base-translation))
>*)

Theorem test : forall x : instruction, exists n, x = Pop n.
Proof.
  intro x; destruct x as [n]; exists n; reflexivity.
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
