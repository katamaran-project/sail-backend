(*<
  (generate (base-translation))
>*)

Theorem test : forall instr : instruction, exists x : Z, exists y : Z, instr = Pop x y.
Proof.
  destruct instr as [a b]; exists a; exists b; trivial.
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
