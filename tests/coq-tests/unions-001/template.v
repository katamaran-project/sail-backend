(*<
  (full-translation)
>*)

Theorem test : forall x : instruction, x = Pop.
Proof.
  destruct x; trivial.
Qed.


(*

(*<
  (untranslated-definitions)
>*)

*)
