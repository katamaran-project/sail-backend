(*<
  (generate (full-translation))

  (if (untranslated-definitions?)
    (generate "\n\nUNTRANSLATED DEFINITIONS"))
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
