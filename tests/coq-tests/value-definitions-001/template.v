(*<
  (generate (full-translation))

  (if (untranslated-definitions?)
    (generate "\n\nUNTRANSLATED DEFINITIONS"))
>*)

Theorem imm_size_correct : imm_size = 12.
Proof. trivial. Qed.

(*

(*<
  (generate (untranslated-definitions))
>*)

*)
