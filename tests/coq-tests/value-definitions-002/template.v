(*<
  (generate (full-translation))

  (if (untranslated-definitions?)
    (generate "\n\nUNTRANSLATED DEFINITIONS"))
>*)

Theorem imm_size_correct : imm_size = 12.
Proof. trivial. Qed.


Theorem imm_ex_size_correct : imm_ext_size = 20.
Proof. trivial. Qed.

(*

(*<
  (generate (untranslated-definitions))
>*)

*)
