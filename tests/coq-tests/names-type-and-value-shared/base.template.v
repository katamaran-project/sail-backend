(*<
  (generate (base-translation))
>*)

(* The value has its own name next to the type *)
Check (word_width_value : Z).
Check (word : Set).

(*<
  (if (untranslated-definitions?)
    (generate "Theorem UNTRANSLATED_DEFINITIONS : False. Qed."))
>*)

(*

(*<
  (generate (untranslated-definitions))
>*)

*)
