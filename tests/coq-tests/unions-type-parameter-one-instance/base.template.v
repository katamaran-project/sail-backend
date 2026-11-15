(*<
  (generate (base-translation))
>*)

(* The union is used at one type only, so it keeps Sail's names. *)
Check (Read KData : Access).
Check (ReadWrite KCap KData : Access).
Check (Execute : Access).

(*<
  (if (untranslated-definitions?)
    (generate "Theorem UNTRANSLATED_DEFINITIONS : False. Qed."))
>*)

(*

(*<
  (generate (untranslated-definitions))
>*)

*)
