(*<
  (generate (base-translation))
>*)

(* The union is used at two types, so each copy has suffixed names. *)
Check (Val_bool true : Res_bool).
Check (Exc_bool : bv 4 -> Res_bool).
Check (Val_bits_8 : bv 8 -> Res_bits_8).
Check (Exc_bits_8 : bv 4 -> Res_bits_8).

(*<
  (if (untranslated-definitions?)
    (generate "Theorem UNTRANSLATED_DEFINITIONS : False. Qed."))
>*)

(*

(*<
  (generate (untranslated-definitions))
>*)

*)
