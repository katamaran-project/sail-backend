(*<
  (generate (base-translation))
>*)

(* Tuple abbreviations are product types; three or more elements are Katamaran's Val (ty.tuple [..]), which starts with unit *)
Check (fun (e : Err) (i : bv 6) => (e, i) : err_and_index).
Check (fun (e : Err) (i : bv 6) (b : bv 2) => (tt, true, (e, i), b) : nested_tuple).

(*<
  (if (untranslated-definitions?)
    (generate "Theorem UNTRANSLATED_DEFINITIONS : False. Qed."))
>*)

(*

(*<
  (generate (untranslated-definitions))
>*)

*)
