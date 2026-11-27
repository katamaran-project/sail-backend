Require Export base.

(*<
  program-translation
>*)

(* A newtype parameter becomes arg#0; the other parameters keep their names *)
Check (fun_rX : Stm [ "arg#0" ∷ ty.union Uregno ] (ty.bvec 32)).
Check (fun_rX_bits : Stm [ "arg#0" ∷ ty.union Uregidx ] (ty.bvec 32)).
Check (fun_wX_bits : Stm [ "arg#0" ∷ ty.union Uregidx; "data" ∷ ty.bvec 32 ] (ty.bvec 32)).
