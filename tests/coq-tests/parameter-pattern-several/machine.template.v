Require Export base.

(*<
  (generate (program-translation))
>*)

Check (fun_add_indices : Stm [ "arg#0" ∷ ty.union Uregidx; "offset" ∷ ty.bvec 5; "arg#1" ∷ ty.union Uregidx ] (ty.bvec 5)).
