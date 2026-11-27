Require Export base.

(*<
  (generate (program-translation))
>*)

Check (fun_sum3 : Stm [ "arg#0" ∷ ty.prod (ty.bvec 8) (ty.bvec 8); "c" ∷ ty.bvec 8 ] (ty.bvec 8)).
