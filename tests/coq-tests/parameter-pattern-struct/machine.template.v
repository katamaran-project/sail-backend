Require Export base.

(*<
  (generate (program-translation))
>*)

Check (fun_first_of : Stm [ "arg#0" ∷ ty.record Rpair ] (ty.bvec 8)).
