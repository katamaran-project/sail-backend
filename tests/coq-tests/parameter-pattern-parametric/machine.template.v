Require Export base.

(*<
  program-translation
>*)

Check (@fun_payload 8 _ : Stm [ "n" ∷ ty.int; "arg#0" ∷ ty.union (Usized 8) ] (ty.bvec 8)).
