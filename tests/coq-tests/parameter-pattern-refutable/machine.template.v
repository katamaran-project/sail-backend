Require Export base.

(*<
  program-translation
>*)

Check (fun_radius : Stm [ "merge#var" ∷ ty.union Ushape ] (ty.bvec 8)).
Check (fun_when_set : Stm [ "p0#" ∷ ty.bvec 1; "x" ∷ ty.bvec 8 ] (ty.bvec 8)).
