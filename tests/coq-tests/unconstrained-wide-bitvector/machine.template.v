Require Export base.

(* Building these inhabitants takes about a second; reducing the width made it minutes. *)
Set Default Timeout 60.

(*<
  (generate (program-translation))
>*)

(* The inhabitants at a 65536-bit width, found by type class search. *)
Definition wide : ModelProgram.Unconstrained (ty.bvec 65536) := _.
