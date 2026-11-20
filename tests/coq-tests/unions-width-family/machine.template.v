Require Export base.

(*<
  program-translation
>*)

(* One family indexed by the width; its closed uses are the family at that width *)
Check (Val_bits (bv.zero) : Res_bits 32).
Check (last : Reg (ty.union (Ures_bits 16))).
Check (@read : forall (p n : nat) {H : IsTrue _}, Fun [ "width" ∷ ty.int ] (ty.union (Ures_bits p))).

(* Other uses are copies, as before *)
Check (Val_bool true : Res_bool).
