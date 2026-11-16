Require Export base.

(*<
  (generate (program-translation))
>*)

(* One family indexed by the width; its closed uses are the family at that width *)
Check (@MkS 4 bv.zero true : S 4).
Check (last : Reg (ty.record (Rs 2))).
Check (@wrap : forall (n : nat) {H : IsTrue _}, Fun [ "b" ∷ ty.bvec n ] (ty.record (Rw n))).
