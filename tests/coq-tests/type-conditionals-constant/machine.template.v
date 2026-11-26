Require Export base.

(*<
  (generate (program-translation))
>*)

(* The conditional widths are constants: if xlen == 32 then 34 else 64 is 34, if false then 4 else 5 is 5 *)
Check (last_address : Reg (ty.bvec 34)).
Check (fun_record_access : Stm [ "address" ∷ ty.bvec 34 ] ty.unit).
Check (fun_low_index : Stm [ "address" ∷ ty.bvec 34 ] (ty.bvec 5)).
Check (fun_make_access : Stm [ "address" ∷ ty.bvec 34 ] (ty.record Raccess)).
