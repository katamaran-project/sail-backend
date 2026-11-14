Require Export base.

(*<
  (generate (program-translation))
>*)

(* Externs that are muSail operations *)
Goal fun_shift_left_by_four = stm_exp (exp_binop bop.shiftl (exp_var "x") (exp_val (ty.bvec 3) [bv 4])).
Proof. reflexivity. Qed.

Goal fun_shift_right_by_two = stm_exp (exp_binop bop.shiftr (exp_var "x") (exp_val (ty.bvec 2) [bv 2])).
Proof. reflexivity. Qed.

Goal fun_shift_left_by_bits = stm_exp (exp_binop bop.shiftl (exp_var "x") (exp_var "y")).
Proof. reflexivity. Qed.

Goal fun_shift_right_by_bits = stm_exp (exp_binop bop.shiftr (exp_var "x") (exp_var "y")).
Proof. reflexivity. Qed.

Goal fun_subtract = stm_exp (exp_binop bop.bvsub (exp_var "x") (exp_var "y")).
Proof. reflexivity. Qed.

Goal fun_same_mode = stm_exp (exp_binop (bop.relop bop.eq) (exp_var "x") (exp_var "y")).
Proof. reflexivity. Qed.

Goal fun_top_byte = stm_exp (exp_unop (uop.vector_subrange 24 8) (exp_var "x")).
Proof. reflexivity. Qed.

Goal fun_low_byte_of = stm_exp (exp_unop (uop.get_slice_int (n := 8)) (exp_var "n")).
Proof. reflexivity. Qed.

(* No extern that became an operation is a foreign function *)
Fail Check shift_bits_left.
Fail Check sub_vec.
Fail Check eq_anything.
Fail Check truncateLSB.

(* The other externs are foreign functions with known semantics *)
Local Open Scope Z_scope.
Example emod_positive  : semantics_emod_int (-7) 3 = 2%Z.       Proof. reflexivity. Qed.
Example emod_negative  : semantics_emod_int (-7) (-3) = 2%Z.    Proof. reflexivity. Qed.
Example ediv_positive  : semantics_ediv_int (-7) 3 = (-3)%Z.    Proof. reflexivity. Qed.
Example ediv_negative  : semantics_ediv_int (-7) (-3) = 3%Z.    Proof. reflexivity. Qed.
Example quot           : semantics_quot_round_zero (-7) 2 = (-3)%Z. Proof. reflexivity. Qed.
Example rem            : semantics_rem_round_zero (-7) 2 = (-1)%Z.  Proof. reflexivity. Qed.
Example pow2_10        : semantics_pow2 10 = 1024%Z.            Proof. reflexivity. Qed.
Example min            : semantics_min_int 3 (-4) = (-4)%Z.     Proof. reflexivity. Qed.
Example max            : semantics_max_int 3 (-4) = 3%Z.        Proof. reflexivity. Qed.
Example clz_0          : semantics_count_leading_zeros (bv.of_Z 0 : bv 16) = 16%Z.    Proof. reflexivity. Qed.
Example clz_f0         : semantics_count_leading_zeros (bv.of_Z 240 : bv 16) = 8%Z.   Proof. reflexivity. Qed.
Example clz_8000       : semantics_count_leading_zeros (bv.of_Z 32768 : bv 16) = 0%Z. Proof. reflexivity. Qed.
Example slice          : (semantics_get_slice_int 8 (0x1234)%Z 8 : bv 8) = bv.of_Z 0x12. Proof. reflexivity. Qed.
Example access_3       : semantics_bitvector_access (bv.of_Z 8 : bv 8) 3 = bv.of_Z 1. Proof. reflexivity. Qed.
Example access_2       : semantics_bitvector_access (bv.of_Z 8 : bv 8) 2 = bv.of_Z 0. Proof. reflexivity. Qed.
