Require Export base.

(*<
  (generate (program-translation))
>*)

(* The excluded functions are not in Fun *)
Fail Check trace_write.
Fail Check print_reg.
Check write_x1.

(* The call to trace_write is the unit value *)
Goal fun_write_and_return = stm_seq (stm_exp (exp_val ty.unit tt)) (stm_exp (exp_var "v")).
Proof. reflexivity. Qed.
