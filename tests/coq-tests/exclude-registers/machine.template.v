Require Export base.

(*<
  (generate (program-translation))
>*)

(* The excluded registers are not in Reg; mstatus is *)
Fail Check pmpaddr0.
Fail Check pmpcfg_n.
Check (mstatus : Reg (ty.bvec 32)).

(* The read of pmpaddr0 is an unconstrained value of its type *)
Goal fun_get_addr =
  stm_let "жreg_pmpaddr0_1" (ty.bvec 32) (stm_foreign (unconstrained (ty.bvec 32)) env.nil)
    (stm_exp (exp_var "жreg_pmpaddr0_1")).
Proof. reflexivity. Qed.

(* The write to pmpaddr0 computes the written value, and writes nothing *)
Goal fun_set_addr =
  stm_seq
    (stm_let "жreg_mstatus_2" (ty.bvec 32) (stm_read_register mstatus)
       (stm_exp (exp_binop bop.bvadd (exp_var "v") (exp_var "жreg_mstatus_2"))))
    (stm_exp (exp_val ty.unit tt)).
Proof. reflexivity. Qed.
