Require Export base.

(*<
  (generate (program-translation))
>*)

(* The excluded functions are not in Fun *)
Fail Check pmp_check.
Fail Check pmp_count.
Check checked.
Check checked_mstatus.

(* pmp_count() is 0, and pmp_check(addr, 4) is None() (the right injection) *)
Goal fun_checked =
  stm_let "ga#0" ty.bool (stm_exp (((exp_int 0%Z))=((exp_int 0%Z))))
    (stm_if (stm_exp (exp_var "ga#0"))
       (stm_exp exp_false)
       (stm_let "ga#1" (ty.sum (ty.enum Eexceptiontype) ty.unit)
          (stm_exp (exp_unop (uop.inr (σ1 := ty.enum Eexceptiontype)) (exp_val ty.unit tt)))
          (stm_call is_fault (env.snoc env.nil (_::_) (exp_var "ga#1"))%exp))).
Proof. reflexivity. Qed.
