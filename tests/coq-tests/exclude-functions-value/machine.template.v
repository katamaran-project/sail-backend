Require Export base.

(*<
  (generate (program-translation))
>*)

(* The excluded functions are not in Fun *)
Fail Check pmp_check.
Fail Check pmp_count.
Check checked.
Check checked_mstatus.

(* pmp_count() is 0, so the condition 0 == 0 is known at compile time and the if is its live branch *)
Goal fun_checked = stm_exp exp_false.
Proof. reflexivity. Qed.

(* pmp_check(addr, 4) is None() (the right injection) *)
Goal fun_fault_at =
  stm_let "ga#0" (ty.sum (ty.enum Eexceptiontype) ty.unit)
    (stm_exp (exp_unop (uop.inr (σ1 := ty.enum Eexceptiontype)) (exp_val ty.unit tt)))
    (stm_call is_fault (env.snoc env.nil (_::_) (exp_var "ga#0"))%exp).
Proof. reflexivity. Qed.
