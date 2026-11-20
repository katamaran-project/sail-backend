Require Export base.

(*<
  program-translation
>*)

(* The match is on the call, not on a variable bound to it *)
Goal ModelProgram.fun_pick =
  stm_match_sum (stm_call twice (env.snoc env.nil (_::_) (exp_var "o"))%exp)
    "x" (stm_exp (exp_var "x"))
    "ж1" (stm_exp (exp_val (ty.bvec 8) [bv 0])).
Proof. reflexivity. Qed.
