Require Export base.

(*<
  (generate (program-translation))
>*)

(* One constructor per polymorphic function, with the widths as implicit arguments, in order of appearance *)
Check (@extend_to 16 64 : Fun [ "m" ∷ ty.int; "v" ∷ ty.bvec 16 ] (ty.bvec 64)).
Check (@fail_with ty.unit : Fun [ "message" ∷ ty.string ] ty.unit).

(* and one definition for all widths *)
Check (@fun_extend_to : forall {n m : nat}, Stm [ "m" ∷ ty.int; "v" ∷ ty.bvec n ] (ty.bvec m)).

(* Rocq infers the widths at the call *)
Goal fun_extend_byte =
  stm_let "ga#0" ty.int (stm_exp (exp_int 32%Z))
    (stm_call extend_to (env.snoc (env.snoc env.nil (_∷_) (exp_var "ga#0")) (_∷_) (exp_var "b"))).
Proof. reflexivity. Qed.

(* Inside, the extension is a foreign call (uop.zext would need n <= m) *)
Goal forall n m, @fun_extend_to n m =
  stm_let "m" ty.int (stm_exp (exp_var "m"))
    (stm_foreign sail_zero_extend (env.snoc (env.snoc env.nil (_∷_) (exp_var "v")) (_∷_) (exp_var "m"))).
Proof. reflexivity. Qed.

(* A discarded result whose type nothing determines is unit *)
Goal fun_stop_then_continue = stm_seq (@stm_call _ _ _ (@fail_with ty.unit) (env.snoc env.nil (_∷_) (exp_string "stop"))) (stm_exp (exp_int 0%Z)).
Proof. reflexivity. Qed.

(* A bit is a bitvector of one bit *)
Check (flag : Reg (ty.bvec 1)).
