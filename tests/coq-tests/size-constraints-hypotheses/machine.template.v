Require Export base.

(*<
  (generate (program-translation))
>*)

(* A constraint on a width is an implicit hypothesis of the constructor and of the definition *)
Check (@low_half 32 _ : Fun [ "v" ∷ ty.bvec 32 ] (ty.bvec 16)).
Check (@fun_low_half : forall {n : nat} {H : IsTrue (16 <=? Z.of_nat n)%Z}, Stm [ "v" ∷ ty.bvec n ] (ty.bvec 16)).

(* A size variable that only the constraints relate to the types is a parameter too *)
Check (@store_bytes 32 4 _ : Fun [ "width" ∷ ty.int; "data" ∷ ty.bvec 32 ] ty.unit).

(* At a concrete width, the hypothesis is computed; a caller passes on its own *)
Goal fun_low_half_of_word = stm_call low_half (env.snoc env.nil (_∷_) (exp_var "w")).
Proof. reflexivity. Qed.
Goal forall n H, @fun_low_half_twice n H = stm_call low_half (env.snoc env.nil (_∷_) (exp_var "v")).
Proof. reflexivity. Qed.

(* A size variable Rocq cannot infer is passed by name *)
Goal fun_store_word = stm_call (store_bytes (kid__n := 4)) (env.snoc (env.snoc env.nil (_∷_) (exp_int 4%Z)) (_∷_) (exp_var "w")).
Proof. reflexivity. Qed.

(* A branch that Sail's flow typing specialises: at width 4, data is a bits(32) *)
Goal forall H, @fun_store_flow 32 4 H =
  stm_let "ga#0" ty.bool (stm_exp (exp_binop (bop.relop bop.eq) (exp_var "width") (exp_int 4%Z)))
    (stm_if (stm_exp (exp_var "ga#0"))
       (stm_let "data" (ty.bvec 32) (stm_exp (exp_var "data"))      (* rebound at the branch's width *)
          (stm_seq (stm_let "ж1" (ty.bvec 32) (stm_exp (exp_var "data")) (stm_write_register r32 (exp_var "ж1")))
                   (stm_exp (exp_val ty.unit tt))))
       (stm_let "ga#1" ty.bool (stm_exp (exp_binop (bop.relop bop.eq) (exp_var "width") (exp_int 2%Z)))
          (stm_if (stm_exp (exp_var "ga#1"))
             (stm_fail ty.unit "unreachable: Sail's typing fixes kid__n = 2, kid__p8_times_n = 16 here")
             (stm_exp (exp_val ty.unit tt))))).
Proof. intros H. reflexivity. Qed.
