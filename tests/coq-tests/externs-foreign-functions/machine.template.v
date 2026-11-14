Require Export base.

(*<
  (generate (program-translation))
>*)

(* Each extern is a foreign function with the extern's signature *)
Check (platform_ram_base : FunX [ "arg1" ∷ ty.unit ] (ty.bvec 32)).
Check (read_device       : FunX [ "arg1" ∷ ty.bvec 32 ] (ty.bvec 32)).
Check (write_device      : FunX [ "arg1" ∷ ty.bvec 32; "arg2" ∷ ty.bvec 32 ] ty.unit).
Check (@popcount 8       : FunX [ "arg1" ∷ ty.bvec 8 ] ty.int).
Check (gcd_int           : FunX [ "arg1" ∷ ty.int; "arg2" ∷ ty.int ] ty.int).

(* No extern is in Fun *)
Fail Check (read_device : Fun _ _).

(* A pure extern without semantics returns any value and changes nothing *)
Lemma platform_ram_base_any_value (v : Val (ty.bvec 32)) γ μ :
  ModelProgram.ForeignCall platform_ram_base (env.snoc env.nil ("arg1" ∷ ty.unit) tt) (inr v) γ γ μ μ.
Proof. cbn. eauto. Qed.

Lemma platform_ram_base_changes_nothing args (res : string + Val (ty.bvec 32)) γ γ' μ μ' :
  ModelProgram.ForeignCall platform_ram_base args res γ γ' μ μ' ->
  γ' = γ /\ μ' = μ /\ exists v, res = inr v.
Proof. cbn. auto. Qed.

(* A monadic extern without semantics returns any value and may change the memory *)
Lemma read_device_any_value_any_memory (a v : Val (ty.bvec 32)) γ μ μ' :
  ModelProgram.ForeignCall read_device (env.snoc env.nil ("arg1" ∷ ty.bvec 32) a) (inr v) γ γ μ μ'.
Proof. cbn. eauto. Qed.

(* An extern whose semantics the configuration gives *)
Lemma gcd_int_semantics γ μ :
  ModelProgram.ForeignCall gcd_int
    (env.snoc (env.snoc env.nil ("arg1" ∷ ty.int) 12%Z) ("arg2" ∷ ty.int) 18%Z)
    (inr 6%Z) γ γ μ μ.
Proof. cbn. auto. Qed.

(* Calls are foreign calls *)
Goal fun_gcd_with_12 =
  stm_foreign gcd_int (env.snoc (env.snoc env.nil (_∷_) (exp_int 12%Z)) (_∷_) (exp_var "x")).
Proof. reflexivity. Qed.
