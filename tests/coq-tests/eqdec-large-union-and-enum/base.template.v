(*<
  (generate (base-translation))
>*)

(*<
  (if (untranslated-definitions?)
    (generate "Theorem UNTRANSLATED_DEFINITIONS : False. Qed."))
>*)

(* eq_dec computes on closed values: equal values give left eq_refl, different ones right. *)
Definition decide {A : Type} `{EqDec A} (x y : A) : bool :=
  if eq_dec x y then true else false.

Lemma big_equal : eq_dec E59 E59 = left eq_refl.
Proof. reflexivity. Qed.

Lemma big_different : decide E0 E59 = false.
Proof. reflexivity. Qed.

Lemma instr_equal_nullary : eq_dec I0 I0 = left eq_refl.
Proof. reflexivity. Qed.

Lemma instr_equal_enum : eq_dec (I3 E42) (I3 E42) = left eq_refl.
Proof. reflexivity. Qed.

Lemma instr_equal_fields : decide (I2 (bv.of_N 3) (bv.of_N 7)) (I2 (bv.of_N 3) (bv.of_N 7)) = true.
Proof. reflexivity. Qed.

Lemma instr_different_fields : decide (I2 (bv.of_N 3) (bv.of_N 7)) (I2 (bv.of_N 3) (bv.of_N 8)) = false.
Proof. reflexivity. Qed.

Lemma instr_different_constructors : decide (I3 E1) (I13 E1) = false.
Proof. reflexivity. Qed.

Lemma instr_nested_union : decide (I7 (SmallB E5)) (I7 (SmallB E6)) = false.
Proof. reflexivity. Qed.

Lemma instr_eq_dec_refl (i : Instr) : eq_dec i i = left eq_refl.
Proof. apply eq_dec_refl. Qed.

Lemma registers_different : decide (existT _ r_big) (existT _ r_instr) = false.
Proof. reflexivity. Qed.

Lemma registers_equal : eq_dec (existT _ r_pair) (existT _ r_pair) = left eq_refl.
Proof. reflexivity. Qed.

(*

(*<
  (generate (untranslated-definitions))
>*)

*)
