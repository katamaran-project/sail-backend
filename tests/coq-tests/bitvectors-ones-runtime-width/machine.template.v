Require Export base.

(*<
  (generate (program-translation))
>*)

(*
   The translation rewrites truncate(X @ zeros(n), w) to sail_shiftleft(truncate(X, w), n).
   Example: X = 0b1011 @ 0b10110, n = 3, w = 8.
   In Sail, X @ zeros(3) = 0b101110110000, whose 8 low bits are 0b10110000 = 176.
   bv.app takes the low bits first, as in the generated concatenation.
*)
Lemma truncate_zero_padding_example :
  bv.shiftl
    (bv.truncate 8 (bv.app (@bv.of_N 5 22) (@bv.of_N 4 11)))
    (@bv.of_Z 8 3)
  = @bv.of_N 8 176.
Proof. reflexivity. Qed.
