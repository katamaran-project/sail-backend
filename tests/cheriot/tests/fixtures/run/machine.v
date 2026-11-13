Require Export base.

Module Import CheriotProgram.

    Definition fun_neq_bits : Stm [ "x" ∷ ty.bvec 8 ] (ty.bool) :=
      stm_exp (exp_true).

    (*
      [0] : Not yet implemented; see nanosail/NanosailToMicrosail/FunctionCalls.ml line 521 (only calls to sail_zero_extend supported where second argument's value is known at compile time; was given Var[Identifier["len"],
                Type:Int[NumExpr:Var[Identifier["kid__len"]]]] instead)
    *)
    Definition fun_sail_mask : Stm [ "len" ∷ ty.int; "v" ∷ ty.bvec 8 ] (ty.bvec 8) :=
      stm_if ((stm_call neq_bits (env.snoc env.nil (_::_) (exp_var "v"))%exp))
             (stm_call truncate (env.snoc env.nil (_::_) (exp_var "v"))%exp)
             (stm_exp (exp_unop NYI[0] (exp_var "v"))).

    (*
      [0] : Not yet implemented; see nanosail/NanosailToMicrosail/Statements.ml line 7 (first hole)
      [1] : Not yet implemented; see nanosail/NanosailToMicrosail/Statements.ml line 9 (second hole)
    *)
    Definition fun_two_holes : Stm [ "x" ∷ ty.int ] (ty.int) :=
      stm_seq (stm_exp NYI[1])
              (stm_seq (stm_call bool_to_bits (env.snoc env.nil (_::_) (exp_var "x"))%exp)
                       (stm_seq (stm_call bool_to_bits (env.snoc env.nil (_::_) (exp_var "x"))%exp)
                                (stm_seq (stm_call undefined_bool env.nil)
                                         (stm_seq (stm_call mystery env.nil) (stm_exp NYI[2]))))).

End CheriotProgram.
