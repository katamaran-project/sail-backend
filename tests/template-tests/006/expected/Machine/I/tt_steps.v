Require Import base machine_decl.

(*The functions of I/tt_steps.sail*)

From Stdlib Require Import Classes.EquivDec
                           Strings.String
                           ZArith.BinInt.

From Katamaran Require Import Semantics.Registers
                              Bitvector
                              Program.

From stdpp Require Import finite.

From Equations Require Import Equations.

Import ctx.notations
       ctx.resolution
       env.notations
       bv.notations.

Local Open Scope string_scope.
Local Open Scope list_scope.

Import UntitledBase.

Import ModelProgramDecl.

Definition fun_step : Stm [
                            "ж_4"  ∷  ty.unit
                          ]
                          (ty.unit) :=
  stm_seq (stm_let "ж5"
                   (ty.enum Emode)
                   (stm_let "жreg_current_6"
                            (ty.enum Emode)
                            (stm_read_register current)
                            (stm_call toggle (env.snoc (env.nil)
                                                       (_::_)
                                                       ((exp_var "жreg_current_6"))%exp)))
                   (stm_write_register current (exp_var "ж5")))
          (stm_exp (exp_val (ty.unit) (tt))).
