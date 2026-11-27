Require Import base machine_decl.

(*The functions of model.sail*)

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

Definition fun_main : Stm [
                            "ж_7"  ∷  ty.unit
                          ]
                          (ty.unit) :=
  stm_call step (env.snoc (env.nil)
                          (_::_)
                          ((exp_val (ty.unit) (tt)))%exp).
