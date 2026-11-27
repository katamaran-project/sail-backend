Require Import base machine_decl.

(*The functions of tt_registers.sail*)

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

Definition fun_toggle : Stm [
                              "m"  ∷  ty.enum Emode
                            ]
                            (ty.enum Emode) :=
  stm_match_enum Emode
                 (stm_exp (exp_var "m"))
                 (fun K => match K with
                           | Off => stm_exp (exp_val (ty.enum Emode) (On))
                           | On  => stm_exp (exp_val (ty.enum Emode) (Off))
                           end).

Definition fun_Mk_flags : Stm [
                                "v"  ∷  ty.bvec (2)
                              ]
                              (ty.bvec (2)) :=
  stm_exp (exp_var "v").

Definition fun__get_flags_bits : Stm [
                                       "v"  ∷  ty.bvec (2)
                                     ]
                                     (ty.bvec (2)) :=
  stm_let "ga#0"
          (ty.int)
          (stm_exp (((exp_int 2%Z))-((exp_int 1%Z))))
          (stm_exp (exp_unop (uop.vector_subrange 0 2) (exp_var "v"))).

Definition fun__update_flags_bits : Stm [
                                          "v"  ∷  ty.bvec (2);
                                          "x"  ∷  ty.bvec (2)
                                        ]
                                        (ty.bvec (2)) :=
  stm_let "ga#1"
          (ty.int)
          (stm_exp (((exp_int 2%Z))-((exp_int 1%Z))))
          (stm_exp (exp_binop (bop.update_vector_subrange 0 2) (exp_var "v") (exp_var "x"))).

Definition fun__get_flags_C : Stm [
                                    "v"  ∷  ty.bvec (2)
                                  ]
                                  (ty.bvec (1)) :=
  stm_exp (exp_unop (uop.vector_subrange 1 1) (exp_var "v")).

Definition fun__update_flags_C : Stm [
                                       "v"  ∷  ty.bvec (2);
                                       "x"  ∷  ty.bvec (1)
                                     ]
                                     (ty.bvec (2)) :=
  stm_exp (exp_binop (bop.update_vector_subrange 1 1) (exp_var "v") (exp_var "x")).

Definition fun__get_flags_Z : Stm [
                                    "v"  ∷  ty.bvec (2)
                                  ]
                                  (ty.bvec (1)) :=
  stm_exp (exp_unop (uop.vector_subrange 0 1) (exp_var "v")).

Definition fun__update_flags_Z : Stm [
                                       "v"  ∷  ty.bvec (2);
                                       "x"  ∷  ty.bvec (1)
                                     ]
                                     (ty.bvec (2)) :=
  stm_exp (exp_binop (bop.update_vector_subrange 0 1) (exp_var "v") (exp_var "x")).
