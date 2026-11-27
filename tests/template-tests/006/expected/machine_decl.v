Require Export base.

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

Module Export ModelProgramDecl <: FunDecl UntitledBase.
  Class Unconstrained (σ : Ty) : Set := unconstrained_inhabitant : Val σ.
  
  Ltac construct_inhabitant :=
    first [ exact bv.zero | hnf; constructor; construct_inhabitant ].
  
  #[export] Hint Extern 1 (Unconstrained _) => unfold Unconstrained; construct_inhabitant : typeclass_instances.
  
  Section FunDeclKit.
    Inductive Fun : PCtx -> Ty -> Set :=
      | toggle             : Fun[
                                  "m"  ∷  ty.enum Emode
                                ](ty.enum Emode)
      | Mk_flags           : Fun[
                                  "v"  ∷  ty.bvec (2)
                                ](ty.bvec (2))
      | _get_flags_bits    : Fun[
                                  "v"  ∷  ty.bvec (2)
                                ](ty.bvec (2))
      | _update_flags_bits : Fun[
                                  "v"  ∷  ty.bvec (2);
                                  "x"  ∷  ty.bvec (2)
                                ](ty.bvec (2))
      | _get_flags_C       : Fun[
                                  "v"  ∷  ty.bvec (2)
                                ](ty.bvec (1))
      | _update_flags_C    : Fun[
                                  "v"  ∷  ty.bvec (2);
                                  "x"  ∷  ty.bvec (1)
                                ](ty.bvec (2))
      | _get_flags_Z       : Fun[
                                  "v"  ∷  ty.bvec (2)
                                ](ty.bvec (1))
      | _update_flags_Z    : Fun[
                                  "v"  ∷  ty.bvec (2);
                                  "x"  ∷  ty.bvec (1)
                                ](ty.bvec (2))
      | step               : Fun[
                                  "ж_4"  ∷  ty.unit
                                ](ty.unit)
      | main               : Fun[
                                  "ж_7"  ∷  ty.unit
                                ](ty.unit).
    Inductive FunX : PCtx -> Ty -> Set :=
      | unconstrained (σ : Ty) {inhabitant : Unconstrained σ} : FunX ctx.nil σ.
    Definition 𝑭  : PCtx -> Ty -> Set := Fun.
    Definition 𝑭𝑿 : PCtx -> Ty -> Set := FunX.
    Definition 𝑳  : PCtx -> Set := fun _ => Empty_set.
  End FunDeclKit.
  
  Include FunDeclMixin UntitledBase.
End ModelProgramDecl.
