From Katamaran Require Import Base.


Definition len_word := 16.

Inductive Enums : Set := .

Inductive Unions : Set := .

Inductive Records : Set := .


Module Export UntitledBase <: Base.

  (* Problem solved if len_word gets replaced by 16 *)
  Notation "'ty.wordBits'" := (ty.bvec len_word).
  
  #[export] Instance typedeclkit : TypeDeclKit :=
    {|
       enumi   := Enums;
       unioni  := Unions;
       recordi := Records;
    |}.

    Inductive Reg : Ty -> Set :=
      | verbosity            : Reg (ty.bvec 64)
      | old_PC_reg           : Reg (ty.wordBits)
    .
    
    Section TransparentObligations.
      Local Set Transparent Obligations.
      Derive Signature NoConfusion NoConfusionHom EqDec for Reg.
    End TransparentObligations.
 
