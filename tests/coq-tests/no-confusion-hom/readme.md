# NoConfusionHom

When translated with `(inline-definitions-in-notations #f)` we get

```coq's
(*
  Original Sail code
  
    type len_word : Int = 16
*)
Definition len_word :=
  16.

(*
  Original Sail code
  
    type wordBits = bits(len_word)
*)
Definition wordBits :=
  bv len_word.
```

With `(inline-definitions-in-notations #t)` we get

```coq's
(*
  Original Sail code
  
    type len_word : Int = 16
*)
Definition len_word :=
  16.

(*
  Original Sail code
  
    type wordBits = bits(len_word)
*)
Definition wordBits :=
  bv 16.                              # <----- 
```

This change is required to allow Coq to successfully derive NoConfusionHom for Reg.

```coq
  Section RegDeclKit.
    Inductive Reg : Ty -> Set :=
      | verbosity  : Reg (ty.bvec (8))
      | old_PC_reg : Reg (ty.wordBits).
    
    Section TransparentObligations.
      Local Set Transparent Obligations.
      Derive Signature NoConfusion NoConfusionHom EqDec for Reg.  # <----
    End TransparentObligations.
```
