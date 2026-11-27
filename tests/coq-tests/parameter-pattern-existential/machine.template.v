Require Export base.

(*<
  program-translation
>*)

Check (fun_unwrap : Stm [ "arg#0" ∷ ty.union Uwrapped ] ty.int).
