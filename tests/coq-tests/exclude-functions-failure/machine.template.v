Require Export base.

(*<
  (generate (program-translation))
>*)

(* The excluded function is not in Fun *)
Fail Check translate32.
Check translateAddr.

(* The call to translate32 is a failure *)
Goal fun_translate_only = stm_fail (ty.bvec 34) "translate32 is excluded by the configuration".
Proof. reflexivity. Qed.
