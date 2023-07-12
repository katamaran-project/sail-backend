open Nanosail.Ast

(******************************************************************************)
(* Functions bodies *)

let fun_prod = Stm_val (Val_prod (Val_prod (
  Val_prod (Val_int (Big_int.of_int 1), Val_string "one"),
  Val_prod (Val_int (Big_int.of_int 2), Val_string "two")),
  Val_prod (Val_int (Big_int.of_int 1), Val_string "one")
))


(******************************************************************************)
(* Intermediate Representation Lists *)


let funDefList = [
  { name = "ex_prod";
    funType = {
      arg_types = [ ("tt", Ty_id Unit); ("tt", Ty_id Unit) ];
      ret_type = Ty_app (Prod, [Ty_app (Prod, [
        Ty_app (Prod, [Ty_id Int; Ty_id String]);
        Ty_app (Prod, [Ty_id Int; Ty_id String])]);
        Ty_app (Prod, [Ty_id Int; Ty_id String])
        ])
    };
    funBody = fun_prod
  }
]

let ir = { 
  program_name = "Prod";
  funDefList = funDefList
}