open Lib.IR

(******************************************************************************)
(* Functions bodies *)

let fun_prod = Stm_val (Val_prod (Val_prod (
  Val_prod (Val_int 1, Val_string "one"),
  Val_prod (Val_int 2, Val_string "two")),
  Val_prod (Val_int 1, Val_string "one")
))


(******************************************************************************)
(* Intermediate Representation Lists *)


let prodFunDefList = [
  { name = "ex_prod";
    funType = {
      arg_types = [ ("tt", Unit); ("tt", Unit) ];
      ret_type = Prod (Prod (
        Prod (Int, String),
        Prod (Int, String)),
        Prod (Int, String)
      )
    };
    funBody = fun_prod
  }
]

let ir = { 
  program_name = "Prod";
  funDefList = prodFunDefList
}