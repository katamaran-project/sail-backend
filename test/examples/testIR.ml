open Lib.IR

(******************************************************************************)
(* Functions bodies *)

let fun_test = Stm_val (Val_prod (Val_prod (
  Val_prod (Val_int 1, Val_string "one"),
  Val_prod (Val_int 2, Val_string "two")),
  Val_prod (Val_int 1, Val_string "one")
))


(******************************************************************************)
(* Intermediate Representation Lists *)


let testFunDefList = [
  { name = "test";
    funType = {
      arg_types = [ ("tt", Unit); ("tt", Unit) ];
      ret_type = Prod (Prod (
        Prod (Int, String),
        Prod (Int, String)),
        Prod (Int, String)
      )
    };
    funBody = fun_test
  }
]

let ir = { 
  program_name = "Test";
  funDefList = testFunDefList
}