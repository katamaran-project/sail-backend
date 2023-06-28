open Lib.IR
open TestIR
open ListsIR
open LongIR


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
  program_name = "All";
  funDefList = testFunDefList @
               listsFunDefList @
               longFunDefList
}
