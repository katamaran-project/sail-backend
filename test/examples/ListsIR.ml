open Lib.IR

(******************************************************************************)
(* Functions bodies *)

let fun_is_empty = Stm_match_list {
  s        = Stm_exp (Exp_var "l");
  alt_nil  = Stm_val (Val_bool true);
  xh       = "h";
  xt       = "t";
  alt_cons = Stm_val (Val_bool false)
}

let fun_empty = Stm_exp (Exp_val (Val_list []))

(******************************************************************************)
(* Intermediate Representation Lists *)


let testFunDefList = [
  { name = "is_empty";
    funType = {
      arg_types = [ ("l", List Int) ];
      ret_type = Bool
    };
    funBody = fun_is_empty
  };
  { name = "empty";
    funType = {
      arg_types = [ ("tt", Unit)];
      ret_type = List Int
    };
    funBody = fun_empty
  };
]

let ir = { 
  program_name = "Lists";
  funDefList = testFunDefList
}