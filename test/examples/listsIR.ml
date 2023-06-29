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

let fun_empty = Stm_exp (Exp_list [])

let fun_onetwothree = Stm_exp (Exp_list [Exp_val (Val_int 1);
                                        Exp_val (Val_int 2);
                                        Exp_val (Val_int 3);])

(******************************************************************************)
(* Intermediate Representation Lists *)


let listsFunDefList = [
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
  { name = "onetwothree";
    funType = {
      arg_types = [ ("tt", Unit)];
      ret_type = List Int
    };
    funBody = fun_onetwothree
  };
]

let ir = { 
  program_name = "Lists";
  funDefList = listsFunDefList
}