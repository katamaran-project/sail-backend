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

let fun_last = Stm_match_list {
  s        = Stm_exp (Exp_var "l");
  alt_nil  = Stm_val (Val_prod (Val_int 0, Val_bool false));
  xh       = "h";
  xt       = "t";
  alt_cons = Stm_match_list {
    s        = Stm_exp (Exp_var "t");
    alt_nil  = Stm_exp (Exp_binop (Pair, Exp_var "h", Exp_val (Val_bool true)));
    xh       = "h'";
    xt       = "t'";
    alt_cons = Stm_call ("last", [Exp_var "t"])
  }
}

let fun_append = Stm_match_list {
  s        = Stm_exp (Exp_var "l1");
  alt_nil  = Stm_exp (Exp_var "l2");
  xh       = "h";
  xt       = "t";
  alt_cons = Stm_let ("r", Stm_call ("append", [Exp_var "t"; Exp_var "l2"]),
    Stm_exp (Exp_binop (Cons, Exp_var "h", Exp_var "r")))
}

let fun_length = Stm_match_list {
  s        = Stm_exp (Exp_var "l");
  alt_nil  = Stm_val (Val_int 0);
  xh       = "h";
  xt       = "t";
  alt_cons = Stm_let ("n", Stm_call ("length", [Exp_var "t"]), Stm_exp
    (Exp_binop (Plus, Exp_var "n", Exp_val (Val_int 1))))
}

let fun_reverse_aux = Stm_match_list {
  s        = Stm_exp (Exp_var "l");
  alt_nil  = Stm_exp (Exp_var "acc");
  xh       = "h";
  xt       = "t";
  alt_cons = Stm_call ("reverse_aux", [Exp_var "t"; Exp_binop (Cons,
    Exp_var "h", Exp_var "acc")])
}

let fun_reverse = Stm_call ("reverse_aux", [Exp_var "l"; Exp_list []])

let fun_reverse_bis = Stm_match_list {
  s        = Stm_exp (Exp_var "l");
  alt_nil  = Stm_exp (Exp_list []);
  xh       = "h";
  xt       = "t";
  alt_cons = Stm_let ("r", Stm_call ("reverse_bis", [Exp_var "t"]), Stm_exp
    (Exp_binop (Append, Exp_var "r", Exp_list [Exp_var "h"])))
}


(******************************************************************************)
(* Intermediate Representation Lists *)


let listsFunDefList = [
  { name = "is_empty";
    funType = {
      arg_types = [("l", List Int)];
      ret_type = Bool
    };
    funBody = fun_is_empty
  };
  { name = "empty";
    funType = {
      arg_types = [("tt", Unit)];
      ret_type = List Int
    };
    funBody = fun_empty
  };
  { name = "onetwothree";
    funType = {
      arg_types = [("tt", Unit)];
      ret_type = List Int
    };
    funBody = fun_onetwothree
  };
  { name = "last";
    funType = {
      arg_types = [("l", List Int)];
      ret_type = Prod (Int, Bool)
    };
    funBody = fun_last
  };
  { name = "append";
    funType = {
      arg_types = [("l1", List Int); ("l2", List Int)];
      ret_type = List Int
    };
    funBody = fun_append
  };
  { name = "length";
    funType = {
      arg_types = [("l", List Int)];
      ret_type = Int
    };
    funBody = fun_length
  };
  { name = "reverse_aux";
    funType = {
      arg_types = [("l", List Int); ("acc", List Int)];
      ret_type = List Int
    };
    funBody = fun_reverse_aux
  };
  { name = "reverse";
    funType = {
      arg_types = [("l", List Int);];
      ret_type = List Int
    };
    funBody = fun_reverse
  };
  { name = "reverse_bis";
    funType = {
      arg_types = [("l", List Int)];
      ret_type = List Int
    };
    funBody = fun_reverse_bis
  };
]

let ir = { 
  program_name = "Lists";
  funDefList = listsFunDefList
}