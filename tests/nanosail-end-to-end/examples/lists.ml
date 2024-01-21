open Nanosail.Ast


let fun_is_empty = Stm_match begin
    MP_list {
      matched   = Stm_exp (Exp_var "l");
      when_nil  = Stm_exp (Exp_val (Val_bool true));
      when_cons = ("h", "t", Stm_exp (Exp_val (Val_bool false)))
    }
  end

let fun_empty = Stm_exp (Exp_list [])

let fun_onetwothree = Stm_exp (Exp_list [Exp_val (Val_int (Big_int.of_int 1));
                                        Exp_val (Val_int (Big_int.of_int 2));
                                        Exp_val (Val_int (Big_int.of_int 3));])

let fun_last = Stm_match begin
    MP_list {
      matched = Stm_exp (Exp_var "l");
      when_nil = Stm_exp (Exp_val (Val_prod (Val_int (Big_int.of_int 0), Val_bool false)));
      when_cons = (
        "h",
        "t",
        Stm_match begin
          MP_list {
            matched = Stm_exp (Exp_var "t");
            when_nil = Stm_exp (Exp_binop (Pair, Exp_var "h", Exp_val (Val_bool true)));
            when_cons = ("h'", "t'", Stm_call ("last", [Exp_var "t"]))
          }
        end
      )
    }
  end

let fun_append = Stm_match begin
    MP_list {
      matched = Stm_exp (Exp_var "l1");
      when_nil = Stm_exp (Exp_var "l2");
      when_cons = ("h", "t", Stm_let ("r", Stm_call ("append", [Exp_var "t"; Exp_var "l2"]), Stm_exp (Exp_binop (Cons, Exp_var "h", Exp_var "r"))))
    }
  end

let fun_length = Stm_match begin
    MP_list {
      matched = Stm_exp (Exp_var "l");
      when_nil = Stm_exp (Exp_val (Val_int (Big_int.of_int 0)));
      when_cons = ("h", "t", Stm_let ("n", Stm_call ("length", [Exp_var "t"]), Stm_exp (Exp_binop (Plus, Exp_var "n", Exp_val (Val_int (Big_int.of_int 1))))))
    }
  end

let fun_reverse_aux = Stm_match begin
    MP_list {
      matched = Stm_exp (Exp_var "l");
      when_nil = Stm_exp (Exp_var "acc");
      when_cons = ("h", "t", Stm_call ("reverse_aux", [Exp_var "t"; Exp_binop (Cons, Exp_var "h", Exp_var "acc")]))
    }
  end

let fun_reverse = Stm_call ("reverse_aux", [Exp_var "l"; Exp_list []])

let fun_reverse_bis = Stm_match begin
    MP_list {
      matched = Stm_exp (Exp_var "l");
      when_nil = Stm_exp (Exp_list []);
      when_cons = ("h", "t", Stm_let ("r", Stm_call ("reverse_bis", [Exp_var "t"]), Stm_exp (Exp_binop (Append, Exp_var "r", Exp_list [Exp_var "h"]))))
    }
  end


(******************************************************************************)
(* Intermediate Representation Lists *)


let list_of t =
  Ty_list t

let product_of ts =
  Ty_tuple ts

let funDefList = [
  { function_name = "is_empty";
    function_type = {
      arg_types = [("l", list_of Ty_int)];
      ret_type = Ty_bool
    };
    function_body = fun_is_empty
  };
  { function_name = "empty";
    function_type = {
      arg_types = [("tt", Ty_unit)];
      ret_type = list_of Ty_int
    };
    function_body = fun_empty
  };
  { function_name = "onetwothree";
    function_type = {
      arg_types = [("tt", Ty_unit)];
      ret_type = list_of Ty_int
    };
    function_body = fun_onetwothree
  };
  { function_name = "last";
    function_type = {
      arg_types = [("l", list_of Ty_int)];
      ret_type = product_of [Ty_int; Ty_bool]
    };
    function_body = fun_last
  };
  { function_name = "append";
    function_type = {
      arg_types = [("l1", list_of Ty_int);
                   ("l2", list_of Ty_int)];
      ret_type = list_of Ty_int
    };
    function_body = fun_append
  };
  { function_name = "length";
    function_type = {
      arg_types = [("l", list_of Ty_int)];
      ret_type = Ty_int
    };
    function_body = fun_length
  };
  { function_name = "reverse_aux";
    function_type = {
      arg_types = [("l", list_of Ty_int);
                   ("acc", list_of Ty_int)];
      ret_type = list_of Ty_int
    };
    function_body = fun_reverse_aux
  };
  { function_name = "reverse";
    function_type = {
      arg_types = [("l", list_of Ty_int)];
      ret_type = list_of Ty_int
    };
    function_body = fun_reverse
  };
  { function_name = "reverse_bis";
    function_type = {
      arg_types = [("l", list_of Ty_int)];
      ret_type = list_of Ty_int
    };
    function_body = fun_reverse_bis
  };
]

let program_name = "Lists"

let ir : program = {
    program_name = program_name;
    definitions  = List.map (fun x -> (Util.dummy_sail_def, FunctionDefinition x)) funDefList;
  }
