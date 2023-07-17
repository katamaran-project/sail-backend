open Nanosail.Ast

(******************************************************************************)
(* Functions bodies *)

let fun_bool_expr = Stm_exp (Exp_binop (Or,
  Exp_not (Exp_binop (And,
    Exp_binop (Eq,
      Exp_val (Val_int (Big_int.of_int 0)),
      Exp_val (Val_int (Big_int.of_int 1))),
    Exp_binop (Neq,
      Exp_val (Val_int (Big_int.of_int 2)),
      Exp_val (Val_int (Big_int.of_int 3))))),
  Exp_binop (And,
    Exp_binop (Or,
      Exp_binop (Le,
        Exp_val (Val_int (Big_int.of_int 4)),
        Exp_val (Val_int (Big_int.of_int 5))),
      Exp_binop (Lt,
        Exp_val (Val_int (Big_int.of_int 6)),
        Exp_val (Val_int (Big_int.of_int 7)))),
    Exp_binop (Or,
      Exp_binop (Ge,
        Exp_val (Val_int (Big_int.of_int 8)),
        Exp_val (Val_int (Big_int.of_int 9))),
      Exp_binop (Lt,
        Exp_val (Val_int (Big_int.of_int 10)),
        Exp_val (Val_int (Big_int.of_int 11)))))
))

let fun_arith_expr = Stm_exp (Exp_binop (Plus,
  Exp_binop (Times,
    Exp_binop (Minus,
      Exp_neg (Exp_val (Val_int (Big_int.of_int 1))),
      Exp_neg (Exp_val (Val_int (Big_int.of_int 2)))),
    Exp_val (Val_int (Big_int.of_int 3))),
  Exp_val (Val_int (Big_int.of_int 4))
))

(******************************************************************************)
(* Intermediate Representation Lists *)


let funDefList = [
  { funName = "bool_expr";
    funType = {
      arg_types = [ ("tt", Ty_id Unit) ];
      ret_type = Ty_id Bool
    };
    funBody = fun_bool_expr
  };
  { funName = "arith_expr";
    funType = {
      arg_types = [ ("tt", Ty_id Unit) ];
      ret_type = Ty_id Int
    };
    funBody = fun_arith_expr
  };
]

let ir = { 
  program_name = "Expr";
  funDefList = funDefList
}