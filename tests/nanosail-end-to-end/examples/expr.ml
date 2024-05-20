open Nanosail.Ast

module Id = Nanosail.Id


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
  {
    function_name = Id.mk "bool_expr";
    function_type = {
      parameter_types = [ (Id.mk "tt", Ty_unit) ];
      ret_type        = Ty_bool
    };
    function_body = fun_bool_expr
  };
  {
    function_name = Id.mk "arith_expr";
    function_type = {
      parameter_types = [ (Id.mk "tt", Ty_unit) ];
      ret_type        = Ty_int
    };
    function_body = fun_arith_expr
  };
]

let program_name = "Expr"

let ir : program = {
    program_name = program_name;
    definitions  = List.map (fun x -> (Util.dummy_sail_def, FunctionDefinition x)) funDefList;
  }
