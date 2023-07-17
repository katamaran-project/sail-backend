open Nanosail.Ast

(******************************************************************************)
(* Functions bodies *)

let fun_ex_prod = Stm_exp (Exp_val (Val_prod (Val_prod (
  Val_prod (Val_int (Big_int.of_int 1), Val_string "one"),
  Val_prod (Val_int (Big_int.of_int 2), Val_string "two")),
  Val_prod (Val_int (Big_int.of_int 1), Val_string "one")
)))

let fun_switch = Stm_match_prod {
  s = Stm_exp (Exp_var "p");
  xl = "l";
  xr = "r";
  rhs = Stm_exp (Exp_binop (Pair, Exp_var "r", Exp_var "l"));
}


(******************************************************************************)
(* Intermediate Representation Lists *)


let funDefList = [
  { funName = "ex_prod";
    funType = {
      arg_types = [ ("tt", Ty_id Unit); ("tt", Ty_id Unit) ];
      ret_type = Ty_app (Prod, [Ty_app (Prod, [
        Ty_app (Prod, [Ty_id Int; Ty_id String]);
        Ty_app (Prod, [Ty_id Int; Ty_id String])]);
        Ty_app (Prod, [Ty_id Int; Ty_id String])
        ])
    };
    funBody = fun_ex_prod;
  };
  { funName = "switch";
    funType = {
      arg_types = [ ("p", Ty_app (Prod, [Ty_id Int; Ty_id Bool])) ];
      ret_type =  Ty_app (Prod, [Ty_id Bool; Ty_id Int]);
    };
    funBody = fun_switch;
  };
]

let ir = { 
  program_name = "Prod";
  funDefList = funDefList
}