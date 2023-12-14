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

let product_of ts =
  Ty_tuple ts


let funDefList = [
  { funName = "ex_prod";
    funType = {
        arg_types = [ ("tt", Ty_unit); ("tt", Ty_unit) ];
        ret_type = product_of [
                       product_of [
                           product_of [Ty_int; Ty_string];
                           product_of [Ty_int; Ty_string]
                         ];
                         product_of [Ty_int; Ty_string]
                     ]
    };
    funBody = fun_ex_prod;
  };
  { funName = "switch";
    funType = {
      arg_types = [ ("p", product_of [Ty_int; Ty_bool]) ];
      ret_type =  product_of [Ty_bool; Ty_int];
    };
    funBody = fun_switch;
  };
]

let add_dummy_sail =
  List.map (fun x -> (Util.dummy_sail_def, x))

let ir = make_ir_t ~function_definitions:(add_dummy_sail funDefList) "Prod"

