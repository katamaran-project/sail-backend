open Nanosail.Ast

(******************************************************************************)
(* Functions bodies *)

let fun_very_long_naaaaaaaaaaaaaaaaame = Stm_exp (
  Exp_list [
    Exp_list [
      Exp_list [Exp_val (Val_bool true)];
      Exp_list [Exp_val (Val_bool false)];
      Exp_list [Exp_val (Val_bool false)]
    ];
    Exp_list [
      Exp_list [Exp_val (Val_bool false)];
      Exp_list [Exp_val (Val_bool true)];
      Exp_list [Exp_val (Val_bool false)]
    ];
    Exp_list [
      Exp_list [Exp_val (Val_bool false)];
      Exp_list [Exp_val (Val_bool false)];
      Exp_list [Exp_val (Val_bool true)]
    ]
  ]
)

(******************************************************************************)
(* Intermediate Representation Lists *)


let funDefList = [
  { funName = "very_long_naaaaaaaaaaaaaaaaame";
    funType = {
      arg_types = [("l", Ty_app (List, [Ty_id Int]));
                   ("b", Ty_id Bool);
                   ("n", Ty_id Int)];
      ret_type = Ty_app (List, [Ty_app (List, [Ty_app (List, [Ty_id Bool])])])
    };
    funBody = fun_very_long_naaaaaaaaaaaaaaaaame
  }
]

let ir = { 
  program_name = "Long";
  funDefList = funDefList
}