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
      arg_types = [("l", Ty_app (List, [TA_type (Ty_id Int)]));
                   ("b", Ty_id Bool);
                   ("n", Ty_id Int)];
      ret_type = Ty_app (List,
                         [TA_type (Ty_app (List,
                                           [TA_type (Ty_app (List,
                                                             [TA_type (Ty_id Bool)]))]))])
                   };
    funBody = fun_very_long_naaaaaaaaaaaaaaaaame
  }
]

let add_dummy_sail =
  List.map (fun x -> (Util.dummy_sail_def, x))

let ir = make_ir_t ~function_definitions:(add_dummy_sail funDefList) "Long"

