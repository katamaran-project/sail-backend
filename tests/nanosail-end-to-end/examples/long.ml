open Nanosail.Ast

module Id = Nanosail.Id

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
  { function_name = Id.mk "very_long_naaaaaaaaaaaaaaaaame";
    function_type = {
      parameters  = [(Id.mk "l", Ty_list Ty_int);
                     (Id.mk "b", Ty_bool);
                     (Id.mk "n", Ty_int)];
      return_type = Ty_list (Ty_list (Ty_list Ty_bool))
    };
    function_body = fun_very_long_naaaaaaaaaaaaaaaaame
  }
]

let program_name = "Long"

let ir : program = {
    program_name = program_name;
    definitions  = List.map (fun x -> (Util.dummy_sail_def, FunctionDefinition x)) funDefList;
  }
