open Lib.IR

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


let longFunDefList = [
  { name = "very_long_naaaaaaaaaaaaaaaaame";
    funType = {
      arg_types = [ ("l", List Int); ("b", Bool); ("n", Int) ];
      ret_type = List (List (List Bool))
    };
    funBody = fun_very_long_naaaaaaaaaaaaaaaaame
  }
]

let ir = { 
  program_name = "Long";
  funDefList = longFunDefList
}