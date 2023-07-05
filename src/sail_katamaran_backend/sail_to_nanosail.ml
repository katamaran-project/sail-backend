open Option
open Libsail.Ast_defs
open Libsail.Ast
open Nanosail.Ast

let empty_ir = {
  program_name = "Empty";
  funDefList = []
}

let string_of_id (Id_aux (aux, _)) =
  match aux with
  | Id x -> x
  | _    -> "###"

let ir_funcl (FCL_aux (FCL_funcl (id, _(*pexp*)), _)) = {
  name    = string_of_id(id);
  funType = {
    arg_types = [];
    ret_type  = Unit
  };
  funBody = Stm_val (Val_unit)
}

let ir_fundef (FD_aux ((FD_function (_, _, funcls)), _)) =
  match funcls with
  | [funcl] -> some (ir_funcl funcl)
  | _       -> none


let ir_def (DEF_aux (aux, _)) =
  match aux with
  | DEF_fundef fd -> join (some (ir_fundef fd))
  | _             -> none 

let ast_to_ir {defs; _} =  {
  program_name = "#####";
  funDefList   = List.filter_map ir_def defs
}