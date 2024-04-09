open Base
(* open Auxlib *)
open Ast

module PP = PPrint



let pp_value (value : value) -> PP.document =
  match value with
  | Val_unit -> PP.(string "tt")
  | Val_bool b -> begin
      if b
      then PP.(string "true")
      else PP.(string "false")
    end
  | Val_int n -> PP.(string string_of_int n)
  | Val_string _ -> _
  | Val_prod (_, _) -> _


let pp_value_definition (value_definition : value_definition) -> PP.document =
  let { identifier; value } = value_definition
  in
  


let generate (definitions : (sail_definition * definition) list) =
  let value_definitions =
    select Extract.value_definition definitions
  in
  
