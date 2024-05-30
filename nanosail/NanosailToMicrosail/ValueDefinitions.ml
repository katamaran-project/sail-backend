open Base
(* open Auxlib *)
open Ast
open Identifier


let pp_value (value : value) : PP.document =
  match value with
  | Val_unit        -> PP.(string "tt")
  | Val_int n       -> PP.(string @@ Z.to_string n)
  | Val_string _    -> PP.string "not yet implemented" (* todo *)
  | Val_prod (_, _) -> PP.string "not yet implemented" (* todo *)
  | Val_bool b      -> PP.string @@ if b then "true" else "false"


let pp_value_definition (value_definition : value_definition) : PP.document =
  let { identifier; value } = value_definition
  in
  let definition =
    let identifier = pp_identifier identifier
    and parameters = []
    and result_type = None
    and body = pp_value value
    in
    Coq.definition ~identifier ~parameters ~result_type ~body
  in
  definition


let generate (definitions : (Sail.sail_definition * definition) list) =
  let value_definitions =
    List.map ~f:snd @@ select Extract.value_definition definitions
  in
  let coq_lines = List.map ~f:pp_value_definition value_definitions
  in
  PP.(separate hardline coq_lines)
