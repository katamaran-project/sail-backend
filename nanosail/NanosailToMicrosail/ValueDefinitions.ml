open Base


let pp_value (value : Ast.Value.t) : PP.document =
  match value with
  | Unit        -> PP.(string "tt")
  | Int n       -> PP.(string @@ Z.to_string n)
  | String _    -> PP.string "not yet implemented" (* todo *)
  | Prod (_, _) -> PP.string "not yet implemented" (* todo *)
  | Bool b      -> PP.string @@ if b then "true" else "false"


let pp_value_definition (value_definition : Ast.Definition.value_definition) : PP.document =
  let { identifier; value } : Ast.Definition.value_definition = value_definition
  in
  let definition =
    let identifier = Identifier.pp identifier
    and result_type = None
    and body = pp_value value
    in
    Coq.definition ~identifier ~result_type body
  in
  definition


let generate (definitions : (Sail.sail_definition * Ast.Definition.t) list) =
  let value_definitions =
    List.map ~f:snd @@ Ast.(select Extract.value_definition definitions)
  in
  let coq_lines = List.map ~f:pp_value_definition value_definitions
  in
  PP.(separate hardline coq_lines)
