open Base

let generate (ignored_definitions : Ast.sail_definition list) =
  let ignored_definitions' = List.map ~f:Sail.pp_sail_definition ignored_definitions
  in
  PPrint.(Coq.comment @@ separate (twice hardline) ignored_definitions')
