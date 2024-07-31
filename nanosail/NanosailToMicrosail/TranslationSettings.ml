open Base


let convert_constructor_name_to_tag (identifier : Ast.Identifier.t) : Ast.Identifier.t =
  Ast.Identifier.update (fun x -> "K" ^ String.lowercase x) identifier


let derive_record_constructor_from_identifier (identifier : Ast.Identifier.t) : Ast.Identifier.t =
  Ast.Identifier.add_prefix "Mk" identifier
