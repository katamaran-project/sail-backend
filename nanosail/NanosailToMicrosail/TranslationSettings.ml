open Base


let convert_enum_name_to_tag (identifier : Ast.Identifier.t) : Ast.Identifier.t =
  Ast.Identifier.update (fun x -> "E" ^ String.lowercase x) identifier


let convert_record_name_to_tag (identifier : Ast.Identifier.t) : Ast.Identifier.t =
  Ast.Identifier.update (fun x -> "R" ^ String.lowercase x) identifier


let convert_variant_name_to_tag (identifier : Ast.Identifier.t) : Ast.Identifier.t =
  Ast.Identifier.update (fun x -> "U" ^ String.lowercase x) identifier
