let pp (identifier : Ast.Identifier.t) =
  PP.string @@ Ast.Identifier.string_of identifier


let reified_enum_name (identifier : Ast.Identifier.t) : Ast.Identifier.t =
  Configuration.reified_enum_name identifier
