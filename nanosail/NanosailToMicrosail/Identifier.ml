let pp (identifier : Ast.Identifier.t) =
  PP.string @@ Ast.Identifier.string_of identifier


let reified_enum_name (identifier : Ast.Identifier.t) : Ast.Identifier.t =
  TranslationSettings.convert_enum_name_to_tag' identifier
