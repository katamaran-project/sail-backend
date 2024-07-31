let pp (identifier : Ast.Identifier.t) =
  PP.string @@ Ast.Identifier.string_of identifier


let derive_tag_from_enum_name (identifier : Ast.Identifier.t) : Ast.Identifier.t =
  TranslationSettings.convert_enum_name_to_tag' identifier
