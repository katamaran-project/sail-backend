let translate_definition (definition : Sail.type_annotation Libsail.Ast.def) =
  match definition with
  | DEF_overload _ -> TC.not_yet_implemented [%here] location
  | DEF_pragma _ -> TC.not_yet_implemented [%here] location
