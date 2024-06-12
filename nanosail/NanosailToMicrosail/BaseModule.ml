open Base


let pp_typedeclkit () =
  PP.(separate hardline [
          string "#[export] Instance typedeclkit : TypeDeclKit :=";
          string "  {| enumi   := Enums;";
          string "     unioni  := Unions;";
          string "     recordi := Records;";
          string "  |}.";
        ]
  )


let pp_enum_denote (definitions : (Sail.sail_definition * Ast.definition) list) : PP.document =
  let enum_definitions =
    List.map ~f:snd Ast.(select Extract.(type_definition of_enum) definitions)
  in
  let parameter_identifier = "e"
  in
  let identifier  = PP.(string "enum_denote")
  and parameters  = [ (PP.string parameter_identifier, Some (PP.string "Enums")) ]
  and result_type = Some (PP.string "Set")
  and body =
    let matched_expression = PP.string parameter_identifier
    and cases =
      let process_case identifier =
        (
          PP.string @@ Ast.Identifier.string_of @@ TranslationSettings.convert_enum_name_to_tag identifier,
          PP.string @@ Ast.Identifier.string_of identifier
        )
      in
      List.map ~f:process_case @@ List.map ~f:(fun enum_definition -> enum_definition.identifier) enum_definitions
    in
    Coq.match' matched_expression cases
  in
  Coq.definition ~identifier ~parameters ~result_type body


let pp_base_module (definitions : (Sail.sail_definition * Ast.definition) list) : PP.document
  =
  let base_module_name = "UntitledBase"
  and flag = Coq.Export
  and includes = [ "Base" ]
  and contents =
    let sections = [
        pp_typedeclkit ();
        pp_enum_denote definitions;
      ]
    in
    PP.(separate small_step sections)
  in
  Coq.module' ~flag ~includes base_module_name contents
