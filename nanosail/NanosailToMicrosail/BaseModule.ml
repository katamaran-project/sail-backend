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


let pp_denote_function
      ~(denotations          : (PP.document * PP.document) list)
      ~(parameter_identifier : PP.document                     )
      ~(tag_type_identifier  : PP.document                     )
      ~(function_identifier  : PP.document                     ) : PP.document
  =
  let identifier  = function_identifier
  and parameters  = [ (parameter_identifier, Some tag_type_identifier) ]
  and result_type = Some (PP.string "Set")
  and body =
    let matched_expression = parameter_identifier
    and cases              = denotations
    in
    Coq.match' matched_expression cases
  in
  Coq.definition ~identifier ~parameters ~result_type body


let pp_enum_denote (definitions : (Sail.sail_definition * Ast.definition) list) : PP.document =
  let denotations =
    let enum_definitions =
      List.map ~f:snd Ast.(select Extract.(type_definition of_enum) definitions)
    in
    let enum_identifiers =
      List.map ~f:(fun enum_definition -> enum_definition.identifier) enum_definitions
    in
    let denotation_pair_for enum_identifier =
      (
        Identifier.pp_identifier @@ TranslationSettings.convert_enum_name_to_tag enum_identifier,
        Identifier.pp_identifier enum_identifier
      )
    in
    List.map ~f:denotation_pair_for enum_identifiers
  and parameter_identifier = PP.string "e"
  and tag_type_identifier  = PP.string "Enums"
  and function_identifier  = PP.string "enum_denote"
  in
  pp_denote_function ~denotations ~parameter_identifier ~tag_type_identifier ~function_identifier


let pp_variant_denote (definitions : (Sail.sail_definition * Ast.definition) list) : PP.document =
  let denotations =
    let variant_definitions =
      List.map ~f:snd Ast.(select Extract.(type_definition of_variant) definitions)
    in
    let variant_identifiers =
      List.map ~f:(fun variant_definition -> variant_definition.identifier) variant_definitions
    in
    let denotation_pair_for variant_identifier =
      (
        Identifier.pp_identifier @@ TranslationSettings.convert_variant_name_to_tag variant_identifier,
        Identifier.pp_identifier variant_identifier
      )
    in
    List.map ~f:denotation_pair_for variant_identifiers
  and parameter_identifier = PP.string "u"
  and tag_type_identifier  = PP.string "Unions"
  and function_identifier  = PP.string "union_denote"
  in
  pp_denote_function ~denotations ~parameter_identifier ~tag_type_identifier ~function_identifier


let pp_base_module (definitions : (Sail.sail_definition * Ast.definition) list) : PP.document
  =
  let base_module_name = "UntitledBase"
  and flag = Coq.Export
  and includes = [ "Base" ]
  and contents =
    let sections = [
        pp_typedeclkit ();
        pp_enum_denote definitions;
        pp_variant_denote definitions;
      ]
    in
    PP.(separate small_step sections)
  in
  Coq.module' ~flag ~includes base_module_name contents
