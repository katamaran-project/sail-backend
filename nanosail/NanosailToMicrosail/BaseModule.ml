open Base

module AC = AnnotationContext


let pp_typedeclkit () =
  let coq_lines = [
      "#[export] Instance typedeclkit : TypeDeclKit :=";
      "  {| enumi   := Enums;";
      "     unioni  := Unions;";
      "     recordi := Records;";
      "  |}.";
    ]
  in
  PP.(separate_map hardline string coq_lines)


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


let pp_enum_denote (enum_definitions : Ast.Definition.Type.Enum.t list) : PP.document =
  let denotations =
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


let pp_union_denote (variant_definitions : Ast.Definition.Type.Variant.t list) : PP.document =
  let denotations =
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


let pp_record_denote (record_definitions : Ast.Definition.Type.Record.t list) : PP.document =
  let denotations =
    let record_identifiers =
      List.map ~f:(fun record_definition -> record_definition.identifier) record_definitions
    in
    let denotation_pair_for record_identifier =
      (
        Identifier.pp_identifier @@ TranslationSettings.convert_record_name_to_tag record_identifier,
        Identifier.pp_identifier record_identifier
      )
    in
    List.map ~f:denotation_pair_for record_identifiers
  and parameter_identifier = PP.string "r"
  and tag_type_identifier  = PP.string "Records"
  and function_identifier  = PP.string "record_denote"
  in
  pp_denote_function ~denotations ~parameter_identifier ~tag_type_identifier ~function_identifier


let pp_typedenotekit () =
  let coq_lines = [
      "#[export] Instance typedenotekit : TypeDenoteKit typedeclkit :=";
      "  {|";
      "     enumt := enum_denote;";
      "     uniont := union_denote;";
      "     recordt := record_denote;";
      "  |}.";
    ]
  in
  PP.(separate_map hardline string coq_lines)


let pp_union_constructor (variant_definitions : Ast.Definition.Type.Variant.t list) : PP.document =
  let denotations =
    let variant_identifiers =
      List.map ~f:(fun variant_definition -> variant_definition.identifier) variant_definitions
    in
    let denotation_pair_for variant_identifier =
      (
        Identifier.pp_identifier @@ TranslationSettings.convert_variant_name_to_tag variant_identifier,
        Identifier.pp_identifier @@ TranslationSettings.derive_variant_constructor_type variant_identifier
      )
    in
    List.map ~f:denotation_pair_for variant_identifiers
  and parameter_identifier = PP.string "u"
  and tag_type_identifier  = PP.string "Unions"
  and function_identifier  = PP.string "union_constructor"
  in
  pp_denote_function ~denotations ~parameter_identifier ~tag_type_identifier ~function_identifier


let pp_union_constructor_type (variant_definitions : Ast.Definition.Type.Variant.t list) : PP.document =
  let identifier  = PP.string "union_constructor_type"
  and parameters  = [ (PP.string "u", Some (PP.string "Unions")) ]
  and result_type = Some (PP.string "union_constructor u -> Ty")
  and body =
    let matched_expression = PP.string "u"
    and cases =
      let pp_variant_case (variant_definition : Ast.Definition.Type.Variant.t) =
        let match_constructor_cases =
          let constructor_cases =
            let pp_constructor_case (constructor : Ast.Definition.Type.Variant.constructor) =
              let (constructor_identifier, constructor_field_types) = constructor
              in
              let pp_constructor_tag =
                Identifier.pp_identifier @@ TranslationSettings.convert_constructor_name_to_tag constructor_identifier
              and pp_constructor_field_types =
                let packed_type =
                  match constructor_field_types with
                  | []     -> Ast.Type.Unit
                  | [x]    -> x
                  | [x; y] -> Ast.Type.Product (x, y)
                  | xs     -> Ast.Type.Tuple xs
                in
                AnnotationContext.drop_annotations @@ Nanotype.pp_nanotype packed_type
              in
              (
                pp_constructor_tag,
                pp_constructor_field_types
              )
            in
            List.map ~f:pp_constructor_case variant_definition.constructors
          in
          Coq.match' (PP.string "k") constructor_cases
        in
        (
          Identifier.pp_identifier @@ TranslationSettings.convert_variant_name_to_tag variant_definition.identifier,
          PP.(string "fun k => " ^^ align match_constructor_cases)
        )
      in
      List.map ~f:pp_variant_case variant_definitions
    in
    Coq.match' matched_expression cases
  in
  Coq.definition ~identifier ~parameters ~result_type body


let pp_eqdec_and_finite_instances () =
  let coq_lines = [
      "#[export] Instance eqdec_enum_denote E : EqDec (enum_denote E) :=";
      "  ltac:(destruct E; auto with typeclass_instances).";
      "#[export] Instance finite_enum_denote E : finite.Finite (enum_denote E) :=";
      "  ltac:(destruct E; auto with typeclass_instances).";
      "#[export] Instance eqdec_union_denote U : EqDec (union_denote U) :=";
      "  ltac:(destruct U; cbn; auto with typeclass_instances).";
      "#[export] Instance eqdec_union_constructor U : EqDec (union_constructor U) :=";
      "  ltac:(destruct U; cbn; auto with typeclass_instances).";
      "#[export] Instance finite_union_constructor U : finite.Finite (union_constructor U) :=";
      "  ltac:(destruct U; cbn; auto with typeclass_instances).";
      "#[export] Instance eqdec_record_denote R : EqDec (record_denote R) :=";
      "  ltac:(destruct R; auto with typeclass_instances).";
    ]
  in
  PP.(separate_map hardline string coq_lines)


(* Helper function for pp_union_fold and pp_union_unfold *)
let pp_match_variant_constructors
    ~(matched_identifier  : Ast.Identifier.t                  )
    ~(variant_definitions : Ast.Definition.Type.Variant.t list)
    ~(constructor_case_handler : Ast.Identifier.t * Ast.Type.t list -> PP.document * PP.document) : PP.document
  =
  let variant_case_handler (variant_definition : Ast.Definition.Type.Variant.t) : PP.document * PP.document =
    let parameter_identifier = Ast.Identifier.mk "Kv"
    in
    let pattern =
      Identifier.pp_identifier @@ TranslationSettings.convert_variant_name_to_tag variant_definition.identifier
    and expression =
      let lambda_body =
        Types.Variants.generate_constructor_match
          ~matched_identifier:parameter_identifier
          ~variant_definition
          ~constructor_case_handler
      in
      Coq.lambda (Identifier.pp_identifier parameter_identifier) lambda_body
    in
    (
      pattern,
      expression
    )
  in
  Types.Variants.generate_tag_match ~matched_identifier ~variant_definitions ~variant_case_handler
  

let pp_union_fold (variant_definitions : Ast.Definition.Type.Variant.t list) : PP.document =
  let identifier = PP.string "union_fold"
  and parameters = [ (PP.string "U", Some (PP.string "unioni")) ]
  and result_type = Some (PP.string "{ K & Val (union_constructor_type U K) } -> uniont U")
  and contents =
    let matched_identifier = Ast.Identifier.mk "U"
    in
    let constructor_case_handler (variant_constructor : Ast.Definition.Type.Variant.constructor) : PP.document * PP.document =
      let (constructor_identifier, constructor_field_types) = variant_constructor
      in
      let field_variables =
        let generate_identifier index =
          PP.string @@ Printf.sprintf "x%d" index
        and indices =
          let n_fields = List.length constructor_field_types
          in
          List.range ~start:`inclusive ~stop:`inclusive 1 n_fields
        in
        List.map ~f:generate_identifier indices
      in
      let pattern =
        let fields =
          let tt = PP.string "tt"
          in
          match field_variables with
          | []     -> tt
          | [t]    -> t
          | [_; _] -> PP.(parens @@ separate (comma ^^ space) field_variables)
          | _      -> PP.(parens @@ separate (comma ^^ space) (tt :: field_variables))
        in
        let parts = [
          PP.string "existT";
          Identifier.pp_identifier @@ TranslationSettings.convert_constructor_name_to_tag constructor_identifier;
          fields
        ]
        in
        PP.(separate space parts)
      and expression =
        PP.(separate space @@ Identifier.pp_identifier constructor_identifier :: field_variables)
      in
      (pattern, expression)
    in
    pp_match_variant_constructors ~variant_definitions ~matched_identifier ~constructor_case_handler
  in  
  Coq.definition ~identifier ~parameters ~result_type contents


let pp_union_unfold (variant_definitions : Ast.Definition.Type.Variant.t list) : PP.document =
  let identifier = PP.string "union_unfold"
  and parameters = [ (PP.string "U", Some (PP.string "unioni")) ]
  and result_type = Some (PP.string "uniont U -> { K & Val (union_constructor_type U K) }")
  and contents =
    let matched_identifier = Ast.Identifier.mk "U"
    in
    let constructor_case_handler (constructor_identifier, field_types) =
      let field_names =
        let generate_identifier index =
          PP.string @@ Printf.sprintf "x%d" index
        and indices =
          let n_fields = List.length field_types
          in
          List.range ~start:`inclusive ~stop:`inclusive 1 n_fields
        in
        List.map ~f:generate_identifier indices
      in
      let pattern = PP.separate PP.space @@ Auxlib.build_list @@ fun { add; addall; _ } -> begin
          add    @@ Identifier.pp_identifier constructor_identifier;
          addall @@ field_names
        end
      and expression =
        let tuple =
          let tt = PP.string "tt"
          in
          match field_names with
          | []     -> tt
          | [t]    -> t
          | [_; _] -> PP.parens @@ PP.separate (PP.string ", ") field_names
          | _      -> PP.parens @@ PP.separate (PP.string ", ") (tt :: field_names)
        in
        PP.(separate space [
            string "existT";
            Identifier.pp_identifier @@ TranslationSettings.convert_constructor_name_to_tag constructor_identifier;
            tuple
          ])
      in
      (
        pattern,
        expression
      )
    in
    pp_match_variant_constructors ~variant_definitions ~matched_identifier ~constructor_case_handler
  in
  Coq.definition ~identifier ~parameters ~result_type contents


let pp_record_field_type (record_definitions : Ast.Definition.Type.Record.t list) : PP.document =
  let matched_identifier = Ast.Identifier.mk "R"
  in
  let identifier = PP.string "record_field_type"
  and parameters = [ (Identifier.pp_identifier matched_identifier, Some (PP.string "recordi")) ]
  and result_type = Some (PP.string "NCtx string Ty")
  and contents =
    let record_case_handler (record_definition : Ast.Definition.Type.Record.t) : PP.document * PP.document =
      let pattern =
        Identifier.pp_identifier @@ TranslationSettings.convert_record_name_to_tag record_definition.identifier
      and expression =
        let pp_field (field_identifier, field_type) =
          let id = Identifier.pp_identifier field_identifier
          and t  = AnnotationContext.drop_annotations @@ Nanotype.pp_nanotype field_type
          in
          PP.(separate space [ id; string "::"; t ])
        in
        Coq.list @@ List.map ~f:pp_field record_definition.fields
      in
      (pattern, expression)
    in
    Types.Records.generate_tag_match ~matched_identifier ~record_definitions ~record_case_handler
  in
  Coq.definition ~identifier ~parameters ~result_type contents


let pp_record_fold (record_definitions : Ast.Definition.Type.Record.t list) : PP.document =
  let matched_identifier = Ast.Identifier.mk "R"
  in
  let identifier = PP.string "record_fold"
  and parameters = [ (Identifier.pp_identifier matched_identifier, Some (PP.string "recordi")) ]
  and result_type = Some (PP.(separate space [ string "recordt"; Identifier.pp_identifier matched_identifier ]))
  and contents =
    let record_case_handler (record_definition : Ast.Definition.Type.Record.t) : PP.document * PP.document =
      let pattern =
        Identifier.pp_identifier @@ TranslationSettings.convert_record_name_to_tag record_definition.identifier
      and expression =
        let lambda_parameter = Ast.Identifier.mk "fields"
        in
        let lambda_body =
          let arguments =
            let f (field_identifier, _field_type) =
              PP.string @@ Printf.sprintf "%s.[??\"%s\"]"
                (Ast.Identifier.string_of lambda_parameter)
                (Ast.Identifier.string_of field_identifier)
            in
            List.map ~f record_definition.fields
          in
          Coq.application (PP.string "MkCap") arguments
        in
        Coq.lambda (Identifier.pp_identifier lambda_parameter) lambda_body
      in
      (pattern, expression)
    in
    Types.Records.generate_tag_match ~matched_identifier ~record_definitions ~record_case_handler
  in
  Coq.definition ~identifier ~parameters ~result_type contents


let pp_record_unfold (record_definitions : Ast.Definition.Type.Record.t list) : PP.document =
  let matched_identifier = Ast.Identifier.mk "R"
  in
  let identifier = PP.string "record_unfold"
  and parameters = [ (Identifier.pp_identifier matched_identifier, Some (PP.string "recordi")) ]
  and result_type =
    (* recordt R -> NamedEnv Val (record_field_type R) *)
    let parameter_type = PP.simple_app [ PP.string "recordt"; Identifier.pp_identifier matched_identifier ]
    and return_type =
      PP.simple_app [
        PP.string "NamedEnv";
        PP.string "Val";
        PP.parens @@ PP.simple_app [ PP.string "record_field_type"; Identifier.pp_identifier matched_identifier ]
      ]
    in
    Some (Coq.function_type [ parameter_type ] return_type)
  and contents =
    let record_case_handler (record_definition : Ast.Definition.Type.Record.t) : PP.document * PP.document =
      let pattern =
        Identifier.pp_identifier @@ TranslationSettings.convert_record_name_to_tag record_definition.identifier
      and expression =
        let lambda_parameter = Ast.Identifier.mk "r"
        in
        let lambda_body =
          let bindings =
            let make_binding (field_identifier, field_type) =
              PP.separate PP.space [
                PP.utf8string "►";
                PP.parens @@ PP.separate PP.space [
                  PP.dquotes @@ Identifier.pp_identifier field_identifier;
                  PP.utf8string "∷";
                  AnnotationContext.drop_annotations @@ Nanotype.pp_nanotype field_type;
                  PP.utf8string "↦";
                  Identifier.pp_identifier field_identifier;
                  Identifier.pp_identifier lambda_parameter
                ]
              ]
            in
            List.map ~f:make_binding record_definition.fields
          in
          PP.(string "env.nil" ^^ hardline ^^ twice space ^^ align (separate hardline bindings))
        in
        Coq.lambda (Identifier.pp_identifier lambda_parameter) lambda_body
      in
      (pattern, expression)
    in
    Types.Records.generate_tag_match ~matched_identifier ~record_definitions ~record_case_handler
  in
  Coq.definition ~identifier ~parameters ~result_type contents


let pp_typedefkit_instance () =
  PP.lines [
    "#[export,refine] Instance typedefkit : TypeDefKit typedenotekit :=";
    "  {| unionk           := union_constructor;";
    "     unionk_ty        := union_constructor_type;";
    "     recordf          := string;";
    "     recordf_ty       := record_field_type;";
    "     unionv_fold      := union_fold;";
    "     unionv_unfold    := union_unfold;";
    "     recordv_fold     := record_fold;";
    "     recordv_unfold   := record_unfold;";
    "  |}.";
    "Proof.";
    "  - abstract (now intros [] []).";
    "  - abstract (intros [] [[] x]; cbn in x;";
    "              repeat";
    "                match goal with";
    "                | x: unit     |- _ => destruct x";
    "                | x: prod _ _ |- _ => destruct x";
    "                end; auto).";
    "  - abstract (now intros [] []).";
    "  - abstract (intros []; now apply env.Forall_forall).";
    "Defined.";
  ]


let pp_canonicals () =
  let identifiers =
    List.map ~f:Ast.Identifier.mk [ "typedeclkit"; "typedenotekit"; "typedefkit" ]
  in
  PP.separate_map PP.hardline Coq.canonical identifiers


let pp_varkit_instance () =
  PP.string "#[export] Instance varkit : VarKit := DefaultVarKit."


let pp_regdeclkit register_definitions =
  Registers.generate_regdeclkit register_definitions


let pp_memory_model () =
  let identifier = Ast.Identifier.mk "MemoryModel"
  and content =
    Coq.comment @@ PP.string "TODO"
  in
  Coq.section identifier content


let pp_include_mixin () =
  Coq.include_module (PP.string "BaseMixin")


let pp_base_module (definitions : (Sail.sail_definition * Ast.Definition.t) list) : PP.document AC.t =
  let enum_definitions =
    List.map ~f:snd Ast.(select Extract.(type_definition of_enum) definitions)
  and variant_definitions =
    List.map ~f:snd Ast.(select Extract.(type_definition of_variant) definitions)
  and record_definitions =
    List.map ~f:snd Ast.(select Extract.(type_definition of_record) definitions)
  and register_definitions =
    Ast.(select Extract.register_definition definitions)
  in
  begin
    let base_module_name = "UntitledBase"
    and flag = Coq.Export
    and includes = [ "Base" ]
    and contents =
      let sections = [
        pp_typedeclkit ();
        pp_enum_denote enum_definitions;
        pp_union_denote variant_definitions;
        pp_record_denote record_definitions;
        pp_typedenotekit ();
        pp_union_constructor variant_definitions;
        pp_union_constructor_type variant_definitions;
        pp_eqdec_and_finite_instances ();
        pp_union_fold variant_definitions;
        pp_union_unfold variant_definitions;
        pp_record_field_type record_definitions;
        pp_record_fold record_definitions;
        pp_record_unfold record_definitions;
        pp_typedefkit_instance ();
        pp_canonicals ();
        pp_varkit_instance ();
        pp_regdeclkit register_definitions;
        pp_memory_model ();
        pp_include_mixin ();
      ]
      in
      PP.(separate small_step sections)
    in
    AC.return @@ Coq.module' ~flag ~includes base_module_name contents
  end
