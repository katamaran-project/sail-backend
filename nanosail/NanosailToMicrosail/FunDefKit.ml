open Base
open PP
open Auxlib
open Identifier
open Statements
open Monads.Notations.Star(AnnotationContext)

module AC = AnnotationContext


let pp_function_definition
      ((sail_function_definition : Sail.sail_definition), (function_definition : Ast.function_definition))
      type_constraint =
  let identifier = pp_identifier @@ Ast.Identifier.add_prefix "fun_" function_definition.function_name
  in
  let coq_definition =
    let* result_type =
      let* bindings =
        let* docs = AC.map ~f:PPSail.pp_bind function_definition.function_type.parameters
        in
        AC.return @@ Coq.list docs
      in
      let* result_type =
        Nanotype.pp_nanotype function_definition.function_type.return_type
      in
      AC.return @@ Some (
                      hanging_list (PP.string "Stm") [
                          bindings;
                          parens result_type
                        ]
                    )
    in
    let* body =
      pp_statement function_definition.function_body
    in
    let* extended_function_type' = Types.ExtendedType.pp_extended_function_type function_definition.function_type function_definition.extended_function_type
    in
    let* _ = AC.create_annotation_from_document extended_function_type'
    in
    AC.return @@ Coq.definition ~identifier ~result_type body
  in
  let original_sail_code =
    build_list (fun { add; _ } ->
        (
          match type_constraint with
          | Some (sail_type_constraint, _) -> add sail_type_constraint
          | None                           -> ()
        );
        add sail_function_definition;
      )
  in
  Coq.annotate_with_original_definitions
    original_sail_code
    (Coq.annotate coq_definition)


let pp_function_definitions
      (function_definitions : (Sail.sail_definition * Ast.function_definition) list)
      (top_level_type_constraint_definitions : (Sail.sail_definition * Ast.top_level_type_constraint_definition) list) =
  let type_and_function_pairs =
    let find_type_constraint function_name =
      match
        List.filter
          ~f:(fun (_, type_constraint) -> Ast.Identifier.equal type_constraint.identifier function_name)
          top_level_type_constraint_definitions
      with
      | [x] -> Some x
      | []  -> None
      | _   -> None
    in
    List.map ~f:(fun ((_sail_definition, function_definition) as fdef) ->
        (fdef, find_type_constraint function_definition.function_name))
      function_definitions
  in
  List.map ~f:(uncurry pp_function_definition) type_and_function_pairs


let pp_function_definition_kit
      function_definitions
      top_level_type_constraint_definitions =
  let fundef =
    let identifier = pp_identifier @@ Ast.Identifier.mk "FunDef"
    and implicit_parameters = [
        (utf8string "Δ", None);
        (utf8string "τ", None);
      ]
    and parameters = [
        (utf8string "f", utf8string "Fun Δ τ")
      ]
    and result_type = Some (utf8string "Stm Δ τ")
    and body =
      let matched_expression =
        utf8string "f in Fun Δ τ return Stm Δ τ"
      and cases =
        let case_of_function_definition (function_definition : Ast.function_definition) =
          (
            pp_identifier function_definition.function_name,
            pp_identifier @@ Ast.Identifier.add_prefix "fun_" function_definition.function_name
          )
        in
        List.map ~f:case_of_function_definition (List.map ~f:snd function_definitions)
      in
      Coq.match' matched_expression cases
    in
    Coq.definition ~identifier ~implicit_parameters ~parameters ~result_type body
  in
  let contents =
    separate small_step (
        build_list (fun { add; addall; _ } ->
            addall @@ pp_function_definitions function_definitions top_level_type_constraint_definitions;
            add fundef
          )
      )
  in
  Coq.section (Ast.Identifier.mk "FunDefKit") contents
