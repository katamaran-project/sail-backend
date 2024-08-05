open Base
open Monads.Notations.Star(GenerationContext)

module GC = GenerationContext


let pp_function_definition
      ((sail_function_definition : Sail.sail_definition), (function_definition : Ast.Definition.Function.t))
      (type_constraint           : (Sail.sail_definition * 'a) option                                      ) : PP.document GC.t =
  let identifier = Identifier.pp @@ Ast.Identifier.add_prefix "fun_" function_definition.function_name
  in
  let* coq_definition =
    let* result_type =
      let* bindings =
        let* parameters : (Ast.Identifier.t * PP.document) list =
          let pp (id : Ast.Identifier.t) (t : Ast.Type.t) =
            let* t' = Nanotype.pp_nanotype t
            in
            GC.return (id, t')
          in
          GC.map ~f:(Auxlib.uncurry pp) function_definition.function_type.parameters
        in
        let docs = List.map ~f:PPSail.pp_bind parameters
        in
        GC.return @@ Coq.pp_list docs
      in
      let* result_type =
        Nanotype.pp_nanotype function_definition.function_type.return_type
      in
      GC.return @@ Some (
        PP.hanging_list (PP.string "Stm") [
          bindings;
          PP.parens result_type
        ]
      )
    in
    let* body =
      Statements.pp_statement function_definition.function_body
    in
    let* extended_function_type' = Types.ExtendedType.pp_extended_function_type function_definition.function_type function_definition.extended_function_type
    in
    let* () = GC.add_comment extended_function_type'
    in
    GC.return @@ Coq.definition ~identifier ~result_type body
  in
  let original_sail_code =
    Auxlib.build_list (fun { add; _ } ->
        (
          match type_constraint with
          | Some (sail_type_constraint, _) -> add sail_type_constraint
          | None                           -> ()
        );
        add sail_function_definition;
      )
  in
  GC.block begin
    let* () = GC.add_original_definitions original_sail_code
    in
    GC.return @@ coq_definition
  end


let pp_function_definitions
      (function_definitions : (Sail.sail_definition * Ast.Definition.Function.t) list)
      (top_level_type_constraint_definitions : (Sail.sail_definition * Ast.Definition.top_level_type_constraint_definition) list) : PP.document list GC.t =
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
  GC.map ~f:(Auxlib.uncurry pp_function_definition) type_and_function_pairs


let pp_function_definition_kit
      (function_definitions                  : (Sail.sail_definition * Ast.Definition.Function.t) list                          )
      (top_level_type_constraint_definitions : (Sail.sail_definition * Ast.Definition.top_level_type_constraint_definition) list) : PP.document GC.t =
  let fundef =
    let identifier = Identifier.pp @@ Ast.Identifier.mk "FunDef"
    and implicit_parameters = [
        (PP.utf8string "Δ", None);
        (PP.utf8string "τ", None);
      ]
    and parameters = [
        (PP.utf8string "f", Some (PP.utf8string "Fun Δ τ"))
      ]
    and result_type = Some (PP.utf8string "Stm Δ τ")
    and body =
      let matched_expression =
        PP.utf8string "f in Fun Δ τ return Stm Δ τ"
      and cases =
        let case_of_function_definition (function_definition : Ast.Definition.Function.t) =
          (
            Identifier.pp function_definition.function_name,
            Identifier.pp @@ Ast.Identifier.add_prefix "fun_" function_definition.function_name
          )
        in
        List.map ~f:case_of_function_definition (List.map ~f:snd function_definitions)
      in
      Coq.match' matched_expression cases
    in
    Coq.definition ~identifier ~implicit_parameters ~parameters ~result_type body
  in
  let* contents =
    let* function_definitions =
      pp_function_definitions function_definitions top_level_type_constraint_definitions
    in
    GC.return @@ PP.vertical ~spacing:2 begin
        Auxlib.build_list (fun { add; addall; _ } ->
            addall function_definitions;
            add    fundef
          )
      end
  in
  GC.return @@ Coq.pp_section (Ast.Identifier.mk "FunDefKit") contents
