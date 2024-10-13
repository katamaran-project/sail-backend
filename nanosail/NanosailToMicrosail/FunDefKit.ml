open Base
open Monads.Notations.Star(GenerationContext)

module GC = struct
  include GenerationContext
  include Monads.Util.Make(GenerationContext)
end


let genblock loc label doc =
  let* doc = doc
  in
  GC.generation_block loc label doc


let pp_function_definition
      ((sail_function_definition : Sail.sail_definition), (function_definition : Ast.Definition.Function.t))
      (type_constraint           : (Sail.sail_definition * 'a) option                                      ) : PP.document GC.t
  =
  genblock [%here] PP.(horizontal [ string "Function Definition "; Identifier.pp function_definition.function_name ]) begin
    GC.block begin
        let* () =
          GC.log Logging.debug @@ lazy (Printf.sprintf "Generating code for function %s" (StringOf.Nanosail.identifier function_definition.function_name))
        in
        let pp_identifier =
          PP.annotate [%here] @@ Identifier.pp @@ Ast.Identifier.add_prefix "fun_" function_definition.function_name
        in
        let* coq_definition =
          let* pp_result_type =
            let* bindings =
              let* parameters : (PP.document * PP.document) list =
                let pp
                      (id  : Ast.Identifier.t)
                      (typ : Ast.Type.t      ) : (PP.document * PP.document) GC.t
                  =
                  let pp_id =
                    PP.annotate [%here] @@ Identifier.pp id
                  in
                  let* pp_typ =
                    GC.pp_annotate [%here] @@ Nanotype.pp_nanotype typ
                  in
                  GC.return (pp_id, pp_typ)
                in
                GC.map ~f:(Auxlib.uncurry pp) function_definition.function_type.parameters
              in
              let docs =
                List.map ~f:(Auxlib.uncurry MuSail.pp_bind) parameters
              in
              GC.return @@ PP.annotate [%here] @@ Coq.pp_list docs
            in
            let* pp_result_type =
              GC.pp_annotate [%here] begin
                  Nanotype.pp_nanotype function_definition.function_type.return_type
                end
            in
            GC.return begin
                Some begin
                    PP.annotate [%here] begin
                        Coq.pp_hanging_application (PP.string "Stm") [
                            bindings;
                            PP.(surround parens) pp_result_type
                          ]
                      end
                  end
              end
          in
          let* pp_body =
            GC.pp_annotate [%here] begin
                Statements.pp_statement function_definition.function_body
              end
          in
          let* pp_extended_function_type =
            GC.pp_annotate [%here] begin
                Types.ExtendedType.pp_extended_function_type
                  function_definition.function_type
                  function_definition.extended_function_type
              end
          in
          let* () = GC.add_comment begin
                        PP.vertical [
                            PP.string "Extended type";
                            PP.indent pp_extended_function_type
                          ]
                      end
          and* () = GC.add_comment begin
                        PP.vertical [
                            PP.string "AST";
                            PP.indent @@ FExpr.pp @@ Ast.Definition.Function.to_fexpr function_definition
                          ]
                      end
          in
          GC.return begin
              PP.annotate [%here] begin
                  Coq.pp_definition
                    ~identifier:pp_identifier
                    ~result_type:pp_result_type
                    pp_body
                end
            end
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
        let* () = GC.add_original_definitions original_sail_code
        in
        GC.return @@ PP.annotate [%here] @@ coq_definition
      end
    end


let pp_function_definitions
      (function_definitions : (Sail.sail_definition * Ast.Definition.Function.t) list)
      (top_level_type_constraint_definitions : (Sail.sail_definition * Ast.Definition.top_level_type_constraint_definition) list) : PP.document list GC.t
  =
  let* () =
    GC.log Logging.debug @@ lazy "Generating translations for all functions"
  in
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
    List.map
      function_definitions
      ~f:(fun ((_sail_definition, function_definition) as fdef) ->
        (fdef, find_type_constraint function_definition.function_name))
  in
  GC.map ~f:(Auxlib.uncurry pp_function_definition) type_and_function_pairs


let pp_function_definition_kit
      (function_definitions                  : (Sail.sail_definition * Ast.Definition.Function.t) list                          )
      (top_level_type_constraint_definitions : (Sail.sail_definition * Ast.Definition.top_level_type_constraint_definition) list) : PP.document GC.t
  =
  genblock [%here] (PP.string "FunDefKit") begin
      let* () =
        GC.log Logging.debug @@ lazy "Generation FunDefKit"
      in
      let fundef =
        let identifier =
          PP.annotate [%here] @@ Identifier.pp @@ Ast.Identifier.mk "FunDef"
      
        and implicit_parameters = [
            (PP.annotate [%here] @@ PP.string "Δ", None);
            (PP.annotate [%here] @@ PP.string "τ", None);
          ]
      
        and parameters =
          [
            (
              PP.annotate [%here] @@ PP.string "f",
              Some (PP.annotate [%here] @@ PP.string "Fun Δ τ")
            )
          ]
      
        and result_type =
          Some (PP.annotate [%here] @@ PP.string "Stm Δ τ")
      
        and body =
          let matched_expression =
            PP.annotate [%here] @@ PP.string "f in Fun Δ τ return Stm Δ τ"
          and cases =
            let case_of_function_definition (function_definition : Ast.Definition.Function.t) =
              (
                PP.annotate [%here] @@ Identifier.pp function_definition.function_name,
                PP.annotate [%here] @@ Identifier.pp @@ Ast.Identifier.add_prefix "fun_" function_definition.function_name
              )
            in
            List.map ~f:case_of_function_definition @@ List.map ~f:snd function_definitions
          in
          PP.annotate [%here] @@ Coq.pp_match matched_expression cases
        in
        PP.annotate [%here] @@ Coq.pp_definition ~identifier ~implicit_parameters ~parameters ~result_type body
      in
      let* contents =
        let* function_definitions =
          pp_function_definitions function_definitions top_level_type_constraint_definitions
        in
        GC.return begin
            PP.annotate [%here] begin
                PP.paragraphs begin
                    Auxlib.build_list (fun { add; addall; _ } ->
                        addall function_definitions;
                        add    fundef
                      )
                  end
              end
          end
      in
      let section =
        PP.annotate [%here] @@ Coq.pp_section (Ast.Identifier.mk "FunDefKit") contents
      in
      GC.return @@ PP.annotate [%here] @@ section
    end
