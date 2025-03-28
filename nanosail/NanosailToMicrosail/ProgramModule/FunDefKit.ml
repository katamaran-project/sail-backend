open ExtBase
open Monads.Notations.Star(GenerationContext)

module GC = struct
  include GenerationContext
  include Monads.Util.Make(GenerationContext)
end


let rec pp_function_definition
    ((sail_function_definition, function_definition) : Sail.sail_definition * Ast.Definition.Function.t                       )
    (type_constraint                                 : (Sail.sail_definition * Ast.Definition.TopLevelTypeConstraint.t) option) : PP.t GC.t
  =
  if
    function_definition.polymorphic && not (List.is_empty function_definition.monomorphs)
  then
    let pairs : (Ast.Definition.Function.t * (Sail.sail_definition * Ast.Definition.TopLevelTypeConstraint.t) option) list =
      match type_constraint with
      | Some (sail_top_level_type_constraint, top_level_type_constraint) -> begin
          let pairs =
            (*
               todo we're assuming that the monomorphs in function_definition and type_constraint match.
               This ought to be the case, but maybe we want to add some sanity checks.
            *)
            List.zip_exn function_definition.monomorphs top_level_type_constraint.monomorphs
          in
          List.map ~f:(fun (fundef, constr) -> (fundef, Some (sail_top_level_type_constraint, constr))) pairs
        end
      | None -> List.map ~f:(fun fundef -> (fundef, None)) function_definition.monomorphs
    in
    let* pp_monomorphs =
      GC.map
        pairs
        ~f:(fun (fundef, constr) -> pp_function_definition (sail_function_definition, fundef) constr)
    in
    GC.return @@ PP.paragraphs pp_monomorphs
  else begin
    GC.generation_block [%here] (Printf.sprintf "Function Definition %s" @@ Ast.Identifier.to_string function_definition.function_name) begin
      GC.block begin
        let* () =
          GC.log [%here] Logging.debug begin
            lazy begin
              let string_of_function_name =
                Ast.Identifier.to_string function_definition.function_name
              in
              PP.string begin
                Printf.sprintf
                  "Generating code for function %s"
                  string_of_function_name
              end
            end
          end
        in
        let pp_identifier =
          Identifier.pp @@ Ast.Identifier.add_prefix "fun_" function_definition.function_name
        in
        let* coq_definition =
          let* pp_result_type =
            let* bindings =
              let* parameters : (PP.t * PP.t) list =
                let pp
                    (id  : Ast.Identifier.t)
                    (typ : Ast.Type.t      ) : (PP.t * PP.t) GC.t
                  =
                  let pp_id =
                    Identifier.pp id
                  in
                  let* pp_typ =
                    Type.pp_type typ
                  in
                  GC.return (pp_id, pp_typ)
                in
                GC.map ~f:(Fn.uncurry pp) function_definition.function_type.parameters
              in
              let docs =
                List.map ~f:(Fn.uncurry MuSail.pp_bind) parameters
              in
              GC.return @@ PP.annotate [%here] @@ Coq.pp_list docs
            in
            let* pp_result_type =
              Type.pp_type function_definition.function_type.return_type
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
          let* () =
            if
              Configuration.(get annotate_functions_with_ast)
            then
              GC.add_comment begin
                PP.vertical begin
                  ExtBase.List.build_list @@ fun { add; _ } -> begin
                    match type_constraint with
                    | Some (_sail_type_constraint, nanosail_type_constraint) -> begin
                        add @@ PP.string "Top Level Type Constraint";
                        add @@ PP.indent @@ PP.undecorate @@ FExpr.pp @@ Ast.Definition.TopLevelTypeConstraint.to_fexpr nanosail_type_constraint;
                        add @@ PP.string "";
                      end
                    | None -> ()
                    end;
                    add @@ PP.string "AST";
                    add @@ PP.indent @@ PP.undecorate @@ FExpr.pp @@ Ast.Definition.Function.to_fexpr function_definition
                  end
                end
            else
              GC.return ()
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
          List.build_list (fun { add; _ } ->
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
        GC.return coq_definition
      end
    end
  end


let pp_function_definitions
    (function_definitions                  : (Sail.sail_definition * Ast.Definition.Function.t) list              )
    (top_level_type_constraint_definitions : (Sail.sail_definition * Ast.Definition.TopLevelTypeConstraint.t) list) : PP.t list GC.t
  =
  let* () =
    GC.log [%here] Logging.debug @@ lazy (PP.string "Generating translations for all functions")
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
  GC.map ~f:(Fn.uncurry pp_function_definition) type_and_function_pairs


let pp_fundef_inductive_type (function_definitions : Ast.Definition.Function.t list) : PP.t GC.t =
  (* extract monomorphs *)
  let function_definitions : Ast.Definition.Function.t list =
    let fetch_monomorphs_if_present (function_definition : Ast.Definition.Function.t) : Ast.Definition.Function.t list =
      if
        List.is_empty function_definition.monomorphs
      then
        [ function_definition ]
      else
        function_definition.monomorphs
    in
    List.concat_map function_definitions ~f:fetch_monomorphs_if_present
  in
  GC.generation_block [%here] "FunDef" begin
    let identifier =
      Identifier.pp @@ Ast.Identifier.mk "FunDef"
    and implicit_parameters = [
      PP.string "Δ", None;
      PP.string "τ", None;
    ]
    and parameters =
      [
        (
          PP.string "f",
          Some (PP.string "Fun Δ τ")
        )
      ]
    and result_type =
      Some (PP.string "Stm Δ τ")
    and body =
      let matched_expression =
        PP.string "f in Fun Δ τ return Stm Δ τ"
      and cases =
        let case_of_function_definition (function_definition : Ast.Definition.Function.t) =
          (
            Identifier.pp function_definition.function_name,
            Identifier.pp @@ Ast.Identifier.add_prefix "fun_" function_definition.function_name
          )
        in
        List.map ~f:case_of_function_definition function_definitions
      in
      Coq.pp_match matched_expression cases
    in
    GC.return @@ Coq.pp_definition ~identifier ~implicit_parameters ~parameters ~result_type body
  end


let pp_function_definition_kit
    (function_definitions                  : (Sail.sail_definition * Ast.Definition.Function.t) list              )
    (top_level_type_constraint_definitions : (Sail.sail_definition * Ast.Definition.TopLevelTypeConstraint.t) list) : PP.t GC.t
  =
  GC.generation_block [%here] "FunDefKit" begin
    let* contents =
      let* pp_function_definitions =
        pp_function_definitions function_definitions top_level_type_constraint_definitions
      and* pp_fundef =
        pp_fundef_inductive_type @@ Ast.Definition.Select.drop_sail_definitions function_definitions
      in
      GC.return begin
        PP.paragraphs begin
          List.build_list (fun { add; addall; _ } ->
              addall pp_function_definitions;
              add    pp_fundef
            )
        end
      end
    in
    let section =
      Coq.pp_section (PP.string "FunDefKit") contents
    in
    GC.return @@ PP.annotate [%here] @@ section
  end
