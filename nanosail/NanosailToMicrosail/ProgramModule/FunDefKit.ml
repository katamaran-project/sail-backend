open ExtBase
open Monads.Notations.Star(GenerationContext)

module GC = struct
  include GenerationContext
  include Monads.Util.Make(GenerationContext)
end


(*
   Wraps a generation computation so that, if it fails, the resulting error
   message is augmented with the location of the given Sail definition.
*)
let augment_failure_with_sail_location
    (sail_definition : Sail.sail_definition)
    (computation     : PP.t GC.t           ) : PP.t GC.t
  =
  GC.recover computation begin fun error_message ->
    let Libsail.Ast.DEF_aux (_, annotation) = sail_definition in
    let location_string = Sail.string_of_location annotation.loc in
    GC.fail [%here] @@
      Printf.sprintf "%s\n  (while generating Sail code at %s)" error_message location_string
  end


let rec pp_function_definition
    ((sail_function_definition, function_definition) : Sail.sail_definition * Ast.Definition.Function.t                       )
    (type_constraint                                 : (Sail.sail_definition * Ast.Definition.TopLevelTypeConstraint.t) option) : PP.t GC.t
  =
  augment_failure_with_sail_location sail_function_definition begin
  if
    (* if the function definition is polymorphic, break up into
      function monomorphs and recall this function on each. if not,
      generate Rocq code. *)
    function_definition.polymorphic && not (List.is_empty function_definition.monomorphs)
  then
    (* pair up each function monomorph with the sail constraint and
      the corresponding Nanosail constraint monomorph. *)
    let function_monomorphs_with_constraints : (Ast.Definition.Function.t * (Sail.sail_definition * Ast.Definition.TopLevelTypeConstraint.t) option) list =
      match type_constraint with
      | Some (sail_top_level_type_constraint, top_level_type_constraint) -> begin
          let monomorph_pairs =
            (*
               todo we're assuming that the monomorphs in function_definition and type_constraint match.
               This ought to be the case, but maybe we want to add some sanity checks.
            *)
            List.zip_exn function_definition.monomorphs top_level_type_constraint.monomorphs
          in
          List.map ~f:(fun (fundef, constr) -> (fundef, Some (sail_top_level_type_constraint, constr))) monomorph_pairs
        end
      (* if no type constraint was given, pair up each function
        monomorph with None instead. *)
      | None -> List.map ~f:(fun fundef -> (fundef, None)) function_definition.monomorphs
    in
    let* pp_monomorphs =
      GC.map
        function_monomorphs_with_constraints
        ~f:(fun (fundef, constr) -> pp_function_definition (sail_function_definition, fundef) constr)
    in
    GC.return @@ PP.paragraphs pp_monomorphs
  else begin
    (* in this branch, the function is monomorphic, so we generate the
      rocq code. *)
    GC.generation_block [%here] (Printf.sprintf "Function Definition %s" @@ Ast.Identifier.to_string function_definition.function_name) begin
      GC.block begin
        (* log that we're generating code for the current function. *)
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
        (* create the identifier by which the function will be known in
          the rocq code. *)
        let pp_identifier =
          Identifier.pp @@ Ast.Identifier.add_prefix "fun_" function_definition.function_name
        in
        let* coq_definition =
          (* a PP.t with the result type, as expressed in rocq, will be
           one of the arguments passed to Coq.pp_definition. *)
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
          (* the body is a statement and is pretty-printed by the code
            for statements. it will be passed to Coq.pp_definition. *)
          let* pp_body =
            GC.pp_annotate [%here] begin
              Statements.pp_statement function_definition.function_body
            end
          in
          (* if the config setting is set, annotate the rocq code for a
            function with its sail ast. *)
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
          (* in the end we print the definition simply by passing the
            identifier, result type and body (which are all PP.t) to
            Coq.definition. *)
          GC.return begin
            PP.annotate [%here] begin
              Coq.pp_definition
                ~identifier:pp_identifier
                ~result_type:pp_result_type
                pp_body
            end
          end
        in
        (* we have now defined the rocq definition. *)
        (* we now turn the Sail.sail_definition that was passed to us into a
          list of such definitions and add it to the GC. *)
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
        (* return the rocq definition  *)
        GC.return coq_definition
      end
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
