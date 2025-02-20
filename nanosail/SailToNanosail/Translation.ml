module S = struct
  include Libsail
  include Libsail.Ast
  include Libsail.Ast_defs
  include Libsail.Anf
end

module Big_int = Nat_big_num
module TC = TranslationContext

open Monads.Notations.Star(TC)
open! ExtBase


let should_ignore_definition (definition : Sail.sail_definition) : bool =
  let open Libsail.Ast
  in
  let open Configuration
  in
  let Libsail.Ast.DEF_aux (definition, _annotation) = definition
  in
  let member setting item =
    List.mem (get setting) item ~equal:String.equal
  in

  let should_ignore_pragma identifier =
    member ignored_pragmas identifier

  and should_ignore_function_definition function_definition =
    let identifier = Sail.identifier_of_function_definition function_definition
    in
    let arguments = [ Slang.Value.String identifier ]
    in
    let result, _ = Slang.EvaluationContext.run @@ get ignore_function_definition_predicate arguments
    in
    match result with
    | Success result -> Slang.Value.truthy result
    | Failure _      -> failwith "Error while reading configuration"

  and should_ignore_type_definition type_definition =
    let identifier = Sail.identifier_of_type_definition type_definition
    in
    let arguments = [ Slang.Value.String identifier ]
    in
    let result, _ = Slang.EvaluationContext.run @@ get ignore_type_definition_predicate arguments
    in
    match result with
    | Success result -> Slang.Value.truthy result
    | Failure _      -> failwith "Error while reading configuration"

  and should_ignore_value_definition (value_definition : Libsail.Type_check.tannot letbind) =
    let LB_aux (LB_val (pattern, E_aux (_, _)), (_location2, _type_annotation)) = value_definition
    in
    let identifier = Sail.identifier_of_pattern pattern
    in
    let arguments  = [ Slang.Value.String identifier ]
    in
    let result, _  = Slang.EvaluationContext.run @@ get ignore_value_definition_predicate arguments
    in
    match result with
    | Success result -> Slang.Value.truthy result
    | Failure _      -> failwith "Error while reading configuration"

  and should_ignore_top_level_type_constraint (top_level_type_constraint : Sail.type_annotation val_spec) =
    let identifier = Sail.identifier_of_top_level_type_constraint top_level_type_constraint
    in
    let arguments = [ Slang.Value.String identifier ]
    in
    let result, _ = Slang.EvaluationContext.run @@ get ignore_top_level_type_constraint_predicate arguments
    in
    match result with
    | Success result -> Slang.Value.truthy result
    | Failure _      -> failwith "Error while reading configuration"

  and should_ignore_default_definition (_default_spec : default_spec) : bool =
    get ignore_default_order

  in
  match definition with
  | DEF_type type_definition          -> should_ignore_type_definition type_definition
  | DEF_fundef function_definition    -> should_ignore_function_definition function_definition
  | DEF_mapdef _                      -> false
  | DEF_impl _                        -> false
  | DEF_let value_definition          -> should_ignore_value_definition value_definition
  | DEF_val top_level_type_constraint -> should_ignore_top_level_type_constraint top_level_type_constraint
  | DEF_outcome (_, _)                -> false
  | DEF_instantiation (_, _)          -> false
  | DEF_fixity (_, _, _)              -> false
  | DEF_overload (_, _)               -> get (ignore_overloads)
  | DEF_default default_spec          -> should_ignore_default_definition default_spec
  | DEF_scattered _                   -> false
  | DEF_measure (_, _, _)             -> false
  | DEF_loop_measures (_, _)          -> false
  | DEF_register _                    -> false
  | DEF_internal_mutrec _             -> false
  | DEF_constraint _                  -> false
  | DEF_pragma (identifier, _, _)     -> should_ignore_pragma identifier



let translate_definition (sail_definition : Sail.sail_definition) : (Sail.sail_definition * Ast.Definition.t) TC.t =
  let label =
    let open PP
    in
    vertical [
      string "Processing definition";
      indent @@ from_multiline_string @@ StringOf.Sail.definition sail_definition
    ]
  in
  TC.translation_block [%here] label begin
    let S.DEF_aux (unwrapped_sail_definition, annotation) = sail_definition
    in
    if
      should_ignore_definition sail_definition
    then
      let* () = TC.log [%here] Logging.info @@ lazy (PP.string "Skipping this definition")
      in
      TC.return (sail_definition, Ast.Definition.IgnoredDefinition)
    else begin
      let translation =
        let* result =
          match unwrapped_sail_definition with
          | DEF_type type_definition                 -> Translate.TypeDefinition.translate_type_definition annotation type_definition
          | DEF_let value_definition                 -> Translate.ValueDefinition.translate_value_definition annotation value_definition
          | DEF_val value_specification              -> Translate.TopLevelTypeConstraint.translate_top_level_type_constraint annotation value_specification
          | DEF_register specification               -> Translate.Register.translate_register annotation specification
          | DEF_fundef function_definition           -> Translate.Function.translate_function_definition sail_definition annotation function_definition
          | DEF_pragma (pragma, _argument, location) -> TC.not_yet_implemented ~message:("pragma " ^ pragma) [%here] location
          | DEF_fixity (_, _, _)                     -> TC.return Ast.Definition.IgnoredDefinition
          | DEF_outcome (_, _)                       -> TC.not_yet_implemented [%here] annotation.loc
          | DEF_impl _                               -> TC.not_yet_implemented [%here] annotation.loc
          | DEF_mapdef _                             -> TC.not_yet_implemented [%here] annotation.loc
          | DEF_instantiation (_, _)                 -> TC.not_yet_implemented [%here] annotation.loc
          | DEF_overload (_, _)                      -> TC.not_yet_implemented [%here] annotation.loc
          | DEF_default _                            -> TC.not_yet_implemented [%here] annotation.loc
          | DEF_scattered _                          -> TC.not_yet_implemented [%here] annotation.loc
          | DEF_measure (_, _, _)                    -> TC.not_yet_implemented [%here] annotation.loc
          | DEF_loop_measures (_, _)                 -> TC.not_yet_implemented [%here] annotation.loc
          | DEF_internal_mutrec _                    -> TC.not_yet_implemented [%here] annotation.loc
          | DEF_constraint _                         -> TC.not_yet_implemented [%here] annotation.loc
        in
        let* () = TC.store_definition result
        in
        TC.return (sail_definition, result)
      in
      TC.recover translation begin fun error ->
        match error with
        | NotYetImplemented (ocaml_location, sail_location, message) -> begin
            let untranslated_definition = Ast.Definition.UntranslatedDefinition
                {
                  filename      = ocaml_location.pos_fname;
                  line_number   = ocaml_location.pos_lnum ;
                  sail_location                           ;
                  message                                 ;
                }
            in
            TC.return (sail_definition, untranslated_definition)
          end
        | AssertionFailure (ocaml_location, message) -> begin
            let location_string =
              Printf.sprintf "%s line %d" ocaml_location.pos_fname ocaml_location.pos_lnum
            and pretty_printed_sail_code =
              PP.from_multiline_string @@ StringOf.Sail.definition sail_definition
            in
            let* () =
              let message = lazy begin
                let open PP
                in
                vertical [
                  format "Assertion error at %s" location_string;
                  horizontal [ string "Message: "; string message ];
                  horizontal [ string "Sail code: "; pretty_printed_sail_code ];
                ]
              end
              in
              TC.log [%here] Logging.error message
            in
            let untranslated_definition = Ast.Definition.UntranslatedDefinition
                {
                  filename      = ocaml_location.pos_fname;
                  line_number   = ocaml_location.pos_lnum ;
                  sail_location = annotation.loc          ;
                  message       = Some message            ;
                }
            in
            TC.return (sail_definition, untranslated_definition)
          end
      end
    end
  end

let translate (ast : Sail.ast) : Ast.program =
  let translate =
    let* () = Prelude.register_types ()
    in
    TC.translation_block [%here] (PP.string "Translating Sail to Nanosail") begin
      TC.map ~f:translate_definition ast.defs
    end
  in
  let (result, _context) = TC.run translate
  in
  match result with
  | TC.Success definitions -> { definitions = definitions }
  | TC.Failure _           -> failwith "Bug: failures should have been recovered from earlier"
