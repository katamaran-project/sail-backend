module S = struct
  include Libsail
  include Libsail.Ast
  include Libsail.Ast_defs
  include Libsail.Anf
end

module Big_int = Nat_big_num
module TC = TranslationContext

open Monads.Notations.Star(TC)
open Base


let translate_definition (sail_definition : Sail.sail_definition) : (Sail.sail_definition * Ast.Definition.t) TC.t =
  TC.translation_block [%here] ("Processing definition " ^ String.strip @@ StringOf.Sail.definition sail_definition) begin
    let S.DEF_aux (unwrapped_sail_definition, annotation) = sail_definition
    in
    if
      Configuration.should_ignore_definition sail_definition
    then
      let* () = TC.log [%here] Logging.debug @@ lazy "Skipping this definition"
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
          | DEF_fundef function_definition           -> Translate.Function.translate_function_definition annotation function_definition
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
              StringOf.Sail.definition sail_definition
            in
            let* () = TC.log [%here] Logging.error @@ lazy (Printf.sprintf "Assertion error at %s\nMessage: %s\nSail code:\n%s" location_string message pretty_printed_sail_code)
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
    TC.translation_block [%here] "Translating Sail to Nanosail" begin
      TC.map ~f:translate_definition ast.defs
    end
  in
  let (result, _context) = TC.run translate
  in
  match result with
  | TC.Success definitions -> { definitions = definitions }
  | TC.Failure _           -> failwith "Bug: failures should have been recovered from earlier"
