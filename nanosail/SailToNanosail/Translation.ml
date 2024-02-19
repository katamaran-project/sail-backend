module S = struct
  include Libsail
  include Libsail.Ast
  include Libsail.Ast_defs
  include Libsail.Anf
end

module Big_int = Nat_big_num
module N  = Ast
module TC = TranslationContext

open Monads.Notations.Star(TC)
open Base
open Basics
open Function
open TypeDefinition
open TopLevelTypeConstraint
open Register
open ValueDefinition


let translate_definition (S.DEF_aux (def, annotation) as sail_definition) : (N.sail_definition * N.definition) TC.t =
  if
    Configuration.ignore_definition sail_definition
  then
    TC.return (sail_definition, N.IgnoredDefinition)
  else begin
    let translation =
      let* result =
        match def with
        | DEF_type type_definition ->
           translate_type_definition annotation type_definition
        | DEF_let let_definition ->
           translate_value_definition annotation let_definition
        | DEF_val value_specification ->
           translate_top_level_type_constraint annotation value_specification
        | DEF_register specification ->
           translate_register annotation specification
        | DEF_fundef function_definition ->
           translate_function_definition annotation function_definition
        | DEF_outcome (_, _) ->
           TC.not_yet_implemented [%here] annotation.loc
        | DEF_impl _ ->
           TC.not_yet_implemented [%here] annotation.loc
        | DEF_mapdef _ ->
           TC.not_yet_implemented [%here] annotation.loc
        | DEF_instantiation (_, _) ->
           TC.not_yet_implemented [%here] annotation.loc
        | DEF_fixity (_, _, _) ->
           TC.not_yet_implemented [%here] annotation.loc
        | DEF_overload (_, _) ->
           TC.not_yet_implemented [%here] annotation.loc
        | DEF_default _ ->
           TC.not_yet_implemented [%here] annotation.loc
        | DEF_scattered _ ->
           TC.not_yet_implemented [%here] annotation.loc
        | DEF_measure (_, _, _) ->
           TC.not_yet_implemented [%here] annotation.loc
        | DEF_loop_measures (_, _) ->
           TC.not_yet_implemented [%here] annotation.loc
        | DEF_internal_mutrec _ ->
           TC.not_yet_implemented [%here] annotation.loc
        | DEF_pragma (pragma, _argument, location) ->
           TC.not_yet_implemented ~message:("pragma " ^ pragma) [%here] location
      in
      let* () = TC.register result
      in
      TC.return (sail_definition, result)
    in
    TC.recover translation begin fun error ->
      match error with
      | TC.NotYetImplemented (ocaml_location, sail_location, message) -> begin
          let untranslated_definition = N.UntranslatedDefinition {
              filename = ocaml_location.pos_fname;
              line_number = ocaml_location.pos_lnum;
              sail_location = sail_location;
              message = message
            }
          in
          TC.return (sail_definition, untranslated_definition)
        end
      | TC.AssertionFailure (ocaml_location, message) -> begin
          let location_string =
            Printf.sprintf "%s line %d" ocaml_location.pos_fname ocaml_location.pos_lnum
          and pretty_printed_sail_code =
            string_of_sail_definition sail_definition
          in
          failwith @@ Printf.sprintf "Assertion error at %s\nMessage: %s\nSail code:\n%s" location_string message pretty_printed_sail_code
        end
    end
  end

let translate (ast : Libsail.Type_check.tannot Libsail.Ast_defs.ast) name : N.program =
  let translate =
    let* () = Prelude.register_types ()
    in
    TC.map ~f:translate_definition ast.defs
  in
  let (result, _context) = TC.run translate
  in
  match result with
  | TC.Success definitions -> { program_name = name; definitions  = definitions }
  | TC.Failure _           -> failwith "Bug: failures should have been recovered from earlier"
