open Base

module Big_int = Nat_big_num

module S = struct
  include Libsail
  include Libsail.Ast
  include Libsail.Ast_defs
  include Libsail.Anf
end

module TC = TranslationContext
open Monads.Notations.Star(TC)
open Identifier
open Nanotype
open TopLevelTypeConstraint


let translate_register
      (_definition_annotation        : Sail.definition_annotation     )
      (annotated_register_definition : Sail.type_annotation S.dec_spec) : Ast.Definition.t TC.t
  =
  let (S.DEC_aux (DEC_reg (sail_type, identifier, initial_value), (_spec_location, _spec_annotation))) = annotated_register_definition
  in
  let* identifier'    = translate_identifier [%here] identifier
  and* nanotype       = nanotype_of_sail_type sail_type
  in
  let* initial_value' =
    match initial_value with
    | Some unwrapped_initial_value -> begin
        let on_failed_translation (error : TC.Error.t) =
          Logging.info @@ lazy (Printf.sprintf "Failed to translate initial value for register %s:\n  %s" (Ast.Identifier.string_of identifier') (TC.Error.to_string error));
          TC.return @@ Ast.Definition.Register.RawSpecified (StringOf.Sail.exp unwrapped_initial_value)
        in
        TC.recover
          (let* v = ValueDefinition.translate_expression unwrapped_initial_value in TC.return @@ Ast.Definition.Register.Specified v)
          on_failed_translation
      end
    | None -> TC.return Ast.Definition.Register.NoneSpecified
    
  in
  TC.return @@ Ast.Definition.RegisterDefinition {
    identifier    = identifier'   ;
    typ           = nanotype      ;
    initial_value = initial_value';
  }
