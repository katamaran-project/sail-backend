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
  and* initial_value' = TC.lift_option @@ Option.map initial_value ~f:ValueDefinition.translate_expression
  in
  TC.return @@ Ast.Definition.RegisterDefinition {
    identifier    = identifier'   ;
    typ           = nanotype      ;
    initial_value = initial_value';
  }
