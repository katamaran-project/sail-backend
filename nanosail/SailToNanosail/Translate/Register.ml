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
      (_definition_annotation        : S.def_annot                    )
      (annotated_register_definition : Sail.type_annotation S.dec_spec) : Ast.definition TC.t
  =
  let (S.DEC_aux (DEC_reg (sail_type, identifier, expression), (_spec_location, _spec_annotation))) = annotated_register_definition
  in
  match expression with
  | None -> begin
      let* identifier' = translate_identifier [%here] identifier
      and* nanotype    = nanotype_of_sail_type sail_type
      in
      TC.return @@ Ast.RegisterDefinition {
          identifier = identifier';
          typ        = nanotype   ;
        }
    end
  | Some (E_aux (_expr, (location, _annotation))) -> TC.not_yet_implemented [%here] location
