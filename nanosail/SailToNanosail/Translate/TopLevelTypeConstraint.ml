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
open TypeDefinition


let translate_top_level_type_constraint
      (_definition_annotation : S.def_annot)
      (S.VS_aux (value_specification, _vspec_annotation)) : Ast.definition TC.t =
  let VS_val_spec (
          TypSchm_aux (
              TypSchm_ts (_quantifiers, Typ_aux (_typ, _type_location)),
              _type_scheme_location),
          identifier, _extern) = value_specification
  in
  let* identifier' = translate_identifier [%here] identifier
  in
  TC.return @@ Ast.TopLevelTypeConstraintDefinition { identifier = identifier' }
