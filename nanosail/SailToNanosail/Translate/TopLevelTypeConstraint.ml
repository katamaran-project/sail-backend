open! ExtBase

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
      (_definition_annotation : Sail.definition_annotation)
      (S.VS_aux (value_specification, _vspec_annotation)) : Ast.Definition.t TC.t
  =
  TC.translation_block [%here] (PP.string "Translating top level type constraint") begin
    let VS_val_spec (
        TypSchm_aux (
          TypSchm_ts (type_quantifier, Typ_aux (_typ, _type_location)),
          _type_scheme_location),
        identifier, _extern) = value_specification
    in
    let* identifier' = translate_identifier [%here] identifier
    in
    let* () =
      let* type_quantifier' =
        TypeQuantifier.translate_type_quantifier type_quantifier
      in
      let message = lazy begin
        let properties =
          PP.description_list [
            (
              PP.string "Target",
              PP.string @@ Ast.Identifier.to_string identifier'
            );
            (
              PP.string "Type quantifier",
              PP.string @@ FExpr.to_string @@ Ast.TypeQuantifier.to_fexpr type_quantifier'
            );
          ]
        in
        PP.vertical [
          PP.format "Translated top level type constraint %s" @@ Ast.Identifier.to_string identifier';
          PP.indent properties;
        ]
      end
      in
      TC.log [%here] Logging.info message
    in
    TC.return @@ Ast.Definition.TopLevelTypeConstraintDefinition { identifier = identifier' }
  end
