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
    (_definition_annotation    : Sail.definition_annotation     )
    (top_level_type_constraint : Sail.type_annotation S.val_spec) : Ast.Definition.t TC.t
  =  
  TC.translation_block [%here] (PP.string "Translating top level type constraint") begin
    let VS_aux (unwrapped_top_level_type_constraint, _vspec_annotation) = top_level_type_constraint
    in
    let VS_val_spec (type_scheme, identifier, _extern) = unwrapped_top_level_type_constraint
    in
    let TypSchm_aux (unwrapped_type_scheme, _type_scheme_location) = type_scheme
    in
    let TypSchm_ts (type_quantifier, typ) = unwrapped_type_scheme
    in
    let* identifier' = translate_identifier [%here] identifier
    in
    let* type_quantifier' =
      TypeQuantifier.translate_type_quantifier type_quantifier
    in
    let* typ' =
      Nanotype.nanotype_of_sail_type typ
    in
    let polymorphic =
      match type_quantifier' with
      | TypeQuantifier [] -> false
      | TypeQuantifier _  -> true   (* we assume this straightforward check is sufficient to determine whether we're dealing with polymorphism *)
    in
    let* () =
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
            (
              PP.string "Type",
              PP.string @@ FExpr.to_string @@ Ast.Type.to_fexpr typ'
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
    TC.return begin
      Ast.Definition.TopLevelTypeConstraintDefinition {
        identifier = identifier';
        polymorphic
      }
    end
  end
