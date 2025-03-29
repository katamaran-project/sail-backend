open ExtBase

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
      Type.translate_type typ
    in
    let polymorphic =
      match type_quantifier' with
      | TypeQuantifier [] -> false
      | TypeQuantifier _  -> true   (* we assume this straightforward check is sufficient to determine whether we're dealing with polymorphism *)
    in
    let* monomorphs : Ast.Definition.TopLevelTypeConstraint.t list =
      match polymorphic, Configuration.requested_monomorphizations_for identifier' with
      | false, None -> TC.return []
      | true , None -> begin
          let* () =
            let message = lazy begin
              PP.format "Encountered polymorphic top level type constraint %s without requests for monomorphization" (Ast.Identifier.to_string identifier')
            end
            in
            TC.log [%here] Logging.warning message
          in
          TC.return []
        end
      | false, Some _ -> begin
          let* () =
            let message = lazy begin
              PP.format "Monomorphization requests found for %s, which is not polymorphic" (Ast.Identifier.to_string identifier')
            end
            in
            TC.log [%here] Logging.error message
          in
          TC.return []
        end
      | true, Some monomorphization_requests -> begin
          let build_monomorph (monomorphization_request : Configuration.monomorphization_request) : Ast.Definition.TopLevelTypeConstraint.t TC.t =
            (* todo this function is duplicated (see module Function) *)
            let substitution (identifier : Ast.Identifier.t) : Ast.Numeric.Expression.t =
              match List.Assoc.find monomorphization_request.substitutions ~equal:Ast.Identifier.equal identifier with
              | Some value -> Ast.Numeric.Expression.Constant (Z.of_int value)
              | None       -> Ast.Numeric.Expression.Id identifier
            in
            let top_level_type_constraint : Ast.Definition.TopLevelTypeConstraint.t =
              {
                identifier      = monomorphization_request.monomorphization_identifier;
                type_quantifier = Ast.TypeQuantifier.TypeQuantifier []; (* todo we might want to do better than to indiscriminately throw it all away *)
                typ             = Ast.Type.substitute_numeric_expression_identifier substitution typ';
                polymorphic     = false;
                monomorphs      = []
              }
            in
            TC.return top_level_type_constraint
          in
          TC.map monomorphization_requests ~f:build_monomorph
        end
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
              FExpr.pp @@ Ast.TypeQuantifier.to_fexpr type_quantifier'
            );
            (
              PP.string "Type",
              FExpr.pp @@ Ast.Type.to_fexpr typ'
            );
            (
              PP.string "Polymorphic",
              FExpr.pp @@ FExpr.mk_bool polymorphic
            );
            (
              PP.string "Monomorphs",
              FExpr.pp @@ FExpr.mk_list begin
                List.map ~f:Ast.Definition.TopLevelTypeConstraint.to_fexpr monomorphs
              end
            )
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
        identifier      = identifier';
        type_quantifier = type_quantifier';
        typ             = typ';
        polymorphic;
        monomorphs;
      }
    end
  end
