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


let translate_type_abbreviation
      (_definition_annotation : S.def_annot                 )
      (_type_annotation       : Sail.type_annotation S.annot)
      (identifier             : S.id                        )
      (quantifier             : S.typquant                  )
      (type_argument          : S.typ_arg                   ) : Ast.Definition.Type.t TC.t
  =
  let S.A_aux (unwrapped_type_argument, _type_argument_location) = type_argument
  in
  let* quantifier' = TypeQuantifier.translate_type_quantifier quantifier
  and* identifier' = Identifier.translate_identifier [%here] identifier
  in
  let* type_abbreviation =
    match unwrapped_type_argument with
    | A_nexp numeric_expression -> begin
        let* numeric_expression' = Numeric.translate_numeric_expression numeric_expression
        in
        TC.return @@ Ast.Definition.TA_numeric_expression (quantifier', numeric_expression')
      end
    | A_typ typ -> begin
        let* typ' = Nanotype.nanotype_of_sail_type typ
        in
        TC.return @@ Ast.Definition.TA_alias (quantifier', typ')
      end
    | A_bool numeric_constraint -> begin
        let* numeric_constraint' = Numeric.translate_numeric_constraint numeric_constraint
        in
        TC.return @@ Ast.Definition.TA_numeric_constraint (quantifier', numeric_constraint')
      end
  in
  TC.return @@ Ast.Definition.Type.TD_abbreviation { identifier = identifier'; abbreviation = type_abbreviation }
