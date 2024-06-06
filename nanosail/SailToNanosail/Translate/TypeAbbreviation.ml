open Base

module Big_int = Nat_big_num

module S = struct
  include Libsail
  include Libsail.Ast
  include Libsail.Ast_defs
  include Libsail.Anf
end

module N = Ast

module TC = TranslationContext
open Monads.Notations.Star(TC)
open Identifier
open Numeric
open Nanotype
open TypeQuantifier


let translate_type_abbreviation
      (_definition_annotation : Libsail.Ast.def_annot)
      (_type_annotation : Sail.type_annotation S.annot)
      (identifier : S.id)
      (quantifier : S.typquant)
      (S.A_aux (arg, _arg_location)) : N.type_definition TC.t
  =
  let* quantifier' = translate_type_quantifier quantifier
  and* identifier' = translate_identifier [%here] identifier
  in
  let* type_abbreviation =
    match arg with
    | A_nexp numeric_expression -> begin
        let* numeric_expression' = translate_numeric_expression numeric_expression
        in
        TC.return @@ N.TA_numeric_expression (quantifier', numeric_expression')
      end
    | A_typ typ -> begin
        let* typ' = nanotype_of_sail_type typ
        in
        TC.return @@ N.TA_alias (quantifier', typ')
      end
    | A_bool numeric_constraint -> begin
        let* numeric_constraint' = translate_numeric_constraint numeric_constraint
        in
        TC.return @@ N.TA_numeric_constraint (quantifier', numeric_constraint')
      end
  in
  TC.return @@ N.TD_abbreviation { identifier = identifier'; abbreviation = type_abbreviation }
