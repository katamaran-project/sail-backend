open Base

module Big_int = Nat_big_num

module Sanitation = Sanitation

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


let rec translate_numeric_expression (numeric_expression : Libsail.Ast.nexp) : N.numeric_expression TC.t =
  let S.Nexp_aux (numeric_expression, numexp_location) = numeric_expression
  in
  match numeric_expression with
  | Nexp_constant constant                     -> TC.return @@ N.NE_constant constant
  | Nexp_var (Kid_aux (Var string, _location)) -> TC.return @@ N.NE_var (Id.mk string)
  | Nexp_times (x, y) -> begin
      let* x' = translate_numeric_expression x
      and* y' = translate_numeric_expression y
      in
      TC.return @@ N.NE_times (x', y')
    end
  | Nexp_sum (x, y) -> begin
      let* x' = translate_numeric_expression x
      and* y' = translate_numeric_expression y
      in
      TC.return @@ N.NE_add (x', y')
    end
  | Nexp_minus (x, y) -> begin
      let* x' = translate_numeric_expression x
      and* y' = translate_numeric_expression y
      in
      TC.return @@ N.NE_minus (x', y')
    end
  | Nexp_neg x  -> begin
      let* x' = translate_numeric_expression x
      in
      TC.return @@ N.NE_neg x'
    end
  | Nexp_id identifier -> begin
      let* identifier' = translate_identifier [%here] identifier
      in
      TC.return @@ N.NE_id identifier'
    end
  | Nexp_exp _      -> TC.not_yet_implemented [%here] numexp_location
  | Nexp_app (_, _) -> TC.not_yet_implemented [%here] numexp_location

and translate_numeric_constraint (numeric_constraint : Libsail.Ast.n_constraint) : N.numeric_constraint TC.t =
  let S.NC_aux (numeric_constraint, location) = numeric_constraint
  in
  match numeric_constraint with
  | S.NC_equal (x, y) -> begin
      let* x' = translate_numeric_expression x
      and* y' = translate_numeric_expression y
      in
      TC.return @@ N.NC_equal (x', y')
    end
  | S.NC_bounded_ge (x, y) -> begin
      let* x' = translate_numeric_expression x
      and* y' = translate_numeric_expression y
      in
      TC.return @@ N.NC_bounded_ge (x', y')
    end
  | S.NC_bounded_gt (x, y) -> begin
      let* x' = translate_numeric_expression x
      and* y' = translate_numeric_expression y
      in
      TC.return @@ N.NC_bounded_gt (x', y')
    end
  | S.NC_bounded_le (x, y) -> begin
      let* x' = translate_numeric_expression x
      and* y' = translate_numeric_expression y
      in
      TC.return @@ N.NC_bounded_le (x', y')
    end
  | S.NC_bounded_lt (x, y) -> begin
      let* x' = translate_numeric_expression x
      and* y' = translate_numeric_expression y
      in
      TC.return @@ N.NC_bounded_lt (x', y')
    end
  | S.NC_not_equal (x, y) -> begin
      let* x' = translate_numeric_expression x
      and* y' = translate_numeric_expression y
      in
      TC.return @@ N.NC_not_equal (x', y')
    end
  | S.NC_set (Kid_aux (Var kind_id, _loc), ns) -> TC.return @@ N.NC_set (Id.mk kind_id, ns)
  | S.NC_or (x, y) -> begin
      let* x' = translate_numeric_constraint x
      and* y' = translate_numeric_constraint y
      in
      TC.return @@ N.NC_or (x', y')
    end
  | S.NC_and (x, y) -> begin
      let* x' = translate_numeric_constraint x
      and* y' = translate_numeric_constraint y
      in
      TC.return @@ N.NC_and (x', y')
    end
  | S.NC_var (Kid_aux (Var kind_id, _loc))     -> TC.return @@ N.NC_var (Id.mk kind_id)
  | S.NC_true                                  -> TC.return @@ N.NC_true
  | S.NC_false                                 -> TC.return @@ N.NC_false
  | S.NC_app (_, _)                            -> TC.not_yet_implemented [%here] location
