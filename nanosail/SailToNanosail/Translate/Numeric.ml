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
  let translate_binary_operation
      (factory : N.numeric_expression -> N.numeric_expression -> N.numeric_expression)
      (left    : S.nexp                                                              )
      (right   : S.nexp                                                              ) : N.numeric_expression TC.t
    =
    let* left'  = translate_numeric_expression left
    and* right' = translate_numeric_expression right
    in
    TC.return @@ factory left' right'
  in

  let translate_sum   = translate_binary_operation @@ fun l r -> N.NE_add   (l, r)
  and translate_minus = translate_binary_operation @@ fun l r -> N.NE_minus (l, r)
  and translate_times = translate_binary_operation @@ fun l r -> N.NE_times (l, r)

  in
  let S.Nexp_aux (unwrapped_numeric_expression, numexp_location) = numeric_expression
  in
  match unwrapped_numeric_expression with
  | Nexp_constant constant                     -> TC.return @@ N.NE_constant constant
  | Nexp_var (Kid_aux (Var string, _location)) -> TC.return @@ N.NE_var (Ast.Identifier.mk string)
  | Nexp_times (x, y)                          -> translate_times x y
  | Nexp_sum (x, y)                            -> translate_sum x y
  | Nexp_minus (x, y)                          -> translate_minus x y
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
  let translate_comparison
      (factory : N.numeric_expression -> N.numeric_expression -> N.numeric_constraint)
      (left    : Libsail.Ast.nexp                                                    )
      (right   : Libsail.Ast.nexp                                                    ) : N.numeric_constraint TC.t
    =
    let* left'  = translate_numeric_expression left
    and* right' = translate_numeric_expression right
    in
    TC.return @@ factory left' right'

  and translate_binary_operation
      (factory : N.numeric_constraint -> N.numeric_constraint -> N.numeric_constraint)
      (left    : Libsail.Ast.n_constraint                                            )
      (right   : Libsail.Ast.n_constraint                                            ) : N.numeric_constraint TC.t
    =
      let* left' = translate_numeric_constraint left
      and* right' = translate_numeric_constraint right
      in
      TC.return @@ factory left' right'

  in
  let translate_equal      = translate_comparison       @@ fun l r -> N.NC_equal      (l, r)
  and translate_not_equal  = translate_comparison       @@ fun l r -> N.NC_not_equal  (l, r)
  and translate_bounded_ge = translate_comparison       @@ fun l r -> N.NC_bounded_ge (l, r)
  and translate_bounded_gt = translate_comparison       @@ fun l r -> N.NC_bounded_gt (l, r)
  and translate_bounded_le = translate_comparison       @@ fun l r -> N.NC_bounded_le (l, r)
  and translate_bounded_lt = translate_comparison       @@ fun l r -> N.NC_bounded_lt (l, r)
  and translate_or         = translate_binary_operation @@ fun l r -> N.NC_or         (l, r)
  and translate_and        = translate_binary_operation @@ fun l r -> N.NC_and        (l, r)

  in
  let S.NC_aux (unwrapped_numeric_constraint, numeric_constraint_location) = numeric_constraint
  in
  match unwrapped_numeric_constraint with
  | S.NC_equal (x, y)                          -> translate_equal      x y
  | S.NC_not_equal (x, y)                      -> translate_not_equal  x y
  | S.NC_bounded_ge (x, y)                     -> translate_bounded_ge x y
  | S.NC_bounded_gt (x, y)                     -> translate_bounded_gt x y
  | S.NC_bounded_le (x, y)                     -> translate_bounded_le x y
  | S.NC_bounded_lt (x, y)                     -> translate_bounded_lt x y
  | S.NC_or (x, y)                             -> translate_or x y
  | S.NC_and (x, y)                            -> translate_and x y
  | S.NC_set (Kid_aux (Var kind_id, _loc), ns) -> TC.return @@ N.NC_set (Ast.Identifier.mk kind_id, ns)
  | S.NC_var (Kid_aux (Var kind_id, _loc))     -> TC.return @@ N.NC_var (Ast.Identifier.mk kind_id)
  | S.NC_true                                  -> TC.return @@ N.NC_true
  | S.NC_false                                 -> TC.return @@ N.NC_false
  | S.NC_app (_, _)                            -> TC.not_yet_implemented [%here] numeric_constraint_location
