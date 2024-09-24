open Base

module Big_int = Nat_big_num

module Sanitation = Sanitation

module S = struct
  include Libsail
  include Libsail.Ast
  include Libsail.Ast_defs
  include Libsail.Anf
end

module TC = TranslationContext
open Monads.Notations.Star(TC)


let rec translate_numeric_expression (numeric_expression : Libsail.Ast.nexp) : Ast.Numeric.Expression.t TC.t =
  let translate_binary_operation
        (factory : Ast.Numeric.Expression.t -> Ast.Numeric.Expression.t -> Ast.Numeric.Expression.t)
        (left    : S.nexp                                                                          )
        (right   : S.nexp                                                                          ) : Ast.Numeric.Expression.t TC.t
    =
    let* left'  = translate_numeric_expression left
    and* right' = translate_numeric_expression right
    in
    TC.return @@ factory left' right'
  in

  let translate_sum   = translate_binary_operation @@ fun l r -> Add   (l, r)
  and translate_minus = translate_binary_operation @@ fun l r -> Minus (l, r)
  and translate_times = translate_binary_operation @@ fun l r -> Times (l, r)

  in
  let S.Nexp_aux (unwrapped_numeric_expression, numexp_location) = numeric_expression
  in
  match unwrapped_numeric_expression with
  | Nexp_constant constant                     -> TC.return @@ Ast.Numeric.Expression.Constant constant
  | Nexp_var (Kid_aux (Var string, _location)) -> TC.return @@ Ast.Numeric.Expression.Var (Ast.Identifier.mk string)
  | Nexp_times (x, y)                          -> translate_times x y
  | Nexp_sum (x, y)                            -> translate_sum x y
  | Nexp_minus (x, y)                          -> translate_minus x y
  | Nexp_neg x  -> begin
      let* x' = translate_numeric_expression x
      in
      TC.return @@ Ast.Numeric.Expression.Neg x'
    end
  | Nexp_id identifier -> begin
      let* identifier' = Identifier.translate_identifier [%here] identifier
      in
      TC.return @@ Ast.Numeric.Expression.Id identifier'
    end
  | Nexp_exp _      -> TC.not_yet_implemented [%here] numexp_location
  | Nexp_app (_, _) -> TC.not_yet_implemented [%here] numexp_location

and translate_numeric_constraint (numeric_constraint : Libsail.Ast.n_constraint) : Ast.Numeric.Constraint.t TC.t =
  let translate_comparison
        (factory : Ast.Numeric.Expression.t -> Ast.Numeric.Expression.t -> Ast.Numeric.Constraint.t)
        (left    : Libsail.Ast.nexp                                                                )
        (right   : Libsail.Ast.nexp                                                                ) : Ast.Numeric.Constraint.t TC.t
    =
    let* left'  = translate_numeric_expression left
    and* right' = translate_numeric_expression right
    in
    TC.return @@ factory left' right'

  and translate_binary_operation
      (factory : Ast.Numeric.Constraint.t -> Ast.Numeric.Constraint.t -> Ast.Numeric.Constraint.t)
      (left    : Libsail.Ast.n_constraint                                                        )
      (right   : Libsail.Ast.n_constraint                                                        ) : Ast.Numeric.Constraint.t TC.t
    =
      let* left'  = translate_numeric_constraint left
      and* right' = translate_numeric_constraint right
      in
      TC.return @@ factory left' right'

  and translate_application
      (function_identifier : Libsail.Ast.id          )
      (arguments           : Libsail.Ast.typ_arg list) : Ast.Numeric.Constraint.t TC.t
    =
    let* function_identifier' =
      Identifier.translate_identifier [%here] function_identifier
    and* arguments' =
      TC.return [] (* todo translate arguments *)
    in
    TC.return @@ Ast.Numeric.Constraint.App (function_identifier', arguments')
  in

  let translate_equal      = translate_comparison       @@ fun l r -> Equal      (l, r)
  and translate_not_equal  = translate_comparison       @@ fun l r -> NotEqual   (l, r)
  and translate_bounded_ge = translate_comparison       @@ fun l r -> BoundedGE  (l, r)
  and translate_bounded_gt = translate_comparison       @@ fun l r -> BoundedGT  (l, r)
  and translate_bounded_le = translate_comparison       @@ fun l r -> BoundedLE  (l, r)
  and translate_bounded_lt = translate_comparison       @@ fun l r -> BoundedLT  (l, r)
  and translate_or         = translate_binary_operation @@ fun l r -> Or         (l, r)
  and translate_and        = translate_binary_operation @@ fun l r -> And        (l, r)
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
  | S.NC_app (function_id, arguments)          -> translate_application function_id arguments
  | S.NC_or (x, y)                             -> translate_or x y
  | S.NC_and (x, y)                            -> translate_and x y
  | S.NC_set (Kid_aux (Var kind_id, _loc), ns) -> TC.return @@ Ast.Numeric.Constraint.Set (Ast.Identifier.mk kind_id, ns)
  | S.NC_var (Kid_aux (Var kind_id, _loc))     -> TC.return @@ Ast.Numeric.Constraint.Var (Ast.Identifier.mk kind_id)
  | S.NC_true                                  -> TC.return @@ Ast.Numeric.Constraint.True
  | S.NC_false                                 -> TC.return @@ Ast.Numeric.Constraint.False
