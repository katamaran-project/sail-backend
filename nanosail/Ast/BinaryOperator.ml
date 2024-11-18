module Signedness = struct
  type t = Signed | Unsigned

  let to_fexpr (signedness : t) : FExpr.t =
    match signedness with
    | Signed   -> FExpr.mk_symbol "Signed"
    | Unsigned -> FExpr.mk_symbol "Unsigned"
end

module Comparison = struct
  type t =
    | LessThanOrEqualTo
    | LessThan
    | GreaterThanOrEqualTo
    | GreaterThan

  let to_fexpr (comparison : t) : FExpr.t =
    match comparison with
    | LessThanOrEqualTo    -> FExpr.mk_symbol "LessThanOrEqualTo"
    | LessThan             -> FExpr.mk_symbol "LessThan"
    | GreaterThanOrEqualTo -> FExpr.mk_symbol "GreaterThanOrEqualTo"
    | GreaterThan          -> FExpr.mk_symbol "GreaterThan"
end
  

type t =
  | Plus
  | Times
  | Minus
  | And
  | Or
  | Pair
  | Cons
  | Append
  | EqualTo
  | NotEqualTo
  | LessThanOrEqualTo
  | LessThan
  | GreaterThanOrEqualTo
  | GreaterThan
  | BitvectorComparison of Signedness.t * Comparison.t


let to_fexpr (operator : t) : FExpr.t =
  match operator with
   | Plus                 -> FExpr.mk_symbol "Plus"
   | Times                -> FExpr.mk_symbol "Times"
   | Minus                -> FExpr.mk_symbol "Minus"
   | And                  -> FExpr.mk_symbol "And"
   | Or                   -> FExpr.mk_symbol "Or"
   | Pair                 -> FExpr.mk_symbol "Pair"
   | Cons                 -> FExpr.mk_symbol "Cons"
   | Append               -> FExpr.mk_symbol "Append"
   | EqualTo              -> FExpr.mk_symbol "EqualTo"
   | NotEqualTo           -> FExpr.mk_symbol "NotEqualTo"
   | LessThanOrEqualTo    -> FExpr.mk_symbol "LessThanOrEqualTo"
   | LessThan             -> FExpr.mk_symbol "LessThan"
   | GreaterThanOrEqualTo -> FExpr.mk_symbol "GreaterThanOrEqualTo"
   | GreaterThan          -> FExpr.mk_symbol "GreaterThan"
   | BitvectorComparison (signedness, comparison) -> begin
       let positional = [
         Signedness.to_fexpr signedness;
         Comparison.to_fexpr comparison
       ]
       in
       FExpr.mk_application ~positional "BitvectorComparison"
     end
