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
  | BitvectorSignedLessThan


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
   | BitvectorSignedLessThan -> FExpr.mk_symbol "BVSignedLessThan"
