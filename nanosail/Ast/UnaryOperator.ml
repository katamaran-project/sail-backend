type t =
  | Neg
  | Not


let to_fexpr (operator : t) : FExpr.t =
  match operator with
  | Neg -> FExpr.mk_symbol "Neg"
  | Not -> FExpr.mk_symbol "Not"


let equal
    (operator_1 : t)
    (operator_2 : t) : bool
  =
  match operator_1, operator_2 with
  | Neg, Neg -> true
  | Neg, _   -> false
  | Not, Not -> true
  | Not, _   -> false
