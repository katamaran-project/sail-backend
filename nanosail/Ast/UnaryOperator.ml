type t =
  | Neg
  | Not


let to_fexpr (operator : t) : FExpr.t =
  match operator with
  | Neg -> FExpr.mk_symbol "Neg"
  | Not -> FExpr.mk_symbol "Not"
