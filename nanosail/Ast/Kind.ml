type t =
  | Type
  | Int
  | Bool


let to_string (kind : t) =
  match kind with
  | Type -> "Kind.Type"
  | Int  -> "Kind.Int"
  | Bool -> "Kind.Bool"


let to_fexpr (kind : t) : FExpr.t =
  match kind with
  | Type -> FExpr.mk_symbol "KindType"
  | Int  -> FExpr.mk_symbol "KindInt"
  | Bool -> FExpr.mk_symbol "KindBool"
