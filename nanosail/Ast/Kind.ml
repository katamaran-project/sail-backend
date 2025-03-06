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
  let mk (name : string) =
    FExpr.mk_symbol @@ "Kind:" ^ name
  in
  match kind with
  | Type -> mk "Type"
  | Int  -> mk "Int"
  | Bool -> mk "Bool"
