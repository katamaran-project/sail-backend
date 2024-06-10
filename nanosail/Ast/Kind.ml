type t =
  | Type
  | Int
  | Bool


let to_string (kind : t) =
  match kind with
  | Type -> "Kind.Type"
  | Int  -> "Kind.Int"
  | Bool -> "Kind.Bool"
