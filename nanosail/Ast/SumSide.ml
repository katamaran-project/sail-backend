(*
   Identifies one of the two sides of a sum type (muSail's ty.sum).
   Left corresponds to inl, Right to inr.
*)
type t =
  | Left
  | Right


let equal
    (side_1 : t)
    (side_2 : t) : bool
  =
  match side_1, side_2 with
  | Left , Left  -> true
  | Right, Right -> true
  | Left , Right -> false
  | Right, Left  -> false


let to_string (side : t) : string =
  match side with
  | Left  -> "Left"
  | Right -> "Right"


let to_fexpr (side : t) : FExpr.t =
  FExpr.mk_symbol @@ "SumSide:" ^ to_string side
