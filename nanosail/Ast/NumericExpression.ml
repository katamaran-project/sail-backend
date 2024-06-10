type t =
  | Constant of Z.t
  | Add      of t * t
  | Minus    of t * t
  | Times    of t * t
  | Neg      of t
  | Id       of Identifier.t
  | Var      of Identifier.t
