type t =
  | NE_constant of Z.t
  | NE_add      of t * t
  | NE_minus    of t * t
  | NE_times    of t * t
  | NE_neg      of t
  | NE_id       of Identifier.t
  | NE_var      of Identifier.t
