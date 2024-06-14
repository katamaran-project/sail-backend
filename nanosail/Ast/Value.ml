module Big_int       = Nat_big_num


type t =
  | Val_unit
  | Val_bool   of bool
  | Val_int    of Big_int.num
  | Val_string of string
  | Val_prod   of t * t
