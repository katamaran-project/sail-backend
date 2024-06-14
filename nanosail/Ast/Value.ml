module Big_int       = Nat_big_num


type t =
  | Unit
  | Bool   of bool
  | Int    of Big_int.num
  | String of string
  | Prod   of t * t
