module Big_int       = Nat_big_num


type t =
  | Unit
  | Bool   of bool
  | Int    of Big_int.num
  | String of string
  | Prod   of t * t


let rec type_of_value (value : t) : Nanotype.t =
  match value with
  | Unit          -> Unit
  | Bool _        -> Bool
  | Int _         -> Int
  | String _      -> String
  | Prod (v1, v2) -> Tuple [type_of_value v1; type_of_value v2]
