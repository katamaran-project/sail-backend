module Big_int       = Nat_big_num


type t =
  | Unit
  | Bool      of bool
  | Int       of Big_int.num
  | String    of string
  | Prod      of t * t
  | Bit       of bool


let rec type_of_value (value : t) : Nanotype.t =
  match value with
  | Unit          -> Nanotype.Unit
  | Bool _        -> Nanotype.Bool
  | Int _         -> Nanotype.Int
  | String _      -> Nanotype.String
  | Prod (v1, v2) -> Nanotype.Tuple [type_of_value v1; type_of_value v2]
  | Bit _         -> Nanotype.Bit


let rec to_fexpr (value : t) : FExpr.t =
  match value with
   | Unit          -> FExpr.mk_symbol "Unit"
   | Bool b        -> FExpr.mk_bool b
   | Int n         -> FExpr.mk_int @@ Z.to_int n
   | String s      -> FExpr.mk_string s
   | Prod (v1, v2) -> FExpr.mk_application ~positional:[to_fexpr v1; to_fexpr v2] "Prod"
   | Bit b         -> FExpr.mk_application ~positional:[FExpr.mk_bool b] "Bit"
