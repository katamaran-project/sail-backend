open Base

module Big_int       = Nat_big_num


type t =
  | Unit
  | Bool      of bool
  | Int       of Big_int.num
  | String    of string
  | Prod      of t * t
  | Bit       of bool
  | Bitvector of bool list


let rec type_of_value (value : t) : Nanotype.t =
  match value with
  | Unit          -> Nanotype.Unit
  | Bool _        -> Nanotype.Bool
  | Int _         -> Nanotype.Int
  | String _      -> Nanotype.String
  | Prod (v1, v2) -> Nanotype.Tuple [type_of_value v1; type_of_value v2]
  | Bit _         -> Nanotype.Bit
  | Bitvector bs  -> Nanotype.Bitvector (Numeric.Expression.Constant (Z.of_int @@ List.length bs))


let rec to_fexpr (value : t) : FExpr.t =
  match value with
   | Unit          -> FExpr.mk_symbol "Unit"
   | Bool b        -> FExpr.mk_bool b
   | Int n         -> FExpr.mk_int @@ Z.to_int n
   | String s      -> FExpr.mk_string s
   | Prod (v1, v2) -> FExpr.mk_application ~positional:[to_fexpr v1; to_fexpr v2] "Prod"
   | Bit b         -> FExpr.mk_application ~positional:[FExpr.mk_bool b] "Bit"
   | Bitvector bs  -> begin
       let positional =
         List.map ~f:(fun b -> FExpr.mk_int @@ if b then 1 else 0) bs
       in
       FExpr.mk_application ~positional "Bitvector"
     end


let rec equal
    (value_1 : t)
    (value_2 : t) : bool
  =
  match value_1, value_2 with
   | Unit                , Unit                 -> true
   | Unit                , _                    -> false
   | Bool b1             , Bool b2              -> Bool.equal b1 b2
   | Bool _              , _                    -> false
   | Int n1              , Int n2               -> Big_int.equal n1 n2
   | Int _               , _                    -> false
   | String s1           , String s2            -> String.equal s1 s2
   | String _            , _                    -> false
   | Prod (left1, right1), Prod (left2, right2) -> equal left1 left2 && equal right1 right2
   | Prod (_, _)         , _                    -> false
   | Bit b1              , Bit b2               -> Bool.equal b1 b2
   | Bit _               , _                    -> false
   | Bitvector bs1       , Bitvector bs2        -> List.equal Bool.equal bs1 bs2
   | Bitvector _         , _                    -> false
