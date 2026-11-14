open ExtBase

module Big_int       = Nat_big_num


type t =
  | Unit
  | Bool      of bool
  | Int       of Big_int.num
  | String    of string
  | Prod      of t * t         (* todo replace this by tuple *)
  | Bit       of bool
  | Bitvector of bool list
  (*
     A record value. The fields are listed in the order of the record's definition.
     Values of this kind are produced only by evaluating top-level lets (see ValueDefinition.ml).
  *)
  | Record    of { type_identifier        : Identifier.t;
                   fields                 : (Identifier.t * t) list }
  | Enum      of { type_identifier        : Identifier.t;
                   constructor_identifier : Identifier.t }


let mk_bool (value : bool) =
  Bool value

let mk_int (value : int) =
  Int (Big_int.of_int value)

let mk_string (value : string) =
  String value


let rec type_of_value (value : t) : Type.t =
  match value with
  | Unit          -> Unit
  | Bool _        -> Bool
  | Int _         -> Int None
  | String _      -> String
  | Prod (v1, v2) -> Tuple [type_of_value v1; type_of_value v2]
  | Bit _         -> Bit
  | Bitvector bs  -> Bitvector (Numeric.Expression.Constant (Z.of_int @@ List.length bs))
  | Record { type_identifier; fields = _ }                 -> Record type_identifier
  | Enum { type_identifier; constructor_identifier = _ }   -> Enum type_identifier


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
   | Record { type_identifier; fields } -> begin
       let keyword =
         [
           ("type"  , Identifier.to_fexpr type_identifier);
           ("fields", FExpr.mk_list @@ List.map fields ~f:(fun (field, value) -> FExpr.mk_list [Identifier.to_fexpr field; to_fexpr value]));
         ]
       in
       FExpr.mk_application ~keyword "Record"
     end
   | Enum { type_identifier; constructor_identifier } -> begin
       FExpr.mk_application ~positional:[Identifier.to_fexpr type_identifier; Identifier.to_fexpr constructor_identifier] "Enum"
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
   | Record { type_identifier = type_identifier_1; fields = fields_1 },
     Record { type_identifier = type_identifier_2; fields = fields_2 } -> begin
       let equal_field (field_1, value_1) (field_2, value_2) =
         Identifier.equal field_1 field_2 && equal value_1 value_2
       in
       Identifier.equal type_identifier_1 type_identifier_2
       &&
       List.equal equal_field fields_1 fields_2
     end
   | Record _            , _                    -> false
   | Enum { type_identifier = type_identifier_1; constructor_identifier = constructor_identifier_1 },
     Enum { type_identifier = type_identifier_2; constructor_identifier = constructor_identifier_2 } -> begin
       Identifier.equal type_identifier_1 type_identifier_2
       &&
       Identifier.equal constructor_identifier_1 constructor_identifier_2
     end
   | Enum _              , _                    -> false
