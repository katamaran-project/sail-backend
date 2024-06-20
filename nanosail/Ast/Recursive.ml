open Base

(*
  Type, TypeArgument and NumericConstraint

  Put in same file because they are recursive
*)

module rec Type : sig
  type t =
    | Int
    | Bool
    | String
    | List        of t
    | Product     of t * t
    | Sum         of t * t
    | Unit
    | Enum        of Identifier.t
    | Bitvector   of NumericExpression.t
    | Tuple       of t list
    (* | Ty_union *)                                  (* TODO add *)
    | Record      of Identifier.t
    | Application of t * TypeArgument.t list

  val to_string : t -> string

  val equal : t -> t -> bool
end = struct
(*
  should mirror

    Inductive Ty : Set :=
    | int
    | bool
    | string
    | list (σ : Ty)
    | prod (σ τ : Ty)
    | sum  (σ τ : Ty)
    | unit
    | enum (E : enumi)
    | bvec (n : nat)
    | tuple (σs : Ctx Ty)
    | union (U : unioni)
    | record (R : recordi)
    .

   defined in theories/Syntax/TypeDecl.v
 *)
  type t =
    | Int
    | Bool
    | String
    | List        of t
    | Product     of t * t
    | Sum         of t * t
    | Unit
    | Enum        of Identifier.t
    | Bitvector   of NumericExpression.t
    | Tuple       of t list
    (* | Ty_union *)                                  (* TODO add *)
    | Record      of Identifier.t
    | Application of t * TypeArgument.t list

  let rec to_string (t : t) : string =
    match t with
    | Int              -> "Type.Int"
    | Bool             -> "Type.Bool"
    | String           -> "Type.String"
    | List _           -> "Type.List"
    | Product (t1, t2) -> Printf.sprintf "(%s * %s)" (to_string t1) (to_string t2)
    | Sum (t1, t2)     -> Printf.sprintf "(%s + %s)" (to_string t1) (to_string t2)
    | Unit             -> "Type.Unit"
    | Bitvector numexp -> Printf.sprintf "Type.Bitvector(%s)" (NumericExpression.to_string numexp)
    | Enum id          -> Printf.sprintf "Type.Enum(%s)" (Identifier.string_of id)
    | Record id        -> Printf.sprintf "Type.Record(%s)" (Identifier.string_of id)
    | Application (constructor, targs) -> begin
        let constructor' = to_string constructor
        and targs' = List.map ~f:TypeArgument.to_string targs
        in
        Printf.sprintf "%s(%s)" constructor' (String.concat ~sep:"," targs')
      end
    | Tuple ts -> begin
        let ts' = List.map ~f:to_string ts
        in
        Printf.sprintf "(%s)" (String.concat ~sep:"," ts')
      end

  let rec equal (t1 : t) (t2 : t) : bool =
    match t1, t2 with
    | Int                     , Int                      -> true
    | Bool                    , Bool                     -> true
    | String                  , String                   -> true
    | List t1                 , List t2                  -> equal t1 t2
    | Product (t1a, t1b)      , Product (t2a, t2b)       -> equal t1a t2a && equal t1b t2b
    | Sum (t1a, t1b)          , Sum (t2a, t2b)           -> equal t1a t2a && equal t1b t2b
    | Unit                    , Unit                     -> true
    | Enum id1                , Enum id2                 -> Identifier.equal id1 id2
    | Bitvector nexp1         , Bitvector nexp2          -> NumericExpression.equal nexp1 nexp2
    | Tuple ts1               , Tuple ts2                -> Auxlib.equal_lists ~eq:equal ts1 ts2
    | Record id1              , Record id2               -> Identifier.equal id1 id2
    | Application (c1, targs1), Application (c2, targs2) -> equal c1 c2 && Auxlib.equal_lists ~eq:TypeArgument.equal targs1 targs2
    | _                       , _                        -> false
end

and TypeArgument : sig
  type t =
    | Type              of Type.t
    | NumericExpression of NumericExpression.t
    | Bool              of NumericConstraint.t

  val to_string : t -> string

  val equal : t -> t -> bool
end = struct
  type t =
    | Type              of Type.t
    | NumericExpression of NumericExpression.t
    | Bool              of NumericConstraint.t

  let to_string (type_argument : t) : string =
    match type_argument with
    | Type t                   -> Type.to_string t
    | NumericExpression numexp -> NumericExpression.to_string numexp
    | Bool nc                  -> NumericConstraint.to_string nc

  let equal (t1 : t) (t2 : t) : bool =
    match t1, t2 with
    | Type t1                , Type t2                 -> Type.equal t1 t2
    | NumericExpression nexp1, NumericExpression nexp2 -> NumericExpression.equal nexp1 nexp2
    | Bool nconstr1          , Bool nconstr2           -> NumericConstraint.equal nconstr1 nconstr2
    | _                      , _                       -> false
end

and NumericConstraint : sig
  type t =
    | Equal      of NumericExpression.t * NumericExpression.t
    | BoundedGE  of NumericExpression.t * NumericExpression.t
    | BoundedGT  of NumericExpression.t * NumericExpression.t
    | BoundedLE  of NumericExpression.t * NumericExpression.t
    | BoundedLT  of NumericExpression.t * NumericExpression.t
    | NotEqual   of NumericExpression.t * NumericExpression.t
    | Set        of Identifier.t        * Z.t list
    | Or         of t                   * t
    | And        of t                   * t
    | App        of Identifier.t        * TypeArgument.t list
    | Var        of Identifier.t
    | True
    | False

  val to_string : t -> string

  val equal : t -> t -> bool
end = struct
  type t =
    | Equal      of NumericExpression.t * NumericExpression.t
    | BoundedGE  of NumericExpression.t * NumericExpression.t
    | BoundedGT  of NumericExpression.t * NumericExpression.t
    | BoundedLE  of NumericExpression.t * NumericExpression.t
    | BoundedLT  of NumericExpression.t * NumericExpression.t
    | NotEqual   of NumericExpression.t * NumericExpression.t
    | Set        of Identifier.t        * Z.t list
    | Or         of NumericConstraint.t * NumericConstraint.t
    | And        of NumericConstraint.t * NumericConstraint.t
    | App        of Identifier.t        * TypeArgument.t list
    | Var        of Identifier.t
    | True
    | False

  let rec to_string (numeric_constraint : t) =
    match numeric_constraint with
    | Equal     (e1, e2)  -> Printf.sprintf "(%s == %s)" (NumericExpression.to_string e1) (NumericExpression.to_string e2)
    | BoundedGE (e1, e2)  -> Printf.sprintf "(%s >= %s)" (NumericExpression.to_string e1) (NumericExpression.to_string e2)
    | BoundedGT (e1, e2)  -> Printf.sprintf "(%s > %s)"  (NumericExpression.to_string e1) (NumericExpression.to_string e2)
    | BoundedLE (e1, e2)  -> Printf.sprintf "(%s <= %s)" (NumericExpression.to_string e1) (NumericExpression.to_string e2)
    | BoundedLT (e1, e2)  -> Printf.sprintf "(%s < %s)"  (NumericExpression.to_string e1) (NumericExpression.to_string e2)
    | NotEqual  (e1, e2)  -> Printf.sprintf "(%s != %s)" (NumericExpression.to_string e1) (NumericExpression.to_string e2)
    | Var id              -> Identifier.string_of id
    | True                -> "NC_true"
    | False               -> "NC_false"
    | Set (_, _)          -> failwith "Not yet imnplemented"
    | Or (c1, c2)         -> Printf.sprintf "(%s || %s)" (to_string c1) (to_string c2)
    | And (c1, c2)        -> Printf.sprintf "(%s && %s)" (to_string c1) (to_string c2)
    | App (_, _)          -> failwith "Not yet imnplemented"

  let rec equal (t1 : t) (t2 : t) : bool =
    match t1, t2 with
     | Equal (t1a, t1b)    , Equal (t2a, t2b)     -> NumericExpression.equal t1a t2a && NumericExpression.equal t1b t2b
     | NotEqual (t1a, t1b) , NotEqual (t2a, t2b)  -> NumericExpression.equal t1a t2a && NumericExpression.equal t1b t2b
     | BoundedGE (t1a, t1b), BoundedGE (t2a, t2b) -> NumericExpression.equal t1a t2a && NumericExpression.equal t1b t2b
     | BoundedGT (t1a, t1b), BoundedGT (t2a, t2b) -> NumericExpression.equal t1a t2a && NumericExpression.equal t1b t2b
     | BoundedLE (t1a, t1b), BoundedLE (t2a, t2b) -> NumericExpression.equal t1a t2a && NumericExpression.equal t1b t2b
     | BoundedLT (t1a, t1b), BoundedLT (t2a, t2b) -> NumericExpression.equal t1a t2a && NumericExpression.equal t1b t2b
     | Set (id1, ns1)      , Set (id2, ns2)       -> Identifier.equal id1 id2 && Auxlib.equal_lists ~eq:Z.equal ns1 ns2
     | Or (t1a, t1b)       , Or (t2a, t2b)        -> equal t1a t2a && equal t1b t2b
     | And (t1a, t1b)      , And (t2a, t2b)       -> equal t1a t2a && equal t1b t2b
     | App (id1, targs1)   , App (id2, targs2)    -> Identifier.equal id1 id2 && Auxlib.equal_lists ~eq:TypeArgument.equal targs1 targs2
     | Var id1             , Var id2              -> Identifier.equal id1 id2
     | True                , True                 -> true
     | False               , False                -> true
     | _                   , _                    -> false
end
