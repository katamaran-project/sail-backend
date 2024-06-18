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
    | Record                                          (* TODO complete *)

    | Atom                                            (* TODO remove *)
    | Application of t * TypeArgument.t list          (* TODO remove *)

  val to_string : t -> string
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
    | Record                                          (* TODO complete *)

    | Atom                                            (* TODO remove *)
    | Application of t * TypeArgument.t list          (* TODO remove *)

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
    | Record           -> "Type.Record"
    | Atom             -> "Type.Atom"
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
end

and TypeArgument : sig
  type t =
    | Type              of Type.t
    | NumericExpression of NumericExpression.t
    | Bool              of NumericConstraint.t

  val to_string : t -> string
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

  let to_string (numeric_constraint : t) =
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
    | Or (_, _)           -> failwith "Not yet imnplemented"
    | And (_, _)          -> failwith "Not yet imnplemented"
    | App (_, _)          -> failwith "Not yet imnplemented"
end
