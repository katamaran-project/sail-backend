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

    | Nat                                             (* TODO remove *)
    | Atom                                            (* TODO remove *)
    | Application of t * TypeArgument.t list          (* TODO remove *)
    | Custom      of Identifier.t                     (* TODO remove *)
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

    | Nat                                             (* TODO remove *)
    | Atom                                            (* TODO remove *)
    | Application of t * TypeArgument.t list          (* TODO remove *)
    | Custom      of Identifier.t                     (* TODO remove *)
end
and TypeArgument : sig
  type t =
    | Type              of Type.t
    | NumericExpression of NumericExpression.t
    | Bool              of NumericConstraint.t
end = struct
  type t =
    | Type              of Type.t
    | NumericExpression of NumericExpression.t
    | Bool              of NumericConstraint.t
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
