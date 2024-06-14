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
    | TA_type   of Type.t
    | TA_numexp of NumericExpression.t
    | TA_bool   of NumericConstraint.t
end = struct
  type t =
    | TA_type   of Type.t
    | TA_numexp of NumericExpression.t
    | TA_bool   of NumericConstraint.t
end
and NumericConstraint : sig
  type t =
    | NC_equal      of NumericExpression.t * NumericExpression.t
    | NC_bounded_ge of NumericExpression.t * NumericExpression.t
    | NC_bounded_gt of NumericExpression.t * NumericExpression.t
    | NC_bounded_le of NumericExpression.t * NumericExpression.t
    | NC_bounded_lt of NumericExpression.t * NumericExpression.t
    | NC_not_equal  of NumericExpression.t * NumericExpression.t
    | NC_set        of Identifier.t       * Z.t list
    | NC_or         of t                  * t
    | NC_and        of t                  * t
    | NC_app        of Identifier.t       * TypeArgument.t list
    | NC_var        of Identifier.t
    | NC_true
    | NC_false
end = struct
  type t =
    | NC_equal      of NumericExpression.t * NumericExpression.t
    | NC_bounded_ge of NumericExpression.t * NumericExpression.t
    | NC_bounded_gt of NumericExpression.t * NumericExpression.t
    | NC_bounded_le of NumericExpression.t * NumericExpression.t
    | NC_bounded_lt of NumericExpression.t * NumericExpression.t
    | NC_not_equal  of NumericExpression.t * NumericExpression.t
    | NC_set        of Identifier.t        * Z.t list
    | NC_or         of NumericConstraint.t * NumericConstraint.t
    | NC_and        of NumericConstraint.t * NumericConstraint.t
    | NC_app        of Identifier.t        * TypeArgument.t list
    | NC_var        of Identifier.t
    | NC_true
    | NC_false
end
