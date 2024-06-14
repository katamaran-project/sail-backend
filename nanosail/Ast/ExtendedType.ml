open Base


module Parameter = struct
  type t =
    | Tuple of t list
    | Int   of int
    | Bool  of int
    | Other of string

  let rec string_of (extended_type : t) : string =
    match extended_type with
    | Tuple ts -> String.concat ~sep:" * " @@ List.map ~f:(fun t -> Printf.sprintf "(%s)" (string_of t)) ts
    | Int k    -> Printf.sprintf "int(#%d)" k
    | Bool k   -> Printf.sprintf "bool(#%d)" k
    | Other s  -> s
end

(* OCaml requires this duplication for recursive modules *)
module rec IntExpression : sig
  type t =
    | Var      of int
    | Constant of Z.t
    | Add      of t * t
    | Sub      of t * t
    | Mul      of t * t
    | Neg      of t
end = struct
  type t =
    | Var      of int
    | Constant of Z.t
    | Add      of t * t
    | Sub      of t * t
    | Mul      of t * t
    | Neg      of t
end and BoolExpression : sig
  type t =
    | Var                  of int
    | And                  of t * t
    | Or                   of t * t
    | Equal                of IntExpression.t * IntExpression.t
    | NotEqual             of IntExpression.t * IntExpression.t
    | LessThan             of IntExpression.t * IntExpression.t
    | LessThanOrEqualTo    of IntExpression.t * IntExpression.t
    | GreaterThan          of IntExpression.t * IntExpression.t
    | GreaterThanOrEqualTo of IntExpression.t * IntExpression.t
end = struct
      type t =
        | Var                  of int
        | And                  of t * t
        | Or                   of t * t
        | Equal                of IntExpression.t * IntExpression.t
        | NotEqual             of IntExpression.t * IntExpression.t
        | LessThan             of IntExpression.t * IntExpression.t
        | LessThanOrEqualTo    of IntExpression.t * IntExpression.t
        | GreaterThan          of IntExpression.t * IntExpression.t
        | GreaterThanOrEqualTo of IntExpression.t * IntExpression.t
    end

module ReturnValue = struct
  type t =
    | Int   of IntExpression.t
    | Bool  of BoolExpression.t
    | Other of string
end

