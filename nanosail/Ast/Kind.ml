(*
   Nanosail version of Libsail.Ast.kind
   Kinds are the types of values on the type level.

   For example, take the following function that fetches a value from a vector:

     val at : forall 'a 'n 'i, 0 <= 'i & 'i < 'n. (vector('n, 'a), int('i)) -> 'a

   (This function would only be usable if the index is known at compile-time.)
   Here, 'n and 'i have kinds Int and 'a has kind Type.
*)

type t =
  | Type
  | Int
  | Bool


let to_string (kind : t) =
  match kind with
  | Type -> "Kind.Type"
  | Int  -> "Kind.Int"
  | Bool -> "Kind.Bool"


let to_fexpr (kind : t) : FExpr.t =
  let mk (name : string) =
    FExpr.mk_symbol @@ "Kind:" ^ name
  in
  match kind with
  | Type -> mk "Type"
  | Int  -> mk "Int"
  | Bool -> mk "Bool"
