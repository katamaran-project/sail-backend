open Base
open Monads.Notations.Star(GenerationContext)

module GC = GenerationContext


(* todo move this *)
let pp_bind (arg, t) =
  GC.return @@ PP.(utf8string ("\"" ^ (Ast.Identifier.string_of arg) ^ "\" ∷ " ) ^^ t)


(* todo move this *)
let pp_kind (kind : Ast.Kind.t) : PP.document GC.t =
  match kind with
  | Type -> GC.not_yet_implemented [%here]
  | Int  -> GC.return @@ PP.string @@ "nat"
  | Bool -> GC.not_yet_implemented [%here]


(* todo move this *)
let pp_type_quantifier quantifier =
  let pp_type_quantifier_item (identifier, kind) =
    let identifier' = Identifier.pp identifier
    in
    let* kind' = pp_kind kind
    in
    GC.return (identifier', Some kind')
  in
  GC.map ~f:pp_type_quantifier_item quantifier
